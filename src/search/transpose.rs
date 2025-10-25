// src/search/transpose.rs


use rand::rngs::SmallRng;
use rand::SeedableRng;
use rand::RngCore;

use crate::game::board::Position;
use crate::game::defs::{Color, Square, Piece};
use crate::game::gamestate::{CastlingRights, State};
use crate::game::history::Undo;
use crate::game::moves::{Move, Flag};

// ===================== Bounds / TT entry =====================

const MAX_PLY: usize = 512;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Bound {
    Exact,
    LowerBound,
    UpperBound,
}

#[derive(Clone, Copy)]
pub struct TTEntry {
    pub hash: u64,
    pub depth: i32,
    pub score: i32,           // stored as normalized (see to_tt/from_tt)
    pub bound: Bound,
    pub best_move: Option<Move>,
}

// ------------------- TT sizing and table ---------------------

const TT_SIZE_MB: usize = 64;
const ENTRY_SIZE: usize = core::mem::size_of::<TTEntry>();
const NUM_ENTRIES: usize = (TT_SIZE_MB * 1024 * 1024) / ENTRY_SIZE;

pub struct TranspositionTable {
    entries: Vec<Option<TTEntry>>,
}

impl TranspositionTable {
    pub fn new() -> Self {
        Self { entries: vec![None; NUM_ENTRIES] }
    }

    #[inline]
    fn index(&self, hash: u64) -> usize {
        (hash as usize) % NUM_ENTRIES
    }

    #[inline]
    pub fn probe(&self, hash: u64) -> Option<TTEntry> {
        let i = self.index(hash);
        if let Some(e) = self.entries[i] {
            if e.hash == hash { return Some(e); }
        }
        None
    }

    /// Depth-preferred replacement: keep deeper entries on collision.
    pub fn store(&mut self, entry: TTEntry) {
        let i = self.index(entry.hash);
        match self.entries[i] {
            None => self.entries[i] = Some(entry),
            Some(old) => {
                if old.hash != entry.hash || entry.depth >= old.depth {
                    self.entries[i] = Some(entry);
                }
            }
        }
    }
}

// ---------------- Mate score normalization helpers ----------

const MATE: i32 = 29_000;

#[inline]
pub fn to_tt(score: i32, ply: i32) -> i32 {
    if score >  MATE - 1000 { score + ply }
    else if score < -MATE + 1000 { score - ply }
    else { score }
}

#[inline]
pub fn from_tt(score: i32, ply: i32) -> i32 {
    if score >  MATE - 1000 { score - ply }
    else if score < -MATE + 1000 { score + ply }
    else { score }
}

// --------------- Zobrist hashing ----------------------------

pub struct ZobristKeys {
    ps: [[[u64; 64]; 6]; 2],
    castling: [u64; 4],
    ep_file:  [u64; 8],
    side:     u64,
}


impl ZobristKeys {
    pub fn new_with_seed(seed: u64) -> Self {
        let mut rng = SmallRng::seed_from_u64(seed);

        let mut ps = [[[0u64; 64]; 6]; 2];
        for c in 0..2 {
            for p in 0..6 {
                for s in 0..64 {
                    ps[c][p][s] = rng.next_u64();
                }
            }
        }

        let castling = [rng.next_u64(), rng.next_u64(), rng.next_u64(), rng.next_u64()];
        let mut ep_file = [0u64; 8];
        for f in 0..8 { ep_file[f] = rng.next_u64(); }
        let side = rng.next_u64();

        Self { ps, castling, ep_file, side }
    }

    #[inline] fn ps_key(&self, c: Color, p: Piece, sq: Square) -> u64 { self.ps[c as usize][p as usize][sq.0 as usize] }
    #[inline] fn castle_key(&self, i: usize) -> u64 { self.castling[i] }
    #[inline] fn ep_key(&self, file: u8) -> u64 { self.ep_file[file as usize] }
    #[inline] fn stm_key(&self) -> u64 { self.side }
}

pub struct Zobrist {
    pub keys: ZobristKeys,
    pub hash: u64,
    stack: [u64; MAX_PLY],
    stack_ptr: usize
}

impl Zobrist {
    pub fn from_position(keys: ZobristKeys, pos: &Position) -> Self {
        let mut h: u64 = 0;

        for &c in [Color::White, Color::Black].iter() {
            for &p in [Piece::King, Piece::Queen, Piece::Rook, Piece::Knight, Piece::Bishop, Piece::Pawn].iter() {
                let mut bb = pos.pieces(c, p).0;
                while bb != 0 {
                    let sq = Square(bb.trailing_zeros() as u8);
                    h ^= keys.ps_key(c, p, sq);
                    bb &= bb - 1;
                }
            }
        }

        if pos.state.stm == Color::Black { h ^= keys.stm_key(); }

        let cr = pos.state.castling_rights.0;
        if (cr & CastlingRights::WK.0) != 0 { h ^= keys.castle_key(0); }
        if (cr & CastlingRights::WQ.0) != 0 { h ^= keys.castle_key(1); }
        if (cr & CastlingRights::BK.0) != 0 { h ^= keys.castle_key(2); }
        if (cr & CastlingRights::BQ.0) != 0 { h ^= keys.castle_key(3); }

        if let Some(ep) = pos.state.en_passant_square {
            h ^= keys.ep_key(ep.get_file());
        }

        Self { keys, hash: h, stack: [0; MAX_PLY], stack_ptr: 0 }
    }

    /// Incremental hash update using the pre/post State + Undo info.
    pub fn apply_move_delta(&mut self, before: &State, after: &State, us: Color, mv: Move, undo: &Undo) {
        let from = mv.from_sq();
        let to   = mv.to_sq();

        // side-to-move
        self.hash ^= self.keys.stm_key();

        // EP keys
        if let Some(ep) = before.en_passant_square { self.hash ^= self.keys.ep_key(ep.get_file()); }
        if let Some(ep) = after.en_passant_square  { self.hash ^= self.keys.ep_key(ep.get_file()); }

        // moving piece off 'from'
        self.hash ^= self.keys.ps_key(us, undo.mover_before, from);

        // captured piece (EP or normal)
        if let Some((their, vic, sq)) = undo.captured {
            self.hash ^= self.keys.ps_key(their, vic, sq);
        }

        // piece placed on 'to' (promotion changes kind)
        let placed = if mv.is_promo() {
            match mv.flags() {
                x if x == Flag::PROMO_N as u16 || x == Flag::PROMO_N_CAPTURE as u16 => Piece::Knight,
                x if x == Flag::PROMO_B as u16 || x == Flag::PROMO_B_CAPTURE as u16 => Piece::Bishop,
                x if x == Flag::PROMO_R as u16 || x == Flag::PROMO_R_CAPTURE as u16 => Piece::Rook,
                _ => Piece::Queen,
            }
        } else { undo.mover_before };
        self.hash ^= self.keys.ps_key(us, placed, to);

        // castling rook hop
        if let Some((rf, rt)) = undo.rook_move {
            self.hash ^= self.keys.ps_key(us, Piece::Rook, rf);
            self.hash ^= self.keys.ps_key(us, Piece::Rook, rt);
        }

        // castling rights diff
        let ob = before.castling_rights.0;
        let nb = after.castling_rights.0;
        for i in 0..4 {
            if ((ob ^ nb) & (1 << i)) != 0 { self.hash ^= self.keys.castle_key(i); }
        }
    }

    pub fn apply_null_delta(&mut self, before: &State, after: &State) {
        // side-to-move
        self.hash ^= self.keys.stm_key();

        // EP keys
        if let Some(ep) = before.en_passant_square { self.hash ^= self.keys.ep_key(ep.get_file()); }
        if let Some(ep) = after.en_passant_square  { self.hash ^= self.keys.ep_key(ep.get_file()); }

    }

    

    #[inline] pub fn push(&mut self) { self.stack[self.stack_ptr] = self.hash; self.stack_ptr += 1; }
    #[inline] pub fn pop(&mut self)  { self.stack_ptr -= 1; self.hash = self.stack[self.stack_ptr]; }

    #[inline]
    pub fn is_threefold(&self, reversible_plies: usize) -> bool {
    let n = self.stack_ptr;
    if n < 2 { return false; }

    let mut seen = 1usize; // current position
    let limit = reversible_plies.min(n);

    let mut back = 2usize; // step by 2 so STM matches
    while back <= limit {
        if self.stack[n - back] == self.hash {
            seen += 1;
            if seen >= 3 { return true; }
        }
        back += 2;
    }
    false
}
}
