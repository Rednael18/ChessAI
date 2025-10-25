// src/game/history.rs

use crate::game;
use crate::game::board::Position;
use crate::game::defs::{Color, Piece, Square};
use crate::game::moves::{Move, Flag};
use crate::search::transpose::Zobrist;
use std::ops::Not;


// =================== Undo record ===================

#[derive(Clone, Debug)]
pub struct Undo {
    pub mv: Move,
    pub mover_before: Piece,                    // pre-promotion kind
    pub captured: Option<(Color, Piece, Square)>,
    pub rook_move: Option<(Square, Square)>,    // if castling, rook from->to
    pub ep_captured_sq: Option<Square>,         // EP victim square (for clarity)
    pub prev_state: game::gamestate::State,     // castling/ep/halfmove/stm
    pub is_null: bool
}

// =================== History ===================

pub struct History {
    pos: Position,
    pt: [Option<(Color, Piece)>; 64],
    pub stack: Vec<Undo>,
    prev_cache: Option<Position>, // optional: last pre-apply snapshot for previous()
    half_move: u32,
    half_move_cache: Vec<u32>
}

impl History {
    pub fn new(start: Position) -> Self {
        let pt0 = build_piece_table(&start);
        Self {
            pos: start,
            pt: pt0,
            stack: Vec::with_capacity(512),
            prev_cache: None,
            half_move: 0,
            half_move_cache: Vec::with_capacity(512)
        }
    }

    #[inline]
    pub fn current(&self) -> &Position {
        &self.pos
    }

    #[inline]
    pub fn last_was_null(&self) -> bool {
        self.stack.last().map_or(false, |u| u.is_null)
    }

    pub fn apply_null(&mut self, z: &mut Zobrist) {
        let before = self.pos.state.clone();

        // EP is always cleared on a null move
        self.pos.state.en_passant_square = None;

        // Flip side to move
        self.pos.state.stm = !before.stm;

        // Push half-move history and increment half-move clock (no capture / no pawn move)
        self.half_move_cache.push(self.half_move);
        self.half_move = self.half_move.saturating_add(1);

        // Zobrist: update using only state changes (stm toggle + EP clear)
        z.apply_null_delta(&before, &self.pos.state);

        // Push a null-undo frame to the stack
        self.stack.push(Undo {
            mv: Move::default(),                // sentinel; never used for unmake
            mover_before: Piece::Pawn,          // irrelevant for null; any value
            captured: None,
            rook_move: None,
            ep_captured_sq: None,
            prev_state: before,
            is_null: true,
        });

        // Keep ply counter consistent with your normal apply()
        self.pos.state.increase_ply();

        self.prev_cache = None;
    }

    /// Undo the last null move (call z.pop() in the caller).
    pub fn undo_null(&mut self) -> bool {
        if let Some(u) = self.stack.pop() {
            debug_assert!(u.is_null, "undo_null called but top of stack is not a null move");

            // Restore previous state
            self.pos.state = u.prev_state;

            // Restore half-move clock
            let hm = self.half_move_cache.pop();
            self.half_move = hm.unwrap_or(0);

            self.prev_cache = None;
            true
        } else {
            false
        }
    }

    /// Apply a move in-place, push Undo, and update Zobrist.
    pub fn apply(&mut self, mv: Move, z: &mut Zobrist) {
        // Snapshot only the small State (no cloning Position)
        let before = self.pos.state.clone();
        let us = before.stm;

        // Make move and get undo record
        let u = make_move_in_place(&mut self.pos, &mut self.pt, mv);

        self.half_move_cache.push(self.half_move);
        if mv.is_capture() || u.mover_before == Piece::Pawn {
            self.half_move = 0;
        } else {
            self.half_move += 1;
        }

        // Delta hash using the undo info and before/after State
        z.apply_move_delta(&before, &self.pos.state, us, mv, &u);

        // Push undo onto the history stack
        self.stack.push(u);

        self.pos.state.increase_ply();

        // We don’t keep prev_cache anymore in the hot path
        self.prev_cache = None;
    }

    /// Undo last move (call z.pop() in the caller).
    pub fn undo(&mut self) -> bool {
        if let Some(true) = self.stack.last().map(|u| u.is_null) {
            return self.undo_null();
        }
        if let Some(u) = self.stack.pop() {
            unmake_move_in_place(&mut self.pos, &mut self.pt, &u);
            self.prev_cache = None;
            self.half_move = self.half_move_cache.pop().unwrap_or(0);
            true
        } else { false }
    }

    #[inline]
    pub fn plies(&self) -> usize {
        self.stack.len()
    }

    #[inline]
    pub fn half_move(&self) -> u32 {
        self.half_move
    }

    #[inline]
    pub fn piece_table(&self) -> &[Option<(Color, Piece)>; 64] {
        &self.pt
    }
}

/// Build a 64-entry piece lookup table from a Position.
pub fn build_piece_table(pos: &Position) -> [Option<(Color, Piece)>; 64] {
    let mut pt = [None; 64];
    for &c in &[Color::White, Color::Black] {
        for &p in &[Piece::King, Piece::Queen, Piece::Rook, Piece::Knight, Piece::Bishop, Piece::Pawn] {
            let mut b = pos.bb_pieces[c as usize][p as usize].0;
            while b != 0 {
                let idx = b.trailing_zeros() as usize;
                pt[idx] = Some((c, p));
                b &= b - 1;
            }
        }
    }
    pt
}

// =================== Make / Unmake ===================

fn make_move_in_place(
    pos: &mut Position,
    tbl: &mut [Option<(Color, Piece)>; 64],
    mv: Move,
) -> Undo {
    use game::defs::squarename as SQ;

    let us   = pos.state.stm;
    let them = !us;
    let from = mv.from_sq();
    let to   = mv.to_sq();
    let flags = mv.flags();

    let from_i = from.0 as usize;
    let to_i   = to.0 as usize;

    let (c_here, mut mover) = tbl[from_i]
        .expect("make_move_in_place: no moving piece on 'from'");
    debug_assert!(c_here == us, "make_move_in_place: side-to-move mismatch");

    let mut undo = Undo {
        mv,
        mover_before: mover,
        captured: None,
        rook_move: None,
        ep_captured_sq: None,
        prev_state: pos.state.clone(),
        is_null: false, 
    };

    // Clear EP by default
    pos.state.en_passant_square = None;

    // Captures (EP or normal)
    if flags == Flag::EP_CAPTURE as u16 {
        let cap_sq = if us == Color::White { Square(to.0 - 8) } else { Square(to.0 + 8) };
        let cap_i = cap_sq.0 as usize;
        if let Some((vc, vp)) = tbl[cap_i] {
            clear_piece(pos, vc, vp, cap_sq);
            tbl[cap_i] = None;
            undo.captured = Some((vc, vp, cap_sq));
            undo.ep_captured_sq = Some(cap_sq);
        } else {
            panic!("EP flag set but no pawn behind target");
        }
    } else if (flags & (Flag::CAPTURE as u16)) != 0 {
        if let Some((vc, vp)) = tbl[to_i] {
            clear_piece(pos, vc, vp, to);
            tbl[to_i] = None;
            undo.captured = Some((vc, vp, to));
            strip_rights_if_rook_square_captured(pos, vc, to);
        } else {
            panic!("Capture flag set but no piece on 'to'");
        }
    }

    // Move mover (apply promotion if any)
    clear_piece(pos, us, mover, from);
    tbl[from_i] = None;

    if (flags & 0b1000) != 0 {
        mover = match flags {
            x if x == Flag::PROMO_N as u16 || x == Flag::PROMO_N_CAPTURE as u16 => Piece::Knight,
            x if x == Flag::PROMO_B as u16 || x == Flag::PROMO_B_CAPTURE as u16 => Piece::Bishop,
            x if x == Flag::PROMO_R as u16 || x == Flag::PROMO_R_CAPTURE as u16 => Piece::Rook,
            _ => Piece::Queen,
        };
    }
    set_piece(pos, us, mover, to);
    tbl[to_i] = Some((us, mover));

    // Castling rook move
    if flags == Flag::CASTLE_KING as u16 || flags == Flag::CASTLE_QUEEN as u16 {
        let (rf, rt) = match (us, flags) {
            (Color::White, x) if x == Flag::CASTLE_KING  as u16 => (Square(SQ::H1.0), Square(SQ::F1.0)),
            (Color::White, _)                                  => (Square(SQ::A1.0), Square(SQ::D1.0)),
            (Color::Black, x) if x == Flag::CASTLE_KING  as u16 => (Square(SQ::H8.0), Square(SQ::F8.0)),
            (Color::Black, _)                                  => (Square(SQ::A8.0), Square(SQ::D8.0)),
        };
        clear_piece(pos, us, Piece::Rook, rf);
        set_piece  (pos, us, Piece::Rook, rt);
        tbl[rf.0 as usize] = None;
        tbl[rt.0 as usize] = Some((us, Piece::Rook));
        undo.rook_move = Some((rf, rt));
    }

    // EP target on double push
    if flags == Flag::DOUBLE_PUSH as u16 {
        let mid = Square((from.0 + to.0) / 2);
        pos.state.en_passant_square = Some(mid);
    }

    // Castling rights (mover side)
    if undo.mover_before == Piece::King {
        strip_both_rights(pos, us);
    } else if undo.mover_before == Piece::Rook {
        strip_rights_if_rook_moved_from(pos, us, from);
    }

    // Side to move
    pos.state.stm = them;

    // Cheap sanity
    let wk = pos.bb_pieces[Color::White as usize][Piece::King as usize].0.count_ones();
    let bk = pos.bb_pieces[Color::Black as usize][Piece::King as usize].0.count_ones();
    debug_assert!(wk == 1, "White king count = {wk}, last move = {:?}", mv.as_u16());
    debug_assert!(bk == 1, "Black king count = {bk}, last move = {:?}", mv.as_u16());

    undo
}

fn unmake_move_in_place(
    pos: &mut Position,
    tbl: &mut [Option<(Color, Piece)>; 64],
    u: &Undo,
) {
    let to   = u.mv.to_sq();
    let from = u.mv.from_sq();
    let us   = pos.state.stm.not(); // side that moved
    let to_i = to.0 as usize;
    let from_i = from.0 as usize;

    // Restore state (includes stm flip back)
    pos.state = u.prev_state.clone();

    // Undo castling rook move if any
    if let Some((rf, rt)) = u.rook_move {
        clear_piece(pos, us, Piece::Rook, rt);
        set_piece  (pos, us, Piece::Rook, rf);
        tbl[rt.0 as usize] = None;
        tbl[rf.0 as usize] = Some((us, Piece::Rook));
    }

    // Remove our piece from 'to' (promoted kind or not), restore original to 'from'
    if let Some((_c, _p)) = tbl[to_i] {
        // Clear all possibilities at 'to'
        clear_piece(pos, us, Piece::Queen,  to);
        clear_piece(pos, us, Piece::Rook,   to);
        clear_piece(pos, us, Piece::Bishop, to);
        clear_piece(pos, us, Piece::Knight, to);
        clear_piece(pos, us, Piece::Pawn,   to);
        clear_piece(pos, us, Piece::King,   to);
        tbl[to_i] = None;
    }
    set_piece(pos, us, u.mover_before, from);
    tbl[from_i] = Some((us, u.mover_before));

    // Restore captured piece (EP or normal)
    if let Some((vc, vp, sq)) = u.captured {
        set_piece(pos, vc, vp, sq);
        tbl[sq.0 as usize] = Some((vc, vp));
    }
}

// =================== Helpers (local) ===================

#[inline]
fn set_piece(pos: &mut Position, c: Color, p: Piece, sq: Square) {
    let bit = 1u64 << sq.0;
    pos.bb_pieces[c as usize][p as usize].0 |= bit;
    pos.bb_sides[c as usize].0 |= bit;
}

#[inline]
fn clear_piece(pos: &mut Position, c: Color, p: Piece, sq: Square) {
    let bit = 1u64 << sq.0;
    pos.bb_pieces[c as usize][p as usize].0 &= !bit;
    pos.bb_sides[c as usize].0 &= !bit;
}

fn strip_both_rights(pos: &mut Position, c: Color) {
    use game::gamestate::CastlingRights as CR;
    if c == Color::White {
        pos.state.castling_rights.remove(CR::WK);
        pos.state.castling_rights.remove(CR::WQ);
    } else {
        pos.state.castling_rights.remove(CR::BK);
        pos.state.castling_rights.remove(CR::BQ);
    }
}

fn strip_rights_if_rook_moved_from(pos: &mut Position, c: Color, from: Square) {
    use game::gamestate::CastlingRights as CR;
    match (c, from.0) {
        (Color::White, x) if x == game::defs::squarename::A1.0 => pos.state.castling_rights.remove(CR::WQ),
        (Color::White, x) if x == game::defs::squarename::H1.0 => pos.state.castling_rights.remove(CR::WK),
        (Color::Black, x) if x == game::defs::squarename::A8.0 => pos.state.castling_rights.remove(CR::BQ),
        (Color::Black, x) if x == game::defs::squarename::H8.0 => pos.state.castling_rights.remove(CR::BK),
        _ => {}
    }
}

fn strip_rights_if_rook_square_captured(pos: &mut Position, victim_color: Color, sq: Square) {
    use game::gamestate::CastlingRights as CR;
    match (victim_color, sq.0) {
        (Color::White, x) if x == game::defs::squarename::A1.0 => pos.state.castling_rights.remove(CR::WQ),
        (Color::White, x) if x == game::defs::squarename::H1.0 => pos.state.castling_rights.remove(CR::WK),
        (Color::Black, x) if x == game::defs::squarename::A8.0 => pos.state.castling_rights.remove(CR::BQ),
        (Color::Black, x) if x == game::defs::squarename::H8.0 => pos.state.castling_rights.remove(CR::BK),
        _ => {}
    }
}
