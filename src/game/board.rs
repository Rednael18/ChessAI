// src/game/board.rs

use core::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr, BitOrAssign};
use std::iter::FusedIterator;
use colored::*;
use std::fmt;

use crate::game::defs::{self, Color, Piece};
use crate::game::defs::Square;
use crate::game::gamestate;
use crate::game::moves::{MAGICS, Magics};

#[inline]
fn magics() -> &'static Magics {
    MAGICS.get_or_init(|| Magics::new())
}

// ############################
//           BITBOARDS
// ############################


#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct BitBoard(pub u64);

pub mod rankfile {
    #![allow(dead_code)]
    use crate::game::board::BitBoard;

    pub const fn build_file(file: u32) -> BitBoard {
        BitBoard(0x0101_0101_0101_0101u64 << file)
    }


    pub const fn build_rank(rank: u32) -> BitBoard {
        BitBoard(0xFFu64 << (rank * 8))
    }


    pub const FILE_A: BitBoard = build_file(0);
    pub const FILE_B: BitBoard = build_file(1);
    pub const FILE_C: BitBoard = build_file(2);
    pub const FILE_D: BitBoard = build_file(3);
    pub const FILE_E: BitBoard = build_file(4);
    pub const FILE_F: BitBoard = build_file(5);
    pub const FILE_G: BitBoard = build_file(6);
    pub const FILE_H: BitBoard = build_file(7);

    pub const FILE_AB: BitBoard = BitBoard(FILE_A.0 | FILE_B.0);
    pub const FILE_GH: BitBoard = BitBoard(FILE_G.0 | FILE_H.0);

    pub const RANK_1: BitBoard = build_rank(0);
    pub const RANK_2: BitBoard = build_rank(1);
    pub const RANK_3: BitBoard = build_rank(2);
    pub const RANK_4: BitBoard = build_rank(3);
    pub const RANK_5: BitBoard = build_rank(4);
    pub const RANK_6: BitBoard = build_rank(5);
    pub const RANK_7: BitBoard = build_rank(6);
    pub const RANK_8: BitBoard = build_rank(7);
    
}


impl BitBoard {
    pub fn north(self) -> Self {
        self << 8
    }

    pub fn south(self) -> Self {
        self >> 8
    }

    pub fn east(self) -> Self {
        (self << 1) & !rankfile::FILE_H
    }

    pub fn west(self) -> Self {
        (self >> 1) & !rankfile::FILE_A
    }

    pub const fn from_square(sq:defs::Square) -> Self {
        Self(1 << sq.0)
    }

    pub fn to_squares(self) -> Vec<defs::Square> {
        let mut bb = self.0;
        let mut squares = Vec::new();

        while bb != 0 {
            let idx = bb.trailing_zeros() as u8;
            squares.push(defs::Square(idx));
            bb &= bb - 1;
        }

        squares
    }

    #[inline]
    pub fn iter_squares(&self) -> SquareIter {
        SquareIter(self.0)
    }

    #[inline]
    pub fn into_iter(self) -> SquareIter {
        SquareIter(self.0)
    }
}

impl IntoIterator for BitBoard {
    type Item = Square;
    type IntoIter = SquareIter;
    fn into_iter(self) -> Self::IntoIter {
        SquareIter(self.0)
    }
}

impl BitOr for BitBoard { 
    type Output = Self; 
    fn bitor(self, rhs: Self)->Self { 
        BitBoard(self.0 | rhs.0) 
    } 
}

impl BitAnd for BitBoard {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        BitBoard(self.0 & rhs.0)
    }
}

impl BitXor for BitBoard {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        BitBoard(self.0 ^ rhs.0)
    }
}

impl Not for BitBoard {
    type Output = Self;
    fn not(self) -> Self::Output {
        BitBoard(!self.0)
    }
}

impl Shl<u32> for BitBoard {
    type Output = Self;
    fn shl(self, rhs: u32) -> Self::Output {
        BitBoard(self.0 << rhs)
    }
}

impl Shr<u32> for BitBoard {
    type Output = Self;
    fn shr(self, rhs: u32) -> Self::Output {
        BitBoard(self.0 >> rhs)
    }
}

impl BitAnd<u64> for BitBoard {
    type Output = BitBoard;
    fn bitand(self, rhs: u64) -> Self::Output { 
        BitBoard(self.0 & rhs) 
    }
}

impl BitOr<u64> for BitBoard {
    type Output = BitBoard;
    fn bitor(self, rhs: u64) -> Self::Output { 
        BitBoard(self.0 | rhs) 
    }
}

impl BitXor<u64> for BitBoard {
    type Output = BitBoard;
    fn bitxor(self, rhs: u64) -> Self::Output { 
        BitBoard(self.0 ^ rhs) 
    }
}

impl BitOrAssign for BitBoard {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl PartialEq<u64> for BitBoard {
    fn eq(&self, other: &u64) -> bool {
        self.0 == *other
    }
}

impl PartialEq<BitBoard> for u64 {
    fn eq(&self, other: &BitBoard) -> bool {
        *self == other.0
    }
}

impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Checkerboard colors (lichess-like)
        let light = (240, 217, 181);
        let dark  = (181, 136,  99);

        // Highlight marker (bright, high-contrast)
        let mark_fg = (220, 0, 0); // red
        let marker  = "●";         // U+25CF (not an emoji)

        // Print ranks 8 -> 1, files a -> h
        for rank in (0..8).rev() {
            write!(f, "{} ", (rank + 1).to_string().red())?;

            for file in 0..8 {
                // Bit 0 = a1, bit 63 = h8; adjust if your mapping differs
                let idx  = rank * 8 + file;
                let mask = 1u64 << idx;

                let (r, g, b) = if (rank + file) % 2 == 0 { light } else { dark };

                if (self.0 & mask) != 0 {
                    // Active square: keep the checkerboard background, draw a bold red dot on top
                    let cell = format!(" {} ", marker)
                        .bold()
                        .truecolor(mark_fg.0, mark_fg.1, mark_fg.2)
                        .on_truecolor(r, g, b);
                    write!(f, "{}", cell)?;
                } else {
                    // Empty square: just the background
                    let cell = "   ".on_truecolor(r, g, b);
                    write!(f, "{}", cell)?;
                }
            }
            writeln!(f)?;
        }

        writeln!(f, "   {}", " a  b  c  d  e  f  g  h".red())
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct SquareIter(u64);


impl Iterator for SquareIter {
    type Item = Square;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let b = self.0;
        if b == 0 {
            return None;
        }
        let idx = b.trailing_zeros() as u8;
        self.0 = b & (b - 1);
        Some(Square(idx))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.0.count_ones() as usize))
    }
}

impl DoubleEndedIterator for SquareIter {
    #[inline]
    fn next_back(&mut self) -> Option<Square> {
        let b = self.0;
        if b == 0 {
            return None;
        }

        let idx = 63 - b.leading_zeros() as u8;
        self.0 &= !(1u64 << idx);
        Some(Square(idx))
    }
}

impl FusedIterator for SquareIter {}







// ############################
//           ATTACKING
// ############################

pub struct PieceMovement;

impl PieceMovement {
    pub const fn king_attacks_from_sq(sq: defs::Square) -> BitBoard {
        let b = 1u64 << sq.0;
        let n = b << 8;
        let s = b >> 8;
        let e = (b << 1) & !rankfile::FILE_A.0;
        let w = (b >> 1) & !rankfile::FILE_H.0;
        let ne = (b << 9) & !rankfile::FILE_A.0;
        let nw = (b << 7) & !rankfile::FILE_H.0;
        let se = (b >> 7) & !rankfile::FILE_A.0;
        let sw = (b >> 9) & !rankfile::FILE_H.0;
        BitBoard(n | s | e | w | ne | nw | se | sw)
    }

    pub fn king_attacks_from_bb(bb: BitBoard) -> BitBoard {
        let mut b = bb.0;
        let mut acc = 0u64;
        while b != 0 {
            let sq = defs::Square(b.trailing_zeros() as u8);
            acc |= KING_ATK[sq.0 as usize].0;
            b &= b - 1;
        }
        BitBoard(acc)
    }

    pub const fn knight_attacks_from_sq(sq: defs::Square) -> BitBoard {
        let b = 1u64 << sq.0;
        let n2e = (b << 17) & !rankfile::FILE_A.0;
        let n2w = (b << 15) & !rankfile::FILE_H.0;
        let e2n = (b << 10) & !rankfile::FILE_AB.0;
        let w2s = (b >> 10) & !rankfile::FILE_GH.0;
        let s2e = (b >> 15) & !rankfile::FILE_A.0;
        let s2w = (b >> 17) & !rankfile::FILE_H.0;

        // Corrected moves
        let e2s = (b >> 6) & !rankfile::FILE_AB.0; // Was `(b << 6) & !rankfile::FILE_GH.0`
        let w2n = (b << 6) & !rankfile::FILE_GH.0; // Was `(b >> 6) & !rankfile::FILE_AB.0`
        
        BitBoard(n2e | n2w | e2n | e2s | s2e | s2w | w2n | w2s)
    }

    pub fn knight_attacks_from_bb(bb: BitBoard) -> BitBoard {
        let mut b = bb.0;
        let mut acc = 0u64;
        while b != 0 {
            let sq = defs::Square(b.trailing_zeros() as u8);
            acc |= KNIGHT_ATK[sq.0 as usize].0;
            b &= b - 1;
        }
        BitBoard(acc)
    }

    pub const fn white_pawn_attacks_from_sq(sq: defs::Square) -> BitBoard {
        let b = 1u64 << sq.0;
        let nw = (b << 7) & !rankfile::FILE_H.0;
        let ne = (b << 9) & !rankfile::FILE_A.0;
        BitBoard(nw | ne)
    }

    pub const fn black_pawn_attacks_from_sq(sq: defs::Square) -> BitBoard {
        let b = 1u64 << sq.0;
        let sw = (b >> 9) & !rankfile::FILE_H.0;
        let se = (b >> 7) & !rankfile::FILE_A.0;
        BitBoard(sw | se)
    }

    pub fn white_pawn_attacks_from_bb(bb: BitBoard) -> BitBoard {
        let mut b = bb.0;
        let mut acc = 0u64;
        while b != 0 {
            let sq = defs::Square(b.trailing_zeros() as u8);
            acc |= WHITE_PAWN_ATK[sq.0 as usize].0;
            b &= b - 1;
        }
        BitBoard(acc)
    }

    pub fn black_pawn_attacks_from_bb(bb: BitBoard) -> BitBoard {
        let mut b = bb.0;
        let mut acc = 0u64;
        while b != 0 {
            let sq = defs::Square(b.trailing_zeros() as u8);
            acc |= BLACK_PAWN_ATK[sq.0 as usize].0;
            b &= b - 1;
        }
        BitBoard(acc)
    }

    #[inline(always)]
    fn rev64(x: u64) -> u64 { x.reverse_bits() }

    #[inline(always)]
    pub fn hq_line(occ: u64, bb: u64, mask: u64) -> u64 {
        let line = occ & mask;
        let left = line.wrapping_sub(bb << 1);
        let right = Self::rev64(Self::rev64(line).wrapping_sub(Self::rev64(bb) << 1));
        (left ^ right) & mask
    }

    #[inline(always)]
    pub const fn rank_mask(sq: defs::Square) -> u64 {
        0xFFu64 << ((sq.0 & 56) as u32)
    }

    #[inline(always)]
    pub const fn file_mask(sq: defs::Square) -> u64 {
        0x0101_0101_0101_0101u64 << ((sq.0 & 7) as u32)
    }

    #[inline(always)]
    pub const fn diag_mask(sq: defs::Square) -> u64 {
        let mut m = 0u64;
        let f0 = (sq.0 & 7) as i8;
        let r0 = (sq.0 >> 3) as i8;
        let (mut f1, mut r1) = (f0, r0);
        while f1 < 7 && r1 < 7 { f1 += 1; r1 += 1; m |= 1u64 << (r1 as u32 * 8 + f1 as u32); }
        let (mut f2, mut r2) = (f0, r0);
        while f2 > 0 && r2 > 0 { f2 -= 1; r2 -= 1; m |= 1u64 << (r2 as u32 * 8 + f2 as u32); }
        m | (1u64 << sq.0)
    }

    #[inline(always)]
    pub const fn anti_mask(sq: defs::Square) -> u64 {
        let mut m = 0u64;
        let f0 = (sq.0 & 7) as i8;
        let r0 = (sq.0 >> 3) as i8;
        let (mut f1, mut r1) = (f0, r0);
        while f1 > 0 && r1 < 7 { f1 -= 1; r1 += 1; m |= 1u64 << (r1 as u32 * 8 + f1 as u32); }
        let (mut f2, mut r2) = (f0, r0);
        while f2 < 7 && r2 > 0 { f2 += 1; r2 -= 1; m |= 1u64 << (r2 as u32 * 8 + f2 as u32); }
        m | (1u64 << sq.0)
    }

    pub const DIAG_MASK: [u64; 64] = {
        let mut a = [0u64; 64];
        let mut i = 0;
        while i < 64 {
            a[i] = PieceMovement::diag_mask(defs::Square(i as u8));
            i += 1;
        }
        a
    };

    pub const ANTI_MASK: [u64; 64] = {
        let mut a = [0u64; 64];
        let mut i = 0;
        while i < 64 {
            a[i] = PieceMovement::anti_mask(defs::Square(i as u8));
            i += 1;
        }
        a
    };

    pub fn rook_attacks_from_sq(sq: defs::Square, occ: BitBoard) -> BitBoard {
        magics().rook_attacks(sq, occ)
    }

    pub fn bishop_attacks_from_sq(sq: defs::Square, occ: BitBoard) -> BitBoard {
        magics().bishop_attacks(sq, occ)
    }

    pub fn rook_attacks_from_bb(bb: BitBoard, occ: BitBoard) -> BitBoard {
        let mut b = bb.0;
        let mut acc = 0u64;
        while b != 0 {
            let sq = defs::Square(b.trailing_zeros() as u8);
            acc |= Self::rook_attacks_from_sq(sq, occ).0;
            b &= b - 1;
        }
        BitBoard(acc)
    }

    pub fn bishop_attacks_from_bb(bb: BitBoard, occ: BitBoard) -> BitBoard {
        let mut b = bb.0;
        let mut acc = 0u64;
        while b != 0 {
            let sq = defs::Square(b.trailing_zeros() as u8);
            acc |= Self::bishop_attacks_from_sq(sq, occ).0;
            b &= b - 1;
        }
        BitBoard(acc)
    }

}


pub const fn build_king_table() -> [BitBoard; 64] {
    let mut tbl: [BitBoard; 64] = [BitBoard(0); 64];
    let mut i: usize = 0;
    while i < 64 {
        tbl[i] = PieceMovement::king_attacks_from_sq(defs::Square(i as u8));
        i += 1;
    }
    tbl
}

pub const fn build_knight_table() -> [BitBoard; 64] {
    let mut tbl: [BitBoard; 64] = [BitBoard(0); 64];
    let mut i: usize = 0;
    while i < 64 {
        tbl[i] = PieceMovement::knight_attacks_from_sq(defs::Square(i as u8));
        i += 1;
    }
    tbl
}

pub const fn build_white_pawn_table() -> [BitBoard; 64] {
    let mut tbl: [BitBoard; 64] = [BitBoard(0); 64];
    let mut i: usize = 0;
    while i < 64 {
        tbl[i] = PieceMovement::white_pawn_attacks_from_sq(defs::Square(i as u8));
        i += 1;
    }
    tbl
}

pub const fn build_black_pawn_table() -> [BitBoard; 64] {
    let mut tbl: [BitBoard; 64] = [BitBoard(0); 64];
    let mut i: usize = 0;
    while i < 64 {
        tbl[i] = PieceMovement::black_pawn_attacks_from_sq(defs::Square(i as u8));
        i += 1;
    }
    tbl
}

pub const KING_ATK: [BitBoard; 64] = build_king_table();
pub const KNIGHT_ATK: [BitBoard; 64] = build_knight_table();
pub const WHITE_PAWN_ATK: [BitBoard; 64] = build_white_pawn_table();
pub const BLACK_PAWN_ATK: [BitBoard; 64] = build_black_pawn_table();


// ############################
//         ATTACKED BY
// ############################



pub fn enemy_attacks(pos: &Position, them: Color, occ: BitBoard) -> BitBoard {
    let pawns = if them == Color::White {
        PieceMovement::white_pawn_attacks_from_bb(pos.pieces(them, Piece::Pawn))
    } else {
        PieceMovement::black_pawn_attacks_from_bb(pos.pieces(them, Piece::Pawn))
    };

    let knights = PieceMovement::knight_attacks_from_bb(pos.pieces(them, Piece::Knight));
    let kings   = PieceMovement::king_attacks_from_bb(pos.pieces(them, Piece::King));

    let mut bishops = BitBoard(0);
    let mut b = (pos.pieces(them, Piece::Bishop) | pos.pieces(them, Piece::Queen)).0;
    while b != 0 {
        let sq = Square(b.trailing_zeros() as u8);
        bishops |= magics().bishop_attacks(sq, occ);
        b &= b - 1;
    }

    let mut rooks = BitBoard(0);
    let mut r = (pos.pieces(them, Piece::Rook) | pos.pieces(them, Piece::Queen)).0;
    while r != 0 {
        let sq = Square(r.trailing_zeros() as u8);
        rooks |= magics().rook_attacks(sq, occ);
        r &= r - 1;
    }

    pawns | knights | bishops | rooks | kings
}




// ############################
//           POSITION
// ############################


#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct Position{
    pub bb_sides: [BitBoard; 2],
    pub bb_pieces: [[BitBoard; 6]; 2],
    pub state: gamestate::State,
}


impl Position {
    pub fn side(&self, c: defs::Color) -> BitBoard {
        self.bb_sides[c as usize]
    }

    pub fn pieces(&self, c: defs::Color, p: defs::Piece) -> BitBoard {
        self.bb_pieces[c as usize][p as usize]
    }

    pub fn occupied(&self) -> BitBoard {
        BitBoard(self.bb_sides[0].0 | self.bb_sides[1].0)
    }

    pub fn set_piece(&mut self, c: defs::Color, p: defs::Piece, sq: defs::Square) {
        let bit = 1u64 << sq.0;
        self.bb_pieces[c as usize][p as usize].0 |= bit;
        self.bb_sides[c as usize].0 |= bit;
    }

    pub fn clear_piece(&mut self, c: defs::Color, p: defs::Piece, sq: defs::Square) {
        let bit = 1u64 << sq.0;
        self.bb_pieces[c as usize][p as usize].0 &= !bit;
        self.bb_sides[c as usize].0 &= !bit;
    }
    
    pub fn piece_at(&self, sq: defs::Square) -> Option<(defs::Color, defs::Piece)> {
        let bit = 1u64 << sq.0;
        for &c in &[defs::Color::White, defs::Color::Black] {
            for &p in &[Piece::King, defs::Piece::Queen, defs::Piece::Rook, defs::Piece::Knight, defs::Piece::Bishop, defs::Piece::Pawn] {
                if (self.pieces(c, p).0 & bit) != 0 {
                    return Some((c, p));
                }
            }
        }
        None
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            bb_sides: [BitBoard(defs::StartPosition::WHITE_SIDE), BitBoard(defs::StartPosition::BLACK_SIDE)],
            bb_pieces: [
                [
                    BitBoard(defs::StartPosition::WHITE_KING),
                    BitBoard(defs::StartPosition::WHITE_QUEEN),
                    BitBoard(defs::StartPosition::WHITE_ROOKS),
                    BitBoard(defs::StartPosition::WHITE_KNIGHTS),
                    BitBoard(defs::StartPosition::WHITE_BISHOPS),
                    BitBoard(defs::StartPosition::WHITE_PAWNS)
                ],
                [
                    BitBoard(defs::StartPosition::BLACK_KING),
                    BitBoard(defs::StartPosition::BLACK_QUEEN),
                    BitBoard(defs::StartPosition::BLACK_ROOKS),
                    BitBoard(defs::StartPosition::BLACK_KNIGHTS),
                    BitBoard(defs::StartPosition::BLACK_BISHOPS),
                    BitBoard(defs::StartPosition::BLACK_PAWNS)
                ]
            ],
            state: gamestate::State::default()
        }
    }
}


impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Unicode chess symbols
        let symbols = [
            ["♔\u{FE0E}", "♕\u{FE0E}", "♖\u{FE0E}", "♘\u{FE0E}", "♗\u{FE0E}", "♙\u{FE0E}"], // white
            ["♚\u{FE0E}", "♛\u{FE0E}", "♜\u{FE0E}", "♞\u{FE0E}", "♝\u{FE0E}", "♟\u{FE0E}"], // black
        ];

        // board colors (lichess palette)
        let light = (240, 217, 181);
        let dark = (181, 136, 99);

        // loop over ranks 8 -> 1
        for rank in (0..8).rev() {
            // rank label in red
            write!(f, "{} ", (rank + 1).to_string().red())?;

            for file in 0..8 {
                let sq_index = rank * 8 + file;
                let bit: u64 = 1u64 << sq_index;
                let mut symbol = " ".to_string(); // default empty square

                for color in 0..2 {
                    for piece in 0..6 {
                        if self.bb_pieces[color][piece].0 & bit != 0 {
                            symbol = symbols[color][piece].to_string();
                            break;
                        }
                    }
                }

                // background depends on square color
                let (r, g, b) = if (rank + file) % 2 == 0 { light } else { dark };

                // render square with background and centered piece symbol
                let square = format!(" {} ", symbol)
                    .on_truecolor(r, g, b)
                    .truecolor(0, 0, 0); // black foreground for empty

                write!(f, "{}", square)?;
            }
            writeln!(f)?; // newline after each rank
        }

        // file labels in red
        writeln!(f, "   {}", " a  b  c  d  e  f  g  h".red())?;

        Ok(())
    }
}
