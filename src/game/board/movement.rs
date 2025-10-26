// src/game/board/movement.rs

use crate::game::board::BitBoard;
use crate::game::board::rankfile;
use crate::game::defs;
use crate::game::moves::{MAGICS, Magics};

#[inline]
fn magics() -> &'static Magics {
    MAGICS.get_or_init(|| Magics::new())
}

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
        let e2s = (b >> 6) & !rankfile::FILE_AB.0;
        let w2n = (b << 6) & !rankfile::FILE_GH.0;
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
    pub const fn rank_mask(sq: defs::Square) -> u64 { 0xFFu64 << ((sq.0 & 56) as u32) }
    #[inline(always)]
    pub const fn file_mask(sq: defs::Square) -> u64 { 0x0101_0101_0101_0101u64 << ((sq.0 & 7) as u32) }
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
            a[i] = Self::diag_mask(defs::Square(i as u8));
            i += 1;
        }
        a
    };

    pub const ANTI_MASK: [u64; 64] = {
        let mut a = [0u64; 64];
        let mut i = 0;
        while i < 64 {
            a[i] = Self::anti_mask(defs::Square(i as u8));
            i += 1;
        }
        a
    };

    pub fn rook_attacks_from_sq(sq: defs::Square, occ: BitBoard) -> BitBoard { magics().rook_attacks(sq, occ) }
    pub fn bishop_attacks_from_sq(sq: defs::Square, occ: BitBoard) -> BitBoard { magics().bishop_attacks(sq, occ) }

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
    while i < 64 { tbl[i] = PieceMovement::king_attacks_from_sq(defs::Square(i as u8)); i += 1; }
    tbl
}
pub const fn build_knight_table() -> [BitBoard; 64] {
    let mut tbl: [BitBoard; 64] = [BitBoard(0); 64];
    let mut i: usize = 0;
    while i < 64 { tbl[i] = PieceMovement::knight_attacks_from_sq(defs::Square(i as u8)); i += 1; }
    tbl
}
pub const fn build_white_pawn_table() -> [BitBoard; 64] {
    let mut tbl: [BitBoard; 64] = [BitBoard(0); 64];
    let mut i: usize = 0;
    while i < 64 { tbl[i] = PieceMovement::white_pawn_attacks_from_sq(defs::Square(i as u8)); i += 1; }
    tbl
}
pub const fn build_black_pawn_table() -> [BitBoard; 64] {
    let mut tbl: [BitBoard; 64] = [BitBoard(0); 64];
    let mut i: usize = 0;
    while i < 64 { tbl[i] = PieceMovement::black_pawn_attacks_from_sq(defs::Square(i as u8)); i += 1; }
    tbl
}

pub const KING_ATK: [BitBoard; 64] = build_king_table();
pub const KNIGHT_ATK: [BitBoard; 64] = build_knight_table();
pub const WHITE_PAWN_ATK: [BitBoard; 64] = build_white_pawn_table();
pub const BLACK_PAWN_ATK: [BitBoard; 64] = build_black_pawn_table();
