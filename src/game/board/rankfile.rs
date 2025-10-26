// src/game/board/rankfile.rs

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

