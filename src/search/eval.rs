// src/search/eval.rs
use crate::game::defs::{Color, Piece};
use crate::game::board::{Position, enemy_attacks, BitBoard};
use crate::game::board::PieceMovement;
use crate::game::history::Undo;
use crate::game::moves::Flag;

pub type Score = i32; // centipawns

pub const QUEEN_VALUE:  Score = 900;
pub const ROOK_VALUE:   Score = 500;
pub const BISHOP_VALUE: Score = 325;
pub const KNIGHT_VALUE: Score = 300;
pub const PAWN_VALUE:   Score = 100;

pub const PESTO_QUEEN_VALUE_MG:  Score = 1025;
pub const PESTO_ROOK_VALUE_MG:   Score = 477;
pub const PESTO_BISHOP_VALUE_MG: Score = 365;
pub const PESTO_KNIGHT_VALUE_MG: Score = 337;
pub const PESTO_PAWN_VALUE_MG:   Score = 82;

pub const PESTO_QUEEN_VALUE_EG:  Score = 936;
pub const PESTO_ROOK_VALUE_EG:   Score = 512;
pub const PESTO_BISHOP_VALUE_EG: Score = 297;
pub const PESTO_KNIGHT_VALUE_EG: Score = 281;
pub const PESTO_PAWN_VALUE_EG:   Score = 94;

pub const PESTO_QUEEN_VALUE_MG_ALT:  Score = 1025;
pub const PESTO_ROOK_VALUE_MG_ALT:   Score = 477;
pub const PESTO_BISHOP_VALUE_MG_ALT: Score = 365;
pub const PESTO_KNIGHT_VALUE_MG_ALT: Score = 337;
pub const PESTO_PAWN_VALUE_MG_ALT:   Score = 82;

pub const PESTO_QUEEN_VALUE_EG_ALT:  Score = 936;
pub const PESTO_ROOK_VALUE_EG_ALT:   Score = 512;
pub const PESTO_BISHOP_VALUE_EG_ALT: Score = 297;
pub const PESTO_KNIGHT_VALUE_EG_ALT: Score = 281;
pub const PESTO_PAWN_VALUE_EG_ALT:   Score = 94;

#[inline]
const fn neg_i16(a: [i16; 64]) -> [i16; 64] {
    let mut out = [0i16; 64];
    let mut i = 0usize;
    while i < 64 {
        out[i] = -a[i];
        i += 1;
    }
    out
}

const KING_BASE_MG_REV:   [i16; 64] = neg_i16(flip_ranks_i16(KING_BASE_MG));
const KING_BASE_EG_REV:   [i16; 64] = neg_i16(flip_ranks_i16(KING_BASE_EG));
const PAWN_BASE_MG_REV:   [i16; 64] = neg_i16(flip_ranks_i16(PAWN_BASE_MG));
const PAWN_BASE_EG_REV:   [i16; 64] = neg_i16(flip_ranks_i16(PAWN_BASE_EG));
const ROOK_BASE_MG_REV:   [i16; 64] = neg_i16(flip_ranks_i16(ROOK_BASE_MG));
const ROOK_BASE_EG_REV:   [i16; 64] = neg_i16(flip_ranks_i16(ROOK_BASE_EG));
const QUEEN_BASE_MG_REV:  [i16; 64] = neg_i16(flip_ranks_i16(QUEEN_BASE_MG));
const QUEEN_BASE_EG_REV:  [i16; 64] = neg_i16(flip_ranks_i16(QUEEN_BASE_EG));
const BISHOP_BASE_MG_REV: [i16; 64] = neg_i16(flip_ranks_i16(BISHOP_BASE_MG));
const BISHOP_BASE_EG_REV: [i16; 64] = neg_i16(flip_ranks_i16(BISHOP_BASE_EG));
const KNIGHT_BASE_MG_REV: [i16; 64] = neg_i16(flip_ranks_i16(KNIGHT_BASE_MG));
const KNIGHT_BASE_EG_REV: [i16; 64] = neg_i16(flip_ranks_i16(KNIGHT_BASE_EG));

#[allow(dead_code)]
const KING_BASE_MG_REV_ALT:   [i16; 64] = neg_i16(flip_ranks_i16(KING_BASE_MG_ALT));
const KING_BASE_EG_REV_ALT:   [i16; 64] = neg_i16(flip_ranks_i16(KING_BASE_EG_ALT));
const PAWN_BASE_MG_REV_ALT:   [i16; 64] = neg_i16(flip_ranks_i16(PAWN_BASE_MG_ALT));
const PAWN_BASE_EG_REV_ALT:   [i16; 64] = neg_i16(flip_ranks_i16(PAWN_BASE_EG_ALT));
const ROOK_BASE_MG_REV_ALT:   [i16; 64] = neg_i16(flip_ranks_i16(ROOK_BASE_MG_ALT));
const ROOK_BASE_EG_REV_ALT:   [i16; 64] = neg_i16(flip_ranks_i16(ROOK_BASE_EG_ALT));
const QUEEN_BASE_MG_REV_ALT:  [i16; 64] = neg_i16(flip_ranks_i16(QUEEN_BASE_MG_ALT));
const QUEEN_BASE_EG_REV_ALT:  [i16; 64] = neg_i16(flip_ranks_i16(QUEEN_BASE_EG_ALT));
const BISHOP_BASE_MG_REV_ALT: [i16; 64] = neg_i16(flip_ranks_i16(BISHOP_BASE_MG_ALT));
const BISHOP_BASE_EG_REV_ALT: [i16; 64] = neg_i16(flip_ranks_i16(BISHOP_BASE_EG_ALT));
const KNIGHT_BASE_MG_REV_ALT: [i16; 64] = neg_i16(flip_ranks_i16(KNIGHT_BASE_MG_ALT));
const KNIGHT_BASE_EG_REV_ALT: [i16; 64] = neg_i16(flip_ranks_i16(KNIGHT_BASE_EG_ALT));


// ---------- Utilities ----------

#[inline]
pub const fn pst_score(bb: BitBoard, values: &[i16; 64]) -> i32 {
    let mut b = bb.0;
    let mut acc: i32 = 0;
    while b != 0 {
        let idx = b.trailing_zeros() as usize;
        acc += values[idx] as i32;
        b &= b - 1;
    }
    acc
}

#[inline]
pub const fn flip_ranks_i16(a: [i16; 64]) -> [i16; 64] {
    let mut out = [0i16; 64];
    let mut i = 0usize;
    while i < 64 {
        let file = i & 7;
        let rank = i >> 3;
        let src  = ((7 - rank) << 3) | file;
        out[i] = a[src];
        i += 1;
    }
    out
}

// ---------- Piece–Square Tables (compile-time flipped, no runtime work) ----------

const SAFETY_TABLE: [i32; 100] = [
      0,   0,   1,   2,   3,   5,   7,   9,  12,  15,
     18,  22,  26,  30,  35,  39,  44,  50,  56,  62,
     68,  75,  82,  89,  97, 105, 113, 122, 131, 140,
    150, 160, 170, 180, 190, 200, 210, 220, 230, 240,
    250, 260, 270, 280, 290, 300, 310, 320, 330, 340,
    350, 360, 370, 380, 390, 400, 410, 420, 430, 440,
    450, 460, 470, 480, 490, 500, 510, 520, 530, 540,
    550, 560, 570, 580, 590, 600, 610, 620, 630, 640,
    650, 660, 670, 680, 690, 700, 710, 720, 730, 740,
    750, 760, 770, 780, 790, 800, 810, 820, 830, 840,
];



const WHITE_PAWN_BASE: [i16; 64] = [
    15, 15, 15, 15, 15, 15, 15, 15,
    12, 12, 12, 12, 15, 15, 15, 15,
     5, 10, 10, 10, 15, 15, 15,  5,
     2,  2, 10, 10, 15, 15,  2,  2,
     4,  0,  2, 10, 15,  0,  0,  1,
     2,  0,  4,  4,  4,  0,  0,  2,
     5,  5,  4,  0,  0,  5,  5,  5,
     0,  0,  0,  0,  0,  0,  0,  0,
];

const WHITE_KING_BASE: [i16; 64] = [
     0,   0,   0,   0,   0,   0,   0,   0,
    -5,  -5,  -4,   0,   0,  -5,  -5,  -5,
    -2,   0,  -4,  -4,  -4,   0,   0,  -2,
    -4,   0,  -2, -10, -15,   0,   0,  -1,
    -2,  -2, -10, -10, -15, -15,  -2,  -2,
    -5, -10, -10, -10, -15, -15, -15,  -5,
   -12, -12, -12, -12, -15, -15, -15, -15,
   -15, -15, -15, -15, -15, -15, -15, -15,
];

const BLACK_PAWN_BASE: [i16; 64] = [
     0,   0,   0,   0,   0,   0,   0,   0,
    -5,  -5,  -4,   0,   0,  -5,  -5,  -5,
    -2,   0,  -4,  -4,  -4,   0,   0,  -2,
    -4,   0,  -2, -20, -25,   0,   0,  -1,
    -2,  -2,  -2,  -2,  -2,  -2,  -2,  -2,
    -5,  -5,  -5,  -5,  -5,  -5,  -5,  -5,
    -9,  -9,  -9,  -9,  -9,  -9,  -9,  -9,
   -15, -15, -15, -15, -15, -15, -15, -15,
];

const BLACK_KING_BASE: [i16; 64] = [
    -10, -12,  -3,   0,   0,  -5, -15, -12,
      0,   0,   5,  10,  10,   5,   0,   0,
     10,  10,  10,  10,  10,  10,  10,  10,
     10,  10,  10,  10,  10,  10,  10,  10,
     10,  10,  10,  10,  10,  10,  10,  10,
     10,  10,  10,  10,  10,  10,  10,  10,
     10,  10,  10,  10,  10,  10,  10,  10,
     10,  10,  10,  10,  10,  10,  10,  10,
];

// ----------- PeSTO -----------------------------------------

const PAWN_BASE_MG_RAW: [i16; 64] = [
    0,   0,   0,   0,   0,   0,  0,   0,
     98, 134,  61,  95,  68, 126, 34, -11,
     -6,   7,  26,  31,  65,  56, 25, -20,
    -14,  13,   6,  21,  23,  12, 17, -23,
    -27,  -2,  -5,  12,  20,   6, 10, -25,
    -26,  -4,  -4, -10,   3,   3, 33, -12,
    -35,  -1, -20, -23, -25,  24, 38, -22,
      0,   0,   0,   0,   0,   0,  0,   0,
];

const PAWN_BASE_EG_RAW: [i16; 64] = [
     0,   0,   0,   0,   0,   0,   0,   0,
    178, 173, 158, 134, 147, 132, 165, 187,
     94, 100,  85,  67,  56,  53,  82,  84,
     32,  24,  13,   5,  -2,   4,  17,  17,
     13,   9,  -3,  -7,  -7,  -8,   3,  -1,
      4,   7,  -6,   1,   0,  -5,  -1,  -8,
     13,   8,   8,  10,  13,   0,   2,  -7,
      0,   0,   0,   0,   0,   0,   0,   0,
];

const KNIGHT_BASE_MG_RAW: [i16; 64] = [
    -167, -89, -34, -49,  61, -97, -15, -107,
     -73, -41,  72,  36,  23,  62,   7,  -17,
     -47,  60,  37,  65,  84, 129,  73,   44,
      -9,  17,  19,  53,  37,  69,  18,   22,
     -13,   4,  16,  13,  28,  19,  21,   -8,
     -23,  -9,  12,  10,  19,  17,  25,  -16,
     -29, -53, -12,  -3,  -1,  18, -14,  -19,
    -105, -21, -58, -33, -17, -28, -19,  -23,
];

const KNIGHT_BASE_EG_RAW: [i16; 64] = [
    -58, -38, -13, -28, -31, -27, -63, -99,
    -25,  -8, -25,  -2,  -9, -25, -24, -52,
    -24, -20,  10,   9,  -1,  -9, -19, -41,
    -17,   3,  22,  22,  22,  11,   8, -18,
    -18,  -6,  16,  25,  16,  17,   4, -18,
    -23,  -3,  -1,  15,  10,  -3, -20, -22,
    -42, -20, -10,  -5,  -2, -20, -23, -44,
    -29, -51, -23, -15, -22, -18, -50, -64,
];

const BISHOP_BASE_MG_RAW: [i16; 64] = [
    -167, -89, -34, -49,  61, -97, -15, -107,
     -73, -41,  72,  36,  23,  62,   7,  -17,
     -47,  60,  37,  65,  84, 129,  73,   44,
      -9,  17,  19,  53,  37,  69,  18,   22,
     -13,   4,  16,  13,  28,  19,  21,   -8,
     -23,  -9,  12,  10,  19,  17,  25,  -16,
     -29, -53, -12,  -3,  -1,  18, -14,  -19,
    -105, -21, -58, -33, -17, -28, -19,  -23,
];

const BISHOP_BASE_EG_RAW: [i16; 64] = [
    -14, -21, -11,  -8, -7,  -9, -17, -24,
     -8,  -4,   7, -12, -3, -13,  -4, -14,
      2,  -8,   0,  -1, -2,   6,   0,   4,
     -3,   9,  12,   9, 14,  10,   3,   2,
     -6,   3,  13,  19,  7,  10,  -3,  -9,
    -12,  -3,   8,  10, 13,   3,  -7, -15,
    -14, -18,  -7,  -1,  4,  -9, -15, -27,
    -23,  -9, -23,  -5, -9, -16,  -5, -17,
];

const ROOK_BASE_MG_RAW: [i16; 64] = [
    32,  42,  32,  51, 63,  9,  31,  43,
     27,  32,  58,  62, 80, 67,  26,  44,
     -5,  19,  26,  36, 17, 45,  61,  16,
    -24, -11,   7,  26, 24, 35,  -8, -20,
    -36, -26, -12,  -1,  9, -7,   6, -23,
    -45, -25, -16, -17,  3,  0,  -5, -33,
    -44, -16, -20,  -9, -1, 11,  -6, -71,
    -19, -13,   1,  17, 16,  7, -37, -26,
];

const ROOK_BASE_EG_RAW: [i16; 64] = [
    13, 10, 18, 15, 12,  12,   8,   5,
    11, 13, 13, 11, -3,   3,   8,   3,
     7,  7,  7,  5,  4,  -3,  -5,  -3,
     4,  3, 13,  1,  2,   1,  -1,   2,
     3,  5,  8,  4, -5,  -6,  -8, -11,
    -4,  0, -5, -1, -7, -12,  -8, -16,
    -6, -6,  0,  2, -9,  -9, -11,  -3,
    -9,  2,  3, -1, -5, -13,   4, -20,
];


const QUEEN_BASE_MG_RAW: [i16; 64] = [
    -28,   0,  29,  12,  59,  44,  43,  45,
    -24, -39,  -5,   1, -16,  57,  28,  54,
    -13, -17,   7,   8,  29,  56,  47,  57,
    -27, -27, -16, -16,  -1,  17,  -2,  -54,
     -9, -26,  -9, -10,  -2,  -4,   3,  -3,
    -14,   2, -11,  -2,  -5,   2,  14,   5,
    -35,  -8,  11,   2,   8,  15,  -3,   1,
     -1, -18,  -9,  10, -15, -25, -31, -50,
];

const QUEEN_BASE_EG_RAW: [i16; 64] = [
     -9,  22,  22,  27,  27,  19,  10,  20,
    -17,  20,  32,  41,  58,  25,  30,   0,
    -20,   6,   9,  49,  47,  35,  19,   9,
      3,  22,  24,  45,  57,  40,  57,  36,
    -18,  28,  19,  47,  31,  34,  39,  23,
    -16, -27,  15,   6,   9,  17,  10,   5,
    -22, -23, -30, -16, -16, -23, -36, -32,
    -33, -28, -22, -43,  -5, -32, -20, -41,
];

const KING_BASE_MG_RAW: [i16; 64] = [
    -65,  23,  16, -15, -56, -34,   2,  13,
     29,  -1, -20,  -7,  -8,  -4, -38, -29,
     -9,  24,   2, -16, -20,   6,  22, -22,
    -17, -20, -12, -27, -30, -25, -14, -36,
    -49,  -1, -27, -39, -46, -44, -33, -51,
    -14, -14, -22, -46, -44, -30, -15, -27,
      1,   7,  -8, -64, -43, -16,   9,   8,
    -15,  36,  12, -54,   8, -28,  24,  14,
];

const KING_BASE_EG_RAW: [i16; 64] = [
    -74, -35, -18, -18, -11,  15,   4, -17,
    -12,  17,  14,  17,  17,  38,  23,  11,
     10,  17,  23,  15,  20,  45,  44,  13,
     -8,  22,  24,  27,  26,  33,  26,   3,
    -18,  -4,  21,  24,  27,  23,   9, -11,
    -19,  -3,  11,  21,  23,  16,   7,  -9,
    -27, -11,   4,  13,  14,   4,  -5, -17,
    -53, -34, -21, -11, -28, -14, -24, -43
];








const PAWN_BASE_MG_RAW_ALT: [i16; 64] = [
     0,   0,   0,   0,   0,   0,   0,   0,
     32,  44,  20,  31,  22,  42,  11,  -3,
     -2,   2,   8,  10,  21,  18,   8,  -6,
     -4,   4,   2,   7,   7,   4,   5,  -7,
     -9,  -1,  -1,   4,   6,   2,   3,  -8,
     -8,  -2,  -2,  -3,   1,   1,  11,  -4,
    -11,   0,  -6,  -7,  -8,   8,  12,  -7,
      0,   0,   0,   0,   0,   0,   0,   0,
];

const PAWN_BASE_EG_RAW_ALT: [i16; 64] = [
      0,   0,   0,   0,   0,   0,   0,   0,
     59,  57,  52,  44,  49,  44,  55,  62,
     31,  33,  28,  22,  18,  17,  27,  28,
     10,   8,   4,   1,   0,   1,   5,   5,
      4,   3,  -1,  -3,  -3,  -3,   1,   0,
      1,   2,  -2,   0,   0,  -1,   0,  -2,
      4,   2,   2,   3,   4,   0,   0,  -2,
      0,   0,   0,   0,   0,   0,   0,   0,
];

const KNIGHT_BASE_MG_RAW_ALT: [i16; 64] = [
     -55, -29, -11, -16,  20, -32,  -5, -35,
     -24, -13,  24,  12,   7,  20,   2,  -5,
     -15,  20,  12,  21,  28,  43,  24,  14,
      -3,   5,   6,  17,  12,  23,   6,   7,
      -4,   1,   5,   4,   9,   6,   7,  -2,
      -7,  -3,   4,   3,   6,   5,   8,  -5,
      -9, -17,  -4,  -1,   0,   6,  -4,  -6,
     -35,  -7, -19, -11,  -5,  -9,  -6,  -7,
];

const KNIGHT_BASE_EG_RAW_ALT: [i16; 64] = [
     -19, -13,  -4,  -9, -10,  -9, -21, -33,
      -8,  -2,  -8,   0,  -3,  -8,  -8, -17,
      -8,  -6,   3,   3,   0,  -3,  -6, -13,
      -5,   1,   7,   7,   7,   3,   2,  -6,
      -6,  -2,   5,   8,   5,   5,   1,  -6,
      -7,  -1,   0,   5,   3,  -1,  -6,  -7,
     -14,  -6,  -3,  -1,   0,  -6,  -7, -14,
      -9, -17,  -7,  -5,  -7,  -6, -16, -21,
];

const BISHOP_BASE_MG_RAW_ALT: [i16; 64] = [
     -55, -29, -11, -16,  20, -32,  -5, -35,
     -24, -13,  24,  12,   7,  20,   2,  -5,
     -15,  20,  12,  21,  28,  43,  24,  14,
      -3,   5,   6,  17,  12,  23,   6,   7,
      -4,   1,   5,   4,   9,   6,   7,  -2,
      -7,  -3,   4,   3,   6,   5,   8,  -5,
      -9, -17,  -4,  -1,   0,   6,  -4,  -6,
     -35,  -7, -19, -11,  -5,  -9,  -6,  -7,
];

const BISHOP_BASE_EG_RAW_ALT: [i16; 64] = [
      -4,  -7,  -3,  -2,  -2,  -3,  -5,  -8,
      -2,  -2,   2,  -4,  -1,  -4,  -1,  -4,
       0,  -2,   0,   0,  -1,   2,   0,   1,
      -1,   3,   4,   3,   4,   3,   1,   0,
      -2,   1,   4,   6,   2,   3,  -1,  -3,
      -4,  -1,   2,   3,   4,   1,  -2,  -5,
      -4,  -6,  -2,   0,   1,  -3,  -5,  -9,
      -7,  -3,  -7,  -1,  -3,  -5,  -1,  -5,
];

const ROOK_BASE_MG_RAW_ALT: [i16; 64] = [
     10,  14,  10,  17,  21,   3,  10,  14,
      9,  10,  19,  20,  26,  22,   8,  14,
     -1,   6,   8,  12,   5,  15,  20,   5,
     -8,  -3,   2,   8,   8,  11,  -2,  -6,
    -12,  -8,  -4,   0,   3,  -3,   2,  -7,
    -15,  -8,  -5,  -5,   1,   0,  -1, -11,
    -14,  -5,  -6,  -3,   0,   3,  -2, -23,
     -6,  -4,   0,   5,   5,   2, -12,  -8,
];

const ROOK_BASE_EG_RAW_ALT: [i16; 64] = [
      4,   3,   6,   5,   4,   4,   2,   1,
      3,   4,   4,   3,  -1,   1,   2,   1,
      2,   2,   2,   1,   1,  -1,  -1,  -1,
      1,   1,   4,   0,   0,   0,   0,   0,
      1,   1,   2,   1,  -1,  -2,  -2,  -3,
     -1,   0,  -1,   0,  -2,  -4,  -2,  -5,
     -2,  -2,   0,   0,  -3,  -3,  -3,  -1,
     -3,   0,   1,   0,  -1,  -4,   1,  -6,
];

const QUEEN_BASE_MG_RAW_ALT: [i16; 64] = [
     -9,   0,   9,   4,  19,  14,  14,  15,
      -8, -13,  -1,   0,  -5,  19,   9,  18,
      -4,  -5,   2,   2,   9,  18,  15,  19,
      -9,  -9,  -5,  -5,   0,   5,   0, -18,
      -3,  -8,  -3,  -3,   0,  -1,   1,  -1,
      -4,   0,  -3,   0,  -1,   0,   4,   1,
     -11,  -2,   3,   0,   2,   5,  -1,   0,
      0,  -6,  -3,   3,  -5,  -8, -10, -16,
];

const QUEEN_BASE_EG_RAW_ALT: [i16; 64] = [
      -3,   7,   7,   9,   9,   6,   3,   6,
      -5,   6,  10,  13,  19,   8,  10,   0,
      -6,   2,   3,  16,  15,  11,   6,   3,
       1,   7,   8,  15,  19,  13,  19,  12,
      -6,   9,   6,  15,  10,  11,  13,   7,
      -6,  -9,   5,   2,   3,   5,   3,   1,
      -7,  -7, -10,  -5,  -5,  -7, -12, -10,
     -11,  -9,  -7, -14,  -1, -10,  -6, -13,
];

const KING_BASE_MG_RAW_ALT: [i16; 64] = [
     -21,   7,   5,  -5, -18, -12,   0,   4,
       9,   0,  -6,  -2,  -2,  -2, -12,  -9,
      -3,   8,   0,  -5,  -6,   2,   7,  -7,
      -5,  -6,  -4,  -9, -10,  -8,  -4, -12,
     -16,   0,  -9, -13, -15, -14, -11, -17,
      -4,  -4,  -7, -15, -14, -10,  -5,  -9,
       0,   2,  -2, -21, -14,  -5,   3,   2,
      -5,  12,   4, -18,   2,  -9,   8,   4,
];

const KING_BASE_EG_RAW_ALT: [i16; 64] = [
     -24, -12,  -6,  -6,  -3,   5,   1,  -5,
      -4,   5,   4,   5,   5,  12,   7,   3,
       3,   5,   7,   5,   6,  15,  14,   4,
      -2,   7,   8,   9,   8,  11,   8,   1,
      -6,  -1,   7,   8,   9,   7,   3,  -3,
      -6,  -1,   3,   7,   7,   5,   2,  -3,
      -9,  -3,   1,   4,   4,   1,  -1,  -5,
     -17, -11,  -7,  -3,  -9,  -5,  -8, -14,
];



// Final PSTs used by eval (compile-time flipped).
pub const WHITE_PAWN_PST:  [i16; 64] = flip_ranks_i16(WHITE_PAWN_BASE);
pub const WHITE_KING_PST:  [i16; 64] = flip_ranks_i16(WHITE_KING_BASE);
pub const BLACK_PAWN_PST:  [i16; 64] = flip_ranks_i16(BLACK_PAWN_BASE);
pub const BLACK_KING_PST:  [i16; 64] = flip_ranks_i16(BLACK_KING_BASE);

pub const PAWN_BASE_MG:    [i16; 64] = flip_ranks_i16(PAWN_BASE_MG_RAW);
pub const KING_BASE_MG:    [i16; 64] = flip_ranks_i16(KING_BASE_MG_RAW);
pub const QUEEN_BASE_MG:   [i16; 64] = flip_ranks_i16(QUEEN_BASE_MG_RAW);
pub const KNIGHT_BASE_MG:  [i16; 64] = flip_ranks_i16(KNIGHT_BASE_MG_RAW);
pub const BISHOP_BASE_MG:  [i16; 64] = flip_ranks_i16(BISHOP_BASE_MG_RAW);
pub const ROOK_BASE_MG:    [i16; 64] = flip_ranks_i16(ROOK_BASE_MG_RAW);
pub const PAWN_BASE_EG:    [i16; 64] = flip_ranks_i16(PAWN_BASE_EG_RAW);
pub const KING_BASE_EG:    [i16; 64] = flip_ranks_i16(KING_BASE_EG_RAW);
pub const QUEEN_BASE_EG:   [i16; 64] = flip_ranks_i16(QUEEN_BASE_EG_RAW);
pub const KNIGHT_BASE_EG:  [i16; 64] = flip_ranks_i16(KNIGHT_BASE_EG_RAW);
pub const BISHOP_BASE_EG:  [i16; 64] = flip_ranks_i16(BISHOP_BASE_EG_RAW);
pub const ROOK_BASE_EG:    [i16; 64] = flip_ranks_i16(ROOK_BASE_EG_RAW);


pub const PAWN_BASE_MG_ALT:    [i16; 64] = flip_ranks_i16(PAWN_BASE_MG_RAW_ALT);
pub const KING_BASE_MG_ALT:    [i16; 64] = flip_ranks_i16(KING_BASE_MG_RAW_ALT);
pub const QUEEN_BASE_MG_ALT:   [i16; 64] = flip_ranks_i16(QUEEN_BASE_MG_RAW_ALT);
pub const KNIGHT_BASE_MG_ALT:  [i16; 64] = flip_ranks_i16(KNIGHT_BASE_MG_RAW_ALT);
pub const BISHOP_BASE_MG_ALT:  [i16; 64] = flip_ranks_i16(BISHOP_BASE_MG_RAW_ALT);
pub const ROOK_BASE_MG_ALT:    [i16; 64] = flip_ranks_i16(ROOK_BASE_MG_RAW_ALT);
pub const PAWN_BASE_EG_ALT:    [i16; 64] = flip_ranks_i16(PAWN_BASE_EG_RAW_ALT);
pub const KING_BASE_EG_ALT:    [i16; 64] = flip_ranks_i16(KING_BASE_EG_RAW_ALT);
pub const QUEEN_BASE_EG_ALT:   [i16; 64] = flip_ranks_i16(QUEEN_BASE_EG_RAW_ALT);
pub const KNIGHT_BASE_EG_ALT:  [i16; 64] = flip_ranks_i16(KNIGHT_BASE_EG_RAW_ALT);
pub const BISHOP_BASE_EG_ALT:  [i16; 64] = flip_ranks_i16(BISHOP_BASE_EG_RAW_ALT);
pub const ROOK_BASE_EG_ALT:    [i16; 64] = flip_ranks_i16(ROOK_BASE_EG_RAW_ALT);

const fn file_of(s: usize) -> usize { s & 7 }
const fn rank_of(s: usize) -> usize { s >> 3 }

const fn white_mask_for(s: usize) -> u64 {
    let rank = rank_of(s);
    let file = file_of(s);
    let lf = if file == 0 { 0 } else { file - 1 };
    let hf = if file == 7 { 7 } else { file + 1 };

    let mut mask: u64 = 0;
    let mut r = rank + 1;            // strictly ahead
    while r < 8 {
        let base = r << 3;
        let mut f = lf;
        while f <= hf {
            mask |= 1u64 << (base + f);
            f += 1;
        }
        r += 1;
    }
    mask
}

const fn black_mask_for(s: usize) -> u64 {
    let rank = rank_of(s);
    let file = file_of(s);
    let lf = if file == 0 { 0 } else { file - 1 };
    let hf = if file == 7 { 7 } else { file + 1 };

    let mut mask: u64 = 0;
    let mut r = 0usize;              // strictly ahead toward rank 0
    while r < rank {
        let base = r << 3;
        let mut f = lf;
        while f <= hf {
            mask |= 1u64 << (base + f);
            f += 1;
        }
        r += 1;
    }
    mask
}

const fn build_white_masks() -> [u64; 64] {
    let mut out = [0u64; 64];
    let mut s = 0usize;
    while s < 64 {
        out[s] = white_mask_for(s);
        s += 1;
    }
    out
}

const fn build_black_masks() -> [u64; 64] {
    let mut out = [0u64; 64];
    let mut s = 0usize;
    while s < 64 {
        out[s] = black_mask_for(s);
        s += 1;
    }
    out
}

/// Chebyshev (king-move) neighborhood mask around square `s`.
/// `radius = 1` => up to 3x3 (includes `s`)
/// `radius = 2` => up to 5x5 (includes radius-1)
// --- fix the mask generator (elsewhere in the file) ---
const fn king_neighborhood_mask_for(s: usize, radius: usize) -> u64 {
    let sr = rank_of(s) as isize;
    let sf = file_of(s) as isize;
    let rmin = if sr > radius as isize { sr - radius as isize } else { 0 };
    let rmax = if sr + radius as isize <= 7 { sr + radius as isize } else { 7 }; // < 8, not <= 8
    let fmin = if sf > radius as isize { sf - radius as isize } else { 0 };
    let fmax = if sf + radius as isize <= 7 { sf + radius as isize } else { 7 }; // < 8, not <= 8

    let mut mask: u64 = 0;
    let mut r = rmin;
    while r <= rmax {
        let base = (r as usize) << 3;
        let mut f = fmin;
        while f <= fmax {
            mask |= 1u64 << (base + f as usize);
            f += 1;
        }
        r += 1;
    }
    mask
}


const fn build_king_masks(radius: usize) -> [u64; 64] {
    let mut out = [0u64; 64];
    let mut s = 0usize;
    while s < 64 {
        out[s] = king_neighborhood_mask_for(s, radius);
        s += 1;
    }
    out
}

// Public masks (identical geometry for both colors; exported under both names)
pub const MASK_1: [u64; 64] = build_king_masks(1);
pub const MASK_2: [u64; 64] = build_king_masks(2);

// --- Temporary compatibility re-exports (remove when you switch callers) ---
#[allow(deprecated)]
pub const WHITE_PASSED_PAWN_MASK: [u64; 64] = build_white_masks();

#[allow(deprecated)]
pub const BLACK_PASSED_PAWN_MASK: [u64; 64] = build_black_masks();






// ---------- Simple evals ----------

#[allow(dead_code)]
pub fn material_eval(pos: &Position) -> Score {
    let w_pawns   = pos.pieces(Color::White, Piece::Pawn).0.count_ones() as Score;
    let w_bishops = pos.pieces(Color::White, Piece::Bishop).0.count_ones() as Score;
    let w_knights = pos.pieces(Color::White, Piece::Knight).0.count_ones() as Score;
    let w_rooks   = pos.pieces(Color::White, Piece::Rook).0.count_ones() as Score;
    let w_queens  = pos.pieces(Color::White, Piece::Queen).0.count_ones() as Score;

    let b_pawns   = pos.pieces(Color::Black, Piece::Pawn).0.count_ones() as Score;
    let b_bishops = pos.pieces(Color::Black, Piece::Bishop).0.count_ones() as Score;
    let b_knights = pos.pieces(Color::Black, Piece::Knight).0.count_ones() as Score;
    let b_rooks   = pos.pieces(Color::Black, Piece::Rook).0.count_ones() as Score;
    let b_queens  = pos.pieces(Color::Black, Piece::Queen).0.count_ones() as Score;

    let white = w_pawns * PAWN_VALUE
        + w_bishops * BISHOP_VALUE
        + w_knights * KNIGHT_VALUE
        + w_rooks * ROOK_VALUE
        + w_queens * QUEEN_VALUE;

    let black = b_pawns * PAWN_VALUE
        + b_bishops * BISHOP_VALUE
        + b_knights * KNIGHT_VALUE
        + b_rooks * ROOK_VALUE
        + b_queens * QUEEN_VALUE;

    white - black
}

#[allow(dead_code)]
pub fn material_and_attack_eval(pos: &Position) -> Score {
    let material_score = material_eval(pos);

    // Attack score (very lightweight)
    let occ = pos.occupied();
    let wa = enemy_attacks(pos, Color::White, occ).0.count_ones() as i32;
    let ba = enemy_attacks(pos, Color::Black, occ).0.count_ones() as i32;
    let attack_score = (wa - ba) * 3;

    material_score + attack_score
}

// ---------- Main eval used by your search, with PSTs but no per-node setup ----------

#[allow(dead_code)]
pub fn material_attack_position_eval(pos: &Position) -> Score {
    // Material (bitboard counts only)
    let w_pawns_bb = pos.pieces(Color::White, Piece::Pawn);
    let w_king_bb  = pos.pieces(Color::White, Piece::King);
    let w_queen_bb = pos.pieces(Color::White, Piece::Queen);
    let w_rook_bb  = pos.pieces(Color::White, Piece::Rook);

    let b_pawns_bb = pos.pieces(Color::Black, Piece::Pawn);
    let b_king_bb  = pos.pieces(Color::Black, Piece::King);
    let b_queen_bb = pos.pieces(Color::Black, Piece::Queen);
    let b_rook_bb  = pos.pieces(Color::Black, Piece::Rook);

    let w_pawns   = w_pawns_bb.0.count_ones() as Score;
    let w_bishops = pos.pieces(Color::White, Piece::Bishop).0.count_ones() as Score;
    let w_knights = pos.pieces(Color::White, Piece::Knight).0.count_ones() as Score;
    let w_rooks   = w_rook_bb.0.count_ones() as Score;
    let w_queens  = w_queen_bb.0.count_ones() as Score;

    let b_pawns   = b_pawns_bb.0.count_ones() as Score;
    let b_bishops = pos.pieces(Color::Black, Piece::Bishop).0.count_ones() as Score;
    let b_knights = pos.pieces(Color::Black, Piece::Knight).0.count_ones() as Score;
    let b_rooks   = b_rook_bb.0.count_ones() as Score;
    let b_queens  = b_queen_bb.0.count_ones() as Score;

    let white = w_pawns * PAWN_VALUE
        + w_bishops * BISHOP_VALUE
        + w_knights * KNIGHT_VALUE
        + w_rooks * ROOK_VALUE
        + w_queens * QUEEN_VALUE;

    let black = b_pawns * PAWN_VALUE
        + b_bishops * BISHOP_VALUE
        + b_knights * KNIGHT_VALUE
        + b_rooks * ROOK_VALUE
        + b_queens * QUEEN_VALUE;

    let material_score = white - black;

    // Attack term
    let occ = pos.occupied();
    let wa = enemy_attacks(pos, Color::White, occ).0.count_ones() as i32;
    let ba = enemy_attacks(pos, Color::Black, occ).0.count_ones() as i32;
    let attack_score = (wa - ba) * 3;

    // PSTs: pure lookups (no mutation or flipping)
    let mut wkpos = 0;
    let mut bkpos = 0;

    // Keep your “only score king PST when heavy pieces exist” condition.
    if (b_queen_bb.0 | b_rook_bb.0) != 0 {
        wkpos = pst_score(w_king_bb, &WHITE_KING_PST);
    }
    if (w_queen_bb.0 | w_rook_bb.0) != 0 {
        bkpos = pst_score(b_king_bb, &BLACK_KING_PST);
    }

    let wppos = pst_score(w_pawns_bb, &WHITE_PAWN_PST);
    let bppos = pst_score(b_pawns_bb, &BLACK_PAWN_PST);

    let position_score = wkpos + wppos + bkpos + bppos;

    material_score + attack_score + position_score
}

pub fn material_attack_pesto_eval(pos: &Position) -> Score {
    // Material (bitboard counts only)
    let w_pawns_bb = pos.pieces(Color::White, Piece::Pawn);
    let w_king_bb  = pos.pieces(Color::White, Piece::King);
    let w_queen_bb = pos.pieces(Color::White, Piece::Queen);
    let w_rook_bb  = pos.pieces(Color::White, Piece::Rook);
    let w_bish_bb  = pos.pieces(Color::White, Piece::Bishop);
    let w_knig_bb  = pos.pieces(Color::White, Piece::Knight);

    let b_pawns_bb = pos.pieces(Color::Black, Piece::Pawn);
    let b_king_bb  = pos.pieces(Color::Black, Piece::King);
    let b_queen_bb = pos.pieces(Color::Black, Piece::Queen);
    let b_rook_bb  = pos.pieces(Color::Black, Piece::Rook);
    let b_bish_bb  = pos.pieces(Color::Black, Piece::Bishop);
    let b_knig_bb  = pos.pieces(Color::Black, Piece::Knight);

    let w_pawns   = w_pawns_bb.0.count_ones() as Score;
    let w_bishops = w_bish_bb.0.count_ones() as Score;
    let w_knights = w_knig_bb.0.count_ones() as Score;
    let w_rooks   = w_rook_bb.0.count_ones() as Score;
    let w_queens  = w_queen_bb.0.count_ones() as Score;

    let b_pawns   = b_pawns_bb.0.count_ones() as Score;
    let b_bishops = b_bish_bb.0.count_ones() as Score;
    let b_knights = b_knig_bb.0.count_ones() as Score;
    let b_rooks   = b_rook_bb.0.count_ones() as Score;
    let b_queens  = b_queen_bb.0.count_ones() as Score;

    let endgame = if (w_queens == 0 && b_queens == 0)
        || (w_queens + w_rooks + b_queens + b_rooks < 4)
    {
        true
    } else {
        false
    };

    let white = w_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + w_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + w_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + w_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + w_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };

    let black = b_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + b_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + b_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + b_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + b_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };

    let material_score = white - black;

    // Attack term
    let occ = pos.occupied();
    let wa = enemy_attacks(pos, Color::White, occ).0.count_ones() as i32;
    let ba = enemy_attacks(pos, Color::Black, occ).0.count_ones() as i32;
    let attack_score = (wa - ba) * 3;

    // --- PSTs ---
    let (wkpos, wppos, wrpos, wqpos, wbpos, wnpos);
    let (bkpos, bppos, brpos, bqpos, bbpos, bnpos);

    if endgame {
        // White (EG)
        wkpos = pst_score(w_king_bb,  &KING_BASE_EG);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_EG);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_EG);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_EG);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_EG);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_EG);

        // Black (EG) — mirrored + negated via pt_reverse
        bkpos = pst_score(b_king_bb,  &KING_BASE_EG_REV);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_EG_REV);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_EG_REV);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_EG_REV);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_EG_REV);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_EG_REV);
    } else {
        // White (MG)
        wkpos = pst_score(w_king_bb,  &KING_BASE_MG);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_MG);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_MG);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_MG);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_MG);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_MG);

        // Black (MG) — mirrored + negated via pt_reverse
        bkpos = pst_score(b_king_bb,  &KING_BASE_MG_REV);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_MG_REV);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_MG_REV);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_MG_REV);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_MG_REV);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_MG_REV);
    }

    let wpos = wkpos + wppos + wrpos + wqpos + wbpos + wnpos;
    let bpos = bkpos + bppos + brpos + bqpos + bbpos + bnpos;
    let position_score = wpos + bpos;

    material_score + attack_score + position_score
}

pub fn alternate_pesto(pos: &Position) -> Score {
    let w_queen_bb = pos.pieces(Color::White, Piece::Queen);
    let w_rook_bb  = pos.pieces(Color::White, Piece::Rook);

    let b_queen_bb = pos.pieces(Color::Black, Piece::Queen);
    let b_rook_bb  = pos.pieces(Color::Black, Piece::Rook);

    let w_queens  = w_queen_bb.0.count_ones() as Score;
    let w_rooks: i32   = w_rook_bb.0.count_ones() as Score;
    let b_queens  = b_queen_bb.0.count_ones() as Score;
    let b_rooks: i32   = b_rook_bb.0.count_ones() as Score;

    let endgame = if (w_queens == 0 && b_queens == 0)
        || (w_queens + w_rooks + b_queens + b_rooks < 4 || pos.state.ply_counter > 60)
    {
        true
    } else {
        false
    };

    if pos.state.ply_counter < 6 || endgame {
        material_attack_pesto_eval(pos)
    } else {
        material_attack_position_eval(pos)
    }
}


// ###############################################################################################
// #######################    FEATURE EVAL   #####################################################
// ###############################################################################################


pub fn map_pawn_info_eval(pos: &Position) -> Score {
    // Material (bitboard counts only)
    let w_pawns_bb = pos.pieces(Color::White, Piece::Pawn);
    let w_king_bb  = pos.pieces(Color::White, Piece::King);
    let w_queen_bb = pos.pieces(Color::White, Piece::Queen);
    let w_rook_bb  = pos.pieces(Color::White, Piece::Rook);
    let w_bish_bb  = pos.pieces(Color::White, Piece::Bishop);
    let w_knig_bb  = pos.pieces(Color::White, Piece::Knight);

    let b_pawns_bb = pos.pieces(Color::Black, Piece::Pawn);
    let b_king_bb  = pos.pieces(Color::Black, Piece::King);
    let b_queen_bb = pos.pieces(Color::Black, Piece::Queen);
    let b_rook_bb  = pos.pieces(Color::Black, Piece::Rook);
    let b_bish_bb  = pos.pieces(Color::Black, Piece::Bishop);
    let b_knig_bb  = pos.pieces(Color::Black, Piece::Knight);

    let w_pawns   = w_pawns_bb.0.count_ones() as Score;
    let w_bishops = w_bish_bb.0.count_ones() as Score;
    let w_knights = w_knig_bb.0.count_ones() as Score;
    let w_rooks   = w_rook_bb.0.count_ones() as Score;
    let w_queens  = w_queen_bb.0.count_ones() as Score;

    let b_pawns   = b_pawns_bb.0.count_ones() as Score;
    let b_bishops = b_bish_bb.0.count_ones() as Score;
    let b_knights = b_knig_bb.0.count_ones() as Score;
    let b_rooks   = b_rook_bb.0.count_ones() as Score;
    let b_queens  = b_queen_bb.0.count_ones() as Score;

    let endgame = if (w_queens == 0 && b_queens == 0)
        || (w_queens + w_rooks + b_queens + b_rooks < 4)
    {
        true
    } else {
        false
    };

    let white = w_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + w_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + w_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + w_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + w_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };

    let black = b_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + b_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + b_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + b_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + b_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };

    let material_score = white - black;

    // Attack term
    let occ = pos.occupied();
    let wa = enemy_attacks(pos, Color::White, occ).0.count_ones() as i32;
    let ba = enemy_attacks(pos, Color::Black, occ).0.count_ones() as i32;
    let attack_score = (wa - ba) * 3;

    // --- PSTs ---
    let (wkpos, wppos, wrpos, wqpos, wbpos, wnpos);
    let (bkpos, bppos, brpos, bqpos, bbpos, bnpos);

    if endgame {
        wkpos = pst_score(w_king_bb,  &KING_BASE_EG);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_EG);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_EG);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_EG);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_EG);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_EG);

        bkpos = pst_score(b_king_bb,  &KING_BASE_EG_REV);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_EG_REV);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_EG_REV);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_EG_REV);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_EG_REV);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_EG_REV);
    } else {
        wkpos = pst_score(w_king_bb,  &KING_BASE_MG);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_MG);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_MG);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_MG);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_MG);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_MG);

        bkpos = pst_score(b_king_bb,  &KING_BASE_MG_REV);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_MG_REV);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_MG_REV);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_MG_REV);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_MG_REV);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_MG_REV);
    }

    let mg_pass_tbl: [i32; 7] = [0, 4, 8, 12, 18, 26, 36];
    let eg_pass_tbl: [i32; 7] = [0, 6, 12, 20, 30, 44, 60];

    let occ = pos.occupied().0;
    let w_pawns_bits = w_pawns_bb.0;
    let b_pawns_bits = b_pawns_bb.0;

    let mut white_pawn_structure: i32 = 0;
    let mut black_pawn_structure: i32 = 0;

    for sq in w_pawns_bb.iter_squares() {
        let idx = sq.0 as usize;
        if (WHITE_PASSED_PAWN_MASK[idx] & b_pawns_bits) == 0 {
            let rank = sq.get_rank() as usize; // 0..7
            let rank_for_table = rank.min(6);
            let base = if endgame { eg_pass_tbl[rank_for_table] } else { mg_pass_tbl[rank_for_table] };

            let scale = if rank < 7 {
                let stop_idx = idx + 8;
                if (occ & (1u64 << stop_idx)) != 0 { 0.25f32 } else { 1.0f32 }
            } else {
                1.0f32
            };

            white_pawn_structure += (base as f32 * scale) as i32;
        }
    }

    for sq in b_pawns_bb.iter_squares() {
        let idx = sq.0 as usize;
        if (BLACK_PASSED_PAWN_MASK[idx] & w_pawns_bits) == 0 {
            let rank = sq.get_rank() as usize;
            let dist = (7 - rank).min(6);
            let base = if endgame { eg_pass_tbl[dist] } else { mg_pass_tbl[dist] };

            let scale = if rank > 0 {
                let stop_idx = idx - 8;
                if (occ & (1u64 << stop_idx)) != 0 { 0.25f32 } else { 1.0f32 }
            } else {
                1.0f32
            };

            black_pawn_structure -= (base as f32 * scale) as i32;
        }
    }

    let file_mask_base: u64 = 0x0101010101010101u64;
    let doubled_penalty = 15;
    for file in 0..8 {
        let file_mask = file_mask_base << file;
        let w_cnt = (w_pawns_bits & file_mask).count_ones() as i32;
        if w_cnt > 1 {
            white_pawn_structure -= doubled_penalty * (w_cnt - 1);
        }
        let b_cnt = (b_pawns_bits & file_mask).count_ones() as i32;
        if b_cnt > 1 {
            black_pawn_structure += doubled_penalty * (b_cnt - 1);
        }
    }

        

    let wpos = wkpos + wppos + wrpos + wqpos + wbpos + wnpos + white_pawn_structure;
    let bpos = bkpos + bppos + brpos + bqpos + bbpos + bnpos + black_pawn_structure;
    let position_score = wpos + bpos;

    material_score + attack_score + position_score
}


pub fn map_double_pawn_eval(pos: &Position) -> Score {
    // Material (bitboard counts only)
    let w_pawns_bb = pos.pieces(Color::White, Piece::Pawn);
    let w_king_bb  = pos.pieces(Color::White, Piece::King);
    let w_queen_bb = pos.pieces(Color::White, Piece::Queen);
    let w_rook_bb  = pos.pieces(Color::White, Piece::Rook);
    let w_bish_bb  = pos.pieces(Color::White, Piece::Bishop);
    let w_knig_bb  = pos.pieces(Color::White, Piece::Knight);

    let b_pawns_bb = pos.pieces(Color::Black, Piece::Pawn);
    let b_king_bb  = pos.pieces(Color::Black, Piece::King);
    let b_queen_bb = pos.pieces(Color::Black, Piece::Queen);
    let b_rook_bb  = pos.pieces(Color::Black, Piece::Rook);
    let b_bish_bb  = pos.pieces(Color::Black, Piece::Bishop);
    let b_knig_bb  = pos.pieces(Color::Black, Piece::Knight);

    let w_pawns   = w_pawns_bb.0.count_ones() as Score;
    let w_bishops = w_bish_bb.0.count_ones() as Score;
    let w_knights = w_knig_bb.0.count_ones() as Score;
    let w_rooks   = w_rook_bb.0.count_ones() as Score;
    let w_queens  = w_queen_bb.0.count_ones() as Score;

    let b_pawns   = b_pawns_bb.0.count_ones() as Score;
    let b_bishops = b_bish_bb.0.count_ones() as Score;
    let b_knights = b_knig_bb.0.count_ones() as Score;
    let b_rooks   = b_rook_bb.0.count_ones() as Score;
    let b_queens  = b_queen_bb.0.count_ones() as Score;

    let endgame = if (w_queens == 0 && b_queens == 0)
        || (w_queens + w_rooks + b_queens + b_rooks < 4)
    {
        true
    } else {
        false
    };

    let white = w_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + w_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + w_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + w_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + w_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };

    let black = b_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + b_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + b_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + b_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + b_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };

    let material_score = white - black;

    // Attack term
    let occ = pos.occupied();
    let wa = enemy_attacks(pos, Color::White, occ).0.count_ones() as i32;
    let ba = enemy_attacks(pos, Color::Black, occ).0.count_ones() as i32;
    let attack_score = (wa - ba) * 3;

    // --- PSTs ---
    let (wkpos, wppos, wrpos, wqpos, wbpos, wnpos);
    let (bkpos, bppos, brpos, bqpos, bbpos, bnpos);

    if endgame {
        wkpos = pst_score(w_king_bb,  &KING_BASE_EG);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_EG);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_EG);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_EG);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_EG);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_EG);

        bkpos = pst_score(b_king_bb,  &KING_BASE_EG_REV);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_EG_REV);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_EG_REV);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_EG_REV);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_EG_REV);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_EG_REV);
    } else {
        wkpos = pst_score(w_king_bb,  &KING_BASE_MG);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_MG);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_MG);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_MG);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_MG);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_MG);

        bkpos = pst_score(b_king_bb,  &KING_BASE_MG_REV);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_MG_REV);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_MG_REV);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_MG_REV);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_MG_REV);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_MG_REV);
    }

    let w_pawns_bits = w_pawns_bb.0;
    let b_pawns_bits = b_pawns_bb.0;

    let mut white_pawn_structure: i32 = 0;
    let mut black_pawn_structure: i32 = 0;

    let file_mask_base: u64 = 0x0101010101010101u64;
    let doubled_penalty = 15;
    for file in 0..8 {
        let file_mask = file_mask_base << file;
        let w_cnt = (w_pawns_bits & file_mask).count_ones() as i32;
        if w_cnt > 1 {
            white_pawn_structure -= doubled_penalty * (w_cnt - 1);
        }
        let b_cnt = (b_pawns_bits & file_mask).count_ones() as i32;
        if b_cnt > 1 {
            black_pawn_structure += doubled_penalty * (b_cnt - 1);
        }
    }

    let wpos = wkpos + wppos + wrpos + wqpos + wbpos + wnpos + white_pawn_structure;
    let bpos = bkpos + bppos + brpos + bqpos + bbpos + bnpos + black_pawn_structure;
    let position_score = wpos + bpos;

    material_score + attack_score + position_score
}



pub fn map_king_safety(pos: &Position) -> Score {
    // === Phase 1: Standard Evaluation (Material, PSTs, etc.) ===

    let w_pawns_bb = pos.pieces(Color::White, Piece::Pawn);
    let w_king_bb  = pos.pieces(Color::White, Piece::King);
    let w_queen_bb = pos.pieces(Color::White, Piece::Queen);
    let w_rook_bb  = pos.pieces(Color::White, Piece::Rook);
    let w_bish_bb  = pos.pieces(Color::White, Piece::Bishop);
    let w_knig_bb  = pos.pieces(Color::White, Piece::Knight);

    let b_pawns_bb = pos.pieces(Color::Black, Piece::Pawn);
    let b_king_bb  = pos.pieces(Color::Black, Piece::King);
    let b_queen_bb = pos.pieces(Color::Black, Piece::Queen);
    let b_rook_bb  = pos.pieces(Color::Black, Piece::Rook);
    let b_bish_bb  = pos.pieces(Color::Black, Piece::Bishop);
    let b_knig_bb  = pos.pieces(Color::Black, Piece::Knight);

    let w_pawns   = w_pawns_bb.0.count_ones() as Score;
    let w_bishops = w_bish_bb.0.count_ones() as Score;
    let w_knights = w_knig_bb.0.count_ones() as Score;
    let w_rooks   = w_rook_bb.0.count_ones() as Score;
    let w_queens  = w_queen_bb.0.count_ones() as Score;

    let b_pawns   = b_pawns_bb.0.count_ones() as Score;
    let b_bishops = b_bish_bb.0.count_ones() as Score;
    let b_knights = b_knig_bb.0.count_ones() as Score;
    let b_rooks   = b_rook_bb.0.count_ones() as Score;
    let b_queens  = b_queen_bb.0.count_ones() as Score;

    let endgame = (w_queens == 0 && b_queens == 0) || (w_rooks + b_rooks < 2 && w_queens + b_queens <= 1);

    let white_material = w_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + w_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + w_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + w_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + w_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };

    let black_material = b_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + b_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + b_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + b_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + b_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };
    let material_score = white_material - black_material;

    // Restore the general attack term to its original, effective value.
    let occ = pos.occupied();
    let wa = enemy_attacks(pos, Color::White, occ).0.count_ones() as i32;
    let ba = enemy_attacks(pos, Color::Black, occ).0.count_ones() as i32;
    let attack_score = (wa - ba) * 3;

    // Standard PST calculation...
    let (wkpos, wppos, wrpos, wqpos, wbpos, wnpos);
    let (bkpos, bppos, brpos, bqpos, bbpos, bnpos);

    if endgame {
        wkpos = pst_score(w_king_bb,  &KING_BASE_EG); wppos = pst_score(w_pawns_bb, &PAWN_BASE_EG); wrpos = pst_score(w_rook_bb,  &ROOK_BASE_EG); wqpos = pst_score(w_queen_bb, &QUEEN_BASE_EG); wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_EG); wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_EG);
        bkpos = pst_score(b_king_bb,  &KING_BASE_EG_REV); bppos = pst_score(b_pawns_bb, &PAWN_BASE_EG_REV); brpos = pst_score(b_rook_bb,  &ROOK_BASE_EG_REV); bqpos = pst_score(b_queen_bb, &QUEEN_BASE_EG_REV); bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_EG_REV); bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_EG_REV);
    } else {
        wkpos = pst_score(w_king_bb,  &KING_BASE_MG); wppos = pst_score(w_pawns_bb, &PAWN_BASE_MG); wrpos = pst_score(w_rook_bb,  &ROOK_BASE_MG); wqpos = pst_score(w_queen_bb, &QUEEN_BASE_MG); wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_MG); wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_MG);
        bkpos = pst_score(b_king_bb,  &KING_BASE_MG_REV); bppos = pst_score(b_pawns_bb, &PAWN_BASE_MG_REV); brpos = pst_score(b_rook_bb,  &ROOK_BASE_MG_REV); bqpos = pst_score(b_queen_bb, &QUEEN_BASE_MG_REV); bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_MG_REV); bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_MG_REV);
    }
    let position_score = (wkpos + wppos + wrpos + wqpos + wbpos + wnpos) + (bkpos + bppos + brpos + bqpos + bbpos + bnpos);


    // === Phase 2: New King Safety Evaluation ===
    let mut king_safety_score = 0;

    // Only run this expensive evaluation if there are major pieces on the board.
    if !endgame {
        let wk_sq = w_king_bb.0.trailing_zeros() as usize;
        let bk_sq = b_king_bb.0.trailing_zeros() as usize;

        // Define a "danger zone" around each king.
        let white_king_zone = MASK_2[wk_sq];
        let black_king_zone = MASK_2[bk_sq];

        // --- Calculate attack power directed at each king's zone ---
        let mut white_king_attack_power = 0;
        let mut black_king_attack_power = 0;
        
        // Attacker weights
        const QUEEN_ATTACK_WEIGHT:  i32 = 10;
        const ROOK_ATTACK_WEIGHT:   i32 = 5;
        const BISHOP_ATTACK_WEIGHT: i32 = 3;
        const KNIGHT_ATTACK_WEIGHT: i32 = 3;

        // Use the 1-ring instead of a 5x5
        let white_ring = MASK_1[wk_sq];
        let black_ring = MASK_1[bk_sq];

        let occ_bb = occ; // already a BitBoard

        // Count *pieces* that attack the ring (not squares).
        fn attackers_on_ring_rooks(bb: BitBoard, occ: BitBoard, ring: BitBoard) -> i32 {
            let mut c = 0;
            for sq in bb.iter_squares() {
                let src = BitBoard(1u64 << sq.0);
                if (PieceMovement::rook_attacks_from_bb(src, occ) & ring).0 != 0 { c += 1; }
            }
            c
        }
        fn attackers_on_ring_bishops(bb: BitBoard, occ: BitBoard, ring: BitBoard) -> i32 {
            let mut c = 0;
            for sq in bb.iter_squares() {
                let src = BitBoard(1u64 << sq.0);
                if (PieceMovement::bishop_attacks_from_bb(src, occ) & ring).0 != 0 { c += 1; }
            }
            c
        }
        fn attackers_on_ring_knights(bb: BitBoard, ring: BitBoard) -> i32 {
            let mut c = 0;
            for sq in bb.iter_squares() {
                let src = BitBoard(1u64 << sq.0);
                if (PieceMovement::knight_attacks_from_bb(src) & ring).0 != 0 { c += 1; }
            }
            c
        }
        fn attackers_on_ring_queens(bb: BitBoard, occ: BitBoard, ring: BitBoard) -> i32 {
            let mut c = 0;
            for sq in bb.iter_squares() {
                let src = BitBoard(1u64 << sq.0);
                let qa = (PieceMovement::rook_attacks_from_bb(src, occ) |
                        PieceMovement::bishop_attacks_from_bb(src, occ)) & ring;
                if qa.0 != 0 { c += 1; }
            }
            c
        }

        // Piece weights are per *attacker*, not per square.
        const QW: i32 = 20;
        const RW: i32 = 12;
        const BW: i32 = 8;
        const NW: i32 = 8;

        // Black → White king
        let wring_bb = BitBoard(white_ring);
        let white_power =
            attackers_on_ring_queens(b_queen_bb, occ_bb, wring_bb) * QW
            + attackers_on_ring_rooks (b_rook_bb,  occ_bb, wring_bb) * RW
            + attackers_on_ring_bishops(b_bish_bb, occ_bb, wring_bb) * BW
            + attackers_on_ring_knights(b_knig_bb,            wring_bb) * NW;

        // White → Black king
        let bring_bb = BitBoard(black_ring);
        let black_power =
            attackers_on_ring_queens(w_queen_bb, occ_bb, bring_bb) * QW
            + attackers_on_ring_rooks (w_rook_bb,  occ_bb, bring_bb) * RW
            + attackers_on_ring_bishops(w_bish_bb, occ_bb, bring_bb) * BW
            + attackers_on_ring_knights(w_knig_bb,            bring_bb) * NW;

        // Look up, but scale down to keep this < a minor piece almost always
        let white_king_danger = SAFETY_TABLE[white_power.clamp(0, 99) as usize] / 2; // cap ≈ 420 cp
        let black_king_danger = SAFETY_TABLE[black_power.clamp(0, 99) as usize] / 2;

        // Pawn shield: direct CP, not part of "power"
        let file_mask_base: u64 = 0x0101010101010101;
        let mut white_shield_penalty = 0;
        let mut black_shield_penalty = 0;

        let wk_file = (wk_sq & 7) as i32;
        let bk_file = (bk_sq & 7) as i32;
        for df in -1..=1 {
            let f = wk_file + df;
            if (0..=7).contains(&f) {
                let m = file_mask_base << f;
                if (w_pawns_bb.0 & m) == 0 { white_shield_penalty += if (b_pawns_bb.0 & m) == 0 { 12 } else { 6 }; }
            }
            let g = bk_file + df;
            if (0..=7).contains(&g) {
                let m = file_mask_base << g;
                if (b_pawns_bb.0 & m) == 0 { black_shield_penalty += if (w_pawns_bb.0 & m) == 0 { 12 } else { 6 }; }
            }
        }

        // Orient the sign the same way as your other terms
        king_safety_score = (black_king_danger - white_king_danger)
                            + (black_shield_penalty - white_shield_penalty);
    }

    // === Final Sum ===
    material_score + attack_score + position_score + king_safety_score
}

#[inline]
pub fn map_king_safety_optimized(pos: &Position) -> Score {
    // ===== Material (PeSTO values) + phase =====
    let w_pawns_bb = pos.pieces(Color::White, Piece::Pawn);
    let w_king_bb  = pos.pieces(Color::White, Piece::King);
    let w_queen_bb = pos.pieces(Color::White, Piece::Queen);
    let w_rook_bb  = pos.pieces(Color::White, Piece::Rook);
    let w_bish_bb  = pos.pieces(Color::White, Piece::Bishop);
    let w_knig_bb  = pos.pieces(Color::White, Piece::Knight);

    let b_pawns_bb = pos.pieces(Color::Black, Piece::Pawn);
    let b_king_bb  = pos.pieces(Color::Black, Piece::King);
    let b_queen_bb = pos.pieces(Color::Black, Piece::Queen);
    let b_rook_bb  = pos.pieces(Color::Black, Piece::Rook);
    let b_bish_bb  = pos.pieces(Color::Black, Piece::Bishop);
    let b_knig_bb  = pos.pieces(Color::Black, Piece::Knight);

    let w_pawns   = w_pawns_bb.0.count_ones() as Score;
    let w_bishops = w_bish_bb.0.count_ones() as Score;
    let w_knights = w_knig_bb.0.count_ones() as Score;
    let w_rooks   = w_rook_bb.0.count_ones() as Score;
    let w_queens  = w_queen_bb.0.count_ones() as Score;

    let b_pawns   = b_pawns_bb.0.count_ones() as Score;
    let b_bishops = b_bish_bb.0.count_ones() as Score;
    let b_knights = b_knig_bb.0.count_ones() as Score;
    let b_rooks   = b_rook_bb.0.count_ones() as Score;
    let b_queens  = b_queen_bb.0.count_ones() as Score;

    // Same endgame notion you used in map_king_safety
    let endgame = (w_queens == 0 && b_queens == 0)
        || (w_rooks + b_rooks < 2 && w_queens + b_queens <= 1);

    let white_material = w_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + w_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + w_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + w_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + w_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };

    let black_material = b_pawns   * if endgame { PESTO_PAWN_VALUE_EG }   else { PESTO_PAWN_VALUE_MG }
        + b_bishops * if endgame { PESTO_BISHOP_VALUE_EG } else { PESTO_BISHOP_VALUE_MG }
        + b_knights * if endgame { PESTO_KNIGHT_VALUE_EG } else { PESTO_KNIGHT_VALUE_MG }
        + b_rooks   * if endgame { PESTO_ROOK_VALUE_EG }   else { PESTO_ROOK_VALUE_MG }
        + b_queens  * if endgame { PESTO_QUEEN_VALUE_EG }  else { PESTO_QUEEN_VALUE_MG };

    let material_score = white_material - black_material;

    // ===== PSTs (unchanged semantics) =====
    let (wkpos, wppos, wrpos, wqpos, wbpos, wnpos);
    let (bkpos, bppos, brpos, bqpos, bbpos, bnpos);
    if endgame {
        wkpos = pst_score(w_king_bb,  &KING_BASE_EG);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_EG);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_EG);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_EG);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_EG);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_EG);

        bkpos = pst_score(b_king_bb,  &KING_BASE_EG_REV);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_EG_REV);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_EG_REV);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_EG_REV);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_EG_REV);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_EG_REV);
    } else {
        wkpos = pst_score(w_king_bb,  &KING_BASE_MG);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_MG);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_MG);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_MG);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_MG);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_MG);

        bkpos = pst_score(b_king_bb,  &KING_BASE_MG_REV);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_MG_REV);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_MG_REV);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_MG_REV);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_MG_REV);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_MG_REV);
    }
    let position_score = (wkpos + wppos + wrpos + wqpos + wbpos + wnpos)
                       + (bkpos + bppos + brpos + bqpos + bbpos + bnpos);

    // ===== Fast king safety (ring → outward; no global enemy_attacks) =====
    let mut king_safety_score = 0;
    if !endgame {
        // King ring (radius 1)
        let wk_sq = w_king_bb.0.trailing_zeros() as usize;
        let bk_sq = b_king_bb.0.trailing_zeros() as usize;
        let wring  = BitBoard(MASK_1[wk_sq]);
        let bring  = BitBoard(MASK_1[bk_sq]);
        let occ_bb = pos.occupied();

        // Precompute “reach from ring” once per king
        let w_ro_reach = PieceMovement::rook_attacks_from_bb(wring, occ_bb);
        let w_bi_reach = PieceMovement::bishop_attacks_from_bb(wring, occ_bb);
        let b_ro_reach = PieceMovement::rook_attacks_from_bb(bring, occ_bb);
        let b_bi_reach = PieceMovement::bishop_attacks_from_bb(bring, occ_bb);

        // Count enemy pieces that attack the ring by intersecting those reaches
        // (rook-like and bishop-like share queens; count queens via union).
        let white_ro_att = (w_ro_reach & b_rook_bb).0.count_ones() as i32;
        let white_bi_att = (w_bi_reach & b_bish_bb).0.count_ones() as i32;
        let white_q_att  = ((w_ro_reach | w_bi_reach) & b_queen_bb).0.count_ones() as i32;
        let white_kn_att = (PieceMovement::knight_attacks_from_bb(wring) & b_knig_bb).0.count_ones() as i32;

        let black_ro_att = (b_ro_reach & w_rook_bb).0.count_ones() as i32;
        let black_bi_att = (b_bi_reach & w_bish_bb).0.count_ones() as i32;
        let black_q_att  = ((b_ro_reach | b_bi_reach) & w_queen_bb).0.count_ones() as i32;
        let black_kn_att = (PieceMovement::knight_attacks_from_bb(bring) & w_knig_bb).0.count_ones() as i32;

        // Same per-attacker weights you used conceptually, tuned for speed
        const QW: i32 = 20;
        const RW: i32 = 12;
        const BW: i32 =  8;
        const NW: i32 =  8;

        let white_power = white_q_att * QW + white_ro_att * RW + white_bi_att * BW + white_kn_att * NW;
        let black_power = black_q_att * QW + black_ro_att * RW + black_bi_att * BW + black_kn_att * NW;

        // Convert to danger (kept capped and halved like before)
        let white_danger = SAFETY_TABLE[white_power.clamp(0, 99) as usize] / 2;
        let black_danger = SAFETY_TABLE[black_power.clamp(0, 99) as usize] / 2;

        // Simple pawn shield around king’s file (integer-only; same idea as before)
        let file_mask_base: u64 = 0x0101_0101_0101_0101;
        let wk_file = (wk_sq & 7) as i32;
        let bk_file = (bk_sq & 7) as i32;

        let mut white_shield_penalty = 0;
        let mut black_shield_penalty = 0;

        for df in -1..=1 {
            let wf = wk_file + df;
            if (0..=7).contains(&wf) {
                let m = file_mask_base << wf;
                let have_w_pawn = (w_pawns_bb.0 & m) != 0;
                if !have_w_pawn {
                    white_shield_penalty += if (b_pawns_bb.0 & m) == 0 { 12 } else { 6 };
                }
            }
            let bf = bk_file + df;
            if (0..=7).contains(&bf) {
                let m = file_mask_base << bf;
                let have_b_pawn = (b_pawns_bb.0 & m) != 0;
                if !have_b_pawn {
                    black_shield_penalty += if (w_pawns_bb.0 & m) == 0 { 12 } else { 6 };
                }
            }
        }

        // Side-to-white sign convention
        king_safety_score = (black_danger - white_danger) + (black_shield_penalty - white_shield_penalty);
    }

    // ===== No global enemy_attacks(): we drop the old (wa - ba) * 3 term on purpose. =====
    material_score + position_score + king_safety_score
}


pub fn map_double_pawn_eval_alt(pos: &Position) -> Score {
    // Material (bitboard counts only)
    let w_pawns_bb = pos.pieces(Color::White, Piece::Pawn);
    let w_king_bb  = pos.pieces(Color::White, Piece::King);
    let w_queen_bb = pos.pieces(Color::White, Piece::Queen);
    let w_rook_bb  = pos.pieces(Color::White, Piece::Rook);
    let w_bish_bb  = pos.pieces(Color::White, Piece::Bishop);
    let w_knig_bb  = pos.pieces(Color::White, Piece::Knight);

    let b_pawns_bb = pos.pieces(Color::Black, Piece::Pawn);
    let b_king_bb  = pos.pieces(Color::Black, Piece::King);
    let b_queen_bb = pos.pieces(Color::Black, Piece::Queen);
    let b_rook_bb  = pos.pieces(Color::Black, Piece::Rook);
    let b_bish_bb  = pos.pieces(Color::Black, Piece::Bishop);
    let b_knig_bb  = pos.pieces(Color::Black, Piece::Knight);

    let w_pawns   = w_pawns_bb.0.count_ones() as Score;
    let w_bishops = w_bish_bb.0.count_ones() as Score;
    let w_knights = w_knig_bb.0.count_ones() as Score;
    let w_rooks   = w_rook_bb.0.count_ones() as Score;
    let w_queens  = w_queen_bb.0.count_ones() as Score;

    let b_pawns   = b_pawns_bb.0.count_ones() as Score;
    let b_bishops = b_bish_bb.0.count_ones() as Score;
    let b_knights = b_knig_bb.0.count_ones() as Score;
    let b_rooks   = b_rook_bb.0.count_ones() as Score;
    let b_queens  = b_queen_bb.0.count_ones() as Score;

    let endgame = if (w_queens == 0 && b_queens == 0)
        || (w_queens + w_rooks + b_queens + b_rooks < 4)
    {
        true
    } else {
        false
    };

    let white = w_pawns   * if endgame { PESTO_PAWN_VALUE_EG_ALT }   else { PESTO_PAWN_VALUE_MG_ALT }
        + w_bishops * if endgame { PESTO_BISHOP_VALUE_EG_ALT } else { PESTO_BISHOP_VALUE_MG_ALT }
        + w_knights * if endgame { PESTO_KNIGHT_VALUE_EG_ALT } else { PESTO_KNIGHT_VALUE_MG_ALT }
        + w_rooks   * if endgame { PESTO_ROOK_VALUE_EG_ALT }   else { PESTO_ROOK_VALUE_MG_ALT }
        + w_queens  * if endgame { PESTO_QUEEN_VALUE_EG_ALT }  else { PESTO_QUEEN_VALUE_MG_ALT };

    let black = b_pawns   * if endgame { PESTO_PAWN_VALUE_EG_ALT }   else { PESTO_PAWN_VALUE_MG_ALT }
        + b_bishops * if endgame { PESTO_BISHOP_VALUE_EG_ALT } else { PESTO_BISHOP_VALUE_MG_ALT }
        + b_knights * if endgame { PESTO_KNIGHT_VALUE_EG_ALT } else { PESTO_KNIGHT_VALUE_MG_ALT }
        + b_rooks   * if endgame { PESTO_ROOK_VALUE_EG_ALT }   else { PESTO_ROOK_VALUE_MG_ALT }
        + b_queens  * if endgame { PESTO_QUEEN_VALUE_EG_ALT }  else { PESTO_QUEEN_VALUE_MG_ALT };

    let material_score = white - black;

    // Attack term
    let occ = pos.occupied();
    let wa = enemy_attacks(pos, Color::White, occ).0.count_ones() as i32;
    let ba = enemy_attacks(pos, Color::Black, occ).0.count_ones() as i32;
    let attack_score = (wa - ba) * 3;

    // --- PSTs ---
    let (wkpos, wppos, wrpos, wqpos, wbpos, wnpos);
    let (bkpos, bppos, brpos, bqpos, bbpos, bnpos);

    if endgame {
        wkpos = pst_score(w_king_bb,  &KING_BASE_EG_ALT);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_EG_ALT);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_EG_ALT);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_EG_ALT);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_EG_ALT);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_EG_ALT);

        bkpos = pst_score(b_king_bb,  &KING_BASE_EG_REV_ALT);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_EG_REV_ALT);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_EG_REV_ALT);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_EG_REV_ALT);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_EG_REV_ALT);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_EG_REV_ALT);
    } else {
        wkpos = pst_score(w_king_bb,  &KING_BASE_MG_ALT);
        wppos = pst_score(w_pawns_bb, &PAWN_BASE_MG_ALT);
        wrpos = pst_score(w_rook_bb,  &ROOK_BASE_MG_ALT);
        wqpos = pst_score(w_queen_bb, &QUEEN_BASE_MG_ALT);
        wbpos = pst_score(w_bish_bb,  &BISHOP_BASE_MG_ALT);
        wnpos = pst_score(w_knig_bb,  &KNIGHT_BASE_MG_ALT);

        bkpos = pst_score(b_king_bb,  &KING_BASE_MG_REV_ALT);
        bppos = pst_score(b_pawns_bb, &PAWN_BASE_MG_REV_ALT);
        brpos = pst_score(b_rook_bb,  &ROOK_BASE_MG_REV_ALT);
        bqpos = pst_score(b_queen_bb, &QUEEN_BASE_MG_REV_ALT);
        bbpos = pst_score(b_bish_bb,  &BISHOP_BASE_MG_REV_ALT);
        bnpos = pst_score(b_knig_bb,  &KNIGHT_BASE_MG_REV_ALT);
    }

    let w_pawns_bits = w_pawns_bb.0;
    let b_pawns_bits = b_pawns_bb.0;

    let mut white_pawn_structure: i32 = 0;
    let mut black_pawn_structure: i32 = 0;

    let file_mask_base: u64 = 0x0101010101010101u64;
    let doubled_penalty = 15;
    for file in 0..8 {
        let file_mask = file_mask_base << file;
        let w_cnt = (w_pawns_bits & file_mask).count_ones() as i32;
        if w_cnt > 1 {
            white_pawn_structure -= doubled_penalty * (w_cnt - 1);
        }
        let b_cnt = (b_pawns_bits & file_mask).count_ones() as i32;
        if b_cnt > 1 {
            black_pawn_structure += doubled_penalty * (b_cnt - 1);
        }
    }

    let wpos = wkpos + wppos + wrpos + wqpos + wbpos + wnpos + white_pawn_structure;
    let bpos = bkpos + bppos + brpos + bqpos + bbpos + bnpos + black_pawn_structure;
    let position_score = wpos + bpos;

    material_score + attack_score + position_score
}




