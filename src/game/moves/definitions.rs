use std::fmt;

use crate::game::defs;

// #############################
//           MOVE DEF
// ############################

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Move(pub(crate) u16);

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum Flag {
    QUIET           = 0b0000,
    DOUBLE_PUSH     = 0b0001,
    CASTLE_KING     = 0b0010,
    CASTLE_QUEEN    = 0b0011,
    CAPTURE         = 0b0100,
    EP_CAPTURE      = 0b0101,

    PROMO_N         = 0b1000,
    PROMO_B         = 0b1001,
    PROMO_R         = 0b1010,
    PROMO_Q         = 0b1011,
    PROMO_N_CAPTURE = 0b1100,
    PROMO_B_CAPTURE = 0b1101,
    PROMO_R_CAPTURE = 0b1110,
    PROMO_Q_CAPTURE = 0b1111,
}

impl Move {
    #[inline]
    pub const fn pack(from: defs::Square, to: defs::Square, flags: Flag) -> Self {
        Self(
              ((from.0 as u16 & 0x3F) << 10)
            | ((to.0   as u16 & 0x3F) << 4)
            | (flags   as u16 & 0x0F)
        )
    }

    #[inline] pub const fn from_sq(self) -> defs::Square { defs::Square(((self.0 >> 10) & 0x3F) as u8) }
    #[inline] pub const fn to_sq(self)   -> defs::Square { defs::Square(((self.0 >>  4) & 0x3F) as u8) }
    #[inline] pub const fn flags(self)   -> u16          { self.0 & 0x0F }
    #[inline] pub const fn is_capture(self) -> bool      { (self.0 & 0x0F) & 0b0100 != 0 }
    #[inline] pub const fn is_promo(self)   -> bool      { (self.0 & 0x0F) & 0b1000 != 0 }
    #[inline] pub const fn butterfly_index(self) -> u16  { self.0 >> 4 }
    #[inline] pub const fn as_u16(self)  -> u16          { self.0 }

    pub fn to_uci(self) -> String {
        let from = self.from_sq();
        let to = self.to_sq();
        let mut s = format!("{}{}", from, to);

        match self.flags() {
            x if x == Flag::PROMO_N as u16 || x == Flag::PROMO_N_CAPTURE as u16 => s.push('n'),
            x if x == Flag::PROMO_B as u16 || x == Flag::PROMO_B_CAPTURE as u16 => s.push('b'),
            x if x == Flag::PROMO_R as u16 || x == Flag::PROMO_R_CAPTURE as u16 => s.push('r'),
            x if x == Flag::PROMO_Q as u16 || x == Flag::PROMO_Q_CAPTURE as u16 => s.push('q'),
            _ => {}
        }

        s
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

#[derive(Clone, Copy)]
pub struct ScoredMove {
    pub mv: Move,
    pub score: f32,
}

pub struct ScoredMoveList {
    pub len: usize,
    pub items: [ScoredMove; MAX_MOVES],
}

impl ScoredMoveList {
    pub const fn new() -> Self {
        Self { len: 0, items: [ScoredMove { mv: Move(0), score: -30_000.0 }; MAX_MOVES] }
    }

    #[inline]
    #[allow(dead_code)]
    pub fn push(&mut self, m: ScoredMove) {
        debug_assert!(self.len < MAX_MOVES);
        self.items[self.len] = m;
        self.len += 1;
    }

    #[inline]
    #[allow(dead_code)]
    pub fn delete(&mut self) {
        self.items[self.len] = ScoredMove { mv: Move(0), score: -30_000.0 };
        self.len -= 1;
    }

    #[allow(dead_code)]
    pub fn iter(&self) -> impl Iterator<Item = &ScoredMove> {
        self.items[..self.len].iter()
    }
}

pub const MAX_MOVES: usize = 256;

#[derive(Clone)]
pub struct MoveList {
    pub len: usize,
    pub items: [Move; MAX_MOVES],
}

impl MoveList {
    pub const fn new() -> Self {
        Self { len: 0, items: [Move(0); MAX_MOVES] }
    }

    #[inline]
    pub fn push(&mut self, m: Move) {
        debug_assert!(self.len < MAX_MOVES);
        self.items[self.len] = m;
        self.len += 1;
    }

    #[inline]
    #[allow(dead_code)]
    pub fn delete(&mut self) {
        self.items[self.len] = Move(0);
        self.len -= 1;
    }

    pub fn iter(&self) -> impl Iterator<Item = &Move> {
        self.items[..self.len].iter()
    }
}

