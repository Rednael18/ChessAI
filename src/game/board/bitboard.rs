// src/game/board/bitboard.rs

use core::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr, BitOrAssign};
use std::iter::FusedIterator;
use std::fmt;
use colored::*;

use crate::game::defs::{self, Square};
use crate::game::board::rankfile;

#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct BitBoard(pub u64);

impl BitBoard {
    pub fn north(self) -> Self { self << 8 }
    pub fn south(self) -> Self { self >> 8 }
    pub fn east(self) -> Self { (self << 1) & !rankfile::FILE_H }
    pub fn west(self) -> Self { (self >> 1) & !rankfile::FILE_A }

    pub const fn from_square(sq: defs::Square) -> Self { Self(1 << sq.0) }

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
    pub fn iter_squares(&self) -> SquareIter { SquareIter(self.0) }
    #[inline]
    pub fn into_iter(self) -> SquareIter { SquareIter(self.0) }
}

impl IntoIterator for BitBoard {
    type Item = Square;
    type IntoIter = SquareIter;
    fn into_iter(self) -> Self::IntoIter { SquareIter(self.0) }
}

impl BitOr for BitBoard { type Output = Self; fn bitor(self, rhs: Self)->Self { BitBoard(self.0 | rhs.0) } }
impl BitAnd for BitBoard { type Output = Self; fn bitand(self, rhs: Self)->Self { BitBoard(self.0 & rhs.0) } }
impl BitXor for BitBoard { type Output = Self; fn bitxor(self, rhs: Self)->Self { BitBoard(self.0 ^ rhs.0) } }
impl Not for BitBoard { type Output = Self; fn not(self)->Self { BitBoard(!self.0) } }
impl Shl<u32> for BitBoard { type Output = Self; fn shl(self, rhs:u32)->Self{ BitBoard(self.0 << rhs) } }
impl Shr<u32> for BitBoard { type Output = Self; fn shr(self, rhs:u32)->Self{ BitBoard(self.0 >> rhs) } }
impl BitAnd<u64> for BitBoard { type Output = BitBoard; fn bitand(self, rhs:u64)->Self{ BitBoard(self.0 & rhs) } }
impl BitOr<u64>  for BitBoard { type Output = BitBoard; fn bitor (self, rhs:u64)->Self{ BitBoard(self.0 | rhs) } }
impl BitXor<u64> for BitBoard { type Output = BitBoard; fn bitxor(self, rhs:u64)->Self{ BitBoard(self.0 ^ rhs) } }

impl BitOrAssign for BitBoard { #[inline] fn bitor_assign(&mut self, rhs: Self) { self.0 |= rhs.0; } }

impl PartialEq<u64> for BitBoard { fn eq(&self, other: &u64) -> bool { self.0 == *other } }
impl PartialEq<BitBoard> for u64 { fn eq(&self, other: &BitBoard) -> bool { *self == other.0 } }

impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let light = (240, 217, 181);
        let dark  = (181, 136,  99);
        let mark_fg = (220, 0, 0);
        let marker  = "●";
        for rank in (0..8).rev() {
            write!(f, "{} ", (rank + 1).to_string().red())?;
            for file in 0..8 {
                let idx  = rank * 8 + file;
                let mask = 1u64 << idx;
                let (r, g, b) = if (rank + file) % 2 == 0 { light } else { dark };
                if (self.0 & mask) != 0 {
                    let cell = format!(" {} ", marker)
                        .bold()
                        .truecolor(mark_fg.0, mark_fg.1, mark_fg.2)
                        .on_truecolor(r, g, b);
                    write!(f, "{}", cell)?;
                } else {
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
        if b == 0 { return None; }
        let idx = b.trailing_zeros() as u8;
        self.0 = b & (b - 1);
        Some(Square(idx))
    }
    fn size_hint(&self) -> (usize, Option<usize>) { (0, Some(self.0.count_ones() as usize)) }
}

impl DoubleEndedIterator for SquareIter {
    #[inline]
    fn next_back(&mut self) -> Option<Square> {
        let b = self.0;
        if b == 0 { return None; }
        let idx = 63 - b.leading_zeros() as u8;
        self.0 &= !(1u64 << idx);
        Some(Square(idx))
    }
}

impl FusedIterator for SquareIter {}

