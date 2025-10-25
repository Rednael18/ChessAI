// src/game/defs.rs

use std::fmt;
use std::ops::Not;



// ############################
//           SQUARES
// ############################

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// Represents a single square on the board.
/// # Representation
/// 0 is A1
/// 1 is B1
/// 63 is H8
/// 
pub struct Square(pub u8);
impl Square {
    #[inline]
    pub const fn from_file_rank(file: u8, rank: u8) -> Self {
        Self(rank*8 + file)
    }

    pub const fn get_file(self) -> u8 {
        self.0 & 7
    }

    pub const fn get_rank(self) -> u8 {
        self.0 >> 3
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let file = (b'a' + (self.0 & 7)) as char;
        let rank = ((self.0 >> 3) + 1) as u8;
        write!(f, "{}{}", file, rank)
    }
}

pub mod squarename {
    #![allow(dead_code)]
    
    use super::Square;
    pub const A1: Square = Square(0);
    pub const B1: Square = Square(1);
    pub const C1: Square = Square(2);
    pub const D1: Square = Square(3);
    pub const E1: Square = Square(4);
    pub const F1: Square = Square(5);
    pub const G1: Square = Square(6);
    pub const H1: Square = Square(7);

    pub const A2: Square = Square(8);
    pub const B2: Square = Square(9);
    pub const C2: Square = Square(10);
    pub const D2: Square = Square(11);
    pub const E2: Square = Square(12);
    pub const F2: Square = Square(13);
    pub const G2: Square = Square(14);
    pub const H2: Square = Square(15);

    pub const A3: Square = Square(16);
    pub const B3: Square = Square(17);
    pub const C3: Square = Square(18);
    pub const D3: Square = Square(19);
    pub const E3: Square = Square(20);
    pub const F3: Square = Square(21);
    pub const G3: Square = Square(22);
    pub const H3: Square = Square(23);

    pub const A4: Square = Square(24);
    pub const B4: Square = Square(25);
    pub const C4: Square = Square(26);
    pub const D4: Square = Square(27);
    pub const E4: Square = Square(28);
    pub const F4: Square = Square(29);
    pub const G4: Square = Square(30);
    pub const H4: Square = Square(31);

    pub const A5: Square = Square(32);
    pub const B5: Square = Square(33);
    pub const C5: Square = Square(34);
    pub const D5: Square = Square(35);
    pub const E5: Square = Square(36);
    pub const F5: Square = Square(37);
    pub const G5: Square = Square(38);
    pub const H5: Square = Square(39);

    pub const A6: Square = Square(40);
    pub const B6: Square = Square(41);
    pub const C6: Square = Square(42);
    pub const D6: Square = Square(43);
    pub const E6: Square = Square(44);
    pub const F6: Square = Square(45);
    pub const G6: Square = Square(46);
    pub const H6: Square = Square(47);

    pub const A7: Square = Square(48);
    pub const B7: Square = Square(49);
    pub const C7: Square = Square(50);
    pub const D7: Square = Square(51);
    pub const E7: Square = Square(52);
    pub const F7: Square = Square(53);
    pub const G7: Square = Square(54);
    pub const H7: Square = Square(55);

    pub const A8: Square = Square(56);
    pub const B8: Square = Square(57);
    pub const C8: Square = Square(58);
    pub const D8: Square = Square(59);
    pub const E8: Square = Square(60);
    pub const F8: Square = Square(61);
    pub const G8: Square = Square(62);
    pub const H8: Square = Square(63);

}

pub const fn bit(idx: u8) -> u64 {
    1u64 << (idx as u32)
}


// ############################
//      STARTING POSITIONS
// ############################

pub struct StartPosition;
impl StartPosition {
    pub const WHITE_KING: u64      = bit(squarename::E1.0);
    pub const WHITE_QUEEN: u64     = bit(squarename::D1.0);
    pub const WHITE_ROOKS: u64     = bit(squarename::A1.0)
                                   | bit(squarename::H1.0);
    pub const WHITE_KNIGHTS: u64   = bit(squarename::B1.0)
                                   | bit(squarename::G1.0);
    pub const WHITE_BISHOPS: u64   = bit(squarename::C1.0)
                                   | bit(squarename::F1.0);
    pub const WHITE_PAWNS: u64     = bit(squarename::A2.0)
                                   | bit(squarename::B2.0)
                                   | bit(squarename::C2.0)
                                   | bit(squarename::D2.0)
                                   | bit(squarename::E2.0)
                                   | bit(squarename::F2.0)
                                   | bit(squarename::G2.0)
                                   | bit(squarename::H2.0);

    pub const BLACK_KING: u64      = bit(squarename::E8.0);
    pub const BLACK_QUEEN: u64     = bit(squarename::D8.0);
    pub const BLACK_ROOKS: u64     = bit(squarename::A8.0)
                                   | bit(squarename::H8.0);
    pub const BLACK_KNIGHTS: u64   = bit(squarename::B8.0)
                                   | bit(squarename::G8.0);
    pub const BLACK_BISHOPS: u64   = bit(squarename::C8.0)
                                   | bit(squarename::F8.0);
    pub const BLACK_PAWNS: u64     = bit(squarename::A7.0)
                                   | bit(squarename::B7.0)
                                   | bit(squarename::C7.0)
                                   | bit(squarename::D7.0)
                                   | bit(squarename::E7.0)
                                   | bit(squarename::F7.0)
                                   | bit(squarename::G7.0)
                                   | bit(squarename::H7.0);

    pub const WHITE_SIDE: u64      = StartPosition::WHITE_KING    | StartPosition::WHITE_QUEEN
                                   | StartPosition::WHITE_BISHOPS | StartPosition::WHITE_KNIGHTS
                                   | StartPosition::WHITE_ROOKS   | StartPosition::WHITE_PAWNS;

    pub const BLACK_SIDE: u64      = StartPosition::BLACK_KING    | StartPosition::BLACK_QUEEN
                                   | StartPosition::BLACK_BISHOPS | StartPosition::BLACK_KNIGHTS
                                   | StartPosition::BLACK_ROOKS   | StartPosition::BLACK_PAWNS;
}



// ############################
//            ENUMS
// ############################


#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
#[repr(u8)]
pub enum Color { White = 0, Black = 1 }

impl Not for Color {
    type Output = Self;
    fn not(self) -> Self::Output {
        if self == Color::White {
            Color::Black
        } else {
            Color::White
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
#[repr(u8)]
pub enum Piece { King = 0, Queen, Rook, Knight, Bishop, Pawn }
