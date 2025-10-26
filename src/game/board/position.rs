// src/game/board/position.rs

use std::fmt;
use colored::*;

use crate::game::board::BitBoard;
use crate::game::{defs, gamestate};
use crate::game::defs::Piece;

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct Position{
    pub bb_sides: [BitBoard; 2],
    pub bb_pieces: [[BitBoard; 6]; 2],
    pub state: gamestate::State,
}

impl Position {
    pub fn side(&self, c: defs::Color) -> BitBoard { self.bb_sides[c as usize] }
    pub fn pieces(&self, c: defs::Color, p: defs::Piece) -> BitBoard { self.bb_pieces[c as usize][p as usize] }
    pub fn occupied(&self) -> BitBoard { BitBoard(self.bb_sides[0].0 | self.bb_sides[1].0) }

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
        let symbols = [
            ["♔\u{FE0E}", "♕\u{FE0E}", "♖\u{FE0E}", "♘\u{FE0E}", "♗\u{FE0E}", "♙\u{FE0E}"],
            ["♚\u{FE0E}", "♛\u{FE0E}", "♜\u{FE0E}", "♞\u{FE0E}", "♝\u{FE0E}", "♟\u{FE0E}"],
        ];

        let light = (240, 217, 181);
        let dark = (181, 136, 99);

        for rank in (0..8).rev() {
            write!(f, "{} ", (rank + 1).to_string().red())?;
            for file in 0..8 {
                let sq_index = rank * 8 + file;
                let bit: u64 = 1u64 << sq_index;
                let mut symbol = " ".to_string();
                for color in 0..2 {
                    for piece in 0..6 {
                        if self.bb_pieces[color][piece].0 & bit != 0 {
                            symbol = symbols[color][piece].to_string();
                            break;
                        }
                    }
                }
                let (r, g, b) = if (rank + file) % 2 == 0 { light } else { dark };
                let square = format!(" {} ", symbol)
                    .on_truecolor(r, g, b)
                    .truecolor(0, 0, 0);
                write!(f, "{}", square)?;
            }
            writeln!(f)?;
        }
        writeln!(f, "   {}", " a  b  c  d  e  f  g  h".red())?;
        Ok(())
    }
}

