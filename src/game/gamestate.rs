// src/game/gamestate.rs

use crate::game::defs;



// ############################
//           CASTLING
// ############################

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct CastlingRights(pub u8);

impl CastlingRights {
    pub const WK: Self = Self(1 << 0);
    pub const WQ: Self = Self(1 << 1);
    pub const BK: Self = Self(1 << 2);
    pub const BQ: Self = Self(1 << 3);

    pub const NONE: Self = Self(0);
    pub const ALL:  Self = Self(Self::WK.0 | Self::WQ.0 | Self::BK.0 | Self::BQ.0);

    pub const fn all() -> Self {Self::ALL}
    pub const fn none() -> Self {Self::NONE}

    pub fn insert(&mut self, other: Self) {
        self.0 |= other.0;
    }

    pub fn remove(&mut self, other: Self) {
        self.0 &= !other.0;
    }

}

impl Default for CastlingRights {
    fn default() -> Self {
        Self(CastlingRights::ALL.0)
    }
}


// ############################
//           GAMESTATE
// ############################


#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct State {
    pub castling_rights: CastlingRights,
    pub en_passant_square: Option<defs::Square>,
    pub ply_counter: u8,
    pub stm: defs::Color,
}

impl State {
    #![allow(dead_code)]
    pub fn increase_ply(&mut self) {
        self.ply_counter += 1;
    }

    pub fn decrease_ply(&mut self) {
        self.ply_counter -= 1;
    }
}

impl Default for State {
    fn default() -> Self {
        Self {
            castling_rights: CastlingRights::all(),
            en_passant_square: None,
            ply_counter: 0,
            stm: defs::Color::White
        }
    }
}