// src/game/board/mod.rs

pub mod rankfile;
mod bitboard;
pub mod movement;
mod attacks;
mod position;

// Re-exports to preserve the public API
pub use bitboard::BitBoard;
#[allow(unused_imports)]
pub use bitboard::SquareIter;
pub use movement::{PieceMovement, KING_ATK, KNIGHT_ATK, WHITE_PAWN_ATK, BLACK_PAWN_ATK};
pub use attacks::enemy_attacks;
pub use position::Position;
