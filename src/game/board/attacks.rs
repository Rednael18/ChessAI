// src/game/board/attacks.rs

use crate::game::board::{BitBoard, PieceMovement, Position};
use crate::game::defs::{Color, Piece, Square};
use crate::game::moves::{MAGICS, Magics};

#[inline]
fn magics() -> &'static Magics {
    MAGICS.get_or_init(|| Magics::new())
}

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

