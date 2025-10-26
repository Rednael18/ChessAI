use super::{magics, Flag, Move, MoveList};

use crate::game::board::{self, BitBoard, PieceMovement, Position, KNIGHT_ATK, KING_ATK, WHITE_PAWN_ATK, BLACK_PAWN_ATK};
use crate::game::defs::{self, Square, Color, Piece};
use crate::game::gamestate;

#[derive(Clone, Copy)]
pub struct PinInfo {
    pub pinned: BitBoard,
    pub ray_through_king: [BitBoard; 64],
}

impl Default for PinInfo {
    fn default() -> Self {
        PinInfo {
            pinned: BitBoard(0),
            ray_through_king: [BitBoard(0); 64],
        }
    }
}

fn line_between(a: Square, b: Square) -> BitBoard {
    let a_bit = 1u64 << a.0;
    let b_bit = 1u64 << b.0;

    let same_rank = (PieceMovement::rank_mask(a) & b_bit) != 0;
    let same_file = (PieceMovement::file_mask(a) & b_bit) != 0;
    let same_diag = (PieceMovement::diag_mask(a) & b_bit) != 0;
    let same_anti = (PieceMovement::anti_mask(a) & b_bit) != 0;

    if !(same_rank || same_file || same_diag || same_anti) {
        return BitBoard(0);
    }

    let mask = if same_rank {
        PieceMovement::rank_mask(a)
    } else if same_file {
        PieceMovement::file_mask(a)
    } else if same_diag {
        PieceMovement::diag_mask(a)
    } else {
        PieceMovement::anti_mask(a)
    };

    let occ = a_bit | b_bit;

    let from_a = PieceMovement::hq_line(occ, a_bit, mask);
    let from_b = PieceMovement::hq_line(occ, b_bit, mask);
    BitBoard((from_a & from_b) & !(a_bit | b_bit))
}

fn compute_pins(pos: &Position, us: Color) -> PinInfo {
    let them = !us;
    let occ = pos.occupied();
    let our_occ = pos.side(us);
    let kbb = pos.pieces(us, Piece::King);
    let ksq = Square(kbb.0.trailing_zeros() as u8);

    let occ_wo_us = BitBoard(occ.0 & !our_occ.0);

    let enemy_rooklikes   = pos.pieces(them, Piece::Rook)   | pos.pieces(them, Piece::Queen);
    let enemy_bishoplikes = pos.pieces(them, Piece::Bishop) | pos.pieces(them, Piece::Queen);

    let rook_xray   = magics().rook_attacks(ksq, occ_wo_us);
    let bishop_xray = magics().bishop_attacks(ksq, occ_wo_us);

    let mut rook_pinners   = (rook_xray   & enemy_rooklikes).0;
    let mut bishop_pinners = (bishop_xray & enemy_bishoplikes).0;

    let mut pins = PinInfo::default();

    while rook_pinners != 0 {
        let psq = Square(rook_pinners.trailing_zeros() as u8);
        let between = line_between(ksq, psq);
        let blockers = between & our_occ;
        if blockers.0.count_ones() == 1 {
            let pinned_sq = Square(blockers.0.trailing_zeros() as u8);
            pins.pinned |= blockers;
            pins.ray_through_king[pinned_sq.0 as usize] =
                BitBoard(between.0 | (1u64 << ksq.0) | (1u64 << psq.0));
        }
        rook_pinners &= rook_pinners - 1;
    }

    while bishop_pinners != 0 {
        let psq = Square(bishop_pinners.trailing_zeros() as u8);
        let between = line_between(ksq, psq);
        let blockers = between & our_occ;
        if blockers.0.count_ones() == 1 {
            let pinned_sq = Square(blockers.0.trailing_zeros() as u8);
            pins.pinned |= blockers;
            pins.ray_through_king[pinned_sq.0 as usize] =
                BitBoard(between.0 | (1u64 << ksq.0) | (1u64 << psq.0));
        }
        bishop_pinners &= bishop_pinners - 1;
    }

    pins
}

#[inline]
fn check_mask_single(pos: &Position, us: Color, ksq: Square, checkers: BitBoard) -> BitBoard {
    debug_assert!(checkers.0.count_ones() == 1);
    let csq = Square(checkers.0.trailing_zeros() as u8);
    let slider = (pos.pieces(!us, Piece::Bishop) | pos.pieces(!us, Piece::Rook) | pos.pieces(!us, Piece::Queen)) & checkers;
    if slider != BitBoard(0) {
        line_between(ksq, csq) | checkers
    } else {
        checkers
    }
}

#[inline]
fn king_square_is_attacked_by_sliders_after_move(
    occ_after: board::BitBoard,
    to: Square,
    enemy_rooks_or_queens: board::BitBoard,
    enemy_bishops_or_queens: board::BitBoard,
) -> bool {
    let bb = 1u64 << to.0;

    let r = PieceMovement::hq_line(occ_after.0, bb, PieceMovement::rank_mask(to));
    let f = PieceMovement::hq_line(occ_after.0, bb, PieceMovement::file_mask(to));
    if ((r | f) & enemy_rooks_or_queens.0) != 0 {
        return true;
    }

    let d = PieceMovement::hq_line(occ_after.0, bb, PieceMovement::DIAG_MASK[to.0 as usize]);
    let a = PieceMovement::hq_line(occ_after.0, bb, PieceMovement::ANTI_MASK[to.0 as usize]);
    ((d | a) & enemy_bishops_or_queens.0) != 0
}

pub fn generate_legal_moves(pos: &board::Position, out: &mut MoveList) -> usize {
    out.len = 0;

    let us   = pos.state.stm;
    let them = !us;

    let occ  = pos.occupied();
    let our_occ = pos.side(us);
    let their_occ = pos.side(them);
    let opp_king = pos.pieces(them, Piece::King);
    let their_occ_no_king = their_occ & !opp_king;

    let kbb = pos.pieces(us, defs::Piece::King).0;
    debug_assert!(kbb != 0);
    let ksq = defs::Square(kbb.trailing_zeros() as u8);

    let occ_wo_ksq = BitBoard(occ.0 & !(1u64 << ksq.0));

    let rook_like   = magics().rook_attacks(ksq, occ_wo_ksq)
        & (pos.pieces(them, Piece::Rook) | pos.pieces(them, Piece::Queen));
    let bishop_like = magics().bishop_attacks(ksq, occ_wo_ksq)
        & (pos.pieces(them, Piece::Bishop) | pos.pieces(them, Piece::Queen));

    let knight_chk = KNIGHT_ATK[ksq.0 as usize] & pos.pieces(them, Piece::Knight);
    let pawn_chk = if us == Color::White {
        WHITE_PAWN_ATK[ksq.0 as usize] & pos.pieces(them, Piece::Pawn)
    } else {
        BLACK_PAWN_ATK[ksq.0 as usize] & pos.pieces(them, Piece::Pawn)
    };
    let king_adj = KING_ATK[ksq.0 as usize] & pos.pieces(them, Piece::King);
    let checkers = BitBoard(rook_like.0 | bishop_like.0 | knight_chk.0 | pawn_chk.0 | king_adj.0);

    let double_check = checkers.0.count_ones() >= 2;

    let enemy_pawn_attacks = if them == Color::White {
        board::PieceMovement::white_pawn_attacks_from_bb(pos.pieces(them, Piece::Pawn))
    } else {
        board::PieceMovement::black_pawn_attacks_from_bb(pos.pieces(them, Piece::Pawn))
    };
    let enemy_knight_attacks = board::PieceMovement::knight_attacks_from_bb(pos.pieces(them, Piece::Knight));
    let enemy_king_attacks   = board::PieceMovement::king_attacks_from_bb(pos.pieces(them, Piece::King));

    let enemy_rooks_or_queens   = pos.pieces(them, Piece::Rook)   | pos.pieces(them, Piece::Queen);
    let enemy_bishops_or_queens = pos.pieces(them, Piece::Bishop) | pos.pieces(them, Piece::Queen);

    {
        let kmoves = KING_ATK[ksq.0 as usize];

        for to in (kmoves & !our_occ & !opp_king).iter_squares() {
            let to_bb = board::BitBoard::from_square(to);

            if ((enemy_pawn_attacks | enemy_knight_attacks | enemy_king_attacks) & to_bb) != board::BitBoard(0) {
                continue;
            }

            let mut occ_after = occ.0;
            occ_after &= !(1u64 << ksq.0);
            let is_capture = (their_occ_no_king & to_bb) != board::BitBoard(0);
            if is_capture {
                occ_after &= !(1u64 << to.0);
            }
            occ_after |= 1u64 << to.0;

            if king_square_is_attacked_by_sliders_after_move(
                board::BitBoard(occ_after),
                to,
                enemy_rooks_or_queens,
                enemy_bishops_or_queens,
            ) {
                continue;
            }

            out.push(Move::pack(
                ksq,
                to,
                if is_capture { Flag::CAPTURE } else { Flag::QUIET }
            ));
        }
    }

    if double_check {
        return out.len;
    }

    let is_attacked = |sq: defs::Square| -> bool {
        let sq_bb = BitBoard::from_square(sq);

        if ((enemy_pawn_attacks | enemy_knight_attacks | enemy_king_attacks) & sq_bb) != BitBoard(0) {
            return true;
        }

        if (magics().rook_attacks(sq, occ) & enemy_rooks_or_queens) != BitBoard(0) {
            return true;
        }
        if (magics().bishop_attacks(sq, occ) & enemy_bishops_or_queens) != BitBoard(0) {
            return true;
        }

        false
    };

    if checkers == BitBoard(0) {
        const F1G1: BitBoard = BitBoard(0b01100000);
        const B1C1D1: BitBoard = BitBoard(0b00001110);
        const F8G8: BitBoard = BitBoard(F1G1.0 << 56);
        const B8C8D8: BitBoard = BitBoard(B1C1D1.0 << 56);

        if us == Color::White {
            if pos.state.castling_rights.0 & gamestate::CastlingRights::WK.0 != 0 {
                if (occ & F1G1) == BitBoard(0) && !is_attacked(defs::squarename::F1) && !is_attacked(defs::squarename::G1) {
                    out.push(Move::pack(ksq, defs::squarename::G1, Flag::CASTLE_KING));
                }
            }
            if pos.state.castling_rights.0 & gamestate::CastlingRights::WQ.0 != 0 {
                if (occ & B1C1D1) == BitBoard(0) && !is_attacked(defs::squarename::C1) && !is_attacked(defs::squarename::D1) {
                    out.push(Move::pack(ksq, defs::squarename::C1, Flag::CASTLE_QUEEN));
                }
            }
        } else {
            if pos.state.castling_rights.0 & gamestate::CastlingRights::BK.0 != 0 {
                if (occ & F8G8) == BitBoard(0) && !is_attacked(defs::squarename::F8) && !is_attacked(defs::squarename::G8) {
                    out.push(Move::pack(ksq, defs::squarename::G8, Flag::CASTLE_KING));
                }
            }
            if pos.state.castling_rights.0 & gamestate::CastlingRights::BQ.0 != 0 {
                if (occ & B8C8D8) == BitBoard(0) && !is_attacked(defs::squarename::C8) && !is_attacked(defs::squarename::D8) {
                    out.push(Move::pack(ksq, defs::squarename::C8, Flag::CASTLE_QUEEN));
                }
            }
        }
    }

    let check_mask = if checkers.0 != 0 {
        check_mask_single(pos, us, ksq, checkers)
    } else {
        BitBoard(!0u64)
    };

    let pins = compute_pins(pos, us);

    {
        let pawns = pos.pieces(us, Piece::Pawn);
        let empty = !occ;

        let (one_step, two_step, _push_from_rank, _ep_rank) = if us == Color::White {
            (pawns.north() & empty,
             ((pawns & board::rankfile::RANK_2).north().north()) & empty & (empty.north()),
             board::rankfile::RANK_2,
             board::rankfile::RANK_5)
        } else {
            (pawns.south() & empty,
             ((pawns & board::rankfile::RANK_7).south().south()) & empty & (empty.south()),
             board::rankfile::RANK_7,
             board::rankfile::RANK_4)
        };

        let mut pushes = one_step & check_mask;
        while pushes.0 != 0 {
            let to = Square(pushes.0.trailing_zeros() as u8);
            let from = if us == Color::White { Square(to.0 - 8) } else { Square(to.0 + 8) };
            if (pins.pinned & BitBoard::from_square(from)) != BitBoard(0) {
                let ray = pins.ray_through_king[from.0 as usize];
                if (ray & BitBoard::from_square(to)) == BitBoard(0) {
                    pushes = BitBoard(pushes.0 & (pushes.0 - 1));
                    continue;
                }
            }
            if to.get_rank() == if us == defs::Color::White { 7 } else { 0 } {
                out.push(Move::pack(from, to, Flag::PROMO_B));
                out.push(Move::pack(from, to, Flag::PROMO_N));
                out.push(Move::pack(from, to, Flag::PROMO_R));
                out.push(Move::pack(from, to, Flag::PROMO_Q));
            } else {
                out.push(Move::pack(from, to, Flag::QUIET));
            }
            pushes = BitBoard(pushes.0 & (pushes.0 - 1));
        }

        let mut d2 = two_step & check_mask;
        while d2.0 != 0 {
            let to = Square(d2.0.trailing_zeros() as u8);
            let from = if us == Color::White { Square(to.0 - 16) } else { Square(to.0 + 16) };

            if (pins.pinned & BitBoard::from_square(from)) != BitBoard(0) {
                let ray = pins.ray_through_king[from.0 as usize];
                if (ray & BitBoard::from_square(to)) == BitBoard(0) {
                    d2 = BitBoard(d2.0 & (d2.0 - 1));
                    continue;
                }
            }

            out.push(Move::pack(from, to, Flag::DOUBLE_PUSH));
            d2 = BitBoard(d2.0 & (d2.0 - 1));
        }

        let mut p = pawns.0;
        while p != 0 {
            let from = Square(p.trailing_zeros() as u8);
            p &= p - 1;

            let candidates: [Option<Square>; 2] = if us == Color::White {
                let c1 = if from.get_file() > 0 { Some(Square(from.0 + 7)) } else { None };
                let c2 = if from.get_file() < 7 { Some(Square(from.0 + 9)) } else { None };
                [c1, c2]
            } else {
                let c1 = if from.get_file() < 7 { Some(Square(from.0 - 7)) } else { None };
                let c2 = if from.get_file() > 0 { Some(Square(from.0 - 9)) } else { None };
                [c1, c2]
            };

            for &opt_to in &candidates {
                let Some(to) = opt_to else { continue };

                if (their_occ_no_king & BitBoard::from_square(to)) == BitBoard(0) { continue; }
                if (check_mask & BitBoard::from_square(to)) == BitBoard(0) { continue; }

                if (pins.pinned & BitBoard::from_square(from)) != BitBoard(0) {
                    let ray = pins.ray_through_king[from.0 as usize];
                    if (ray & BitBoard::from_square(to)) == BitBoard(0) { continue; }
                }

                let last_rank = if us == Color::White { 7 } else { 0 };
                if to.get_rank() == last_rank {
                    out.push(Move::pack(from, to, Flag::PROMO_B_CAPTURE));
                    out.push(Move::pack(from, to, Flag::PROMO_N_CAPTURE));
                    out.push(Move::pack(from, to, Flag::PROMO_R_CAPTURE));
                    out.push(Move::pack(from, to, Flag::PROMO_Q_CAPTURE));
                } else {
                    out.push(Move::pack(from, to, Flag::CAPTURE));
                }
            }
        }

        if let Some(ep_sq) = pos.state.en_passant_square {
            let ep_attackers = if us == Color::White {
                BLACK_PAWN_ATK[ep_sq.0 as usize]
            } else {
                WHITE_PAWN_ATK[ep_sq.0 as usize]
            } & pawns;

            let mut ep_froms = ep_attackers.0;
            while ep_froms != 0 {
                let from = Square(ep_froms.trailing_zeros() as u8);
                let to = ep_sq;

                if (pins.pinned & BitBoard::from_square(from)) != BitBoard(0) {
                    let ray = pins.ray_through_king[from.0 as usize];
                    if (ray & BitBoard::from_square(to)) == BitBoard(0) {
                        ep_froms &= ep_froms - 1;
                        continue;
                    }
                }

                let captured_sq = if us == Color::White { Square(to.0 - 8) } else { Square(to.0 + 8) };

                let mut occ_tmp = occ.0;
                occ_tmp &= !(1u64 << from.0);
                occ_tmp |=  1u64 << to.0;
                occ_tmp &= !(1u64 << captured_sq.0);

                let mut their_pawns = pos.pieces(them, Piece::Pawn);
                their_pawns.0 &= !(1u64 << captured_sq.0);

                let pawns_att   = if them == Color::White {
                    PieceMovement::white_pawn_attacks_from_bb(their_pawns)
                } else {
                    PieceMovement::black_pawn_attacks_from_bb(their_pawns)
                };
                let knights_att = PieceMovement::knight_attacks_from_bb(pos.pieces(them, Piece::Knight));
                let bishops_att = PieceMovement::bishop_attacks_from_bb(
                    pos.pieces(them, Piece::Bishop) | pos.pieces(them, Piece::Queen),
                    BitBoard(occ_tmp),
                );
                let rooks_att   = PieceMovement::rook_attacks_from_bb(
                    pos.pieces(them, Piece::Rook) | pos.pieces(them, Piece::Queen),
                    BitBoard(occ_tmp),
                );
                let kings_att   = PieceMovement::king_attacks_from_bb(pos.pieces(them, Piece::King));

                let attacked_after = pawns_att | knights_att | bishops_att | rooks_att | kings_att;

                if (attacked_after & BitBoard::from_square(ksq)) == BitBoard(0) {
                    out.push(Move::pack(from, to, Flag::EP_CAPTURE));
                }

                ep_froms &= ep_froms - 1;
            }
        }
    }

    {
        let knights = pos.pieces(us, Piece::Knight);
        let mut b = knights.0;
        while b != 0 {
            let from = Square(b.trailing_zeros() as u8);
            b &= b - 1;

            if (pins.pinned & BitBoard::from_square(from)) != BitBoard(0) {
                continue;
            }

            let moves = KNIGHT_ATK[from.0 as usize]
                & !our_occ & check_mask & !opp_king;
            for to in moves.iter_squares() {
                let flag = if (their_occ & BitBoard::from_square(to)) != BitBoard(0) { Flag::CAPTURE } else { Flag::QUIET };
                out.push(Move::pack(from, to, flag));
            }
        }
    }

    macro_rules! gen_slider_magic {
        ($piece:expr, $method:ident) => {{
            let bb = pos.pieces(us, $piece);
            let mut s = bb.0;
            while s != 0 {
                let from = Square(s.trailing_zeros() as u8);
                s &= s - 1;

                let pin_ray = if (pins.pinned & BitBoard::from_square(from)) != BitBoard(0) {
                    pins.ray_through_king[from.0 as usize]
                } else {
                    BitBoard(!0)
                };

                let mut targets = (magics().$method(from, occ).0
                                & !our_occ.0 & check_mask.0 & !opp_king.0 & pin_ray.0);

                while targets != 0 {
                    let to_idx = targets.trailing_zeros() as u8;
                    targets &= targets - 1;
                    let to = Square(to_idx);
                    let to_bit = 1u64 << to_idx;

                    let flag = if (their_occ_no_king.0 & to_bit) != 0 { Flag::CAPTURE } else { Flag::QUIET };
                    out.push(Move::pack(from, to, flag));
                }
            }
        }}
    }

    gen_slider_magic!(Piece::Bishop, bishop_attacks);
    gen_slider_magic!(Piece::Rook,   rook_attacks);
    gen_slider_magic!(Piece::Queen,  queen_attacks);
    out.len
}

