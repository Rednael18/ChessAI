// src/search/engine.rs

use crate::game::board::Position;
use crate::game::history::History;
use crate::game::defs::{Color, Square};
use crate::game::moves::{Move, MoveList, Flag, generate_legal_moves};
use crate::search::search::{complete_search};
#[allow(unused_imports)]
use crate::search::eval::{material_eval, 
                          material_and_attack_eval, 
                          material_attack_position_eval, 
                          material_attack_pesto_eval,
                          alternate_pesto};
use crate::game::io::position_to_fen;
use crate::search::transpose::{TranspositionTable, Zobrist, ZobristKeys};
use std::io::{self, Write};


pub fn run_game(init_pos: Position, play_as: Color) {
    let bot = !play_as;
    let keys = ZobristKeys::new_with_seed(0xCAFE_F00D);
    let mut history = History::new(init_pos.clone());
    let mut z = Zobrist::from_position(keys, &init_pos);
    let mut tt = TranspositionTable::new();

    const DEPTH: i32 = 6;

    loop {
        let pos = history.current().clone();
        let eval_func = alternate_pesto;

        let bot_out = &mut MoveList::new();
        let bot_moves = generate_legal_moves(&pos, bot_out);

        if pos.state.stm == bot {
            if bot_moves == 0 {
                print!("CHECKMATE! YOU WIN!\n");
                break;
            }
            let mv = complete_search(&pos, DEPTH, None, eval_func, true, &mut z, &mut tt);
            history.apply(mv, &mut z);
            println!("{}", history.current());
            let curr_eval = eval_func(&history.current());
            println!("Evaluation: {} centipawns", curr_eval);
            continue;
        }

        

        let out = &mut MoveList::new();
        let player_moves = generate_legal_moves(&pos, out);
        if player_moves == 0 {
            print!("CHECKMATE! YOU LOSE!\n");
            break;
        }

        let player_decision = input("Make move (e.g. e2e4, 'undo'): ");

        if player_decision.eq_ignore_ascii_case("undo") {
            if history.undo() {
                let _ = history.undo();
            }
            println!("{}", history.current());
            continue;
        }

        if player_decision.eq_ignore_ascii_case("fen") {
            let fullmove = (history.plies() as u32) / 2 + 1;
            println!("{}", position_to_fen(history.current(), fullmove, history.half_move()));
            continue;
        }

        match parse_move(history.current(), &player_decision) {
            Some(mv) => {
                history.apply(mv, &mut z);
                println!("{}", history.current());
            }
            None => {
                eprintln!("Illegal or unparseable move: {}", player_decision);
            }
        }
    }
}

pub fn run_game_simulated(init_pos: Position) {
    let bot1 = Color::White;
    let _bot2 = !bot1;
    let keys = ZobristKeys::new_with_seed(0xCAFE_F00D);
    let mut history = History::new(init_pos.clone());
    let mut z = Zobrist::from_position(keys, &init_pos);
    let mut tt = TranspositionTable::new();

    const DEPTH: i32 = 7;

    loop {
        let pos = history.current().clone();
        let eval_func = alternate_pesto;

        let bot_out = &mut MoveList::new();
        let bot_moves = generate_legal_moves(&pos, bot_out);

        if pos.state.stm == bot1 {
            if bot_moves == 0 {
                print!("CHECKMATE! YOU WIN!\n");
                break;
            }
            let mv = complete_search(&pos, DEPTH, None, eval_func, true, &mut z, &mut tt);
            history.apply(mv, &mut z);
            println!("{}", history.current());
            let curr_eval = eval_func(&history.current());
            println!("Evaluation: {} centipawns", curr_eval);
            continue;
        }

        

        let bot2_out = &mut MoveList::new();
        let bot2_moves = generate_legal_moves(&pos, bot2_out);
        if bot2_moves == 0 {
            print!("CHECKMATE! YOU LOSE!\n");
            break;
        }

        let mv = complete_search(&pos, DEPTH, None, eval_func, true, &mut z, &mut tt);
            history.apply(mv, &mut z);
            println!("{}", history.current());
            let curr_eval = eval_func(&history.current());
            println!("Evaluation: {} centipawns", curr_eval);
            continue;
    }
}

pub fn parse_move(pos: &Position, mv: &str) -> Option<Move> {
    // Accept exactly 4 (e2e4) or 5 (e7e8q) characters.
    if mv.len() != 4 && mv.len() != 5 {
        return None;
    }

    // We only ever slice ASCII here, so byte slicing is fine.
    let b = mv.as_bytes();
    let from = std::str::from_utf8(&b[0..2]).ok()?;
    let to   = std::str::from_utf8(&b[2..4]).ok()?;

    // Optional promotion piece, case-insensitive: q r b n
    let promo = if mv.len() == 5 {
        let p = b[4] as char;
        Some(p.to_ascii_lowercase())
    } else {
        None
    };

    pack_move(pos, from, to, promo)
}

#[inline]
fn sq_from_str(s: &str) -> Option<Square> {
    let b = s.as_bytes();
    if b.len() != 2 { return None; }

    let file = match b[0] {
        b'a'..=b'h' => b[0] - b'a',
        b'A'..=b'H' => b[0] - b'A',
        _ => return None,
    };
    let rank = match b[1] {
        b'1'..=b'8' => b[1] - b'1',
        _ => return None,
    };

    Some(Square::from_file_rank(file, rank))
}

fn is_promo_flag_of(flags: u16, want: char) -> bool {
    // Match both quiet promotions and capture promotions.
    match want {
        'q' => flags == Flag::PROMO_Q as u16 || flags == Flag::PROMO_Q_CAPTURE as u16,
        'r' => flags == Flag::PROMO_R as u16 || flags == Flag::PROMO_R_CAPTURE as u16,
        'b' => flags == Flag::PROMO_B as u16 || flags == Flag::PROMO_B_CAPTURE as u16,
        'n' => flags == Flag::PROMO_N as u16 || flags == Flag::PROMO_N_CAPTURE as u16,
        _ => false,
    }
}

fn promo_priority(flags: u16) -> i32 {
    // Used only when no explicit promo piece was given; prefer Q > R > B > N.
    if flags == Flag::PROMO_Q as u16 || flags == Flag::PROMO_Q_CAPTURE as u16 { 4 }
    else if flags == Flag::PROMO_R as u16 || flags == Flag::PROMO_R_CAPTURE as u16 { 3 }
    else if flags == Flag::PROMO_B as u16 || flags == Flag::PROMO_B_CAPTURE as u16 { 2 }
    else if flags == Flag::PROMO_N as u16 || flags == Flag::PROMO_N_CAPTURE as u16 { 1 }
    else { 0 }
}

pub fn pack_move(pos: &Position, from: &str, to: &str, promo: Option<char>) -> Option<Move> {
    let from_sq = sq_from_str(from)?;
    let to_sq   = sq_from_str(to)?;

    // Precompute the butterfly key for quick comparisons:
    // butterfly_index = (from << 6) | to  (because Move packs from<<10 | to<<4)
    let wanted_key: u16 = (((from_sq.0 as u16) & 0x3F) << 6) | ((to_sq.0 as u16) & 0x3F);

    let mut moves = MoveList::new();
    generate_legal_moves(pos, &mut moves);

    // If a promotion piece was specified, choose the matching promotion move only.
    if let Some(p) = promo {
        let want = p.to_ascii_lowercase();
        if !matches!(want, 'q' | 'r' | 'b' | 'n') {
            return None;
        }
        for m in moves.iter() {
            if m.butterfly_index() == wanted_key && is_promo_flag_of(m.flags(), want) {
                return Some(*m);
            }
        }
        // Explicit promo requested but not legal.
        return None;
    }

    // Otherwise, pick the unique non-promotion move if present, or prefer promotions by Q>R>B>N.
    let mut best: Option<Move> = None;
    let mut best_score: i32 = -1;

    for m in moves.iter() {
        if m.butterfly_index() != wanted_key { continue; }
        let score = promo_priority(m.flags());
        if score > best_score {
            best = Some(*m);
            best_score = score;
            if score == 4 { break; } // can't beat a queen promotion
        } else if best_score < 0 {
            // First exact match seen (non-promo). If later we see a promo, it'll win.
            best = Some(*m);
            best_score = 0;
        }
    }

    best
}

fn input(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush().unwrap();

    let mut buffer = String::new();
    io::stdin()
        .read_line(&mut buffer)
        .expect("Failed to read line");

    buffer.trim().to_string()
}
