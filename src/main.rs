// src/main.rs

mod game;
mod search;
mod gui_piston;

#[allow(unused_imports)]
use game::board::Position;
use std::io::{self, Write};

use crate::{game::io::{display_moves, position_from_fen}, search::{engine::run_game_simulated, perft::{find_first_legality_mismatch, print_perft}}};



fn main() {
    gui_piston::play_simulated();
    //perft_search();
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

#[allow(dead_code)]
fn play(simulated: bool) {
    let init_pos = Position::default();
    if simulated {
        run_game_simulated(init_pos);
    } else {
        let player = input("Choose side: ");
        let play_as = if player.eq_ignore_ascii_case("w") { crate::game::defs::Color::White } else { crate::game::defs::Color::Black };
        crate::search::engine::run_game(init_pos, play_as);
    }
}

// Changed return type to Box<dyn Error> to handle `?` correctly
#[allow(dead_code)]
fn perft_search() -> Result<(), Box<dyn std::error::Error>> {
    let start_fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";

    println!("Start perft!");
    // 1) print a perft table to stdout
    print_perft(start_fen, 7)?;

    println!("Start legality mismatch checker!");
    // 2) walk until we find the first legality mismatch vs Stockfish
    //    If Stockfish isn't on PATH, pass Some("/path/to/stockfish")
    find_first_legality_mismatch(start_fen, 6, None)?;

    Ok(())
}

#[allow(dead_code)]
fn disp_moveset() {
    let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ";
    let pos = position_from_fen(fen).unwrap();
    display_moves(&pos);
}