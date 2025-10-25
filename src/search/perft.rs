// src/search/perft.rs

use std::collections::HashSet;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::time::{Duration, Instant};

use crate::game::board::{BitBoard, Position, enemy_attacks};
use crate::game::defs::{Color, Piece, Square};
use crate::game::gamestate::CastlingRights;
use crate::game::io::position_from_fen;
use crate::game::moves::{generate_legal_moves, Flag, Move, MoveList};

//
// ---------- Perft-specific MoveMaker ----------
//

#[derive(Clone, Copy)]
pub struct UndoMove {
    mv: Move,
    captured: Option<(Piece, Square)>,
    prev_castling: CastlingRights,
    prev_ep_square: Option<Square>,
    prev_halfmove: u8,
}

#[derive(Default)]
pub struct History {} // Dummy struct to match original API

impl History {
    pub fn make(&mut self, pos: &mut Position, mv: Move) -> UndoMove {
        let us = pos.state.stm;
        let them = !us;
        let from = mv.from_sq();
        let to = mv.to_sq();
        let flags = mv.flags();

        let mut undo = UndoMove {
            mv,
            captured: None,
            prev_castling: pos.state.castling_rights,
            prev_ep_square: pos.state.en_passant_square,
            prev_halfmove: pos.state.ply_counter,
        };

        let (_, mut mover) = pos.piece_at(from).expect("perft make: no piece at from");

        pos.state.en_passant_square = None;
        pos.state.ply_counter += 1;

        if mv.is_capture() {
            pos.state.ply_counter = 0;
            if flags == Flag::EP_CAPTURE as u16 {
                let cap_sq = if us == Color::White { Square(to.0 - 8) } else { Square(to.0 + 8) };
                undo.captured = Some((Piece::Pawn, cap_sq));
                pos.clear_piece(them, Piece::Pawn, cap_sq);
            } else if let Some((_, p)) = pos.piece_at(to) {
                undo.captured = Some((p, to));
                pos.clear_piece(them, p, to);
            }
        }

        pos.clear_piece(us, mover, from);

        if mv.is_promo() {
            mover = match flags {
                x if x == Flag::PROMO_N as u16 || x == Flag::PROMO_N_CAPTURE as u16 => Piece::Knight,
                x if x == Flag::PROMO_B as u16 || x == Flag::PROMO_B_CAPTURE as u16 => Piece::Bishop,
                x if x == Flag::PROMO_R as u16 || x == Flag::PROMO_R_CAPTURE as u16 => Piece::Rook,
                _ => Piece::Queen,
            };
        }

        if mover == Piece::Pawn {
            pos.state.ply_counter = 0;
        }

        pos.set_piece(us, mover, to);

        if flags == Flag::DOUBLE_PUSH as u16 {
            pos.state.en_passant_square = Some(Square((from.0 + to.0) / 2));
        } else if flags == Flag::CASTLE_KING as u16 {
            let (rf, rt) = if us == Color::White { (Square(7), Square(5)) } else { (Square(63), Square(61)) };
            pos.clear_piece(us, Piece::Rook, rf);
            pos.set_piece(us, Piece::Rook, rt);
        } else if flags == Flag::CASTLE_QUEEN as u16 {
            let (rf, rt) = if us == Color::White { (Square(0), Square(3)) } else { (Square(56), Square(59)) };
            pos.clear_piece(us, Piece::Rook, rf);
            pos.set_piece(us, Piece::Rook, rt);
        }

        let mut cr = pos.state.castling_rights;
        cr.0 &= undo.prev_castling.0;
        if mover == Piece::King {
            if us == Color::White { cr.remove(CastlingRights::WK); cr.remove(CastlingRights::WQ); }
            else { cr.remove(CastlingRights::BK); cr.remove(CastlingRights::BQ); }
        }
        if from.0 == 0 || to.0 == 0 { cr.remove(CastlingRights::WQ); }
        if from.0 == 7 || to.0 == 7 { cr.remove(CastlingRights::WK); }
        if from.0 == 56 || to.0 == 56 { cr.remove(CastlingRights::BQ); }
        if from.0 == 63 || to.0 == 63 { cr.remove(CastlingRights::BK); }
        pos.state.castling_rights = cr;
        
        pos.state.stm = them;
        undo
    }

    pub fn unmake(&mut self, pos: &mut Position, undo: UndoMove) {
        let us = pos.state.stm; // Now it's them
        let them = !us; // Now it's us
        let from = undo.mv.from_sq();
        let to = undo.mv.to_sq();
        let flags = undo.mv.flags();

        let (_, mover) = pos.piece_at(to).expect("perft unmake: no piece at to");

        if undo.mv.is_promo() {
            pos.clear_piece(them, mover, to);
            pos.set_piece(them, Piece::Pawn, from);
        } else {
            pos.clear_piece(them, mover, to);
            pos.set_piece(them, mover, from);
        }

        if let Some((p, sq)) = undo.captured {
            pos.set_piece(us, p, sq);
        }
        
        if flags == Flag::CASTLE_KING as u16 {
            let (rf, rt) = if them == Color::White { (Square(7), Square(5)) } else { (Square(63), Square(61)) };
            pos.clear_piece(them, Piece::Rook, rt);
            pos.set_piece(them, Piece::Rook, rf);
        } else if flags == Flag::CASTLE_QUEEN as u16 {
            let (rf, rt) = if them == Color::White { (Square(0), Square(3)) } else { (Square(56), Square(59)) };
            pos.clear_piece(them, Piece::Rook, rt);
            pos.set_piece(them, Piece::Rook, rf);
        }

        pos.state.castling_rights = undo.prev_castling;
        pos.state.en_passant_square = undo.prev_ep_square;
        pos.state.ply_counter = undo.prev_halfmove;
        pos.state.stm = them;
    }
}
//
// ---------- Perft table ----------
//

#[derive(Clone, Copy, Default)]
pub struct PerftStats {
    pub nodes: u64,
    pub captures: u64,
    pub ep: u64,
    pub castles: u64,
    pub promotions: u64,
    pub checks: u64,
    pub mates: u64,
}


fn perft_recurse(pos: &mut Position, depth: usize, hist: &mut History, stats: &mut PerftStats) {
    if depth == 0 {
        stats.nodes += 1;
        return;
    }

    let mut list = MoveList::new();
    let _move_count = generate_legal_moves(pos, &mut list);

    for mv in list.iter() {
        let undo = hist.make(pos, *mv);

        // --- Post-move analysis for checks and mates ---
        let mut child_list = MoveList::new();
        let child_move_count = generate_legal_moves(pos, &mut child_list);
        
        let ksq_bb = pos.pieces(pos.state.stm, Piece::King);
        let ksq = Square(ksq_bb.0.trailing_zeros() as u8);
        let occ = pos.occupied();
        let is_in_check = enemy_attacks(pos, !pos.state.stm, occ) & BitBoard::from_square(ksq) != BitBoard(0);

        // Classify the move we just made
        let flags = mv.flags();
        if flags == Flag::CASTLE_KING as u16 || flags == Flag::CASTLE_QUEEN as u16 {
            stats.castles += 1;
        } else if flags == Flag::EP_CAPTURE as u16 {
            stats.ep += 1;
        } else if mv.is_promo() {
            stats.promotions += 1;
        } else if mv.is_capture() {
            stats.captures += 1;
        }

        if is_in_check {
            stats.checks += 1;
            if child_move_count == 0 {
                stats.mates += 1;
            }
        }
        // --- End of post-move analysis ---

        perft_recurse(pos, depth - 1, hist, stats);
        hist.unmake(pos, undo);
    }
}


pub fn print_perft(fen: &str, max_depth: usize) -> Result<(), Box<dyn std::error::Error>> {
    println!(
        "Depth\tNodes\t\tCaptures\tE.p.\tCastles\tPromotions\tChecks\t\tMates"
    );

    for d in 0..=max_depth {
        let mut pos = position_from_fen(fen)?;
        let mut hist = History::default();
        let mut s = PerftStats::default();
        
        // At depth 0, there is just one position (the root) and no moves.
        if d == 0 {
            s.nodes = 1;
        } else {
            perft_recurse(&mut pos, d, &mut hist, &mut s);
        }

        println!(
            "{d}\t{}\t{}\t\t{}\t{}\t{}\t\t{}\t\t{}",
            format_with_commas(s.nodes),
            format_with_commas(s.captures),
            format_with_commas(s.ep),
            format_with_commas(s.castles),
            format_with_commas(s.promotions),
            format_with_commas(s.checks),
            format_with_commas(s.mates),
        );
    }

    Ok(())
}

fn format_with_commas(n: u64) -> String {
    let mut s = n.to_string();
    let mut i = s.len() as isize - 3;
    while i > 0 {
        s.insert(i as usize, ',');
        i -= 3;
    }
    s
}

//
// ---------- Stockfish helper ----------
//

pub struct Stockfish {
    _child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
}

impl Stockfish {
    pub fn new(path: Option<&str>) -> Result<Self, String> {
        let exe = path.unwrap_or("/usr/games/stockfish");
        let mut child = Command::new(exe)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .map_err(|e| format!("failed to spawn stockfish at {exe}: {e}"))?;

        let stdin = child.stdin.take().ok_or("no stdin for stockfish")?;
        let stdout = child.stdout.take().ok_or("no stdout for stockfish")?;
        let mut sf = Stockfish {
            _child: child,
            stdin,
            stdout: BufReader::new(stdout),
        };
        sf.uci_handshake()?;
        Ok(sf)
    }

    fn write_line(&mut self, s: &str) -> Result<(), String> {
        self.stdin.write_all(s.as_bytes()).map_err(|e| e.to_string())?;
        self.stdin.write_all(b"\n").map_err(|e| e.to_string())?;
        self.stdin.flush().map_err(|e| e.to_string())
    }

    fn read_line(&mut self, buf: &mut String) -> Result<usize, String> {
        buf.clear();
        self.stdout.read_line(buf).map_err(|e| e.to_string())
    }

    fn uci_handshake(&mut self) -> Result<(), String> {
        self.write_line("uci")?;
        let deadline = Instant::now() + Duration::from_secs(2);
        let mut line = String::new();
        loop {
            if Instant::now() > deadline {
                return Err("stockfish uci handshake timed out".into());
            }
            if self.read_line(&mut line)? == 0 {
                continue;
            }
            let t = line.trim();
            if t == "uciok" {
                break;
            }
        }
        self.write_line("isready")?;
        let deadline = Instant::now() + Duration::from_secs(2);
        loop {
            if Instant::now() > deadline {
                return Err("stockfish readyok timed out".into());
            }
            if self.read_line(&mut line)? == 0 {
                continue;
            }
            if line.trim() == "readyok" {
                break;
            }
        }
        self.write_line("ucinewgame")?;
        Ok(())
    }

    /// Robust: uses `go perft 1` to enumerate root legal UCI moves for `fen`.
    pub fn legal_moves(&mut self, fen: &str) -> Result<HashSet<String>, String> {
        self.write_line(&format!("position fen {fen}"))?;
        self.write_line("go perft 1")?;

        let mut moves = HashSet::new();
        let mut line = String::new();
        let deadline = Instant::now() + Duration::from_secs(5);

        loop {
            if Instant::now() > deadline {
                return Err("timed out waiting for perft output".into());
            }

            if self.read_line(&mut line)? == 0 {
                std::thread::sleep(Duration::from_millis(2));
                continue;
            }

            let t = line.trim();

            // Per-move lines look like: "e2e4: 20"
            if let Some(colon) = t.find(':') {
                let m = t[..colon].trim();
                if is_uci_move_like(m) {
                    moves.insert(m.to_string());
                }
            }

            // Final line is "Nodes searched: <N>"
            if t.starts_with("Nodes searched") {
                break;
            }
        }
        Ok(moves)
    }

    /// After "position fen FEN moves UCI", `d` prints a "Fen: ..." line we parse.
    pub fn fen_after_move(&mut self, fen: &str, uci: &str) -> Result<String, String> {
        self.write_line(&format!("position fen {fen} moves {uci}"))?;
        self.write_line("d")?;

        let mut line = String::new();
        let deadline = Instant::now() + Duration::from_secs(3);

        loop {
            if Instant::now() > deadline {
                return Err("timed out reading child FEN via `d`".into());
            }
            if self.read_line(&mut line)? == 0 {
                std::thread::sleep(Duration::from_millis(2));
                continue;
            }
            let t = line.trim();
            if t.starts_with("Fen: ") || t.starts_with("FEN: ") {
                if let Some(f) = t.splitn(2, ": ").nth(1) {
                    return Ok(f.trim().to_string());
                }
            }
        }
    }
}

fn is_uci_move_like(m: &str) -> bool {
    let b = m.as_bytes();
    (b.len() == 4 || b.len() == 5)
        && b[0].is_ascii_lowercase()
        && b[1].is_ascii_digit()
        && b[2].is_ascii_lowercase()
        && b[3].is_ascii_digit()
}

//
// ---------- Legality mismatch search ----------
//

fn collect_our_uci_legal_moves(pos: &Position) -> HashSet<String> {
    let mut set = HashSet::new();
    let mut list = MoveList::new();
    generate_legal_moves(pos, &mut list);
    for mv in list.iter() {
        set.insert(mv.to_uci());
    }
    set
}

/// Iteratively deepens to `max_depth`, and at each node compares our UCI legal moves
/// vs Stockfish legal UCI moves. On first mismatch, prints details and returns Err.
pub fn find_first_legality_mismatch(
    fen: &str,
    max_depth: usize,
    stockfish_path: Option<&str>,
) -> Result<(), Box<dyn std::error::Error>> {
    println!(
        "\n[legality] start  depth=0..{max_depth}  stockfish={}",
        stockfish_path.unwrap_or("/usr/games/stockfish")
    );
    println!("[legality] FEN: {fen}");

    let mut sf = Stockfish::new(stockfish_path)?;

    for d in 1..=max_depth {
        let mut visited = 0_u64;

        fn dfs(
            sf: &mut Stockfish,
            fen: &str,
            depth_left: usize,
            visited: &mut u64,
        ) -> Result<(), String> {
            *visited += 1;

            let pos = position_from_fen(fen).map_err(|e| e.to_string())?;
            let our = collect_our_uci_legal_moves(&pos);
            let sf_moves = sf.legal_moves(fen)?;

            if let Some(bad) = our.iter().find(|m| !sf_moves.contains(*m)) {
                let mut sfl: Vec<_> = sf_moves.iter().cloned().collect();
                sfl.sort();
                let mut oml: Vec<_> = our.iter().cloned().collect();
                oml.sort();
                eprintln!("\n[legality] mismatch at depth {}: our move not in SF: {bad}", depth_left);
                eprintln!("[legality] FEN: {fen}");
                eprintln!("[legality] SF legal moves ({}): {}", sfl.len(), sfl.join(" "));
                eprintln!("[legality] Our legal moves ({}): {}", oml.len(), oml.join(" "));
                return Err("our-illegal-move".into());
            }
            if let Some(miss) = sf_moves.iter().find(|m| !our.contains(*m)) {
                let mut sfl: Vec<_> = sf_moves.iter().cloned().collect();
                sfl.sort();
                let mut oml: Vec<_> = our.iter().cloned().collect();
                oml.sort();
                eprintln!(
                    "\n[legality] mismatch at depth {}: SF move missing from us: {miss}",
                    depth_left
                );
                eprintln!("[legality] FEN: {fen}");
                eprintln!("[legality] SF legal moves ({}): {}", sfl.len(), sfl.join(" "));
                eprintln!("[legality] Our legal moves ({}): {}", oml.len(), oml.join(" "));
                return Err("missing-legal-move".into());
            }

            if depth_left == 1 {
                return Ok(());
            }

            for m in our {
                let child_fen = sf.fen_after_move(fen, &m)?;
                dfs(sf, &child_fen, depth_left - 1, visited)?;
            }
            Ok(())
        }

        if let Err(e) = dfs(&mut sf, fen, d, &mut visited) {
            return Err(e.into());
        }

        println!("\n[legality] depth {d}: positions={visited}");
    }

    println!("\n[legality] no mismatches found up to depth {max_depth}.");
    Ok(())
}
