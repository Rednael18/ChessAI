// src/bin/view_dump.rs
// View the first N positions in ./data/texel_dump.bin
// Use one window for all positions; Right/Space=next, Left=prev, Esc/Q=quit.

use std::fs::File;
use std::io::{BufReader};
use std::io::ErrorKind as IoErrorKind;
use std::path::PathBuf;

use anyhow::Result;
use bincode;
use serde::{Deserialize, Serialize};

// --- pull your engine modules into this bin ---
#[path = "../game/mod.rs"]
mod game;
#[path = "../search/mod.rs"]
mod search;

use game::board::{self, Position};
use game::defs::{Color, Piece, Square};
use game::gamestate::{CastlingRights, State};
use game::moves::{self, MoveList};

// Piston/gl window stuff for drawing (duplicated lightly from game::io)
use piston_window::*;
use std::collections::HashMap;

// ---- must match the dumper ----
#[derive(Debug, Clone, Serialize, Deserialize)]
struct DumpRow {
    // [my P, my N, my B, my R, my Q, my K, opp P, opp N, opp B, opp R, opp Q, opp K]
    bb: [u64; 12],
    castling: u8,      // absolute KQkq = 1|2|4|8
    ep: Option<u8>,    // absolute a1=0..h8=63
    y: f32,            // unused here
    mover_is_white: bool,
    was_mirrored: bool,
}

// mirror ranks (a1<->a8, etc.)
fn mirror_vertical_u64(bb: u64) -> u64 {
    let mut out = 0u64;
    let mut b = bb;
    while b != 0 {
        let sq = b.trailing_zeros() as u8; // 0..63
        b &= b - 1;
        let file = sq % 8;
        let rank = sq / 8;
        let mrank = 7 - rank;
        let msq = mrank * 8 + file;
        out |= 1u64 << msq;
    }
    out
}

fn row_to_position(row: &DumpRow) -> Position {
    // Start empty
    let mut pos = Position {
        bb_sides: [board::BitBoard(0); 2],
        bb_pieces: [[board::BitBoard(0); 6]; 2],
        state: State::default(),
    };

    // 1) Bring bitboards back to ABSOLUTE orientation (undo mirroring if any).
    let mut bbs_abs = row.bb;
    if row.was_mirrored {
        for bb in &mut bbs_abs {
            *bb = mirror_vertical_u64(*bb);
        }
    }

    // 2) Split into "my"/"opp" after unmirroring.
    let my: &[u64; 6] = (&bbs_abs[0..6]).try_into().unwrap();
    let opp: &[u64; 6] = (&bbs_abs[6..12]).try_into().unwrap();

    // 3) Map "my"/"opp" to absolute colors.
    let (white_side, black_side): (&[u64; 6], &[u64; 6]) = if row.mover_is_white {
        (my, opp)
    } else {
        (opp, my)
    };

    // 4) Write piece bitboards
    let set_side = |pos: &mut Position, color_idx: usize, side: &[u64; 6]| {
        let map: [(usize, Piece); 6] = [
            (0, Piece::Pawn),
            (1, Piece::Knight),
            (2, Piece::Bishop),
            (3, Piece::Rook),
            (4, Piece::Queen),
            (5, Piece::King),
        ];
        for (i, p) in map {
            let bb = board::BitBoard(side[i]);
            pos.bb_pieces[color_idx][p as usize] = bb;
            pos.bb_sides[color_idx] |= bb;
        }
    };
    set_side(&mut pos, Color::White as usize, white_side);
    set_side(&mut pos, Color::Black as usize, black_side);

    // 5) Restore STM (absolute)
    pos.state.stm = if row.mover_is_white { Color::White } else { Color::Black };

    // 6) Restore absolute EP square (already absolute in the dump — do NOT mirror)
    pos.state.en_passant_square = row.ep.map(|idx| {
        let file = idx % 8;
        let rank = idx / 8;
        Square::from_file_rank(file, rank)
    });

    // 7) Restore castling rights from KQkq bitfield
    let mut cr = CastlingRights::none();
    if (row.castling & 0b0001) != 0 { cr.insert(CastlingRights::WK); }
    if (row.castling & 0b0010) != 0 { cr.insert(CastlingRights::WQ); }
    if (row.castling & 0b0100) != 0 { cr.insert(CastlingRights::BK); }
    if (row.castling & 0b1000) != 0 { cr.insert(CastlingRights::BQ); }
    pos.state.castling_rights = cr;

    pos
}

fn read_rows_bincode(path: &PathBuf, limit: usize) -> Result<Vec<DumpRow>> {
    let f = File::open(path)?;
    let mut rdr = BufReader::new(f);
    let mut out = Vec::with_capacity(limit);

    for _ in 0..limit {
        match bincode::deserialize_from::<_, DumpRow>(&mut rdr) {
            Ok(row) => out.push(row),
            Err(e) => {
                // Clean EOF -> stop; anything else -> bubble up
                if let bincode::ErrorKind::Io(ref ioe) = *e {
                    if ioe.kind() == IoErrorKind::UnexpectedEof {
                        break;
                    }
                }
                return Err(anyhow::anyhow!("bincode::deserialize_from failed: {}", e));
            }
        }
    }
    Ok(out)
}

// ------------------------ Simple viewer (one window) ------------------------

const WINDOW_SIZE: u32 = 640;
const SQUARE_SIZE: f64 = (WINDOW_SIZE / 8) as f64;

fn load_piece_textures(window: &mut PistonWindow, assets: &std::path::Path) -> HashMap<String, G2dTexture> {
    let mut map = HashMap::new();
    let pieces = ["wP", "wN", "wB", "wR", "wQ", "wK", "bP", "bN", "bB", "bR", "bQ", "bK"];
    for piece in pieces.iter() {
        let path = assets.join(format!("{}.png", piece));
        let texture = Texture::from_path(
            &mut window.create_texture_context(),
            &path,
            Flip::None,
            &TextureSettings::new(),
        ).unwrap();
        map.insert(piece.to_string(), texture);
    }
    map
}

fn draw_board(c: &Context, g: &mut G2d) {
    let light = [0.94, 0.85, 0.71, 1.0];
    let dark  = [0.71, 0.53, 0.39, 1.0];

    for rank in 0..8 {
        for file in 0..8 {
            let color = if (rank + file) % 2 == 0 { light } else { dark };
            let (x, y) = (file as f64 * SQUARE_SIZE, (7 - rank) as f64 * SQUARE_SIZE);
            rectangle(color, [x, y, SQUARE_SIZE, SQUARE_SIZE], c.transform, g);
        }
    }
}

fn draw_pieces(pos: &Position, piece_map: &HashMap<String, G2dTexture>, c: &Context, g: &mut G2d) {
    for color_idx in 0..2 {
        for piece_idx in 0..6 {
            let color = if color_idx == 0 { Color::White } else { Color::Black };
            let piece = unsafe { std::mem::transmute(piece_idx as u8) };

            let piece_char = match color { Color::White => "w", Color::Black => "b" };
            let piece_str = match piece {
                Piece::Pawn => "P", Piece::Knight => "N", Piece::Bishop => "B",
                Piece::Rook => "R", Piece::Queen => "Q", Piece::King => "K",
            };
            let key = format!("{}{}", piece_char, piece_str);
            let texture = &piece_map[&key];

            for sq in pos.pieces(color, piece) {
                let (x, y) = (
                    sq.get_file() as f64 * SQUARE_SIZE,
                    (7 - sq.get_rank()) as f64 * SQUARE_SIZE
                );
                image(texture, c.transform.trans(x, y).scale(
                    SQUARE_SIZE / texture.get_width() as f64,
                    SQUARE_SIZE / texture.get_height() as f64
                ), g);
            }
        }
    }
}

fn draw_move_arrows<'a, I>(moves: I, c: &Context, g: &mut G2d)
where
    I: Iterator<Item = &'a game::moves::Move>,
{
    let arrow_color = [0.2, 0.5, 0.2, 0.7]; // semi-transparent green
    for mv in moves {
        let from = mv.from_sq();
        let to = mv.to_sq();
        let (x1, y1) = (
            from.get_file() as f64 * SQUARE_SIZE + SQUARE_SIZE / 2.0,
            (7 - from.get_rank()) as f64 * SQUARE_SIZE + SQUARE_SIZE / 2.0,
        );
        let (x2, y2) = (
            to.get_file() as f64 * SQUARE_SIZE + SQUARE_SIZE / 2.0,
            (7 - to.get_rank()) as f64 * SQUARE_SIZE + SQUARE_SIZE / 2.0,
        );
        line(arrow_color, 4.0, [x1, y1, x2, y2], c.transform, g);

        let angle = (y2 - y1).atan2(x2 - x1);
        let arrow_len = 15.0;
        let p1 = [
            x2 - arrow_len * angle.cos() - (angle + std::f64::consts::PI / 2.0).cos() * 8.0,
            y2 - arrow_len * angle.sin() - (angle + std::f64::consts::PI / 2.0).sin() * 8.0,
        ];
        let p2 = [
            x2 - arrow_len * angle.cos() + (angle + std::f64::consts::PI / 2.0).cos() * 8.0,
            y2 - arrow_len * angle.sin() + (angle + std::f64::consts::PI / 2.0).sin() * 8.0,
        ];
        polygon(arrow_color, &[[x2, y2], p1, p2], c.transform, g);
    }
}

fn main() -> Result<()> {
    // Args: [path] [count]
    let path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "./data/texel_dump.bin".to_string());
    let count: usize = std::env::args()
        .nth(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);

    let path = PathBuf::from(path);
    let rows = read_rows_bincode(&path, count)?;
    if rows.is_empty() {
        eprintln!("No rows decoded from {}.", path.display());
        return Ok(());
    }

    // Convert rows -> positions (absolute)
    let mut positions: Vec<Position> = rows.iter().map(row_to_position).collect();

    // One window, textures once
    let mut window: PistonWindow =
        WindowSettings::new("Dump viewer: ←/→ or Space to step, Esc/Q to quit",
                            [WINDOW_SIZE, WINDOW_SIZE])
            .exit_on_esc(true)
            .build()
            .unwrap();

    let assets = find_folder::Search::ParentsThenKids(3, 3).for_folder("assets").unwrap();
    let piece_map = load_piece_textures(&mut window, &assets);

    // Navigation state
    let mut idx: usize = 0;
    let mut last_printed: Option<usize> = None;
    let mut moves_cache: Vec<MoveList> = Vec::with_capacity(positions.len());
    moves_cache.resize_with(positions.len(), MoveList::new);

    // Prime first position’s moves/print
    {
        let mut ml = MoveList::new();
        moves::generate_legal_moves(&positions[idx], &mut ml);
        moves_cache[idx] = ml;
    }

    while let Some(e) = window.next() {
        if let Some(Button::Keyboard(k)) = e.press_args() {
            match k {
                Key::Right | Key::Space => {
                    if idx + 1 < positions.len() { idx += 1; }
                    if moves_cache[idx].iter().count() == 0 {
                        let mut ml = MoveList::new();
                        moves::generate_legal_moves(&positions[idx], &mut ml);
                        moves_cache[idx] = ml;
                    }
                }
                Key::Left => {
                    if idx > 0 { idx -= 1; }
                }
                Key::Q | Key::Escape => break,
                _ => {}
            }
        }

        // Print moves once per index
        if last_printed != Some(idx) {
            let ml = &moves_cache[idx];
            println!("Position #{}: {} legal moves", idx, ml.iter().count());
            for mv in ml.iter() {
                println!("  {}{}", mv.from_sq(), mv.to_sq());
            }
            last_printed = Some(idx);
        }

        window.draw_2d(&e, |c, g, _| {
            clear([1.0; 4], g);
            draw_board(&c, g);
            draw_pieces(&positions[idx], &piece_map, &c, g);
            draw_move_arrows(moves_cache[idx].iter(), &c, g);
        });
    }

    Ok(())
}
