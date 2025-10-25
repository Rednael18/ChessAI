// src/game/io.rs

use crate::game::board::{self, Position};
use crate::game::defs::{Color, Piece, Square};
use crate::game::gamestate::{CastlingRights, State};
use crate::game::moves::{self, Move, MoveList};
use piston_window::*;
use std::collections::HashMap;

// ############################
//       FEN PARSING
// ############################

/// Parses a FEN string and returns a `Position` object.
/// Returns `Err` if the FEN string is invalid.
pub fn position_from_fen(fen: &str) -> Result<Position, String> {
    let mut pos = Position {
        bb_sides: [board::BitBoard(0); 2],
        bb_pieces: [[board::BitBoard(0); 6]; 2],
        state: State::default(),
    };

    let parts: Vec<&str> = fen.split_whitespace().collect();
    if parts.len() < 4 {
        return Err("Invalid FEN: not enough parts.".to_string());
    }

    // 1. Piece Placement
    let placement = parts[0];
    let mut rank = 7;
    let mut file = 0;
    for ch in placement.chars() {
        if ch == '/' {
            rank -= 1;
            file = 0;
        } else if let Some(digit) = ch.to_digit(10) {
            file += digit as i32;
        } else {
            if rank < 0 || file >= 8 {
                return Err(format!("Invalid FEN: board position overflow at '{}'", ch));
            }
            let sq = Square::from_file_rank(file as u8, rank as u8);
            let color = if ch.is_uppercase() { Color::White } else { Color::Black };
            let piece = match ch.to_ascii_lowercase() {
                'k' => Piece::King,
                'q' => Piece::Queen,
                'r' => Piece::Rook,
                'b' => Piece::Bishop,
                'n' => Piece::Knight,
                'p' => Piece::Pawn,
                _ => return Err(format!("Invalid FEN: unknown piece character '{}'", ch)),
            };
            
            let bb = board::BitBoard::from_square(sq);
            pos.bb_pieces[color as usize][piece as usize] |= bb;
            pos.bb_sides[color as usize] |= bb;

            file += 1;
        }
    }

    // 2. Active Color
    pos.state.stm = match parts[1] {
        "w" => Color::White,
        "b" => Color::Black,
        _ => return Err("Invalid FEN: invalid active color.".to_string()),
    };

    // 3. Castling Rights
    pos.state.castling_rights = CastlingRights::none();
    for ch in parts[2].chars() {
        match ch {
            'K' => pos.state.castling_rights.insert(CastlingRights::WK),
            'Q' => pos.state.castling_rights.insert(CastlingRights::WQ),
            'k' => pos.state.castling_rights.insert(CastlingRights::BK),
            'q' => pos.state.castling_rights.insert(CastlingRights::BQ),
            '-' => {},
            _ => return Err("Invalid FEN: invalid castling rights.".to_string()),
        }
    }

    // 4. En Passant Square
    pos.state.en_passant_square = if parts[3] == "-" {
        None
    } else {
        let chars: Vec<char> = parts[3].chars().collect();
        if chars.len() != 2 {
            return Err("Invalid FEN: invalid en passant square.".to_string());
        }
        let file = (chars[0] as u8).wrapping_sub(b'a');
        let rank = (chars[1] as u8).wrapping_sub(b'1');
        if file > 7 || rank > 7 {
            return Err("Invalid FEN: invalid en passant square.".to_string());
        }
        Some(Square::from_file_rank(file, rank))
    };
    
    // 5. Halfmove Clock (optional)
    if let Some(part) = parts.get(4) {
        pos.state.ply_counter = part.parse().unwrap_or(0);
    }

    // 6. Fullmove Number is not implemented in your State struct, so we ignore it.

    Ok(pos)
}

pub fn position_to_fen(pos: &Position, fullmove_number: u32, half_moves:u32) -> String {
    // 1) Piece placement
    let mut placement = String::with_capacity(64 + 7); // rough
    for rank in (0..8).rev() {
        let mut empty_run = 0;

        for file in 0..8 {
            let sq = Square::from_file_rank(file, rank);
            let bit = board::BitBoard::from_square(sq).0;

            // Find which piece (if any) sits on `sq`
            let mut written = false;
            'color_loop: for color in [Color::White, Color::Black] {
                for piece in [
                    Piece::King,
                    Piece::Queen,
                    Piece::Rook,
                    Piece::Bishop,
                    Piece::Knight,
                    Piece::Pawn,
                ] {
                    if (pos.pieces(color, piece).0 & bit) != 0 {
                        // Flush empty squares run if any
                        if empty_run > 0 {
                            placement.push(char::from(b'0' + empty_run as u8));
                            empty_run = 0;
                        }
                        // Map piece to char
                        let base = match piece {
                            Piece::King   => 'K',
                            Piece::Queen  => 'Q',
                            Piece::Rook   => 'R',
                            Piece::Bishop => 'B',
                            Piece::Knight => 'N',
                            Piece::Pawn   => 'P',
                        };
                        placement.push(if color == Color::White { base } else { base.to_ascii_lowercase() });
                        written = true;
                        break 'color_loop;
                    }
                }
            }

            if !written {
                empty_run += 1;
            }
        }

        if empty_run > 0 {
            placement.push(char::from(b'0' + empty_run as u8));
        }
        if rank != 0 {
            placement.push('/');
        }
    }

    // 2) Active color
    let side_to_move = if pos.state.stm == Color::White { "w" } else { "b" };

    // 3) Castling rights
    let cr = pos.state.castling_rights.0;
    let mut castling = String::new();
    if (cr & CastlingRights::WK.0) != 0 { castling.push('K'); }
    if (cr & CastlingRights::WQ.0) != 0 { castling.push('Q'); }
    if (cr & CastlingRights::BK.0) != 0 { castling.push('k'); }
    if (cr & CastlingRights::BQ.0) != 0 { castling.push('q'); }
    if castling.is_empty() { castling.push('-'); }

    // 4) En passant target square
    let ep = match pos.state.en_passant_square {
        Some(sq) => format!("{}", sq),
        None => "-".to_string(),
    };

    // 5) Halfmove clock
    let halfmove = half_moves;

    // 6) Fullmove number (caller provides this)
    let fullmove = fullmove_number.to_string();

    format!("{placement} {side_to_move} {castling} {ep} {halfmove} {fullmove}")
}


// ############################
//      GRAPHICAL DISPLAY
// ############################

const WINDOW_SIZE: u32 = 640;
const SQUARE_SIZE: f64 = (WINDOW_SIZE / 8) as f64;

/// Opens a new window and displays the board and all legal moves as arrows.
pub fn display_moves(pos: &Position) {
    let mut window: PistonWindow = WindowSettings::new("Legal Moves", [WINDOW_SIZE, WINDOW_SIZE])
        .exit_on_esc(true)
        .build()
        .unwrap();

    // Load assets (e.g., piece images). This path assumes an 'assets' folder next to 'src'.
    let assets = find_folder::Search::ParentsThenKids(3, 3).for_folder("assets").unwrap();
    let piece_map = load_piece_textures(&mut window, &assets);

    let mut moves = MoveList::new();
    moves::generate_legal_moves(pos, &mut moves);

    while let Some(e) = window.next() {
        window.draw_2d(&e, |c, g, _| {
            clear([1.0; 4], g); // Clear screen with white

            draw_board(&c, g);
            draw_pieces(pos, &piece_map, &c, g);
            draw_move_arrows(moves.iter(), &c, g);
        });
    }
}

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

            rectangle(
                color,
                [x, y, SQUARE_SIZE, SQUARE_SIZE],
                c.transform,  // this is valid now
                g,
            );
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
    I: Iterator<Item = &'a Move>,
{
    let arrow_color = [0.2, 0.5, 0.2, 0.7]; // A semi-transparent green
    
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

        draw_arrow(x1, y1, x2, y2, arrow_color, c, g);
    }
}

// Helper to draw a line with an arrowhead
fn draw_arrow(x1: f64, y1: f64, x2: f64, y2: f64, color: [f32; 4], c: &Context, g: &mut G2d) {
    line(color, 4.0, [x1, y1, x2, y2], c.transform, g);
    
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

    polygon(color, &[[x2, y2], p1, p2], c.transform, g);
}