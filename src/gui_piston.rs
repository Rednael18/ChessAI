use piston_window::*;
use std::collections::HashMap;
use std::fmt;
use std::sync::{Arc, RwLock, mpsc};
use std::sync::atomic::{AtomicI32, Ordering};
use std::thread;
use std::time::Duration;
use rand::Rng;



use crate::search::eval::{map_double_pawn_eval, map_double_pawn_eval_alt, map_king_safety, map_king_safety_optimized};
use crate::search::search::{idwq_lmr, idwq_lmr_see, idwq_null_move, iterative_deepening_score};
#[allow(unused_imports)]
use crate::{game::{
    board::{enemy_attacks, Position},
    defs::{Color as GameColor, Piece, Square},
    history::History,
    moves::{generate_legal_moves, Move, MoveList},
    
}, search::{eval::{alternate_pesto, material_and_attack_eval, material_attack_pesto_eval, material_attack_position_eval, material_eval}}};
#[allow(unused_imports)]
use crate::search::{
    engine::parse_move,
    search::{complete_search, iterative_deepening, iterative_deepening_with_quiescence},
    transpose::{TranspositionTable, Zobrist, ZobristKeys},
};


// --------------------------- UI constants ---------------------------

const BOARD_SIZE: f64 = 800.0;
const SQUARE: f64 = BOARD_SIZE / 8.0;
const MARGIN: f64 = 40.0; // (880 - 800)/2

const COLOR_BG: [f32; 4] = [0.15, 0.15, 0.15, 1.0];
const LIGHT: [f32; 4] = [0.94, 0.85, 0.71, 1.0];
const DARK:  [f32; 4] = [0.71, 0.53, 0.39, 1.0];
const HL_YELLOW: [f32; 4] = [0.93, 0.93, 0.31, 0.45];
const DOT: [f32; 4] = [0.0, 0.0, 0.0, 0.28];
const TURN_GREEN: [f32; 4] = [0.2, 0.7, 0.2, 1.0];
const BUTTON_BG: [f32; 4] = [0.25, 0.25, 0.28, 1.0];
const BUTTON_FG: [f32; 4] = [0.95, 0.95, 0.95, 1.0];

const AI_COOLDOWN_S: f64 = 0.0001;  // small delay between AI plies
const ZSEED: u64 = 0xCAFE_F00D;


const SIDE_PANEL_W: f64 = 96.0; // width of the right sidebar
const WINDOW_W: f64 = MARGIN * 2.0 + BOARD_SIZE + SIDE_PANEL_W;
const WINDOW_H: f64 = MARGIN * 2.0 + BOARD_SIZE;

const EVAL_PANEL_BG: [f32; 4] = [0.12, 0.12, 0.13, 1.0];
const EVAL_WHITE:    [f32; 4] = [0.96, 0.96, 0.96, 1.0];
const EVAL_BLACK:    [f32; 4] = [0.10, 0.10, 0.10, 1.0];

const GUI_EVAL_SLICE_MS: i32 = 60;

#[inline]
fn eval_to_ratio(cp_white: i32) -> f64 {
    // treat +/- mate as hard extremes
    if cp_white <= -10_000 { return 0.0; }
    if cp_white >=  10_000 { return 1.0; }
    // 400 cp ~ 75/25
    let x = (cp_white as f64) / 400.0;
    1.0 / (1.0 + (-x).exp())
}



// --------------------------- Bot configuration ---------------------------

type EvalFn = fn(&Position) -> i32;
#[allow(clippy::type_complexity)]
type SearchFn = fn(
    &Position,
    Option<i32>,
    Option<i32>,
    EvalFn,
    bool,
    &mut Zobrist,
    &mut TranspositionTable
) -> Move;

#[derive(Clone, Copy, Debug)]
enum PlayWhite {
    Fixed(bool),
    Random,
}

#[derive(Clone, Copy)]
struct BotConfig {
    name: &'static str,
    depth: Option<i32>,
    time: Option<i32>,
    eval: EvalFn,
    eval_name: &'static str,
    search: SearchFn,
    search_name: &'static str,
    play_white: Option<PlayWhite>,
}

impl fmt::Display for BotConfig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let depth_str = match (self.depth, self.time) {
            (Some(d), _) => format!("depth={}", d),
            (None, Some(ms)) => format!("time={}ms", ms),
            (None, None) => "depth=-1".to_string(), // or whatever default you prefer
        };
        write!(
            f,
            "{}({depth_str}, eval={}, search={})\n\n",
            self.name, self.eval_name, self.search_name
        )
    }
}

fn default_bots() -> (BotConfig, BotConfig) {
    // Bot 1
    let (eval_name_1, eval_fn_1) = ("map_king_safety", map_king_safety);
    let (search_name_1, search_fn_1) = ("idwq_lmr", idwq_lmr);

    // Bot 2
    let (eval_name_2, eval_fn_2) = ("map_king_safety", map_king_safety);
    let (search_name_2, search_fn_2) = ("idwq_lmr_see", idwq_lmr_see);

    let bot1 = BotConfig {
        name: "Bot1",
        depth: None,
        time: Some(100),
        eval: eval_fn_1,
        eval_name: eval_name_1,
        search: search_fn_1,
        search_name: search_name_1,
        play_white: Some(PlayWhite::Random),
    };

    let bot2 = BotConfig {
        name: "Bot2",
        depth: None,
        time: Some(100),
        eval: eval_fn_2,
        eval_name: eval_name_2,
        search: search_fn_2,
        search_name: search_name_2,
        play_white: None,
    };

    (bot1, bot2)
}


const PLAY_ITERATIONS: usize = 50;


#[inline]
fn xorshift64star(seed: &mut u64) -> u64 {
    let mut x = *seed;
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    *seed = x;
    x.wrapping_mul(2685821657736338717)
}

#[inline]
fn choose_bot1_white(pw: PlayWhite, seed: &mut u64) -> bool {
    match pw {
        PlayWhite::Fixed(b) => b,
        PlayWhite::Random => (xorshift64star(seed) & 1) == 1,
    }
}

// --------------------------- Modes ---------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Mode {
    FreePlay,                    // user moves both sides
    HumanPlays(GameColor),       // user plays given color; engine plays other
    Simulated,                   // engine vs engine
}

// --------------------------- Small utils ---------------------------

#[inline] fn file_of(sq: Square) -> i32 { (sq.0 as i32) & 7 }
#[inline] fn rank_of(sq: Square) -> i32 { (sq.0 as i32) >> 3 }
#[inline] fn opp(c: GameColor) -> GameColor { if c == GameColor::White { GameColor::Black } else { GameColor::White } }

#[inline]
fn sq_from_file_rank(file: i32, rank: i32) -> Square {
    Square::from_file_rank(file as u8, rank as u8)
}

// a1 at bottom-left; screen y grows downward, so invert rank.
#[inline]
fn square_to_center_xy(sq: Square) -> (f64, f64) {
    let f = file_of(sq) as f64;
    let r = rank_of(sq) as f64;
    let x = MARGIN + f * SQUARE + SQUARE * 0.5;
    let y = MARGIN + (7.0 - r) * SQUARE + SQUARE * 0.5;
    (x, y)
}

#[inline]
fn cursor_to_square(x: f64, y: f64) -> Square {
    let fx = ((x - MARGIN) / SQUARE).floor() as i32;
    let fy_from_top = ((y - MARGIN) / SQUARE).floor() as i32;
    let file = fx.clamp(0, 7);
    let rank = (7 - fy_from_top).clamp(0, 7);
    sq_from_file_rank(file, rank)
}

#[inline]
fn pt_in_rect(x: f64, y: f64, r: [f64; 4]) -> bool {
    x >= r[0] && x <= r[0] + r[2] && y >= r[1] && y <= r[1] + r[3]
}

// --------------------------- Assets ---------------------------

fn load_textures(tcx: &mut G2dTextureContext) -> HashMap<String, G2dTexture> {
    let mut map = HashMap::new();
    let pieces = ["K","Q","R","N","B","P"];
    let colors = ["w","b"];
    let ts = TextureSettings::new();

    for c in colors {
        for p in pieces {
            let key = format!("{c}{p}");
            // Try assets/ first, then cwd
            let paths = [format!("assets/{key}.png"), format!("{key}.png")];
            let mut tex_opt = None;
            for path in &paths {
                if let Ok(t) = Texture::from_path(tcx, path, Flip::None, &ts) {
                    tex_opt = Some(t);
                    break;
                }
            }
            match tex_opt {
                Some(t) => { map.insert(key, t); }
                None => panic!("Missing piece texture for {key} (looked in assets/ and cwd)"),
            }
        }
    }
    map
}

// --------------------------- Drawing ---------------------------

fn draw_board<G: Graphics>(c: Context, g: &mut G) {
    clear(COLOR_BG, g);
    for rank in 0..8 {
        for file in 0..8 {
            let x = MARGIN + file as f64 * SQUARE;
            let y = MARGIN + (7 - rank) as f64 * SQUARE;
            let color = if (rank + file) % 2 == 0 { DARK } else { LIGHT };
            rectangle(color, [x, y, SQUARE, SQUARE], c.transform, g);
        }
    }
}

fn draw_last_move<G: Graphics>(c: Context, g: &mut G, mv: Option<(Square, Square)>) {
    if let Some((from, to)) = mv {
        for &sq in &[from, to] {
            let x = MARGIN + file_of(sq) as f64 * SQUARE;
            let y = MARGIN + (7 - rank_of(sq)) as f64 * SQUARE;
            rectangle(HL_YELLOW, [x, y, SQUARE, SQUARE], c.transform, g);
        }
    }
}

fn draw_turn_indicator<G: Graphics>(c: Context, g: &mut G, stm: GameColor) {
    let y = if stm == GameColor::White {
        MARGIN + 7.0 * SQUARE + SQUARE * 0.5
    } else {
        MARGIN + SQUARE * 0.5
    };
    rectangle(TURN_GREEN, [MARGIN - 24.0, y - 10.0, 20.0, 20.0], c.transform, g);
}

fn draw_move_dots<G: Graphics>(c: Context, g: &mut G, targets: &[Square]) {
    for &sq in targets {
        let (cx, cy) = square_to_center_xy(sq);
        let r = SQUARE * 0.19;
        ellipse(DOT, [cx - r, cy - r, 2.0 * r, 2.0 * r], c.transform, g);
    }
}

fn piece_key(color: GameColor, piece: Piece) -> &'static str {
    match (color, piece) {
        (GameColor::White, Piece::King)   => "wK",
        (GameColor::White, Piece::Queen)  => "wQ",
        (GameColor::White, Piece::Rook)   => "wR",
        (GameColor::White, Piece::Knight) => "wN",
        (GameColor::White, Piece::Bishop) => "wB",
        (GameColor::White, Piece::Pawn)   => "wP",
        (GameColor::Black, Piece::King)   => "bK",
        (GameColor::Black, Piece::Queen)  => "bQ",
        (GameColor::Black, Piece::Rook)   => "bR",
        (GameColor::Black, Piece::Knight) => "bN",
        (GameColor::Black, Piece::Bishop) => "bB",
        (GameColor::Black, Piece::Pawn)   => "bP",
    }
}

fn iter_pieces(pos: &Position) -> Vec<(GameColor, Piece, Square)> {
    let mut out = Vec::with_capacity(32);
    for &c in &[GameColor::White, GameColor::Black] {
        for &p in &[Piece::King, Piece::Queen, Piece::Rook, Piece::Knight, Piece::Bishop, Piece::Pawn] {
            let bb = pos.pieces(c, p);
            for sq in bb.iter_squares() {
                out.push((c, p, sq));
            }
        }
    }
    out
}

fn draw_pieces<G: Graphics<Texture = G2dTexture>>(
    c: Context,
    g: &mut G,
    textures: &HashMap<String, G2dTexture>,
    pos: &Position,
    dragged_from: Option<Square>,
    cursor_xy: Option<[f64; 2]>,
) {
    // draw all except dragged
    for (color, p, sq) in iter_pieces(pos) {
        if Some(sq) == dragged_from { continue; }
        let key = piece_key(color, p);
        let tex = &textures[key];
        let (cx, cy) = square_to_center_xy(sq);
        let img = Image::new().rect([cx - SQUARE * 0.5, cy - SQUARE * 0.5, SQUARE, SQUARE]);
        img.draw(tex, &c.draw_state, c.transform, g);
    }
    // draw dragged on top at cursor
    if let (Some(from_sq), Some([mx, my])) = (dragged_from, cursor_xy) {
        if let Some((color, piece)) = pos.piece_at(from_sq) {
            let key = piece_key(color, piece);
            let tex = &textures[key];
            let img = Image::new().rect([mx - SQUARE * 0.5, my - SQUARE * 0.5, SQUARE, SQUARE]);
            img.draw(tex, &c.draw_state, c.transform, g);
        }
    }
}

fn draw_eval_panel<G: Graphics<Texture = G2dTexture>>(
     c: Context,
     g: &mut G,
     score_cp: i32,
     glyphs_opt: Option<&mut Glyphs>,
 ) {
    let x0 = MARGIN + BOARD_SIZE + 8.0;
    let y0 = MARGIN;
    let w  = SIDE_PANEL_W - 16.0;
    let h  = BOARD_SIZE;

    // panel background
    rectangle(EVAL_PANEL_BG, [x0 - 4.0, y0, w + 8.0, h], c.transform, g);

    // split (white on top, black on bottom)
    let r = eval_to_ratio(score_cp).clamp(0.0, 1.0);
    let white_h = h * r;
    let black_h = h - white_h;

    rectangle(EVAL_WHITE, [x0, y0,        w, white_h], c.transform, g);
    rectangle(EVAL_BLACK, [x0, y0+white_h, w, black_h], c.transform, g);

    // cp label (white POV, 2 decimals)
    if let Some(glyphs) = glyphs_opt {
        let txt = text::Text::new_color(BUTTON_FG, 16);
        let disp = (score_cp as f64) / 100.0;
        let s = if disp >= 0.0 { format!("+{disp:.2}") } else { format!("{disp:.2}") };
        // Draw near bottom for readability
        let tx = x0 + 8.0;
        let ty = y0 + h - 12.0;
        let _ = txt.draw(&s, glyphs, &c.draw_state, c.transform.trans(tx, ty), g);
    }
}


// --------------------------- Controls ---------------------------

struct UiButtons {
    left: [f64; 4],
    right: [f64; 4],
    undo: [f64; 4],
    play: [f64; 4], // visible only in Simulated
}

fn buttons_layout() -> UiButtons {
    let y = 8.0;
    let w = 40.0;
    let h = 28.0;
    let gap = 10.0;
    let left  = [MARGIN, y, w, h];
    let right = [MARGIN + w + gap, y, w, h];
    let undo  = [MARGIN + 2.0*(w + gap), y, 80.0, h];
    let play  = [undo[0] + undo[2] + gap, y, 86.0, h];
    UiButtons { left, right, undo, play }
}

fn draw_controls<G: Graphics<Texture = G2dTexture>>(
    c: Context,
    g: &mut G,
    btns: &UiButtons,
    glyphs_opt: Option<&mut Glyphs>,
    mode: Mode,
    sim_running: bool,
) {
    // Base buttons
    for r in [&btns.left, &btns.right, &btns.undo] {
        rectangle(BUTTON_BG, *r, c.transform, g);
    }
    // Left arrow triangle
    let l = &btns.left;
    let tri_left = [
        [l[0] + 10.0, l[1] + l[3] * 0.5],
        [l[0] + l[2] - 10.0, l[1] + 8.0],
        [l[0] + l[2] - 10.0, l[1] + l[3] - 8.0],
    ];
    polygon(BUTTON_FG, &tri_left, c.transform, g);

    // Right arrow triangle
    let r = &btns.right;
    let tri_right = [
        [r[0] + r[2] - 10.0, r[1] + r[3] * 0.5],
        [r[0] + 10.0, r[1] + 8.0],
        [r[0] + 10.0, r[1] + r[3] - 8.0],
    ];
    polygon(BUTTON_FG, &tri_right, c.transform, g);

    // Play/Pause background (only in Simulated) — draw BEFORE borrowing glyphs
    if mode == Mode::Simulated {
        let bg = if sim_running { [0.20,0.45,0.20,1.0] } else { BUTTON_BG };
        rectangle(bg, btns.play, c.transform, g);
    }

    // Single borrow of glyphs to render all labels
    if let Some(glyphs) = glyphs_opt {
        let txt = text::Text::new_color(BUTTON_FG, 14);

        // Undo label
        let tx = btns.undo[0] + 10.0;
        let ty = btns.undo[1] + btns.undo[3] * 0.72;
        let _ = txt.draw("Undo", glyphs, &c.draw_state, c.transform.trans(tx, ty), g);

        // Play/Pause label (if Simulated)
        if mode == Mode::Simulated {
            let label = if sim_running { "Pause ⏸" } else { "Play ▶" };
            let txp = btns.play[0] + 10.0;
            let typ = btns.play[1] + btns.play[3] * 0.72;
            let _ = txt.draw(label, glyphs, &c.draw_state, c.transform.trans(txp, typ), g);
        }
    }
}

// --------------------------- Game end (mate/stalemate) ---------------------------

#[derive(Clone, Copy)]
enum GameEnd { Checkmate, Stalemate, Threefold }

fn compute_game_end(pos: &Position) -> Option<GameEnd> {
    let mut moves = MoveList::new();
    if generate_legal_moves(pos, &mut moves) > 0 {
        return None;
    }
    let stm = pos.state.stm;
    let them = opp(stm);
    let occ = pos.occupied();
    let attacks = enemy_attacks(pos, them, occ);
    let kbb = pos.pieces(stm, Piece::King);
    
    if (attacks.0 & kbb.0) != 0 { Some(GameEnd::Checkmate) } else { Some(GameEnd::Stalemate) }
}
fn draw_game_end<G: Graphics<Texture = G2dTexture>>(
    c: Context, g: &mut G, end: GameEnd, glyphs_opt: Option<&mut Glyphs>
) {
    let (color, msg) = match end {
        GameEnd::Checkmate => ([0.8, 0.1, 0.1, 0.45], "CHECKMATE"),
        GameEnd::Stalemate => ([0.1, 0.1, 0.8, 0.45], "STALEMATE"),
        GameEnd::Threefold => ([0.1, 0.8, 0.1, 0.45], "DRAW BY REPETITION"),
    };
    rectangle(color, [MARGIN, MARGIN, BOARD_SIZE, BOARD_SIZE], c.transform, g);

    if let Some(glyphs) = glyphs_opt {
        let txt = text::Text::new_color([1.0, 1.0, 1.0, 1.0], 28);
        let tx = MARGIN + BOARD_SIZE * 0.5 - 160.0;
        let ty = MARGIN + BOARD_SIZE * 0.5 + 10.0;
        let _ = txt.draw(msg, glyphs, &c.draw_state, c.transform.trans(tx, ty), g);
    }
}

#[inline]
fn is_threefold(hashes: &[u64], idx: usize) -> bool {
    let h = hashes[idx];
    let mut count = 0u8;
    for &x in &hashes[..=idx] {
        if x == h {
            count += 1;
            if count >= 3 { return true; }
        }
    }
    false
}

// --------------------------- Mode policies ---------------------------

fn at_tip(timeline_len: usize, idx: usize) -> bool {
    idx + 1 == timeline_len
}

fn can_human_move(mode: Mode, stm: GameColor, _at_tip: bool, game_over: bool) -> bool {
    // Allow human to branch from any point in history; future will be truncated on move.
    if game_over { return false; }
    match mode {
        Mode::FreePlay => true,
        Mode::HumanPlays(c) => stm == c,
        Mode::Simulated => false,
    }
}

fn should_ai_move(mode: Mode, stm: GameColor, at_tip: bool, sim_running: bool, game_over: bool) -> bool {
    if !at_tip || game_over { return false; }
    match mode {
        Mode::FreePlay      => false,
        Mode::HumanPlays(c) => stm != c,
        Mode::Simulated     => sim_running, // only when Play is ON
    }
}

// --------------------------- Move / timeline helpers ---------------------------

fn apply_move_from(pos: &Position, mv: Move, zseed: u64, zobrist: &mut Zobrist) -> Position {
    let mut h = History::new(pos.clone());
    *zobrist = Zobrist::from_position(ZobristKeys::new_with_seed(zseed), pos);
    h.apply(mv, zobrist);
    h.current().clone()
}

// --------------------------- Simulation bookkeeping ---------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WhichBot { B1, B2 }

#[derive(Clone, Copy, Debug)]
struct Pairing { white: WhichBot, black: WhichBot }

impl Pairing {
    fn bot_for_color(&self, c: GameColor) -> WhichBot { if c == GameColor::White { self.white } else { self.black } }
}

struct SimStats {
    games: usize,
    b1_wins: usize,
    b2_wins: usize,
    stalemates: usize,
    threefolds: usize,
    total_plies: usize,
    logs: Vec<String>,
}
impl SimStats {
    fn new() -> Self { Self { games: 0, b1_wins: 0, b2_wins: 0, stalemates: 0, threefolds: 0, total_plies: 0, logs: Vec::new() } }
}

// --------------------------- Main runner ---------------------------


fn spawn_eval_worker(
    shared_pos: Arc<RwLock<Position>>,
    period_ms: u64,
    zseed: u64
) -> Arc<AtomicI32> {
    let score_atomic = Arc::new(AtomicI32::new(0));
    let out = Arc::clone(&score_atomic);

    thread::spawn(move || {
        let mut last_hash: u64 = 0;
        let mut tt = TranspositionTable::new();

        loop {
            let p = { shared_pos.read().unwrap().clone() };

            // Build Zobrist fresh each loop (no moved-keys problem)
            let mut z = Zobrist::from_position(ZobristKeys::new_with_seed(zseed), &p);

            if z.hash != last_hash {
                tt = TranspositionTable::new();
                last_hash = z.hash;
            }

            // Timed iterative-deepening score (side-to-move POV)
            let sc_stm = iterative_deepening_score(
                &p,
                GUI_EVAL_SLICE_MS,
                material_attack_pesto_eval,
                &mut z,
                &mut tt,
            );

            // Convert to White POV for the bar
            let sc_white = if p.state.stm == GameColor::White { sc_stm } else { -sc_stm };
            out.store(sc_white, Ordering::Relaxed);

            if period_ms > 0 {
                thread::sleep(Duration::from_millis(period_ms));
            }
        }
    });

    score_atomic
}



fn run_with_mode(mode: Mode, title: &str) {
    // Window
    let mut window: PistonWindow = WindowSettings::new(title, [WINDOW_W, WINDOW_H])
        .exit_on_esc(true)
        .samples(0)
        .resizable(false)
        .build()
        .expect("failed to create window");
    window.set_ups(60);


    // Assets
    let mut tex_ctx = window.create_texture_context();
    let textures = load_textures(&mut tex_ctx);

    // Optional font for labels / messages
    let mut glyphs: Option<Glyphs> = window.load_font("assets/DejaVuSans.ttf").ok();

    // Bots and simulation config
    let (bot1, bot2) = default_bots();
    let play_iterations = PLAY_ITERATIONS.max(1);

    // Who is white this game? (only bot1's play_white controls this)
    let mut rng_thread = rand::thread_rng();
    let mut rng_seed = ZSEED ^ 0x9E37_79B9_7F4A_7C15 + rng_thread.gen_range(0u64..10_000u64);
    let mut game_zseed = ZSEED ^ xorshift64star(&mut rng_seed);

    let mut pairing = {
        let b1_white = match bot1.play_white.unwrap_or(PlayWhite::Fixed(true)) {
            PlayWhite::Fixed(b) => b,
            PlayWhite::Random => choose_bot1_white(PlayWhite::Random, &mut rng_seed),
        };
        if b1_white { Pairing { white: WhichBot::B1, black: WhichBot::B2 } }
        else { Pairing { white: WhichBot::B2, black: WhichBot::B1 } }
    };

    // Game state: timeline of positions + parallel moves list
    let mut zobrist = Zobrist::from_position(ZobristKeys::new_with_seed(game_zseed), &Position::default());
    let mut tt = TranspositionTable::new();

    let mut timeline: Vec<Position> = vec![Position::default()];
    let mut hashes: Vec<u64> = vec![zobrist.hash];
    let mut moves_between: Vec<(Square, Square)> = Vec::new(); // len = timeline.len() - 1
    let mut idx: usize = 0;

    // Share the display position with the background eval worker
    let shared_pos = Arc::new(RwLock::new(Position::default()));
    let sync_eval_position = |p: &Position| {
        *shared_pos.write().unwrap() = p.clone();
    };
    sync_eval_position(&timeline[0]);

    // Start the always-on evaluator (choose the eval you want to *display*)
    let eval_score = spawn_eval_worker(Arc::clone(&shared_pos), 10, game_zseed);

    // Engine worker channel (GUI polls this without blocking)
    let (eng_tx, eng_rx) = mpsc::channel::<Move>();
    let mut engine_thinking: bool = false;

    // UI state
    let mut cursor_xy: [f64; 2] = [MARGIN + SQUARE * 3.5, MARGIN + SQUARE * 3.5];
    let mut dragged_from: Option<Square> = None;
    let mut legal_targets: Vec<Square> = Vec::new();
    let mut last_move: Option<(Square, Square)> = None;
    let btns = buttons_layout();
    let mut ai_cooldown = 0.0_f64;

    // Simulated-mode play/pause (starts paused)
    let mut sim_running: bool = false;

    // Multi-iteration bookkeeping
    let mut processed_this_game: bool = false;
    let mut stats = SimStats::new();

    // Resets everything for a fresh game and (re)decides the pairing if needed
    let reset_for_next_game = |timeline: &mut Vec<Position>,
                                moves_between: &mut Vec<(Square, Square)>,
                                hashes: &mut Vec<u64>,
                                idx: &mut usize,
                                zobrist: &mut Zobrist,
                                tt: &mut TranspositionTable,
                                last_move: &mut Option<(Square,Square)>,
                                processed_this_game: &mut bool,
                                pairing: &mut Pairing,
                                game_zseed: &mut u64,                 // NEW
                                rng_seed: &mut u64,                   // NEW
                                rng_thread: &mut rand::rngs::ThreadRng| // NEW
    {
        *zobrist = Zobrist::from_position(ZobristKeys::new_with_seed(*game_zseed), &Position::default());
        *tt = TranspositionTable::new();

        // advance both seeds, then derive the next per-game seed
        *rng_seed = (*game_zseed) ^ 0x9E37_79B9_7F4A_7C15
                + rng_thread.gen_range(0u64..10_000u64);
        *game_zseed ^= xorshift64star(rng_seed);

        *zobrist = Zobrist::from_position(ZobristKeys::new_with_seed(*game_zseed), &Position::default());
        timeline.clear(); timeline.push(Position::default());
        hashes.clear(); hashes.push(zobrist.hash);
        moves_between.clear();
        *idx = 0;
        *last_move = None;
        *processed_this_game = false;

        if matches!(bot1.play_white, Some(PlayWhite::Random)) {
            let b1_white = choose_bot1_white(PlayWhite::Random, rng_seed);
            *pairing = if b1_white { Pairing { white: WhichBot::B1, black: WhichBot::B2 } }
                    else        { Pairing { white: WhichBot::B2, black: WhichBot::B1 } };
        }
        sync_eval_position(&timeline[*idx]);
    };


    while let Some(e) = window.next() {
        // Update timer (for AI pacing)
        if let Some(u) = e.update_args() {
            ai_cooldown = (ai_cooldown - u.dt).max(0.0);
        }

        // Snapshot the current position to avoid borrowing timeline during mutations
        let pos = timeline[idx].clone();
        let stm = pos.state.stm;
        let end = compute_game_end(&pos);
        let threefold = is_threefold(&hashes, idx);
        let game_over = end.is_some() || threefold;
        let tip = at_tip(timeline.len(), idx);

        // If a move finished in the worker, apply it now
        if let Ok(mv) = eng_rx.try_recv() {
            engine_thinking = false;
            // Apply the move exactly like before
            let new_pos = apply_move_from(&pos, mv, game_zseed, &mut zobrist);
            if !tip { timeline.truncate(idx + 1); moves_between.truncate(idx); hashes.truncate(idx + 1); }
            timeline.push(new_pos);
            moves_between.push((mv.from_sq(), mv.to_sq()));
            hashes.push(zobrist.hash);
            idx += 1;
            last_move = Some((mv.from_sq(), mv.to_sq()));
            ai_cooldown = AI_COOLDOWN_S;

            // keep the eval worker in sync with what’s on screen
            sync_eval_position(&timeline[idx]);
        }


        // If a simulated game is finished, handle match flow here (once)
        if mode == Mode::Simulated && tip && sim_running && game_over && !processed_this_game {
            processed_this_game = true; // guard
            let plies = idx; // number of half-moves played
            stats.total_plies += plies;

            let winner_bot_opt = if threefold {
                stats.threefolds += 1; None
            } else if let Some(GameEnd::Stalemate) = end {
                stats.stalemates += 1; None
            } else {
                // Checkmate: winner is the side that is not on move
                let winner_color = opp(stm);
                let wb = pairing.bot_for_color(winner_color);
                match wb {
                    WhichBot::B1 => { stats.b1_wins += 1; }
                    WhichBot::B2 => { stats.b2_wins += 1; }
                }
                Some(wb)
            };

            // Build game log
            let white_desc = match pairing.white { WhichBot::B1 => format!("{}", bot1), WhichBot::B2 => format!("{}", bot2) };
            let black_desc = match pairing.black { WhichBot::B1 => format!("{}", bot1), WhichBot::B2 => format!("{}", bot2) };
            let result_pretty = if threefold { "1/2-1/2 (threefold)".to_string() }
                                else if let Some(GameEnd::Stalemate) = end { "1/2-1/2 (stalemate)".to_string() }
                                else {
                                    match pairing.white {
                                        WhichBot::B1 => if winner_bot_opt == Some(WhichBot::B1) { "1-0" } else { "0-1" },
                                        WhichBot::B2 => if winner_bot_opt == Some(WhichBot::B2) { "1-0" } else { "0-1" },
                                    }.to_string()
                                };
            let moves_full = plies / 2;
            let log = format!(
                "Game {}: White = {white_desc}; Black = {black_desc} — result {result_pretty} in {plies} plies (~{moves_full} moves)",
                stats.games + 1
            );
            stats.logs.push(log);
            stats.games += 1;

            if stats.games < play_iterations {
                println!("{}", stats.logs.pop().unwrap());
                // Start next game immediately
                reset_for_next_game(
                    &mut timeline, &mut moves_between, &mut hashes, &mut idx,
                    &mut zobrist, &mut tt, &mut last_move, &mut processed_this_game,
                    &mut pairing, &mut game_zseed, &mut rng_seed, &mut rng_thread
                );
            } else {
                // Match is complete — print report and stop autoplay
                println!("================ Match complete ================");
                println!("Bots: {} (B1)  vs  {} (B2)", bot1, bot2);
                println!("Side assignment: Bot1 play_white = {:?}", bot1.play_white);
                for entry in &stats.logs { println!("{entry}"); }
                let draws = stats.stalemates + stats.threefolds;
                println!("-----------------------------------------------");
                println!("Games: {}", stats.games);
                println!("Bot1 wins: {}", stats.b1_wins);
                println!("Bot2 wins: {}", stats.b2_wins);
                println!("Draws: {} (stalemate: {}, threefold: {})", draws, stats.stalemates, stats.threefolds);
                let avg_plies = if stats.games > 0 { stats.total_plies as f64 / stats.games as f64 } else { 0.0 };
                println!("Avg. plies per game: {:.2}", avg_plies);
                println!("===============================================");
                sim_running = false; // stop autoplay; board shows last result
            }
        }

        // AI step (only when allowed, at tip, not game-over, and cooldown elapsed)
        if ai_cooldown <= 0.0
            && should_ai_move(mode, stm, tip, sim_running, game_over)
            && dragged_from.is_none()
            && !engine_thinking
        {
            let active_bot = match mode {
                Mode::HumanPlays(_) => bot1,
                Mode::Simulated => match pairing.bot_for_color(stm) {
                    WhichBot::B1 => bot1,
                    WhichBot::B2 => bot2,
                },
                Mode::FreePlay => unreachable!(),
            };

            let tx = eng_tx.clone();
            let pos_for_engine = pos.clone();
            thread::spawn(move || {
                let mut zloc = Zobrist::from_position(ZobristKeys::new_with_seed(game_zseed), &pos_for_engine);
                let mut ttloc = TranspositionTable::new();
                let mv = (active_bot.search)(
                    &pos_for_engine,
                    active_bot.depth,
                    active_bot.time,
                    active_bot.eval,
                    true,
                    &mut zloc,
                    &mut ttloc
                );
                let _ = tx.send(mv);
            });
            engine_thinking = true;
        }

        if let Some(pos_xy) = e.mouse_cursor_args() {
            cursor_xy = [pos_xy[0], pos_xy[1]];
        }

        // Keyboard: Space toggles play/pause in Simulated
        if let Some(Button::Keyboard(Key::Space)) = e.press_args() {
            if mode == Mode::Simulated && !(end.is_some() || threefold) {
                sim_running = !sim_running;
            }
        }

        // Mouse press: buttons first, then drag-start
        if let Some(Button::Mouse(MouseButton::Left)) = e.press_args() {
            let [mx, my] = cursor_xy;

            // Buttons
            if pt_in_rect(mx, my, btns.left) {
                if idx > 0 {
                    idx -= 1;
                    last_move = if idx > 0 { Some(moves_between[idx - 1]) } else { None };
                    dragged_from = None;
                    legal_targets.clear();
                    zobrist = Zobrist::from_position(ZobristKeys::new_with_seed(game_zseed), &timeline[idx]);
                    sync_eval_position(&timeline[idx]);
                }
            } else if pt_in_rect(mx, my, btns.right) {
                if idx + 1 < timeline.len() {
                    idx += 1;
                    last_move = Some(moves_between[idx - 1]);
                    dragged_from = None;
                    legal_targets.clear();
                    zobrist = Zobrist::from_position(ZobristKeys::new_with_seed(game_zseed), &timeline[idx]);
                    sync_eval_position(&timeline[idx]);
                }
            } else if pt_in_rect(mx, my, btns.undo) {
                if idx > 0 {
                    idx -= 1;
                    last_move = if idx > 0 { Some(moves_between[idx - 1]) } else { None };
                    dragged_from = None;
                    legal_targets.clear();
                    zobrist = Zobrist::from_position(ZobristKeys::new_with_seed(game_zseed), &timeline[idx]);
                    sync_eval_position(&timeline[idx]);
                }
            } else if mode == Mode::Simulated && pt_in_rect(mx, my, btns.play) {
                if !(end.is_some() || threefold) {
                    sim_running = !sim_running;
                }
            } else {
                // Drag start (only allowed side, and not game over)
                if can_human_move(mode, stm, tip, game_over) {
                    let sq = cursor_to_square(mx, my);
                    if let Some((c, _p)) = pos.piece_at(sq) {
                        if c == stm {
                            dragged_from = Some(sq);
                            // collect legal targets
                            legal_targets.clear();
                            let mut moves = MoveList::new();
                            generate_legal_moves(&pos, &mut moves);
                            for m in moves.iter() {
                                if m.from_sq() == sq {
                                    legal_targets.push(m.to_sq());
                                }
                            }
                        }
                    }
                }
            }
        }

        // Drag release -> attempt human move
        if let Some(Button::Mouse(MouseButton::Left)) = e.release_args() {
            if let Some(from_sq) = dragged_from.take() {
                legal_targets.clear();

                // Only accept human move if allowed right now (turn + not over)
                if can_human_move(mode, stm, tip, game_over) {
                    let to_sq = cursor_to_square(cursor_xy[0], cursor_xy[1]);
                    if let Some(mv) = parse_move(&pos, &format!("{}{}", from_sq, to_sq)) {
                        let new_pos = apply_move_from(&pos, mv, game_zseed, &mut zobrist);
                        if !tip { timeline.truncate(idx + 1); moves_between.truncate(idx); hashes.truncate(idx + 1); }
                        timeline.push(new_pos);
                        moves_between.push((mv.from_sq(), mv.to_sq()));
                        hashes.push(zobrist.hash);
                        idx += 1;
                        last_move = Some((from_sq, to_sq));
                        ai_cooldown = AI_COOLDOWN_S;
                        sync_eval_position(&timeline[idx]);
                    }
                }
            }
        }

        // Draw
        window.draw_2d(&e, |c, g, device| {
            draw_board(c, g);
            draw_last_move(c, g, last_move);
            draw_move_dots(c, g, &legal_targets);
            draw_pieces(c, g, &textures, &timeline[idx], dragged_from, Some(cursor_xy));
            draw_turn_indicator(c, g, timeline[idx].state.stm);

            let glyphs_mut = glyphs.as_mut();
            draw_controls(c, g, &btns, glyphs_mut, mode, sim_running);

            let glyphs_mut = glyphs.as_mut();
            let cp_now = eval_score.load(Ordering::Relaxed);
            draw_eval_panel(c, g, cp_now, glyphs_mut);


            // Game end overlay: Threefold first, then checkmate/stalemate
            if is_threefold(&hashes, idx) {
                let glyphs_mut = glyphs.as_mut();
                draw_game_end(c, g, GameEnd::Threefold, glyphs_mut);
            } else if let Some(end) = compute_game_end(&timeline[idx]) {
                let glyphs_mut = glyphs.as_mut();
                draw_game_end(c, g, end, glyphs_mut);
            }

            if let Some(ref mut gl) = glyphs {
                gl.factory.encoder.flush(device);
            }
        });
    }
}

// --------------------------- Public entry points ---------------------------

/// Free play: user can move both sides.

#[allow(dead_code)]
pub fn run() {
    run_with_mode(Mode::FreePlay, "Rust Chess (free play)");
}

/// User plays White; engine plays Black.
#[allow(dead_code)]
pub fn play_white() {
    run_with_mode(Mode::HumanPlays(GameColor::White), "Rust Chess (you = White)");
}

/// User plays Black; engine plays White.
#[allow(dead_code)]
pub fn play_black() {
    run_with_mode(Mode::HumanPlays(GameColor::Black), "Rust Chess (you = Black)");
}

/// Engine vs engine (Play/Pause controls; starts paused). If `PLAY_ITERATIONS >= 2`,
/// a multi-game match is played and a terminal report is printed when it finishes.
#[allow(dead_code)]
pub fn play_simulated() {
    run_with_mode(Mode::Simulated, "Rust Chess (simulated)");
}
