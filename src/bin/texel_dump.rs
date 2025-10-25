//! texel_dump.rs
//! Reads a PGN file and emits training samples for Texel-style tuning.
//! - Uses `pgn-reader` (0.26) + `shakmaty`
//! - Converts every encountered position to a **side-to-move ("my") perspective**
//! - Progress bar via `indicatif`
//! - A few `const` knobs near the top to tune behavior
//!
//! Output format: a continuous stream of bincode-serialized `DumpRow`
//! (read back by repeatedly calling `bincode::deserialize_from` until EOF).
//!
//! NOTE: This version stores two extra fields per row:
//!   - `mover_is_white`: who was to move before orientation (true=White)
//!   - `was_mirrored`: whether the position was vertically mirrored
//! These allow a viewer to reconstruct the absolute position exactly.
//!
//! CLI: `cargo run --bin texel_dump -- <input.pgn> <output.bin>`

use std::fs::File;
use std::io::{BufReader, BufWriter, Write};
use std::path::PathBuf;

use anyhow::{Context, Result};
use indicatif::{ProgressBar, ProgressStyle};
use pgn_reader::{BufferedReader, RawHeader, SanPlus, Skip, Visitor};
use serde::{Deserialize, Serialize};
use shakmaty::fen::Fen;
use shakmaty::{Board, CastlingMode, Color as ShColor, EnPassantMode, Position as _, Role, Chess};

// ===================== Tunables =====================
/// Sample only every Nth ply (1 = take every ply)
const SAMPLE_EVERY_NTH_PLY: usize = 1;
/// If true, skip games with unknown/"*" result
const SKIP_UNKNOWN_RESULT: bool = true;
/// If true and it is Black to move, mirror the board vertically so the mover is always "from the bottom"
const MIRROR_FOR_BLACK: bool = true;
/// Stop after N games (None = all)
const LIMIT_GAMES: Option<usize> = None;
/// Ignore very short games (based on plies we actually managed to parse)
const SKIP_SHORT_GAMES: bool = true;
/// "Short" means strictly less than this many plies
const MIN_PLIES: u32 = 12;
/// Show a progress bar (spinner or counter when LIMIT_GAMES is Some)
const SHOW_PROGRESS: bool = true;
/// En-passant mode for shakmaty
const EP_MODE: EnPassantMode = EnPassantMode::Legal;

// ===================== Data =====================
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DumpRow {
    /// 12 oriented bitboards:
    ///   [my P, my N, my B, my R, my Q, my K, opp P, opp N, opp B, opp R, opp Q, opp K]
    pub bb: [u64; 12],
    /// Castling rights bitfield (KQkq = 1|2|4|8) for the real board (not relabeled)
    pub castling: u8,
    /// En-passant target square (0..63, a1 = 0). None if N/A.
    pub ep: Option<u8>,
    /// Result from the *side-to-move* perspective: -1.0 loss, 0.0 draw, +1.0 win
    pub y: f32,
    /// Who was to move in the *absolute* position (before orientation)
    pub mover_is_white: bool,
    /// Whether we mirrored vertically (i.e., MIRROR_FOR_BLACK && mover_is_white == false)
    pub was_mirrored: bool,
}

// =============== Helpers: orientation & extraction ===============
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

fn castling_bits(ch: &Chess) -> u8 {
    use shakmaty::CastlingSide;
    let cr = ch.castles();
    let mut bits = 0u8;
    if cr.has(ShColor::White, CastlingSide::KingSide) {
        bits |= 1;
    }
    if cr.has(ShColor::White, CastlingSide::QueenSide) {
        bits |= 2;
    }
    if cr.has(ShColor::Black, CastlingSide::KingSide) {
        bits |= 4;
    }
    if cr.has(ShColor::Black, CastlingSide::QueenSide) {
        bits |= 8;
    }
    bits
}

fn extract_bb(ch: &Chess, role: Role, color: ShColor) -> u64 {
    let board: &Board = ch.board();
    let bitboard = board.by_role(role) & board.by_color(color);
    let mut out = 0u64;
    for sq in bitboard {
        let idx: u8 = sq as u8; // Square is a C-like enum in shakmaty
        out |= 1u64 << idx;
    }
    out
}

fn ep_square_index(ch: &Chess) -> Option<u8> {
    ch.ep_square(EP_MODE).map(|sq| sq as u8)
}

/// Build the 12 piece bitboards in [my.., opp..] order, in the mover's perspective.
fn oriented_bitboards(ch: &Chess) -> [u64; 12] {
    let stm = ch.turn();

    let pack = |color_my: ShColor| -> [u64; 6] {
        [
            extract_bb(ch, Role::Pawn, color_my),
            extract_bb(ch, Role::Knight, color_my),
            extract_bb(ch, Role::Bishop, color_my),
            extract_bb(ch, Role::Rook, color_my),
            extract_bb(ch, Role::Queen, color_my),
            extract_bb(ch, Role::King, color_my),
        ]
    };

    let (mut mine, mut theirs) = match stm {
        ShColor::White => (pack(ShColor::White), pack(ShColor::Black)),
        ShColor::Black => (pack(ShColor::Black), pack(ShColor::White)),
    };

    if MIRROR_FOR_BLACK && matches!(stm, ShColor::Black) {
        for bb in mine.iter_mut().chain(theirs.iter_mut()) {
            *bb = mirror_vertical_u64(*bb);
        }
    }

    [
        mine[0], mine[1], mine[2], mine[3], mine[4], mine[5], //
        theirs[0], theirs[1], theirs[2], theirs[3], theirs[4], theirs[5],
    ]
}

fn white_pov_result_to_y(result_tag: &str) -> Option<f32> {
    match result_tag.trim() {
        "1-0" => Some(1.0),
        "0-1" => Some(-1.0),
        "1/2-1/2" | "1/2-1/2\u{200f}" | "1/2-1/2\u{200e}" => Some(0.0),
        "*" => None,
        _ => None,
    }
}

fn side_to_move_adjusted_y(white_pov_y: f32, stm: ShColor) -> f32 {
    match stm {
        ShColor::White => white_pov_y,
        ShColor::Black => -white_pov_y,
    }
}

// ===================== PGN Visitor =====================
struct TexelVisitor {
    chess: Chess,
    start_turn: ShColor,
    pending_bb: Vec<[u64; 12]>,
    pending_castling: Vec<u8>,
    pending_ep: Vec<Option<u8>>,
    // NEW: per-sample metadata
    pending_mover_is_white: Vec<bool>,
    pending_was_mirrored: Vec<bool>,

    plies: usize,
    result_tag: Option<String>,
}

impl TexelVisitor {
    fn new() -> Self {
        Self {
            chess: Chess::default(),
            start_turn: ShColor::White,
            pending_bb: Vec::new(),
            pending_castling: Vec::new(),
            pending_ep: Vec::new(),
            pending_mover_is_white: Vec::new(),
            pending_was_mirrored: Vec::new(),
            plies: 0,
            result_tag: None,
        }
    }
    fn reset(&mut self) {
        *self = Self::new();
    }
}
impl Default for TexelVisitor {
    fn default() -> Self {
        Self::new()
    }
}

impl Visitor for TexelVisitor {
    type Result = Vec<DumpRow>;

    fn begin_game(&mut self) {
        self.reset();
    }

    fn header(&mut self, key: &[u8], val: RawHeader) {
        // Result
        if key.eq_ignore_ascii_case(b"Result") {
            if let Ok(s) = val.decode_utf8() {
                self.result_tag = Some(s.to_string());
            }
            return;
        }

        // Optional FEN support
        if key.eq_ignore_ascii_case(b"FEN") {
            if let Ok(s) = val.decode_utf8() {
                if let Ok(fen) = Fen::from_ascii(s.as_bytes()) {
                    if let Ok(pos) = fen.into_position(CastlingMode::Standard) {
                        self.chess = pos;
                        self.start_turn = self.chess.turn();
                    }
                }
            }
        }
    }

    fn san(&mut self, san: SanPlus) {
        // Record the *current* position (before the move) if this ply is sampled
        if SAMPLE_EVERY_NTH_PLY == 1 || (self.plies % SAMPLE_EVERY_NTH_PLY == 0) {
            let stm = self.chess.turn();
            let mover_is_white = matches!(stm, ShColor::White);
            let was_mirrored = MIRROR_FOR_BLACK && !mover_is_white;

            let bb = oriented_bitboards(&self.chess);
            let cr = castling_bits(&self.chess);
            let ep = ep_square_index(&self.chess);

            self.pending_bb.push(bb);
            self.pending_castling.push(cr);
            self.pending_ep.push(ep);
            self.pending_mover_is_white.push(mover_is_white);
            self.pending_was_mirrored.push(was_mirrored);
        }

        // Try to apply the move; if it fails, nuke buffers so this game is discarded.
        if let Ok(mv) = san.san.to_move(&self.chess) {
            self.chess.play_unchecked(&mv);
            self.plies += 1;
        } else {
            self.plies = 0;
            self.pending_bb.clear();
            self.pending_castling.clear();
            self.pending_ep.clear();
            self.pending_mover_is_white.clear();
            self.pending_was_mirrored.clear();
        }
    }

    fn end_game(&mut self) -> Self::Result {
        let result_tag = match &self.result_tag {
            Some(s) => s.as_str(),
            None => "*",
        };

        if (SKIP_SHORT_GAMES && (self.plies as u32) < MIN_PLIES)
            || (SKIP_UNKNOWN_RESULT && result_tag == "*")
        {
            self.reset();
            return Vec::new();
        }

        let Some(white_pov_y) = white_pov_result_to_y(result_tag) else {
            self.reset();
            return Vec::new();
        };

        // Move out the pending buffers and build labeled rows.
        let bbs = std::mem::take(&mut self.pending_bb);
        let crs = std::mem::take(&mut self.pending_castling);
        let eps = std::mem::take(&mut self.pending_ep);
        let movers = std::mem::take(&mut self.pending_mover_is_white);
        let mirrs = std::mem::take(&mut self.pending_was_mirrored);

        let mut rows = Vec::with_capacity(bbs.len());
        for i in 0..bbs.len() {
            let mover = if movers[i] { ShColor::White } else { ShColor::Black };
            let y = side_to_move_adjusted_y(white_pov_y, mover);
            rows.push(DumpRow {
                bb: bbs[i],
                castling: crs[i],
                ep: eps[i],
                y,
                mover_is_white: movers[i],
                was_mirrored: mirrs[i],
            });
        }

        self.reset();
        rows
    }

    fn begin_variation(&mut self) -> Skip {
        Skip(true) // stay in mainline only
    }
}

// ===================== PGN streaming wrapper =====================
struct PgnGameReader {
    inner: BufferedReader<BufReader<File>>,
}

impl PgnGameReader {
    fn open(path: impl Into<PathBuf>) -> Result<Self> {
        let p = path.into();
        let f = File::open(&p).with_context(|| format!("opening PGN: {}", p.display()))?;
        let br = BufReader::new(f);
        Ok(Self {
            inner: BufferedReader::new(br),
        })
    }

    /// Returns `Ok(Some(rows))` for the next game, or `Ok(None)` at EOF.
    fn next_rows(&mut self) -> std::io::Result<Option<Vec<DumpRow>>> {
        let mut v = TexelVisitor::new();
        self.inner.read_game(&mut v)
    }
}

// ===================== I/O helpers =====================
fn write_row<W: Write>(w: &mut W, row: &DumpRow) -> Result<()> {
    bincode::serialize_into(w, row).context("serializing DumpRow")
}

// ===================== Main =====================
fn main() -> Result<()> {
    // Args: <input.pgn> <output.bin>
    let in_path = std::env::args()
        .nth(1)
        .expect("usage: texel_dump <input.pgn> <output.bin>");
    let out_path = std::env::args()
        .nth(2)
        .expect("usage: texel_dump <input.pgn> <output.bin>");

    let mut reader = PgnGameReader::open(&in_path)?;
    let out_file =
        File::create(&out_path).with_context(|| format!("creating output file: {}", out_path))?;
    let mut writer = BufWriter::new(out_file);

    // Progress bar
    let pb = if SHOW_PROGRESS {
        let bar = match LIMIT_GAMES {
            Some(n) => ProgressBar::new(n as u64),
            None => ProgressBar::new_spinner(),
        };
        bar.set_style(
            ProgressStyle::with_template(
                "{spinner} games {pos}/{len} • {elapsed_precise} • {bar:40} {msg}",
            )
            .unwrap(),
        );
        bar.enable_steady_tick(std::time::Duration::from_millis(80));
        Some(bar)
    } else {
        None
    };

    let mut games_done: usize = 0;
    let mut samples_total: usize = 0;

    // Main loop
    loop {
        let rows_opt = reader.next_rows()?;
        let Some(rows) = rows_opt else {
            break;
        };

        // `TexelVisitor` already does short/unknown-result filtering.
        for row in rows {
            write_row(&mut writer, &row)?;
            samples_total += 1;
        }

        games_done += 1;

        if let Some(pb) = &pb {
            pb.inc(1);
            if games_done % 50 == 0 {
                pb.set_message(format!("samples: {}", samples_total));
            }
        }

        if let Some(limit) = LIMIT_GAMES {
            if games_done >= limit {
                break;
            }
        }
    }

    if let Some(pb) = pb {
        pb.finish_and_clear();
    }
    writer.flush()?;

    eprintln!(
        "Done. Games: {games_done}, samples: {samples_total}, out: {}",
        out_path
    );
    Ok(())
}
