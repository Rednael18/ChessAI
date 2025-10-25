// src/search/search.rs

use crate::game::defs::{squarename, Color, Piece, Square};
use crate::game::moves::{generate_legal_moves, Move, MoveList, ScoredMove, ScoredMoveList};
use crate::game::board::{Position, enemy_attacks, BitBoard};
use crate::game::history::{History};
use rand::Rng;
use crate::search::transpose::{Bound, TTEntry, TranspositionTable, Zobrist, from_tt};

use std::cell::Cell;
use std::time::{Duration, Instant};



pub struct SearchCtrl {
    pub deadline: Option<Instant>,
    nodes: Cell<u64>,
    stop: Cell<bool>,
}

impl SearchCtrl {
    #[inline]
    pub fn new(deadline: Option<Instant>) -> Self {
        Self { deadline, nodes: Cell::new(0), stop: Cell::new(false) }
    }

    #[inline]
    pub fn stopped(&self) -> bool { self.stop.get() }

    // Poll every 1024 nodes (cheap bitmask). Tweak if you want tighter latency.
    #[inline]
    pub fn poll(&self) -> bool {
        let n = self.nodes.get().wrapping_add(1);
        self.nodes.set(n);
        if (n & 0x3FF) == 0 {
            if let Some(dl) = self.deadline {
                if Instant::now() >= dl {
                    self.stop.set(true);
                }
            }
        }
        self.stop.get()
    }
}

type Score = i32;

const INF:  Score = 30_000;
const MATE: Score = 29_000;
const DRAW: Score = 0;

const CONTEMPT: i32 = 80; // centipawns

#[inline]
fn draw_value(pos: &Position, static_eval_white_pov: i32) -> i32 {
    // Convert to side-to-move POV
    let stm = pos.state.stm;
    let se_stm = if stm == Color::White { static_eval_white_pov } else { -static_eval_white_pov };
    // If we're doing fine, dislike the draw a bit; if we're worse, like it a bit.
    if se_stm >= 0 { -CONTEMPT } else { CONTEMPT }
}


const SMALL_OPENING_DICT_WHITE: [(Move, f32); 7] = [
    (Move::pack(squarename::E2, squarename::E4, crate::game::moves::Flag::QUIET), 0.26882),
    (Move::pack(squarename::D2, squarename::D4, crate::game::moves::Flag::QUIET), 0.26882),
    (Move::pack(squarename::G1, squarename::F3, crate::game::moves::Flag::QUIET), 0.21506),
    (Move::pack(squarename::C2, squarename::C4, crate::game::moves::Flag::QUIET), 0.13441),
    (Move::pack(squarename::G2, squarename::G3, crate::game::moves::Flag::QUIET), 0.08065),
    (Move::pack(squarename::B2, squarename::B3, crate::game::moves::Flag::QUIET), 0.02151),
    (Move::pack(squarename::B1, squarename::C3, crate::game::moves::Flag::QUIET), 0.01075),
];

const SMALL_OPENING_DICT_BLACK: [[(Move, f32); 7]; 7] = [
    // E2E4
    [
        (Move::pack(squarename::E7, squarename::E5, crate::game::moves::Flag::QUIET), 0.21505),
        (Move::pack(squarename::C7, squarename::C5, crate::game::moves::Flag::QUIET), 0.21505),
        (Move::pack(squarename::E7, squarename::E6, crate::game::moves::Flag::QUIET), 0.17204),
        (Move::pack(squarename::C7, squarename::C6, crate::game::moves::Flag::QUIET), 0.16129),
        (Move::pack(squarename::D7, squarename::D6, crate::game::moves::Flag::QUIET), 0.10753),
        (Move::pack(squarename::G7, squarename::G6, crate::game::moves::Flag::QUIET), 0.10753),
        (Move::pack(squarename::D7, squarename::D5, crate::game::moves::Flag::QUIET), 0.02151),
    ],
    // D2D4
    [
        (Move::pack(squarename::D7, squarename::D5, crate::game::moves::Flag::QUIET), 0.32258),
        (Move::pack(squarename::G8, squarename::F6, crate::game::moves::Flag::QUIET), 0.32258),
        (Move::pack(squarename::E7, squarename::E6, crate::game::moves::Flag::QUIET), 0.20968),
        (Move::pack(squarename::C5, squarename::C6, crate::game::moves::Flag::QUIET), 0.06452),
        (Move::pack(squarename::G7, squarename::G6, crate::game::moves::Flag::QUIET), 0.03226),
        (Move::pack(squarename::C7, squarename::C5, crate::game::moves::Flag::QUIET), 0.03226),
        (Move::pack(squarename::F7, squarename::F5, crate::game::moves::Flag::QUIET), 0.01613),
    ],
    // G1F3
    [
        (Move::pack(squarename::D7, squarename::D5, crate::game::moves::Flag::QUIET), 0.21053),
        (Move::pack(squarename::G8, squarename::F6, crate::game::moves::Flag::QUIET), 0.21053),
        (Move::pack(squarename::E7, squarename::E6, crate::game::moves::Flag::QUIET), 0.21053),
        (Move::pack(squarename::C7, squarename::C6, crate::game::moves::Flag::QUIET), 0.16842),
        (Move::pack(squarename::B7, squarename::B6, crate::game::moves::Flag::QUIET), 0.08421),
        (Move::pack(squarename::C7, squarename::C5, crate::game::moves::Flag::QUIET), 0.08421),
        (Move::pack(squarename::G7, squarename::G6, crate::game::moves::Flag::QUIET), 0.03158),
    ],
    // C2C4
    [
        (Move::pack(squarename::E7, squarename::E5, crate::game::moves::Flag::QUIET), 0.18182),
        (Move::pack(squarename::C7, squarename::C5, crate::game::moves::Flag::QUIET), 0.18182),
        (Move::pack(squarename::E7, squarename::E6, crate::game::moves::Flag::QUIET), 0.18182),
        (Move::pack(squarename::G8, squarename::F6, crate::game::moves::Flag::QUIET), 0.18182),
        (Move::pack(squarename::G7, squarename::G6, crate::game::moves::Flag::QUIET), 0.14545),
        (Move::pack(squarename::D7, squarename::D6, crate::game::moves::Flag::QUIET), 0.10909),
        (Move::pack(squarename::B7, squarename::B6, crate::game::moves::Flag::QUIET), 0.01818),
    ],
    // G2G3
    [
        (Move::pack(squarename::E7, squarename::E5, crate::game::moves::Flag::QUIET), 0.18519),
        (Move::pack(squarename::D7, squarename::D5, crate::game::moves::Flag::QUIET), 0.18519),
        (Move::pack(squarename::E7, squarename::E6, crate::game::moves::Flag::QUIET), 0.14815),
        (Move::pack(squarename::G8, squarename::F6, crate::game::moves::Flag::QUIET), 0.14815),
        (Move::pack(squarename::G7, squarename::G6, crate::game::moves::Flag::QUIET), 0.14815),
        (Move::pack(squarename::C7, squarename::C6, crate::game::moves::Flag::QUIET), 0.14815),
        (Move::pack(squarename::C7, squarename::C5, crate::game::moves::Flag::QUIET), 0.03704),
    ],
    // B2B3
    [
        (Move::pack(squarename::E7, squarename::E5, crate::game::moves::Flag::QUIET), 0.18519),
        (Move::pack(squarename::D7, squarename::D5, crate::game::moves::Flag::QUIET), 0.18519),
        (Move::pack(squarename::G8, squarename::F6, crate::game::moves::Flag::QUIET), 0.14815),
        (Move::pack(squarename::E7, squarename::E6, crate::game::moves::Flag::QUIET), 0.14815),
        (Move::pack(squarename::C7, squarename::C5, crate::game::moves::Flag::QUIET), 0.14815),
        (Move::pack(squarename::C7, squarename::C6, crate::game::moves::Flag::QUIET), 0.14815),
        (Move::pack(squarename::D7, squarename::D6, crate::game::moves::Flag::QUIET), 0.03704),
    ],
    // B1C3
    [
        (Move::pack(squarename::D7, squarename::D5, crate::game::moves::Flag::QUIET), 0.27027),
        (Move::pack(squarename::G8, squarename::F6, crate::game::moves::Flag::QUIET), 0.27027),
        (Move::pack(squarename::B8, squarename::C6, crate::game::moves::Flag::QUIET), 0.21622),
        (Move::pack(squarename::C7, squarename::C6, crate::game::moves::Flag::QUIET), 0.08108),
        (Move::pack(squarename::C7, squarename::C5, crate::game::moves::Flag::QUIET), 0.08108),
        (Move::pack(squarename::E7, squarename::E6, crate::game::moves::Flag::QUIET), 0.04054),
        (Move::pack(squarename::E7, squarename::E5, crate::game::moves::Flag::QUIET), 0.04054),
    ],
];




#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum BoundType { Exact, LowerBound, UpperBound, Abort }

// --- Evaluation from the side to move's perspective ---
#[inline]
pub fn eval_stm(pos: &Position, eval_func: fn(&Position) -> Score) -> Score {
    let base = eval_func(pos);
    if pos.state.stm == Color::White { base } else { -base }
}

#[inline]
fn king_square(pos: &Position, side: Color) -> Square {
    let kbb = pos.pieces(side, Piece::King).0;
    debug_assert!(kbb != 0, "No king for side {side:?}");
    Square(kbb.trailing_zeros() as u8)
}

#[inline]
fn in_check(pos: &Position, side: Color) -> bool {
    let occ = pos.occupied();
    let ksq = king_square(pos, side);
    let attacks = enemy_attacks(pos, !side, occ);
    (attacks & BitBoard::from_square(ksq)) != BitBoard(0)
}


fn negamax(
    history: &mut History,
    z: &mut Zobrist,
    mut alpha: Score,
    tt: &mut TranspositionTable,
    mut beta: Score,
    depth: i32,
    ply: i32,
    eval_func: fn(&Position) -> Score,
    killers: &mut [[Option<Move>; 2]; 256],
    histtbl: &mut [[[u16; 64]; 64]; 2],
    cm_hist: &mut [[Option<Move>; 64]; 2],
) -> (Score, BoundType) {
    let orig_alpha = alpha;

    // Single probe, reused later for TT best
    let probe = tt.probe(z.hash);
    if let Some(entry) = probe {
        if entry.depth >= depth {
            let sc = from_tt(entry.score, ply);
            match entry.bound {
                Bound::Exact => return (sc, BoundType::Exact),
                Bound::LowerBound => {
                    if sc > alpha { alpha = sc; }
                    if alpha >= beta { return (sc, BoundType::LowerBound); }
                }
                Bound::UpperBound => {
                    if sc < beta { beta = sc; }
                    if alpha >= beta { return (sc, BoundType::UpperBound); }
                }
            }
        }
    }

    // Leaf
    if depth <= 0 {
        let val = eval_stm(history.current(), eval_func);
        tt.store(TTEntry { hash: z.hash, depth: 0, score: crate::search::transpose::to_tt(val, ply), bound: Bound::Exact, best_move: None });
        return (val, BoundType::Exact);
    }

    // Generate moves
    let mut list = MoveList::new();
    let len = generate_legal_moves(history.current(), &mut list);

    if z.is_threefold(history.half_move() as usize) || history.half_move() >= 100 {
        let cur = history.current();
        let se = eval_func(cur); // e.g. material_attack_pesto_eval(cur)
        let dv = draw_value(cur, se);
        // Store draw in TT as a leaf/exact
        tt.store(TTEntry {
            hash: z.hash,
            depth,
            score: crate::search::transpose::to_tt(dv, ply),
            bound: Bound::Exact,
            best_move: None,
        });
        return (dv, BoundType::Exact);
    }

    if len == 0 {
        let score = if in_check(history.current(), history.current().state.stm) { -MATE + ply } else { DRAW };
        tt.store(TTEntry { hash: z.hash, depth, score: crate::search::transpose::to_tt(score, ply), bound: Bound::Exact, best_move: None });
        return (score, BoundType::Exact);
    }

    // Ordering inputs
    let tt_best = probe.and_then(|e| e.best_move);
    let ply_usize = (ply as usize).min(255);
    let side_stm: usize = { history.current().state.stm as usize };

    // Opponent's last move 'to' square at this node (for countermove key)
    let prev_to: Option<u8> = history.stack.iter().rfind(|u| !u.is_null)
    .map(|u| u.mv.to_sq().0);

    // Order once, then loop
    {
        let pos_ref = history.current();
        let pt_ref  = history.piece_table();
        order_moves_in_place(
            pos_ref,
            pt_ref,
            &mut list,
            tt_best,
            &killers[ply_usize],
            histtbl,
            &cm_hist,
            prev_to,
            z.hash, // position key for deterministic jitter
        );
    }


    // PVS loop (same semantics as before)
    let mut best = -INF;
    let mut best_move: Option<Move> = None;


    for (i, m) in list.items[..len].iter().copied().enumerate() {
        z.push();
        history.apply(m, z);

        let (child, _) = if i == 0 {
            negamax(history, z, -beta, tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist)
        } else {
            let (probe_score, _) = negamax(history, z, -(alpha + 1), tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist);
            if -probe_score > alpha {
                negamax(history, z, -beta, tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist)
            } else {
                (probe_score, BoundType::Exact)
            }
        };

        history.undo();
        z.pop();

        let score = -child;
        if score > best {
            best = score;
            best_move = Some(m);
            if best > alpha {
                alpha = best;
                if alpha >= beta {
                    // Beta cutoff: update killers/history (existing) and countermove (NEW)
                    killers[ply_usize][1] = killers[ply_usize][0];
                    killers[ply_usize][0] = Some(m);

                    let from = m.from_sq().0 as usize;
                    let to   = m.to_sq().0 as usize;
                    histtbl[side_stm][from][to] =
                        histtbl[side_stm][from][to].saturating_add((depth as u16) * 2);

                    if let Some(p2) = prev_to {
                        cm_hist[side_stm][p2 as usize] = Some(m);
                    }
                    break;
                }
            }
        }
    }

    let bt = if best <= orig_alpha { BoundType::UpperBound }
             else if best >= beta { BoundType::LowerBound }
             else { BoundType::Exact };
    let bound = match bt { BoundType::Exact=>Bound::Exact, BoundType::LowerBound=>Bound::LowerBound, BoundType::UpperBound=>Bound::UpperBound, BoundType::Abort=>Bound::UpperBound };

    tt.store(TTEntry {
        hash: z.hash, depth,
        score: crate::search::transpose::to_tt(best, ply),
        bound,
        best_move,
    });

    (best, bt)
}




pub fn complete_search(
    root_pos: &Position, 
    depth: i32, 
    _time: Option<i32>,
    eval_func: fn(&Position) -> Score, 
    random_open: bool,
    z: &mut Zobrist,
    tt: &mut TranspositionTable,
) -> Move {
    let mut history = History::new(root_pos.clone());

    let mut root_moves = MoveList::new();
    generate_legal_moves(root_pos, &mut root_moves);
    if root_moves.len == 0 { return Move::default(); }

    // Heuristics shared with inner search
    let mut killers = [[None; 2]; 256];
    let mut histtbl = [[[0u16; 64]; 64]; 2];
    let mut cm_hist = [[None; 64]; 2]; // NEW: countermove table (ordering only)

    // Order the root (TT+killers+history+countermove) before search
    {
        let tt_best_root = tt.probe(z.hash).and_then(|e| e.best_move);
        let pt_ref = history.piece_table();
        // No "prev_to" at root (no opponent move before root), pass None
        order_moves_in_place(
            root_pos, pt_ref, &mut root_moves,
            tt_best_root, &killers[0], &histtbl, &cm_hist,
            None, // prev_to
            z.hash, // NEW
        );

    }

    let mut best_move  = root_moves.items[0];
    let mut best_score = -INF;
    let mut best_shallow = -INF;

    let ply_count = history.current().state.ply_counter;
    let rand_move = random_open && (ply_count < 10);

    let scored_move_list = ScoredMoveList::new();

    // Root PVS window
    // Root PVS window
    let mut alpha = -INF;
    let beta = INF;

    for (i, m) in root_moves.items[..root_moves.len].iter().copied().enumerate() {
        z.push();
        history.apply(m, z);

        // tie-break helper (unchanged)
        let shallow = -eval_stm(history.current(), eval_func);

        // Run either full-window (first move) or null-window (others)
        let mut score: Score;
        let mut exact = false;

        if i == 0 {
            let (child, _) = negamax(&mut history, z, -beta, tt, -alpha, depth - 1, 1, eval_func,
                                    &mut killers, &mut histtbl, &mut cm_hist);
            score = -child;
            exact = true; // first move always exact
        } else {
            // null-window first
            let (probe_score, _) = negamax(&mut history, z, -(alpha + 1), tt, -alpha, depth - 1, 1, eval_func,
                                        &mut killers, &mut histtbl, &mut cm_hist);
            score = -probe_score;

            // If this beats alpha, do the full re-search to get the exact value.
            if score > alpha {
                let (child, _) = negamax(&mut history, z, -beta, tt, -alpha, depth - 1, 1, eval_func,
                                        &mut killers, &mut histtbl, &mut cm_hist);
                score = -child;
                exact = true;
            }
        }

        history.undo();
        z.pop();

        // record for optional logging (see section B)
        // print!("Move: {m}, Score: {score}{}\n", if exact { "" } else { " (bound)" });

        // Selection rule: never let a non-exact value dethrone the best.
        if exact && (score > best_score || (score == best_score && shallow > best_shallow)) {
            best_score   = score;
            best_shallow = shallow;
            best_move    = m;
        }

        if score > alpha { alpha = score; }
    }


    if rand_move {
        // keep your existing randomization logic
        let mut moves: Vec<ScoredMove> = scored_move_list.items[..scored_move_list.len].to_vec();
        moves.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        if moves.is_empty() { return best_move; }
        let best_f = moves[0].score;
        let threshold = (best_f.abs() * (0.36/(ply_count as f32 + 1f32).sqrt())).max(1.0);
        let mut candidates: Vec<ScoredMove> = moves.into_iter()
            .filter(|m| m.score >= best_f - threshold)
            .collect();
        candidates.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        if candidates.is_empty() { return best_move; }
        let mut weights: Vec<f32> = candidates.iter().enumerate().map(|(i, _)| 1.0_f32 / ((i as f32) + 10.0)).collect();
        let sum: f32 = weights.iter().sum();
        if sum <= 0.0 { return candidates[0].mv; }
        for w in &mut weights { *w /= sum; }
        let mut rng = rand::thread_rng();
        let r = rng.gen_range(0.0f32..1.0f32);
        let mut cum = 0.0_f32;
        for (i, w) in weights.iter().enumerate() {
            cum += *w;
            if r <= cum { return candidates[i].mv; }
        }
        return candidates.last().unwrap().mv;
    }

    best_move
}



pub fn iterative_deepening(
    root_pos: &Position,
    depth: Option<i32>,
    time_ms: Option<i32>,
    eval_func: fn(&Position) -> Score,
    random_open: bool,            // only applied to the final pick, if you want it
    z: &mut Zobrist,
    tt: &mut TranspositionTable,
) -> Move {
    #![allow(dead_code)]
    debug_assert_ne!(depth.is_some(), time_ms.is_some(), "Set either depth or time_ms, not both");

    // println!("----------------New Move (ID) --------({} : {})--------", if root_pos.state.stm == Color::White {"white"} else {"black"}, root_pos.state.ply_counter);


    // Early out: no legal moves
    let mut history = History::new(root_pos.clone());
    let mut root_moves = MoveList::new();
    generate_legal_moves(root_pos, &mut root_moves);
    if root_moves.len == 0 { return Move::default(); }

    // Shared heuristics across all iterations
    let mut killers = [[None; 2]; 256];
    let mut histtbl = [[[0u16; 64]; 64]; 2];
    let mut cm_hist = [[None; 64]; 2];

    // Time budget (soft): we stop between depths; if it expires mid-depth,
    // we return the last completed depth’s best move (lossless w.r.t. that depth).
    let deadline = time_ms.map(|ms| Instant::now() + Duration::from_millis(ms as u64));

    let max_d = if let Some(d) = depth { d.max(1) } else { i32::MAX };
    let use_time = time_ms.is_some();

    // ID state
    let mut best_move = root_moves.items[0];
    let mut last_score: Score = 0;
    let mut _dep: i32 = 0;

    // Iterate depths 1..=max_d (time may stop us earlier)
    'depth_loop: for d in 1..=max_d {
        if use_time {
            if let Some(dl) = deadline {
                if Instant::now() >= dl {
                    break 'depth_loop;
                }
            }
        }
        _dep += 1;
        // println!("Depth: {}", _dep);
        // Fresh root move list each iteration (position doesn’t change)
        root_moves.len = 0;
        generate_legal_moves(root_pos, &mut root_moves);

        // Root ordering: seed with previous iteration’s best, then TT/killer/history/countermove
        {
            let tt_hint = Some(best_move).or_else(|| tt.probe(z.hash).and_then(|e| e.best_move));
            let pt_ref = history.piece_table();
            order_moves_in_place(root_pos, pt_ref, &mut root_moves,
                tt_hint, &killers[0], &histtbl, &cm_hist,
                None,
                z.hash,
            );

        }

        // Aspiration window
        let (mut alpha, mut beta) = if d == 1 {
            (-INF, INF)
        } else {
            // ~25 cp band; widens automatically on fail
            let near_mate = last_score.abs() > MATE - 1000;
            if near_mate { (-INF, INF) } else { (last_score - 25, last_score + 25) }
        };

        // Re-search with widening on fails
        loop {
            let a0 = alpha;
            let b0 = beta;

            let mut best_sc = -INF;
            let mut best_shallow = -INF;
            let mut best_m = root_moves.items[0];

            // Root PVS with exact-only selection
            for (i, m) in root_moves.items[..root_moves.len].iter().copied().enumerate() {
                // Optional soft time check inside the root loop (keeps iterations snappy)
                if use_time {
                    if let Some(dl) = deadline {
                        if Instant::now() >= dl {
                            // Abort this depth; keep result from previous depth
                            break 'depth_loop;
                        }
                    }
                }

                z.push();
                history.apply(m, z);

                let shallow = -eval_stm(history.current(), eval_func);

                let mut score;
                let mut exact = false;

                if i == 0 {
                    let (child, _) = negamax(
                        &mut history, z, -beta, tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist
                    );
                    score = -child;
                    exact = true;
                } else {
                    let (probe_sc, _) = negamax(
                        &mut history, z, -(alpha + 1), tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist
                    );
                    score = -probe_sc;

                    if score > alpha {
                        let (child, _) = negamax(
                            &mut history, z, -beta, tt, -alpha, d - 1, 1,
                            eval_func, &mut killers, &mut histtbl, &mut cm_hist
                        );
                        score = -child;
                        exact = true;
                    }
                }
                // if exact {
                //     println!("Move {} gives score of about {} (exact)", m, score);
                // } else {
                //     println!("Move {} gives score of about {}", m, score);
                // }

                history.undo();
                z.pop();

                if exact && (score > best_sc || (score == best_sc && shallow > best_shallow)) {
                    best_sc = score;
                    best_shallow = shallow;
                    best_m = m;
                }
                if score > alpha { alpha = score; }
            }


            // Aspiration decisions based on the original window
            if best_sc <= a0 {
                // Fail low → widen low side
                if a0 <= -INF / 2 {
                    // Already full window; accept result
                    best_move = best_m; last_score = best_sc; break;
                }
                let step = (b0 - a0).abs().max(50);
                alpha = a0.saturating_sub(step);
                continue;
            }
            if best_sc >= b0 {
                // Fail high → widen high side
                if b0 >= INF / 2 {
                    best_move = best_m; last_score = best_sc; break;
                }
                let step = (b0 - a0).abs().max(50);
                beta = b0.saturating_add(step);
                continue;
            }

            // Inside window: exact at this depth
            best_move = best_m;
            last_score = best_sc;
            break;
        }
    }
    println!("Best move and score: {} with score {}", best_move, last_score);
    println!("Reached depth {}!\n\n", _dep);
    // Optional: keep your opening randomization, but only for the final pick.
    if random_open && history.current().state.ply_counter < 10 {
        // Slightly randomize the final pick among the top few root moves.
        let ply = history.current().state.ply_counter as usize;

        // 2..=3 candidates depending on how early we are in the game.
        let horizon = 2 + (10usize.saturating_sub(ply + 1)) / 5; // 3 for very early, otherwise 2

        // Candidate set: the current best + a couple of next-best ordered moves.
        let mut cands: Vec<Move> = Vec::new();
        cands.push(best_move);

        let limit = root_moves.len.min(4);
        for &m in root_moves.items[..limit].iter() {
            if !cands.contains(&m) {
                cands.push(m);
            }
            if cands.len() >= horizon {
                break;
            }
        }

        if cands.len() > 1 {
            // Geometric weights strongly prefer earlier moves.
            let mut rng = rand::thread_rng();
            let mut weights: Vec<f32> = (0..cands.len())
                .map(|i| (0.55f32).powi(i as i32))
                .collect();

            let sum: f32 = weights.iter().sum();
            if sum > 0.0 {
                for w in &mut weights { *w /= sum; }
                let r = rng.gen_range(0.0f32..1.0f32);
                let mut acc = 0.0f32;
                for (i, w) in weights.iter().enumerate() {
                    acc += *w;
                    if r <= acc {
                        return cands[i];
                    }
                }
            }
            // Fallback: just use the best.
            return cands[0];
        }
    }


    best_move
}







fn quiescent(
    history: &mut History,
    z: &mut Zobrist,
    mut alpha: Score,
    beta: Score,
    eval_func: fn(&Position) -> Score,
    ctrl: &SearchCtrl, 
) -> Score {
    if ctrl.poll() { return alpha; } 
    let stand_pat = eval_stm(history.current(), eval_func);
    if stand_pat >= beta {
        return stand_pat;
    }
    if stand_pat > alpha {
        alpha = stand_pat;
    }

    if ctrl.poll() { return alpha; }  

    let mut list = MoveList::new();
    let len = generate_legal_moves(history.current(), &mut list);

    // Collect captures/promos and order by MVV-LVA
    let pt_ref = history.piece_table();
    let mut caps: Vec<Move> = Vec::new();
    for &m in list.items[..len].iter() {
        if m.is_capture() || m.is_promo() {
            caps.push(m);
        }
    }
    caps.sort_unstable_by(|&a, &b| {
        mvv_lva_fast(pt_ref, b).cmp(&mvv_lva_fast(pt_ref, a))
    });

    for m in caps {
        if ctrl.poll() { return alpha; }      
        z.push();
        history.apply(m, z);

        // Proper negamax flip: swap and negate once
        let score = -quiescent(history, z, -beta, -alpha, eval_func, ctrl);

        history.undo();
        z.pop();

        if score >= beta {
            return score; // fail-high
        }
        if score > alpha {
            alpha = score;
        }
    }

    alpha
}













fn negamax_with_quiescence(
    history: &mut History,
    z: &mut Zobrist,
    mut alpha: Score,
    tt: &mut TranspositionTable,
    mut beta: Score,
    depth: i32,
    ply: i32,
    eval_func: fn(&Position) -> Score,
    killers: &mut [[Option<Move>; 2]; 256],
    histtbl: &mut [[[u16; 64]; 64]; 2],
    cm_hist: &mut [[Option<Move>; 64]; 2],
    ctrl: &SearchCtrl, 
) -> (Score, BoundType) {
    if ctrl.poll() { return (alpha, BoundType::Abort); } 
    let orig_alpha = alpha;

    // Single probe, reused later for TT best
    let probe = tt.probe(z.hash);
    if let Some(entry) = probe {
        if entry.depth >= depth {
            let sc = from_tt(entry.score, ply);
            match entry.bound {
                Bound::Exact => return (sc, BoundType::Exact),
                Bound::LowerBound => {
                    if sc > alpha { alpha = sc; }
                    if alpha >= beta { return (sc, BoundType::LowerBound); }
                }
                Bound::UpperBound => {
                    if sc < beta { beta = sc; }
                    if alpha >= beta { return (sc, BoundType::UpperBound); }
                }
            }
        }
    }

    // Leaf
    if depth <= 0 {
        let val = quiescent(history, z, alpha, beta, eval_func, ctrl);
        if !ctrl.stopped() {
            tt.store(TTEntry { hash: z.hash, depth: 0, score: crate::search::transpose::to_tt(val, ply), bound: Bound::Exact, best_move: None });
        }
        return (val, BoundType::Exact);
    }

    if ctrl.poll() { return (alpha, BoundType::Abort); }

    // Generate moves
    let mut list = MoveList::new();
    let len = generate_legal_moves(history.current(), &mut list);
    if z.is_threefold(history.half_move() as usize) || history.half_move() >= 100 {
        let cur = history.current();
        let se = eval_func(cur); // e.g. material_attack_pesto_eval(cur)
        let dv = draw_value(cur, se);
        // Store draw in TT as a leaf/exact
        tt.store(TTEntry {
            hash: z.hash,
            depth,
            score: crate::search::transpose::to_tt(dv, ply),
            bound: Bound::Exact,
            best_move: None,
        });
        return (dv, BoundType::Exact);
    }

    if len == 0 {
        let score = if in_check(history.current(), history.current().state.stm) { -MATE + ply } else { DRAW };
        tt.store(TTEntry { hash: z.hash, depth, score: crate::search::transpose::to_tt(score, ply), bound: Bound::Exact, best_move: None });
        return (score, BoundType::Exact);
    }

    // Ordering inputs
    let tt_best = probe.and_then(|e| e.best_move);
    let ply_usize = (ply as usize).min(255);
    let side_stm: usize = { history.current().state.stm as usize };

    // Opponent's last move 'to' square at this node (for countermove key)
    let prev_to: Option<u8> = history.stack.last().map(|u| u.mv.to_sq().0);

    // Order once, then loop
    {
        let pos_ref = history.current();
        let pt_ref  = history.piece_table();
        order_moves_in_place(
            pos_ref,
            pt_ref,
            &mut list,
            tt_best,
            &killers[ply_usize],
            histtbl,
            &cm_hist,
            prev_to,
            z.hash, // position key for deterministic jitter
        );
    }


    // PVS loop (same semantics as before)
    let mut best = -INF;
    let mut best_move: Option<Move> = None;


    for (i, m) in list.items[..len].iter().copied().enumerate() {
        if ctrl.poll() { return (alpha, BoundType::Abort); }

        z.push();
        history.apply(m, z);

        let (child, _) = if i == 0 {
            negamax_with_quiescence(history, z, -beta, tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl)
        } else {
            let (probe_score, _) = negamax_with_quiescence(history, z, -(alpha + 1), tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl);
            if -probe_score > alpha {
                negamax_with_quiescence(history, z, -beta, tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl)
            } else {
                (probe_score, BoundType::Exact)
            }
        };

        history.undo();
        z.pop();

        let score = -child;
        if score > best {
            best = score;
            best_move = Some(m);
            if best > alpha {
                alpha = best;
                if alpha >= beta {
                    // Beta cutoff: update killers/history (existing) and countermove (NEW)
                    killers[ply_usize][1] = killers[ply_usize][0];
                    killers[ply_usize][0] = Some(m);

                    let from = m.from_sq().0 as usize;
                    let to   = m.to_sq().0 as usize;
                    histtbl[side_stm][from][to] =
                        histtbl[side_stm][from][to].saturating_add((depth as u16) * 2);

                    if let Some(p2) = prev_to {
                        cm_hist[side_stm][p2 as usize] = Some(m);
                    }
                    break;
                }
            }
        }
    }

    let bt = if best <= orig_alpha { BoundType::UpperBound }
             else if best >= beta { BoundType::LowerBound }
             else { BoundType::Exact };
    if !ctrl.stopped() {
        let bound = match bt { BoundType::Exact=>Bound::Exact, BoundType::LowerBound=>Bound::LowerBound, BoundType::UpperBound=>Bound::UpperBound, BoundType::Abort=>Bound::UpperBound /*unused*/ };
        tt.store(TTEntry { hash: z.hash, depth, score: crate::search::transpose::to_tt(best, ply), bound, best_move });
    }

    (best, bt)
}


fn negamax_null_move(
    history: &mut History,
    z: &mut Zobrist,
    mut alpha: Score,
    tt: &mut TranspositionTable,
    mut beta: Score,
    depth: i32,
    ply: i32,
    eval_func: fn(&Position) -> Score,
    killers: &mut [[Option<Move>; 2]; 256],
    histtbl: &mut [[[u16; 64]; 64]; 2],
    cm_hist: &mut [[Option<Move>; 64]; 2],
    ctrl: &SearchCtrl, 
) -> (Score, BoundType) {
    if ctrl.poll() { return (alpha, BoundType::Abort); } 
    let orig_alpha = alpha;

    // Single probe, reused later for TT best
    let probe = tt.probe(z.hash);
    if let Some(entry) = probe {
        if entry.depth >= depth {
            let sc = from_tt(entry.score, ply);
            match entry.bound {
                Bound::Exact => return (sc, BoundType::Exact),
                Bound::LowerBound => {
                    if sc > alpha { alpha = sc; }
                    if alpha >= beta { return (sc, BoundType::LowerBound); }
                }
                Bound::UpperBound => {
                    if sc < beta { beta = sc; }
                    if alpha >= beta { return (sc, BoundType::UpperBound); }
                }
            }
        }
    }

    // Leaf
    if depth <= 0 {
        let val = quiescent(history, z, alpha, beta, eval_func, ctrl);
        if !ctrl.stopped() {
            tt.store(TTEntry { hash: z.hash, depth: 0, score: crate::search::transpose::to_tt(val, ply), bound: Bound::Exact, best_move: None });
        }
        return (val, BoundType::Exact);
    }

    if ctrl.poll() { return (alpha, BoundType::Abort); }
    

    // --- Null move pruning: do this ONCE per node, before real moves ---
    let is_pv = (beta - alpha) > 1;
    let stm   = history.current().state.stm;

    if depth >= 3
        && !is_pv
        && !in_check(history.current(), stm)
        && has_non_pawn_material(history.current(), stm)
        && !history.last_was_null()
    {
        z.push();
        history.apply_null(z);

        let r = if depth >= 6 { 3 } else { 2 };

        let (child, _bt) = negamax_null_move(
            history, z,
            -beta, tt, -(beta - 1),
            depth - r - 1, ply + 1,
            eval_func, killers, histtbl, cm_hist, ctrl
        );

        history.undo_null();
        z.pop();

        let nscore = -child; // <-- important negation

        if nscore >= beta {
            if !ctrl.stopped() {
                tt.store(TTEntry {
                    hash: z.hash,
                    depth,
                    score: crate::search::transpose::to_tt(beta, ply),
                    bound: Bound::LowerBound, // fail-high => lower bound
                    best_move: None,
                });
            }
            return (beta, BoundType::LowerBound);
        }
    }
    // --- End null move ---

    // Generate and order moves
    let mut list = MoveList::new();
    let len = generate_legal_moves(history.current(), &mut list);
    if z.is_threefold(history.half_move() as usize) || history.half_move() >= 100 {
        let cur = history.current();
        let se = eval_func(cur); // e.g. material_attack_pesto_eval(cur)
        let dv = draw_value(cur, se);
        // Store draw in TT as a leaf/exact
        tt.store(TTEntry {
            hash: z.hash,
            depth,
            score: crate::search::transpose::to_tt(dv, ply),
            bound: Bound::Exact,
            best_move: None,
        });
        return (dv, BoundType::Exact);
    }

    if len == 0 {
        let score = if in_check(history.current(), history.current().state.stm) { -MATE + ply } else { DRAW };
        tt.store(TTEntry { hash: z.hash, depth, score: crate::search::transpose::to_tt(score, ply), bound: Bound::Exact, best_move: None });
        return (score, BoundType::Exact);
    }



    let probe = tt.probe(z.hash);
    let tt_best = probe.and_then(|e| e.best_move);
    let ply_usize = (ply as usize).min(255);
    let side_stm: usize = history.current().state.stm as usize;

    // Use the last non-null move for countermove key:
    let prev_to: Option<u8> = history.stack.iter().rfind(|u| !u.is_null)
        .map(|u| u.mv.to_sq().0);

    {
        let pos_ref = history.current();
        let pt_ref  = history.piece_table();
        order_moves_in_place(
            pos_ref, pt_ref, &mut list,
            tt_best, &killers[ply_usize], histtbl, &cm_hist,
            prev_to, z.hash
        );
    }

    // Order once, then loop
    {
        let pos_ref = history.current();
        let pt_ref  = history.piece_table();
        order_moves_in_place(
            pos_ref,
            pt_ref,
            &mut list,
            tt_best,
            &killers[ply_usize],
            histtbl,
            &cm_hist,
            prev_to,
            z.hash, // position key for deterministic jitter
        );
    }


    // PVS loop (same semantics as before)
    let mut best = -INF;
    let mut best_move: Option<Move> = None;


    for (i, m) in list.items[..len].iter().copied().enumerate() {
        if ctrl.poll() { return (alpha, BoundType::Abort); }

        z.push();
        history.apply(m, z);

        let (child, _) = if i == 0 {
            negamax_null_move(history, z, -beta, tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl)
        } else {
            let (probe_score, _) = negamax_null_move(history, z, -(alpha + 1), tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl);
            if -probe_score > alpha {
                negamax_null_move(history, z, -beta, tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl)
            } else {
                (probe_score, BoundType::Exact)
            }
        };

        history.undo();
        z.pop();

        let score = -child;
        if score > best {
            best = score;
            best_move = Some(m);
            if best > alpha {
                alpha = best;
                if alpha >= beta {
                    // Beta cutoff: update killers/history (existing) and countermove (NEW)
                    killers[ply_usize][1] = killers[ply_usize][0];
                    killers[ply_usize][0] = Some(m);

                    let from = m.from_sq().0 as usize;
                    let to   = m.to_sq().0 as usize;
                    histtbl[side_stm][from][to] =
                        histtbl[side_stm][from][to].saturating_add((depth as u16) * 2);

                    if let Some(p2) = prev_to {
                        cm_hist[side_stm][p2 as usize] = Some(m);
                    }
                    break;
                }
            }
        }
    }

    let bt = if best <= orig_alpha { BoundType::UpperBound }
             else if best >= beta { BoundType::LowerBound }
             else { BoundType::Exact };
    if !ctrl.stopped() {
        let bound = match bt { BoundType::Exact=>Bound::Exact, BoundType::LowerBound=>Bound::LowerBound, BoundType::UpperBound=>Bound::UpperBound, BoundType::Abort=>Bound::UpperBound /*unused*/ };
        tt.store(TTEntry { hash: z.hash, depth, score: crate::search::transpose::to_tt(best, ply), bound, best_move });
    }

    (best, bt)
}


fn negamax_null_move_lmr(
    history: &mut History,
    z: &mut Zobrist,
    mut alpha: Score,
    tt: &mut TranspositionTable,
    mut beta: Score,
    depth: i32,
    ply: i32,
    eval_func: fn(&Position) -> Score,
    killers: &mut [[Option<Move>; 2]; 256],
    histtbl: &mut [[[u16; 64]; 64]; 2],
    cm_hist: &mut [[Option<Move>; 64]; 2],
    ctrl: &SearchCtrl,
) -> (Score, BoundType) {
    if ctrl.poll() { return (alpha, BoundType::Abort); }
    let orig_alpha = alpha;

    if ply > 0 { // Don't check for draws at the root, let ID handle it
        if z.is_threefold(history.half_move() as usize) || history.half_move() >= 100 {
            let cur = history.current();
            let se = eval_func(cur);
            return (draw_value(cur, se), BoundType::Exact);
        }
    }

    let probe = tt.probe(z.hash);
    if let Some(entry) = probe {
        if entry.depth >= depth {
            let sc = from_tt(entry.score, ply);
            match entry.bound {
                Bound::Exact => return (sc, BoundType::Exact),
                Bound::LowerBound => {
                    if sc > alpha { alpha = sc; }
                    if alpha >= beta { return (sc, BoundType::LowerBound); }
                }
                Bound::UpperBound => {
                    if sc < beta { beta = sc; }
                    if alpha >= beta { return (sc, BoundType::UpperBound); }
                }
            }
        }
    }

    if depth <= 0 {
        let val = quiescent(history, z, alpha, beta, eval_func, ctrl);
        return (val, BoundType::Exact);
    }

    let stm = history.current().state.stm;
    let in_check = in_check(history.current(), stm);

    if !in_check {
        let is_pv = (beta - alpha) > 1;
        if depth >= 3 && !is_pv && has_non_pawn_material(history.current(), stm) && !history.last_was_null() {
            z.push();
            history.apply_null(z);
            let r = if depth >= 6 { 3 } else { 2 };
            let (child, _) = negamax_null_move_lmr(history, z, -beta, tt, -(beta - 1), depth - r - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl);
            history.undo_null();
            z.pop();
            if -child >= beta {
                return (beta, BoundType::LowerBound);
            }
        }
    }

    let mut list = MoveList::new();
    let len = generate_legal_moves(history.current(), &mut list);

    if len == 0 {
        let score = if in_check { -MATE + ply } else { DRAW };
        return (score, BoundType::Exact);
    }

    let tt_best = probe.and_then(|e| e.best_move);
    let ply_usize = (ply as usize).min(255);
    let side_stm = stm as usize;
    let prev_to = history.stack.iter().rfind(|u| !u.is_null).map(|u| u.mv.to_sq().0);

    order_moves_in_place(history.current(), history.piece_table(), &mut list, tt_best, &killers[ply_usize], histtbl, &cm_hist, prev_to, z.hash);

    let mut best = -INF;
    let mut best_move: Option<Move> = None;

    for (i, m) in list.items[..len].iter().copied().enumerate() {
        z.push();
        history.apply(m, z);

        let mut score;

        // --- LMR Logic Integration ---
        if i > 0 { // First move is always searched fully
            let mut can_reduce = false;
            if depth >= 3 && i >= 3 && !m.is_capture() && !m.is_promo() && !in_check {
                 // Don't reduce killer moves or the TT move
                if Some(m) != killers[ply_usize][0] && Some(m) != killers[ply_usize][1] && Some(m) != tt_best {
                    can_reduce = true;
                }
            }

            if can_reduce {
                let reduction = 1; // Start with a simple reduction
                let (child, _) = negamax_null_move_lmr(history, z, -(alpha + 1), tt, -alpha, depth - 1 - reduction, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl);
                score = -child;

                // Re-search if it looks promising
                if score > alpha {
                    let (child_full, _) = negamax_null_move_lmr(history, z, -beta, tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl);
                    score = -child_full;
                }
            } else {
                // Standard PVS for moves that are not reduced
                let (probe_score, _) = negamax_null_move_lmr(history, z, -(alpha + 1), tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl);
                score = -probe_score;

                if score > alpha && score < beta { // Check if a re-search is needed
                    let (child, _) = negamax_null_move_lmr(history, z, -beta, tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl);
                    score = -child;
                }
            }
        } else {
            // Full, principal variation search for the first move
            let (child, _) = negamax_null_move_lmr(history, z, -beta, tt, -alpha, depth - 1, ply + 1, eval_func, killers, histtbl, cm_hist, ctrl);
            score = -child;
        }

        history.undo();
        z.pop();

        if score > best {
            best = score;
            best_move = Some(m);
            if best > alpha {
                alpha = best;
                if alpha >= beta {
                    if !m.is_capture() {
                        killers[ply_usize][1] = killers[ply_usize][0];
                        killers[ply_usize][0] = Some(m);
                        if let Some(p2) = prev_to {
                            cm_hist[side_stm][p2 as usize] = Some(m);
                        }
                    }
                    break;
                }
            }
        }
    }

    let bt = if best <= orig_alpha { BoundType::UpperBound } else if best >= beta { BoundType::LowerBound } else { BoundType::Exact };
    if !ctrl.stopped() {
        let bound = match bt { BoundType::Exact => Bound::Exact, BoundType::LowerBound => Bound::LowerBound, BoundType::UpperBound => Bound::UpperBound, BoundType::Abort => Bound::UpperBound };
        tt.store(TTEntry { hash: z.hash, depth, score: crate::search::transpose::to_tt(best, ply), bound, best_move });
    }

    (best, bt)
}



pub fn iterative_deepening_with_quiescence(
    root_pos: &Position,
    depth: Option<i32>,
    time_ms: Option<i32>,
    eval_func: fn(&Position) -> Score,
    _random_open: bool,            // only applied to the final pick, if you want it
    z: &mut Zobrist,
    tt: &mut TranspositionTable,
) -> Move {
    let timed = time_ms.is_some();
    if !timed {
        println!("----------------New Move (QUI)--------({} : {})--------",
                 if root_pos.state.stm == Color::White {"white"} else {"black"},
                 root_pos.state.ply_counter);
    }

    let mut history = History::new(root_pos.clone());

    if history.current().state.ply_counter < 2 {
        let mut rng = rand::thread_rng();
        let randnum = rng.gen_range(0.0f32..1.0f32);
        let mut sum = 0.0;
        if history.current().state.stm == Color::White {
            for element in SMALL_OPENING_DICT_WHITE {
                sum += element.1;
                if randnum < sum {
                    return element.0
                }
            }
        } else {
            if let Some(prev) = history.stack.last().map(|u| u.mv) {
                let mut row_idx: Option<usize> = None;
                for (i, &(wm, _)) in SMALL_OPENING_DICT_WHITE.iter().enumerate() {
                    if prev == wm ||
                       (prev.from_sq() == wm.from_sq() && prev.to_sq() == wm.to_sq()) {
                        row_idx = Some(i);
                        break;
                    }
                }
                if let Some(i) = row_idx {
                    for element in SMALL_OPENING_DICT_BLACK[i] {
                        sum += element.1;
                        if randnum < sum {
                            return element.0
                        }
                    }
                }
            }
        }
    }

    let mut root_moves = MoveList::new();
    generate_legal_moves(root_pos, &mut root_moves);
    if root_moves.len == 0 { return Move::default(); }

    let mut killers = [[None; 2]; 256];
    let mut histtbl = [[[0u16; 64]; 64]; 2];
    let mut cm_hist = [[None; 64]; 2];

    let ctrl = SearchCtrl::new(time_ms.map(|ms| Instant::now() + Duration::from_millis(ms as u64)));

    let max_d = if let Some(d) = depth { d.max(1) } else { i32::MAX };

    let mut best_move = root_moves.items[0];
    let mut last_score: Score = 0;
    let mut _dep: i32 = 0;


    'depth_loop: for d in 1..=max_d {
        if ctrl.stopped() { break 'depth_loop; }
        _dep += 1;
        if !timed { println!("Depth: {}", _dep); }

        root_moves.len = 0;
        generate_legal_moves(root_pos, &mut root_moves);

        {
            let tt_hint = Some(best_move).or_else(|| tt.probe(z.hash).and_then(|e| e.best_move));
            let pt_ref = history.piece_table();
            order_moves_in_place(root_pos, pt_ref, &mut root_moves,
                tt_hint, &killers[0], &histtbl, &cm_hist,
                None,
                z.hash,
            );
        }

        let (mut alpha, mut beta) = if d == 1 {
            (-INF, INF)
        } else {
            let near_mate = last_score.abs() > MATE - 1000;
            if near_mate { (-INF, INF) } else { (last_score - 25, last_score + 25) }
        };

        loop {
            if ctrl.stopped() { break 'depth_loop; }  // aspiration widen may iterate; check here too
            let a0 = alpha;
            let b0 = beta;

            let mut best_sc = -INF;
            let mut best_shallow = -INF;
            let mut best_m = root_moves.items[0];

            // Collect current depth’s root evals
            let mut cur_root: Vec<(Move, Score, bool, Score)> = Vec::with_capacity(root_moves.len);

            for (i, m) in root_moves.items[..root_moves.len].iter().copied().enumerate() {
                if ctrl.stopped() { break 'depth_loop; }

                z.push();
                history.apply(m, z);

                let shallow = -eval_stm(history.current(), eval_func);

                let (score, exact, aborted) = if i == 0 {
                    let (child, bt) = negamax_with_quiescence(
                        &mut history, z, -beta, tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                    );
                    (-child, true, matches!(bt, BoundType::Abort))
                } else {
                    let (probe_sc, bt0) = negamax_with_quiescence(
                        &mut history, z, -(alpha + 1), tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                    );
                    if matches!(bt0, BoundType::Abort) {
                        history.undo(); z.pop(); break 'depth_loop;
                    }
                    let mut sc = -probe_sc;
                    let mut ex = false;
                    if sc > alpha {
                       let (child, bt) = negamax_with_quiescence(
                           &mut history, z, -beta, tt, -alpha, d - 1, 1,
                           eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                       );
                       if matches!(bt, BoundType::Abort) {
                           history.undo(); z.pop(); break 'depth_loop;
                       }
                       sc = -child; ex = true;
                    }
                    (sc, ex, false)
                };

                if !timed {
                    if exact { println!("Move {} gives score of about {} (exact)", m, score); }
                    else { println!("Move {} gives score of about {}", m, score); }
                }

                history.undo();
                z.pop();

                if aborted { break 'depth_loop; }

                // Save root evaluation for later sampling
                cur_root.push((m, score, exact, shallow));

                if exact && (score > best_sc || (score == best_sc && shallow > best_shallow)) {
                    best_sc = score;
                    best_shallow = shallow;
                    best_m = m;
                }
                if score > alpha { alpha = score; }
            }

            if ctrl.stopped() { break 'depth_loop; }

            if best_sc <= a0 {
                if a0 <= -INF / 2 {
                    // Accept result; also persist root evals for this depth
                    best_move = best_m; last_score = best_sc; break;
                }
                let step = (b0 - a0).abs().max(50);
                alpha = a0.saturating_sub(step);
                continue;
            }
            if best_sc >= b0 {
                if b0 >= INF / 2 {
                    best_move = best_m; last_score = best_sc; break;
                }
                let step = (b0 - a0).abs().max(50);
                beta = b0.saturating_add(step);
                continue;
            }

            // Inside window: exact results at this depth
            best_move = best_m;
            last_score = best_sc;
            break;
        }
    }

    println!("Best move and score: {} with score {}", best_move, last_score);
    println!("Reached depth {}!\n\n", _dep);
    best_move
}



pub fn has_non_pawn_material(pos: &Position, stm: Color) -> bool {
    if pos.state.ply_counter < 60 {
        // If 30 moves or less played, it is highly probable that non pawn
        // material is available. Skip costly check.
        return true
    }

    let queen_bb;
    let rook_bb;
    let bish_bb;
    let knig_bb;
    let bishops;
    let knights;
    let rooks;
    let queens;

    if stm == Color::White {
        queen_bb = pos.pieces(Color::White, Piece::Queen);
        rook_bb  = pos.pieces(Color::White, Piece::Rook);
        bish_bb  = pos.pieces(Color::White, Piece::Bishop);
        knig_bb  = pos.pieces(Color::White, Piece::Knight);

        bishops = bish_bb.0.count_ones() as Score;
        knights = knig_bb.0.count_ones() as Score;
        rooks   = rook_bb.0.count_ones() as Score;
        queens  = queen_bb.0.count_ones() as Score;
    } else {
        queen_bb = pos.pieces(Color::Black, Piece::Queen);
        rook_bb  = pos.pieces(Color::Black, Piece::Rook);
        bish_bb  = pos.pieces(Color::Black, Piece::Bishop);
        knig_bb  = pos.pieces(Color::Black, Piece::Knight);

        bishops = bish_bb.0.count_ones() as Score;
        knights = knig_bb.0.count_ones() as Score;
        rooks   = rook_bb.0.count_ones() as Score;
        queens  = queen_bb.0.count_ones() as Score;
    }

    if (bishops + knights + rooks + queens) != 0 {
        true
    } else {
        false
    }
}




pub fn idwq_null_move(
    root_pos: &Position,
    depth: Option<i32>,
    time_ms: Option<i32>,
    eval_func: fn(&Position) -> Score,
    _random_open: bool,
    z: &mut Zobrist,
    tt: &mut TranspositionTable,
) -> Move {
    let timed = time_ms.is_some();
    if !timed {
        println!("----------------New Move (QUI)--------({} : {})--------",
                 if root_pos.state.stm == Color::White {"white"} else {"black"},
                 root_pos.state.ply_counter);
    }

    let mut history = History::new(root_pos.clone());

    if history.current().state.ply_counter < 2 {
        let mut rng = rand::thread_rng();
        let randnum = rng.gen_range(0.0f32..1.0f32);
        let mut sum = 0.0;
        if history.current().state.stm == Color::White {
            for element in SMALL_OPENING_DICT_WHITE {
                sum += element.1;
                if randnum < sum {
                    return element.0
                }
            }
        } else {
            if let Some(prev) = history.stack.last().map(|u| u.mv) {
                let mut row_idx: Option<usize> = None;
                for (i, &(wm, _)) in SMALL_OPENING_DICT_WHITE.iter().enumerate() {
                    if prev == wm ||
                       (prev.from_sq() == wm.from_sq() && prev.to_sq() == wm.to_sq()) {
                        row_idx = Some(i);
                        break;
                    }
                }
                if let Some(i) = row_idx {
                    for element in SMALL_OPENING_DICT_BLACK[i] {
                        sum += element.1;
                        if randnum < sum {
                            return element.0
                        }
                    }
                }
            }
        }
    }

    let mut root_moves = MoveList::new();
    generate_legal_moves(root_pos, &mut root_moves);
    if root_moves.len == 0 { return Move::default(); }

    let mut killers = [[None; 2]; 256];
    let mut histtbl = [[[0u16; 64]; 64]; 2];
    let mut cm_hist = [[None; 64]; 2];

    let ctrl = SearchCtrl::new(time_ms.map(|ms| Instant::now() + Duration::from_millis(ms as u64)));

    let max_d = if let Some(d) = depth { d.max(1) } else { i32::MAX };

    let mut best_move = root_moves.items[0];
    let mut last_score: Score = 0;
    let mut _dep: i32 = 0;


    'depth_loop: for d in 1..=max_d {
        if ctrl.stopped() { break 'depth_loop; }
        _dep += 1;
        if !timed { println!("Depth: {}", _dep); }

        root_moves.len = 0;
        generate_legal_moves(root_pos, &mut root_moves);

        {
            let tt_hint = Some(best_move).or_else(|| tt.probe(z.hash).and_then(|e| e.best_move));
            let pt_ref = history.piece_table();
            order_moves_in_place(root_pos, pt_ref, &mut root_moves,
                tt_hint, &killers[0], &histtbl, &cm_hist,
                None,
                z.hash,
            );
        }

        let (mut alpha, mut beta) = if d == 1 {
            (-INF, INF)
        } else {
            let near_mate = last_score.abs() > MATE - 1000;
            if near_mate { (-INF, INF) } else { (last_score - 25, last_score + 25) }
        };

        loop {
            if ctrl.stopped() { break 'depth_loop; }  // aspiration widen may iterate; check here too
            let a0 = alpha;
            let b0 = beta;

            let mut best_sc = -INF;
            let mut best_shallow = -INF;
            let mut best_m = root_moves.items[0];

            // Collect current depth’s root evals
            let mut cur_root: Vec<(Move, Score, bool, Score)> = Vec::with_capacity(root_moves.len);

            for (i, m) in root_moves.items[..root_moves.len].iter().copied().enumerate() {
                if ctrl.stopped() { break 'depth_loop; }

                z.push();
                history.apply(m, z);

                let shallow = -eval_stm(history.current(), eval_func);

                let (score, exact, aborted) = if i == 0 {
                    let (child, bt) = negamax_null_move(
                        &mut history, z, -beta, tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                    );
                    (-child, true, matches!(bt, BoundType::Abort))
                } else {
                    let (probe_sc, bt0) = negamax_null_move(
                        &mut history, z, -(alpha + 1), tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                    );
                    if matches!(bt0, BoundType::Abort) {
                        history.undo(); z.pop(); break 'depth_loop;
                    }
                    let mut sc = -probe_sc;
                    let mut ex = false;
                    if sc > alpha {
                       let (child, bt) = negamax_null_move(
                           &mut history, z, -beta, tt, -alpha, d - 1, 1,
                           eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                       );
                       if matches!(bt, BoundType::Abort) {
                           history.undo(); z.pop(); break 'depth_loop;
                       }
                       sc = -child; ex = true;
                    }
                    (sc, ex, false)
                };

                if !timed {
                    if exact { println!("Move {} gives score of about {} (exact)", m, score); }
                    else { println!("Move {} gives score of about {}", m, score); }
                }

                history.undo();
                z.pop();

                if aborted { break 'depth_loop; }

                // Save root evaluation for later sampling
                cur_root.push((m, score, exact, shallow));

                if exact && (score > best_sc || (score == best_sc && shallow > best_shallow)) {
                    best_sc = score;
                    best_shallow = shallow;
                    best_m = m;
                }
                if score > alpha { alpha = score; }
            }

            if ctrl.stopped() { break 'depth_loop; }

            if best_sc <= a0 {
                if a0 <= -INF / 2 {
                    // Accept result; also persist root evals for this depth
                    best_move = best_m; last_score = best_sc; break;
                }
                let step = (b0 - a0).abs().max(50);
                alpha = a0.saturating_sub(step);
                continue;
            }
            if best_sc >= b0 {
                if b0 >= INF / 2 {
                    best_move = best_m; last_score = best_sc; break;
                }
                let step = (b0 - a0).abs().max(50);
                beta = b0.saturating_add(step);
                continue;
            }

            // Inside window: exact results at this depth
            best_move = best_m;
            last_score = best_sc;
            break;
        }
    }

    println!("Best move and score: {} with score {}", best_move, last_score);
    println!("Reached NULLMOVE depth {}!\n\n", _dep);
    best_move
}


pub fn idwq_lmr(
    root_pos: &Position,
    depth: Option<i32>,
    time_ms: Option<i32>,
    eval_func: fn(&Position) -> Score,
    _random_open: bool,
    z: &mut Zobrist,
    tt: &mut TranspositionTable,
) -> Move {
    let timed = time_ms.is_some();
    if !timed {
        println!("----------------New Move (QUI)--------({} : {})--------",
                 if root_pos.state.stm == Color::White {"white"} else {"black"},
                 root_pos.state.ply_counter);
    }

    let mut history = History::new(root_pos.clone());

    if history.current().state.ply_counter < 2 {
        let mut rng = rand::thread_rng();
        let randnum = rng.gen_range(0.0f32..1.0f32);
        let mut sum = 0.0;
        if history.current().state.stm == Color::White {
            for element in SMALL_OPENING_DICT_WHITE {
                sum += element.1;
                if randnum < sum {
                    return element.0
                }
            }
        } else {
            if let Some(prev) = history.stack.last().map(|u| u.mv) {
                let mut row_idx: Option<usize> = None;
                for (i, &(wm, _)) in SMALL_OPENING_DICT_WHITE.iter().enumerate() {
                    if prev == wm ||
                       (prev.from_sq() == wm.from_sq() && prev.to_sq() == wm.to_sq()) {
                        row_idx = Some(i);
                        break;
                    }
                }
                if let Some(i) = row_idx {
                    for element in SMALL_OPENING_DICT_BLACK[i] {
                        sum += element.1;
                        if randnum < sum {
                            return element.0
                        }
                    }
                }
            }
        }
    }

    let mut root_moves = MoveList::new();
    generate_legal_moves(root_pos, &mut root_moves);
    if root_moves.len == 0 { return Move::default(); }

    let mut killers = [[None; 2]; 256];
    let mut histtbl = [[[0u16; 64]; 64]; 2];
    let mut cm_hist = [[None; 64]; 2];

    let ctrl = SearchCtrl::new(time_ms.map(|ms| Instant::now() + Duration::from_millis(ms as u64)));

    let max_d = if let Some(d) = depth { d.max(1) } else { i32::MAX };

    let mut best_move = root_moves.items[0];
    let mut last_score: Score = 0;
    let mut _dep: i32 = 0;


    'depth_loop: for d in 1..=max_d {
        if ctrl.stopped() { break 'depth_loop; }
        _dep += 1;
        if !timed { println!("Depth: {}", _dep); }

        root_moves.len = 0;
        generate_legal_moves(root_pos, &mut root_moves);

        {
            let tt_hint = Some(best_move).or_else(|| tt.probe(z.hash).and_then(|e| e.best_move));
            let pt_ref = history.piece_table();
            order_moves_in_place(root_pos, pt_ref, &mut root_moves,
                tt_hint, &killers[0], &histtbl, &cm_hist,
                None,
                z.hash,
            );
        }

        let (mut alpha, mut beta) = if d == 1 {
            (-INF, INF)
        } else {
            let near_mate = last_score.abs() > MATE - 1000;
            if near_mate { (-INF, INF) } else { (last_score - 25, last_score + 25) }
        };

        loop {
            if ctrl.stopped() { break 'depth_loop; }  // aspiration widen may iterate; check here too
            let a0 = alpha;
            let b0 = beta;

            let mut best_sc = -INF;
            let mut best_shallow = -INF;
            let mut best_m = root_moves.items[0];

            // Collect current depth’s root evals
            let mut cur_root: Vec<(Move, Score, bool, Score)> = Vec::with_capacity(root_moves.len);

            for (i, m) in root_moves.items[..root_moves.len].iter().copied().enumerate() {
                if ctrl.stopped() { break 'depth_loop; }

                z.push();
                history.apply(m, z);

                let shallow = -eval_stm(history.current(), eval_func);

                let (score, exact, aborted) = if i == 0 {
                    let (child, bt) = negamax_null_move_lmr(
                        &mut history, z, -beta, tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                    );
                    (-child, true, matches!(bt, BoundType::Abort))
                } else {
                    let (probe_sc, bt0) = negamax_null_move_lmr(
                        &mut history, z, -(alpha + 1), tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                    );
                    if matches!(bt0, BoundType::Abort) {
                        history.undo(); z.pop(); break 'depth_loop;
                    }
                    let mut sc = -probe_sc;
                    let mut ex = false;
                    if sc > alpha {
                       let (child, bt) = negamax_null_move_lmr(
                           &mut history, z, -beta, tt, -alpha, d - 1, 1,
                           eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                       );
                       if matches!(bt, BoundType::Abort) {
                           history.undo(); z.pop(); break 'depth_loop;
                       }
                       sc = -child; ex = true;
                    }
                    (sc, ex, false)
                };

                if !timed {
                    if exact { println!("Move {} gives score of about {} (exact)", m, score); }
                    else { println!("Move {} gives score of about {}", m, score); }
                }

                history.undo();
                z.pop();

                if aborted { break 'depth_loop; }

                // Save root evaluation for later sampling
                cur_root.push((m, score, exact, shallow));

                if exact && (score > best_sc || (score == best_sc && shallow > best_shallow)) {
                    best_sc = score;
                    best_shallow = shallow;
                    best_m = m;
                }
                if score > alpha { alpha = score; }
            }

            if ctrl.stopped() { break 'depth_loop; }

            if best_sc <= a0 {
                if a0 <= -INF / 2 {
                    // Accept result; also persist root evals for this depth
                    best_move = best_m; last_score = best_sc; break;
                }
                let step = (b0 - a0).abs().max(50);
                alpha = a0.saturating_sub(step);
                continue;
            }
            if best_sc >= b0 {
                if b0 >= INF / 2 {
                    best_move = best_m; last_score = best_sc; break;
                }
                let step = (b0 - a0).abs().max(50);
                beta = b0.saturating_add(step);
                continue;
            }

            // Inside window: exact results at this depth
            best_move = best_m;
            last_score = best_sc;
            break;
        }
    }

    println!("Best move and score: {} with score {}", best_move, last_score);
    println!("Reached LMR depth {}!\n\n", _dep);
    best_move
}




// --- helpers used by ordering ---

#[inline]
fn order_jitter_from(hash: u64, m: Move, max_cp: i32) -> i32 {
    if max_cp <= 0 { return 0; }
    // Derive a per-move pseudo-random from (position hash ⊕ move)
    let mut x = hash ^ (m.0 as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    x ^= x >> 30; x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x ^= x >> 27; x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^= x >> 31;
    // Map to [-max_cp, +max_cp] (approximately)
    let v = (x & 0xFFFF) as i32;          // 0..65535
    let centered = v - 32768;             // -32768..32767
    (centered * max_cp) / 32768
}



// MVV-LVA with a small promo bump; EP treated as decent capture
#[inline]
fn mvv_lva_fast(
    pt: &[Option<(Color, Piece)>; 64],
    m: Move
) -> i32 {
    #[inline] fn pv(p: Piece) -> i32 {
        match p { Piece::Pawn=>100, Piece::Knight=>300, Piece::Bishop=>325,
                  Piece::Rook=>500, Piece::Queen=>900, Piece::King=>10_000 }
    }
    let mut s = 0;
    if m.is_promo() { s += 500; }
    if m.is_capture() {
        let to = m.to_sq().0 as usize;
        if let Some((_, vic)) = pt[to] {
            let att = pt[m.from_sq().0 as usize].map(|(_,p)| pv(p)).unwrap_or(100);
            s += 1000 + pv(vic) - att / 10;
        } else {
            s += 900; // EP path
        }
    }
    s
}

#[inline]
fn order_moves_in_place(
    pos: &Position,
    pt: &[Option<(Color, Piece)>; 64],
    list: &mut crate::game::moves::MoveList,
    tt_best: Option<Move>,
    killers: &[Option<Move>; 2],
    history: &[[[u16; 64]; 64]; 2],
    cm_hist: &[[Option<Move>; 64]; 2],
    prev_to: Option<u8>, // opponent's last to-square at this node, if any
    pos_key: u64,        // NEW: zobrist key for deterministic jitter
) {
    let side = pos.state.stm as usize;
    let len = list.len;
    if len == 0 { return; }

    // Root-opening jitter: only when prev_to is None (root) and very early game.
    let open_ply = pos.state.ply_counter as i32;
    let jitter_cap_cp: i32 = if prev_to.is_none() && open_ply < 10 {
        // Up to ~30 cp at ply 0, decays linearly; tweak to taste
        (30 - 2 * open_ply).max(0)
    } else { 0 };

    // ... (existing buckets & constants)
    let mut tt_bucket: Option<(Move, i32)> = None;
    let mut tacticals: Vec<(Move, i32)> = Vec::with_capacity(len);
    let mut quiets:    Vec<(Move, i32)> = Vec::with_capacity(len);

    #[inline] fn base_quiet_score() -> i32 { 1 }

    let cm_bonus = 40_000;
    let killer_bonus = 50_000;
    let tt_bonus = 1_000_000;

    let cm_target = prev_to
        .and_then(|to| cm_hist[side][to as usize])
        .map(|m| m);

    for &m in list.items[..len].iter() {
        let mut sc = 0i32;

        if Some(m) == tt_best {
            tt_bucket = Some((m, tt_bonus));
            continue;
        }
        if killers.iter().any(|&k| k == Some(m)) { sc += killer_bonus; }
        if cm_target.is_some() && Some(m) == cm_target { sc += cm_bonus; }

        if m.is_capture() || m.is_promo() {
            sc += mvv_lva_fast(pt, m);
            // tiny jitter even on tacticals, but half strength
            if jitter_cap_cp > 0 {
                sc += order_jitter_from(pos_key, m, jitter_cap_cp / 2);
            }
            tacticals.push((m, sc));
        } else {
            sc += history[side][m.from_sq().0 as usize][m.to_sq().0 as usize] as i32;
            sc += base_quiet_score();
            if jitter_cap_cp > 0 {
                sc += order_jitter_from(pos_key, m, jitter_cap_cp);
            }
            quiets.push((m, sc));
        }
    }

    tacticals.sort_unstable_by(|a, b| b.1.cmp(&a.1));
    quiets.sort_unstable_by(|a, b| b.1.cmp(&a.1));

    let mut write = 0usize;
    if let Some((m, _)) = tt_bucket.take() { list.items[write] = m; write += 1; }
    for (m, _) in tacticals.into_iter() { list.items[write] = m; write += 1; }
    for (m, _) in quiets.into_iter()    { list.items[write] = m; write += 1; }
    debug_assert_eq!(write, len, "phase rebuild size mismatch");
}





















// --------------------- ##### SCORING ###### ------------------------------------



// Timed, score-only iterative deepening for the GUI eval bar.
pub fn iterative_deepening_score(
    root_pos: &Position,
    time_ms: i32,
    eval_func: fn(&Position) -> Score,
    z: &mut Zobrist,
    tt: &mut TranspositionTable,
) -> Score {
    use std::time::{Duration, Instant};

    let mut history = History::new(root_pos.clone());
    let mut root_moves = MoveList::new();
    generate_legal_moves(root_pos, &mut root_moves);
    if root_moves.len == 0 {
        if in_check(root_pos, root_pos.state.stm) { return -MATE; }
        return DRAW;
    }

    let mut killers = [[None; 2]; 256];
    let mut histtbl = [[[0u16; 64]; 64]; 2];
    let mut cm_hist = [[None; 64]; 2];

    let ctrl = SearchCtrl::new(Some(Instant::now() + Duration::from_millis(time_ms as u64)));

    {
        let tt_hint = tt.probe(z.hash).and_then(|e| e.best_move);
        let pt_ref = history.piece_table();
        order_moves_in_place(root_pos, pt_ref, &mut root_moves,
            tt_hint, &killers[0], &histtbl, &cm_hist,
            None,
            z.hash,
        );
    }

    let mut last_score: Score = 0;
    let max_d = 99;

    'depths: for d in 1..=max_d {
        if ctrl.stopped() { break 'depths; }

        root_moves.len = 0;
        generate_legal_moves(root_pos, &mut root_moves);

        {
            let tt_hint = tt.probe(z.hash).and_then(|e| e.best_move);
            let pt_ref  = history.piece_table();
            order_moves_in_place(root_pos, pt_ref, &mut root_moves,
                tt_hint, &killers[0], &histtbl, &cm_hist,
                None,
                z.hash,
            );
        }

        let near_mate = last_score.abs() > MATE - 1000;
        let (mut alpha, mut beta) = if d == 1 || near_mate { (-INF, INF) } else { (last_score - 25, last_score + 25) };

        loop {
            if ctrl.stopped() { break 'depths; }

            let a0 = alpha;
            let b0 = beta;
            let mut best_sc = -INF;

            for (i, m) in root_moves.items[..root_moves.len].iter().copied().enumerate() {
                if ctrl.stopped() { break 'depths; }

                z.push();
                history.apply(m, z);

                let score = if i == 0 {
                    let (child, _bt) = negamax_with_quiescence(
                        &mut history, z, -beta, tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                    );
                    -child
                } else {
                    let (probe_sc, bt0) = negamax_with_quiescence(
                        &mut history, z, -(alpha + 1), tt, -alpha, d - 1, 1,
                        eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                    );
                    if matches!(bt0, BoundType::Abort) { history.undo(); z.pop(); break 'depths; }
                    let mut sc = -probe_sc;
                    if sc > alpha {
                        let (child, _bt) = negamax_with_quiescence(
                            &mut history, z, -beta, tt, -alpha, d - 1, 1,
                            eval_func, &mut killers, &mut histtbl, &mut cm_hist, &ctrl
                        );
                        sc = -child;
                    }
                    sc
                };

                history.undo();
                z.pop();

                if score > best_sc { best_sc = score; }
                if score > alpha { alpha = score; }
            }

            if best_sc <= a0 {
                if a0 <= -INF / 2 { last_score = best_sc; break; }
                let step = (b0 - a0).abs().max(50);
                alpha = a0.saturating_sub(step);
                continue;
            }
            if best_sc >= b0 {
                if b0 >= INF / 2 { last_score = best_sc; break; }
                let step = (b0 - a0).abs().max(50);
                beta = b0.saturating_add(step);
                continue;
            }

            last_score = best_sc;
            break;
        }
    }

    last_score
}



