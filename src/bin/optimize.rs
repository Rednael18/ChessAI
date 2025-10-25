// src/bin/optimize.rs
// Evolutionary optimization of evaluation weights for a chess engine.
// - Log-loss (cross-entropy) fitness
// - Proper PSQT mirroring for the opponent
// - Sensible mutation scales
// - Fixed fitness subset for stabler progress
// - Tees all console output to logs.txt
// - Saves full, copy-pasteable Rust constants (material, structure, PSQTs MG/EG, K_SIG)

use std::fs::{File, OpenOptions};
use std::io::{BufReader, Write};
use std::io::ErrorKind as IoErrorKind;
use std::path::PathBuf;
use std::time::Instant;

use anyhow::{Context, Result};
use rand::prelude::*;
use serde::{Deserialize, Serialize};

// ------------------------- Tunables -------------------------

const INPUT_BIN: &str = "./data/texel_dump.bin";
const MAX_SAMPLES: Option<usize> = None;

const POPULATION_SIZE: usize = 100;
const NUM_EPOCHS: usize = 10_000;
const ELITISM_COUNT: usize = 2;
const TOURNAMENT_SIZE: usize = 5;
const MUTATION_RATE: f64 = 0.02;
const FITNESS_SAMPLE_SIZE: usize = 4000; // larger for stability
const SAVE_EVERY: usize = 200;

// ------------------------- Data -------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
struct DumpRow {
    // [my P,N,B,R,Q,K, opp P,N,B,R,Q,K]
    bb: [u64; 12],
    castling: u8,
    ep: Option<u8>,
    // -1.0 loss, 0.0 draw, +1.0 win (from side-to-move perspective)
    y: f32,
    mover_is_white: bool,
    was_mirrored: bool,
}

// ------------------------- Model layout -------------------------

const NUM_ROLES: usize = 6;
const NUM_SQ: usize = 64;
const MAT_ROLES: usize = 5; // Pawn..Queen (King not used for material)

#[inline] fn popcnt(x: u64) -> i32 { x.count_ones() as i32 }
const FILE_MASK_BASE: u64 = 0x0101010101010101u64;

#[derive(Clone, Debug)]
struct Weights {
    // Material values by phase
    mat_mg: [f32; MAT_ROLES],
    mat_eg: [f32; MAT_ROLES],
    // Piece-Square Tables by phase
    psqt_mg: [[f32; NUM_SQ]; NUM_ROLES],
    psqt_eg: [[f32; NUM_SQ]; NUM_ROLES],
    // Other parameters
    doubled_pawn_penalty: f32,
    attack_weight: f32,
    k_sig: f32, // logistic scale (centipawns^-1)
}

impl Weights {
    fn default_starting() -> Self {
        Self {
            mat_mg: [100.0, 300.0, 325.0, 500.0, 900.0],
            mat_eg: [120.0, 310.0, 335.0, 525.0, 950.0],
            psqt_mg: [[0.0; NUM_SQ]; NUM_ROLES],
            psqt_eg: [[0.0; NUM_SQ]; NUM_ROLES],
            doubled_pawn_penalty: 15.0,
            attack_weight: 3.0,
            k_sig: 1.0 / 350.0,
        }
    }
}

// ------------------------- Logging (tee to file) -------------------------

struct TeeLogger {
    file: File,
}
impl TeeLogger {
    fn new(path: &str) -> Result<Self> {
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true) // fresh log each run
            .open(path)
            .with_context(|| format!("opening {}", path))?;
        Ok(Self { file })
    }
    fn log(&mut self, line: &str) {
        eprintln!("{}", line);
        let _ = writeln!(self.file, "{}", line);
    }
}

// ------------------------- GA Individual -------------------------

#[derive(Clone, Debug)]
struct Individual {
    weights: Weights,
    fitness: f32,
}
impl Individual {
    fn new(weights: Weights) -> Self { Self { weights, fitness: 0.0 } }
}

// ------------------------- Eval from DumpRow -------------------------

#[inline]
fn endgame_bool(bb: &[u64; 12]) -> bool {
    let my_q = popcnt(bb[4]);
    let my_r = popcnt(bb[3]);
    let op_q = popcnt(bb[10]);
    let op_r = popcnt(bb[9]);
    (my_q == 0 && op_q == 0) || (my_q + my_r + op_q + op_r < 4)
}

#[inline]
fn extras_on_file(pawns: u64, file: usize) -> i32 {
    let cnt = (pawns & (FILE_MASK_BASE << file)).count_ones() as i32;
    (cnt - 1).max(0)
}

#[inline]
fn doubled_extras(pawns: u64) -> i32 {
    (0..8).map(|f| extras_on_file(pawns, f)).sum()
}

#[inline]
fn mirror_sq(sq: usize) -> usize { (7 - (sq / 8)) * 8 + (sq % 8) }

#[inline]
fn sum_psqt(bits: u64, tbl: &[f32; NUM_SQ]) -> f32 {
    let mut b = bits;
    let mut acc = 0.0;
    while b != 0 {
        let sq = b.trailing_zeros() as usize;
        acc += tbl[sq];
        b &= b - 1;
    }
    acc
}

#[inline]
fn sum_psqt_mirrored(bits: u64, tbl: &[f32; NUM_SQ]) -> f32 {
    let mut b = bits;
    let mut acc = 0.0;
    while b != 0 {
        let sq = b.trailing_zeros() as usize;
        acc += tbl[mirror_sq(sq)];
        b &= b - 1;
    }
    acc
}

/// Evaluates a position in **centipawns** from the side-to-move perspective.
fn eval_cp(row: &DumpRow, w: &Weights) -> f32 {
    let eg = endgame_bool(&row.bb);

    // Material
    let mut mat = 0.0f32;
    for (role_i, bb_idx_my, bb_idx_op) in [(0usize, 0, 6), (1, 1, 7), (2, 2, 8), (3, 3, 9), (4, 4, 10)] {
        let my_count = popcnt(row.bb[bb_idx_my]) as f32;
        let opp_count = popcnt(row.bb[bb_idx_op]) as f32;
        let diff = my_count - opp_count;
        mat += if eg { w.mat_eg[role_i] } else { w.mat_mg[role_i] } * diff;
    }

    // Piece-Square Tables (mirror opponent)
    let mut pst = 0.0f32;
    for (role_i, bb_idx_my, bb_idx_op) in [(0, 0, 6), (1, 1, 7), (2, 2, 8), (3, 3, 9), (4, 4, 10), (5, 5, 11)] {
        if eg {
            pst += sum_psqt(row.bb[bb_idx_my], &w.psqt_eg[role_i]);
            pst -= sum_psqt_mirrored(row.bb[bb_idx_op], &w.psqt_eg[role_i]);
        } else {
            pst += sum_psqt(row.bb[bb_idx_my], &w.psqt_mg[role_i]);
            pst -= sum_psqt_mirrored(row.bb[bb_idx_op], &w.psqt_mg[role_i]);
        }
    }

    // Doubled Pawns
    let my_doubled = doubled_extras(row.bb[0]);
    let opp_doubled = doubled_extras(row.bb[6]);
    let doubled_term = -w.doubled_pawn_penalty * (my_doubled - opp_doubled) as f32;

    // Simple attack term
    let my_attack = popcnt(row.bb[1]) + popcnt(row.bb[2]) * 2 + popcnt(row.bb[3]) * 3 + popcnt(row.bb[4]) * 5;
    let opp_attack = popcnt(row.bb[7]) + popcnt(row.bb[8]) * 2 + popcnt(row.bb[9]) * 3 + popcnt(row.bb[10]) * 5;
    let attack_term = (my_attack - opp_attack) as f32 * w.attack_weight;

    mat + pst + doubled_term + attack_term
}

// ------------------------- Fitness (log-loss) -------------------------

fn calculate_fitness(weights: &Weights, samples: &[DumpRow]) -> f32 {
    let mut total_loss = 0.0f32;
    for row in samples {
        let cp = eval_cp(row, weights);
        let mut p = 1.0 / (1.0 + (-weights.k_sig * cp).exp());
        p = p.clamp(1e-6, 1.0 - 1e-6);
        let t = 0.5 * (row.y + 1.0); // -1,0,1 -> 0,0.5,1
        total_loss += -(t * p.ln() + (1.0 - t) * (1.0 - p).ln());
    }
    -total_loss / samples.len() as f32 // maximize => minimize loss
}

// ------------------------- GA Ops -------------------------

fn mutate(weights: &mut Weights, rng: &mut ThreadRng, rate: f64) {
    let mut mutate_scaled = |val: &mut f32, strength: f32| {
        if rng.gen_bool(rate) {
            *val += rng.gen_range(-strength..strength);
        }
    };
    // Material (MG/EG)
    for i in 0..MAT_ROLES {
        mutate_scaled(&mut weights.mat_mg[i], 30.0);
        mutate_scaled(&mut weights.mat_eg[i], 30.0);
    }
    // PSQTs (MG/EG)
    for r in 0..NUM_ROLES {
        for s in 0..NUM_SQ {
            mutate_scaled(&mut weights.psqt_mg[r][s], 10.0);
            mutate_scaled(&mut weights.psqt_eg[r][s], 10.0);
        }
    }
    // Others
    mutate_scaled(&mut weights.doubled_pawn_penalty, 5.0);
    mutate_scaled(&mut weights.attack_weight, 1.0);
    mutate_scaled(&mut weights.k_sig, 0.0001); // keep near ~1/350
}

fn crossover(p1: &Weights, p2: &Weights, rng: &mut ThreadRng) -> Weights {
    let mut child = p1.clone();
    for i in 0..MAT_ROLES {
        if rng.gen_bool(0.5) { child.mat_mg[i] = p2.mat_mg[i]; }
        if rng.gen_bool(0.5) { child.mat_eg[i] = p2.mat_eg[i]; }
    }
    for r in 0..NUM_ROLES {
        for s in 0..NUM_SQ {
            if rng.gen_bool(0.5) { child.psqt_mg[r][s] = p2.psqt_mg[r][s]; }
            if rng.gen_bool(0.5) { child.psqt_eg[r][s] = p2.psqt_eg[r][s]; }
        }
    }
    if rng.gen_bool(0.5) { child.doubled_pawn_penalty = p2.doubled_pawn_penalty; }
    if rng.gen_bool(0.5) { child.attack_weight = p2.attack_weight; }
    if rng.gen_bool(0.5) { child.k_sig = p2.k_sig; }
    child
}

fn tournament_selection<'a>(pop: &'a [Individual], rng: &mut ThreadRng) -> &'a Individual {
    pop.choose_multiple(rng, TOURNAMENT_SIZE)
        .max_by(|a, b| a.fitness.partial_cmp(&b.fitness).unwrap())
        .unwrap()
}

fn initialize_population(rng: &mut ThreadRng) -> Vec<Individual> {
    (0..POPULATION_SIZE)
        .map(|_| {
            let mut w = Weights::default_starting();
            // strong init randomization for diversity
            mutate(&mut w, rng, 1.0);
            Individual::new(w)
        })
        .collect()
}

// ------------------------- I/O -------------------------

fn read_rows(path: &PathBuf, max: Option<usize>) -> Result<Vec<DumpRow>> {
    let f = File::open(path).with_context(|| format!("opening {}", path.display()))?;
    let mut rdr = BufReader::new(f);
    let mut out = Vec::new();
    loop {
        if let Some(m) = max {
            if out.len() >= m { break; }
        }
        match bincode::deserialize_from::<_, DumpRow>(&mut rdr) {
            Ok(row) => out.push(row),
            Err(e) => {
                if let bincode::ErrorKind::Io(ref ioe) = *e {
                    if ioe.kind() == IoErrorKind::UnexpectedEof { break; }
                }
                return Err(anyhow::anyhow!("bincode::deserialize_from failed: {}", e));
            }
        }
    }
    Ok(out)
}

// ------------------------- Save pretty, copy-pasteable weights -------------------------

fn save_weights_readable(path: &str, w: &Weights) -> Result<()> {
    let role_names = ["PAWN","KNIGHT","BISHOP","ROOK","QUEEN","KING"];
    let mut s = String::new();

    s.push_str("// ======== Trained Evaluation Weights ========\n");
    s.push_str("// Units: centipawns. PSQTs are indexed a1..h8 (row-major by ranks).\n\n");

    // Scalars / small arrays
    s.push_str(&format!("pub const K_SIG: f32 = {:.9};\n", w.k_sig));
    s.push_str(&format!("pub const DOUBLED_PAWN_PENALTY: i32 = {};\n", w.doubled_pawn_penalty.round() as i32));
    s.push_str(&format!("pub const ATTACK_WEIGHT: i32 = {};\n\n", w.attack_weight.round() as i32));

    s.push_str("// --- Material (MG): [P, N, B, R, Q]\n");
    s.push_str(&format!(
        "pub const MATERIAL_MG: [i32; 5] = [{}, {}, {}, {}, {}];\n\n",
        w.mat_mg[0].round() as i32, w.mat_mg[1].round() as i32, w.mat_mg[2].round() as i32,
        w.mat_mg[3].round() as i32, w.mat_mg[4].round() as i32,
    ));

    s.push_str("// --- Material (EG): [P, N, B, R, Q]\n");
    s.push_str(&format!(
        "pub const MATERIAL_EG: [i32; 5] = [{}, {}, {}, {}, {}];\n\n",
        w.mat_eg[0].round() as i32, w.mat_eg[1].round() as i32, w.mat_eg[2].round() as i32,
        w.mat_eg[3].round() as i32, w.mat_eg[4].round() as i32,
    ));

    // Helper to format a 64-entry PSQT as 8 lines of 8
    fn fmt_tbl(tbl: &[f32; NUM_SQ]) -> String {
        let mut out = String::new();
        for r in 0..8 {
            let line = (0..8)
                .map(|f| (tbl[r * 8 + f].round() as i16).to_string())
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!("    {},\n", line));
        }
        out
    }

    // PSQTs MG/EG for all roles
    for r in 0..NUM_ROLES {
        let name = role_names[r];
        s.push_str(&format!("// --- PSQT MG {} ---\n", name));
        s.push_str(&format!("pub const PSQT_MG_{}: [i16; 64] = [\n", name));
        s.push_str(&fmt_tbl(&w.psqt_mg[r]));
        s.push_str("];\n\n");

        s.push_str(&format!("// --- PSQT EG {} ---\n", name));
        s.push_str(&format!("pub const PSQT_EG_{}: [i16; 64] = [\n", name));
        s.push_str(&fmt_tbl(&w.psqt_eg[r]));
        s.push_str("];\n\n");
    }

    if let Some(parent) = std::path::Path::new(path).parent() {
        std::fs::create_dir_all(parent).ok();
    }
    let mut file = File::create(path).with_context(|| format!("creating {}", path))?;
    file.write_all(s.as_bytes())?;
    Ok(())
}

// ------------------------- Main -------------------------

fn main() -> Result<()> {
    let mut log = TeeLogger::new("logs.txt")?;

    let start_time = Instant::now();
    let path = PathBuf::from(std::env::args().nth(1).unwrap_or_else(|| INPUT_BIN.to_string()));
    let all_rows = read_rows(&path, MAX_SAMPLES)?;
    if all_rows.is_empty() {
        log.log(&format!("No samples found in {}.", path.display()));
        return Ok(());
    }
    log.log(&format!("Loaded {} samples.", all_rows.len()));

    let mut rng = thread_rng();
    let fitness_samples: Vec<DumpRow> = all_rows
        .choose_multiple(&mut rng, FITNESS_SAMPLE_SIZE)
        .cloned()
        .collect();
    log.log(&format!("Using {} fixed samples for fitness.", fitness_samples.len()));

    // Initialization
    log.log(&format!("Initializing population of size {}...", POPULATION_SIZE));
    let mut population = initialize_population(&mut rng);

    // Evolution loop
    for epoch in 1..=NUM_EPOCHS {
        // Evaluate
        for ind in &mut population {
            ind.fitness = calculate_fitness(&ind.weights, &fitness_samples);
        }

        // Sort & log
        population.sort_by(|a, b| b.fitness.partial_cmp(&a.fitness).unwrap());
        let best = population[0].fitness;
        let worst = population.last().unwrap().fitness;
        let avg = population.iter().map(|i| i.fitness).sum::<f32>() / POPULATION_SIZE as f32;

        log.log(&format!(
            "Epoch {:>3}/{}: Best = {:.6}, Avg = {:.6}, Worst = {:.6}",
            epoch, NUM_EPOCHS, best, avg, worst
        ));

        // Periodic save of full constants
        if epoch % SAVE_EVERY == 0 {
            let out = format!("./data/values_{}.txt", epoch);
            save_weights_readable(&out, &population[0].weights)?;
            log.log(&format!("-> Saved best weights of epoch {} to {}", epoch, out));
        }

        // Next generation
        let mut next = Vec::with_capacity(POPULATION_SIZE);
        for i in 0..ELITISM_COUNT {
            next.push(population[i].clone());
        }
        while next.len() < POPULATION_SIZE {
            let p1 = tournament_selection(&population, &mut rng);
            let p2 = tournament_selection(&population, &mut rng);
            let mut child = crossover(&p1.weights, &p2.weights, &mut rng);
            mutate(&mut child, &mut rng, MUTATION_RATE);
            next.push(Individual::new(child));
        }
        population = next;
    }

    log.log(&format!("Training finished in {:.2?}.", start_time.elapsed()));
    let best = population
        .iter()
        .max_by(|a, b| a.fitness.partial_cmp(&b.fitness).unwrap())
        .unwrap();

    let final_path = "./data/values_final.txt";
    save_weights_readable(final_path, &best.weights)?;
    log.log(&format!("Saved final best weights to {}", final_path));

    Ok(())
}
