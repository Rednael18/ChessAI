// src/game/moves.rs

use crate::game::board::{self, BitBoard, PieceMovement, Position, KNIGHT_ATK, KING_ATK, WHITE_PAWN_ATK, BLACK_PAWN_ATK};
use crate::game::defs::{self, Square, Color, Piece};
use crate::game::gamestate;
use std::fmt;

use std::sync::OnceLock;

// near your OnceLock
pub static MAGICS: OnceLock<Magics> = OnceLock::new();

#[inline]
pub fn magics() -> &'static Magics {
    MAGICS.get_or_init(|| Magics::new())
}




// #############################
//           MOVE DEF
// ############################

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Move(pub(crate) u16);

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum Flag {
    QUIET           = 0b0000,
    DOUBLE_PUSH     = 0b0001,
    CASTLE_KING     = 0b0010,
    CASTLE_QUEEN    = 0b0011,
    CAPTURE         = 0b0100,
    EP_CAPTURE      = 0b0101,

    PROMO_N         = 0b1000,
    PROMO_B         = 0b1001,
    PROMO_R         = 0b1010,
    PROMO_Q         = 0b1011,
    PROMO_N_CAPTURE = 0b1100,
    PROMO_B_CAPTURE = 0b1101,
    PROMO_R_CAPTURE = 0b1110,
    PROMO_Q_CAPTURE = 0b1111,
}

impl Move {
    #[inline]
    pub const fn pack(from: defs::Square, to: defs::Square, flags: Flag) -> Self {
        Self(
              ((from.0 as u16 & 0x3F) << 10)  // Magic num 0x3F = 0b111111
            | ((to.0   as u16 & 0x3F) << 4)
            | (flags   as u16 & 0x0F)  // Magic num 0x0F = 0b1111
        )
    }

    #[inline] pub const fn from_sq(self) -> defs::Square { defs::Square(((self.0 >> 10) & 0x3F) as u8) }
    #[inline] pub const fn to_sq(self)   -> defs::Square { defs::Square(((self.0 >>  4) & 0x3F) as u8) }
    #[inline] pub const fn flags(self)   -> u16          { self.0 & 0x0F }
    #[inline] pub const fn is_capture(self) -> bool      { (self.0 & 0x0F) & 0b0100 != 0}
    #[inline] pub const fn is_promo(self)   -> bool      { (self.0 & 0x0F) & 0b1000 != 0}
    #[inline] pub const fn butterfly_index(self) -> u16  { self.0 >> 4 }
    #[inline] pub const fn as_u16(self)  -> u16          { self.0 }

    pub fn to_uci(self) -> String {
        let from = self.from_sq();
        let to = self.to_sq();
        let mut s = format!("{}{}", from, to);

        // Append promotion piece if this is a promotion move.
        match self.flags() {
            x if x == Flag::PROMO_N as u16 || x == Flag::PROMO_N_CAPTURE as u16 => s.push('n'),
            x if x == Flag::PROMO_B as u16 || x == Flag::PROMO_B_CAPTURE as u16 => s.push('b'),
            x if x == Flag::PROMO_R as u16 || x == Flag::PROMO_R_CAPTURE as u16 => s.push('r'),
            x if x == Flag::PROMO_Q as u16 || x == Flag::PROMO_Q_CAPTURE as u16 => s.push('q'),
            _ => {}
        }

        s
    }

}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

#[derive(Clone, Copy)]
pub struct ScoredMove {
    pub mv: Move,
    pub score: f32
}

pub struct ScoredMoveList {
    pub len: usize,
    pub items: [ScoredMove; MAX_MOVES],
}

impl ScoredMoveList {
    pub const fn new() -> Self {
        Self { len: 0, items: [ScoredMove{mv: Move(0), score: -30_000.0}; MAX_MOVES] }
    }

    #[inline]
    #[allow(dead_code)]
    pub fn push(&mut self, m: ScoredMove) {
        debug_assert!(self.len < MAX_MOVES);
        self.items[self.len] = m;
        self.len += 1;
    }

    #[inline]
    #[allow(dead_code)]
    pub fn delete(&mut self) {
        self.items[self.len] = ScoredMove{ mv: Move(0), score : -30_000.0 };
        self.len -= 1;
    }

    #[allow(dead_code)]
    pub fn iter(&self) -> impl Iterator<Item=&ScoredMove> {
        self.items[..self.len].iter()
    }
}


// #############################
//        GENERATE MOVES
// ############################



// ---------- Magic bitboards -----------------

#[allow(non_snake_case)]
pub struct MagicTable {
    pub magic:   [u64; 64],
    pub mask:    [u64; 64],
    pub shift:   [u8; 64],
    pub offset:  [usize; 64],
    pub attacks: Vec<u64>
}

impl MagicTable {
    #[inline]
    pub fn attacks(&self, sq: Square, occ: u64) -> u64 {
        let s = sq.0 as usize;
        let idx = self.offset[s]
            + (((occ & self.mask[s]).wrapping_mul(self.magic[s])) >> self.shift[s]) as usize;
        debug_assert!(idx < self.attacks.len());
        unsafe { *self.attacks.get_unchecked(idx) }
    }
}


pub struct Magics {
    pub rook: MagicTable,
    pub bishop: MagicTable,
}

impl Magics {
    pub fn new() -> Self {
        pub const ROOK_MAGIC: [u64; 64] = [
            324268244150067216u64,
            18014467241615360u64,
            144133098098016288u64,
            9835879179034305920u64, // 3
            2341893796599431808u64,
            72058693650744320u64,
            72138957906837508u64,
            36031718687244416u64, // 7
            9429417088524417u64,
            3448137205161984u64,
            599541975809556992u64,
            24206986510467089u64, // 11
            9225764591361853442u64,
            1299570010489946152u64,
            2324701927143178244u64,
            289497015695460608u64, // 15
            1188950576547758144u64,
            22518273019486208u64,
            150083874115584u64,
            72480356527245318u64, // 19
            622061898095134864u64,
            282574522155016u64,
            1513354610365432208u64,
            4505798717767745u64, // 23
            36033475312148480u64,
            9042385782718464u64,
            9042385782706176u64,
            146375786131095680u64, // 27
            9241461204310820864u64,
            11263399264125056u64,
            4509183089578022u64,
            1549379062991045888u64, // 31
            36029072077234177u64,
            282033356030564u64,
            648537875641540608u64,
            149535804362752u64, // 35
            144119588278257664u64,
            563023001422856u64,
            867242304870547969u64,
            140772930224896u64, // 39
            36068964627415040u64,
            2312633594369556480u64,
            2882374688608223264u64,
            4611704443839315978u64, // 43
            743376530318622736u64,
            288793394971541572u64,
            576746633916645632u64,
            288232035151118337u64, // 47
            9337371051694237952u64,
            2900459172409647360u64,
            140806209929344u64,
            1153255760570908800u64, // 51
            9233826195559678080u64,
            146648471594860800u64,
            9288831031312640u64,
            117375068610339072u64, // 55
            88510713348353u64,
            4621348666215514117u64,
            145276555823317074u64,
            4611862391259680769u64, // 59
            72620612914251778u64,
            844459325784069u64,
            36031064895947012u64,
            714962008014946u64, // 63
        ];

        pub const BISHOP_MAGIC: [u64; 64] = [
            580610875736196u64,
            23564740739597186u64,
            326529222334482440u64,
            77691588255417536u64, // 3
            1130985450111360u64,
            4611972303834579969u64,
            1130315334811664u64,
            2315207556594663936u64, // 7
            9223970858442211456u64,
            18031995028275712u64,
            576469583309971472u64,
            4450265595906u64, // 11
            2305865019312570880u64,
            217523137082843666u64,
            4045642047909150722u64,
            4710837787133356048u64, // 15
            434880007787511936u64,
            9009433585680512u64,
            436849172780421152u64,
            108649358224916772u64, // 19
            438540779691705378u64,
            35192970682377u64,
            649081313491486720u64,
            576847870658939912u64, // 23
            27035144259896384u64,
            1319572837126377617u64,
            1155175503460828416u64,
            9804340787373408264u64, // 27
            1153488854787851781u64,
            36100815047401473u64,
            9232521077431212034u64,
            2594372731718076560u64, // 31
            1130573932306952u64,
            576614890645490304u64,
            2595201520813278208u64,
            289359031280599552u64, // 35
            1154329996182815008u64,
            148621056000790529u64,
            289958812725561412u64,
            54330187963237376u64, // 39
            144260529904435472u64,
            2306040990160650752u64,
            35326911842304u64,
            5931291424623436032u64, // 43
            74036998572082176u64,
            9042389024940161u64,
            6944551733516304896u64,
            9800118666640099842u64, // 47
            9235778941229858816u64,
            900755763354943488u64,
            2305845209479021065u64,
            578862094850990096u64, // 51
            2379167309467090945u64,
            576496074418061450u64,
            2938952834131167360u64,
            1161933104089677892u64, // 55
            18298381948686900u64,
            18312403876054272u64,
            11529496522221159426u64,
            4611690528285924352u64, // 59
            1170936316574368768u64,
            8814904738050u64,
            7084338273224524178u64,
            93458500239262209u64, // 63
        ];


        let mut r_mask = [0u64; 64];
        let mut b_mask = [0u64; 64];
        let mut r_shift = [0u8; 64];
        let mut b_shift = [0u8; 64];

        for s in 0..64 {
            let sq = crate::game::defs::Square(s as u8);
            r_mask[s] = rook_relevant_mask(sq);
            b_mask[s] = bishop_relevant_mask(sq);
            let rbits = r_mask[s].count_ones() as u8;
            let bbits = b_mask[s].count_ones() as u8;
            r_shift[s] = 64 - rbits;
            b_shift[s] = 64 - bbits;
        }

        let mut r_offset = [0usize; 64];
        let mut b_offset = [0usize; 64];
        let mut r_total = 0usize;
        let mut b_total = 0usize;

        for s in 0..64 {
            r_offset[s] = r_total;
            b_offset[s] = b_total;
            r_total += 1usize << (64 - r_shift[s]);
            b_total += 1usize << (64 - b_shift[s]);
        }

        let mut r_attacks = vec![0u64; r_total];
        let mut b_attacks = vec![0u64; b_total];

        for s in 0..64 {
            let sq = crate::game::defs::Square(s as u8);

            // Rook
            enumerate_subsets(r_mask[s], |subocc| {
                let idx = r_offset[s]
                    + ((subocc.wrapping_mul(ROOK_MAGIC[s])) >> r_shift[s]) as usize;
                let atk = rook_attacks_slow(sq, subocc);
                if r_attacks[idx] != 0 && r_attacks[idx] != atk {
                    panic!("rook magic collision at sq {s}, idx {idx}");
                }
                r_attacks[idx] = atk;
            });

            // Bishop
            enumerate_subsets(b_mask[s], |subocc| {
                let idx = b_offset[s]
                    + ((subocc.wrapping_mul(BISHOP_MAGIC[s])) >> b_shift[s]) as usize;
                let atk = bishop_attacks_slow(sq, subocc);
                if b_attacks[idx] != 0 && b_attacks[idx] != atk {
                    panic!("bishop magic collision at sq {s}, idx {idx}");
                }
                b_attacks[idx] = atk;
            });
        }

        Self {
            rook: MagicTable { magic: ROOK_MAGIC, mask: r_mask, shift: r_shift, offset: r_offset, attacks: r_attacks },
            bishop: MagicTable { magic: BISHOP_MAGIC, mask: b_mask, shift: b_shift, offset: b_offset, attacks: b_attacks },
        }
    }

    #[inline]
    pub fn rook_attacks(&self, sq: crate::game::defs::Square, occ: crate::game::board::BitBoard) -> crate::game::board::BitBoard {
        crate::game::board::BitBoard(self.rook.attacks(sq, occ.0))
    }
    #[inline]
    pub fn bishop_attacks(&self, sq: crate::game::defs::Square, occ: crate::game::board::BitBoard) -> crate::game::board::BitBoard {
        crate::game::board::BitBoard(self.bishop.attacks(sq, occ.0))
    }
    #[inline]
    pub fn queen_attacks(&self, sq: crate::game::defs::Square, occ: crate::game::board::BitBoard) -> crate::game::board::BitBoard {
        self.rook_attacks(sq, occ) | self.bishop_attacks(sq, occ)
    }
}

fn enumerate_subsets(mask: u64, mut f: impl FnMut(u64)) {
    let mut sub = 0u64;
    loop {
        f(sub);
        sub = sub.wrapping_sub(mask) & mask;
        if sub == 0 { break; }
    }
}


fn rook_relevant_mask(sq: crate::game::defs::Square) -> u64 {
    let f = (sq.0 % 8) as i32;
    let r = (sq.0 / 8) as i32;
    let mut m = 0u64;

    // north (exclude edge)
    for rr in r+1..7 { m |= 1u64 << (rr*8 + f); }
    // south
    for rr in (1..r).rev() { m |= 1u64 << (rr*8 + f); }
    // east
    for ff in f+1..7 { m |= 1u64 << (r*8 + ff); }
    // west
    for ff in (1..f).rev() { m |= 1u64 << (r*8 + ff); }
    m
}

fn bishop_relevant_mask(sq: crate::game::defs::Square) -> u64 {
    let f = (sq.0 % 8) as i32;
    let r = (sq.0 / 8) as i32;
    let mut m = 0u64;

    // NE
    { let mut rr = r+1; let mut ff = f+1; while rr <= 6 && ff <= 6 { m |= 1u64 << (rr*8 + ff); rr += 1; ff += 1; } }
    // NW
    { let mut rr = r+1; let mut ff = f-1; while rr <= 6 && ff >= 1 { m |= 1u64 << (rr*8 + ff); rr += 1; ff -= 1; } }
    // SE
    { let mut rr = r-1; let mut ff = f+1; while rr >= 1 && ff <= 6 { m |= 1u64 << (rr*8 + ff); rr -= 1; ff += 1; } }
    // SW
    { let mut rr = r-1; let mut ff = f-1; while rr >= 1 && ff >= 1 { m |= 1u64 << (rr*8 + ff); rr -= 1; ff -= 1; } }
    m
}

fn rook_attacks_slow(sq: crate::game::defs::Square, occ: u64) -> u64 {
    use crate::game::board::PieceMovement as PM;
    let bb = 1u64 << sq.0;
    let r = PM::hq_line(occ, bb, PM::rank_mask(sq));
    let f = PM::hq_line(occ, bb, PM::file_mask(sq));
    r | f
}
fn bishop_attacks_slow(sq: crate::game::defs::Square, occ: u64) -> u64 {
    use crate::game::board::PieceMovement as PM;
    let bb = 1u64 << sq.0;
    let d = PM::hq_line(occ, bb, PM::diag_mask(sq));
    let a = PM::hq_line(occ, bb, PM::anti_mask(sq));
    d | a
}


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

    // Must share a rank, file, or diagonal to have anything "between".
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

    // Only the two endpoints are considered "occupied" on that line.
    let occ = a_bit | b_bit;

    // Attacks from both ends, then intersect, then drop the endpoints.
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

    // For x-ray from our king, ignore our own pieces.
    let occ_wo_us = BitBoard(occ.0 & !our_occ.0);

    let enemy_rooklikes   = pos.pieces(them, Piece::Rook)   | pos.pieces(them, Piece::Queen);
    let enemy_bishoplikes = pos.pieces(them, Piece::Bishop) | pos.pieces(them, Piece::Queen);

    // Use magic tables here:
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

pub const MAX_MOVES: usize = 256;
#[derive(Clone)]
pub struct MoveList {
    pub len: usize,
    pub items: [Move; MAX_MOVES],
}

impl MoveList {
    pub const fn new() -> Self {
        Self { len: 0, items: [Move(0); MAX_MOVES] }
    }

    #[inline]
    pub fn push(&mut self, m: Move) {
        debug_assert!(self.len < MAX_MOVES);
        self.items[self.len] = m;
        self.len += 1;
    }

    #[inline]
    #[allow(dead_code)]
    pub fn delete(&mut self) {
        self.items[self.len] = Move(0);
        self.len -= 1;
    }

    pub fn iter(&self) -> impl Iterator<Item=&Move> {
        self.items[..self.len].iter()
    }
}

#[inline]
fn king_square_is_attacked_by_sliders_after_move(
    occ_after: crate::game::board::BitBoard,
    to: Square,
    enemy_rooks_or_queens: crate::game::board::BitBoard,
    enemy_bishops_or_queens: crate::game::board::BitBoard,
) -> bool {
    use crate::game::board::PieceMovement as PM;

    let bb = 1u64 << to.0;

    // Rook/Queen rays (rank + file)
    let r = PM::hq_line(occ_after.0, bb, PM::rank_mask(to));
    let f = PM::hq_line(occ_after.0, bb, PM::file_mask(to));
    if ((r | f) & enemy_rooks_or_queens.0) != 0 {
        return true;
    }

    // Bishop/Queen rays (diagonals)
    let d = PM::hq_line(occ_after.0, bb, PM::DIAG_MASK[to.0 as usize]);
    let a = PM::hq_line(occ_after.0, bb, PM::ANTI_MASK[to.0 as usize]);
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

    // Precompute enemy non-slider attacks once per node (independent of our king hop):
    let enemy_pawn_attacks = if them == Color::White {
        board::PieceMovement::white_pawn_attacks_from_bb(pos.pieces(them, Piece::Pawn))
    } else {
        board::PieceMovement::black_pawn_attacks_from_bb(pos.pieces(them, Piece::Pawn))
    };
    let enemy_knight_attacks = board::PieceMovement::knight_attacks_from_bb(pos.pieces(them, Piece::Knight));
    let enemy_king_attacks   = board::PieceMovement::king_attacks_from_bb(pos.pieces(them, Piece::King));

    // Enemy slider sets (positions don’t change within this node):
    let enemy_rooks_or_queens   = pos.pieces(them, Piece::Rook)   | pos.pieces(them, Piece::Queen);
    let enemy_bishops_or_queens = pos.pieces(them, Piece::Bishop) | pos.pieces(them, Piece::Queen);

    // King (cheap legality: pre-reject by non-sliders, targeted ray test for sliders)
    {
        let kmoves = KING_ATK[ksq.0 as usize];

        // Candidate king destinations (can’t move onto our own men, nor onto the enemy king square)
        for to in (kmoves & !our_occ & !opp_king).iter_squares() {
            let to_bb = board::BitBoard::from_square(to);

            // Quick reject: squares controlled by enemy non-sliders never become safe by moving our king.
            if ((enemy_pawn_attacks | enemy_knight_attacks | enemy_king_attacks) & to_bb) != board::BitBoard(0) {
                continue;
            }

            // Build occupancy after the king move (remove our king from 'from', remove victim if capture, place king on 'to').
            let mut occ_after = occ.0;
            occ_after &= !(1u64 << ksq.0); // king leaves origin
            let is_capture = (their_occ_no_king & to_bb) != board::BitBoard(0);
            if is_capture {
                occ_after &= !(1u64 << to.0); // captured piece removed
            }
            occ_after |= 1u64 << to.0;        // our king now occupies 'to'

            // Targeted slider test: do any enemy sliders have unobstructed line-of-sight to 'to' with occ_after?
            if king_square_is_attacked_by_sliders_after_move(
                board::BitBoard(occ_after),
                to,
                enemy_rooks_or_queens,
                enemy_bishops_or_queens,
            ) {
                continue; // square is still illegal
            }

            // Safe -> emit move
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

        // quick non-slider rejection
        if ((enemy_pawn_attacks | enemy_knight_attacks | enemy_king_attacks) & sq_bb) != BitBoard(0) {
            return true;
        }

        // targeted slider checks using magics: "if any enemy rooklike/bishoplike sits on a ray
        // returned by magics.attacks(sq, occ), then they attack sq"
        if (magics().rook_attacks(sq, occ) & enemy_rooks_or_queens) != BitBoard(0) {
            return true;
        }
        if (magics().bishop_attacks(sq, occ) & enemy_bishops_or_queens) != BitBoard(0) {
            return true;
        }

        false
    };

    // Castling logic
    if checkers == BitBoard(0) {
        const F1G1: BitBoard = BitBoard(0b01100000); // Squares f1 and g1
        const B1C1D1: BitBoard = BitBoard(0b00001110); // Squares b1, c1, d1
        const F8G8: BitBoard = BitBoard(F1G1.0 << 56);     // Squares f8 and g8
        const B8C8D8: BitBoard = BitBoard(B1C1D1.0 << 56); // Squares b8, c8, d8

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
        } else { // Black
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

    // Pawns
    {
        let pawns = pos.pieces(us, Piece::Pawn);
        let empty = !occ;

        let (one_step, two_step, _push_from_rank, _ep_rank) = if us == Color::White {
            (pawns.north() & empty,
             ((pawns & board::rankfile::RANK_2) .north().north()) & empty & (empty.north()),
             board::rankfile::RANK_2,
             board::rankfile::RANK_5)
        } else {
            (pawns.south() & empty,
             ((pawns & board::rankfile::RANK_7) .south().south()) & empty & (empty.south()),
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
            if to.get_rank() == if us == defs::Color::White {7} else {0} {
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

            // Two diagonal candidates depending on side, with file-edge guards.
            let candidates: [Option<Square>; 2] = if us == Color::White {
                let c1 = if from.get_file() > 0 { Some(Square(from.0 + 7)) } else { None }; // NW
                let c2 = if from.get_file() < 7 { Some(Square(from.0 + 9)) } else { None }; // NE
                [c1, c2]
            } else {
                let c1 = if from.get_file() < 7 { Some(Square(from.0 - 7)) } else { None }; // SE
                let c2 = if from.get_file() > 0 { Some(Square(from.0 - 9)) } else { None }; // SW
                [c1, c2]
            };

            for &opt_to in &candidates {
                let Some(to) = opt_to else { continue };

                // Must land on an enemy piece and satisfy the current check-evasion mask.
                if (their_occ_no_king & BitBoard::from_square(to)) == BitBoard(0) { continue; }
                if (check_mask & BitBoard::from_square(to)) == BitBoard(0) { continue; }

                // If the pawn is pinned, only moves along the pin ray are legal.
                if (pins.pinned & BitBoard::from_square(from)) != BitBoard(0) {
                    let ray = pins.ray_through_king[from.0 as usize];
                    if (ray & BitBoard::from_square(to)) == BitBoard(0) { continue; }
                }

                // Promotion vs. normal capture.
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
            // Pawns of ours that can capture onto ep_sq
            let ep_attackers = if us == Color::White {
                BLACK_PAWN_ATK[ep_sq.0 as usize]
            } else {
                WHITE_PAWN_ATK[ep_sq.0 as usize]
            } & pawns;

            let mut ep_froms = ep_attackers.0;
            while ep_froms != 0 {
                let from = Square(ep_froms.trailing_zeros() as u8);
                let to = ep_sq;

                // (Do NOT filter by check_mask here — EP may resolve a pawn check even though `to` != checker square.)

                // Optional: quick pin guard. Redundant but cheap; the final king-attack test is authoritative.
                if (pins.pinned & BitBoard::from_square(from)) != BitBoard(0) {
                    let ray = pins.ray_through_king[from.0 as usize];
                    if (ray & BitBoard::from_square(to)) == BitBoard(0) {
                        ep_froms &= ep_froms - 1;
                        continue;
                    }
                }

                // Square of the pawn that would be captured en passant
                let captured_sq = if us == Color::White { Square(to.0 - 8) } else { Square(to.0 + 8) };

                // Build hypothetical occupancy after EP: move our pawn, remove the captured pawn
                let mut occ_tmp = occ.0;
                occ_tmp &= !(1u64 << from.0);
                occ_tmp |=  1u64 << to.0;
                occ_tmp &= !(1u64 << captured_sq.0);

                // Recompute enemy attacks with that occupancy (and with the captured pawn removed from their pawn set)
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

                // EP is legal iff our king is not attacked in the resulting position.
                if (attacked_after & BitBoard::from_square(ksq)) == BitBoard(0) {
                    out.push(Move::pack(from, to, Flag::EP_CAPTURE));
                }

                ep_froms &= ep_froms - 1;
            }
        }


    }

    // Knight
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

    // Sliders
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