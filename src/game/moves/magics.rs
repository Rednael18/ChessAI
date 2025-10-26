use std::sync::OnceLock;

use crate::game::board::{self, PieceMovement};
use crate::game::defs::{self, Square};

// near your OnceLock
pub static MAGICS: OnceLock<Magics> = OnceLock::new();

#[inline]
pub fn magics() -> &'static Magics {
    MAGICS.get_or_init(|| Magics::new())
}

#[allow(non_snake_case)]
pub struct MagicTable {
    pub magic:   [u64; 64],
    pub mask:    [u64; 64],
    pub shift:   [u8; 64],
    pub offset:  [usize; 64],
    pub attacks: Vec<u64>,
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
            9835879179034305920u64,
            2341893796599431808u64,
            72058693650744320u64,
            72138957906837508u64,
            36031718687244416u64,
            9429417088524417u64,
            3448137205161984u64,
            599541975809556992u64,
            24206986510467089u64,
            9225764591361853442u64,
            1299570010489946152u64,
            2324701927143178244u64,
            289497015695460608u64,
            1188950576547758144u64,
            22518273019486208u64,
            150083874115584u64,
            72480356527245318u64,
            622061898095134864u64,
            282574522155016u64,
            1513354610365432208u64,
            4505798717767745u64,
            36033475312148480u64,
            9042385782718464u64,
            9042385782706176u64,
            146375786131095680u64,
            9241461204310820864u64,
            11263399264125056u64,
            4509183089578022u64,
            1549379062991045888u64,
            36029072077234177u64,
            282033356030564u64,
            648537875641540608u64,
            149535804362752u64,
            144119588278257664u64,
            563023001422856u64,
            867242304870547969u64,
            140772930224896u64,
            36068964627415040u64,
            2312633594369556480u64,
            2882374688608223264u64,
            4611704443839315978u64,
            743376530318622736u64,
            288793394971541572u64,
            576746633916645632u64,
            288232035151118337u64,
            9337371051694237952u64,
            2900459172409647360u64,
            140806209929344u64,
            1153255760570908800u64,
            9233826195559678080u64,
            146648471594860800u64,
            9288831031312640u64,
            117375068610339072u64,
            88510713348353u64,
            4621348666215514117u64,
            145276555823317074u64,
            4611862391259680769u64,
            72620612914251778u64,
            844459325784069u64,
            36031064895947012u64,
            714962008014946u64,
        ];

        pub const BISHOP_MAGIC: [u64; 64] = [
            580610875736196u64,
            23564740739597186u64,
            326529222334482440u64,
            77691588255417536u64,
            1130985450111360u64,
            4611972303834579969u64,
            1130315334811664u64,
            2315207556594663936u64,
            9223970858442211456u64,
            18031995028275712u64,
            576469583309971472u64,
            4450265595906u64,
            2305865019312570880u64,
            217523137082843666u64,
            4045642047909150722u64,
            4710837787133356048u64,
            434880007787511936u64,
            9009433585680512u64,
            436849172780421152u64,
            108649358224916772u64,
            438540779691705378u64,
            35192970682377u64,
            649081313491486720u64,
            576847870658939912u64,
            27035144259896384u64,
            1319572837126377617u64,
            1155175503460828416u64,
            9804340787373408264u64,
            1153488854787851781u64,
            36100815047401473u64,
            9232521077431212034u64,
            2594372731718076560u64,
            1130573932306952u64,
            576614890645490304u64,
            2595201520813278208u64,
            289359031280599552u64,
            1154329996182815008u64,
            148621056000790529u64,
            289958812725561412u64,
            54330187963237376u64,
            144260529904435472u64,
            2306040990160650752u64,
            35326911842304u64,
            5931291424623436032u64,
            74036998572082176u64,
            9042389024940161u64,
            6944551733516304896u64,
            9800118666640099842u64,
            9235778941229858816u64,
            900755763354943488u64,
            2305845209479021065u64,
            578862094850990096u64,
            2379167309467090945u64,
            576496074418061450u64,
            2938952834131167360u64,
            1161933104089677892u64,
            18298381948686900u64,
            18312403876054272u64,
            11529496522221159426u64,
            4611690528285924352u64,
            1170936316574368768u64,
            8814904738050u64,
            7084338273224524178u64,
            93458500239262209u64,
        ];

        let mut r_mask = [0u64; 64];
        let mut b_mask = [0u64; 64];
        let mut r_shift = [0u8; 64];
        let mut b_shift = [0u8; 64];

        for s in 0..64 {
            let sq = defs::Square(s as u8);
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
            let sq = defs::Square(s as u8);

            enumerate_subsets(r_mask[s], |subocc| {
                let idx = r_offset[s]
                    + ((subocc.wrapping_mul(ROOK_MAGIC[s])) >> r_shift[s]) as usize;
                let atk = rook_attacks_slow(sq, subocc);
                if r_attacks[idx] != 0 && r_attacks[idx] != atk {
                    panic!("rook magic collision at sq {s}, idx {idx}");
                }
                r_attacks[idx] = atk;
            });

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
    pub fn rook_attacks(&self, sq: defs::Square, occ: board::BitBoard) -> board::BitBoard {
        board::BitBoard(self.rook.attacks(sq, occ.0))
    }
    #[inline]
    pub fn bishop_attacks(&self, sq: defs::Square, occ: board::BitBoard) -> board::BitBoard {
        board::BitBoard(self.bishop.attacks(sq, occ.0))
    }
    #[inline]
    pub fn queen_attacks(&self, sq: defs::Square, occ: board::BitBoard) -> board::BitBoard {
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

fn rook_relevant_mask(sq: defs::Square) -> u64 {
    let f = (sq.0 % 8) as i32;
    let r = (sq.0 / 8) as i32;
    let mut m = 0u64;

    for rr in r+1..7 { m |= 1u64 << (rr*8 + f); }
    for rr in (1..r).rev() { m |= 1u64 << (rr*8 + f); }
    for ff in f+1..7 { m |= 1u64 << (r*8 + ff); }
    for ff in (1..f).rev() { m |= 1u64 << (r*8 + ff); }
    m
}

fn bishop_relevant_mask(sq: defs::Square) -> u64 {
    let f = (sq.0 % 8) as i32;
    let r = (sq.0 / 8) as i32;
    let mut m = 0u64;

    { let mut rr = r+1; let mut ff = f+1; while rr <= 6 && ff <= 6 { m |= 1u64 << (rr*8 + ff); rr += 1; ff += 1; } }
    { let mut rr = r+1; let mut ff = f-1; while rr <= 6 && ff >= 1 { m |= 1u64 << (rr*8 + ff); rr += 1; ff -= 1; } }
    { let mut rr = r-1; let mut ff = f+1; while rr >= 1 && ff <= 6 { m |= 1u64 << (rr*8 + ff); rr -= 1; ff += 1; } }
    { let mut rr = r-1; let mut ff = f-1; while rr >= 1 && ff >= 1 { m |= 1u64 << (rr*8 + ff); rr -= 1; ff -= 1; } }
    m
}

fn rook_attacks_slow(sq: defs::Square, occ: u64) -> u64 {
    let bb = 1u64 << sq.0;
    let r = PieceMovement::hq_line(occ, bb, PieceMovement::rank_mask(sq));
    let f = PieceMovement::hq_line(occ, bb, PieceMovement::file_mask(sq));
    r | f
}

fn bishop_attacks_slow(sq: defs::Square, occ: u64) -> u64 {
    let bb = 1u64 << sq.0;
    let d = PieceMovement::hq_line(occ, bb, PieceMovement::diag_mask(sq));
    let a = PieceMovement::hq_line(occ, bb, PieceMovement::anti_mask(sq));
    d | a
}
