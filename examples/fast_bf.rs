#![feature(portable_simd)]
use std::collections::BTreeSet;
use std::simd::{u64x4, Simd, cmp::SimdPartialEq};

pub type Edge = u16;
pub type Triple = [Edge; 3];

fn edges(triples: &[Triple]) -> BTreeSet<Edge> {
    triples.iter().flatten().cloned().collect()
}

pub fn slow_brute_force(base: &[Triple], exts: &[Triple]) -> Vec<u64> {
    // base edges in order as they occur in base triples
    let base_edges: Vec<Edge> = base
        .iter().flatten().cloned().collect();

    let edges: Vec<Edge> =
        base_edges
            .iter()
            .chain(edges(exts).difference(&edges(base)))
            .cloned()
            .collect();

    let mut masks = Vec::new();
    for t in base.into_iter().chain(exts) {
        let mut mask: u64 = 0;
        for l in t {
            let i = edges.iter().position(|e| e == l).unwrap();
            mask |= 1 << i;
        }
        masks.push(mask);
    }

    let n: u64 = 2_u64.pow(edges.len() as u32);
    let mut x = 0;
    let mut solutions = vec![];
    while x < n {
        if masks.iter().all(|m| x & m != *m && x & m != 0) {
            solutions.push(x);
        }
        x += 1;
    }

    solutions
}

pub fn fast_brute_force(base: &[Triple], exts: &[Triple]) -> usize {
    // base edges in order as they occur in base triples
    let base_edges: Vec<Edge> = base
        .iter().flatten().cloned().collect();

    let edges: Vec<Edge> =
        base_edges
            .iter()
            .chain(edges(exts).difference(&edges(base)))
            .cloned()
            .collect();

    let mut masks = Vec::new();
    for t in exts {
        let mut mask: u64 = 0;
        for l in t {
            let i = edges.iter().position(|e| e == l).unwrap();
            mask |= 1 << i;
        }
        masks.push(mask);
    }

    let n: u64 = 2_u64.pow(edges.len() as u32);
    let mut x = 0;

    for _ in base {
        x = (x << 3) | 0b001;
    }

    let mut solutions = 0;
    while x < n {
        for _ in 0..6 {
            if masks.iter().all(|m| x & m != *m && x & m != 0) {
                solutions += 1;
            }
            x += 1;
        }
        x += 2;
        let mut mask = 0b111_000;
        let mut incr = 0b010_000;
        while x & mask == mask {
            x += incr;
            mask = mask << 3;
            incr = incr << 3;
        }
    }

    solutions
}

pub fn simd_fast_brute_force(base: &[Triple], exts: &[Triple]) -> usize {
    // base edges in order as they occur in base triples
    let base_edges: Vec<Edge> = base
        .iter().flatten().cloned().collect();

    let edges: Vec<Edge> =
        base_edges
            .iter()
            .chain(edges(exts).difference(&edges(base)))
            .cloned()
            .collect();

    let masks: Vec<u64> = exts.into_iter()
        .map(|t| {
            let mut mask: u64 = 0;
            for l in t {
                let i = edges.iter().position(|e| e == l).unwrap();
                mask |= 1 << i;
            }
            mask
        })
        .collect();

    let vectorized_masks: Vec<u64x4> = masks
        .chunks(u64x4::LEN)
        .map(|chunk| Simd::load_or(chunk, u64x4::splat(chunk[0])))
        .collect();
    let zero_vec = u64x4::splat(0);
    let unit_vec = u64x4::splat(1);

    let n: u64 = 2_u64.pow(edges.len() as u32);
    let mut x = 0;

    // starting value is 0b..._001_001_001
    for _ in base {
        x = (x << 3) | 0b001;
    }

    let mut solutions = 0;
    while x < n {
        let mut x_vec = u64x4::splat(x);
        for _ in 0..6 {
            let is_solution = vectorized_masks
                .iter()
                .all(|m_vec| {
                    let mv_vec = m_vec & x_vec;
                    (mv_vec.simd_ne(*m_vec) & mv_vec.simd_ne(zero_vec)).all()
                });
            if is_solution {
                solutions += 1; //.push(x_vec[0]);
            }
            x_vec += unit_vec;
        }
        x += 8;
        let mut mask = 0b111_000;
        let mut incr = 0b010_000;
        while x & mask == mask {
            x += incr;
            mask = mask << 3;
            incr = incr << 3;
        }
    }

    solutions
}


pub fn main() {
    let clusters = vec![
        vec![[324,432,540],[324,2175,2199],[432,2900,2932],[540,1155,1275],[540,1296,1404],[540,1575,1665],[540,3625,3665],[880,924,1276],[880,2340,2500],[924,2457,2625],[999,1332,1665],[999,1932,2175],[1155,2772,3003],[1275,3060,3315],[1276,3393,3625],[1296,2772,3060],[1296,3780,3996],[1332,2576,2900],[1404,3003,3315],[1404,4095,4329],[1500,1575,2175],[1500,2000,2500],[1575,2100,2625],[1575,3780,4095],[1665,3220,3625],[1665,3996,4329],[1932,2576,3220],[2000,2100,2900],[2175,2900,3625],[2199,2932,3665],[2340,2457,3393],[2500,2625,3625]],
        vec![[645,860,1075],[645,1548,1677],[645,2736,2811],[860,2064,2236],[860,3648,3748],[903,1204,1505],[903,3096,3225],[1075,2580,2795],[1075,4560,4685],[1161,1548,1935],[1161,2652,2895],[1204,4128,4300],[1290,1720,2150],[1290,3096,3354],[1505,5160,5375],[1548,2064,2580],[1548,3536,3860],[1677,2236,2795],[1720,4128,4472],[1935,2580,3225],[1935,4420,4825],[2064,2752,3440],[2064,3870,4386],[2150,5160,5590],[2580,3440,4300],[2652,3536,4420],[2736,3648,4560],[2752,5160,5848],[2811,3748,4685],[2895,3860,4825],[3096,4128,5160],[3225,4300,5375],[3354,4472,5590],[3440,6450,7310],[3870,5160,6450],[4386,5848,7310]],
        vec![[174,232,290],[174,2520,2526],[232,3360,3368],[290,4200,4210],[462,616,770],[462,2520,2562],[616,3360,3416],[770,4200,4270],[792,945,1233],[792,3520,3608],[918,1224,1530],[918,2520,2682],[945,4200,4305],[1224,3360,3576],[1233,5480,5617],[1530,4200,4470],[2112,2520,3288],[2112,2816,3520],[2499,2520,3549],[2499,3332,4165],[2520,2646,3654],[2526,3368,4210],[2562,3416,4270],[2646,3528,4410],[2682,3576,4470],[2816,3360,4384],[3288,4384,5480],[3332,3360,4732],[3360,3528,4872],[3520,4200,5480],[3549,4732,5915],[3608,4305,5617],[3654,4872,6090],[4165,4200,5915],[4200,4410,6090]],
        vec![[24,32,40],[24,45,51],[32,60,68],[36,48,60],[36,105,111],[40,75,85],[45,60,75],[45,108,117],[45,336,339],[48,140,148],[51,68,85],[60,80,100],[60,144,156],[60,175,185],[60,297,303],[60,448,452],[75,180,195],[75,560,565],[80,192,208],[80,396,404],[100,240,260],[100,495,505],[105,140,175],[108,144,180],[111,148,185],[117,156,195],[144,192,240],[144,420,444],[156,208,260],[156,455,481],[175,420,455],[185,444,481],[297,396,495],[303,404,505],[336,448,560],[339,452,565]],
    ];

    for c in &clusters {
        run(c);
    }
}

fn run(cluster: &[Triple]) {
    let mut base = vec![];
    let mut exts = vec![];
    let mut used_edges = BTreeSet::new();
    for t in cluster {
        if t.iter().any(|e| used_edges.contains(e)) {
            exts.push(*t);
        } else {
            for e in t {
                used_edges.insert(*e);
            }
            base.push(*t);
        }
    }

    println!(
        "triples: {}, base: {}, edges: {} ({} covered by base)",
        cluster.len(),
        base.len(),
        edges(cluster).len(),
        base.len() * 3,
    );

    let now = std::time::Instant::now();
    let res1 = simd_fast_brute_force(&base, &exts);
    println!(
        "simd: solutions: {}, time: {:?}",
        res1, // .len(),
        now.elapsed());

    let now = std::time::Instant::now();
    let res2 = fast_brute_force(&base, &exts);
    println!(
        "fast: solutions: {}, time: {:?}",
        res2, //.len(),
        now.elapsed());

    //for i in 0..res1.len() {
    //    if res1[i] != res2[i] {
    //        println!("{i}");
    //        println!("{:036b} {:036b}", res1[i-1], res1[i]);
    //        println!("{:036b} {:036b}", res2[i-1], res2[i]);
    //        break;
    //    }
    //}
}
