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
        vec![[176,330,374],[176,960,976],[330,1800,1830],[351,468,585],[351,720,801],[374,2040,2074],[468,960,1068],[585,1200,1335],[714,720,1014],[714,952,1190],[720,1254,1446],[801,1068,1335],[952,960,1352],[960,1456,1744],[960,1672,1928],[960,3536,3664],[960,3780,3900],[976,1830,2074],[1014,1352,1690],[1190,1200,1690],[1200,2090,2410],[1254,1672,2090],[1446,1928,2410],[1456,2730,3094],[1456,5733,5915],[1672,3135,3553],[1744,3270,3706],[1744,6867,7085],[1800,2730,3270],[1800,3135,3615],[1800,6630,6870],[1928,3615,4097],[2040,3094,3706],[2040,3553,4097],[2040,7514,7786],[3536,6630,7514],[3664,6870,7786],[3780,5733,6867],[3900,5915,7085]],
        vec![[168,224,280],[168,315,357],[168,576,600],[195,260,325],[195,468,507],[195,1260,1275],[224,420,476],[224,768,800],[260,624,676],[260,1680,1700],[280,525,595],[280,960,1000],[315,420,525],[315,1080,1125],[325,780,845],[325,2100,2125],[357,476,595],[357,1224,1275],[420,1440,1500],[468,624,780],[468,3024,3060],[476,1632,1700],[507,676,845],[507,3276,3315],[525,1800,1875],[576,768,960],[576,1080,1224],[595,600,845],[595,2040,2125],[600,800,1000],[600,1125,1275],[624,4032,4080],[676,4368,4420],[768,1440,1632],[780,5040,5100],[800,1500,1700],[845,5460,5525],[960,1800,2040],[1000,1875,2125],[1080,1440,1800],[1125,1500,1875],[1224,1632,2040],[1260,1680,2100],[1260,3024,3276],[1275,1700,2125],[1275,3060,3315],[1680,4032,4368],[1700,4080,4420],[2100,5040,5460],[2125,5100,5525],[3024,4032,5040],[3060,4080,5100],[3276,4368,5460],[3315,4420,5525]],
        vec![[324,432,540],[324,2175,2199],[432,2900,2932],[540,1155,1275],[540,1296,1404],[540,1575,1665],[540,3625,3665],[880,924,1276],[880,2340,2500],[924,2457,2625],[999,1332,1665],[999,1932,2175],[1155,2772,3003],[1275,3060,3315],[1276,3393,3625],[1296,2772,3060],[1296,3780,3996],[1332,2576,2900],[1404,3003,3315],[1404,4095,4329],[1500,1575,2175],[1500,2000,2500],[1575,2100,2625],[1575,3780,4095],[1665,3220,3625],[1665,3996,4329],[1932,2576,3220],[2000,2100,2900],[2175,2900,3625],[2199,2932,3665],[2340,2457,3393],[2500,2625,3625]],
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
