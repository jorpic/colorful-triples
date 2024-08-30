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
    /* 32 triples, 39 edges
    // solutions: 108243216, time: 57.536871301s
    let cluster = [
        [324,432,540],    [324,2175,2199],  [432,2900,2932],  [540,1155,1275],
        [540,1296,1404],  [540,1575,1665],  [540,3625,3665],  [880,924,1276],
        [880,2340,2500],  [924,2457,2625],  [999,1332,1665],  [999,1932,2175],
        [1155,2772,3003], [1275,3060,3315], [1276,3393,3625], [1296,2772,3060],
        [1296,3780,3996], [1332,2576,2900], [1404,3003,3315], [1404,4095,4329],
        [1500,1575,2175], [1500,2000,2500], [1575,2100,2625], [1575,3780,4095],
        [1665,3220,3625], [1665,3996,4329], [1932,2576,3220], [2000,2100,2900],
        [2175,2900,3625], [2199,2932,3665], [2340,2457,3393], [2500,2625,3625],
    ];
    / **/

    /* 53 triples, 45 edges
    // solutions: 1172705850, time: 213.93466287s
    let cluster = [
        [116,837,845],[837,1116,1395],[837,1320,1563],[837,3720,3813],[837,4284,4365],[1080,3813,3963],[1116,1760,2084],[1340,3216,3484],[1395,6200,6355],[1440,5084,5284],[1563,2084,2605],[1800,6355,6605],[1925,6600,6875],[2420,6435,6875],[2604,5797,6355],[2613,3484,4355],[2728,5115,5797],[3484,3813,5165],[3696,5797,6875],[3813,5084,6355],[3963,5284,6605],[4125,5500,6875]
    ];
    / **/

    /* 35 triples, 45 edges */
    let cluster = [
        [264,1927,1945],[423,564,705],[423,1064,1145],[423,1880,1927],[423,3300,3327],[564,4400,4436],[567,1020,1167],[705,5500,5545],[756,1360,1556],[793,1776,1945],[945,1700,1945],[1167,1556,1945],[1701,3060,3501],[1945,4668,5057],[2268,4080,4668],[2736,4823,5545],[3024,5440,6224],[3327,4436,5545],[3501,4668,5835],[3864,3977,5545],[4668,6224,7780]
    ];
    /**/


    let mut base = vec![];
    let mut exts = vec![];
    let mut used_edges = BTreeSet::new();
    for t in &cluster {
        if t.iter().any(|e| used_edges.contains(e)) {
            exts.push(*t);
        } else {
            for e in t {
                used_edges.insert(*e);
            }
            base.push(*t);
        }
    }

    let now = std::time::Instant::now();
    let res1 = simd_fast_brute_force(&base, &exts);
    println!(
        "solutions: {}, time: {:?}",
        res1, // .len(),
        now.elapsed());

    let now = std::time::Instant::now();
    let res2 = fast_brute_force(&base, &exts);
    println!(
        "solutions: {}, time: {:?}",
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
