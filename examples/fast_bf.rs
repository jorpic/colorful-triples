use std::collections::BTreeSet;

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

pub fn fast_brute_force(base: &[Triple], exts: &[Triple]) -> Vec<u64> {
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

    let mut solutions = vec![];
    while x < n {
        for _ in 0..6 {
            if masks.iter().all(|m| x & m != *m && x & m != 0) {
                solutions.push(x);
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

pub fn main() {
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
    let res1 = fast_brute_force(&base, &exts);
    println!(
        "solutions: {}, time: {:?}",
        res1.len(),
        now.elapsed());

    let now = std::time::Instant::now();
    let res2 = slow_brute_force(&base, &exts);
    println!(
        "solutions: {}, time: {:?}",
        res2.len(),
        now.elapsed());

    for i in 0..res1.len() {
        if res1[i] != res2[i] {
            println!("{i}");
            println!("{:036b} {:036b}", res1[i-1], res1[i]);
            println!("{:036b} {:036b}", res2[i-1], res2[i]);
            break;
        }
    }
}

