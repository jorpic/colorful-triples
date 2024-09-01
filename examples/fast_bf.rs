use std::collections::BTreeSet;
use colorful_triples::brute_force::fast_brute_force;

pub type Edge = u16;
pub type Triple = [Edge; 3];

fn edges(triples: &[Triple]) -> BTreeSet<Edge> {
    triples.iter().flatten().cloned().collect()
}

pub fn slow_brute_force(base: &[Triple], exts: &[Triple]) -> usize {
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
    let mut solutions = 0;
    while x < n {
        if masks.iter().all(|m| x & m != *m && x & m != 0) {
            solutions += 1;
        }
        x += 1;
    }

    solutions
}


pub fn main() {
    let clusters = vec![
        vec![[324,432,540],[324,2175,2199],[432,2900,2932],[540,1155,1275],[540,1296,1404],[540,1575,1665],[540,3625,3665],[880,924,1276],[880,2340,2500],[924,2457,2625],[999,1332,1665],[999,1932,2175],[1155,2772,3003],[1275,3060,3315],[1276,3393,3625],[1296,2772,3060],[1296,3780,3996],[1332,2576,2900],[1404,3003,3315],[1404,4095,4329],[1500,1575,2175],[1500,2000,2500],[1575,2100,2625],[1575,3780,4095],[1665,3220,3625],[1665,3996,4329],[1932,2576,3220],[2000,2100,2900],[2175,2900,3625],[2199,2932,3665],[2340,2457,3393],[2500,2625,3625]],
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
    let res1 = fast_brute_force(&base, &exts);
    println!(
        "simd: solutions: {}, time: {:?}",
        res1, // .len(),
        now.elapsed());

    let now = std::time::Instant::now();
    let res2 = slow_brute_force(&base, &exts);
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
