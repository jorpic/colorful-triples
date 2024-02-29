use std::collections::BTreeSet;

mod triples;
use triples::*;

mod brute_force;
use brute_force::*;

fn main() {
    let triples = pythagorean_triples(7825);
    let triples = drop_weak_links(&triples, 2); // drop pendants

    let mut subs = all_subgraphs(&triples, 3)
        .iter()
        .map(|s| drop_weak_links(s, 3))
        .filter(|s| s.len() > 0)
        .map(|s| (links(&s).len(), s.len(), s))
        .collect::<Vec<_>>();
    subs.sort();
    subs.dedup();

    println!("3,3 tight subgraphs: {}", subs.len());
    let s27 = subs.into_iter().filter(|s| 20 < s.0 && s.0 <= 32).rev();

    let subs = all_subgraphs(&triples, 3)
        .into_iter()
        .map(|s| drop_weak_links(&s, 2))
        .filter(|s| s.len() > 0)
        .map(|s| (links(&s).len(), s.len(), s));

    let subs = s27.chain(subs).collect::<Vec<_>>();
    let mut used_triples: BTreeSet<Triple> = BTreeSet::new();
    let mut res = Vec::new();

    for (_lns, _tps, sub) in subs {
        let new_sub = sub
            .iter()
            .filter(|t| !used_triples.contains(*t))
            .cloned()
            .collect::<Vec<Triple>>();

        let new_lns = links(&new_sub).len();
        let new_tps = new_sub.len();

        if new_tps < 6 || new_lns < 20 || new_lns > 32 {
            continue;
        }

        if new_lns - new_tps <= new_tps {
            for t in &new_sub {
                used_triples.insert(*t);
            }
            let drop1 = brute_force_selected(&new_sub, 0b0100_0001_0010_0100_0101_0010u64);
            let drop2 = brute_force_selected(&new_sub, 0b0001_0010_1100_1000_0010_0100u64);
            let drop3 = brute_force_selected(&new_sub, 0b1010_0100_0001_0010_1000_1000u64);
            println!("{new_lns} {new_tps} {drop1} {drop2} {drop3}");
            res.push((new_lns, new_tps, new_sub));
        }
    }

    // FIXME: extend small subs with unused triples

    println!(
        "with {} subgraphs we cover {} unique triples",
        res.len(),
        used_triples.len()
    );

    // res.sort();

    // for a in &res {
    //     let a_lns = links(&a.2);
    //     print!("\n{:?}", (a.0, a.1));
    //     for b in &res {
    //         let b_lns = links(&b.2);
    //         let xx = b_lns.intersection(&a_lns).count();
    //         if xx > 5 {
    //             // TODO: calculate joined table size
    //             print!("{:>2}", xx);
    //         } else {
    //             print!("  ");
    //         }
    //     }
    //     // print!("lns={}, tps={}", s.0, s.1);
    //     // println!(", sol={:>10}", brute_force(&s.2));
    // }
}

fn to_string(triples: &[Triple]) -> String {
    let mut res = String::new();
    for t in triples {
        res = res + &format!("{},{},{};", t[0], t[1], t[2]);
    }
    res
}

fn from_string(s: &str) -> Vec<Triple> {
    let mut res = Vec::new();
    for ts in s.split(";") {
        if ts.len() > 0 {
            let mut ls = ts.split(",");
            res.push([
                ls.next().unwrap().parse::<u16>().unwrap(),
                ls.next().unwrap().parse::<u16>().unwrap(),
                ls.next().unwrap().parse::<u16>().unwrap(),
            ]);
        }
    }
    res
}
