mod triples;
use triples::*;

mod brute_force;
use brute_force::*;


fn main() {
    let triples = pythagorean_triples(7825);
    let mut subs = all_subgraphs(&triples, 3)
        .iter()
        .map(|s| drop_weak_links(s, 3))
        .filter(|s| s.len() > 0)
        .collect::<Vec<_>>();

    subs.sort();
    subs.dedup();
    let mut subs = subs.iter()
        .map(|s| (s, links(s)))
        .collect::<Vec<_>>();
    subs.sort_unstable_by(
        |(_, a),(_, b)| a.len().cmp(&b.len())
    );
    println!("{:?}", subs.len());

    for i in 0..100 {
        println!(
            "{}: has {} triples, {} links and {} solutions",
            i,
            subs[i].0.len(),
            subs[i].1.len(),
            brute_force(&subs[i].0)
        );
    }
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
