use crate::hgraph::*;
use std::collections::BTreeSet;

fn edges(triples: &[Triple]) -> BTreeSet<Edge> {
    triples.iter().flatten().cloned().collect()
}

pub fn brute_force(triples: &[Triple], internal_edges: &BTreeSet<Edge>) -> usize {
    let mut edges: Vec<Edge> = edges(triples).into_iter().collect();
    // Internal edges first (at the lower-end of a machine word).
    edges.sort_by_key(|e| (!internal_edges.contains(e), *e));

    let mut masks = Vec::new();
    for t in triples {
        let mut mask: u64 = 0;
        for l in t {
            let i = edges.iter().position(|e| e == l).unwrap();
            mask |= 1 << i;
        }
        masks.push(mask);
    }

    let next_outer = 2_u64.pow(internal_edges.len() as u32);
    let outer_mask = !(next_outer - 1);

    let n: u64 = 2_u64.pow(edges.len() as u32 - 1);
    let mut solutions = 0;
    let mut x = 0;
    while x < n {
        if masks.iter().all(|m| x & m != *m && x & m != 0) {
            solutions += 1;
            x = (x & outer_mask) + next_outer;
        } else {
            x += 1;
        }
    }

    solutions
}
