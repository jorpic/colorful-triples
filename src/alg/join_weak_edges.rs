use crate::types::*;
use super::edge_index::*;
use std::collections::BTreeSet;

pub struct MiniCluster {
    pub triples: Vec<Triple>,
    pub out_edges: Vec<Edge>,
}

impl MiniCluster {
    pub fn singleton(t: &Triple) -> Self {
        MiniCluster {
            triples: vec![t.clone()],
            out_edges: Vec::from(t),
        }
    }

    pub fn with_captive(ts: &[Triple], captive: Edge) -> Self {
        MiniCluster {
            triples: Vec::from(ts),
            // NB: assume no other common edges
            out_edges: ts.iter().flatten().cloned().filter(|e| *e != captive).collect(),
        }
    }
}

pub fn join_weak_edges(
    all_triples: &[Triple],
    free_triples: &[Triple]
) -> Vec<MiniCluster> {
    let all_triples_ix = mk_edge_index(all_triples);
    let free_triples_set: BTreeSet<Triple> = free_triples.iter().cloned().collect();

    let mut res = vec![];
    let mut used_triples_set: BTreeSet<Triple> = BTreeSet::new();
    for weight in [3,2] {
        for (e, ts) in &all_triples_ix {
            if ts.len() == weight {
                let is_free_unused = |t|
                        !used_triples_set.contains(t)
                            && free_triples_set.contains(t);
                if ts.iter().all(is_free_unused) {
                    res.push(MiniCluster::with_captive(ts, *e));
                    ts.iter().for_each(|t| { used_triples_set.insert(*t); });
                }
            }
        }
    }

    for t in free_triples_set.difference(&used_triples_set) {
        res.push(MiniCluster::singleton(t));
    }

    res
}
