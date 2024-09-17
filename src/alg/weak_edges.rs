use super::edge_index::mk_edge_index;
use crate::types::Constraint;
use std::collections::BTreeSet;

// Weak edge is an edge that connects few nodes.
pub fn join_weak_edges(cs: &[Constraint]) -> Vec<Constraint> {
    let all_cs_ix = mk_edge_index(cs);

    let mut res = vec![];
    let mut used_cs_set: BTreeSet<Constraint> = BTreeSet::new();
    for weight in [3, 2] {
        for (e, ts) in &all_cs_ix {
            if ts.len() == weight {
                if ts.iter().all(|t| t.triples.len() == 1 && !used_cs_set.contains(t)) {
                    let triples = ts.iter().flat_map(|t| t.triples.iter()).cloned().collect::<Vec<_>>();
                    res.push(Constraint::with_captive(&triples, *e));
                    ts.iter().for_each(|t| {
                        used_cs_set.insert(t.clone());
                    });
                }
            }
        }
    }

    for c in cs {
        if !used_cs_set.contains(c) {
            res.push(c.clone());
        }
    }

    res
}
