use super::edge_index::mk_edge_index;
use crate::types::Constraint;
use std::collections::BTreeSet;

// Weak edge is an edge that connects too few nodes.
pub fn join_weak_edges(cs: &[Constraint], weight: usize) -> Vec<Constraint> {
    let mut res = vec![];
    let mut used_cs_set: BTreeSet<Constraint> = BTreeSet::new();
    for (e, ts) in mk_edge_index(cs) {
        if ts.len() == weight {
            if ts.iter().all(|t| t.triples.len() == 1 && !used_cs_set.contains(t)) {
                let triples = ts.iter().flat_map(|t| t.triples.iter()).cloned().collect::<Vec<_>>();
                res.push(Constraint::with_captives(&triples, [e].into()));
                ts.into_iter().for_each(|t| {
                    used_cs_set.insert(t);
                });
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

pub fn join_chains(cs: &[Constraint]) -> Vec<Constraint> {
    let mut res = vec![];
    let mut used_cs_set: BTreeSet<Constraint> = BTreeSet::new();
    for (e, ts) in mk_edge_index(cs) {
        if ts.len() == 2 {
            let a = &ts[0];
            let b = &ts[1];
            let can_join = a.triples.len() + b.triples.len() <= 3
                && !used_cs_set.contains(a)
                && !used_cs_set.contains(b);
            if can_join {
                let triples = ts.iter().flat_map(|t| t.triples.iter()).cloned().collect::<Vec<_>>();
                let mut captives = ts.iter().flat_map(|c| c.captives.iter()).cloned().collect::<BTreeSet<_>>();
                captives.insert(e);

                res.push(Constraint::with_captives(&triples, captives));

                ts.into_iter().for_each(|t| {
                    used_cs_set.insert(t);
                });
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
