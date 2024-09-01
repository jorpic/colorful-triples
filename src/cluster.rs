use serde::Serialize;
use std::collections::{btree_map, btree_set};
use std::collections::{BTreeMap, BTreeSet};

use crate::types::*;

impl Node for Triple {
    type IterEdges<'a> = Triple;
    type IterTriples<'a> = [Triple; 1];

    fn edges(&self) -> Self::IterEdges<'_> {
        *self
    }

    fn triples(&self) -> Self::IterTriples<'_> {
        [*self; 1]
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Cluster {
    pub nodes: BTreeSet<Triple>,
    pub base: BTreeSet<Triple>,
    pub edge_weights: BTreeMap<Edge, usize>,
}

impl Node for Cluster {
    type IterEdges<'a> = std::iter::Cloned<btree_map::Keys<'a, Edge, usize>>;
    type IterTriples<'a> = std::iter::Cloned<btree_set::Iter<'a, Triple>>;

    fn edges(&self) -> Self::IterEdges<'_> {
        self.edge_weights.keys().cloned()
    }

    fn triples(&self) -> Self::IterTriples<'_> {
        self.nodes.iter().cloned()
    }
}

pub type ClusterId = Triple;

impl Cluster {
    pub fn id(&self) -> ClusterId {
        *self.nodes.first().unwrap()
    }

    pub fn singleton(t: &Triple) -> Self {
        Cluster::from_triples([t])
    }

    pub fn from_triples<'a, T>(triples: T) -> Self
    where
        T: IntoIterator<Item = &'a Triple>,
    {
        let mut res = Cluster::default();
        res.add_triples(triples);
        res
    }

    fn add_triples<'a, T>(&mut self, triples: T)
    where
        T: IntoIterator<Item = &'a Triple>,
    {
        for triple in triples {
            for edge in triple.edges() {
                self.edge_weights
                    .entry(edge)
                    .and_modify(|weight| *weight += 1)
                    .or_insert(1);
            }
            self.nodes.insert(*triple);
        }

        self.base = Cluster::get_base(&self.nodes);
    }

    fn get_base<'a, T>(triples: T) -> BTreeSet<Triple>
        where
            T: IntoIterator<Item = &'a Triple>,
    {
        let mut base = BTreeSet::new();
        let mut used_edges = BTreeSet::new();
        for t in triples {
            if !t.iter().any(|e| used_edges.contains(e)) {
                for e in t {
                    used_edges.insert(*e);
                }
                base.insert(*t);
            }
        }
        base
    }


    pub fn merge_with(&mut self, b: &Cluster) {
        self.add_triples(&b.nodes)
    }

    pub fn inner_edges(&self, global_weights: &BTreeMap<Edge, usize>) -> BTreeSet<Edge> {
        self.edge_weights
            .iter()
            .filter(|(e, w)| global_weights.get(e).unwrap() <= *w)
            .map(|x| *x.0)
            .collect()
    }
}

// tests
//  - unfold merged clusters into Triples
//  - clusters are disjoint
//  - no triples added or lost
//  - sum of link weights in clusters should match global link weights

#[cfg(test)]
mod tests {
    use super::*;
    use crate::triples::pythagorean_triples;

    #[test]
    fn test_add_triples() {
        let triples = pythagorean_triples(7825);
        let mut all = Cluster::default();
        all.add_triples(&triples);
        assert_eq!(9472, all.nodes.len());
    }
}
