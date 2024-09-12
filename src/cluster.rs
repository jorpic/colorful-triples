use serde::Serialize;
use std::collections::{btree_map, btree_set};
use std::collections::{BTreeMap, BTreeSet};

use crate::alg::exact_cover::*;
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

#[derive(
    Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize,
)]
pub struct Cluster {
    pub nodes: BTreeSet<Triple>,
    pub cover: BTreeSet<Triple>,
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

impl Cluster {
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

    pub fn from_cover(
        cover: BTreeSet<Triple>,
        nodes: BTreeSet<Triple>,
    ) -> Self {
        let mut c = Cluster::default();
        for triple in &nodes {
            for edge in triple.edges() {
                c.edge_weights
                    .entry(edge)
                    .and_modify(|weight| *weight += 1)
                    .or_insert(1);
            }
        }
        c.nodes = nodes;
        c.cover = cover;
        c
    }

    fn add_triples<'a, T>(&mut self, triples: T)
    where
        T: IntoIterator<Item = &'a Triple>,
    {
        let mut triples_clone = vec![];
        for triple in triples {
            for edge in triple.edges() {
                self.edge_weights
                    .entry(edge)
                    .and_modify(|weight| *weight += 1)
                    .or_insert(1);
            }
            self.nodes.insert(*triple);
            triples_clone.push(*triple);
        }

        self.cover = exact_cover(13, &triples_clone).into_iter().collect();
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
