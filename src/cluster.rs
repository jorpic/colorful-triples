use serde::Serialize;
use std::collections::btree_set;
use std::collections::BTreeSet;

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
    Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize,
)]
pub struct Cluster {
    pub nodes: BTreeSet<Triple>,
    pub cover: BTreeSet<Triple>,
    pub edges: BTreeSet<Edge>,
    pub captured_edges: BTreeSet<Edge>,
}

impl Node for Cluster {
    type IterEdges<'a> = std::iter::Cloned<btree_set::Iter<'a, Edge>>;
    type IterTriples<'a> = std::iter::Cloned<btree_set::Iter<'a, Triple>>;

    fn edges(&self) -> Self::IterEdges<'_> {
        self.edges.iter().cloned()
    }

    fn triples(&self) -> Self::IterTriples<'_> {
        self.nodes.iter().cloned()
    }
}

impl Cluster {
    pub fn singleton(t: &Triple) -> Self {
        Cluster::from_triples(&[*t])
    }

    pub fn from_triples<T>(triples: &T) -> Self
    where
        T: IntoIterator<Item = Triple> + Clone,
    {
        let triples_vec: Vec<Triple> = triples.clone().into_iter().collect();
        Cluster {
            nodes: triples.clone().into_iter().collect(),
            cover: exact_cover(13, &triples_vec).into_iter().collect(),
            edges: triples.clone().into_iter().flatten().collect(),
            captured_edges: BTreeSet::new(),
        }
    }

    pub fn from_cover(
        cover: BTreeSet<Triple>,
        nodes: BTreeSet<Triple>,
    ) -> Self {
        Cluster {
            nodes: nodes.clone(),
            cover: cover,
            edges: nodes.into_iter().flatten().collect(),
            captured_edges: BTreeSet::new(),
        }
    }
}
