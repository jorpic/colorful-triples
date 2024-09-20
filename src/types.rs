use std::collections::BTreeSet;

pub type Edge = u16;
pub type Triple = [Edge; 3];

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Constraint {
    pub triples: BTreeSet<Triple>,
    pub edges: Vec<Edge>,
    pub captives: BTreeSet<Edge>,
}

impl Constraint {
    pub fn one(t: Triple) -> Self {
        Constraint {
            triples: BTreeSet::from([t]),
            edges: t.into(),
            captives: BTreeSet::new(),
        }
    }

    pub fn with_captives(ts: &[Triple], captives: BTreeSet<Edge>) -> Self {
        Constraint {
            triples: ts.iter().cloned().collect(),
            edges: ts
                .iter()
                .flatten()
                .cloned()
                .filter(|e| !captives.contains(e))
                .collect(),
            captives: captives,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cluster {
    pub nodes: BTreeSet<Constraint>,
    // FIXME: should cover be a part of a Cluster
    pub cover: BTreeSet<Constraint>,
    pub edges: BTreeSet<Edge>,
}

impl Cluster {
    pub fn new<I>(cs: &I) -> Self
    where
        I: IntoIterator<Item = Constraint> + Clone,
    {
        Cluster {
            nodes: cs.clone().into_iter().collect(),
            cover: BTreeSet::new(), // FIXME: exact_cover
            edges: cs
                .clone()
                .into_iter()
                .flat_map(|c| c.edges().collect::<Vec<_>>())
                .collect(),
        }
    }
}

pub trait HasIterableEdges {
    /// Iterates hyperedges in ascending order of their Id.
    fn edges(&self) -> impl Iterator<Item = Edge>;
}

impl HasIterableEdges for Triple {
    fn edges(&self) -> impl Iterator<Item = Edge> {
        self.iter().cloned()
    }
}

impl HasIterableEdges for Constraint {
    fn edges(&self) -> impl Iterator<Item = Edge> {
        self.edges.iter().cloned()
    }
}

impl HasIterableEdges for Cluster {
    fn edges(&self) -> impl Iterator<Item = Edge> {
        self.edges.iter().cloned()
    }
}

