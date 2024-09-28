use std::collections::BTreeSet;

pub type Edge = u16;
pub type Triple = [Edge; 3];

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Node {
    Triple([Edge; 3]),
    Quad([Edge; 4]),
}

impl Node {
    pub fn is_triple(&self) -> bool {
        match self {
            Node::Triple(_) => true,
            _ => false,
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

impl HasIterableEdges for Node {
    fn edges(&self) -> impl Iterator<Item = Edge> {
        match self {
            Node::Triple(es) => es.iter().cloned(),
            Node::Quad(es) => es.iter().cloned(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Pyramid {
    pub edges: Vec<Edge>,
    pub nodes: BTreeSet<Node>,
    edge_set: BTreeSet<Edge>,
}

impl Pyramid {
    pub fn new(n0: &Node, l1: &[Node], l2: &[Node]) -> Self {
        let edge_set = BTreeSet::from_iter(
            n0.edges()
                .chain(l1.iter().flat_map(|n| n.edges()))
                .chain(l2.iter().flat_map(|n| n.edges())),
        );

        let nodes = [*n0].iter().chain(l1).chain(l2).cloned().collect();

        // FIXME: reorder edges canonically
        let mut edges = vec![];

        Self {
            edges,
            nodes,
            edge_set,
        }
    }

    pub fn covers(&self, n: &Node) -> bool {
        n.edges().all(|e| self.edge_set.contains(&e))
    }
}

#[derive(Clone, Debug)]
pub struct ExtendedPyramid {
    pub base: Pyramid,
    pub extension: BTreeSet<Node>,
}
