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
