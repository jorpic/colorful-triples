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
pub struct Claw {
    pub nodes: BTreeSet<Node>,
    pub edges: BTreeSet<Edge>,
}

impl Claw {
    pub fn new(n0: &Node, l1: &[Node]) -> Self {
        let nodes = [*n0].iter().chain(l1).cloned().collect::<BTreeSet<_>>();
        let edges = nodes.iter().flat_map(|n| n.edges()).collect();

        Self { nodes, edges }
    }
}

impl HasIterableEdges for Claw {
    fn edges(&self) -> impl Iterator<Item = Edge> {
        self.edges.iter().cloned()
    }
}


#[derive(Clone, Debug, Default)]
pub struct ClawCluster {
    pub base: Vec<Claw>,
    pub extension: BTreeSet<Node>,
    pub edges: BTreeSet<Edge>,
    pub nodes: BTreeSet<Node>,
}

impl ClawCluster {
    pub fn new(c0: Claw) -> Self {
        let mut cluster = Self::default();
        cluster.append(c0, BTreeSet::new());
        cluster
    }

    pub fn append(&mut self, c: Claw, mut ext: BTreeSet<Node>) {
        self.base.push(c);
        self.extension.append(&mut ext);

        self.edges = self.base
            .iter()
            .flat_map(|c| &c.edges)
            .cloned()
            .collect();
        self.nodes = self.base
            .iter()
            .flat_map(|c| &c.nodes)
            .chain(&self.extension)
            .cloned()
            .collect();
    }
}
