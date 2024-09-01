pub type Edge = u16;
pub type Triple = [Edge; 3];

pub trait Node {
    type IterEdges<'a>: IntoIterator<Item = Edge>
    where
        Self: 'a;
    type IterTriples<'a>: IntoIterator<Item = Triple>
    where
        Self: 'a;

    /// Iterates hyperedges in ascending order of their Id.
    fn edges(&self) -> Self::IterEdges<'_>;

    /// Iterates triples in ascending order.
    fn triples(&self) -> Self::IterTriples<'_>;
}
