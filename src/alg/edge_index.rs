use crate::types::*;
use std::collections::BTreeMap;

pub type EdgeIx<N> = BTreeMap<Edge, Vec<N>>;

// Each vector in the Map will be ordered if the `nodes` is ordered.
pub fn mk_edge_index<'a, N, I>(nodes: I) -> EdgeIx<&'a N>
where
    N: Node,
    I: IntoIterator<Item = &'a N>,
{
    let mut res: EdgeIx<&'a N> = BTreeMap::new();
    for node in nodes {
        for edge in node.edges() {
            res.entry(edge)
                .and_modify(|x| x.push(node))
                .or_insert(vec![node]);
        }
    }
    res
}

pub fn mk_edge_weights<'a, N, I>(nodes: I) -> BTreeMap<Edge, usize>
where
    N: Node + 'a,
    I: IntoIterator<Item = &'a N>,
{
    let mut res = BTreeMap::new();
    for node in nodes {
        for edge in node.edges() {
            res.entry(edge).and_modify(|x| *x = &*x + 1).or_insert(1);
        }
    }
    res
}
