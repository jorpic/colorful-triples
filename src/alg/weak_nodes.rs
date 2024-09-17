use super::edge_index::mk_edge_weights;
use crate::types::HasIterableEdges;

// Weak node is a node that is connected to a weak edge.
pub fn drop_weak_nodes<N, I>(nodes: I, min_weight: usize) -> Vec<N>
where
    N: HasIterableEdges,
    I: IntoIterator<Item = N>,
{
    let mut res: Vec<_> = nodes.into_iter().collect();
    loop {
        let edge_weights = mk_edge_weights(&res);
        let prev_len = res.len();
        res.retain(|node| {
            node.edges()
                .into_iter()
                .all(|edge| edge_weights.get(&edge).unwrap() >= &min_weight)
        });

        if res.len() == prev_len {
            break;
        }
    }

    res
}
