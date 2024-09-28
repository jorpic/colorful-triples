use super::edge_index::mk_edge_index;
use crate::types::{HasIterableEdges, Node};
use std::collections::BTreeSet;

// Weak edge is an edge that connects too few nodes.
pub fn join_weak_edges(nodes: &[Node], weight: usize) -> Vec<Node> {
    let mut res = vec![];
    let mut joined_nodes: BTreeSet<Node> = BTreeSet::new();

    for (e, ns) in mk_edge_index(nodes) {
        if ns.len() == weight {
            if ns
                .iter()
                .all(|n| n.is_triple() && !joined_nodes.contains(n))
            {
                // Add bunch of quads instead of joined triples.
                // One quad for weight=2, three quads for weight=3.
                for i in 0..ns.len() - 1 {
                    let a: Vec<_> = ns[i].edges().filter(|x| *x != e).collect();
                    for j in i + 1..ns.len() {
                        let b: Vec<_> =
                            ns[j].edges().filter(|x| *x != e).collect();

                        res.push(Node::Quad([a[0], a[1], b[0], b[1]]));
                    }
                }

                for n in ns {
                    joined_nodes.insert(n);
                }
            }
        }
    }

    for n in nodes {
        if !joined_nodes.contains(n) {
            res.push(*n);
        }
    }

    res.sort();
    res
}
