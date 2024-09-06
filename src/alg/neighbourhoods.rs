use std::collections::BTreeSet;

use crate::cluster::Cluster;
use crate::types::*;

use super::connected_components::*;
use super::edge_index::*;

pub struct NeighborhoodOptions {
    pub width: usize,
    pub min_weight: usize,
}

pub fn tight_neighborhoods<'a>(
    edge_ix: &'a EdgeIx<Cluster>,
    opt: &'a NeighborhoodOptions,
) -> impl Iterator<Item = Cluster> + 'a {
    edge_ix
        .keys()
        .map(move |edge| edge_neighborhood(*edge, edge_ix, opt.width))
        .filter_map(move |cluster| {
            let triples = drop_weak_nodes(cluster.triples(), opt.min_weight);
            if triples.is_empty() {
                None
            } else {
                // Some([Cluster::from_triples(&triples)])
                Some(connected_components(&triples))
            }
        })
        .flatten()
}

fn edge_neighborhood(
    center: Edge,
    edge_ix: &EdgeIx<Cluster>,
    width: usize,
) -> Cluster {
    let mut subgraph_nodes = BTreeSet::new();
    let mut subgraph_edges = BTreeSet::new();
    let mut prev_edges = BTreeSet::new();
    prev_edges.insert(center);
    let mut new_edges = BTreeSet::new();

    for _w in 0..width {
        for e in &prev_edges {
            for n in edge_ix.get(e).unwrap() {
                // NB: We are skipping large clusters here
                // to prevent NH explosion!
                if n.edge_weights.len() > 20 {
                    continue;
                }
                if subgraph_nodes.insert(n) {
                    n.edges().for_each(|new_edge| {
                        let is_new_edge = e != &new_edge
                            && !prev_edges.contains(&new_edge)
                            && !subgraph_edges.contains(&new_edge);
                        if is_new_edge {
                            new_edges.insert(new_edge);
                        }
                    })
                }
            }
        }

        subgraph_edges.append(&mut prev_edges); // prev_edges is empty now
        prev_edges.append(&mut new_edges); // new_edges is empty now
    }

    Cluster::from_triples(subgraph_nodes.iter().flat_map(|n| &n.nodes))
}

// Weak node is a node that is connected to a weak edge.
// Weak edge is an edge that connects < min_weight nodes.
pub fn drop_weak_nodes<N, I>(graph: I, min_weight: usize) -> Vec<N>
where
    N: Node,
    I: IntoIterator<Item = N>,
{
    let mut res: Vec<_> = graph.into_iter().collect();
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
