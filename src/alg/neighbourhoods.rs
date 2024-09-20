use std::collections::BTreeSet;

use crate::types::*;

use super::connected_components::*;
use super::edge_index::*;
use super::weak_nodes::drop_weak_nodes;

pub struct NeighborhoodOptions {
    pub width: usize,
    pub min_weight: usize,
}

pub fn tight_neighborhoods<'a>(
    constraints: &[Constraint],
    opt: &'a NeighborhoodOptions,
) -> impl Iterator<Item = Cluster> + 'a {
    let edge_ix = mk_edge_index(constraints);
    let all_edges: Vec<Edge> = edge_ix.keys().cloned().collect();

    all_edges.into_iter().flat_map(move |edge| {
        let nodes = edge_neighborhood(edge, &edge_ix, opt.width);
        let strong_nodes = drop_weak_nodes(nodes, opt.min_weight);
        connected_components(&strong_nodes)
    })
}

fn edge_neighborhood(
    center: Edge,
    edge_ix: &EdgeIx<Constraint>,
    width: usize,
) -> Vec<Constraint> {
    let mut subgraph_nodes: BTreeSet<Constraint> = BTreeSet::new();
    let mut subgraph_edges = BTreeSet::new();
    let mut prev_edges = BTreeSet::from([center]);
    let mut new_edges = BTreeSet::new();

    for _w in 0..width {
        for e in &prev_edges {
            for n in edge_ix.get(e).unwrap() {
                if subgraph_nodes.insert(n.clone()) {
                    for new_edge in n.edges() {
                        let is_new_edge = e != &new_edge
                            && !prev_edges.contains(&new_edge)
                            && !subgraph_edges.contains(&new_edge);
                        if is_new_edge {
                            new_edges.insert(new_edge);
                        }
                    }
                }
            }
        }

        subgraph_edges.append(&mut prev_edges); // prev_edges is empty now
        prev_edges.append(&mut new_edges); // new_edges is empty now
    }

    subgraph_nodes.into_iter().collect()
}
