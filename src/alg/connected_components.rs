use std::collections::BTreeSet;

use super::edge_index::*;
use crate::types::*;

pub fn connected_components(cs: &[Constraint]) -> Vec<Cluster> {
    let edge_ix = mk_edge_index(cs);
    let mut cs = Vec::from(cs);
    let mut components = vec![];

    while let Some(node) = cs.pop() {
        let mut component: BTreeSet<Constraint> = BTreeSet::new();
        let mut prev_nodes: BTreeSet<Constraint> = BTreeSet::from([node]);
        let mut new_nodes: BTreeSet<Constraint> = BTreeSet::new();

        loop {
            for node in prev_nodes.iter() {
                for edge in node.edges() {
                    for nn in edge_ix.get(&edge).unwrap() {
                        if !component.contains(nn) && !prev_nodes.contains(nn) {
                            new_nodes.insert(nn.clone());
                        }
                    }
                }
            }

            component.append(&mut prev_nodes);

            if new_nodes.is_empty() {
                break;
            }

            prev_nodes.append(&mut new_nodes);
        }

        cs.retain(|t| !component.contains(t));
        components.push(Cluster::new(&component));
    }

    components
}
