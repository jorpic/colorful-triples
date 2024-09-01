use std::collections::BTreeSet;

use crate::types::*;
use crate::cluster::Cluster;
use super::edge_index::*;


pub fn connected_components(triples: &[Triple]) -> Vec<Cluster> {
    let edge_ix = mk_edge_index(triples);
    let mut triples = Vec::from(triples);
    let mut components = vec![];

    while let Some(node) = triples.pop() {
        let mut component: BTreeSet<Triple> = BTreeSet::new();
        let mut prev_nodes: BTreeSet<Triple> = BTreeSet::from([node]);
        let mut new_nodes: BTreeSet<Triple> = BTreeSet::new();

        loop {
            for node in prev_nodes.iter() {
                for edge in node.edges() {
                    for nn in edge_ix.get(&edge).unwrap() {
                        if !component.contains(*nn) && !prev_nodes.contains(*nn) {
                            new_nodes.insert(**nn);
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

        triples.retain(|t| !component.contains(t));
        components.push(Cluster::from_triples(&component));
    }

    components
}
