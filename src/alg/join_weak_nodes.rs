use std::collections::BTreeSet;
use crate::cluster::{Cluster, ClusterId};
use super::edge_index::*;

pub struct JoinNodesOptions {
    pub min_edge_weight: usize,
    pub max_edges: usize,
}

pub fn join_weak_nodes(
    clusters: &[Cluster],
    opt: &JoinNodesOptions
) -> Vec<Cluster> {
    let mut clusters = clusters.to_vec();
    loop {
        let new_clusters = join_weak_nodes_single_pass(&clusters, opt);
        if new_clusters.len() == clusters.len() {
            return new_clusters;
        }
        clusters = new_clusters;
    }
}

fn join_weak_nodes_single_pass(
    clusters: &[Cluster],
    opt: &JoinNodesOptions,
) -> Vec<Cluster> {
    let edge_index = mk_edge_index(clusters);
    let weak_edges = edge_index
        .values()
        .filter(|cs| 1 < cs.len() && cs.len() < opt.min_edge_weight);

    let mut merged: BTreeSet<ClusterId> = BTreeSet::new();
    let mut new_clusters: Vec<Cluster> = Vec::new();

    'next_link: for adjacent_clusters in weak_edges {
        // Check if there is a conflict.
        for c in adjacent_clusters {
            if merged.contains(&c.id()) {
                continue 'next_link;
            }
        }

        // No conflicts, safe to merge nodes.
        let mut new_cluster = Cluster::default();
        for c in adjacent_clusters {
            new_cluster.merge_with(c);
        }

        if new_cluster.edge_weights.len() <= opt.max_edges {
            for c in adjacent_clusters {
                merged.insert(c.id());
            }
            new_clusters.push(new_cluster);
        }
    }

    clusters
        .iter()
        .filter(|c| !merged.contains(&c.id()))
        .chain(new_clusters.iter())
        .cloned()
        .collect()
}


