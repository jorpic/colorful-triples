// #![feature(portable_simd)]

use std::collections::BTreeSet;
//use std::fs::File;
//use std::io::{stdout, BufWriter, Write};

mod alg;
// mod brute_force;
mod triples;
mod types;

use alg::edge_index::{mk_edge_index, mk_edge_weights, EdgeIx};
use alg::weak_edges::join_weak_edges;
use alg::weak_nodes::drop_weak_nodes;

use triples::pythagorean_triples;
use types::*;

fn main() -> anyhow::Result<()> {
    let triples = pythagorean_triples(7825);
    println!("all triples = {}", triples.len());

    let triples = drop_weak_nodes(triples, 2);
    println!("without pendants = {}", triples.len());

    let nodes: Vec<_> = triples.into_iter().map(Node::Triple).collect();
    //let nodes = join_weak_edges(&nodes, 3);
    //let nodes = join_weak_edges(&nodes, 2);

    print_weak_edges(&nodes);
    print_stats1(&nodes);

    let mut nodes = nodes;
    let mut claws = vec![];
    loop {
        if nodes.is_empty() {
            break;
        }
        let edge_ix = mk_edge_index(&nodes);
        let claw = nodes.iter().flat_map(|n| mk_claw(n, &edge_ix)).next();
        if let Some(claw) = claw {
            nodes.retain(|n| !claw.nodes.contains(n));
            claws.push(claw);
        } else {
            break;
        }
    }

    println!("claws = {}, remaining nodes = {}", claws.len(), nodes.len());

    // TODO: can we cover remaining nodes with claw clusters?
    let claw_ix = mk_edge_index(&claws);
    for n in nodes {
        let mut claw_cover: Vec<_> = n
            .edges()
            .flat_map(|e| claw_ix.get(&e))
            .flat_map(|cs|
                cs
                    .iter()
                    .filter(|c| true) // not used in prev covers
                    .next())
            .collect();
    }

    Ok(())
}

fn get_clusters(nodes: &mut Vec<Node>, num_claws: usize, min_ext_len: usize) -> Vec<ClawCluster> {

    let edge_ix = mk_edge_index(&nodes.clone());
    let all_claws: Vec<Claw> = nodes
        .iter()
        .filter(|n| n.is_triple())
        .flat_map(|n| mk_claw(n, &edge_ix))
        .collect();

    println!("all_claws = {}", all_claws.len());

    let mut clusters = vec![];
    let mut new_clusters = 0;
    let mut used_nodes: BTreeSet<Node> = BTreeSet::new();

    'cluster: for c0 in &all_claws {
        let mut cluster = ClawCluster::new(c0.clone());

        for _ in 0..num_claws-1 {
            let mut best_next_claw: Option<Claw> = None;
            let mut best_extension = BTreeSet::new();

            for c1 in &all_claws {
                if !cluster.edges.is_disjoint(&c1.edges) {
                    continue;
                }

                // Nodes covered by selected claws.
                let extension: BTreeSet<Node> = c1.edges
                    .iter()
                    .flat_map(|e| edge_ix.get(e).unwrap())
                    .filter(|n| {
                        n.edges().all(|e| cluster.edges.contains(&e) || c1.edges.contains(&e))
                            && !used_nodes.contains(n)
                            && !cluster.nodes.contains(n)
                            && !c1.nodes.contains(n)
                    })
                    .cloned()
                    .collect();

                // Choose best next claw.
                if extension.len() > best_extension.len() {
                    best_next_claw = Some(c1.clone());
                    best_extension = extension;
                }
            }

            if let Some(next_claw) = best_next_claw {
                cluster.append(next_claw, best_extension);
            } else {
                continue 'cluster;
            }
        }

        if cluster.extension.len() >= min_ext_len {
            println!("{} {}", cluster.base.len(), cluster.extension.len());
            for n in &cluster.nodes {
                used_nodes.insert(n.clone());
            }
            clusters.push(cluster);
            new_clusters += 1;
        }
    }

    nodes.retain(|n| !used_nodes.contains(n));
    clusters
}


fn print_stats1(nodes: &[Node]) {
    let mut triples = 0;
    let mut quads = 0;
    for n in nodes {
        match n {
            Node::Triple(_) => triples += 1,
            Node::Quad(_) => quads += 1,
        }
    }

    println!("triples = {}, quads = {}", triples, quads);
}

fn print_weak_edges(nodes: &[Node]) {
    let edges = mk_edge_weights(nodes);
    let weak_edges = edges.values().filter(|ns| **ns < 3).count();
    println!("weak edges = {}", weak_edges);
}

fn mk_claw(node: &Node, edge_ix: &EdgeIx<Node>) -> Option<Claw> {
    let mut children = vec![];

    for e in node.edges() {
        let child = edge_ix
            .get(&e)
            .unwrap()
            .iter()
            .filter(|n| n.is_triple() && *n != node)
            .next();
        if let Some(child) = child {
            children.push(*child);
        } else {
            return None;
        }
    }

    Some(Claw::new(node, &children))
}

//fn save_all(clusters: &[Cluster], file_name: &str) -> anyhow::Result<()> {
//    let file = File::create(file_name)?;
//    let mut writer = BufWriter::new(file);
//    serde_json::to_writer(&mut writer, &clusters)?;
//    Ok(writer.flush()?)
//}
