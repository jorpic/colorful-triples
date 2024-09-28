#![feature(portable_simd)]
#![feature(impl_trait_in_assoc_type)]

use std::cmp;
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
    let nodes = join_weak_edges(&nodes, 3);
    let nodes = join_weak_edges(&nodes, 2);

    print_weak_edges(&nodes);
    print_stats1(&nodes);

    let mut nodes = nodes;
    for is_triple in [false, true] {
        loop {
            let (used_nodes, res) = mk_pyramid(&nodes, 0, is_triple);
            if res.is_empty() {
                break;
            }

            println!(
                "{}: {:?}",
                res.len(),
                res.iter().map(|c| c.extension.len()).collect::<Vec<_>>()
            );
            nodes.retain(|n| !used_nodes.contains(n));
        }

        print_stats1(&nodes);
    }

    Ok(())
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

fn mk_pyramid(
    nodes: &Vec<Node>,
    min_ext_size: usize,
    is_triple: bool,
) -> (BTreeSet<Node>, Vec<ExtendedPyramid>) {
    let mut candidates = vec![];

    let edge_ix = mk_edge_index(nodes);
    'level0: for n0 in nodes {
        if n0.is_triple() != is_triple {
            continue;
        }

        let l0 = vec![*n0];

        if let Some(n0_children) = get_children(n0, &edge_ix, &[&l0]) {
            let l1 = n0_children;
            let mut l2 = vec![];

            for n1 in &l1 {
                let n1_children = get_children(n1, &edge_ix, &[&l0, &l1, &l2]);
                if let Some(n1_children) = n1_children {
                    for n2 in &n1_children {
                        l2.push(*n2);
                    }
                } else {
                    continue 'level0;
                }
            }

            let mut used_nodes = BTreeSet::new();
            for n in l0.iter().chain(&l1).chain(&l2) {
                used_nodes.insert(*n);
            }

            // Get pyramid extension
            let p = Pyramid::new(n0, &l1, &l2);
            let extension: BTreeSet<Node> = nodes
                .iter()
                .filter(|n| !used_nodes.contains(n) && p.covers(n))
                .cloned()
                .collect();

            candidates.push(ExtendedPyramid { base: p, extension });
        }
    }

    candidates.sort_by_key(|p| cmp::Reverse(p.extension.len()));

    // select multiple non-intersecting and with big extension
    let mut used_nodes = BTreeSet::new();
    let mut res = vec![];
    for c in candidates {
        let is_disjoint = used_nodes.is_disjoint(&c.extension)
            && used_nodes.is_disjoint(&c.base.nodes);
        if is_disjoint && c.extension.len() >= min_ext_size {
            for n in &c.base.nodes {
                used_nodes.insert(*n);
            }
            for n in &c.extension {
                used_nodes.insert(*n);
            }
            res.push(c);
        }
    }

    (used_nodes, res)
}

fn get_children(
    node: &Node,
    edge_ix: &EdgeIx<Node>,
    used_nodes: &[&Vec<Node>],
) -> Option<Vec<Node>> {
    let mut children = vec![];

    for e in node.edges() {
        if let Some(child) = get_first_child(&e, edge_ix, used_nodes) {
            children.push(child);
        } else {
            return None;
        }
    }

    Some(children)
}

fn get_first_child(
    edge: &Edge,
    edge_ix: &EdgeIx<Node>,
    used_nodes: &[&Vec<Node>],
) -> Option<Node> {
    edge_ix
        .get(edge)
        .unwrap()
        .iter()
        .filter(|x| {
            x.is_triple()
                && !used_nodes.iter().any(|u| u.iter().any(|y| *x == y))
        })
        .next()
        .copied()
}

//fn save_all(clusters: &[Cluster], file_name: &str) -> anyhow::Result<()> {
//    let file = File::create(file_name)?;
//    let mut writer = BufWriter::new(file);
//    serde_json::to_writer(&mut writer, &clusters)?;
//    Ok(writer.flush()?)
//}
