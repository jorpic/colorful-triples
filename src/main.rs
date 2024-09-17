#![feature(portable_simd)]
#![feature(impl_trait_in_assoc_type)]

use std::cmp;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::{stdout, BufWriter, Write};
use thousands::Separable;

mod alg;
mod brute_force;
mod triples;
mod types;

use alg::neighbourhoods::{tight_neighborhoods, NeighborhoodOptions};
use alg::weak_edges::join_weak_edges;
use alg::weak_nodes::drop_weak_nodes;
use brute_force::fast_brute_force;
use triples::pythagorean_triples;
use types::*;

fn main() -> anyhow::Result<()> {
    let triples = pythagorean_triples(7825);
    println!("all triples = {}", triples.len());

    let triples = drop_weak_nodes(triples, 2);
    println!("without pendants = {}", triples.len());

    let constraints = triples.into_iter().map(Constraint::one).collect::<Vec<_>>();
    let mut constraints = join_weak_edges(&constraints);
    println!("weak edges joined = {}", constraints.len());

    {
        let mut stats = BTreeMap::new();
        for c in &constraints {
            let key = (c.triples.len(), c.edges.len());
            stats
                .entry(key)
                .and_modify(|x| *x += 1usize)
                .or_insert(1usize);
        }
        println!("{:?}", stats);
    }

    let mut clusters: Vec<Cluster> = vec![];

    for min_weight in [3, 2] {
        loop {
            println!();

            let tight_clusters = get_tight_clusters(
                &constraints,
                &NeighborhoodOptions {
                    width: 3,
                    min_weight,
                },
            );

            if tight_clusters.is_empty() {
                break;
            }

            for tc in tight_clusters {
                println!(
                    "mw={} triples={} edges={} cover={}",
                    min_weight,
                    tc.nodes.len(),
                    tc.edges.len(),
                    tc.cover.len(),
                );

                constraints.retain(|c| !tc.nodes.contains(c));
                clusters.push(tc);
            }
        }
    }

    println!(
        "clusters = {}, triples in clusters = {}",
        clusters.len(),
        clusters.iter().map(|c| c.nodes.len()).sum::<usize>(),
    );

    clusters.sort_by_key(|c| cmp::Reverse(c.nodes.len()));

    // save_all(&clusters, "clusters.json")?;
    //solve_all(&clusters);

    Ok(())
}

//fn save_all(clusters: &[Cluster], file_name: &str) -> anyhow::Result<()> {
//    let file = File::create(file_name)?;
//    let mut writer = BufWriter::new(file);
//    serde_json::to_writer(&mut writer, &clusters)?;
//    Ok(writer.flush()?)
//}

//fn solve_all(clusters: &[Cluster]) {
//    for c in clusters {
//        print!(
//            "triples={} edges={} cover={} ",
//            c.nodes.len(),
//            c.edges.len(),
//            c.cover.len(),
//        );
//
//        stdout().flush().unwrap();
//
//        let now = std::time::Instant::now();
//        println!(
//            "solutions={} elapsed={:.2?}",
//            fast_brute_force(
//                &c.cover,
//                &c.nodes.difference(&c.cover).cloned().collect()
//            )
//            .separate_with_commas(),
//            now.elapsed()
//        );
//    }
//}
//
fn get_tight_clusters(
    constraints: &[Constraint],
    opts: &NeighborhoodOptions,
) -> Vec<Cluster> {
    let mut nhs = tight_neighborhoods(constraints, opts)
        .map(|c| {
            // Shrink cover and drop uncovered triples.
            // FIXME: Try to order nodes by weight before shrinking.
            let cover: BTreeSet<Constraint> =
                c.cover.iter().take(14).cloned().collect();
            let covered_edges: BTreeSet<Edge> = cover
                .iter()
                .flat_map(|c| c.edges().collect::<Vec<_>>())
                .collect();
            let nodes: BTreeSet<Constraint> = c
                .nodes
                .iter()
                .filter(|t| t.edges().all(|e| covered_edges.contains(&e)))
                .cloned()
                .collect();
            // FIXME: store cover somewhere
            Cluster::new(&nodes)
        })
        // FIXME: cover.len() * 3 is not correct
        .filter(|c| 25 <= c.nodes.len() && c.edges.len() == c.cover.len() * 3)
        .collect::<Vec<_>>();

    // Prefer more nodes but smaller cover.
    nhs.sort_by_key(|c: &Cluster| (cmp::Reverse(c.nodes.len()), c.cover.len()));

    // Select nonintersecting clusters.
    let mut used_edges = BTreeSet::new();
    let mut res = vec![];
    for c in nhs {
        let has_intersection = c.edges.iter().any(|e| used_edges.contains(e));

        if !has_intersection {
            for e in &c.edges {
                used_edges.insert(*e);
            }
            res.push(c);
        }
    }
    res
}
