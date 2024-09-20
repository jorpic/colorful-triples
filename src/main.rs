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

use alg::exact_cover::exact_cover;
use alg::neighbourhoods::{tight_neighborhoods, NeighborhoodOptions};
use alg::weak_edges::{join_weak_edges, join_chains};
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

    {
        let all_edges = constraints.iter().flat_map(|c| c.edges.iter()).collect::<BTreeSet<_>>();
        println!("all edges = {}", all_edges.len());
    }

    let constraints = join_weak_edges(&constraints, 3);
    let constraints = join_weak_edges(&constraints, 2);
    let mut constraints = join_chains(&constraints);

    {
        let strong_edges = constraints.iter().flat_map(|c| c.edges.iter()).collect::<BTreeSet<_>>();
        println!(
            "strong edges = {}, joined constraints = {}",
            strong_edges.len(),
            constraints.len(),
        );
    }

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

    for nh_min_weight in [3, 2] {
        loop {
            println!();

            let opts = TightOptions {
                nh_min_weight,
                cover_nodes: 14,
                min_constraints: 25,
            };

            let tight_clusters = get_tight_clusters(&constraints, opts);

            if tight_clusters.is_empty() {
                break;
            }

            for tc in tight_clusters {
                println!(
                    "mw={} triples={} edges={} cover={}",
                    nh_min_weight,
                    tc.nodes.len(),
                    tc.edges.len(),
                    tc.cover.len(),
                );

                constraints.retain(|c| !tc.nodes.contains(c));
                clusters.push(tc);
            }
        }

        let covered_edges = clusters.iter().flat_map(|c| c.edges.iter()).collect::<BTreeSet<_>>();
        println!(
            "clusters = {}, constraints in clusters = {}, covered_edges = {}, remaining constraints = {}",
            clusters.len(),
            clusters.iter().map(|c| c.nodes.len()).sum::<usize>(), // FIXME: don't count
                                                                   // repititions
            covered_edges.len(),
            constraints.len(),
        );
    }

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


struct TightOptions {
    nh_min_weight: usize,
    cover_nodes: usize,
    min_constraints: usize,
}

fn get_tight_clusters(
    constraints: &[Constraint],
    opts: TightOptions,
) -> Vec<Cluster> {
    let nh_opts = NeighborhoodOptions {
        width: 3,
        min_weight: opts.nh_min_weight,
    };

    let mut nhs = vec![];
    for c in tight_neighborhoods(constraints, &nh_opts) {
        // Shrink cover and drop uncovered triples.
        // FIXME: Try to order nodes by weight before shrinking.
        let nodes = c.nodes.iter().cloned().collect::<Vec<_>>();
        let cover = exact_cover(opts.cover_nodes, &nodes);
        let cover: BTreeSet<Constraint> =
            cover.iter().take(opts.cover_nodes).cloned().collect();
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
        if opts.min_constraints <= c.nodes.len() && c.edges.len() == covered_edges.len() {
            nhs.push(Cluster::new(&nodes));
        }
    }

    // Prefer more nodes but smaller cover.
    nhs.sort_by_key(|c: &Cluster| (cmp::Reverse(c.nodes.len()), c.cover.len()));

    // Select disjoint clusters (i.e. not having common constraints).
    // FIXME: selecting clusters with disjoint EDGES allegedly allows to cover more edges.
    let mut used_constraints = BTreeSet::new();
    let mut res = vec![];
    for c in nhs {
        if c.nodes.intersection(&used_constraints).count() < 10 {
            for n in &c.nodes {
                used_constraints.insert(n.clone());
            }
            res.push(c);
        }
    }
    res
}
