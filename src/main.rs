#![feature(portable_simd)]
use std::cmp;
use std::collections::BTreeSet;
use std::fs::File;
use std::io::{stdout, BufWriter, Write};
use thousands::Separable;

mod alg;
mod brute_force;
mod cluster;
mod triples;
mod types;

use alg::edge_index::*;
use alg::neighbourhoods::{
    drop_weak_nodes, tight_neighborhoods, NeighborhoodOptions,
};
use alg::join_weak_edges::*;
use brute_force::fast_brute_force;
use cluster::Cluster;
use triples::pythagorean_triples;
use types::*;

fn main() -> anyhow::Result<()> {
    let triples = pythagorean_triples(7825);

    let hgraph: Vec<_> = triples.iter().map(Cluster::singleton).collect();
    let mut hgraph = drop_weak_nodes(hgraph, 2);
    let mut clusters: Vec<Cluster> = vec![];
    let all_triples: Vec<Triple> = hgraph.iter().flat_map(|t| &t.nodes).cloned().collect();

    for min_weight in [2] {
        loop {
            println!();

            let tight_clusters = get_tight_clusters(
                &hgraph,
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
                    tc.edge_weights.len(),
                    tc.cover.len(),
                );

                hgraph.retain(|c| c.nodes.is_disjoint(&tc.nodes));
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

    let free_triples: Vec<Triple> =
        hgraph.into_iter().flat_map(|c| c.nodes).collect();

    println!(
        "remaining triples = {}",
        free_triples.len(),
    );

    let mini_clusters = join_weak_edges(&all_triples, &free_triples);

    println!(
        "1-clusters = {}, 2-clusters = {}, 3-clusters = {}",
        mini_clusters.iter().filter(|c| c.triples.len() == 1).count(),
        mini_clusters.iter().filter(|c| c.triples.len() == 2).count(),
        mini_clusters.iter().filter(|c| c.triples.len() == 3).count(),
    );


    save_all(&clusters, "clusters.json")?;
    solve_all(&clusters);

    Ok(())
}

fn save_all(clusters: &[Cluster], file_name: &str) -> anyhow::Result<()> {
    let file = File::create(file_name)?;
    let mut writer = BufWriter::new(file);
    serde_json::to_writer(&mut writer, &clusters)?;
    Ok(writer.flush()?)
}

fn solve_all(clusters: &[Cluster]) {
    for c in clusters {
        print!(
            "triples={} edges={} cover={} ",
            c.nodes.len(),
            c.edge_weights.len(),
            c.cover.len(),
        );

        stdout().flush().unwrap();

        let now = std::time::Instant::now();
        println!(
            "solutions={} elapsed={:.2?}",
            fast_brute_force(
                &c.cover,
                &c.nodes.difference(&c.cover).cloned().collect()
            )
            .separate_with_commas(),
            now.elapsed()
        );
    }
}

fn mk_companion_cluster(
    c: &Cluster,
    free_triples_ix: &EdgeIx<Triple>,
) -> Cluster {
    let mut cover: BTreeSet<Triple> = BTreeSet::new();
    let mut covered_edges: BTreeSet<Edge> = BTreeSet::new();

    let empty = vec![];
    let free_and_adjacent_to =
        |e: Edge| free_triples_ix.get(&e).unwrap_or(&empty);
    let touches = |edges: &BTreeSet<Edge>, t: &Triple| {
        t.iter().any(|e| edges.contains(e))
    };
    let disjoint =
        |a: &Triple, b: &Triple| a.iter().all(|x| b.iter().all(|y| x != y));

    for t in &c.cover {
        'out: for a in free_and_adjacent_to(t[0]) {
            if touches(&covered_edges, a) {
                continue;
            }
            for b in free_and_adjacent_to(t[1]) {
                if touches(&covered_edges, b) || !disjoint(a, b) {
                    continue;
                }
                for c in free_and_adjacent_to(t[2]) {
                    if touches(&covered_edges, c)
                        || !disjoint(a, c)
                        || !disjoint(b, c)
                    {
                        continue;
                    }

                    a.iter().for_each(|e| {
                        covered_edges.insert(*e);
                    });
                    b.iter().for_each(|e| {
                        covered_edges.insert(*e);
                    });
                    c.iter().for_each(|e| {
                        covered_edges.insert(*e);
                    });
                    cover.insert(*a);
                    cover.insert(*b);
                    cover.insert(*c);
                    break 'out;
                }
            }
        }

        if cover.len() == 12 {
            break;
        }
    }

    let triples = covered_edges
        .iter()
        .flat_map(|e| free_triples_ix.get(e))
        .flatten()
        .filter(|t| t.iter().all(|e| covered_edges.contains(e)))
        .cloned()
        .collect();

    Cluster::from_cover(cover, triples)
}

fn get_tight_clusters(
    clusters: &[Cluster],
    opts: &NeighborhoodOptions,
) -> Vec<Cluster> {
    let global_edge_ix = mk_edge_index(clusters);

    let mut nhs = tight_neighborhoods(&global_edge_ix, opts)
        .map(|c| {
            // Shrink cover and drop uncovered triples.
            // FIXME: Try to order nodes by weight before shrinking.
            let cover: BTreeSet<Triple> =
                c.cover.iter().take(14).cloned().collect();
            let covered_edges: BTreeSet<Edge> =
                cover.iter().flatten().cloned().collect();
            let nodes: BTreeSet<Triple> = c
                .nodes
                .iter()
                .filter(|t| t.iter().all(|e| covered_edges.contains(e)))
                .cloned()
                .collect();
            Cluster::from_cover(cover, nodes)
        })
        .filter(|c| {
            25 <= c.nodes.len() && c.edge_weights.len() == c.cover.len() * 3
        })
        .collect::<Vec<_>>();

    // Prefer more nodes but smaller cover
    nhs.sort_by_key(|c: &Cluster| (cmp::Reverse(c.nodes.len()), c.cover.len()));

    // Select nonintersecting clusters
    let mut used_edges = BTreeSet::new();
    let mut res = vec![];
    for c in nhs {
        let has_intersection =
            c.edge_weights.keys().any(|e| used_edges.contains(e));

        if !has_intersection {
            for e in c.edge_weights.keys() {
                used_edges.insert(*e);
            }
            res.push(c);
        }
    }
    res
}
