#![feature(portable_simd)]
use std::cmp;
use std::fs::File;
use std::io::{BufWriter, Write, stdout};
use std::collections::BTreeSet;
use thousands::Separable;

mod alg;
mod brute_force;
mod cluster;
mod triples;
mod types;

use types::*;
use triples::pythagorean_triples;
use brute_force::fast_brute_force;
use cluster::Cluster;
use alg::edge_index::{mk_edge_index, mk_edge_weights};
use alg::neighbourhoods::{NeighborhoodOptions, tight_neighborhoods, drop_weak_nodes};


fn main() -> anyhow::Result<()> {
    let triples = pythagorean_triples(7825);

    let hgraph: Vec<_> = triples.iter().map(Cluster::singleton).collect();
    let global_edge_weights = mk_edge_weights(&hgraph);
    let mut hgraph = drop_weak_nodes(hgraph, 2);
    let mut clusters: Vec<Cluster> = vec![];

    //for min_edge_weight in 3..19 {
    //    hgraph = join_weak_nodes(
    //        &hgraph,
    //        &JoinNodesOptions {min_edge_weight, max_edges: 41});
    //
    //    println!(
    //        "nodes after joining with min_edge_weight={}: {}",
    //        min_edge_weight,
    //        hgraph.len());
    //}
    //
    //{
    //    let mut ts: Vec<Cluster> = vec![];
    //    for c in hgraph {
    //        let inner_edges = c.inner_edges(&global_edge_weights).len();
    //        if inner_edges > 5 {
    //            println!(
    //                "innr triples={} edges={} inner_edges={} base={}",
    //                c.nodes.len(),
    //                c.edge_weights.len(),
    //                inner_edges,
    //                c.base.len(),
    //            );
    //            clusters.push(c);
    //        } else {
    //            for t in &c.nodes {
    //                ts.push(Cluster::from_triples([t]));
    //            }
    //        }
    //    }
    //    hgraph = ts;
    //}

    for min_weight in [3] {
        loop {
            println!("");

            let tight_clusters = get_tight_clusters(
                &hgraph,
                &NeighborhoodOptions {width: 3, min_weight});

            if tight_clusters.len() == 0 {
                break;
            }

            for tc in tight_clusters {
                println!(
                    "mw={} triples={} edges={} inner_edges={} base={}",
                    min_weight,
                    tc.nodes.len(),
                    tc.edge_weights.len(),
                    tc.inner_edges(&global_edge_weights).len(),
                    tc.base.len(),
                );

                hgraph.retain(|c| c.nodes.is_disjoint(&tc.nodes));
                clusters.push(tc);
            }
        }
    }

    println!(
        "remaining triples = {}",
        hgraph.iter().map(|c| c.nodes.len()).sum::<usize>());

    println!(
        "clusters = {}, triples in clusters = {}",
        clusters.len(),
        clusters.iter().map(|c| c.nodes.len()).sum::<usize>(),
    );

    clusters.sort_by_key(
        |c| (
            cmp::Reverse(c.base.len()),
            cmp::Reverse(c.nodes.len()),
        )
    );


    let mut new_clusters: Vec<Cluster> = vec![];
    {
        let free_triples: Vec<Triple> = hgraph
            .into_iter().flat_map(|c| c.nodes).collect();
        let free_triples_ix = mk_edge_index(&free_triples);
        let mut used_triples: BTreeSet<Triple> = BTreeSet::new();

        for c in clusters {
            println!("");
            loop {
                let mut companion = vec![];
                let mut used_edges: BTreeSet<Edge> = BTreeSet::new();
                for e in c.edges() {
                    let Some(ts) = free_triples_ix.get(&e) else {
                        continue;
                    };
                    for t in ts {
                        if used_triples.contains(*t) {
                            continue;
                        }
                        if t.iter().any(|x| used_edges.contains(x)) {
                            continue
                        }
                        for x in *t {
                            used_edges.insert(*x);
                        }
                        companion.push(*t);
                        break;
                    }
                    if companion.len() >= 13 {
                        break;
                    }
                }
                if companion.len() >= 13 {
                    for t in &companion {
                        used_triples.insert(**t);
                    }
                    // Add more triples
                    for e in &used_edges {
                        let Some(ts) = free_triples_ix.get(e) else {
                            continue;
                        };
                        for t in ts {
                            if used_triples.contains(*t) {
                                continue;
                            }
                            if t.iter().all(|x| used_edges.contains(x)) {
                                used_triples.insert(**t);
                                companion.push(*t);
                            }
                        }
                    }
                    let cc = Cluster::from_triples(companion);
                    println!(
                        "comp triples={} edges={} base={}",
                        cc.nodes.len(),
                        cc.edge_weights.len(),
                        cc.base.len(),
                    );
                    new_clusters.push(cc);
                } else {
                    break; // can't make good companion
                }
            }
            new_clusters.push(c);
        }
    }

    let clusters = new_clusters;

    {
        let file = File::create("clusters.json")?;
        let mut writer = BufWriter::new(file);
        serde_json::to_writer(&mut writer, &clusters)?;
        writer.flush()?;
    }

    for c in &clusters {
        print!(
            "triples={} edges={} base={} inner_edges={} ",
            c.nodes.len(),
            c.edge_weights.len(),
            c.base.len(),
            c.inner_edges(&global_edge_weights).len(),
        );

        stdout().flush().unwrap();

        let now = std::time::Instant::now();
        println!(
            "solutions={} elapsed={:.2?}",
            fast_brute_force(
                &c.base,
                &c.nodes.difference(&c.base).cloned().collect()
            )
                .separate_with_commas(),
            now.elapsed()
        );
    }

    Ok(())
}

fn get_tight_clusters(clusters: &[Cluster], opts: &NeighborhoodOptions) -> Vec<Cluster> {
    let global_edge_ix = mk_edge_index(clusters);

    let mut nhs = tight_neighborhoods(&global_edge_ix, opts)
        .filter(|c|
            c.edge_weights.len() <= 42
                && 25 <= c.nodes.len()
                && c.edge_weights.len() == c.base.len() * 3
        )
        .collect::<Vec<_>>();

    nhs.sort_by_key(|c: &Cluster| (
        cmp::Reverse(c.base.len()),
        cmp::Reverse(c.nodes.len()),
        c.edge_weights.len()
    ));

    let mut used_edges = BTreeSet::new();
    let mut res = vec![];
    for c in nhs {
        let has_intersection = c.edge_weights.keys().any(
            |e| used_edges.contains(e)
        );

        if !has_intersection {
            for e in c.edge_weights.keys() {
                used_edges.insert(*e);
            }
            res.push(c);
        }
    }
    res
}
