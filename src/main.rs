use std::cmp;
use std::fs::File;
use std::io::{BufWriter, Write, stdout};
use thousands::Separable;

mod triples;
use triples::*;

mod brute_force;
use brute_force::*;

mod hgraph;
use hgraph::*;

fn main() -> anyhow::Result<()> {
    let triples = pythagorean_triples(7825);

    let hgraph: Vec<_> = triples.iter().map(Cluster::singleton).collect();
    let global_edge_weights = mk_edge_weights(&hgraph);
    let mut hgraph = drop_weak_nodes(hgraph, 2);
    let mut clusters: Vec<Cluster> = vec![];

    for min_edge_weight in 3..19 {
        hgraph = join_weak_nodes(
            &hgraph,
            &JoinNodesOptions {min_edge_weight, max_edges: 43});

        println!(
            "nodes after joining with min_edge_weight={}: {}",
            min_edge_weight,
            hgraph.len());
    }

    {
        let mut ts: Vec<Cluster> = vec![];
        for c in hgraph {
            if c.nodes.len() >= 20 {
                clusters.push(c);
            } else {
                for t in &c.nodes {
                    ts.push(Cluster::from_triples([t]));
                }
            }
        }
        hgraph = ts;
    }

    for min_weight in [3, 2] {
        loop {
            let Some(best_cluster) = get_tightest_cluster(
                &hgraph,
                &NeighborhoodOptions {width: 3, min_weight},
            ) else {
                break;
            };

            println!(
                "mw={} triples={} edges={} inner_edges={}",
                min_weight,
                best_cluster.nodes.len(),
                best_cluster.edge_weights.len(),
                best_cluster.inner_edges(&global_edge_weights).len(),
            );

            hgraph.retain(|c| c.nodes.is_disjoint(&best_cluster.nodes));
            clusters.push(best_cluster);
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
            cmp::Reverse(c.inner_edges(&global_edge_weights).len()),
            cmp::Reverse(c.nodes.len())
        )
    );

    {
        let file = File::create("clusters.json")?;
        let mut writer = BufWriter::new(file);
        serde_json::to_writer(&mut writer, &clusters)?;
        writer.flush()?;
    }

    for c in &clusters {
        print!(
            "triples={} edges={} inner_edges={} ",
            c.nodes.len(),
            c.edge_weights.len(),
            c.inner_edges(&global_edge_weights).len(),
        );

        stdout().flush().unwrap();

        let triples = c.triples().collect::<Vec<_>>();
        let inner_edges = c.inner_edges(&global_edge_weights);
        let now = std::time::Instant::now();
        println!(
            "solutions={} elapsed={:.2?}",
            brute_force(&triples, &inner_edges)
                .separate_with_commas(),
            now.elapsed()
        );
    }

    Ok(())
}


fn get_tightest_cluster(clusters: &[Cluster], opts: &NeighborhoodOptions) -> Option<Cluster> {
    let global_edge_ix = mk_edge_index(clusters);

    let sort_key = |c: &Cluster| (
        cmp::Reverse(c.nodes.len()),
        c.edge_weights.len()
    );

    let mut nhs = tight_neighborhoods(&global_edge_ix, opts)
        // I have tried 42,41,40,39..35
        // and 35 and 41 give best results.
        // FIXME: find best treshold or better heuristic. The goal is to balance bruteforce
        // time and number of free triples left
        .filter(|c| c.edge_weights.len() <= 41 && 20 <= c.nodes.len())
        .map(|c| (sort_key(&c), c))
        .collect::<Vec<_>>();

    nhs.sort_by_key(|c| c.0);
    nhs.first().map(|c| c.1.clone())
}
