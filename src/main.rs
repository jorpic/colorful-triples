use std::cmp;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::collections::BTreeMap;

mod triples;
use triples::*;

mod brute_force;
use brute_force::*;

mod hgraph;
use hgraph::*;

fn main() -> anyhow::Result<()> {
    let triples = pythagorean_triples(7825);

    let hgraph: Vec<_> = triples.iter().map(Cluster::singleton).collect();

    let mut hgraph = drop_weak_nodes(hgraph, 2);
    let global_edge_weights = mk_edge_weights(&hgraph);

    loop {
        println!("nodes: {}", hgraph.len());

        let Some(best_cluster) = get_tightest_cluster_3(&hgraph) else {
            break;
        };

        println!(
            "mw=3 triples={} edges={} inner_edges={}",
            best_cluster.nodes.len(),
            best_cluster.edge_weights.len(),
            best_cluster.inner_edges(&global_edge_weights),
        );

        //let now = std::time::Instant::now();
        //println!(
        //    "solutions={} elapsed={:.2?}",
        //    brute_force(&best_cluster.triples().collect::<Vec<_>>()),
        //    now.elapsed()
        //);

        hgraph.retain(|c| c.nodes.is_disjoint(&best_cluster.nodes));
        hgraph.push(best_cluster);
    }

    loop {
        hgraph = join_weak_nodes(
            &hgraph,
            &global_edge_weights,
            &JoinNodesOptions {min_edge_weight: 15, max_out_edges: 40});
        println!("nodes: {}", hgraph.len());

        let Some(best_cluster) = get_tightest_cluster_2(&hgraph, &global_edge_weights) else {
            break;
        };

        println!(
            "mw=2 triples={} edges={} inner_edges={}",
            best_cluster.nodes.len(),
            best_cluster.edge_weights.len(),
            best_cluster.inner_edges(&global_edge_weights),
        );

        hgraph.retain(|c| c.nodes.is_disjoint(&best_cluster.nodes));
        hgraph.push(best_cluster);
    }


    // join all .triples() and check

    hgraph = hgraph.into_iter().filter(|c| c.nodes.len() >= 20).collect();
    hgraph.sort_by_key(|c| cmp::Reverse(c.nodes.len()));

    {
        let file = File::create("clusters.json")?;
        let mut writer = BufWriter::new(file);
        serde_json::to_writer(&mut writer, &hgraph)?;
        writer.flush()?;
    }

    //for c in hgraph {
    //    println!(
    //        "triples={} edges={} int_edges={}",
    //        c.nodes.len(),
    //        c.edge_weights.len(),
    //        c.edge_weights
    //            .iter()
    //            .filter(|(e, w)| global_edge_weights.get(e).unwrap() == *w)
    //            .count(),
    //    );
    //}


    Ok(())
}


fn get_tightest_cluster_3(clusters: &[Cluster]) -> Option<Cluster> {
    let global_edge_ix = mk_edge_index(clusters);

    let sort_key = |c: &Cluster| (
        cmp::Reverse(c.nodes.len()),
        c.edge_weights.len()
    );

    let mut nhs = tight_neighborhoods(
        &global_edge_ix,
        &NeighborhoodOptions {width: 3, min_weight: 3}
        )
        // I have tried 42,41,40,39..35
        // and 35 and 41 give best results.
        // FIXME: find best treshold or better heuristic. The goal is to balance bruteforce
        // time and number of free triples left
        .filter(|c| c.edge_weights.len() <= 39)
        .map(|c| (sort_key(&c), c))
        .collect::<Vec<_>>();

    nhs.sort_by_key(|c| c.0);
    nhs.first().map(|c| c.1.clone())
}

fn get_tightest_cluster_2(clusters: &[Cluster], global_weights: &BTreeMap<Edge, usize>) -> Option<Cluster> {
    let global_edge_ix = mk_edge_index(clusters);

    let sort_key = |c: &Cluster| (
        cmp::Reverse(c.nodes.len()),
        c.edge_weights.len()
    );

    let mut nhs = tight_neighborhoods(
        &global_edge_ix,
        &NeighborhoodOptions {width: 3, min_weight: 2}
        )
        .filter(|c| c.outer_edges(global_weights) <= 37 && 20 <= c.nodes.len())
        .map(|c| (sort_key(&c), c))
        .collect::<Vec<_>>();

    nhs.sort_by_key(|c| c.0);
    nhs.first().map(|c| c.1.clone())
}
