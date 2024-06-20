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

    let hgraph = drop_weak_nodes(&hgraph, 2);
    let hgraph = join_weak_nodes(&hgraph, 3, 42);

    let edge_weights: BTreeMap<_, _> = mk_edge_index(&hgraph)
        .into_iter()
        .map(|(edge, nodes)| (edge, nodes.len()))
        .collect();

    serde_json::to_writer(std::io::stdout(), &edge_weights)?;
    // serde_json::to_writer(std::io::stdout(), &step1)?;

    Ok(())
}
