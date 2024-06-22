use std::collections::BTreeMap;
use std::cmp::Reverse;

mod triples;
use triples::*;

mod brute_force;
use brute_force::*;

mod hgraph;
use hgraph::*;

fn main() -> anyhow::Result<()> {
    let triples = pythagorean_triples(7825);

    let hgraph: Vec<_> = triples.iter().map(Cluster::singleton).collect();

    let hgraph = drop_weak_nodes(hgraph, 2);

    let global_edge_ix = mk_edge_index(&hgraph);
    let global_edge_weights = mk_edge_weights(&hgraph);
    let internal_edges = |c: &Cluster| {
        c.edge_weights
            .iter()
            .filter(|(e,w)| global_edge_weights.get(e).unwrap() == *w)
            .count()
    };

    let sort_key = |c: &Cluster| (
        internal_edges(c),
        c.edge_weights.len(),
        c.nodes.len());

    let mut nhs = tight_neighborhoods(&global_edge_ix, 3, 3)
        .filter(|c| c.edge_weights.len() < 45)
        .map(|c| (sort_key(&c), c))
        .collect::<Vec<_>>();

    nhs.sort_by_key(|c| Reverse(c.0));

    for n in nhs {
        println!(
            "int_edges={} triples={} edges={}",
            n.0.0,
            n.0.1,
            n.0.2,
        );
    }

    // serde_json::to_writer(std::io::stdout(), &nhs)?;
    // serde_json::to_writer(std::io::stdout(), &step1)?;

    Ok(())
}
