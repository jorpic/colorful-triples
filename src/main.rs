use std::collections::{VecDeque, BTreeMap};
use serde_json::{Map, Value};

mod triples;
use triples::*;

mod brute_force;
use brute_force::*;

mod hgraph;
use hgraph::*;

fn main() -> anyhow::Result<()> {
    let mut program: VecDeque<String> =  std::env::args().collect();
    program.pop_front(); // drop program name

    let triples = pythagorean_triples(7825);
    let triples = drop_weak_links(&triples, 2);

    match program.pop_front().as_deref() {
        Some("merge_edges") => {
            let singleton_clusters: Vec<_> = triples.iter()
                .map(Cluster::new_singleton)
                .collect();
            let step1 = join_weak_edges(&singleton_clusters, 3, 42);
            let edge_weights: BTreeMap<String, Value> = mk_edge_index(&step1)
                .into_iter()
                .map(|(edge, nodes)| (format!("{edge:?}"), nodes.len().into()))
                .collect();

            let mut map = Map::new();
            map.insert("edge_weights".to_string(), edge_weights.into_iter().collect());
            map.insert(
                "clusters".to_string(),
                serde_json::to_value(
                    step1.iter()
                        .map(|c| c.edges().collect::<Vec<_>>())
                        .filter(|c| c.len() > 3)
                        .collect::<Vec<_>>()
                    ).unwrap());

            let res = Value::Object(map);
            serde_json::to_writer(std::io::stdout(), &res)?;
        },
        Some("get_tight_3_3") => {
            let subs = get_tight_subgraphs(&triples, 3, 3);
            serde_json::to_writer(std::io::stdout(), &subs)?;
        },
        Some("get_tight_3_2") => {
            let subs = get_tight_subgraphs(&triples, 3, 2);
            serde_json::to_writer(std::io::stdout(), &subs)?;
        },
        Some(cmd) => panic!("Unexpected command: {}", cmd),
        None => {
            serde_json::to_writer(std::io::stdout(), &triples)?;
        }
    };

    Ok(())
}
