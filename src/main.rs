use std::collections::VecDeque;

mod triples;
use triples::*;

mod brute_force;
use brute_force::*;

fn main() -> anyhow::Result<()> {
    let mut program: VecDeque<String> =  std::env::args().collect();
    program.pop_front(); // drop program name

    let triples = match program.pop_front().as_deref() {
        Some("triples") => pythagorean_triples(7825),
        Some(cmd) => panic!("Unexpected command: {}", cmd),
        None => return Ok(()),
    };

    let triples = match program.pop_front().as_deref() {
        Some("drop_pendants") => drop_weak_links(&triples, 2),
        Some(cmd) => panic!("Unexpected command: {}", cmd),
        None => triples,
    };

    match program.pop_front().as_deref() {
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
