#![feature(stmt_expr_attributes)]

use std::collections::BTreeMap;

mod triples;
use triples::*;

mod systems;
use systems::*;



fn main() {
    const N: u64 = 7825;
    let py = Triples::pythagorean(N).filter_by_link_weight(2);
    let links = py.links_map();

    let n_7825_2_2 = links.neighbours(N, 2).filter_by_link_weight(2);
    println!(
        "7825[2,2]: {} links, {} triples",
        n_7825_2_2.links().len(),
        n_7825_2_2.len()
    );
    {
        let sys = TripleSystem { triples: n_7825_2_2 };
        let res = sys.eval();
        println!("{}: {}", 7825, res.len());
    }


    // let mut res = BTreeMap::new();
    for (l, _) in links.iter() {
        let sub = links.neighbours(*l, 3).filter_by_link_weight(3);
        if sub.links().len() == 27 {
            // res.insert(l, sub);

            let sys = TripleSystem { triples: sub };
            let res = sys.eval();
            println!("{}: {}", l, res.len());
        }
    }
}
