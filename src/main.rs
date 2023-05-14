#![feature(stmt_expr_attributes)]

use std::collections::BTreeMap;

mod triples;
use triples::*;

mod systems;
use systems::*;



fn main() {
    let py = Triples::pythagorean(7825).filter_by_link_weight(2);
    let links = py.links_map();
    let mut res = BTreeMap::new();
    for (l, _) in links.iter() {
        let sub = links.neighbours(*l, 3).filter_by_link_weight(3);
        if sub.len() > 0 {
            res.insert(l, sub);
        }
    }
    assert_eq!(res.len(), 892);
}
