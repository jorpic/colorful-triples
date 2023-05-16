#![feature(stmt_expr_attributes)]

use std::collections::BTreeSet;

mod triples;
use triples::*;

mod brute_force;
use brute_force::*;



fn main() {
    const N: u64 = 7825;
    let py = Triples::pythagorean(N).filter_by_link_weight(2);
    let links = py.links_map();
    assert_eq!(py.len(), 7336);
    assert_eq!(links.len(), 3745);

    let mut blocks_2 = BTreeSet::new();
    for (l, _) in links.iter() {
        let b = links.neighbours(*l, 2).filter_by_link_weight(2);
        if b.links().len() == 27 {
            blocks_2.insert(b);
        }
    }
    println!("{} blocks of width 2-2-27 found", blocks_2.len());

    let mut blocks_3 = BTreeSet::new();
    for (l, _) in links.iter() {
        let b = links.neighbours(*l, 3).filter_by_link_weight(3);
        if b.links().len() == 27 {
            blocks_3.insert(b);
        }
    }
    println!("{} blocks of width 3-3-27 found", blocks_3.len());

    let blocks: Vec<_> = blocks_2.union(&blocks_3).collect();
    println!("{} total blocks without duplicates", blocks.len());

    let x7825 = links.neighbours(7825, 2).filter_by_link_weight(2);
    let l7825 = x7825.link_set();
    let mut b7825 = BfBlock::new(&x7825);

    for x in &blocks {
        if l7825.intersection(&x.link_set()).count() > 5 {
            let b = BfBlock::new(&x);
            println!("{}", b);
            let res = b7825.filter_by(&b);
            println!("{}", res);
        }
    }
}
