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
        if b.link_set().len() == 27 {
            blocks_2.insert(b);
        }
    }
    println!("{} blocks of width 2-2-27 found", blocks_2.len());

    for b in blocks_2.iter().take(2) {
        let xxx = BfBlock::new(&b);
        println!("{}", xxx);
        xxx.pair_sets();
    }

    let mut blocks_3 = BTreeSet::new();
    for (l, _) in links.iter() {
        let b = links.neighbours(*l, 3).filter_by_link_weight(3);
        if b.link_set().len() == 27 {
            blocks_3.insert(b);
        }
    }
    println!("{} blocks of width 3-3-27 found", blocks_3.len());

    for b in blocks_3.iter().take(2) {
        let xxx = BfBlock::new(&b);
        println!("{}", xxx);
        xxx.pair_sets();
    }

    let mut blocks: Vec<_> = blocks_2.union(&blocks_3)
        .map(|x| BfBlock::new(&x))
        .collect();
    println!("{} total blocks without duplicates", blocks.len());

    for i in 0..blocks.len() {
        let (ba, other_blocks) = {
            let (xs, y_ys) = blocks.split_at_mut(i);
            let (y, ys) = y_ys.split_at_mut(1);
            (&mut y[0], xs.iter().chain(ys.iter()))
        };

        println!("{}", ba);
        for bb in other_blocks {
            if BfBlock::common_links(ba, bb).len() > 7 {
                let res = ba.filter_by(bb);
                if res > 0 {
                    println!("{}", res);
                }
            }
        }
    }
}
