#![feature(stmt_expr_attributes)]

use std::collections::BTreeSet;

mod triples;
use triples::*;

mod brute_force;
use brute_force::*;


const N: u64 = 7825;

fn main() {
    experiment1();
    experiment2();
}

fn get_neighbors(
    links: &LinksMap,
    depth: usize, weight: usize, link_size: usize)
    -> BTreeSet<Triples>
{
    let mut blocks = BTreeSet::new();
    for (l, _) in links.iter() {
        let b = links.neighbours(*l, depth).filter_by_link_weight(weight);
        if b.link_set().len() == link_size {
            blocks.insert(b);
        }
    }
    println!("{} blocks of width 2-2-27 found", blocks.len());
    blocks
}

fn experiment1() {
    let py = Triples::pythagorean(N).filter_by_link_weight(2);
    let links = py.links_map();

    let blocks_2 = get_neighbors(&links, 2, 2, 27);
    let blocks_3 = get_neighbors(&links, 3, 3, 27);
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

fn experiment2() {
    let py = Triples::pythagorean(N).filter_by_link_weight(2);
    let links = py.links_map();

    let blocks_2 = get_neighbors(&links, 2, 2, 27);
    for b in blocks_2.iter().take(2) {
        let xxx = BfBlock::new(&b);
        println!("{}", xxx);
        xxx.pair_sets();
    }

    let blocks_3 = get_neighbors(&links, 3, 3, 27);
    for b in blocks_3.iter().take(2) {
        let xxx = BfBlock::new(&b);
        println!("{}", xxx);
        xxx.pair_sets();
    }
}
