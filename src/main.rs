#![feature(stmt_expr_attributes)]
#![allow(dead_code)]


use std::collections::BTreeSet;

mod triples;
use triples::*;

mod brute_force;
use brute_force::*;


const N: u64 = 7825;

fn main() {
    // experiment1();
    // experiment2();
    experiment3();
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

fn experiment3() {
    let py = Triples::pythagorean(N).filter_by_link_weight(2);
    let links = py.links_map();

    let root_triples = links.neighbours(N, 2).filter_by_link_weight(2);
    let mut root_links = root_triples.link_set();

    let mut root_block = BfBlock::new(&root_triples);
    root_block.just_one_solution(1234);
    println!("{}", &root_block);

    let mut triples_set = BTreeSet::new();
    triples_set.insert(root_triples);

    let mut blocks = Vec::new();
    blocks.push(root_block);

    println!("found {} root links", root_links.len());
    add_blocks(&root_links, &links, &mut triples_set, &mut blocks);

    for layer in 1..3 {
        let mut new_links = BTreeSet::new();
        for t in &triples_set {
            for l in t.link_set() {
                new_links.insert(l);
            }
        }
        for l in root_links {
            new_links.remove(&l);
        }
        println!("found {} layer-{} links", new_links.len(), layer);
        add_blocks(&new_links, &links, &mut triples_set, &mut blocks);

        root_links = new_links;
    }

    // /// Merge
    loop {
        println!("Join blocks");
        let res = join_blocks(&mut blocks);
        if res == 0 {
            break;
        } else {
            println!("deleted {} partial solutions", res);
        }
    }
}

fn add_blocks(
    ls: &BTreeSet<Link>,
    links: &LinksMap,
    triples_set: &mut BTreeSet<Triples>,
    blocks: &mut Vec<BfBlock>
) {
    for l in ls {
        let l_2_2 = links.neighbours(*l, 2).filter_by_link_weight(2);
        let ll = l_2_2.link_set().len();
        if ll > 15 && ll < 30 && !triples_set.contains(&l_2_2) {
            let block = BfBlock::new(&l_2_2);
            println!("link={}, {}", l, block);
            blocks.push(block);
            triples_set.insert(l_2_2);
        }
    }
}

fn join_blocks(blocks: &mut Vec<BfBlock>) -> usize {
    let mut deleted = 0;

    for i in 0..blocks.len() {
        let (ba, other_blocks) = {
            let (xs, y_ys) = blocks.split_at_mut(i);
            let (y, ys) = y_ys.split_at_mut(1);
            (&mut y[0], xs.iter().chain(ys.iter()))
        };

        println!("{}", ba);
        for bb in other_blocks {
            if BfBlock::common_links(ba, bb).len() > 5 {
                deleted += ba.filter_by(bb);
            }
        }
    }

    deleted
}
