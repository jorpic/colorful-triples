use std::collections::BTreeSet;
use std::fmt;

use crate::triples::*;

pub struct BfBlock {
    links: Vec<Link>,
    masks: Vec<u32>,
    solutions: Vec<u32>
}

impl fmt::Display for BfBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Width={}, Eqs={}, Len={}",
               self.links.len(),
               self.masks.len(),
               self.solutions.len())
    }
}

impl BfBlock {
    pub fn new(triples: &Triples) -> Self {
        let links: Vec<Link> = triples.links();

        let mut masks = Vec::new();
        for t in triples.iter() {
            let mut mask: u32 = 0;
            for x in t.iter() {
                if let Ok(i) = links.binary_search(&x) {
                    mask |= 1 << i;
                } else {
                    unreachable!();
                }
            }
            masks.push(mask);
        }

        let mut solutions = Vec::new();
        let n = 2_u32.pow(links.len() as u32);
        for x in 0..n {
            if masks.iter().all(|m| x & m != *m && x & m != 0) {
                solutions.push(x);
            }
        }

        BfBlock { links, masks, solutions }
    }

    pub fn filter_by(&mut self, other: &BfBlock) -> usize {
        let cl = BfBlock::common_links(self, other);
        let mut ix = Vec::new();

        for l in &cl {
            let i = self.links.binary_search(l).unwrap();
            let j = other.links.binary_search(l).unwrap();
            ix.push((i, j));
        }

        let xs_mask = ix.iter().fold(0, |m, (i,_)| m | (1 << i));
        let ys_mask = ix.iter().fold(0, |m, (_,j)| m | (1 << j));

        let mut other_part = BTreeSet::new();
        for y in &other.solutions {
            other_part.insert(y & ys_mask);
        }
        println!("cl.len = {}, other_part.len = {}", cl.len(), other_part.len());

        let mut self_part = BTreeSet::new();
        for y in other_part {
            let mut x = 0;
            for (i, j) in &ix {
                if y & (1 << j) != 0 {
                    x |= 1 << i;
                }
            }
            self_part.insert(x);
        }

        let mut res = Vec::new();
        for x in &self.solutions {
            if self_part.contains(&(x & xs_mask)) {
                res.push(*x);
            }
        }

        self.solutions = res;
        self.solutions.len()
    }

    pub fn common_links(xs: &BfBlock, ys: &BfBlock) -> Vec<Link> {
        let mut res = Vec::new();
        for x in &xs.links {
            if ys.links.binary_search(&x).is_ok() {
                res.push(*x);
            }
        }

        res
    }
}
