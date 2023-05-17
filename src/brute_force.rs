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

    pub fn pair_sets(&self) {
        let mut pairs = Vec::new();
        let n = self.links.len() as u32;
        for i in 0..n {
            for j in i+1..n {
                pairs.push((1 << i) | (1 << j));
            }
        }

        let mut res: Vec<BTreeSet<u32>> = Vec::new();
        for _ in &pairs {
            res.push(BTreeSet::new());
        }

        for x in &self.solutions {
            for i in 0..pairs.len() {
                res[i].insert(x & pairs[i]);
            }
        }

        for s in res {
            if s.len() < 4 {
                println!("{:?}", s);
            }
        }
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

        let diff = self.solutions.len() - res.len();
        self.solutions = res;
        diff
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Write;

    #[test]
    fn block_creation() {
        let ts = Triples::pythagorean(17);
        let ts_str: Vec<_> = ts.iter().map(|x| {
            let mut s = String::new();
            let _ = write!(&mut s, "{}", x);
            s
        }).collect();

        assert_eq!(
            ts_str,
            vec![
                "(3, 4, 5)",
                "(5, 12, 13)",
                "(6, 8, 10)",
                "(8, 15, 17)",
                "(9, 12, 15)"
            ]);

        let b = BfBlock::new(&ts);
        assert_eq!(b.links, vec![3, 4, 5, 6, 8, 9, 10, 12, 13, 15, 17]);
        assert_eq!(
            b.masks,
            vec![
                0b00000000111,
                0b00110000100,
                0b00001011000,
                0b11000010000,
                0b01010100000,
            ]);
    }

    #[test]
    fn block_filtering() {
        let mut x = BfBlock {
            links: vec![3,5,7,9,11,13],
            masks: vec![],
            solutions: vec![
                0b0_u32,
            ]
        };

        let y = BfBlock {
            links: vec![2,3,4,5,6,7,8,10],
            masks: vec![],
            solutions: vec![
                0b11_u32,
            ]
        };

        assert_eq!(BfBlock::common_links(&x, &y), vec![3,5,7]);

        x.filter_by(&y);
        assert_eq!(x.solutions, vec![0]);
    }
}
