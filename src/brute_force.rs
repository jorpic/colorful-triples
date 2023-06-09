use std::collections::BTreeSet;
use std::fmt;

use crate::triples::*;

#[derive(Clone)]
pub struct BfBlock {
    links: Vec<Link>,
    masks: Vec<u64>,
    pub solutions: Vec<u64>
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
            let mut mask: u64 = 0;
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
        let n = 2_u64.pow(links.len() as u32);
        for x in 0..n {
            if masks.iter().all(|m| x & m != *m && x & m != 0) {
                solutions.push(x);
            }
        }

        BfBlock { links, masks, solutions }
    }

    pub fn len(&self) -> usize {
        self.solutions.len()
    }

    pub fn pair_sets(&self) {
        let mut pairs = Vec::new();
        let n = self.links.len() as u64;
        for i in 0..n {
            for j in i+1..n {
                for k in j+1..n {
                    pairs.push((1 << i) | (1 << j) | (1 << k));
                }
            }
        }

        let mut res: Vec<BTreeSet<u64>> = Vec::new();
        for _ in &pairs {
            res.push(BTreeSet::new());
        }

        for x in &self.solutions {
            for i in 0..pairs.len() {
                res[i].insert(x & pairs[i]);
            }
        }

        for i in 0..res.len() {
            let s = &res[i];
            if s.len() < 8 {
                println!("{:#012b}, {:?}", pairs[i], s);
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

    pub fn join(xs: &BfBlock, ys: &BfBlock) -> BfBlock {
        let zs_links: Vec<Link> =
            BTreeSet::from_iter(xs.links.iter().copied())
            .union(&BTreeSet::from_iter(ys.links.iter().copied()))
            .copied()
            .collect();

        assert!(zs_links.len() <= 64);

        let common_links = BfBlock::common_links(&xs, &ys);
        let x_to_y = BfBlock::build_translation_map(&xs.links, &ys.links);
        let x_to_z: Vec<usize> = BfBlock::build_translation_map(&xs.links, &zs_links)
            .iter()
            .map(|m| m.unwrap())
            .collect();
        let y_to_z: Vec<usize> = BfBlock::build_translation_map(&ys.links, &zs_links)
            .iter()
            .map(|m| m.unwrap())
            .collect();

        // this mask selects only common links in ys
        let mut y_common_mask = 0;
        for (i, y) in ys.links.iter().enumerate() {
            if common_links.binary_search(&y).is_ok() {
                y_common_mask |= 1 << i;
            }
        }

        let mut zs: Vec<u64> = vec![];
        for x in &xs.solutions {
            // common links translated from x to y
            let mut y_partial_val = 0_u64;
            for (i, mj) in x_to_y.iter().enumerate() {
                if let Some(j) = mj {
                    y_partial_val |= x.get_bit(i) << j;
                }
            }

            // x links translated from x to z
            let mut z_partial_val = 0_u64;
            for (i, j) in x_to_z.iter().enumerate() {
                z_partial_val |= x.get_bit(i) << j;
            }

            for y in &ys.solutions {
                if y & y_common_mask == y_partial_val & y_common_mask {
                    let mut z = z_partial_val;
                    for (i, j) in y_to_z.iter().enumerate() {
                        z |= y.get_bit(i) << j;
                    }
                    zs.push(z);
                }
            }
        }

        BfBlock { links: zs_links, masks: vec![], solutions: zs }
    }


    pub fn build_translation_map(xs: &[Link], ys: &[Link])
        -> Vec<Option<usize>>
    {
        xs.iter()
            .map(|l| ys.binary_search(&l).ok())
            .collect()
    }

    pub fn just_one_solution(&mut self, i: usize) {
        let x = self.solutions[i];
        self.solutions = vec![x];
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

// TODO:: add SortedVec wrapper


trait GetBit {
    fn get_bit(&self, i: usize) -> Self;
}

impl GetBit for u64 {
    fn get_bit(&self, i: usize) -> Self {
        (*self & (1 << i)) >> i
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
                0b0_u64,
            ]
        };

        let y = BfBlock {
            links: vec![2,3,4,5,6,7,8,10],
            masks: vec![],
            solutions: vec![
                0b11_u64,
            ]
        };

        assert_eq!(BfBlock::common_links(&x, &y), vec![3,5,7]);

        x.filter_by(&y);
        assert_eq!(x.solutions, vec![0]);
    }

    #[test]
    fn translation_map() {
        let xs = vec![2, 10, 20, 30];
        let ys = vec![1, 2, 20, 40];
        let zs = vec![1, 2, 3, 10, 20, 30, 40];
        let x_to_y = BfBlock::build_translation_map(&xs, &ys);
        assert_eq!(x_to_y, vec![Some(1), None, Some(2), None]);
        let y_to_x = BfBlock::build_translation_map(&ys, &xs);
        assert_eq!(y_to_x, vec![None, Some(0), Some(2), None]);
        let y_to_z = BfBlock::build_translation_map(&ys, &zs);
        assert_eq!(y_to_z, vec![Some(0), Some(1), Some(4), Some(6)]);
    }

    #[test]
    fn bits() {
        let x = 0b01011101_u64;
        assert_eq!(x.get_bit(0), 1);
        assert_eq!(x.get_bit(1), 0);
        assert_eq!(x.get_bit(2), 1);
        assert_eq!(x.get_bit(3), 1);
        assert_eq!(x.get_bit(4), 1);
    }

    #[test]
    fn join() {
        let xs = BfBlock {
            links: vec![1,2,10,20,100,200],
            masks: vec![],
            solutions: vec![
                0b01_11_11_u64, // filtered out
                0b10_11_11_u64, // A
                0b11_11_11_u64, // B
            ]
        };

        let ys = BfBlock {
            links: vec![2,3,20,30,200,300],
            masks: vec![],
            solutions: vec![
                0b11_11_11_u64,
                0b11_01_01_u64,
                0b01_01_01_u64,
            ]
        };

        let common_links = BfBlock::common_links(&xs, &ys);
        let zs = BfBlock::join(&xs, &ys);

        assert_eq!(common_links, vec![2,20,200]);
        assert_eq!(zs.links, vec![1,2,3,10,20,30,100,200,300]);
        assert!(zs.solutions.len() <= xs.solutions.len() * ys.solutions.len());
        assert_eq!(
            zs.solutions,
            vec![
                0b110_111_111_u64, // A
                0b110_011_011_u64, // A
                0b010_011_011_u64, // A

                0b111_111_111_u64, // B
                0b111_011_011_u64, // B
                0b011_011_011_u64, // B
            ]);
    }
}
