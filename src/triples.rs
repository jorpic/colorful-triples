use std::collections::{BTreeSet, BTreeMap};
use std::fmt;

pub type Link = u64;

#[derive(Eq, PartialEq, Ord, PartialOrd)]
#[derive(Copy, Clone)]
pub struct Triple(u64);

impl Triple {
    pub fn new(a: u64, b: u64, c: u64) -> Triple {
        assert!(a <= 0xffff && b <= 0xffff && c <= 0xffff, "must fit in u16");
        assert!(a <= b && b <= c, "must be ordered");
        Triple(a | (b << 16) | (c << 32))
    }

    pub fn to_tuple(self) -> (u64, u64, u64) {
        (
            (self.0 & 0xffff),
            (self.0 >> 16) & 0xffff,
            (self.0 >> 32) & 0xffff,
        )
    }

    #[inline(always)]
    pub fn iter(&self) -> impl Iterator<Item = Link> {
        *self
    }
}

impl Iterator for Triple {
    type Item = Link;

    fn next(&mut self) -> Option<Link> {
        if self.0 == 0 {
            None
        } else {
            let res = self.0 & 0xffff;
            self.0 >>= 16;
            Some(res)
        }
    }
}

impl fmt::Display for Triple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.to_tuple())
    }
}


pub struct Triples(Vec<Triple>);

impl Triples {
    // There is no way to construct from vector, so we can be sure that internal representation
    // is always sorted.

    pub fn pythagorean(n: u64) -> Self {
        let mut vec = Vec::with_capacity(n as usize);
        // NB: ranges are not inclusive but this is fine as `a` and
        // `b` should be both strictly less than `n`.
        for a in 2..n {
            for b in a..n {
                let ab = a*a + b*b;
                let c = (ab as f64).sqrt() as u64;
                if c <= n && c*c == ab {
                    vec.push(Triple::new(a, b, c));
                }
            }
        }
        Triples(vec)
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline(always)]
    pub fn iter(&self) -> impl Iterator<Item = &Triple> {
        self.0.iter()
    }

    #[inline(always)]
    fn push(&mut self, t: Triple) {
        // FIXME: check that Triples are still ordered.
        self.0.push(t);
    }

    pub fn links(&self) -> BTreeSet<Link> {
        let mut res = BTreeSet::new();
        for t in self.0.iter() {
            for l in t.iter() {
                res.insert(l);
            }
        }
        res
    }

    pub fn link_weights(&self) -> Vec<usize> {
        let mut res = vec![0; 8000]; // FIXME: magic constant
        for t in self.0.iter() {
            for x in t.iter() {
                res[x as usize] += 1;
            }
        }
        res
    }

    pub fn links_map(&self) -> LinksMap {
        let mut map = BTreeMap::<Link, Triples>::new();
        for t in self.0.iter() {
            for x in t.iter() {
                if let Some(x_triples) = map.get_mut(&x) {
                    // We iterate over triples in sorted order, so it is safe to call
                    // Triples::push().
                    x_triples.push(*t);
                } else {
                    map.insert(x, Triples(vec![*t]));
                }
            }
        }
        LinksMap(map)
    }

    pub fn filter_by_link_weight(self, min_weight: usize) -> Self {
        let mut prev = self;
        let mut done = false;

        while !done {
            done = true;
            let link_weight = prev.link_weights();
            let mut res = Triples(vec![]);

            for t in prev.0.iter() {
                if t.iter().any(|x| link_weight[x as usize] < min_weight) {
                    done = false;
                } else {
                    res.push(*t);
                }
            }

            prev = res;
        }
        prev
    }
}

pub struct LinksMap(BTreeMap<Link, Triples>);

impl LinksMap {
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline(always)]
    pub fn iter(&self) -> impl Iterator<Item = (&Link, &Triples)> {
        self.0.iter()
    }

    pub fn neighbours(&self, root: Link, width: usize) -> Triples {
        let mut res = BTreeSet::<Triple>::new();
        let mut link_traversed = vec![0; 8000]; // FIXME: magic constant

        let add_link_triples =
            #[inline(always)]
            |set: &mut BTreeSet<Triple>, l|
            self.0.get(&l).iter()
                .for_each(|ts| ts.iter()
                    .for_each(|t| { set.insert(*t); }));

        add_link_triples(&mut res, root);
        link_traversed[root as usize] += 1;

        let mut new_triples = BTreeSet::new();
        for _ in 1..width {
            for t in res.iter() {
                for l in t.iter() {
                    if link_traversed[l as usize] == 0 {
                        link_traversed[l as usize] += 1;
                        add_link_triples(&mut new_triples, l);
                    }
                }
            }
            res.append(&mut new_triples); // new_triples is empty after this
        }

        Triples(res.iter().copied().collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const N: u64 = 7825;

    #[test]
    fn triple_works() {
        let t = (1234, 4567, 7890);
        let tt = Triple::new(t.0, t.1, t.2);
        assert_eq!(tt.to_tuple(), t);
    }

    #[test]
    fn gen_triples() {
        let py = Triples::pythagorean(N);
        assert_eq!(py.len(), 9472);
        assert_eq!(py.links().len(), 6494);
    }

    #[test]
    fn drop_pendants() {
        let py = Triples::pythagorean(N);
        assert_eq!(py.len(), 9472);
        let filtered = py.filter_by_link_weight(2);
        assert_eq!(filtered.len(), 7336);
        assert_eq!(filtered.links().len(), 3745);
    }

    #[test]
    fn gen_subgraphs() {
        let py = Triples::pythagorean(N).filter_by_link_weight(2);
        let links = py.links_map();
        let mut count = 0;
        for (l, _) in links.iter() {
            let sub = links.neighbours(*l, 3).filter_by_link_weight(3);
            if sub.len() > 0 {
                count += 1;
            }
        }
        assert_eq!(count, 892);
    }
}
