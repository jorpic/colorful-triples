use std::fmt;
use std::collections::{BTreeSet, BTreeMap};

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

    pub fn len(&self) -> usize {
        self.0.len()
    }


    fn empty() -> Self {
        Triples(vec![])
    }

    fn singleton(t: Triple) -> Self {
        Triples(vec![t])
    }

    fn push(&mut self, t: Triple) {
        // FIXME: check that Triples are still ordered.
        self.0.push(t);
    }

    pub fn links(&self) -> BTreeSet<u64> {
        let mut set = BTreeSet::new();
        for t in self.0.iter() {
            let tt = t.to_tuple();
            set.insert(tt.0);
            set.insert(tt.1);
            set.insert(tt.2);
        }
        set
    }

    pub fn link_weights(&self) -> BTreeMap<u64, u64> {
        let mut map = BTreeMap::new();
        for t in self.0.iter() {
            let (a, b, c) = t.to_tuple();
            if let Some(ta) = map.get_mut(&a) {
                *ta += 1;
            } else {
                map.insert(a, 1);
            }
            if let Some(tb) = map.get_mut(&b) {
                *tb += 1;
            } else {
                map.insert(b, 1);
            }
            if let Some(tc) = map.get_mut(&c) {
                *tc += 1;
            } else {
                map.insert(c, 1);
            }
        }
        map
    }

    pub fn link_map(&self) -> BTreeMap<u64, Triples> {
        let mut map = BTreeMap::<u64, Triples>::new();
        for t in self.0.iter() {
            let (a, b, c) = t.to_tuple();
            // We iterate over triples in sorted order, so it is safe to call Triple::push().
            if let Some(ta) = map.get_mut(&a) {
                ta.push(*t);
            } else {
                map.insert(a, Triples::singleton(*t));
            }
            if let Some(tb) = map.get_mut(&b) {
                tb.push(*t);
            } else {
                map.insert(b, Triples::singleton(*t));
            }
            if let Some(tc) = map.get_mut(&c) {
                tc.push(*t);
            } else {
                map.insert(c, Triples::singleton(*t));
            }
        }
        map
    }

    pub fn filter_by_link_weight(self, min_weight: u64) -> Self {
        let mut prev = self;
        let mut done = false;

        while !done {
            done = true;
            let link_weight = prev.link_weights();
            let mut res = Triples::empty();

            for t in prev.0.iter() {
                let (a, b, c) = t.to_tuple();
                if *link_weight.get(&a).unwrap_or(&0) < min_weight
                    || *link_weight.get(&b).unwrap_or(&0) < min_weight
                    || *link_weight.get(&c).unwrap_or(&0) < min_weight
                {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn triple_works() {
        let t = (1234, 4567, 7890);
        let tt = Triple::new(t.0, t.1, t.2);
        assert_eq!(tt.to_tuple(), t);
    }

    #[test]
    fn pythagorean() {
        let py = Triples::pythagorean(7825);
        assert_eq!(py.len(), 9472);
        assert_eq!(py.links().len(), 6494);
    }

    #[test]
    fn drop_pendants() {
        let py = Triples::pythagorean(7825);
        assert_eq!(py.len(), 9472);
        let filtered = py.filter_by_link_weight(2);
        assert_eq!(filtered.len(), 7336);
        assert_eq!(filtered.links().len(), 3745);

    }
}
