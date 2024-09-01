use std::collections::BTreeSet;
use std::simd::{cmp::SimdPartialEq, u64x4, Simd};

use crate::types::{Edge, Triple};

fn edges<'a, T>(triples: T) -> BTreeSet<Edge>
where
    T: IntoIterator<Item = &'a Triple>,
{
    triples.into_iter().flatten().cloned().collect()
}

pub fn brute_force(
    triples: &[Triple],
    internal_edges: &BTreeSet<Edge>,
) -> usize {
    let mut edges: Vec<Edge> = edges(triples).into_iter().collect();
    // Internal edges first (at the lower-end of a machine word).
    edges.sort_by_key(|e| (!internal_edges.contains(e), *e));

    let mut masks = Vec::new();
    for t in triples {
        let mut mask: u64 = 0;
        for l in t {
            let i = edges.iter().position(|e| e == l).unwrap();
            mask |= 1 << i;
        }
        masks.push(mask);
    }

    let next_outer = 2_u64.pow(internal_edges.len() as u32);
    let outer_mask = !(next_outer - 1);

    let n: u64 = 2_u64.pow(edges.len() as u32 - 1);
    let mut solutions = 0;
    let mut x = 0;
    while x < n {
        if masks.iter().all(|m| x & m != *m && x & m != 0) {
            solutions += 1;
            x = (x & outer_mask) + next_outer;
        } else {
            x += 1;
        }
    }

    solutions
}

pub fn fast_brute_force(
    base: &BTreeSet<Triple>,
    exts: &BTreeSet<Triple>,
) -> usize {
    // base edges in order as they occur in base triples
    let base_edges: Vec<Edge> = base.iter().flatten().cloned().collect();

    let edges: Vec<Edge> = base_edges
        .iter()
        .chain(edges(exts).difference(&edges(base)))
        .cloned()
        .collect();

    let masks: Vec<u64> = exts
        .iter()
        .map(|t| {
            let mut mask: u64 = 0;
            for l in t {
                let i = edges.iter().position(|e| e == l).unwrap();
                mask |= 1 << i;
            }
            mask
        })
        .collect();

    let vectorized_masks: Vec<u64x4> = masks
        .chunks(u64x4::LEN)
        .map(|chunk| Simd::load_or(chunk, u64x4::splat(chunk[0])))
        .collect();
    let zero_vec = u64x4::splat(0);
    let unit_vec = u64x4::splat(1);

    let n: u64 = 2_u64.pow(edges.len() as u32 - 1);
    let mut x = 0;

    // starting value is 0b..._001_001_001
    for _ in base {
        x = (x << 3) | 0b001;
    }

    let mut solutions = 0;
    while x < n {
        let mut x_vec = u64x4::splat(x);
        for _ in 0..6 {
            let is_solution = vectorized_masks.iter().all(|m_vec| {
                let mv_vec = m_vec & x_vec;
                (mv_vec.simd_ne(*m_vec) & mv_vec.simd_ne(zero_vec)).all()
            });
            if is_solution {
                solutions += 1; //.push(x_vec[0]);
            }
            x_vec += unit_vec;
        }
        x += 8;
        let mut mask = 0b111_000;
        let mut incr = 0b010_000;
        while x & mask == mask {
            x += incr;
            mask <<= 3;
            incr <<= 3;
        }
    }

    solutions
}
