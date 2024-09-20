use crate::types::{Edge, Constraint};
use std::collections::BTreeSet;

// Calculating exact cover is NP-complete.
// Backtracking is too slow, greedy is too dumb.
// We start greedy from different partial covers and choose best of results.
pub fn exact_cover(max_size: usize, cs: &[Constraint]) -> Vec<Constraint> {
    let mut best_cover = vec![];

    for i in 0..cs.len() {
        let a = &cs[i];
        let a_edges = a.edges.iter().cloned().collect::<BTreeSet<_>>();
        for j in (i + 1)..cs.len() {
            let b = &cs[j];
            let b_edges = b.edges.iter().cloned().collect::<BTreeSet<_>>();
            if !b_edges.is_disjoint(&a_edges) {
                continue;
            }
            for k in (j + 1)..cs.len() {
                let c = &cs[k];
                let c_edges = c.edges.iter().cloned().collect::<BTreeSet<_>>();
                if !c_edges.is_disjoint(&a_edges)
                    || !c_edges.is_disjoint(&b_edges)
                {
                    continue;
                }

                let cover = extend_greedily(vec![a.clone(), b.clone(), c.clone()], &cs[i + 1..]);
                if max_size <= cover.len() {
                    return cover;
                }
                if best_cover.len() < cover.len() {
                    best_cover = cover;
                }
            }
        }
    }

    best_cover
}

fn extend_greedily(mut cover: Vec<Constraint>, cs: &[Constraint]) -> Vec<Constraint> {
    let mut used_edges: BTreeSet<Edge> =
        cover.iter().flat_map(|c| &c.edges).cloned().collect();

    for c in cs {
        if c.edges.iter().any(|e| used_edges.contains(e)) {
            continue;
        }
        cover.push(c.clone());
        for e in &c.edges {
            used_edges.insert(*e);
        }
    }

    cover
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_trivial_cover() {
        let N = 20;
        let triples: Vec<Triple> = (0..N as u16)
            .into_iter()
            .map(|i| [3 * i, 3 * i + 1, 3 * i + 2])
            .collect();

        let cover = exact_cover(N, &triples);
        assert_eq!(N, cover.len());
    }

    #[test]
    fn test_trivial_cover_with_extra_nodes() {
        let N = 12;
        let triples = (0..N as u16)
            .into_iter()
            .map(|i| [3 * i, 3 * i + 1, 3 * i + 2]);

        let triples: Vec<Triple> =
            [[3, 7, 9], [1, 4, 5]].into_iter().chain(triples).collect();

        let cover = exact_cover(N, &triples);
        assert_eq!(N, cover.len());
    }

    #[test]
    fn test_real_cover() {
        let triples = vec![
            [324, 432, 540],
            [324, 2175, 2199],
            [432, 2900, 2932],
            [540, 1155, 1275],
            [540, 1296, 1404],
            [540, 1575, 1665],
            [540, 3625, 3665],
            [880, 924, 1276],
            [880, 2340, 2500],
            [924, 2457, 2625],
            [999, 1332, 1665],
            [999, 1932, 2175],
            [1155, 2772, 3003],
            [1275, 3060, 3315],
            [1276, 3393, 3625],
            [1296, 2772, 3060],
            [1296, 3780, 3996],
            [1332, 2576, 2900],
            [1404, 3003, 3315],
            [1404, 4095, 4329],
            [1500, 1575, 2175],
            [1500, 2000, 2500],
            [1575, 2100, 2625],
            [1575, 3780, 4095],
            [1665, 3220, 3625],
            [1665, 3996, 4329],
            [1932, 2576, 3220],
            [2000, 2100, 2900],
            [2175, 2900, 3625],
            [2199, 2932, 3665],
            [2340, 2457, 3393],
            [2500, 2625, 3625],
        ];
        let cover = exact_cover(32, &triples);
        assert_eq!(13, cover.len());
    }
}
