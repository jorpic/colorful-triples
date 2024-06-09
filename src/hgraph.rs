use std::collections::btree_map;
use std::collections::{BTreeSet, BTreeMap, HashMap};

pub type Edge = u16;
pub type Triple = [Edge; 3];

pub trait Node {
    type Iter<'a>: IntoIterator<Item = Edge> where Self: 'a;
    fn edges(&self) -> Self::Iter<'_>;
}

impl Node for Triple {
    type Iter<'a> = Triple;

    fn edges(&self) -> Self::Iter<'_> {
        *self
    }
}


#[derive(PartialEq, Eq, Clone, Default, Hash)]
pub struct Cluster {
    nodes: BTreeSet<Triple>,
    edge_weights: BTreeMap<Edge, usize>,
}

impl Node for Cluster {
    type Iter<'a> =  std::iter::Cloned<btree_map::Keys<'a, Edge, usize>>;

    fn edges(&self) -> Self::Iter<'_> {
        self.edge_weights.keys().cloned()
    }
}


type ClusterId = Triple;

impl Cluster {
    fn id(&self) -> ClusterId {
        *self.nodes.first().unwrap()
    }

    pub fn new_singleton(t: &Triple) -> Self {
        let mut res = Cluster::default();
        res.add_triples([t]);
        res
    }

    fn add_triples<'a, T>(&mut self, triples: T)
        where
            T: IntoIterator<Item = &'a Triple>
    {
        for triple in triples {
            for edge in triple.edges() {
                self.edge_weights.entry(edge)
                    .and_modify(|weight| *weight += 1)
                    .or_insert(1);
            }
            self.nodes.insert(*triple);
        }
    }

    fn merge_with(&mut self, b: &Cluster) {
        self.add_triples(&b.nodes)
    }
}

type EdgeIx<N> = BTreeMap<Edge, Vec<N>>;

// Each vector in the Map values will be ordered if the `nodes` is ordered.
pub fn mk_edge_index<'a, N, I>(nodes: I) -> EdgeIx<&'a N>
    where
        N: Node,
        I: IntoIterator<Item = &'a N>
{
    let mut res: EdgeIx<&'a N> = BTreeMap::new();
    for node in nodes {
        for edge in node.edges() {
            res.entry(edge)
                .and_modify(|x| x.push(node))
                .or_insert(vec![node]);
        }
    }
    res
}

// Weak node is a node that is connected to a weak edge.
// Weak edge is an edge that connects < min_weight nodes.
pub fn drop_weak_nodes<N: Node + Clone>(graph: &[N], min_weight: usize) -> Vec<N>
{
    let mut res = graph.to_vec();
    loop {
        let edge_index: BTreeMap<_, _> = mk_edge_index(&res)
            .into_iter()
            .map(|(edge, nodes)| (edge, nodes.len()))
            .collect();

        let prev_len = res.len();
        res.retain(
            |node| node.edges().into_iter().all(
                |edge| edge_index.get(&edge).unwrap() >= &min_weight)
        );

        if res.len() == prev_len {
            break;
        }
    }

    res
}

pub fn join_weak_edges(mut clusters: &[Cluster], min_weight: usize, max_width: usize) -> Vec<Cluster> {
    let mut clusters = clusters.to_vec();
    loop {
        let new_clusters = join_weak_edges_single_pass(&clusters, min_weight, max_width);
        if new_clusters.len() == clusters.len() {
            return new_clusters;
        }
        clusters = new_clusters;
    }
}


pub fn join_weak_edges_single_pass(
    clusters: &[Cluster],
    min_weight: usize,
    max_width: usize
    ) -> Vec<Cluster>
{
    let edge_index = mk_edge_index(clusters);
    let weak_edges = edge_index.values().filter(|cs| 1 < cs.len() && cs.len() < min_weight);

    let mut merged: BTreeSet<ClusterId> = BTreeSet::new();
    let mut new_clusters: Vec<Cluster> = Vec::new();

    'next_link: for adjacent_clusters in weak_edges {
        // Check if there is a conflict.
        for c in adjacent_clusters {
            if merged.contains(&c.id()) {
                continue 'next_link;
            }
        }

        // No conflicts, safe to merge nodes.
        let mut new_cluster = Cluster::default();
        for c in adjacent_clusters {
            new_cluster.merge_with(&c);
        }
        if new_cluster.edges().count() <= max_width {
            for c in adjacent_clusters {
                merged.insert(c.id());
            }
            new_clusters.push(new_cluster);
        }
    }

    clusters
        .iter()
        .filter(|c| !merged.contains(&c.id()))
        .chain(new_clusters.iter())
        .cloned()
        .collect()
}

// tests
//  - unfold merged clusters into Triples
//  - clusters are disjoint
//  - no triples added or lost
//  - sum of link weights in clusters should match global link weights


#[cfg(test)]
mod tests {
    use super::*;
    use crate::triples::pythagorean_triples;

    #[test]
    fn test_add_triples() {
        let triples = pythagorean_triples(7825);
        let mut all = Cluster::default();
        all.add_triples(&triples);
        assert_eq!(9472, all.nodes.len());
    }

    #[test]
    fn test_mk_edge_index() {
        let triples = pythagorean_triples(7825);
        let e_ix = mk_edge_index(&triples);
        assert!(e_ix.len() > 0);

        let triple_set = BTreeSet::from_iter(triples);
        let e_ix = mk_edge_index(&triple_set);
        assert!(e_ix.len() > 0);
    }

    #[test]
    fn test_drop_weak_nodes() {
        let triples = pythagorean_triples(7825);
        let e_ix = mk_edge_index(&triples);
        assert_eq!(2239, e_ix.values().filter(|ts| ts.len() == 1).count());

        let strong_triples = drop_weak_nodes(&triples, 2);
        let e_ix = mk_edge_index(&strong_triples);
        assert_eq!(0, e_ix.values().filter(|ts| ts.len() == 1).count());
    }

    #[test]
    fn test_join_weak_edges() {
        let triples = pythagorean_triples(7825);
        let triples = drop_weak_nodes(&triples, 2);
        let singleton_clusters: Vec<_> = triples.iter()
            .map(Cluster::new_singleton)
            .collect();
        let res = join_weak_edges(&singleton_clusters, 3, 42);
        let res = join_weak_edges(&res, 4, 42);
        //let e_ix = mk_edge_index(&res);
        // assert_eq!(0, res.iter().filter(|ts| ts.edges().count() == 3).count());
        assert_eq!(1236, res.len());
    }
}
