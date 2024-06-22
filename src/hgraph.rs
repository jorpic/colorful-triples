use serde::Serialize;
use std::collections::{btree_map, btree_set};
use std::collections::{BTreeMap, BTreeSet};

pub type Edge = u16;
pub type Triple = [Edge; 3];

pub trait Node {
    type IterEdges<'a>: IntoIterator<Item = Edge>
    where
        Self: 'a;
    type IterTriples<'a>: IntoIterator<Item = Triple>
    where
        Self: 'a;

    /// Iterates hyperedges in ascending order of their Id.
    fn edges(&self) -> Self::IterEdges<'_>;

    /// Iterates triples in ascending order.
    fn triples(&self) -> Self::IterTriples<'_>;
}

impl Node for Triple {
    type IterEdges<'a> = Triple;
    type IterTriples<'a> = [Triple; 1];

    fn edges(&self) -> Self::IterEdges<'_> {
        *self
    }

    fn triples(&self) -> Self::IterTriples<'_> {
        [*self; 1]
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Cluster {
    nodes: BTreeSet<Triple>,
    edge_weights: BTreeMap<Edge, usize>,
}

impl Node for Cluster {
    type IterEdges<'a> = std::iter::Cloned<btree_map::Keys<'a, Edge, usize>>;
    type IterTriples<'a> = std::iter::Cloned<btree_set::Iter<'a, Triple>>;

    fn edges(&self) -> Self::IterEdges<'_> {
        self.edge_weights.keys().cloned()
    }

    fn triples(&self) -> Self::IterTriples<'_> {
        self.nodes.iter().cloned()
    }
}

type ClusterId = Triple;

impl Cluster {
    fn id(&self) -> ClusterId {
        *self.nodes.first().unwrap()
    }

    pub fn singleton(t: &Triple) -> Self {
        Cluster::from_triples([t])
    }

    pub fn from_triples<'a, T>(triples: T) -> Self
    where
        T: IntoIterator<Item = &'a Triple>,
    {
        let mut res = Cluster::default();
        res.add_triples(triples);
        res
    }

    fn add_triples<'a, T>(&mut self, triples: T)
    where
        T: IntoIterator<Item = &'a Triple>,
    {
        for triple in triples {
            for edge in triple.edges() {
                self.edge_weights
                    .entry(edge)
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
    I: IntoIterator<Item = &'a N>,
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

pub fn mk_edge_weights<'a, N, I>(nodes: I) -> BTreeMap<Edge, usize>
where
    N: Node + 'a,
    I: IntoIterator<Item = &'a N>,
{
    let mut res = BTreeMap::new();
    for node in nodes {
        for edge in node.edges() {
            res.entry(edge).and_modify(|x| *x = &*x + 1).or_insert(1);
        }
    }
    res
}

pub fn tight_neighborhoods<'a>(
    edge_ix: &'a EdgeIx<&'a Cluster>,
    width: usize,
    min_weight: usize,
) -> impl Iterator<Item = Cluster> + 'a {
    edge_ix
        .keys()
        .map(move |edge| edge_neighborhood(*edge, edge_ix, width))
        .filter_map(move |cluster| {
            let triples = drop_weak_nodes(cluster.triples(), min_weight);
            if triples.is_empty() {
                None
            } else {
                Some(Cluster::from_triples(&triples))
            }
        })
}

pub fn edge_neighborhood(center: Edge, edge_ix: &EdgeIx<&Cluster>, width: usize) -> Cluster {
    let mut subgraph_nodes = BTreeSet::new();
    let mut subgraph_edges = BTreeSet::new();
    let mut prev_edges = BTreeSet::new();
    prev_edges.insert(center);
    let mut new_edges = BTreeSet::new();

    for _w in 0..width {
        for e in &prev_edges {
            for n in edge_ix.get(e).unwrap() {
                if subgraph_nodes.insert(n) {
                    n.edges().for_each(|new_edge| {
                        let is_new_edge = e != &new_edge
                            && !prev_edges.contains(&new_edge)
                            && !subgraph_edges.contains(&new_edge);
                        if is_new_edge {
                            new_edges.insert(new_edge);
                        }
                    })
                }
            }
        }

        subgraph_edges.append(&mut prev_edges); // prev_edges is empty now
        prev_edges.append(&mut new_edges); // new_edges is empty now
    }

    Cluster::from_triples(subgraph_nodes.iter().flat_map(|n| &n.nodes))
}

// Weak node is a node that is connected to a weak edge.
// Weak edge is an edge that connects < min_weight nodes.
pub fn drop_weak_nodes<N, I>(graph: I, min_weight: usize) -> Vec<N>
where
    N: Node,
    I: IntoIterator<Item = N>,
{
    let mut res: Vec<_> = graph.into_iter().collect();
    loop {
        let edge_weights = mk_edge_weights(&res);
        let prev_len = res.len();
        res.retain(|node| {
            node.edges()
                .into_iter()
                .all(|edge| edge_weights.get(&edge).unwrap() >= &min_weight)
        });

        if res.len() == prev_len {
            break;
        }
    }

    res
}

pub fn join_weak_nodes(clusters: &[Cluster], min_weight: usize, max_width: usize) -> Vec<Cluster> {
    let mut clusters = clusters.to_vec();
    loop {
        let new_clusters = join_weak_nodes_single_pass(&clusters, min_weight, max_width);
        if new_clusters.len() == clusters.len() {
            return new_clusters;
        }
        clusters = new_clusters;
    }
}

fn join_weak_nodes_single_pass(
    clusters: &[Cluster],
    min_weight: usize,
    max_width: usize,
) -> Vec<Cluster> {
    let edge_index = mk_edge_index(clusters);
    let weak_edges = edge_index
        .values()
        .filter(|cs| 1 < cs.len() && cs.len() < min_weight);

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
            new_cluster.merge_with(c);
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
        let singleton_clusters: Vec<_> = triples.iter().map(Cluster::singleton).collect();
        let res = join_weak_edges(&singleton_clusters, 3, 42);
        let res = join_weak_edges(&res, 4, 42);
        //let e_ix = mk_edge_index(&res);
        // assert_eq!(0, res.iter().filter(|ts| ts.edges().count() == 3).count());
        assert_eq!(1236, res.len());
    }
}
