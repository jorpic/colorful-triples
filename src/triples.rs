use std::collections::{BTreeSet, BTreeMap};

pub type Link = u16;
pub type Triple = [Link; 3];
pub type Graph = Vec<Triple>;


pub fn pythagorean_triples(n: u64) -> Vec<Triple> {
    let mut res = Vec::new();
    for a in 2..n {
        let aa = a*a;
        for b in a..n {
            let ab = aa + b*b;
            let c = (ab as f64).sqrt() as u64;
            if c <= n && c*c == ab {
                res.push([a as Link, b as Link, c as Link])
            }
        }
    }
    res
}

fn inc_key(map: &mut BTreeMap<Link, usize>, k: &Link) {
    if let Some(x) = map.get_mut(&k) {
        *x = *x + 1;
    } else {
        map.insert(*k, 1);
    }
}

pub fn links(triples: &[Triple]) -> BTreeSet<Link> {
    let mut links = BTreeSet::new();
    for t in triples {
        for l in t {
            links.insert(*l);
        }
    }
    links
}


pub fn link_weights(triples: &[Triple]) -> BTreeMap<Link, usize> {
    let mut res = BTreeMap::new();
    for t in triples {
        t.iter().for_each(|l| inc_key(&mut res, &l));
    }
    res
}

fn link_index(triples: &[Triple]) -> BTreeMap<Link, Vec<Triple>> {
    let mut res = BTreeMap::<Link, Vec<Triple>>::new();
    for t in triples {
        t.iter().for_each(|l| {
            if let Some(x) = res.get_mut(&l) {
                x.push(*t);
            } else {
                res.insert(*l, vec![*t]);
            }
        });
    }
    res
}

pub fn drop_weak_links(triples: &[Triple], min_weight: usize) -> Vec<Triple> {
    let mut ts: Vec<Triple> = triples.to_vec();
    loop {
        let lw = link_weights(&ts);
        let prev_len = ts.len();
        ts = ts.into_iter()
            .filter(
                |t| t.iter().all(|l| lw.get(&l).unwrap() >= &min_weight)
            ).collect();

        if ts.len() == prev_len {
            break;
        }
    }
    ts
}

pub fn all_subgraphs(triples: &[Triple], depth: usize) -> Vec<Graph> {
    let link_ix = link_index(triples);
    let mut res = Vec::new();

    for root in link_ix.keys() {
        let mut subgraph_triples = BTreeSet::new();
        let mut subgraph_links = BTreeSet::new();
        let mut prev_links = BTreeSet::new();
        prev_links.insert(root);
        let mut new_links = BTreeSet::new();

        for _d in 0..depth {
            for l in &prev_links {
                for t in link_ix.get(&l).unwrap() {
                    if subgraph_triples.insert(*t) {
                        t.iter().for_each(|nl| {
                            let is_new_link = l != &nl
                                && !prev_links.contains(&nl)
                                && !subgraph_links.contains(&nl);
                            if is_new_link {
                                new_links.insert(nl);
                            }
                        })
                    }
                }
            }

            subgraph_links.append(&mut prev_links); // prev_links is empty now
            prev_links.append(&mut new_links); // new_links is empty now
        }

        res.push(subgraph_triples.iter().cloned().collect());
    }
    res
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gen_triples() {
        let py = pythagorean_triples(7825);
        assert_eq!(py.len(), 9472);
    }
}
