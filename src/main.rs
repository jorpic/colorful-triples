// #![feature(portable_simd)]

use std::collections::{BTreeSet, BTreeMap};
//use std::fs::File;
//use std::io::{stdout, BufWriter, Write};

mod alg;
// mod brute_force;
mod triples;
mod types;

use alg::edge_index::{mk_edge_index, mk_edge_weights, EdgeIx};
use alg::weak_edges::join_weak_edges;
use alg::weak_nodes::drop_weak_nodes;

use triples::pythagorean_triples;
use types::*;

fn main() -> anyhow::Result<()> {
    let triples = pythagorean_triples(7825);
    println!("all triples = {}", triples.len());

    let triples = drop_weak_nodes(triples, 2);
    println!("without pendants = {}", triples.len());

    let triple_set: BTreeSet<_> = triples.iter().collect();

    let mut xy_ix = BTreeMap::new();
    for t in &triples {
        xy_ix.insert((t[0], t[1]), t[2]);
    }

    // ---------------------

    let k4s = get_4knots(&triples);
    println!("got {} 4-knots", k4s.len());

    for i in 0..k4s.len() {
        for j in i+1..k4s.len() {
            for k in j+1..k4s.len() {
                let x = [k4s[i][0][0], k4s[j][0][0], k4s[k][0][0]];
                let z = [k4s[i][3][2], k4s[j][3][2], k4s[k][3][2]];

                if triple_set.contains(&x) && triple_set.contains(&z) {
                    println!("{:?} {:?} {:?}", (i,j,k), x, z);
                }
            }
        }
    }


    // ---------------------

    let k6s = get_6knots(&triples);
    println!("got {} 6-knots", k6s.len());

    let used_triples: BTreeSet<_> = k6s.iter().flatten().collect();
    println!("{} triples used", used_triples.len());


    let mut k6t0_ix: BTreeMap<Triple, Vec<usize>> = BTreeMap::new();
    for i in 0..k6s.len() {
        k6t0_ix.entry(k6s[i][0])
            .and_modify(|x| x.push(i))
            .or_insert(vec![i]);
    }

    for (t0, v) in k6t0_ix {
        // println!("{:?} {:?}", t0, v);
        for i in 0..v.len() {
            for j in i+1..v.len() {
                let [_, a1, a2, a3, _, _] = k6s[v[i]];
                let [_, b1, b2, b3, _, _] = k6s[v[j]];

                let Some(x) = xy_ix.get(&(a1[1], b1[2])) else { continue; };
                let Some(xx) = xy_ix.get(&(a1[2], b1[1])) else { continue; };
                if x != xx {
                    continue;
                }

                let Some(y) = xy_ix.get(&(a2[1], b2[2])) else { continue; };
                let Some(yy) = xy_ix.get(&(a2[2], b2[1])) else { continue; };
                if y != yy {
                    continue;
                }

                let Some(z) = xy_ix.get(&(a3[1], b3[2])) else { continue; };
                let Some(zz) = xy_ix.get(&(a3[2], b3[1])) else { continue; };
                if z != zz {
                    continue;
                }

                if triple_set.contains(&[*x, *y, *z]) {
                    println!("{:?} -> {:?}", t0, [x, y, z]);
                }

                //[a1[1], b1[2], x]
                //[a1[2], b1[1], x]
                //[a2[1], b2[2], y]
                //[a2[2], b2[1], y]
                //[a3[1], b3[2], z]
                //[a3[2], b3[1], z]
                //[x, y, z]
            }
        }
    }

    Ok(())
}

type Knot4 = [Triple; 4];
type Knot6 = [Triple; 6];
type Knot18 = [Triple; 18];


fn get_4knots(triples: &[Triple]) -> Vec<Knot4> {
    let triple_set: BTreeSet<_> = triples.iter().collect();

    let mut edge_ix: BTreeMap<Edge, Vec<Triple>> = BTreeMap::new();
    for t in triples {
        edge_ix.entry(t[0])
            .and_modify(|x| x.push(*t))
            .or_insert(vec![*t]);
    }

    let mut xy_ix = BTreeMap::new();
    for t in triples {
        xy_ix.insert((t[0], t[1]), t[2]);
    }

    let mut knots = vec![];
    for (x, v) in edge_ix {
        for i in 0..v.len() {
            let [_, a, b] = v[i];
            for j in i+1..v.len() {
                let [_, c, d] = v[j];
                let Some(y) = xy_ix.get(&(a, d)) else { continue; };
                let Some(z) = xy_ix.get(&(b, c)) else { continue; };
                if y == z {
                    println!("{:?}", [[x, a, b], [x, c, d], [a, d, *y], [b, c,*y]]);
                    knots.push([[x, a, b], [x, c, d], [a, d, *y], [b, c, *y]]);
                }
            }
        }
    }

    knots
}

fn get_6knots(triples: &[Triple]) -> Vec<Knot6> {
    let triple_set: BTreeSet<_> = triples.iter().collect();
    let edge_ix = mk_edge_index(triples);
    let mut knots = vec![];

    for t0 in triples {
        let [a, b, c] = t0;

        for t1 in edge_ix.get(a).unwrap() {
            if t1 <= t0 || t1[0] != *a {
                continue;
            }

            let [_, d, e] = t1;

            for t2 in edge_ix.get(b).unwrap() {
                if t2[0] != *b {
                    continue;
                }

                let [_, f, g] = t2;

                for t3 in edge_ix.get(c).unwrap() {
                    if t3[0] != *c {
                        continue;
                    }

                    let [_, h, x] = t3;
                    let t4 = [*d, *f, *h];
                    let t5 = [*e, *g, *x];

                    let exists = triple_set.contains(&t4)
                        && triple_set.contains(&t5);
                    if exists {
                        knots.push([*t0, *t1, *t2, *t3, t4, t5]);
                    }
                }
            }
        }
    }

    knots
}
