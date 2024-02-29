use crate::triples::*;

pub fn brute_force(triples: &[Triple]) -> usize {
    let links: Vec<Link> = links(triples).into_iter().collect();
    let mut masks = Vec::new();
    for t in triples {
        let mut mask: u64 = 0;
        for l in t {
            let i = links.binary_search(&l).unwrap();
            mask |= 1 << i;
        }
        masks.push(mask);
    }

    // let mut solutions = Vec::new();
    let mut solutions = 0;
    let n = 2_u64.pow(links.len() as u32);
    for x in 0..n {
        if masks.iter().all(|m| x & m != *m && x & m != 0) {
            // solutions.push(x);
            solutions = solutions + 1;
        }
    }

    solutions
}


pub fn brute_force_selected(triples: &[Triple], selected_links: u64) -> usize {
    use bitintr::{Popcnt, Pext};

    let links: Vec<Link> = links(triples).into_iter().collect();
    let mut masks = Vec::new();
    for t in triples {
        let mut mask: u64 = 0;
        for l in t {
            let i = links.binary_search(&l).unwrap();
            mask |= 1 << i;
        }
        masks.push(mask);
    }

    let mut counts: Vec<usize> = vec![0; 1 << selected_links.popcnt()];
    let n = 2_u64.pow(links.len() as u32);
    for x in 0..n {
        if masks.iter().all(|m| x & m != *m && x & m != 0) {
            counts[x.pext(selected_links) as usize] += 1;
        }
    }

    counts.into_iter().filter(|x| *x == 0).count()
}
