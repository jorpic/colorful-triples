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
