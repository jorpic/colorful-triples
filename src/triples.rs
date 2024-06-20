pub type Triple = [u16; 3];

pub fn pythagorean_triples(n: u64) -> Vec<Triple> {
    let mut res = Vec::new();
    for a in 2..n {
        let aa = a * a;
        for b in a..n {
            let ab = aa + b * b;
            let c = (ab as f64).sqrt() as u64;
            if c <= n && c * c == ab {
                res.push([a as u16, b as u16, c as u16])
            }
        }
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
