use std::fmt;

use crate::triples::*;


type Var = u16;
type Eqn = Vec<Var>;

pub struct TripleSystem {
    pub triples: Triples
}

impl TripleSystem {
    pub fn eval(&self) -> Vec<u32> {
        let vars: Vec<Link> = self.triples.links().iter().copied().collect();
        // FIXME: panic if too many vars

        let mut masks = Vec::new();
        for t in self.triples.iter() {
            let mut mask: u32 = 0;
            for x in t.iter() {
                if let Ok(i) = vars.binary_search(&x) {
                    mask |= 1 << i;
                } else {
                    panic!("Impossible!");
                }
            }
            masks.push(mask);
        }

        let mut res = Vec::new();
        for x in 0..2_u32.pow(vars.len() as u32) {
            if masks.iter().all(|m| x & m != *m && x & m != 0) {
                res.push(x);
            }
        }

        res
    }
}


#[derive(Debug)]
pub struct EqSystem {
    eqs: Vec<Eqn>
}

impl fmt::Display for EqSystem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "EqSystem {{");
        for eq in &self.eqs {
            writeln!(f, "\t{:?}", eq);
        }
        writeln!(f, "}}")
    }
}


impl EqSystem {
    pub fn to_row_echelon(&mut self) {
        let eqs = &mut self.eqs;
        let len = eqs.len();
        for m in 0..len {
            if eqs[m].len() == 0 {
                continue;
            }

            let mut pivot_j = m;
            let mut pivot_val = eqs[m][0];
            for j in m+1..len {
                if eqs[j].len() > 0 && eqs[j][0] < pivot_val {
                    pivot_j = j;
                    pivot_val = eqs[j][0];
                }
            }

            if pivot_j != m {
                eqs[m] = add_eqs(&eqs[m], &eqs[pivot_j]);
            }

            for j in 0..len {
                if j != m && eqs[j].binary_search(&pivot_val).is_ok() {
                    eqs[j] = add_eqs(&eqs[m], &eqs[j]);
                }
            }
        }
    }

    pub fn vars(&self) -> Vec<Var> {
        unimplemented!("!!")
    }
}

fn add_eqs(xs: &Eqn, ys: &Eqn) -> Eqn {
    let mut res = Vec::new();
    let mut i = 0;
    let mut j = 0;
    while i < xs.len() && j < ys.len() {
        let x = xs[i];
        let y = ys[j];

        if x < y {
            res.push(x);
            i += 1;
        } else if y < x {
            res.push(y);
            j += 1;
        } else { // skip both
            i += 1;
            j += 1;
        }
    }

    // push remaining
    while i < xs.len() {
        res.push(xs[i]);
        i += 1;
    }
    while j < ys.len() {
        res.push(ys[j]);
        j += 1;
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let xs = vec![1,2,30];
        let ys = vec![2,3,40];
        assert_eq!(add_eqs(&xs, &ys), vec![1,3,30,40]);
    }

    #[test]
    fn test_rre_1() {
        let mut sys = EqSystem {
            eqs: vec![
                vec![15, 20, 25],
                vec![15, 36, 39],
                vec![20, 48, 52],
                vec![25, 60, 65],
                vec![36, 48, 60],
                vec![39, 52, 65],
            ]
        };
        sys.to_row_echelon();
        println!("{}", sys);
    }
}
