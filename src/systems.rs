use std::fmt;

type Var = u16;
type Eqn = Vec<Var>;


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

    #[test]
    fn test_rre_2() {
        let mut sys = EqSystem {
            eqs: vec![
                vec![48,64,80],
                vec![48,90,102],
                vec![54,72,90],
                vec![54,240,246],
                vec![64,120,136],
                vec![72,320,328],
                vec![80,150,170],
                vec![90,120,150],
                vec![90,216,234],
                vec![90,400,410],
                vec![90,672,678],
                vec![102,136,170],
                vec![120,288,312],
                vec![120,896,904],
                vec![150,360,390],
                vec![150,1120,1130],
                vec![216,288,360],
                vec![216,960,984],
                vec![234,312,390],
                vec![234,1040,1066],
                vec![240,320,400],
                vec![246,328,410],
                vec![400,960,1040],
                vec![410,984,1066],
                vec![672,896,1120],
                vec![678,904,1130]
            ]
        };
        sys.to_row_echelon();
        println!("{}", sys);
    //
    //
    // "[[48,136,170,960,984,1040,1066],[54,320,328,960,984,1040,1066],[64,136,960,984,1040,1066,1120,1130],[72,320,328],[80,170,1120,1130],[90,960,984,1040,1066],[102,136,170],[120,960,984,1040,1066,1120,1130],[150,1120,1130],[216,960,984],[234,1040,1066],[240,320,960,1040],[246,328,984,1066],[288,390,960,984,1120,1130],[312,390,1040,1066],[360,390,1120,1130],[400,960,1040],[410,984,1066],[672,904,960,984,1040,1066,1130],[678,904,1130],[896,904,960,984,1040,1066,1120,1130]]"
    }
}
