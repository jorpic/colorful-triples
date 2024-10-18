
export function pythagoreanTriples(n) {
  const res = [];
  for(let a = 2; a < n-1; a++) {
    const aa = a * a;
    for(let b = a; b < n; b++) {
      const ab = aa + b*b;
      const c = Math.floor(Math.sqrt(ab));
      if(c <= n && c*c == ab) {
        res.push([a,b,c]);
      }
    }
  }
  return res;
}

export function linkSet(nodes) {
  return new Set(nodes.flat());
}

export function groupByEdges(ts) {
  const ls = new Map();
  ts.forEach(xs =>
    xs.forEach(x =>
      ls.has(x) ? ls.get(x).push(xs) : ls.set(x, [xs])
    )
  );
  return ls;
}

export function filterByLinkWeight(ts, {minWeight}) {
  ts = new Set(ts);
  while(true) {
    let deleted = 0;
    groupByEdges(ts).forEach((xs, _) => {
      if (xs.length < minWeight) {
        xs.forEach(x => ts.delete(x));
        deleted += 1;
      }
    });
    if (deleted == 0) return [...ts];
  }
}

export function mkTriples(N) {
    // const all = pythagoreanTriples(N);
    const all = filterByLinkWeight(
      pythagoreanTriples(N),
      {minWeight: 2}
    );

    const by_edge = {};
    for(let t of all) {
        for(let e of t) {
            if(e in by_edge) {
                by_edge[e].push(t);
            } else {
                by_edge[e] = [t];
            }
        }
    }

    const by_first_edge = {};
    for(let t of all) {
        const e = t[0];
        if(e in by_first_edge) {
            by_first_edge[e].push(t);
        } else {
            by_first_edge[e] = [t];
        }
    }

    const is_valid = (t) => {
        const ts = by_first_edge[t[0]];
        return ts != undefined
            && ts.some(x => x[1] == t[1] && x[2] == t[2]);
    };

    return { all, by_edge, by_first_edge, is_valid };
}
