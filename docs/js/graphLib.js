
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

export function mkTriples(N, {dropPendants}) {
    const all = dropPendants
      ? filterByLinkWeight(pythagoreanTriples(N), {minWeight: 2})
      : pythagoreanTriples(N);

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

    const all_edges = [...new Set(all.flat())]
      .sort((a,b) => a-b);

    const by_first_edge = {};
    for(let t of all) {
        const e = t[0];
        if(e in by_first_edge) {
            by_first_edge[e].push(t);
        } else {
            by_first_edge[e] = [t];
        }
    }

    const get = (a, b, c) => (by_first_edge[a] || [])
        .find(t => b === t[1] && c === t[2]);

    const is_valid = (t) => !!get(...t);

    return { all, all_edges, by_edge, by_first_edge, get, is_valid };
}
