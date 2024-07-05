
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
    xs.forEach(x => {
      if (ls.has(x)) { ls.get(x).push(xs); } else { ls.set(x, [xs]); }
    })
  );
  return ls;
}

export function filterByLinkWeight(ts, {minWeight}) {
  ts = new Set(ts);
  while(true) {
    let deleted = 0;
    groupByEdges(ts).forEach((_, xs) => {
      if (xs.length < minWeight) {
        xs.forEach(x => ts.delete(x));
        deleted += 1;
      }
    });
    if (deleted == 0) return [...ts];
  }
}
