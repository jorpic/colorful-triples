
export function linkSet(nodes) {
  return new Set(nodes.flat());
}

export function groupByEdges(ts) {
  const ls = new Map();
  ts.forEach(xs =>
    xs.map(x => {
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
