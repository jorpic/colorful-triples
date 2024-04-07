function genTriples(n) {
  const ts = [];
  for(let a = 1; a < n; a++) {
      for(let b = a+1; b < n; b++) {
        const ab = a*a + b*b;
        const c = Math.floor(Math.sqrt(a*a + b*b));
        if(c <= n && ab == c*c) {
          ts.push([a,b,c]);
        }
      }
  }
  return ts;
}

function groupByEdges(ts) {
  const ls = {};
  ts.forEach(xs =>
    xs.map(x => {
      if (x in ls) { ls[x].push(xs); } else { ls[x] = [xs]; }
    })
  );
  return ls;
}

function filterByLinkWeight(ts, {minWeight}) {
  ts = new Set(ts);
  while(true) {
    let deleted = 0;
    Object.values(groupByEdges(ts)).forEach(xs => {
      if (xs.length < minWeight) {
        xs.forEach(x => ts.delete(x));
        deleted += 1;
      }
    });
    if (deleted == 0) return [...ts];
  }
}

function neighbourhood(lnk, links, {radius}) {
  let nodes = {}; // this is a replacement for Set<[number]>()
  lnk.forEach(l => links[l].forEach(n => {nodes[n] = n;}));
  while(--radius > 0) {
    Object.values(nodes).forEach(n =>
      n.forEach(l => 
          links[l].forEach(x => {nodes[x] = x;})
    ));
  }
  return Object.values(nodes);
}


function compareArrays(xs, ys) {
  for(let i = 0; i < Math.min(xs.length, ys.length); i++) {
    if (xs[i] != ys[i]) return xs[i] - ys[i];
  }
  return 0;
}

function dropDuplicates(groups) {
  return Object.values(
    groups
      .map(group => group.sort(compareArrays))
      .reduce((set, grp) => {
        set[JSON.stringify(grp)] = grp;
        return set;
      },
      {})
  );
}

function linkSet(nodes) {
  const ls = new Set();
  nodes.forEach(n => n.forEach(l => ls.add(l)));
  return ls;
}

const nodes = genTriples(7825);
const edges = groupByEdges(nodes);

function getTightGroups({radius, minWeight, maxWidth}) {
  let groups = Object.keys(edges)
    .map(lnk => neighbourhood([Number(lnk)], edges, {radius}))
    .map(group => filterByLinkWeight(group, {minWeight}))
    .filter(group => group.length > 0 && linkSet(group).size <= maxWidth);

  return dropDuplicates(groups);
}

const tightGroups = getTightGroups({
  radius: 3,
  minWeight: 3,
  maxWidth: 42
});

function groupToNormalForm(grp) {
  let links = [...new Set(grp.flatMap(t => t))].map(l => Number(l)).sort((a,b) => a - b);
  // `revLinks` is a monotinic mapping.
  let revLinks = links.reduce((map, l, ix) => { map[l] = ix; return map; }, {});
  let schema = grp.map(t => t.map(l => revLinks[l]));
  return {links, schema};
}

const schemas = [...new Set(
  tightGroups.map(g => JSON.stringify(groupToNormalForm(g).schema)))]
    .map(JSON.parse)
    .sort((a, b) => a.length - b.length);


process.stdout.write(JSON.stringify({
  nodes,
  edges,
  tightGroups,
  schemas
}));
