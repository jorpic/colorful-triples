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

const nodes = genTriples(7825);
const edges = groupByEdges(nodes);

process.stdout.write(JSON.stringify({nodes, edges}));
