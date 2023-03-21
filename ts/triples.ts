export function data(N: number) {
  const triples = {};
  for(let a = 1; a <= N; a++) {
    for(let b = a; b <= N; b++) {
      const ab = a*a + b*b;
      const c = Math.floor(Math.sqrt(ab));
      if (ab === c*c) {
        const key = `${a},${b},${c}`;
        triples[key] = [a,b,c];
      }
    }
  }
  console.log("number of triples", Object.keys(triples).length);

  const points = {};
  for(let k in triples) {
    for(let x of triples[k]) {
      if (x in points == false) {
        points[x] = [];
      }
      points[x].push(k);
    }
  }
  console.log("number of points", Object.keys(points).length);
  return {triples, points};
}


export function graph(N: number) {
  const {triples, points} = data(N);

  const edges = [];
  for(let pt in points) {
    const nodes = points[pt];
    for(let source of nodes) {
      for(let target of nodes) {
        if(source < target) {
          edges.push({
            key: `${source}#${target}`,
            source, target,
            attributes: {label: pt}
          });
        }
      }
    }
  }
  console.log("number of edges", edges.length);

  return {
    nodes: Object.keys(triples).map(key => ({
      key, attributes: {label: key}
    })),
    edges
  };
}

export default {graph, data};
