
export default class Triples {
  constructor(N: number) {
    const triples = {};
    for(let a = 1; a <= N; a++) {
      for(let b = a; b <= N; b++) {
        const ab = a*a + b*b;
        const c = Math.floor(Math.sqrt(ab));
        if (c <= N && ab === c*c) {
          const key = `${a},${b},${c}`;
          triples[key] = [a,b,c];
        }
      }
    }

    this.triples = triples;
    this.points = this.getPoints(triples);
  }

  private getPoints(triples) {
    const points = {};
    for(let k in triples) {
      for(let x of triples[k]) {
        if (x in points == false) {
          points[x] = [];
        }
        points[x].push(k);
      }
    }
    return points;
  }


  logStats() {
    console.log("number of triples", Object.keys(this.triples).length);
    console.log("number of points", Object.keys(this.points).length);
  }

  toGraphData() {
    const edges = [];
    for(let pt in this.points) {
      const nodes = this.points[pt];
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

    const nodeSize = key => Math.floor(
      this.triples[key]
        .map(p => this.points[p].length)
        .reduce((s,x) => s + x, 0) / 30
    );

    return {
      nodes: Object.keys(this.triples).map(key => {
        const size = nodeSize(key);
        return {
          key, attributes: {
            label: key, size,
            color: size > 1
              ? size > 2 ? "#ff5050" : "#50ff50"
              : "#eeeeee"
          }
        }
      }),
      edges
    };
  }

  // Pendants are triples that contain a point without a link.
  // I.e. a point that does not occur in any other triple.
  dropPendants() {
    const pendants = Object.keys(this.points).reduce(
      (res, p) => this.points[p].length == 1 ? res.add(p) : res,
      new Set()
    );

    if (pendants.size > 0) {
      console.log("dropping", pendants.size, "pendants");
      for(let p of pendants) {
        this.points[p].forEach(t => delete this.triples[t]);
      }
      this.points = this.getPoints(this.triples);
      this.dropPendants();
    }
    return this;
  }
}
