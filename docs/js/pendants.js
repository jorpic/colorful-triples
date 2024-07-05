import * as d3 from "npm:d3";

export function mkGraph(ns) {
  const nodes = ns.map((n, id) => ({
    labels: n,
  }));

  const labels = new Map(); // index of edge labels
  nodes.forEach(n =>
    n.labels.forEach(l =>
      labels.has(l) ? labels.get(l).push(n) : labels.set(l, [n])
    )
  );

  nodes.forEach(n => {
    n.weight = n.labels.reduce((s, l) => s + labels.get(l).length, -3);
  });

  const edges = [];
  labels.forEach((ns, label) => {
    const weight = ns.length;
    for(let i = 0; i < ns.length-1; i++) {
        for(let j = i+1; j < ns.length; j++) {
            edges.push({source: ns[i], target: ns[j], label,
            weight});
        }
    }
  });

  return {
    nodes, edges, labels,
    maxNodeWeight: Math.max(...nodes.map(n => n.weight)),
    maxLabelWeight: Math.max(...edges.map(e => e.weight)),
  };
}

export function markPendants(graph) {
  for(let iter = 1; true; iter++) {
    let count = 0;

    graph.nodes.forEach(n => {
      if(n.pendant) return;

      const isPendant = n.labels.some(l =>
        graph.labels.get(l)
          .filter(x => !x.pendant || x.pendant == iter)
          .length <= 1
      );
      if(isPendant) {
        count++;
        n.pendant = iter;
      }
    });

    if(count === 0) {
      graph.maxPendantDepth = iter;
      break;
    }
  }
}


export function applyLayout(graph) {
  d3.forceSimulation(graph.nodes)
    .force("link",
      d3.forceLink()
        .links(graph.edges)
        .distance(10))
    .force(
      "charge",
      d3.forceManyBody().strength(-100)
    )
    .force("x", d3.forceX(0))
    .force("y", d3.forceY(0))
    .tick(100);

  const maxX = Math.max(...graph.nodes.map(n => n.x));
  const minX = Math.min(...graph.nodes.map(n => n.x));
  const maxY = Math.max(...graph.nodes.map(n => n.y));
  const minY = Math.min(...graph.nodes.map(n => n.y));
  const w = maxX - minX;
  const h = maxY - minY;

  graph.viewBox = [minX, minY, w, h];
}


export function graphSVG(graph, {width, minLabelWeight}) {
  const [mx, my, w, h] = graph.viewBox;
  const margin = 10;
  const viewBox = [mx - margin, my - margin, w + 2*margin, h + 2*margin];

  const svg = d3.create("svg")
      .attr("preserveAspectRatio", "xMidYMid")
      .attr("viewBox", viewBox)
      .attr("width", width / (1 + 4*Math.exp(-w/600)));

  const edgeColor = e =>
    d3.interpolateBlues(e.weight / graph.maxLabelWeight);
  svg.selectAll("edge")
    .data(graph.edges.filter(e => e.weight >= minLabelWeight))
    .join("line")
      .attr("stroke", edgeColor)
      .attr("stroke-width", 1)
      .attr("x1", e => e.source.x)
      .attr("y1", e => e.source.y)
      .attr("x2", e => e.target.x)
      .attr("y2", e => e.target.y)
      .append("title").text(e => `l:${e.label} w:${e.weight}`);

  const nodeColor = n =>
      n.pendant
        ? d3.interpolateReds(n.pendant / graph.maxPendantDepth)
        : d3.interpolateGreens(0.8 * n.weight / graph.maxNodeWeight);

  svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", nodeColor)
      .attr("cx", n => n.x)
      .attr("cy", n => n.y)
      .attr("r", 6)
      .append("title")
        .text(n => JSON.stringify(n.labels))

  return svg.node();
}
