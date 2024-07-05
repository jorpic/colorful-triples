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

  return {w, h, maxX, minX, maxY, minY};
}


export function graphSVG(graph, {w, h, minLabelWeight}) {
  w += 40;
  h += 40;

  const svg = d3.create("svg")
      .attr("width", w)
      .attr("height", h);

  const edgeColor = e =>
    d3.interpolateBlues(e.weight / graph.maxLabelWeight);
  svg.selectAll("edge")
    .data(graph.edges.filter(e => e.weight >= minLabelWeight))
    .join("line")
      .attr("stroke", edgeColor)
      .attr("stroke-width", 1)
      .attr("x1", e => e.source.x + w/2)
      .attr("y1", e => e.source.y + h/2)
      .attr("x2", e => e.target.x + w/2)
      .attr("y2", e => e.target.y + h/2)
      .append("title").text(e => `l:${e.label} w:${e.weight}`);

  const nodeColor = n =>
      d3.interpolateGreens(0.8 * n.weight / graph.maxNodeWeight);
  svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", nodeColor)
      .attr("cx", n => n.x + w/2)
      .attr("cy", n => n.y + h/2)
      .attr("r", 6)
      .append("title")
        .text(n => JSON.stringify(n.labels))

  return svg.node();
}
