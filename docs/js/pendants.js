import * as d3 from "npm:d3";

export function mkGraph(ns) {
  const nodes = ns.map((n, id) => ({
    labels: n,
  }));

  // Create a map from label to an array of nodes sharing that label.
  const labels = new Map();
  nodes.forEach(n =>
    n.labels.forEach(l =>
      labels.has(l) ? labels.get(l).push(n) : labels.set(l, [n])
    )
  );

  // Node's weight is a sum of its labels' weights.
  nodes.forEach(n => {
    n.weight = n.labels.reduce((s, l) => s + labels.get(l).length, -3);
  });

  const edges = [];
  labels.forEach((ns, label) => {
    for(let i = 0; i < ns.length-1; i++) {
        for(let j = i+1; j < ns.length; j++) {
            edges.push({
              source: ns[i],
              target: ns[j],
              // Edge weight is the same as its label weight.
              weight: ns.length,
              label,
            });
        }
    }
  });

  return {
    nodes, edges, labels,
    maxNodeWeight: Math.max(...nodes.map(n => n.weight)),
    maxLabelWeight: Math.max(...edges.map(e => e.weight)),
  };
}


export function getWeakNodes(graph, {maxWeight}) {
  const weakNodes = new Map();
  let maxDepth = 0;

  for(let depth = 1; true; depth++) {
    let foundWeak = false;

    graph.labels.forEach((ns, _l) => {
      // ns = nodes connected by the edges with a label _l
      const xs = ns
        .filter(n => !weakNodes.has(n) || weakNodes.get(n) == depth);

      if(0 < xs.length && xs.length <= maxWeight) {
        foundWeak = true;
        xs.forEach(n => weakNodes.set(n, depth));
      }
    });

    if(!foundWeak) {
      break;
    }
    maxDepth = depth;
  }
  return {nodes: weakNodes, maxDepth};
}


// FIXME: filterWeakLinks(graph, {maxDepth, maxWeight}) -> graph


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


export function graphSVG(graph, pendants, {width, minLabelWeight}) {
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

  const nodeColor = n => {
      const pendantDepth = pendants.nodes.get(n);
      return pendantDepth !== undefined
        ? d3.interpolateReds(pendantDepth / pendants.maxDepth)
        : d3.interpolateGreens(0.8 * n.weight / graph.maxNodeWeight);
  };

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
