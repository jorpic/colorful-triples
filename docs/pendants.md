
```js
import {pythagoreanTriples, groupByEdges} from "./graphLib.js";
const MAX_N = 7825;
const allTriples = pythagoreanTriples(MAX_N);
```



```js
const N = view(Inputs.range([5, MAX_N], {value: 5, step: 1, label: "N"}));
```

```js
const triples = allTriples.filter(([a,b,c]) => a <= N && b <= N && c <= N);
const labels = groupByEdges(triples);
display(triples);
```

We've got **${triples.length}** nodes(s) and **${labels.size}** edge(s).

```js
function mkGraph(ns) {
  // copy nodes and add extra props
  const nodes = ns.map((n, id) => ({
    label: JSON.stringify(n),
    edges: n,
  }));

  const ix = new Map(); // index of edge labels
  nodes.forEach(n =>
    n.edges.forEach(l =>
      ix.has(l) ? ix.get(l).push(n) : ix.set(l, [n])
    )
  );

  nodes.forEach(n => {
    n.weight = n.edges.reduce((s, l) => s + ix.get(l).length, -3);
  });

  const edges = [];
  ix.forEach((ns, label) => {
    const weight = ns.length;
    for(let i = 0; i < ns.length-1; i++) {
        for(let j = i+1; j < ns.length; j++) {
            edges.push({source: ns[i], target: ns[j], label,
            weight});
        }
    }
  });

  return {
    nodes, edges,
    maxNodeWeight: Math.max(...nodes.map(n => n.weight)),
    maxLabelWeight: Math.max(...edges.map(e => e.weight)),
  };
}


function forceLayout(graph) {
  d3.forceSimulation(graph.nodes)
    .force("link",
      d3.forceLink()
        .links(graph.edges)
        .distance(10))
    .force(
      "charge",
      d3.forceManyBody().strength(-250)
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


function draw(graph, {w, h, minLabelWeight}) {
  w += 40;
  h += 40;

  const svg = d3.create("svg")
      .attr("width", w)
      .attr("height", h);

  const edgeColor = e =>
    d3.interpolateYlGn(e.weight / graph.maxLabelWeight);
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
      d3.interpolateBlues(0.8 * n.weight / graph.maxNodeWeight);
  svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", nodeColor)
      .attr("cx", n => n.x + w/2)
      .attr("cy", n => n.y + h/2)
      .attr("r", 6)
      .append("title")
        .text(n => n.label)

  return svg.node();
}

const graph = mkGraph(triples);
display(graph);
const size = forceLayout(graph);
const minLabelWeight = view(Inputs.range([0, 30], {value: 0, step: 1, label: "Hide weak edges"}));
```

<div style="display: flex;">
    ${display(draw(graph, {...size, minLabelWeight}))}
</div>

