---
style: max-width.css
---

```js
import * as Plot from "npm:@observablehq/plot";
import {pythagoreanTriples, filterByLinkWeight} from "./js/graphLib.js";
import {mkGraph, applyLayout, getWeakNodes} from "./js/pendants.js";

const MAX_N = 7825;
const allTriples = pythagoreanTriples(MAX_N);
```


```js
const N = view(
    Inputs.range(
        [5, MAX_N],
        {value: 1235, step: 1, label: "N"}
    )
);
```

```js
const triples = filterByLinkWeight(
    allTriples
        .filter(([a,b,c]) => a <= N && b <= N && c <= N),
    {minWeight: 2}
);

const graph = mkGraph(triples);
display(graph);
```

```js
const rootIx = view(
    Inputs.range(
        [0, graph.nodes.length-1],
        {value: 0, step: 1, label: "Select root node"}
    )
);
```

```js
// returns Map<Node, depth>
function neighborhood(graph, root, depth) {
  const nh = new Map();
  nh.set(root, 0);
  for(let d = 1; d <= depth; d++) {
    // NB. We are modifying the collection we are iterationg over.
    // Copy of keys is required so that modifications do not interfere.
    [...nh.keys()].forEach(node =>
      node.labels.forEach(l =>
        graph.labels.get(l).forEach(n =>
          nh.get(n) === undefined && nh.set(n, d)
        )
      )
    );
  }
  return nh;
}


function graphSVG(graph, neighborhood, {width}) {
  const [mx, my, w, h] = graph.viewBox;
  const margin = 10;
  const viewBox = [mx - margin, my - margin, w + 2*margin, h + 2*margin];

  const svg = d3.create("svg")
      .attr("preserveAspectRatio", "xMidYMid")
      .attr("viewBox", viewBox)
      .attr("width", width / (1 + 4*Math.exp(-w/600)));

  const inNh =
    e => neighborhood.get(e.source) !== undefined
      && neighborhood.get(e.target) !== undefined;
  const edgeColor = e => inNh(e)
    ? "salmon"
    : d3.interpolateBlues(e.weight / graph.maxLabelWeight);

  for(let lw = 0; lw <= graph.maxLabelWeight; lw++) {
    svg.selectAll("edge")
        .data(graph.edges.filter(e => e.weight == lw))
        .join("line")
        .attr("stroke", edgeColor)
        .attr("stroke-width", e => inNh(e) ? 5 : 1)
        .attr("x1", e => e.source.x)
        .attr("y1", e => e.source.y)
        .attr("x2", e => e.target.x)
        .attr("y2", e => e.target.y)
        .append("title").text(e => `l:${e.label} w:${e.weight}`);
    }

  const nodeColor = n => {
    const depth = neighborhood.get(n);
    return depth !== undefined
      ? d3.interpolateReds(1 - (depth + 1) / 4)
      : d3.interpolateGreens(0.8 * n.weight / graph.maxNodeWeight);
  };

  const nodeSize = n => neighborhood.get(n) !== undefined ? 10 : 6;
  svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", nodeColor)
      .attr("cx", n => n.x)
      .attr("cy", n => n.y)
      .attr("r", nodeSize)
      .append("title")
        .text(n => JSON.stringify(n.labels))

  return svg.node();
}

function filterWeakNodes(graph, {minWeight}) {
  const weakNodes = getWeakNodes(graph, {maxWeight: minWeight-1});
  return mkGraph(
    graph.nodes
      .filter(n => !weakNodes.nodes.has(n))
      .map(n => n.labels)
  );
}
```

```js
const nh = neighborhood(graph, graph.nodes[rootIx], 3);
const nhGraph = filterWeakNodes(
    mkGraph([...nh.keys()].map(n => n.labels)),
    {minWeight: 3}
);

display(nh);
display(nhGraph);

applyLayout(nhGraph);
display(graphSVG(nhGraph, nh, {width}));
```
