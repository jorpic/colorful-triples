---
style: max-width.css
---



```js
import {pythagoreanTriples, filterByLinkWeight} from "./js/graphLib.js";
import {mkGraph, applyLayout} from "./js/pendants.js";

const clusters = FileAttachment("data/clusters.json").json();

const MAX_N = 7825;
const allTriples = filterByLinkWeight(
    pythagoreanTriples(MAX_N),
    {minWeight: 2}
);
```


```js
const clusterIx = view(
    Inputs.range(
        [0, clusters.length-1],
        {value: 0, step: 1, label: "Select a cluster"}
    )
);
```

```js
const cluster = clusters[clusterIx];
const graph = mkGraph(cluster.nodes);
applyLayout(graph);
```

```js
display(cluster);
display(graph);
display(graphSVG(graph, {width}))
```

```js
function markComponent(graph, id) {
    const visitedNodes = new Set();
    for(let n of graph.nodes) {
        if(n.component === undefined) {
            visitedNodes.add(n);
            n.component = {id, depth: 0};
            break;
        }
    }

    if(visitedNodes.size == 0) return null;

    for(let depth = 1; ; depth++) {
        let hasNew = false;
        for(let e of graph.edges) {
            if(visitedNodes.has(e.source) && !visitedNodes.has(e.target)) {
                visitedNodes.add(e.target);
                e.target.component = {id, depth};
                hasNew = true;
            }
            else if(visitedNodes.has(e.target) && !visitedNodes.has(e.source)) {
                visitedNodes.add(e.source);
                e.source.component = {id, depth};
                hasNew = true;
            }
        }

        if(!hasNew) {
            return visitedNodes;
        }
    }
}

function markComponents(graph) {
    let c = 0;
    while(markComponent(graph, c) != null) c++;
}


markComponents(graph);
```

```js
function graphSVG(graph, {width}) {
  const [mx, my, w, h] = graph.viewBox;
  const margin = 10;
  const viewBox = [mx - margin, my - margin, w + 2*margin, h + 2*margin];

  const svg = d3.create("svg")
      .attr("preserveAspectRatio", "xMidYMid")
      .attr("viewBox", viewBox)
      .attr("width", width / (1 + 4*Math.exp(-w/600)));

  const edgeColor = e => d3.interpolateBlues(e.weight / graph.maxLabelWeight);

  svg.selectAll("edge")
      .data(graph.edges)
      .join("line")
      .attr("stroke", edgeColor)
      .attr("stroke-width", 1)
      .attr("x1", e => e.source.x)
      .attr("y1", e => e.source.y)
      .attr("x2", e => e.target.x)
      .attr("y2", e => e.target.y)
      .append("title").text(e => `l:${e.label} w:${e.weight}`);

  const nodeColor = n =>
        d3.schemeSet1[n.component.id]

  svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", nodeColor)
      .attr("cx", n => n.x)
      .attr("cy", n => n.y)
      .attr("r",  n => n.weight / 2)
      .append("title")
        .text(n => JSON.stringify(n.labels))

  return svg.node();
}
```


