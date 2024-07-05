---
title: Hello!
toc: true
---

# Stats

```js
import {linkSet, groupByEdges, filterByLinkWeight} from "./js/graphLib.js";
import {triplesToGraph, lineLayout, forceLayout} from "./js/graphDrawing.js";

const clusters = FileAttachment("data/clusters.json").json();
```

```js
display(clusters);

function mkGraph(nodes) {
  // copy nodes and add extra props
  nodes = nodes.map((n, id) => ({
    ...n,
    id,
    weight: 0,
    size: Object.keys(n.edge_weights).length,
  }));

  const edges = [];
  for(let i = 0; i < nodes.length; i++) {
    const a = nodes[i];
    for(let j = i+1; j < nodes.length; j++) {
      const b = nodes[j];
      const weight = Object.keys(a.edge_weights)
        .filter(x => x in b.edge_weights)
        .length;
      if(0 < weight) {
        edges.push({source: a, target: b, weight});
      }
      if(6 < weight) {
        a.weight += weight;
        b.weight += weight;
      }
    }
  }

  return {nodes, edges};
}
```

```js
const graph = mkGraph(clusters);
display(graph);
```

```js
const min_weight = view(Inputs.range([1, 7], {value:4, step: 1}));
```


<div style="display: flex;">
 ${display(forceLayout(graph, {w: 1200, h: 1000, min_weight}))}
</div>

```js
function joinOneEdge(graph) {

    const maxWeightEdge = graph.nodes.reduce(
        (r, n) => r.weight < n.weight ? n : r,
        graph.nodes[0]
    );
    const candidateEdges = graph.edges.filter(e => e.source ==
    maxWeightEdge || e.target == maxWeightEdge);
    const edgeToJoin = candidateEdges.reduce(
        (r, e) => r.weight < e.weight ? e : r,
        candidateEdges[0]
    );

    console.log({edgeToJoin});

    const cluster = graph.nodes.flatMap(n => {
        if(n == edgeToJoin.source) {
            const dst = edgeToJoin.target;
            return [{
                joined: `${n.size}+${dst.size}`,
                nodes: [...new Set([...n.nodes, ...dst.nodes])],
                edge_weights: {...n.edge_weights, ...dst.edge_weights}
            }];
        }
        if(n == edgeToJoin.target) {
            return [];
        }
        return [n];
    });
    return mkGraph(cluster);
}
```

```js
const iter = view(Inputs.range([0, 17], {value:0, step: 1}));
```

```js
let graph1 = graph;
for(let i = 0; i < iter; i++) {
    graph1 = joinOneEdge(graph1);
}
display(graph1);
```

```js
const min_weight_1 = view(Inputs.range([1, 7], {value:4, step: 1}));
```
<div style="display: flex;">
 ${display(forceLayout(graph1, {w: 1200, h: 1000, min_weight: min_weight_1}))}
</div>
