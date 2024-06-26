---
title: Hello!
toc: true
---

# Stats

```js
import {linkSet, groupByEdges, filterByLinkWeight} from "./graphLib.js";
import {triplesToGraph, lineLayout, forceLayout} from "./graphDrawing.js";

const clusters = FileAttachment("data/clusters.json").json();
```

```js
display(clusters);

function mkGraph(nodes) {
  let edges = [];
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
    }
  }
  for(let n of nodes) {
    n.weight = Object.keys(n.edge_weights).length;
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
