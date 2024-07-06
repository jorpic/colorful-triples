---
style: max-width.css
---

```js
import * as Plot from "npm:@observablehq/plot";
import {pythagoreanTriples} from "./js/graphLib.js";
import {mkGraph, applyLayout, markPendants, graphSVG} from "./js/pendants.js";

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

const dropPendants = view(
    Inputs.toggle({
        label: "Drop pendants",
        value: false
    })
);
```

```js
const triples = allTriples.filter(([a,b,c]) => a <= N && b <= N && c <= N);
const fullGraph = mkGraph(triples);
markPendants(fullGraph);
const shrinkedGraph = mkGraph(
    fullGraph.nodes
        .filter(n => !n.pendant)
        .map(n => n.labels)
);

const graph = dropPendants ? shrinkedGraph : fullGraph;
applyLayout(graph);

display(fullGraph);
display(shrinkedGraph);
```

```js
const labelWeights = Array(graph.maxLabelWeight+1)
    .fill()
    .map((_, weight) => ({weight, count: 0}));
graph.labels.forEach(ns => labelWeights[ns.length].count++);

display(Plot.plot({
    width,
    marks: [
        Plot.barY(labelWeights, {x: "weight", y: "count", fill: "lightblue"})
    ]
}));
```


```js
const minLabelWeight = view(
    Inputs.range(
        [0, graph.maxLabelWeight],
        {value: 0, step: 1, label: "Hide weak edges"}
    )
);
```

<div style="display: flex;">
    ${display(graphSVG(graph, {width, minLabelWeight}))}
</div>

