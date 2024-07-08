---
style: max-width.css
---

```js
import * as Plot from "npm:@observablehq/plot";
import {pythagoreanTriples} from "./js/graphLib.js";
import {mkGraph, applyLayout, getWeakNodes, graphSVG} from "./js/pendants.js";

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
const triples = allTriples.filter(([a,b,c]) => a <= N && b <= N && c <= N);
const fullGraph = mkGraph(triples);
const pendants = getWeakNodes(fullGraph, {maxWeight: 1});

const shrinkedGraph = mkGraph(
    fullGraph.nodes
        .filter(n => !pendants.nodes.has(n))
        .map(n => n.labels)
);

display(pendants);
display(fullGraph);
display(shrinkedGraph);

const graph = dropPendants ? shrinkedGraph : fullGraph;
applyLayout(graph);

```

```js
const allWeights = Array(fullGraph.maxLabelWeight+1)
    .fill()
    .map((_, weight) => ({
        weight,
        count: 0,
        filter: "all"
    }));
fullGraph.labels.forEach(ns => allWeights[ns.length].count++);

const shrinkedWeights = Array(shrinkedGraph.maxLabelWeight+1)
    .fill()
    .map((_, weight) => ({
        weight,
        count: 0,
        filter: "no pendants"
    }));
shrinkedGraph.labels.forEach(ns => shrinkedWeights[ns.length].count++);

display(Plot.plot({
    width,
    x: {axis: null},
    color: {scheme: "BuRd", legend: true},
    marks: [
        Plot.barY(
            allWeights.concat(shrinkedWeights),
            {   x: "filter",
                fx: "weight",
                y: "count",
                fill: "filter",
            }
        )
    ]
}));
```


```js
const dropPendants = view(
    Inputs.toggle({
        label: "Drop pendants",
        value: false
    })
);
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
    ${display(graphSVG(graph, pendants, {width, minLabelWeight}))}
</div>

