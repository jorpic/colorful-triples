---
style: max-width.css
---

```js
import {pythagoreanTriples} from "./js/graphLib.js";
import {mkGraph, applyLayout, graphSVG} from "./js/pendants.js";
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
const graph = mkGraph(triples);
display(graph);
const size = applyLayout(graph);
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
    ${display(graphSVG(graph, {...size, minLabelWeight}))}
</div>

