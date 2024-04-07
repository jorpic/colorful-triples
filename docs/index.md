---
title: Hello!
toc: true
---

# Stats

```js
const hg = FileAttachment("data/hypergraph.json").json();
```

Our hypergraph consists of:
- **${hg.nodes.length}** nodes
- **${Object.keys(hg.edges).length}** hyperedges

```js
display(hg.tightGroups)
```

```js
display(hg.schemas)
```

```js
import {triplesToGraph, lineLayout, forceLayout} from "./graphDrawing.js";
```

```js
const i = view(Inputs.range([0, hg.schemas.length-1], {step: 1}));
```

```js
const s = hg.schemas[i];
const g = triplesToGraph(s);
display(g);
display(lineLayout(g));
display(forceLayout(g));
```
