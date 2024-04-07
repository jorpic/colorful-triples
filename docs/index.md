---
title: Hello!
toc: true
---

# Stats

```js
const triples = FileAttachment("data/triples.json").json();
const tight_3_3 = FileAttachment("data/tight_3_3.json").json();
const tight_3_2 = FileAttachment("data/tight_3_2.json").json();
```

```js
display(triples);
```

```js
function linkSet(nodes) {
  const ls = new Set();
  nodes.forEach(n => n.forEach(l => ls.add(l)));
  return ls;
}

function groupToNormalForm(grp) {
  let links = [...new Set(grp.flatMap(t => t))].map(l => Number(l)).sort((a,b) => a - b);
  // `revLinks` is a monotinic mapping.
  let revLinks = links.reduce((map, l, ix) => { map[l] = ix; return map; }, {});
  let schema = grp.map(t => t.map(l => revLinks[l]));
  return {links, schema};
}

const schemas = [...new Set(
  tight_3_3.map(g => JSON.stringify(groupToNormalForm(g).schema)))]
    .map(JSON.parse)
    .sort((a, b) => a.length - b.length);
```

```js
display(schemas)
```

```js
import {triplesToGraph, lineLayout, forceLayout} from "./graphDrawing.js";
```

```js
const i = view(Inputs.range([0, schemas.length-1], {value:0, step: 1}));
```

```js
const s = schemas[i];
const g = triplesToGraph(s);
display({links: linkSet(s), triples: s});
```

<div style="display: flex;">
 <div style="flex-basis:50%">${display(forceLayout(g))}</div>

 <div style="flex-basis:50%">${display(lineLayout(g))} </div>
</div>
