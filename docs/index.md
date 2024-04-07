---
title: Hello!
toc: true
---

# Stats

```js
import {triplesToGraph, lineLayout, forceLayout} from "./graphDrawing.js";

const tight_3_3 = FileAttachment("data/tight_3_3.json").json();
const tight_3_2 = FileAttachment("data/tight_3_2.json").json();

function linkSet(nodes) {
  const ls = new Set();
  nodes.forEach(n => n.forEach(l => ls.add(l)));
  return ls;
}

function groupByEdges(ts) {
  const ls = {};
  ts.forEach(xs =>
    xs.map(x => {
      if (x in ls) { ls[x].push(xs); } else { ls[x] = [xs]; }
    })
  );
  return ls;
}

function filterByLinkWeight(ts, {minWeight}) {
  ts = new Set(ts);
  while(true) {
    let deleted = 0;
    Object.values(groupByEdges(ts)).forEach(xs => {
      if (xs.length < minWeight) {
        xs.forEach(x => ts.delete(x));
        deleted += 1;
      }
    });
    if (deleted == 0) return [...ts];
  }
}
```

```js
display(tight_3_3);

const best = tight_3_3
    .map(s => [linkSet(s).size, s.length, s])
    .filter(s => 27 <= s[0] && s[0] <= 44)
    .sort((a,b) => -b[1] + a[1])
    .map(s => s[2]);

display(best);

const usedTriples = new Set();
const subs = [];
for(let x of best.concat(tight_3_3)) {
    let y = x.filter(t => !usedTriples.has(JSON.stringify(t)));
    y = filterByLinkWeight(y, {minWeight: 2});
    if (y.length >= 20) {
        subs.push([x, y]);
        for(let t of y) {
            usedTriples.add(JSON.stringify(t));
        }
    }
}

display(subs);
```

```js
const i = view(Inputs.range([0, subs.length-1], {value:0, step: 1}));
```

```js
const s = subs[i];
const a = triplesToGraph(s[0]);
const b = triplesToGraph(s[1]);
display({links: linkSet(s[0]), triples: s[0]});
display({links: linkSet(s[1]), triples: s[1]});
```

<div style="display: flex;">
 <div style="flex-basis:50%">${display(forceLayout(a))}</div>
 <div style="flex-basis:50%">${display(forceLayout(b))}</div>
</div>
