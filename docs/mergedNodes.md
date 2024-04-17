# One more page

```js
import {linkSet, groupByEdges, filterByLinkWeight} from "./graphLib.js";
import {triplesToGraph, forceLayout} from "./graphDrawing.js";

const triples = FileAttachment("data/triples.json").json();
```

```js
const links = groupByEdges(triples);
display(triples);
display(links);
```

```js
function cmpNum(a, b) { return a - b; }
// display([17,1,30,3].sort())
// display([17,1,30,3].sort(cmpNum))

function cmpNumArray(a, b) {
    for(let i = 0; i < a.length && i < b.length; i++) {
        if (a[i] != b[i]) return a[i] - b[i];
    }
    return a.length - b.length;
}

// display([[2],[],[17,1],[1]].sort(cmpNumArray))
// display([[1,2,3,4], [1,2,3]].sort(cmpNumArray))
// display([[1,2,3,5], [1,2,3,4]].sort(cmpNumArray))
```



```js
function emptyNode() {
    return {
        ts: [],
        intVars: [],
        extVars: [],
    };
}

function appendNodes(n, l, ts) {
    // NB. This can break anytime.
    // The `Set` below drops duplicates only because they reference
    // same values as a result of `groupByEdges`.
    n.ts = [...new Set(ts.concat(n.ts))].sort(cmpNumArray);
    n.intVars.push(l);
    n.intVars.sort(cmpNum);
    n.extVars = [...new Set(n.ts.flat())]
        .filter(x => !n.intVars.includes(x))
        .sort(cmpNum);
    return n;
}

// display(appendNodes(emptyNode(), 3, [[1,2,3], [3,6,7]]));
// display(
//     appendNodes(
//         appendNodes(emptyNode(), 3, [[1,2,3], [3,6,7]]),
//         6,
//         [[4,5,6], [6,8,9]])
// );
```

```js
function mergeNodes(weight, nodes) {
    const mergedNodes = [];
    const node2merged = {};

    groupByEdges(nodes).forEach((ts, l) => {
        if (ts.length <= weight) {
            let i = undefined;
            for(let t of ts) {
                i = node2merged[JSON.stringify(t)];
                if (i !== undefined) {
                    break;
                }
            }

            if (i === undefined) {
                i = mergedNodes.length;
                mergedNodes.push(emptyNode());
            }
            appendNodes(mergedNodes[i], l, ts);
            ts.forEach(t => node2merged[JSON.stringify(t)] = i);
        }
    });

    mergedNodes.sort((a,b) => -a.intVars.length + b.intVars.length);

    const newNodes = mergedNodes
        .map(n => n.extVars)
        .concat(
            nodes.filter(t => !(JSON.stringify(t) in node2merged))
        ).sort(cmpNumArray)

    return {mergedNodes, newNodes, links: linkSet(newNodes)};
}
```

```js
const m2 = mergeNodes(3, triples);
display(m2);
const m3 = mergeNodes(3, m2.newNodes);
display(m3);
const m4 = mergeNodes(2, m3.newNodes);
display(m4);
```
