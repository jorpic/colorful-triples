
```js
import {mkTriples} from "./js/graphLib.js";
import {triplesToGraph} from "./js/graphDrawing.js";
import {applyLayout} from "./js/pendants.js";
```

```js
const N = 7825;
const triples = mkTriples(N);
```

```js
display(triples);
```


# 4-knots

```js
const k4s = [];
for(let ts of Object.values(triples.by_first_edge)) {
    for(let i = 0; i < ts.length; i++) {
        for(let j = i+1; j < ts.length; j++) {
            const [x, a, b] = ts[i];
            const [_, c, d] = ts[j];
            const t2 = [Math.min(a, d), Math.max(a, d), Math.sqrt(a*a + d*d)];
            const t3 = [Math.min(b, c), Math.max(b, c), Math.sqrt(b*b + c*c)];

            if(triples.is_valid(t2) && triples.is_valid(t3)) {
                k4s.push([ts[i], ts[j], t2, t3]);
            }
        }
    }
}
```

```js
display(k4s);
const graph = triplesToGraph(k4s.flat());
applyLayout(graph);
display(graphSVG(graph));
```

## Joined 4-knots

```js
// TODO
```

# 6-knots

```js
const k6s = [];
for(let t0 of triples.all) {
    const [a, b, c] = t0;

    for(let t1 of triples.by_first_edge[a] || []) {
        if(t1 <= t0 || t1[0] != a) continue;
        const [aa, d, e] = t1;

        for(let t2 of triples.by_first_edge[b] || []) {
            if(t2[0] != b) continue;
            const [bb, f, g] = t2;

            for(let t3 of triples.by_first_edge[c] || []) {
                if(t3[0] != c) continue;
                const [cc, h, x] = t3;

                const t4 = [d, f, h].sort((a,b) => a-b);
                const t5 = [e, g, x].sort((a,b) => a-b);

                if(triples.is_valid(t4) && triples.is_valid(t5)) {
                    k6s.push([t0, t1, t2, t3, t4, t5]);
                }
            }
        }
    }
}
```

```js
display(k6s);
const graph = triplesToGraph(
    k6s.slice(0,1).flat()
);
applyLayout(graph);
display(graphSVG(graph));
```

```js
const clusters = [];
const used_knots = new Set();
while(true) {
    const used_edges = new Set();
    const cluster = [];

    for(let k of k6s) {
        if(used_knots.has(k)) {
            continue;
        }
        let k_edges = [...new Set(k.flat().flat())];
        if(k_edges.some(e => used_edges.has(e))) {
            continue;
        }

        for(let e of k_edges) {
            used_edges.add(e);
        }
        used_knots.add(k);
        cluster.push(k);
        if(cluster.length == 5) {
            break;
        }
    }

    if(cluster.length < 5) {
        break;
    }
    clusters.push(cluster);
}

display(clusters);
display(used_knots.size);
const covered_edges = [...new Set(clusters.flat().flat().flat())].sort((a,b) => b-a);
display(covered_edges);
const covered_triples = [...new Set(clusters.flat().flat().map(JSON.stringify))].sort();
display(covered_triples);
```

```js
const clusters_edges = clusters.map(c => new Set(c.flat().flat()));
display(clusters_edges);
const intersections = [];
for(let i = 0; i < clusters_edges.length; i++) {
    for(let j = i+1; j < clusters_edges.length; j++) {
        const c = [...clusters_edges[i]].filter(e => clusters_edges[j].has(e)).length;
        if(c > 13) intersections.push({ i, j, c });
    }
}
display(intersections);
```


```js
function graphSVG(graph) {
    const [mx, my, w, h] = graph.viewBox;
    const margin = 10;
    const viewBox = [mx - margin, my - margin, w + 2*margin, h + 2*margin];

    const svg = d3.create("svg")
        .attr("preserveAspectRatio", "xMidYMid")
        .attr("viewBox", viewBox);

    svg.selectAll("edge")
        .data(graph.edges)
        .join("line")
        .attr("stroke", "grey")
        .attr("stroke-width", 1)
        .attr("x1", e => e.source.x)
        .attr("y1", e => e.source.y)
        .attr("x2", e => e.target.x)
        .attr("y2", e => e.target.y)
        .append("title").text(e => e.label);

    svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
        .attr("fill", "blue")
        .attr("cx", n => n.x)
        .attr("cy", n => n.y)
        .attr("r", 5)
        .append("title").text(n => JSON.stringify(n.links));

    return svg.node();
}
```
