
```js
import {mkTriples} from "./js/graphLib.js";
import {triplesToGraph} from "./js/graphDrawing.js";
import {applyLayout} from "./js/pendants.js";
```

```js
const N = 7825;
const triples = mkTriples(N, {dropPendants: true});
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
const covered_edges = new Set(k6s.flat().flat());
const uncovered_edges = Object.entries(triples.by_edge)
    .filter(([e,ts]) => !covered_edges.has(parseInt(e)));

display({
    covered_edges,
    uncovered_edges,
    total_edges: Object.keys(triples.by_edge).length
});

display({
    uncovered_edges_hist: uncovered_edges
        .map(([_,ts]) => ts.length)
        .reduce(
            (r,x) => {
                if(x in r) { r[x] += 1 } else { r[x] = 1 };
                return r;
            }, {})
});

const covered_triples = new Set(k6s.flat().map(JSON.stringify));
const uncovered_triples = triples.all.filter(t => !covered_triples.has(JSON.stringify(t)));
display({covered_triples, total_triples: triples.all.length, uncovered_triples});
```

How many uncovered triples each knot touches?

```js
display(
    k6s.map(k => {
        const k_edges = new Set(k.flat());
        return {k, xs: uncovered_triples.filter(t => t.some(e => k_edges.has(e)))};
    })
);
```


Cover triples with knots:

```js
const knot_ix = k6s.reduce((r,k) => {
    const k_edges = [...new Set(k.flat())];
    for(let e of k_edges) {
        if(e in r) {
            r[e].push(k);
        } else {
            r[e] = [k];
        }
    }
    return r;
}, {});

display({
    knot_ix,
    triple_covers: uncovered_triples.map(t => ({t, ks: t.map(e => knot_ix[e] || null)})),
    schemes: uncovered_triples.map(t => t.map(
        e => JSON.stringify((knot_ix[e] || [])
            .map(k => k.flat().indexOf(e)))
    ))
});

display({
  "[_,_,11]":
    uncovered_triples
      .map(t => ({t,
        s: t.map(
          e => (knot_ix[e] || []).map(k => k.flat().indexOf(e))
        )}))
      .filter(xs => xs.s.every(x => x.length > 0))
      .filter(xs => xs.s[2].indexOf(11) < 0)
      .map(xs => JSON.stringify(xs))
});
```

Uncovered triples that can't be covered by knots.

```js
display(
    uncovered_triples.filter(t => !t.every(e => covered_edges.has(e)))
);
```



```js
function mkGraphWithLinks(triples) {
    const nodes = triples
        .map(links => ({links, type: "triple"}));

    for(let e of [...new Set(triples.flat())]) {
        if(!covered_edges.has(e)) {
            nodes.push({link: e, type: "edge", isCovered: covered_edges.has(e)});
        }
    }

    const edge_to_id = {};
    nodes.forEach((n, id) => {
        n.id = id;
        if(n.type === "edge") {
            edge_to_id[n.link] = id;
        }
    });

    const edges = [];
    for(let i = 0; i < nodes.length; i++) {
        const n = nodes[i];
        if(n.type === "triple") {
            for(let e of n.links) {
                if(edge_to_id[e]) {
                    edges.push({source: n.id, target: edge_to_id[e]});
                }
            }
        }
    }
    return {nodes, edges};
}

function graphWithLinks(graph) {
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
        .attr("y2", e => e.target.y);

    svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
        .attr("fill", n =>
            n.type === "triple" ? "black" : n.isCovered ? "green" : "red")
        .attr("cx", n => n.x)
        .attr("cy", n => n.y)
        .attr("r", n => n.type === "triple" ? 8 : 8)
        .append("title").text(n => JSON.stringify(n));

    return svg.node();
}
```

```js

const uncovered_graph = mkGraphWithLinks(
    uncovered_triples.filter(t => !t.every(e => covered_edges.has(e)))
);
applyLayout(uncovered_graph);
display(graphWithLinks(uncovered_graph));
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
