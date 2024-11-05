
```js
import {mkTriples} from "./js/graphLib.js";
import {triplesToGraph} from "./js/graphDrawing.js";
import {applyLayout} from "./js/pendants.js";
```

```js
const N = 7825;
const triples = mkTriples(N, {dropPendants: true});
```

# 6-knots

```js
const triples_in_knots = new Set();
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
                const t4 = triples.get(d, f, h);
                const t5 = triples.get(e, g, x);

                if(t4 && t5) {
                    const knot = [t0, t1, t2, t3, t4, t5];
                    // we add only knots that cover at least one new triple
                    if(knot.some(t => !triples_in_knots.has(t))) {
                        k6s.push(knot);
                        knot.forEach(t => triples_in_knots.add(t));
                    }
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
const knots_by_edge = k6s.reduce((r,k) => {
    const k_edges = [...new Set(k.flat())];
    for(let e of k_edges) {
        if(r.has(e)) {
            r.get(e).push(k);
        } else {
            r.set(e, [k]);
        }
    }
    return r;
}, new Map());

const knots_by_triple = k6s.reduce((r, k) => {
    for(let t of k) {
        if(r.has(t)) {
            r.get(t).push(k);
        } else {
            r.set(t, [k]);
        }
    }
    return r;
}, new Map());


const edges_not_covered_by_knots = triples.all_edges.filter(
    e => !knots_by_edge.has(e)
);
const triples_not_in_knots = triples.all.filter(
    t => !knots_by_triple.has(t));

const knot_edges = k6s.reduce((r, k) => {
    r.set(k, new Set(k.flat()));
    return r;
}, new Map());

display({
    knots_by_edge,
    knots_by_triple,
    triples_not_in_knots,
    edges_not_covered_by_knots,
    knot_edges,
});
```

Partition knots into groups of 5 pairwise disjoint ones.

```js
const simple_clusters = [];
const used_knots = new Set();
while(true) {
    const cluster = [];
    for(let k of k6s) {
        if(used_knots.has(k)) {
            continue;
        }
        const k_edges = knot_edges.get(k);
        const isDisjoint = cluster.every(
            kk => knot_edges.get(kk).isDisjointFrom(k_edges)
        );
        if(!isDisjoint) {
            continue;
        }
        cluster.push(k);
        if(cluster.length == 5) {
            break;
        }
    }
    if(cluster.length < 5) {
        break;
    }
    simple_clusters.push(cluster);
    cluster.forEach(k => used_knots.add(k));
}

display({simple_clusters, used_knots});
```

Triples covered by simple clusters.

```js
const triples_in_simple_clusters = new Set(
    simple_clusters.flat().flat()
);

display(
    simple_clusters.map(c => {
        const c_edges = new Set(c.flat().flat());
        const ts = triples.all.filter(t =>
            !triples_in_simple_clusters.has(t)
                && t.every(e => c_edges.has(e)));
        return {c, ts};
    })
);
```



```js
function graphSVG(graph) {
    const [mx, my, w, h] = graph.viewBox;
    const margin = 10;
    const viewBox = [mx - margin, my - margin, w + 2*margin, h + 2*margin];

    const svg = d3.create("svg")
        .attr("preserveAspectRatio", "xMidYMid")
        .attr("viewBox", viewBox)
        .attr("width",
            Math.max(600, Math.min(300, width / 4))
        );

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
