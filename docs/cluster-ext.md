---
style: max-width.css
---



```js
import {pythagoreanTriples, groupByEdges, filterByLinkWeight} from "./js/graphLib.js";
import {mkGraph, applyLayout} from "./js/pendants.js";

const raw_clusters = FileAttachment("data/clusters.json").json();

const MAX_N = 7825;
const allTriples = filterByLinkWeight(
    pythagoreanTriples(MAX_N),
    {minWeight: 2}
);
```


```js
const clustered_triples = raw_clusters.reduce(
    (r, c) => {
        c.nodes.forEach(n => r.add(JSON.stringify(n)));
        return r;
    },
    new Set()
);

let free_triples = allTriples.filter(
    t => !clustered_triples.has(JSON.stringify(t))
);

for(let c of raw_clusters) {
    const edge_ix = groupByEdges(free_triples);
    const ext = [];
    for(let e of Object.keys(c.edge_weights)) {
        const nodes = edge_ix.get(Number.parseInt(e));
        if(nodes) {
            ext.push(nodes);
        }
    }
    ext.sort((a,b) => b.length - a.length);
    c.extensions = [new Set(), new Set()];
    let ext_i = 0;
    for(let ns of ext) {
        c.extensions[ext_i].add(ns[0]);
        if(c.extensions[ext_i].size == 13) {
            ext_i++;
            if(c.extensions.length <= ext_i) break;
        }
    }

    free_triples = free_triples
        .filter(t => !c.extensions.some(e => e.has(t)));
}

const clusters = raw_clusters; // this is for dependency ordering
```


```js
display({free_triples, clusters});

const clusterIx = view(
    Inputs.range(
        [0, clusters.length-1],
        {value: 0, step: 1, label: "Select a cluster"}
    )
);
const showExtension = view(
    Inputs.toggle({
        label: "Show extension",
        value: false
    })
);
```

```js
const cluster = clusters[clusterIx];
const graph = mkGraph(
    cluster.nodes
        .concat(showExtension ? [...cluster.extensions[0]] : [])
);
applyLayout(graph);
```

```js
display(cluster);
display(graphSVG(graph, cluster, {width}))
```

```js
function markComponent(graph, id) {
    const visitedNodes = new Set();
    for(let n of graph.nodes) {
        if(n.component === undefined) {
            visitedNodes.add(n);
            n.component = {id, depth: 0};
            break;
        }
    }

    if(visitedNodes.size == 0) return null;

    for(let depth = 1; ; depth++) {
        let hasNew = false;
        for(let e of graph.edges) {
            if(visitedNodes.has(e.source) && !visitedNodes.has(e.target)) {
                visitedNodes.add(e.target);
                e.target.component = {id, depth};
                hasNew = true;
            }
            else if(visitedNodes.has(e.target) && !visitedNodes.has(e.source)) {
                visitedNodes.add(e.source);
                e.source.component = {id, depth};
                hasNew = true;
            }
        }

        if(!hasNew) {
            return visitedNodes;
        }
    }
}

function markComponents(graph) {
    let c = 0;
    while(markComponent(graph, c) != null) c++;
}


markComponents(graph);
```

```js
function graphSVG(graph, cluster, {width}) {
  const [mx, my, w, h] = graph.viewBox;
  const margin = 10;
  const viewBox = [mx - margin, my - margin, w + 2*margin, h + 2*margin];

  const svg = d3.create("svg")
      .attr("preserveAspectRatio", "xMidYMid")
      .attr("viewBox", viewBox)
      .attr("width", width / (1 + 4*Math.exp(-w/600)));

  const edgeColor = e => d3.interpolateBlues(e.weight / graph.maxLabelWeight);

  svg.selectAll("edge")
      .data(graph.edges)
      .join("line")
      .attr("stroke", edgeColor)
      .attr("stroke-width", 1)
      .attr("x1", e => e.source.x)
      .attr("y1", e => e.source.y)
      .attr("x2", e => e.target.x)
      .attr("y2", e => e.target.y)
      .append("title").text(e => `l:${e.label} w:${e.weight}`);

  const nodeColor = n =>
    cluster.extensions[0].has(n.labels)
        ? "green"
        : d3.schemeSet1[n.component.id];

  const nodeSize = n =>
    cluster.extensions[0].has(n.labels)
        ? 4
        : n.weight / 2;

  svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", nodeColor)
      .attr("cx", n => n.x)
      .attr("cy", n => n.y)
      .attr("r",  nodeSize)
      .append("title")
        .text(n => JSON.stringify(n.labels))

  return svg.node();
}
```


```js
const cluster_nodes = clusters.map((c, ix) => ({
    ix,
    color: "red",
    labels: new Set(c.nodes.flat()),
}));

const extension_nodes = clusters.map((c, ix) => ({
    ix: ix + "_0",
    color: "green",
    labels: new Set([...c.extensions[0]].flat()),
})).concat(
    clusters.map((c, ix) => ({
        ix: ix + "_1",
        color: "blue",
        labels: new Set([...c.extensions[1]].flat()),
})));

const joined_nodes = cluster_nodes.concat(extension_nodes);
const joined_edges = [];

for(let i = 0; i < joined_nodes.length-1; i++) {
    const source = joined_nodes[i];
    for(let j = i+1; j < joined_nodes.length; j++) {
        const target = joined_nodes[j];
        const common_labels = source.labels
            .intersection(target.labels);
        if (common_labels.size >= 6) {
            joined_edges.push({
                source, target,
                weight: common_labels.size,
            });
        }
    }
}

const joined_graph = {
    nodes: joined_nodes,
    edges: joined_edges,
    maxEdgeWeight: Math.max(...joined_edges.map(e => e.weight))
};
```

```js
display(joined_graph);

function joinedSVG(graph) {
  const [mx, my, w, h] = graph.viewBox;
  const margin = 10;
  const viewBox = [mx - margin, my - margin, w + 2*margin, h + 2*margin];

  const svg = d3.create("svg")
      .attr("preserveAspectRatio", "xMidYMid")
      .attr("viewBox", viewBox)
      .attr("width", width / (1 + 4*Math.exp(-w/600)));

  const edgeColor = e => d3.interpolateBlues(e.weight / graph.maxEdgeWeight);

  svg.selectAll("edge")
      .data(graph.edges)
      .join("line")
      .attr("stroke", edgeColor)
      .attr("stroke-width", 4)
      .attr("x1", e => e.source.x)
      .attr("y1", e => e.source.y)
      .attr("x2", e => e.target.x)
      .attr("y2", e => e.target.y)
      .append("title").text(e => `w:${e.weight}`);

  svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", n => n.ix < 5 ? "orange" : n.color)
      .attr("cx", n => n.x)
      .attr("cy", n => n.y)
      .attr("r", n => n.ix < 5 ? 10 : 5)
      .append("title").text(n => `ix:${n.ix}`);

  return svg.node();
}

```


```js
applyLayout(joined_graph);
display(joinedSVG(joined_graph));
```
