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
    c.extension = new Set();
    for(let ns of ext) {
        c.extension.add(ns[0]);
        if(c.extension.size == 13) {
            break;
        }
    }

    free_triples = free_triples
        .filter(t => !c.extension.has(t));
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
```

```js
const cluster = clusters[clusterIx];
const graph = mkGraph(
    cluster.nodes
        .concat([...cluster.extension])
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
    cluster.extension.has(n.labels)
        ? "green"
        : d3.schemeSet1[n.component.id];

  const nodeSize = n =>
    cluster.extension.has(n.labels)
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
const cluster_nodes = clusters.map(c => ({
    is_cluster: true,
    labels: new Set(c.nodes.flat()),
}));

const extension_nodes = clusters.map(c => ({
    is_extension: true,
    labels: new Set([...c.extension].flat()),
}));

const joined_nodes = cluster_nodes.concat(extension_nodes);
const joined_edges = [];

for(let i = 0; i < joined_nodes.length-1; i++) {
    const source = joined_nodes[i];
    for(let j = i+1; j < joined_nodes.length; j++) {
        const target = joined_nodes[j];
        const common_labels = source.labels
            .intersection(target.labels);
        if (common_labels.size > 5) {
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

  const nodeColor = n => n.is_cluster ? "red" : "green";

  svg.selectAll("node")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", nodeColor)
      .attr("cx", n => n.x)
      .attr("cy", n => n.y)
      .attr("r",  4);

  return svg.node();
}

```


```js
applyLayout(joined_graph);
display(joinedSVG(joined_graph));
```
