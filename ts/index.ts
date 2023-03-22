import Graph from "graphology";
import Sigma from "sigma";

import random from "graphology-layout/random";
import {assignLayout} from "graphology-layout/utils";
import forceAtlas2 from "graphology-layout-forceatlas2";

import Triples from "./triples";

const N = 7825;
const graph = new Graph();
const triples = new Triples(N);
triples.logStats();
triples.dropPendants().logStats();
graph.import(triples.toGraphData());

// We initialize the graph with random positions because FA2Layout has an
// edge-case where the layout cannot be computed if all of your nodes starts
// with x=0 and y=0.
random.assign(graph);

const sensibleSettings = forceAtlas2.inferSettings(graph);
const positions = forceAtlas2(graph, {
  iterations: 20,
  settings: {
    ...sensibleSettings,
    adjustSizes: true,
    barnesHutOptimize: true,
  }
});
assignLayout(graph, positions);


new Sigma(
  graph,
  document.getElementById("main") as HTMLElement,
  {renderEdgeLabels: true}
);
