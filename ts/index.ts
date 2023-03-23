import Graph from "graphology";
import Sigma from "sigma";

import random from "graphology-layout/random";
import {assignLayout} from "graphology-layout/utils";
import forceAtlas2 from "graphology-layout-forceatlas2";

import {bfsFromNode} from "graphology-traversal/bfs";

import Triples from "./triples";


function fa2Layout(graph) {
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
  console.log(positions);
  assignLayout(graph, positions);
  return graph;
}


function show(graph) {
  // NB. don't forget to .kill() it
  return new Sigma(
    graph,
    document.getElementById("main") as HTMLElement,
    {renderEdgeLabels: true}
  );
}


function showNodeNeighbourhood(graph, key, maxDepth=2) {
  const triples = {};

  const layers = new Array(maxDepth+1).fill(0).map(_ => []);
  bfsFromNode(graph, key, (node, attr, depth) => {
    const done = depth > maxDepth;
    if(!done) {
      triples[node] = attr.value;
      layers[depth].push(node);
    }
    return done;
  });

  const g = Graph.from(
    Triples.toGraphData(triples),
    {type: "mixed"}
  );

  const positions = {};
  for(let i in layers) {
    const len = layers[i].length;
    layers[i].forEach((k, j) => {
      const phi = 2 * Math.PI * j / (len + 1);
      const r = i*10;
      positions[k] = {
        x: r * Math.cos(phi),
        y: r * Math.sin(phi),
      };
    })
  }

  console.log(positions);

  assignLayout(g, positions);
  show(g);
}

function customLayout(graph) {
  const positions = {
  };
  assignLayout(graph, positions);
  return graph;
}


const N = 7825;
const triples = Triples.allUpTo(N);
triples.logStats();
triples.dropPendants().logStats();
const bigGraph = Graph.from(triples.toGraphData(), {type: "mixed"});


const key = "44,240,244";
showNodeNeighbourhood(bigGraph, key);
