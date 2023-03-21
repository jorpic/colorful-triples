import Graph from "graphology";
import Sigma from "sigma";

import random from "graphology-layout/random";
import {assignLayout} from "graphology-layout/utils";
// import forceAtlas2 from "graphology-layout-forceatlas2";

import triples from "./triples";

const N = 7825;
const graph = new Graph();
graph.import(triples.graph(N));

// We initialize the graph with random positions because FA2Layout has an
// edge-case where the layout cannot be computed if all of your nodes starts
// with x=0 and y=0.
assignLayout(graph, random(graph));


new Sigma(graph, document.getElementById("main") as HTMLElement);
