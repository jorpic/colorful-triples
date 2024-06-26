import * as d3 from "npm:d3";

export function triplesToGraph(triples) {
  const nodes = triples.map((links, id) => ({links, id}));
  const edges = [];
  for(let i = 0; i < nodes.length; i++) {
    const a = nodes[i];
    for(let j = i+1; j < nodes.length; j++) {
      for(let x of a.links) {
        const b = nodes[j];
        if(b.links.indexOf(x) >= 0) {
          edges.push({source: a.id, target: b.id, label: x});
          break;
        }
      }
    }
  }
  return {nodes, edges};
}

export function lineLayout(graph, {w, h}) {
  const r = 4;

  const xScale = d3.scalePoint()
    .range([r, w-r])
    .domain(graph.nodes.map(n => n.id));

  const svg = d3.create("svg")
      .attr("width", w)
      .attr("height", h);

    svg
    .selectAll('link')
    .data(graph.edges)
    .join('path')
      .attr('d', e => {
        const startX = xScale(e.source);
        const endX = xScale(e.target);
        return [
          'M', startX, h-r,
          'A',
          (startX - endX)/2, ',',
          (startX - endX)/2, 0, 0, ',',
          1,
          endX, ',', h-r]
        .join(' ');
    })
    .style("fill", "none")
    .attr("stroke", "black");

  svg
    .selectAll("circle")
    .data(graph.nodes)
    .join("circle")
      .attr("cx", n => xScale(n.id))
      .attr("cy", h-r)
      .attr("r", r)
      .style("fill", "green");

  return svg.node();
}

// expects graph in a following format
// {nodes, edges}
//   - `nodes` is an array of objects that will be enriched with `{x, y}` by
// `forceSimulation`
//   - `edges` is an array of objects `{source, target}`, where `source` and
//   `target` are indexes in the `nodes` array or references to nodes.
export function forceLayout(graph, {w, h, min_weight}) {
  const svg = d3.create("svg")
      .attr("width", w)
      .attr("height", h);

  const force = d3
    .forceSimulation(graph.nodes)
    .force("link",
      d3.forceLink()
        .links(graph.edges.filter(e => min_weight <= e.weight))
        .distance(10))
    .force(
      "charge",
      d3.forceManyBody().strength(-150)
    )
    .force("x", d3.forceX(w / 2))
    .force("y", d3.forceY(h / 2));

  force.tick(100);

  svg.selectAll("grey-line")
    .data(graph.edges.filter(e => e.weight < min_weight))
    .join("line")
      .attr("stroke", "lightgrey")
      .attr("stroke-width", 0.1)
      .attr("x1", l => l.source.x)
      .attr("y1", l => l.source.y)
      .attr("x2", l => l.target.x)
      .attr("y2", l => l.target.y);

  svg.selectAll("orange-line")
    .data(graph.edges.filter(e => min_weight <= e.weight))
    .join("line")
      .attr("stroke", l => d3.schemeOranges[9][l.weight])
      .attr("stroke-width", l => l.weight/2)
      .attr("x1", l => l.source.x)
      .attr("y1", l => l.source.y)
      .attr("x2", l => l.target.x)
      .attr("y2", l => l.target.y)
      .append("title").text(l => l.weight);

  svg.selectAll("circle")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", "green")
      .attr("cx", c => c.x)
      .attr("cy", c => c.y)
      .attr("r", c => c.weight / 6)
      .append("title").text(c => c.weight);

  return svg.node();
}
