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

export function lineLayout(graph) {
  const w = 800;
  const h = 400;

  const xScale = d3.scalePoint()
    .range([10, w-10])
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
          'M', startX, h-10,
          'A',
          (startX - endX)/2, ',',
          (startX - endX)/2, 0, 0, ',',
          1,
          endX, ',', h-10]
        .join(' ');
    })
    .style("fill", "none")
    .attr("stroke", "black");

  svg
    .selectAll("circle")
    .data(graph.nodes)
    .join("circle")
      .attr("cx", n => xScale(n.id))
      .attr("cy", h-10)
      .attr("r", 8)
      .style("fill", "green");

  return svg.node();
}

export function forceLayout(graph) {
  const w = 1000;
  const h = 1000;

  const svg = d3.create("svg")
      .attr("width", w)
      .attr("height", h);

  const force = d3
    .forceSimulation(graph.nodes)
    .force("link",
      d3.forceLink().links(graph.edges).distance(25))
    .force(
      "charge",
      d3.forceManyBody().strength(-300)
    )
    .force("x", d3.forceX(w / 2))
    .force("y", d3.forceY(h / 2));

  force.tick(100);

  svg.selectAll("line")
    .data(graph.edges)
    .join("line")
      .attr("stroke", "grey")
      .attr("x1", l => l.source.x)
      .attr("y1", l => l.source.y)
      .attr("x2", l => l.target.x)
      .attr("y2", l => l.target.y);

  svg.selectAll("circle")
    .data(graph.nodes)
    .join("circle")
      .attr("fill", "green")
      .attr("cx", c => c.x)
      .attr("cy", c => c.y)
      .attr("r", c => 4);

  return svg.node();
}
