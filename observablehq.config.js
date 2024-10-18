export default {
  title: "Colorful triples",
  pages: [
    {
      name: "Pages",
      pages: [
        {name: "Knots", path: "/knots"},
        {name: "Drop 2-links", path: "/mergedNodes"},
        {name: "Pendants", path: "/pendants"},
        {name: "Neighborhoods", path: "/neighborhoods"},
        {name: "Cluster Extensions", path: "/cluster-ext"},
      ]
    }
  ],

  theme: "default", // try "light", "dark", "slate", etc.
  // header: "", // what to show in the header (HTML)
  // footer: "Built with Observable.", // what to show in the footer (HTML)
  // toc: true, // whether to show the table of contents
  pager: false, // whether to show previous & next links in the footer
  root: "docs", // path to the source root for preview
  output: "dist", // path to the output root for build
  search: false, // activate search
};
