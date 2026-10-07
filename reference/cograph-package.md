# cograph: Modern Network Visualization for R

A modern, extensible network visualization package that provides
high-quality static network plots and ggplot2 conversions. cograph
accepts adjacency matrices, edge lists, igraph, statnet network, qgraph
and tna objects and offers customizable layouts, node shapes, edge
styles, and themes.

## Main Functions

- [`splot`](https://sonsoles.me/cograph/reference/splot.md): Plot a
  network with base R graphics

- [`soplot`](https://sonsoles.me/cograph/reference/soplot.md): Plot a
  network with grid graphics

- [`cograph`](https://sonsoles.me/cograph/reference/cograph.md): Create
  a network object for the builder functions

- [`sn_layout`](https://sonsoles.me/cograph/reference/sn_layout.md):
  Apply layout algorithms

- [`sn_nodes`](https://sonsoles.me/cograph/reference/sn_nodes.md):
  Customize node aesthetics

- [`sn_edges`](https://sonsoles.me/cograph/reference/sn_edges.md):
  Customize edge aesthetics

- [`sn_theme`](https://sonsoles.me/cograph/reference/sn_theme.md): Apply
  visual themes

- [`sn_render`](https://sonsoles.me/cograph/reference/soplot.md): Render
  to device

- [`sn_ggplot`](https://sonsoles.me/cograph/reference/sn_ggplot.md):
  Convert to ggplot2 object

## Layouts

cograph provides several built-in layouts:

- `circle`: Nodes arranged in a circle

- `spring`: Fruchterman-Reingold force-directed layout

- `groups`: Group-based circular layout

- `custom`: User-provided coordinates

## Themes

Built-in themes include:

- `classic`: Traditional network visualization style

- `colorblind`: Accessible color scheme

- `gray`: Grayscale theme

- `dark`: Dark background theme

- `minimal`: Clean, minimal style

- `viridis`: Viridis-based color theme

- `nature`: Nature-inspired color theme

## Weight conventions

In the analytic functions an edge weight is a strength. A higher weight
means a stronger connection, such as a larger transition probability or
a stronger correlation. This follows the convention of qgraph and tna.

Path-based measures such as betweenness, closeness, harmonic centrality
and eccentricity can convert weights to distances as `1 / weight^alpha`.
The `invert_weights` argument of
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
controls this conversion. Its default is `TRUE` for tna objects and
`FALSE` for other inputs, which matches igraph and sna. The `alpha`
argument (default 1) sets the exponent.

Measures that do not use paths, such as degree, strength, eigenvector
centrality, PageRank and transitivity, use the weights as given.
Functions with a `weights` argument, such as
[`shortest_paths`](https://sonsoles.me/cograph/reference/shortest_paths.md),
compute unweighted results when `weights = NA`. Individual help pages
document exceptions.

## See also

Useful links:

- <https://sonsoles.me/cograph/>

- <https://github.com/sonsoleslp/cograph>

- Report bugs at <https://github.com/sonsoleslp/cograph/issues>

## Author

**Maintainer**: Sonsoles López-Pernas <sonsoles.lopez@uef.fi>
\[copyright holder\]

Authors:

- Mohammed Saqr \[copyright holder\]
