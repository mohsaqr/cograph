# cograph: Modern Network Visualization for R

A modern, extensible network visualization package that provides
high-quality static network plots and ggplot2 conversions. cograph
accepts adjacency matrices, edge lists, or igraph objects and offers
customizable layouts, node shapes, edge styles, and themes.

## Main Functions

- [`cograph`](https://sonsoles.me/cograph/reference/cograph.md): Main
  entry point for creating network visualizations

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

- `viridis`: Viridis-based colour theme

- `nature`: Nature-inspired colour theme

## Weight conventions

cograph's analytic functions follow a single convention for edge
weights:

- **Semantics.** A weight is a *strength*: higher weight means a
  stronger connection (larger transition probability, thicker
  correlation, stronger tie). This matches the qgraph / tna convention
  and the intuition of most user-facing inputs.

- **Path-based measures** (betweenness, closeness, harmonic,
  eccentricity, stress, load, radiality, etc.) invert weights to
  *distances* via `1 / weight ^ alpha`. The `alpha` argument (default 1)
  tunes how strongly weight differences compress paths. Controlled by
  the `invert_weights` argument, which auto-detects to `TRUE` for tna
  objects and `FALSE` for matrices/igraph (matching native igraph / sna
  defaults).

- **Non-path measures** (degree, strength, eigenvector, PageRank,
  transitivity, modularity, ...) use the raw weights as-is without
  inversion.

- **Unweighted override.** Passing `weights = NA` to any analytic
  function forces unweighted behavior regardless of what is attached to
  the graph.

Individual functions may document exceptions in their own help pages.
Any deviation from this convention is a bug — please report.

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
