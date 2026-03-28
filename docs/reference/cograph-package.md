# cograph: Modern Network Visualization for R

A modern, extensible network visualization package that provides
high-quality static and interactive network plots. cograph accepts
adjacency matrices, edge lists, or igraph objects and offers
customizable layouts, node shapes, edge styles, and themes.

## Main Functions

- [`cograph`](http://sonsoles.me/cograph/reference/cograph.md): Main
  entry point for creating network visualizations

- [`sn_layout`](http://sonsoles.me/cograph/reference/sn_layout.md):
  Apply layout algorithms

- [`sn_nodes`](http://sonsoles.me/cograph/reference/sn_nodes.md):
  Customize node aesthetics

- [`sn_edges`](http://sonsoles.me/cograph/reference/sn_edges.md):
  Customize edge aesthetics

- [`sn_theme`](http://sonsoles.me/cograph/reference/sn_theme.md): Apply
  visual themes

- [`sn_render`](http://sonsoles.me/cograph/reference/soplot.md): Render
  to device

- [`sn_ggplot`](http://sonsoles.me/cograph/reference/sn_ggplot.md):
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

- Santtu Tikka
