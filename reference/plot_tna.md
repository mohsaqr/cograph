# TNA-Style Network Plot (qgraph Compatible)

Plots a network with
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) and TNA
styling, using qgraph argument names such as `vsize`, `edge.color` and
`pie`. `tplot()` is an alias.

## Usage

``` r
plot_tna(
  x,
  color = NULL,
  labels = NULL,
  layout = "oval",
  theme = "colorblind",
  mar = c(0.1, 0.1, 0.1, 0.1),
  cut = NULL,
  edge.label.position = 0.7,
  edge.label.cex = 0.6,
  edge.color = COGRAPH_SCALE$tna_edge_color,
  vsize = 7,
  pie = NULL,
  pieColor = NULL,
  lty = NULL,
  directed = NULL,
  minimum = NULL,
  posCol = NULL,
  negCol = NULL,
  arrowAngle = NULL,
  title = NULL,
  ...
)

tplot(
  x,
  color = NULL,
  labels = NULL,
  layout = "oval",
  theme = "colorblind",
  mar = c(0.1, 0.1, 0.1, 0.1),
  cut = NULL,
  edge.label.position = 0.7,
  edge.label.cex = 0.6,
  edge.color = COGRAPH_SCALE$tna_edge_color,
  vsize = 7,
  pie = NULL,
  pieColor = NULL,
  lty = NULL,
  directed = NULL,
  minimum = NULL,
  posCol = NULL,
  negCol = NULL,
  arrowAngle = NULL,
  title = NULL,
  ...
)
```

## Arguments

- x:

  A weight matrix (adjacency matrix) or tna object

- color:

  Node fill colors

- labels:

  Node labels

- layout:

  Layout: "oval" (default), "circle", "spring", or a coordinate matrix.

- theme:

  Plot theme. Default "colorblind".

- mar:

  Plot margins (numeric vector of length 4)

- cut:

  Edge emphasis threshold, passed to `edge_cutoff` of
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md).

- edge.label.position:

  Position of edge labels along edge (0-1)

- edge.label.cex:

  Edge label size multiplier

- edge.color:

  Edge colors

- vsize:

  Node size

- pie:

  Donut fill values in 0-1 (for example, initial probabilities).

- pieColor:

  Donut fill colors.

- lty:

  Line type for edges: 1 solid, 2 dashed, 3 dotted, 4 dotdash, 5
  longdash, 6 twodash, or a line-type name.

- directed:

  Logical, is the graph directed? NULL (default) treats a symmetric
  weight matrix as undirected and any other input as directed.

- minimum:

  Minimum absolute edge weight to display, passed to `threshold` of
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md).

- posCol:

  Color for positive edges

- negCol:

  Color for negative edges

- arrowAngle:

  Arrow head angle in radians. Default NULL, which leaves
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md)'s own
  `arrow_angle` default of pi/6 (30 degrees) in place.

- title:

  Plot title

- ...:

  Additional arguments passed to
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md). They take
  precedence over the translated qgraph arguments.

## Value

The `cograph_network` object returned by
[`splot()`](https://sonsoles.me/cograph/reference/splot.md), invisibly.

## Examples

``` r
plot_tna(regulation_net)

```
