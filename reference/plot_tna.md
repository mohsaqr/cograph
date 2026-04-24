# TNA-Style Network Plot (qgraph Compatible)

A drop-in replacement for qgraph::qgraph() that uses cograph's splot
engine. Accepts qgraph parameter names for seamless migration from
qgraph to cograph.

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

  Layout: "circle", "spring", "oval", or a coordinate matrix

- theme:

  Plot theme ("colorblind", "gray", etc.)

- mar:

  Plot margins (numeric vector of length 4)

- cut:

  Edge emphasis threshold

- edge.label.position:

  Position of edge labels along edge (0-1)

- edge.label.cex:

  Edge label size multiplier

- edge.color:

  Edge colors

- vsize:

  Node size

- pie:

  Pie/donut fill values (e.g., initial probabilities)

- pieColor:

  Pie/donut segment colors

- lty:

  Line type for edges (1=solid, 2=dashed, 3=dotted)

- directed:

  Logical, is the graph directed?

- minimum:

  Minimum edge weight to display

- posCol:

  Color for positive edges

- negCol:

  Color for negative edges

- arrowAngle:

  Arrow head angle in radians. Default pi/6 (30 degrees).

- title:

  Plot title

- ...:

  Additional arguments passed to splot()

## Value

Invisibly returns the cograph_network object from splot().

Invisibly returns the cograph_network object from splot().

## Examples

``` r
# Simple usage
m <- matrix(runif(25), 5, 5)
plot_tna(m)


# With qgraph-style parameters
plot_tna(m, vsize = 15, edge.label.cex = 2, layout = "circle")


# With custom colors
plot_tna(m, color = rainbow(5), vsize = 10)


m <- matrix(runif(25), 5, 5)
tplot(m)
```
