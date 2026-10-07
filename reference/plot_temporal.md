# Temporal Network Prism (3D Glass Box)

Plots a network at different time points as vertical planes inside a 3D
oblique-projection box, with time running from left to right. Each
network plane extends into the depth of the box, and all planes share
one node layout. At least two time points are required.

## Usage

``` r
plot_temporal(
  x,
  time = NULL,
  slices = NULL,
  cumulative = FALSE,
  labels = NULL,
  layout = "spring",
  node_size = 2.5,
  node_color = "steelblue",
  color_by = c("layer", "node"),
  node_shape = 21,
  node_border = "gray30",
  edge_color = "#E41A1C",
  edge_width = 1.5,
  edge_alpha = 0.35,
  plane_color = "gray92",
  plane_alpha = 0.2,
  plane_border = "gray60",
  plane_lty = 2,
  box = TRUE,
  box_color = "gray40",
  connections = FALSE,
  connection_color = "gray50",
  connection_alpha = 0.15,
  minimum = 0,
  show_labels = FALSE,
  label_size = 0.4,
  title = NULL,
  angle = c(1, 0.7),
  seed = 42,
  ...
)
```

## Arguments

- x:

  An edge list data frame with columns `from`, `to`, optionally
  `weight`, and a time column; a `cograph_network` whose stored edge
  data contain the time column; or a list of network objects.

- time:

  Character. Name of the time column. Required for data frame input. It
  also labels the time axis.

- slices:

  Integer or NULL. Number of equal-width bins of the numeric time
  column. Empty bins are kept as empty planes. Default NULL uses the
  unique time values.

- cumulative:

  Logical. If TRUE, each plane contains all edges up to its time point.
  Default FALSE.

- labels:

  Character vector of layer labels, one per plane. The default NULL uses
  the time values, or `"T1"`, `"T2"`, ... for list input.

- layout:

  Character or matrix. A character value computes one
  Fruchterman-Reingold layout from the summed network. A two-column
  matrix supplies shared coordinates, one row per node. Default
  `"spring"`.

- node_size:

  Numeric. Point size (`cex`). Default 2.5.

- node_color:

  Character or vector. Node fill color. A single color applies to every
  node. An unnamed vector is recycled across layers and colors each
  plane as a whole, unless `color_by = "node"`. A named vector is
  matched to node names and gives each node the same color on every
  plane. A named vector that lacks a node raises an error of class
  `cograph_node_color_incomplete`. Default `"steelblue"`.

- color_by:

  One of `"layer"` (default) or `"node"`. It sets whether an unnamed
  `node_color` vector is recycled over layers or over nodes. A named
  `node_color` always colors by node.

- node_shape:

  Integer. Point shape (`pch`). Default 21 (filled circle).

- node_border:

  Character. Node border color. Default `"gray30"`.

- edge_color:

  Character or vector. Edge color (single or per-layer). Default
  `"#E41A1C"`.

- edge_width:

  Numeric. Maximum added edge width. An edge has width
  `0.3 + edge_width * abs(w) / max(abs(w))`. Default 1.5.

- edge_alpha:

  Numeric. Edge transparency (0-1). Default 0.35.

- plane_color:

  Character or vector. Plane fill color (single or per-layer). Default
  `"gray92"`.

- plane_alpha:

  Numeric. Plane fill transparency (0-1). Default 0.2.

- plane_border:

  Character. Plane border color. Default `"gray60"`.

- plane_lty:

  Integer. Plane border line type. Default 2 (dashed).

- box:

  Logical. Whether the 3D bounding box is plotted. Default TRUE.

- box_color:

  Character. Box edge color. Default `"gray40"`.

- connections:

  Logical. Whether lines connect each node to itself on the next plane.
  Default FALSE.

- connection_color:

  Character. Color of the connecting lines. Default `"gray50"`.

- connection_alpha:

  Numeric. Transparency of the connecting lines. Default 0.15.

- minimum:

  Numeric. Only edges with weight greater than `minimum` are plotted.
  Default 0.

- show_labels:

  Logical. Show node labels. Default FALSE.

- label_size:

  Numeric. Label text size. Default 0.4.

- title:

  Character or NULL. Plot title. Default NULL.

- angle:

  Numeric vector of length 2: `c(dz_x, dz_y)` controlling the oblique
  projection shear. Default `c(1.0, 0.7)`.

- seed:

  Integer or NULL. Random seed for the shared layout. The caller's
  random number state is restored on exit. NULL sets no seed. Default
  42.

- ...:

  Currently unused.

## Value

Invisibly, a list of weight matrices, one per plane, with one row and
one column per node.

## See also

[`plot_network_evolution`](https://sonsoles.me/cograph/reference/plot_network_evolution.md),
[`plot_mlna`](https://sonsoles.me/cograph/reference/plot_mlna.md)

## Examples

``` r
set.seed(1)
edges <- data.frame(
  from = sample(LETTERS[1:5], 30, replace = TRUE),
  to   = sample(LETTERS[1:5], 30, replace = TRUE),
  week = sample(1:3, 30, replace = TRUE))
cograph::plot_temporal(edges, time = "week")
```
