# Plot Mixed Network

Plots one network that combines the edges of a symmetric matrix, shown
as straight undirected edges, with the edges of an asymmetric matrix,
shown as curved directed edges.

## Usage

``` r
plot_mixed_network(
  sym_matrix,
  asym_matrix,
  layout = "oval",
  sym_color = "ivory4",
  asym_color = COGRAPH_SCALE$tna_edge_color,
  curvature = 0.3,
  edge_width = NULL,
  node_size = 7,
  title = NULL,
  threshold = 0,
  edge_labels = TRUE,
  arrow_size = 0.61,
  edge_label_size = 0.6,
  edge_label_position = 0.7,
  initial = NULL,
  ...
)
```

## Arguments

- sym_matrix:

  A symmetric matrix of undirected relationships. Each non-zero pair is
  plotted once as a straight edge without arrows.

- asym_matrix:

  An asymmetric matrix of directed relationships, with the same
  dimensions as `sym_matrix`. Its edges are plotted as curved arrows.

- layout:

  Layout algorithm or coordinate matrix. Default "oval".

- sym_color:

  Color for undirected edges. Default `"ivory4"`.

- asym_color:

  Color for directed edges. Either a single color, or two colors for
  reciprocal pairs: the first for the edge from the lower-indexed node
  and the second for the reverse edge. Non-reciprocal edges use the
  first color. Default `"#003355"` (dark blue, the TNA edge color).

- curvature:

  Curvature magnitude for directed edges. Default 0.3.

- edge_width:

  Edge width(s). If NULL (default), widths scale with edge weight as in
  TNA plots. A numeric value overrides the scaling.

- node_size:

  Node size. Default 7.

- title:

  Plot title. Default NULL.

- threshold:

  Minimum absolute edge weight to display. Values with
  `abs(value) < threshold` are set to zero, and zero-weight edges are
  not plotted. Default 0.

- edge_labels:

  Show edge weight labels. Default TRUE.

- arrow_size:

  Arrow head size for directed edges. Default 0.61 (TNA style).

- edge_label_size:

  Size of edge labels. Default 0.6.

- edge_label_position:

  Position of edge labels along edge (0-1). Default 0.7.

- initial:

  Optional numeric vector of initial state probabilities. A named vector
  is matched to the node names, with missing states set to 0; an unnamed
  vector is used in node order. Nodes are then plotted as donuts filled
  in proportion to the initial probability. A warning is issued when the
  values do not sum to 1 (tolerance 0.01). Default NULL.

- ...:

  Additional arguments passed to
  [`splot`](https://sonsoles.me/cograph/reference/splot.md).

## Value

Invisibly, a list with three elements: `edges`, a data frame with one
row per plotted edge and columns `from`, `to` (node indices), `weight`,
`type` ("undirected" or "directed") and `color`; and `sym_matrix` and
`asym_matrix`, the input matrices after thresholding.

## Examples

``` r
plot_mixed_network(symmetrize(regulation_net, keep_format = TRUE), regulation_net)

```
