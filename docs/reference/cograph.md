# Create a Network Visualization

The main entry point for cograph. Accepts adjacency matrices, edge
lists, igraph, statnet network, qgraph, or tna objects and creates a
visualization-ready network object.

## Usage

``` r
cograph(
  input,
  layout = NULL,
  directed = NULL,
  nodes = NULL,
  seed = 42,
  simplify = FALSE,
  ...
)
```

## Arguments

- input:

  Network input. Can be:

  - A square numeric matrix (adjacency/weight matrix)

  - A data frame with edge list (from, to, optional weight columns)

  - An igraph object

  - A statnet network object

  - A qgraph object

  - A tna object

- layout:

  Layout algorithm: "circle", "spring", "groups", "grid", "random",
  "star", "bipartite", or "custom". Default NULL (no layout computed).
  Set to a layout name to compute immediately, or use sn_layout() later.

- directed:

  Logical. Force directed interpretation. NULL for auto-detect.

- nodes:

  Node metadata. Can be NULL or a data frame with node attributes. If
  data frame has a `label` or `labels` column, those are used for
  display.

- seed:

  Random seed for deterministic layouts. Default 42. Set NULL for
  random.

- simplify:

  Logical or character. If FALSE (default), every transition from tna
  sequence data is a separate edge. If TRUE or a string ("sum", "mean",
  "max", "min"), duplicate edges are aggregated.

- ...:

  Additional arguments passed to the layout function.

## Value

A cograph_network object that can be further customized and rendered.

## See also

[`splot`](https://sonsoles.me/cograph/reference/splot.md) for base R
graphics rendering,
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) for grid
graphics rendering,
[`sn_nodes`](https://sonsoles.me/cograph/reference/sn_nodes.md) for node
customization,
[`sn_edges`](https://sonsoles.me/cograph/reference/sn_edges.md) for edge
customization,
[`sn_layout`](https://sonsoles.me/cograph/reference/sn_layout.md) for
changing layouts,
[`sn_theme`](https://sonsoles.me/cograph/reference/sn_theme.md) for
visual themes,
[`sn_palette`](https://sonsoles.me/cograph/reference/sn_palette.md) for
color palettes,
[`from_qgraph`](https://sonsoles.me/cograph/reference/from_qgraph.md)
and [`from_tna`](https://sonsoles.me/cograph/reference/from_tna.md) for
converting external objects

## Examples

``` r
# From adjacency matrix (layout computed lazily on first plot)
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
cograph(adj) |> splot()


# From edge list
edges <- data.frame(from = c(1, 1, 2), to = c(2, 3, 3))
cograph(edges) |> splot(layout = "circle")


# Pipe-friendly customization
cograph(adj) |>
  sn_nodes(fill = "steelblue") |>
  sn_edges(color = "gray50") |>
  splot(layout = "circle")
```
