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

  Layout algorithm name such as "circle", "oval", "spring", "groups",
  "grid", "random", "star", "bipartite" or "gephi" (see
  [`list_layouts`](https://sonsoles.me/cograph/reference/layout_registry.md));
  a coordinate matrix or data frame; or an igraph layout function, name
  or two-letter code. Default NULL computes no layout. A layout can also
  be set later with
  [`sn_layout`](https://sonsoles.me/cograph/reference/sn_layout.md).

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

  Logical or character. Used for tna input only. If FALSE (default),
  every transition from tna sequence data is a separate edge. If TRUE or
  a string ("sum", "mean", "max", "min"), duplicate edges are
  aggregated, and TRUE uses "sum".

- ...:

  Additional arguments passed to the layout function.

## Value

A `cograph_network` object. It is a list with the elements `nodes`,
`edges`, `directed`, `weights`, `data`, `meta` and `node_groups`.

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
cograph(regulation_net) |> splot(layout = "circle")
```
