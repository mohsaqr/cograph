# Convert a qgraph object to cograph parameters

Extracts the network, the layout and the plotting arguments from a
qgraph object and passes them to a cograph plotting engine. Node, edge
and graph settings are read from the resolved `graphAttributes` of the
object. The colors `posCol`, `negCol` and the `theme` are read from its
`Arguments`.

## Usage

``` r
from_qgraph(
  qgraph_object,
  engine = c("splot", "soplot"),
  plot = TRUE,
  weight_digits = 2,
  show_zero_edges = FALSE,
  preserve_node_size = FALSE,
  ...
)
```

## Arguments

- qgraph_object:

  Return value of
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html)

- engine:

  Which cograph renderer to use: `"splot"` or `"soplot"`. Default:
  `"splot"`.

- plot:

  Logical. If TRUE (default), immediately plot using the chosen engine.

- weight_digits:

  Number of decimal places to round edge weights to. Default 2. Edges
  whose weight rounds to zero at this precision are dropped unless
  `show_zero_edges = TRUE`.

- show_zero_edges:

  Logical. A zero weight means that the edge is absent, so an edge whose
  weight rounds to zero at `weight_digits` is dropped. With `TRUE` each
  such edge is plotted at the smallest magnitude that `weight_digits`
  can express, with its original sign. Other weights are unchanged.
  Default: `FALSE`.

- preserve_node_size:

  Logical. If TRUE, use the node sizes extracted from the qgraph object.
  Default FALSE uses cograph's standard sizing.

- ...:

  Overrides for any extracted parameter, given by cograph name (for
  example `threshold`). The qgraph names `minimum` and `cut` are
  translated to `threshold` and `edge_cutoff`.

## Value

Invisibly, a named list of plotting parameters (the weight matrix `x`,
`weight_digits`, the layout and the extracted node, edge and graph
settings, after the overrides in `...`). It can be passed to
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) with
[`do.call()`](https://rdrr.io/r/base/do.call.html). An input without an
`Arguments` field that does not inherit from `"qgraph"` raises an error.

## Details

### Parameter Mapping

The following qgraph parameters are extracted and mapped to their
cograph equivalents.

Node properties:

- `labels`/`names` `->` `labels`

- `color` `->` `node_fill`

- `width` `->` `node_size` (scaled by 1.3x) when
  `preserve_node_size = TRUE`

- `shape` `->` `node_shape` (mapped to cograph equivalents)

- `border.color` `->` `node_border_color`

- `border.width` `->` `node_border_width`

- `label.cex` `->` `label_size`

- `label.color` `->` `label_color`

Edge properties:

- `labels` `->` `edge_labels`

- `label.cex` `->` `edge_label_size` (scaled by 0.5x)

- `lty` `->` `edge_style` (numeric to name conversion)

- `curve` `->` `curvature` (only when qgraph resolved a single curvature
  for the whole graph)

- `asize` `->` `arrow_size` (scaled by 0.3x)

- `edge.label.position` `->` `edge_label_position`

Graph properties:

- `minimum` `->` `threshold`

- `maximum` `->` `maximum`

- `groups` `->` `groups`

- `directed` `->` `directed`

- `posCol`/`negCol` `->` `edge_positive_color`/`edge_negative_color`

- `theme` `->` `theme`

Pie and donut:

- `pie` values `->` `donut_fill` with `donut_inner_ratio = 0.8` and
  `donut_empty = FALSE`

- `pieColor` `->` `donut_color`

### Settings that are not extracted

The edge colors and widths are not extracted, because qgraph stores them
with its `cut`-based fading applied. cograph styles the edges by weight
instead. The `cut` value is not extracted either.

### Layout

The qgraph layout coordinates are kept with `rescale = FALSE`. When
`layout` is supplied in `...`, `rescale` is removed and the new layout
is rescaled.

## See also

[`cograph`](https://sonsoles.me/cograph/reference/cograph.md) for
creating networks from scratch,
[`splot`](https://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) for plotting
engines, [`from_tna`](https://sonsoles.me/cograph/reference/from_tna.md)
for tna object conversion

## Examples

``` r
q <- qgraph::qgraph(regulation_net, DoNotPlot = TRUE)
from_qgraph(q)
```
