# Convert a qgraph object to cograph parameters

Extracts the network, layout, and all relevant arguments from a qgraph
object and passes them to a cograph plotting engine. Reads resolved
values from `graphAttributes` rather than raw `Arguments`.

## Usage

``` r
from_qgraph(
  qgraph_object,
  engine = c("splot", "soplot"),
  plot = TRUE,
  weight_digits = 2,
  show_zero_edges = FALSE,
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
  that round to zero are removed unless `show_zero_edges = TRUE`.

- show_zero_edges:

  Logical. If TRUE, keep edges even if their weight rounds to zero.
  Default: FALSE.

- ...:

  Override any extracted parameter. Use qgraph-style names (e.g.,
  `minimum`) or cograph names (e.g., `threshold`).

## Value

Invisibly, a named list of cograph parameters that can be passed to
[`splot()`](http://sonsoles.me/cograph/reference/splot.md) or
[`soplot()`](http://sonsoles.me/cograph/reference/soplot.md).

## Details

### Parameter Mapping

The following qgraph parameters are automatically extracted and mapped
to cograph equivalents:

**Node properties:**

- `labels`/`names` `->` `labels`

- `color` `->` `node_fill`

- `width` `->` `node_size` (scaled by 1.3x)

- `shape` `->` `node_shape` (mapped to cograph equivalents)

- `border.color` `->` `node_border_color`

- `border.width` `->` `node_border_width`

- `label.cex` `->` `label_size`

- `label.color` `->` `label_color`

**Edge properties:**

- `labels` `->` `edge_labels`

- `label.cex` `->` `edge_label_size` (scaled by 0.5x)

- `lty` `->` `edge_style` (numeric to name conversion)

- `curve` `->` `curvature`

- `asize` `->` `arrow_size` (scaled by 0.3x)

**Graph properties:**

- `minimum` `->` `threshold`

- `maximum` `->` `maximum`

- `groups` `->` `groups`

- `directed` `->` `directed`

- `posCol`/`negCol` `->` `edge_positive_color`/`edge_negative_color`

**Pie/Donut:**

- `pie` values `->` `donut_fill` with `donut_inner_ratio=0.8`

- `pieColor` `->` `donut_color`

### Important Notes

- **edge_color and edge_width are NOT extracted** because qgraph bakes
  its cut-based fading into these vectors, producing near-invisible
  edges. cograph applies its own weight-based styling instead.

- The `cut` parameter is also not passed because it causes faint edges
  with hanging labels.

- Layout coordinates from qgraph are preserved with `rescale=FALSE`.

- If you override layout, rescale is automatically re-enabled.

## See also

[`cograph`](http://sonsoles.me/cograph/reference/cograph.md) for
creating networks from scratch,
[`splot`](http://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](http://sonsoles.me/cograph/reference/soplot.md) for plotting
engines, [`from_tna`](http://sonsoles.me/cograph/reference/from_tna.md)
for tna object conversion

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert and plot a qgraph object
library(qgraph)
adj <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
q <- qgraph(adj)
from_qgraph(q)  # Plots with splot

# Use soplot engine instead
from_qgraph(q, engine = "soplot")

# Override extracted parameters
from_qgraph(q, node_fill = "steelblue", layout = "circle")

# Extract parameters without plotting
params <- from_qgraph(q, plot = FALSE)
names(params)  # See what was extracted

# Works with themed qgraph objects
q_themed <- qgraph(adj, theme = "colorblind", posCol = "blue")
from_qgraph(q_themed)
} # }
```
