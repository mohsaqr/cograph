# Simplicial Complex Visualization

Visualize higher-order pathways as smooth blobs overlaid on a network
layout. Source nodes are blue, target nodes are red.

## Usage

``` r
plot_simplicial(
  x = NULL,
  pathways = NULL,
  method = "hon",
  max_pathways = 10L,
  layout = "circle",
  labels = NULL,
  node_color = "#4A7FB5",
  target_color = "#E8734A",
  ring_color = "#F5A623",
  node_size = 22,
  label_size = 5,
  blob_alpha = 0.25,
  blob_colors = NULL,
  blob_linetype = NULL,
  blob_linewidth = 0.7,
  blob_line_alpha = 0.8,
  shadow = TRUE,
  title = NULL,
  dismantled = FALSE,
  ncol = NULL,
  ...
)
```

## Arguments

- x:

  A network object: `tna`, `netobject`, matrix, `igraph`,
  `cograph_network`, `net_hon`, or `net_hypa`. When `x` is a `tna` or
  `netobject` with sequence data and `pathways` is `NULL`, higher-order
  pathways are built automatically using the `method` parameter.

- pathways:

  Character vector of pathway strings, a list of character vectors, or a
  `net_hon` / `net_hypa` object. String separators: `"A B -> C"`,
  `"A, B, C"`, `"A - B - C"`, `"A B C"`. Last state is the target. When
  `NULL` and `x` is a model with sequence data, pathways are built
  automatically.

- method:

  Pathway source when auto-building from a `tna`/`netobject`: `"hon"`
  (default, higher-order network), `"hypa"` (anomalous paths via
  hypergeometric null), or `"rules"` (association-rule itemsets via
  [`Nestimate::association_rules`](https://rdrr.io/pkg/Nestimate/man/association_rules.html);
  rules are rendered as single-colored blobs because itemsets are
  undirected).

- max_pathways:

  Maximum number of pathways to display. HON pathways are ranked by
  count, HYPA by anomaly ratio. `NULL` shows all. Default `10`.

- layout:

  `"circle"` (default) or a coordinate matrix.

- labels:

  Display labels. `NULL` uses state names.

- node_color:

  Source node fill color.

- target_color:

  Target node fill color.

- ring_color:

  Donut ring color.

- node_size:

  Node point size.

- label_size:

  Label text size.

- blob_alpha:

  Blob fill transparency.

- blob_colors:

  Blob fill colors (recycled).

- blob_linetype:

  Blob border line styles (recycled).

- blob_linewidth:

  Blob border line width.

- blob_line_alpha:

  Blob border line transparency.

- shadow:

  Draw soft drop shadows?

- title:

  Plot title.

- dismantled:

  If `TRUE`, one panel per pathway arranged in a grid layout.

- ncol:

  Number of columns in the grid when `dismantled = TRUE`. Default `NULL`
  auto-selects based on the number of pathways.

- ...:

  Additional arguments passed to
  [`Nestimate::build_hon()`](https://rdrr.io/pkg/Nestimate/man/build_hon.html)
  or
  [`Nestimate::build_hypa()`](https://rdrr.io/pkg/Nestimate/man/build_hypa.html)
  when auto-building.

## Value

A `ggplot` object (or combined grid if dismantled), invisibly.

## Details

Supports direct use with `tna` and `netobject` models: when `x` has
sequence data, HON or HYPA pathways are built automatically (requires
the Nestimate package). Pathways can also be passed as `net_hon` or
`net_hypa` objects, with labels auto-translated when `x` is a
`tna`/`netobject`.

## Examples

``` r
if (FALSE) { # \dontrun{
mat <- matrix(runif(16), 4, 4,
              dimnames = list(LETTERS[1:4], LETTERS[1:4]))
diag(mat) <- 0
plot_simplicial(mat, c("A B -> C", "B C -> D"))

# Direct from tna model (requires Nestimate):
# model <- tna::tna(tna::group_regulation)
# plot_simplicial(model, dismantled = TRUE)
# plot_simplicial(model, method = "hypa")

# With pre-built HON + tna for label translation:
# hon <- Nestimate::build_hon(as.data.frame(model$data))
# plot_simplicial(model, hon, dismantled = TRUE)
} # }
```
