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
  pathway_index = NULL,
  anomaly = c("all", "over", "under"),
  layout = "circle",
  labels = NULL,
  node_color = "#4A7FB5",
  target_color = "#E8734A",
  ring_color = "#F5A623",
  node_size = 22,
  label_size = 5,
  label_color = "#e8e8e8",
  target_label_color = NULL,
  label_halo = TRUE,
  label_halo_color = NULL,
  label_halo_width = 0.035,
  label_halo_alpha = 0.6,
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

  Character vector of pathway strings, a list of character vectors, a
  `net_hon` / `net_hypa` object, or any data.frame with a `path` column
  (e.g., the output of
  [`Nestimate::mogen_transitions()`](https://rdrr.io/pkg/Nestimate/man/mogen_transitions.html)).
  String separators: `"A B -> C"`, `"A -> B -> C"`, `"A, B, C"`,
  `"A - B - C"`, `"A B C"`. Last state is the target. When a data.frame
  is passed and a `count` column is present, rows are sorted by count
  descending before `max_pathways` is applied. When `NULL` and `x` is a
  model with sequence data, pathways are built automatically.

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

- pathway_index:

  Optional positive integer vector selecting ranked pathways after
  extraction and ranking, before `max_pathways` is applied. For example,
  `2` plots the second-ranked pathway and `2:4` plots pathways ranked
  second through fourth.

- anomaly:

  HYPA anomaly type to display when plotting a `net_hypa` object or
  auto-building HYPA pathways via `method = "hypa"`. One of `"all"`,
  `"over"`, or `"under"`. Default `"all"`. Ignored (with a warning) for
  non-HYPA inputs such as `net_hon`, `net_association_rules`,
  `net_link_prediction`, character pathway vectors, or `method = "hon"`
  / `"rules"`, which have no anomaly concept.

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

- label_color:

  Label text color (default `"#e8e8e8"`, very light grey). Light grey
  reads on both white and dark fills when the auto-contrast halo is
  enabled (it is by default). Applied to both source and target labels
  unless `target_label_color` overrides for targets.

- target_label_color:

  Target-node label color. `NULL` (default) reuses `label_color`.

- label_halo:

  Logical. Draw a contrasting halo behind each label so it stays
  readable on any fill — node disc, blob, or the white canvas. Default
  `TRUE`. The halo is the only reliable way to keep, e.g., white labels
  legible when `node_color` is also light.

- label_halo_color:

  Halo color. `NULL` (default) auto-picks black or white based on the
  luminance of `label_color`, so a white label gets a dark halo and vice
  versa.

- label_halo_width:

  Halo thickness in plot units. Default `0.035`; raise for chunkier
  outlines, lower for subtler ones, or set to `0` to disable without
  touching `label_halo`.

- label_halo_alpha:

  Halo opacity (0–1). Default `0.6` reads as a soft glow rather than a
  hard outline; raise toward `1` for sharper contrast on very busy
  backgrounds.

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
set.seed(1)
mat <- matrix(runif(16), 4, 4,
              dimnames = list(LETTERS[1:4], LETTERS[1:4]))
diag(mat) <- 0
plot_simplicial(mat, c("A B -> C", "B C -> D"))

```
