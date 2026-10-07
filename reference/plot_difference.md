# Plot Network Difference

Plots the difference between two networks (x - y) with
[`splot`](https://sonsoles.me/cograph/reference/splot.md). Positive
differences (x \> y) are shown in `pos_color` and negative differences
(x \< y) in `neg_color`. Node-level differences, such as initial
probabilities, can be shown as donut charts.

## Usage

``` r
plot_difference(
  x,
  y = NULL,
  i = NULL,
  j = NULL,
  pos_color = "#009900",
  neg_color = "#C62828",
  labels = NULL,
  title = NULL,
  inits_x = NULL,
  inits_y = NULL,
  show_inits = NULL,
  donut_inner_ratio = 0.8,
  force = FALSE,
  combined = TRUE,
  difference = FALSE,
  ...
)
```

## Arguments

- x:

  First network: matrix, `cograph_network`, `tna`, `igraph`, list with a
  matrix `weights` component, plain list of networks, or `group_tna`.
  For a `group_tna` with two groups, the groups are compared directly.
  With more groups and `i` and `j` both `NULL`, all pairwise comparisons
  are plotted.

- y:

  Second network, of the same type as `x`. Ignored when `x` is a list or
  `group_tna`.

- i:

  Index or name of the first group when `x` is a `group_tna` or a plain
  list. `NULL` selects the first element, except that all pairs are
  plotted for a `group_tna` of more than two groups when `j` is also
  `NULL`.

- j:

  Index or name of the second group, with the same rules as `i`. `NULL`
  selects the second element.

- pos_color:

  Color for positive differences (x \> y).

- neg_color:

  Color for negative differences (x \< y).

- labels:

  Node labels. `NULL` uses the row names of the weight matrix, or node
  indices when there are none.

- title:

  Plot title. `NULL` uses `"Network Difference (x - y)"`, or the two
  group names when they are available.

- inits_x:

  Node values for x (e.g., initial probabilities). `NULL` extracts them
  from a tna object.

- inits_y:

  Node values for y. `NULL` extracts them from a tna object.

- show_inits:

  Logical. Show node differences as donuts? `NULL` (default) shows them
  when node values are available for both networks.

- donut_inner_ratio:

  Inner radius ratio for donut (0-1).

- force:

  Logical. Plot all pairs for a `group_tna` with more than four groups.
  Without it, such input stops with an error.

- combined:

  Logical. When `TRUE` (default) and `x` is a multi-group input that
  triggers all-pairs plotting, panels are arranged in a grid via
  `graphics::par(mfrow = ...)`. `FALSE` plots into a layout the caller
  has already configured (e.g. via
  [`panel_layout()`](https://sonsoles.me/cograph/reference/panel_layout.md)).
  It has no effect for a single pair.

- difference:

  Logical. If `TRUE`, `x` is treated as an already-subtracted difference
  network and `y` is ignored with a warning. A `tna_comparison` object
  (from
  [`tna::compare()`](https://sonsoles.me/tna/reference/compare.html)) or
  a `netdifference` object is detected automatically and its difference
  matrix is used.

- ...:

  Additional arguments passed to
  [`splot`](https://sonsoles.me/cograph/reference/splot.md). They
  override the defaults set here (`layout = "oval"`, `minimum = 0` and
  the TNA or psychometric styling preset).

## Value

Invisibly, a list with elements `weights` (the element-wise difference
matrix `x - y`) and `inits` (the node-value difference, or `NULL` when
no node values were available). For the `group_tna` all-pairs path, a
named list of such lists with one element per pair, named
`"<group_i>_vs_<group_j>"`.

## Details

The weight matrices are subtracted element-wise. Both networks must have
the same dimensions and, when present, the same node labels; otherwise
the function stops with an error. A directed difference (or tna input)
is styled with the TNA preset and an undirected difference with the
psychometric preset.

When node values (inits) are given or extracted from tna objects, each
node is shown as a donut whose filled fraction is the absolute
difference, capped at 1, colored `pos_color` when x is higher and
`neg_color` when y is higher.

[`plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.md)
is an alias of `plot_difference()`.

## See also

[`plot_compare`](https://sonsoles.me/cograph/reference/plot_compare.md),
[`plot_comparison_heatmap`](https://sonsoles.me/cograph/reference/plot_comparison_heatmap.md)

## Examples

``` r
plot_difference(regulation_net, t(regulation_net))

```
