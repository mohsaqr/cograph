# Plot All Pairwise Comparisons

Internal helper to plot all pairwise comparisons from a group_tna
object.

## Usage

``` r
.plot_compare_all_pairs(
  x,
  pos_color,
  neg_color,
  labels,
  show_inits,
  donut_inner_ratio,
  ...
)
```

## Arguments

- x:

  A group_tna object.

- pos_color:

  Color for positive differences.

- neg_color:

  Color for negative differences.

- labels:

  Node labels.

- show_inits:

  Show donut inits.

- donut_inner_ratio:

  Donut inner ratio.

- ...:

  Additional arguments passed to splot().

## Value

Invisibly returns list of comparison results.
