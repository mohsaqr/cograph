# Plot Nestimate Permutation Test Results

Visualizes `net_permutation` objects from the Nestimate package. Differs
from `plot_permutation`: p_values and effect_size are already p×p
matrices (no edge-name parsing needed), and `directed` comes from
`x$x$directed`.

## Usage

``` r
splot.net_permutation(
  x,
  show_nonsig = FALSE,
  show_effect = FALSE,
  edge_positive_color = "#009900",
  edge_negative_color = "#C62828",
  edge_nonsig_color = "#888888",
  edge_nonsig_style = 2L,
  show_stars = TRUE,
  ...
)
```

## Arguments

- x:

  A `net_permutation` object (from Nestimate).

- show_nonsig:

  Logical: show non-significant edges? Default FALSE.

- show_effect:

  Logical: show effect size in parentheses? Default FALSE.

- edge_positive_color:

  Color for positive differences. Default `"#009900"`.

- edge_negative_color:

  Color for negative differences. Default `"#C62828"`.

- edge_nonsig_color:

  Color for non-significant edges. Default `"#888888"`.

- edge_nonsig_style:

  Line style for non-significant edges. Default 2L.

- show_stars:

  Logical: show significance stars? Default TRUE.

- ...:

  Additional arguments passed to
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md).

## Value

Invisibly returns the plot.
