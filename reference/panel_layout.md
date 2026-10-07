# Configure a custom multi-panel layout

Sets up a multi-panel device layout for use with cograph plotting
functions called with `combined = FALSE`. The previous
[`par()`](https://rdrr.io/r/graphics/par.html) settings are returned so
that the caller can restore the device state.

## Usage

``` r
panel_layout(spec, mar = c(2, 2, 3, 1), widths = NULL, heights = NULL)
```

## Arguments

- spec:

  Either a length-2 vector of positive integers `c(nrow, ncol)` for a
  uniform grid, or a numeric matrix of non-negative panel numbers with
  at least one positive cell, passed to
  [`graphics::layout()`](https://rdrr.io/r/graphics/layout.html).

- mar:

  Numeric vector of length 4 giving panel margins. Default
  `c(2, 2, 3, 1)`.

- widths, heights:

  Optional numeric vectors of column widths and row heights, passed to
  [`graphics::layout()`](https://rdrr.io/r/graphics/layout.html). They
  are valid only when `spec` is a matrix. Supplying them with a length-2
  `spec` is an error.

## Value

Invisibly, a list of the previous
[`par()`](https://rdrr.io/r/graphics/par.html) settings (`mar` and
`mfrow`). Passing it to
[`graphics::par()`](https://rdrr.io/r/graphics/par.html) restores the
prior device state and also clears a layout set by
[`graphics::layout()`](https://rdrr.io/r/graphics/layout.html).

## Details

A length-2 `spec = c(nrow, ncol)` creates a uniform grid through
`graphics::par(mfrow = ...)`. A matrix `spec` creates a non-uniform
layout through
[`graphics::layout()`](https://rdrr.io/r/graphics/layout.html). The
matrix values number the panel cells and are read in column-major order,
so `matrix(c(1, 1, 2, 3), 2, 2)` gives one tall cell in the left column
and two stacked cells in the right column.

## Combined-flag scope

The `combined = FALSE` argument applies to the multi-panel plot
functions
[`plot_netobject_group()`](https://sonsoles.me/cograph/reference/plot-results.md),
[`plot_netobject_ml()`](https://sonsoles.me/cograph/reference/plot-results.md),
[`plot_net_bootstrap_group()`](https://sonsoles.me/cograph/reference/plot-results.md),
[`plot_group_permutation()`](https://sonsoles.me/cograph/reference/plot-results.md),
[`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md),
`splot.net_mlvar(type = "all")`,
[`plot_network_evolution()`](https://sonsoles.me/cograph/reference/plot_network_evolution.md),
`plot.cograph_motifs(type = "network")`,
`plot.cograph_motif_result(type = "patterns")`,
`plot.cograph_motif_analysis(type = "patterns")`,
`plot.tna_disparity(type = "comparison")`, and
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) on
`group_tna` and other list inputs. A single-network
[`splot()`](https://sonsoles.me/cograph/reference/splot.md) call plots
one panel and ignores `combined`.

## Examples

``` r
op <- panel_layout(c(1, 2))
splot(regulation_net, combined = FALSE)
splot(regulation_net, layout = "circle", combined = FALSE)

graphics::par(op)
```
