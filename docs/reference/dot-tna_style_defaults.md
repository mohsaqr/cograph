# TNA Visual Style Defaults

Returns the standard TNA visual defaults as a named list. Used by
`splot(tna_styling = TRUE)`,
[`from_tna()`](http://sonsoles.me/cograph/reference/from_tna.md), and
[`plot_tna()`](http://sonsoles.me/cograph/reference/plot_tna.md).

## Usage

``` r
.tna_style_defaults(n_nodes = NULL, directed = TRUE)
```

## Arguments

- n_nodes:

  Number of nodes (for color palette). NULL skips node_fill.

- directed:

  Logical. If TRUE, includes arrow/edge-start defaults.

## Value

Named list of splot parameters.
