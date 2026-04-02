# Translate qgraph-style parameter names to cograph equivalents

When splot() receives a tna object, users often pass qgraph-style
parameter names (e.g., `size = 20`, `edge.color = "red"`) because the
tna package uses qgraph for plotting. This function renames those keys
to their cograph equivalents and applies value transforms where needed.

## Usage

``` r
.translate_qgraph_dots(dots)
```

## Arguments

- dots:

  Named list (typically from `list(...)`).

## Value

Named list with qgraph keys renamed to cograph equivalents.
