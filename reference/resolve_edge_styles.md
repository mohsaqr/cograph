# Resolve Edge Styles

Converts edge style strings to numeric lty values and adjusts edge
widths for dotted style (30% reduction).

## Usage

``` r
resolve_edge_styles(edge_style, edge_widths, n_edges)
```

## Arguments

- edge_style:

  Edge style specification (character or numeric).

- edge_widths:

  Numeric vector of edge widths.

- n_edges:

  Number of edges.

## Value

List with `ltys` (numeric lty vector) and `edge_widths`.
