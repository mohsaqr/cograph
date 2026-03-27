# Resolve Edge Colors

Determines edge colors based on weights, explicit colors, or defaults.

## Usage

``` r
resolve_edge_colors(
  edges,
  edge.color = NULL,
  posCol = "#2E7D32",
  negCol = "#C62828",
  default_col = "gray50"
)
```

## Arguments

- edges:

  Edge data frame with from, to, weight columns.

- edge.color:

  User-specified edge color(s) or NULL.

- posCol:

  Color for positive weights.

- negCol:

  Color for negative weights.

- default_col:

  Default color when no weight.

## Value

Vector of colors for each edge.
