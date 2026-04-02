# Resolve Node Colors

Determines node colors from various inputs.

## Usage

``` r
resolve_node_colors(
  color,
  n,
  nodes = NULL,
  groups = NULL,
  default_col = "#4A90D9"
)
```

## Arguments

- color:

  User-specified color(s) or NULL.

- n:

  Number of nodes.

- nodes:

  Node data frame (for group coloring).

- groups:

  Group assignments for color mapping.

- default_col:

  Default node color.

## Value

Vector of colors for each node.
