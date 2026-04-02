# Parse Network Input

Automatically detects input type and converts to internal format.

## Usage

``` r
parse_input(input, directed = NULL, simplify = FALSE)
```

## Arguments

- input:

  Network input: matrix, data.frame (edge list), or igraph object.

- directed:

  Logical. Force directed interpretation. NULL for auto-detect.

## Value

List with nodes, edges, directed, and weights components.
