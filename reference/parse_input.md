# Parse Network Input

Automatically detects input type and converts to internal format.

## Usage

``` r
parse_input(input, directed = NULL, simplify = FALSE)
```

## Arguments

- input:

  Network input: matrix, data.frame (edge list), igraph, statnet
  network, qgraph, tna, or an already-parsed list with an `edges`
  component.

- directed:

  Logical. Force directed interpretation. NULL for auto-detect.

- simplify:

  Logical or character. Passed to `parse_tna()` for tna inputs.

## Value

List with parsed network components, usually including nodes, edges,
directed, and weight information.
