# Target Layout (focal-node, topological)

Port of qgraph's `flow()` layout. One node of interest (the `target`) is
placed alone, then every other node is drawn in successive levels
ordered by unweighted graph distance (BFS hops) from it. This shows how
the target node connects out into the rest of the network.

## Usage

``` r
layout_target(network, target = NULL, horizontal = TRUE, equalize = TRUE, ...)
```

## Arguments

- network:

  A `CographNetwork` or `cograph_network` object.

- target:

  Node of interest, given as a label (character) or 1-based index. When
  `NULL` (default) the highest-degree node is used.

- horizontal:

  Logical. If `TRUE` (default) levels flow left to right with the target
  node on the left; if `FALSE` they flow top to bottom.

- equalize:

  Logical. If `TRUE` (default) nodes are evenly spaced within each
  level.

- ...:

  Additional arguments (ignored).

## Value

Data frame with `x`, `y` coordinates, one row per node.

## Details

Unlike qgraph's implementation, weights are binarized for layering (only
connectivity matters) and disconnected nodes are placed in an extra
trailing level instead of raising an error.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1,
                1, 0, 0, 0, 0, 1, 0, 0), nrow = 4, byrow = TRUE)
net <- CographNetwork$new(adj)
layout_target(net, target = 1)
#>   x         y
#> 1 0 0.5000000
#> 2 1 0.3333333
#> 3 1 0.6666667
#> 4 2 0.5000000
```
