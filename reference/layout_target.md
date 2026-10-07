# Target Layout (focal-node, topological)

Adapts the `flow()` layout of qgraph. One node of interest (the
`target`) is placed alone, and every other node is placed in successive
levels ordered by its unweighted graph distance (number of hops) from
the target. Edge direction is ignored. The layout shows how the target
node connects to the rest of the network.

## Usage

``` r
layout_target(network, target = NULL, horizontal = TRUE, equalize = TRUE, ...)
```

## Arguments

- network:

  A `CographNetwork` or `cograph_network` object.

- target:

  Node of interest, given as a label (character) or 1-based index. When
  `NULL` (default) the node with the most neighbors is used. A label
  that is not found or an index out of range raises an error.

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

Weights are binarized for layering, so only connectivity matters. Nodes
that cannot be reached from the target are placed in one extra level
after the last. qgraph raises an error for such nodes.

## Examples

``` r
layout_target(CographNetwork$new(regulation_net), target = "Plan")
#>    x         y
#> 1  2 0.2500000
#> 2  0 0.5000000
#> 3  1 0.1428571
#> 4  2 0.5000000
#> 5  2 0.7500000
#> 6  1 0.2857143
#> 7  1 0.4285714
#> 8  1 0.5714286
#> 9  1 0.7142857
#> 10 1 0.8571429
```
