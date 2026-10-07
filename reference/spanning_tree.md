# Minimum or Maximum Spanning Tree

Computes a spanning tree with Prim's algorithm on each connected
component, so a disconnected network yields a spanning forest. A
directed network is symmetrized first, each pair taking the larger of
its two arc weights. Self-loops are ignored. Missing or infinite weights
raise a `cograph_bad_selection` error.

## Usage

``` r
spanning_tree(
  x,
  weights = c("weight", "none"),
  maximum = FALSE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- weights:

  `"weight"` (default) uses the edge weights as costs; `"none"` treats
  every edge as cost 1.

- maximum:

  Logical. Find the maximum spanning tree instead of the minimum.
  Default FALSE. Set TRUE when the weights are similarities.

- keep_format:

  Logical. If TRUE, a matrix, igraph, statnet network or tna input is
  returned in its own format. An edge-list data frame or a qgraph object
  is returned as a `cograph_network` with a
  `cograph_no_format_roundtrip` warning. Default FALSE returns a
  `cograph_network`.

- directed:

  Logical or NULL. Directedness used to read the input. NULL (default)
  detects it from the input. The tree itself is undirected.

## Value

An undirected `cograph_network` holding the spanning tree (or forest),
or the input format when `keep_format = TRUE`. Every node is kept.

## References

Prim, R. C. (1957). Shortest connection networks and some
generalizations. *Bell System Technical Journal*, 36(6), 1389–1401.

## See also

[`disparity_filter`](https://sonsoles.me/cograph/reference/disparity_filter.md),
[`threshold_edges`](https://sonsoles.me/cograph/reference/threshold_edges.md)

## Examples

``` r
spanning_tree(regulation_net, maximum = TRUE)
#> Cograph network: 10 nodes, 9 edges ( undirected )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 9 / 45 (density: 20.0%)
#>   Weights: [0.350, 0.490]  |  mean: 0.412
#>   Strongest edges:
#>     Plan -- Evaluate  0.490
#>     Monitor -- Share  0.490
#>     Adapt -- Evaluate  0.430
#>     Reflect -- Synthesize  0.420
#>     Plan -- Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
