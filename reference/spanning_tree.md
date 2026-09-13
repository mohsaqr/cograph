# Minimum or Maximum Spanning Tree

Prim's algorithm on each connected component, so a disconnected network
yields a spanning forest.

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

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. Directedness to read the input with; the tree itself
  is undirected.

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
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

spanning_tree(adj)
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.300, 0.500]  |  mean: 0.400
#>   Strongest edges:
#>     A -- B  0.500
#>     C -- D  0.400
#>     B -- C  0.300
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
spanning_tree(adj, maximum = TRUE)
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.500, 0.800]  |  mean: 0.633
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
