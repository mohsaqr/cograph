# Percolation Centrality

Importance for spreading processes using node states. Each node has a
state (0-1) representing how activated it is. When all states are equal,
equivalent to betweenness.

## Usage

``` r
centrality_percolation(x, states = NULL, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- states:

  Named numeric vector of node states (0-1). Default `NULL` (all nodes
  get state 1).

- ...:

  Additional arguments passed to
  [`centrality`](http://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of percolation centrality values.

## See also

[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_betweenness`](http://sonsoles.me/cograph/reference/centrality_betweenness.md)
which this generalizes.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_percolation(adj)
#> A B C 
#> 0 0 0 
centrality_percolation(adj, states = c(A = 0.8, B = 0.2, C = 0.5))
#> A B C 
#> 0 0 0 
```
