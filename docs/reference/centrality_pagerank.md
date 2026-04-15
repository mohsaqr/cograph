# PageRank Centrality

Random walk centrality measuring node importance. Simulates a random
walker that follows edges with probability `damping` and jumps to a
random node with probability `1 - damping`.

## Usage

``` r
centrality_pagerank(x, damping = 0.85, personalized = NULL, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- damping:

  Damping factor (probability of following an edge). Default 0.85.

- personalized:

  Named numeric vector for personalized PageRank. Values should sum
  to 1. Default `NULL` (uniform).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of PageRank values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_eigenvector`](https://sonsoles.me/cograph/reference/centrality_eigenvector.md)
for a related measure.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_pagerank(adj)
#>         A         B         C 
#> 0.3333333 0.3333333 0.3333333 
centrality_pagerank(adj, damping = 0.9)
#>         A         B         C 
#> 0.3333333 0.3333333 0.3333333 
```
