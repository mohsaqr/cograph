# Eigenvector Centrality

Influence-based centrality where a node's score depends on the scores of
its neighbors. Nodes connected to other high-scoring nodes get higher
scores.

## Usage

``` r
centrality_eigenvector(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of eigenvector centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_pagerank`](https://sonsoles.me/cograph/reference/centrality_pagerank.md)
for a random walk variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_eigenvector(adj)
#> A B C 
#> 1 1 1 
```
