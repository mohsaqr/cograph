# Load Centrality

Fraction of all shortest paths passing through a node, similar to
betweenness but weighting paths by 1/count (Goh et al. 2001).

## Usage

``` r
centrality_load(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](http://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of load centrality values.

## See also

[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_betweenness`](http://sonsoles.me/cograph/reference/centrality_betweenness.md)
for the standard variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_load(adj)
#> A B C 
#> 5 5 5 
```
