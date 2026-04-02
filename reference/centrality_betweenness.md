# Betweenness Centrality

Fraction of shortest paths passing through each node. Nodes with high
betweenness act as bridges connecting different parts of the network.

## Usage

``` r
centrality_betweenness(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](http://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `normalized`, `weighted`, `directed`, `cutoff`,
  `invert_weights`).

## Value

Named numeric vector of betweenness values.

## See also

[`centrality`](http://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_load`](http://sonsoles.me/cograph/reference/centrality_load.md)
for a related measure.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_betweenness(adj)
#> A B C 
#> 0 0 0 
```
