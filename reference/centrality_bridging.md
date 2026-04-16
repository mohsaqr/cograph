# Bridging Centrality

Product of betweenness and bridging coefficient. Identifies nodes that
bridge communities.

## Usage

``` r
centrality_bridging(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of bridging centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_local_bridging`](https://sonsoles.me/cograph/reference/centrality_local_bridging.md)
for the local variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_bridging(adj)
#> A B C 
#> 0 0 0 
```
