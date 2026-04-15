# Stress Centrality

Number of shortest paths passing through each node. Unlike betweenness,
does not normalize by the total number of shortest paths.

## Usage

``` r
centrality_stress(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of stress centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md)
for the normalized variant.

## Examples

``` r
adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
centrality_stress(adj)
#> A B C D 
#> 0 2 2 0 
```
