# Flow Betweenness Centrality

Max-flow based betweenness centrality.

## Usage

``` r
centrality_flow_betweenness(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md).

## Value

Named numeric vector of flow betweenness values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md)
for shortest-path variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_flow_betweenness(adj)
#> A B C 
#> 1 1 1 
```
