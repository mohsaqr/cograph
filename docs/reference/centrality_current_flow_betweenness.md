# Current Flow Betweenness Centrality

Betweenness based on electrical current flow rather than shortest paths.
Uses the Laplacian pseudoinverse. Requires a connected graph.

## Usage

``` r
centrality_current_flow_betweenness(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of current flow betweenness values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once,
[`centrality_betweenness`](https://sonsoles.me/cograph/reference/centrality_betweenness.md)
for the shortest-path variant.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_current_flow_betweenness(adj)
#>         A         B         C 
#> 0.3333333 0.3333333 0.3333333 
```
