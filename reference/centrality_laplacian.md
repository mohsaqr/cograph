# Laplacian Centrality

Energy drop from the graph Laplacian when a node is removed (Qi et al.
2012). Measures a node's importance to the overall network energy.

## Usage

``` r
centrality_laplacian(x, ...)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object).

- ...:

  Additional arguments passed to
  [`centrality`](https://sonsoles.me/cograph/reference/centrality.md)
  (e.g., `weighted`, `directed`).

## Value

Named numeric vector of Laplacian centrality values.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) for
computing multiple measures at once.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality_laplacian(adj)
#>  A  B  C 
#> 14 14 14 
```
