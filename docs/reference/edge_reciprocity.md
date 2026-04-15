# Edge Reciprocity

Convenience wrapper around
[`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md)
that returns only reciprocity information for directed networks.

## Usage

``` r
edge_reciprocity(x, top = NULL, directed = NULL, digits = NULL, ...)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- top:

  Integer or NULL. Return only the top N edges. Default NULL.

- directed:

  Logical or NULL. Default NULL (auto-detect).

- digits:

  Integer or NULL. Round numeric columns. Default NULL.

- ...:

  Additional arguments passed to
  [`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md).

## Value

A data frame with columns: `from`, `to`, `weight`, `reciprocated`,
`reverse_weight`, `weight_ratio`.

## See also

[`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md)

## Examples

``` r
adj <- matrix(c(0, 0.8, 0, 0.3, 0, 0.5, 0.7, 0, 0), 3, 3, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
cograph::edge_reciprocity(adj, directed = TRUE)
#>   from to weight reciprocated reverse_weight weight_ratio
#> 1    B  A    0.3         TRUE            0.8     2.666667
#> 2    A  B    0.8         TRUE            0.3     0.375000
#> 3    B  C    0.5        FALSE             NA           NA
#> 4    C  A    0.7        FALSE             NA           NA
```
