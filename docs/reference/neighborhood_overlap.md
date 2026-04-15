# Neighborhood Overlap (Jaccard) for Each Edge

Convenience wrapper around
[`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md)
that returns only the overlap measure sorted by overlap descending.

## Usage

``` r
neighborhood_overlap(x, top = NULL, directed = NULL, digits = NULL, ...)
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

A data frame sorted by `overlap` (descending) with columns: `from`,
`to`, `weight` (if weighted), `overlap`, `shared_neighbors`.

## See also

[`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md),
[`simmelian_strength`](https://sonsoles.me/cograph/reference/simmelian_strength.md)

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
cograph::neighborhood_overlap(adj)
#>   from to weight overlap shared_neighbors
#> 1    A  B      1       1                1
#> 2    A  C      1       1                1
#> 3    B  C      1       1                1
```
