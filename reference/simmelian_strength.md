# Simmelian Strength (Triangle Count per Edge)

Convenience wrapper around
[`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md)
that returns only the triangle count per edge, sorted descending.

## Usage

``` r
simmelian_strength(x, top = NULL, directed = NULL, digits = NULL, ...)
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

A data frame sorted by `triangles` (descending) with columns: `from`,
`to`, `weight` (if weighted), `triangles`.

## See also

[`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md),
[`neighborhood_overlap`](https://sonsoles.me/cograph/reference/neighborhood_overlap.md)

## Examples

``` r
k4 <- matrix(1, 4, 4); diag(k4) <- 0
rownames(k4) <- colnames(k4) <- c("A", "B", "C", "D")
cograph::simmelian_strength(k4)
#>   from to weight triangles
#> 1    A  B      1         2
#> 2    A  C      1         2
#> 3    A  D      1         2
#> 4    B  C      1         2
#> 5    B  D      1         2
#> 6    C  D      1         2
```
