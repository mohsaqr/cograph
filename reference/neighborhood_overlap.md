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
`to`, `weight`, `overlap`, `shared_neighbors`.

## See also

[`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md),
[`simmelian_strength`](https://sonsoles.me/cograph/reference/simmelian_strength.md)

## Examples

``` r
cograph::neighborhood_overlap(regulation_net, top = 5)
#>         from      to weight   overlap shared_neighbors
#> 1       Plan  Create   0.20 0.6666667                4
#> 2   Evaluate Monitor   0.33 0.6666667                4
#> 3    Discuss Explore   0.30 0.6000000                3
#> 4       Plan Monitor   0.13 0.5714286                4
#> 5 Synthesize Monitor   0.07 0.5000000                3
```
