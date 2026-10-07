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
`to`, `weight`, `triangles`.

## See also

[`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md),
[`neighborhood_overlap`](https://sonsoles.me/cograph/reference/neighborhood_overlap.md)

## Examples

``` r
cograph::simmelian_strength(regulation_net, top = 5)
#>       from      to weight triangles
#> 1     Plan Monitor   0.13         4
#> 2     Plan  Create   0.20         4
#> 3 Evaluate Monitor   0.33         4
#> 4  Monitor   Adapt   0.16         3
#> 5  Monitor  Create   0.37         3
```
