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

A data frame with one row per directed edge and columns `from`, `to`,
`weight`, `reciprocated` (logical), `reverse_weight` (NA when not
reciprocated) and `weight_ratio` (`reverse_weight / weight`; NA when not
reciprocated). Rows are ordered with reciprocated edges first, then by
`|weight_ratio|` descending.

## Errors

Raises an error when the resolved network is undirected: reciprocity is
only defined for directed edges.

## See also

[`edge_centrality`](https://sonsoles.me/cograph/reference/edge_centrality.md)

## Examples

``` r
cograph::edge_reciprocity(regulation_net, top = 5)
#>      from      to weight reciprocated reverse_weight weight_ratio
#> 1 Reflect Explore   0.05         TRUE           0.35    7.0000000
#> 2  Create Monitor   0.17         TRUE           0.37    2.1764706
#> 3   Share    Plan   0.21         TRUE           0.36    1.7142857
#> 4    Plan   Share   0.36         TRUE           0.21    0.5833333
#> 5 Monitor  Create   0.37         TRUE           0.17    0.4594595
```
