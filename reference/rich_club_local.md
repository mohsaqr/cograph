# Local Rich Club Score

For each node, measures whether it preferentially directs its strongest
ties toward prominent nodes. A score \> 1 means the node's ties to
prominent nodes are stronger than average.

## Usage

``` r
rich_club_local(
  x,
  prominence = NULL,
  rich = c("k", "s"),
  directed = NULL,
  digits = NULL,
  sort_by = "score",
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna
  object.

- prominence:

  Integer or logical vector indicating which nodes are prominent (1/TRUE
  = prominent), OR a numeric threshold. If NULL, nodes above median
  degree (or strength) are prominent.

- rich:

  Character. `"k"` (degree, default) or `"s"` (strength). Used when
  `prominence` is NULL or a threshold.

- directed:

  Logical or NULL. Default NULL (auto-detect).

- digits:

  Integer or NULL. Round scores. Default NULL.

- sort_by:

  Character or NULL. Column to sort by (descending). Default `"score"`.

- ...:

  Additional arguments passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md).

## Value

A data frame with columns `node` and `score`, sorted by `score`
descending. Values \> 1 indicate the node directs disproportionately
strong ties to prominent nodes.

## Details

For each node i: \\r_i = \bar{w}\_{i \to rich} / \bar{w}\_i\\

## References

Opsahl, T., Colizza, V., Panzarasa, P. & Ramasco, J.J. (2008).
Prominence and control: The weighted rich-club effect. *Physical Review
Letters*, 101, 168702.

## See also

[`rich_club`](https://sonsoles.me/cograph/reference/rich_club.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)

## Examples

``` r
adj <- matrix(c(0,5,3,1, 5,0,4,2, 3,4,0,1, 1,2,1,0), 4, 4)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
cograph::rich_club_local(adj, prominence = c(1, 1, 0, 0))
#>   node    score
#> 1    A 1.666667
#> 2    B 1.363636
#> 3    C 1.312500
#> 4    D 1.125000
```
