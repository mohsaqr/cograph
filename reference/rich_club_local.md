# Local Rich Club Score

Computes, for each node, whether its strongest ties go to prominent
nodes. The score is the mean weight of the node's ties to prominent
neighbors divided by the mean weight of all its ties. A score above 1
means that the node's ties to prominent nodes are stronger than its
average tie. A directed network is converted to an undirected one before
the computation, with the weights of reciprocal edges summed, and
self-loops are removed.

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

  Which nodes are prominent. Either a logical or 0/1 vector with one
  element per node (TRUE or 1 marks a prominent node), or a single
  number used as a threshold, in which case nodes with degree or
  strength strictly greater than it are prominent. If NULL (default),
  nodes with degree or strength strictly above the median are prominent.

- rich:

  Character. `"k"` (degree, default) or `"s"` (strength). Used when
  `prominence` is NULL or a threshold.

- directed:

  Logical or NULL. Default NULL (auto-detect).

- digits:

  Integer or NULL. Round scores. Default NULL.

- sort_by:

  Character or NULL. Column to sort by in descending order, `"score"`
  (default) or `"node"`. Any other value, and NULL, keep the node order.

- ...:

  Passed to
  [`to_igraph`](https://sonsoles.me/cograph/reference/to_igraph.md),
  which takes no further arguments, so any argument supplied here raises
  an error.

## Value

A plain data frame with one row per node and columns `node` (node label)
and `score`. A node with no neighbors or no prominent neighbor scores 1.

## Details

For each node \\i\\, \\r_i = \bar{w}\_{i, rich} / \bar{w}\_i\\, where
both means are taken over the ties of \\i\\ with positive weight.

## References

Opsahl, T., Colizza, V., Panzarasa, P. & Ramasco, J.J. (2008).
Prominence and control: The weighted rich-club effect. *Physical Review
Letters*, 101, 168702.

## See also

[`rich_club`](https://sonsoles.me/cograph/reference/rich_club.md),
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md)

## Examples

``` r
cograph::rich_club_local(regulation_net)
#>          node     score
#> 1      Create 1.3536585
#> 2    Evaluate 1.1988304
#> 3       Share 1.0769231
#> 4     Monitor 1.0356506
#> 5     Discuss 0.9586057
#> 6     Explore 0.7553957
#> 7  Synthesize 0.6060606
#> 8       Adapt 0.5423729
#> 9     Reflect 0.5395683
#> 10       Plan 0.5210526
```
