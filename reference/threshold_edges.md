# Threshold Edges by Weight, Count, Proportion or Density

Keeps the edges that satisfy every criterion supplied. The operation
corresponds to qgraph's `minimum` argument and to
[`tna::prune()`](https://sonsoles.me/tna/reference/prune.html). The
result is a network that can be analysed and plotted.

## Usage

``` r
threshold_edges(
  x,
  minimum = NULL,
  maximum = NULL,
  proportion = NULL,
  density = NULL,
  top = NULL,
  absolute = TRUE,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input: cograph_network, matrix, igraph, network, tna, or an
  edge-list data frame.

- minimum:

  Numeric. Keep edges whose weight is at least this value.

- maximum:

  Numeric. Keep edges whose weight is at most this value.

- proportion:

  Numeric in (0, 1\]. Keep this fraction of the edges, the strongest
  first.

- density:

  Numeric in (0, 1\]. Keep as many of the strongest edges as gives this
  density (edges as a fraction of the possible edges).

- top:

  Non-negative integer. Keep this many edges, the strongest first.
  `top = 0` removes every edge.

- absolute:

  Logical. Compare `abs(weight)` instead of the signed weight. Default
  TRUE, which suits correlation and partial-correlation networks. The
  `minimum` and `maximum` comparisons and the ranking used by
  `proportion`, `density` and `top` all follow this flag.

- keep_isolates:

  Logical. Keep nodes that end up with no edges? Default TRUE. Set
  FALSE, or call
  [`remove_isolates()`](https://sonsoles.me/cograph/reference/remove_isolates.md),
  to drop them.

- keep_format:

  Logical. Return the input format when TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A `cograph_network` with the surviving edges, or the input format when
`keep_format = TRUE`. Every node is kept unless `keep_isolates = FALSE`;
nodes the threshold stranded are reported in a
`cograph_isolates_created` warning. A non-finite `minimum` or `maximum`,
a `proportion` or `density` outside (0, 1\], and a negative or
fractional `top` raise a `cograph_bad_selection` error.

## Details

Several criteria are combined with AND. For example,
`threshold_edges(x, minimum = 0.2, top = 20)` keeps the twenty strongest
edges among those of weight at least 0.2. When `proportion`, `density`
and `top` are combined, the smallest of the implied edge counts is used.

Ties at the cut point are all kept, so `top = 10` can return more than
ten edges when the tenth and eleventh weights are equal. The result
therefore does not depend on the order in which the edges are stored.

## References

Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., &
Borsboom, D. (2012). qgraph: Network visualizations of relationships in
psychometric data. *Journal of Statistical Software*, 48(4), 1–18.

## See also

[`binarize`](https://sonsoles.me/cograph/reference/binarize.md),
[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md),
[`disparity_filter`](https://sonsoles.me/cograph/reference/disparity_filter.md),
[`remove_isolates`](https://sonsoles.me/cograph/reference/remove_isolates.md)

## Examples

``` r
threshold_edges(regulation_net, minimum = 0.1)
#> Cograph network: 10 nodes, 27 edges ( directed )
#> Source: matrix 
#>   Nodes (10): Explore, Plan, Monitor, Adapt, Reflect, Discuss, ... +4 more
#>   Edges: 27 / 90 (density: 30.0%)
#>   Weights: [0.110, 0.490]  |  mean: 0.288
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
