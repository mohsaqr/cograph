# Threshold Edges by Weight, Count, Proportion or Density

Keeps the edges that satisfy every criterion supplied. This is the
network equivalent of qgraph's `minimum`/`cut` arguments and of
[`tna::prune()`](http://sonsoles.me/tna/reference/prune.md), except that
it returns a network rather than a plot setting, so the thresholded
network can be analysed, not only drawn.

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

  Integer. Keep this many edges, the strongest first.

- absolute:

  Logical. Compare `abs(weight)` rather than the signed weight. Default
  TRUE, which is what correlation and partial-correlation networks need.
  `minimum`/`maximum` and the ranking used by `proportion`, `density`
  and `top` both follow this flag.

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
`keep_format = TRUE`. Every node is kept unless `keep_isolates = FALSE`.

## Details

When several criteria are given they are combined with AND: for example
`threshold_edges(x, minimum = 0.2, top = 20)` keeps the twenty strongest
edges among those of weight at least 0.2.

Ties at the cut point are all kept, so `top = 10` can return more than
ten edges when the tenth and eleventh weights are equal. This is
deliberate: breaking ties on edge order would make the result depend on
how the network was built.

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
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

threshold_edges(adj, minimum = 0.5)
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.500, 0.800]  |  mean: 0.633
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
threshold_edges(adj, top = 2)
#> Cograph network: 4 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [0.600, 0.800]  |  mean: 0.700
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
threshold_edges(adj, density = 0.5)
#> Cograph network: 4 nodes, 3 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.500, 0.800]  |  mean: 0.633
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
