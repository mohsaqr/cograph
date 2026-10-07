# Select Top N Edges

Select the top N edges ranked by weight or another metric.

## Usage

``` r
select_top_edges(
  x,
  n,
  by = "weight",
  ...,
  keep_isolates = TRUE,
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- n:

  Integer. Number of top edges to select.

- by:

  Character. Metric for ranking. One of: `"weight"` (default),
  `"abs_weight"`, `"edge_betweenness"`, `"from_degree"`, `"to_degree"`,
  `"from_strength"`, `"to_strength"`, `"weight_rank"`. Any other name
  raises a `cograph_bad_selection` error.

- ...:

  Additional filter expressions, applied after the top `n` edges are
  selected.

- keep_isolates:

  Keep nodes that end up with no edges? Default TRUE.

- keep_format:

  Keep input format? Default FALSE.

- directed:

  Auto-detect if NULL.

## Value

A cograph_network with the top N edges.

## See also

[`select_edges`](https://sonsoles.me/cograph/reference/select_edges.md),
[`select_top`](https://sonsoles.me/cograph/reference/select_top.md)

## Examples

``` r
select_top_edges(regulation_net, n = 5, keep_isolates = FALSE)
#> Cograph network: 8 nodes, 5 edges ( directed )
#> Source: matrix 
#>   Nodes (8): Plan, Monitor, Adapt, Reflect, Discuss, Synthesize, Evaluate, Share
#>   Edges: 5 / 56 (density: 8.9%)
#>   Weights: [0.400, 0.490]  |  mean: 0.446
#>   Strongest edges:
#>     Share -> Monitor  0.490
#>     Plan -> Evaluate  0.490
#>     Evaluate -> Adapt  0.430
#>     Synthesize -> Reflect  0.420
#>     Plan -> Discuss  0.400
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
