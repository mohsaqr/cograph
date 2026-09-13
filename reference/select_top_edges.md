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

  Additional filter expressions.

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
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Top 3 edges by weight
select_top_edges(adj, n = 3)
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

# Top 2 by edge betweenness
select_top_edges(adj, n = 2, by = "edge_betweenness")
#> Warning: 1 node(s) have no edges left. Nodes are kept; call remove_isolates() to drop them.
#> Cograph network: 4 nodes, 2 edges ( undirected )
#> Source: matrix 
#>   Nodes (4): A, B, C, D
#>   Edges: 2 / 6 (density: 33.3%)
#>   Weights: [0.500, 0.600]  |  mean: 0.550
#>   Strongest edges:
#>     B -- D  0.600
#>     A -- B  0.500
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
