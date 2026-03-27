# Select Top N Edges

Select the top N edges ranked by weight or another metric.

## Usage

``` r
select_top_edges(
  x,
  n,
  by = "weight",
  ...,
  .keep_isolates = FALSE,
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

  Character. Metric for ranking. One of: `"weight"`, `"abs_weight"`,
  `"edge_betweenness"`. Default `"weight"`.

- ...:

  Additional filter expressions.

- .keep_isolates:

  Keep nodes with no edges? Default FALSE.

- keep_format:

  Keep input format? Default FALSE.

- directed:

  Auto-detect if NULL.

## Value

A cograph_network with the top N edges.

## See also

[`select_edges`](http://sonsoles.me/cograph/reference/select_edges.md),
[`select_top`](http://sonsoles.me/cograph/reference/select_top.md)

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
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.500, 0.800]  |  mean: 0.633
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#> Layout: none 

# Top 2 by edge betweenness
select_top_edges(adj, n = 2, by = "edge_betweenness")
#> Cograph network: 3 nodes, 2 edges ( undirected )
#> Source: filtered 
#>   Nodes (3): A, B, D
#>   Edges: 2 / 3 (density: 66.7%)
#>   Weights: [0.500, 0.600]  |  mean: 0.550
#>   Strongest edges:
#>     B -- D  0.600
#>     A -- B  0.500
#> Layout: none 
```
