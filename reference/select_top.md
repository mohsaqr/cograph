# Select Top N Nodes by Centrality

Select the top N nodes ranked by a centrality measure.

## Usage

``` r
select_top(
  x,
  n,
  by = "degree",
  ...,
  keep_edges = c("internal", "none"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input.

- n:

  Integer. Number of top nodes to select.

- by:

  Character. Centrality measure for ranking: `"degree"`, `"indegree"`,
  `"outdegree"`, `"strength"`, `"instrength"`, `"outstrength"`,
  `"betweenness"`, `"closeness"`, `"eigenvector"`, `"pagerank"`,
  `"hub"`, `"authority"`, `"coreness"`, or the name of any other measure
  [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
  computes (see
  [`list_centralities()`](https://sonsoles.me/cograph/reference/list_centralities.md)).
  An unknown name raises a `cograph_bad_selection` error. Default
  `"degree"`.

- ...:

  Additional filter expressions, applied after the top `n` nodes are
  selected.

- keep_edges:

  How to handle edges. Default "internal".

- keep_format:

  Logical. Keep input format? Default FALSE.

- directed:

  Logical or NULL. Auto-detect if NULL.

## Value

A cograph_network with the top N nodes.

## See also

[`select_nodes`](https://sonsoles.me/cograph/reference/select_nodes.md),
[`select_component`](https://sonsoles.me/cograph/reference/select_component.md)

## Examples

``` r
select_top(regulation_net, n = 3, by = "pagerank")
#> Cograph network: 3 nodes, 3 edges ( directed )
#> Source: matrix 
#>   Nodes (3): Monitor, Reflect, Create
#>   Edges: 3 / 6 (density: 50.0%)
#>   Weights: [0.150, 0.370]  |  mean: 0.230
#>   Strongest edges:
#>     Monitor -> Create  0.370
#>     Create -> Monitor  0.170
#>     Reflect -> Monitor  0.150
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
