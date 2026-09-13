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

  Additional filter expressions to apply.

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
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Top 2 by degree
select_top(adj, n = 2)
#> Cograph network: 2 nodes, 1 edges ( undirected )
#> Source: matrix 
#>   Nodes (2): B, C
#>   Edges: 1 / 1 (density: 100.0%)
#>   Weights: [0.300, 0.300]  |  mean: 0.300
#>   Strongest edges:
#>     B -- C  0.300
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.

# Top 2 by PageRank
select_top(adj, n = 2, by = "pagerank")
#> Cograph network: 2 nodes, 1 edges ( undirected )
#> Source: matrix 
#>   Nodes (2): B, C
#>   Edges: 1 / 1 (density: 100.0%)
#>   Weights: [0.300, 0.300]  |  mean: 0.300
#>   Strongest edges:
#>     B -- C  0.300
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
