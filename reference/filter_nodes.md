# Filter Nodes by Metadata or Centrality

Filter nodes using dplyr-style expressions on any node column or
centrality measure. Returns a cograph_network object by default
(universal format), or optionally the same format as input when
`keep_format = TRUE`.

## Usage

``` r
filter_nodes(
  x,
  ...,
  .keep_edges = c("internal", "none"),
  keep_format = FALSE,
  directed = NULL
)

subset_nodes(
  x,
  ...,
  .keep_edges = c("internal", "none"),
  keep_format = FALSE,
  directed = NULL
)
```

## Arguments

- x:

  Network input: cograph_network, matrix, igraph, network, or tna
  object.

- ...:

  Filter expressions using any node column or centrality measure.
  Available variables include:

  Node columns

  :   All columns in the nodes dataframe: `id`, `label`, `name`, `x`,
      `y`, `inits`, `color`, plus any custom

  Centrality measures

  :   `degree`, `indegree`, `outdegree`, `strength`, `instrength`,
      `outstrength`, `betweenness`, `closeness`, `eigenvector`,
      `pagerank`, `hub`, `authority`

  Examples: `degree >= 3`, `label %in% c("A", "B")`,
  `pagerank > 0.1 & degree >= 2`.

- .keep_edges:

  How to handle edges. One of:

  `"internal"`

  :   (default) Keep only edges between remaining nodes

  `"none"`

  :   Remove all edges

- keep_format:

  Logical. If TRUE, return the same format as input (matrix returns
  matrix, igraph returns igraph, etc.). Default FALSE returns
  cograph_network (universal format).

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected. Only used for
  non-cograph_network inputs.

## Value

A cograph_network object with filtered nodes. If `keep_format = TRUE`,
returns the same type as input (matrix, igraph, network, etc.).

## See also

[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md),
[`splot`](https://sonsoles.me/cograph/reference/splot.md),
`subset_nodes`

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0, .5, 0, .3, .6,
                .8, .3, 0, .4, 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Keep only high-degree nodes
filter_nodes(adj, degree >= 3)
#> Cograph network: 2 nodes, 1 edges ( undirected )
#> Source: filtered 
#>   Nodes (2): B, C
#>   Edges: 1 / 1 (density: 100.0%)
#>   Weights: [0.300, 0.300]  |  mean: 0.300
#>   Strongest edges:
#>     B -- C  0.300
#> Layout: none 

# Filter by label, combined with degree
filter_nodes(adj, degree >= 2 & label != "D")
#> Cograph network: 3 nodes, 3 edges ( undirected )
#> Source: filtered 
#>   Nodes (3): A, B, C
#>   Edges: 3 / 3 (density: 100.0%)
#>   Weights: [0.300, 0.800]  |  mean: 0.533
#>   Strongest edges:
#>     A -- C  0.800
#>     A -- B  0.500
#>     B -- C  0.300
#> Layout: none 
```
