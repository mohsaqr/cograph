# Select Nodes with Lazy Centrality Computation

A more nuanced node selection function that improves upon
[`filter_nodes()`](http://sonsoles.me/cograph/reference/filter_nodes.md)
with lazy centrality computation (only computes measures actually
referenced), multiple selection modes, and global context variables for
structural awareness.

## Usage

``` r
select_nodes(
  x,
  ...,
  name = NULL,
  index = NULL,
  top = NULL,
  by = "degree",
  neighbors_of = NULL,
  order = 1L,
  component = NULL,
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

  Filter expressions using node columns, centrality measures, or global
  context variables. Centrality measures are computed lazily (only those
  actually referenced). Available variables:

  Node columns

  :   All columns in the nodes dataframe: `id`, `label`, `name`, `x`,
      `y`, `inits`, `color`, plus any custom

  Centrality measures

  :   `degree`, `indegree`, `outdegree`, `strength`, `instrength`,
      `outstrength`, `betweenness`, `closeness`, `eigenvector`,
      `pagerank`, `hub`, `authority`, `coreness`

  Global context

  :   `component`, `component_size`, `is_largest_component`,
      `neighborhood_size`, `k_core`, `is_articulation`,
      `is_bridge_endpoint`

- name:

  Character vector. Select nodes by name/label.

- index:

  Integer vector. Select nodes by index (1-based).

- top:

  Integer. Select top N nodes by centrality measure.

- by:

  Character. Centrality measure for top selection. Default `"degree"`.

- neighbors_of:

  Character or integer. Select neighbors of these nodes (by name or
  index).

- order:

  Integer. Neighborhood order (1 = direct neighbors, 2 = neighbors of
  neighbors, etc.). Default 1.

- component:

  Selection mode for connected components:

  `"largest"`

  :   Select nodes in the largest connected component

  Integer

  :   Select nodes in component with this ID

  Character

  :   Select component containing node with this name

- .keep_edges:

  How to handle edges. One of:

  `"internal"`

  :   (default) Keep only edges between remaining nodes

  `"none"`

  :   Remove all edges

- keep_format:

  Logical. If TRUE, return the same format as input. Default FALSE
  returns cograph_network.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

## Value

A cograph_network object with selected nodes. If `keep_format = TRUE`,
returns the same type as input.

## Details

Selection modes are combined with AND logic (like tidygraph/dplyr):

- `select_nodes(x, top = 10, component = "largest")` selects top 10
  nodes **within** the largest component

- All criteria must be satisfied for a node to be selected

Centrality measures are computed lazily - only measures actually
referenced in expressions or the `by` parameter are computed. This makes
`select_nodes()` faster than
[`filter_nodes()`](http://sonsoles.me/cograph/reference/filter_nodes.md)
for large networks.

For networks with negative edge weights, `betweenness` and `closeness`
will return NA with a warning (igraph cannot compute these with negative
weights).

## See also

[`filter_nodes`](http://sonsoles.me/cograph/reference/filter_nodes.md),
[`select_neighbors`](http://sonsoles.me/cograph/reference/select_neighbors.md),
[`select_component`](http://sonsoles.me/cograph/reference/select_component.md),
[`select_top`](http://sonsoles.me/cograph/reference/select_top.md)

## Examples

``` r
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

# Lazy - only computes degree
select_nodes(adj, degree >= 3)
#> Cograph network: 2 nodes, 1 edges ( undirected )
#> Source: filtered 
#>   Nodes (2): B, C
#>   Edges: 1 / 1 (density: 100.0%)
#>   Weights: [0.300, 0.300]  |  mean: 0.300
#>   Strongest edges:
#>     B -- C  0.300
#> Layout: none 

# Global context - computes component info
select_nodes(adj, is_largest_component & degree >= 2)
#> Cograph network: 4 nodes, 5 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 5 / 6 (density: 83.3%)
#>   Weights: [0.300, 0.800]  |  mean: 0.520
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#>     C -- D  0.400
#>     B -- C  0.300
#> Layout: none 

# By name
select_nodes(adj, name = c("A", "B", "C"))
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

# Top 2 by PageRank
select_nodes(adj, top = 2, by = "pagerank")
#> Cograph network: 2 nodes, 1 edges ( undirected )
#> Source: filtered 
#>   Nodes (2): B, C
#>   Edges: 1 / 1 (density: 100.0%)
#>   Weights: [0.300, 0.300]  |  mean: 0.300
#>   Strongest edges:
#>     B -- C  0.300
#> Layout: none 

# Neighborhood of "A" up to 2 hops
select_nodes(adj, neighbors_of = "A", order = 2)
#> Cograph network: 4 nodes, 5 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 5 / 6 (density: 83.3%)
#>   Weights: [0.300, 0.800]  |  mean: 0.520
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#>     C -- D  0.400
#>     B -- C  0.300
#> Layout: none 

# Largest connected component
select_nodes(adj, component = "largest")
#> Cograph network: 4 nodes, 5 edges ( undirected )
#> Source: filtered 
#>   Nodes (4): A, B, C, D
#>   Edges: 5 / 6 (density: 83.3%)
#>   Weights: [0.300, 0.800]  |  mean: 0.520
#>   Strongest edges:
#>     A -- C  0.800
#>     B -- D  0.600
#>     A -- B  0.500
#>     C -- D  0.400
#>     B -- C  0.300
#> Layout: none 

# Combined: top 2 in largest component
select_nodes(adj, component = "largest", top = 2, by = "degree")
#> Cograph network: 2 nodes, 1 edges ( undirected )
#> Source: filtered 
#>   Nodes (2): B, C
#>   Edges: 1 / 1 (density: 100.0%)
#>   Weights: [0.300, 0.300]  |  mean: 0.300
#>   Strongest edges:
#>     B -- C  0.300
#> Layout: none 

# Articulation points with high degree
# select_nodes(adj, is_articulation & degree >= 2)
```
