# Select Nodes

Selects nodes by name, index, rank on a centrality measure,
neighborhood, connected component, or filter expressions on node
columns, centrality measures and structural variables.

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
  keep_edges = c("internal", "none"),
  keep_format = FALSE,
  directed = NULL,
  .keep_edges = NULL
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

  :   All columns of the node table, such as `id`, `label`, `name`, `x`,
      `y` and any custom columns.

  Centrality measures

  :   `degree`, `indegree`, `outdegree`, `strength`, `instrength`,
      `outstrength`, `betweenness`, `closeness`, `eigenvector`,
      `pagerank`, `hub`, `authority`, `coreness`. Any other measure
      [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
      computes can be named too; see
      [`list_centralities()`](https://sonsoles.me/cograph/reference/list_centralities.md).

  Global context

  :   `component`, `component_size`, `is_largest_component`,
      `neighborhood_size`, `k_core`, `is_articulation`,
      `is_bridge_endpoint`

  Predicates

  :   `is_isolated`, `is_source`, `is_sink`, `is_leaf`, `is_cut`,
      `local_transitivity`, `local_triangles`

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

- keep_edges:

  How to handle edges. One of:

  `"internal"`

  :   (default) Keep only edges between remaining nodes

  `"none"`

  :   Remove all edges

- keep_format:

  Logical. If TRUE, matrix, igraph, statnet network and tna inputs are
  returned in that format. Default FALSE returns cograph_network.

- directed:

  Logical or NULL. If NULL (default), auto-detect.

- .keep_edges:

  Deprecated. Use `keep_edges`.

## Value

A cograph_network object with selected nodes. If `keep_format = TRUE`,
matrix, igraph, statnet network and tna inputs are converted back to
that type.

## Details

Selection criteria are combined with AND logic, so a node is selected
only when it satisfies all of them. The `top` ranking is applied to the
nodes that pass `name`, `index`, `component` and `neighbors_of`, and the
filter expressions in `...` are applied afterwards. For example,
`select_nodes(x, top = 10, component = "largest")` selects the 10
highest-degree nodes within the largest component.

Only the centrality measures referenced in expressions or in `by` are
computed.

For networks with negative edge weights, `betweenness`, `closeness` and
`pagerank` are undefined and return `NA`, with a
`cograph_negative_weights` warning.

## See also

[`filter_nodes`](https://sonsoles.me/cograph/reference/filter_nodes.md),
[`select_neighbors`](https://sonsoles.me/cograph/reference/select_neighbors.md),
[`select_component`](https://sonsoles.me/cograph/reference/select_component.md),
[`select_top`](https://sonsoles.me/cograph/reference/select_top.md)

## Examples

``` r
select_nodes(regulation_net, top = 3, by = "pagerank")
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
