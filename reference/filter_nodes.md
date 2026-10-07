# Filter Nodes by Metadata or Centrality

Filter nodes using dplyr-style expressions on any node column or
centrality measure. Returns a cograph_network object by default
(universal format), or optionally a matrix, igraph, statnet network or
tna object when `keep_format = TRUE` and the input used one of those
formats.

## Usage

``` r
filter_nodes(
  x,
  ...,
  keep_edges = c("internal", "none"),
  keep_format = FALSE,
  directed = NULL,
  .keep_edges = NULL
)

subset_nodes(
  x,
  ...,
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

  Filter expressions using any node column or centrality measure.
  Available variables include:

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

  Structural context and predicates

  :   The same vocabulary
      [`select_nodes()`](https://sonsoles.me/cograph/reference/select_nodes.md)
      documents, for example `component`, `component_size`, `k_core`,
      `is_isolated`, `is_cut`, `local_transitivity`.

  Examples: `degree >= 3`, `label %in% c("A", "B")`,
  `pagerank > 0.1 & degree >= 2`.

  On a network with negative edge weights, `betweenness`, `closeness`
  and `pagerank` are undefined: they return `NA` with a
  `cograph_negative_weights` warning.

- keep_edges:

  How to handle edges. One of:

  `"internal"`

  :   (default) Keep only edges between remaining nodes

  `"none"`

  :   Remove all edges

- keep_format:

  Logical. If TRUE, matrix, igraph, statnet network and tna inputs are
  returned in that format. Default FALSE returns cograph_network
  (universal format).

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected. Only used for
  non-cograph_network inputs.

- .keep_edges:

  Deprecated. Use `keep_edges`.

## Value

A cograph_network object with filtered nodes. If `keep_format = TRUE`,
matrix, igraph, statnet network and tna inputs are converted back to
that type.

## See also

[`filter_edges`](https://sonsoles.me/cograph/reference/filter_edges.md),
[`splot`](https://sonsoles.me/cograph/reference/splot.md),
`subset_nodes`

## Examples

``` r
filter_nodes(regulation_net, degree >= 7)
#> Cograph network: 3 nodes, 4 edges ( directed )
#> Source: matrix 
#>   Nodes (3): Plan, Monitor, Create
#>   Edges: 4 / 6 (density: 66.7%)
#>   Weights: [0.130, 0.370]  |  mean: 0.217
#>   Strongest edges:
#>     Monitor -> Create  0.370
#>     Plan -> Create  0.200
#>     Create -> Monitor  0.170
#>     Plan -> Monitor  0.130
#> Layout: none 
#>   Use as.data.frame() for the edge table, as.data.frame(what = "nodes") for the nodes.
```
