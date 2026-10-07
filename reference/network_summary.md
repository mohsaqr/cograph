# Network-Level Summary Statistics

Computes network-level statistics and returns them as a one-row data
frame. The basic set covers size, density, connectivity, path lengths,
centralization, transitivity, reciprocity and degree assortativity.

## Usage

``` r
network_summary(
  x,
  directed = NULL,
  weighted = TRUE,
  mode = "all",
  loops = TRUE,
  simplify = "sum",
  detailed = FALSE,
  extended = FALSE,
  digits = 3,
  ...
)
```

## Arguments

- x:

  Network input: matrix, igraph, network, cograph_network, or tna object

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- weighted:

  Logical. Use edge weights for strength, shortest-path, and centrality
  calculations where the underlying igraph routine accepts them. Default
  TRUE.

- mode:

  For directed networks: "all", "in", or "out". Used by the degree,
  strength and closeness statistics added by `detailed = TRUE`. Default
  "all".

- loops:

  Logical. If TRUE (default), keep self-loops. Set FALSE to remove them.

- simplify:

  How to combine multiple edges between the same node pair. Options:
  "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
  multiple edges.

- detailed:

  Logical. If TRUE, add 11 summary statistics of node-level centralities
  to the 16 basic metrics. Default FALSE.

- extended:

  Logical. If TRUE, add 8 structural metrics (girth, radius, vertex
  connectivity, clique size, cut vertices, bridges, global and local
  efficiency). Default FALSE.

- digits:

  Integer. Round numeric results to this many decimal places. Default 3.
  NULL skips rounding.

- ...:

  Additional arguments (currently unused)

## Value

A data frame with one row. The basic measures are always computed:

- node_count:

  Number of nodes in the network

- edge_count:

  Number of edges in the network

- density:

  Edge density (proportion of possible edges)

- component_count:

  Number of connected components

- diameter:

  Longest shortest path in the network

- mean_distance:

  Average shortest path length

- min_cut:

  Minimum number of edges whose removal disconnects the network. Edge
  weights are not used.

- centralization_degree:

  Degree centralization over all ties (0-1)

- centralization_in_degree:

  In-degree centralization (directed only)

- centralization_out_degree:

  Out-degree centralization (directed only)

- centralization_betweenness:

  Betweenness centralization (0-1)

- centralization_closeness:

  Closeness centralization (0-1)

- centralization_eigen:

  Eigenvector centralization (0-1)

- transitivity:

  Global clustering coefficient

- reciprocity:

  Proportion of mutual edges (directed only)

- assortativity_degree:

  Degree assortativity coefficient

The extended measures are added when `extended = TRUE`:

- girth:

  Length of shortest cycle (Inf if acyclic)

- radius:

  Minimum eccentricity over all nodes

- vertex_connectivity:

  Minimum nodes to remove to disconnect graph

- largest_clique_size:

  Size of the largest complete subgraph

- cut_vertex_count:

  Number of articulation points (cut vertices)

- bridge_count:

  Number of bridge edges

- global_efficiency:

  Average inverse shortest path length

- local_efficiency:

  Average local efficiency across nodes

The detailed measures are added when `detailed = TRUE`:

- mean_degree, sd_degree, median_degree:

  Degree distribution statistics

- mean_strength, sd_strength:

  Weighted degree statistics

- mean_betweenness:

  Average betweenness centrality

- mean_closeness:

  Average closeness centrality

- mean_eigenvector:

  Average eigenvector centrality

- mean_pagerank:

  Average PageRank

- mean_constraint:

  Average Burt's constraint

- mean_local_transitivity:

  Average local clustering coefficient

## Examples

``` r
network_summary(regulation_net)
#>   node_count edge_count density component_count diameter mean_distance min_cut
#> 1         10         30   0.333               1     0.97         0.435       1
#>   centralization_degree centralization_in_degree centralization_out_degree
#> 1                 0.123                    0.333                     0.222
#>   centralization_betweenness centralization_closeness centralization_eigen
#> 1                      0.149                    0.238                0.479
#>   transitivity reciprocity assortativity_degree
#> 1        0.423       0.111               -0.116
```
