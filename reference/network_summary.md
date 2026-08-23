# Network-Level Summary Statistics

Computes comprehensive network-level statistics for a network. Returns a
data frame with one row containing various metrics including density,
centralization scores, transitivity, and more.

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

  For directed networks: "all", "in", or "out". Affects degree-based
  calculations. Default "all".

- loops:

  Logical. If TRUE (default), keep self-loops. Set FALSE to remove them.

- simplify:

  How to combine multiple edges between the same node pair. Options:
  "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
  multiple edges.

- detailed:

  Logical. If TRUE, include mean/sd centrality statistics. Default FALSE
  returns 18 basic metrics; TRUE returns 29 metrics.

- extended:

  Logical. If TRUE, include additional structural metrics (girth,
  radius, clique size, cut vertices, bridges, efficiency). Default
  FALSE.

- digits:

  Integer. Round numeric results to this many decimal places. Default 3.

- ...:

  Additional arguments (currently unused)

## Value

A data frame with one row containing network-level statistics:

**Basic measures (always computed):**

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

  Minimum cut value (edge connectivity)

- centralization_degree:

  Degree centralization (0-1)

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

- hub_score:

  Maximum hub score (HITS algorithm)

- authority_score:

  Maximum authority score (HITS algorithm)

**Extended measures (when extended = TRUE):**

- girth:

  Length of shortest cycle (Inf if acyclic)

- radius:

  Minimum eccentricity (shortest max-distance from any node)

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

**Detailed measures (when detailed = TRUE):**

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
# Basic usage with adjacency matrix
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
network_summary(adj)
#>   node_count edge_count density component_count diameter mean_distance min_cut
#> 1          3          3       1               1        1             1       2
#>   centralization_degree centralization_in_degree centralization_out_degree
#> 1                     0                       NA                        NA
#>   centralization_betweenness centralization_closeness centralization_eigen
#> 1                          0                        0                    0
#>   transitivity reciprocity assortativity_degree hub_score authority_score
#> 1            1          NA                  NaN        NA              NA

# With detailed statistics
network_summary(adj, detailed = TRUE)
#>   node_count edge_count density component_count diameter mean_distance min_cut
#> 1          3          3       1               1        1             1       2
#>   centralization_degree centralization_in_degree centralization_out_degree
#> 1                     0                       NA                        NA
#>   centralization_betweenness centralization_closeness centralization_eigen
#> 1                          0                        0                    0
#>   transitivity reciprocity assortativity_degree hub_score authority_score
#> 1            1          NA                  NaN        NA              NA
#>   mean_degree sd_degree median_degree mean_strength sd_strength
#> 1           2         0             2             2           0
#>   mean_betweenness mean_closeness mean_eigenvector mean_pagerank
#> 1                0            0.5                1         0.333
#>   mean_constraint mean_local_transitivity
#> 1           1.125                       1

# With extended structural metrics
network_summary(adj, extended = TRUE)
#>   node_count edge_count density component_count diameter mean_distance min_cut
#> 1          3          3       1               1        1             1       2
#>   centralization_degree centralization_in_degree centralization_out_degree
#> 1                     0                       NA                        NA
#>   centralization_betweenness centralization_closeness centralization_eigen
#> 1                          0                        0                    0
#>   transitivity reciprocity assortativity_degree hub_score authority_score girth
#> 1            1          NA                  NaN        NA              NA     3
#>   radius vertex_connectivity largest_clique_size cut_vertex_count bridge_count
#> 1      1                   2                   3                0            0
#>   global_efficiency local_efficiency
#> 1                 1                1

# All metrics
network_summary(adj, detailed = TRUE, extended = TRUE)
#>   node_count edge_count density component_count diameter mean_distance min_cut
#> 1          3          3       1               1        1             1       2
#>   centralization_degree centralization_in_degree centralization_out_degree
#> 1                     0                       NA                        NA
#>   centralization_betweenness centralization_closeness centralization_eigen
#> 1                          0                        0                    0
#>   transitivity reciprocity assortativity_degree hub_score authority_score girth
#> 1            1          NA                  NaN        NA              NA     3
#>   radius vertex_connectivity largest_clique_size cut_vertex_count bridge_count
#> 1      1                   2                   3                0            0
#>   global_efficiency local_efficiency mean_degree sd_degree median_degree
#> 1                 1                1           2         0             2
#>   mean_strength sd_strength mean_betweenness mean_closeness mean_eigenvector
#> 1             2           0                0            0.5                1
#>   mean_pagerank mean_constraint mean_local_transitivity
#> 1         0.333           1.125                       1

# From igraph object
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- igraph::sample_gnp(20, 0.3)
  network_summary(g)
}
#>   node_count edge_count density component_count diameter mean_distance min_cut
#> 1         20         58   0.305               1        3         1.816       2
#>   centralization_degree centralization_in_degree centralization_out_degree
#> 1                 0.221                       NA                        NA
#>   centralization_betweenness centralization_closeness centralization_eigen
#> 1                      0.104                     0.21                0.426
#>   transitivity reciprocity assortativity_degree hub_score authority_score
#> 1        0.328          NA               -0.134        NA              NA
```
