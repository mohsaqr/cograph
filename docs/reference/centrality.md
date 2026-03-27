# Calculate Network Centrality Measures

Computes centrality measures for nodes in a network and returns a tidy
data frame. Accepts matrices, igraph objects, cograph_network, or tna
objects.

## Usage

``` r
centrality(
  x,
  measures = "all",
  mode = "all",
  normalized = FALSE,
  weighted = TRUE,
  directed = NULL,
  loops = TRUE,
  simplify = "sum",
  digits = NULL,
  sort_by = NULL,
  cutoff = -1,
  invert_weights = NULL,
  alpha = 1,
  damping = 0.85,
  personalized = NULL,
  transitivity_type = "local",
  isolates = "nan",
  lambda = 1,
  k = 3,
  states = NULL,
  ...
)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object)

- measures:

  Which measures to calculate. Default "all" calculates all available
  measures. Can be a character vector of measure names: "degree",
  "strength", "betweenness", "closeness", "eigenvector", "pagerank",
  "authority", "hub", "eccentricity", "coreness", "constraint",
  "transitivity", "harmonic", "diffusion", "leverage", "kreach",
  "alpha", "power", "subgraph", "laplacian", "load",
  "current_flow_closeness", "current_flow_betweenness", "voterank",
  "percolation".

- mode:

  For directed networks: "all", "in", or "out". Affects degree,
  strength, closeness, eccentricity, coreness, and harmonic centrality.

- normalized:

  Logical. Normalize values to 0-1 range by dividing by max. For
  closeness, this is passed directly to igraph (proper normalization).

- weighted:

  Logical. Use edge weights if available. Default TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- loops:

  Logical. If TRUE (default), keep self-loops. Set to FALSE to remove
  them before calculation.

- simplify:

  How to combine multiple edges between the same node pair. Options:
  "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
  multiple edges.

- digits:

  Integer or NULL. Round all numeric columns to this many decimal
  places. Default NULL (no rounding).

- sort_by:

  Character or NULL. Column name to sort results by (descending order).
  Default NULL (original node order).

- cutoff:

  Maximum path length to consider for betweenness and closeness. Default
  -1 (no limit). Set to a positive value for faster computation on large
  networks at the cost of accuracy.

- invert_weights:

  Logical or NULL. For path-based measures (betweenness, closeness,
  harmonic, eccentricity, kreach), should weights be inverted so that
  higher weights mean shorter paths? Default NULL which auto-detects:
  TRUE for tna objects (transition probabilities), FALSE otherwise
  (matching igraph/sna). Set explicitly to TRUE for strength/frequency
  weights (qgraph style) or FALSE for distance/cost weights.

- alpha:

  Numeric. Exponent for weight transformation when
  `invert_weights = TRUE`. Distance is computed as `1 / weight^alpha`.
  Default 1. Higher values increase the influence of weight differences
  on path lengths.

- damping:

  PageRank damping factor. Default 0.85. Must be between 0 and 1.

- personalized:

  Named numeric vector for personalized PageRank. Default NULL (standard
  PageRank). Values should sum to 1.

- transitivity_type:

  Type of transitivity to calculate: "local" (default), "global",
  "undirected", "localundirected", "barrat" (weighted), or "weighted".

- isolates:

  How to handle isolate nodes in transitivity calculation: "nan"
  (default) returns NaN, "zero" returns 0.

- lambda:

  Diffusion scaling factor for diffusion centrality. Default 1.

- k:

  Path length parameter for geodesic k-path centrality. Default 3.

- states:

  Named numeric vector of percolation states (0-1) for percolation
  centrality. Each value represents how "activated" or "infected" a node
  is. Default NULL (all nodes get state 1, equivalent to betweenness).

- ...:

  Additional arguments (currently unused)

## Value

A data frame with columns:

- `node`: Node labels/names

- One column per measure, with mode suffix for directional measures
  (e.g., `degree_in`, `closeness_all`)

## Details

The following centrality measures are available:

- degree:

  Count of edges (supports mode: in/out/all)

- strength:

  Weighted degree (supports mode: in/out/all)

- betweenness:

  Shortest path centrality

- closeness:

  Inverse distance centrality (supports mode: in/out/all)

- eigenvector:

  Influence-based centrality

- pagerank:

  Random walk centrality (supports damping and personalization)

- authority:

  HITS authority score

- hub:

  HITS hub score

- eccentricity:

  Maximum distance to other nodes (supports mode)

- coreness:

  K-core membership (supports mode: in/out/all)

- constraint:

  Burt's constraint (structural holes)

- transitivity:

  Local clustering coefficient (supports multiple types)

- harmonic:

  Harmonic centrality - handles disconnected graphs better than
  closeness (supports mode: in/out/all)

- diffusion:

  Diffusion degree centrality - sum of scaled degrees of node and its
  neighbors (supports mode: in/out/all, lambda scaling)

- leverage:

  Leverage centrality - measures influence over neighbors based on
  relative degree differences (supports mode: in/out/all)

- kreach:

  Geodesic k-path centrality - count of nodes reachable within distance
  k (supports mode: in/out/all, k parameter)

- alpha:

  Alpha/Katz centrality - influence via paths, penalized by distance.
  Similar to eigenvector but includes exogenous contribution

- power:

  Bonacich power centrality - measures influence based on connections to
  other influential nodes

- subgraph:

  Subgraph centrality - participation in closed loops/walks, weighting
  shorter loops more heavily

- laplacian:

  Laplacian centrality using Qi et al. (2012) local formula. Matches
  NetworkX and centiserve::laplacian()

- load:

  Load centrality - fraction of all shortest paths through node, similar
  to betweenness but weights paths by 1/count

- current_flow_closeness:

  Information centrality - closeness based on electrical current flow
  (requires connected graph)

- current_flow_betweenness:

  Random walk betweenness - betweenness based on current flow rather
  than shortest paths (requires connected graph)

- voterank:

  VoteRank - identifies influential spreaders via iterative voting
  mechanism. Returns normalized rank (1 = most influential)

- percolation:

  Percolation centrality - importance for spreading processes. Uses node
  states (0-1) to weight paths. When all states equal, equivalent to
  betweenness. Useful for epidemic/information spreading analysis.

## Examples

``` r
# Basic usage with matrix
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality(adj)
#>   node degree_all strength_all closeness_all eccentricity_all coreness_all
#> 1    A          2            2           0.5                1            2
#> 2    B          2            2           0.5                1            2
#> 3    C          2            2           0.5                1            2
#>   harmonic_all diffusion_all leverage_all kreach_all alpha_all power_all
#> 1            2             6            0          2        -1        -1
#> 2            2             6            0          2        -1        -1
#> 3            2             6            0          2        -1        -1
#>   betweenness eigenvector  pagerank authority hub constraint transitivity
#> 1           0           1 0.3333333         1   1      1.125            1
#> 2           0           1 0.3333333         1   1      1.125            1
#> 3           0           1 0.3333333         1   1      1.125            1
#>   subgraph laplacian load current_flow_closeness current_flow_betweenness
#> 1 2.708272        14    5                    1.5                0.3333333
#> 2 2.708272        14    5                    1.5                0.3333333
#> 3 2.708272        14    5                    1.5                0.3333333
#>    voterank percolation
#> 1 1.0000000           0
#> 2 0.6666667           0
#> 3 0.3333333           0

# Specific measures
centrality(adj, measures = c("degree", "betweenness"))
#>   node degree_all betweenness
#> 1    A          2           0
#> 2    B          2           0
#> 3    C          2           0

# Directed network with normalization
centrality(adj, mode = "in", normalized = TRUE)
#>   node degree_in strength_in closeness_in eccentricity_in coreness_in
#> 1    A         1           1            1               1           1
#> 2    B         1           1            1               1           1
#> 3    C         1           1            1               1           1
#>   harmonic_in diffusion_in leverage_in kreach_in alpha_in power_in betweenness
#> 1           1            1           0         1       -1       -1           0
#> 2           1            1           0         1       -1       -1           0
#> 3           1            1           0         1       -1       -1           0
#>   eigenvector pagerank authority hub constraint transitivity subgraph laplacian
#> 1           1        1         1   1          1            1        1         1
#> 2           1        1         1   1          1            1        1         1
#> 3           1        1         1   1          1            1        1         1
#>   load current_flow_closeness current_flow_betweenness  voterank percolation
#> 1    1                      1                        1 1.0000000           0
#> 2    1                      1                        1 0.6666667           0
#> 3    1                      1                        1 0.3333333           0

# Sort by pagerank
centrality(adj, sort_by = "pagerank", digits = 3)
#>   node degree_all strength_all closeness_all eccentricity_all coreness_all
#> 1    A          2            2           0.5                1            2
#> 2    B          2            2           0.5                1            2
#> 3    C          2            2           0.5                1            2
#>   harmonic_all diffusion_all leverage_all kreach_all alpha_all power_all
#> 1            2             6            0          2        -1        -1
#> 2            2             6            0          2        -1        -1
#> 3            2             6            0          2        -1        -1
#>   betweenness eigenvector pagerank authority hub constraint transitivity
#> 1           0           1    0.333         1   1      1.125            1
#> 2           0           1    0.333         1   1      1.125            1
#> 3           0           1    0.333         1   1      1.125            1
#>   subgraph laplacian load current_flow_closeness current_flow_betweenness
#> 1    2.708        14    5                    1.5                    0.333
#> 2    2.708        14    5                    1.5                    0.333
#> 3    2.708        14    5                    1.5                    0.333
#>   voterank percolation
#> 1    1.000           0
#> 2    0.667           0
#> 3    0.333           0

# PageRank with custom damping
centrality(adj, measures = "pagerank", damping = 0.9)
#>   node  pagerank
#> 1    A 0.3333333
#> 2    B 0.3333333
#> 3    C 0.3333333

# Harmonic centrality (better for disconnected graphs)
centrality(adj, measures = "harmonic")
#>   node harmonic_all
#> 1    A            2
#> 2    B            2
#> 3    C            2

# Global transitivity
centrality(adj, measures = "transitivity", transitivity_type = "global")
#>   node transitivity
#> 1    A            1
#> 2    B            1
#> 3    C            1
```
