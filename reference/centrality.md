# Calculate Network Centrality Measures

Computes centrality measures for nodes in a network and returns a tidy
data frame. Accepts matrices, igraph objects, cograph_network, or tna
objects.

## Usage

``` r
centrality(
  x,
  type = c("basic", "extended", "all"),
  measures = NULL,
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
  diffusion_method = NULL,
  k = 3,
  states = NULL,
  decay_parameter = 0.5,
  dmnc_epsilon = 1.7,
  membership = NULL,
  katz_alpha = 0.1,
  hubbell_weight = 0.5,
  tna_network = NULL,
  ...
)
```

## Arguments

- x:

  Network input (matrix, igraph, network, cograph_network, tna object)

- type:

  Character scalar selecting a curated tier of measures when `measures`
  is not supplied. One of:

  `"basic"`

  :   (default) 6 canonical measures: `degree`, `strength`, `closeness`,
      `betweenness`, `eigenvector`, `pagerank`.

  `"extended"`

  :   Basic plus commonly-reported second-tier measures (~28 total):
      harmonic, coreness, eccentricity, radiality, lin, decay, load,
      stress, katz, alpha, power, authority, leverage, constraint,
      effective_size, bridging, transitivity, subgraph, diffusion,
      laplacian, kreach, current_flow_betweenness,
      current_flow_closeness.

  `"all"`

  :   Every available measure (87).

  Passing `measures` explicitly overrides `type`.

- measures:

  Character vector of specific measure names to compute. When `NULL`
  (default) the tier selected by `type` is used. Accepts `"all"` as a
  shortcut for every measure. Any custom vector of valid measure names
  is also accepted. **Core** (igraph-backed): "degree", "strength",
  "betweenness", "closeness", "eigenvector", "pagerank", "authority",
  "hub", "eccentricity", "coreness", "constraint", "transitivity",
  "harmonic", "alpha", "power", "subgraph". **Native**: "diffusion",
  "leverage", "kreach", "laplacian", "load", "current_flow_closeness",
  "current_flow_betweenness", "voterank", "percolation".
  **Distance-based**: "radiality", "lin", "decay", "residual_closeness",
  "dangalchev", "generalized_closeness", "harary", "average_distance",
  "barycenter", "wiener", "closeness_vitality". **Spectral/walk**:
  "communicability", "communicability_betweenness", "random_walk".
  **Path-based**: "stress", "flow_betweenness". **Local/neighborhood**:
  "lobby", "entropy", "semilocal", "clusterrank", "bottleneck",
  "centroid", "mnc", "dmnc", "lac", "topological_coefficient",
  "bridging", "local_bridging", "effective_size", "diversity",
  "cross_clique", "markov". **Influence**: "integration", "expected",
  "gilschmidt". **Directed-only**: "salsa", "leaderrank",
  "trophic_level", "pairwisedis", "prestige_domain",
  "prestige_domain_proximity". **Community-aware** (require
  `membership`): "participation", "within_module_z", "gateway",
  "brokerage_coordinator", "brokerage_itinerant",
  "brokerage_representative", "brokerage_gatekeeper",
  "brokerage_liaison" (the last 5 also require a directed graph; see
  [`centrality_brokerage_coordinator`](https://sonsoles.me/cograph/reference/centrality_brokerage_coordinator.md)).
  **Zoo (batch 2)**: "gravity", "collective_influence", "local_hindex",
  "hindex_strength", "onion", "second_order", "infection",
  "nonbacktracking", "spanning_tree". **Classical (batch 3,
  reference-validated)**: "katz" (Katz 1953), "hubbell" (Hubbell 1965),
  "information" (Stephenson-Zelen 1989), "reaching_local" (Mones et al.
  2012). See
  [`centrality_katz`](https://sonsoles.me/cograph/reference/centrality_katz.md),
  [`centrality_hubbell`](https://sonsoles.me/cograph/reference/centrality_hubbell.md),
  [`centrality_information`](https://sonsoles.me/cograph/reference/centrality_information.md),
  [`centrality_pairwisedis`](https://sonsoles.me/cograph/reference/centrality_pairwisedis.md),
  [`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md).
  **Psychometric (signed-weight)**: "expected_influence_1",
  "expected_influence_2" (Robinaugh, Millner & McNally 2016). Unlike
  strength (which takes \|w\|), expected influence keeps the sign — the
  appropriate measure when edges can be negative (partial-correlation,
  glasso, signed correlation networks).

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
  "undirected", "localundirected", "barrat" (weighted), "weighted", or
  "onnela". The first six dispatch to
  [`igraph::transitivity()`](https://r.igraph.org/reference/transitivity.html);
  `"onnela"` computes the Onnela / Holme weighted clustering coefficient
  on the symmetrized matrix (`wcc(x + t(x))`) and matches
  `tna::centralities(., "Clustering")` byte-for-byte. Auto-set to
  `"onnela"` when `tna_network = TRUE` and the user did not pass an
  explicit value.

- isolates:

  How to handle isolate nodes in transitivity calculation: "nan"
  (default) returns NaN, "zero" returns 0.

- lambda:

  Diffusion scaling factor for diffusion centrality. Default 1. Only
  used when `diffusion_method = "kandhway_kuri"`.

- diffusion_method:

  Character or NULL. Selects the diffusion-centrality formula.
  `"kandhway_kuri"` (Kandhway & Kuri, 2014) computes the 1-hop
  binary-degree neighborhood sum \\\lambda d_v + \lambda \sum\_{u \in
  N(v)} d_u\\. `"power_series"` computes the matrix power series
  \\\mathrm{rowSums}(P + P^2 + \ldots + P^n)\\ on the (optionally
  diagonal-zeroed) weighted matrix and matches
  `tna::centralities(., measures = "Diffusion")` when `loops = FALSE`.
  Default NULL auto-detects: `"power_series"` for tna objects
  (transition probabilities), `"kandhway_kuri"` otherwise.

- k:

  Path length parameter for geodesic k-path centrality. Default 3.

- states:

  Named numeric vector of percolation states (0-1) for percolation
  centrality. Each value represents how "activated" or "infected" a node
  is. Default NULL (all nodes get state 1, equivalent to betweenness).

- decay_parameter:

  Numeric. Decay parameter for decay and generalized closeness
  centrality. Default 0.5. Must be between 0 and 1.

- dmnc_epsilon:

  Numeric. Epsilon exponent for DMNC (Density of Maximum Neighborhood
  Component). Default 1.7 as recommended by Lin et al. (2008).
  centiserve uses 1.67 (four-community assumption). Must be between 1
  and 2.

- membership:

  Integer vector of community assignments (one per node) for
  community-aware measures: participation, within_module_z, gateway.
  Default NULL. Required when requesting these measures.

- katz_alpha:

  Attenuation factor for Katz centrality. Must satisfy \\\alpha \< 1 /
  \rho(A)\\. Default 0.1 (matches centiserve and NetworkX conventions).
  Only used when `"katz"` is in `measures`.

- hubbell_weight:

  Weight factor \\w\\ for Hubbell centrality. Must satisfy \\w \cdot
  \rho(W) \le 1\\ for solvability. Default 0.5. Only used when
  `"hubbell"` is in `measures`.

- tna_network:

  Logical or NULL. Umbrella switch that forces tna-style conventions
  across all measures. `NULL` (default) auto-detects from the input
  class — TRUE iff `x` is a `tna` or related sequence-network object.
  `TRUE` forces tna conventions even on raw matrices:
  `invert_weights = TRUE`, `loops = FALSE`,
  `diffusion_method = "power_series"`, `transitivity_type = "onnela"`.
  `FALSE` suppresses all tna defaults even for tna inputs, giving the
  cograph defaults verbatim. Precedence: any arg the user passes
  explicitly always wins over `tna_network`.

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

- radiality:

  Radiality centrality (centiserve). Sum of (diam + 1 - d) normalized by
  n-1.

- lin:

  Lin's centrality. Reachable nodes squared divided by sum of distances.

- decay:

  Decay centrality. Sum of delta^d for parameter delta.

- residual_closeness:

  Residual closeness. Sum of 1/2^d.

- dangalchev:

  Dangalchev closeness (alias for residual closeness).

- generalized_closeness:

  Generalized closeness. Sum of alpha^d.

- harary:

  Harary centrality. Sum of 1/d^2 for all reachable pairs.

- average_distance:

  Average distance (centiserve). Sum of distances / (n+1).

- barycenter:

  Barycenter centrality. 1 / sum of distances.

- wiener:

  Wiener index. Total sum of shortest path distances from node.

- closeness_vitality:

  Closeness vitality. Drop in Wiener index when node removed.

- communicability:

  Total communicability. Row sums of matrix exponential.

- communicability_betweenness:

  Communicability betweenness. Fraction of communicability through each
  node.

- random_walk:

  Random walk centrality. Inverse sum of random walk distances (requires
  connected graph).

- stress:

  Stress centrality. Number of shortest paths through node.

- flow_betweenness:

  Flow betweenness. Max-flow based betweenness.

- lobby:

  Lobby index (h-index of neighborhood).

- entropy:

  Graph entropy centrality. Entropy change on node removal.

- semilocal:

  Semi-local centrality. Triple-nested neighborhood sum.

- clusterrank:

  ClusterRank. Clustering coefficient times neighbor degree sum.

- bottleneck:

  Bottleneck centrality. Count of shortest path trees where node is
  critical.

- centroid:

  Centroid value. Minimum f(v,i) across all nodes.

- mnc:

  Maximum Neighborhood Component size.

- dmnc:

  Density of Maximum Neighborhood Component.

- topological_coefficient:

  Topological coefficient. Shared neighbor ratio.

- bridging:

  Bridging centrality. Betweenness times bridging coefficient.

- local_bridging:

  Local bridging. (1/degree) times bridging coefficient.

- effective_size:

  Burt's effective size. Degree minus redundancy.

- diversity:

  Diversity centrality. Shannon entropy of edge weight distribution.

- cross_clique:

  Cross-clique connectivity. Count of cliques containing node.

- markov:

  Markov centrality. Inverse mean first passage time (requires connected
  graph).

- integration:

  Integration centrality. Distance-based influence.

- expected:

  Expected centrality. Sum of neighbor degrees.

- gilschmidt:

  Gil-Schmidt power index. Sum of 1/d normalized by n-1.

- salsa:

  SALSA authority scores (directed graphs only).

- leaderrank:

  LeaderRank. PageRank with ground node (directed graphs only).

- participation:

  Participation coefficient. Diversity of inter-community connections
  (requires `membership`).

- within_module_z:

  Within-module degree z-score. Intra-community connectivity (requires
  `membership`).

- gateway:

  Gateway coefficient. Inter-community brokerage weighted by centrality
  (requires `membership`).

## Examples

``` r
# Basic usage with matrix
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(adj) <- colnames(adj) <- c("A", "B", "C")
centrality(adj)
#>   node degree_all strength_all closeness_all betweenness eigenvector  pagerank
#> 1    A          2            2           0.5           0           1 0.3333333
#> 2    B          2            2           0.5           0           1 0.3333333
#> 3    C          2            2           0.5           0           1 0.3333333

# Specific measures
centrality(adj, measures = c("degree", "betweenness"))
#>   node degree_all betweenness
#> 1    A          2           0
#> 2    B          2           0
#> 3    C          2           0

# Directed network with normalization
centrality(adj, mode = "in", normalized = TRUE)
#>   node degree_in strength_in closeness_in betweenness eigenvector pagerank
#> 1    A         1           1            1           0           1        1
#> 2    B         1           1            1           0           1        1
#> 3    C         1           1            1           0           1        1

# Sort by pagerank
centrality(adj, sort_by = "pagerank", digits = 3)
#>   node degree_all strength_all closeness_all betweenness eigenvector pagerank
#> 1    A          2            2           0.5           0           1    0.333
#> 2    B          2            2           0.5           0           1    0.333
#> 3    C          2            2           0.5           0           1    0.333

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
