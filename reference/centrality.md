# Calculate Network Centrality Measures

Computes centrality measures for nodes in a network and returns a tidy
data frame. Accepts matrices, edge-list data frames, igraph objects,
cograph_network, or tna objects.

## Usage

``` r
centrality(
  x,
  type = c("basic", "extended", "all"),
  measures = NULL,
  include = NULL,
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
  shapley_k = 2,
  shapley_cutoff = 2,
  s_shell_a = 0.5,
  discount_p = 0.01,
  ncvote_theta = 0.5,
  comm_r = "max_intra",
  ld_radius = 2,
  enrenew_depth = 2,
  voterank_lambda = 0.1,
  contraction_rho = 5,
  wks_alpha = 1,
  wks_beta = 1,
  renewed_threshold = 2,
  kpath_k = 3,
  kpath_len = 3,
  epc_threshold = 0.5,
  epc_runs = 1000,
  epc_seed = NULL,
  betweenness_delta = 1,
  closeness_delta = 1,
  gravity_mass = "kshell",
  gravity_radius = 3,
  mdd_lambda = 0.7,
  volume_radius = 2,
  diffusion_q = 1,
  diffusion_steps = 3,
  ds_beta = 0.1,
  ds_mu = 1,
  ds_steps = 5,
  cda_alpha = 0.5,
  icc_alpha = 0.2,
  exogenous_base = "reverse_closeness",
  wlr_alpha = 1,
  alr_h_mode = "all",
  grc_gamma = 1,
  rwd_decay = 0.5,
  rwd_node_weights = NULL,
  linerank_aggregation = "probability",
  bridging_steps = 2,
  bridging_values = NULL,
  proximal_variant = "source",
  exf_alpha = 2,
  beta_direction = "positive",
  ninl_order = 3,
  ninl_radius = NULL,
  map_flow = "unrecorded",
  map_convention = "paper",
  sr_prior = 0,
  mcgm_radius = 2,
  mcgm_alpha = NULL,
  dkgm_radius = 2,
  nd_order = 2,
  nd_decay = 0.2,
  nd_mass = "degree",
  ira_mass = "coreness",
  ira_alpha = 1,
  ira_tol = 1e-06,
  ira_max_iter = 1000,
  iira_beta = 0.2,
  iira_steps = 50,
  hcc_delta = 0.5,
  lhc_radius = 2,
  tpr_alpha = 0.85,
  tpr_k = 0.85,
  tpr_decay = 1,
  tpr_tol = 1e-14,
  tpr_max_iter = 1000,
  rsp_beta = 0.01,
  rsp_cost = c("inverse", "weight"),
  re_indexes = c("degree", "closeness", "betweenness", "constraint"),
  re_negative = NULL,
  tna_network = NULL,
  psych_network = NULL,
  ...
)
```

## Arguments

- x:

  Network input (matrix, edge-list data frame, igraph, network,
  cograph_network, tna object).

- type:

  Character scalar selecting a curated tier of measures when `measures`
  is not supplied. One of:

  `"basic"`

  :   The default. Six measures: `degree`, `strength`, `closeness`,
      `betweenness`, `eigenvector`, `pagerank`.

  `"extended"`

  :   The basic tier plus harmonic, coreness, eccentricity, radiality,
      lin, decay, load, stress, katz, alpha, power, authority, leverage,
      constraint, effective_size, bridging, transitivity, subgraph,
      diffusion, laplacian, kreach, current_flow_betweenness and
      current_flow_closeness.

  `"all"`

  :   Every measure except the costly ones (see `include` and
      [`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)).

  Passing `measures` explicitly overrides `type`.

- measures:

  Character vector of measure names to compute. When `NULL` (default)
  the tier selected by `type` is used. `"all"` is a shortcut for
  `type = "all"`. Unknown names raise an error.
  [`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
  returns every valid name together with its orientation, mode support,
  partition requirement, weight use and cost. The measures fall into
  these groups:

  Core

  :   "degree", "strength", "betweenness", "closeness", "eigenvector",
      "pagerank", "authority", "hub", "eccentricity", "coreness",
      "constraint", "transitivity", "harmonic", "alpha", "power",
      "subgraph".

  Flow and spreading

  :   "diffusion", "leverage", "kreach", "laplacian", "load",
      "current_flow_closeness", "current_flow_betweenness", "voterank",
      "percolation".

  Distance-based

  :   "radiality", "lin", "decay", "residual_closeness", "dangalchev",
      "generalized_closeness", "harary", "average_distance",
      "barycenter", "wiener", "closeness_vitality".

  Spectral and walk-based

  :   "communicability", "communicability_betweenness", "random_walk".

  Path-based

  :   "stress", "flow_betweenness".

  Local and neighborhood

  :   "lobby", "entropy", "semilocal", "clusterrank", "bottleneck",
      "centroid", "mnc", "dmnc", "lac", "topological_coefficient",
      "bridging", "local_bridging", "effective_size", "diversity",
      "cross_clique", "markov".

  Influence

  :   "integration", "expected", "gilschmidt".

  Directed only

  :   "salsa", "leaderrank", "trophic_level", "pairwisedis",
      "prestige_domain", "prestige_domain_proximity".

  Community-aware (require `membership`)

  :   "participation", "within_module_z", "gateway",
      "modularity_vitality" (Magelinski et al. 2021),
      "community_hub_bridge" (Ghalmane et al. 2019), "community_based"
      (Zhao et al. 2015), "comm_centrality" (Gupta et al. 2016),
      "community_mediator" (Tulu et al. 2018), and the Gould-Fernandez
      roles "brokerage_coordinator", "brokerage_itinerant",
      "brokerage_representative", "brokerage_gatekeeper",
      "brokerage_liaison", which also require a directed graph. See
      [`centrality_modularity_vitality`](https://sonsoles.me/cograph/reference/centrality_modularity_vitality.md)
      and
      [`centrality_brokerage_coordinator`](https://sonsoles.me/cograph/reference/centrality_brokerage_coordinator.md).

  Spreader identification

  :   "gravity", "collective_influence", "local_hindex",
      "hindex_strength", "onion", "second_order", "infection",
      "nonbacktracking", "spanning_tree". See
      [`centrality_gravity`](https://sonsoles.me/cograph/reference/centrality_gravity.md).

  Classical

  :   "katz" (Katz 1953), "hubbell" (Hubbell 1965), "information"
      (Stephenson and Zelen 1989), "reaching_local" (Mones et al. 2012).
      See
      [`centrality_katz`](https://sonsoles.me/cograph/reference/centrality_katz.md),
      [`centrality_hubbell`](https://sonsoles.me/cograph/reference/centrality_hubbell.md),
      [`centrality_information`](https://sonsoles.me/cograph/reference/centrality_information.md),
      [`centrality_pairwisedis`](https://sonsoles.me/cograph/reference/centrality_pairwisedis.md)
      and
      [`centrality_reaching_local`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md).

  Psychometric

  :   "expected_influence_1", "expected_influence_2" (Robinaugh, Millner
      and McNally 2016). Expected influence keeps the sign of each edge,
      which matters in networks with negative edges such as
      partial-correlation and glasso networks.

  Scaling and dimension

  :   "distance_entropy" (Stella and De Domenico 2018),
      "local_dimension" (Pu et al. 2014), "local_information_dimension"
      (Wen and Deng 2020), "neighborhood_connectivity" (Maslov and
      Sneppen 2002), "local_dimension_fixed" (Silva and Costa 2013),
      "fuzzy_local_dimension" (Wen and Jiang 2019),
      "local_volume_dimension" (Li and Deng 2021). See
      [`centrality_distance_entropy`](https://sonsoles.me/cograph/reference/centrality_distance_entropy.md),
      [`centrality_local_dimension`](https://sonsoles.me/cograph/reference/centrality_local_dimension.md),
      [`centrality_local_information_dimension`](https://sonsoles.me/cograph/reference/centrality_local_information_dimension.md),
      [`centrality_neighborhood_connectivity`](https://sonsoles.me/cograph/reference/centrality_neighborhood_connectivity.md)
      and
      [`centrality_local_dimension_fixed`](https://sonsoles.me/cograph/reference/centrality_local_dimension_fixed.md).

  Games, information and seed selection

  :   "shapley_game1", "shapley_game2", "shapley_game3" (Michalak et al.
      2013), "access_information", "hide_information" (Rosvall et al.
      2005), "rumor" (Shah and Zaman 2011), "entropy_variation_degree",
      "entropy_variation_betweenness" (Ai 2017), "s_shell" (Liu et al.
      2017), "degree_discount", "single_discount" (Chen, Wang and Yang
      2009). See
      [`centrality_shapley_game1`](https://sonsoles.me/cograph/reference/centrality_shapley_game1.md),
      [`centrality_access_information`](https://sonsoles.me/cograph/reference/centrality_access_information.md),
      [`centrality_rumor`](https://sonsoles.me/cograph/reference/centrality_rumor.md),
      [`centrality_community_hub_bridge`](https://sonsoles.me/cograph/reference/centrality_community_hub_bridge.md),
      [`centrality_entropy_variation`](https://sonsoles.me/cograph/reference/centrality_entropy_variation.md),
      [`centrality_s_shell`](https://sonsoles.me/cograph/reference/centrality_s_shell.md),
      [`centrality_degree_discount`](https://sonsoles.me/cograph/reference/centrality_degree_discount.md)
      and
      [`centrality_community_based`](https://sonsoles.me/cograph/reference/centrality_community_based.md).

  VoteRank family

  :   "ncvoterank" (Kumar and Panda 2020), "wvoterank" (Sun et al.
      2019), "enrenew" (Guo et al. 2020), "voterank_plus" (Liu et al.
      2021). See
      [`centrality_ncvoterank`](https://sonsoles.me/cograph/reference/centrality_ncvoterank.md)
      and
      [`centrality_wvoterank`](https://sonsoles.me/cograph/reference/centrality_wvoterank.md).

  Contraction, walks and local structure

  :   "node_contraction", "node_contraction_improved" (Tan et al. 2006;
      Wang et al. 2011), "two_way_rw" (Curado et al. 2022), "heatmap"
      (Duron 2020), "flow_coefficient" (Honey et al. 2007),
      "local_entropy" (Nie et al. 2016), "weighted_h_index" (Gao et al.
      2019), "redundancy" (Burt 1992), "weighted_kshell" (Garas et al.
      2012), "renewed_coreness" (Liu et al. 2015), "geodesic_kpath"
      (Borgatti and Everett 2006). See
      [`centrality_node_contraction`](https://sonsoles.me/cograph/reference/centrality_node_contraction.md),
      [`centrality_two_way_rw`](https://sonsoles.me/cograph/reference/centrality_two_way_rw.md),
      [`centrality_heatmap`](https://sonsoles.me/cograph/reference/centrality_heatmap.md)
      and
      [`centrality_weighted_kshell`](https://sonsoles.me/cograph/reference/centrality_weighted_kshell.md).

  Efficiency, cores and percolation

  :   "local_efficiency" (Latora and Marchiori 2001), "s_core" (Eidsaa
      and Almaas 2013), "fragmentation" (Borgatti 2006), "kpath" (Sade
      1989), "epc" (Lin et al. 2008). See
      [`centrality_local_efficiency`](https://sonsoles.me/cograph/reference/centrality_local_efficiency.md).

  Betweenness and closeness variants

  :   "length_scaled_betweenness" (Brandes 2008), "delta_betweenness"
      and "delta_closeness" (Agneessens et al. 2017), "ego_betweenness"
      (Everett and Borgatti 2005). Bounded-distance (k-) betweenness is
      betweenness with `cutoff = k`. See
      [`centrality_length_scaled_betweenness`](https://sonsoles.me/cograph/reference/centrality_length_scaled_betweenness.md).

  The remaining measures are described under Details.

- include:

  Character vector of costly measures to add to a tier, or `"costly"`
  for all of them. `type = "all"` holds back the measures whose cost
  grows steeply with network size (the `costly` column of
  [`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)).
  A measure named in `measures` is always computed, whatever its cost.
  Unknown names raise a `cograph_unknown_measure` error. Default `NULL`.

- mode:

  For directed networks: "all" (default), "in", or "out". Affects the
  mode-aware measures, whose output columns carry a mode suffix. These
  include degree, strength, closeness, eccentricity, coreness, harmonic,
  diffusion, leverage, kreach, the distance-based measures, most
  community-aware measures, and expected influence (the `mode_aware`
  column of
  [`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)).

- normalized:

  Logical. If `TRUE`, each measure is divided by its maximum, which
  scales non-negative measures to 0-1. A measure whose maximum is not
  positive is left unchanged. Closeness follows igraph's normalization
  instead, multiplying each value by the number of nodes the node
  reaches. Under `psych_network = TRUE` expected influence is divided by
  its maximum absolute value and keeps its sign. Default `FALSE`.

- weighted:

  Logical. Use edge weights if available. Default TRUE.

- directed:

  Logical or NULL. If NULL (default), auto-detect from matrix symmetry.
  Set TRUE to force directed, FALSE to force undirected.

- loops:

  Logical. If TRUE (default), keep self-loops. Set to FALSE to remove
  them before calculation. `tna_network = TRUE` changes the default to
  FALSE.

- simplify:

  How to combine multiple edges between the same node pair (possible
  only from edge-list, cograph_network or igraph input). Options: "sum"
  (default), "mean", "max", "min". `FALSE` and `"none"` also sum them,
  because the network is held as a dense weight matrix, which cannot
  carry parallel edges.

- digits:

  Integer or NULL. Round all numeric columns to this many decimal
  places. Default NULL (no rounding).

- sort_by:

  Character or NULL. Column name to sort results by (descending order).
  Default NULL (original node order). A name that is not a column of the
  result raises an error.

- cutoff:

  Maximum path length to consider for betweenness, closeness, harmonic
  centrality and the distance-based closeness variants (radiality, lin,
  decay, residual_closeness, dangalchev, generalized_closeness, harary,
  average_distance, barycenter, wiener, centroid, closeness_vitality,
  delta_closeness). Default -1 (no limit). A positive value ignores
  longer paths, which shortens computation on large networks and changes
  the values.

- invert_weights:

  Logical or NULL. Whether path- and distance-based measures (for
  example betweenness, closeness, harmonic, k-reach, radiality, decay,
  stress, flow betweenness and related variants) invert the weights, so
  that higher weights mean shorter paths. A message reports the
  inversion. Eccentricity always reads the raw edge weights as
  distances. The default `NULL` is TRUE for tna objects (transition
  probabilities) and FALSE otherwise, as in igraph and sna. TRUE suits
  strength or frequency weights (the qgraph convention) and FALSE suits
  distance or cost weights.

- alpha:

  Numeric. Exponent for weight transformation when
  `invert_weights = TRUE`. Distance is computed as `1 / weight^alpha`.
  Default 1. Higher values increase the influence of weight differences
  on path lengths.

- damping:

  PageRank damping factor. Default 0.85. Must be between 0 and 1.

- personalized:

  Non-negative numeric vector of reset probabilities for personalized
  PageRank, one value per node in node order. Names are not used for
  matching. The vector is rescaled to sum to 1. Default NULL (standard
  PageRank).

- transitivity_type:

  Type of transitivity to calculate: "local" (default), "global",
  "undirected", "localundirected", "barrat" (weighted), "weighted", or
  "onnela". The first six follow the conventions of
  [`igraph::transitivity()`](https://r.igraph.org/reference/transitivity.html).
  `"global"` and `"undirected"` give one graph-level value, repeated on
  every row. `"onnela"` computes the Onnela weighted clustering
  coefficient on the symmetrized matrix `x + t(x)` and matches
  `tna::centralities(x, "Clustering")`. `tna_network = TRUE` changes the
  default to `"onnela"`.

- isolates:

  Value of local transitivity at nodes where it is undefined: "nan"
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

  Distance bound for `"kreach"`, the number of nodes reachable within
  `k` steps. Default 3.

- states:

  Numeric vector of percolation states for percolation centrality, one
  per node. Each value represents how "activated" or "infected" a node
  is. A named vector is matched to the node labels, missing nodes get
  state 1, and values are clipped to 0-1. Default NULL gives every node
  state 1, which makes percolation equal to betweenness divided by
  \\(n-1)(n-2)\\.

- decay_parameter:

  Numeric. Decay parameter for decay and generalized closeness
  centrality. A value between 0 and 1 discounts distant nodes. Default
  0.5.

- dmnc_epsilon:

  Numeric. Epsilon exponent for DMNC (Density of Maximum Neighborhood
  Component). Default 1.7, as recommended by Lin et al. (2008).
  centiserve uses 1.67.

- membership:

  Integer vector of community assignments (one per node) for the
  community-aware measures listed under `measures`. Default NULL.
  Without it those measures warn and return `NA`. `"map_equation"` uses
  it when supplied.

- katz_alpha:

  Attenuation factor for Katz centrality. The Katz series converges only
  for \\\alpha \< 1 / \rho(A)\\. Otherwise the measure raises a
  `cograph_katz_diverged` warning. Default 0.1, the centiserve and
  NetworkX convention. Only used when `"katz"` is in `measures`.

- hubbell_weight:

  Weight factor \\w\\ for Hubbell centrality. Must be positive and
  satisfy \\w \cdot \rho(W) \< 1\\ for solvability; otherwise the
  measure warns and returns `NA`. Default 0.5. Only used when
  `"hubbell"` is in `measures`.

- shapley_k:

  Neighbor threshold \\k\\ for `"shapley_game2"`. Default 2. See
  [`centrality_shapley_game2`](https://sonsoles.me/cograph/reference/centrality_shapley_game1.md).

- shapley_cutoff:

  Hop cutoff for `"shapley_game3"`. Default 2. See
  [`centrality_shapley_game3`](https://sonsoles.me/cograph/reference/centrality_shapley_game1.md).

- s_shell_a:

  Exponent of the asymmetric link weights for `"s_shell"`. A single
  non-negative number; default 0.5. See
  [`centrality_s_shell`](https://sonsoles.me/cograph/reference/centrality_s_shell.md).

- discount_p:

  Propagation probability for `"degree_discount"`. Default 0.01. See
  [`centrality_degree_discount`](https://sonsoles.me/cograph/reference/centrality_degree_discount.md).

- ncvote_theta:

  Weight of the plain vote in `"ncvoterank"`. Default 0.5. See
  [`centrality_ncvoterank`](https://sonsoles.me/cograph/reference/centrality_ncvoterank.md).

- comm_r:

  Scale \\R\\ of `"comm_centrality"`: `"max_intra"` (default) or a
  single positive number.

- ld_radius:

  Radius for `"local_dimension_fixed"`, in hops. A single number of at
  least 1; default 2.

- enrenew_depth:

  Renewal radius for `"enrenew"`. Default 2.

- voterank_lambda:

  Suppression factor for `"voterank_plus"`. Default 0.1.

- contraction_rho:

  \\\alpha / \beta\\ for `"node_contraction_improved"`. Default 5.

- wks_alpha, wks_beta:

  Degree and strength exponents for `"weighted_kshell"`. Default 1 and
  1.

- renewed_threshold:

  Diffusion-importance threshold for `"renewed_coreness"`. Default 2.

- kpath_k:

  Maximum path length for `"geodesic_kpath"`. Default 3.

- kpath_len:

  Maximum path length for `"kpath"`. Default 3; the enumeration is
  exhaustive, so cost grows with the branching factor to this power.

- epc_threshold:

  Edge removal probability for `"epc"`. Default 0.5.

- epc_runs:

  Number of percolation realizations for `"epc"`. Default 1000.

- epc_seed:

  Random seed for `"epc"`. Default `NULL` draws from the session's
  random number stream, so the estimate varies between calls. A seed
  makes the estimate reproducible, and the caller's random number state
  is restored afterwards.

- betweenness_delta:

  Decay exponent for `"delta_betweenness"`. Default 1; 0 gives ordinary
  betweenness.

- closeness_delta:

  Distance exponent for `"delta_closeness"`. Default 1, which is
  `harmonic` over \\n - 1\\.

- gravity_mass:

  Mass in `"gravity"`: `"kshell"` (default, Ma et al. 2016), `"degree"`
  (Li et al. 2019) or `"legacy"`, which uses a unit focal mass and a
  partner mass equal to degree times k-shell index.

- gravity_radius:

  Largest distance each gravity source reaches in `"gravity"`,
  `"extended_gravity"`, `"mixed_gravity"` or `"extended_mixed_gravity"`:
  a number (default 3), `"auto"` for half the mean distance, or `NULL`
  for the whole graph. The automatic radius averages the finite positive
  distances, rounds half to even, and is at least 1.

- mdd_lambda:

  Exhausted-degree weight for `"mdd"`, between 0 and 1. Default 0.7. See
  [`centrality_truss`](https://sonsoles.me/cograph/reference/centrality_truss.md).

- volume_radius:

  Closed neighborhood radius for `"volume"`: a nonnegative integer or
  `Inf`, default 2. Degrees are measured in the full simple undirected
  graph. See
  [`centrality_volume`](https://sonsoles.me/cograph/reference/centrality_volume.md).

- diffusion_q:

  Multiplier between 0 and 1 for `"diffusion_centrality"`, default 1. It
  is separate from `lambda`, which scales `"diffusion"`.

- diffusion_steps:

  Nonnegative integer horizon for `"diffusion_centrality"`, default 3.
  See
  [`centrality_diffusion_centrality`](https://sonsoles.me/cograph/reference/centrality_diffusion_centrality.md)
  for its weighted-walk definition, direction, probability
  interpretation and precision limits.

- ds_beta:

  Spreading rate for `"dynamics_sensitive"`, between zero and one,
  default 0.1.

- ds_mu:

  Recovery rate for `"dynamics_sensitive"`, between zero and one,
  default 1. Zero selects the SI case.

- ds_steps:

  Nonnegative integer horizon for `"dynamics_sensitive"`, default 5. See
  [`centrality_dynamics_sensitive`](https://sonsoles.me/cograph/reference/centrality_dynamics_sensitive.md).

- cda_alpha:

  Degree-versus-strength weight for `"cda"`, between zero and one;
  default 0.5. See
  [`centrality_cda`](https://sonsoles.me/cograph/reference/centrality_cda.md).

- icc_alpha:

  Shortest-path multiplicity exponent for `"improved_closeness"`,
  between zero and one; default 0.2.

- exogenous_base:

  Base for `"exogenous"`: reverse_closeness (default), betweenness or
  degree. See
  [`centrality_exogenous`](https://sonsoles.me/cograph/reference/centrality_exogenous.md).

- wlr_alpha:

  Finite in-degree exponent for `"weighted_leaderrank"`, default one.
  See
  [`centrality_weighted_leaderrank`](https://sonsoles.me/cograph/reference/centrality_weighted_leaderrank.md).

- alr_h_mode:

  H-index convention for `"adaptive_leaderrank"`: all (default), out or
  in. See
  [`centrality_adaptive_leaderrank`](https://sonsoles.me/cograph/reference/centrality_adaptive_leaderrank.md).

- grc_gamma:

  Finite nonnegative regularization strength for
  `"graph_regularization"`, default one. See
  [`centrality_graph_regularization`](https://sonsoles.me/cograph/reference/centrality_graph_regularization.md).

- rwd_decay:

  Finite first-arrival discount in \\\[0,1)\\ for `"random_walk_decay"`,
  default 0.5.

- rwd_node_weights:

  Nonnegative starting weights for `"random_walk_decay"`; NULL means
  ones. See
  [`centrality_random_walk_decay`](https://sonsoles.me/cograph/reference/centrality_random_walk_decay.md).

- linerank_aggregation:

  LineRank endpoint aggregation: probability (default) or weight. See
  [`centrality_linerank`](https://sonsoles.me/cograph/reference/centrality_linerank.md).

- bridging_steps:

  Nonnegative bridging-capital walk horizon, default two.

- bridging_values:

  Optional source-destination value matrix for
  [`centrality_bridging_capital`](https://sonsoles.me/cograph/reference/centrality_bridging_capital.md);
  NULL uses ones.

- proximal_variant:

  Proximal betweenness role: source (default), target, sum, or union.
  See
  [`centrality_proximal_betweenness`](https://sonsoles.me/cograph/reference/centrality_proximal_betweenness.md).

- exf_alpha:

  Modified Expected Force degree factor, default two, finite and greater
  than one.

- beta_direction:

  BG-index orientation, positive (default) or negative. See
  [`centrality_beta_measure`](https://sonsoles.me/cograph/reference/centrality_beta_measure.md).

- ninl_order:

  Nonnegative NINL iteration count, default three.

- ninl_radius:

  NINL hop radius, NULL for ceiling of mean path length. See
  [`centrality_ninl`](https://sonsoles.me/cograph/reference/centrality_ninl.md)
  for disconnected graphs and overrides.

- map_flow:

  Map equation flow model, unrecorded (default) or recorded.

- map_convention:

  Map equation coding convention, paper (default) or infomap. See
  [`centrality_map_equation`](https://sonsoles.me/cograph/reference/centrality_map_equation.md).

- sr_prior:

  SpectralRank diagonal prior, default zero; scalar or one value per
  node. See
  [`centrality_spectralrank`](https://sonsoles.me/cograph/reference/centrality_spectralrank.md).

- mcgm_radius:

  MCGM hop cutoff, default two; NULL includes all reachable nodes.

- mcgm_alpha:

  MCGM coefficient, NULL for the published adaptive rule. See
  [`centrality_mcgm`](https://sonsoles.me/cograph/reference/centrality_mcgm.md)
  for disconnected-graph conventions.

- dkgm_radius:

  DKGM hop cutoff, default two as in the paper's printed example; NULL
  or infinity includes all reachable nodes and "auto" applies the
  paper's half-mean-distance rule with cograph rounding. See
  [`centrality_dkgm`](https://sonsoles.me/cograph/reference/centrality_dkgm.md).

- nd_order:

  Steps of neighbors summed by `"neighbor_distance"`, a nonnegative
  whole number, default two; zero returns `nd_mass`. See
  [`centrality_neighbor_distance`](https://sonsoles.me/cograph/reference/centrality_neighbor_distance.md).

- nd_decay:

  Per-step decay for `"neighbor_distance"`, a finite number, default 0.2
  as in the source.

- nd_mass:

  Benchmark centrality summed by `"neighbor_distance"`: degree (default)
  or coreness.

- ira_mass:

  Node centrality allocated by `"ira"` and `"iira"`: coreness (default,
  the k-shell index both sources use in their worked examples) or
  degree. See
  [`centrality_ira`](https://sonsoles.me/cograph/reference/centrality_ira.md).

- ira_alpha:

  Exponent on the `"ira"` mass, a finite number, default one as in the
  source.

- ira_tol:

  Stopping tolerance for `"ira"` on the largest absolute change between
  iterates, a positive finite number, default `1e-6` as in the source.

- ira_max_iter:

  Iteration bound for `"ira"`, a whole number of at least one,
  default 1000. Reaching it raises a `cograph_no_converge` warning,
  which always happens on a bipartite component with unequal vertex
  classes. See
  [`centrality_ira`](https://sonsoles.me/cograph/reference/centrality_ira.md).

- iira_beta:

  Spreading rate for `"iira"`, a number in \\(0,1\]\\, default 0.2 as in
  the source.

- iira_steps:

  Iterations for `"iira"`, a nonnegative whole number, default 50 as in
  the source; zero returns the initial unit resource. See
  [`centrality_iira`](https://sonsoles.me/cograph/reference/centrality_iira.md).

- hcc_delta:

  Weight on a node's own degree in the extended degree used by `"hcc"`
  and `"ehcc"`, a single number in \\\[0,1\]\\, default 0.5 as in the
  source. A value of one gives the classical degree and zero drops the
  node's own degree. Values outside \\\[0,1\]\\ raise an error. See
  [`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md).

- lhc_radius:

  Radius of the ball \\\Phi(v)\\ summed over by `"lhc"`, the \\d\\ of
  the source's equation (1); a single whole number of at least one,
  default 2 as in the source, which reports 2-3 as optimal. At one the
  ball contains only the neighbors. At or above the diameter the score
  no longer changes. Values below one and non-integers raise an error.
  See
  [`centrality_lhc`](https://sonsoles.me/cograph/reference/centrality_lhc.md).

- tpr_alpha:

  Jump probability of the trust-PageRank iteration used by
  `"trust_pagerank"`, a single number strictly between zero and one,
  default 0.85 as the source sets it below its equation (7). See
  [`centrality_trust_pagerank`](https://sonsoles.me/cograph/reference/centrality_trust_pagerank.md).

- tpr_k:

  Weight of the degree ratio in the trust value of `"trust_pagerank"`,
  the \\k\\ of the source's equation (6). The similarity ratio receives
  weight \\1-k\\. A single number in \\\[0,1\]\\, default 0.85, the
  value selected in section 3.3 of the source. One drops the similarity
  and zero drops the degree.

- tpr_decay:

  Attenuation factor of the similarity recursion used by
  `"trust_pagerank"`, the \\C\\ of the source's equation (4); a single
  number in \\(0,1\]\\, default 1 as in the source. \\C\\ changes the
  result. See
  [`centrality_trust_pagerank`](https://sonsoles.me/cograph/reference/centrality_trust_pagerank.md).

- tpr_tol:

  Convergence tolerance on the largest relative change of either
  trust-PageRank recursion, a single positive number, default `1e-14`.
  The tolerance is relative because the similarities on one graph span
  many orders of magnitude. See
  [`centrality_trust_pagerank`](https://sonsoles.me/cograph/reference/centrality_trust_pagerank.md).

- tpr_max_iter:

  Iteration bound for both trust-PageRank recursions, a whole number of
  at least one, default 1000. Reaching it raises a `cograph_no_converge`
  warning.

- rsp_beta:

  Inverse temperature of the randomized-shortest-paths model used by
  `"rsp_betweenness"`, a single finite number strictly above zero,
  default 0.01, the value `NetworkToolbox::rspbc()` recommends. Small
  values approach the random-walk limit. Values of 1 and above move the
  measure towards shortest paths. See
  [`centrality_rsp_betweenness`](https://sonsoles.me/cograph/reference/centrality_rsp_betweenness.md).

- rsp_cost:

  How an edge weight becomes a traversal cost for `"rsp_betweenness"`:
  `"inverse"` (default) for \\C=1/w\\, reading a weight as an affinity,
  or `"weight"` for \\C=w\\, reading it as a distance. Both settings
  give unit cost per arc on a binary graph. See
  [`centrality_rsp_betweenness`](https://sonsoles.me/cograph/reference/centrality_rsp_betweenness.md).

- re_indexes:

  Constituent indexes integrated by `"relative_entropy"`, default the
  source's four distinctiveness indexes; the vocabulary also holds
  `"n_components"` and `"largest_component"`. See
  [`centrality_relative_entropy`](https://sonsoles.me/cograph/reference/centrality_relative_entropy.md).

- re_negative:

  Which of `re_indexes` are negative indexes, NULL for the source's own
  declarations. See
  [`centrality_relative_entropy`](https://sonsoles.me/cograph/reference/centrality_relative_entropy.md).

- tna_network:

  Logical or NULL. Umbrella switch that forces tna-style conventions
  across all measures. `NULL` (default) is TRUE when `x` is a `tna`,
  `group_tna`, `ctna`, `ftna` or `atna` object (or a group of these) and
  FALSE otherwise. `TRUE` applies the tna conventions to any input,
  setting `invert_weights = TRUE`, `loops = FALSE`,
  `diffusion_method = "power_series"` and
  `transitivity_type = "onnela"`. `FALSE` keeps the cograph defaults for
  tna inputs as well. An argument passed explicitly always takes
  precedence over `tna_network`.

- psych_network:

  Logical or NULL. Switch for signed psychometric network conventions.
  `NULL` (default) auto-detects TRUE when a signed weighted network is
  evaluated with expected-influence measures. When `TRUE`, normalized
  expected influence is divided by the maximum absolute
  expected-influence value, which keeps its sign and bounds it between
  -1 and 1. `FALSE` keeps the generic cograph normalization.

- ...:

  Additional arguments (currently unused)

## Value

A base `data.frame` with one row per node, in the input's node order
unless `sort_by` is given, and the columns:

- `node`: character, the node labels (the index as a string when the
  input carried no names)

- One numeric column per requested measure, with a mode suffix for the
  mode-aware measures (e.g., `degree_in`, `closeness_all`); see
  [`list_centralities`](https://sonsoles.me/cograph/reference/list_centralities.md)
  for which measures carry a suffix. A measure that a tier supplied but
  that has no value on this input is an all-`NA` column.

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

  Maximum distance to other nodes (supports mode). Distances use the raw
  edge weights, whatever `weighted` and `invert_weights` say.

- coreness:

  K-core membership (supports mode: in/out/all)

- constraint:

  Burt's constraint (structural holes)

- transitivity:

  Local clustering coefficient (supports multiple types)

- harmonic:

  Harmonic centrality, the sum of inverse distances. It stays finite on
  disconnected graphs (supports mode: in/out/all)

- diffusion:

  Diffusion degree centrality. With `diffusion_method = "kandhway_kuri"`
  it is the scaled degree of the node plus the scaled degrees of its
  neighbors (supports mode: in/out/all, lambda scaling)

- leverage:

  Leverage centrality. Influence over neighbors based on relative degree
  differences (supports mode: in/out/all)

- kreach:

  K-reach centrality. Number of nodes reachable within `k` steps
  (supports mode: in/out/all)

- alpha:

  Alpha centrality. Influence through paths, attenuated by length, with
  a unit exogenous contribution at every node

- power:

  Bonacich power centrality. Influence based on connections to other
  influential nodes

- subgraph:

  Subgraph centrality. Participation in closed walks, with shorter walks
  weighted more heavily

- laplacian:

  Laplacian centrality with the local formula of Qi et al. (2012).
  Matches NetworkX and
  [`centiserve::laplacian()`](https://rdrr.io/pkg/centiserve/man/laplacian.html)

- load:

  Load centrality. Share of shortest-path load passing through the node,
  with load split evenly at each branching point

- current_flow_closeness:

  Information centrality. Closeness based on electrical current flow
  (requires a connected graph)

- current_flow_betweenness:

  Random-walk betweenness. Betweenness based on electrical current flow
  (requires a connected graph)

- voterank:

  VoteRank. Influential spreaders selected by iterative voting. The
  value is the election order rescaled so that the first elected node
  scores 1 and the last scores \\1/n\\

- percolation:

  Percolation centrality. Shortest-path betweenness weighted by the node
  states in `states`. With equal states it is betweenness divided by
  \\(n-1)(n-2)\\

- radiality:

  Radiality centrality (centiserve). Sum of (diam + 1 - d) normalized by
  n-1.

- lin:

  Lin's centrality. Reachable nodes squared divided by sum of distances.

- decay:

  Decay centrality. Sum of \\\delta^d\\ over all nodes, the node itself
  included, with \\\delta\\ = `decay_parameter`.

- residual_closeness:

  Residual closeness. Sum of \\1/2^d\\ over all nodes, the node itself
  included.

- dangalchev:

  Dangalchev closeness. Same values as `residual_closeness`.

- generalized_closeness:

  Generalized closeness. Same formula as `decay`, using
  `decay_parameter`.

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

- distance_entropy:

  Normalized Shannon entropy of a node's hop-distance profile. It is 1
  when the distances are spread evenly and 0 when all lie at one
  distance.

- local_dimension:

  Growth exponent of the ball around a node (slope of \\\ln B_i(r)\\ on
  \\\ln r\\). Lower values mark more influential nodes.

- local_information_dimension:

  Entropy-weighted local dimension over boxes up to half the node's
  eccentricity. Higher values mark more influential nodes.

- neighborhood_connectivity:

  Mean degree of a node's neighbors (average neighbor degree); isolates
  score 0.

- modularity_vitality:

  Drop in modularity when the node is removed under a fixed partition.
  Positive values mark community hubs and negative values mark bridges
  (requires `membership`).

- shapley_game1, shapley_game2, shapley_game3:

  Shapley value of the node in the coverage games of Michalak et al.
  (2013): one-hop coverage, `shapley_k`-neighbor coverage, and coverage
  within `shapley_cutoff` hops. Values sum to the node count.

- access_information:

  Mean bits needed to reach every other node along shortest paths
  without a map. Low values mark well-connected nodes.

- hide_information:

  Mean bits others need to find the node. High values mark hidden nodes.

- rumor:

  Log rumor centrality on the node's BFS tree: log of the number of
  spreading orders that could start there.

- community_hub_bridge:

  Community size times intra-community degree plus number of other
  communities touched times inter-community degree (requires
  `membership`).

- entropy_variation_degree, entropy_variation_betweenness:

  Drop in the Shannon entropy of the degree (by `mode`) or betweenness
  distribution when the node is deleted; signed, nats.

- s_shell:

  Shell index of the strength-based peeling with asymmetric topological
  link weights, exponent `s_shell_a`.

- degree_discount, single_discount:

  Greedy seed-selection order under degree discounting (`discount_p`) or
  unit discounting, scored 1 for the first selected down to 1/n.

- ncvoterank:

  VoteRank with voters weighted by normalized neighborhood coreness
  (`ncvote_theta`); election order scored like `voterank`.

- community_based, comm_centrality, community_mediator:

  Links weighted by the size of the community they reach; Gupta's scaled
  intra/inter-degree combination (`comm_r`); base-2 entropy of the link
  distribution over communities times degree share (all require
  `membership`).

- local_dimension_fixed, fuzzy_local_dimension, local_volume_dimension:

  Silva-Costa estimator at `ld_radius`; slope of the fuzzy ball, where
  higher values mark more influential nodes; slope of the degree volume,
  where lower values mark more important nodes.

- wvoterank, enrenew, voterank_plus:

  Election orders of the weighted, entropy-based (`enrenew_depth`) and
  degree-weighted (`voterank_lambda`) VoteRank variants, scored like
  `voterank`.

- node_contraction, node_contraction_improved:

  One minus the agglomeration ratio after contracting the node with its
  neighbors; the improved form adds the same score of its edges on the
  line graph (`contraction_rho`).

- two_way_rw:

  Number of node pairs whose most likely two-way random-walk route
  passes through the node.

- heatmap:

  Farness minus mean neighbor farness. Lower values mark more central
  nodes.

- flow_coefficient:

  Share of neighbor pairs linked through the node but not directly.

- local_entropy:

  \\-\sum\_{j \in N(i)} k_j \ln k_j\\. Lower values mark more central
  nodes.

- weighted_h_index:

  h-index over topological link weights \\k_i k_j\\ repeated \\k_j\\
  times.

- redundancy:

  Mean degree of the neighbors inside the ego network; degree minus
  effective size.

- weighted_kshell:

  k-shell on \\(k^\alpha s^\beta)^{1/(\alpha + \beta)}\\ after Garas'
  weight normalization (`wks_alpha`, `wks_beta`).

- renewed_coreness:

  k-core of the graph after removing links whose diffusion importance is
  below `renewed_threshold`.

- geodesic_kpath:

  Number of shortest paths of length at most `kpath_k` starting at the
  node.

- local_efficiency:

  Global efficiency of the subgraph induced on the node's neighbors, the
  node itself removed. This differs from
  [`igraph::local_efficiency()`](https://r.igraph.org/reference/global_efficiency.html),
  which measures the distances between those neighbors through the rest
  of the network.

- s_core:

  Largest strength threshold whose s-core still contains the node; the
  k-core number when weights are absent.

- fragmentation:

  Distance-weighted fragmentation of the network after deleting the
  node. Higher means a more disruptive removal.

- kpath:

  Number of simple paths of length at most `kpath_len` that the node
  lies on, endpoints included.

- epc:

  Edge percolated component: mean size of the node's component over
  `epc_runs` bond-percolation realizations, as a share of the network. A
  Monte Carlo estimate; `epc_seed` makes it reproducible.

- length_scaled_betweenness:

  Betweenness with each separated pair weighted by \\1 / d(s,t)\\.

- delta_betweenness:

  Betweenness with the pair weight \\(d(s,t) - 1)^{-\delta}\\
  (`betweenness_delta`).

- ego_betweenness:

  Betweenness inside the node's own ego network.

- delta_closeness:

  \\\sum_j d\_{ij}^{-\delta} / (n-1)\\ (`closeness_delta`).

- truss, mdd:

  Node truss number (k-2 triangles convention) and mixed-degree shell
  threshold (`mdd_lambda`). Both use the simple undirected skeleton; see
  [`centrality_truss`](https://sonsoles.me/cograph/reference/centrality_truss.md).

- bridging_coefficient, godfather, support:

  Reciprocal-degree ratio, count of unconnected neighbor pairs, and
  count of triangle-supported relationships on the simple undirected
  skeleton.

- volume:

  Sum of degrees in the closed `volume_radius`-hop neighborhood on the
  simple undirected skeleton.

- mcc:

  Maximal clique centrality: sum of \\(\|C\|-1)!\\ over incident maximal
  cliques of size at least two. Costly; see
  [`centrality_mcc`](https://sonsoles.me/cograph/reference/centrality_mcc.md)
  for isolate and precision conventions.

- diffusion_centrality:

  Finite-horizon weighted outgoing walks:
  \\\sum\_{t=1}^{T}(qA)^t\mathbf{1}\\, with `diffusion_q` and
  `diffusion_steps`. Distinct from diffusion degree.

- dynamical_importance:

  Relative spectral-radius loss on vertex deletion, evaluated by
  repeated eigendecomposition. Costly; see
  [`centrality_dynamical_importance`](https://sonsoles.me/cograph/reference/centrality_dynamical_importance.md)
  for zero-radius graphs.

- dynamics_sensitive:

  Finite-time spreading score including `ds_beta`, `ds_mu` and
  `ds_steps`; uses the simple undirected skeleton.

- malatya:

  Sum of focal-to-neighbor degree ratios on the simple undirected
  skeleton; the reciprocal of the bridging coefficient on nonisolated
  vertices.

- resistance_curvature:

  One minus half the incident conductance times effective-resistance
  sum. Weighted, componentwise and costly; see
  [`centrality_resistance_curvature`](https://sonsoles.me/cograph/reference/centrality_resistance_curvature.md).

- extended_coreness:

  Sum of neighbors' neighborhood coreness; equivalently the squared
  simple adjacency times core numbers.

- dkgm:

  Gravity with the degree k-shell index as the mass at both ends,
  default radius two; see
  [`centrality_dkgm`](https://sonsoles.me/cograph/reference/centrality_dkgm.md).

- neighbor_distance:

  Benchmark centrality plus its decayed sums over non-backtracking walks
  of up to `nd_order` steps. See
  [`centrality_neighbor_distance`](https://sonsoles.me/cograph/reference/centrality_neighbor_distance.md).

- ira:

  Steady state of a unit resource repeatedly reallocated to neighbors in
  proportion to their `ira_mass`; conserved, so the scores of a
  component sum to its size. Warns `cograph_no_converge` where no steady
  state exists. See
  [`centrality_ira`](https://sonsoles.me/cograph/reference/centrality_ira.md).

- iira:

  The same recursion with each share scaled by \\1-(1-\beta)^{k_i}\\ for
  the `iira_beta` spreading rate, run `iira_steps` times. Decays
  geometrically, so only the order is meaningful. See
  [`centrality_iira`](https://sonsoles.me/cograph/reference/centrality_iira.md).

- lnc:

  Local neighbor contribution: the cubed degree times the binomial
  own-contribution factor \\(1-1/d_i)^{d_i-1}\\ times the neighbors'
  degree sum over \\n-1\\. Parameter-free; raw scores depend on the
  whole graph's order. See
  [`centrality_lnc`](https://sonsoles.me/cograph/reference/centrality_lnc.md).

- ked:

  KED method: the degree times one plus the normalized entropy of the
  neighbors' degrees times \\\exp(K_i/N)\\ for the neighbor-degree sum
  \\K_i\\ and the whole graph's order \\N\\. Parameter-free. See
  [`centrality_ked`](https://sonsoles.me/cograph/reference/centrality_ked.md).

- hcc:

  Hybrid characteristic centrality: the extended degree \\\delta
  k_i+(1-\delta)\sum\_{j\in N(i)}k_j\\ over its maximum, plus the
  E-shell peeling round in which the node leaves over the number of
  rounds. Raw scores lie in \\\[0,2\]\\ and are not component-local. See
  [`centrality_hcc`](https://sonsoles.me/cograph/reference/centrality_hcc.md).

- ehcc:

  Extended hybrid characteristic centrality: the closed-neighborhood sum
  of `hcc`, the focal node counted once. See
  [`centrality_ehcc`](https://sonsoles.me/cograph/reference/centrality_ehcc.md).

- lhc:

  Lhc index: the degree-and-triangle-share influence
  \\C(v)=\sum\_{u\in\Phi(v)}k_u(1+TP(u))/d^2(uv)\\ over the ball of
  radius `lhc_radius`, summed over the open neighborhood. The triangle
  share is normalized by \\TNTS=\sum_u NTS(u)\\, three times the number
  of distinct triangles, and is written as zero on a triangle-free
  graph. Raw scores are not component-local. See
  [`centrality_lhc`](https://sonsoles.me/cograph/reference/centrality_lhc.md).

- iec:

  Immediate effects centrality: the reciprocal mean length of the
  influence sequences that end at a node, \\(n-1)/\sum\_{i\neq
  j}m\_{ij}\\ for the mean first passage times
  \\M=(I-Z+EZ\_{dg})\mathrm{diag}(1/c)\\ of the influence chain
  \\W=A/\mathrm{rowSums}(A)\\ built with \\a\_{ii}=1\\.
  Direction-sensitive and costly (one eigenproblem and two dense
  solves). `NA` at every node when the chain is reducible or the graph
  has one node. Not the same measure as `markov`. See
  [`centrality_iec`](https://sonsoles.me/cograph/reference/centrality_iec.md).

- dil:

  Degree and importance of lines: the degree plus the share of each
  incident line's importance \\I_e=(k_m-p-1)(k_n-p-1)/ (p/2+1)\\ that
  the node's own degree claims,
  \\k_i+\sum\_{j\in\Gamma_i}I\_{e\_{ij}}(k_i-1)/(k_i+k_j-2)\\, with
  \\p\\ the number of triangles on the line. Two-hop local and
  component-local; never below the node's degree. See
  [`centrality_dil`](https://sonsoles.me/cograph/reference/centrality_dil.md).

- trust_pagerank:

  Trust-PageRank: a damped PageRank whose split of a node's score among
  its neighbors is the column-stochastic trust-value
  \\T(i,j)=(1-k)s(i,j)/\sum\_{l\in N_j}s(j,l)+ k\\d_i/\sum\_{l\in
  N_j}d_l\\, with \\s\\ the fixed point of SimRank restricted to the
  lines of the graph. Scores sum to one when no node is isolated. `NA`
  at every node of a component that has lines but no triangle, where the
  similarity vanishes and the ratio is undefined. Costly (two
  fixed-point recursions over dense matrices). See
  [`centrality_trust_pagerank`](https://sonsoles.me/cograph/reference/centrality_trust_pagerank.md).

- rsp_betweenness:

  Simple randomized shortest paths betweenness: the expected number of
  visits a node receives over the Boltzmann distribution on absorbing
  walks, summed over every ordered source-target pair. `rsp_beta`
  interpolates between the random-walk and shortest-path readings.
  Direction-sensitive, component-local, and costly (one dense inverse).
  See
  [`centrality_rsp_betweenness`](https://sonsoles.me/cograph/reference/centrality_rsp_betweenness.md).

- relative_entropy:

  Normalized geometric mean of several index distributions, the
  minimum-relative-entropy integration of `re_indexes`; sums to one. See
  [`centrality_relative_entropy`](https://sonsoles.me/cograph/reference/centrality_relative_entropy.md).

- mixed_gravity:

  Gravity with focal core-number and partner-degree masses, default
  radius three.

- extended_mixed_gravity:

  Sum of immediate neighbors' raw mixed gravitational centralities.

- extended_gravity:

  Sum of neighbors' raw k-shell gravity scores, with `gravity_radius`
  applied around each neighbor.

- cda:

  Weighted degree and strength, adjusted by Barrat clustering, plus
  weighted neighbor contributions; uses `cda_alpha`.

- improved_closeness:

  Closeness using distances divided by the number of shortest paths
  raised to `icc_alpha`.

- exogenous:

  Contribution to all other nodes' base centrality, measured by
  deletion. Selects a base using `exogenous_base`.

- global_structure:

  Exponential focal coreness times distance-discounted partner coreness
  (GSM).

- hybrid_global_structure:

  Exponential degree-coreness influences with an adaptive distance
  exponent (H-GSM).

- improved_global_structure:

  Exponential focal degree with partner degrees discounted by a global
  mean-degree distance exponent (IGSM).

- weighted_leaderrank:

  Stationary scores with ground-node outgoing weights determined by
  original in-degree and `wlr_alpha`.

- linerank:

  PageRank on the line graph, aggregated at endpoints; uses `damping`
  and `linerank_aggregation`.

- expected_force:

  Entropy of onward boundary degrees over all two-event transmission
  sequences.

- mcgm:

  Multi-characteristics gravity with degree, coreness and eigenvector
  masses; default radius two.

- spectralrank:

  Outgoing Perron eigenvector with a unit-linked ground node; `sr_prior`
  supplies optional diagonal information.

- controlrank:

  Smallest eigenvalue of each grounded symmetric row-Laplacian; see
  [`centrality_controlrank`](https://sonsoles.me/cograph/reference/centrality_controlrank.md).

- map_equation:

  Codelength saving on silencing a node, conditional on the supplied
  partition, flow model and coding convention.

- ninl:

  Finite neighbor propagation of closed-neighborhood degree volume; uses
  `ninl_order` and `ninl_radius`.

- beta_measure:

  BG power shared by successors among predecessors; `beta_direction`
  selects positive or negative orientation.

- localized_bridging, extended_local_bridging:

  Betweenness in one-hop or two-hop ego networks times the original
  bridging coefficient.

- modified_expected_force:

  Expected Force multiplied by log degree with the scaling parameter
  `exf_alpha`.

- proximal_betweenness:

  First/last shortest-path intermediaries; uses `proximal_variant` on
  the directed unweighted skeleton.

- x_degree:

  Counts four-edge nonbacktracking walks with each node at the middle,
  using original neighbor excess degrees.

- coleman_theil:

  Concentration of dyadic Burt constraints across contacts; isolates
  zero and single-contact nodes one.

- bridging_capital:

  Information-walk loss under single-entry deletion; uses
  `bridging_steps` and `bridging_values`.

- random_walk_decay:

  Weighted sum of discounted first arrivals from random walks; uses
  `rwd_decay` and `rwd_node_weights`.

- graph_regularization:

  Reciprocal diagonal of the inverse regularized weighted Laplacian,
  using `grc_gamma`.

- adaptive_leaderrank:

  Stationary scores with destination weights determined by original
  H-indices using `alr_h_mode`.

## Measures without a value on a given input

The community-aware measures warn and return an all-`NA` column when
`membership` is missing, and the brokerage roles do the same on an
undirected graph. `"relative_entropy"` has no value when one of its
constituent indexes is zero at every node. Named in `measures` or
`include`, it then raises a `cograph_undefined_index` error. Supplied by
a tier, it gives a `cograph_undefined_measure` warning and an all-`NA`
column, and the other measures of the tier are still computed.
`"flow_betweenness"` requires the igraph package. Without igraph, naming
it raises a `cograph_needs_igraph` error, and a tier gives the same
warning and all-`NA` column.

## Examples

``` r
centrality(regulation_net)
#>          node degree_all strength_all closeness_all betweenness eigenvector
#> 1     Explore          6         1.39     0.5154639         5.0   0.7209674
#> 2        Plan          7         1.90     0.4484305        15.5   0.2010163
#> 3     Monitor          8         1.87     0.5714286        18.0   1.0000000
#> 4       Adapt          6         1.77     0.3787879        15.0   0.8380103
#> 5     Reflect          6         1.39     0.5208333        10.0   0.7619198
#> 6     Discuss          5         1.53     0.3460208         0.5   0.5077694
#> 7  Synthesize          4         0.77     0.4830918         6.5   0.1980065
#> 8    Evaluate          5         1.71     0.4032258         3.0   0.4995074
#> 9      Create          7         1.64     0.5263158        13.0   0.6689427
#> 10      Share          6         1.95     0.3521127         9.0   0.5849834
#>      pagerank
#> 1  0.11846147
#> 2  0.03640953
#> 3  0.18376724
#> 4  0.12356096
#> 5  0.12513119
#> 6  0.06803638
#> 7  0.03760071
#> 8  0.07386400
#> 9  0.13821279
#> 10 0.09495573
```
