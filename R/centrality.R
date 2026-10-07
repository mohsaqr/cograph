#' Calculate Network Centrality Measures
#'
#' Computes centrality measures for nodes in a network and returns a tidy
#' data frame. Accepts matrices, edge-list data frames, igraph objects,
#' cograph_network, or tna objects.
#'
#' @param x Network input (matrix, edge-list data frame, igraph, network,
#'   cograph_network, tna object).
#' @param type Character scalar selecting a curated tier of measures when
#'   \code{measures} is not supplied. One of:
#'   \describe{
#'     \item{\code{"basic"}}{The default. Six measures: \code{degree},
#'       \code{strength}, \code{closeness}, \code{betweenness},
#'       \code{eigenvector}, \code{pagerank}.}
#'     \item{\code{"extended"}}{The basic tier plus harmonic, coreness,
#'       eccentricity, radiality, lin, decay, load, stress, katz, alpha,
#'       power, authority, leverage, constraint, effective_size, bridging,
#'       transitivity, subgraph, diffusion, laplacian, kreach,
#'       current_flow_betweenness and current_flow_closeness.}
#'     \item{\code{"all"}}{Every measure except the costly ones (see
#'       \code{include} and \code{\link{list_centralities}}).}
#'   }
#'   Passing \code{measures} explicitly overrides \code{type}.
#' @param include Character vector of costly measures to add to a tier, or
#'   \code{"costly"} for all of them. \code{type = "all"} holds back the
#'   measures whose cost grows steeply with network size (the \code{costly}
#'   column of \code{\link{list_centralities}}). A measure named in
#'   \code{measures} is always computed, whatever its cost. Unknown names
#'   raise a \code{cograph_unknown_measure} error. Default \code{NULL}.
#' @param measures Character vector of measure names to compute. When
#'   \code{NULL} (default) the tier selected by \code{type} is used.
#'   \code{"all"} is a shortcut for \code{type = "all"}. Unknown names raise
#'   an error. \code{\link{list_centralities}} returns every valid name
#'   together with its orientation, mode support, partition requirement,
#'   weight use and cost. The measures fall into these groups:
#'   \describe{
#'     \item{Core}{"degree", "strength", "betweenness", "closeness",
#'       "eigenvector", "pagerank", "authority", "hub", "eccentricity",
#'       "coreness", "constraint", "transitivity", "harmonic", "alpha",
#'       "power", "subgraph".}
#'     \item{Flow and spreading}{"diffusion", "leverage", "kreach",
#'       "laplacian", "load", "current_flow_closeness",
#'       "current_flow_betweenness", "voterank", "percolation".}
#'     \item{Distance-based}{"radiality", "lin", "decay",
#'       "residual_closeness", "dangalchev", "generalized_closeness",
#'       "harary", "average_distance", "barycenter", "wiener",
#'       "closeness_vitality".}
#'     \item{Spectral and walk-based}{"communicability",
#'       "communicability_betweenness", "random_walk".}
#'     \item{Path-based}{"stress", "flow_betweenness".}
#'     \item{Local and neighborhood}{"lobby", "entropy", "semilocal",
#'       "clusterrank", "bottleneck", "centroid", "mnc", "dmnc", "lac",
#'       "topological_coefficient", "bridging", "local_bridging",
#'       "effective_size", "diversity", "cross_clique", "markov".}
#'     \item{Influence}{"integration", "expected", "gilschmidt".}
#'     \item{Directed only}{"salsa", "leaderrank", "trophic_level",
#'       "pairwisedis", "prestige_domain", "prestige_domain_proximity".}
#'     \item{Community-aware (require \code{membership})}{"participation",
#'       "within_module_z", "gateway", "modularity_vitality" (Magelinski et
#'       al. 2021), "community_hub_bridge" (Ghalmane et al. 2019),
#'       "community_based" (Zhao et al. 2015), "comm_centrality" (Gupta et
#'       al. 2016), "community_mediator" (Tulu et al. 2018), and the
#'       Gould-Fernandez roles "brokerage_coordinator",
#'       "brokerage_itinerant", "brokerage_representative",
#'       "brokerage_gatekeeper", "brokerage_liaison", which also require a
#'       directed graph. See \code{\link{centrality_modularity_vitality}}
#'       and \code{\link{centrality_brokerage_coordinator}}.}
#'     \item{Spreader identification}{"gravity", "collective_influence",
#'       "local_hindex", "hindex_strength", "onion", "second_order",
#'       "infection", "nonbacktracking", "spanning_tree". See
#'       \code{\link{centrality_gravity}}.}
#'     \item{Classical}{"katz" (Katz 1953), "hubbell" (Hubbell 1965),
#'       "information" (Stephenson and Zelen 1989), "reaching_local" (Mones
#'       et al. 2012). See \code{\link{centrality_katz}},
#'       \code{\link{centrality_hubbell}},
#'       \code{\link{centrality_information}},
#'       \code{\link{centrality_pairwisedis}} and
#'       \code{\link{centrality_reaching_local}}.}
#'     \item{Psychometric}{"expected_influence_1", "expected_influence_2"
#'       (Robinaugh, Millner and McNally 2016). Expected influence keeps the
#'       sign of each edge, which matters in networks with negative edges
#'       such as partial-correlation and glasso networks.}
#'     \item{Scaling and dimension}{"distance_entropy" (Stella and De
#'       Domenico 2018), "local_dimension" (Pu et al. 2014),
#'       "local_information_dimension" (Wen and Deng 2020),
#'       "neighborhood_connectivity" (Maslov and Sneppen 2002),
#'       "local_dimension_fixed" (Silva and Costa 2013),
#'       "fuzzy_local_dimension" (Wen and Jiang 2019),
#'       "local_volume_dimension" (Li and Deng 2021). See
#'       \code{\link{centrality_distance_entropy}},
#'       \code{\link{centrality_local_dimension}},
#'       \code{\link{centrality_local_information_dimension}},
#'       \code{\link{centrality_neighborhood_connectivity}} and
#'       \code{\link{centrality_local_dimension_fixed}}.}
#'     \item{Games, information and seed selection}{"shapley_game1",
#'       "shapley_game2", "shapley_game3" (Michalak et al. 2013),
#'       "access_information", "hide_information" (Rosvall et al. 2005),
#'       "rumor" (Shah and Zaman 2011), "entropy_variation_degree",
#'       "entropy_variation_betweenness" (Ai 2017), "s_shell" (Liu et al.
#'       2017), "degree_discount", "single_discount" (Chen, Wang and Yang
#'       2009). See \code{\link{centrality_shapley_game1}},
#'       \code{\link{centrality_access_information}},
#'       \code{\link{centrality_rumor}},
#'       \code{\link{centrality_community_hub_bridge}},
#'       \code{\link{centrality_entropy_variation}},
#'       \code{\link{centrality_s_shell}},
#'       \code{\link{centrality_degree_discount}} and
#'       \code{\link{centrality_community_based}}.}
#'     \item{VoteRank family}{"ncvoterank" (Kumar and Panda 2020),
#'       "wvoterank" (Sun et al. 2019), "enrenew" (Guo et al. 2020),
#'       "voterank_plus" (Liu et al. 2021). See
#'       \code{\link{centrality_ncvoterank}} and
#'       \code{\link{centrality_wvoterank}}.}
#'     \item{Contraction, walks and local structure}{"node_contraction",
#'       "node_contraction_improved" (Tan et al. 2006; Wang et al. 2011),
#'       "two_way_rw" (Curado et al. 2022), "heatmap" (Duron 2020),
#'       "flow_coefficient" (Honey et al. 2007), "local_entropy" (Nie et
#'       al. 2016), "weighted_h_index" (Gao et al. 2019), "redundancy"
#'       (Burt 1992), "weighted_kshell" (Garas et al. 2012),
#'       "renewed_coreness" (Liu et al. 2015), "geodesic_kpath" (Borgatti
#'       and Everett 2006). See \code{\link{centrality_node_contraction}},
#'       \code{\link{centrality_two_way_rw}},
#'       \code{\link{centrality_heatmap}} and
#'       \code{\link{centrality_weighted_kshell}}.}
#'     \item{Efficiency, cores and percolation}{"local_efficiency" (Latora
#'       and Marchiori 2001), "s_core" (Eidsaa and Almaas 2013),
#'       "fragmentation" (Borgatti 2006), "kpath" (Sade 1989), "epc" (Lin
#'       et al. 2008). See \code{\link{centrality_local_efficiency}}.}
#'     \item{Betweenness and closeness variants}{"length_scaled_betweenness"
#'       (Brandes 2008), "delta_betweenness" and "delta_closeness"
#'       (Agneessens et al. 2017), "ego_betweenness" (Everett and Borgatti
#'       2005). Bounded-distance (k-) betweenness is betweenness with
#'       \code{cutoff = k}. See
#'       \code{\link{centrality_length_scaled_betweenness}}.}
#'   }
#'   The remaining measures are described under Details.
#' @param mode For directed networks: "all" (default), "in", or "out".
#'   Affects the mode-aware measures, whose output columns carry a mode
#'   suffix. These include degree, strength, closeness, eccentricity,
#'   coreness, harmonic, diffusion, leverage, kreach, the distance-based
#'   measures, most community-aware measures, and expected influence (the
#'   \code{mode_aware} column of \code{\link{list_centralities}}).
#' @param normalized Logical. If \code{TRUE}, each measure is divided by its
#'   maximum, which scales non-negative measures to 0-1. A measure whose
#'   maximum is not positive is left unchanged. Closeness follows igraph's
#'   normalization instead, multiplying each value by the number of nodes
#'   the node reaches, and harmonic centrality is divided by \eqn{n - 1}
#'   only. Under \code{psych_network = TRUE} expected influence
#'   is divided by its maximum absolute value and keeps its sign. Default
#'   \code{FALSE}.
#' @param weighted Logical. Use edge weights if available. Default TRUE.
#'   With \code{FALSE} every measure works on the binary adjacency, every
#'   edge with weight one.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param loops Logical. If TRUE (default), keep self-loops. Set to FALSE to
#'   remove them before calculation. \code{tna_network = TRUE} changes the
#'   default to FALSE.
#' @param simplify How to combine multiple edges between the same node pair
#'   (possible only from edge-list, cograph_network or igraph input).
#'   Options: "sum" (default), "mean", "max", "min". \code{FALSE} and
#'   \code{"none"} also sum them, because the network is held as a dense
#'   weight matrix, which cannot carry parallel edges.
#' @param digits Integer or NULL. Round all numeric columns to this many
#'   decimal places. Default NULL (no rounding).
#' @param sort_by Character or NULL. Column name to sort results by
#'   (descending order). Default NULL (original node order). A name that is
#'   not a column of the result raises an error.
#' @param cutoff Maximum path length to consider for betweenness, closeness,
#'   harmonic centrality and the distance-based closeness variants (radiality,
#'   lin, decay, residual_closeness, dangalchev, generalized_closeness,
#'   harary, average_distance, barycenter, wiener, centroid,
#'   closeness_vitality, delta_closeness).
#'   Default -1 (no limit). A positive value ignores longer paths, which
#'   shortens computation on large networks and changes the values.
#' @param invert_weights Logical or NULL. Whether path- and distance-based
#'   measures (for example betweenness, closeness, harmonic, k-reach,
#'   radiality, decay, stress, flow betweenness and related variants) invert
#'   the weights, so that higher weights mean shorter paths. A message reports
#'   the inversion. Eccentricity always reads the raw edge weights as
#'   distances. The default \code{NULL} is TRUE for tna objects (transition
#'   probabilities) and FALSE otherwise, as in igraph and sna. TRUE suits
#'   strength or frequency weights (the qgraph convention) and FALSE suits
#'   distance or cost weights.
#' @param alpha Numeric. Exponent for weight transformation when \code{invert_weights = TRUE}.
#'   Distance is computed as \code{1 / weight^alpha}. Default 1. Higher values
#'   increase the influence of weight differences on path lengths.
#' @param damping PageRank damping factor. Default 0.85. Must be between 0 and 1.
#' @param personalized Non-negative numeric vector of reset probabilities
#'   for personalized PageRank, one value per node. A named vector is
#'   matched to the node names, an unnamed one is taken in node order. The
#'   vector is rescaled to sum to 1. Default
#'   NULL (standard PageRank).
#' @param transitivity_type Type of transitivity to calculate: "local" (default),
#'   "global", "undirected", "localundirected", "barrat" (weighted),
#'   "weighted", or "onnela". The first six follow the conventions of
#'   \code{igraph::transitivity()}. \code{"global"} and \code{"undirected"}
#'   give one graph-level value, repeated on every row. \code{"onnela"}
#'   computes the weighted clustering coefficient of Zhang and Horvath
#'   (2005) on the symmetrized matrix \code{x + t(x)}, the form tna uses
#'   under that name, and matches
#'   \code{tna::centralities(x, "Clustering")}. \code{tna_network = TRUE}
#'   changes the default to \code{"onnela"}.
#' @param isolates Value of local transitivity at nodes where it is
#'   undefined: "nan" (default) returns NaN, "zero" returns 0.
#' @param lambda Diffusion scaling factor for diffusion centrality. Default 1.
#'   Only used when \code{diffusion_method = "kandhway_kuri"}.
#' @param diffusion_method Character or NULL. Selects the diffusion-centrality
#'   formula. \code{"kandhway_kuri"} (Kandhway & Kuri, 2014) computes the
#'   1-hop binary-degree neighborhood sum
#'   \eqn{\lambda d_v + \lambda \sum_{u \in N(v)} d_u}{lambda d_v + lambda sum_{u in N(v)} d_u}. \code{"power_series"}
#'   computes the matrix power series \eqn{\mathrm{rowSums}(P + P^2 + \ldots + P^n)}{rowSums(P + P^2 + ... + P^n)}
#'   on the (optionally diagonal-zeroed) weighted matrix and matches
#'   \code{tna::centralities(., measures = "Diffusion")} when
#'   \code{loops = FALSE}. Default NULL auto-detects: \code{"power_series"}
#'   for tna objects (transition probabilities), \code{"kandhway_kuri"}
#'   otherwise.
#' @param k Distance bound for \code{"kreach"}, the number of nodes reachable
#'   within \code{k} steps. Default 3.
#' @param states Numeric vector of percolation states for percolation
#'   centrality, one per node. Each value represents how "activated" or
#'   "infected" a node is. A named vector is matched to the node labels,
#'   missing nodes get state 1, and values are clipped to 0-1. Default NULL
#'   gives every node state 1, which makes percolation equal to betweenness
#'   divided by \eqn{(n-1)(n-2)}.
#' @param decay_parameter Numeric. Decay parameter for decay and generalized
#'   closeness centrality, strictly between 0 and 1; smaller values discount
#'   distant nodes more. Other values raise a \code{cograph_bad_parameter}
#'   error when one of these measures is requested. Default 0.5.
#' @param dmnc_epsilon Numeric. Epsilon exponent for DMNC (Density of Maximum
#'   Neighborhood Component), a single positive number. Default 1.7, as
#'   recommended by Lin et al. (2008). centiserve uses 1.67. Other values
#'   raise a \code{cograph_bad_parameter} error when \code{"dmnc"} is
#'   requested.
#' @param membership Integer vector of community assignments (one per node) for
#'   the community-aware measures listed under \code{measures}. Default NULL.
#'   Without it those measures return \code{NA} with a warning of classes
#'   \code{cograph_bad_membership} and \code{cograph_undefined_measure}.
#'   \code{"map_equation"} uses it when supplied.
#' @param katz_alpha Attenuation factor for Katz centrality. The Katz series
#'   converges only for \eqn{\alpha < 1 / \rho(A)}{alpha < 1 / rho(A)}. Otherwise the measure
#'   raises a \code{cograph_katz_diverged} warning. Default 0.1, the
#'   centiserve and NetworkX convention. Only used when \code{"katz"} is in
#'   \code{measures}.
#' @param shapley_k Neighbor threshold \eqn{k} for \code{"shapley_game2"}.
#'   Default 2. See \code{\link{centrality_shapley_game2}}.
#' @param shapley_cutoff Hop cutoff for \code{"shapley_game3"}. Default 2.
#'   See \code{\link{centrality_shapley_game3}}.
#' @param s_shell_a Exponent of the asymmetric link weights for
#'   \code{"s_shell"}. A single non-negative number; default 0.5. See
#'   \code{\link{centrality_s_shell}}.
#' @param discount_p Propagation probability for \code{"degree_discount"}.
#'   Default 0.01. See \code{\link{centrality_degree_discount}}.
#' @param ncvote_theta Weight of the plain vote in \code{"ncvoterank"}.
#'   Default 0.5. See \code{\link{centrality_ncvoterank}}.
#' @param comm_r Scale \eqn{R} of \code{"comm_centrality"}:
#'   \code{"max_intra"} (default) or a single positive number.
#' @param ld_radius Radius for \code{"local_dimension_fixed"}, in hops. A
#'   single number of at least 1; default 2.
#' @param enrenew_depth Renewal radius for \code{"enrenew"}. Default 2.
#' @param voterank_lambda Suppression factor for \code{"voterank_plus"}.
#'   Default 0.1.
#' @param contraction_rho \eqn{\alpha / \beta}{alpha / beta} for
#'   \code{"node_contraction_improved"}. Default 5.
#' @param wks_alpha,wks_beta Degree and strength exponents for
#'   \code{"weighted_kshell"}. Default 1 and 1.
#' @param renewed_threshold Diffusion-importance threshold for
#'   \code{"renewed_coreness"}. Default 2.
#' @param kpath_k Maximum path length for \code{"geodesic_kpath"}. Default 3.
#' @param kpath_len Maximum path length for \code{"kpath"}. Default 3; the
#'   enumeration is exhaustive, so cost grows with the branching factor to
#'   this power.
#' @param epc_threshold Edge removal probability for \code{"epc"}.
#'   Default 0.5.
#' @param epc_runs Number of percolation realizations for \code{"epc"}.
#'   Default 1000.
#' @param epc_seed Random seed for \code{"epc"}. Default \code{NULL} draws
#'   from the session's random number stream, so the estimate varies between
#'   calls. A seed makes the estimate reproducible, and the caller's random
#'   number state is restored afterwards.
#' @param betweenness_delta Decay exponent for \code{"delta_betweenness"}.
#'   Default 1; 0 gives ordinary betweenness.
#' @param closeness_delta Distance exponent for \code{"delta_closeness"}.
#'   Default 1, which is \code{harmonic} over \eqn{n - 1}.
#' @param gravity_mass Mass in \code{"gravity"}: \code{"kshell"} (default,
#'   Ma et al. 2016), \code{"degree"} (Li et al. 2019) or \code{"legacy"},
#'   which uses a unit focal mass and a partner mass equal to degree times
#'   k-shell index.
#' @param gravity_radius Largest distance each gravity source reaches in
#'   \code{"gravity"}, \code{"extended_gravity"},
#'   \code{"mixed_gravity"} or \code{"extended_mixed_gravity"}: a
#'   number (default 3), \code{"auto"} for half the mean distance, or
#'   \code{NULL} for the whole graph. The automatic radius averages the
#'   finite positive distances, rounds half to even, and is at least 1.
#' @param mdd_lambda Exhausted-degree weight for \code{"mdd"}, between
#'   0 and 1. Default 0.7. See \code{\link{centrality_truss}}.
#' @param volume_radius Closed neighborhood radius for \code{"volume"}:
#'   a nonnegative integer or \code{Inf}, default 2. Degrees are measured
#'   in the full simple undirected graph. See \code{\link{centrality_volume}}.
#' @param diffusion_q Multiplier between 0 and 1 for \code{"diffusion_centrality"},
#'   default 1. It is separate from \code{lambda}, which scales
#'   \code{"diffusion"}.
#' @param diffusion_steps Nonnegative integer horizon for
#'   \code{"diffusion_centrality"}, default 3. See
#'   \code{\link{centrality_diffusion_centrality}} for its weighted-walk
#'   definition, direction, probability interpretation and precision limits.
#' @param ds_beta Spreading rate for \code{"dynamics_sensitive"}, between
#'   zero and one, default 0.1.
#' @param ds_mu Recovery rate for \code{"dynamics_sensitive"}, between zero
#'   and one, default 1. Zero selects the SI case.
#' @param ds_steps Nonnegative integer horizon for \code{"dynamics_sensitive"},
#'   default 5. See \code{\link{centrality_dynamics_sensitive}}.
#' @param cda_alpha Degree-versus-strength weight for \code{"cda"},
#'   between zero and one; default 0.5. See \code{\link{centrality_cda}}.
#' @param icc_alpha Shortest-path multiplicity exponent for
#'   \code{"improved_closeness"}, between zero and one; default 0.2.
#' @param exogenous_base Base for \code{"exogenous"}: reverse_closeness
#'   (default), betweenness or degree. See \code{\link{centrality_exogenous}}.
#' @param wlr_alpha Finite in-degree exponent for \code{"weighted_leaderrank"},
#'   default one. See \code{\link{centrality_weighted_leaderrank}}.
#' @param linerank_aggregation LineRank endpoint aggregation: probability
#'   (default) or weight. See \code{\link{centrality_linerank}}.
#' @param exf_alpha Modified Expected Force degree factor, default two,
#'   finite and greater than one.
#' @param proximal_variant Proximal betweenness role: source (default),
#'   target, sum, or union. See \code{\link{centrality_proximal_betweenness}}.
#' @param map_flow Map equation flow model, unrecorded (default) or recorded.
#' @param mcgm_radius MCGM hop cutoff, default two; NULL includes all reachable nodes.
#' @param mcgm_alpha MCGM coefficient, NULL for the published adaptive rule.
#'   See \code{\link{centrality_mcgm}} for disconnected-graph conventions.
#' @param dkgm_radius DKGM hop cutoff, default two as in the paper's printed
#'   example; NULL or infinity includes all reachable nodes and "auto"
#'   applies the paper's half-mean-distance rule with cograph rounding.
#'   See \code{\link{centrality_dkgm}}.
#' @param re_indexes Constituent indexes integrated by
#'   \code{"relative_entropy"}, default the source's four distinctiveness
#'   indexes; the vocabulary also holds \code{"n_components"} and
#'   \code{"largest_component"}. See \code{\link{centrality_relative_entropy}}.
#' @param re_negative Which of \code{re_indexes} are negative indexes, NULL
#'   for the source's own declarations. See
#'   \code{\link{centrality_relative_entropy}}.
#' @param nd_order Steps of neighbors summed by \code{"neighbor_distance"},
#'   a nonnegative whole number, default two; zero returns \code{nd_mass}.
#'   See \code{\link{centrality_neighbor_distance}}.
#' @param nd_decay Per-step decay for \code{"neighbor_distance"}, a finite
#'   number, default 0.2 as in the source.
#' @param nd_mass Benchmark centrality summed by
#'   \code{"neighbor_distance"}: degree (default) or coreness.
#' @param ira_mass Node centrality allocated by \code{"ira"} and
#'   \code{"iira"}: coreness (default, the k-shell index both sources use in
#'   their worked examples) or degree. See \code{\link{centrality_ira}}.
#' @param ira_alpha Exponent on the \code{"ira"} mass, a finite number,
#'   default one as in the source.
#' @param ira_tol Stopping tolerance for \code{"ira"} on the largest
#'   absolute change between iterates, a positive finite number, default
#'   \code{1e-6} as in the source.
#' @param ira_max_iter Iteration bound for \code{"ira"}, a whole number of
#'   at least one, default 1000. Reaching it raises a
#'   \code{cograph_no_converge} warning, which always happens on a bipartite
#'   component with unequal vertex classes. See \code{\link{centrality_ira}}.
#' @param iira_beta Spreading rate for \code{"iira"}, a number in
#'   \eqn{(0,1]}, default 0.2 as in the source.
#' @param iira_steps Iterations for \code{"iira"}, a nonnegative whole
#'   number, default 50 as in the source; zero returns the initial unit
#'   resource. See \code{\link{centrality_iira}}.
#' @param hcc_delta Weight on a node's own degree in the extended degree
#'   used by \code{"hcc"} and \code{"ehcc"}, a single number in
#'   \eqn{[0,1]}, default 0.5 as in the source. A value of one gives the
#'   classical degree and zero drops the node's own degree. Values outside
#'   \eqn{[0,1]} raise an error. See \code{\link{centrality_hcc}}.
#' @param lhc_radius Radius of the ball \eqn{\Phi(v)}{Phi(v)} summed over by
#'   \code{"lhc"}, the \eqn{d} of the source's equation (1); a single whole
#'   number of at least one, default 2 as in the source, which reports 2-3
#'   as optimal. At one the ball contains only the neighbors. At or above the
#'   diameter the score no longer changes. Values below one and non-integers
#'   raise an error. See \code{\link{centrality_lhc}}.
#' @param tpr_alpha Jump probability of the trust-PageRank iteration used
#'   by \code{"trust_pagerank"}, a single number strictly between zero and
#'   one, default 0.85 as the source sets it below its equation (7). See
#'   \code{\link{centrality_trust_pagerank}}.
#' @param tpr_k Weight of the degree ratio in the trust value of
#'   \code{"trust_pagerank"}, the \eqn{k} of the source's equation (6). The
#'   similarity ratio receives weight \eqn{1-k}. A single number in
#'   \eqn{[0,1]}, default 0.85, the value selected in section 3.3 of the
#'   source. One drops the similarity and zero drops the degree.
#' @param tpr_decay Attenuation factor of the similarity recursion used by
#'   \code{"trust_pagerank"}, the \eqn{C} of the source's equation (4); a
#'   single number in \eqn{(0,1]}, default 1 as in the source. \eqn{C}
#'   changes the result. See \code{\link{centrality_trust_pagerank}}.
#' @param tpr_tol Convergence tolerance on the largest relative change of
#'   either trust-PageRank recursion, a single positive number, default
#'   \code{1e-14}. The tolerance is relative because the similarities on one
#'   graph span many orders of magnitude. See
#'   \code{\link{centrality_trust_pagerank}}.
#' @param tpr_max_iter Iteration bound for both trust-PageRank recursions, a
#'   whole number of at least one, default 1000. Reaching it raises a
#'   \code{cograph_no_converge} warning.
#' @param rsp_beta Inverse temperature of the randomized-shortest-paths
#'   model used by \code{"rsp_betweenness"}, a single finite number strictly
#'   above zero, default 0.01, the value \code{NetworkToolbox::rspbc()}
#'   recommends. Small values approach the random-walk limit. Values of 1
#'   and above move the measure towards shortest paths. See
#'   \code{\link{centrality_rsp_betweenness}}.
#' @param rsp_cost How an edge weight becomes a traversal cost for
#'   \code{"rsp_betweenness"}: \code{"inverse"} (default) for \eqn{C=1/w},
#'   reading a weight as an affinity, or \code{"weight"} for \eqn{C=w},
#'   reading it as a distance. Both settings give unit cost per arc on a
#'   binary graph. See
#'   \code{\link{centrality_rsp_betweenness}}.
#' @param sr_prior SpectralRank diagonal prior, default zero; scalar or one
#'   value per node. See \code{\link{centrality_spectralrank}}.
#' @param map_convention Map equation coding convention, paper (default) or
#'   infomap. See \code{\link{centrality_map_equation}}.
#' @param ninl_order Nonnegative NINL iteration count, default three.
#' @param ninl_radius NINL hop radius, NULL for ceiling of mean path length.
#'   See \code{\link{centrality_ninl}} for disconnected graphs and overrides.
#' @param beta_direction BG-index orientation, positive (default) or negative.
#'   See \code{\link{centrality_beta_measure}}.
#' @param bridging_steps Nonnegative bridging-capital walk horizon, default two.
#' @param bridging_values Optional source-destination value matrix for
#'   \code{\link{centrality_bridging_capital}}; NULL uses ones.
#' @param rwd_decay Finite first-arrival discount in \eqn{[0,1)} for
#'   \code{"random_walk_decay"}, default 0.5.
#' @param rwd_node_weights Nonnegative starting weights for
#'   \code{"random_walk_decay"}; NULL means ones. See
#'   \code{\link{centrality_random_walk_decay}}.
#' @param grc_gamma Finite nonnegative regularization strength for
#'   \code{"graph_regularization"}, default one. See
#'   \code{\link{centrality_graph_regularization}}.
#' @param alr_h_mode H-index convention for \code{"adaptive_leaderrank"}:
#'   all (default), out or in. See \code{\link{centrality_adaptive_leaderrank}}.
#' @param tna_network Logical or NULL. Umbrella switch that forces tna-style
#'   conventions across all measures. \code{NULL} (default) is TRUE when
#'   \code{x} is a \code{tna}, \code{group_tna}, \code{ctna}, \code{ftna}
#'   or \code{atna} object (or a group of these) and FALSE otherwise.
#'   \code{TRUE} applies the tna conventions to any input, setting
#'   \code{invert_weights = TRUE}, \code{loops = FALSE},
#'   \code{diffusion_method = "power_series"} and
#'   \code{transitivity_type = "onnela"}. \code{FALSE} keeps the cograph
#'   defaults for tna inputs as well. An argument passed explicitly always
#'   takes precedence over \code{tna_network}.
#' @param psych_network Logical or NULL. Switch for signed psychometric
#'   network conventions. \code{NULL} (default) auto-detects TRUE when a
#'   signed weighted network is evaluated with expected-influence measures.
#'   When \code{TRUE}, normalized expected influence is divided by the maximum
#'   absolute expected-influence value, which keeps its sign and bounds it
#'   between -1 and 1. \code{FALSE} keeps the generic cograph normalization.
#' @param hubbell_weight Weight factor \eqn{w} for Hubbell centrality. Must be
#'   positive and satisfy \eqn{w \cdot \rho(W) < 1}{w * rho(W) < 1} for solvability; otherwise
#'   the measure warns and returns \code{NA}. Default 0.5. Only used when
#'   \code{"hubbell"} is in \code{measures}.
#' @param ... Additional arguments (currently unused)
#'
#' @return A base \code{data.frame} with one row per node, in the input's node
#'   order unless \code{sort_by} is given, and the columns:
#'   \itemize{
#'     \item \code{node}: character, the node labels (the index as a string
#'       when the input carried no names)
#'     \item One numeric column per requested measure, with a mode suffix for
#'       the mode-aware measures (e.g., \code{degree_in},
#'       \code{closeness_all}); see \code{\link{list_centralities}} for which
#'       measures carry a suffix. A measure that a tier supplied but that has
#'       no value on this input is an all-\code{NA} column.
#'   }
#'
#' @details
#' The following centrality measures are available:
#' \describe{
#'   \item{degree}{Count of edges (supports mode: in/out/all)}
#'   \item{strength}{Weighted degree (supports mode: in/out/all)}
#'   \item{betweenness}{Shortest path centrality}
#'   \item{closeness}{Inverse distance centrality (supports mode: in/out/all)}
#'   \item{eigenvector}{Influence-based centrality}
#'   \item{pagerank}{Random walk centrality (supports damping and personalization)}
#'   \item{authority}{HITS authority score}
#'   \item{hub}{HITS hub score}
#'   \item{eccentricity}{Maximum distance to other nodes (supports mode).
#'     Distances use the raw edge weights whatever \code{invert_weights}
#'     says, and hop counts with \code{weighted = FALSE}.}
#'   \item{coreness}{K-core membership (supports mode: in/out/all)}
#'   \item{constraint}{Burt's constraint (structural holes)}
#'   \item{transitivity}{Local clustering coefficient (supports multiple types)}
#'   \item{harmonic}{Harmonic centrality, the sum of inverse distances. It
#'     stays finite on disconnected graphs (supports mode: in/out/all)}
#'   \item{diffusion}{Diffusion degree centrality. With
#'     \code{diffusion_method = "kandhway_kuri"} it is the scaled degree of
#'     the node plus the scaled degrees of its neighbors (supports mode:
#'     in/out/all, lambda scaling)}
#'   \item{leverage}{Leverage centrality. Influence over neighbors based on
#'     relative degree differences (supports mode: in/out/all)}
#'   \item{kreach}{K-reach centrality. Number of nodes reachable within
#'     \code{k} steps (supports mode: in/out/all)}
#'   \item{alpha}{Alpha centrality. Influence through paths, attenuated by
#'     length, with a unit exogenous contribution at every node (supports mode:
#'     in/out/all)}
#'   \item{power}{Bonacich power centrality. Influence based on connections
#'     to other influential nodes (supports mode: in/out/all)}
#'   \item{subgraph}{Subgraph centrality. Participation in closed walks,
#'     with shorter walks weighted more heavily}
#'   \item{laplacian}{Laplacian centrality with the local formula of Qi et
#'     al. (2012). Matches NetworkX and \code{centiserve::laplacian()}}
#'   \item{load}{Load centrality. Share of shortest-path load passing
#'     through the node, with load split evenly at each branching point}
#'   \item{current_flow_closeness}{Information centrality. Closeness based on
#'     electrical current flow (requires a connected graph)}
#'   \item{current_flow_betweenness}{Random-walk betweenness. Betweenness
#'     based on electrical current flow (requires a connected graph)}
#'   \item{voterank}{VoteRank. Influential spreaders selected by iterative
#'     voting. The value is the election order rescaled so that the first
#'     elected node scores 1 and the last scores \eqn{1/n}}
#'   \item{percolation}{Percolation centrality. Shortest-path betweenness
#'     weighted by the node states in \code{states}. With equal states it is
#'     betweenness divided by \eqn{(n-1)(n-2)}}
#'   \item{radiality}{Radiality centrality (centiserve). Sum of (diam + 1 - d)
#'     normalized by n-1.}
#'   \item{lin}{Lin's centrality. Reachable nodes squared divided by sum of
#'     distances.}
#'   \item{decay}{Decay centrality. Sum of \eqn{\delta^d}{delta^d} over all nodes,
#'     the node itself included, with \eqn{\delta}{delta} = \code{decay_parameter}.}
#'   \item{residual_closeness}{Residual closeness. Sum of \eqn{1/2^d} over
#'     all nodes, the node itself included.}
#'   \item{dangalchev}{Dangalchev closeness. Same values as
#'     \code{residual_closeness}.}
#'   \item{generalized_closeness}{Generalized closeness. Same formula as
#'     \code{decay}, using \code{decay_parameter}.}
#'   \item{harary}{Harary centrality. Sum of 1/d^2 for all reachable pairs.}
#'   \item{average_distance}{Average distance (centiserve). Sum of distances /
#'     (n+1).}
#'   \item{barycenter}{Barycenter centrality. 1 / sum of distances.}
#'   \item{wiener}{Wiener index. Total sum of shortest path distances from node.}
#'   \item{closeness_vitality}{Closeness vitality. Drop in Wiener index when
#'     node removed.}
#'   \item{communicability}{Total communicability. Row sums of matrix exponential.}
#'   \item{communicability_betweenness}{Communicability betweenness. Fraction of
#'     communicability through each node.}
#'   \item{random_walk}{Random walk centrality. Inverse sum of random walk
#'     distances (requires connected graph).}
#'   \item{stress}{Stress centrality. Number of shortest paths through node.}
#'   \item{flow_betweenness}{Flow betweenness. Max-flow based betweenness.}
#'   \item{lobby}{Lobby index (h-index of neighborhood).}
#'   \item{entropy}{Graph entropy centrality. Entropy change on node removal.}
#'   \item{semilocal}{Semi-local centrality. Triple-nested neighborhood sum.}
#'   \item{clusterrank}{ClusterRank. Clustering coefficient times neighbor
#'     degree sum.}
#'   \item{bottleneck}{Bottleneck centrality. Count of shortest path trees where
#'     node is critical.}
#'   \item{centroid}{Centroid value. Minimum f(v,i) across all nodes.}
#'   \item{mnc}{Maximum Neighborhood Component size.}
#'   \item{dmnc}{Density of Maximum Neighborhood Component.}
#'   \item{topological_coefficient}{Topological coefficient. Shared neighbor
#'     ratio.}
#'   \item{bridging}{Bridging centrality. Betweenness times bridging
#'     coefficient.}
#'   \item{local_bridging}{Local bridging. (1/degree) times bridging
#'     coefficient.}
#'   \item{effective_size}{Burt's effective size. Degree minus redundancy.}
#'   \item{diversity}{Diversity centrality. Shannon entropy of edge weight
#'     distribution.}
#'   \item{cross_clique}{Cross-clique connectivity. Count of cliques containing
#'     node.}
#'   \item{markov}{Markov centrality. Inverse mean first passage time
#'     (requires connected graph).}
#'   \item{integration}{Integration centrality. Distance-based influence.}
#'   \item{expected}{Expected centrality. Sum of neighbor degrees.}
#'   \item{gilschmidt}{Gil-Schmidt power index. Sum of 1/d normalized by n-1.}
#'   \item{salsa}{SALSA authority scores (directed graphs only).}
#'   \item{leaderrank}{LeaderRank. PageRank with ground node
#'     (directed graphs only).}
#'   \item{participation}{Participation coefficient. Diversity of inter-community
#'     connections (requires \code{membership}).}
#'   \item{within_module_z}{Within-module degree z-score. Intra-community
#'     connectivity (requires \code{membership}).}
#'   \item{gateway}{Gateway coefficient. Inter-community brokerage weighted by
#'     centrality (requires \code{membership}).}
#'   \item{distance_entropy}{Normalized Shannon entropy of a node's
#'     hop-distance profile. It is 1 when the distances are spread evenly
#'     and 0 when all lie at one distance.}
#'   \item{local_dimension}{Growth exponent of the ball around a node
#'     (slope of \eqn{\ln B_i(r)}{ln B_i(r)} on \eqn{\ln r}{ln r}). Lower values mark more
#'     influential nodes.}
#'   \item{local_information_dimension}{Entropy-weighted local dimension
#'     over boxes up to half the node's eccentricity. Higher values mark
#'     more influential nodes.}
#'   \item{neighborhood_connectivity}{Mean degree of a node's neighbors
#'     (average neighbor degree); isolates score 0.}
#'   \item{modularity_vitality}{Drop in modularity when the node is removed
#'     under a fixed partition. Positive values mark community hubs and
#'     negative values mark bridges (requires \code{membership}).}
#'   \item{shapley_game1, shapley_game2, shapley_game3}{Shapley value of the
#'     node in the coverage games of Michalak et al. (2013): one-hop
#'     coverage, \code{shapley_k}-neighbor coverage, and coverage within
#'     \code{shapley_cutoff} hops. Values sum to the node count.}
#'   \item{access_information}{Mean bits needed to reach every other node
#'     along shortest paths without a map. Low values mark well-connected
#'     nodes.}
#'   \item{hide_information}{Mean bits others need to find the node. High
#'     values mark hidden nodes.}
#'   \item{rumor}{Log rumor centrality on the node's BFS tree: log of the
#'     number of spreading orders that could start there.}
#'   \item{community_hub_bridge}{Community size times intra-community
#'     degree plus number of other communities touched times
#'     inter-community degree (requires \code{membership}).}
#'   \item{entropy_variation_degree, entropy_variation_betweenness}{Drop in
#'     the Shannon entropy of the degree (by \code{mode}) or betweenness
#'     distribution when the node is deleted; signed, nats.}
#'   \item{s_shell}{Shell index of the strength-based peeling with
#'     asymmetric topological link weights, exponent \code{s_shell_a}.}
#'   \item{degree_discount, single_discount}{Greedy seed-selection order
#'     under degree discounting (\code{discount_p}) or unit discounting,
#'     scored 1 for the first selected down to 1/n.}
#'   \item{ncvoterank}{VoteRank with voters weighted by normalized
#'     neighborhood coreness (\code{ncvote_theta}); election order scored
#'     like \code{voterank}.}
#'   \item{community_based, comm_centrality, community_mediator}{Links
#'     weighted by the size of the community they reach; Gupta's scaled
#'     intra/inter-degree combination (\code{comm_r}); base-2 entropy of the
#'     link distribution over communities times degree share (all require
#'     \code{membership}).}
#'   \item{local_dimension_fixed, fuzzy_local_dimension,
#'     local_volume_dimension}{Silva-Costa estimator at \code{ld_radius};
#'     slope of the fuzzy ball, where higher values mark more influential
#'     nodes; slope of the degree volume, where lower values mark more
#'     important nodes.}
#'   \item{wvoterank, enrenew, voterank_plus}{Election orders of the
#'     weighted, entropy-based (\code{enrenew_depth}) and degree-weighted
#'     (\code{voterank_lambda}) VoteRank variants, scored like
#'     \code{voterank}.}
#'   \item{node_contraction, node_contraction_improved}{One minus the
#'     agglomeration ratio after contracting the node with its neighbors;
#'     the improved form adds the same score of its edges on the line graph
#'     (\code{contraction_rho}).}
#'   \item{two_way_rw}{Number of node pairs whose most likely two-way
#'     random-walk route passes through the node.}
#'   \item{heatmap}{Farness minus mean neighbor farness. Lower values mark
#'     more central nodes.}
#'   \item{flow_coefficient}{Share of neighbor pairs linked through the
#'     node but not directly.}
#'   \item{local_entropy}{\eqn{-\sum_{j \in N(i)} k_j \ln k_j}{-sum_{j in N(i)} k_j ln k_j}. Lower values
#'     mark more central nodes.}
#'   \item{weighted_h_index}{h-index over topological link weights
#'     \eqn{k_i k_j} repeated \eqn{k_j} times.}
#'   \item{redundancy}{Mean degree of the neighbors inside the ego
#'     network; degree minus effective size.}
#'   \item{weighted_kshell}{k-shell on \eqn{(k^\alpha s^\beta)^{1/(\alpha
#'     + \beta)}}{(k^alpha s^beta)^{1/(alpha + beta)}} after Garas' weight normalization (\code{wks_alpha},
#'     \code{wks_beta}).}
#'   \item{renewed_coreness}{k-core of the graph after removing links whose
#'     diffusion importance is below \code{renewed_threshold}.}
#'   \item{geodesic_kpath}{Number of shortest paths of length at most
#'     \code{kpath_k} starting at the node.}
#'   \item{local_efficiency}{Global efficiency of the subgraph induced on
#'     the node's neighbors, the node itself removed. This differs from
#'     \code{igraph::local_efficiency()}, which measures the distances
#'     between those neighbors through the rest of the network.}
#'   \item{s_core}{Largest strength threshold whose s-core still contains
#'     the node; the k-core number when weights are absent.}
#'   \item{fragmentation}{Distance-weighted fragmentation of the network
#'     after deleting the node. Higher means a more disruptive removal.}
#'   \item{kpath}{Number of simple paths of length at most
#'     \code{kpath_len} that the node lies on, endpoints included.}
#'   \item{epc}{Edge percolated component: mean size of the node's
#'     component over \code{epc_runs} bond-percolation realizations, as a
#'     share of the network. A Monte Carlo estimate; \code{epc_seed} makes
#'     it reproducible.}
#'   \item{length_scaled_betweenness}{Betweenness with each separated pair
#'     weighted by \eqn{1 / d(s,t)}.}
#'   \item{delta_betweenness}{Betweenness with the pair weight
#'     \eqn{(h(s,t) - 1)^{-\delta}}{(h(s,t) - 1)^{-delta}}
#'     (\code{betweenness_delta}), where \eqn{h(s,t)} is the number of edges
#'     on a shortest path.}
#'   \item{ego_betweenness}{Betweenness inside the node's own ego network.}
#'   \item{delta_closeness}{\eqn{\sum_j d_{ij}^{-\delta} / (n-1)}{sum_j d_{ij}^{-delta} / (n-1)}
#'     (\code{closeness_delta}).}
#'   \item{truss, mdd}{Node truss number (k-2 triangles convention) and
#'     mixed-degree shell threshold (\code{mdd_lambda}). Both use the
#'     simple undirected skeleton; see \code{\link{centrality_truss}}.}
#'   \item{bridging_coefficient, godfather, support}{Reciprocal-degree
#'     ratio, count of unconnected neighbor pairs, and count of
#'     triangle-supported relationships on the simple undirected skeleton.}
#'   \item{volume}{Sum of degrees in the closed \code{volume_radius}-hop
#'     neighborhood on the simple undirected skeleton.}
#'   \item{mcc}{Maximal clique centrality: sum of \eqn{(|C|-1)!} over
#'     incident maximal cliques of size at least two. Costly; see
#'     \code{\link{centrality_mcc}} for isolate and precision conventions.}
#'   \item{diffusion_centrality}{Finite-horizon weighted outgoing walks:
#'     \eqn{\sum_{t=1}^{T}(qA)^t\mathbf{1}}{sum_{t=1}^{T}(qA)^t 1}, with \code{diffusion_q} and
#'     \code{diffusion_steps}. Distinct from diffusion degree.}
#'   \item{dynamical_importance}{Relative spectral-radius loss on vertex
#'     deletion, evaluated by repeated eigendecomposition. Costly; see
#'     \code{\link{centrality_dynamical_importance}} for zero-radius graphs.}
#'   \item{dynamics_sensitive}{Finite-time spreading score including
#'     \code{ds_beta}, \code{ds_mu} and \code{ds_steps}; uses the simple
#'     undirected skeleton.}
#'   \item{malatya}{Sum of focal-to-neighbor degree ratios on the simple
#'     undirected skeleton; the reciprocal of the bridging coefficient
#'     on nonisolated vertices.}
#'   \item{resistance_curvature}{One minus half the incident conductance
#'     times effective-resistance sum. Weighted, componentwise and costly;
#'     see \code{\link{centrality_resistance_curvature}}.}
#'   \item{extended_coreness}{Sum of neighbors' neighborhood coreness;
#'     equivalently the squared simple adjacency times core numbers.}
#'   \item{dkgm}{Gravity with the degree k-shell index as the mass at both
#'     ends, default radius two; see \code{\link{centrality_dkgm}}.}
#'   \item{neighbor_distance}{Benchmark centrality plus its decayed sums
#'     over non-backtracking walks of up to \code{nd_order} steps. See
#'     \code{\link{centrality_neighbor_distance}}.}
#'   \item{ira}{Steady state of a unit resource repeatedly reallocated to
#'     neighbors in proportion to their \code{ira_mass}; conserved, so the
#'     scores of a component sum to its size. Warns
#'     \code{cograph_no_converge} where no steady state exists. See
#'     \code{\link{centrality_ira}}.}
#'   \item{iira}{The same recursion with each share scaled by
#'     \eqn{1-(1-\beta)^{k_i}}{1-(1-beta)^{k_i}} for the \code{iira_beta} spreading rate,
#'     run \code{iira_steps} times.
#'     Decays geometrically, so only the order is meaningful. See
#'     \code{\link{centrality_iira}}.}
#'   \item{lnc}{Local neighbor contribution: the cubed degree times the
#'     binomial own-contribution factor \eqn{(1-1/d_i)^{d_i-1}} times the
#'     neighbors' degree sum over \eqn{n-1}. Parameter-free; raw scores
#'     depend on the whole graph's order. See \code{\link{centrality_lnc}}.}
#'   \item{ked}{KED method: the degree times one plus the normalized
#'     entropy of the neighbors' degrees times \eqn{\exp(K_i/N)}{exp(K_i/N)} for the
#'     neighbor-degree sum \eqn{K_i} and the whole graph's order
#'     \eqn{N}. Parameter-free. See \code{\link{centrality_ked}}.}
#'   \item{hcc}{Hybrid characteristic centrality: the extended degree
#'     \eqn{\delta k_i+(1-\delta)\sum_{j\in N(i)}k_j}{delta k_i+(1-delta)sum_{j in N(i)}k_j} over its maximum,
#'     plus the E-shell peeling round in which the node leaves over the
#'     number of rounds. Raw scores lie in \eqn{[0,2]} and are not
#'     component-local. See \code{\link{centrality_hcc}}.}
#'   \item{ehcc}{Extended hybrid characteristic centrality: the
#'     closed-neighborhood sum of \code{hcc}, the focal node counted once.
#'     See \code{\link{centrality_ehcc}}.}
#'   \item{lhc}{Lhc index: the degree-and-triangle-share influence
#'     \eqn{C(v)=\sum_{u\in\Phi(v)}k_u(1+TP(u))/d^2(uv)}{C(v)=sum_{u in Phi(v)}k_u(1+TP(u))/d^2(uv)} over the ball of
#'     radius \code{lhc_radius}, summed over the open neighborhood. The
#'     triangle share is normalized by \eqn{TNTS=\sum_u NTS(u)}{TNTS=sum_u NTS(u)}, three
#'     times the number of distinct triangles, and is written as zero on a
#'     triangle-free graph. Raw scores are not component-local. See
#'     \code{\link{centrality_lhc}}.}
#'   \item{iec}{Immediate effects centrality: the reciprocal mean length
#'     of the influence sequences that end at a node,
#'     \eqn{(n-1)/\sum_{i\neq j}m_{ij}}{(n-1)/sum_{i!= j}m_{ij}} for the mean first passage times
#'     \eqn{M=(I-Z+EZ_{dg})\mathrm{diag}(1/c)}{M=(I-Z+EZ_{dg})diag(1/c)} of the influence chain
#'     \eqn{W=A/\mathrm{rowSums}(A)}{W=A/rowSums(A)} built with \eqn{a_{ii}=1}.
#'     Direction-sensitive and costly (one eigenproblem and two dense
#'     solves). \code{NA} at every node when the chain is reducible or the
#'     graph has one node. Not the same measure as \code{markov}. See
#'     \code{\link{centrality_iec}}.}
#'   \item{dil}{Degree and importance of lines: the degree plus the share
#'     of each incident line's importance \eqn{I_e=(k_m-p-1)(k_n-p-1)/
#'     (p/2+1)} that the node's own degree claims,
#'     \eqn{k_i+\sum_{j\in\Gamma_i}I_{e_{ij}}(k_i-1)/(k_i+k_j-2)}{k_i+sum_{j in Gamma_i}I_{e_{ij}}(k_i-1)/(k_i+k_j-2)}, with
#'     \eqn{p} the number of triangles on the line. Two-hop local and
#'     component-local; never below the node's degree. See
#'     \code{\link{centrality_dil}}.}
#'   \item{trust_pagerank}{Trust-PageRank: a damped PageRank whose split of
#'     a node's score among its neighbors is the column-stochastic
#'     trust-value \eqn{T(i,j)=(1-k)s(i,j)/\sum_{l\in N_j}s(j,l)+
#'     k\,d_i/\sum_{l\in N_j}d_l}{T(i,j)=(1-k)s(i,j)/sum_{l in N_j}s(j,l)+ k d_i/sum_{l in N_j}d_l}, with \eqn{s} the fixed point of SimRank
#'     restricted to the lines of the graph. Scores sum to one when no node
#'     is isolated. \code{NA} at every node of a component that has lines
#'     but no triangle, where the similarity vanishes and the ratio is
#'     undefined. Costly
#'     (two fixed-point recursions over dense matrices). See
#'     \code{\link{centrality_trust_pagerank}}.}
#'   \item{rsp_betweenness}{Simple randomized shortest paths betweenness:
#'     the expected number of visits a node receives over the Boltzmann
#'     distribution on absorbing walks, summed over every ordered
#'     source-target pair. \code{rsp_beta} interpolates between the
#'     random-walk and shortest-path readings. Direction-sensitive,
#'     component-local, and costly (one dense inverse). See
#'     \code{\link{centrality_rsp_betweenness}}.}
#'   \item{relative_entropy}{Normalized geometric mean of several index
#'     distributions, the minimum-relative-entropy integration of
#'     \code{re_indexes}; sums to one. See
#'     \code{\link{centrality_relative_entropy}}.}
#'   \item{mixed_gravity}{Gravity with focal core-number and partner-degree
#'     masses, default radius three.}
#'   \item{extended_mixed_gravity}{Sum of immediate neighbors' raw
#'     mixed gravitational centralities.}
#'   \item{extended_gravity}{Sum of neighbors' raw k-shell gravity scores,
#'     with \code{gravity_radius} applied around each neighbor.}
#'   \item{cda}{Weighted degree and strength, adjusted by Barrat clustering,
#'     plus weighted neighbor contributions; uses \code{cda_alpha}.}
#'   \item{improved_closeness}{Closeness using distances divided by the
#'     number of shortest paths raised to \code{icc_alpha}.}
#'   \item{exogenous}{Contribution to all other nodes' base centrality,
#'     measured by deletion. Selects a base using \code{exogenous_base}.}
#'   \item{global_structure}{Exponential focal coreness times
#'     distance-discounted partner coreness (GSM).}
#'   \item{hybrid_global_structure}{Exponential degree-coreness influences
#'     with an adaptive distance exponent (H-GSM).}
#'   \item{improved_global_structure}{Exponential focal degree with partner
#'     degrees discounted by a global mean-degree distance exponent (IGSM).}
#'   \item{weighted_leaderrank}{Stationary scores with ground-node outgoing
#'     weights determined by original in-degree and \code{wlr_alpha}.}
#'   \item{linerank}{PageRank on the line graph, aggregated at endpoints;
#'     uses \code{damping} and \code{linerank_aggregation}.}
#'   \item{expected_force}{Entropy of onward boundary degrees over
#'     all two-event transmission sequences.}
#'   \item{mcgm}{Multi-characteristics gravity with degree, coreness and
#'     eigenvector masses; default radius two.}
#'   \item{spectralrank}{Outgoing Perron eigenvector with a unit-linked
#'     ground node; \code{sr_prior} supplies optional diagonal information.}
#'   \item{controlrank}{Smallest eigenvalue of each grounded symmetric
#'     row-Laplacian; see \code{\link{centrality_controlrank}}.}
#'   \item{map_equation}{Codelength saving on silencing a node, conditional
#'     on the supplied partition, flow model and coding convention.}
#'   \item{ninl}{Finite neighbor propagation of closed-neighborhood degree
#'     volume; uses \code{ninl_order} and \code{ninl_radius}.}
#'   \item{beta_measure}{BG power shared by successors among predecessors;
#'     \code{beta_direction} selects positive or negative orientation.}
#'   \item{localized_bridging, extended_local_bridging}{Betweenness in
#'     one-hop or two-hop ego networks times the original bridging coefficient.}
#'   \item{modified_expected_force}{Expected Force multiplied by
#'     log degree with the scaling parameter \code{exf_alpha}.}
#'   \item{proximal_betweenness}{First/last shortest-path intermediaries;
#'     uses \code{proximal_variant} on the directed unweighted skeleton.}
#'   \item{x_degree}{Counts four-edge nonbacktracking walks with each node
#'     at the middle, using original neighbor excess degrees.}
#'   \item{coleman_theil}{Concentration of dyadic Burt constraints across
#'     contacts; isolates zero and single-contact nodes one.}
#'   \item{bridging_capital}{Information-walk loss under single-entry deletion;
#'     uses \code{bridging_steps} and \code{bridging_values}.}
#'   \item{random_walk_decay}{Weighted sum of discounted first arrivals
#'     from random walks; uses \code{rwd_decay} and \code{rwd_node_weights}.}
#'   \item{graph_regularization}{Reciprocal diagonal of the inverse
#'     regularized weighted Laplacian, using \code{grc_gamma}.}
#'   \item{adaptive_leaderrank}{Stationary scores with destination weights
#'     determined by original H-indices using \code{alr_h_mode}.}
#' }
#'
#' @section Measures without a value on a given input: The community-aware
#'   measures warn and return an all-\code{NA} column when
#'   \code{membership} is missing, and the brokerage roles do the same on an
#'   undirected graph. \code{"relative_entropy"} has no value when one of
#'   its constituent indexes is zero at every node. Named in
#'   \code{measures} or \code{include}, it then raises a
#'   \code{cograph_undefined_index} error. Supplied by a tier, it gives a
#'   \code{cograph_undefined_measure} warning and an all-\code{NA} column,
#'   and the other measures of the tier are still computed.
#'   \code{"flow_betweenness"} requires the igraph package. Without igraph,
#'   naming it raises a \code{cograph_needs_igraph} error, and a tier gives
#'   the same warning and all-\code{NA} column.
#'
#' @export
#' @examples
#' centrality(regulation_net)
centrality <- function(x, type = c("basic", "extended", "all"),
                       measures = NULL, include = NULL, mode = "all",
                       normalized = FALSE, weighted = TRUE,
                       directed = NULL, loops = TRUE, simplify = "sum",
                       digits = NULL, sort_by = NULL,
                       cutoff = -1, invert_weights = NULL, alpha = 1,
                       damping = 0.85, personalized = NULL,
                       transitivity_type = "local", isolates = "nan",
                       lambda = 1, diffusion_method = NULL,
                       k = 3, states = NULL,
                       decay_parameter = 0.5, dmnc_epsilon = 1.7,
                       membership = NULL,
                       katz_alpha = 0.1, hubbell_weight = 0.5,
                       shapley_k = 2, shapley_cutoff = 2,
                       s_shell_a = 0.5, discount_p = 0.01, ncvote_theta = 0.5,
                       comm_r = "max_intra", ld_radius = 2, enrenew_depth = 2,
                       voterank_lambda = 0.1, contraction_rho = 5,
                       wks_alpha = 1, wks_beta = 1, renewed_threshold = 2,
                       kpath_k = 3,
                       kpath_len = 3, epc_threshold = 0.5,
                       epc_runs = 1000, epc_seed = NULL,
                       betweenness_delta = 1, closeness_delta = 1,
                       gravity_mass = "kshell", gravity_radius = 3,
                       mdd_lambda = 0.7, volume_radius = 2,
                       diffusion_q = 1, diffusion_steps = 3,
                       ds_beta = 0.1, ds_mu = 1, ds_steps = 5, cda_alpha = 0.5,
                       icc_alpha = 0.2, exogenous_base = "reverse_closeness",
                       wlr_alpha = 1, alr_h_mode = "all", grc_gamma = 1,
                       rwd_decay = 0.5, rwd_node_weights = NULL,
                       linerank_aggregation = "probability",
                       bridging_steps = 2, bridging_values = NULL,
                       proximal_variant = "source", exf_alpha = 2,
                       beta_direction = "positive",
                       ninl_order = 3, ninl_radius = NULL,
                       map_flow = "unrecorded", map_convention = "paper",
                       sr_prior = 0, mcgm_radius = 2, mcgm_alpha = NULL,
                       dkgm_radius = 2,
                       nd_order = 2, nd_decay = 0.2, nd_mass = "degree",
                       ira_mass = "coreness", ira_alpha = 1, ira_tol = 1e-6,
                       ira_max_iter = 1000, iira_beta = 0.2, iira_steps = 50,
                       hcc_delta = 0.5, lhc_radius = 2,
                       tpr_alpha = 0.85, tpr_k = 0.85, tpr_decay = 1,
                       tpr_tol = 1e-14, tpr_max_iter = 1000,
                       rsp_beta = 0.01, rsp_cost = c("inverse", "weight"),
                       re_indexes = c("degree", "closeness", "betweenness",
                                      "constraint"),
                       re_negative = NULL,
                       tna_network = NULL,
                       psych_network = NULL,
                       ...) {

  type <- match.arg(type)

  # Detect tna-class input. Group/conditional/factorial tna objects all carry
  # transition probabilities so the same conventions apply.
  is_tna_input <- inherits(x, c("tna", "group_tna", "ctna", "ftna", "atna",
                                 "group_ctna", "group_ftna", "group_atna"))

  # Resolve tna_network. NULL auto-detects from class so existing tna users get
  # tna conventions for free; TRUE/FALSE force the umbrella on/off regardless
  # of class. Precedence: user-explicit per-arg > tna_network > cograph default.
  if (is.null(tna_network)) {
    tna_network <- is_tna_input
  }
  if (!isTRUE(tna_network) && !isFALSE(tna_network)) {
    .cg_stop_bad_parameter("`tna_network` must be NULL, TRUE or FALSE")
  }
  if (!is.null(psych_network) && !isTRUE(psych_network) && !isFALSE(psych_network)) {
    .cg_stop_bad_parameter("`psych_network` must be NULL, TRUE or FALSE")
  }

  # Capture which args the caller explicitly passed so tna_network only fills
  # in the gaps. NULL-default args are also "unset" if the caller passed NULL.
  .explicit <- names(match.call())[-1L]

  # invert_weights: NULL default, auto under tna_network.
  if (is.null(invert_weights)) {
    invert_weights <- isTRUE(tna_network)
  }

  # loops: hard default TRUE in cograph; under tna_network flip to FALSE only
  # if the user did not explicitly pass it.
  if (isTRUE(tna_network) && !"loops" %in% .explicit) {
    loops <- FALSE
  }

  # diffusion_method: NULL default, auto under tna_network.
  if (is.null(diffusion_method)) {
    diffusion_method <- if (isTRUE(tna_network)) "power_series" else "kandhway_kuri"
  }
  diffusion_method <- match.arg(diffusion_method,
                                c("kandhway_kuri", "power_series"))

  # transitivity_type: hard default "local"; under tna_network switch to
  # "onnela" only if the user did not explicitly pass it.
  if (isTRUE(tna_network) && !"transitivity_type" %in% .explicit) {
    transitivity_type <- "onnela"
  }

  # Validate mode
  mode <- match.arg(mode, c("all", "in", "out"))

  # Validate transitivity_type and isolates
  transitivity_type <- match.arg(
    transitivity_type,
    c("local", "global", "undirected", "localundirected",
      "barrat", "weighted", "onnela")
  )
  isolates <- match.arg(isolates, c("nan", "zero"))

  if (!is.numeric(damping) || length(damping) != 1L ||
        !is.finite(damping) || damping < 0 || damping > 1) {
    .cg_stop_bad_parameter("damping must be between 0 and 1")
  }

  # Native graph context (R/kernels-graph.R): dense weights, canonical edge
  # order, loops and duplicate edges resolved once, no igraph needed.
  cg <- .cg_graph(x, directed = directed, loops = loops, simplify = simplify)
  # `weighted = FALSE` means the binary adjacency for every measure. The
  # context itself is made binary, so no kernel can fall back to the stored
  # weights (`weights %||% cg$weights`, `cg$w`) when it is handed NULL.
  if (!isTRUE(weighted)) cg <- .cg_unweighted(cg)

  # Define which measures support mode parameter
  mode_measures <- .cg_mode_measures()
  no_mode_measures <- .cg_no_mode_measures()
  all_measures <- c(mode_measures, no_mode_measures)

  # Curated tiers. basic = canonical measures every paper reports;
  # extended = basic plus the commonly-reported second tier; all = everything.
  basic_measures <- c("degree", "strength", "closeness", "betweenness",
                      "eigenvector", "pagerank")
  extended_measures <- c(basic_measures,
                         "harmonic", "coreness", "eccentricity",
                         "radiality", "lin", "decay",
                         "load", "stress",
                         "katz", "alpha", "power", "authority", "leverage",
                         "constraint", "effective_size", "bridging",
                         "transitivity", "subgraph",
                         "diffusion", "laplacian", "kreach",
                         "current_flow_betweenness", "current_flow_closeness")

  # Measures whose cost grows steeply with network size. They are held back
  # from `type = "all"` so that one call cannot take minutes by accident;
  # `include = ` puts them back, and naming one in `measures = ` always
  # computes it. See .cg_costly_measures() and list_centralities().
  costly <- .cg_costly_measures()

  # Resolve measures: explicit `measures =` wins; otherwise use the tier.
  # `tier_measures` records which ones the caller did not name, so that a
  # measure with no value on this input can be reported as NA there instead
  # of taking the whole tier down. See .cg_tier_guard().
  if (is.null(measures)) {
    measures <- switch(type,
                       basic = basic_measures,
                       extended = extended_measures,
                       all = setdiff(all_measures, costly))
    tier_measures <- measures
  } else if (identical(measures, "all")) {
    measures <- setdiff(all_measures, costly)
    tier_measures <- measures
  } else {
    tier_measures <- character()
    invalid <- setdiff(measures, all_measures)
    if (length(invalid) > 0) {
      stop(errorCondition(
        paste0("Unknown measures: ", paste(invalid, collapse = ", "),
               "\nAvailable: ", paste(all_measures, collapse = ", ")),
        class = "cograph_unknown_measure", call = NULL))
    }
  }

  # `include = ` adds costly measures back to a tier. "costly" adds them all.
  if (!is.null(include)) {
    include <- if (identical(include, "costly")) costly else include
    unknown <- setdiff(include, all_measures)
    if (length(unknown) > 0) {
      stop(errorCondition(
        sprintf("Unknown measures in `include`: %s",
                paste(unknown, collapse = ", ")),
        class = "cograph_unknown_measure", call = NULL))
    }
    measures <- union(measures, include)
  }

  # Decay centrality (Jackson 2008) is defined for 0 < delta < 1: at 1 an
  # unreachable node counts fully (1^Inf is 1 in R), above 1 its term is Inf.
  if (any(c("decay", "generalized_closeness") %in% measures) &&
        !(is.numeric(decay_parameter) && length(decay_parameter) == 1L &&
            is.finite(decay_parameter) && decay_parameter > 0 &&
            decay_parameter < 1)) {
    .cg_stop_bad_parameter("`decay_parameter` must be a single number ",
                           "strictly between 0 and 1")
  }
  if ("dmnc" %in% measures &&
        !(is.numeric(dmnc_epsilon) && length(dmnc_epsilon) == 1L &&
            is.finite(dmnc_epsilon) && dmnc_epsilon > 0)) {
    .cg_stop_bad_parameter("`dmnc_epsilon` must be a single positive ",
                           "finite number")
  }

  # Get node labels
  labels <- cg$labels

  # Calculate each measure
  results <- list(node = labels)
  weights <- if (weighted && !is.null(cg$weights)) cg$weights else NULL
  psychometric_measures <- c("expected_influence_1", "expected_influence_2")
  if (is.null(psych_network)) {
    psych_network <- any(measures %in% psychometric_measures) &&
      !is.null(weights) &&
      any(weights < 0, na.rm = TRUE)
  }

  # Path-based measures need inverted weights (higher weight = shorter path)
  # Following qgraph's approach: distance = 1 / weight^alpha
  path_based_measures <- c("betweenness", "closeness", "harmonic",
                           "eccentricity", "kreach", "load",
                           "radiality", "lin", "decay", "residual_closeness",
                           "dangalchev", "generalized_closeness", "harary",
                           "average_distance", "barycenter", "wiener",
                           "closeness_vitality", "centroid", "stress",
                           "flow_betweenness", "integration", "gilschmidt",
                           "markov", "local_efficiency", "fragmentation",
                           "length_scaled_betweenness", "delta_betweenness",
                           "delta_closeness", "percolation")
  needs_path_weights <- any(measures %in% path_based_measures)

  weights_for_paths <- weights
  if (!is.null(weights) && invert_weights && needs_path_weights) {
    # Invert weights: distance = 1 / weight^alpha (qgraph/tna style)
    weights_for_paths <- 1 / (weights ^ alpha)
    # Handle zeros/infinities
    weights_for_paths[!is.finite(weights_for_paths)] <- .Machine$double.xmax
    reason <- if (is_tna_input) "tna object detected" else "invert_weights=TRUE"
    message("Note: Weights inverted (1/w^", alpha, ") for path-based measures (",
            reason, "). Higher weights = shorter paths.")
  }

  # Pre-calculate HITS scores if needed (avoid computing twice)
  hits_result <- NULL
  if (any(c("authority", "hub") %in% measures)) {
    hits_result <- .cg_hits(.cg_attr_matrix(cg, weights), cg$n)
  }

  # Pre-compute the shared shortest-path matrix once when any distance-based
  # measure is requested. At n=1000 this saves ~580 ms per measure; a
  # full type="extended" call on 11 distance-based measures drops by ~6 s.
  distance_based_measures <- c("radiality", "lin", "decay",
                               "residual_closeness", "dangalchev",
                               "generalized_closeness", "harary",
                               "average_distance", "barycenter", "wiener",
                               "centroid", "closeness_vitality",
                               "delta_closeness")
  shared_dist_mat <- NULL
  if (any(measures %in% distance_based_measures)) {
    # Dependency-free all-pairs shortest paths (see R/kernels-distance.R).
    # The kernel takes a weight matrix, so the graph's edges and the
    # path weights actually in force are assembled into one first.
    shared_dist_mat <- .cg_distances(
      .cg_path_matrix(cg, weights_for_paths), mode, cutoff)
  }

  # Batch 7 scaling measures are defined on hop counts, not path weights,
  # so they share one unweighted all-pairs matrix regardless of `weighted`.
  hop_distance_measures <- c("distance_entropy", "local_dimension",
                             "local_information_dimension",
                             "local_dimension_fixed", "fuzzy_local_dimension",
                             "local_volume_dimension", "heatmap",
                             "geodesic_kpath")
  shared_hop_mat <- NULL
  if (any(measures %in% hop_distance_measures)) {
    shared_hop_mat <- .cg_hop_distances(cg, mode)
  }

  # Batch 8 no-mode measures walk along out-edges whatever `mode` says, so
  # they share one out-direction hop matrix of their own.
  out_hop_measures <- c("shapley_game3", "access_information",
                        "hide_information")
  shared_out_hop_mat <- NULL
  if (any(measures %in% out_hop_measures)) {
    shared_out_hop_mat <- .cg_hop_distances(cg, "out")
  }

  for (m in measures) {
    # Use inverted weights for path-based measures, original for others
    measure_weights <- if (m %in% path_based_measures) weights_for_paths else weights
    # Thread shared distance matrix only for measures that use it
    this_dist_mat <- if (m %in% distance_based_measures) shared_dist_mat else NULL
    this_hop_mat <- if (m %in% hop_distance_measures) shared_hop_mat else NULL
    this_out_hop_mat <- if (m %in% out_hop_measures) shared_out_hop_mat else NULL

    # Calculate value
    compute <- function() calculate_measure(
      cg, m, mode, measure_weights, normalized,
      cutoff = cutoff, damping = damping, personalized = personalized,
      transitivity_type = transitivity_type, isolates = isolates,
      hits_result = hits_result, lambda = lambda,
      diffusion_method = diffusion_method, loops = loops,
      k = k, states = states,
      decay_parameter = decay_parameter, dmnc_epsilon = dmnc_epsilon,
      membership = membership,
      katz_alpha = katz_alpha, hubbell_weight = hubbell_weight,
      dist_mat = this_dist_mat,
      hop_mat = this_hop_mat,
      out_hop_mat = this_out_hop_mat,
      shapley_k = shapley_k, shapley_cutoff = shapley_cutoff,
      s_shell_a = s_shell_a, discount_p = discount_p,
      ncvote_theta = ncvote_theta,
      comm_r = comm_r, ld_radius = ld_radius, enrenew_depth = enrenew_depth,
      voterank_lambda = voterank_lambda, contraction_rho = contraction_rho,
      wks_alpha = wks_alpha, wks_beta = wks_beta,
      renewed_threshold = renewed_threshold, kpath_k = kpath_k,
      kpath_len = kpath_len, epc_threshold = epc_threshold,
      epc_runs = epc_runs, epc_seed = epc_seed,
      betweenness_delta = betweenness_delta,
      closeness_delta = closeness_delta, gravity_mass = gravity_mass,
      gravity_radius = gravity_radius, mdd_lambda = mdd_lambda,
      volume_radius = volume_radius, diffusion_q = diffusion_q,
      diffusion_steps = diffusion_steps, ds_beta = ds_beta,
      ds_mu = ds_mu, ds_steps = ds_steps, cda_alpha = cda_alpha,
      icc_alpha = icc_alpha, exogenous_base = exogenous_base,
      wlr_alpha = wlr_alpha, alr_h_mode = alr_h_mode,
      grc_gamma = grc_gamma, rwd_decay = rwd_decay,
      rwd_node_weights = rwd_node_weights,
      linerank_aggregation = linerank_aggregation,
      bridging_steps = bridging_steps, bridging_values = bridging_values,
      proximal_variant = proximal_variant, exf_alpha = exf_alpha,
      beta_direction = beta_direction,
      ninl_order = ninl_order, ninl_radius = ninl_radius,
      map_flow = map_flow, map_convention = map_convention,
      sr_prior = sr_prior, mcgm_radius = mcgm_radius,
      mcgm_alpha = mcgm_alpha, dkgm_radius = dkgm_radius,
      nd_order = nd_order, nd_decay = nd_decay, nd_mass = nd_mass,
      ira_mass = ira_mass, ira_alpha = ira_alpha, ira_tol = ira_tol,
      ira_max_iter = ira_max_iter, iira_beta = iira_beta,
      iira_steps = iira_steps, hcc_delta = hcc_delta,
      lhc_radius = lhc_radius,
      tpr_alpha = tpr_alpha, tpr_k = tpr_k, tpr_decay = tpr_decay,
      tpr_tol = tpr_tol, tpr_max_iter = tpr_max_iter,
      rsp_beta = rsp_beta, rsp_cost = rsp_cost,
      re_indexes = re_indexes, re_negative = re_negative
    )
    value <- .cg_tier_guard(m, m %in% tier_measures, cg$n,
                            compute())

    # Normalize if requested. Closeness (igraph's rule) and harmonic
    # (divided by n - 1) carry their own normalization in the kernel.
    if (normalized && !m %in% c("closeness", "harmonic") &&
          any(!is.na(value))) {
      max_val <- if (isTRUE(psych_network) && m %in% psychometric_measures) {
        max(abs(value), na.rm = TRUE)
      } else {
        max(value, na.rm = TRUE)
      }
      if (!is.na(max_val) && max_val > 0) {
        value <- value / max_val
      }
    }

    # Column name with mode suffix for directional measures
    col_name <- if (m %in% mode_measures) paste0(m, "_", mode) else m
    results[[col_name]] <- value
  }

  df <- as.data.frame(results, stringsAsFactors = FALSE)

  # Round if digits specified
  if (!is.null(digits)) {
    num_cols <- vapply(df, is.numeric, logical(1))
    df[num_cols] <- lapply(df[num_cols], round, digits = digits)
  }

  # Sort if sort_by specified
  if (!is.null(sort_by)) {
    if (!sort_by %in% names(df)) {
      .cg_stop_bad_parameter("sort_by column '", sort_by, "' not found in results")
    }
    df <- df[order(df[[sort_by]], decreasing = TRUE), ]
    rownames(df) <- NULL
  }

  df
}

#' Zhang-Horvath weighted clustering coefficient, tna's "onnela" (matches tna)
#'
#' Implements `wcc(x + t(x))` per the formula used by `tna::centralities(.,
#' "Clustering")`: symmetrize the directed weight matrix, zero the diagonal,
#' then for each node compute `diag(M^3)_v / ((sum_j M_vj)^2 - sum_j M_vj^2)`.
#' Returns a numeric vector of length n.
#'
#' @param cg A `cg_graph` context. Weights are read from `cg$w`; an
#'   unweighted graph carries a binary matrix there.
#' @return Numeric vector of clustering values, one per vertex.
#' @noRd
calculate_clustering_onnela <- function(cg) {
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  M <- cg$w + t(cg$w)
  diag(M) <- 0
  num <- diag(M %*% M %*% M)
  den <- .colSums(M, n, n)^2 - .colSums(M^2, n, n)
  num / den
}

calculate_diffusion_power_series <- function(cg, loops = TRUE) {
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  W <- cg$w
  if (!isTRUE(loops)) diag(W) <- 0
  s <- matrix(0, n, n)
  p <- diag(1, n, n)
  # Each power depends on the previous one, so the series is sequential.
  for (i in seq_len(n)) {
    p <- p %*% W
    s <- s + p
  }
  .rowSums(s, n, n)
}

# Calculate diffusion centrality (vectorized). For each node, sums the
# scaled degrees of itself and its neighbors.
calculate_diffusion <- function(cg, mode = "all", lambda = 1) {
  cg <- .cg_context(cg)
  if (cg$n == 0) return(numeric(0))
  .cg_diffusion(cg$b, cg$directed, mode = mode, lambda = lambda)
}

#' Calculate leverage centrality (vectorized)
#'
#' Fast vectorized implementation of leverage centrality.
#' Measures how much a node influences its neighbors based on relative degrees.
#' Formula: l_i = (1/k_i) * sum_j((k_i - k_j) / (k_i + k_j)) for neighbors j
#'
#' @param cg A `cg_graph` context.
#' @param mode "all", "in", or "out" for directed graphs
#' @param loops Logical; whether to count loop edges
#' @return Numeric vector of leverage centrality values
#' @noRd
calculate_leverage <- function(cg, mode = "all", loops = TRUE) {
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  b <- cg$b
  if (!isTRUE(loops)) diag(b) <- 0
  # Degrees at `mode` (a loop counts as igraph counts it), and the neighbor
  # set at `mode` -- a vertex with a loop is its own neighbor, as in the
  # adjacency igraph reported.
  k <- .cg_degree(b, cg$directed, mode)
  adj <- if (!cg$directed) b != 0
         else switch(mode, out = b != 0, `in` = t(b) != 0,
                     all = (b + t(b)) != 0)
  vapply(seq_len(n), function(i) {
    if (k[i] == 0) return(NaN)
    j_set <- which(adj[i, ])
    if (length(j_set) == 0L) return(NaN) # nocov
    denom <- k[i] + k[j_set]
    mean(ifelse(denom == 0, 0, (k[i] - k[j_set]) / denom))
  }, numeric(1L))
}

#' Calculate geodesic k-path centrality (vectorized)
#'
#' Fast vectorized implementation of geodesic k-path centrality.
#' Counts neighbors that are on a geodesic path less than or equal to k away.
#'
#' @param cg A `cg_graph` context (an igraph object is accepted and converted).
#' @param mode "all", "in", or "out" for directed graphs
#' @param weights Edge weights (NULL for unweighted)
#' @param k Maximum path length. Default 3.
#' @return Numeric vector of kreach centrality values
#' @noRd
calculate_kreach <- function(cg, mode = "all", weights = NULL, k = 3) {
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  if (k <= 0) {
    .cg_stop_bad_parameter("The k parameter must be greater than 0.")
  }

  # Count nodes within distance k (excluding self)
  sp <- .cg_distances(.cg_attr_matrix(cg, weights), mode)
  as.integer(.cg_kreach(sp, n, k))
}

#' Calculate Laplacian centrality
#'
#' Measures the drop in Laplacian energy when a node is removed.
#' Higher values indicate more important nodes.
#'
#' @param cg A `cg_graph` context (an igraph object is accepted and converted).
#' @param weights Edge weights (NULL for unweighted)
#' @param normalized Whether to normalize by max value
#' @return Numeric vector of Laplacian centrality values
#' @noRd
calculate_laplacian <- function(cg, weights = NULL, normalized = FALSE) {
  cg <- .cg_context(cg)
  # Qi et al. (2012) local formula: deg² + deg + 2 * Σ(neighbor_degrees)
  # Matches NetworkX and centiserve::laplacian()
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n == 1) return(0)

  # Degrees count a loop twice (igraph's convention), and the neighbor
  # list igraph reported includes the vertex itself for a loop -- once on
  # a directed graph, twice on an undirected one -- so the neighbor-degree
  # sum is a count-weighted product rather than a plain adjacency product.
  deg <- .cg_degree(cg$b, cg$directed, "all")
  count <- cg$b
  if (!cg$directed) diag(count) <- 2 * diag(cg$b)
  result <- unname(deg^2 + deg + 2 * as.numeric(count %*% deg))

  if (normalized && max(result) > 0) {
    result <- result / max(result)
  }

  result
}

#' Calculate load centrality
#'
#' Goh et al.'s load centrality as implemented in sna::loadcent.
#' Uses Brandes-style algorithm where flow is divided equally among
#' shortest-path predecessors. Matches sna::loadcent().
#'
#' @param cg A `cg_graph` context (an igraph object is accepted and converted).
#' @param weights Edge weights (NULL for unweighted)
#' @param directed Whether to consider edge direction
#' @return Numeric vector of load centrality values
#' @noRd
calculate_load <- function(cg, weights = NULL, directed = TRUE) {
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n == 1) return(0)

  # Unweighted unless weights are given (NULL never falls back to the
  # graph's own weights here), and reversed first on a directed graph,
  # which is sna's convention.
  m <- .cg_path_matrix(cg, weights)
  if (directed && cg$directed) m <- t(m)
  if (!directed) m <- .cg_mode_weights(m, "all")
  .cg_load(m, n, directed)
}

#' Calculate current-flow closeness centrality (information centrality)
#'
#' Based on electrical current flow through the network.
#' Uses the pseudoinverse of the Laplacian matrix.
#'
#' @param cg A `cg_graph` context (an igraph object is accepted and converted).
#' @param weights Edge weights (NULL for unweighted)
#' @return Numeric vector of current-flow closeness values
#' @noRd
calculate_current_flow_closeness <- function(cg, weights = NULL) {
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n <= 1) return(rep(NA_real_, n))

  # Must be connected for current flow
  if (.cg_n_components(cg$b) > 1L) {
    .cg_warn_undefined("Graph is not connected; current-flow closeness ",
                       "undefined for disconnected nodes")
    return(rep(NA_real_, n))
  }

  # Effective resistance R_ij = L+_ii + L+_jj - 2 L+_ij from the
  # pseudo-inverse of the Laplacian; closeness is (n - 1) over their sum.
  L_pinv <- .cg_laplacian_pinv(cg, weights)
  if (is.null(L_pinv)) return(rep(NA_real_, n)) # nocov
  dg <- diag(L_pinv)
  total <- vapply(seq_len(n), function(i) {
    j <- seq_len(n)[seq_len(n) != i]
    sum(dg[i] + dg[j] - 2 * L_pinv[i, j])
  }, numeric(1L))
  (n - 1) / total
}

#' Pseudo-inverse of the Laplacian, as the current-flow measures read it
#'
#' The Laplacian takes the given weights, else the graph's own (igraph's
#' `weights = NULL` reading). It is shifted by the all-ones matrix over `n`
#' and inverted through an SVD with rank filtering, which is well defined
#' even when the shifted matrix is rank deficient.
#'
#' @param cg A `cg_graph` context.
#' @param weights Edge weights in canonical order, or NULL.
#' @return An `n x n` matrix, or NULL when every singular value is zero.
#' @noRd
.cg_laplacian_pinv <- function(cg, weights = NULL) {
  n <- cg$n
  L <- .cg_laplacian_matrix(.cg_attr_matrix(cg, weights), cg$directed)
  L_tilde <- L - 1 / n
  s <- svd(L_tilde)
  positive <- s$d > max(dim(L_tilde)) * max(s$d) * .Machine$double.eps
  if (!any(positive)) return(NULL) # nocov
  s$v[, positive, drop = FALSE] %*%
    diag(1 / s$d[positive], nrow = sum(positive)) %*%
    t(s$u[, positive, drop = FALSE])
}

#' Calculate current-flow betweenness centrality
#'
#' Betweenness based on current flow rather than shortest paths.
#' Measures the amount of current passing through each node.
#'
#' @param cg A `cg_graph` context (an igraph object is accepted and converted).
#' @param weights Edge weights (NULL for unweighted, treated as conductances)
#' @return Numeric vector of current-flow betweenness values
#' @noRd
calculate_current_flow_betweenness <- function(cg, weights = NULL) {
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n <= 2) return(rep(0, n))

  # Must be connected and undirected
  if (.cg_n_components(cg$b) > 1L) {
    .cg_warn_undefined("Graph is not connected; current-flow betweenness undefined")
    return(rep(NA_real_, n))
  }

  L_pinv <- .cg_laplacian_pinv(cg, weights)
  if (is.null(L_pinv)) return(rep(NA_real_, n)) # nocov
  # Throughput uses the same conductances as the Laplacian above, so the
  # potentials and the currents they drive come from one network.
  A_mat <- .cg_attr_matrix(cg, weights)

  # Brandes & Fleischer: one unit of current per (s, t) pair; the pairs
  # cannot be collapsed because each induces a different potential field.
  # The per-node throughput is rowSums(A * |p_v - p_u|) / 2.
  pairs <- which(upper.tri(diag(n)), arr.ind = TRUE)
  throughputs <- vapply(seq_len(nrow(pairs)), function(k) {
    s <- pairs[k, 1L]; t <- pairs[k, 2L]
    potential <- L_pinv[, s] - L_pinv[, t]
    throughput <- rowSums(A_mat * abs(outer(potential, potential, `-`))) * 0.5
    throughput[c(s, t)] <- 0
    throughput
  }, numeric(n))
  betweenness <- rowSums(matrix(throughputs, nrow = n))

  # Normalize: 2 / ((n-1)(n-2)) matches NetworkX normalized=TRUE
  betweenness * 2 / ((n - 1) * (n - 2))
}

#' Calculate VoteRank centrality
#'
#' Iteratively finds influential spreaders by voting mechanism.
#' Each iteration selects the node with most votes, then reduces voting
#' power of its neighbors.
#'
#' @param cg A `cg_graph` context (an igraph object is accepted and converted).
#' @param directed Whether to consider edge direction
#' @return Numeric vector with rank order (1 = most influential, higher = less)
#' @noRd
calculate_voterank <- function(cg, directed = TRUE)
{
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n == 1) return(1)
  .cg_voterank(cg$b, directed)
}

#' Calculate percolation centrality
#'
#' Measures node importance for percolation/spreading processes using Brandes algorithm.
#' Each node has a "percolation state" (0-1) representing how activated/infected it is.
#' When all states are 1, this equals betweenness centrality.
#'
#' @param cg A `cg_graph` context (an igraph object is accepted and converted).
#' @param states Named numeric vector of percolation states (0-1) for each node.
#'   If NULL, all nodes get state 1 (equivalent to betweenness).
#' @param weights Edge weights (NULL for unweighted)
#' @param directed Whether to respect edge direction
#' @return Numeric vector of percolation centrality values
#' @references
#' Piraveenan, M., Prokopenko, M., & Hossain, L. (2013).
#' Percolation centrality: Quantifying graph-theoretic impact of nodes during percolation in networks.
#' @noRd
calculate_percolation <- function(cg, states = NULL, weights = NULL, directed = TRUE) {
  cg <- .cg_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n <= 2) return(rep(0, n))

  # Initialize percolation states (default all 1.0)
  if (is.null(states)) {
    states <- rep(1.0, n)
  } else {
    if (!is.null(names(states))) {
      states <- states[as.character(cg$labels)]
    }
    if (length(states) != n) {
      .cg_stop_bad_parameter("states vector length must match number of nodes")
    }
    states[is.na(states)] <- 1.0
    states <- pmax(0, pmin(1, states))
  }

  # Unweighted unless weights are given; direction as the graph has it.
  m <- .cg_path_matrix(cg, weights)
  if (!directed) m <- .cg_mode_weights(m, "all")
  .cg_percolation(m, n, states)
}

#' Accept an igraph object where a context is expected
#'
#' The `calculate_*` helpers take a `cg_graph`; an igraph object handed to
#' them directly (as the older tests do) is converted once, using igraph
#' only to read the object the caller already built.
#'
#' @param cg A `cg_graph` context or an igraph object.
#' @return A `cg_graph` context.
#' @noRd
.cg_context <- function(cg) {
  if (inherits(cg, "cg_graph")) return(cg)
  .cg_graph(cg)
}

#' Weight matrix igraph would have read for a measure
#'
#' igraph's `weights = NULL` means "use the graph's own weight attribute if
#' it has one", which is what every measure ported from igraph saw when
#' `centrality()` passed `NULL` (unweighted call, or an unweighted input).
#' This reproduces that reading: the explicit vector when given, else the
#' graph's weights, else the binary matrix.
#'
#' @param cg A `cg_graph` context.
#' @param weights Edge weights in canonical order, or NULL.
#' @return Numeric weight matrix.
#' @noRd
.cg_attr_matrix <- function(cg, weights = NULL) {
  .cg_path_matrix(cg, weights %||% cg$weights)
}

#' Transitivity in igraph's vocabulary
#'
#' `"local"` / `"localundirected"` give the per-vertex clustering
#' coefficient, `"global"` / `"undirected"` the single graph-level ratio.
#' The weighted variants (`"barrat"`, `"weighted"`) have no native kernel
#' yet and still go through igraph.
#'
#' @param cg A `cg_graph` context.
#' @param type One of igraph's transitivity types.
#' @param isolates `"nan"` or `"zero"` for vertices with fewer than two
#'   neighbors (local types only).
#' @return Numeric vector (local) or a single number (global).
#' @noRd
calculate_transitivity <- function(cg, type = "local", isolates = "nan") {
  cg <- .cg_context(cg)
  if (type %in% c("global", "undirected")) {
    return(.cg_global_transitivity(cg$b))
  }
  if (type %in% c("local", "localundirected")) {
    out <- .cg_local_transitivity(cg$b, cg$n, cg$directed)
    if (identical(isolates, "zero")) out[is.nan(out)] <- 0
    return(out)
  }
  if (type %in% c("barrat", "weighted")) {
    out <- .cg_barrat_transitivity(cg$w, cg$directed)
    if (identical(isolates, "zero")) out[is.nan(out)] <- 0
    return(out)
  }
  stop(errorCondition(paste0("unknown transitivity type '", type, "'"),
                      class = "cograph_bad_input", call = NULL))
}

#' Calculate a single centrality measure
#' @noRd
calculate_measure <- function(cg, measure, mode, weights, normalized,
                              cutoff, damping, personalized,
                              transitivity_type, isolates,
                              hits_result = NULL, lambda = 1,
                              diffusion_method = "kandhway_kuri",
                              loops = TRUE,
                              k = 3,
                              states = NULL, decay_parameter = 0.5,
                              dmnc_epsilon = 1.7,
                              membership = NULL,
                              katz_alpha = 0.1, hubbell_weight = 0.5,
                              dist_mat = NULL, hop_mat = NULL,
                              out_hop_mat = NULL,
                              shapley_k = 2, shapley_cutoff = 2,
                              s_shell_a = 0.5, discount_p = 0.01,
                              ncvote_theta = 0.5,
                              comm_r = "max_intra", ld_radius = 2,
                              enrenew_depth = 2, voterank_lambda = 0.1,
                              contraction_rho = 5, wks_alpha = 1,
                              wks_beta = 1, renewed_threshold = 2,
                              kpath_k = 3, kpath_len = 3,
                              epc_threshold = 0.5, epc_runs = 1000,
                              epc_seed = NULL, betweenness_delta = 1,
                              closeness_delta = 1, gravity_mass = "kshell",
                              gravity_radius = 3, mdd_lambda = 0.7,
                              volume_radius = 2, diffusion_q = 1,
                              diffusion_steps = 3, ds_beta = 0.1,
                              ds_mu = 1, ds_steps = 5, cda_alpha = 0.5,
                              icc_alpha = 0.2,
                              exogenous_base = "reverse_closeness",
                              wlr_alpha = 1, alr_h_mode = "all",
                              grc_gamma = 1, rwd_decay = 0.5,
                              rwd_node_weights = NULL,
                              linerank_aggregation = "probability",
                              bridging_steps = 2, bridging_values = NULL,
                              proximal_variant = "source", exf_alpha = 2,
                              beta_direction = "positive",
                              ninl_order = 3, ninl_radius = NULL,
                              map_flow = "unrecorded",
                              map_convention = "paper", sr_prior = 0,
                              mcgm_radius = 2, mcgm_alpha = NULL,
                              dkgm_radius = 2, nd_order = 2,
                              nd_decay = 0.2, nd_mass = "degree",
                              ira_mass = "coreness", ira_alpha = 1,
                              ira_tol = 1e-6, ira_max_iter = 1000,
                              iira_beta = 0.2, iira_steps = 50,
                              hcc_delta = 0.5, lhc_radius = 2,
                              tpr_alpha = 0.85, tpr_k = 0.85, tpr_decay = 1,
                              tpr_tol = 1e-14, tpr_max_iter = 1000,
                              rsp_beta = 0.01,
                              rsp_cost = c("inverse", "weight"),
                              re_indexes = c("degree", "closeness",
                                             "betweenness", "constraint"),
                              re_negative = NULL) {
  cg <- .cg_context(cg)
  directed <- cg$directed
  n <- cg$n

  value <- switch(measure,
    # Measures that support mode
    "degree" = .cg_degree(cg$b, directed, mode),
    "strength" = .cg_strength(.cg_attr_matrix(cg, weights), directed, mode),
    "closeness" = {
      d <- .cg_distances(.cg_attr_matrix(cg, weights), mode, cutoff)
      cl <- .cg_closeness(d, n)
      # igraph's normalization: multiply by the number of vertices reached.
      if (normalized) cl * (rowSums(is.finite(d)) - 1) else cl
    },
    # igraph::eccentricity() reads the graph's own weights whatever
    # `weights` says, so the path weights in force are not used here.
    "eccentricity" = .cg_eccentricity(
      .cg_distances(.cg_path_matrix(cg, cg$weights), mode), n),
    # igraph keeps a self-loop as a permanent degree offset while peeling;
    # .cg_loop_coreness() (R/kernels-batch11.R) reproduces that.
    "coreness" = .cg_loop_coreness(cg$b, n, directed, mode),
    "harmonic" = {
      d <- .cg_distances(.cg_attr_matrix(cg, weights), mode, cutoff)
      h <- .cg_harmonic(d, n)
      if (normalized && n > 1L) h / (n - 1) else h
    },
    "diffusion" = if (identical(diffusion_method, "power_series")) {
      calculate_diffusion_power_series(cg, loops = loops)
    } else {
      calculate_diffusion(cg, mode = mode, lambda = lambda)
    },
    "leverage" = calculate_leverage(cg, mode = mode),
    "kreach" = calculate_kreach(cg, mode = mode, weights = weights, k = k),
    # Both solve (I - alpha A) x = b, which is singular when alpha sits at an
    # eigenvalue of A. The kernels report that as NaN; the caller is told
    # which measure failed and why through cograph_singular_system.
    "alpha" = .cg_solve_or_stop("alpha", function() {
      if (n == 0L) stop("there is no system to solve on an empty graph")
      a <- .cg_mode_matrix(.cg_attr_matrix(cg, weights), directed, mode, "in")
      out <- .cg_alpha(a, n, alpha = 1)
      if (anyNA(out)) stop("the system (I - alpha A) is singular")
      out
    }),
    "power" = .cg_solve_or_stop("power", function() {
      b <- .cg_mode_matrix(cg$b, directed, mode, "out", binary = TRUE)
      out <- .cg_power(b, n, alpha = 1)
      # An edgeless graph is NaN by definition (0/0), not a failed solve.
      if (anyNA(out) && any(b != 0)) stop("the system (I - alpha A) is singular")
      out
    }),

    # Measures without mode
    "subgraph" = {
      if (n == 0L) stop("subgraph centrality is undefined on a 0 x 0 matrix", call. = FALSE)
      .cg_subgraph(cg$w, n, directed)
    },
    "laplacian" = calculate_laplacian(cg, weights = weights, normalized = normalized),
    "load" = calculate_load(cg, weights = weights, directed = directed),
    "current_flow_closeness" = calculate_current_flow_closeness(cg, weights = weights),
    "current_flow_betweenness" = calculate_current_flow_betweenness(cg, weights = weights),
    "voterank" = calculate_voterank(cg, directed = directed),
    "percolation" = calculate_percolation(cg, states = states, weights = weights, directed = directed),
    "betweenness" = .cg_betweenness(.cg_attr_matrix(cg, weights), n, directed,
                                    cutoff = cutoff),
    "eigenvector" = .cg_eigenvector(.cg_attr_matrix(cg, weights), n),
    "pagerank" = .cg_pagerank(.cg_attr_matrix(cg, weights), n,
                              damping = damping,
                              personalized = .cg_match_personalized(
                                personalized, cg$labels)),
    "authority" = hits_result$authority,
    "hub" = hits_result$hub,
    "constraint" = {
      cw <- .cg_attr_matrix(cg, weights)
      out <- .cg_constraint(cw, n)
      # A vertex whose only tie is a self-loop is not an isolate to igraph:
      # it has an (empty) ego network and scores 0, not NaN.
      out[is.nan(out) & diag(cw) != 0] <- 0
      out
    },
    "transitivity" = if (identical(transitivity_type, "onnela")) {
      calculate_clustering_onnela(cg)
    } else {
      calculate_transitivity(cg, type = transitivity_type, isolates = isolates)
    },

    # Extended measures — distance-based closeness variants
    "radiality" = calculate_radiality(cg, mode = mode, weights = weights,
                                      dist_mat = dist_mat),
    "lin" = calculate_lin(cg, mode = mode, weights = weights,
                          dist_mat = dist_mat),
    "decay" = calculate_decay(cg, mode = mode, weights = weights,
                              decay_parameter = decay_parameter,
                              dist_mat = dist_mat),
    "residual_closeness" = calculate_residual_closeness(cg, mode = mode,
                                                        weights = weights,
                                                        dist_mat = dist_mat),
    "dangalchev" = calculate_dangalchev(cg, mode = mode, weights = weights,
                                        dist_mat = dist_mat),
    "generalized_closeness" = calculate_generalized_closeness(cg, mode = mode, weights = weights, alpha = decay_parameter,
      dist_mat = dist_mat
    ),
    "harary" = calculate_harary(cg, mode = mode, weights = weights,
                                dist_mat = dist_mat),
    "average_distance" = calculate_average_distance(cg, mode = mode,
                                                    weights = weights,
                                                    dist_mat = dist_mat),
    "barycenter" = calculate_barycenter(cg, mode = mode, weights = weights,
                                        dist_mat = dist_mat),
    "wiener" = calculate_wiener(cg, mode = mode, weights = weights,
                                dist_mat = dist_mat),
    "closeness_vitality" = calculate_closeness_vitality(cg, mode = mode,
                                                        weights = weights,
                                                        dist_mat = dist_mat),

    # Extended measures — spectral/walk-based
    "communicability" = calculate_communicability(cg),
    "communicability_betweenness" = calculate_communicability_betweenness(cg),
    "random_walk" = calculate_random_walk(cg),

    # Extended measures — path-based
    "stress" = calculate_stress(cg, weights = weights, directed = directed),
    # Deliberately left on igraph (max-flow); see docs/igraph-removal-contract.md.
    "flow_betweenness" = {
      .cg_need_igraph("flow_betweenness")
      calculate_flow_betweenness(cg, weights = weights, directed = directed)
    },

    # Extended measures — local/neighborhood
    "lobby" = calculate_lobby(cg, mode = mode),
    "entropy" = calculate_entropy(cg, mode = mode),
    "semilocal" = calculate_semilocal(cg, mode = mode),
    "clusterrank" = calculate_clusterrank(cg, mode = mode),
    "bottleneck" = calculate_bottleneck(cg, mode = mode),
    "centroid" = calculate_centroid(cg, mode = mode, weights = weights,
                                    dist_mat = dist_mat),
    "mnc" = calculate_mnc(cg, mode = mode),
    "dmnc" = calculate_dmnc(cg, mode = mode, epsilon = dmnc_epsilon),
    "lac" = calculate_lac(cg, mode = mode),
    "topological_coefficient" = calculate_topological_coefficient(cg),
    "bridging" = calculate_bridging(cg, weights = weights, directed = directed),
    "local_bridging" = calculate_local_bridging(cg),
    "effective_size" = calculate_effective_size(cg),
    "diversity" = calculate_diversity(cg, weights = weights),
    "cross_clique" = calculate_cross_clique(cg),
    "markov" = calculate_markov(cg),

    # Extended measures — with mode support
    "integration" = calculate_integration(cg, mode = mode),
    "expected" = calculate_expected(cg, mode = mode),
    "gilschmidt" = calculate_gilschmidt(cg, mode = mode),

    # Zoo batch 2 — mode measures
    "gravity" = calculate_gravity(cg, mode = mode, mass = gravity_mass,
                                  radius = gravity_radius),
    "collective_influence" = calculate_collective_influence(cg, mode = mode),
    "local_hindex" = as.numeric(calculate_local_hindex(cg, mode = mode)),
    "hindex_strength" = as.numeric(calculate_hindex_strength(cg, mode = mode)),
    "onion" = as.numeric(calculate_onion(cg)),

    # Zoo batch 2 — no-mode measures
    "second_order" = calculate_second_order(cg),
    "infection" = calculate_infection(cg),
    "nonbacktracking" = calculate_nonbacktracking(cg),
    "spanning_tree" = calculate_spanning_tree(cg),
    "expected_influence_1" = calculate_expected_influence(cg, weights = weights, step = 1L, mode = mode),
    "expected_influence_2" = calculate_expected_influence(cg, weights = weights, step = 2L, mode = mode),

    # Directed-only measures
    "salsa" = calculate_salsa(cg),
    "leaderrank" = calculate_leaderrank(cg),
    "weighted_leaderrank" = calculate_weighted_leaderrank(cg, wlr_alpha),
    "adaptive_leaderrank" = calculate_adaptive_leaderrank(cg, alr_h_mode),
    "trophic_level" = calculate_trophic_level(cg),

    # Community-aware measures (require membership parameter)
    "participation" = calculate_participation(cg, membership = membership,
                                              mode = mode),
    "within_module_z" = calculate_within_module_z(cg, membership = membership,
                                                   mode = mode),
    "gateway" = calculate_gateway(cg, membership = membership, mode = mode),

    # Batch 3 — classical measures with reference-package validation
    "katz" = calculate_katz(cg, weights = weights, alpha = katz_alpha),
    "hubbell" = calculate_hubbell(cg, weights = weights,
                                  weightfactor = hubbell_weight),
    "information" = calculate_information(cg, weights = weights),
    "pairwisedis" = calculate_pairwisedis(cg),
    "reaching_local" = calculate_reaching_local(cg, mode = mode,
                                                weights = weights),

    # Batch 4 — directed prestige family (Wasserman-Faust / sna)
    "prestige_domain" = calculate_prestige_domain(cg),
    "prestige_domain_proximity" = calculate_prestige_domain_proximity(cg),

    # Batch 5 — Gould-Fernandez brokerage (5 roles)
    "brokerage_coordinator"    = calculate_brokerage(cg, membership, "coordinator"),
    "brokerage_itinerant"      = calculate_brokerage(cg, membership, "itinerant"),
    "brokerage_representative" = calculate_brokerage(cg, membership, "representative"),
    "brokerage_gatekeeper"     = calculate_brokerage(cg, membership, "gatekeeper"),
    "brokerage_liaison"        = calculate_brokerage(cg, membership, "liaison"),

    # Batch 7 — Centrality Zoo comparison batch (R/centrality-batch7.R)
    "distance_entropy" = calculate_distance_entropy(cg, mode = mode,
                                                    hop_mat = hop_mat),
    "local_dimension" = calculate_local_dimension(cg, mode = mode,
                                                  hop_mat = hop_mat),
    "local_information_dimension" = calculate_local_information_dimension(cg, mode = mode, hop_mat = hop_mat),
    "neighborhood_connectivity" = calculate_neighborhood_connectivity(cg, mode = mode),
    "modularity_vitality" = calculate_modularity_vitality(cg, weights = weights, membership = membership),

    # Batch 8 — Centrality Zoo "on the way" batch (R/centrality-batch8.R)
    "shapley_game1" = calculate_shapley(cg, game = 1L),
    "shapley_game2" = calculate_shapley(cg, game = 2L, k = shapley_k),
    "shapley_game3" = calculate_shapley(cg, game = 3L, cutoff = shapley_cutoff,
                                        hop_mat = out_hop_mat),
    "access_information" = calculate_search_information(cg, what = "access", hop_mat = out_hop_mat),
    "hide_information" = calculate_search_information(cg, what = "hide", hop_mat = out_hop_mat),
    "rumor" = calculate_rumor(cg),
    "community_hub_bridge" = calculate_community_hub_bridge(cg, membership = membership, mode = mode),
    "entropy_variation_degree" = calculate_entropy_variation(cg, of = "degree", mode = mode),
    "entropy_variation_betweenness" = calculate_entropy_variation(cg, of = "betweenness"),
    "s_shell" = calculate_s_shell(cg, a = s_shell_a),
    "degree_discount" = calculate_degree_discount(cg, p = discount_p),
    "single_discount" = calculate_degree_discount(cg, single = TRUE),
    "ncvoterank" = calculate_ncvoterank(cg, theta = ncvote_theta),

    # Batch 9 — remaining Zoo measures (R/centrality-batch9.R)
    "community_based" = calculate_community_based(cg, membership = membership, mode = mode),
    "comm_centrality" = calculate_comm_centrality(cg, membership = membership, mode = mode, r = comm_r),
    "community_mediator" = calculate_community_mediator(cg, membership = membership, mode = mode),
    "local_dimension_fixed" = calculate_local_dimension_fixed(cg, mode = mode, r = ld_radius, hop_mat = hop_mat),
    "fuzzy_local_dimension" = calculate_fuzzy_local_dimension(cg, mode = mode, hop_mat = hop_mat),
    "local_volume_dimension" = calculate_local_volume_dimension(cg, mode = mode, hop_mat = hop_mat),
    "wvoterank" = calculate_wvoterank(cg, weights = weights),
    "enrenew" = calculate_enrenew(cg, depth = enrenew_depth),
    "voterank_plus" = calculate_voterank_plus(cg, lambda = voterank_lambda),
    "node_contraction" = calculate_node_contraction(cg),
    "node_contraction_improved" = calculate_node_contraction(cg, improved = TRUE, rho = contraction_rho),
    "two_way_rw" = calculate_two_way_rw(cg, weights = weights),
    "heatmap" = calculate_heatmap(cg, mode = mode, hop_mat = hop_mat),
    "flow_coefficient" = calculate_flow_coefficient(cg),
    "local_entropy" = calculate_local_entropy(cg, mode = mode),
    "weighted_h_index" = calculate_weighted_h_index(cg, mode = mode),
    "redundancy" = calculate_redundancy(cg),
    "weighted_kshell" = calculate_weighted_kshell(cg, weights = weights, alpha = wks_alpha, beta = wks_beta),
    "renewed_coreness" = calculate_renewed_coreness(cg, threshold = renewed_threshold),
    "geodesic_kpath" = calculate_geodesic_kpath(cg, mode = mode, k = kpath_k, hop_mat = hop_mat),

    # Batch 10 — cross-package gaps (R/centrality-batch10.R)
    "local_efficiency" = calculate_local_efficiency(cg, mode = mode, weights = weights),
    "s_core" = calculate_s_core(cg, weights = weights),
    "fragmentation" = calculate_fragmentation(cg, mode = mode, weights = weights),
    "kpath" = calculate_kpath(cg, mode = mode, k = kpath_len),
    "epc" = calculate_epc(cg, threshold = epc_threshold, runs = epc_runs,
                          seed = epc_seed),

    # Batch 11 — parameterized family members (R/centrality-batch11.R)
    "length_scaled_betweenness" = calculate_length_scaled_betweenness(cg, weights = weights),
    "delta_betweenness" = calculate_delta_betweenness(cg, weights = weights, delta = betweenness_delta),
    "ego_betweenness" = calculate_ego_betweenness(cg),
    "delta_closeness" = calculate_delta_closeness(cg, mode = mode, delta = closeness_delta, dist_mat = dist_mat,
      weights = weights),

    # Batch 12 — simple undirected topology (R/centrality-batch12.R)
    "truss" = calculate_candidate_local(cg, "truss"),
    "mdd" = calculate_candidate_local(cg, "mdd", mdd_lambda),
    "bridging_coefficient" = calculate_candidate_local(cg, "bridging_coefficient"),
    "godfather" = calculate_candidate_local(cg, "godfather"),
    "support" = calculate_candidate_local(cg, "support"),

    # Batch 13 — volume and maximal clique structure
    "volume" = calculate_candidate_structure(cg, "volume", volume_radius),
    "mcc" = calculate_candidate_structure(cg, "mcc"),
    "diffusion_centrality" = calculate_finite_diffusion(cg, weights, diffusion_q, diffusion_steps),
    "dynamical_importance" = calculate_dynamical_importance(cg, weights),
    "dynamics_sensitive" = calculate_dynamics_sensitive(cg, ds_beta, ds_mu, ds_steps),
    "malatya" = calculate_malatya(cg),
    "expected_force" = calculate_expected_force(cg),
    "mcgm" = calculate_mcgm(cg, mcgm_radius, mcgm_alpha, normalized),
    "spectralrank" = calculate_spectralrank(cg, weights, sr_prior),
    "controlrank" = calculate_controlrank(cg, weights, normalized),
    "map_equation" = calculate_map_equation(cg, weights, membership, damping, map_flow, map_convention),
    "ninl" = calculate_ninl(cg, ninl_order, ninl_radius, normalized),
    "beta_measure" = calculate_beta_measure(cg, beta_direction),
    "localized_bridging" = calculate_localized_bridging(cg, 1L),
    "extended_local_bridging" = calculate_localized_bridging(cg, 2L),
    "modified_expected_force" = calculate_expected_force(cg, TRUE, exf_alpha),
    "proximal_betweenness" = calculate_proximal_betweenness(cg, proximal_variant),
    "x_degree" = calculate_x_degree(cg),
    "coleman_theil" = calculate_coleman_theil(cg, weights),
    "bridging_capital" = calculate_bridging_capital(cg, weights, bridging_steps, bridging_values, normalized),
    "linerank" = calculate_linerank(cg, weights, damping, linerank_aggregation, normalized),
    "random_walk_decay" = calculate_random_walk_decay(cg, weights, rwd_decay, rwd_node_weights, normalized),
    "graph_regularization" = calculate_graph_regularization(cg, weights, grc_gamma),
    "resistance_curvature" = calculate_resistance_curvature(cg, weights),
    "extended_coreness" = calculate_extended_core(cg, "extended_coreness"),
    "cda" = calculate_cda(cg, weights, cda_alpha),
    "improved_closeness" = calculate_improved_closeness(cg, icc_alpha),
    "exogenous" = calculate_exogenous(cg, mode, exogenous_base),
    "global_structure" = calculate_global_structure(cg, "gsm", normalized),
    "hybrid_global_structure" = calculate_global_structure(cg, "hgsm", normalized),
    "improved_global_structure" = calculate_global_structure(cg, "igsm", normalized),
    "dkgm" = calculate_dkgm(cg, dkgm_radius),
    "neighbor_distance" = calculate_neighbor_distance(cg, nd_order, nd_decay,
                                                      nd_mass),
    "ira" = calculate_ira(cg, ira_mass, ira_alpha, ira_tol, ira_max_iter),
    "iira" = calculate_iira(cg, ira_mass, iira_beta, iira_steps),
    "lnc" = calculate_lnc(cg),
    "ked" = calculate_ked(cg),
    "hcc" = calculate_hcc(cg, hcc_delta),
    "ehcc" = calculate_ehcc(cg, hcc_delta),
    "lhc" = calculate_lhc(cg, lhc_radius),
    "iec" = calculate_iec(cg),
    "dil" = calculate_dil(cg),
    "trust_pagerank" = calculate_trust_pagerank(cg, tpr_alpha, tpr_k,
                                                tpr_decay, tpr_tol,
                                                tpr_max_iter),
    "rsp_betweenness" = calculate_rsp_betweenness(cg, weights, rsp_beta,
                                                  rsp_cost),
    "relative_entropy" = calculate_relative_entropy(cg, re_indexes,
                                                   re_negative),
    "mixed_gravity" = calculate_mixed_gravity(cg, gravity_radius),
    "extended_mixed_gravity" = calculate_mixed_gravity(cg, gravity_radius, extended = TRUE),
    "extended_gravity" = calculate_extended_core(cg, "extended_gravity",
                                                gravity_radius),

    stop(errorCondition(paste0("Unknown measure: ", measure),
                        class = "cograph_unknown_measure", call = NULL))
  )

  # Remove names to ensure consistent output
  unname(value)
}

#' Degree Centrality
#'
#' Degree centrality counts the edges incident to each node. With
#' \code{mode = "in"} it counts incoming edges and with \code{mode = "out"}
#' outgoing edges. On an undirected network the three modes agree.
#'
#' @details
#' Edge weights are ignored; \code{\link{centrality_strength}} sums them
#' instead. \code{normalized = TRUE} divides the scores by their maximum.
#' \code{centrality_indegree()} and \code{centrality_outdegree()} are the
#' \code{mode = "in"} and \code{mode = "out"} forms.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"in"} or
#'   \code{"out"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Freeman, L. C. (1978). Centrality in social networks conceptual
#'   clarification. Social Networks, 1(3), 215-239.
#'   \doi{10.1016/0378-8733(78)90021-7}.
#' @seealso \code{\link{centrality_strength}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_degree(regulation_net)
centrality_degree <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "degree", mode = mode, ...)
  col <- paste0("degree_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_degree
#' @export
centrality_indegree <- function(x, ...) {
  df <- centrality(x, measures = "degree", mode = "in", ...)
  stats::setNames(df$degree_in, df$node)
}

#' @rdname centrality_degree
#' @export
centrality_outdegree <- function(x, ...) {
  df <- centrality(x, measures = "degree", mode = "out", ...)
  stats::setNames(df$degree_out, df$node)
}

#' Strength Centrality
#'
#' Strength (Barrat et al. 2004) is the sum of the weights of the edges
#' incident to a node, the weighted counterpart of degree. With
#' \code{mode = "in"} it sums incoming weights and with \code{mode = "out"}
#' outgoing weights. \code{centrality_instrength()} and
#' \code{centrality_outstrength()} are these two forms.
#'
#' @details
#' The stored weights are summed, and \code{weighted = FALSE} gives every
#' edge weight one, so the scores equal \code{\link{centrality_degree}}. A
#' self-loop
#' counts twice on an undirected network and under \code{mode = "all"}, and
#' once under \code{"in"} or \code{"out"}; \code{loops = FALSE} drops it.
#' Negative weights are summed with their sign. \code{normalized = TRUE}
#' divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"in"} or \code{"out"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{loops} (keep self-loops, default \code{TRUE}) and
#'   \code{normalized} (default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A. (2004).
#'   The architecture of complex weighted networks. Proceedings of the
#'   National Academy of Sciences, 101(11), 3747-3752.
#'   \doi{10.1073/pnas.0400087101}.
#' @seealso \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_strength(regulation_net)
centrality_strength <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "strength", mode = mode, ...)
  col <- paste0("strength_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_strength
#' @export
centrality_instrength <- function(x, ...) {
  df <- centrality(x, measures = "strength", mode = "in", ...)
  stats::setNames(df$strength_in, df$node)
}

#' @rdname centrality_strength
#' @export
centrality_outstrength <- function(x, ...) {
  df <- centrality(x, measures = "strength", mode = "out", ...)
  stats::setNames(df$strength_out, df$node)
}

#' Betweenness Centrality
#'
#' Betweenness centrality (Freeman 1977) sums, over pairs of other nodes, the
#' fraction of shortest paths between them that pass through the node:
#' \deqn{B(v) = \sum_{s \ne v \ne t} \frac{\sigma_{st}(v)}{\sigma_{st}},}{
#'   B(v) = sum_{s != v != t} sigma_st(v) / sigma_st,}
#' where \eqn{\sigma_{st}}{sigma_st} is the number of shortest paths from
#' \eqn{s} to \eqn{t} and \eqn{\sigma_{st}(v)}{sigma_st(v)} the number of
#' those through \eqn{v}.
#'
#' @details
#' On a directed network the sum runs over ordered pairs along the edge
#' direction, and on an undirected network over unordered pairs. Edge weights
#' are read as path lengths, and \code{invert_weights = TRUE} uses
#' \eqn{1/w^\alpha}{1/w^alpha} instead. \code{weighted = FALSE} uses hop
#' counts. \code{cutoff} drops paths longer
#' than the given length. \code{normalized = TRUE} divides the scores by
#' their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{invert_weights} (default \code{NULL}, which is \code{TRUE} for tna
#'   input), \code{alpha} (inversion exponent, default 1), \code{cutoff}
#'   (largest path length considered, default -1 for no limit) and
#'   \code{normalized} (default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Freeman, L. C. (1977). A set of measures of centrality based on
#'   betweenness. Sociometry, 40(1), 35-41. \doi{10.2307/3033543}.
#' @seealso \code{\link{centrality_load}}, \code{\link{centrality_stress}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_betweenness(regulation_net)
centrality_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "betweenness", ...)
  stats::setNames(df$betweenness, df$node)
}

#' Closeness Centrality
#'
#' Closeness centrality (Sabidussi 1966) is the reciprocal of the total
#' shortest-path distance from a node to the nodes it reaches:
#' \deqn{C(v) = \frac{1}{\sum_{w \ne v} d(v, w)}.}{
#'   C(v) = 1 / sum_{w != v} d(v, w).}
#' Unreachable nodes are left out of the sum, as in
#' \code{igraph::closeness()}.
#'
#' @details
#' Edge weights are read as path lengths, and \code{invert_weights = TRUE}
#' uses \eqn{1/w^\alpha}{1/w^alpha} instead. \code{weighted = FALSE} uses
#' hop counts. \code{mode = "out"} follows paths
#' leaving the node and \code{mode = "in"} paths arriving at it.
#' \code{centrality_outcloseness()} and \code{centrality_incloseness()} are
#' these two forms. A node that reaches no other node returns \code{NaN}.
#' \code{normalized = TRUE} multiplies each score by the number of other
#' nodes the node reaches, as igraph does.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{invert_weights} (default \code{NULL}, which is \code{TRUE} for tna
#'   input), \code{alpha} (inversion exponent, default 1), \code{cutoff}
#'   (largest path length considered, default -1 for no limit) and
#'   \code{normalized} (default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Sabidussi, G. (1966). The centrality index of a graph. Psychometrika,
#'   31(4), 581-603. \doi{10.1007/BF02289527}.
#' @seealso \code{\link{centrality_harmonic}},
#'   \code{\link{centrality_barycenter}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_closeness(regulation_net)
centrality_closeness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "closeness", mode = mode, ...)
  col <- paste0("closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_closeness
#' @export
centrality_incloseness <- function(x, ...) {
  df <- centrality(x, measures = "closeness", mode = "in", ...)
  stats::setNames(df$closeness_in, df$node)
}

#' @rdname centrality_closeness
#' @export
centrality_outcloseness <- function(x, ...) {
  df <- centrality(x, measures = "closeness", mode = "out", ...)
  stats::setNames(df$closeness_out, df$node)
}

#' Eigenvector Centrality
#'
#' Eigenvector centrality (Bonacich 1972) scores a node by the scores of the
#' nodes that point to it. The scores form the dominant eigenvector of the
#' transposed weight matrix:
#' \deqn{\lambda x = A^{T} x.}{lambda x = A^T x.}
#' The vector is scaled to a maximum of one.
#'
#' @details
#' \eqn{A} holds the edge weights, or ones with \code{weighted = FALSE}.
#' On a directed network a node gains
#' standing from its incoming edges. The scores lie between 0 and 1. A
#' network without edges gives every node a score of one.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Bonacich, P. (1972). Factoring and weighting approaches to status scores
#'   and clique identification. Journal of Mathematical Sociology, 2(1),
#'   113-120. \doi{10.1080/0022250X.1972.9989806}.
#' @seealso \code{\link{centrality_pagerank}}, \code{\link{centrality_alpha}},
#'   \code{\link{centrality_authority}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_eigenvector(regulation_net)
centrality_eigenvector <- function(x, ...) {
  df <- centrality(x, measures = "eigenvector", ...)
  stats::setNames(df$eigenvector, df$node)
}

#' PageRank Centrality
#'
#' PageRank (Brin and Page 1998) is the stationary distribution of a random
#' walk that follows an out-edge with probability \eqn{d}, choosing edges in
#' proportion to their weights, and otherwise jumps to a node drawn from the
#' reset distribution \eqn{p}:
#' \deqn{PR = (1 - d)\,p + d\,P^{T} PR,}{PR = (1 - d) p + d t(P) PR,}
#' where \eqn{P} is the row-normalized weight matrix and \eqn{d} is
#' \code{damping}.
#'
#' @details
#' \code{weighted = FALSE} gives every edge weight one. A node without
#' out-edges passes its score to the reset distribution. The scores sum to
#' one and equal \code{igraph::page_rank()}. The vector \code{personalized}
#' is rescaled to sum to one. A named vector is matched to the node names,
#' and its names must be the node names, each used once; an unnamed vector
#' is matched by position. A negative weight raises a
#' \code{cograph_negative_weights} error, an invalid \code{personalized} a
#' \code{cograph_bad_input} error, and a \code{damping} outside
#' \eqn{[0, 1]} a \code{cograph_bad_parameter} error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param damping Probability \eqn{d} of following an edge (default 0.85).
#' @param personalized Reset distribution \eqn{p}, a non-negative numeric
#'   vector with one entry per node, named by node or in input node order.
#'   The default
#'   \code{NULL} gives the uniform distribution.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Brin, S., & Page, L. (1998). The anatomy of a large-scale hypertextual Web
#'   search engine. Computer Networks and ISDN Systems, 30(1-7), 107-117.
#'   \doi{10.1016/S0169-7552(98)00110-X}.
#' @seealso \code{\link{centrality_eigenvector}},
#'   \code{\link{centrality_leaderrank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_pagerank(regulation_net)
centrality_pagerank <- function(x, damping = 0.85, personalized = NULL, ...) {
  df <- centrality(x, measures = "pagerank",
                   damping = damping, personalized = personalized, ...)
  stats::setNames(df$pagerank, df$node)
}

#' HITS Authority and Hub Scores
#'
#' The HITS algorithm (Kleinberg 1999) assigns each node an authority score
#' from the hubs that point to it and a hub score from the authorities it
#' points to:
#' \deqn{a = \lambda^{-1} A^{T} h, \qquad h = \lambda^{-1} A a .}{
#'   a = A^T h / lambda, h = A a / lambda.}
#' Authorities are the dominant eigenvector of \eqn{A^{T} A}{A^T A} and hubs
#' the dominant eigenvector of \eqn{A A^{T}}{A A^T}.
#'
#' @details
#' \eqn{A} holds the edge weights, or ones with \code{weighted = FALSE}.
#' Both scores are scaled to a maximum
#' of one, so they lie between 0 and 1. On an undirected network both equal
#' eigenvector centrality. A network without edges gives every node a score
#' of one. \code{centrality_hub()} returns the hub scores.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Kleinberg, J. M. (1999). Authoritative sources in a hyperlinked
#'   environment. Journal of the ACM, 46(5), 604-632.
#'   \doi{10.1145/324133.324140}.
#' @seealso \code{\link{centrality_eigenvector}},
#'   \code{\link{centrality_pagerank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_authority(regulation_net)
#' centrality_hub(regulation_net)
centrality_authority <- function(x, ...) {
  df <- centrality(x, measures = "authority", ...)
  stats::setNames(df$authority, df$node)
}

#' @rdname centrality_authority
#' @export
centrality_hub <- function(x, ...) {
  df <- centrality(x, measures = "hub", ...)
  stats::setNames(df$hub, df$node)
}

#' Eccentricity
#'
#' The eccentricity of a node (Hage and Harary 1995) is its largest
#' shortest-path distance to a node it reaches:
#' \deqn{e(v) = \max_{w:\, d(v, w) < \infty} d(v, w).}{
#'   e(v) = max_{w: d(v, w) < Inf} d(v, w).}
#' Lower values mark more central nodes.
#'
#' @details
#' Edge weights are read as path lengths, and \code{invert_weights} has no
#' effect. \code{weighted = FALSE}, or an unweighted input, gives hop
#' counts. \code{mode = "out"} follows
#' paths leaving the node and \code{mode = "in"} paths arriving at it.
#' \code{centrality_outeccentricity()} and \code{centrality_ineccentricity()}
#' are these two forms. A node that reaches no other node scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Hage, P., & Harary, F. (1995). Eccentricity and centrality in networks.
#'   Social Networks, 17(1), 57-63. \doi{10.1016/0378-8733(94)00248-9}.
#' @seealso \code{\link{centrality_closeness}},
#'   \code{\link{centrality_radiality}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_eccentricity(regulation_net)
centrality_eccentricity <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "eccentricity", mode = mode, ...)
  col <- paste0("eccentricity_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_eccentricity
#' @export
centrality_ineccentricity <- function(x, ...) {
  df <- centrality(x, measures = "eccentricity", mode = "in", ...)
  stats::setNames(df$eccentricity_in, df$node)
}

#' @rdname centrality_eccentricity
#' @export
centrality_outeccentricity <- function(x, ...) {
  df <- centrality(x, measures = "eccentricity", mode = "out", ...)
  stats::setNames(df$eccentricity_out, df$node)
}

#' Coreness
#'
#' Coreness (Seidman 1983) is the largest \eqn{k} for which a node belongs to
#' the \eqn{k}-core, the maximal subnetwork in which every node has degree at
#' least \eqn{k}. It is found by repeatedly removing the nodes of lowest
#' degree.
#'
#' @details
#' Edge weights are ignored. \code{mode = "in"} and \code{mode = "out"} peel
#' by in-degree and out-degree, and \code{mode = "all"} by total degree, in
#' which a reciprocated tie counts twice. A self-loop adds a fixed amount to
#' the degree of its node throughout the peeling. The values match
#' \code{igraph::coreness()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{loops} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Seidman, S. B. (1983). Network structure and minimum degree. Social
#'   Networks, 5(3), 269-287. \doi{10.1016/0378-8733(83)90028-X}.
#' @seealso \code{\link{centrality_degree}},
#'   \code{\link{centrality_s_core}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_coreness(regulation_net)
centrality_coreness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "coreness", mode = mode, ...)
  col <- paste0("coreness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Burt's Constraint
#'
#' Burt's constraint measures how much a node's ties are concentrated in
#' contacts that are themselves tied to each other. With \eqn{p_{ij}}{p_ij}
#' the proportion of the tie strength of \eqn{i} invested in \eqn{j},
#' \deqn{C_i = \sum_{j \ne i} \Big( p_{ij} + \sum_{q \ne i, j} p_{iq}
#'   p_{qj} \Big)^2.}{
#'   C_i = sum_{j != i} (p_ij + sum_{q != i, j} p_iq p_qj)^2.}
#' Low constraint marks access to structural holes.
#'
#' @details
#' Ties are symmetrized as \eqn{w_{ij} + w_{ji}}{w_ij + w_ji} before the
#' proportions are formed, so edge direction is ignored. With
#' \code{weighted = FALSE} every tie has weight one. The values match
#' \code{igraph::constraint()}. An isolated node returns \code{NaN}, and a
#' node whose only tie is a self-loop scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_effective_size}},
#'   \code{\link{centrality_bridging}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_constraint(regulation_net)
centrality_constraint <- function(x, ...) {
  df <- centrality(x, measures = "constraint", ...)
  stats::setNames(df$constraint, df$node)
}

#' Local Transitivity
#'
#' Local transitivity, the clustering coefficient of Watts and Strogatz
#' (1998), is the share of pairs of a node's neighbors that are themselves
#' linked:
#' \deqn{C_i = \frac{2 T_i}{k_i (k_i - 1)},}{C_i = 2 T_i / (k_i (k_i - 1)),}
#' where \eqn{T_i} is the number of triangles through node \eqn{i} and
#' \eqn{k_i} its degree.
#'
#' @details
#' Triangles and degrees are counted on the undirected skeleton, so a
#' reciprocated tie counts once, and edge weights are ignored. The values
#' equal \code{igraph::transitivity(type = "local")} on directed and
#' undirected networks. \code{"localundirected"} gives the same values as
#' \code{"local"}, as in igraph. \code{"global"} and \code{"undirected"}
#' return the network-level ratio of closed to connected triples for every
#' node. \code{"barrat"} and \code{"weighted"} compute the weighted
#' coefficient of Barrat et al. (2004) and raise a
#' \code{cograph_directed_unsupported} error on directed input.
#' \code{"onnela"} computes the weighted clustering coefficient of Zhang and
#' Horvath (2005),
#' \eqn{(M^3)_{ii} / (s_i^2 - \sum_j M_{ij}^2)}{(M^3)_ii / (s_i^2 - sum_j M_ij^2)}
#' on \eqn{M = W + W^{T}}{M = W + t(W)} with strengths \eqn{s_i}. This is the
#' value \code{tna::centralities()} reports as Clustering, and it is the
#' default for tna input. The option keeps the name \code{"onnela"} used by
#' tna, but the formula multiplies the raw weights of a triangle and differs
#' from the geometric-mean form of Onnela et al. (2005). Under the local and
#' Barrat types a node with fewer than two neighbors is \code{NaN}, or 0
#' with \code{isolates = "zero"}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param transitivity_type One of \code{"local"} (default), \code{"global"},
#'   \code{"undirected"}, \code{"localundirected"}, \code{"barrat"},
#'   \code{"weighted"} or \code{"onnela"}.
#' @param isolates Value for nodes with fewer than two ties: \code{"nan"}
#'   (default) or \code{"zero"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{tna_network} (default \code{NULL}, which detects tna input).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Watts, D. J., & Strogatz, S. H. (1998). Collective dynamics of
#'   'small-world' networks. Nature, 393(6684), 440-442.
#'   \doi{10.1038/30918}.
#'
#' Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A. (2004).
#'   The architecture of complex weighted networks. Proceedings of the
#'   National Academy of Sciences, 101(11), 3747-3752.
#'   \doi{10.1073/pnas.0400087101}.
#'
#' Zhang, B., & Horvath, S. (2005). A general framework for weighted gene
#'   co-expression network analysis. Statistical Applications in Genetics and
#'   Molecular Biology, 4(1), Article 17. \doi{10.2202/1544-6115.1128}.
#'
#' Onnela, J.-P., Saramaki, J., Kertesz, J., & Kaski, K. (2005). Intensity
#'   and coherence of motifs in weighted complex networks. Physical Review E,
#'   71(6), 065103. \doi{10.1103/PhysRevE.71.065103}.
#' @seealso \code{\link{centrality_clusterrank}},
#'   \code{\link{centrality_topological_coefficient}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_transitivity(regulation_net)
centrality_transitivity <- function(x, transitivity_type = "local",
                                    isolates = "nan", ...) {
  df <- centrality(x, measures = "transitivity",
                   transitivity_type = transitivity_type, isolates = isolates, ...)
  stats::setNames(df$transitivity, df$node)
}

#' Harmonic Centrality
#'
#' Harmonic centrality (Marchiori and Latora 2000) sums the inverse
#' shortest-path distances from a node to the other nodes:
#' \deqn{H(i) = \sum_{j \ne i} \frac{1}{d_{ij}},}{
#'   H(i) = sum_{j != i} 1 / d_ij,}
#' with \eqn{1/\infty = 0}{1/Inf = 0}, so the score is defined on
#' disconnected networks (Boldi and Vigna 2014).
#' \code{centrality_inharmonic()} and \code{centrality_outharmonic()} are the
#' \code{mode = "in"} and \code{mode = "out"} forms.
#'
#' @details
#' Edge weights are read as distances, and \code{invert_weights = TRUE}
#' converts a weight \eqn{w} to the distance \eqn{1/w^\alpha}{1/w^alpha}.
#' \code{weighted = FALSE} uses hop counts. \code{mode = "all"} treats edges
#' as undirected, \code{"out"} uses distances from the node and \code{"in"}
#' distances to it. The scores equal \code{igraph::harmonic_centrality()} on
#' the weighted graph. \code{normalized = TRUE} divides the scores by
#' \eqn{n - 1}, as \code{igraph::harmonic_centrality(normalized = TRUE)}
#' does.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{invert_weights} (default \code{NULL}, which inverts for tna input
#'   only), \code{alpha} (inversion exponent, default 1), \code{cutoff}
#'   (largest distance counted, default -1 for no limit) and
#'   \code{normalized} (default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Marchiori, M., & Latora, V. (2000). Harmony in the small-world. Physica A,
#'   285(3-4), 539-546. \doi{10.1016/S0378-4371(00)00311-3}.
#'
#' Boldi, P., & Vigna, S. (2014). Axioms for centrality. Internet
#'   Mathematics, 10(3-4), 222-262. \doi{10.1080/15427951.2013.865686}.
#' @seealso \code{\link{centrality_closeness}},
#'   \code{\link{centrality_reaching_local}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_harmonic(regulation_net)
centrality_harmonic <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "harmonic", mode = mode, ...)
  col <- paste0("harmonic_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_harmonic
#' @export
centrality_inharmonic <- function(x, ...) {
  df <- centrality(x, measures = "harmonic", mode = "in", ...)
  stats::setNames(df$harmonic_in, df$node)
}

#' @rdname centrality_harmonic
#' @export
centrality_outharmonic <- function(x, ...) {
  df <- centrality(x, measures = "harmonic", mode = "out", ...)
  stats::setNames(df$harmonic_out, df$node)
}

#' Diffusion Centrality
#'
#' Diffusion centrality has two forms, chosen by \code{diffusion_method}. The
#' \code{"kandhway_kuri"} form (Kandhway and Kuri 2014) adds the degrees of a
#' node's neighbors to its own degree, both scaled by \eqn{\lambda}{lambda}:
#' \eqn{DC(v) = \lambda k_v + \lambda \sum_{u \in N(v)} k_u}{DC(v) = lambda
#' k_v + lambda sum_{u in N(v)} k_u}. The \code{"power_series"} form sums the
#' rows of the first \eqn{n} powers of the weight matrix:
#' \deqn{DC(v) = \sum_{w} \left( W + W^2 + \cdots + W^n \right)_{vw}.}{
#'   DC(v) = sum_w (W + W^2 + ... + W^n)_vw.}
#'
#' @details
#' The default is \code{"kandhway_kuri"}, and \code{"power_series"} for tna
#' input. The \code{"kandhway_kuri"} form uses binary degrees, so edge
#' weights are ignored, and \code{mode} sets both the degrees and the
#' neighbor set. On a directed network \code{mode = "all"} uses total degrees
#' and the undirected neighbor set. \code{lambda} multiplies every score. The
#' \code{"power_series"} form uses the edge weights, or ones with
#' \code{weighted = FALSE}, and ignores \code{mode} and \code{lambda}. With
#' \code{loops = FALSE} the diagonal
#' of \eqn{W} is set to zero. The \code{"power_series"} values match
#' \code{tna::centralities(measures = "Diffusion")}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param lambda Scale factor \eqn{\lambda}{lambda} of the
#'   \code{"kandhway_kuri"} form. Default 1.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{diffusion_method} (\code{"kandhway_kuri"} or
#'   \code{"power_series"}, default \code{NULL}, which picks by input type)
#'   and \code{loops} (default \code{TRUE}, \code{FALSE} for tna input).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_expected}},
#'   \code{\link{centrality_diffusion_centrality}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_diffusion(regulation_net)
centrality_diffusion <- function(x, mode = "all", lambda = 1, ...) {
  df <- centrality(x, measures = "diffusion", mode = mode, lambda = lambda, ...)
  col <- paste0("diffusion_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Leverage Centrality
#'
#' Leverage centrality (Joyce et al. 2010) compares the degree of a node with
#' the degrees of its neighbors:
#' \deqn{l_i = \frac{1}{|N(i)|} \sum_{j \in N(i)} \frac{k_i - k_j}{k_i + k_j}.}{
#'   l_i = (1 / |N(i)|) sum_{j in N(i)} (k_i - k_j) / (k_i + k_j).}
#' Positive values mark nodes with more ties than their typical neighbor.
#'
#' @details
#' Edge weights are ignored. \code{mode} selects both the degree and the
#' neighbor set, and with \code{mode = "all"} on a directed network the
#' degree is in plus out. The score lies between -1 and 1. An isolated node
#' is \code{NaN}. On undirected networks the values equal
#' \code{centiserve::leverage()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Joyce, K. E., Laurienti, P. J., Burdette, J. H., & Hayasaka, S. (2010). A
#'   new measure of centrality for brain networks. PLoS ONE, 5(8), e12200.
#'   \doi{10.1371/journal.pone.0012200}.
#' @seealso \code{\link{centrality_degree}},
#'   \code{\link{centrality_neighborhood_connectivity}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_leverage(regulation_net)
centrality_leverage <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "leverage", mode = mode, ...)
  col <- paste0("leverage_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Geodesic K-Reach Centrality
#'
#' Geodesic k-reach centrality (Borgatti and Everett 2006) counts the nodes
#' that lie within shortest-path distance \eqn{k} of a node.
#'
#' @details
#' Edge weights are read as distances, so on a weighted network \eqn{k} is
#' compared with the summed weights along a path; on \code{regulation_net},
#' whose weights lie below one, every node reaches all others within
#' \eqn{k = 1}. \code{invert_weights = TRUE} converts a weight \eqn{w} to the
#' distance \eqn{1/w^\alpha}{1/w^alpha}. \code{weighted = FALSE} uses hop
#' counts. \code{mode = "all"} treats edges as undirected, \code{"out"} counts nodes
#' reached from the node and \code{"in"} nodes that reach it. A \code{k} of
#' zero or below raises a \code{cograph_bad_parameter} error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param k Largest distance counted, a positive number (default 3).
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{invert_weights} (default \code{NULL}, which inverts for tna input
#'   only) and \code{alpha} (inversion exponent, default 1).
#' @return A named integer vector with one count per node, in input node
#'   order.
#' @references
#' Borgatti, S. P., & Everett, M. G. (2006). A graph-theoretic perspective on
#'   centrality. Social Networks, 28(4), 466-484.
#'   \doi{10.1016/j.socnet.2005.11.005}.
#' @seealso \code{\link{centrality_geodesic_kpath}},
#'   \code{\link{centrality_harmonic}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_kreach(regulation_net, k = 0.2)
centrality_kreach <- function(x, mode = "all", k = 3, ...) {
  df <- centrality(x, measures = "kreach", mode = mode, k = k, ...)
  col <- paste0("kreach_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Alpha Centrality
#'
#' Alpha centrality (Bonacich and Lloyd 2001) gives every node an exogenous
#' score of one plus the scores of the nodes that point to it:
#' \deqn{x = \alpha A^{T} x + e, \qquad x = (I - \alpha A^{T})^{-1} e.}{
#'   x = alpha A^T x + e, x = (I - alpha A^T)^(-1) e.}
#' The attenuation is fixed at \eqn{\alpha = 1}{alpha = 1} and \eqn{e} is a
#' vector of ones, the defaults of \code{igraph::alpha_centrality()}.
#'
#' @details
#' \eqn{A} holds the edge weights, or ones with \code{weighted = FALSE},
#' with the diagonal set to zero. The
#' \code{alpha} argument of \code{\link{centrality}} is the weight-inversion
#' exponent, which this measure does not read. The
#' scores are positive when the spectral radius of \eqn{A} is below one and
#' can be negative otherwise. A singular system or a negative edge weight
#' raises an error of class \code{cograph_singular_system}.
#'
#' On a directed network \code{mode = "in"} sums over incoming ties as above
#' and equals \code{igraph::alpha_centrality()}, \code{mode = "out"} uses
#' \eqn{A} in place of \eqn{A^{T}}{A^T} and so sums over outgoing ties, and
#' \code{mode = "all"} (default) uses the symmetrized weights
#' \eqn{A + A^{T}}{A + t(A)}. On an undirected network the three modes agree.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"in"} or
#'   \code{"out"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Bonacich, P., & Lloyd, P. (2001). Eigenvector-like measures of centrality
#'   for asymmetric relations. Social Networks, 23(3), 191-201.
#'   \doi{10.1016/S0378-8733(01)00038-7}.
#' @seealso \code{\link{centrality_katz}}, \code{\link{centrality_eigenvector}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_alpha(regulation_net)
centrality_alpha <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "alpha", mode = mode, ...)
  col <- paste0("alpha_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Bonacich Power Centrality
#'
#' Bonacich (1987) power centrality with exponent \eqn{\beta = 1}{beta = 1}
#' sums the walks leaving a node, each further step weighted by \eqn{\beta}{beta}:
#' \deqn{c = \gamma\,(I - A)^{-1} A \mathbf{1},}{c = g (I - A)^(-1) A 1,}
#' where \eqn{A} is the binary adjacency matrix and \eqn{\gamma}{g} scales
#' the scores so that their squares sum to \eqn{n}.
#'
#' @details
#' The exponent is fixed at 1. Edge weights and self-loops are ignored. On a
#' directed network \code{mode = "out"} sums over out-ties and equals
#' \code{igraph::power_centrality(exponent = 1)}, \code{mode = "in"} sums
#' over in-ties, and \code{mode = "all"} (default) uses the undirected
#' skeleton, the binary matrix with a tie wherever either direction has one.
#' On an undirected network the three modes agree. Where \eqn{I - A} is
#' singular, as on a
#' network that contains an isolated edge, a \code{cograph_singular_system}
#' error is raised, and a network without edges gives \code{NaN}. The scores
#' can be negative, as on \code{regulation_net}, and \code{normalized = TRUE}
#' leaves scores that are all negative unchanged.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Bonacich, P. (1987). Power and centrality: A family of measures. American
#'   Journal of Sociology, 92(5), 1170-1182. \doi{10.1086/228631}.
#' @seealso \code{\link{centrality_eigenvector}},
#'   \code{\link{centrality_katz}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_power(regulation_net)
centrality_power <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "power", mode = mode, ...)
  col <- paste0("power_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Subgraph Centrality
#'
#' Subgraph centrality (Estrada and Rodriguez-Velazquez 2005) counts the
#' closed walks that start and end at a node, weighting a walk of length
#' \eqn{k} by \eqn{1/k!}:
#' \deqn{SC(i) = \sum_{k=0}^{\infty} \frac{(A^k)_{ii}}{k!} = \left(e^{A}\right)_{ii}.}{
#'   SC(i) = sum_k (A^k)_ii / k! = exp(A)_ii.}
#'
#' @details
#' \eqn{A} is the binary adjacency matrix without self-loops, so edge weights
#' are ignored. On a directed network \eqn{A} is replaced by
#' \eqn{A + A^{T}}{A + t(A)}, so a reciprocated pair has entry 2; the values
#' then equal \code{igraph::subgraph_centrality()}. An empty network raises
#' an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Estrada, E., & Rodriguez-Velazquez, J. A. (2005). Subgraph centrality in
#'   complex networks. Physical Review E, 71(5), 056103.
#'   \doi{10.1103/PhysRevE.71.056103}.
#' @seealso \code{\link{centrality_communicability}},
#'   \code{\link{centrality_eigenvector}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_subgraph(regulation_net)
centrality_subgraph <- function(x, ...) {
  df <- centrality(x, measures = "subgraph", ...)
  stats::setNames(df$subgraph, df$node)
}

#' Laplacian Centrality
#'
#' Laplacian centrality (Qi et al. 2012) is the drop in the Laplacian energy
#' of a network when a node is removed. Without edge weights the drop is
#' \deqn{L(v) = k_v^2 + k_v + 2 \sum_{u \in N(v)} k_u.}{
#'   L(v) = k_v^2 + k_v + 2 sum_{u in N(v)} k_u.}
#'
#' @details
#' Edge weights are ignored, so the score is the unweighted case of the
#' weighted measure of Qi et al. (2012). On a directed network \eqn{k} is the
#' total degree, in plus out, and the neighbor sum runs over out-neighbors.
#' A self-loop adds 2 to the degree. On undirected networks the values equal
#' \code{centiserve::laplacian()}. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{normalized} (default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Qi, X., Fuller, E., Wu, Q., Wu, Y., & Zhang, C.-Q. (2012). Laplacian
#'   centrality: A new centrality measure for weighted networks. Information
#'   Sciences, 194, 240-253. \doi{10.1016/j.ins.2011.12.027}.
#' @seealso \code{\link{centrality_degree}},
#'   \code{\link{centrality_semilocal}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_laplacian(regulation_net)
centrality_laplacian <- function(x, ...) {
  df <- centrality(x, measures = "laplacian", ...)
  stats::setNames(df$laplacian, df$node)
}

#' Load Centrality
#'
#' Load centrality (Goh et al. 2001) sends one unit of load from every node
#' to every other node along shortest paths. At each branching the load is
#' split equally among the shortest-path predecessors, and the score of a
#' node is the total load that passes through it.
#'
#' @details
#' Edge weights are read as distances. \code{weighted = FALSE} uses hop
#' counts, and \code{invert_weights = TRUE} converts a weight \eqn{w} to the
#' distance \eqn{1/w^\alpha}{1/w^alpha}. Paths follow edge direction on a
#' directed network. The values equal \code{sna::loadcent()}, which credits
#' the endpoints of each path as well as the intermediate nodes, so the
#' scores are larger than betweenness.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which inverts for tna input only) and \code{alpha}
#'   (inversion exponent, default 1).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Goh, K.-I., Kahng, B., & Kim, D. (2001). Universal behavior of load
#'   distribution in scale-free networks. Physical Review Letters, 87(27),
#'   278701. \doi{10.1103/PhysRevLett.87.278701}.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_stress}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_load(regulation_net)
centrality_load <- function(x, ...) {
  df <- centrality(x, measures = "load", ...)
  stats::setNames(df$load, df$node)
}

#' Current-Flow Closeness
#'
#' Current-flow closeness (Brandes and Fleischer 2005), equal to the
#' information centrality of Stephenson and Zelen (1989), replaces the
#' shortest-path distance in closeness by the effective resistance
#' \eqn{R_{vw}}{R_vw} of the network read as an electrical circuit with the
#' edge weights as conductances:
#' \deqn{CFC(v) = \frac{n - 1}{\sum_{w \ne v} R_{vw}}, \qquad
#'   R_{vw} = L^{+}_{vv} + L^{+}_{ww} - 2 L^{+}_{vw},}{
#'   CFC(v) = (n - 1) / sum_{w != v} R_vw,
#'   R_vw = L+_vv + L+_ww - 2 L+_vw,}
#' with \eqn{L^{+}}{L+} the pseudoinverse of the Laplacian.
#'
#' @details
#' The measure is defined for connected undirected networks. On a
#' disconnected network every score is \code{NA}, with a
#' \code{cograph_undefined_measure} warning.
#' On a directed network the Laplacian is built from the asymmetric weight
#' matrix, and \code{directed = FALSE} gives the undirected reading. Edge
#' weights are conductances, and \code{weighted = FALSE} gives every edge
#' conductance one.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Stephenson, K., & Zelen, M. (1989). Rethinking centrality: Methods and
#'   examples. Social Networks, 11(1), 1-37.
#'   \doi{10.1016/0378-8733(89)90016-6}.
#'
#' Brandes, U., & Fleischer, D. (2005). Centrality measures based on current
#'   flow. In STACS 2005, Lecture Notes in Computer Science, 3404 (pp.
#'   533-544). Springer. \doi{10.1007/978-3-540-31856-9_44}.
#' @seealso \code{\link{centrality_current_flow_betweenness}},
#'   \code{\link{centrality_information}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_current_flow_closeness(regulation_net, directed = FALSE)
centrality_current_flow_closeness <- function(x, ...) {
  df <- centrality(x, measures = "current_flow_closeness", ...)
  stats::setNames(df$current_flow_closeness, df$node)
}

#' Current-Flow Betweenness
#'
#' Current-flow betweenness (Brandes and Fleischer 2005), also known as
#' random-walk betweenness (Newman 2005), treats the network as an electrical
#' circuit with the edge weights as conductances. One unit of current is sent
#' between every pair \eqn{s, t}{s, t}, and the score is the average current
#' that flows through the node:
#' \deqn{CFB(v) = \frac{2}{(n-1)(n-2)} \sum_{s < t} \frac{1}{2} \sum_{u}
#'   w_{vu} \left| p^{(st)}_v - p^{(st)}_u \right|,}{
#'   CFB(v) = 2 / ((n-1)(n-2)) sum_{s < t} (1/2) sum_u w_vu |p_v - p_u|,}
#' where \eqn{p^{(st)}}{p} are the node potentials of that pair, taken from
#' the pseudoinverse of the Laplacian, and the inner sum is set to zero for
#' \eqn{v = s} and \eqn{v = t}.
#'
#' @details
#' The measure is defined for connected undirected networks. On a
#' disconnected network every score is \code{NA}, with a
#' \code{cograph_undefined_measure} warning.
#' On a directed network the Laplacian is built from the asymmetric weight
#' matrix, and \code{directed = FALSE} gives the undirected reading. The
#' potentials and the currents come from the same weights, and
#' \code{weighted = FALSE} gives every edge conductance one.
#' The fixed factor \eqn{2/((n-1)(n-2))} is the normalization of
#' \code{networkx::current_flow_betweenness_centrality()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Brandes, U., & Fleischer, D. (2005). Centrality measures based on current
#'   flow. In STACS 2005, Lecture Notes in Computer Science, 3404 (pp.
#'   533-544). Springer. \doi{10.1007/978-3-540-31856-9_44}.
#'
#' Newman, M. E. J. (2005). A measure of betweenness centrality based on
#'   random walks. Social Networks, 27(1), 39-54.
#'   \doi{10.1016/j.socnet.2004.11.009}.
#' @seealso \code{\link{centrality_current_flow_closeness}},
#'   \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_current_flow_betweenness(regulation_net, directed = FALSE)
centrality_current_flow_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "current_flow_betweenness", ...)
  stats::setNames(df$current_flow_betweenness, df$node)
}

#' VoteRank Centrality
#'
#' VoteRank (Zhang et al. 2016) elects spreaders one at a time. Every node
#' votes for its neighbors with a voting ability that starts at 1, the node
#' with the most votes is elected, and each neighbor of the elected node
#' loses \eqn{1/\langle k \rangle}{1/<k>} of its ability, where
#' \eqn{\langle k \rangle}{<k>} is the mean degree. A node elected in round
#' \eqn{r} of \eqn{m} rounds scores \eqn{(m + 1 - r)/m}{(m + 1 - r) / m}, so
#' the first node elected scores 1.
#'
#' @details
#' Edge weights are ignored. On a directed network a node receives the votes
#' of its in-neighbors, the election reduces the ability of the out-neighbors
#' of the elected node, and the mean degree counts in-ties and out-ties.
#' Every node is elected in turn, so on \eqn{n} nodes the scores are
#' \eqn{1/n, 2/n, \ldots, 1}{1/n, 2/n, ..., 1}. A tie in votes goes to the
#' node listed first.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhang, J.-X., Chen, D.-B., Dong, Q., & Zhao, Z.-D. (2016). Identifying a
#'   set of influential spreaders in complex networks. Scientific Reports, 6,
#'   27823. \doi{10.1038/srep27823}.
#' @seealso \code{\link{centrality_ncvoterank}},
#'   \code{\link{centrality_wvoterank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_voterank(regulation_net)
centrality_voterank <- function(x, ...) {
  df <- centrality(x, measures = "voterank", ...)
  stats::setNames(df$voterank, df$node)
}

#' Percolation Centrality
#'
#' Percolation centrality (Piraveenan et al. 2013) weights each shortest path
#' through a node by the percolation state \eqn{x_s} of its source:
#' \deqn{PC(v) = \frac{1}{n-2} \sum_{s \ne v \ne t}
#'   \frac{\sigma_{st}(v)}{\sigma_{st}} \frac{x_s}{\sum_i x_i - x_v},}{
#'   PC(v) = (1 / (n - 2)) sum_{s != v != t} (sigma_st(v) / sigma_st) x_s / (sum_i x_i - x_v),}
#' where \eqn{\sigma_{st}}{sigma_st} is the number of shortest paths from
#' \eqn{s} to \eqn{t} and \eqn{\sigma_{st}(v)}{sigma_st(v)} the number through
#' \eqn{v}. With equal states the score is the normalized betweenness.
#'
#' @details
#' Edge weights are read as distances, \code{weighted = FALSE} uses hop
#' counts, and \code{invert_weights = TRUE} uses the distance
#' \eqn{1/w^\alpha}{1/w^alpha}, so tna input is inverted by default. Paths
#' follow edge direction on a directed network. \code{states} is matched to
#' nodes by name when it has names and by position otherwise. Its values are
#' clipped to \eqn{[0, 1]} and missing values are set to 1, and a vector of
#' the wrong length raises an error. A network with fewer than three nodes
#' scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param states Percolation state of each node, between 0 and 1. The
#'   default \code{NULL} gives every node state 1.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which inverts for tna input only) and \code{alpha}
#'   (inversion exponent, default 1).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Piraveenan, M., Prokopenko, M., & Hossain, L. (2013). Percolation
#'   centrality: Quantifying graph-theoretic impact of nodes during
#'   percolation in networks. PLoS ONE, 8(1), e53095.
#'   \doi{10.1371/journal.pone.0053095}.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_load}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_percolation(regulation_net)
centrality_percolation <- function(x, states = NULL, ...) {
  df <- centrality(x, measures = "percolation", states = states, ...)
  stats::setNames(df$percolation, df$node)
}

# =============================================================================
# Extended centrality convenience wrappers
# =============================================================================

#' Radiality Centrality
#'
#' Radiality (Valente and Foreman 1998) reverses each distance against the
#' diameter \eqn{D} and averages over the network:
#' \deqn{R(i) = \frac{1}{n - 1} \sum_{j:\, d_{ij} < \infty} (D + 1 - d_{ij}).}{
#'   R(i) = sum_{j: d_ij < Inf} (D + 1 - d_ij) / (n - 1).}
#' Nodes close to the others score high.
#'
#' @details
#' The sum includes the node itself, which contributes \eqn{D + 1}, and the
#' values equal \code{centiserve::radiality()} on undirected networks.
#' Unreachable nodes contribute 0. Edge weights are read as distances;
#' \code{weighted = FALSE} uses hop counts, and \code{invert_weights = TRUE}
#' converts a weight \eqn{w} to the distance \eqn{1/w^\alpha}{1/w^alpha}.
#' \code{mode = "all"} treats edges as undirected, \code{"out"} uses
#' distances from the node and \code{"in"} distances to it. The diameter is
#' taken on the stored edge weights in the direction of the network,
#' whatever \code{mode} and \code{invert_weights} are. A single-node network
#' gives \code{NA}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which inverts for tna input only), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest distance counted,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Valente, T. W., & Foreman, R. K. (1998). Integration and radiality:
#'   Measuring the extent of an individual's connectedness and reachability
#'   in a network. Social Networks, 20(1), 89-105.
#'   \doi{10.1016/S0378-8733(97)00007-5}.
#' @seealso \code{\link{centrality_integration}},
#'   \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_radiality(regulation_net)
centrality_radiality <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "radiality", mode = mode, ...)
  col <- paste0("radiality_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Lin Centrality
#'
#' Lin centrality divides the squared number of nodes a node reaches by the
#' sum of its distances to them:
#' \deqn{L(i) = \frac{r_i^2}{\sum_{j \in R_i} d_{ij}},}{
#'   L(i) = r_i^2 / sum_{j in R_i} d_ij,}
#' where \eqn{R_i} is the set of \eqn{r_i} nodes reachable from \eqn{i}. The
#' score is defined on disconnected networks.
#'
#' @details
#' Edge weights are read as distances. \code{weighted = FALSE} uses hop
#' counts, and \code{invert_weights = TRUE} converts a weight \eqn{w} to the
#' distance \eqn{1/w^\alpha}{1/w^alpha}. \code{mode = "all"} treats edges as
#' undirected, \code{"out"} uses distances from the node and \code{"in"}
#' distances to it. A node that reaches no other node scores 0, and a
#' single-node network gives \code{NA}. On undirected networks the values
#' equal \code{centiserve::lincent()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which inverts for tna input only), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest distance counted,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_closeness}},
#'   \code{\link{centrality_harmonic}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_lin(regulation_net)
centrality_lin <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "lin", mode = mode, ...)
  col <- paste0("lin_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Decay Centrality
#'
#' Decay centrality sums a decay factor \eqn{\delta}{delta} raised to the
#' distance from the node to every node:
#' \deqn{D(v) = \sum_{w} \delta^{d(v, w)}.}{D(v) = sum_w delta^d(v, w).}
#' The sum includes the node itself, which adds one to every score, and
#' unreachable nodes contribute 0. Values of \eqn{\delta}{delta} between 0
#' and 1 discount distant nodes.
#'
#' @details
#' Edge weights are read as path lengths. \code{invert_weights = TRUE} uses
#' \eqn{1/w^\alpha}{1/w^alpha} as the length, and \code{weighted = FALSE}
#' counts hops. \code{mode} sets the direction of the paths.
#' \code{decay_parameter} must lie strictly between 0 and 1, and other values
#' raise a \code{cograph_bad_parameter} error. \code{\link{centrality_generalized_closeness}} computes
#' the same quantity, and \code{decay_parameter = 0.5} gives
#' \code{\link{centrality_dangalchev}}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param decay_parameter Decay factor \eqn{\delta}{delta}. Default 0.5.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which is \code{TRUE} for tna input), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest path length considered,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_generalized_closeness}},
#'   \code{\link{centrality_dangalchev}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_decay(regulation_net)
centrality_decay <- function(x, mode = "all", decay_parameter = 0.5, ...) {
  df <- centrality(x, measures = "decay", mode = mode,
                   decay_parameter = decay_parameter, ...)
  col <- paste0("decay_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Residual Closeness Centrality
#'
#' Residual closeness (Dangalchev 2006) sums distances that decay by half
#' with every step:
#' \deqn{C(i) = \sum_{j} 2^{-d_{ij}},}{C(i) = sum_j 2^(-d_ij),}
#' with \eqn{2^{-\infty} = 0}{2^(-Inf) = 0}, so the score is defined on
#' disconnected networks.
#'
#' @details
#' The sum includes the node itself, which adds 1, and the values equal
#' \code{centiserve::closeness.residual()} on undirected networks. Edge
#' weights are read as distances; \code{weighted = FALSE} uses hop counts,
#' and \code{invert_weights = TRUE} converts a weight \eqn{w} to the distance
#' \eqn{1/w^\alpha}{1/w^alpha}. \code{mode = "all"} treats edges as
#' undirected, \code{"out"} uses distances from the node and \code{"in"}
#' distances to it. \code{\link{centrality_dangalchev}} returns the same
#' values.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which inverts for tna input only), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest distance counted,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Dangalchev, C. (2006). Residual closeness in networks. Physica A, 365(2),
#'   556-564. \doi{10.1016/j.physa.2005.12.020}.
#' @seealso \code{\link{centrality_dangalchev}},
#'   \code{\link{centrality_decay}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_residual_closeness(regulation_net)
centrality_residual_closeness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "residual_closeness", mode = mode, ...)
  col <- paste0("residual_closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Dangalchev Closeness
#'
#' Dangalchev closeness, the residual closeness of Dangalchev (2006), sums an
#' exponentially decaying function of the distance to every node:
#' \deqn{D(v) = \sum_{w} 2^{-d(v, w)}.}{D(v) = sum_w 2^(-d(v, w)).}
#' The sum includes the node itself, which adds one to every score, as in the
#' centiserve package. Unreachable nodes contribute 0.
#'
#' @details
#' Edge weights are read as path lengths. \code{invert_weights = TRUE} uses
#' \eqn{1/w^\alpha}{1/w^alpha} as the length, and \code{weighted = FALSE}
#' counts hops. \code{mode} sets the direction of the paths. The values equal
#' those of \code{\link{centrality_residual_closeness}} and of
#' \code{\link{centrality_decay}} with \code{decay_parameter = 0.5}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which is \code{TRUE} for tna input), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest path length considered,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Dangalchev, C. (2006). Residual closeness in networks. Physica A, 365(2),
#'   556-564. \doi{10.1016/j.physa.2005.12.020}.
#' @seealso \code{\link{centrality_residual_closeness}},
#'   \code{\link{centrality_decay}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dangalchev(regulation_net)
centrality_dangalchev <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "dangalchev", mode = mode, ...)
  col <- paste0("dangalchev_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Generalized Closeness
#'
#' Generalized closeness, as in the tidygraph package, sums a decay factor
#' \eqn{\alpha}{alpha} raised to the distance from the node to every node:
#' \deqn{GC(v) = \sum_{w} \alpha^{d(v, w)}.}{GC(v) = sum_w alpha^d(v, w).}
#' The sum includes the node itself, which adds one to every score, and
#' unreachable nodes contribute 0.
#'
#' @details
#' Edge weights are read as path lengths. \code{invert_weights = TRUE} uses
#' \eqn{1/w^\alpha}{1/w^alpha} as the length, with the inversion exponent
#' \code{alpha} of \code{\link{centrality}}, and \code{weighted = FALSE}
#' counts hops. \code{mode} sets the direction of the paths. The values equal
#' those of \code{\link{centrality_decay}} with the same
#' \code{decay_parameter}, which must lie strictly between 0 and 1; other
#' values raise a \code{cograph_bad_parameter} error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param decay_parameter Decay factor \eqn{\alpha}{alpha} of the formula.
#'   Default 0.5.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which is \code{TRUE} for tna input), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest path length considered,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_decay}},
#'   \code{\link{centrality_harmonic}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_generalized_closeness(regulation_net)
centrality_generalized_closeness <- function(x, mode = "all",
                                             decay_parameter = 0.5, ...) {
  df <- centrality(x, measures = "generalized_closeness", mode = mode,
                   decay_parameter = decay_parameter, ...)
  col <- paste0("generalized_closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Harary Centrality
#'
#' Harary centrality sums the inverse squared shortest-path distances from a
#' node to the other nodes:
#' \deqn{H(i) = \sum_{j \ne i} \frac{1}{d_{ij}^{2}},}{
#'   H(i) = sum_{j != i} 1 / d_ij^2,}
#' with \eqn{1/\infty = 0}{1/Inf = 0}, so an unreachable node contributes
#' nothing and the score is defined on disconnected networks.
#'
#' @details
#' Edge weights are read as distances. On \code{regulation_net}, whose
#' weights lie below one, the scores are therefore large.
#' \code{weighted = FALSE} uses hop counts, and \code{invert_weights = TRUE}
#' converts a weight \eqn{w} to the distance \eqn{1/w^\alpha}{1/w^alpha}.
#' \code{mode = "all"} treats edges as undirected, \code{"out"} uses
#' distances from the node and \code{"in"} distances to it. The Harary index
#' of Plavsic et al. (1993) sums the inverse distances \eqn{1/d_{ij}}{1/d_ij};
#' its node-level form is \code{\link{centrality_harmonic}}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which inverts for tna input only), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest distance counted,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Plavsic, D., Nikolic, S., Trinajstic, N., & Mihalic, Z. (1993). On the
#'   Harary index for the characterization of chemical graphs. Journal of
#'   Mathematical Chemistry, 12(1), 235-250. \doi{10.1007/BF01164638}.
#' @seealso \code{\link{centrality_harmonic}},
#'   \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_harary(regulation_net)
centrality_harary <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "harary", mode = mode, ...)
  col <- paste0("harary_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Average Distance
#'
#' Average distance is the sum of the shortest-path distances from a node to
#' every node, divided by \eqn{n + 1} as in the centiserve package:
#' \deqn{AD(v) = \frac{1}{n + 1} \sum_{w} d(v, w).}{
#'   AD(v) = sum_w d(v, w) / (n + 1).}
#' Lower values mark more central nodes.
#'
#' @details
#' Edge weights are read as path lengths. \code{invert_weights = TRUE} uses
#' \eqn{1/w^\alpha}{1/w^alpha} as the length, and \code{weighted = FALSE}
#' counts hops. \code{mode} sets the direction of the paths. A node that
#' cannot reach every other node scores \code{Inf}, so on a disconnected
#' network every score is \code{Inf}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which is \code{TRUE} for tna input), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest path length considered,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_barycenter}},
#'   \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_average_distance(regulation_net)
centrality_average_distance <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "average_distance", mode = mode, ...)
  col <- paste0("average_distance_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Barycenter Centrality
#'
#' Barycenter centrality is the reciprocal of the total shortest-path distance
#' from a node to the nodes it reaches:
#' \deqn{BC(v) = \frac{1}{\sum_{w \ne v} d(v, w)}.}{
#'   BC(v) = 1 / sum_{w != v} d(v, w).}
#' Unreachable nodes are left out of the sum.
#'
#' @details
#' Edge weights are read as path lengths. \code{invert_weights = TRUE} uses
#' \eqn{1/w^\alpha}{1/w^alpha} as the length, and \code{weighted = FALSE}
#' counts hops. \code{mode} sets the direction of the paths. The formula is
#' the one \code{\link{centrality_closeness}} computes, and with the default
#' \code{weighted = TRUE} the two agree on every node that reaches another
#' node. A node that reaches no other node scores 0, where closeness returns
#' \code{NaN}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which is \code{TRUE} for tna input), \code{alpha} (inversion
#'   exponent, default 1), \code{cutoff} (largest path length considered,
#'   default -1 for no limit) and \code{normalized} (divide by the maximum,
#'   default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_closeness}},
#'   \code{\link{centrality_average_distance}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_barycenter(regulation_net)
centrality_barycenter <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "barycenter", mode = mode, ...)
  col <- paste0("barycenter_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Wiener Index Centrality
#'
#' The Wiener centrality of a node is the sum of its shortest-path distances
#' to the nodes it reaches:
#' \deqn{W(i) = \sum_{j \ne i,\, d_{ij} < \infty} d_{ij}.}{
#'   W(i) = sum_{j != i, d_ij < Inf} d_ij.}
#' High values mark peripheral nodes. On a connected undirected network half
#' the sum of the scores is the Wiener index of the network (Wiener 1947).
#'
#' @details
#' Edge weights are read as distances. \code{weighted = FALSE} uses hop
#' counts, and \code{invert_weights = TRUE} converts a weight \eqn{w} to the
#' distance \eqn{1/w^\alpha}{1/w^alpha}. \code{mode = "all"} treats edges as
#' undirected, \code{"out"} uses distances from the node and \code{"in"}
#' distances to it. Unreachable nodes add nothing, so on a disconnected
#' network a node in a small component also scores low.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which inverts for tna input only), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest distance counted,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wiener, H. (1947). Structural determination of paraffin boiling points.
#'   Journal of the American Chemical Society, 69(1), 17-20.
#'   \doi{10.1021/ja01193a005}.
#' @seealso \code{\link{centrality_closeness}},
#'   \code{\link{centrality_average_distance}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_wiener(regulation_net)
centrality_wiener <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "wiener", mode = mode, ...)
  col <- paste0("wiener_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Closeness Vitality
#'
#' Closeness vitality (Koschuetzki et al. 2005) is the drop in the Wiener
#' index when a node is removed:
#' \deqn{CV(v) = W(G) - W(G - v), \qquad W(G) = \sum_{s \ne t} d(s, t).}{
#'   CV(v) = W(G) - W(G - v), W(G) = sum_{s != t} d(s, t).}
#' The Wiener index sums the finite distances over ordered pairs, as in
#' \code{networkx::closeness_vitality()}.
#'
#' @details
#' Edge weights are read as path lengths. \code{invert_weights = TRUE} uses
#' \eqn{1/w^\alpha}{1/w^alpha} as the length, and \code{weighted = FALSE}
#' counts hops. \code{mode} sets the direction of the paths. Pairs that are
#' not connected contribute nothing to either index. An isolated node scores
#' 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which is \code{TRUE} for tna input), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest path length considered,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Koschuetzki, D., Lehmann, K. A., Peeters, L., Richter, S., Tenfelde-Podehl,
#'   D., & Zlotowski, O. (2005). Centrality indices. In U. Brandes & T.
#'   Erlebach (Eds.), Network Analysis: Methodological Foundations (pp.
#'   16-61). Springer. \doi{10.1007/978-3-540-31955-9_3}.
#' @seealso \code{\link{centrality_wiener}}, \code{\link{centrality_centroid}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_closeness_vitality(regulation_net)
centrality_closeness_vitality <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "closeness_vitality", mode = mode, ...)
  col <- paste0("closeness_vitality_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Lobby Index
#'
#' The lobby index (Korn et al. 2009) is the h-index of the degrees in the
#' closed neighborhood of a node. It is the largest \eqn{k} such that the
#' node and its neighbors include at least \eqn{k} nodes of degree \eqn{k}
#' or more.
#'
#' @details
#' Edge weights are ignored. \code{mode} selects both the degree and the
#' neighbors, and with \code{mode = "all"} on a directed network the degree
#' is in plus out. An isolated node scores 0. On undirected networks the
#' values equal \code{centiserve::lobby()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named integer vector with one index per node, in input node
#'   order.
#' @references
#' Korn, A., Schubert, A., & Telcs, A. (2009). Lobby index in networks.
#'   Physica A, 388(11), 2221-2226. \doi{10.1016/j.physa.2009.02.013}.
#' @seealso \code{\link{centrality_degree}},
#'   \code{\link{centrality_coreness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_lobby(regulation_net)
centrality_lobby <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "lobby", mode = mode, ...)
  col <- paste0("lobby_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Entropy Centrality
#'
#' Entropy centrality, in the form of the centiserve package, removes the node
#' and measures how evenly reachability is spread over the remaining network.
#' With \eqn{r_j}{r_j} the number of nodes that node \eqn{j} reaches in
#' \eqn{G - v} and \eqn{P} half the total of the \eqn{r_j}{r_j},
#' \deqn{H(v) = -\sum_{j} y_j \log_2 y_j, \qquad y_j = \frac{r_j}{P}.}{
#'   H(v) = -sum_j y_j log2(y_j), y_j = r_j / P.}
#' Terms with \eqn{y_j = 0}{y_j = 0} are dropped.
#'
#' @details
#' Edge weights are ignored, and \code{mode} sets the direction of
#' reachability. When the network stays strongly connected after the removal
#' of any one node, every node scores \eqn{2 \log_2((n-1)/2)}{2 log2((n-1)/2)},
#' as on \code{regulation_net}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_distance_entropy}},
#'   \code{\link{centrality_diversity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_entropy(regulation_net)
centrality_entropy <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "entropy", mode = mode, ...)
  col <- paste0("entropy_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Semi-Local Centrality
#'
#' Semi-local centrality (Chen et al. 2012) sums, over the neighbors \eqn{u}
#' of a node and the neighbors \eqn{w} of each \eqn{u}, the number
#' \eqn{N(w)} of nodes within two steps of \eqn{w}:
#' \deqn{C_L(v) = \sum_{u \in \Gamma(v)} \sum_{w \in \Gamma(u)} N(w).}{
#'   C_L(v) = sum_{u in N(v)} sum_{w in N(u)} N(w).}
#'
#' @details
#' Edge weights are ignored. \code{mode} selects the neighbors, and with
#' \code{mode = "all"} on a directed network a reciprocated tie counts twice.
#' An isolated node scores 0. On undirected networks the values equal
#' \code{centiserve::semilocal()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Chen, D., Lu, L., Shang, M.-S., Zhang, Y.-C., & Zhou, T. (2012).
#'   Identifying influential nodes in complex networks. Physica A, 391(4),
#'   1777-1787. \doi{10.1016/j.physa.2011.09.017}.
#' @seealso \code{\link{centrality_laplacian}},
#'   \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_semilocal(regulation_net)
centrality_semilocal <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "semilocal", mode = mode, ...)
  col <- paste0("semilocal_", mode)
  stats::setNames(df[[col]], df$node)
}

#' ClusterRank
#'
#' ClusterRank (Chen et al. 2013) combines the local clustering coefficient
#' \eqn{c_v} of a node with the degrees of its neighbors:
#' \deqn{CR(v) = c_v \sum_{u \in N(v)} (k_u + 1).}{
#'   CR(v) = c_v sum_{u in N(v)} (k_u + 1).}
#'
#' @details
#' Edge weights are ignored. \code{mode} sets both the neighbor set and the
#' degrees, and on a directed network a reciprocated neighbor enters the sum
#' twice under \code{mode = "all"}. The clustering coefficient ignores edge
#' direction. A node with fewer than two neighbors has no clustering
#' coefficient and returns \code{NaN}. Chen et al. weight the sum by
#' \eqn{10^{-c_v}}{10^(-c_v)}, and the measure here multiplies by \eqn{c_v}
#' itself, as the centiserve package does.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Chen, D.-B., Gao, H., Lu, L., & Zhou, T. (2013). Identifying influential
#'   nodes in large-scale directed networks: The role of clustering. PLoS ONE,
#'   8(10), e77455. \doi{10.1371/journal.pone.0077455}.
#' @seealso \code{\link{centrality_transitivity}},
#'   \code{\link{centrality_expected}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_clusterrank(regulation_net)
centrality_clusterrank <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "clusterrank", mode = mode, ...)
  col <- paste0("clusterrank_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Bottleneck Centrality
#'
#' Bottleneck centrality (Przulj, Wigle and Jurisica 2004) counts the
#' shortest-path trees in which a node is a bottleneck. For each source
#' \eqn{s}, every shortest path from \eqn{s} to every reachable node is
#' enumerated, and a node \eqn{v \ne s}{v != s} scores one for that source
#' when it lies on more than \eqn{n/4} of these paths.
#'
#' @details
#' Distances are hop counts, so edge weights are ignored. \code{mode} sets the
#' direction in which the paths leave the source. Paths that end at \eqn{v}
#' count toward its total. A network with one node gives that node a score of
#' one.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named integer vector with one score per node, in input node
#'   order.
#' @references
#' Przulj, N., Wigle, D. A., & Jurisica, I. (2004). Functional topology in a
#'   network of protein interactions. Bioinformatics, 20(3), 340-348.
#'   \doi{10.1093/bioinformatics/btg415}.
#' @seealso \code{\link{centrality_stress}},
#'   \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_bottleneck(regulation_net)
centrality_bottleneck <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "bottleneck", mode = mode, ...)
  col <- paste0("bottleneck_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Centroid Value
#'
#' The centroid value (Koschuetzki et al. 2005) compares a node with every
#' other node by the number of nodes each one is closer to. With
#' \eqn{\gamma(v, u)}{gamma(v, u)} the number of nodes strictly closer to
#' \eqn{v} than to \eqn{u},
#' \deqn{CV(v) = \min_{u} \left[ \gamma(v, u) - \gamma(u, v) \right].}{
#'   CV(v) = min_u [gamma(v, u) - gamma(u, v)].}
#' The minimum includes \eqn{u = v}, so the score is at most 0, and values
#' closer to 0 mark more central nodes.
#'
#' @details
#' Edge weights are read as path lengths. \code{invert_weights = TRUE} uses
#' \eqn{1/w^\alpha}{1/w^alpha} as the length, and \code{weighted = FALSE}
#' counts hops. \code{mode} sets the direction of the paths.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which is \code{TRUE} for tna input), \code{alpha} (inversion
#'   exponent, default 1) and \code{cutoff} (largest path length considered,
#'   default -1 for no limit).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Koschuetzki, D., Lehmann, K. A., Peeters, L., Richter, S., Tenfelde-Podehl,
#'   D., & Zlotowski, O. (2005). Centrality indices. In U. Brandes & T.
#'   Erlebach (Eds.), Network Analysis: Methodological Foundations (pp.
#'   16-61). Springer. \doi{10.1007/978-3-540-31955-9_3}.
#' @seealso \code{\link{centrality_closeness}},
#'   \code{\link{centrality_closeness_vitality}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_centroid(regulation_net)
centrality_centroid <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "centroid", mode = mode, ...)
  col <- paste0("centroid_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Maximum Neighborhood Component
#'
#' The maximum neighborhood component (Lin et al. 2008) is the number of
#' nodes in the largest connected component of the subgraph induced by the
#' neighbors of a node.
#'
#' @details
#' Edge weights are ignored and the neighbor subgraph is read as undirected.
#' \code{mode} selects the neighbors. An isolated node scores 0. On
#' undirected networks the values equal \code{centiserve::mnc()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named integer vector with one size per node, in input node
#'   order.
#' @references
#' Lin, C.-Y., Chin, C.-H., Wu, H.-H., Chen, S.-H., Ho, C.-W., & Ko, M.-T.
#'   (2008). Hubba: hub objects analyzer, a framework of interactome hubs
#'   identification for network biology. Nucleic Acids Research, 36,
#'   W438-W443. \doi{10.1093/nar/gkn257}.
#' @seealso \code{\link{centrality_dmnc}}, \code{\link{centrality_lac}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mnc(regulation_net)
centrality_mnc <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "mnc", mode = mode, ...)
  col <- paste0("mnc_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Density of Maximum Neighborhood Component
#'
#' The density of maximum neighborhood component (Lin et al. 2008) looks at
#' the subnetwork induced by the neighbors of a node, the node itself left
#' out, and takes its largest connected component with \eqn{E} edges and
#' \eqn{N} nodes:
#' \deqn{DMNC(v) = \frac{E}{N^{\varepsilon}}.}{DMNC(v) = E / N^epsilon.}
#' A node without neighbors scores 0.
#'
#' @details
#' Edge weights are ignored, and \code{mode} sets the neighbor set. On an
#' undirected network the result follows this definition. On a directed
#' network the component is a strongly connected component of the induced
#' subnetwork, \eqn{E} counts its directed edges, and a reciprocated
#' neighbor enters the subnetwork once. When several components share the
#' largest size, \eqn{E} counts the edges among all of their nodes. A
#' \code{dmnc_epsilon} that is not a single positive finite number raises a
#' \code{cograph_bad_parameter} error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param dmnc_epsilon Exponent \eqn{\varepsilon}{epsilon}. Default 1.7, the
#'   value Lin et al. (2008) recommend. The centiserve package uses 1.67.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Lin, C.-Y., Chin, C.-H., Wu, H.-H., Chen, S.-H., Ho, C.-W., & Ko, M.-T.
#'   (2008). Hubba: Hub objects analyzer, a framework of interactome hubs
#'   identification for network biology. Nucleic Acids Research, 36(suppl 2),
#'   W438-W443. \doi{10.1093/nar/gkn257}.
#' @seealso \code{\link{centrality_mnc}}, \code{\link{centrality_mcc}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dmnc(regulation_net, directed = FALSE)
centrality_dmnc <- function(x, mode = "all", dmnc_epsilon = 1.7, ...) {
  df <- centrality(x, measures = "dmnc", mode = mode,
                   dmnc_epsilon = dmnc_epsilon, ...)
  col <- paste0("dmnc_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Local Average Connectivity
#'
#' Local average connectivity (Li et al. 2011) is the mean degree of the
#' neighbors of a node within the subgraph \eqn{C_v} induced by those
#' neighbors:
#' \deqn{LAC(v) = \frac{1}{k_v} \sum_{u \in N(v)} k_u^{C_v},}{
#'   LAC(v) = (1 / k_v) sum_{u in N(v)} k_u^(C_v),}
#' where \eqn{k_u^{C_v}}{k_u^(C_v)} is the degree of \eqn{u} inside
#' \eqn{C_v}. High values mark nodes whose neighbors are tied to each other.
#'
#' @details
#' Edge weights are ignored. \code{mode} selects the neighbors and the degree
#' counted inside \eqn{C_v}. With \code{mode = "all"} on a directed network
#' in-ties and out-ties are both counted, so a reciprocated tie counts twice.
#' An isolated node scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Li, M., Wang, J., Chen, X., Wang, H., & Pan, Y. (2011). A local average
#' connectivity-based method for identifying essential proteins from the network
#' level. \emph{Computational Biology and Chemistry}, 35(3), 143-150.
#' @seealso \code{\link{centrality_dmnc}}, \code{\link{centrality_mnc}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_lac(regulation_net)
centrality_lac <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "lac", mode = mode, ...)
  col <- paste0("lac_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Communicability Centrality
#'
#' Total communicability (Estrada and Hatano 2008; Benzi and Klymko 2013) sums
#' the walks of every length that start at a node, a walk of length \eqn{k}
#' weighted by \eqn{1/k!}:
#' \deqn{TC(v) = \sum_{w} \left[ e^{A} \right]_{vw}
#'   = \sum_{w} \sum_{k = 0}^{\infty} \frac{(A^k)_{vw}}{k!}.}{
#'   TC(v) = sum_w [exp(A)]_vw = sum_w sum_{k >= 0} (A^k)_vw / k!.}
#'
#' @details
#' \eqn{A} is the binary adjacency matrix, so edge weights are ignored. On an
#' undirected network the matrix exponential is formed from the
#' eigendecomposition of the symmetric \eqn{A}. On a directed network it is
#' computed by scaling and squaring with a Pade approximation (Moler and Van
#' Loan 2003), and the scores are the row sums of \eqn{e^{A}}{exp(A)}, the
#' walks that leave each node. \code{directed = FALSE} gives the undirected
#' reading.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Estrada, E., & Hatano, N. (2008). Communicability in complex networks.
#'   Physical Review E, 77(3), 036111. \doi{10.1103/PhysRevE.77.036111}.
#'
#' Benzi, M., & Klymko, C. (2013). Total communicability as a centrality
#'   measure. Journal of Complex Networks, 1(2), 124-149.
#'   \doi{10.1093/comnet/cnt007}.
#'
#' Moler, C., & Van Loan, C. (2003). Nineteen dubious ways to compute the
#'   exponential of a matrix, twenty-five years later. SIAM Review, 45(1),
#'   3-49. \doi{10.1137/S00361445024180}.
#' @seealso \code{\link{centrality_subgraph}},
#'   \code{\link{centrality_communicability_betweenness}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_communicability(regulation_net, directed = FALSE)
centrality_communicability <- function(x, ...) {
  df <- centrality(x, measures = "communicability", ...)
  stats::setNames(df$communicability, df$node)
}

#' Communicability Betweenness
#'
#' Communicability betweenness (Estrada, Higham and Hatano 2009) is the share
#' of the communicability between other pairs of nodes that is lost when the
#' node is removed. With \eqn{G = e^{A}}{G = exp(A)} and \eqn{G^{(r)}}{G^(r)}
#' the same exponential after the edges of \eqn{r} are deleted,
#' \deqn{\omega_r = \frac{1}{(n-1)(n-2)} \sum_{p \ne q,\; p, q \ne r}
#'   \frac{G_{pq} - G^{(r)}_{pq}}{G_{pq}}.}{
#'   omega_r = 1 / ((n-1)(n-2)) sum_{p != q; p, q != r}
#'   (G_pq - G^(r)_pq) / G_pq.}
#'
#' @details
#' \eqn{A} is the binary adjacency matrix, so edge weights are ignored. The
#' scores lie between 0 and 1. The measure is defined for undirected networks.
#' On a directed network \eqn{G}{G} and \eqn{G^{(r)}}{G^(r)} are computed by
#' scaling and squaring with a Pade approximation, as in
#' \code{\link{centrality_communicability}}, so the measure is defined there
#' as well. \code{directed = FALSE} gives the undirected reading.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Estrada, E., Higham, D. J., & Hatano, N. (2009). Communicability
#'   betweenness in complex networks. Physica A, 388(5), 764-774.
#'   \doi{10.1016/j.physa.2008.11.011}.
#' @seealso \code{\link{centrality_communicability}},
#'   \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_communicability_betweenness(regulation_net, directed = FALSE)
centrality_communicability_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "communicability_betweenness", ...)
  stats::setNames(df$communicability_betweenness, df$node)
}

#' Random Walk Centrality
#'
#' Random walk centrality is the inverse of the summed random-walk distances
#' from a node to the others:
#' \deqn{RW(i) = \left(\sum_{j} \frac{m_{ij} + m_{ji}}{2}\right)^{-1},}{
#'   RW(i) = 1 / sum_j (m_ij + m_ji) / 2,}
#' where \eqn{m_{ij}}{m_ij} is the mean first passage time from \eqn{i} to
#' \eqn{j} of a walk that moves to each out-neighbor with equal probability.
#'
#' @details
#' Edge weights are ignored. The measure is defined for connected undirected
#' and strongly connected directed networks. On any other network every
#' score is \code{NA}, with a \code{cograph_undefined_measure} warning,
#' because some passage times are infinite. The passage times are
#' symmetrized before
#' the sum, so the values differ from
#' \code{tidygraph::centrality_random_walk()}, which sums them unsymmetrized.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_markov}},
#'   \code{\link{centrality_current_flow_closeness}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_random_walk(regulation_net)
centrality_random_walk <- function(x, ...) {
  df <- centrality(x, measures = "random_walk", ...)
  stats::setNames(df$random_walk, df$node)
}

#' Stress Centrality
#'
#' Stress centrality (Shimbel 1953) counts the shortest paths between other
#' pairs of nodes that pass through a node:
#' \deqn{S(v) = \sum_{s \ne v \ne t} \sigma_{st}(v),}{
#'   S(v) = sum_{s != v != t} sigma_st(v),}
#' where \eqn{\sigma_{st}(v)}{sigma_st(v)} is the number of shortest paths
#' from \eqn{s} to \eqn{t} through \eqn{v}. Betweenness divides each count by
#' the number of shortest paths between the pair; stress keeps the counts.
#'
#' @details
#' Edge weights are read as distances. \code{weighted = FALSE} uses hop
#' counts, and \code{invert_weights = TRUE} converts a weight \eqn{w} to the
#' distance \eqn{1/w^\alpha}{1/w^alpha}. On a directed network the paths
#' follow edge direction, and on an undirected network each pair is counted
#' once. The values equal \code{sna::stresscent()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which inverts for tna input only) and \code{alpha}
#'   (inversion exponent, default 1).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Shimbel, A. (1953). Structural parameters of communication networks. The
#'   Bulletin of Mathematical Biophysics, 15(4), 501-507.
#'   \doi{10.1007/BF02476438}.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_load}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_stress(regulation_net)
centrality_stress <- function(x, ...) {
  df <- centrality(x, measures = "stress", ...)
  stats::setNames(df$stress, df$node)
}

#' Flow Betweenness
#'
#' Flow betweenness (Freeman, Borgatti and White 1991) sums, over pairs of
#' other nodes \eqn{s} and \eqn{t}, the flow that passes through the node
#' when a maximum flow is sent from \eqn{s} to \eqn{t} with the edge weights
#' as capacities.
#'
#' @details
#' The measure requires the igraph package and raises an error of class
#' \code{cograph_needs_igraph} without it. The flow through a node is read
#' from the maximum flow that \code{igraph::max_flow()} returns. On a
#' directed network the sum runs over ordered pairs along the edge direction,
#' and on an undirected network over unordered pairs. \code{weighted = FALSE}
#' gives every edge capacity one, and \code{invert_weights = TRUE} uses
#' \eqn{1/w^\alpha}{1/w^alpha} as the capacity.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}), \code{invert_weights} (default
#'   \code{NULL}, which is \code{TRUE} for tna input), \code{alpha} (inversion
#'   exponent, default 1) and \code{normalized} (default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Freeman, L. C., Borgatti, S. P., & White, D. R. (1991). Centrality in
#'   valued graphs: A measure of betweenness based on network flow. Social
#'   Networks, 13(2), 141-154. \doi{10.1016/0378-8733(91)90017-N}.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_current_flow_betweenness}},
#'   \code{\link{centrality}}.
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' centrality_flow_betweenness(regulation_net)
centrality_flow_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "flow_betweenness", ...)
  stats::setNames(df$flow_betweenness, df$node)
}

#' One-Step Expected Influence
#'
#' One-step expected influence (Robinaugh, Millner and McNally 2016) sums the
#' signed weights of a node's edges:
#' \deqn{EI_1(i) = \sum_{j} w_{ij}.}{EI1(i) = sum_j w_ij.}
#' Negative edges lower the score, which makes the measure suited to
#' partial-correlation and other signed networks.
#'
#' @details
#' \code{mode = "out"} (the default here) sums the outgoing weights,
#' \code{mode = "in"} the incoming weights and \code{mode = "all"} both, with
#' a self-loop counted once. On an undirected network the three modes
#' agree and each edge is counted once. With \code{weighted = FALSE}, or on
#' an unweighted input, the score is the degree in the chosen mode. When the
#' network has a negative edge, \code{normalized = TRUE} divides by the
#' largest absolute score and keeps the sign.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"out"} (default), \code{"in"} or
#'   \code{"all"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} and \code{psych_network}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Robinaugh, D. J., Millner, A. J., & McNally, R. J. (2016). Identifying
#'   highly influential nodes in the complicated grief network. Journal of
#'   Abnormal Psychology, 125(6), 747-757. \doi{10.1037/abn0000181}.
#' @seealso \code{\link{centrality_expected_influence_2}},
#'   \code{\link{centrality_strength}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_expected_influence_1(regulation_net)
centrality_expected_influence_1 <- function(x, mode = "out", ...) {
  df <- centrality(x, measures = "expected_influence_1", mode = mode, ...)
  stats::setNames(df[[paste0("expected_influence_1_", mode)]], df$node)
}

#' Two-Step Expected Influence
#'
#' Two-step expected influence (Robinaugh, Millner and McNally 2016) adds to
#' the one-step expected influence of a node the one-step expected influence
#' of its neighbors, each weighted by the signed edge between them:
#' \deqn{EI_2(i) = EI_1(i) + \sum_{j} w_{ij} EI_1(j).}{
#'   EI2(i) = EI1(i) + sum_j w_ij EI1(j).}
#'
#' @details
#' \code{mode = "out"} (the default here) follows outgoing edges at both
#' steps, \code{mode = "in"} incoming edges and \code{mode = "all"} both, with
#' a self-loop counted once. On an undirected network the three modes
#' agree and each edge is counted once. \code{weighted = FALSE} gives every
#' edge weight one. When the
#' network has a negative edge, \code{normalized = TRUE} divides by the
#' largest absolute score and keeps the sign.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"out"} (default), \code{"in"} or
#'   \code{"all"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} and \code{psych_network}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Robinaugh, D. J., Millner, A. J., & McNally, R. J. (2016). Identifying
#'   highly influential nodes in the complicated grief network. Journal of
#'   Abnormal Psychology, 125(6), 747-757. \doi{10.1037/abn0000181}.
#' @seealso \code{\link{centrality_expected_influence_1}},
#'   \code{\link{centrality_strength}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_expected_influence_2(regulation_net)
centrality_expected_influence_2 <- function(x, mode = "out", ...) {
  df <- centrality(x, measures = "expected_influence_2", mode = mode, ...)
  stats::setNames(df[[paste0("expected_influence_2_", mode)]], df$node)
}

#' Topological Coefficient
#'
#' The topological coefficient (Stelzl et al. 2005) measures how far a node
#' shares neighbors with the nodes it is linked to through a common neighbor:
#' \deqn{T(v) = \frac{\sum_{u \in U_v} J(v, u)}{|U_v|\, k_v},}{
#'   T(v) = sum_{u in U_v} J(v, u) / (|U_v| k_v),}
#' where \eqn{U_v} is the set of nodes that share at least one neighbor with
#' \eqn{v}, \eqn{J(v, u)} is the number of shared neighbors plus one when
#' \eqn{u} and \eqn{v} are linked, and \eqn{k_v} is the degree.
#'
#' @details
#' Edge weights and direction are ignored. A node that shares no neighbor
#' with any other node scores 0, as does an isolated node. On undirected
#' networks the values equal \code{centiserve::topocoefficient()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Stelzl, U., Worm, U., Lalowski, M., Haenig, C., Brembeck, F. H., Goehler,
#'   H., et al. (2005). A human protein-protein interaction network: A
#'   resource for annotating the proteome. Cell, 122(6), 957-968.
#'   \doi{10.1016/j.cell.2005.08.029}.
#' @seealso \code{\link{centrality_transitivity}},
#'   \code{\link{centrality_lac}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_topological_coefficient(regulation_net)
centrality_topological_coefficient <- function(x, ...) {
  df <- centrality(x, measures = "topological_coefficient", ...)
  stats::setNames(df$topological_coefficient, df$node)
}

#' Bridging Centrality
#'
#' Bridging centrality (Hwang et al. 2008) is the product of betweenness
#' \eqn{B(v)} and the bridging coefficient, which compares the inverse degree
#' of a node with the inverse degrees of its neighbors:
#' \deqn{BrC(v) = B(v) \frac{1/k_v}{\sum_{u \in N(v)} 1/k_u}.}{
#'   BrC(v) = B(v) (1/k_v) / sum_{u in N(v)} 1/k_u.}
#' High values mark nodes that lie on many shortest paths and connect densely
#' linked regions.
#'
#' @details
#' The betweenness factor reads edge weights as path lengths and follows the
#' edge direction of a directed network. \code{weighted = FALSE} uses hop
#' counts, and \code{invert_weights} has no effect. The
#' degrees are total degrees, and on a directed network a reciprocated
#' neighbor enters the sum twice. An isolated node scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Hwang, W., Kim, T., Ramanathan, M., & Zhang, A. (2008). Bridging
#'   centrality: Graph mining from element level to group level. In
#'   Proceedings of the 14th ACM SIGKDD International Conference on Knowledge
#'   Discovery and Data Mining (pp. 336-344). \doi{10.1145/1401890.1401934}.
#' @seealso \code{\link{centrality_local_bridging}},
#'   \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_bridging(regulation_net)
centrality_bridging <- function(x, ...) {
  df <- centrality(x, measures = "bridging", ...)
  stats::setNames(df$bridging, df$node)
}

#' Local Bridging Centrality
#'
#' Local bridging centrality multiplies the bridging coefficient of a node by
#' its inverse degree:
#' \deqn{LB(v) = \frac{1}{k_v} \cdot \frac{1/k_v}{\sum_{u \in N(v)} 1/k_u}.}{
#'   LB(v) = (1 / k_v) * (1 / k_v) / sum_{u in N(v)} (1 / k_u).}
#' A node of low degree whose neighbors have high degree scores high.
#'
#' @details
#' Edge weights are ignored. On a directed network \eqn{k} is the total
#' degree, in plus out, and a reciprocated tie counts twice. An isolated node
#' scores 0. The local bridging centrality of Nanda and Kotz, the product of
#' ego betweenness and the bridging coefficient, is
#' \code{\link{centrality_localized_bridging}}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_bridging}},
#'   \code{\link{centrality_localized_bridging}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_local_bridging(regulation_net)
centrality_local_bridging <- function(x, ...) {
  df <- centrality(x, measures = "local_bridging", ...)
  stats::setNames(df$local_bridging, df$node)
}

#' Effective Size
#'
#' Burt's effective size is the number of a node's contacts minus their
#' redundancy, the average number of ties each contact has to the other
#' contacts:
#' \deqn{ES(v) = k_v - \frac{1}{k_v} \sum_{j \in N(v)} |N(v) \cap N(j)|.}{
#'   ES(v) = k_v - (1 / k_v) sum_{j in N(v)} |N(v) & N(j)|.}
#' On an undirected network this is \eqn{k_v - 2 t_v / k_v}{k_v - 2 t_v / k_v},
#' with \eqn{t_v}{t_v} the number of ties among the contacts.
#'
#' @details
#' Edge weights are ignored. On a directed network each reciprocated neighbor
#' enters the neighbor list twice, so \eqn{k_v}{k_v} counts it twice and the
#' result differs from that of the undirected skeleton. An isolated node
#' scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_constraint}},
#'   \code{\link{centrality_redundancy}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_effective_size(regulation_net)
centrality_effective_size <- function(x, ...) {
  df <- centrality(x, measures = "effective_size", ...)
  stats::setNames(df$effective_size, df$node)
}

#' Diversity Centrality
#'
#' Diversity centrality (Eagle, Macy and Claxton 2010) is the Shannon entropy
#' of the weights on the edges incident to a node, divided by its maximum
#' \eqn{\log_2 k_v}{log2 k_v}:
#' \deqn{D(v) = -\frac{\sum_{j} p_{vj} \log_2 p_{vj}}{\log_2 k_v}, \qquad
#'   p_{vj} = \frac{|w_{vj}|}{\sum_{l} |w_{vl}|}.}{
#'   D(v) = -sum_j p_vj log2(p_vj) / log2(k_v),
#'   p_vj = |w_vj| / sum_l |w_vl|.}
#' The score lies between 0 and 1 and reaches 1 when the weights are equal.
#'
#' @details
#' On a directed network incoming and outgoing edges are separate entries,
#' and \eqn{k_v}{k_v} counts both. With \code{weighted = FALSE}, or on an
#' unweighted input, every node with two or more edges scores 1. A node with fewer than two edges, or
#' with edge weights summing to zero, scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Eagle, N., Macy, M., & Claxton, R. (2010). Network diversity and economic
#'   development. Science, 328(5981), 1029-1031.
#'   \doi{10.1126/science.1186605}.
#' @seealso \code{\link{centrality_entropy}},
#'   \code{\link{centrality_strength}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_diversity(regulation_net)
centrality_diversity <- function(x, ...) {
  df <- centrality(x, measures = "diversity", ...)
  stats::setNames(df$diversity, df$node)
}

#' Cross-Clique Connectivity
#'
#' Cross-clique connectivity (Faghani and Nguyen 2013) counts the cliques that
#' contain a node. Every complete subnetwork counts, including the node
#' itself and each of its edges, so a node of an isolated triangle scores 4.
#'
#' @details
#' Edge direction, edge weights and self-loops are ignored.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named integer vector with one count per node, in input node
#'   order.
#' @references
#' Faghani, M. R., & Nguyen, U. T. (2013). A study of XSS worm propagation and
#'   detection mechanisms in online social networks. IEEE Transactions on
#'   Information Forensics and Security, 8(11), 1815-1826.
#'   \doi{10.1109/TIFS.2013.2280884}.
#' @seealso \code{\link{centrality_coreness}},
#'   \code{\link{centrality_transitivity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_cross_clique(regulation_net)
centrality_cross_clique <- function(x, ...) {
  df <- centrality(x, measures = "cross_clique", ...)
  stats::setNames(df$cross_clique, df$node)
}

#' Markov Centrality
#'
#' Markov centrality (White and Smyth 2003) is the inverse of the mean first
#' passage time of a random walk into a node:
#' \deqn{M(j) = \left(\frac{1}{n} \sum_{i} m_{ij}\right)^{-1},}{
#'   M(j) = 1 / ((1 / n) sum_i m_ij),}
#' where \eqn{m_{ij}}{m_ij} is the expected number of steps from \eqn{i} to
#' the first visit of \eqn{j} and \eqn{m_{jj} = 0}{m_jj = 0}.
#'
#' @details
#' The walk moves from a node to each of its out-neighbors with equal
#' probability, so edge weights are ignored. A disconnected network, or a
#' directed network with a node that has no outgoing edge, gives \code{NA}
#' for every node with a \code{cograph_undefined_measure} warning. On a
#' directed network that is not strongly connected, a node that some other
#' node cannot reach has an infinite mean passage time, and its score is
#' \code{NA} with the same warning. On connected undirected and strongly
#' connected directed networks the values equal
#' \code{centiserve::markovcent()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' White, S., & Smyth, P. (2003). Algorithms for estimating relative
#'   importance in networks. In Proceedings of the Ninth ACM SIGKDD
#'   International Conference on Knowledge Discovery and Data Mining
#'   (pp. 266-275). \doi{10.1145/956750.956782}.
#' @seealso \code{\link{centrality_random_walk}},
#'   \code{\link{centrality_pagerank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_markov(regulation_net)
centrality_markov <- function(x, ...) {
  df <- centrality(x, measures = "markov", ...)
  stats::setNames(df$markov, df$node)
}

#' Integration Centrality
#'
#' Integration centrality (Valente and Foreman 1998) scores each distance
#' against the diameter \eqn{D}, the largest finite hop distance, and sums:
#' \deqn{I(i) = \sum_{j} \left(1 - \frac{d_{ij} - 1}{D}\right),}{
#'   I(i) = sum_j (1 - (d_ij - 1) / D),}
#' where an unreachable node contributes 0.
#'
#' @details
#' Distances are hop counts, so edge weights are ignored. The sum includes
#' the node itself, which contributes \eqn{1 + 1/D}, and is not divided by
#' \eqn{n - 1}; the values equal \code{tidygraph::centrality_integration()}.
#' \code{mode = "all"} treats edges as undirected, \code{"out"} uses
#' distances from the node and \code{"in"} distances to it. On a network
#' without edges every node scores \eqn{n}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Valente, T. W., & Foreman, R. K. (1998). Integration and radiality:
#'   Measuring the extent of an individual's connectedness and reachability
#'   in a network. Social Networks, 20(1), 89-105.
#'   \doi{10.1016/S0378-8733(97)00007-5}.
#' @seealso \code{\link{centrality_radiality}},
#'   \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_integration(regulation_net)
centrality_integration <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "integration", mode = mode, ...)
  col <- paste0("integration_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Expected Centrality
#'
#' Expected centrality is the sum of the degrees of a node's neighbors:
#' \deqn{E(v) = \sum_{u \in N(v)} k_u.}{E(v) = sum_{u in N(v)} k_u.}
#'
#' @details
#' Edge weights are ignored. \code{mode} sets both the degrees and the
#' neighbor set. On a directed network \code{mode = "all"} uses total degrees
#' and the undirected neighbor set. Adding the node's own degree gives the
#' \code{"kandhway_kuri"} form of \code{\link{centrality_diffusion}}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_diffusion}},
#'   \code{\link{centrality_neighborhood_connectivity}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_expected(regulation_net)
centrality_expected <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "expected", mode = mode, ...)
  col <- paste0("expected_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Gil-Schmidt Power Index
#'
#' The Gil-Schmidt power index sums the reciprocal hop distances from a node
#' to the nodes it reaches and divides by \eqn{n - 1}:
#' \deqn{GS(v) = \frac{1}{n - 1} \sum_{w \ne v} \frac{1}{d(v, w)}.}{
#'   GS(v) = sum_{w != v} 1 / d(v, w) / (n - 1).}
#' Unreachable nodes contribute 0, so the score lies between 0 and 1, and a
#' node adjacent to every other node scores 1.
#'
#' @details
#' Distances are hop counts, so edge weights are ignored and
#' \code{invert_weights} has no effect. \code{mode} sets the direction of the
#' paths. With \code{mode = "out"} the values match
#' \code{sna::gilschmidt()} with its default settings.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @seealso \code{\link{centrality_harmonic}},
#'   \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_gilschmidt(regulation_net)
centrality_gilschmidt <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "gilschmidt", mode = mode, ...)
  col <- paste0("gilschmidt_", mode)
  stats::setNames(df[[col]], df$node)
}

#' SALSA Authority Centrality
#'
#' SALSA (Lempel and Moran 2000) ranks authorities by a random walk that
#' alternates between following an edge backward and forward. The authority
#' score of a node is its entry in the stationary distribution of the chain
#' \deqn{\tilde{A} = W_c^{T} W_r,}{A~ = t(W_c) W_r,}
#' where \eqn{W_r} and \eqn{W_c} are the row-normalized and column-normalized
#' adjacency matrices.
#'
#' @details
#' The measure needs a directed network. On undirected input every score is
#' \code{NA} with a \code{cograph_undefined_measure} warning. Edge weights are
#' ignored. The scores are scaled so that the largest is 1, and a node
#' without incoming edges scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Lempel, R., & Moran, S. (2000). The stochastic approach for link-structure
#'   analysis (SALSA) and the TKC effect. Computer Networks, 33(1-6),
#'   387-401. \doi{10.1016/S1389-1286(00)00034-7}.
#' @seealso \code{\link{centrality_authority}},
#'   \code{\link{centrality_pagerank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_salsa(regulation_net)
centrality_salsa <- function(x, ...) {
  df <- centrality(x, measures = "salsa", ...)
  stats::setNames(df$salsa, df$node)
}

#' LeaderRank Centrality
#'
#' LeaderRank (Lu et al. 2011) adds a ground node joined in both directions
#' to every node and runs a random walk without damping on the extended
#' network. The final score of the ground node is shared equally among the
#' other nodes.
#'
#' @details
#' The measure needs a directed network. On undirected input every score is
#' \code{NA} with a \code{cograph_undefined_measure} warning. Edge weights are
#' ignored; \code{\link{centrality_weighted_leaderrank}} uses them. The walk
#' starts with one unit at every node and none at the ground node, so the
#' scores sum to \eqn{n}. The values equal \code{centiserve::leaderrank()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Lu, L., Zhang, Y.-C., Yeung, C. H., & Zhou, T. (2011). Leaders in social
#'   networks, the Delicious case. PLoS ONE, 6(6), e21202.
#'   \doi{10.1371/journal.pone.0021202}.
#' @seealso \code{\link{centrality_weighted_leaderrank}},
#'   \code{\link{centrality_pagerank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_leaderrank(regulation_net)
centrality_leaderrank <- function(x, ...) {
  df <- centrality(x, measures = "leaderrank", ...)
  stats::setNames(df$leaderrank, df$node)
}

#' Participation Coefficient
#'
#' The participation coefficient (Guimera and Nunes Amaral 2005) measures
#' how evenly the ties of a node spread over communities:
#' \deqn{P_i = 1 - \sum_{s} \left(\frac{k_{is}}{k_i}\right)^2,}{
#'   P_i = 1 - sum_s (k_is / k_i)^2,}
#' where \eqn{k_{is}}{k_is} counts the ties of node \eqn{i} to community
#' \eqn{s} and \eqn{k_i} is its degree.
#'
#' @details
#' Edge weights are ignored. \code{mode} selects the ties counted, and with
#' \code{mode = "all"} on a directed network a reciprocated tie counts twice.
#' A node whose ties all stay in one community scores 0, as does an isolated
#' node, and the score is below 1. Without \code{membership} every score is
#' \code{NA} with a warning of classes \code{cograph_bad_membership} and
#' \code{cograph_undefined_measure}, and a \code{membership} of the wrong
#' length raises a \code{cograph_bad_membership} error. On undirected
#' networks the values equal \code{brainGraph::part_coeff()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Community of each node, a vector with one entry per
#'   node in input node order (default \code{NULL}).
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Guimera, R., & Nunes Amaral, L. A. (2005). Functional cartography of
#'   complex metabolic networks. Nature, 433(7028), 895-900.
#'   \doi{10.1038/nature03288}.
#' @seealso \code{\link{centrality_within_module_z}},
#'   \code{\link{centrality_gateway}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_participation(regulation_net, membership = rep(1:2, each = 5))
centrality_participation <- function(x, membership = NULL, mode = "all", ...) {
  df <- centrality(x, measures = "participation", mode = mode,
                   membership = membership, ...)
  col <- paste0("participation_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Within-Module Degree Z-Score
#'
#' The within-module degree z-score (Guimera and Nunes Amaral 2005)
#' standardizes the number of ties a node has inside its own community
#' against the other members of that community:
#' \deqn{z_i = \frac{\kappa_i - \bar{\kappa}_{s_i}}{\sigma_{\kappa_{s_i}}},}{
#'   z_i = (kappa_i - mean(kappa_s)) / sd(kappa_s),}
#' where \eqn{\kappa_i}{kappa_i} counts the ties of \eqn{i} to its community
#' \eqn{s_i}. High values mark hubs within their community.
#'
#' @details
#' Edge weights are ignored. \code{mode} selects the ties counted, and with
#' \code{mode = "all"} on a directed network a reciprocated tie counts twice.
#' The standard deviation is the sample value. A community with one member,
#' or whose members all have the same within-community degree, gives
#' \code{NaN}. Without \code{membership} every score is \code{NA} with a
#' warning of classes \code{cograph_bad_membership} and
#' \code{cograph_undefined_measure}, and a \code{membership} of the wrong
#' length raises a \code{cograph_bad_membership} error. On undirected
#' networks the values equal
#' \code{brainGraph::within_module_deg_z_score()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Community of each node, a vector with one entry per
#'   node in input node order (default \code{NULL}).
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Guimera, R., & Nunes Amaral, L. A. (2005). Functional cartography of
#'   complex metabolic networks. Nature, 433(7028), 895-900.
#'   \doi{10.1038/nature03288}.
#' @seealso \code{\link{centrality_participation}},
#'   \code{\link{centrality_gateway}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_within_module_z(regulation_net, membership = rep(1:2, each = 5))
centrality_within_module_z <- function(x, membership = NULL, mode = "all", ...) {
  df <- centrality(x, measures = "within_module_z", mode = mode,
                   membership = membership, ...)
  col <- paste0("within_module_z_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Gateway Coefficient
#'
#' The gateway coefficient (Ruiz Vargas and Wahl 2014) refines the
#' participation coefficient by weighting the links of node \eqn{i} into each
#' module \eqn{s} by how much of the connection between the two modules they
#' carry and by the degree of the neighbors they reach:
#' \deqn{G_i = 1 - \frac{1}{k_i^2} \sum_{s} k_{is}^2 \, g_{is}^2,}{
#'   G_i = 1 - (1 / k_i^2) sum_s k_is^2 g_is^2,}
#' where \eqn{k_{is}}{k_is} is the number of links of \eqn{i} into module
#' \eqn{s} and \eqn{g_{is}}{g_is} lies between 0 and 1.
#'
#' @details
#' Edge weights are ignored, and the score lies between 0 and 1. On a
#' directed network \code{mode} chooses the ties: \code{"out"} uses outgoing
#' links, \code{"in"} incoming links and \code{"all"} (default) both, with a
#' reciprocated tie counted twice. The degree \eqn{k_i}{k_i}, the module
#' links \eqn{k_{is}}{k_is} and the neighbors whose degrees enter
#' \eqn{g_{is}}{g_is} all use the same ties. On an undirected network the
#' three modes agree and equal \code{brainGraph::gateway_coeff(centr =
#' "degree")}. \code{membership} can hold integer, character or factor
#' labels. Without \code{membership} the function returns \code{NA} with a
#' warning of classes \code{cograph_bad_membership} and
#' \code{cograph_undefined_measure}, and a \code{membership} of the wrong
#' length raises a \code{cograph_bad_membership} error. With a single module
#' every node scores 0, and so does a node without links in the chosen mode.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Module labels, one per node: integer codes, character
#'   labels or a factor.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"} or
#'   \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{directed} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ruiz Vargas, E., & Wahl, L. M. (2014). The gateway coefficient: A novel
#'   metric for identifying critical connections in modular networks. The
#'   European Physical Journal B, 87(7), 161.
#'   \doi{10.1140/epjb/e2014-40800-7}.
#' @seealso \code{\link{centrality_participation}},
#'   \code{\link{centrality_within_module_z}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_gateway(regulation_net, membership = rep(1:2, each = 5))
centrality_gateway <- function(x, membership = NULL, mode = "all", ...) {
  df <- centrality(x, measures = "gateway", mode = mode,
                   membership = membership, ...)
  col <- paste0("gateway_", mode)
  stats::setNames(df[[col]], df$node)
}


# ---------------------------------------------------------------------------
# Batch 3 wrappers: classical measures validated against centiserve / sna /
# igraph / NetworkX reference implementations.
# ---------------------------------------------------------------------------

#' Katz Centrality
#'
#' Katz (1953) status sums the walks of every length that end at a node,
#' each step attenuated by \eqn{\alpha}{alpha}:
#' \deqn{c = (I - \alpha A^{T})^{-1}\mathbf{1},}{c = (I - alpha t(A))^(-1) 1,}
#' where \eqn{A} is the weighted adjacency matrix and \eqn{\alpha}{alpha} is
#' \code{katz_alpha}.
#'
#' @details
#' The series converges for \eqn{\alpha < 1/\rho(A)}{alpha < 1/rho(A)}, where
#' \eqn{\rho(A)}{rho(A)} is the spectral radius. A divergent series is
#' detected from scores below one and raises a \code{cograph_katz_diverged}
#' warning that names the bound; the returned values are then not Katz
#' scores. \code{weighted = FALSE} gives every edge weight one. On a
#' directed network the score counts walks that arrive at the node. A
#' network of one node without a self-loop scores 1. The values equal \code{igraph::alpha_centrality(exo = 1)} with the
#' same \code{alpha}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param katz_alpha Attenuation factor \eqn{\alpha}{alpha} (default 0.1).
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Katz, L. (1953). A new status index derived from sociometric analysis.
#' \emph{Psychometrika}, 18(1), 39-43.
#' @seealso \code{\link{centrality_alpha}},
#'   \code{\link{centrality_eigenvector}}, \code{\link{centrality_hubbell}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_katz(regulation_net)
centrality_katz <- function(x, katz_alpha = 0.1, ...) {
  df <- centrality(x, measures = "katz", katz_alpha = katz_alpha, ...)
  stats::setNames(df$katz, df$node)
}


#' Hubbell Centrality
#'
#' Hubbell (1965) centrality solves an input-output system in which the
#' score of a node is one plus the attenuated scores of the nodes it sends
#' ties to:
#' \deqn{c = (I - wW)^{-1}\mathbf{1},}{c = (I - w W)^(-1) 1,}
#' where \eqn{W} is the weighted adjacency matrix and \eqn{w} is
#' \code{hubbell_weight}.
#'
#' @details
#' The system is solvable when the spectral radius of \eqn{wW} is below one.
#' Otherwise every score is \code{NA} with a \code{cograph_undefined_measure}
#' warning. A \code{hubbell_weight} of zero or below raises a
#' \code{cograph_bad_parameter} error. \code{weighted = FALSE} gives every
#' edge weight one.
#' The rows of \eqn{W} are outgoing ties, so on a directed network the score
#' sums attenuated walks that leave the node. \code{centiserve::hubbell()}
#' with \code{weights = NULL} sets every weight to 1, so it reproduces these
#' values only when the weights are passed explicitly.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param hubbell_weight Attenuation factor \eqn{w}, a positive number
#'   (default 0.5).
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Hubbell, C. H. (1965). An input-output approach to clique identification.
#' \emph{Sociometry}, 28(4), 377-399.
#' @seealso \code{\link{centrality_katz}}, \code{\link{centrality_power}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_hubbell(regulation_net)
centrality_hubbell <- function(x, hubbell_weight = 0.5, ...) {
  df <- centrality(x, measures = "hubbell", hubbell_weight = hubbell_weight, ...)
  stats::setNames(df$hubbell, df$node)
}


#' Information Centrality
#'
#' Information centrality (Stephenson and Zelen 1989) measures the
#' information carried by all paths between a node and the others, each path
#' weighted by its length. With \eqn{C = B^{-1}}{C = B^(-1)}, where \eqn{B}
#' has diagonal \eqn{1 + s_i}{1 + s_i} (\eqn{s_i} the strength) and
#' off-diagonal entries \eqn{1 - w_{ij}}{1 - w_ij},
#' \deqn{I_i = \frac{1}{C_{ii} + (T - 2R_i)/n},}{
#'   I_i = 1 / (C_ii + (T - 2 R_i) / n),}
#' where \eqn{T} is the trace of \eqn{C} and \eqn{R_i} the sum of row
#' \eqn{i}.
#'
#' @details
#' The network is symmetrized with \eqn{(w_{ij} + w_{ji})/2}{(w_ij + w_ji)/2},
#' so direction is ignored. Edge weights enter as tie strengths, and
#' \code{weighted = FALSE} uses the binary matrix. Isolated nodes score 0 and
#' are left out of \eqn{n}. When the other nodes do not form one connected
#' component, or \eqn{B} is singular, every score is \code{NA} with a
#' \code{cograph_undefined_measure} warning. On unweighted
#' undirected networks the values equal \code{sna::infocent()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (default \code{TRUE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Stephenson, K., & Zelen, M. (1989). Rethinking centrality: Methods and
#' examples. \emph{Social Networks}, 11(1), 1-37.
#' @seealso \code{\link{centrality_current_flow_closeness}},
#'   \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_information(regulation_net)
centrality_information <- function(x, ...) {
  df <- centrality(x, measures = "information", ...)
  stats::setNames(df$information, df$node)
}


#' Pairwise Disconnectivity
#'
#' Pairwise disconnectivity (Potapov et al. 2008) is the share of ordered
#' reachable pairs that become unreachable when a node is removed:
#' \deqn{PD(v) = \frac{|P(G)| - |P(G - v)|}{|P(G)|},}{
#'   PD(v) = (|P(G)| - |P(G - v)|) / |P(G)|,}
#' where \eqn{|P(G)|} is the number of ordered pairs \eqn{(s, t)},
#' \eqn{s \ne t}{s != t}, with a directed path from \eqn{s} to \eqn{t}.
#'
#' @details
#' The measure needs a directed network. On undirected input every score is
#' \code{NA} with a \code{cograph_undefined_measure} warning. Reachability
#' uses hop counts, so edge weights are ignored. The score lies between 0
#' and 1, and a network without reachable pairs scores 0 everywhere. The
#' values equal \code{centiserve::pairwisedis()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Potapov, A. P., Goemann, B., & Wingender, E. (2008). The pairwise
#'   disconnectivity index as a new metric for the topological analysis of
#'   regulatory networks. \emph{BMC Bioinformatics}, 9, 227.
#'   \doi{10.1186/1471-2105-9-227}.
#' @seealso \code{\link{centrality_prestige_domain}}, \code{\link{robustness}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_pairwisedis(regulation_net)
centrality_pairwisedis <- function(x, ...) {
  df <- centrality(x, measures = "pairwisedis", ...)
  stats::setNames(df$pairwisedis, df$node)
}


#' Local Reaching Centrality
#'
#' Local reaching centrality (Mones et al. 2012) measures how much of the
#' network a node reaches. On an unweighted directed network it is the share
#' of the other nodes reachable from the node. On an unweighted undirected
#' network it is the mean inverse distance to the other nodes, harmonic
#' centrality divided by \eqn{n - 1}.
#'
#' @details
#' A network counts as weighted unless every weight is 1, and
#' \code{weighted = FALSE} gives the unweighted form. In the weighted form
#' a shortest path uses the edge lengths \eqn{W/w_e}{W / w_e}, with \eqn{W} the total
#' edge weight, and each reached node contributes the mean edge weight along
#' its path; the sum is divided by \eqn{n - 1}. With \code{mode = "out"} the
#' weighted values equal \code{networkx.local_reaching_centrality()} with
#' \code{normalized = False}. \code{mode = "all"} treats edges as undirected,
#' and \code{"in"} counts the nodes that reach the node. A negative weight
#' raises an error. \code{\link{reaching_global}} is the network-level
#' hierarchy measure built from these scores.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for complex
#' networks. \emph{PLoS ONE}, 7(3), e33799.
#' @seealso \code{\link{reaching_global}}, \code{\link{centrality_harmonic}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_reaching_local(regulation_net)
centrality_reaching_local <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "reaching_local", mode = mode, ...)
  col <- paste0("reaching_local_", mode)
  stats::setNames(df[[col]], df$node)
}


#' Global Reaching Centrality (Mones, Vicsek & Vicsek 2012)
#'
#' A graph-level hierarchy measure computed from per-node local reaching
#' centralities:
#' \deqn{GRC(G) = \frac{1}{N - 1} \sum_v \left( \max_u LRC(u) - LRC(v) \right)}{GRC(G) = 1/(N - 1) sum_v ( max_u LRC(u) - LRC(v) )}
#'
#' Values close to 0 indicate a flat network in which all nodes reach equal
#' proportions of the graph. Larger values indicate a more hierarchical
#' structure. The result matches \code{networkx.global_reaching_centrality}.
#'
#' @param x Network input (matrix, edge-list data frame, igraph, network,
#'   cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality_reaching_local}}.
#'
#' @return A single numeric value. On an unweighted graph it lies in
#'   \eqn{[0, 1]}. On a weighted graph the local reaching centralities scale
#'   with the edge weights, so the value is unbounded. A graph with at most
#'   one node returns 0.
#'
#' @seealso \code{\link{centrality_reaching_local}}, \code{\link{summarize_network}}.
#' @references
#' Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for complex
#' networks. \emph{PLoS ONE}, 7(3), e33799.
#'
#' @export
#' @examples
#' reaching_global(regulation_net, mode = "out")
reaching_global <- function(x, mode = "all", ...) {
  lrc <- centrality_reaching_local(x, mode = mode, ...)
  n <- length(lrc)
  if (n <= 1) return(0)
  max_lrc <- max(lrc, na.rm = TRUE)
  sum(max_lrc - lrc, na.rm = TRUE) / (n - 1)
}


# ---------------------------------------------------------------------------
# Batch 4 wrappers: directed prestige family (Wasserman-Faust / sna lineage).
# ---------------------------------------------------------------------------

#' Domain Prestige
#'
#' Domain prestige (Wasserman and Faust 1994) counts the other nodes that
#' reach a node through a directed path:
#' \deqn{D(v) = |\{u \ne v : u \to^{*} v\}|.}{
#'   D(v) = number of nodes u != v with a directed path to v.}
#'
#' @details
#' The measure needs a directed network. On undirected input every score is
#' \code{NA} with a \code{cograph_undefined_measure} warning. Edge weights are
#' ignored. The score is a whole number between 0 and \eqn{n - 1}. The values
#' equal \code{sna::prestige(cmode = "domain")}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wasserman, S., & Faust, K. (1994). \emph{Social Network Analysis: Methods
#' and Applications}. Cambridge University Press.
#' @seealso \code{\link{centrality_prestige_domain_proximity}},
#'   \code{\link{centrality_reaching_local}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_prestige_domain(regulation_net)
centrality_prestige_domain <- function(x, ...) {
  df <- centrality(x, measures = "prestige_domain", ...)
  stats::setNames(df$prestige_domain, df$node)
}


#' Domain Proximity Prestige
#'
#' Domain proximity prestige (Wasserman and Faust 1994) combines the number
#' of nodes that reach a node with their distance to it:
#' \deqn{PD(v) = \frac{R_v^2}{(n - 1)\,D_v},}{PD(v) = R_v^2 / ((n - 1) D_v),}
#' where \eqn{R_v} is the number of other nodes with a directed path to
#' \eqn{v} and \eqn{D_v} the sum of their hop distances to \eqn{v}.
#'
#' @details
#' The measure needs a directed network. On undirected input every score is
#' \code{NA} with a \code{cograph_undefined_measure} warning. Edge weights are
#' ignored. A node that no other node reaches scores 0, and the score lies
#' between 0 and 1. On strongly connected networks the values equal
#' \code{sna::prestige(cmode = "domain.proximity")}. On other networks sna
#' sets some scores to 0, because its sum multiplies an infinite distance by
#' zero; cograph sums the finite distances only.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wasserman, S., & Faust, K. (1994). \emph{Social Network Analysis: Methods
#' and Applications}. Cambridge University Press.
#' @seealso \code{\link{centrality_prestige_domain}},
#'   \code{\link{centrality_reaching_local}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_prestige_domain_proximity(regulation_net)
centrality_prestige_domain_proximity <- function(x, ...) {
  df <- centrality(x, measures = "prestige_domain_proximity", ...)
  stats::setNames(df$prestige_domain_proximity, df$node)
}


# ---------------------------------------------------------------------------
# Batch 5 wrappers: Gould-Fernandez brokerage (5 roles).
# ---------------------------------------------------------------------------

#' Coordinator Brokerage
#'
#' Coordinator brokerage (Gould and Fernandez 1989) counts the open two-paths
#' \eqn{a \to v \to c}{a -> v -> c} through node \eqn{v} in which \eqn{a},
#' \eqn{v} and \eqn{c} all belong to the same group. A two-path is open when
#' the network has no edge from \eqn{a} to \eqn{c}. This role is \eqn{w_I}{w_I}
#' in the notation of the source.
#'
#' @details
#' The measure is defined for directed networks. On an undirected network it
#' returns \code{NA} with a \code{cograph_undefined_measure} warning, and the
#' same happens when \code{membership} is missing, where the warning also has
#' class \code{cograph_bad_membership}. A \code{membership} whose length
#' differs from the number of nodes raises a \code{cograph_bad_membership}
#' error. Group labels may be numbers
#' or strings. Edge weights and self-loops are ignored. The other four roles
#' are \code{\link{centrality_brokerage_itinerant}},
#' \code{\link{centrality_brokerage_representative}},
#' \code{\link{centrality_brokerage_gatekeeper}} and
#' \code{\link{centrality_brokerage_liaison}}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Group labels, one per node.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named integer vector with one count per node, in input node
#'   order. \code{normalized = TRUE} returns a numeric vector.
#' @references
#' Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A formal
#'   approach to brokerage in transaction networks. Sociological Methodology,
#'   19, 89-126. \doi{10.2307/270949}.
#' @seealso \code{\link{centrality_brokerage_gatekeeper}},
#'   \code{\link{centrality_gateway}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_brokerage_coordinator(regulation_net, membership = rep(1:2, each = 5))
centrality_brokerage_coordinator <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_coordinator",
                   membership = membership, ...)
  stats::setNames(df$brokerage_coordinator, df$node)
}

#' Itinerant Brokerage
#'
#' Itinerant brokerage (Gould and Fernandez 1989), also called the consultant
#' role, counts the open two-paths \eqn{a \to v \to c}{a -> v -> c} through
#' node \eqn{v} in which \eqn{a} and \eqn{c} belong to the same group and
#' \eqn{v} to another group. A two-path is open when the network has no edge
#' from \eqn{a} to \eqn{c}. This role is \eqn{w_O}{w_O} in the notation of
#' the source.
#'
#' @details
#' The measure is defined for directed networks. On an undirected network it
#' returns \code{NA} with a \code{cograph_undefined_measure} warning, and the
#' same happens when \code{membership} is missing, where the warning also has
#' class \code{cograph_bad_membership}. A \code{membership} whose length
#' differs from the number of nodes raises a \code{cograph_bad_membership}
#' error. Group labels may be numbers
#' or strings. Edge weights and self-loops are ignored.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Group labels, one per node.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named integer vector with one count per node, in input node
#'   order. \code{normalized = TRUE} returns a numeric vector.
#' @references
#' Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A formal
#'   approach to brokerage in transaction networks. Sociological Methodology,
#'   19, 89-126. \doi{10.2307/270949}.
#' @seealso \code{\link{centrality_brokerage_coordinator}},
#'   \code{\link{centrality_brokerage_liaison}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_brokerage_itinerant(regulation_net, membership = rep(1:2, each = 5))
centrality_brokerage_itinerant <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_itinerant",
                   membership = membership, ...)
  stats::setNames(df$brokerage_itinerant, df$node)
}

#' Representative Brokerage
#'
#' Representative brokerage (Gould and Fernandez 1989) counts the open
#' two-paths \eqn{a \to v \to c}{a -> v -> c} through node \eqn{v} in which
#' \eqn{a} and \eqn{v} belong to the same group and \eqn{c} to another group.
#' The broker passes contact from its own group to the outside. A two-path is
#' open when the network has no edge from \eqn{a} to \eqn{c}. This role is
#' \eqn{b_{IO}}{b_IO} in the notation of the source.
#'
#' @details
#' The measure is defined for directed networks. On an undirected network it
#' returns \code{NA} with a \code{cograph_undefined_measure} warning, and the
#' same happens when \code{membership} is missing, where the warning also has
#' class \code{cograph_bad_membership}. A \code{membership} whose length
#' differs from the number of nodes raises a \code{cograph_bad_membership}
#' error. Group labels may be numbers
#' or strings. Edge weights and self-loops are ignored.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Group labels, one per node.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named integer vector with one count per node, in input node
#'   order. \code{normalized = TRUE} returns a numeric vector.
#' @references
#' Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A formal
#'   approach to brokerage in transaction networks. Sociological Methodology,
#'   19, 89-126. \doi{10.2307/270949}.
#' @seealso \code{\link{centrality_brokerage_gatekeeper}},
#'   \code{\link{centrality_brokerage_coordinator}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_brokerage_representative(regulation_net, membership = rep(1:2, each = 5))
centrality_brokerage_representative <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_representative",
                   membership = membership, ...)
  stats::setNames(df$brokerage_representative, df$node)
}

#' Gatekeeper Brokerage
#'
#' Gatekeeper brokerage (Gould and Fernandez 1989) counts the open two-paths
#' \eqn{a \to v \to c}{a -> v -> c} through node \eqn{v} in which \eqn{v} and
#' \eqn{c} belong to the same group and \eqn{a} to another group. The broker
#' admits contact from outside to a member of its own group. A two-path is
#' open when the network has no edge from \eqn{a} to \eqn{c}. This role is
#' \eqn{b_{OI}}{b_OI} in the notation of the source.
#'
#' @details
#' The measure is defined for directed networks. On an undirected network it
#' returns \code{NA} with a \code{cograph_undefined_measure} warning, and the
#' same happens when \code{membership} is missing, where the warning also has
#' class \code{cograph_bad_membership}. A \code{membership} whose length
#' differs from the number of nodes raises a \code{cograph_bad_membership}
#' error. Group labels may be numbers
#' or strings. Edge weights and self-loops are ignored.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Group labels, one per node.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named integer vector with one count per node, in input node
#'   order. \code{normalized = TRUE} returns a numeric vector.
#' @references
#' Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A formal
#'   approach to brokerage in transaction networks. Sociological Methodology,
#'   19, 89-126. \doi{10.2307/270949}.
#' @seealso \code{\link{centrality_brokerage_representative}},
#'   \code{\link{centrality_brokerage_coordinator}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_brokerage_gatekeeper(regulation_net, membership = rep(1:2, each = 5))
centrality_brokerage_gatekeeper <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_gatekeeper",
                   membership = membership, ...)
  stats::setNames(df$brokerage_gatekeeper, df$node)
}

#' Liaison Brokerage
#'
#' Liaison brokerage (Gould and Fernandez 1989) counts the open two-paths
#' \eqn{a \to v \to c}{a -> v -> c} through node \eqn{v} in which \eqn{a},
#' \eqn{v} and \eqn{c} belong to three different groups. A two-path is open
#' when the network has no edge from \eqn{a} to \eqn{c}. This role is
#' \eqn{b_O}{b_O} in the notation of the source.
#'
#' @details
#' The measure is defined for directed networks. On an undirected network it
#' returns \code{NA} with a \code{cograph_undefined_measure} warning, and the
#' same happens when \code{membership} is missing, where the warning also has
#' class \code{cograph_bad_membership}. A \code{membership} whose length
#' differs from the number of nodes raises a \code{cograph_bad_membership}
#' error. Group labels may be numbers
#' or strings. With fewer than three groups every count is 0. Edge weights and
#' self-loops are ignored.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Group labels, one per node.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named integer vector with one count per node, in input node
#'   order. \code{normalized = TRUE} returns a numeric vector.
#' @references
#' Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A formal
#'   approach to brokerage in transaction networks. Sociological Methodology,
#'   19, 89-126. \doi{10.2307/270949}.
#' @seealso \code{\link{centrality_brokerage_itinerant}},
#'   \code{\link{centrality_brokerage_coordinator}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_brokerage_liaison(regulation_net, membership = rep(1:3, length.out = 10))
centrality_brokerage_liaison <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_liaison",
                   membership = membership, ...)
  stats::setNames(df$brokerage_liaison, df$node)
}


#' Calculate Edge Centrality Measures
#'
#' Computes centrality measures for the edges of a network and returns a
#' tidy data frame with one row per edge.
#'
#' @param x Network input (matrix, edge-list data frame, igraph, network,
#'   cograph_network, tna object).
#' @param measures Which measures to calculate. Default "all" calculates all
#'   available edge measures. Options: "betweenness", "weight", "overlap",
#'   "simmelian", "reciprocity".
#' @param weighted Logical. Use edge weights if available. Default TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param cutoff Maximum path length for betweenness. Default -1 (no limit).
#' @param invert_weights Logical or NULL. Whether edge betweenness inverts the
#'   weights, so that higher weights mean shorter paths. The default
#'   \code{NULL} is TRUE for tna objects and FALSE otherwise.
#' @param alpha Numeric. Exponent of the inversion, which computes distances
#'   as \code{1 / weight^alpha}. Default 1.
#' @param digits Integer or NULL. Round numeric columns. Default NULL.
#' @param sort_by Character or NULL. Column to sort by (descending). Default NULL.
#' @param ... For \code{edge_centrality()}, the graph-construction
#'   arguments \code{loops} and \code{simplify} (see
#'   \code{\link{centrality}}). For \code{edge_betweenness()}, arguments
#'   passed to \code{edge_centrality()}.
#'
#' @return \code{edge_centrality()} returns a base \code{data.frame} with one
#'   row per edge, in the canonical
#'   (row-major) edge order of the input. The first two columns are
#'   \code{from} and \code{to} (character when the input carried node names,
#'   numeric indices otherwise); the remaining columns are those the requested
#'   measures contribute, as listed in Details. \code{measures = "all"} on an
#'   undirected input therefore gives \code{from}, \code{to}, \code{weight},
#'   \code{betweenness}, \code{overlap}, \code{shared_neighbors} and
#'   \code{triangles}, and a directed input adds \code{reciprocated},
#'   \code{reverse_weight} and \code{weight_ratio}.
#'
#' @details
#' Edge measures available, with the column(s) each one adds:
#' \describe{
#'   \item{betweenness}{Edge betweenness, the sum over node pairs of the
#'     share of their shortest paths that pass through the edge. Adds
#'     \code{betweenness}.}
#'   \item{weight}{Original edge weight (1 for an unweighted input). Adds
#'     \code{weight}.}
#'   \item{overlap}{Jaccard neighborhood overlap of the edge endpoints. Adds
#'     \code{overlap} and the raw count \code{shared_neighbors}.}
#'   \item{simmelian}{Number of triangles the edge participates in. Adds
#'     \code{triangles} (there is no column called \code{simmelian}).}
#'   \item{reciprocity}{Whether the reverse edge exists. Directed only: on an
#'     undirected input it warns and adds nothing. Adds
#'     \code{reciprocated}, \code{reverse_weight} and \code{weight_ratio},
#'     the last two \code{NA} where the edge is not reciprocated.}
#' }
#' \code{measures = "all"} requests every measure, dropping
#' \code{reciprocity} on an undirected input.
#'
#' @export
#' @examples
#' edge_centrality(regulation_net, measures = "betweenness")
edge_centrality <- function(x, measures = "all",
                            weighted = TRUE, directed = NULL,
                            cutoff = -1, invert_weights = NULL, alpha = 1,
                            digits = NULL, sort_by = NULL, ...) {

  # Auto-detect invert_weights for tna objects
 is_tna_input <- inherits(x, c("tna", "group_tna", "ctna", "ftna", "atna",
                                 "group_ctna", "group_ftna", "group_atna"))
  if (is.null(invert_weights)) {
    invert_weights <- is_tna_input
  }

  # Native graph context: canonical (row-major) edge order, the same order
  # igraph reads an adjacency matrix in.
  cg <- .cg_graph(x, directed = directed, ...)
  directed <- cg$directed
  n <- cg$n
  from_idx <- cg$edges[, 1L]
  to_idx <- cg$edges[, 2L]

  # Endpoints carry the labels when the input had them, else the indices.
  result <- if (cg$has_names) {
    data.frame(from = cg$labels[from_idx], to = cg$labels[to_idx],
               stringsAsFactors = FALSE)
  } else {
    data.frame(from = as.numeric(from_idx), to = as.numeric(to_idx))
  }

  # Available measures
  all_measures <- c("betweenness", "weight", "overlap", "simmelian",
                    "reciprocity")

  # Resolve measures
 if (identical(measures, "all")) {
    # reciprocity only for directed
    measures <- if (directed) all_measures else
      setdiff(all_measures, "reciprocity")
  } else {
    invalid <- setdiff(measures, all_measures)
    if (length(invalid) > 0) {
      stop(errorCondition(
        paste0("Unknown edge measures: ", paste(invalid, collapse = ", "),
               "\nAvailable: ", paste(all_measures, collapse = ", ")),
        class = "cograph_unknown_measure", call = NULL))
    }
  }

  # Get weights
  weights <- if (weighted && !is.null(cg$weights)) cg$weights else NULL

  # Add weight column if requested
  if ("weight" %in% measures) {
    result$weight <- if (!is.null(weights)) weights else rep(1, nrow(result))
  }

  # Calculate edge betweenness
  if ("betweenness" %in% measures) {
    # Handle weight inversion for path-based measure
    bet_weights <- weights
    if (!is.null(weights) && invert_weights) {
      bet_weights <- 1 / (weights ^ alpha)
      bet_weights[!is.finite(bet_weights)] <- .Machine$double.xmax
      reason <- if (is_tna_input) "tna object detected" else "invert_weights=TRUE"
      message("Note: Weights inverted (1/w^", alpha, ") for edge betweenness (",
              reason, "). Higher weights = shorter paths.")
    }

    # NULL weights fall back to the graph's own, as igraph read them.
    result$betweenness <- .cg_edge_betweenness(
      .cg_attr_matrix(cg, bet_weights), n, directed, cutoff = cutoff,
      edges = cg$edges
    )
  }

  # Overlap and simmelian share neighbor computation -- do it once
  needs_neighbors <- any(c("overlap", "simmelian") %in% measures)
  if (needs_neighbors) {
    adj_list <- .cg_adjlist(cg$b, directed, "all")

    # Compute shared + union neighbor counts per edge (single pass)
    neighbor_stats <- vapply(seq_len(nrow(cg$edges)), function(ei) {
      u <- from_idx[ei]; v <- to_idx[ei]
      n_u <- setdiff(adj_list[[u]], c(u, v))
      n_v <- setdiff(adj_list[[v]], c(u, v))
      sh <- length(intersect(n_u, n_v))
      un <- length(union(n_u, n_v))
      c(sh, un)
    }, numeric(2))

    if ("overlap" %in% measures) {
      result$overlap <- ifelse(neighbor_stats[2, ] == 0L, 0,
                               neighbor_stats[1, ] / neighbor_stats[2, ])
      result$shared_neighbors <- as.integer(neighbor_stats[1, ])
    }
    if ("simmelian" %in% measures) {
      result$triangles <- as.integer(neighbor_stats[1, ])
    }
  }

  # Edge reciprocity (directed only)
  if ("reciprocity" %in% measures && !directed) {
    .cg_warn_undefined("Reciprocity skipped: only meaningful for directed networks.")
  }
  if ("reciprocity" %in% measures && directed) {
    w_vec <- if (!is.null(weights)) weights else rep(1, nrow(result))
    # The reverse arc's weight, read straight off the weight matrix; 0
    # there means the arc does not exist.
    rev_wts <- cg$w[cbind(to_idx, from_idx)]
    if (is.null(weights)) rev_wts <- (rev_wts != 0) * 1
    result$reciprocated <- rev_wts != 0
    result$reverse_weight <- ifelse(result$reciprocated, rev_wts, NA_real_)
    result$weight_ratio <- ifelse(result$reciprocated, rev_wts / w_vec, NA_real_)
  }

  # Round if requested
  if (!is.null(digits)) {
    numeric_cols <- vapply(result, is.numeric, logical(1))
    result[numeric_cols] <- lapply(result[numeric_cols], round, digits = digits)
  }

  # Sort if requested
  if (!is.null(sort_by)) {
    if (!sort_by %in% names(result)) {
      .cg_stop_bad_parameter("sort_by column '", sort_by, "' not found in results")
    }
    result <- result[order(result[[sort_by]], decreasing = TRUE), ]
    rownames(result) <- NULL
  }

  result
}

#' @rdname edge_centrality
#' @return \code{edge_betweenness()} returns a numeric vector of edge
#'   betweenness values named \code{"from->to"}, for directed and undirected
#'   inputs alike.
#' @export
edge_betweenness <- function(x, ...) {
  df <- edge_centrality(x, measures = "betweenness", ...)
  stats::setNames(df$betweenness, paste(df$from, df$to, sep = "->"))
}
