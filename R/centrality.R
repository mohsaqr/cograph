#' Calculate Network Centrality Measures
#'
#' Computes centrality measures for nodes in a network and returns a tidy
#' data frame. Accepts matrices, edge-list data frames, igraph objects,
#' cograph_network, or tna objects.
#'
#' @param x Network input (matrix, edge-list data frame, igraph, network,
#'   cograph_network, tna object)
#' @param type Character scalar selecting a curated tier of measures when
#'   \code{measures} is not supplied. One of:
#'   \describe{
#'     \item{\code{"basic"}}{(default) 6 canonical measures: \code{degree},
#'       \code{strength}, \code{closeness}, \code{betweenness},
#'       \code{eigenvector}, \code{pagerank}.}
#'     \item{\code{"extended"}}{Basic plus commonly-reported second-tier
#'       measures: harmonic, coreness, eccentricity, radiality,
#'       lin, decay, load, stress, katz, alpha, power, authority, leverage,
#'       constraint, effective_size, bridging, transitivity, subgraph,
#'       diffusion, laplacian, kreach, current_flow_betweenness,
#'       current_flow_closeness.}
#'     \item{\code{"all"}}{Every available measure.}
#'   }
#'   Passing \code{measures} explicitly overrides \code{type}.
#' @param include Character vector of costly measures to add back to a tier,
#'   or \code{"costly"} for all of them. \code{type = "all"} holds back the
#'   measures whose cost grows steeply with network size (see
#'   \code{\link{list_centralities}}), so that one call cannot take minutes
#'   by accident. Naming a measure in \code{measures} always computes it,
#'   whatever its cost. Default \code{NULL}.
#' @param measures Character vector of specific measure names to compute.
#'   When \code{NULL} (default) the tier selected by \code{type} is used.
#'   Accepts \code{"all"} as a shortcut for every measure. Any custom vector
#'   of valid measure names is also accepted.
#'   **Core** (igraph-backed): "degree", "strength", "betweenness", "closeness",
#'   "eigenvector", "pagerank", "authority", "hub", "eccentricity", "coreness",
#'   "constraint", "transitivity", "harmonic", "alpha", "power", "subgraph".
#'   **Native**: "diffusion", "leverage", "kreach", "laplacian", "load",
#'   "current_flow_closeness", "current_flow_betweenness", "voterank",
#'   "percolation".
#'   **Distance-based**: "radiality", "lin", "decay", "residual_closeness",
#'   "dangalchev", "generalized_closeness", "harary", "average_distance",
#'   "barycenter", "wiener", "closeness_vitality".
#'   **Spectral/walk**: "communicability", "communicability_betweenness",
#'   "random_walk".
#'   **Path-based**: "stress", "flow_betweenness".
#'   **Local/neighborhood**: "lobby", "entropy", "semilocal", "clusterrank",
#'   "bottleneck", "centroid", "mnc", "dmnc", "lac", "topological_coefficient",
#'   "bridging", "local_bridging", "effective_size", "diversity",
#'   "cross_clique", "markov".
#'   **Influence**: "integration", "expected", "gilschmidt".
#'   **Directed-only**: "salsa", "leaderrank", "trophic_level", "pairwisedis",
#'   "prestige_domain", "prestige_domain_proximity".
#'   **Community-aware** (require \code{membership}): "participation",
#'   "within_module_z", "gateway", "brokerage_coordinator",
#'   "brokerage_itinerant", "brokerage_representative",
#'   "brokerage_gatekeeper", "brokerage_liaison" (the last 5 also require
#'   a directed graph; see \code{\link{centrality_brokerage_coordinator}}).
#'   **Zoo (batch 2)**: "gravity", "collective_influence", "local_hindex",
#'   "hindex_strength", "onion", "second_order", "infection", "nonbacktracking",
#'   "spanning_tree".
#'   **Classical (batch 3, reference-validated)**: "katz" (Katz 1953),
#'   "hubbell" (Hubbell 1965), "information" (Stephenson-Zelen 1989),
#'   "reaching_local" (Mones et al. 2012). See \code{\link{centrality_katz}},
#'   \code{\link{centrality_hubbell}}, \code{\link{centrality_information}},
#'   \code{\link{centrality_pairwisedis}}, \code{\link{centrality_reaching_local}}.
#'   **Psychometric (signed-weight)**: "expected_influence_1",
#'   "expected_influence_2" (Robinaugh, Millner & McNally 2016). Expected
#'   influence keeps signed edge contributions, which is important when edges
#'   can be negative (partial-correlation, glasso, signed correlation networks).
#'   **Zoo (batch 7, lowest rank-redundancy with the rest of the package per
#'   the Centrality Zoo comparison)**: "distance_entropy" (Stella & De
#'   Domenico 2018), "local_dimension" (Pu et al. 2014),
#'   "local_information_dimension" (Wen & Deng 2020),
#'   "neighborhood_connectivity" (Maslov & Sneppen 2002), and
#'   "modularity_vitality" (Magelinski et al. 2021; requires
#'   \code{membership}). The first three are hop-count measures and ignore
#'   edge weights. See \code{\link{centrality_distance_entropy}},
#'   \code{\link{centrality_local_dimension}},
#'   \code{\link{centrality_local_information_dimension}},
#'   \code{\link{centrality_neighborhood_connectivity}},
#'   \code{\link{centrality_modularity_vitality}}.
#'   **Zoo (batch 8, the measures the Zoo comparison left "on the way")**:
#'   "shapley_game1", "shapley_game2", "shapley_game3" (Michalak et al.
#'   2013), "access_information", "hide_information" (Rosvall et al. 2005),
#'   "rumor" (Shah & Zaman 2011), "community_hub_bridge" (Ghalmane et al.
#'   2019; requires \code{membership}), "entropy_variation_degree",
#'   "entropy_variation_betweenness" (Ai 2017), "s_shell" (Liu et al. 2017),
#'   "degree_discount", "single_discount" (Chen, Wang & Yang 2009),
#'   "ncvoterank" (Kumar & Panda 2020). All are hop-count or topology-only
#'   measures; edge weights are ignored. See the per-measure pages, e.g.
#'   \code{\link{centrality_shapley_game1}},
#'   \code{\link{centrality_access_information}},
#'   \code{\link{centrality_rumor}},
#'   \code{\link{centrality_community_hub_bridge}},
#'   \code{\link{centrality_entropy_variation}},
#'   \code{\link{centrality_s_shell}},
#'   \code{\link{centrality_degree_discount}},
#'   \code{\link{centrality_ncvoterank}}.
#'   **Zoo (batch 9, the remaining measures with a pinned definition)**:
#'   community-aware "community_based" (Zhao et al. 2015), "comm_centrality"
#'   (Gupta et al. 2016), "community_mediator" (Tulu et al. 2018), all
#'   requiring \code{membership}; dimension family "local_dimension_fixed"
#'   (Silva & Costa 2013), "fuzzy_local_dimension" (Wen & Jiang 2019),
#'   "local_volume_dimension" (Li & Deng 2021); VoteRank family
#'   "wvoterank" (Sun et al. 2019), "enrenew" (Guo et al. 2020),
#'   "voterank_plus" (Liu et al. 2021); "node_contraction",
#'   "node_contraction_improved" (Tan et al. 2006; Wang et al. 2011);
#'   "two_way_rw" (Curado et al. 2022); local measures "heatmap"
#'   (Duron 2020), "flow_coefficient" (Honey et al. 2007), "local_entropy"
#'   (Nie et al. 2016), "weighted_h_index" (Gao et al. 2019), "redundancy"
#'   (Burt 1992); "weighted_kshell" (Garas et al. 2012),
#'   "renewed_coreness" (Liu et al. 2015), "geodesic_kpath" (Borgatti &
#'   Everett 2006). Only "wvoterank", "two_way_rw" and "weighted_kshell"
#'   use edge weights. See \code{\link{centrality_community_based}},
#'   \code{\link{centrality_local_dimension_fixed}},
#'   \code{\link{centrality_wvoterank}},
#'   \code{\link{centrality_node_contraction}},
#'   \code{\link{centrality_two_way_rw}}, \code{\link{centrality_heatmap}},
#'   \code{\link{centrality_weighted_kshell}}.
#'
#'   Batch 10 closes the gaps other centrality packages had and cograph did
#'   not: "local_efficiency" (Latora & Marchiori 2001), "s_core" (Eidsaa &
#'   Almaas 2013), "fragmentation" (Borgatti 2006), "kpath" (Sade 1989) and
#'   "epc" (Lin et al. 2008). "fragmentation" and "epc" are costly, so
#'   \code{type = "all"} holds them back. See
#'   \code{\link{centrality_local_efficiency}}.
#'
#'   Batch 11 tunes families cograph already had: "length_scaled_betweenness"
#'   (Brandes 2008), "delta_betweenness" and "delta_closeness" (Agneessens
#'   et al. 2017), "ego_betweenness" (Everett & Borgatti 2005). "gravity"
#'   gained \code{gravity_mass} and \code{gravity_radius}, and its formula
#'   was corrected -- see \code{\link{centrality_gravity}}. Bounded-distance
#'   ("k-") betweenness needs no measure of its own: it is
#'   \code{cutoff = k}. See
#'   \code{\link{centrality_length_scaled_betweenness}}.
#' @param mode For directed networks: "all", "in", or "out". Affects measures
#'   whose output columns carry a mode suffix, including degree, strength,
#'   closeness, eccentricity, coreness, harmonic, diffusion, leverage, k-reach,
#'   distance-based measures, community-aware measures, and expected influence.
#' @param normalized Logical. Normalize values by dividing by max. Most measures
#'   are scaled to 0-1; signed expected-influence measures can retain negative
#'   values under psychometric normalization. For closeness, this is passed
#'   directly to igraph.
#' @param weighted Logical. Use edge weights if available. Default TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param loops Logical. If TRUE (default), keep self-loops. Set to FALSE to
#'   remove them before calculation.
#' @param simplify How to combine multiple edges between the same node pair.
#'   Options: "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
#'   multiple edges.
#' @param digits Integer or NULL. Round all numeric columns to this many
#'   decimal places. Default NULL (no rounding).
#' @param sort_by Character or NULL. Column name to sort results by
#'   (descending order). Default NULL (original node order).
#' @param cutoff Maximum path length to consider for betweenness, closeness,
#'   and harmonic centrality.
#'   Default -1 (no limit). Set to a positive value for faster computation
#'   on large networks at the cost of accuracy.
#' @param invert_weights Logical or NULL. For path- and distance-based measures
#'   (for example betweenness, closeness, harmonic, eccentricity, k-reach,
#'   radiality, decay, stress, flow betweenness, and related variants), should
#'   weights be inverted so that higher weights mean shorter paths? Default
#'   NULL auto-detects: TRUE for tna objects (transition probabilities), FALSE
#'   otherwise (matching igraph/sna). Set explicitly to TRUE for
#'   strength/frequency weights (qgraph style) or FALSE for distance/cost
#'   weights.
#' @param alpha Numeric. Exponent for weight transformation when \code{invert_weights = TRUE}.
#'   Distance is computed as \code{1 / weight^alpha}. Default 1. Higher values
#'   increase the influence of weight differences on path lengths.
#' @param damping PageRank damping factor. Default 0.85. Must be between 0 and 1.
#' @param personalized Named numeric vector for personalized PageRank.
#'   Default NULL (standard PageRank). Values should sum to 1.
#' @param transitivity_type Type of transitivity to calculate: "local" (default),
#'   "global", "undirected", "localundirected", "barrat" (weighted),
#'   "weighted", or "onnela". The first six dispatch to
#'   \code{igraph::transitivity()}; \code{"onnela"} computes the Onnela /
#'   Holme weighted clustering coefficient on the symmetrized matrix
#'   (\code{wcc(x + t(x))}) and matches \code{tna::centralities(., "Clustering")}
#'   byte-for-byte. Auto-set to \code{"onnela"} when \code{tna_network = TRUE}
#'   and the user did not pass an explicit value.
#' @param isolates How to handle isolate nodes in transitivity calculation:
#'   "nan" (default) returns NaN, "zero" returns 0.
#' @param lambda Diffusion scaling factor for diffusion centrality. Default 1.
#'   Only used when \code{diffusion_method = "kandhway_kuri"}.
#' @param diffusion_method Character or NULL. Selects the diffusion-centrality
#'   formula. \code{"kandhway_kuri"} (Kandhway & Kuri, 2014) computes the
#'   1-hop binary-degree neighborhood sum
#'   \eqn{\lambda d_v + \lambda \sum_{u \in N(v)} d_u}. \code{"power_series"}
#'   computes the matrix power series \eqn{\mathrm{rowSums}(P + P^2 + \ldots + P^n)}
#'   on the (optionally diagonal-zeroed) weighted matrix and matches
#'   \code{tna::centralities(., measures = "Diffusion")} when
#'   \code{loops = FALSE}. Default NULL auto-detects: \code{"power_series"}
#'   for tna objects (transition probabilities), \code{"kandhway_kuri"}
#'   otherwise.
#' @param k Path length parameter for geodesic k-path centrality. Default 3.
#' @param states Named numeric vector of percolation states (0-1) for percolation
#'   centrality. Each value represents how "activated" or "infected" a node is.
#'   Default NULL (all nodes get state 1, equivalent to betweenness).
#' @param decay_parameter Numeric. Decay parameter for decay and generalized
#'   closeness centrality. Default 0.5. Must be between 0 and 1.
#' @param dmnc_epsilon Numeric. Epsilon exponent for DMNC (Density of Maximum
#'   Neighborhood Component). Default 1.7 as recommended by Lin et al. (2008).
#'   centiserve uses 1.67 (four-community assumption). Must be between 1 and 2.
#' @param membership Integer vector of community assignments (one per node) for
#'   community-aware measures: participation, within_module_z, gateway,
#'   modularity_vitality, and the Gould-Fernandez brokerage roles. Default
#'   NULL. Required when requesting
#'   these measures.
#' @param katz_alpha Attenuation factor for Katz centrality. Must satisfy
#'   \eqn{\alpha < 1 / \rho(A)}. Default 0.1 (matches centiserve and NetworkX
#'   conventions). Only used when \code{"katz"} is in \code{measures}.
#' @param shapley_k Neighbour threshold \eqn{k} for \code{"shapley_game2"}.
#'   Default 2. See \code{\link{centrality_shapley_game2}}.
#' @param shapley_cutoff Hop cutoff for \code{"shapley_game3"}. Default 2.
#'   See \code{\link{centrality_shapley_game3}}.
#' @param s_shell_a Exponent of the asymmetric link weights for
#'   \code{"s_shell"}. Default 0.5. See \code{\link{centrality_s_shell}}.
#' @param discount_p Propagation probability for \code{"degree_discount"}.
#'   Default 0.01. See \code{\link{centrality_degree_discount}}.
#' @param ncvote_theta Weight of the plain vote in \code{"ncvoterank"}.
#'   Default 0.5. See \code{\link{centrality_ncvoterank}}.
#' @param comm_r Scale \eqn{R} of \code{"comm_centrality"}:
#'   \code{"max_intra"} (default) or a positive number.
#' @param ld_radius Radius for \code{"local_dimension_fixed"}. Default 2.
#' @param enrenew_depth Renewal radius for \code{"enrenew"}. Default 2.
#' @param voterank_lambda Suppression factor for \code{"voterank_plus"}.
#'   Default 0.1.
#' @param contraction_rho \eqn{\alpha / \beta} for
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
#' @param epc_runs Number of percolation realisations for \code{"epc"}.
#'   Default 1000.
#' @param epc_seed Random seed for \code{"epc"}. Default \code{NULL},
#'   which leaves the caller's stream alone and lets the estimate vary
#'   between calls.
#' @param betweenness_delta Decay exponent for \code{"delta_betweenness"}.
#'   Default 1; 0 gives ordinary betweenness.
#' @param closeness_delta Distance exponent for \code{"delta_closeness"}.
#'   Default 1, which is \code{harmonic} over \eqn{n - 1}.
#' @param gravity_mass Mass in \code{"gravity"}: \code{"kshell"} (default,
#'   Ma et al. 2016), \code{"degree"} (Li et al. 2019) or \code{"legacy"}
#'   for cograph's pre-2.4.8 form.
#' @param gravity_radius Largest distance each gravity source reaches in
#'   \code{"gravity"}, \code{"extended_gravity"},
#'   \code{"mixed_gravity"} or \code{"extended_mixed_gravity"}: a
#'   number (default 3), \code{"auto"} for half the mean distance, or
#'   \code{NULL} for the whole graph.
#'   The auto radius uses finite positive distances, rounds to the nearest
#'   integer (ties to even), and has minimum 1; these are cograph conventions.
#' @param mdd_lambda Exhausted-degree weight for \code{"mdd"}, between
#'   0 and 1. Default 0.7. See \code{\link{centrality_truss}}.
#' @param volume_radius Closed neighbourhood radius for \code{"volume"}:
#'   a nonnegative integer or \code{Inf}, default 2. Degrees are measured
#'   in the full simple undirected graph. See \code{\link{centrality_volume}}.
#' @param diffusion_q Multiplier between 0 and 1 for \code{"diffusion_centrality"},
#'   default 1. Independent of the existing \code{lambda} argument.
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
#' @section Measures without a value on a given input: A few measures are
#'   undefined on some graphs -- the community-partition measures without
#'   \code{membership}, or \code{"relative_entropy"} when one of its
#'   constituent indexes is zero at every node. Naming such a measure in
#'   \code{measures} or \code{include} raises a classed condition, because
#'   you asked for that measure. When a tier (\code{type = "basic"},
#'   \code{"extended"} or \code{"all"}) supplied it, the condition becomes a
#'   \code{cograph_undefined_measure} warning and the column is \code{NA},
#'   so one undefined measure does not take the rest of the tier with it.
#'
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
#'   at least one, default 1000. Reaching it raises
#'   \code{cograph_no_converge}, which a bipartite component with unequal
#'   vertex classes always does. See \code{\link{centrality_ira}}.
#' @param iira_beta Spreading rate for \code{"iira"}, a number in
#'   \eqn{(0,1]}, default 0.2 as in the source.
#' @param iira_steps Iterations for \code{"iira"}, a nonnegative whole
#'   number, default 50 as in the source; zero returns the initial unit
#'   resource. See \code{\link{centrality_iira}}.
#' @param hcc_delta Weight on a node's own degree in the extended degree
#'   used by \code{"hcc"} and \code{"ehcc"}, a single number in
#'   \eqn{[0,1]}, default 0.5 as in the source; one recovers the classical
#'   degree and zero drops the node's own degree entirely. Values outside
#'   \eqn{[0,1]} are refused. See \code{\link{centrality_hcc}}.
#' @param lhc_radius Radius of the ball \eqn{\Phi(v)} summed over by
#'   \code{"lhc"}, the \eqn{d} of the source's equation (1); a single whole
#'   number of at least one, default 2 as the source sets it. The source
#'   sweeps it and reports 2-3 as optimal. At one the ball collapses to the
#'   neighbours; at or above the diameter the score stops moving. Values
#'   below one and non-integers are refused. See
#'   \code{\link{centrality_lhc}}.
#' @param tpr_alpha Jump probability of the trust-PageRank iteration used
#'   by \code{"trust_pagerank"}, a single number strictly between zero and
#'   one, default 0.85 as the source sets it below its equation (7). See
#'   \code{\link{centrality_trust_pagerank}}.
#' @param tpr_k Weight the trust-value puts on the degree ratio rather than
#'   the similarity ratio in \code{"trust_pagerank"}, the \eqn{k} of the
#'   source's equation (6); a single number in \eqn{[0,1]}, default 0.85,
#'   the value the source's section 3.3 selects from a Kendall-against-SIR
#'   sweep. One drops the similarity entirely and zero drops the degree.
#' @param tpr_decay Attenuation factor of the similarity recursion used by
#'   \code{"trust_pagerank"}, the \eqn{C} of the source's equation (4); a
#'   single number in \eqn{(0,1]}, default 1 as the source fixes it. The
#'   source's claim that \eqn{C} does not affect the result holds only for a
#'   homogeneous recursion and not for this one; see
#'   \code{\link{centrality_trust_pagerank}}.
#' @param tpr_tol Convergence tolerance on the largest \emph{relative}
#'   change of either trust-PageRank recursion, a single positive number,
#'   default \code{1e-14}. The source fixes no iteration count because it
#'   does not need one: both recursions have unique fixed points. The test
#'   is relative rather than absolute because the similarities on one graph
#'   span many orders of magnitude; see
#'   \code{\link{centrality_trust_pagerank}}.
#' @param tpr_max_iter Iteration bound for both trust-PageRank recursions, a
#'   whole number of at least one, default 1000. Reaching it raises
#'   \code{cograph_no_converge}.
#' @param rsp_beta Inverse temperature of the randomized-shortest-paths
#'   model used by \code{"rsp_betweenness"}, a single finite number strictly
#'   above zero, default 0.01. The source fixes no default; 0.01 is the
#'   value \code{NetworkToolbox::rspbc()} recommends, and it sits near the
#'   random-walk limit, so raise it towards 1 and beyond to move the reading
#'   towards shortest paths. See
#'   \code{\link{centrality_rsp_betweenness}}.
#' @param rsp_cost How an edge weight becomes a traversal cost for
#'   \code{"rsp_betweenness"}: \code{"inverse"} (default) for \eqn{C=1/w},
#'   reading a weight as an affinity, or \code{"weight"} for \eqn{C=w},
#'   reading it as a distance. The source leaves the cost matrix free; both
#'   settings give unit cost per arc on a binary graph. See
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
#' @param rwd_decay Finite first-arrival discount in [0,1) for
#'   \code{"random_walk_decay"}, default0.5.
#' @param rwd_node_weights Nonnegative starting weights for
#'   \code{"random_walk_decay"}; NULL means ones. See
#'   \code{\link{centrality_random_walk_decay}}.
#' @param grc_gamma Finite nonnegative regularization strength for
#'   \code{"graph_regularization"}, default one. See
#'   \code{\link{centrality_graph_regularization}}.
#' @param alr_h_mode H-index convention for \code{"adaptive_leaderrank"}:
#'   all (default), out or in. See \code{\link{centrality_adaptive_leaderrank}}.
#' @param tna_network Logical or NULL. Umbrella switch that forces tna-style
#'   conventions across all measures. \code{NULL} (default) auto-detects
#'   from the input class — TRUE iff \code{x} is a \code{tna} or related
#'   sequence-network object. \code{TRUE} forces tna conventions even on
#'   raw matrices: \code{invert_weights = TRUE}, \code{loops = FALSE},
#'   \code{diffusion_method = "power_series"}, \code{transitivity_type
#'   = "onnela"}. \code{FALSE} suppresses all tna defaults even for tna
#'   inputs, giving the cograph defaults verbatim. Precedence: any arg
#'   the user passes explicitly always wins over \code{tna_network}.
#' @param psych_network Logical or NULL. Switch for signed psychometric
#'   network conventions. \code{NULL} (default) auto-detects TRUE when a
#'   signed weighted network is evaluated with expected-influence measures.
#'   When \code{TRUE}, normalized expected influence is divided by the maximum
#'   absolute expected-influence value, preserving sign and bounding the result from
#'   -1 to 1.
#'   \code{FALSE} keeps the generic cograph normalization convention.
#' @param hubbell_weight Weight factor \eqn{w} for Hubbell centrality. Must
#'   satisfy \eqn{w \cdot \rho(W) \le 1} for solvability. Default 0.5. Only
#'   used when \code{"hubbell"} is in \code{measures}.
#' @param ... Additional arguments (currently unused)
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{node}: Node labels/names
#'     \item One column per measure, with mode suffix for directional measures
#'       (e.g., \code{degree_in}, \code{closeness_all})
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
#'   \item{eccentricity}{Maximum distance to other nodes (supports mode)}
#'   \item{coreness}{K-core membership (supports mode: in/out/all)}
#'   \item{constraint}{Burt's constraint (structural holes)}
#'   \item{transitivity}{Local clustering coefficient (supports multiple types)}
#'   \item{harmonic}{Harmonic centrality - handles disconnected graphs better
#'     than closeness (supports mode: in/out/all)}
#'   \item{diffusion}{Diffusion degree centrality - sum of scaled degrees of
#'     node and its neighbors (supports mode: in/out/all, lambda scaling)}
#'   \item{leverage}{Leverage centrality - measures influence over neighbors
#'     based on relative degree differences (supports mode: in/out/all)}
#'   \item{kreach}{Geodesic k-path centrality - count of nodes reachable
#'     within distance k (supports mode: in/out/all, k parameter)}
#'   \item{alpha}{Alpha/Katz centrality - influence via paths, penalized by
#'     distance. Similar to eigenvector but includes exogenous contribution}
#'   \item{power}{Bonacich power centrality - measures influence based on
#'     connections to other influential nodes}
#'   \item{subgraph}{Subgraph centrality - participation in closed loops/walks,
#'     weighting shorter loops more heavily}
#'   \item{laplacian}{Laplacian centrality using Qi et al. (2012) local formula.
#'     Matches NetworkX and centiserve::laplacian()}
#'   \item{load}{Load centrality - fraction of all shortest paths through node,
#'     similar to betweenness but weights paths by 1/count}
#'   \item{current_flow_closeness}{Information centrality - closeness based on
#'     electrical current flow (requires connected graph)}
#'   \item{current_flow_betweenness}{Random walk betweenness - betweenness based
#'     on current flow rather than shortest paths (requires connected graph)}
#'   \item{voterank}{VoteRank - identifies influential spreaders via iterative
#'     voting mechanism. Returns normalized rank (1 = most influential)}
#'   \item{percolation}{Percolation centrality - importance for spreading processes.
#'     Uses node states (0-1) to weight paths. When all states equal, equivalent
#'     to betweenness. Useful for epidemic/information spreading analysis.}
#'   \item{radiality}{Radiality centrality (centiserve). Sum of (diam + 1 - d)
#'     normalized by n-1.}
#'   \item{lin}{Lin's centrality. Reachable nodes squared divided by sum of
#'     distances.}
#'   \item{decay}{Decay centrality. Sum of delta^d for parameter delta.}
#'   \item{residual_closeness}{Residual closeness. Sum of 1/2^d.}
#'   \item{dangalchev}{Dangalchev closeness (alias for residual closeness).}
#'   \item{generalized_closeness}{Generalized closeness. Sum of alpha^d.}
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
#'   \item{distance_entropy}{Normalised Shannon entropy of a node's
#'     hop-distance profile; 1 = distances spread evenly, 0 = all at one
#'     distance.}
#'   \item{local_dimension}{Growth exponent of the ball around a node
#'     (slope of \eqn{\ln B_i(r)} on \eqn{\ln r}); lower = more
#'     influential.}
#'   \item{local_information_dimension}{Entropy-weighted local dimension
#'     over boxes up to half the node's eccentricity; higher = more
#'     influential.}
#'   \item{neighborhood_connectivity}{Mean degree of a node's neighbours
#'     (average neighbour degree); isolates score 0.}
#'   \item{modularity_vitality}{Drop in modularity when the node is removed
#'     under a fixed partition; positive = community hub, negative = bridge
#'     (requires \code{membership}).}
#'   \item{shapley_game1, shapley_game2, shapley_game3}{Shapley value of the
#'     node in the coverage games of Michalak et al. (2013): one-hop
#'     coverage, \code{shapley_k}-neighbour coverage, and coverage within
#'     \code{shapley_cutoff} hops. Values sum to the node count.}
#'   \item{access_information}{Mean bits needed to reach every other node
#'     along shortest paths without a map; low = well connected.}
#'   \item{hide_information}{Mean bits others need to find the node;
#'     high = hidden.}
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
#'   \item{ncvoterank}{VoteRank with voters weighted by normalised
#'     neighbourhood coreness (\code{ncvote_theta}); election order scored
#'     like \code{voterank}.}
#'   \item{community_based, comm_centrality, community_mediator}{Links
#'     weighted by the size of the community they reach; Gupta's scaled
#'     intra/inter-degree combination (\code{comm_r}); base-2 entropy of the
#'     link distribution over communities times degree share (all require
#'     \code{membership}).}
#'   \item{local_dimension_fixed, fuzzy_local_dimension,
#'     local_volume_dimension}{Silva-Costa estimator at \code{ld_radius};
#'     slope of the fuzzy ball (higher = more influential); slope of the
#'     degree volume (lower = more important).}
#'   \item{wvoterank, enrenew, voterank_plus}{Election orders of the
#'     weighted, entropy-based (\code{enrenew_depth}) and degree-weighted
#'     (\code{voterank_lambda}) VoteRank variants, scored like
#'     \code{voterank}.}
#'   \item{node_contraction, node_contraction_improved}{One minus the
#'     agglomeration ratio after contracting the node with its neighbours;
#'     the improved form adds the same score of its edges on the line graph
#'     (\code{contraction_rho}).}
#'   \item{two_way_rw}{Number of node pairs whose most likely two-way
#'     random-walk route passes through the node.}
#'   \item{heatmap}{Farness minus mean neighbour farness; lower = more
#'     central.}
#'   \item{flow_coefficient}{Share of neighbour pairs linked through the
#'     node but not directly.}
#'   \item{local_entropy}{\eqn{-\sum_{j \in N(i)} k_j \ln k_j}; lower = more
#'     central.}
#'   \item{weighted_h_index}{h-index over topological link weights
#'     \eqn{k_i k_j} repeated \eqn{k_j} times.}
#'   \item{redundancy}{Mean degree of the neighbours inside the ego
#'     network; degree minus effective size.}
#'   \item{weighted_kshell}{k-shell on \eqn{(k^\alpha s^\beta)^{1/(\alpha
#'     + \beta)}} after Garas' weight normalisation (\code{wks_alpha},
#'     \code{wks_beta}).}
#'   \item{renewed_coreness}{k-core of the graph after removing links whose
#'     diffusion importance is below \code{renewed_threshold}.}
#'   \item{geodesic_kpath}{Number of shortest paths of length at most
#'     \code{kpath_k} starting at the node.}
#'   \item{local_efficiency}{Global efficiency of the subgraph induced on
#'     the node's neighbours, the node itself removed. Note that
#'     \code{igraph::local_efficiency()} instead measures the distances
#'     between those neighbours through the rest of the network.}
#'   \item{s_core}{Largest strength threshold whose s-core still contains
#'     the node; the k-core number when weights are absent.}
#'   \item{fragmentation}{Distance-weighted fragmentation of the network
#'     after deleting the node. Higher means a more disruptive removal.}
#'   \item{kpath}{Number of simple paths of length at most
#'     \code{kpath_len} that the node lies on, endpoints included.}
#'   \item{epc}{Edge percolated component: mean size of the node's
#'     component over \code{epc_runs} bond-percolation realisations, as a
#'     share of the network. A Monte Carlo estimate.}
#'   \item{length_scaled_betweenness}{Betweenness with each separated pair
#'     weighted by \eqn{1 / d(s,t)}.}
#'   \item{delta_betweenness}{Betweenness with the pair weight
#'     \eqn{(d(s,t) - 1)^{-\delta}} (\code{betweenness_delta}).}
#'   \item{ego_betweenness}{Betweenness inside the node's own ego network.}
#'   \item{delta_closeness}{\eqn{\sum_j d_{ij}^{-\delta} / (n-1)}
#'     (\code{closeness_delta}).}
#'   \item{truss, mdd}{Node truss number (k-2 triangles convention) and
#'     mixed-degree shell threshold (\code{mdd_lambda}). Both use the
#'     simple undirected skeleton; see \code{\link{centrality_truss}}.}
#'   \item{bridging_coefficient, godfather, support}{Reciprocal-degree
#'     ratio, count of unconnected neighbour pairs, and count of
#'     triangle-supported relationships on the simple undirected skeleton.}
#'   \item{volume}{Sum of degrees in the closed \code{volume_radius}-hop
#'     neighbourhood on the simple undirected skeleton.}
#'   \item{mcc}{Maximal clique centrality: sum of \eqn{(|C|-1)!} over
#'     incident maximal cliques of size at least two. Costly; see
#'     \code{\link{centrality_mcc}} for isolate and precision conventions.}
#'   \item{diffusion_centrality}{Finite-horizon weighted outgoing walks:
#'     \eqn{\sum_{t=1}^{T}(qA)^t\mathbf{1}}, with \code{diffusion_q} and
#'     \code{diffusion_steps}. Distinct from diffusion degree.}
#'   \item{dynamical_importance}{Relative spectral-radius loss on vertex
#'     deletion, evaluated by repeated eigendecomposition. Costly; see
#'     \code{\link{centrality_dynamical_importance}} for zero-radius graphs.}
#'   \item{dynamics_sensitive}{Finite-time spreading score including
#'     \code{ds_beta}, \code{ds_mu} and \code{ds_steps}; uses the simple
#'     undirected skeleton.}
#'   \item{malatya}{Sum of focal-to-neighbour degree ratios on the simple
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
#'     over non-backtracking walks of up to \code{nd_order} steps; the
#'     Zoo's neighbor distance centrality at the defaults. See
#'     \code{\link{centrality_neighbor_distance}}.}
#'   \item{ira}{Steady state of a unit resource repeatedly reallocated to
#'     neighbors in proportion to their \code{ira_mass}; conserved, so the
#'     scores of a component sum to its size. Warns
#'     \code{cograph_no_converge} where no steady state exists. See
#'     \code{\link{centrality_ira}}.}
#'   \item{iira}{The same recursion with each share scaled by
#'     \eqn{1-(1-\beta)^{k_i}} for the \code{iira_beta} spreading rate,
#'     run \code{iira_steps} times.
#'     Decays geometrically, so only the order is meaningful. See
#'     \code{\link{centrality_iira}}.}
#'   \item{lnc}{Local neighbor contribution: the cubed degree times the
#'     binomial own-contribution factor \eqn{(1-1/d_i)^{d_i-1}} times the
#'     neighbors' degree sum over \eqn{n-1}. Parameter-free; raw scores
#'     depend on the whole graph's order. See \code{\link{centrality_lnc}}.}
#'   \item{ked}{KED method: the degree times one plus the normalised
#'     entropy of the neighbours' degrees times \eqn{\exp(K_i/N)} for the
#'     neighbour-degree sum \eqn{K_i} and the whole graph's order
#'     \eqn{N}. Parameter-free. See \code{\link{centrality_ked}}.}
#'   \item{hcc}{Hybrid characteristic centrality: the extended degree
#'     \eqn{\delta k_i+(1-\delta)\sum_{j\in N(i)}k_j} over its maximum,
#'     plus the E-shell peeling round in which the node leaves over the
#'     number of rounds. Raw scores lie in \eqn{[0,2]} and are not
#'     component-local. See \code{\link{centrality_hcc}}.}
#'   \item{ehcc}{Extended hybrid characteristic centrality: the
#'     closed-neighborhood sum of \code{hcc}, the focal node counted once.
#'     See \code{\link{centrality_ehcc}}.}
#'   \item{lhc}{Lhc index: the degree-and-triangle-share influence
#'     \eqn{C(v)=\sum_{u\in\Phi(v)}k_u(1+TP(u))/d^2(uv)} over the ball of
#'     radius \code{lhc_radius}, summed over the open neighborhood. The
#'     triangle share is normalized by \eqn{TNTS=\sum_u NTS(u)}, three
#'     times the number of distinct triangles, and is written as zero on a
#'     triangle-free graph. Raw scores are not component-local. See
#'     \code{\link{centrality_lhc}}.}
#'   \item{iec}{Immediate effects centrality: the reciprocal mean length
#'     of the influence sequences that end at a node,
#'     \eqn{(n-1)/\sum_{i\neq j}m_{ij}} for the mean first passage times
#'     \eqn{M=(I-Z+EZ_{dg})\mathrm{diag}(1/c)} of the influence chain
#'     \eqn{W=A/\mathrm{rowSums}(A)} built with \eqn{a_{ii}=1}.
#'     Direction-sensitive and costly (one eigenproblem and two dense
#'     solves). \code{NA} at every node when the chain is reducible or the
#'     graph has one node. Not the same measure as \code{markov}. See
#'     \code{\link{centrality_iec}}.}
#'   \item{dil}{Degree and importance of lines: the degree plus the share
#'     of each incident line's importance \eqn{I_e=(k_m-p-1)(k_n-p-1)/
#'     (p/2+1)} that the node's own degree claims,
#'     \eqn{k_i+\sum_{j\in\Gamma_i}I_{e_{ij}}(k_i-1)/(k_i+k_j-2)}, with
#'     \eqn{p} the number of triangles on the line. Two-hop local and
#'     component-local; never below the node's degree. See
#'     \code{\link{centrality_dil}}.}
#'   \item{trust_pagerank}{Trust-PageRank: a damped PageRank whose split of
#'     a node's score among its neighbors is the column-stochastic
#'     trust-value \eqn{T(i,j)=(1-k)s(i,j)/\sum_{l\in N_j}s(j,l)+
#'     k\,d_i/\sum_{l\in N_j}d_l}, with \eqn{s} the fixed point of SimRank
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
#'   \item{relative_entropy}{Normalised geometric mean of several index
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
#' @export
#' @examples
#' # Built-in edge-list data
#' data(student_interactions)
#' centrality(student_interactions)
#'
#' # Matrix input also works
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality(adj)
#'
#' # Specific measures
#' centrality(adj, measures = c("degree", "betweenness"))
#'
#' # Directed network with normalization
#' centrality(adj, mode = "in", normalized = TRUE)
#'
#' # Sort by pagerank
#' centrality(adj, sort_by = "pagerank", digits = 3)
#'
#' # PageRank with custom damping
#' centrality(adj, measures = "pagerank", damping = 0.9)
#'
#' # Harmonic centrality (better for disconnected graphs)
#' centrality(adj, measures = "harmonic")
#'
#' # Global transitivity
#' centrality(adj, measures = "transitivity", transitivity_type = "global")
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
  stopifnot(is.logical(tna_network), length(tna_network) == 1L, !is.na(tna_network))
  if (!is.null(psych_network)) {
    stopifnot(is.logical(psych_network), length(psych_network) == 1L, !is.na(psych_network))
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
    stop("damping must be between 0 and 1", call. = FALSE)
  }

  # Native graph context (R/kernels-graph.R): dense weights, canonical edge
  # order, loops and duplicate edges resolved once, no igraph needed.
  cg <- .cg_graph(x, directed = directed, loops = loops, simplify = simplify)

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
      stop("Unknown measures: ", paste(invalid, collapse = ", "),
           "\nAvailable: ", paste(all_measures, collapse = ", "), call. = FALSE)
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
                           "delta_closeness")
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

    # Normalize if requested (except for closeness which is handled by igraph)
    if (normalized && m != "closeness" && any(!is.na(value))) {
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
      stop("sort_by column '", sort_by, "' not found in results", call. = FALSE)
    }
    df <- df[order(df[[sort_by]], decreasing = TRUE), ]
    rownames(df) <- NULL
  }

  df
}

# Calculate diffusion centrality (vectorized). For each node, sums the
# scaled degrees of itself and its neighbors.

#' Calculate Onnela-style weighted clustering coefficient (matches tna)
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
  # Degrees at `mode` (a loop counts as igraph counts it), and the neighbour
  # set at `mode` -- a vertex with a loop is its own neighbour, as in the
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
#' @param g igraph object
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
    stop("The k parameter must be greater than 0.", call. = FALSE)
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
#' @param g igraph object
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

  # Degrees count a loop twice (igraph's convention), and the neighbour
  # list igraph reported includes the vertex itself for a loop -- once on
  # a directed graph, twice on an undirected one -- so the neighbour-degree
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
#' @param g igraph object
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
#' @param g igraph object
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
    warning("Graph is not connected; current-flow closeness undefined for disconnected nodes")
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
#' @param g igraph object
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
    warning("Graph is not connected; current-flow betweenness undefined")
    return(rep(NA_real_, n))
  }

  L_pinv <- .cg_laplacian_pinv(cg, weights)
  if (is.null(L_pinv)) return(rep(NA_real_, n)) # nocov
  # Throughput is read off the graph's own weights when weights are given
  # and off the binary adjacency otherwise, which is what the igraph-era
  # code did (the Laplacian above always sees weights).
  A_mat <- if (is.null(weights)) cg$b else cg$w

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
#' @param g igraph object
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
#' @param g igraph object
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
      stop("states vector length must match number of nodes", call. = FALSE)
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
#'   neighbours (local types only).
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
      # igraph's normalisation: multiply by the number of vertices reached.
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
      a <- .cg_attr_matrix(cg, weights)
      diag(a) <- 0
      out <- .cg_alpha(a, n, alpha = 1)
      if (anyNA(out)) stop("the system (I - alpha A) is singular")
      out
    }),
    "power" = .cg_solve_or_stop("power", function() {
      b <- cg$b
      diag(b) <- 0
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
                              damping = damping, personalized = personalized),
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

    # Batch 11 — parameterised family members (R/centrality-batch11.R)
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

    stop("Unknown measure: ", measure, call. = FALSE)
  )

  # Remove names to ensure consistent output
  unname(value)
}

#' Degree Centrality
#'
#' Number of edges connected to each node. For directed networks,
#' \code{centrality_indegree} counts incoming edges and
#' \code{centrality_outdegree} counts outgoing edges.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{normalized}, \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of degree values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_strength}} for the weighted version.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_degree(adj)
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

#' Strength Centrality (Weighted Degree)
#'
#' Sum of edge weights connected to each node. For directed networks,
#' \code{centrality_instrength} sums incoming weights and
#' \code{centrality_outstrength} sums outgoing weights.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of strength values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_degree}} for the unweighted version.
#'
#' @export
#' @examples
#' mat <- matrix(c(0, .5, .3, .5, 0, .8, .3, .8, 0), 3, 3)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C")
#' centrality_strength(mat)
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
#' Fraction of shortest paths passing through each node. Nodes with high
#' betweenness act as bridges connecting different parts of the network.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{normalized}, \code{weighted}, \code{directed}, \code{cutoff},
#'   \code{invert_weights}).
#'
#' @return Named numeric vector of betweenness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_load}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_betweenness(adj)
centrality_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "betweenness", ...)
  stats::setNames(df$betweenness, df$node)
}

#' Closeness Centrality
#'
#' Inverse of the average shortest path distance from a node to all others.
#' For directed networks, \code{centrality_incloseness} and
#' \code{centrality_outcloseness} measure incoming and outgoing closeness.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_harmonic}} for a variant that handles disconnected
#'   graphs.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_closeness(adj)
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
#' Influence-based centrality where a node's score depends on the scores
#' of its neighbors. Nodes connected to other high-scoring nodes get
#' higher scores.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of eigenvector centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_pagerank}} for a random walk variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_eigenvector(adj)
centrality_eigenvector <- function(x, ...) {
  df <- centrality(x, measures = "eigenvector", ...)
  stats::setNames(df$eigenvector, df$node)
}

#' PageRank Centrality
#'
#' Random walk centrality measuring node importance. Simulates a random
#' walker that follows edges with probability \code{damping} and jumps to a
#' random node with probability \code{1 - damping}.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param damping Damping factor (probability of following an edge). Default 0.85.
#' @param personalized Named numeric vector for personalized PageRank.
#'   Values should sum to 1. Default \code{NULL} (uniform).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of PageRank values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_eigenvector}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_pagerank(adj)
#' centrality_pagerank(adj, damping = 0.9)
centrality_pagerank <- function(x, damping = 0.85, personalized = NULL, ...) {
  df <- centrality(x, measures = "pagerank",
                   damping = damping, personalized = personalized, ...)
  stats::setNames(df$pagerank, df$node)
}

#' HITS Authority and Hub Scores
#'
#' Kleinberg's HITS algorithm. \code{centrality_authority} scores nodes
#' pointed to by good hubs. \code{centrality_hub} scores nodes that point
#' to good authorities.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of authority or hub scores.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_authority(adj)
#' centrality_hub(adj)
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
#' Maximum shortest path distance from a node to any other node.
#' For directed networks, \code{centrality_ineccentricity} and
#' \code{centrality_outeccentricity} use incoming and outgoing paths.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of eccentricity values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_eccentricity(adj)
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

#' K-Core Decomposition (Coreness)
#'
#' Assigns each node to its maximum k-core. A k-core is a maximal subgraph
#' where every node has at least k connections within the subgraph.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of coreness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_coreness(adj)
centrality_coreness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "coreness", mode = mode, ...)
  col <- paste0("coreness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Burt's Constraint
#'
#' Network constraint measuring the extent to which a node's connections are
#' redundant. Low constraint indicates access to structural holes (brokerage
#' opportunities).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of constraint values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_constraint(adj)
centrality_constraint <- function(x, ...) {
  df <- centrality(x, measures = "constraint", ...)
  stats::setNames(df$constraint, df$node)
}

#' Local Transitivity (Clustering Coefficient)
#'
#' Proportion of triangles around each node relative to the number of
#' possible triangles. Measures how tightly clustered a node's neighborhood is.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param transitivity_type Type of transitivity: \code{"local"} (default),
#'   \code{"global"}, \code{"undirected"}, \code{"localundirected"},
#'   \code{"barrat"} (weighted), \code{"weighted"}, or \code{"onnela"}.
#'   \code{"onnela"} computes the Onnela / Holme weighted clustering
#'   coefficient on the symmetrized matrix and matches
#'   \code{tna::centralities(., "Clustering")} byte-for-byte. Auto-set
#'   to \code{"onnela"} when \code{tna_network = TRUE} (passed via
#'   \code{...}) and the user did not pass an explicit value.
#' @param isolates How to handle isolate nodes: \code{"nan"} (default) or
#'   \code{"zero"}.
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of transitivity values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_transitivity(adj)
centrality_transitivity <- function(x, transitivity_type = "local",
                                    isolates = "nan", ...) {
  df <- centrality(x, measures = "transitivity",
                   transitivity_type = transitivity_type, isolates = isolates, ...)
  stats::setNames(df$transitivity, df$node)
}

#' Harmonic Centrality
#'
#' Sum of inverse shortest path distances to all other nodes. Unlike closeness,
#' harmonic centrality handles disconnected graphs naturally (unreachable nodes
#' contribute 0 instead of making the measure undefined).
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of harmonic centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_closeness}} for the traditional variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_harmonic(adj)
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
#' Sum of scaled degrees of a node and its neighbors, measuring the node's
#' potential for spreading information through the network.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}. Only used when \code{diffusion_method = "kandhway_kuri"}
#'   (the default for non-tna inputs); ignored under \code{"power_series"},
#'   which always treats the matrix as the row transition operator.
#' @param lambda Scaling factor for neighbor contributions. Default 1. Only
#'   used when \code{diffusion_method = "kandhway_kuri"}.
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{diffusion_method}, \code{loops}, \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of diffusion centrality values.
#'
#' @details
#' Two methods are supported. \code{"kandhway_kuri"} (Kandhway & Kuri, 2014)
#' computes the 1-hop binary-degree neighborhood sum and is the default for
#' raw matrices, igraph objects, and other non-tna inputs.
#' \code{"power_series"} computes
#' \eqn{\mathrm{rowSums}(P + P^2 + \ldots + P^n)} on the weighted matrix
#' (with \code{diag(P) := 0} when \code{loops = FALSE}) and matches
#' \code{tna::centralities(., measures = "Diffusion")} byte-for-byte.
#' For tna inputs, the default switches to \code{"power_series"} to match
#' user expectation; pass \code{diffusion_method = "kandhway_kuri"} to
#' force the binary-degree formula.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_diffusion(adj)
centrality_diffusion <- function(x, mode = "all", lambda = 1, ...) {
  df <- centrality(x, measures = "diffusion", mode = mode, lambda = lambda, ...)
  col <- paste0("diffusion_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Leverage Centrality
#'
#' Measures a node's influence over its neighbors based on relative degree
#' differences. Positive values indicate the node has more connections than
#' its average neighbor.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of leverage centrality values (range -1 to 1).
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_leverage(adj)
centrality_leverage <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "leverage", mode = mode, ...)
  col <- paste0("leverage_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Geodesic K-Path Centrality
#'
#' Count of nodes reachable within shortest path distance \code{k}. Measures
#' how many nodes a given node can reach quickly.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param k Maximum path length. Default 3.
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}, \code{invert_weights}).
#'
#' @return Named numeric vector of k-reach centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_kreach(adj, k = 2)
centrality_kreach <- function(x, mode = "all", k = 3, ...) {
  df <- centrality(x, measures = "kreach", mode = mode, k = k, ...)
  col <- paste0("kreach_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Alpha (Katz) Centrality
#'
#' Influence via all paths penalized by distance. Similar to eigenvector
#' centrality but includes an exogenous contribution, making it well-defined
#' even for directed acyclic graphs.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of alpha centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_eigenvector}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_alpha(adj)
centrality_alpha <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "alpha", mode = mode, ...)
  col <- paste0("alpha_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Bonacich Power Centrality
#'
#' Measures influence based on connections to other influential nodes.
#' The power parameter controls whether connections to well-connected
#' nodes increase or decrease centrality.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of power centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_eigenvector}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_power(adj)
centrality_power <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "power", mode = mode, ...)
  col <- paste0("power_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Subgraph Centrality
#'
#' Participation in closed loops (walks), weighting shorter loops more heavily.
#' Based on the diagonal of the matrix exponential of the adjacency matrix.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of subgraph centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_subgraph(adj)
centrality_subgraph <- function(x, ...) {
  df <- centrality(x, measures = "subgraph", ...)
  stats::setNames(df$subgraph, df$node)
}

#' Laplacian Centrality
#'
#' Energy drop from the graph Laplacian when a node is removed
#' (Qi et al. 2012). Measures a node's importance to the overall
#' network energy.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of Laplacian centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_laplacian(adj)
centrality_laplacian <- function(x, ...) {
  df <- centrality(x, measures = "laplacian", ...)
  stats::setNames(df$laplacian, df$node)
}

#' Load Centrality
#'
#' Fraction of all shortest paths passing through a node, similar to
#' betweenness but weighting paths by 1/count (Goh et al. 2001).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of load centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} for the standard variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_load(adj)
centrality_load <- function(x, ...) {
  df <- centrality(x, measures = "load", ...)
  stats::setNames(df$load, df$node)
}

#' Current Flow Closeness Centrality
#'
#' Information centrality based on electrical current flow through the network.
#' Uses the pseudoinverse of the Laplacian matrix. Requires a connected graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of current flow closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_closeness}} for the shortest-path variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_current_flow_closeness(adj)
centrality_current_flow_closeness <- function(x, ...) {
  df <- centrality(x, measures = "current_flow_closeness", ...)
  stats::setNames(df$current_flow_closeness, df$node)
}

#' Current Flow Betweenness Centrality
#'
#' Betweenness based on electrical current flow rather than shortest paths.
#' Uses the Laplacian pseudoinverse. Requires a connected graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of current flow betweenness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} for the shortest-path variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_current_flow_betweenness(adj)
centrality_current_flow_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "current_flow_betweenness", ...)
  stats::setNames(df$current_flow_betweenness, df$node)
}

#' VoteRank Centrality
#'
#' Identifies influential spreaders via an iterative voting mechanism.
#' Returns normalized rank (1 = most influential). Based on
#' Zhang et al. (2016).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of VoteRank values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_voterank(adj)
centrality_voterank <- function(x, ...) {
  df <- centrality(x, measures = "voterank", ...)
  stats::setNames(df$voterank, df$node)
}

#' Percolation Centrality
#'
#' Importance for spreading processes using node states. Each node has
#' a state (0-1) representing how activated it is. When all states are
#' equal, equivalent to betweenness.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param states Named numeric vector of node states (0-1). Default \code{NULL}
#'   (all nodes get state 1).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of percolation centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} which this generalizes.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_percolation(adj)
#' centrality_percolation(adj, states = c(A = 0.8, B = 0.2, C = 0.5))
centrality_percolation <- function(x, states = NULL, ...) {
  df <- centrality(x, measures = "percolation", states = states, ...)
  stats::setNames(df$percolation, df$node)
}

# =============================================================================
# Extended centrality convenience wrappers
# =============================================================================

#' Radiality Centrality
#'
#' Centrality based on sum of (diameter + 1 - distance) normalized by n-1.
#' Nodes closer to others (on average) have higher radiality.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of radiality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_closeness}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_radiality(adj)
centrality_radiality <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "radiality", mode = mode, ...)
  col <- paste0("radiality_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Lin Centrality
#'
#' Reachable nodes squared divided by sum of distances. Well-defined for
#' disconnected graphs.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Lin centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_closeness}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_lin(adj)
centrality_lin <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "lin", mode = mode, ...)
  col <- paste0("lin_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Decay Centrality
#'
#' Sum of delta^d over all nodes, where d is the shortest path distance.
#' Nodes near many others get higher scores. The \code{decay_parameter}
#' controls the distance penalty.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param decay_parameter Numeric between 0 and 1. Default 0.5.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of decay centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_decay(adj, decay_parameter = 0.5)
centrality_decay <- function(x, mode = "all", decay_parameter = 0.5, ...) {
  df <- centrality(x, measures = "decay", mode = mode,
                   decay_parameter = decay_parameter, ...)
  col <- paste0("decay_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Residual Closeness Centrality
#'
#' Sum of 1/2^d for all nodes, including self. Robust to disconnected graphs.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of residual closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_dangalchev}} (alias).
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_residual_closeness(adj)
centrality_residual_closeness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "residual_closeness", mode = mode, ...)
  col <- paste0("residual_closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Dangalchev Closeness Centrality
#'
#' Alias for residual closeness centrality: sum of 1/2^d.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Dangalchev closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_residual_closeness}} (equivalent).
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_dangalchev(adj)
centrality_dangalchev <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "dangalchev", mode = mode, ...)
  col <- paste0("dangalchev_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Generalized Closeness Centrality
#'
#' Sum of alpha^d over all nodes. Generalization of decay centrality
#' matching tidygraph's implementation.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param decay_parameter Numeric between 0 and 1 (the alpha parameter).
#'   Default 0.5.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of generalized closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_decay}} (equivalent formulation).
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_generalized_closeness(adj)
centrality_generalized_closeness <- function(x, mode = "all",
                                             decay_parameter = 0.5, ...) {
  df <- centrality(x, measures = "generalized_closeness", mode = mode,
                   decay_parameter = decay_parameter, ...)
  col <- paste0("generalized_closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Harary Centrality
#'
#' Sum of 1/d^2 over all reachable node pairs. Robust to disconnected graphs.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Harary centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_harary(adj)
centrality_harary <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "harary", mode = mode, ...)
  col <- paste0("harary_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Average Distance Centrality
#'
#' Sum of shortest path distances divided by (n + 1). Lower values indicate
#' more central nodes.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of average distance values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_average_distance(adj)
centrality_average_distance <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "average_distance", mode = mode, ...)
  col <- paste0("average_distance_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Barycenter Centrality
#'
#' Inverse of the total distance to all reachable nodes.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of barycenter centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_barycenter(adj)
centrality_barycenter <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "barycenter", mode = mode, ...)
  col <- paste0("barycenter_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Wiener Index Centrality
#'
#' Total sum of shortest path distances from a node to all others.
#' Higher values indicate less central (more peripheral) nodes.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Wiener index values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_wiener(adj)
centrality_wiener <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "wiener", mode = mode, ...)
  col <- paste0("wiener_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Closeness Vitality
#'
#' Drop in the Wiener index when a node is removed. Higher values indicate
#' more critical nodes for overall connectivity.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of closeness vitality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_closeness_vitality(adj)
centrality_closeness_vitality <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "closeness_vitality", mode = mode, ...)
  col <- paste0("closeness_vitality_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Lobby Index (H-Index of Neighborhood)
#'
#' Largest k such that the node's closed neighborhood contains at least k
#' nodes with degree >= k. Network analogue of the h-index.
#'
#' @inheritParams centrality_degree
#'
#' @return Named integer vector of lobby index values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_lobby(adj)
centrality_lobby <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "lobby", mode = mode, ...)
  col <- paste0("lobby_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Entropy Centrality
#'
#' Graph-theoretic entropy based on shortest path distribution in the residual
#' graph after removing the node.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of entropy centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_entropy(adj)
centrality_entropy <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "entropy", mode = mode, ...)
  col <- paste0("entropy_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Semi-Local Centrality
#'
#' Triple-nested neighborhood computation measuring 4-hop local influence.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of semi-local centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_semilocal(adj)
centrality_semilocal <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "semilocal", mode = mode, ...)
  col <- paste0("semilocal_", mode)
  stats::setNames(df[[col]], df$node)
}

#' ClusterRank Centrality
#'
#' Product of clustering coefficient and sum of (neighbor degree + 1).
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of ClusterRank values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_clusterrank(adj)
centrality_clusterrank <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "clusterrank", mode = mode, ...)
  col <- paste0("clusterrank_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Bottleneck Centrality
#'
#' Number of shortest path trees where the node appears in more than n/4 paths.
#'
#' @inheritParams centrality_degree
#'
#' @return Named integer vector of bottleneck centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_bottleneck(adj)
centrality_bottleneck <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "bottleneck", mode = mode, ...)
  col <- paste0("bottleneck_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Centroid Value
#'
#' Minimum difference between own and competitor's closer-node count.
#' Measures how much a node is at the center of the graph.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of centroid values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_centroid(adj)
centrality_centroid <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "centroid", mode = mode, ...)
  col <- paste0("centroid_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Maximum Neighborhood Component (MNC)
#'
#' Size of the largest connected component in the node's neighborhood subgraph.
#'
#' @inheritParams centrality_degree
#'
#' @return Named integer vector of MNC values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_dmnc}} for the density variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_mnc(adj)
centrality_mnc <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "mnc", mode = mode, ...)
  col <- paste0("mnc_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Density of Maximum Neighborhood Component (DMNC)
#'
#' Edges divided by nodes raised to \code{dmnc_epsilon}, both taken from the
#' largest connected component of the subgraph induced on a node's
#' neighbours (the focal node excluded).
#'
#' @inheritParams centrality_degree
#' @param dmnc_epsilon Numeric. Epsilon exponent for DMNC. Default 1.7 as
#'   recommended by Lin et al. (2008). centiserve uses 1.67 (four-community
#'   assumption). Must be between 1 and 2.
#'
#' @return Named numeric vector of DMNC values.
#'
#' @section Divergence from centiserve:
#' \code{centiserve::dmnc()} returns different values, and not only because
#' of its different \code{epsilon} default. Its edge count is taken with
#' \code{induced.subgraph(graph, which(c$membership \%in\% ...))}, where the
#' membership vector indexes the neighbourhood subgraph but is used to
#' subset the original graph. The two index spaces are not the same, so the
#' edges counted are those of an unrelated vertex set. On the Zachary karate
#' club the two disagree on 14 of 34 nodes at a matched epsilon, and
#' reproducing that indexing exactly reproduces centiserve's output.
#' cograph counts the edges of the component it actually found.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_mnc}} for the size-only variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_dmnc(adj)
centrality_dmnc <- function(x, mode = "all", dmnc_epsilon = 1.7, ...) {
  df <- centrality(x, measures = "dmnc", mode = mode,
                   dmnc_epsilon = dmnc_epsilon, ...)
  col <- paste0("dmnc_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Local Average Connectivity (LAC)
#'
#' Average degree of neighbors within the neighborhood subgraph. Measures
#' how interconnected a node's neighbors are. Proposed by Li et al. (2011)
#' for identifying essential proteins in PPI networks.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of LAC values.
#'
#' @references
#' Li, M., Wang, J., Chen, X., Wang, H., & Pan, Y. (2011). A local average
#' connectivity-based method for identifying essential proteins from the network
#' level. \emph{Computational Biology and Chemistry}, 35(3), 143-150.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_dmnc}} for another neighborhood density measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_lac(adj)
centrality_lac <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "lac", mode = mode, ...)
  col <- paste0("lac_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Communicability Centrality
#'
#' Total communicability: row sums of the matrix exponential of the adjacency
#' matrix. Measures a node's ability to broadcast information through all paths.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of communicability values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_subgraph}} for the diagonal-only variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_communicability(adj)
centrality_communicability <- function(x, ...) {
  df <- centrality(x, measures = "communicability", ...)
  stats::setNames(df$communicability, df$node)
}

#' Communicability Betweenness Centrality
#'
#' Fraction of total communicability that passes through each node.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of communicability betweenness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_communicability_betweenness(adj)
centrality_communicability_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "communicability_betweenness", ...)
  stats::setNames(df$communicability_betweenness, df$node)
}

#' Random Walk Centrality
#'
#' Inverse sum of random walk distances. Requires a connected graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of random walk centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_random_walk(adj)
centrality_random_walk <- function(x, ...) {
  df <- centrality(x, measures = "random_walk", ...)
  stats::setNames(df$random_walk, df$node)
}

#' Stress Centrality
#'
#' Number of shortest paths passing through each node. Unlike betweenness,
#' does not normalize by the total number of shortest paths.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of stress centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} for the normalized variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_stress(adj)
centrality_stress <- function(x, ...) {
  df <- centrality(x, measures = "stress", ...)
  stats::setNames(df$stress, df$node)
}

#' Flow Betweenness Centrality
#'
#' Max-flow based betweenness centrality.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of flow betweenness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} for shortest-path variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_flow_betweenness(adj)
centrality_flow_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "flow_betweenness", ...)
  stats::setNames(df$flow_betweenness, df$node)
}

#' Expected Influence (one-step)
#'
#' Signed-weight sum of a node's edges (Robinaugh, Millner & McNally 2016).
#' The appropriate centrality for networks with positive *and* negative
#' edges (partial-correlation, glasso, signed correlation networks) where
#' treating negative edges as positive magnitudes can be misleading.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param mode One of "all", "in", "out" for directed graphs. Default "out".
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of expected-influence values (signed).
#'
#' @references Robinaugh DJ, Millner AJ, McNally RJ (2016). Identifying
#'   highly influential nodes in the complicated grief network.
#'   \emph{Journal of Abnormal Psychology}, 125(6), 747-757.
#'
#' @seealso \code{\link{centrality_expected_influence_2}} for the two-step
#'   variant, \code{\link{centrality_strength}} for the weighted-degree analogue.
#'
#' @export
#' @examples
#' # Signed weight matrix (partial correlations, for example)
#' W <- matrix(c( 0.0,  0.5, -0.3,  0.2,
#'                0.5,  0.0,  0.4, -0.1,
#'               -0.3,  0.4,  0.0,  0.6,
#'                0.2, -0.1,  0.6,  0.0), 4, 4, byrow = TRUE)
#' rownames(W) <- colnames(W) <- c("A", "B", "C", "D")
#' centrality_expected_influence_1(W)
centrality_expected_influence_1 <- function(x, mode = "out", ...) {
  df <- centrality(x, measures = "expected_influence_1", mode = mode, ...)
  stats::setNames(df$expected_influence_1, df$node)
}

#' Expected Influence (two-step)
#'
#' Two-step signed-weight sum: a node's own expected influence (EI1) plus
#' the weighted sum of its neighbors' EI1 (Robinaugh, Millner & McNally
#' 2016). Captures both the node's direct influence and the influence it
#' exerts indirectly via highly-connected neighbors.
#'
#' @inheritParams centrality_expected_influence_1
#'
#' @return Named numeric vector of two-step expected-influence values.
#'
#' @references Robinaugh DJ, Millner AJ, McNally RJ (2016). Identifying
#'   highly influential nodes in the complicated grief network.
#'   \emph{Journal of Abnormal Psychology}, 125(6), 747-757.
#'
#' @seealso \code{\link{centrality_expected_influence_1}} for the one-step
#'   variant.
#'
#' @export
#' @examples
#' W <- matrix(c( 0.0,  0.5, -0.3,  0.2,
#'                0.5,  0.0,  0.4, -0.1,
#'               -0.3,  0.4,  0.0,  0.6,
#'                0.2, -0.1,  0.6,  0.0), 4, 4, byrow = TRUE)
#' rownames(W) <- colnames(W) <- c("A", "B", "C", "D")
#' centrality_expected_influence_2(W)
centrality_expected_influence_2 <- function(x, mode = "out", ...) {
  df <- centrality(x, measures = "expected_influence_2", mode = mode, ...)
  stats::setNames(df$expected_influence_2, df$node)
}

#' Topological Coefficient
#'
#' Fraction of shared second-order neighbors, measuring topological overlap
#' between a node and its neighbors.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of topological coefficient values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_topological_coefficient(adj)
centrality_topological_coefficient <- function(x, ...) {
  df <- centrality(x, measures = "topological_coefficient", ...)
  stats::setNames(df$topological_coefficient, df$node)
}

#' Bridging Centrality
#'
#' Product of betweenness and bridging coefficient. Identifies nodes that
#' bridge communities.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of bridging centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_localized_bridging}} for the ego-network variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_bridging(adj)
centrality_bridging <- function(x, ...) {
  df <- centrality(x, measures = "bridging", ...)
  stats::setNames(df$bridging, df$node)
}

#' Local Bridging Centrality
#'
#' (1/degree) times bridging coefficient. Local measure of inter-community
#' connectivity.
#' This legacy score differs from Nanda and Kotz's ego-betweenness product;
#' use \code{\link{centrality_localized_bridging}} for their LBC definition.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of local bridging values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_bridging}} for the betweenness-weighted variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_local_bridging(adj)
centrality_local_bridging <- function(x, ...) {
  df <- centrality(x, measures = "local_bridging", ...)
  stats::setNames(df$local_bridging, df$node)
}

#' Effective Size (Burt's)
#'
#' Network effective size: degree minus redundancy. Measures non-redundant
#' contacts in ego network.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of effective size values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_constraint}} for a related structural holes measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_effective_size(adj)
centrality_effective_size <- function(x, ...) {
  df <- centrality(x, measures = "effective_size", ...)
  stats::setNames(df$effective_size, df$node)
}

#' Diversity Centrality
#'
#' Shannon entropy of the edge weight distribution per node. Measures
#' how evenly a node distributes its connections.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of diversity centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' mat <- matrix(c(0, .5, .3, .5, 0, .8, .3, .8, 0), 3, 3)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C")
#' centrality_diversity(mat)
centrality_diversity <- function(x, ...) {
  df <- centrality(x, measures = "diversity", ...)
  stats::setNames(df$diversity, df$node)
}

#' Cross-Clique Connectivity
#'
#' Count of all cliques (not just maximal) containing each node. Measures
#' embeddedness in dense substructures.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named integer vector of cross-clique counts.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_cross_clique(adj)
centrality_cross_clique <- function(x, ...) {
  df <- centrality(x, measures = "cross_clique", ...)
  stats::setNames(df$cross_clique, df$node)
}

#' Markov Centrality
#'
#' Inverse of column means of the mean first passage time matrix.
#' Requires a connected graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of Markov centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_markov(adj)
centrality_markov <- function(x, ...) {
  df <- centrality(x, measures = "markov", ...)
  stats::setNames(df$markov, df$node)
}

#' Integration Centrality
#'
#' Distance-based influence: sum of 1 - (d-1)/max(d) over all nodes.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of integration centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_integration(adj)
centrality_integration <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "integration", mode = mode, ...)
  col <- paste0("integration_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Expected Centrality
#'
#' Sum of neighbor degrees. Simple but effective influence proxy.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of expected centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_expected(adj)
centrality_expected <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "expected", mode = mode, ...)
  col <- paste0("expected_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Gil-Schmidt Power Index
#'
#' Sum of 1/d(v,w) normalized by (n-1). Variant of closeness using harmonic
#' mean of distances.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Gil-Schmidt power index values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_harmonic}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_gilschmidt(adj)
centrality_gilschmidt <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "gilschmidt", mode = mode, ...)
  col <- paste0("gilschmidt_", mode)
  stats::setNames(df[[col]], df$node)
}

#' SALSA Authority Centrality
#'
#' Stochastic Approach for Link-Structure Analysis. Returns authority scores.
#' Requires a directed graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#'   Must be directed.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of SALSA authority scores.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_authority}} for HITS authority.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_salsa(adj)
centrality_salsa <- function(x, ...) {
  df <- centrality(x, measures = "salsa", ...)
  stats::setNames(df$salsa, df$node)
}

#' LeaderRank Centrality
#'
#' PageRank variant with a ground node connected to all nodes.
#' Requires a directed graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#'   Must be directed.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of LeaderRank values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_pagerank}} for standard PageRank.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_leaderrank(adj)
centrality_leaderrank <- function(x, ...) {
  df <- centrality(x, measures = "leaderrank", ...)
  stats::setNames(df$leaderrank, df$node)
}

#' Participation Coefficient
#'
#' Measures diversity of inter-community connections. Nodes connecting to
#' many communities have high participation. Requires community membership.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param membership Integer vector of community assignments (one per node).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of participation coefficient values (0-1).
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_within_module_z}} for within-community connectivity.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' centrality_participation(adj, membership = c(1, 1, 1, 2, 2))
centrality_participation <- function(x, membership = NULL, mode = "all", ...) {
  df <- centrality(x, measures = "participation", mode = mode,
                   membership = membership, ...)
  col <- paste0("participation_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Within-Module Degree Z-Score
#'
#' Z-score of intra-community connectivity. High values indicate hubs
#' within their own community. Requires community membership.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param membership Integer vector of community assignments (one per node).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of within-module z-score values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_participation}} for between-community diversity.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' centrality_within_module_z(adj, membership = c(1, 1, 1, 2, 2))
centrality_within_module_z <- function(x, membership = NULL, mode = "all", ...) {
  df <- centrality(x, measures = "within_module_z", mode = mode,
                   membership = membership, ...)
  col <- paste0("within_module_z_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Gateway Coefficient
#'
#' Inter-community brokerage weighted by centrality. Combines participation
#' with degree information. Requires community membership.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param membership Integer vector of community assignments (one per node).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of gateway coefficient values (0-1).
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_participation}} for the simpler participation
#'   coefficient.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' centrality_gateway(adj, membership = c(1, 1, 1, 2, 2))
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
#' Katz (1953) status index: \eqn{C = (I - \alpha A^T)^{-1} \mathbf{1}}.
#' Each node's score sums attenuated walks of every length back to it, with
#' attenuation \eqn{\alpha} applied per step. Rankings are identical to
#' Bonacich's alpha centrality with a uniform exogenous vector.
#'
#' Equivalence is verified bit-exact against \code{centiserve::katzcent}
#' (cograph mirrors centiserve's exact LAPACK call sequence) and at machine
#' epsilon against \code{igraph::alpha_centrality(exo = 1)} and
#' \code{networkx.katz_centrality_numpy}.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param katz_alpha Attenuation factor. Must satisfy
#'   \eqn{\alpha < 1 / \rho(A)} where \eqn{\rho(A)} is the spectral radius.
#'   Default 0.1 matches centiserve and NetworkX conventions.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of Katz centrality values.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_eigenvector}},
#'   \code{\link{centrality_pagerank}}.
#' @references
#' Katz, L. (1953). A new status index derived from sociometric analysis.
#' \emph{Psychometrika}, 18(1), 39-43.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_katz(adj)
centrality_katz <- function(x, katz_alpha = 0.1, ...) {
  df <- centrality(x, measures = "katz", katz_alpha = katz_alpha, ...)
  stats::setNames(df$katz, df$node)
}


#' Hubbell Centrality
#'
#' Hubbell (1965) input-output centrality:
#' \eqn{C = (I - w W)^{-1} \mathbf{1}}, where \eqn{W} is the (weighted)
#' adjacency matrix and \eqn{w} is a weight factor that must satisfy
#' \eqn{w \cdot \rho(W) < 1} for the system to be solvable.
#'
#' Bit-exact match against \code{centiserve::hubbell} when edge weights are
#' passed explicitly (cograph mirrors centiserve's full-inverse LAPACK call
#' path).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param hubbell_weight Attenuation factor \eqn{w}. Default 0.5. If
#'   \eqn{w \cdot \rho(W) \ge 1}, the function returns \code{NA} with a warning.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of Hubbell centrality values (or \code{NA} if
#'   the system is not solvable).
#'
#' @section Note on centiserve equivalence:
#' \code{centiserve::hubbell(g, weights = NULL)} silently resets all edge
#' weights to 1, ignoring the graph's weight attribute. To reproduce cograph's
#' values with centiserve on a weighted graph, pass
#' \code{weights = igraph::E(g)$weight} explicitly.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_katz}}.
#' @references
#' Hubbell, C. H. (1965). An input-output approach to clique identification.
#' \emph{Sociometry}, 28(4), 377-399.
#'
#' @export
#' @examples
#' # Small weighted path graph; spectral radius permits weightfactor = 0.5
#' adj <- matrix(0, 4, 4)
#' adj[1,2] <- adj[2,1] <- adj[2,3] <- adj[3,2] <- adj[3,4] <- adj[4,3] <- 0.3
#' rownames(adj) <- colnames(adj) <- LETTERS[1:4]
#' centrality_hubbell(adj, hubbell_weight = 0.5)
centrality_hubbell <- function(x, hubbell_weight = 0.5, ...) {
  df <- centrality(x, measures = "hubbell", hubbell_weight = hubbell_weight, ...)
  stats::setNames(df$hubbell, df$node)
}


#' Information Centrality (Stephenson-Zelen)
#'
#' Information centrality (Stephenson & Zelen 1989) measures a node's
#' importance in terms of the "information" contained in all paths (not only
#' shortest) passing through it. Defined via the inverse of a Laplacian-like
#' matrix, yielding per-node
#' \eqn{IC_i = 1 / (C_{ii} + (\mathrm{tr}(C) - 2 R_i) / n)} where
#' \eqn{C = A^{-1}} and \eqn{R_i} is the row sum of \eqn{C}.
#'
#' Bit-exact match against \code{sna::infocent} on connected undirected
#' graphs (cograph mirrors sna's exact construction and call sequence).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of information centrality values.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_current_flow_closeness}}.
#' @references
#' Stephenson, K., & Zelen, M. (1989). Rethinking centrality: Methods and
#' examples. \emph{Social Networks}, 11(1), 1-37.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,1, 0,1,1,0), 4, 4)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:4]
#' centrality_information(adj)
centrality_information <- function(x, ...) {
  df <- centrality(x, measures = "information", ...)
  stats::setNames(df$information, df$node)
}


#' Pairwise Disconnectivity (Potapov et al. 2008)
#'
#' For a directed network, \code{pairwisedis(v)} is the fraction of ordered
#' reachable pairs \eqn{(s, t)} that become unreachable when node \eqn{v} is
#' removed:
#' \deqn{PD(v) = (|P(G)| - |P(G - v)|) / |P(G)|}
#' where \eqn{|P(G)|} is the number of ordered pairs \eqn{(s, t), s \ne t}
#' with a directed path from \eqn{s} to \eqn{t}.
#'
#' Bit-exact match against \code{centiserve::pairwisedis} on directed
#' graphs. Requires the input to be directed; returns \code{NA} with a
#' warning on undirected inputs.
#'
#' @param x Directed network input (matrix, igraph, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of pairwise disconnectivity values in \eqn{[0, 1]}.
#'
#' @seealso \code{\link{centrality}}, \code{\link{robustness}}.
#' @references
#' Potapov, A. P., Voss, N., Sasse, N., & Wingender, E. (2008). Topology of
#' mammalian transcription networks. \emph{Genome Informatics}, 18, 193-204.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_pairwisedis(adj)
centrality_pairwisedis <- function(x, ...) {
  df <- centrality(x, measures = "pairwisedis", ...)
  stats::setNames(df$pairwisedis, df$node)
}


#' Local Reaching Centrality (Mones, Vicsek & Vicsek 2012)
#'
#' Local reaching centrality measures how much of the network is reachable
#' from a node.
#'
#' \itemize{
#'   \item Directed unweighted: \eqn{LRC(v) = |\{u : u \ne v, v \to u\}| / (N - 1)}.
#'   \item Undirected unweighted: average of \eqn{1/d(v, u)} over all
#'     \eqn{u \ne v}, divided by \eqn{N - 1}. Numerically equal to
#'     \code{igraph::harmonic_centrality(normalized = TRUE)}.
#'   \item Weighted: NetworkX convention, where edge weights are interpreted
#'     as strengths and path length is \eqn{\sum_e (\mathrm{total\_weight} / w_e)}.
#'     Per-path score is the mean of original edge weights along the shortest
#'     path.
#' }
#'
#' Bit-exact match against \code{networkx.local_reaching_centrality} across
#' all three branches. Bit-exact match against
#' \code{igraph::harmonic_centrality(normalized = TRUE)} for the undirected
#' unweighted branch. See \code{\link{reaching_global}} for the graph-level
#' hierarchy measure derived from per-node LRC.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of local reaching centrality values.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_harmonic}},
#'   \code{\link{reaching_global}}.
#' @references
#' Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for complex
#' networks. \emph{PLoS ONE}, 7(3), e33799.
#'
#' @export
#' @examples
#' # Directed path A -> B -> C
#' adj <- matrix(c(0,1,0, 0,0,1, 0,0,0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_reaching_local(adj, mode = "out")
centrality_reaching_local <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "reaching_local", mode = mode, ...)
  col <- paste0("reaching_local_", mode)
  stats::setNames(df[[col]], df$node)
}


#' Global Reaching Centrality (Mones, Vicsek & Vicsek 2012)
#'
#' A graph-level hierarchy measure computed from per-node local reaching
#' centralities:
#' \deqn{GRC(G) = \frac{1}{N - 1} \sum_v \left( \max_u LRC(u) - LRC(v) \right)}
#'
#' Values close to 0 indicate a flat network (all nodes reach equal
#' proportions of the graph); values close to 1 indicate strong hierarchical
#' structure. Matches \code{networkx.global_reaching_centrality} exactly.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality_reaching_local}}.
#'
#' @return A single numeric value in \eqn{[0, 1]}.
#'
#' @seealso \code{\link{centrality_reaching_local}}, \code{\link{summarize_network}}.
#' @references
#' Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for complex
#' networks. \emph{PLoS ONE}, 7(3), e33799.
#'
#' @export
#' @examples
#' # Star graph: highly hierarchical (directed out from center)
#' adj <- matrix(0, 5, 5)
#' adj[1, 2:5] <- 1
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' reaching_global(adj, mode = "out")
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
#' Directed-graph prestige measure: for each node \eqn{v}, the number of
#' other nodes that can reach \eqn{v} via a directed path.
#' \deqn{\mathrm{domain}(v) = |\{u \ne v : u \to^* v\}|}
#'
#' Bit-exact match against \code{sna::prestige(cmode = "domain")}.
#' Directed-only; returns \code{NA} with a warning on undirected input.
#'
#' @param x Directed network input (matrix, igraph, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of domain prestige values in
#'   \eqn{\{0, 1, \ldots, N - 1\}}.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_reaching_local}}
#'   for the dual "out-reachability" measure, \code{\link{centrality_pairwisedis}}
#'   for a related reachability-based directed measure.
#' @references
#' Wasserman, S., & Faust, K. (1994). \emph{Social Network Analysis: Methods
#' and Applications}. Cambridge University Press.
#'
#' @export
#' @examples
#' # Directed 3-cycle: every node reaches every other node
#' adj <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_prestige_domain(adj)
centrality_prestige_domain <- function(x, ...) {
  df <- centrality(x, measures = "prestige_domain", ...)
  stats::setNames(df$prestige_domain, df$node)
}


#' Domain Proximity Prestige
#'
#' Distance-weighted variant of domain prestige. For each directed node
#' \eqn{v}:
#' \deqn{PD(v) = R_v^2 / (D_v \cdot (n - 1))}
#' where \eqn{R_v} is the number of other nodes that reach \eqn{v}, and
#' \eqn{D_v} is the sum of geodesic distances from those reachers to
#' \eqn{v}. A node that is reachable quickly from many others scores high;
#' unreachable nodes score 0.
#'
#' Bit-exact match against \code{sna::prestige(cmode = "domain.proximity")}
#' on strongly connected directed graphs. Directed-only; returns \code{NA}
#' with a warning on undirected input.
#'
#' @section Divergence from sna on disconnected graphs:
#' sna's formula computes \code{(counts > 0) * gdist} element-wise and then
#' sums to get the denominator. For any pair where \code{gdist = Inf}
#' (unreachable), R evaluates \code{FALSE * Inf = NaN}, so the entire
#' denominator becomes \code{NaN} and sna zeros every node via
#' \code{p[is.nan(p)] <- 0}. cograph masks with \code{is.finite()} before
#' summing, producing mathematically correct values on any directed graph,
#' including those with disconnected components.
#'
#' @param x Directed network input (matrix, igraph, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of domain proximity prestige values in
#'   \eqn{[0, 1]}.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_prestige_domain}}
#'   for the unweighted count, \code{\link{centrality_reaching_local}}
#'   for the dual out-reachability measure.
#' @references
#' Wasserman, S., & Faust, K. (1994). \emph{Social Network Analysis: Methods
#' and Applications}. Cambridge University Press.
#'
#' @export
#' @examples
#' # Directed 3-cycle: each node is reached by both others at distance 1 and 2
#' adj <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_prestige_domain_proximity(adj)
centrality_prestige_domain_proximity <- function(x, ...) {
  df <- centrality(x, measures = "prestige_domain_proximity", ...)
  stats::setNames(df$prestige_domain_proximity, df$node)
}


# ---------------------------------------------------------------------------
# Batch 5 wrappers: Gould-Fernandez brokerage (5 roles).
# ---------------------------------------------------------------------------

#' Gould-Fernandez Brokerage — Coordinator Role
#'
#' Coordinator brokerage (w_I): count of open directed 2-paths
#' \eqn{A \to V \to A} passing through node \eqn{V}, where all three nodes
#' belong to \eqn{V}'s group. The broker mediates contact between two
#' in-group members.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "w_I"]}. Counts
#' OPEN 2-paths only — those where no direct edge from \code{a} to \code{c}
#' exists. Directed-only; returns \code{NA} with a warning on undirected input.
#'
#' @param x Directed network input (matrix, igraph, cograph_network, tna object).
#' @param membership Integer or character vector of group assignments, length
#'   equal to the number of nodes. Required.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named integer vector of coordinator role counts.
#'
#' @seealso \code{\link{centrality}},
#'   \code{\link{centrality_brokerage_itinerant}},
#'   \code{\link{centrality_brokerage_representative}},
#'   \code{\link{centrality_brokerage_gatekeeper}},
#'   \code{\link{centrality_brokerage_liaison}}.
#' @references
#' Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A
#' formal approach to brokerage in transaction networks.
#' \emph{Sociological Methodology}, 19, 89-126.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_coordinator(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_coordinator <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_coordinator",
                   membership = membership, ...)
  stats::setNames(df$brokerage_coordinator, df$node)
}

#' Gould-Fernandez Brokerage — Itinerant (Consultant) Role
#'
#' Itinerant brokerage (w_O): count of open directed 2-paths
#' \eqn{A \to V \to A} where the two endpoints are in the same group but
#' the broker \eqn{V} is in a different group. The broker mediates within
#' another group as an outsider.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "w_O"]}.
#' Directed-only.
#'
#' @inheritParams centrality_brokerage_coordinator
#' @return Named integer vector of itinerant role counts.
#' @seealso \code{\link{centrality_brokerage_coordinator}}.
#' @references Gould & Fernandez (1989).
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_itinerant(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_itinerant <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_itinerant",
                   membership = membership, ...)
  stats::setNames(df$brokerage_itinerant, df$node)
}

#' Gould-Fernandez Brokerage — Representative Role
#'
#' Representative brokerage (b_IO): count of open directed 2-paths
#' \eqn{A \to V \to B} where \eqn{A} and \eqn{V} are in the same group
#' and \eqn{B} is in a different group. The broker represents their group
#' outward.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "b_IO"]}.
#' Directed-only.
#'
#' @inheritParams centrality_brokerage_coordinator
#' @return Named integer vector of representative role counts.
#' @seealso \code{\link{centrality_brokerage_coordinator}}.
#' @references Gould & Fernandez (1989).
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_representative(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_representative <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_representative",
                   membership = membership, ...)
  stats::setNames(df$brokerage_representative, df$node)
}

#' Gould-Fernandez Brokerage — Gatekeeper Role
#'
#' Gatekeeper brokerage (b_OI): count of open directed 2-paths
#' \eqn{A \to V \to B} where \eqn{V} and \eqn{B} are in the same group
#' and \eqn{A} is in a different group. The broker acts as a gate letting
#' in-group members receive contact from outside.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "b_OI"]}.
#' Directed-only.
#'
#' @inheritParams centrality_brokerage_coordinator
#' @return Named integer vector of gatekeeper role counts.
#' @seealso \code{\link{centrality_brokerage_coordinator}}.
#' @references Gould & Fernandez (1989).
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_gatekeeper(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_gatekeeper <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_gatekeeper",
                   membership = membership, ...)
  stats::setNames(df$brokerage_gatekeeper, df$node)
}

#' Gould-Fernandez Brokerage — Liaison Role
#'
#' Liaison brokerage (b_O): count of open directed 2-paths
#' \eqn{A \to V \to B} where all three nodes belong to different groups.
#' The broker mediates between two groups to neither of which they belong.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "b_O"]}.
#' Directed-only.
#'
#' @inheritParams centrality_brokerage_coordinator
#' @return Named integer vector of liaison role counts.
#' @seealso \code{\link{centrality_brokerage_coordinator}}.
#' @references Gould & Fernandez (1989).
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_liaison(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_liaison <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_liaison",
                   membership = membership, ...)
  stats::setNames(df$brokerage_liaison, df$node)
}


#' Calculate Edge Centrality Measures
#'
#' Computes centrality measures for edges in a network and returns a tidy
#' data frame. Unlike node centrality, these measures describe edge importance.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object)
#' @param measures Which measures to calculate. Default "all" calculates all
#'   available edge measures. Options: "betweenness", "weight", "overlap",
#'   "simmelian", "reciprocity".
#' @param weighted Logical. Use edge weights if available. Default TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param cutoff Maximum path length for betweenness. Default -1 (no limit).
#' @param invert_weights Logical or NULL. Invert weights for path-based measures?
#'   Default NULL (auto-detect: TRUE for tna objects, FALSE otherwise).
#' @param alpha Numeric. Exponent for weight inversion. Default 1.
#' @param digits Integer or NULL. Round numeric columns. Default NULL.
#' @param sort_by Character or NULL. Column to sort by (descending). Default NULL.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A data frame with columns \code{from}, \code{to}, and one column
#'   per requested measure.
#'
#' @details
#' Edge measures available:
#' \describe{
#'   \item{betweenness}{Number of shortest paths passing through the edge.}
#'   \item{weight}{Original edge weight.}
#'   \item{overlap}{Jaccard neighborhood overlap of edge endpoints.}
#'   \item{simmelian}{Number of triangles the edge participates in.}
#'   \item{reciprocity}{Whether the reverse edge exists (directed only).
#'     Adds columns: \code{reciprocated}, \code{reverse_weight},
#'     \code{weight_ratio}.}
#' }
#'
#' @export
#' @examples
#' # Create test network
#' mat <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,0, 0,1,0,0), 4, 4)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
#'
#' # All edge measures
#' edge_centrality(mat)
#'
#' # Just betweenness
#' edge_centrality(mat, measures = "betweenness")
#'
#' # Sort by betweenness to find bridge edges
#' edge_centrality(mat, sort_by = "betweenness")
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
      stop("Unknown edge measures: ", paste(invalid, collapse = ", "),
           "\nAvailable: ", paste(all_measures, collapse = ", "), call. = FALSE)
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
    warning("Reciprocity skipped: only meaningful for directed networks.",
            call. = FALSE)
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
      stop("sort_by column '", sort_by, "' not found in results", call. = FALSE)
    }
    result <- result[order(result[[sort_by]], decreasing = TRUE), ]
    rownames(result) <- NULL
  }

  result
}

#' @rdname edge_centrality
#' @return Named numeric vector of edge betweenness values (named by
#'   \code{"from->to"}).
#' @export
#' @examples
#' mat <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,0, 0,1,0,0), 4, 4)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
#' edge_betweenness(mat)
edge_betweenness <- function(x, ...) {
  df <- edge_centrality(x, measures = "betweenness", ...)
  stats::setNames(df$betweenness, paste(df$from, df$to, sep = "->"))
}
