# ===========================================================================
# Centrality measure metadata
#
# One source of truth for which measures exist, how they behave, and what
# they cost. `centrality()` builds its tiers from these functions and
# `list_centralities()` reports them, so the two cannot drift apart.
# ===========================================================================

#' Measures that accept a `mode` argument
#'
#' Their output column carries a mode suffix, as in `degree_all`.
#'
#' @return Character vector of measure names.
#' @keywords internal
#' @noRd
.cg_mode_measures <- function() {
  c("degree", "strength", "closeness", "eccentricity", "exogenous",
    "coreness", "harmonic", "diffusion", "leverage", "kreach",
    "alpha", "power",
    # Extended mode measures
    "radiality", "lin", "decay", "residual_closeness",
    "dangalchev", "generalized_closeness", "harary",
    "average_distance", "barycenter", "wiener",
    "lobby", "entropy", "semilocal", "clusterrank",
    "bottleneck", "centroid", "mnc", "dmnc", "lac",
    "closeness_vitality",
    "integration", "expected", "gilschmidt",
    # Community-aware mode measures
    "participation", "within_module_z", "gateway",
    # Zoo batch 2 — mode measures
    "gravity", "collective_influence", "local_hindex",
    "hindex_strength", "onion",
    # Batch 3 — mode measures
    "reaching_local",
    # Batch 7 — Centrality Zoo comparison batch
    "distance_entropy", "local_dimension",
    "local_information_dimension",
    "neighborhood_connectivity",
    # Batch 8 — mode measures
    "community_hub_bridge", "entropy_variation_degree",
    # Batch 9 — mode measures
    "community_based", "comm_centrality", "community_mediator",
    "local_dimension_fixed", "fuzzy_local_dimension",
    "local_volume_dimension", "heatmap", "local_entropy",
    "weighted_h_index", "geodesic_kpath",
    # Batch 10 — cross-package gaps
    "local_efficiency", "fragmentation", "kpath",
    # Batch 11 — parameterised family members
    "delta_closeness",
    # Psychometric family — signed-weight sums
    "expected_influence_1", "expected_influence_2")
}

#' Measures that ignore `mode`
#'
#' Their output column is the bare measure name.
#'
#' @return Character vector of measure names.
#' @keywords internal
#' @noRd
.cg_no_mode_measures <- function() {
  c("betweenness", "eigenvector", "pagerank",
    "authority", "hub", "constraint", "transitivity",
    "subgraph", "laplacian", "load",
    "current_flow_closeness", "current_flow_betweenness",
    "voterank", "percolation",
    # Extended no-mode measures
    "stress", "flow_betweenness",
    "communicability", "communicability_betweenness",
    "random_walk",
    "topological_coefficient", "bridging",
    "local_bridging", "effective_size",
    "diversity", "cross_clique", "markov",
    # Directed-only measures
    "salsa", "leaderrank", "trophic_level",
    # Zoo batch 2 — no-mode measures
    "second_order", "infection", "nonbacktracking",
    "spanning_tree",
    # Batch 3 — classical measures with reference validation
    "katz", "hubbell", "information", "pairwisedis",
    # Batch 4 — directed prestige family (Wasserman-Faust)
    "prestige_domain", "prestige_domain_proximity",
    # Batch 5 — Gould-Fernandez brokerage (5 roles)
    "brokerage_coordinator", "brokerage_itinerant",
    "brokerage_representative", "brokerage_gatekeeper",
    "brokerage_liaison",
    # Batch 7 — Centrality Zoo comparison batch
    "modularity_vitality",
    # Batch 8 — Centrality Zoo "on the way" batch
    "shapley_game1", "shapley_game2", "shapley_game3",
    "access_information", "hide_information", "rumor",
    "entropy_variation_betweenness", "s_shell",
    "degree_discount", "single_discount", "ncvoterank",
    # Batch 9 — no-mode measures
    "wvoterank", "enrenew", "voterank_plus",
    "node_contraction", "node_contraction_improved",
    "two_way_rw", "flow_coefficient", "redundancy",
    "weighted_kshell", "renewed_coreness",
    # Batch 10 — no-mode measures
    "s_core", "epc",
    # Batch 11 — no-mode measures
    "length_scaled_betweenness", "delta_betweenness", "ego_betweenness",
    # Batch 12 — topology-only parameter candidates
    "truss", "mdd", "bridging_coefficient", "godfather", "support",
    # Batch 13 — topology-only parameter candidates
    "volume", "mcc",
    # Batch 14 — outgoing finite-horizon diffusion
    "diffusion_centrality", "dynamical_importance",
    # Batch 15 — dynamics-sensitive spreading on the simple skeleton
    "dynamics_sensitive", "malatya", "resistance_curvature",
    "extended_coreness", "extended_gravity", "cda", "improved_closeness",
    "global_structure", "hybrid_global_structure", "improved_global_structure",
    "weighted_leaderrank", "adaptive_leaderrank", "graph_regularization",
    "random_walk_decay", "linerank", "x_degree", "coleman_theil",
    "bridging_capital", "proximal_betweenness", "expected_force",
    "modified_expected_force", "beta_measure", "localized_bridging",
    "extended_local_bridging", "ninl", "map_equation", "controlrank",
    "spectralrank", "mcgm", "mixed_gravity", "extended_mixed_gravity",
    # Batch 40 — DK-based gravity model
    "dkgm",
    # Batch 42 — neighborhood (neighbor distance) centrality
    "neighbor_distance",
    # Batch 43 — iterative resource allocation and its improved variant
    "ira", "iira",
    # Batch 44 — local neighbor contribution
    "lnc",
    # Batch 45 — KED method
    "ked",
    # Batch 46 — hybrid characteristic centrality and its extension
    "hcc", "ehcc",
    # Batch 47 — randomized shortest paths betweenness
    "rsp_betweenness",
    # Batch 48 — Lhc index
    "lhc",
    # Batch 49 — immediate effects centrality
    "iec",
    # Batch 50 — degree and importance of lines
    "dil",
    # Batch 51 — trust-PageRank
    "trust_pagerank",
    # Batch 41 — relative-entropy integrated evaluation
    "relative_entropy")
}

#' Measures that require a community partition
#'
#' Without `membership` these warn and return `NA`.
#'
#' @return Character vector of measure names.
#' @keywords internal
#' @noRd
.cg_membership_measures <- function() {
  c("participation", "within_module_z", "gateway",
    "brokerage_coordinator", "brokerage_itinerant",
    "brokerage_representative", "brokerage_gatekeeper", "brokerage_liaison",
    "modularity_vitality", "community_hub_bridge", "community_based",
    "comm_centrality", "community_mediator")
}

#' Measures for which a low value marks the more prominent node
#'
#' Every other measure is oriented the usual way, so a high value marks the
#' more prominent node. The orientation follows each measure's defining
#' source; see the measure's own help page for the exact reading.
#'
#' @return Character vector of measure names.
#' @keywords internal
#' @noRd
.cg_lower_is_central <- function() {
  c("eccentricity", "average_distance", "wiener", "constraint",
    "second_order", "heatmap", "local_entropy", "local_dimension",
    "local_dimension_fixed", "local_volume_dimension",
    "access_information", "hide_information")
}

#' Measures whose values change when edge weights are supplied
#'
#' Determined empirically: each measure was computed on the same graph with
#' and without weights, and listed here when the two differ. Everything
#' else reads the topology only, so a weighted input gives the same answer
#' as its unweighted skeleton.
#'
#' @return Character vector of measure names.
#' @keywords internal
#' @noRd
.cg_weighted_measures <- function() {
  c("spectralrank", "controlrank", "map_equation", "alpha", "authority", "average_distance", "barycenter", "betweenness", "cda",
    "bridging", "centroid", "closeness", "closeness_vitality", "constraint",
    "current_flow_betweenness", "current_flow_closeness", "dangalchev",
    "decay", "delta_betweenness", "delta_closeness", "diffusion_centrality",
    "diversity", "dynamical_importance",
    "eccentricity", "eigenvector", "expected_influence_1",
    "expected_influence_2", "flow_betweenness",
    "fragmentation", "generalized_closeness", "graph_regularization",
    "harary", "harmonic",
    "hindex_strength", "hub", "hubbell", "information", "katz", "kreach",
    "length_scaled_betweenness", "lin", "load", "local_efficiency",
    "modularity_vitality", "pagerank", "random_walk_decay", "linerank",
    "coleman_theil",
    "bridging_capital",
    "percolation", "radiality", "reaching_local", "residual_closeness",
    "resistance_curvature", "s_core", "spanning_tree", "strength", "stress", "two_way_rw",
    "weighted_kshell", "wiener", "wvoterank",
    "rsp_betweenness")
}

#' Catalogue of the Centrality Measures
#'
#' A tidy table of every measure \code{\link{centrality}} can compute, with
#' the facts you need before you read a column of results: which end of the
#' scale marks a prominent node, whether the measure needs a community
#' partition, whether it reads edge weights, and whether it is held back
#' from \code{type = "all"} because its cost grows steeply.
#'
#' Twelve measures are oriented so that a **low** value marks the more
#' central node, and sorting their column the usual way puts the periphery on top.
#' Filter with \code{orientation = "lower"} to see them.
#'
#' @param orientation Keep only measures with this orientation:
#'   \code{"higher"} or \code{"lower"}. Default \code{NULL} keeps both.
#' @param costly Keep only costly measures (\code{TRUE}) or only the rest
#'   (\code{FALSE}). Default \code{NULL} keeps both.
#' @param needs_membership Keep only measures that require a partition
#'   (\code{TRUE}) or only those that do not (\code{FALSE}). Default
#'   \code{NULL} keeps both.
#'
#' @return A \code{data.frame} with one row per measure and the columns
#'   \code{measure} (the name to pass to \code{centrality(measures = )}),
#'   \code{orientation} (\code{"higher"} or \code{"lower"}, which end of
#'   the scale marks a prominent node), \code{mode_aware} (whether the
#'   measure accepts \code{mode} and its column carries a mode suffix),
#'   \code{needs_membership}, \code{uses_weights}, and \code{costly}
#'   (held back from \code{type = "all"}; add it with
#'   \code{include = }). Rows are ordered by measure name.
#'
#' @seealso \code{\link{centrality}} to compute them,
#'   \code{\link{centrality_degree}} and the other one-measure verbs.
#'
#' @export
#' @examples
#' # Every measure, with the facts needed to read its column
#' head(list_centralities())
#'
#' # The measures where a low value marks the more central node
#' list_centralities(orientation = "lower")
#'
#' # The measures held back from type = "all"
#' list_centralities(costly = TRUE)
list_centralities <- function(orientation = NULL, costly = NULL,
                              needs_membership = NULL) {
  if (!is.null(orientation)) {
    orientation <- match.arg(orientation, c("higher", "lower"))
  }
  stopifnot(
    "`costly` must be TRUE, FALSE or NULL" =
      is.null(costly) || (is.logical(costly) && length(costly) == 1L),
    "`needs_membership` must be TRUE, FALSE or NULL" =
      is.null(needs_membership) ||
      (is.logical(needs_membership) && length(needs_membership) == 1L)
  )
  mode_aware <- .cg_mode_measures()
  measures <- c(mode_aware, .cg_no_mode_measures())
  out <- data.frame(
    measure = measures,
    orientation = ifelse(measures %in% .cg_lower_is_central(),
                         "lower", "higher"),
    mode_aware = measures %in% mode_aware,
    needs_membership = measures %in% .cg_membership_measures(),
    uses_weights = measures %in% .cg_weighted_measures(),
    costly = measures %in% .cg_costly_measures(),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$measure), ]
  if (!is.null(orientation)) out <- out[out$orientation == orientation, ]
  if (!is.null(costly)) out <- out[out$costly == costly, ]
  if (!is.null(needs_membership)) {
    out <- out[out$needs_membership == needs_membership, ]
  }
  rownames(out) <- NULL
  out
}

#' Run a measure that solves a linear system, or fail with a named condition
#'
#' `alpha` and `power` both invert `I - alpha A`, which is singular when the
#' attenuation sits on an eigenvalue of the adjacency matrix. igraph then
#' raises a bare LU factorization error that names neither the measure nor
#' the cause, so it is translated here.
#'
#' @param measure Measure name, for the message.
#' @param fn Zero-argument function computing the measure.
#' @return The measure's value.
#' @keywords internal
#' @noRd
.cg_solve_or_stop <- function(measure, fn) {
  tryCatch(fn(), error = function(e) {
    stop(errorCondition(
      sprintf(paste0("`%s` could not be computed on this graph: the system ",
                     "(I - alpha A) is singular or numerically unstable ",
                     "here (%s). Try a different attenuation, or a measure ",
                     "that does not invert the adjacency matrix, such as ",
                     "eigenvector or katz."),
              measure, conditionMessage(e)),
      class = "cograph_singular_system", call = NULL
    ))
  })
}

#' Keep a tier request alive when one measure has no value on this input
#'
#' A measure the caller named in `measures =` or `include =` raises its own
#' conditions: the caller asked for that measure, so an undefined result is
#' an error they must see. A measure a *tier* supplied is different --
#' `type = "all"` asks for everything, and one measure without a value on
#' this particular graph must not take the other 178 down with it. Such a
#' measure warns and returns `NA`, exactly as the community-partition
#' measures already do when `membership` is missing.
#'
#' Only `cograph_undefined_index` is caught, the condition a measure raises
#' when its own definition has no value on the input. Every other error
#' propagates.
#'
#' @param measure Measure name, for the message.
#' @param from_tier `TRUE` when a tier supplied the measure rather than the
#'   caller naming it.
#' @param n Vertex count, for the `NA` vector.
#' @param expr Expression computing the measure, evaluated lazily.
#' @return The measure's value, or a vector of `NA`.
#' @keywords internal
#' @noRd
.cg_tier_guard <- function(measure, from_tier, n, expr) {
  if (!from_tier) return(expr)
  na_with_warning <- function(e) {
    warning(warningCondition(
      sprintf("`%s` has no value on this input, so its column is NA. %s",
              measure, conditionMessage(e)),
      class = "cograph_undefined_measure", call = NULL
    ))
    rep(NA_real_, n)
  }
  tryCatch(expr, cograph_needs_igraph = na_with_warning,
           cograph_undefined_index = function(e) {
    warning(warningCondition(
      sprintf("`%s` has no value on this input, so its column is NA. %s",
              measure, conditionMessage(e)),
      class = "cograph_undefined_measure", call = NULL
    ))
    rep(NA_real_, n)
  })
}
