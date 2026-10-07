# ===========================================================================
# Batch 10 — node measures other packages expose and cograph did not
#
# igraph-facing calculators over R/kernels-batch10.R and the exported verbs.
# Every measure here closes a row of docs/CENTRALITY-CROSS-COVERAGE.md.
# ===========================================================================

#' Weighted symmetric view of a graph
#'
#' `.cg_undirected_view()` drops the weights, which the strength-based
#' measures need, so they symmetrize by the stronger of the two directions.
#' @keywords internal
#' @noRd
.cg_weighted_view <- function(b) {
  m <- pmax(b, t(b))
  diag(m) <- 0
  m
}

#' @keywords internal
#' @noRd
calculate_local_efficiency <- function(cg, mode = "all", weights = NULL) {
  if (cg$n == 0L) return(numeric(0))
  b <- .cg_path_matrix(cg, weights)
  .cg_local_efficiency(switch(mode, all = .cg_weighted_view(b),
                              out = b, "in" = t(b)))
}

#' @keywords internal
#' @noRd
calculate_s_core <- function(cg, weights = NULL) {
  if (cg$n == 0L) return(numeric(0))
  .cg_s_core(.cg_weighted_view(.cg_path_matrix(cg, weights)))
}

#' @keywords internal
#' @noRd
calculate_fragmentation <- function(cg, mode = "all", weights = NULL) {
  if (cg$n == 0L) return(numeric(0))
  .cg_fragmentation(.cg_path_matrix(cg, weights), mode)
}

#' @keywords internal
#' @noRd
calculate_kpath <- function(cg, mode = "all", k = 3) {
  if (cg$n == 0L) return(numeric(0))
  .cg_kpath_counts(.cg_mode_neighbours(cg, mode), k = k,
                   directed = cg$directed && mode != "all")
}

#' @keywords internal
#' @noRd
calculate_epc <- function(cg, threshold = 0.5, runs = 1000, seed = NULL) {
  if (cg$n == 0L) return(numeric(0))
  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }
  .cg_epc(.cg_undirected_view(.cg_path_matrix(cg, NULL)),
          threshold = threshold, runs = runs)
}

# ---------------------------------------------------------------------------
# Exported verbs
# ---------------------------------------------------------------------------

#' Local Efficiency, s-Core, Fragmentation, k-Path and EPC
#'
#' Local efficiency (Latora and Marchiori 2001) is the mean of
#' \eqn{1/d_{jl}}{1/d_jl} over ordered pairs of a node's neighbors, with
#' distances measured inside the subgraph induced on those neighbors. The
#' s-core index (Eidsaa and Almaas 2013) is the largest strength threshold
#' \eqn{s} whose s-core contains the node. Fragmentation (Borgatti 2006) is
#' the distance-weighted fragmentation of the network after the node is
#' deleted,
#' \deqn{F_{-v} = 1 - \frac{\sum_{i \ne j} 1/d_{ij}}{(n-1)(n-2)}.}{
#'   F_-v = 1 - sum_{i != j} (1/d_ij) / ((n-1)(n-2)).}
#' The k-path count (Sade 1989) is the number of simple paths of length at
#' most \code{kpath_len} that pass through or end at the node. The edge
#' percolated component (EPC; Lin et al. 2008) is the mean size of the
#' node's component, as a share of all nodes, when each edge is kept with
#' probability \code{1 - epc_threshold}.
#'
#' @details
#' Local efficiency, fragmentation and the k-path count follow \code{mode},
#' and \code{mode = "all"} treats edges as undirected. Local efficiency and
#' fragmentation read edge weights as distances, so with weights below one
#' local efficiency exceeds one and fragmentation is negative;
#' \code{invert_weights = TRUE} converts weights to distances
#' \eqn{1/w^\alpha}{1/w^alpha}. On unweighted input both lie between 0 and
#' 1, and a node with fewer than two neighbors has local efficiency 0.
#' Fragmentation is \code{NaN} on a network with fewer than three nodes.
#' The s-core index symmetrizes the network by the stronger of the two
#' directions and equals the k-core number when all weights are one. The
#' k-path count and EPC ignore weights, and EPC uses the undirected
#' skeleton. EPC is a Monte Carlo estimate, and cytoHubba and
#' \code{centiserve::epc()} report the same quantity multiplied by the
#' number of runs.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction for directed networks: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. Local
#'   efficiency and fragmentation use \code{weighted} (default \code{TRUE}),
#'   \code{invert_weights} (default \code{NULL}, which inverts for tna input
#'   only) and \code{alpha} (inversion exponent, default 1). The s-core
#'   index uses \code{weighted}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Latora, V., & Marchiori, M. (2001). Efficient behavior of small-world
#'   networks. Physical Review Letters, 87(19), 198701.
#'
#' Eidsaa, M., & Almaas, E. (2013). s-core network decomposition: A
#'   generalization of k-core analysis to weighted networks. Physical Review E,
#'   88(6), 062819. \doi{10.1103/PhysRevE.88.062819}.
#'
#' Borgatti, S. P. (2006). Identifying sets of key players in a social
#'   network. Computational and Mathematical Organization Theory, 12(1),
#'   21-34.
#'
#' Sade, D. S. (1989). Sociometrics of Macaca mulatta III: n-path centrality
#'   in grooming networks. Social Networks, 11(3), 273-292.
#'
#' Lin, C.-Y., Chin, C.-H., Wu, H.-H., Chen, S.-H., Ho, C.-W., & Ko, M.-T.
#'   (2008). Hubba: hub objects analyzer, a framework of interactome hubs
#'   identification for network biology. Nucleic Acids Research, 36, W438-W443.
#'   \doi{10.1093/nar/gkn257}.
#' @seealso \code{\link{centrality_coreness}},
#'   \code{\link{centrality_geodesic_kpath}},
#'   \code{\link{network_local_efficiency}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_local_efficiency(regulation_net)
#' centrality_s_core(regulation_net)
#' centrality_fragmentation(regulation_net, weighted = FALSE)
#' centrality_kpath(regulation_net, kpath_len = 2)
#' centrality_epc(regulation_net, epc_runs = 100, epc_seed = 1)
centrality_local_efficiency <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "local_efficiency", mode = mode, ...)
  stats::setNames(df[[paste0("local_efficiency_", mode)]], df$node)
}

#' @rdname centrality_local_efficiency
#' @export
centrality_s_core <- function(x, ...) {
  df <- centrality(x, measures = "s_core", ...)
  stats::setNames(df$s_core, df$node)
}

#' @rdname centrality_local_efficiency
#' @export
centrality_fragmentation <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "fragmentation", mode = mode, ...)
  stats::setNames(df[[paste0("fragmentation_", mode)]], df$node)
}

#' @rdname centrality_local_efficiency
#' @param kpath_len Maximum path length for the k-path count. Default 3.
#'   Length 1 gives the degree in the undirected skeleton.
#' @export
centrality_kpath <- function(x, mode = "all", kpath_len = 3, ...) {
  df <- centrality(x, measures = "kpath", mode = mode,
                   kpath_len = kpath_len, ...)
  stats::setNames(df[[paste0("kpath_", mode)]], df$node)
}

#' @rdname centrality_local_efficiency
#' @param epc_threshold Probability that an edge is removed in one
#'   realization. Default 0.5.
#' @param epc_runs Number of percolation realizations. Default 1000.
#' @param epc_seed Random seed. The default \code{NULL} uses the caller's
#'   random stream, so the estimate varies between calls. A seed gives a
#'   reproducible value and leaves the caller's stream unchanged.
#' @export
centrality_epc <- function(x, epc_threshold = 0.5, epc_runs = 1000,
                           epc_seed = NULL, ...) {
  df <- centrality(x, measures = "epc", epc_threshold = epc_threshold,
                   epc_runs = epc_runs, epc_seed = epc_seed, ...)
  stats::setNames(df$epc, df$node)
}
