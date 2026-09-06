# ===========================================================================
# Batch 10 — node measures other packages expose and cograph did not
#
# igraph-facing calculators over R/kernels-batch10.R and the exported verbs.
# Every measure here closes a row of docs/CENTRALITY-CROSS-COVERAGE.md.
# ===========================================================================

#' Weighted symmetric view of a graph
#'
#' `.cg_undirected_view()` drops the weights, which the strength-based
#' measures need, so they symmetrise by the stronger of the two directions.
#' @keywords internal
#' @noRd
.cg_weighted_view <- function(b) {
  m <- pmax(b, t(b))
  diag(m) <- 0
  m
}

#' @keywords internal
#' @noRd
calculate_local_efficiency <- function(g, mode = "all", weights = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  b <- .cg_path_matrix(g, weights)
  .cg_local_efficiency(switch(mode, all = .cg_weighted_view(b),
                              out = b, "in" = t(b)))
}

#' @keywords internal
#' @noRd
calculate_s_core <- function(g, weights = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_s_core(.cg_weighted_view(.cg_path_matrix(g, weights)))
}

#' @keywords internal
#' @noRd
calculate_fragmentation <- function(g, mode = "all", weights = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_fragmentation(.cg_path_matrix(g, weights), mode)
}

#' @keywords internal
#' @noRd
calculate_kpath <- function(g, mode = "all", k = 3) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_kpath_counts(.cg_mode_neighbours(g, mode), k = k,
                   directed = igraph::is_directed(g) && mode != "all")
}

#' @keywords internal
#' @noRd
calculate_epc <- function(g, threshold = 0.5, runs = 1000, seed = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }
  .cg_epc(.cg_undirected_view(.cg_path_matrix(g, NULL)),
          threshold = threshold, runs = runs)
}

# ---------------------------------------------------------------------------
# Exported verbs
# ---------------------------------------------------------------------------

#' Local efficiency, s-core, fragmentation, k-path census and EPC
#'
#' Five node measures that other centrality packages expose and
#' \code{centrality()} did not. Each is a thin wrapper on
#' \code{\link{centrality}}.
#'
#' \describe{
#'   \item{\code{local_efficiency} (Latora & Marchiori 2001)}{The global
#'     efficiency of the subgraph induced on the node's neighbours, the node
#'     itself removed: the mean of \eqn{1 / d_{jl}} over ordered pairs of
#'     neighbours, with distances measured inside that subgraph. Nodes with
#'     fewer than two neighbours score 0. High values mark a node whose
#'     neighbourhood survives its loss. Matches
#'     \code{igraph::local_efficiency()} and
#'     \code{brainGraph::efficiency(type = "local")}.}
#'   \item{\code{s_core} (Eidsaa & Almaas 2013)}{The weighted k-core: the
#'     largest strength threshold \eqn{s} whose maximal subgraph of nodes
#'     with strength at least \eqn{s} still contains the node. Unit weights
#'     give the k-core number exactly. Uses edge weights.}
#'   \item{\code{fragmentation} (Borgatti 2006)}{Distance-weighted
#'     fragmentation of the network after deleting the node: \eqn{1 - \sum
#'     1/d_{ij} / ((n-1)(n-2))} over the ordered pairs that remain. Higher
#'     means a more disruptive removal. Matches
#'     \code{keyplayer::fragment()} on unweighted input.}
#'   \item{\code{kpath} (Sade 1989)}{The number of simple paths of length at
#'     most \code{kpath_len} (default 3) that the node lies on, endpoints
#'     included; length 1 alone reproduces degree. Matches the per-vertex
#'     column sums of \code{sna::kpath.census()}. Enumeration is exhaustive,
#'     so cost grows with branching factor to the power \code{kpath_len}.}
#'   \item{\code{epc} (Lin et al. 2008)}{Edge percolated component: each
#'     edge survives with probability \code{1 - epc_threshold}, and the
#'     score is the mean size of the node's component over \code{epc_runs}
#'     realisations, as a share of the network. cytoHubba and
#'     \code{centiserve::epc()} divide by the node count alone, so their
#'     number is \code{epc_runs} times this one; the ranking is the same.
#'     A Monte Carlo estimate -- pass \code{epc_seed} for a reproducible
#'     value.}
#' }
#'
#' \code{local_efficiency}, \code{fragmentation} and \code{kpath} follow
#' \code{mode}; \code{s_core} and \code{epc} read the undirected skeleton.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param mode Direction: \code{"all"}, \code{"out"} or \code{"in"}.
#' @param kpath_len Maximum path length for \code{centrality_kpath}.
#'   Default 3.
#' @param epc_threshold Edge removal probability. Default 0.5.
#' @param epc_runs Number of percolation realisations. Default 1000.
#' @param epc_seed Random seed. Default \code{NULL}, which leaves the
#'   caller's stream alone and makes the estimate vary between calls.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node.
#'
#' @references
#' Latora, V., & Marchiori, M. (2001). Efficient behavior of small-world
#'   networks. Physical Review Letters, 87(19), 198701.
#'
#' Eidsaa, M., & Almaas, E. (2013). s-core network decomposition: A
#'   generalization of k-core analysis to weighted networks. Physical Review
#'   E, 88(6), 062819.
#'
#' Borgatti, S. P. (2006). Identifying sets of key players in a social
#'   network. Computational and Mathematical Organization Theory, 12(1),
#'   21-34.
#'
#' Sade, D. S. (1989). Sociometrics of Macaca mulatta III: n-path centrality
#'   in grooming networks. Social Networks, 11(3), 273-292.
#'
#' Lin, C.-Y., Chin, C.-H., Wu, H.-H., Chen, S.-H., Ho, C.-W., & Ko, M.-T.
#'   (2008). Hubba: hub objects analyzer. Nucleic Acids Research, 36,
#'   W438-W443.
#'
#' @seealso \code{\link{centrality_coreness}},
#'   \code{\link{centrality_weighted_kshell}},
#'   \code{\link{centrality_geodesic_kpath}},
#'   \code{\link{network_local_efficiency}}.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_local_efficiency(adj)
#' centrality_s_core(adj)
#' centrality_fragmentation(adj)
#' centrality_kpath(adj, kpath_len = 2)
#' centrality_epc(adj, epc_runs = 50, epc_seed = 1)
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
#' @export
centrality_kpath <- function(x, mode = "all", kpath_len = 3, ...) {
  df <- centrality(x, measures = "kpath", mode = mode,
                   kpath_len = kpath_len, ...)
  stats::setNames(df[[paste0("kpath_", mode)]], df$node)
}

#' @rdname centrality_local_efficiency
#' @export
centrality_epc <- function(x, epc_threshold = 0.5, epc_runs = 1000,
                           epc_seed = NULL, ...) {
  df <- centrality(x, measures = "epc", epc_threshold = epc_threshold,
                   epc_runs = epc_runs, epc_seed = epc_seed, ...)
  stats::setNames(df$epc, df$node)
}
