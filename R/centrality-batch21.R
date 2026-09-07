#' Calculate global structure models on the simple skeleton
#' @keywords internal
#' @noRd
calculate_global_structure <- function(g, model = "gsm", normalized = FALSE) {
  b <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  diag(b) <- 0
  core <- if (model == "igsm") numeric(nrow(b)) else .cg_mdd(b, 0)
  .cg_global_structure(b, core, model, normalized)
}

#' Global structure model centrality
#'
#' The Global Structure Model (GSM) of Ullah et al. (2021) is
#' \eqn{GSM(i)=\exp(k_s(i)/N)\sum_{j\ne i}k_s(j)/d_{ij}}, where
#' k_s denotes original graph core numbers and d denotes hop distances.
#' It combines a focal coreness factor with distance-discounted coreness
#' of other nodes. N is the total original node count, including isolates.
#'
#' Both GSM and \code{\link{centrality_hybrid_global_structure}} use the
#' simple undirected skeleton, ignoring weights, mode, path inversion and
#' distance cutoffs. Loops are removed and parallel connections count once.
#' Only reachable partners contribute; this is an explicit disconnected-graph
#' extension. Isolates and singletons score zero, empty input returns an
#' empty vector. Other components can affect results through the global
#' node count and, for H-GSM, its global mean. These are not independent
#' per-component calculations.
#'
#' Production uses native coreness and all-pairs distance kernels, with
#' worst-case O(N^3) time and O(N^2) memory. Numerical verification uses
#' independent NetworkX cores/distances and exhaustive small-graph oracles.
#' Agreement with a numerical definition does not establish author-software
#' parity or superior epidemic-spreading predictions.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#'   \code{normalized = TRUE} divides final scores by their maximum.
#' @return Named numeric vector in input node order.
#' @references Ullah, A., Wang, B., Sheng, J., Long, J., Khan, N., & Sun, Z.
#'   (2021). Identification of nodes influence based on global structure
#'   model in complex networks. Scientific Reports, 11, 6173, equations
#'   5-8. \doi{10.1038/s41598-021-84684-x}.
#' @export
#' @examples
#' centrality_global_structure(igraph::make_ring(4))
centrality_global_structure <- function(x, ...) {
  df <- centrality(x, measures = "global_structure", ...)
  stats::setNames(df$global_structure, df$node)
}

#' Hybrid global structure model centrality
#'
#' Mukhtar et al.'s H-GSM (2023) uses
#' \eqn{s_i=\exp(k_s(i)k_i/N)},
#' \eqn{a=\lceil\log_2(N^{-1}\sum_i s_i)\rceil}, and
#' \eqn{H\text{-}GSM(i)=s_i\sum_{j\ne i}s_j/d_{ij}^{a}}.
#' k_i is simple degree, k_s(i) is original coreness, and d is hop distance.
#' The ceiling exponent is computed from the mean self-influence over ALL
#' original nodes, including isolates whose self-influence is one. The
#' factor s_i alone is not the final centrality score.
#'
#' Topology and disconnected-graph conventions are shared with
#' \code{\link{centrality_global_structure}}. The adaptive exponent is
#' used exactly as specified, including its discontinuities at powers of
#' two; it is not smoothed or replaced by a fixed exponent.
#'
#' Self-influence, its mean and final sums are evaluated in logarithmic
#' form. Raw scores exceeding double precision raise an error. With
#' \code{normalized = TRUE}, final scores are computed directly as
#' exponentials of log-score differences, so normalized results remain
#' available even when raw scores overflow. Extremely small normalized
#' ratios may underflow to zero. Normalization is applied to the complete
#' score, not separately to self-influence or neighbor contributions.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Mukhtar, M. F., et al. (2023). Integrating local and global
#'   information to identify influential nodes in complex networks.
#'   Scientific Reports, 13, 11411, equations 6-8.
#'   \doi{10.1038/s41598-023-37570-7}.
#' @export
#' @examples
#' centrality_hybrid_global_structure(igraph::make_ring(4))
centrality_hybrid_global_structure <- function(x, ...) { # nolint: object_length_linter
  df <- centrality(x, measures = "hybrid_global_structure", ...)
  stats::setNames(df$hybrid_global_structure, df$node)
}

#' Improved global structure model centrality
#'
#' The IGSM definition reproduced in Mukhtar et al. (2023), equation 5,
#' is \eqn{IGSM(i)=\exp(k_i/N)\sum_{j\ne i}k_j/d_{ij}^{a}}, with
#' \eqn{a=\lceil\log_2(\overline{k})\rceil}. The original method is
#' attributed to Zhu and Wang (2022); the exact equation used here was
#' checked in the later primary experimental paper, not its original full
#' text. IGSM uses simple degrees rather than GSM's core numbers, and its
#' distance exponent depends on global mean degree, including isolates.
#'
#' Topology, normalization and disconnected-graph conventions follow
#' \code{\link{centrality_global_structure}}. For a positive mean degree
#' below one, the exponent may be zero or negative; it is not clamped.
#' With a negative exponent, more distant reachable partners contribute
#' more, an explicit consequence of extending the equation to sparse
#' disconnected inputs. Unreachable partners still contribute zero.
#' Edgeless graphs score zero by an explicit extension because the
#' logarithm of zero in the exponent is otherwise undefined.
#'
#' This implements IGSM itself, without an additional nearest-neighbor
#' aggregation for the extended IGSM variant.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Zhu, J.-C., & Wang, L.-W. (2022). An extended improved global
#'   structure model for influential node identification in complex
#'   networks. Chinese Physics B, 31, 068904.
#'   \doi{10.1088/1674-1056/ac380d}.
#'   The implemented IGSM formula is reproduced as equation 5 in Mukhtar
#'   et al. (2023), \doi{10.1038/s41598-023-37570-7}.
#' @export
#' @examples
#' centrality_improved_global_structure(igraph::make_ring(4))
centrality_improved_global_structure <- function(x, ...) { # nolint: object_length_linter
  df <- centrality(x, measures = "improved_global_structure", ...)
  stats::setNames(df$improved_global_structure, df$node)
}
