#' Neighborhood (neighbor distance) centrality
#' @keywords internal
#' @noRd
calculate_neighbor_distance <- function(cg, order = 2, decay = 0.2,
                                        mass = "degree") {
  if (!is.numeric(order) || length(order) != 1L || !is.finite(order) ||
        order < 0 || order != trunc(order)) {
    stop("nd_order must be a single nonnegative whole number", call. = FALSE)
  }
  if (!is.numeric(decay) || length(decay) != 1L || !is.finite(decay)) {
    stop("nd_decay must be a single finite number", call. = FALSE)
  }
  mass <- match.arg(mass, c("degree", "coreness"))
  a <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(a) <- 0
  n <- nrow(a)
  if (!n) return(numeric())
  theta <- if (identical(mass, "degree")) {
    rowSums(a)
  } else {
    .cg_coreness(a, n)
  }
  steps <- .cg_nb_walk_sums(a, theta, order)
  if (!length(steps)) return(theta)
  theta + Reduce(`+`, Map(function(s, k) decay^k * s, steps, seq_along(steps)))
}

#' Neighborhood Centrality
#'
#' Neighborhood centrality (Liu et al. 2016) adds to a node's benchmark
#' centrality \eqn{\theta}{theta} the benchmark centrality of the endpoints
#' of its non-backtracking walks of length 1 to \eqn{n}, discounted by
#' \eqn{a^k}{a^k} at step \eqn{k}. With the defaults (degree benchmark, two
#' steps, \eqn{a = 0.2}) it is the neighbor distance centrality.
#' \deqn{C_i = \theta_i + a \sum_{j \in \Gamma_i} \theta_j
#'   + a^2 \sum_{j \in \Gamma_i} \sum_{l \in \Gamma_j \setminus i} \theta_l
#'   + \dots}{
#'   C_i = theta_i + a sum_{j in N(i)} theta_j
#'   + a^2 sum_{j in N(i)} sum_{l in N(j), l != i} theta_l + ...}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the network,
#' so direction, weights, loops and parallel edges are ignored. Each level
#' excludes only the node the walk came from, so a walk may revisit a node.
#' Isolates score \eqn{\theta_i}{theta_i}, which is zero for both
#' benchmarks, and \code{nd_order = 0} returns the benchmark itself. The
#' source takes \eqn{a} in \eqn{[0, 1]}, and the function accepts any finite
#' value. The Centrality Zoo paraphrases the measure with sums over distance
#' shells, which agree with the source's walk sums on trees and differ on
#' graphs with short cycles. The implementation follows the source.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param nd_order Number of steps \eqn{n}, a nonnegative whole number
#'   (default 2).
#' @param nd_decay Per-step decay \eqn{a}, a finite number (default 0.2).
#' @param nd_mass Benchmark centrality \eqn{\theta}{theta}: \code{"degree"}
#'   (default) or \code{"coreness"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Liu, Y., Tang, M., Zhou, T. and Do, Y. (2016). Identify influential
#'   spreaders in complex networks, the role of neighborhood. Physica A:
#'   Statistical Mechanics and its Applications, 452, 289-298.
#'   \doi{10.1016/j.physa.2016.02.028}.
#' @seealso \code{\link{centrality_semilocal}},
#'   \code{\link{centrality_extended_coreness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_neighbor_distance(regulation_net)
centrality_neighbor_distance <- function(x, nd_order = 2, nd_decay = 0.2,
                                         nd_mass = "degree", ...) {
  df <- centrality(x, measures = "neighbor_distance", nd_order = nd_order,
                   nd_decay = nd_decay, nd_mass = nd_mass, ...)
  stats::setNames(df$neighbor_distance, df$node)
}
