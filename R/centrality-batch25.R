#' Assemble weighted transitions and starting masses for random walk decay
#' @keywords internal
#' @noRd
calculate_random_walk_decay <- function(cg, weights = NULL, decay = 0.5,
                                        node_weights = NULL,
                                        normalized = FALSE) {
  n <- cg$n
  mass <- node_weights
  if (is.null(mass)) mass <- rep(1, n)
  if (!is.numeric(mass) || length(mass) != n ||
        any(!is.finite(mass)) || any(mass < 0)) {
    stop("rwd_node_weights must contain one finite nonnegative number per node",
         call. = FALSE)
  }
  if (!is.null(names(mass))) {
    labels <- cg$labels
    if (anyDuplicated(names(mass)) || !setequal(names(mass), labels)) {
      stop("rwd_node_weights names must match node names exactly",
           call. = FALSE)
    }
    mass <- mass[match(labels, names(mass))]
  }
  a <- .cg_candidate_adjacency(cg, weights, "random_walk_decay")
  .cg_random_walk_decay(a, decay, unname(mass), normalized)
}

#' Random Walk Decay Centrality
#'
#' Random walk decay centrality (Was, Rahwan and Skibski 2019) sums, over
#' all starting nodes \eqn{u}{u}, the starting weight \eqn{b_u}{b_u} times
#' the expected discounted first arrival of a random walk from \eqn{u}{u}
#' at node \eqn{v}{v}. With \eqn{T_v}{T_v} the first arrival time and
#' \eqn{a}{a} the decay factor \code{rwd_decay},
#' \deqn{RWD_v = \sum_u b_u \, E_u\left[a^{T_v}; T_v < \infty\right].}{
#'   RWD_v = sum_u b_u E_u[a^(T_v); T_v < Inf].}
#'
#' @details
#' The walk follows outgoing edges with probability proportional to their
#' weights, and an undirected edge is traversed in both directions. A walk
#' that reaches a node without outgoing edges stops. Loops are kept as
#' transitions that stay at the node unless \code{loops = FALSE}. The start
#' counts as an arrival at time zero, so an isolated node scores its own
#' starting weight and \code{rwd_decay = 0} returns the starting weights.
#' Edge weights must be finite and nonnegative, and \code{weighted = FALSE}
#' ignores edge weights while keeping \code{rwd_node_weights}. Invalid
#' \code{rwd_decay} or \code{rwd_node_weights}, and raw scores that
#' overflow, raise an error. Example 3 of the paper contains inconsistent
#' numerical values, and the implementation follows Definition 1, equation
#' 6.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param rwd_decay Decay factor \eqn{a}{a}, a finite number in
#'   \eqn{[0, 1)}{[0, 1)}. Default 0.5.
#' @param rwd_node_weights Nonnegative starting weights \eqn{b}{b}, one per
#'   node. \code{NULL} (default) gives every node weight one. A named vector
#'   is matched to the node names.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (use edge weights, default \code{TRUE}) and
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Was, T., Rahwan, T., & Skibski, O. (2019). Random Walk Decay Centrality.
#'   Proceedings of the AAAI Conference on Artificial Intelligence, 33(01),
#'   2197-2204. \doi{10.1609/aaai.v33i01.33012197}.
#' @seealso \code{\link{centrality_pagerank}},
#'   \code{\link{centrality_random_walk}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_random_walk_decay(regulation_net)
centrality_random_walk_decay <- function(x, rwd_decay = 0.5,
                                         rwd_node_weights = NULL, ...) {
  df <- centrality(x, measures = "random_walk_decay", rwd_decay = rwd_decay,
                   rwd_node_weights = rwd_node_weights, ...)
  stats::setNames(df$random_walk_decay, df$node)
}
