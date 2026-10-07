#' LineRank from directed or ordinary undirected line-graph walks
#' @keywords internal
#' @noRd
calculate_linerank <- function(cg, weights = NULL, damping = 0.85,
                               aggregation = "probability",
                               normalized = FALSE) {
  aggregation <- match.arg(aggregation, c("probability", "weight"))
  if (!is.numeric(damping) || length(damping) != 1L ||
        !is.finite(damping) || damping < 0 || damping >= 1) {
    stop("LineRank damping must be finite and in [0,1)", call. = FALSE)
  }
  w <- if (is.null(weights)) rep(1, nrow(cg$edges)) else weights
  if (any(!is.finite(w)) || any(w < 0)) {
    stop("linerank requires finite nonnegative edge weights", call. = FALSE)
  }
  edges <- cg$edges[w > 0, , drop = FALSE]
  w <- w[w > 0]
  n <- cg$n
  m <- length(w)
  out <- numeric(n)
  if (!m) return(out)
  source <- edges[, 1L]
  target <- edges[, 2L]
  if (cg$directed) {
    adjacent <- outer(target, source, "==")
  } else {
    adjacent <- outer(source, source, "==") | outer(source, target, "==") |
      outer(target, source, "==") | outer(target, target, "==")
    diag(adjacent) <- FALSE
  }
  transition <- matrix(0, m, m)
  for (i in seq_len(m)) {
    neighbors <- which(adjacent[i, ])
    if (!length(neighbors)) {
      transition[i, ] <- 1 / m
    } else {
      # The source edge weight cancels from w_i*w_j after row normalization.
      scaled <- w[neighbors] / max(w[neighbors])
      probability <- scaled / sum(scaled)
      if (any(probability == 0)) {
        stop("linerank transition range exceeds double precision",
             call. = FALSE)
      }
      transition[i, neighbors] <- probability
    }
  }
  system <- diag(1, m) - damping * t(transition)
  rhs <- rep((1 - damping) / m, m)
  system[m, ] <- 1
  rhs[m] <- 1
  stationary <- tryCatch(solve(system, rhs), error = function(e) NULL)
  if (is.null(stationary) || any(!is.finite(stationary)) ||
        any(stationary < -1e-10)) {
    stop("linerank stationary system is numerically unstable", call. = FALSE)
  }
  stationary <- pmax(stationary, 0)
  stationary <- stationary / sum(stationary)
  if (aggregation == "weight") {
    edge_weight <- if (normalized) w / max(w) else w
    stationary <- stationary * edge_weight
  }
  for (i in seq_len(m)) {
    out[source[i]] <- out[source[i]] + stationary[i]
    out[target[i]] <- out[target[i]] + stationary[i]
  }
  if (any(!is.finite(out))) {
    stop("linerank raw scores overflow; use normalized = TRUE", call. = FALSE)
  }
  out
}

#' LineRank Centrality
#'
#' LineRank (Kang et al. 2011) computes PageRank on the line graph, whose
#' nodes are the edges of the network, and gives each node the sum of the
#' stationary probabilities of its incident edges. In a directed network
#' edge \eqn{e}{e} leads to edge \eqn{f}{f} when the target of \eqn{e}{e}
#' is the source of \eqn{f}{f}. In an undirected network two edges are
#' adjacent when they share an endpoint (Kosa et al. 2015).
#'
#' @details
#' The walk on the line graph moves to an adjacent edge with probability
#' proportional to that edge's weight and jumps to a uniformly chosen edge
#' with probability \code{1 - damping}. An edge without successors also
#' jumps uniformly. With \code{linerank_aggregation = "probability"} the
#' scores sum to two on a network with edges. With \code{"weight"} each
#' stationary probability is multiplied by its edge weight before
#' aggregation, following the weighted incidence matrix of Algorithm 2 in
#' Kang et al. (2011). A loop contributes twice to its node, and an
#' isolated node scores zero. Edge weights must be finite and nonnegative,
#' and \code{weighted = FALSE} gives every edge weight one. A
#' \code{damping} outside \eqn{[0, 1)}{[0, 1)} raises an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param damping Probability of continuing the walk on the line graph, in
#'   \eqn{[0, 1)}{[0, 1)}. Default 0.85.
#' @param linerank_aggregation \code{"probability"} (default) sums the
#'   stationary edge probabilities. \code{"weight"} multiplies them by the
#'   edge weights first.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (use edge weights, default \code{TRUE}) and
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Kang, U., Papadimitriou, S., Sun, J., & Tong, H. (2011). Centralities in
#'   Large Networks: Algorithms and Observations. Proceedings of the 2011 SIAM
#'   International Conference on Data Mining, 119-130.
#'   \doi{10.1137/1.9781611972818.11}.
#'
#' Kosa, B., Balassi, M., Englert, P., & Kiss, A. (2015). Betweenness versus
#'   Linerank. Computer Science and Information Systems, 12(1), 33-48.
#'   \doi{10.2298/CSIS141101092K}.
#' @seealso \code{\link{centrality_pagerank}},
#'   \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_linerank(regulation_net)
centrality_linerank <- function(x, damping = 0.85,
                                linerank_aggregation = "probability", ...) {
  df <- centrality(x, measures = "linerank", damping = damping,
                   linerank_aggregation = linerank_aggregation, ...)
  stats::setNames(df$linerank, df$node)
}
