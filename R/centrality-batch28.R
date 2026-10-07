#' Coleman-Theil hierarchy using Burt's mutual investment weights
#' @keywords internal
#' @noRd
calculate_coleman_theil <- function(cg, weights = NULL) {
  a <- .cg_candidate_adjacency(cg, weights, "coleman_theil")
  diag(a) <- 0
  n <- nrow(a)
  if (!n || !any(a > 0)) return(numeric(n))
  positive <- a > 0
  a <- a / max(a)
  if (any(a[positive] == 0)) {
    stop("coleman_theil weight range exceeds double precision", call. = FALSE)
  }
  mutual <- a + t(a)
  strength <- rowSums(mutual)
  p <- mutual / ifelse(strength > 0, strength, 1)
  if (any(p[mutual > 0] == 0)) {
    stop("coleman_theil investment range exceeds double precision",
         call. = FALSE)
  }
  indirect <- p %*% p
  vapply(seq_len(n), function(i) {
    neighbors <- which(mutual[i, ] > 0)
    size <- length(neighbors)
    if (size == 0L) return(0)
    if (size == 1L) return(1)
    local <- (p[i, neighbors] + indirect[i, neighbors])^2
    deviation <- (local - mean(local)) / mean(local)
    # Treat indistinguishable local constraints as uniform before max scaling.
    if (max(abs(deviation)) <= 16 * .Machine$double.eps) return(0)
    # r log(r) - (r-1), whose linear terms sum to zero. The series avoids
    # cancellation around r=1; its omitted term is below double precision.
    term <- numeric(size)
    small <- abs(deviation) < 1e-3
    u <- deviation[small]
    polynomial <- -1 / 20 + u * (1 / 30 - u / 42)
    polynomial <- -1 / 6 + u * (1 / 12 + u * polynomial)
    term[small] <- u^2 * (1 / 2 + u * polynomial)
    ordinary <- !small & deviation > -1
    u <- deviation[ordinary]
    term[ordinary] <- (1 + u) * log1p(u) - u
    term[deviation <= -1] <- 1
    max(0, min(1, sum(term) / (size * log(size))))
  }, numeric(1))
}

#' Coleman-Theil Hierarchy Index
#'
#' The Coleman-Theil index (Burt 1992) measures how concentrated Burt's
#' dyadic constraint is across the \eqn{d_i}{d_i} contacts of node
#' \eqn{i}{i}. Let \eqn{p_{ij}}{p_ij} be the share of the mutual tie
#' strength of \eqn{i}{i} invested in \eqn{j}{j},
#' \eqn{c_{ij} = (p_{ij} + \sum_q p_{iq} p_{qj})^2}{c_ij = (p_ij + sum_q
#' p_iq p_qj)^2} the constraint, and \eqn{r_{ij}}{r_ij} the constraint
#' divided by its mean over the contacts. Then
#' \deqn{H_i = \frac{\sum_{j \in N(i)} r_{ij} \log r_{ij}}{d_i \log d_i}.}{
#'   H_i = sum_{j in N(i)} r_ij log(r_ij) / (d_i log(d_i)).}
#'
#' @details
#' The mutual tie strength of a pair is the sum of the weights in both
#' directions, so direction is combined and loops are removed. Edge weights
#' must be finite and nonnegative, and \code{weighted = FALSE} gives every
#' edge weight one before the directions are combined. The index lies
#' between zero, for equal constraints, and one, for constraint
#' concentrated on one contact. Following Burt's STRUCTURE 4.2 manual
#' (pages 181-183), an isolated node scores zero and a node with one
#' contact scores one. The organizational and oligopoly multipliers of
#' STRUCTURE are fixed at one. A weight range beyond double precision
#' raises an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (use edge weights, default \code{TRUE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Burt, R. S. (1992). Structural Holes: The Social Structure of Competition.
#'   Harvard University Press. \doi{10.4159/9780674029095}.
#' @seealso \code{\link{centrality_constraint}},
#'   \code{\link{centrality_effective_size}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_coleman_theil(regulation_net)
centrality_coleman_theil <- function(x, ...) {
  df <- centrality(x, measures = "coleman_theil", ...)
  stats::setNames(df$coleman_theil, df$node)
}
