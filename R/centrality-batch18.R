#' Prepare an undirected weighted graph for CDA
#' @keywords internal
#' @noRd
calculate_cda <- function(cg, weights = NULL, alpha = 0.5) {
  # A loop never enters the measure, so its weight is dropped before the
  # weight check and the diagonal of the matrix stays zero.
  loops <- cg$edges[, 1L] == cg$edges[, 2L]
  if (is.null(weights)) {
    w <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
    diag(w) <- 0
  } else {
    weights[loops] <- 0
    w <- .cg_candidate_adjacency(cg, weights, "cda")
    if (cg$directed) w <- w + t(w)
    if (any(!is.finite(w))) {
      stop("cda edge-weight sum exceeds double precision", call. = FALSE)
    }
  }
  .cg_cda(w, alpha)
}

#' Clustering Degree Algorithm
#'
#' The clustering degree algorithm (CDA; Wang et al. 2018) scores a node by
#' its clustering degree
#' \eqn{CD_i = [\alpha d_i + (1-\alpha) s_i] / [1 + \exp(-C_i^w)]}{CD_i =
#' (alpha d_i + (1 - alpha) s_i) / (1 + exp(-C_i^w))} plus the clustering
#' degrees of its neighbors, each scaled by the edge weight:
#' \deqn{PC_i = CD_i + \sum_{j \in N(i)} \frac{w_{ij}}{w_{\max}} CD_j.}{
#'   PC_i = CD_i + sum_{j in N(i)} (w_ij / w_max) CD_j.}
#' Here \eqn{d_i} is the degree, \eqn{s_i} the strength, \eqn{C_i^w} the
#' weighted clustering coefficient of Barrat et al., and \eqn{w_{\max}}{w_max}
#' the largest edge weight in the network.
#'
#' @details
#' The measure works on an undirected weighted network. On a directed
#' network the weights of the two arcs between a pair are added, and
#' \code{weighted = FALSE} uses the simple undirected skeleton. Self-loops
#' are removed, and \code{mode} and \code{invert_weights} have no effect.
#' A node with fewer than two neighbors has clustering 0, and an isolated
#' node scores 0. Negative or non-finite weights raise an error. With
#' \code{cda_alpha = 1} the scores are unchanged when every weight is
#' multiplied by a constant, and with \code{cda_alpha = 0} they scale by
#' that constant. On an unweighted network \code{cda_alpha} has no effect.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param cda_alpha Weight \eqn{\alpha}{alpha} of the degree against the
#'   strength, between 0 and 1. Default 0.5, the value of Wang et al.
#'   (2018). A value outside that range raises an error.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#'   uses \code{weighted} (default \code{TRUE}) and \code{normalized}
#'   (default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wang, Q., Ren, J., Wang, Y., Zhang, B., Cheng, Y., & Zhao, X. (2018). CDA:
#'   A Clustering Degree Based Influential Spreader Identification Algorithm in
#'   Weighted Complex Network. IEEE Access, 6, 19550-19559.
#'   \doi{10.1109/ACCESS.2018.2822844}.
#' @seealso \code{\link{centrality_strength}},
#'   \code{\link{centrality_transitivity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_cda(regulation_net)
centrality_cda <- function(x, cda_alpha = 0.5, ...) {
  df <- centrality(x, measures = "cda", cda_alpha = cda_alpha, ...)
  stats::setNames(df$cda, df$node)
}
