#' Prepare an undirected weighted graph for CDA
#' @keywords internal
#' @noRd
calculate_cda <- function(g, weights = NULL, alpha = 0.5) {
  loops <- igraph::which_loop(g)
  if (any(loops)) {
    g <- igraph::delete_edges(g, which(loops))
    if (!is.null(weights)) weights <- weights[!loops]
  }
  if (is.null(weights)) {
    w <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  } else {
    w <- .cg_candidate_adjacency(g, weights, "cda")
    if (igraph::is_directed(g)) w <- w + t(w)
    if (any(!is.finite(w))) {
      stop("cda edge-weight sum exceeds double precision", call. = FALSE)
    }
  }
  .cg_cda(w, alpha)
}

#' Clustering degree algorithm centrality
#'
#' Wang et al.'s CDA returns the propagation-capability score
#' \eqn{PC_i=CD_i+\sum_j(w_{ij}/w_{\max})CD_j}, where
#' \eqn{CD_i=[\alpha d_i+(1-\alpha)s_i]/[1+\exp(-C_i^w)]}.
#' Here d is degree, s is strength, and Cw is Barrat's weighted local
#' clustering coefficient. The maximum edge weight is taken over the
#' whole graph, including other connected components. Inner CD scores
#' remain raw until the final optional normalization of PC.
#'
#' Uses finite nonnegative weights on an undirected graph. Zero-weight
#' edges are absent connections. Clustering is set to zero for nodes with
#' fewer than two positive-weight neighbors; this convention agrees with
#' the source's leaf example. Isolates and edgeless graphs score zero.
#' Weights retain their original units: scaling all weights can change
#' scores and rankings because degree and strength are combined. At alpha
#' zero, uniform weight scaling scales scores proportionally; at alpha
#' one, scores are invariant to that scaling. Binary inputs are independent
#' of alpha because their degree and strength coincide.
#'
#' Self-loops are removed. For weighted directed inputs, opposite arcs
#' are added into undirected edge weights. Parallel weights are combined
#' by \code{simplify} first, with any remaining parallel edges added.
#' Without weights, the simple undirected skeleton is used. These are
#' explicit cograph projections to the source's undirected domain.
#' \code{mode} and shortest-path weight inversion do not affect CDA.
#' Nonfinite intermediate strengths or scores raise an error, including
#' when normalization is requested.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param cda_alpha Degree-versus-strength mixing weight between zero and
#'   one. Default 0.5 follows the source; endpoints select strength and
#'   degree respectively while retaining weighted clustering and neighbor
#'   contributions.
#' @param ... Additional arguments to \code{\link{centrality}}. With
#'   \code{normalized = TRUE}, positive final scores are divided by their
#'   maximum.
#' @return Named numeric vector in input node order.
#' @references
#' Wang, Q., Ren, J., Wang, Y., Zhang, B., Cheng, Y., & Zhao, X. (2018).
#' CDA: A Clustering Degree Based Influential Spreader Identification
#' Algorithm in Weighted Complex Network. IEEE Access, 6, 19550-19559,
#' equations 2-6. \doi{10.1109/ACCESS.2018.2822844}.
#' @export
#' @examples
#' centrality_cda(igraph::make_ring(5), cda_alpha = 0.5)
centrality_cda <- function(x, cda_alpha = 0.5, ...) {
  df <- centrality(x, measures = "cda", cda_alpha = cda_alpha, ...)
  stats::setNames(df$cda, df$node)
}
