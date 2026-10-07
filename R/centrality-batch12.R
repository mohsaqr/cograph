#' Calculate topology-only measures from the parameter-candidate list
#' @keywords internal
#' @noRd
calculate_candidate_local <- function(cg, measure, mdd_lambda = 0.7) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  switch(measure,
         truss = .cg_truss(b),
         mdd = .cg_mdd(b, mdd_lambda),
         .cg_local_candidates(b, measure))
}

#' Truss, Mixed-Degree Decomposition, Bridging Coefficient, Godfather and Support
#'
#' The truss number of a node (Malliaros et al. 2016) is the largest truss
#' number of an incident edge, where a k-truss requires every edge to lie
#' in at least \eqn{k-2} triangles of the subgraph. Mixed-degree
#' decomposition (Zeng and Zhang 2013) peels nodes by residual degree plus
#' \code{mdd_lambda} times exhausted degree. The bridging coefficient
#' (Hwang et al. 2008) is
#' \eqn{(1/d_i) / \sum_{j \in N(i)} 1/d_j}{(1/d_i) / sum_{j in N(i)} 1/d_j}.
#' The Godfather index (Jackson 2020) counts the unordered pairs of
#' neighbors with no edge between them, and the support (Jackson 2020)
#' counts the neighbors that share at least one common neighbor with the
#' node.
#'
#' @details
#' All five measures use the simple undirected skeleton, so direction,
#' weights, loops and parallel edges are ignored. Isolated nodes score 0.
#' An edge outside every triangle has truss number 2, and a node of a
#' complete graph on \eqn{k} nodes has truss number \eqn{k}, as in
#' NetworkX. Sources that label trusses by the triangle threshold report
#' values two smaller. \code{mdd_lambda = 0} gives the k-core number and
#' \code{mdd_lambda = 1} gives the degree. The bridging coefficient is the
#' factor that bridging centrality multiplies with betweenness. LocalRank
#' (Chen et al. 2012), listed in the Centrality Zoo, is
#' \code{\link{centrality_semilocal}}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Malliaros, F. D., Rossi, M. E. G., & Vazirgiannis, M. (2016).
#' Locating influential nodes in complex networks. Scientific Reports, 6,
#' 19307. \doi{10.1038/srep19307}.
#'
#' Zeng, A., & Zhang, C. J. (2013). Ranking spreaders by decomposing complex
#' networks. Physics Letters A, 377, 1031-1035.
#' \doi{10.1016/j.physleta.2013.02.039}.
#'
#' Hwang, W., Kim, T., Ramanathan, M., & Zhang, A. (2008). Bridging
#' centrality: graph mining from element level to group level. KDD '08,
#' 336-344. \doi{10.1145/1401890.1401934}.
#'
#' Jackson, M. O. (2020). A typology of social capital and associated
#' network measures. Social Choice and Welfare, 54, 311-336.
#' \doi{10.1007/s00355-019-01189-3}.
#'
#' Chen, D., Lu, L., Shang, M. S., Zhang, Y. C., & Zhou, T. (2012).
#' Identifying influential nodes in complex networks. Physica A, 391,
#' 1777-1787. \doi{10.1016/j.physa.2011.09.017}.
#' @seealso \code{\link{centrality_coreness}},
#'   \code{\link{centrality_bridging}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_truss(regulation_net)
#' centrality_mdd(regulation_net)
#' centrality_bridging_coefficient(regulation_net)
#' centrality_godfather(regulation_net)
#' centrality_support(regulation_net)
centrality_truss <- function(x, ...) {
  df <- centrality(x, measures = "truss", ...)
  stats::setNames(df$truss, df$node)
}

#' @rdname centrality_truss
#' @param mdd_lambda Weight of the exhausted degree, between 0 and 1.
#'   Default 0.7, the value of the worked example of Zeng and Zhang (2013).
#'   A value outside that range raises an error.
#' @export
centrality_mdd <- function(x, mdd_lambda = 0.7, ...) {
  df <- centrality(x, measures = "mdd", mdd_lambda = mdd_lambda, ...)
  stats::setNames(df$mdd, df$node)
}

#' @rdname centrality_truss
#' @export
# nolint start: object_length_linter.
centrality_bridging_coefficient <- function(x, ...) {
  df <- centrality(x, measures = "bridging_coefficient", ...)
  stats::setNames(df$bridging_coefficient, df$node)
}
# nolint end: object_length_linter.

#' @rdname centrality_truss
#' @export
centrality_godfather <- function(x, ...) {
  df <- centrality(x, measures = "godfather", ...)
  stats::setNames(df$godfather, df$node)
}

#' @rdname centrality_truss
#' @export
centrality_support <- function(x, ...) {
  df <- centrality(x, measures = "support", ...)
  stats::setNames(df$support, df$node)
}
