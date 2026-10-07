#' Calculate exogenous centrality on a simple binary graph
#' @keywords internal
#' @noRd
calculate_exogenous <- function(cg, mode = "all", base = "reverse_closeness") {
  b <- .cg_mode_weights(.cg_path_matrix(cg, NULL), mode)
  diag(b) <- 0
  .cg_exogenous(b, base, cg$directed && mode != "all")
}

#' Exogenous Centrality
#'
#' Exogenous centrality (Everett and Borgatti 2010) is the total change in
#' a base centrality of the other nodes when the node is deleted:
#' \deqn{E(i) = \sum_{j \ne i} \left[C_G(j) - C_{G-i}(j)\right].}{
#'   E(i) = sum_{j != i} [C_G(j) - C_{G-i}(j)].}
#' The default base is reverse closeness,
#' \eqn{C_H(j) = \sum_{k \ne j} \max(N - d_H(j,k), 0)}{C_H(j) =
#' sum_{k != j} max(N - d_H(j,k), 0)}, with \eqn{N} the number of nodes of
#' the original network. The other bases are raw betweenness and degree.
#'
#' @details
#' The measure uses the simple binary network, so weights, loops and
#' parallel edges are ignored. \code{mode = "all"} uses the undirected
#' skeleton, and \code{"out"} and \code{"in"} use directed paths and
#' degrees. On an undirected network the three modes agree and the degree
#' base returns the degree. On a directed network the out-degree base
#' returns the in-degree and the in-degree base the out-degree. Exogenous
#' betweenness can be negative when a deletion raises the betweenness of
#' the remaining nodes. An isolated node scores 0. \code{normalized = TRUE}
#' divides by the largest score.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode Direction of the base measure: \code{"all"} (default),
#'   \code{"out"} or \code{"in"}.
#' @param exogenous_base Base centrality: \code{"reverse_closeness"}
#'   (default), \code{"betweenness"} or \code{"degree"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Everett, M. G., & Borgatti, S. P. (2010). Induced, endogenous and
#'   exogenous centrality. Social Networks, 32(4), 339-344.
#'   \doi{10.1016/j.socnet.2010.06.004}.
#' @seealso \code{\link{centrality_closeness_vitality}},
#'   \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_exogenous(regulation_net)
centrality_exogenous <- function(x, mode = "all",
                                 exogenous_base = "reverse_closeness", ...) {
  mode <- match.arg(mode, c("all", "out", "in"))
  df <- centrality(x, measures = "exogenous", mode = mode,
                   exogenous_base = exogenous_base, ...)
  stats::setNames(df[[paste0("exogenous_", mode)]], df$node)
}
