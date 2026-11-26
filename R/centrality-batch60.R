#' Calculate the weighted formal concept analysis centrality
#' @keywords internal
#' @noRd
calculate_wfca <- function(cg, limit = 1e6) {
  a <- .cg_arc_matrix(cg)
  .cg_enumerated("wfca", nrow(a), .cg_wfca(a, limit))
}

#' Weighted Formal Concept Analysis Centrality
#'
#' The weighted formal concept analysis (WFCA) centrality (Sun et al. 2017)
#' reads the network as a table of which node is tied to which, and finds
#' its formal concepts: pairs \eqn{(T, P)} of node sets in which \eqn{P}
#' holds every node that all members of \eqn{T} are tied to, and \eqn{T}
#' holds every node tied to all members of \eqn{P}. The weight of a node is
#' \deqn{W_i = \sum_{(T, P):\, i \in P} \frac{|T|}{|P|}.}{
#'   W_i = sum over the concepts (T, P) with i in P of |T| / |P|.}
#'
#' @details
#' A node gains from every group of nodes that all point to it, and gains
#' more when the group is large and the set of their common targets is
#' small. Direction is used: a tie from \eqn{u} to \eqn{v} makes \eqn{v}
#' an attribute of \eqn{u}. Weights, loops and parallel edges are ignored.
#' Isolated nodes score 0.
#'
#' The number of formal concepts can grow exponentially with the size of
#' the network. The measure is therefore costly, computed only when
#' requested by name or through \code{include}, and it stops once more
#' than \code{enumeration_limit} concepts exist (default one million):
#' every score is then \code{NA} with a \code{cograph_undefined_measure}
#' warning. \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{enumeration_limit} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, or \code{NA} at every node when the enumeration was cut short.
#' @references
#' Sun, Z., Wang, B., Sheng, J., Hu, Y., Wang, Y. and Shao, J. (2017).
#'   Identifying influential nodes in complex networks based on weighted
#'   formal concept analysis. IEEE Access, 5, 3777-3789.
#'   \doi{10.1109/ACCESS.2017.2679038}.
#' @seealso \code{\link{centrality_con}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_wfca(regulation_net)
centrality_wfca <- function(x, ...) {
  df <- centrality(x, measures = "wfca", ...)
  stats::setNames(df$wfca, df$node)
}
