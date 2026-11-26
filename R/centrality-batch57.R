#' Calculate the global importance of nodes
#' @keywords internal
#' @noRd
calculate_gin <- function(cg, alpha = 1) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_gin(b, alpha)
}

#' Calculate Icentr
#' @keywords internal
#' @noRd
calculate_icentr <- function(cg) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_icentr(b)
}

#' Calculate the global and local information score
#' @keywords internal
#' @noRd
calculate_gli <- function(cg, radius = 3) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_gli_terms(b, radius)$gli
}

#' Global Importance of Nodes
#'
#' The global importance of nodes (GIN; Zhao et al. 2020) multiplies a
#' factor for the node's own degree by the degrees of all other nodes,
#' each divided by its distance from the node:
#' \deqn{GIN(i) = e^{\alpha k_i / n} \sum_{j \ne i} \frac{k_j}{d_{ij}},}{
#'   GIN(i) = exp(alpha k_i / n) sum_{j != i} k_j / d_ij,}
#' with \eqn{k} the degree, \eqn{d_{ij}}{d_ij} the shortest-path distance
#' and \eqn{n} the number of nodes.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{gin_alpha} is 1 by default, the value of the source. The source
#' also carries a factor \eqn{\beta}{beta} in front of the sum, which
#' rescales every score alike and is fixed at 1 here. A node of degree zero
#' scores 0. A node that cannot be reached adds nothing to the sum, which
#' the sources consulted do not state. The equation was taken from two
#' later papers that reproduce it; the original article was not consulted.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{gin_alpha} (the weight of the node's own degree, default 1).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhao, J., Wang, Y. and Deng, Y. (2020). Identifying influential nodes in
#'   complex networks from global perspective. Chaos, Solitons and Fractals,
#'   133, 109637. \doi{10.1016/j.chaos.2020.109637}.
#' @seealso \code{\link{centrality_gravity}},
#'   \code{\link{centrality_harmonic}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_gin(regulation_net)
centrality_gin <- function(x, ...) {
  df <- centrality(x, measures = "gin", ...)
  stats::setNames(df$gin, df$node)
}

#' Icentr
#'
#' Icentr (Mussone et al. 2022) scores a node by all the edges of its
#' component, each counted less the farther it lies from the node. Every
#' node gets a level, its distance in steps from the focal node. An edge
#' between two levels takes the level of its farther end, and an edge
#' inside one level takes that level plus one. The score is
#' \deqn{Icentr(i) = \sum_{e} 2^{-(lev(e) - 1)}}{
#'   Icentr(i) = sum_e 2^-(lev(e) - 1)}
#' over the edges of the component of \eqn{i}.
#'
#' @details
#' This is the unweighted form of the source, with unit node and edge
#' weights. It is computed on the simple undirected skeleton, so direction,
#' weights, loops and parallel edges are ignored. Each edge at the node
#' itself adds 1. Isolated nodes score 0. \code{normalized = TRUE} divides
#' the scores by their maximum; the source rescales to the range from 0 to
#' 1 by the minimum and maximum instead.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Mussone, L., Viseh, H. and Notari, R. (2022). Novel centrality measures
#'   and applications to underground networks. Physica A: Statistical
#'   Mechanics and its Applications, 589, 126595.
#'   \doi{10.1016/j.physa.2021.126595}.
#' @seealso \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_icentr(regulation_net)
centrality_icentr <- function(x, ...) {
  df <- centrality(x, measures = "icentr", ...)
  stats::setNames(df$icentr, df$node)
}

#' Global and Local Information Centrality
#'
#' The global and local information (GLI) method (Yang et al. 2020) gives
#' every node the mass \eqn{Iks + DC}, its improved k-shell index plus its
#' degree, and scores a node by its own share of the total mass and the
#' mass around it:
#' \deqn{I_i = \exp\Bigl[\frac{Iks_i + DC_i}{\sum_l (Iks_l + DC_l)}\Bigr]
#'   \sum_{j:\,1 \le d_{ij} \le r} \frac{Iks_j + DC_j}{d_{ij}}.}{
#'   I_i = exp[(Iks_i + DC_i) / sum_l (Iks_l + DC_l)]
#'   sum_{j: 1 <= d_ij <= r} (Iks_j + DC_j) / d_ij.}
#' The improved k-shell index is \eqn{Iks = ks + nit}, the k-shell index
#' plus the round of the k-shell decomposition in which the node is
#' removed.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Rounds are counted from one through the whole decomposition, and all
#' nodes at or below the current degree leave in the same round, as in
#' Table 1 of the source. \code{gli_radius} is 3 by default, the value of
#' the source. The total mass is taken over all nodes, so adding a
#' component changes every score. The source assumes no isolated node; here
#' an isolated node is left out of the decomposition and scores 0. A
#' \code{gli_radius} that is not a whole number of at least one raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{gli_radius} (the distance range \eqn{r}, default 3).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Yang, Y.-Z., Hu, M. and Huang, T.-Y. (2020). Influential nodes
#'   identification in complex networks based on global and local
#'   information. Chinese Physics B, 29(8), 088903.
#'   \doi{10.1088/1674-1056/ab969f}.
#' @seealso \code{\link{centrality_gravity}},
#'   \code{\link{centrality_iksd}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_gli(regulation_net)
centrality_gli <- function(x, ...) {
  df <- centrality(x, measures = "gli", ...)
  stats::setNames(df$gli, df$node)
}
