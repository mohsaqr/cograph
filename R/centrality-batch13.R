#' Calculate volume or maximal clique centrality
#' @keywords internal
#' @noRd
calculate_candidate_structure <- function(cg, measure, volume_radius = 2) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  switch(measure,
         volume = .cg_volume(b, volume_radius),
         mcc = .cg_mcc(b))
}

#' Volume Centrality
#'
#' Volume centrality (Wehmuth and Ziviani 2011, 2013) is the sum of the
#' degrees of all nodes within \code{volume_radius} hops of a node, the
#' node itself included. Degrees are those of the whole network, so edges
#' that leave the neighborhood also count.
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored. Isolated nodes score 0. Radius 0
#' gives the degree, and an infinite radius gives twice the number of edges
#' in the node's component. A radius that is negative or not a whole
#' number raises an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param volume_radius Hop radius: a nonnegative integer or \code{Inf}.
#'   Default 2.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wehmuth, K., & Ziviani, A. (2011). Distributed Assessment of Network
#'   Centrality. arXiv:1108.1067.
#'
#' Wehmuth, K., & Ziviani, A. (2013). DACCER: Distributed Assessment of the
#' Closeness CEntrality Ranking in complex networks. Computer Networks, 57,
#' 2536-2548. \doi{10.1016/j.comnet.2013.05.001}.
#' @seealso \code{\link{centrality_kreach}},
#'   \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_volume(regulation_net, volume_radius = 1)
centrality_volume <- function(x, volume_radius = 2, ...) {
  df <- centrality(x, measures = "volume", volume_radius = volume_radius, ...)
  stats::setNames(df$volume, df$node)
}

#' Maximal Clique Centrality
#'
#' Maximal clique centrality (MCC; Chin et al. 2014) sums
#' \eqn{(|C|-1)!} over the maximal cliques \eqn{C} that contain the node.
#' A clique contained in a larger clique does not count.
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored. Single-node cliques are excluded,
#' so an isolated node scores 0. A node whose neighbors share no edge
#' scores its degree. Maximal clique enumeration has exponential worst-case
#' cost. A score beyond double precision, which includes any clique with
#' more than 171 nodes, raises an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Chin, C. H., et al. (2014). cytoHubba: identifying hub objects and
#' sub-networks from complex interactome. BMC Systems Biology, 8(Suppl 4),
#' S11. \doi{10.1186/1752-0509-8-S4-S11}.
#' @seealso \code{\link{centrality_cross_clique}},
#'   \code{\link{centrality_epc}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mcc(regulation_net)
centrality_mcc <- function(x, ...) {
  df <- centrality(x, measures = "mcc", ...)
  stats::setNames(df$mcc, df$node)
}
