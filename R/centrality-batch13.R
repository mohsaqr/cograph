#' Calculate volume or maximal clique centrality
#' @keywords internal
#' @noRd
calculate_candidate_structure <- function(g, measure, volume_radius = 2) {
  b <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  diag(b) <- 0
  switch(measure,
         volume = .cg_volume(b, volume_radius),
         mcc = .cg_mcc(b))
}

#' Volume centrality
#'
#' Sum of the original-graph degrees of all vertices within
#' \code{volume_radius} hops, including the focal vertex. This is the
#' localized volume measure of Wehmuth & Ziviani (DANCE/DACCER). Degrees
#' include edges leaving the neighbourhood; they are not recomputed inside
#' the induced subgraph. Radius zero returns degree. Infinite radius returns
#' twice the number of edges in the focal connected component.
#'
#' Uses the simple undirected, unweighted skeleton: either direction creates
#' an edge, parallel edges count once, and self-loops are removed. This is
#' an explicit input projection, not a weighted or directed generalisation.
#' Isolates score zero.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param volume_radius Nonnegative integer hop radius, or \code{Inf}.
#'   Default 2, the local radius investigated by Wehmuth & Ziviani.
#' @param ... Additional arguments to \code{\link{centrality}}. With
#'   \code{normalized = TRUE}, positive scores are divided by their maximum.
#' @return Named numeric vector in input node order.
#' @references
#' Wehmuth, K., & Ziviani, A. (2011). Distributed Assessment of Network
#' Centrality. Section II-A, equation 1.
#' \url{https://arxiv.org/abs/1108.1067v1}.
#'
#' Wehmuth, K., & Ziviani, A. (2013). DACCER: Distributed Assessment of the
#' Closeness CEntrality Ranking in complex networks. Computer Networks, 57,
#' 2536-2548. \doi{10.1016/j.comnet.2013.05.001}.
#' @seealso \code{\link{centrality_kreach}}, \code{\link{centrality_degree}}.
#' @export
#' @examples
#' centrality_volume(igraph::make_ring(6), volume_radius = 1)
#' centrality_volume(igraph::make_ring(6), volume_radius = 0)
centrality_volume <- function(x, volume_radius = 2, ...) {
  df <- centrality(x, measures = "volume", volume_radius = volume_radius, ...)
  stats::setNames(df$volume, df$node)
}

#' Maximal clique centrality
#'
#' For every maximal clique C containing a vertex, add \eqn{(|C|-1)!}.
#' Only maximal cliques count: a clique contained in a larger clique is
#' excluded. This is Chin et al.'s MCC, not a count of all cliques.
#'
#' Uses the simple undirected, unweighted skeleton: either direction creates
#' an edge, parallel edges count once, and self-loops are removed. Singleton
#' cliques are excluded, so isolates score zero. This is an explicit cograph
#' convention consistent with the paper's degree reduction when neighbours
#' have no edges between them. Reading the printed sum literally with
#' singleton cliques would instead assign isolates \eqn{0! = 1}.
#'
#' Maximal clique enumeration has exponential worst-case cost. MCC is held
#' back from \code{centrality(type = "all")}; select it explicitly or use
#' \code{include = "mcc"}. Scores use double precision; overflow raises an
#' error, including any clique with more than 171 vertices. Normalization
#' happens after raw calculation and does not bypass this limit.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}. With
#'   \code{normalized = TRUE}, positive scores are divided by their maximum.
#' @return Named numeric vector in input node order.
#' @references
#' Chin, C. H., et al. (2014). cytoHubba: identifying hub objects and
#' sub-networks from complex interactome. BMC Systems Biology, 8(Suppl 4),
#' S11. \doi{10.1186/1752-0509-8-S4-S11}.
#' @seealso \code{\link{centrality_cross_clique}},
#'   \code{\link{list_centralities}}.
#' @export
#' @examples
#' centrality_mcc(igraph::make_full_graph(5))
centrality_mcc <- function(x, ...) {
  df <- centrality(x, measures = "mcc", ...)
  stats::setNames(df$mcc, df$node)
}
