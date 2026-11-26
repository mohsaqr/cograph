#' Calculate the long-range interaction centrality (maximal path)
#' @keywords internal
#' @noRd
calculate_lric_max <- function(cg, weights = NULL, quota = 0.5,
                               path_length = Inf, limit = 1e6) {
  a <- .cg_candidate_adjacency(cg, weights, "lric_max")
  diag(a) <- 0
  .cg_enumerated("lric_max", nrow(a),
                 .cg_lric_max(a, quota, path_length, limit))
}

#' Long-Range Interaction Centrality (Maximal Path)
#'
#' The long-range interaction centrality (LRIC; Aleskerov et al. 2017)
#' measures the influence of a node on the others, directly and through
#' chains of intermediaries. A group of nodes that send ties to \eqn{j} is
#' critical when their weights sum to at least the quota \eqn{q_j}, and a
#' member is pivotal when the group falls below the quota without it. The
#' direct influence of \eqn{i} on \eqn{j} is
#' \deqn{c_{ij} = \frac{w_{ij}}{\min \sum_{l \in \Omega(j)} w_{lj}},}{
#'   c_ij = w_ij / min sum_{l in Omega(j)} w_lj,}
#' the minimum taken over the critical groups \eqn{\Omega(j)}{Omega(j)} in
#' which \eqn{i} is pivotal, and 0 when there is none. The influence along
#' a path is the product of the direct influences on its ties, the total
#' influence \eqn{c^*_{ij}}{c*_ij} is that of the best path, and
#' \deqn{LRIC(i) = \sum_j c^*_{ij} u_j, \qquad
#'   u_j = \frac{\sum_k w_{jk}}{\sum_l \sum_k w_{lk}}.}{
#'   LRIC(i) = sum_j c*_ij u_j,  u_j = sum_k w_jk / sum_l sum_k w_lk.}
#'
#' @details
#' Direction and weights are used: \eqn{w_{ij}}{w_ij} is the influence of
#' \eqn{i} on \eqn{j}, and an undirected tie acts in both directions. Loops
#' are ignored. \code{lric_quota} is the share of the weight entering a
#' node that a group needs in order to be critical, 0.5 by default as in
#' the example of the source. \code{lric_path_length} limits the number of
#' ties of a path; by default (\code{Inf}) all simple paths are considered.
#' The program of the authors limits paths to three ties, which changes
#' some values. A node that sends no tie, or that is pivotal for nobody,
#' scores 0. The scores follow the working-paper version of the source
#' (arXiv:1610.05892); the published chapter was not consulted.
#'
#' Finding the smallest critical group examines sums over subsets of a
#' node's in-neighbors. With many distinct weights their number doubles
#' with every in-neighbor. The measure is therefore costly, computed only
#' when requested by name or through \code{include}, and it stops past
#' \code{enumeration_limit} sums (default one million): every score is then
#' \code{NA} with a \code{cograph_undefined_measure} warning. Parameters
#' outside their domain raise a \code{cograph_bad_parameter} error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{lric_quota} (default 0.5), \code{lric_path_length} (default
#'   \code{Inf}), \code{weighted} and \code{enumeration_limit}.
#' @return A named numeric vector with one score per node, in input node
#'   order, or \code{NA} at every node when the search was cut short.
#' @references
#' Aleskerov, F., Meshcheryakova, N. and Shvydun, S. (2017). Power in
#'   network structures. In Models, Algorithms, and Technologies for Network
#'   Analysis, Springer Proceedings in Mathematics and Statistics,
#'   79-85. \doi{10.1007/978-3-319-56829-4_7}.
#' @seealso \code{\link{centrality_con}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_lric_max(regulation_net)
centrality_lric_max <- function(x, ...) {
  df <- centrality(x, measures = "lric_max", ...)
  stats::setNames(df$lric_max, df$node)
}
