#' Hybrid characteristic centrality (Liu and Zheng 2023)
#' @keywords internal
#' @noRd
calculate_hcc <- function(cg, delta = 0.5) {
  .cg_hcc_terms(.cg_path_matrix(cg, NULL), delta)$hcc
}

#' Extended hybrid characteristic centrality (Liu and Zheng 2023)
#' @keywords internal
#' @noRd
calculate_ehcc <- function(cg, delta = 0.5) {
  .cg_hcc_terms(.cg_path_matrix(cg, NULL), delta)$ehcc
}

#' Hybrid Characteristic Centrality
#'
#' Hybrid characteristic centrality (Liu and Zheng 2023) adds the extended
#' degree
#' \eqn{k^{ex}(u) = \delta k(u) + (1 - \delta) \sum_{v \in \phi(u)} k(v)}{
#'   kex(u) = delta k(u) + (1 - delta) sum_{v in N(u)} k(v)}
#' to the E-shell position index \eqn{pos(u)}, each divided by its maximum.
#' The E-shell decomposition removes, round by round, the remaining nodes of
#' minimum extended degree, recomputed on the residual graph, and
#' \eqn{pos(u)} is the round in which \eqn{u} leaves.
#' \deqn{HCC(u) = \frac{k^{ex}(u)}{k^{ex}_{max}} + \frac{pos(u)}{pos_{max}}}{
#'   HCC(u) = kex(u) / kex_max + pos(u) / pos_max}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the network,
#' so direction, weights, loops and parallel edges are ignored. Scores lie in
#' \eqn{[0, 2]}. Equation (4) uses the extended degrees of the original
#' graph, as the source's worked example requires. The normalizers are
#' global, so adding a disconnected component can change every score. An
#' isolate leaves in the first round, and every node of an edgeless graph
#' scores one. A \code{hcc_delta} outside \eqn{[0, 1]} raises a
#' \code{cograph_bad_parameter} error. Step 3 of the source's E-shell
#' procedure prints \eqn{\arg\max}{argmax} where its text and Table 2
#' require \eqn{\arg\min}{argmin}, and the minimum is implemented. The
#' Centrality Zoo describes the E-shell decomposition as a k-shell variant,
#' which gives different positions on the source's Figure 1.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{hcc_delta} (weight \eqn{\delta}{delta} of the node's own degree in
#'   the extended degree, default 0.5).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Liu, J. and Zheng, J. (2023). Identifying important nodes in complex
#'   networks based on extended degree and E-shell hierarchy decomposition.
#'   Scientific Reports, 13, 3197. \doi{10.1038/s41598-023-30308-5}.
#' @seealso \code{\link{centrality_ehcc}}, \code{\link{centrality_dkgm}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_hcc(regulation_net)
centrality_hcc <- function(x, ...) {
  df <- centrality(x, measures = "hcc", ...)
  stats::setNames(df$hcc, df$node)
}

#' Extended Hybrid Characteristic Centrality
#'
#' Extended hybrid characteristic centrality (Liu and Zheng 2023) adds to a
#' node's \code{\link{centrality_hcc}} score the scores of its neighbors.
#' \deqn{EHCC(u) = HCC(u) + \sum_{v \in \phi(u)} HCC(v)}{
#'   EHCC(u) = HCC(u) + sum_{v in N(u)} HCC(v)}
#'
#' @details
#' The measure inherits the conventions of \code{\link{centrality_hcc}}. It
#' is computed on the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored, and a \code{hcc_delta} outside
#' \eqn{[0, 1]} raises a \code{cograph_bad_parameter} error. Scores lie in
#' \eqn{[0, 2(1 + k_{max})]}{[0, 2 (1 + k_max)]}, and an isolate scores its
#' own HCC.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{hcc_delta} (weight \eqn{\delta}{delta} of the node's own degree in
#'   the extended degree, default 0.5).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Liu, J. and Zheng, J. (2023). Identifying important nodes in complex
#'   networks based on extended degree and E-shell hierarchy decomposition.
#'   Scientific Reports, 13, 3197. \doi{10.1038/s41598-023-30308-5}.
#' @seealso \code{\link{centrality_hcc}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ehcc(regulation_net)
centrality_ehcc <- function(x, ...) {
  df <- centrality(x, measures = "ehcc", ...)
  stats::setNames(df$ehcc, df$node)
}
