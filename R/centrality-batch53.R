#' Warn that a Friedkin (1991) measure has no value on the input
#' @param measure `"tec"` or `"mec"`.
#' @param status Status reported by `.cg_friedkin_terms()`.
#' @return `NULL`, invisibly; raises a `cograph_undefined_measure` warning
#'   when the measure is undefined for `status`.
#' @keywords internal
#' @noRd
.cg_warn_friedkin <- function(measure, status) {
  reason <- switch(
    status,
    singleton = "the network has one node and the measure averages over the other nodes",
    pair = if (identical(measure, "mec")) {
      "the network has two nodes and equation (22) averages over n - 2 other nodes"
    },
    reducible = paste(
      "with the unit self-loops the source mandates, the influence chain is",
      "reducible; use a connected undirected or a strongly connected",
      "directed network"),
    NULL
  )
  if (!is.null(reason)) {
    .cg_warn_undefined("`", measure, "` has no value on this input, so its ",
                       "column is NA: ", reason, ".")
  }
  invisible(NULL)
}

#' Total effects centrality (Friedkin 1991)
#' @keywords internal
#' @noRd
calculate_tec <- function(cg) {
  terms <- .cg_friedkin_terms(.cg_path_matrix(cg, NULL))
  .cg_warn_friedkin("tec", attr(terms, "status"))
  terms$tec
}

#' Mediative effects centrality (Friedkin 1991)
#' @keywords internal
#' @noRd
calculate_mec <- function(cg) {
  terms <- .cg_friedkin_terms(.cg_path_matrix(cg, NULL))
  .cg_warn_friedkin("mec", attr(terms, "status"))
  terms$mec
}

#' Total Effects Centrality
#'
#' Total effects centrality (Friedkin 1991) is the average total effect of
#' a node on the opinions of the other nodes,
#' \deqn{TEC_j = \frac{\sum_{i \ne j} v_{ij}}{n - 1},}{
#'   TEC_j = sum_{i != j} v_ij / (n - 1),}
#' where \eqn{V} is the matrix of total interpersonal effects of the
#' influence process. The influence matrix \eqn{W} is the adjacency matrix
#' with a unit diagonal, divided by its row sums. When the process runs to
#' consensus every row of \eqn{V} is the stationary vector \eqn{c} of
#' \eqn{W}, \eqn{c' = c'W}{c' = c'W}, and \eqn{TEC_j = c_j}. This is the
#' value returned.
#'
#' @details
#' Direction is used, and weights, loops and parallel edges are ignored, so
#' the unit diagonal is calibrated against unit edges. \code{mode} has no
#' effect. The scores sum to one. On an undirected network the score of a
#' node is its degree plus one, divided by the sum of that quantity over the
#' nodes. The measure needs an irreducible chain, that is a connected
#' undirected or a strongly connected directed network. Otherwise, and on a
#' single node, every score is \code{NA} with a
#' \code{cograph_undefined_measure} warning. The measure is computed only
#' when requested by name or through \code{include}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, or \code{NA} at every node when the influence chain is reducible
#'   or the network has one node.
#' @references
#' Friedkin, N. E. (1991). Theoretical foundations for centrality measures.
#'   American Journal of Sociology, 96(6), 1478-1504. \doi{10.1086/229694}.
#' @seealso \code{\link{centrality_iec}}, \code{\link{centrality_mec}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_tec(regulation_net)
centrality_tec <- function(x, ...) {
  df <- centrality(x, measures = "tec", ...)
  stats::setNames(df$tec, df$node)
}

#' Mediative Effects Centrality
#'
#' Mediative effects centrality (Friedkin 1991) measures how much a node
#' transmits the effects of the other nodes. The influence matrix \eqn{W}
#' is the adjacency matrix with a unit diagonal, divided by its row sums.
#' For a node \eqn{k}, \eqn{T_{(k)} = (I - W_{(k)})^{-1}}{
#'   T_(k) = (I - W_(k))^-1}, with \eqn{W_{(k)}}{W_(k)} the matrix \eqn{W}
#' without row and column \eqn{k}. Then
#' \deqn{\bar{t}_{(k)j} = \frac{\sum_{i \ne j,k} t_{(k)ij}}{(n-2)\,t_{(k)jj}},
#'   \qquad MEC_j = \frac{\sum_{k \ne j} \bar{t}_{(k)j}}{n - 1}.}{
#'   tbar_(k)j = sum_{i != j,k} t_(k)ij / ((n - 2) t_(k)jj),
#'   MEC_j = sum_{k != j} tbar_(k)j / (n - 1).}
#'
#' @details
#' The quotient \eqn{t_{(k)ij}/t_{(k)jj}}{t_(k)ij / t_(k)jj} is the
#' probability that a sequence of influence starting at \eqn{i} passes
#' through \eqn{j} before it reaches \eqn{k}. The score is the mean of that
#' probability over the other nodes \eqn{i} and \eqn{k}, so it lies between
#' 0 and 1. A node that every sequence must pass through scores 1.
#'
#' Direction is used, and weights, loops and parallel edges are ignored, so
#' the unit diagonal is calibrated against unit edges. \code{mode} has no
#' effect. The measure needs an irreducible chain, that is a connected
#' undirected or a strongly connected directed network, with at least three
#' nodes. Otherwise every score is \code{NA} with a
#' \code{cograph_undefined_measure} warning. The measure solves one linear
#' system per node, so it is costly and is computed only when requested by
#' name or through \code{include}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, or \code{NA} at every node when the influence chain is reducible
#'   or the network has fewer than three nodes.
#' @references
#' Friedkin, N. E. (1991). Theoretical foundations for centrality measures.
#'   American Journal of Sociology, 96(6), 1478-1504. \doi{10.1086/229694}.
#' @seealso \code{\link{centrality_iec}}, \code{\link{centrality_tec}},
#'   \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mec(regulation_net)
centrality_mec <- function(x, ...) {
  df <- centrality(x, measures = "mec", ...)
  stats::setNames(df$mec, df$node)
}
