#' Simple randomized shortest paths betweenness (Kivimaki et al. 2016)
#' @keywords internal
#' @noRd
calculate_rsp_betweenness <- function(cg, weights = NULL, rsp_beta = 0.01,
                                      rsp_cost = "inverse") {
  .cg_rsp_terms(.cg_path_matrix(cg, weights), rsp_beta, rsp_cost)$score
}

#' Randomized Shortest Paths Betweenness Centrality
#'
#' Randomized shortest paths betweenness (Kivimaki et al. 2016) scores a node
#' by the expected number of visits it receives from absorbing walks between
#' every ordered source-target pair, under a Boltzmann distribution that
#' favors low-cost walks with inverse temperature \eqn{\beta}{beta}. Large
#' \eqn{\beta}{beta} approaches shortest-path betweenness, and
#' \eqn{\beta \to 0}{beta -> 0} approaches a random-walk quantity that is
#' proportional to degree on an undirected graph. With
#' \eqn{Z = (I - W)^{-1}}{Z = (I - W)^-1} and
#' \eqn{W = (D^{-1}A) \circ \exp(-\beta C)}{W = (D^-1 A) * exp(-beta C)}:
#' \deqn{bet_i = \sum_{s,t} \left(\frac{z_{si}}{z_{st}}
#'   - \frac{z_{ti}}{z_{tt}}\right) z_{it}}{
#'   bet_i = sum_{s,t} (z_si / z_st - z_ti / z_tt) z_it}
#'
#' @details
#' Direction and edge weights are used and loops are dropped.
#' \code{mode}, \code{cutoff} and \code{invert_weights} have no effect. The
#' cost \eqn{C} is \eqn{1/w} with \code{rsp_cost = "inverse"} and \eqn{w}
#' with \code{"weight"}, and with \code{weighted = FALSE} every arc costs
#' one.
#' Following the source, a pair with no connecting path contributes zero, so
#' scores are component-local, and nodes with no out-edges score zero. A
#' \code{rsp_beta} at or below zero raises a \code{cograph_bad_parameter}
#' error, and negative or non-finite weights raise a
#' \code{cograph_bad_input} error. The measure needs one dense matrix
#' inverse, so it is costly and is computed only when requested by name or
#' through \code{include}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{rsp_beta} (inverse temperature, default 0.01), \code{rsp_cost}
#'   (\code{"inverse"} (default) or \code{"weight"}) and \code{weighted}
#'   (use edge weights, default \code{TRUE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Kivimaki, I., Lebichot, B., Saramaki, J. and Saerens, M. (2016). Two
#'   betweenness centrality measures based on Randomized Shortest Paths.
#'   Scientific Reports, 6, 19668. \doi{10.1038/srep19668}.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_current_flow_betweenness}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_rsp_betweenness(regulation_net)
centrality_rsp_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "rsp_betweenness", ...)
  stats::setNames(df$rsp_betweenness, df$node)
}
