#' Run an enumeration and turn a passed step limit into NA with a warning
#' @param measure Measure name, for the message.
#' @param n Number of nodes.
#' @param expr Expression computing the scores.
#' @return The scores, or `NA` at every node when the limit was passed.
#' @keywords internal
#' @noRd
.cg_enumerated <- function(measure, n, expr) {
  tryCatch(expr, cograph_enumeration_limit = function(e) {
    .cg_warn_undefined(
      "`", measure, "` was not computed, so its column is NA: the search ",
      "passed `enumeration_limit` steps. The measure enumerates all simple ",
      "paths or cycles; raise `enumeration_limit` or use a smaller or ",
      "sparser network.")
    rep(NA_real_, n)
  })
}

#' Directed 0/1 adjacency of a graph context, without loops
#' @param cg Graph context.
#' @return Binary matrix with a zero diagonal.
#' @keywords internal
#' @noRd
.cg_arc_matrix <- function(cg) {
  a <- (.cg_path_matrix(cg, NULL) != 0) * 1
  diag(a) <- 0
  a
}

#' Calculate all cycle betweenness
#' @keywords internal
#' @noRd
calculate_all_cycle_betweenness <- function(cg, alpha = 0.5, limit = 1e6) {
  b <- .cg_skeleton(cg)
  .cg_enumerated("all_cycle_betweenness", nrow(b),
                 .cg_all_cycle_betweenness(b, alpha, limit))
}

#' Calculate transportation centrality
#' @keywords internal
#' @noRd
calculate_transportation <- function(cg, beta = 0.1, limit = 1e6) {
  a <- .cg_arc_matrix(cg)
  out <- .cg_enumerated("transportation", nrow(a),
                        .cg_transportation(a, beta, limit))
  if (nrow(a) > 0L && nrow(a) < 3L) {
    .cg_warn_undefined("`transportation` has no value on this input, so its ",
                       "column is NA: the network has fewer than three ",
                       "nodes.")
  }
  out
}

#' Calculate path-transfer centrality
#' @keywords internal
#' @noRd
calculate_path_transfer <- function(cg, limit = 1e6) {
  a <- .cg_arc_matrix(cg)
  .cg_enumerated("path_transfer", nrow(a), .cg_path_transfer(a, limit))
}

#' All Cycle Betweenness
#'
#' All cycle betweenness (Zhou et al. 2018) counts the simple cycles that
#' pass through a node, a longer cycle counting less:
#' \deqn{C_A(i) = \sum_{k=3}^{n} \alpha^{k-2} s_k(i),}{
#'   C_A(i) = sum_{k=3}^{n} alpha^(k-2) s_k(i),}
#' where \eqn{s_k(i)} is the number of simple cycles of \eqn{k} edges
#' through \eqn{i}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored. A
#' cycle is counted once, whatever its starting node and direction. A node
#' on no cycle scores 0. \code{cycle_alpha} is 0.5 by default, the value
#' of the worked example of the source; its experiments use 0.1, and it
#' states no single default. A \code{cycle_alpha} outside \eqn{(0,1)}
#' raises a \code{cograph_bad_parameter} error.
#'
#' The number of simple cycles grows exponentially with the size and
#' density of the network. The measure is therefore costly, computed only
#' when requested by name or through \code{include}, and it stops after
#' \code{enumeration_limit} search steps (default one million): every
#' score is then \code{NA} with a \code{cograph_undefined_measure} warning.
#' The count is exact, where the source estimates it on large networks.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{cycle_alpha} (the discount per edge, default 0.5) and
#'   \code{enumeration_limit}.
#' @return A named numeric vector with one score per node, in input node
#'   order, or \code{NA} at every node when the search was cut short.
#' @references
#' Zhou, X., Liang, X., Zhao, J. and Zhang, S. (2018). Cycle based network
#'   centrality. Scientific Reports, 8, 11749.
#'   \doi{10.1038/s41598-018-30249-4}.
#' @seealso \code{\link{centrality_transportation}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' ring <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
#' ring[cbind(1:5, c(2:5, 1))] <- 1
#' ring <- ring + t(ring)
#' centrality_all_cycle_betweenness(ring)
centrality_all_cycle_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "all_cycle_betweenness", ...)
  stats::setNames(df$all_cycle_betweenness, df$node)
}

#' Transportation Centrality
#'
#' Transportation centrality (Piraveenan and Saripada 2023) is a
#' betweenness in which traffic between two nodes spreads over all simple
#' paths, a longer path carrying less:
#' \deqn{TC(v) = \frac{1}{(N-1)(N-2)} \sum_{s \ne v \ne t}
#'   \frac{\sum_{i \in P^v_{s,t}} e^{-\beta C_i}}
#'        {\sum_{j \in P_{s,t}} e^{-\beta C_j}},}{
#'   TC(v) = 1/((N-1)(N-2)) sum_{s != v != t}
#'   [sum_{i in P^v_st} exp(-beta C_i)] / [sum_{j in P_st} exp(-beta C_j)],}
#' where \eqn{P_{s,t}}{P_st} holds all simple paths from \eqn{s} to
#' \eqn{t}, \eqn{P^v_{s,t}}{P^v_st} those through \eqn{v}, and \eqn{C} is
#' the number of edges of a path.
#'
#' @details
#' Direction is used, and weights, loops and parallel edges are ignored.
#' The pairs \eqn{(s, t)} are ordered. \code{transport_beta} is 0.1 by
#' default, the value the experiments of the source concentrate on; its
#' worked example uses 2, and it states no single default. As
#' \code{transport_beta} grows the measure tends to shortest-path
#' betweenness divided by \eqn{(N-1)(N-2)}, and at 0 every simple path
#' counts alike. A pair without a path adds nothing. A node with one tie
#' scores 0. A negative \code{transport_beta} raises a
#' \code{cograph_bad_parameter} error, and a network of fewer than three
#' nodes gives \code{NA} with a \code{cograph_undefined_measure} warning.
#'
#' The number of simple paths grows exponentially with the size and
#' density of the network. The measure is therefore costly, computed only
#' when requested by name or through \code{include}, and it stops after
#' \code{enumeration_limit} search steps (default one million): every
#' score is then \code{NA} with a \code{cograph_undefined_measure} warning.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{transport_beta} (the sensitivity to path length, default 0.1)
#'   and \code{enumeration_limit}.
#' @return A named numeric vector with one score per node, in input node
#'   order, between 0 and 1, or \code{NA} at every node when the score was
#'   not computed.
#' @references
#' Piraveenan, M. and Saripada, N. B. (2023). Transportation centrality:
#'   quantifying the relative importance of nodes in transportation networks
#'   based on traffic modeling. IEEE Access, 11, 142214-142234.
#'   \doi{10.1109/ACCESS.2023.3339121}.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_rsp_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' paw <- matrix(0, 4, 4, dimnames = list(1:4, 1:4))
#' paw[rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4))] <- 1
#' paw <- paw + t(paw)
#' centrality_transportation(paw, transport_beta = 2)
centrality_transportation <- function(x, ...) {
  df <- centrality(x, measures = "transportation", ...)
  stats::setNames(df$transportation, df$node)
}

#' Path-Transfer Centrality
#'
#' Path-transfer centrality (Tutzauer 2007) is the entropy of where a flow
#' that starts at a node comes to rest. The flow never visits a node
#' twice. At a node with \eqn{m} unvisited out-neighbors it stops with
#' probability \eqn{1/(m+1)} and moves to each of them with the same
#' probability. With \eqn{p_{uv}}{p_uv} the probability that a flow from
#' \eqn{u} stops at \eqn{v},
#' \deqn{C_H(u) = -\sum_{v} p_{uv} \log_2 p_{uv}.}{
#'   C_H(u) = -sum_v p_uv log2 p_uv.}
#'
#' @details
#' Direction is used, and weights, loops and parallel edges are ignored.
#' The score is in bits and lies between 0 and the base-2 logarithm of the
#' number of nodes. A node without outgoing ties scores 0, because its
#' flow can only stay. The definition follows its restatement by Oggier et
#' al. (2019); the 2007 article was not consulted.
#'
#' The number of simple paths grows exponentially with the size and
#' density of the network. The measure is therefore costly, computed only
#' when requested by name or through \code{include}, and it stops after
#' \code{enumeration_limit} search steps (default one million): every
#' score is then \code{NA} with a \code{cograph_undefined_measure} warning.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{enumeration_limit} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, or \code{NA} at every node when the search was cut short.
#' @references
#' Tutzauer, F. (2007). Entropy as a measure of centrality in networks
#'   characterized by path-transfer flow. Social Networks, 29(2), 249-265.
#'   \doi{10.1016/j.socnet.2006.10.001}.
#'
#' Oggier, F., Phetsouvanh, S. and Datta, A. (2019). A split-and-transfer
#'   flow based entropic centrality. PeerJ Computer Science, 5, e220.
#'   \doi{10.7717/peerj-cs.220}.
#' @seealso \code{\link{centrality_transportation}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' flow <- matrix(0, 5, 5, dimnames = list(1:5, 1:5))
#' flow[rbind(c(1, 2), c(1, 3), c(1, 4), c(3, 5), c(4, 5))] <- 1
#' centrality_path_transfer(flow)
centrality_path_transfer <- function(x, ...) {
  df <- centrality(x, measures = "path_transfer", ...)
  stats::setNames(df$path_transfer, df$node)
}
