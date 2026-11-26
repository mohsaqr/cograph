#' Calculate dynamical spanning tree centrality
#' @keywords internal
#' @noRd
calculate_dst <- function(cg) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  out <- .cg_dst(b)
  reason <- switch(
    out$status,
    disconnected = "a disconnected network has no spanning tree",
    singleton = "the network has one node",
    NULL
  )
  if (!is.null(reason)) {
    .cg_warn_undefined("`dst` has no value on this input, so its column is ",
                       "NA: ", reason, ".")
  }
  out$value
}

#' Calculate the KDEC score
#' @keywords internal
#' @noRd
calculate_kdec <- function(cg) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_kdec(b)
}

#' Calculate counting betweenness
#' @keywords internal
#' @noRd
calculate_counting_betweenness <- function(cg, weights = NULL) {
  out <- .cg_counting_betweenness(
    .cg_candidate_adjacency(cg, weights, "counting_betweenness"))
  reason <- switch(
    out$status,
    not_strong = paste(
      "the nodes that have both incoming and outgoing ties are not strongly",
      "connected, so some first-passage walks never arrive"),
    too_small = "fewer than two nodes have both incoming and outgoing ties",
    NULL
  )
  if (!is.null(reason)) {
    .cg_warn_undefined("`counting_betweenness` has no value on this input, ",
                       "so its column is NA: ", reason, ".")
  }
  out$value
}

#' Dynamical Spanning Tree Centrality
#'
#' Dynamical spanning tree (DST) centrality (Chen et al. 2003, as given by
#' Lu et al. 2016) is the share of the spanning trees of a network that is
#' lost when a node is removed:
#' \deqn{DST(i) = 1 - \frac{t(G \setminus \{v_i\})}{t(G)},}{
#'   DST(i) = 1 - t(G - v_i) / t(G),}
#' where \eqn{t} counts spanning trees.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The counts come from the matrix-tree theorem. A node whose removal
#' disconnects the network scores 1, and a node with one neighbor scores 0.
#' On a disconnected network and on a single node every score is \code{NA}
#' with a \code{cograph_undefined_measure} warning. The definition follows
#' equation (50) of Lu et al. (2016); the 2003 paper was not consulted.
#' The measure is different from the \code{spanning_tree} measure of
#' \code{\link{centrality}}.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, between 0 and 1, or \code{NA} at every node when the network is
#'   disconnected.
#' @references
#' Chen, Y., Hu, A.-Q., Yip, K.-W., Hu, J. and Zhong, Z.-G. (2003). Finding the most
#'   vital node with respect to the number of spanning trees. In Proceedings
#'   of the 2003 International Conference on Neural Networks and Signal
#'   Processing, 2, 1670-1673. \doi{10.1109/ICNNSP.2003.1281204}.
#'
#' Lu, L., Chen, D., Ren, X.-L., Zhang, Q.-M., Zhang, Y.-C. and Zhou, T.
#'   (2016). Vital nodes identification in complex networks. Physics
#'   Reports, 650, 1-63. \doi{10.1016/j.physrep.2016.06.007}.
#' @seealso \code{\link{centrality_mec}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dst(regulation_net)
centrality_dst <- function(x, ...) {
  df <- centrality(x, measures = "dst", ...)
  stats::setNames(df$dst, df$node)
}

#' KDEC Centrality
#'
#' The KDEC method (Zhang et al. 2019) gives every node the weight
#' \eqn{W(v) = k_s(v)\,d(v)}{W(v) = k_s(v) d(v)}, its core number times its
#' degree, and sums the influence a node exchanges with the nodes one and
#' two steps away, discounted by an effective distance:
#' \deqn{KDEC(i) = \frac{W(i)}{n}\Bigl[\sum_{j \in N(i)}
#'   \frac{W(j)}{e_{ij}^2} + \sum_{j \in N(i)}
#'   \sum_{k \in N(j),\, k \ne i,\, k \notin N(i)}
#'   \frac{W(k)}{(e_{ij} + e_{jk})^2}\Bigr],}{
#'   KDEC(i) = W(i)/n [sum_{j in N(i)} W(j) / e_ij^2 +
#'   sum_{j in N(i)} sum_{k in N(j), k != i, k not in N(i)}
#'   W(k) / (e_ij + e_jk)^2],}
#' with \eqn{e_{ij} = 1 - \log_{10}(1/d_j)}{e_ij = 1 - log10(1/d_j)} and
#' \eqn{n} the number of nodes.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored. A
#' node two steps away is counted once for every neighbor that leads to it.
#' That rule and the division by \eqn{n} follow the worked example of the
#' source. Because \eqn{n} counts all nodes, adding a component lowers
#' every score. Isolated nodes score 0. \code{normalized = TRUE} divides
#' the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhang, J., Wang, B., Sheng, J., Dai, J., Hu, J. and Chen, L. (2019).
#'   Identifying influential nodes in complex networks based on local
#'   effective distance. Information, 10(10), 311.
#'   \doi{10.3390/info10100311}.
#' @seealso \code{\link{centrality_ked}}, \code{\link{centrality_gravity}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_kdec(regulation_net)
centrality_kdec <- function(x, ...) {
  df <- centrality(x, measures = "kdec", ...)
  stats::setNames(df$kdec, df$node)
}

#' Counting Betweenness
#'
#' Counting betweenness (Bloechl et al. 2011) is the expected number of
#' times a random walk passes a node on its way between two other nodes,
#' averaged over all ordered pairs. A walker at \eqn{i} follows the tie to
#' \eqn{j} with probability \eqn{m_{ij} = a_{ij} / \sum_k a_{ik}}{
#'   m_ij = a_ij / sum_k a_ik}. For a walk from \eqn{s} that stops when it
#' first reaches \eqn{t}, the expected number of uses of the tie
#' \eqn{(i, j)} is
#' \eqn{N^{st}_{ij} = m_{ij}\,[(I - M_{-t})^{-1}]_{si}}{
#'   N^st_ij = m_ij [(I - M_-t)^-1]_si}, and
#' \deqn{C_c(i) = \frac{1}{n(n-1)} \sum_{s} \sum_{t \ne s} N^{st}(i),
#'   \qquad N^{st}(i) = \sum_{j \ne t}
#'   \frac{N^{st}_{ij} + N^{st}_{ji}}{2},}{
#'   C_c(i) = 1/(n(n-1)) sum_s sum_{t != s} N^st(i),
#'   N^st(i) = sum_{j != t} (N^st_ij + N^st_ji) / 2,}
#' with one added for the start \eqn{s}, and \eqn{N^{st}(t) = 1}{
#'   N^st(t) = 1}.
#'
#' @details
#' Direction, weights and loops are used. A walk that goes back and forth
#' over a tie is counted every time, which sets the measure apart from
#' random walk betweenness that counts net flow. A loop makes the walker
#' stay, and raises the score of its node. The scores are expected numbers
#' of visits and are not bounded by 1.
#'
#' Following the source, a node without incoming or without outgoing ties
#' scores 0 and is left out of the walk. The remaining nodes must be
#' strongly connected. Otherwise every score is \code{NA} with a
#' \code{cograph_undefined_measure} warning. The measure inverts one matrix
#' per node, so it is costly and is computed only when requested by name or
#' through \code{include}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{weighted} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, or \code{NA} at every node when the score is undefined.
#' @references
#' Bloechl, F., Theis, F. J., Vega-Redondo, F. and Fisher, E. O'N. (2011).
#'   Vertex centralities in input-output networks reveal the structure of
#'   modern economies. Physical Review E, 83(4), 046127.
#'   \doi{10.1103/PhysRevE.83.046127}.
#' @seealso \code{\link{centrality_random_walk}},
#'   \code{\link{centrality_mec}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_counting_betweenness(regulation_net)
centrality_counting_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "counting_betweenness", ...)
  stats::setNames(df$counting_betweenness, df$node)
}
