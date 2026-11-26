#' Common out-neighbor score
#' @keywords internal
#' @noRd
calculate_con <- function(cg, weights = NULL) {
  a <- .cg_candidate_adjacency(cg, weights, "con")
  diag(a) <- 0
  .cg_con(a)
}

#' Entropy and mutual information-based centrality
#' @keywords internal
#' @noRd
calculate_emi <- function(cg) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_emi_terms(b)$emi
}

#' Calculate NWRank
#' @keywords internal
#' @noRd
calculate_nwrank <- function(cg, alpha = 0.5) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  out <- .cg_nwrank(b, alpha)
  reason <- switch(
    out$status,
    no_links = "the network has no links",
    disconnected = paste(
      "the nodes that have links form more than one component, and the",
      "principal eigenvector is then not unique"),
    NULL
  )
  if (!is.null(reason)) {
    .cg_warn_undefined("`nwrank` has no value on this input, so its column ",
                       "is NA: ", reason, ".")
  }
  out$value
}

#' Biased random walk centrality
#' @keywords internal
#' @noRd
calculate_biased_random_walk <- function(cg, alpha = 0.5, jump = 0.15) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_biased_random_walk(b, alpha, jump)
}

#' Common Out-Neighbor Score
#'
#' The common out-neighbor (CON) score (Bonato et al. 2019) counts the
#' out-neighbors a node shares with every other node:
#' \deqn{CON(u) = \sum_{v \ne u} \sum_k \min(A_{uk}, A_{vk}),}{
#'   CON(u) = sum_{v != u} sum_k min(A_uk, A_vk),}
#' where \eqn{A_{uk}}{A_uk} is the number of arcs from \eqn{u} to \eqn{k}.
#' Two nodes that send ties to the same targets are competitors, and a node
#' with a low score has few competitors.
#'
#' @details
#' Direction is used: the targets of a node's outgoing ties are compared.
#' On an undirected network every tie counts in both directions, and the
#' score is the sum over a node's neighbors of their degree minus one. With
#' \code{weighted = TRUE} the weights are read as the numbers of
#' interactions and the smaller of the two is counted. Loops are ignored.
#' A node without outgoing ties scores 0. \code{normalized = TRUE} divides
#' the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{weighted} and \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Bonato, A., Eikmeier, N., Gleich, D. F. and Malik, R. (2019). Centrality
#'   in dynamic competition networks. In Complex Networks and Their
#'   Applications VIII, Studies in Computational Intelligence 881, 105-116.
#'   \doi{10.1007/978-3-030-36683-4_9}.
#' @seealso \code{\link{centrality_semilocal}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_con(regulation_net)
centrality_con <- function(x, ...) {
  df <- centrality(x, measures = "con", ...)
  stats::setNames(df$con, df$node)
}

#' Entropy and Mutual Information Centrality
#'
#' The entropy and mutual information-based centrality (EMI; Li et al.
#' 2020) adds the entropy of the degrees around a node to a mutual
#' information term built on the neighbors it shares with each neighbor:
#' \deqn{S_i = -\sum_{j \in N_i} \frac{d_j}{EXD_i}
#'   \log_{10}\frac{d_j}{EXD_i}, \qquad
#'   MI_i = \sum_{j \in N_i} \frac{c_{ij}}{d_i + d_j}
#'   \log_{10}\frac{d_i + d_j - c_{ij}}{d_i + d_j},}{
#'   S_i = -sum_{j in N_i} (d_j / EXD_i) log10(d_j / EXD_i),
#'   MI_i = sum_{j in N_i} c_ij / (d_i + d_j) log10((d_i + d_j - c_ij) / (d_i + d_j)),}
#' and \eqn{EMI_i = S_i + MI_i}, with \eqn{d} the degree,
#' \eqn{EXD_i = \sum_{j \in N_i} d_j}{EXD_i = sum_{j in N_i} d_j} and
#' \eqn{c_{ij}}{c_ij} the number of common neighbors of \eqn{i} and
#' \eqn{j}.
#'
#' @details
#' This is the form of the source for undirected unweighted networks. It is
#' computed on the simple undirected skeleton, so direction, weights, loops
#' and parallel edges are ignored. The mutual information term is never
#' positive, and is zero for a node that shares no neighbor with any of its
#' neighbors. Isolated nodes and nodes of degree one score 0.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Li, Y., Cai, W., Li, Y. and Du, X. (2020). Key node ranking in complex
#'   networks: a novel entropy and mutual information-based approach.
#'   Entropy, 22(1), 52. \doi{10.3390/e22010052}.
#' @seealso \code{\link{centrality_local_entropy}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_emi(regulation_net)
centrality_emi <- function(x, ...) {
  df <- centrality(x, measures = "emi", ...)
  stats::setNames(df$emi, df$node)
}

#' NWRank
#'
#' NWRank (Wang et al. 2015) ranks nodes and links together: a node passes
#' its score to its links, and a link passes its score to its two end
#' nodes. Node \eqn{j} gives link \eqn{b} the share
#' \deqn{h_{bj} = \alpha \frac{k_b}{\sum_a k_a} +
#'   (1 - \alpha) \frac{BC_b}{\sum_a BC_a},}{
#'   h_bj = alpha k_b / sum_a k_a + (1 - alpha) BC_b / sum_a BC_a,}
#' where the sums run over the links at \eqn{j}, \eqn{k_b} is the degree of
#' the node at the other end of \eqn{b} and \eqn{BC_b} is the betweenness
#' of the link. The node scores are the principal eigenvector of \eqn{WH},
#' with \eqn{W} the node-link incidence matrix, scaled to sum to one.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{nwrank_alpha} is 0.5 by default, the value of the source's case
#' study. At 1 only the degrees decide the shares and at 0 only the link
#' betweenness. Isolated nodes score 0. When the nodes that have links form
#' more than one component the eigenvector is not unique and every score
#' is \code{NA} with a \code{cograph_undefined_measure} warning; the same
#' holds for a network without links. A \code{nwrank_alpha} outside
#' \eqn{[0,1]} raises a \code{cograph_bad_parameter} error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{nwrank_alpha} (the weight of the degree term, default 0.5).
#' @return A named numeric vector with one score per node, in input node
#'   order, summing to one, or \code{NA} at every node when the score is
#'   undefined.
#' @references
#' Wang, Z., Duenas-Osorio, L. and Padgett, J. E. (2015). A new mutually
#'   reinforcing network node and link ranking algorithm. Scientific Reports,
#'   5, 15141. \doi{10.1038/srep15141}.
#' @seealso \code{\link{centrality_linerank}},
#'   \code{\link{centrality_eigenvector}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_nwrank(regulation_net)
centrality_nwrank <- function(x, ...) {
  df <- centrality(x, measures = "nwrank", ...)
  stats::setNames(df$nwrank, df$node)
}

#' Biased Random Walk Centrality
#'
#' Biased random walk centrality (Takes and Kosters 2011) is the share of
#' time a random walker spends at a node when it prefers neighbors that
#' have many ties and whose own neighbors are not tied to each other. From
#' node \eqn{v} the walker moves to the neighbor \eqn{w} with probability
#' proportional to
#' \eqn{\alpha f_{deg}(w) + (1 - \alpha) f_{nd}(w)}{
#'   alpha f_deg(w) + (1 - alpha) f_nd(w)}, where
#' \deqn{f_{deg}(v) = 1 - \frac{1}{|N(v)|}, \qquad
#'   f_{nd}(v) = 1 - \sum_{w \in N(v)}
#'   \frac{|N(w) \cap N(v)|}{(|N(w)| - 1)\,|N(v)|},}{
#'   f_deg(v) = 1 - 1 / |N(v)|,
#'   f_nd(v) = 1 - sum_{w in N(v)} |N(w) n N(v)| / ((|N(w)| - 1) |N(v)|),}
#' and with probability \eqn{p} it jumps to a node chosen uniformly.
#'
#' @details
#' The source estimates the share of visits with one walk of ten steps per
#' node from a random start, so its result varies from run to run. The
#' value returned here is the stationary distribution of the same walk,
#' the limit of that share, and needs no random numbers. The scores sum to
#' one.
#'
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{brw_alpha} is 0.5 and \code{brw_jump} is 0.15 by default, the
#' values of the source. The source leaves three cases open. A neighbor of
#' degree one adds nothing to \eqn{f_{nd}}{f_nd}. When every neighbor of a
#' node has weight zero the walker picks among them uniformly. From an
#' isolated node the walker always jumps. A \code{brw_alpha} outside
#' \eqn{[0,1]} or a \code{brw_jump} outside \eqn{(0,1]} raises a
#' \code{cograph_bad_parameter} error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{brw_alpha} (the weight of the degree term, default 0.5) and
#'   \code{brw_jump} (the jump probability, default 0.15).
#' @return A named numeric vector with one score per node, in input node
#'   order, summing to one.
#' @references
#' Takes, F. W. and Kosters, W. A. (2011). Identifying prominent actors in
#'   online social networks using biased random walks. In Proceedings of the
#'   23rd Benelux Conference on Artificial Intelligence (BNAIC 2011),
#'   215-222.
#' @seealso \code{\link{centrality_pagerank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_biased_random_walk(regulation_net)
centrality_biased_random_walk <- function(x, ...) {
  df <- centrality(x, measures = "biased_random_walk", ...)
  stats::setNames(df$biased_random_walk, df$node)
}
