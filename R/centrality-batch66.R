#' Calculate the batch 66 measures
#' @keywords internal
#' @noRd
calculate_kshell_hybrid <- function(cg, variant, mu = 0.4, radius = 3,
                                    extended = FALSE) {
  b <- .cg_skeleton(cg)
  score <- .cg_kshell_hybrid(b, variant, mu, radius)
  if (extended) as.numeric(b %*% score) else score
}

#' @keywords internal
#' @noRd
calculate_mixed_core <- function(cg, semi_local, weighted) {
  .cg_mixed_core(.cg_skeleton(cg), semi_local, weighted)
}

#' @keywords internal
#' @noRd
calculate_lgc <- function(cg) .cg_lgc(.cg_skeleton(cg))

#' Weighted gravity model; `NA` with a warning where it has no value
#' @keywords internal
#' @noRd
calculate_wgravity <- function(cg) {
  out <- .cg_wgravity(.cg_skeleton(cg))
  if (identical(out$status, "tied")) {
    .cg_warn_undefined("`wgravity` has no value on this input, so its ",
                       "column is NA: two components share the largest ",
                       "spectral radius, so the leading eigenvector is not ",
                       "unique.")
  }
  out$value
}

#' @keywords internal
#' @noRd
calculate_asp_centrality <- function(cg) .cg_asp_centrality(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_weight_degree <- function(cg, alpha = NULL, extended = FALSE) {
  .cg_weight_degree(.cg_skeleton(cg), alpha, extended)
}

#' K-Shell Hybrid Method
#'
#' The k-shell hybrid method (ksh; Namtirtha et al. 2018) adds up, over the
#' nodes within a few steps of a node, the root of the two nodes' summed
#' k-shell indices and a share of the other node's degree, each divided by
#' the squared distance:
#' \deqn{ksh(m) = \sum_{n:\,1 \le d_{mn} \le r} \frac{\sqrt{ks_m + ks_n} + \mu\,k_n}{d_{mn}^2},}{ksh(m) = sum_{n: 1 <= d_mn <= r} (sqrt(ks_m + ks_n) + mu k_n) / d_mn^2,}
#' with \eqn{ks} the k-shell index, \eqn{k} the degree, \eqn{d_{mn}}{d_mn}
#' the distance and \eqn{r} the radius.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{ksh_mu} is 0.4 by default, the value of the source's worked
#' example and of two of its tables. The authors tune it per network (0.4,
#' 0.1 and 0.01 in the source) and recommend no single value; it must lie
#' in \eqn{[0, 1]}. \code{ksh_radius} is 3 by default, the value the source
#' recommends; \code{Inf} sums over the whole component. The root covers
#' the shell sum alone and the distance is squared: with these defaults the
#' implementation reproduces the 13 values and the worked value the source
#' prints for its 13-node example (truncated to two decimals) and the 13
#' values Maji et al. (2020) print for the same network, on the edge list
#' read from the figures of both papers. Isolated nodes score 0. A
#' \code{ksh_mu} outside \eqn{[0, 1]}, or a \code{ksh_radius} that is not a
#' whole number of at least one or \code{Inf}, raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ksh_mu} (the weight of the neighbor's degree, default 0.4)
#' and \code{ksh_radius} (the largest distance summed over, default 3).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Namtirtha, A., Dutta, A. and Dutta, B. (2018). Identifying influential
#' spreaders in complex networks based on kshell hybrid method. Physica A:
#' Statistical Mechanics and its Applications, 499, 310-324.
#' \doi{10.1016/j.physa.2018.02.016}.
#'
#' Maji, G., Namtirtha, A., Dutta, A. and Curado Malta, M. (2020).
#' Influential spreaders identification in complex networks with improved
#' k-shell hybrid method. Expert Systems with Applications, 144, 113092.
#' \doi{10.1016/j.eswa.2019.113092}.
#' @seealso \code{\link{centrality_eskh}}, \code{\link{centrality_ikh}},
#' \code{\link{centrality_coreness}}, \code{\link{centrality_gravity}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ksh(regulation_net)
#' centrality_ksh(regulation_net, ksh_mu = 0.1)
centrality_ksh <- function(x, ...) {
  df <- centrality(x, measures = "ksh", ...)
  stats::setNames(df$ksh, df$node)
}

#' Extended K-Shell Hybrid Method
#'
#' The extended k-shell hybrid method (Namtirtha et al. 2018, who write it
#' \eqn{ks_{h+}}{ksh+}) sums the k-shell hybrid score of the neighbors of a
#' node:
#' \deqn{ESKH(m) = \sum_{j \in N(m)} ksh(j),}{ESKH(m) = sum_{j in N(m)} ksh(j),}
#' with \eqn{ksh} the score \code{\link{centrality_ksh}} returns.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The node's own score is not added: the source's worked example and the
#' values it prints for its leaves require the open neighborhood.
#' \code{ksh_mu} and \code{ksh_radius} are those of
#' \code{\link{centrality_ksh}}. With the defaults the implementation
#' reproduces the 13 values Maji et al. (2020) print for the 13-node
#' example and 11 of the 14 values of the source. The other three are one
#' quantity, which the source obtains by adding values already truncated to
#' two decimals (48.03, where the exact value is 48.045). Isolated nodes
#' score 0. A \code{ksh_mu} outside \eqn{[0, 1]}, or a \code{ksh_radius}
#' that is not a whole number of at least one or \code{Inf}, raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ksh_mu} (the weight of the neighbor's degree, default 0.4)
#' and \code{ksh_radius} (the largest distance summed over, default 3).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Namtirtha, A., Dutta, A. and Dutta, B. (2018). Identifying influential
#' spreaders in complex networks based on kshell hybrid method. Physica A:
#' Statistical Mechanics and its Applications, 499, 310-324.
#' \doi{10.1016/j.physa.2018.02.016}.
#'
#' Maji, G., Namtirtha, A., Dutta, A. and Curado Malta, M. (2020).
#' Influential spreaders identification in complex networks with improved
#' k-shell hybrid method. Expert Systems with Applications, 144, 113092.
#' \doi{10.1016/j.eswa.2019.113092}.
#' @seealso \code{\link{centrality_ksh}}, \code{\link{centrality_eikh}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_eskh(regulation_net)
centrality_eskh <- function(x, ...) {
  df <- centrality(x, measures = "eskh", ...)
  stats::setNames(df$eskh, df$node)
}

#' Improved K-Shell Hybrid Method
#'
#' The improved k-shell hybrid method (IKH; Maji et al. 2020) replaces the
#' sum of the k-shell indices in \code{\link{centrality_ksh}} by their
#' product, and its constant weight of the degree by a weight that falls
#' with the distance:
#' \deqn{IKH(i) = \sum_{j:\,1 \le d_{ij} \le r} \frac{\sqrt{ks_i\,ks_j} + \mu(d_{ij})\,k_j}{d_{ij}^2}, \qquad \mu(l) = \frac{2(r - l + 1)}{r(r + 1)}.}{IKH(i) = sum_{j: 1 <= d_ij <= r} (sqrt(ks_i ks_j) + mu(d_ij) k_j) / d_ij^2,  mu(l) = 2 (r - l + 1) / (r (r + 1)).}
#' The weights \eqn{\mu(1), \dots, \mu(r)}{mu(1), ..., mu(r)} sum to 1, so
#' the measure has no free weight.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The radius is \code{ksh_radius}, 3 by default as in all experiments of
#' the source. It must be finite, because the weights are defined over a
#' finite number of levels. The source asks for a radius no larger than the
#' diameter; a larger one is accepted, and the weights of the levels that
#' exist then sum to less than 1. The shell term is the root of the
#' product: the equation, the text, the worked example and the table of the
#' source all use it. The Centrality Zoo prints the root of the sum, taken
#' over from ksh, which reproduces none of the printed values. The
#' implementation reproduces the 13 values of the source's Table 1 and its
#' worked value, on the 13-node network of Namtirtha et al. (2018).
#' Isolated nodes score 0. A \code{ksh_radius} that is not a finite whole
#' number of at least one raises a \code{cograph_bad_parameter} error.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ksh_radius} (the largest distance summed over, default 3).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Maji, G., Namtirtha, A., Dutta, A. and Curado Malta, M. (2020).
#' Influential spreaders identification in complex networks with improved
#' k-shell hybrid method. Expert Systems with Applications, 144, 113092.
#' \doi{10.1016/j.eswa.2019.113092}.
#' @seealso \code{\link{centrality_ksh}}, \code{\link{centrality_eikh}},
#' \code{\link{centrality_coreness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ikh(regulation_net)
#' centrality_ikh(regulation_net, ksh_radius = 2)
centrality_ikh <- function(x, ...) {
  df <- centrality(x, measures = "ikh", ...)
  stats::setNames(df$ikh, df$node)
}

#' Extended Improved K-Shell Hybrid Method
#'
#' The extended improved k-shell hybrid method (Maji et al. 2020, who write
#' it \eqn{ks_{hi+}}{kshi+}) sums the improved k-shell hybrid score of the
#' neighbors of a node:
#' \deqn{EIKH(i) = \sum_{j \in N(i)} IKH(j),}{EIKH(i) = sum_{j in N(i)} IKH(j),}
#' with \eqn{IKH} the score \code{\link{centrality_ikh}} returns.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The node's own score is not added, as the algorithm and the worked
#' example of the source have it. \code{ksh_radius} is that of
#' \code{\link{centrality_ikh}}. The implementation reproduces the 13
#' values of the source's Table 1 and its worked value, on the 13-node
#' network of Namtirtha et al. (2018). Isolated nodes score 0. A
#' \code{ksh_radius} that is not a finite whole number of at least one
#' raises a \code{cograph_bad_parameter} error. \code{normalized = TRUE}
#' divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ksh_radius} (the largest distance summed over, default 3).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Maji, G., Namtirtha, A., Dutta, A. and Curado Malta, M. (2020).
#' Influential spreaders identification in complex networks with improved
#' k-shell hybrid method. Expert Systems with Applications, 144, 113092.
#' \doi{10.1016/j.eswa.2019.113092}.
#' @seealso \code{\link{centrality_ikh}}, \code{\link{centrality_eskh}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_eikh(regulation_net)
centrality_eikh <- function(x, ...) {
  df <- centrality(x, measures = "eikh", ...)
  stats::setNames(df$eikh, df$node)
}

#' Mixed Core, Degree and Entropy
#'
#' Mixed core, degree and entropy (MCDE; Sheikhahmadi and Nematbakhsh 2017)
#' adds the k-shell index of a node, its degree and the entropy of the
#' shells its neighbors lie in:
#' \deqn{MCDE(v) = ks(v) + k_v + H(v), \qquad H(v) = -\sum_i p_i(v) \log_2 p_i(v),}{MCDE(v) = ks(v) + k_v + H(v),  H(v) = -sum_i p_i(v) log2 p_i(v),}
#' where \eqn{p_i(v)} is the share of the neighbors of \eqn{v} in shell
#' \eqn{i}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The three terms have weight 1 and are added as they are, as in the
#' source's experiments and in its worked table. The logarithm is to base
#' 2, as printed. The degree is the largest term, so the score ranks nodes
#' much like the degree (Kendall correlation 0.97 on four test networks);
#' on a network with a single shell the entropy is 0 and the ranking is
#' that of the degree. The implementation reproduces 22 of the 23 scores of
#' the source's Table 2 to one decimal and its rank order, for its 23-node
#' example on the edge list read from its figure. The source prints node 4
#' as 11.5, where the exact value is 11.449: its entropy was rounded twice.
#' Isolated nodes score 0. \code{normalized = TRUE} divides the scores by
#' their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Sheikhahmadi, A. and Nematbakhsh, M. A. (2017). Identification of
#' multi-spreader users in social networks for viral marketing. Journal of
#' Information Science, 43(3), 412-423. \doi{10.1177/0165551516644171}.
#' @seealso \code{\link{centrality_mcdwe}}, \code{\link{centrality_mcsde}},
#' \code{\link{centrality_coreness}}, \code{\link{centrality_degree}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mcde(regulation_net)
centrality_mcde <- function(x, ...) {
  df <- centrality(x, measures = "mcde", ...)
  stats::setNames(df$mcde, df$node)
}

#' Mixed Core, Degree and Weighted Entropy
#'
#' Mixed core, degree and weighted entropy (MCDWE; Sheikhahmadi and
#' Nematbakhsh 2017) is \code{\link{centrality_mcde}} with the entropy
#' weighted toward the inner shells:
#' \deqn{MCDWE(v) = ks(v) + k_v - \sum_i \frac{p_i(v) \log_2 p_i(v)}{K - i + 1},}{MCDWE(v) = ks(v) + k_v - sum_i p_i(v) log2 p_i(v) / (K - i + 1),}
#' where \eqn{p_i(v)} is the share of the neighbors of \eqn{v} in shell
#' \eqn{i} and \eqn{K} the largest shell index of the network.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Neighbors in the innermost shell have weight 1, those one shell further
#' out 1/2, and so on: the weight belongs to the shell of each term, as the
#' source's equation (11) prints it. The Centrality Zoo gives one weight
#' per node, built from the number of distinct shells among the neighbors,
#' which is not the source's. The terms have weight 1 and are added as they
#' are, as for \code{\link{centrality_mcde}}, and the score ranks nodes
#' much like the degree (Kendall correlation 0.97 on four test networks).
#' The source prints no value of this measure, so it is checked against the
#' definition only; the shell shares it uses are those of
#' \code{\link{centrality_mcde}}, whose printed values are reproduced.
#' Isolated nodes score 0. \code{normalized = TRUE} divides the scores by
#' their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Sheikhahmadi, A. and Nematbakhsh, M. A. (2017). Identification of
#' multi-spreader users in social networks for viral marketing. Journal of
#' Information Science, 43(3), 412-423. \doi{10.1177/0165551516644171}.
#' @seealso \code{\link{centrality_mcde}}, \code{\link{centrality_mcsdwe}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mcdwe(regulation_net)
centrality_mcdwe <- function(x, ...) {
  df <- centrality(x, measures = "mcdwe", ...)
  stats::setNames(df$mcdwe, df$node)
}

#' Mixed Core, Semi-Local Degree and Entropy
#'
#' Mixed core, semi-local degree and entropy (MCSDE; Sheikhahmadi and
#' Nematbakhsh 2017) replaces the degree of \code{\link{centrality_mcde}}
#' by the semi-local centrality and rescales the three terms before adding
#' them:
#' \deqn{MCSDE(v) = \widetilde{ks}(v) + \widetilde{SL}(v) + \widetilde{H}(v),}{MCSDE(v) = ks~(v) + SL~(v) + H~(v),}
#' where \eqn{SL} is the score \code{\link{centrality_semilocal}} returns,
#' \eqn{H} the entropy of the shells of the neighbors and the tilde marks
#' \eqn{(x - \min x) / (\max x - \min x)}{(x - min x) / (max x - min x)}
#' over the nodes.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The source names Chen et al.'s semi-local method for the semi-local
#' degree, and says that min-max normalization is used because the three
#' amounts are on very different scales. Its wording reads \eqn{(x - \min)
#' / \max}{(x - min) / max}; the textbook rule it cites, used here, divides
#' by the range. A term that is the same for every node becomes 0. Without
#' the rescaling the semi-local term swamps the other two and the score
#' ranks the nodes as \code{\link{centrality_semilocal}} does. The score
#' lies between 0 and 3. On a network with a single shell the shell and
#' entropy terms are constant, and the ranking is again that of
#' \code{\link{centrality_semilocal}}. The source prints no value of the
#' measure, so the implementation is checked against the definition only.
#' Isolated nodes score 0. \code{normalized = TRUE} divides the scores by
#' their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Sheikhahmadi, A. and Nematbakhsh, M. A. (2017). Identification of
#' multi-spreader users in social networks for viral marketing. Journal of
#' Information Science, 43(3), 412-423. \doi{10.1177/0165551516644171}.
#' @seealso \code{\link{centrality_mcsdwe}}, \code{\link{centrality_mcde}},
#' \code{\link{centrality_semilocal}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mcsde(regulation_net)
centrality_mcsde <- function(x, ...) {
  df <- centrality(x, measures = "mcsde", ...)
  stats::setNames(df$mcsde, df$node)
}

#' Mixed Core, Semi-Local Degree and Weighted Entropy
#'
#' Mixed core, semi-local degree and weighted entropy (MCSDWE; Sheikhahmadi
#' and Nematbakhsh 2017) is \code{\link{centrality_mcsde}} with the
#' weighted entropy of \code{\link{centrality_mcdwe}}:
#' \deqn{MCSDWE(v) = \widetilde{ks}(v) + \widetilde{SL}(v) + \widetilde{WH}(v), \qquad WH(v) = -\sum_i \frac{p_i(v) \log_2 p_i(v)}{K - i + 1},}{MCSDWE(v) = ks~(v) + SL~(v) + WH~(v),  WH(v) = -sum_i p_i(v) log2 p_i(v) / (K - i + 1),}
#' where the tilde marks \eqn{(x - \min x) / (\max x - \min x)}{(x - min x)
#' / (max x - min x)} over the nodes.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The semi-local term and the rescaling are those of
#' \code{\link{centrality_mcsde}}, and the weight of each entropy term that
#' of \code{\link{centrality_mcdwe}}. A term that is the same for every
#' node becomes 0, and the score lies between 0 and 3. On a network with a
#' single shell the ranking is that of \code{\link{centrality_semilocal}}.
#' The source prints no value of the measure, so the implementation is
#' checked against the definition only. Isolated nodes score 0.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Sheikhahmadi, A. and Nematbakhsh, M. A. (2017). Identification of
#' multi-spreader users in social networks for viral marketing. Journal of
#' Information Science, 43(3), 412-423. \doi{10.1177/0165551516644171}.
#' @seealso \code{\link{centrality_mcsde}}, \code{\link{centrality_mcdwe}},
#' \code{\link{centrality_semilocal}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mcsdwe(regulation_net)
centrality_mcsdwe <- function(x, ...) {
  df <- centrality(x, measures = "mcsdwe", ...)
  stats::setNames(df$mcsdwe, df$node)
}

#' Laplacian Gravity Centrality
#'
#' Laplacian gravity centrality (LGC; Zhang et al. 2022) is a gravity model
#' whose masses are the Laplacian centralities of the nodes:
#' \deqn{LGC(i) = \sum_{j \ne i:\,d_{ij} \le \langle d \rangle / 2} \frac{LC(i)\,LC(j)}{d_{ij}^2}, \qquad LC(i) = k_i^2 + k_i + 2 \sum_{j \in N(i)} k_j,}{LGC(i) = sum_{j != i: d_ij <= <d>/2} LC(i) LC(j) / d_ij^2,  LC(i) = k_i^2 + k_i + 2 sum_{j in N(i)} k_j,}
#' with \eqn{k} the degree, \eqn{d_{ij}}{d_ij} the distance and
#' \eqn{\langle d \rangle}{<d>} the average shortest-path length of the
#' network.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \eqn{LC} is the drop in Laplacian energy when the node is removed, the
#' score of \code{\link{centrality_laplacian}} on an unweighted network;
#' the Centrality Zoo uses the relative drop, which changes every score by
#' one constant. The radius is half the average path length, used as it is
#' in the test; the source says that its radius of 1.73 admits neighbors
#' only. On a network whose average path length is below 2 no node lies
#' within the radius, and every score is 0. This is the rule of the source,
#' and it makes the measure uninformative on small dense networks. Pairs in
#' different components contribute nothing, as the source states, and the
#' average is then taken over the pairs that reach each other. The
#' implementation reproduces the 21 Laplacian centralities of the source's
#' Table 1, the score of its worked example (4788) and the 21 rescaled
#' scores of its Table 2, for the ARPA network on an edge list read from
#' its figure. Isolated nodes score 0. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhang, Q., Shuai, B. and \enc{Lü}{Lu}, M. (2022). A novel method to
#' identify influential nodes in complex networks based on gravity
#' centrality. Information Sciences, 618, 98-117.
#' \doi{10.1016/j.ins.2022.10.070}.
#' @seealso \code{\link{centrality_laplacian}},
#' \code{\link{centrality_ksgc}}, \code{\link{centrality_gravity}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_lgc(regulation_net)
centrality_lgc <- function(x, ...) {
  df <- centrality(x, measures = "lgc", ...)
  stats::setNames(df$lgc, df$node)
}

#' Weighted Gravity Model
#'
#' The weighted gravity model (WGravity; Liu et al. 2020) multiplies the
#' degree gravity model of a node by the node's entry of the leading
#' eigenvector of the adjacency matrix:
#' \deqn{C_{WG}(i) = \sum_{j \ne i:\,d_{ij} \le \langle d \rangle / 2} \frac{e_i\,k_i\,k_j}{d_{ij}^2},}{C_WG(i) = sum_{j != i: d_ij <= <d>/2} e_i k_i k_j / d_ij^2,}
#' with \eqn{e} the eigenvector, \eqn{k} the degree, \eqn{d_{ij}}{d_ij} the
#' distance and \eqn{\langle d \rangle}{<d>} the average shortest-path
#' length.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The source calls the eigenvector normalized without naming a norm. It
#' has unit Euclidean length here, which differs from the maximum-one
#' scaling of \code{\link{centrality_eigenvector}} by one constant per
#' network and changes no ranking. "Weighted" refers to this weight of the
#' node, not to edge weights. The radius is that of
#' \code{\link{centrality_lgc}}: half the average path length, used as it
#' is, so every score is 0 on a network whose average path length is below
#' 2. On a disconnected network the eigenvector is nonzero only on the
#' component with the largest spectral radius, and the other components
#' score 0. When two components share that spectral radius the eigenvector
#' is not unique; the measure then warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA}. The source
#' prints no value of the measure, so the implementation is checked against
#' the definition only. Isolated nodes score 0. \code{normalized = TRUE}
#' divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Liu, F., Wang, Z. and Deng, Y. (2020). GMM: A generalized mechanics
#' model for identifying the importance of nodes in complex networks.
#' Knowledge-Based Systems, 193, 105464.
#' \doi{10.1016/j.knosys.2019.105464}.
#' @seealso \code{\link{centrality_gravity}},
#' \code{\link{centrality_eigenvector}}, \code{\link{centrality_lgc}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_wgravity(regulation_net)
centrality_wgravity <- function(x, ...) {
  df <- centrality(x, measures = "wgravity", ...)
  stats::setNames(df$wgravity, df$node)
}

#' Average Shortest Path Centrality
#'
#' Average shortest path centrality (AC; Lv et al. 2019) is the relative
#' growth of the average distance of a network when the ties of a node are
#' removed:
#' \deqn{AC(k) = \frac{|ASP(G'_k) - ASP(G)|}{ASP(G)}, \qquad ASP(G) = \frac{1}{N(N - 1)} \sum_{i \ne j} d_{ij},}{AC(k) = |ASP(G'_k) - ASP(G)| / ASP(G),  ASP(G) = sum_{i != j} d_ij / (N (N - 1)),}
#' where \eqn{G'_k} is the network without the ties of node \eqn{k}, and a
#' pair that cannot reach each other counts the diameter of the original
#' network.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Node \eqn{k} stays in \eqn{G'_k} as an isolated node, so its own pairs
#' count the diameter and \eqn{N(N - 1)} does not change: the source's
#' worked example computes it so, and its printed numbers, nine scores and
#' two averages, follow this reading and not the \eqn{N - 1} nodes its
#' prose describes. The score is therefore never negative. On a
#' disconnected network the unreachable pairs of the network itself also
#' count its largest finite distance. An edgeless network and a one-node
#' network score 0. On a connected network without cut vertices the score
#' is a decreasing linear function of
#' \code{\link{centrality_closeness_vitality}}, so the two rank nodes in
#' exactly opposite order; they differ at cut vertices, where the pairs
#' that are cut off count the diameter. The score ranks nodes almost like
#' \code{\link{centrality_exogenous}} (Kendall correlation 0.997 on the
#' karate club network, 0.94 to 0.95 on three random networks). The
#' implementation reproduces the nine scores the source prints for its
#' 9-node example, on the edge list read from its figure. The distances are
#' computed again for each node, so the cost is that of the distances times
#' the number of nodes. \code{normalized = TRUE} divides the scores by
#' their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Lv, Z., Zhao, N., Xiong, F. and Chen, N. (2019). A novel measure of
#' identifying influential nodes in complex networks. Physica A:
#' Statistical Mechanics and its Applications, 523, 488-497.
#' \doi{10.1016/j.physa.2019.01.136}.
#' @seealso \code{\link{centrality_closeness_vitality}},
#' \code{\link{centrality_exogenous}},
#' \code{\link{centrality_average_distance}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_asp_centrality(regulation_net)
centrality_asp_centrality <- function(x, ...) {
  df <- centrality(x, measures = "asp_centrality", ...)
  stats::setNames(df$asp_centrality, df$node)
}

#' Weight Degree Centrality
#'
#' Weight degree centrality (Wdc; Liu et al. 2016) multiplies the number of
#' non-backtracking two-step walks from a node by a power of its degree:
#' \deqn{Wdc(i) = \Bigl(\sum_{j \in N(i)} k_j - k_i\Bigr) k_i^{\alpha}, \qquad \alpha = |r|,}{Wdc(i) = (sum_{j in N(i)} k_j - k_i) k_i^alpha,  alpha = |r|,}
#' with \eqn{k} the degree and \eqn{r} the degree assortativity of the
#' network.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{wdc_alpha = NULL}, the default, takes \eqn{\alpha = |r|}{alpha =
#' |r|} as the source does; the source adds that other values can be used,
#' and any single finite nonnegative number is accepted. The assortativity
#' is 0/0 when every node with a tie has the same degree, on a regular
#' network for instance; \eqn{\alpha}{alpha} is then 0, the source's
#' neutral case. With \eqn{\alpha = 0}{alpha = 0} the score is
#' \code{\link{centrality_con}} on a simple undirected network, the number
#' of common neighbors a node has with all the others together, and at the
#' default it is that number times the degree to the power \eqn{|r|}. The
#' score ranks nodes much like \code{\link{centrality_con}},
#' \code{\link{centrality_expected}} and \code{\link{centrality_x_degree}}
#' (Kendall correlation up to 0.994 on four test networks, 0.86 at the
#' lowest). The source prints no value on a network available here, so the
#' implementation is checked against the definition only. Isolated nodes
#' score 0. A \code{wdc_alpha} that is negative or not finite raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{wdc_alpha} (the exponent, default \code{NULL} for the
#' absolute degree assortativity).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Liu, Y., Wei, B., Du, Y., Xiao, F. and Deng, Y. (2016). Identifying
#' influential spreaders by weight degree centrality in complex networks.
#' Chaos, Solitons & Fractals, 86, 1-7. \doi{10.1016/j.chaos.2016.01.030}.
#' @seealso \code{\link{centrality_ewdc}}, \code{\link{centrality_con}},
#' \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_wdc(regulation_net)
#' centrality_wdc(regulation_net, wdc_alpha = 0)
centrality_wdc <- function(x, ...) {
  df <- centrality(x, measures = "wdc", ...)
  stats::setNames(df$wdc, df$node)
}

#' Extended Weight Degree Centrality
#'
#' Extended weight degree centrality (EWdc; Liu et al. 2016) applies the
#' construction of \code{\link{centrality_wdc}} once more, with the weight
#' degree centrality in the place of the degree:
#' \deqn{EWdc(i) = \Bigl(\sum_{j \in N(i)} Wdc(j) - Wdc(i)\Bigr) Wdc(i)^{\alpha},}{EWdc(i) = (sum_{j in N(i)} Wdc(j) - Wdc(i)) Wdc(i)^alpha,}
#' with the same \eqn{\alpha}{alpha}, the absolute degree assortativity by
#' default.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{wdc_alpha} and the rule for an undefined assortativity are those
#' of \code{\link{centrality_wdc}}. \eqn{0^0} is taken as 1, so with
#' \eqn{\alpha = 0}{alpha = 0} a node whose weight degree centrality is 0
#' scores the sum of its neighbors' values. The score can be negative, for
#' instance at the leaves of a star; the source does not discuss this, and
#' the value is kept. The source prints no value on a network available
#' here, so the implementation is checked against the definition only.
#' Isolated nodes score 0. A \code{wdc_alpha} that is negative or not
#' finite raises a \code{cograph_bad_parameter} error. \code{normalized =
#' TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{wdc_alpha} (the exponent, default \code{NULL} for the
#' absolute degree assortativity).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Liu, Y., Wei, B., Du, Y., Xiao, F. and Deng, Y. (2016). Identifying
#' influential spreaders by weight degree centrality in complex networks.
#' Chaos, Solitons & Fractals, 86, 1-7. \doi{10.1016/j.chaos.2016.01.030}.
#' @seealso \code{\link{centrality_wdc}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ewdc(regulation_net)
centrality_ewdc <- function(x, ...) {
  df <- centrality(x, measures = "ewdc", ...)
  stats::setNames(df$ewdc, df$node)
}
