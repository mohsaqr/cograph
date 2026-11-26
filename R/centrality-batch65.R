#' Calculate the batch 65 measures
#' @keywords internal
#' @noRd
calculate_diversity_strength <- function(cg, level) {
  .cg_diversity_strength(.cg_skeleton(cg), level)
}

#' @keywords internal
#' @noRd
calculate_ksgc <- function(cg) .cg_ksgc(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_lsc <- function(cg, alpha = 0.2) .cg_lsc(.cg_skeleton(cg), alpha)

#' @keywords internal
#' @noRd
calculate_classified_neighbors <- function(cg,
                                           weights = c(0.4, 0.35, 0.25, 0.1)) {
  .cg_classified_neighbors(.cg_skeleton(cg), weights)
}

#' Hybrid degree centrality; `NA` with a warning when `p` is not given
#' @keywords internal
#' @noRd
calculate_hybrid_degree <- function(cg, p = NULL, alpha = 1000, beta = 0.1) {
  b <- .cg_skeleton(cg)
  if (is.null(p)) {
    .cg_warn_undefined("`hybrid_degree` needs the spreading probability ",
                       "`hc_p`, which has no default, so its column is NA.")
    return(rep(NA_real_, nrow(b)))
  }
  .cg_hybrid_degree(b, p, alpha, beta)
}

#' TOPSIS centrality; `NA` with a warning where it has no value
#' @keywords internal
#' @noRd
calculate_topsis <- function(cg, attribute = "betweenness",
                             weights = c(1, 1, 1) / 3) {
  if (!is.character(attribute) || length(attribute) != 1L ||
        !attribute %in% c("betweenness", "eigenvector")) {
    .cg_stop_bad_parameter(
      "`topsis_attribute` must be \"betweenness\" or \"eigenvector\"")
  }
  .cg_check_weight_vector(weights, "topsis_weights", 3L)
  b <- .cg_skeleton(cg)
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  out <- if (n < 2L) {
    list(value = NA_real_, status = "singleton")
  } else if (.cg_n_components(b) > 1L) {
    list(value = rep(NA_real_, n), status = "disconnected")
  } else {
    .cg_topsis(.cg_topsis_attributes(b, attribute), weights)
  }
  reason <- switch(
    out$status,
    singleton = "the network has one node",
    disconnected = "closeness has no value on a disconnected network",
    uniform = paste("all nodes are equal on all three attributes, so the",
                    "ideal and the anti-ideal coincide"),
    NULL
  )
  if (!is.null(reason)) {
    .cg_warn_undefined("`topsis` has no value on this input, so its column ",
                       "is NA: ", reason, ".")
  }
  out$value
}

#' @keywords internal
#' @noRd
calculate_all_around <- function(cg) .cg_all_around(.cg_skeleton(cg))

#' Effective gravity model; `NA` with a warning where it has no value
#' @keywords internal
#' @noRd
calculate_egm <- function(cg) {
  b <- .cg_skeleton(cg)
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  reason <- if (n < 2L) {
    "the network has one node"
  } else if (.cg_n_components(b) > 1L) {
    paste("the influence radius is built on the eccentricity, which has no",
          "value on a disconnected network")
  }
  if (!is.null(reason)) {
    .cg_warn_undefined("`egm` has no value on this input, so its column is ",
                       "NA: ", reason, ".")
    return(rep(NA_real_, n))
  }
  .cg_egm(b)
}

#' @keywords internal
#' @noRd
calculate_gls <- function(cg) .cg_gls(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_ks_if <- function(cg) .cg_ks_if(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_nl <- function(cg) .cg_nl(.cg_skeleton(cg))

#' @keywords internal
#' @noRd
calculate_entropy_influence <- function(cg, weights = NULL, alpha = 0.4,
                                       omega = 0.6) {
  counts <- .cg_candidate_adjacency(cg, weights, "entropy_influence")
  .cg_entropy_influence(counts, alpha, omega)$total
}

#' @keywords internal
#' @noRd
calculate_multi_evidence <- function(cg, omega = 1, alpha = 3) {
  .cg_multi_evidence(.cg_skeleton(cg), omega, alpha)
}

#' Diversity-Strength Centrality
#'
#' Diversity-strength centrality (DSC; Zareie et al. 2019) is the product
#' of three properties of the neighbors of a node: how deep they lie in the
#' network on average, how evenly that depth is spread among them, and how
#' much their own neighborhoods differ:
#' \deqn{DSC(v) = \frac{S(v)}{k_v} \; H_1(v) \; JSD(v), \qquad S(v) = \sum_{u \in N(v)} IKs(u).}{DSC(v) = (S(v) / k_v) H1(v) JSD(v),  S(v) = sum_{u in N(v)} IKs(u).}
#' \eqn{IKs} is the improved k-shell index, \eqn{H_1(v)}{H1(v)} the entropy
#' of the shares \eqn{IKs(u)/S(v)} and \eqn{JSD(v)} the Jensen-Shannon
#' divergence of the neighbors' shell distributions, where the shell
#' distribution of \eqn{u} gives the share of the neighbors of \eqn{u} in
#' each shell.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \eqn{IKs} is the index \code{\link{centrality_iksd}} returns. The shells
#' of the divergence term are those of the improved index, as section 3.2
#' of the source states, although the symbols of its definition name the
#' classic k-shell. The natural logarithm is used; the source prints no
#' base, and another base multiplies every score by one constant. A node
#' with one neighbor scores 0, and so does a node whose neighbors all have
#' the same shell distribution. Isolated nodes score 0. The source prints
#' no node scores; the implementation reproduces the monotonicity values it
#' prints for the rankings built on this score (see
#' \code{\link{centrality_dsr}}). \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zareie, A., Sheikhahmadi, A. and Jalili, M. (2019). Influential node
#' ranking in social networks based on neighborhood diversity. Future
#' Generation Computer Systems, 94, 120-129.
#' \doi{10.1016/j.future.2018.11.023}.
#' @seealso \code{\link{centrality_dsr}}, \code{\link{centrality_edsr}},
#' \code{\link{centrality_iksd}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dsc(regulation_net)
centrality_dsc <- function(x, ...) {
  df <- centrality(x, measures = "dsc", ...)
  stats::setNames(df$dsc, df$node)
}

#' Diversity-Strength Ranking
#'
#' Diversity-strength ranking (DSR; Zareie et al. 2019) sums the
#' diversity-strength centrality of the neighbors of a node:
#' \deqn{DSR(v) = \sum_{u \in N(v)} DSC(u),}{DSR(v) = sum_{u in N(v)} DSC(u),}
#' with \eqn{DSC} the score \code{\link{centrality_dsc}} returns.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Isolated nodes score 0. The implementation reproduces the monotonicity
#' of the ranking the source prints for the karate club network (0.9403).
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zareie, A., Sheikhahmadi, A. and Jalili, M. (2019). Influential node
#' ranking in social networks based on neighborhood diversity. Future
#' Generation Computer Systems, 94, 120-129.
#' \doi{10.1016/j.future.2018.11.023}.
#' @seealso \code{\link{centrality_dsc}}, \code{\link{centrality_edsr}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dsr(regulation_net)
centrality_dsr <- function(x, ...) {
  df <- centrality(x, measures = "dsr", ...)
  stats::setNames(df$dsr, df$node)
}

#' Extended Diversity-Strength Ranking
#'
#' Extended diversity-strength ranking (EDSR; Zareie et al. 2019) sums the
#' diversity-strength ranking of the neighbors of a node:
#' \deqn{EDSR(v) = \sum_{u \in N(v)} DSR(u),}{EDSR(v) = sum_{u in N(v)} DSR(u),}
#' with \eqn{DSR} the score \code{\link{centrality_dsr}} returns. It
#' reaches the nodes two steps away.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The sum for a neighbor is not restricted, so it includes the node itself
#' once for each of its neighbors, as the equation of the source is
#' printed. Isolated nodes score 0. The implementation reproduces the
#' monotonicity of the ranking the source prints for the karate club
#' network (0.9542). \code{normalized = TRUE} divides the scores by their
#' maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zareie, A., Sheikhahmadi, A. and Jalili, M. (2019). Influential node
#' ranking in social networks based on neighborhood diversity. Future
#' Generation Computer Systems, 94, 120-129.
#' \doi{10.1016/j.future.2018.11.023}.
#' @seealso \code{\link{centrality_dsc}}, \code{\link{centrality_dsr}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_edsr(regulation_net)
centrality_edsr <- function(x, ...) {
  df <- centrality(x, measures = "edsr", ...)
  stats::setNames(df$edsr, df$node)
}

#' K-Shell Based Gravity Centrality
#'
#' K-shell based gravity centrality (KSGC; Yang and Xiao 2021) is a gravity
#' model in which the pull of a node on another depends on their k-shell
#' indices:
#' \deqn{KSGC(i) = \sum_{j:\,d_{ij} \le \langle d \rangle / 2} \exp\!\Bigl(\frac{ks_i - ks_j}{ks_{max} - ks_{min}}\Bigr) \frac{k_i k_j}{d_{ij}^2},}{KSGC(i) = sum_{j: d_ij <= <d>/2} exp((ks_i - ks_j) / (ks_max - ks_min)) k_i k_j / d_ij^2,}
#' with \eqn{k} the degree, \eqn{ks} the k-shell index, \eqn{d_{ij}}{d_ij}
#' the distance and \eqn{\langle d \rangle}{<d>} the average shortest-path
#' length of the network.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The radius is half the average path length, used as it is in the test
#' \eqn{d_{ij} \le \langle d \rangle / 2}{d_ij <= <d>/2}; it is not
#' rounded, unlike the \code{"auto"} radius of
#' \code{\link{centrality_gravity}}. On a network whose average path length
#' is below 2 no node lies within the radius, and every score is 0. This is
#' the rule of the source, and it makes the measure uninformative on small
#' dense networks. When all nodes are in one shell the exponent is 0/0 and
#' the coefficient is taken as 1, which is the plain gravity model; the
#' source says the method reduces to it when two nodes share a shell. The
#' source covers connected networks; on a disconnected one the average is
#' taken over the pairs that reach each other. Isolated nodes score 0. The
#' source prints no value on a small network, so the implementation is
#' checked against the definition only. \code{normalized = TRUE} divides
#' the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Yang, X. and Xiao, F. (2021). An improved gravity model to identify
#' influential nodes in complex networks based on k-shell method.
#' Knowledge-Based Systems, 227, 107198.
#' \doi{10.1016/j.knosys.2021.107198}.
#' @seealso \code{\link{centrality_gravity}},
#' \code{\link{centrality_coreness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ksgc(regulation_net)
centrality_ksgc <- function(x, ...) {
  df <- centrality(x, measures = "ksgc", ...)
  stats::setNames(df$ksgc, df$node)
}

#' Local Structural Centrality
#'
#' Local structural centrality (LSC; Gao et al. 2014) sums, over the
#' neighbors of a node, the size of each neighbor's two-step neighborhood
#' and the clustering inside it:
#' \deqn{LSC(v) = \sum_{u \in N(v)} \Bigl(\alpha\,|N_2(u)| + (1 - \alpha) \sum_{w \in N_2(u)} c_w\Bigr),}{LSC(v) = sum_{u in N(v)} (alpha |N2(u)| + (1 - alpha) sum_{w in N2(u)} c_w),}
#' where \eqn{N_2(u)}{N2(u)} holds the nodes one or two steps from \eqn{u}
#' and \eqn{c_w} is the local clustering coefficient of \eqn{w}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{lsc_alpha} is 0.2 by default, the value the source fixes for all
#' its experiments on real networks; its worked example uses 0.7. With
#' \code{lsc_alpha = 1} the clustering term drops out. The two-step
#' neighborhood does not contain its center, and a node with fewer than two
#' neighbors has clustering 0; both are what the values of the source
#' require. Isolated nodes score 0. The implementation reproduces the ten
#' values the source prints for its 28-node example at \code{lsc_alpha =
#' 0.7}, on an edge list recovered from the other columns of the same
#' table. A \code{lsc_alpha} outside \eqn{[0, 1]} raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{lsc_alpha} (the weight of the neighborhood size against the
#' clustering sum, default 0.2).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Gao, S., Ma, J., Chen, Z., Wang, G. and Xing, C. (2014). Ranking the
#' spreading ability of nodes in complex networks based on local structure.
#' Physica A: Statistical Mechanics and its Applications, 403, 130-147.
#' \doi{10.1016/j.physa.2014.02.032}.
#' @seealso \code{\link{centrality_semilocal}},
#' \code{\link{centrality_transitivity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_lsc(regulation_net)
#' centrality_lsc(regulation_net, lsc_alpha = 0.7)
centrality_lsc <- function(x, ...) {
  df <- centrality(x, measures = "lsc", ...)
  stats::setNames(df$lsc, df$node)
}

#' Classified Neighbors Centrality
#'
#' Classified neighbors centrality (Li et al. 2018) is a weighted degree in
#' which each neighbor counts according to where it stands in the k-shell
#' decomposition relative to the node:
#' \deqn{k_{CN}(i) = \alpha\,e_u + \beta\,e_{eu} + \gamma\,e_{el} + \mu\,e_l,}{k_CN(i) = alpha e_u + beta e_eu + gamma e_el + mu e_l,}
#' where \eqn{e_u} counts the neighbors in a higher shell,
#' \eqn{e_{eu}}{e_eu} those in the same shell removed in the same step or
#' later, \eqn{e_{el}}{e_el} those in the same shell removed earlier, and
#' \eqn{e_l} those in a lower shell.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The decomposition removes nodes in batches, and the batch number is the
#' layer of the \code{"onion"} measure of \code{\link{centrality}}. A
#' neighbor removed in the same batch counts as "same shell, later": the
#' table of the source's example classifies it so, while its algorithm
#' listing would leave it out. \code{cn_weights} holds the four weights in
#' the order of the formula. The default is \code{c(0.4, 0.35, 0.25, 0.1)},
#' equation (4) of the source; the source then divides them by their sum,
#' which changes the scores by one constant and no rank. Isolated nodes
#' score 0. The implementation reproduces the class of every neighbor in
#' the source's 14-node example. A \code{cn_weights} that is not four
#' finite nonnegative numbers, not all zero, raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{cn_weights} (the weights of the four classes of neighbors).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Li, C., Wang, L., Sun, S. and Xia, C. (2018). Identification of
#' influential spreaders based on classified neighbors in real-world
#' complex networks. Applied Mathematics and Computation, 320, 512-523.
#' \doi{10.1016/j.amc.2017.10.001}.
#' @seealso \code{\link{centrality_coreness}},
#' \code{\link{centrality_iksd}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_classified_neighbors(regulation_net)
centrality_classified_neighbors <- function(x, ...) {
  df <- centrality(x, measures = "classified_neighbors", ...)
  stats::setNames(df$classified_neighbors, df$node)
}

#' Hybrid Degree Centrality
#'
#' Hybrid degree centrality (HC; Ma and Ma 2017) blends the degree of a
#' node with its modified local centrality, in a proportion set by the
#' spreading probability \eqn{p} of the process the ranking is meant for:
#' \deqn{HC(v) = (\beta - p)\,\alpha\,k_v + p\,MLC(v),}{HC(v) = (beta - p) alpha k_v + p MLC(v),}
#' with \eqn{k_v} the degree and \eqn{MLC} the score
#' \code{\link{centrality_mlc}} returns. A low \eqn{p} favors the degree
#' and a high \eqn{p} the wider neighborhood.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{hc_p} must be given: the source varies it between 0.01 and 0.1 and
#' names no default. Without it the measure warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA}. \code{hc_alpha}
#' brings the degree to the scale of the modified local centrality and is
#' 1000 by default, the value the source uses throughout. \code{hc_beta} is
#' 0.1 by default; the source tunes it per network between 0.1 and 0.2.
#' With the defaults the degree term dominates on a small network, and the
#' score ranks nodes much like the degree. At \code{hc_beta = hc_p} the
#' score is \eqn{p\,MLC}{p MLC}, and above that the degree enters with a
#' negative sign, a case the source does not use. Isolated nodes score 0.
#' The source prints no value of the measure, so the implementation is
#' checked against the definition only. A \code{hc_p} outside \eqn{[0, 1]},
#' a negative \code{hc_alpha} or a non-finite \code{hc_beta} raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{hc_p} (the spreading probability, required), \code{hc_alpha}
#' (default 1000) and \code{hc_beta} (default 0.1).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ma, Q. and Ma, J. (2017). Identifying and ranking influential spreaders
#' in complex networks with consideration of spreading probability. Physica
#' A: Statistical Mechanics and its Applications, 465, 312-330.
#' \doi{10.1016/j.physa.2016.08.041}.
#' @seealso \code{\link{centrality_mlc}}, \code{\link{centrality_degree}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_hybrid_degree(regulation_net, hc_p = 0.05)
centrality_hybrid_degree <- function(x, ...) {
  df <- centrality(x, measures = "hybrid_degree", ...)
  stats::setNames(df$hybrid_degree, df$node)
}

#' TOPSIS Centrality
#'
#' TOPSIS centrality (Du et al. 2014) ranks nodes by their closeness to an
#' ideal node on three centrality measures at once. Each measure is divided
#' by its Euclidean norm over the nodes and multiplied by its weight; the
#' ideal node has the largest value on every measure and the anti-ideal
#' node the smallest. With \eqn{S^+_i}{S+_i} and \eqn{S^-_i}{S-_i} the
#' Euclidean distances of node \eqn{i} to the two,
#' \deqn{C_i = \frac{S^-_i}{S^-_i + S^+_i}.}{C_i = S-_i / (S-_i + S+_i).}
#' The measures are degree, closeness and either betweenness or eigenvector
#' centrality.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \code{topsis_attribute} names the third measure: \code{"betweenness"}
#' (the default), which the source uses where traffic follows shortest
#' paths, or \code{"eigenvector"}, which it uses where traffic spreads
#' along walks. \code{topsis_weights} holds the weights of degree,
#' closeness and the third measure. The default is one third each: the
#' source states no weights for networks, and uses equal weights in its
#' only numerical illustration. Only the proportions of the weights matter.
#' The score lies in \eqn{[0, 1]}. It has no value on a disconnected
#' network, where closeness is undefined, on a one-node network, and on a
#' network whose nodes are all equal on the three measures (a measure whose
#' range is within 1e-12 of its largest value counts as equal); there the
#' measure warns (class \code{cograph_undefined_measure}) and returns
#' \code{NA}. The source prints no score of a network node; the
#' implementation reproduces the distances and the ranking of its numerical
#' illustration. \code{\link{centrality_topsis_re}} is a later variant with
#' four measures, fixed weights and a relative-entropy distance. The
#' weighted TOPSIS of Hu et al. (2016) is this measure with the betweenness
#' and weights taken from a simulated spreading process; given those
#' weights through \code{topsis_weights}, it gives the same scores. Its
#' weight rule is not built in: the spreading values it prints do not
#' follow from the model it states, and the weights it gives change
#' strongly from one simulation to the next. With the weights its worked
#' example multiplies into the attributes, \code{topsis_weights = c(0.5824,
#' 0.0705, 0.3471)}, the implementation reproduces the six scores and the
#' ranking of its Table 2; the example prints these weights in the order
#' degree, betweenness, closeness but applies them in the order degree,
#' closeness, betweenness. An unknown \code{topsis_attribute}, or a
#' \code{topsis_weights} that is not three finite nonnegative numbers, not
#' all zero, raises a \code{cograph_bad_parameter} error. \code{normalized
#' = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{topsis_attribute} (\code{\"betweenness\"} or
#' \code{\"eigenvector\"}) and \code{topsis_weights} (three weights,
#' default one third each).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Du, Y., Gao, C., Hu, Y., Mahadevan, S. and Deng, Y. (2014). A new method
#' of identifying influential nodes in complex networks based on TOPSIS.
#' Physica A: Statistical Mechanics and its Applications, 399, 57-69.
#' \doi{10.1016/j.physa.2013.12.031}.
#' Hu, J., Du, Y., Mo, H., Wei, D. and Deng, Y. (2016). A modified weighted
#' TOPSIS to identify influential nodes in complex networks. Physica A:
#' Statistical Mechanics and its Applications, 444, 73-85.
#' \doi{10.1016/j.physa.2015.09.028}.
#' @seealso \code{\link{centrality_topsis_re}},
#' \code{\link{centrality_betweenness}},
#' \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_topsis(regulation_net)
#' centrality_topsis(regulation_net, topsis_attribute = "eigenvector")
centrality_topsis <- function(x, ...) {
  df <- centrality(x, measures = "topsis", ...)
  stats::setNames(df$topsis, df$node)
}

#' All-Around Score
#'
#' The all-around score (Hou et al. 2012) is the length of the vector
#' formed by three rescaled measures of a node, its degree \eqn{k}, its
#' betweenness \eqn{C_B} and its k-shell index \eqn{k_s}:
#' \deqn{d(i) = \sqrt{\tilde{k}(i)^2 + \tilde{C}_B(i)^2 + \tilde{k}_s(i)^2}.}{d(i) = sqrt(k~(i)^2 + C_B~(i)^2 + k_s~(i)^2).}
#' A node scores high when it is high on all three.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The tilde marks a measure divided by its largest value over the nodes.
#' The source says only that the three measures are brought to a common
#' range and gives no formula; dividing by the maximum keeps 0 at 0 and is
#' defined when a measure is the same for every node. A measure that is 0
#' for every node adds nothing. The score lies between 0 and the square
#' root of 3. Isolated nodes score 0. The source prints no value of the
#' measure, so the implementation is checked against the definition only.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Hou, B., Yao, Y. and Liao, D. (2012). Identifying all-around nodes for
#' spreading dynamics in complex networks. Physica A: Statistical Mechanics
#' and its Applications, 391(15), 4012-4017.
#' \doi{10.1016/j.physa.2012.02.033}.
#' @seealso \code{\link{centrality_degree}},
#' \code{\link{centrality_betweenness}}, \code{\link{centrality_coreness}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_all_around(regulation_net)
centrality_all_around <- function(x, ...) {
  df <- centrality(x, measures = "all_around", ...)
  stats::setNames(df$all_around, df$node)
}

#' Effective Gravity Model
#'
#' The effective gravity model (EGM; Li and Xiao 2021) is a gravity model
#' without a free radius. Each node has a value \eqn{V_i = I_i k_i}, its
#' degree times the entropy of the degrees in its closed neighborhood, and
#' its own radius of influence \eqn{R_i}:
#' \deqn{EGM(i) = \sum_{j \ne i:\,d_{ij} \le R_i} \frac{V_i V_j}{d_{ij}^2}, \qquad R_i = \frac{ecc_i}{1 + \sqrt{\bar{k}_{far} / k_i}},}{EGM(i) = sum_{j != i: d_ij <= R_i} V_i V_j / d_ij^2,  R_i = ecc_i / (1 + sqrt(k_far / k_i)),}
#' where \eqn{ecc_i} is the eccentricity of \eqn{i} and
#' \eqn{\bar{k}_{far}}{k_far} the mean degree of the nodes at that
#' distance.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The entropy is \eqn{I_i = -\sum_j p_j \log p_j}{I_i = -sum_j p_j log
#' p_j} over the node and its neighbors, with \eqn{p_j} the degree of
#' \eqn{j} as a share of their degrees. The natural logarithm is used; the
#' source prints no base, and another base multiplies every score by one
#' constant. The radius is a real number and is not rounded, so a node
#' whose radius is below 1 scores 0 although it has neighbors. The measure
#' has no value on a disconnected network, where the eccentricity is
#' infinite, or on a one-node network; there it warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA}. The source
#' prints no value of the measure, so the implementation is checked against
#' the definition only. \code{normalized = TRUE} divides the scores by
#' their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Li, S. and Xiao, F. (2021). The identification of crucial spreaders in
#' complex networks by effective gravity model. Information Sciences, 578,
#' 725-749. \doi{10.1016/j.ins.2021.08.026}.
#' @seealso \code{\link{centrality_gravity}},
#' \code{\link{centrality_ksgc}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_egm(regulation_net)
centrality_egm <- function(x, ...) {
  df <- centrality(x, measures = "egm", ...)
  stats::setNames(df$egm, df$node)
}

#' Global and Local Structure Centrality
#'
#' Global and local structure centrality (GLS; Sheng et al. 2020)
#' multiplies a global term, built on the neighbors a node shares with
#' every other node, by a local term:
#' \deqn{GLS(i) = \Bigl(k_i \sum_{j \ne i} 1.1^{c_{ij}}\Bigr) \frac{k_i}{n - 1},}{GLS(i) = (k_i sum_{j != i} 1.1^(c_ij)) k_i / (n - 1),}
#' with \eqn{k_i} the degree, \eqn{c_{ij}}{c_ij} the number of common
#' neighbors of \eqn{i} and \eqn{j}, and \eqn{n} the number of nodes.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The formula is the one the numbers of the source follow, which differs
#' from its printed equations in two places. The global sum runs over all
#' other nodes, as the text and the worked example of the source say, where
#' the subscript of its equation (4) names the neighbors only. The local
#' term is the degree divided by \eqn{n - 1}: that is what equation (6)
#' gives in the worked example, which assigns every neighbor the selection
#' probability of the node itself. This reading reproduces the worked value
#' (9.58), the rank order of the source's 12-node example and its top ten
#' nodes of the karate club network; the printed equations reproduce none
#' of the three. The base 1.1 is fixed by the source. The score ranks nodes
#' almost like \code{\link{centrality_lnc}} (Kendall correlation of 0.99 on
#' four test networks). Every other node adds at least 1 to the sum,
#' including nodes in other components. Isolated nodes score 0.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Sheng, J., Dai, J., Wang, B., Duan, G., Long, J., Zhang, J., Guan, K.,
#' Hu, S., Chen, L. and Guan, W. (2020). Identifying influential nodes in
#' complex networks based on global and local structure. Physica A:
#' Statistical Mechanics and its Applications, 541, 123262.
#' \doi{10.1016/j.physa.2019.123262}.
#' @seealso \code{\link{centrality_degree}},
#' \code{\link{centrality_topological_coefficient}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_gls(regulation_net)
centrality_gls <- function(x, ...) {
  df <- centrality(x, measures = "gls", ...)
  stats::setNames(df$gls, df$node)
}

#' K-Shell Iteration Factor
#'
#' The k-shell iteration factor (KS-IF; Wang et al. 2016) refines the
#' k-shell index by the iteration in which a node is removed within its
#' shell, and sums the result, weighted by degree, over a node and its
#' neighbors:
#' \deqn{IC(i) = \delta_i k_i + \sum_{j \in N(i)} \delta_j k_j, \qquad \delta_i = ks_i \Bigl(1 + \frac{n_i}{m_i}\Bigr),}{IC(i) = delta_i k_i + sum_{j in N(i)} delta_j k_j,  delta_i = ks_i (1 + n_i / m_i),}
#' where \eqn{ks_i} is the k-shell index of \eqn{i}, \eqn{m_i} the number
#' of iterations the decomposition needs to remove that shell and \eqn{n_i}
#' the iteration in which \eqn{i} is removed.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Within the stage of shell \eqn{k}, an iteration removes every remaining
#' node whose degree is at most \eqn{k}; iterations are counted from 1 in
#' each shell and over the whole network, so components share them. The
#' nodes removed last in a shell have \eqn{\delta = 2\,ks}{delta = 2 ks}.
#' Isolated nodes score 0. No published value checks the implementation:
#' the source's illustration cannot be produced by this decomposition on
#' any network, and its two tables disagree about one node.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wang, Z., Zhao, Y., Xi, J. and Du, C. (2016). Fast ranking influential
#' nodes in complex networks using a k-shell iteration factor. Physica A:
#' Statistical Mechanics and its Applications, 461, 171-181.
#' \doi{10.1016/j.physa.2016.05.048}.
#' @seealso \code{\link{centrality_coreness}},
#' \code{\link{centrality_hks}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ks_if(regulation_net)
centrality_ks_if <- function(x, ...) {
  df <- centrality(x, measures = "ks_if", ...)
  stats::setNames(df$ks_if, df$node)
}

#' NL Centrality
#'
#' NL centrality (Shao et al. 2019) adds, for each neighbor of a node, the
#' size of the neighbor's two-step neighborhood and the importance of the
#' tie to it:
#' \deqn{NL(v) = \sum_{u \in N(v)} \Bigl(\phi(u) + I_{uv} \frac{k_v - 1}{k_v + k_u - 2}\Bigr),}{NL(v) = sum_{u in N(v)} (phi(u) + I_uv (k_v - 1) / (k_v + k_u - 2)),}
#' where \eqn{\phi(u)}{phi(u)} is the number of nodes within two steps of
#' \eqn{u}, \eqn{k} the degree and \eqn{I_{uv}}{I_uv} the importance of the
#' tie, \eqn{(k_u - t - 1)(k_v - t - 1) / (t/2 + 1)} with \eqn{t} the
#' number of triangles on it.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The second term is the tie contribution of \code{\link{centrality_dil}},
#' so the score equals the sum of \eqn{\phi}{phi} over the neighbors plus
#' that measure minus the degree. \eqn{\phi(u)}{phi(u)} does not count
#' \eqn{u} itself: the wording and the example of the source list other
#' nodes only, and its tables were not available to test this. The first
#' term carries most of the score. The share of an isolated tie is 0/0 and
#' is taken as 0. Isolated nodes score 0. No published value checks the
#' implementation. \code{normalized = TRUE} divides the scores by their
#' maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Shao, Z., Liu, S., Zhao, Y. and Liu, Y. (2019). Identifying influential
#' nodes in complex networks based on Neighbours and edges. Peer-to-Peer
#' Networking and Applications, 12, 1528-1537.
#' \doi{10.1007/s12083-018-0681-x}.
#' @seealso \code{\link{centrality_dil}}, \code{\link{centrality_lsc}},
#' \code{\link{centrality_semilocal}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_nl(regulation_net)
centrality_nl <- function(x, ...) {
  df <- centrality(x, measures = "nl", ...)
  stats::setNames(df$nl, df$node)
}

#' Entropy-Based Social Influence
#'
#' The entropy-based influence of Peng et al. (2017) combines the direct
#' influence of a node on its friends with its indirect influence on the
#' friends of its friends:
#' \deqn{I_i = \omega\,DI_i + (1 - \omega)\,II_i, \qquad DI_i = \alpha \log_{10} n_i + (1 - \alpha)\,H_i.}{I_i = omega DI_i + (1 - omega) II_i,  DI_i = alpha log10(n_i) + (1 - alpha) H_i.}
#' Two nodes are friends when each has interacted with the other, \eqn{n_i}
#' is the number of friends of \eqn{i}, and \eqn{H_i} is the entropy, to
#' base 10, of the shares of its outgoing interactions that go to each
#' friend. \eqn{II_i} averages, over the nodes two steps away, \eqn{DI_i}
#' times the mean \eqn{DI} of the friends that connect the two.
#'
#' @details
#' The measure reads the edge weights as numbers of interactions and uses
#' direction: a tie counts only when it runs both ways, and the entropy
#' uses the outgoing weights. On an undirected network every tie is mutual;
#' on a directed network without mutual ties every score is 0. With
#' \code{weighted = FALSE}, or on an unweighted network, \eqn{H_i =
#' \log_{10} n_i}{H_i = log10(n_i)} and the direct influence is the
#' logarithm of the degree. Loops are ignored. \code{ei_alpha} is 0.4 and
#' \code{ei_omega} 0.6 by default, the values of the source's worked
#' example. A node with no node two steps away has indirect influence 0,
#' and a node without friends scores 0; the source leaves both cases open.
#' The implementation reproduces the direct influence of all 11 nodes of
#' the source's worked example and the indirect influence of 8; the other 3
#' printed values follow from slips in its hand calculation, and the
#' printed ranking is unchanged. The Centrality Zoo's improved
#' entropy-based centrality cites this source, but its formula is that of
#' Ni et al. (2020), which is \code{\link{centrality_improved_entropy}};
#' this measure is not a Zoo label. An \code{ei_alpha} or \code{ei_omega}
#' outside \eqn{[0, 1]} raises a \code{cograph_bad_parameter} error, and
#' negative weights a \code{cograph_negative_weights} error.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ei_alpha} (the weight of the friend count in the direct
#' influence, default 0.4) and \code{ei_omega} (the weight of the direct
#' influence, default 0.6).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Peng, S., Yang, A., Cao, L., Yu, S. and Xie, D. (2017). Social influence
#' modeling using information theory in mobile social networks. Information
#' Sciences, 379, 146-159. \doi{10.1016/j.ins.2016.08.023}.
#' @seealso \code{\link{centrality_entropy}},
#' \code{\link{centrality_diversity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_entropy_influence(to_undirected(regulation_net))
centrality_entropy_influence <- function(x, ...) {
  df <- centrality(x, measures = "entropy_influence", ...)
  stats::setNames(df$entropy_influence, df$node)
}

#' Multi-Evidence Centrality
#'
#' Multi-evidence centrality (MeC; Mo and Deng 2019) fuses four measures of
#' a node with Dempster's rule of combination. For each measure \eqn{X}, a
#' node is given a degree of belief that it is important and one that it is
#' not,
#' \deqn{m_X(h) = \frac{X_i - X_{min}}{X_{max} - X_{min} + \omega}, \qquad m_X(l) = \frac{X_{max} - X_i}{X_{max} - X_{min} + \omega},}{m_X(h) = (X_i - X_min) / (X_max - X_min + omega),  m_X(l) = (X_max - X_i) / (X_max - X_min + omega),}
#' and the rest is left undecided. The four assignments are combined, and
#' the score is the combined belief in "important" minus that in "not
#' important". The measures are degree, betweenness, efficiency and
#' correlation centrality.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Degree is divided by \eqn{n - 1}, betweenness by the number of pairs of
#' other nodes, and efficiency is the sum of reciprocal distances divided
#' by \eqn{n}; the fourth measure is
#' \code{\link{centrality_correlation_centrality}} with its
#' \code{coc_alpha}. The score lies between -1 and 1. \code{evidence_omega}
#' is 1 by default, the value of the source's example; a smaller value
#' leaves less belief undecided and can change the ranking, although the
#' source says it does not. Nodes that cannot be reached add nothing to
#' efficiency. The implementation reproduces the 21 values the source
#' prints for the ARPA network to three decimals, on an edge list recovered
#' from the other columns of the same table. An \code{evidence_omega} that
#' is not above 0 and at most 1 raises a \code{cograph_bad_parameter}
#' error. \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{evidence_omega} (default 1) and \code{coc_alpha} (the
#' distance exponent of correlation centrality, default 3).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Mo, H. and Deng, Y. (2019). Identifying node importance based on
#' evidence theory in complex networks. Physica A: Statistical Mechanics
#' and its Applications, 529, 121538. \doi{10.1016/j.physa.2019.121538}.
#' @seealso \code{\link{centrality_correlation_centrality}},
#' \code{\link{centrality_harmonic}}, \code{\link{centrality_betweenness}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_multi_evidence(regulation_net)
centrality_multi_evidence <- function(x, ...) {
  df <- centrality(x, measures = "multi_evidence", ...)
  stats::setNames(df$multi_evidence, df$node)
}
