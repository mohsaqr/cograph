#' Calculate the batch 67 measures
#' @keywords internal
#' @noRd
calculate_mapping_entropy <- function(cg) .cg_mapping_entropy(.cg_skeleton(cg))

#' Degree and clustering coefficient centrality; `NA` with a warning where it
#' has no value
#' @keywords internal
#' @noRd
calculate_dcc <- function(cg) {
  out <- .cg_dcc(.cg_skeleton(cg))
  reason <- switch(
    out$status,
    no_triangle = paste("the network has no triangle, so the clustering term",
                        "is 0 at every node and the entropy weights of",
                        "equations (10)-(12) are 0 / 0"),
    degenerate = paste("the two entropies add up to 2, so equation (12)",
                       "divides by zero"),
    NULL
  )
  if (!is.null(reason)) {
    .cg_warn_undefined("`dcc` has no value on this input, so its column is ",
                       "NA: ", reason, ".")
  }
  out$value
}

#' @keywords internal
#' @noRd
calculate_edge_clustering <- function(cg) {
  .cg_edge_clustering(.cg_skeleton(cg))
}

#' AHP centrality; `NA` with a warning where closeness has no value
#' @keywords internal
#' @noRd
calculate_ahp <- function(cg, weights = c(1, 1, 1) / 3) {
  .cg_check_weight_vector(weights, "ahp_weights", 3L)
  b <- .cg_skeleton(cg)
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  reason <- if (n < 2L) {
    "the network has one node, whose closeness (1 / sum of distances) is 1 / 0"
  } else if (.cg_n_components(b) > 1L) {
    "closeness (1 / sum of distances) has no value on a disconnected network"
  }
  if (!is.null(reason)) {
    .cg_warn_undefined("`ahp` has no value on this input, so its column is ",
                       "NA: ", reason, ".")
    return(rep(NA_real_, n))
  }
  .cg_ahp(b, weights)
}

#' Semi-local iterative algorithm; `NA` with a warning off its domain and a
#' warning when the iteration does not settle
#' @keywords internal
#' @noRd
calculate_semi_ia <- function(cg, radius = NULL, gamma = 0.2,
                              max_iter = 10000) {
  .cg_check_semi_ia_args(radius, gamma, max_iter)
  b <- .cg_skeleton(cg)
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  reason <- if (n < 2L) {
    "the network has one node, so X(1) = 0 and equation (9) divides by zero"
  } else if (.cg_n_components(b) > 1L) {
    paste("the source defines it on a connected network, and on a",
          "disconnected one the iteration keeps only the component with the",
          "largest leading eigenvalue")
  }
  if (!is.null(reason)) {
    .cg_warn_undefined("`semi_ia` has no value on this input, so its column ",
                       "is NA: ", reason, ".")
    return(rep(NA_real_, n))
  }
  out <- .cg_semi_ia(b, radius, gamma, max_iter)
  if (!out$converged) {
    warning(warningCondition(
      sprintf(paste0("`semi_ia` did not settle: after %d updates the largest ",
                     "change of the normalised scores is %g, still at or ",
                     "above 1e-12. The returned scores are the last iterate ",
                     "and depend on that bound; raise `semi_ia_max_iter`. ",
                     "With `semi_ia_radius = 1` the update has no steady ",
                     "state on a bipartite network."),
              out$iterations, out$gap),
      class = "cograph_no_converge", call = NULL))
  }
  out$value
}

#' Synthesize centrality; `NA` with a warning on a one-node network
#' @keywords internal
#' @noRd
calculate_synthesize <- function(cg) {
  b <- .cg_skeleton(cg)
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  if (n < 2L) {
    .cg_warn_undefined("`synthesize` has no value on this input, so its ",
                       "column is NA: the network has one node, and ",
                       "equations (6)-(8) divide by n - 1 = 0.")
    return(NA_real_)
  }
  .cg_synthesize(b)
}

#' Inward accessibility; `NA` with a warning without `ia_steps`, on one
#' node, or past the enumeration limit
#' @keywords internal
#' @noRd
calculate_inward_accessibility <- function(cg, steps = NULL, stuck = "vanish",
                                           limit = 1e6) {
  .cg_check_ia_args(steps, stuck)
  .cg_check_enumeration_limit(limit)
  b <- .cg_skeleton(cg)
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  reason <- if (is.null(steps)) {
    "it needs the number of steps `ia_steps`, which has no default"
  } else if (n < 2L) {
    "the network has one node, and equation (4) divides by N - 1 = 0"
  }
  if (!is.null(reason)) {
    .cg_warn_undefined("`inward_accessibility` has no value on this input, ",
                       "so its column is NA: ", reason, ".")
    return(rep(NA_real_, n))
  }
  .cg_enumerated("inward_accessibility", n,
                 .cg_inward_accessibility(b, steps, stuck, limit))
}

#' Improved entropy-based centrality; `NA` with a warning on a directed input
#' @keywords internal
#' @noRd
calculate_improved_entropy <- function(cg, weights = NULL, beta = 2) {
  .cg_check_ie_beta(beta)
  w <- .cg_candidate_adjacency(cg, weights, "improved_entropy")
  diag(w) <- 0
  n <- nrow(w)
  if (n == 0L) return(numeric(0))
  if (!isSymmetric(unname(w))) {
    .cg_warn_undefined("`improved_entropy` has no value on this input, so ",
                       "its column is NA: the source defines it for ",
                       "undirected networks.")
    return(rep(NA_real_, n))
  }
  .cg_improved_entropy(w, beta)
}

#' Mapping Entropy
#'
#' Mapping entropy (ME; Nie et al. 2016) weighs the logarithms of the
#' degree centralities of a node's neighbors by the node's own degree
#' centrality:
#' \deqn{ME_i = -DC_i \sum_{j \in N(i)} \log DC_j, \qquad DC_i = \frac{k_i}{N - 1},}{ME_i = -DC_i sum_{j in N(i)} log DC_j,  DC_i = k_i / (N - 1),}
#' with \eqn{k} the degree and \eqn{N} the number of nodes.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The source cites degree centrality for \eqn{DC} without giving a
#' formula; \eqn{DC} is the degree divided by \eqn{N - 1}, the usual degree
#' centrality. Only with this reading is the source's equation (1) an
#' entropy, and only with it does removing the nodes in decreasing order of
#' the score beat removing them by degree on the two networks of the
#' source's Table 1 that could be checked, as the source reports. The score
#' is then never negative and grows with the degree. The Centrality Zoo and
#' Omar and Plapper (2021) use the raw degree, which makes every score zero
#' or negative and ranks low-degree nodes first; the two readings order the
#' nodes differently, not on another scale. The logarithm is natural; the
#' base only rescales. \code{\link{centrality_local_entropy}} is equation
#' (2) of the same source with the raw degree, a different quantity: the
#' two have Kendall correlations between -0.56 and -0.27 on four test
#' networks. The score ranks nodes in nearly the reverse order of
#' \code{local_bridging} (mean Kendall correlation -0.98 over the four
#' networks, -0.97 at the least) and much like the degree (0.88 to 0.93)..
#' The source prints no score, so the implementation is checked against the
#' definition only. Isolated nodes, and the node of a one-node network,
#' score 0. \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Nie, T., Guo, Z., Zhao, K. and Lu, Z. (2016). Using mapping entropy to
#' identify node centrality in complex networks. Physica A: Statistical
#' Mechanics and its Applications, 453, 290-297.
#' \doi{10.1016/j.physa.2016.02.009}.
#' @seealso \code{\link{centrality_local_entropy}},
#' \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mapping_entropy(regulation_net)
centrality_mapping_entropy <- function(x, ...) {
  df <- centrality(x, measures = "mapping_entropy", ...)
  stats::setNames(df$mapping_entropy, df$node)
}

#' Degree and Clustering Coefficient Centrality
#'
#' Degree and clustering coefficient centrality (DCC; Yang et al. 2020)
#' adds a degree term and a clustering term, weighted by the entropy weight
#' method:
#' \deqn{DCC(i) = \alpha I_D(i) + \beta I_C(i), \qquad I_D(i) = k_i + \sum_{j \in N(i)} k_j, \qquad I_C(i) = e^{-C_i} \sum_{j \in N_2(i)} C_j,}{DCC(i) = alpha I_D(i) + beta I_C(i),  I_D(i) = k_i + sum_{j in N(i)} k_j,  I_C(i) = exp(-C_i) sum_{j in N2(i)} C_j,}
#' with \eqn{k} the degree, \eqn{C} the local clustering coefficient and
#' \eqn{N_2(i)}{N2(i)} the nodes within two steps of \eqn{i}. Each
#' attribute is divided by its Euclidean norm over the nodes, \eqn{E_1} and
#' \eqn{E_2} are the entropies of the results divided by \eqn{\ln n}{ln n},
#' and \eqn{\alpha = (1 - E_1) / (2 - E_1 - E_2)}{alpha = (1 - E_1) / (2 -
#' E_1 - E_2)}, \eqn{\beta = 1 - \alpha}{beta = 1 - alpha}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \eqn{N_2(i)}{N2(i)} holds the nodes one or two steps away: the
#' clustering sum the source prints for node 8 of its Fig. 1(b), and its
#' karate top five, need that reading; the Centrality Zoo says exactly two
#' steps. The clustering coefficient is 0 below degree 2. The weights are
#' computed for each network, so adding a component changes every score. On
#' a network without a triangle the clustering term is 0 at every node and
#' the entropy weights are 0/0; the measure then warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA}. It does the
#' same when the two entropies add up to 2, where the weights divide by
#' zero. The implementation reproduces the karate top five of the source's
#' Table 3 and the share of tied scores of its Table 2 (32.35 percent). The
#' source's worked example on its Fig. 1(b) prints 23 numbers: the 14
#' attribute values among them follow from the network drawn there, but its
#' normalised attributes, its entropy, its weight \eqn{\alpha}{alpha} and
#' its score of node 1 (16.2677) do not. The printed entropy weights cannot
#' be reproduced from the printed network; the score of node 1 here is
#' 15.7429. On four test networks the score ranks nodes closest to
#' \code{diffusion} and the two-step expected influence (mean Kendall
#' correlation 0.94) and to \code{\link{centrality_semilocal}} (0.88 to
#' 0.96). \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Yang, Y., Wang, X., Chen, Y., Hu, M. and Ruan, C. (2020). A novel
#' centrality of influential nodes identification in complex networks. IEEE
#' Access, 8, 58742-58751. \doi{10.1109/ACCESS.2020.2983053}.
#' @seealso \code{\link{centrality_semilocal}},
#' \code{\link{centrality_transitivity}}, \code{\link{centrality_degree}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dcc(to_undirected(regulation_net))
centrality_dcc <- function(x, ...) {
  df <- centrality(x, measures = "dcc", ...)
  stats::setNames(df$dcc, df$node)
}

#' Edge Clustering Coefficient Centrality
#'
#' Edge clustering coefficient centrality (NC; Wang et al. 2012) sums, over
#' the edges of a node, the share of the triangles each edge could be part
#' of that it is part of:
#' \deqn{NC(u) = \sum_{v \in N(u)} \frac{z_{uv}}{\min(k_u - 1, k_v - 1)},}{NC(u) = sum_{v in N(u)} z_uv / min(k_u - 1, k_v - 1),}
#' with \eqn{z_{uv}}{z_uv} the number of triangles on the edge \eqn{(u, v)}
#' and \eqn{k} the degree.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' There is no \eqn{+1} in the numerator: the source's worked value
#' \eqn{ECC(1, 4) = 2/3} on its Fig. 1 rules out the form of Radicchi et
#' al. An edge with an end of degree one is in no triangle and its
#' denominator is 0; the source does not say what such an edge contributes,
#' and it contributes 0 here. Isolated nodes score 0. \eqn{NC(u)} is at
#' least the degree times the local clustering coefficient, with equality
#' when no neighbor has a smaller degree than \eqn{u}. The source prints
#' one edge value and no node score, so the node scores are checked against
#' the definition only. On four test networks it ranks nodes closest to
#' \code{lac} and \code{redundancy} (mean Kendall correlation 0.90) and
#' \code{mnc} (0.86). \code{normalized = TRUE} divides the scores by their
#' maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wang, J., Li, M., Wang, H. and Pan, Y. (2012). Identification of
#' essential proteins based on edge clustering coefficient. IEEE/ACM
#' Transactions on Computational Biology and Bioinformatics, 9(4),
#' 1070-1080. \doi{10.1109/TCBB.2011.147}.
#' @seealso \code{\link{centrality_transitivity}},
#' \code{\link{centrality_mnc}}, \code{\link{centrality_support}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_edge_clustering(regulation_net)
centrality_edge_clustering <- function(x, ...) {
  df <- centrality(x, measures = "edge_clustering", ...)
  stats::setNames(df$edge_clustering, df$node)
}

#' Analytic Hierarchy Process Centrality
#'
#' Analytic hierarchy process centrality (AHP; Bian et al. 2017) adds the
#' shares a node holds of the degree, the betweenness and the closeness of
#' the network, each discounted by the share of nodes where that measure is
#' not zero:
#' \deqn{AHP(i) = \sum_{j=1}^{3} w_j \frac{N^+_j}{N} \frac{x_{ij}}{\sum_l x_{lj}},}{AHP(i) = sum_j w_j (N+_j / N) x_ij / sum_l x_lj,}
#' with \eqn{x_{i1}}{x_i1} the degree, \eqn{x_{i2}}{x_i2} the betweenness,
#' \eqn{x_{i3}}{x_i3} the closeness (the reciprocal of the sum of
#' distances), \eqn{N^+_j}{N+_j} the number of nodes where \eqn{x_{lj} >
#' 0}{x_lj > 0} and \eqn{w} the weights.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' This is the computation of the source's worked example: for each measure
#' a matrix of the ratios between the nodes, with 0 where either value is
#' 0, whose priority vector is the column-normalised row mean, which gives
#' the formula above. The source's equation (10) names the principal
#' eigenvector instead, which its printed numbers do not follow. The source
#' derives the weights from a simulated spreading process whose printed
#' values cannot be regenerated; evaluated as the source evaluates it, its
#' weight rule gives weights within a few thousandths of one third for any
#' realistic spreading count. \code{ahp_weights} holds the weights of
#' degree, betweenness and closeness, one third each by default, and uses
#' them as given. Each share does not depend on the scale of its measure,
#' so the normalisation of the betweenness does not matter. Closeness has
#' no value on a disconnected or a one-node network; the measure then warns
#' (class \code{cograph_undefined_measure}) and returns \code{NA}. With
#' \code{ahp_weights = c(1, 0, 0)} and \code{c(0, 0, 1)} the implementation
#' reproduces the 20 shares the source prints for the degree and the
#' closeness of the kite network, and with the source's weights its ranking
#' of Table 3. The betweenness matrix the source prints is not that of the
#' kite (two nodes that are symmetric in the kite differ in it), so its
#' betweenness shares and final scores are not reproduced; the final scores
#' differ by at most 0.013. The score ranks nodes much like
#' \code{\link{centrality_topsis}} (Kendall correlation 0.96 to 0.995 on
#' four test networks, mean 0.975) and
#' \code{\link{centrality_relative_entropy}} (mean 0.94, 0.992 on one
#' network). A weight that is negative or not finite, or a
#' \code{ahp_weights} that is not three numbers, raises a
#' \code{cograph_bad_parameter} error. \code{normalized = TRUE} divides the
#' scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ahp_weights} (the weights of degree, betweenness and
#' closeness, default one third each).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Bian, T., Hu, J. and Deng, Y. (2017). Identifying influential nodes in
#' complex networks based on AHP. Physica A: Statistical Mechanics and its
#' Applications, 479, 422-436. \doi{10.1016/j.physa.2017.02.085}.
#' @seealso \code{\link{centrality_topsis}},
#' \code{\link{centrality_degree}}, \code{\link{centrality_betweenness}},
#' \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ahp(to_undirected(regulation_net))
#' centrality_ahp(to_undirected(regulation_net), ahp_weights = c(0.5, 0.25, 0.25))
centrality_ahp <- function(x, ...) {
  df <- centrality(x, measures = "ahp", ...)
  stats::setNames(df$ahp, df$node)
}

#' Semi-Local Iterative Algorithm
#'
#' The semi-local iterative algorithm (semi-IA; Luan et al. 2021) repeats
#' an update in the manner of eigenvector centrality and HITS, in which a
#' node collects the normalised scores of the nodes within \eqn{r} steps,
#' weighted by the number of shortest paths to them and divided by the
#' distance:
#' \deqn{X_i(t + 1) = \sum_{j \ne i:\, d_{ij} \le r} \frac{n_{ij}^{\gamma}}{d_{ij}} \frac{X_j(t)}{\|X(t)\|}, \qquad X(0) = 1,}{X_i(t + 1) = sum_{j != i: d_ij <= r} (n_ij^gamma / d_ij) X_j(t) / ||X(t)||,  X(0) = 1,}
#' with \eqn{d_{ij}}{d_ij} the distance and \eqn{n_{ij}}{n_ij} the number
#' of shortest paths. The score is \eqn{X} at the steady state.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The source prints its equation (8) with the node's own normalised score
#' \eqn{X'_i} inside the sum. Read so, the update ranks the nodes by the
#' row sums of the weight matrix and its normalised vector collapses onto
#' one node, which the source's Fig. 7 contradicts. The neighbor's score
#' \eqn{X'_j} is used, the reading of the eigenvector and HITS updates the
#' source says it follows. The norm is Euclidean, as in the source's HITS
#' normalisation. The steady state is the leading eigenvalue times the unit
#' leading eigenvector of the weight matrix. \code{semi_ia_radius = NULL},
#' the default, takes \eqn{r = 3} below 1000 nodes and \eqn{r = 5} from
#' 1000 nodes on, the rule of the source; \code{Inf} admits the whole
#' network. \code{semi_ia_gamma} is 0.2, the value the source chooses, and
#' must lie in \eqn{[0, 1]}. The source stops when two successive
#' normalised vectors differ by less than \eqn{10^{-5}}{1e-5}; here the
#' largest difference must fall below \eqn{10^{-12}}{1e-12}, within at most
#' \code{semi_ia_max_iter} updates (default 10000). Past that bound the
#' last iterate is returned with a \code{cograph_no_converge} warning; with
#' \code{semi_ia_radius = 1} this happens on a bipartite network, which has
#' no steady state. The source treats connected networks; on a disconnected
#' network, or a one-node network, the measure warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA}. The source
#' prints no score. It prints the monotonicity of the ranking on the
#' adjective-noun network (0.9997, Table 2), which the implementation
#' reproduces; the value does not separate the two readings of equation
#' (8), so the scores are otherwise checked against the definition only. On
#' four test networks the score ranks nodes closest to \code{icentr} (mean
#' Kendall correlation 0.90); with \code{\link{centrality_eigenvector}} the
#' correlation is 0.69 to 0.84. \code{normalized = TRUE} divides the scores
#' by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{semi_ia_radius} (default \code{NULL}, 3 or 5 by the size of
#' the network), \code{semi_ia_gamma} (default 0.2) and
#' \code{semi_ia_max_iter} (default 10000).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Luan, Y., Bao, Z. and Zhang, H. (2021). Identifying influential
#' spreaders in complex networks by considering the impact of the number of
#' shortest paths. Journal of Systems Science and Complexity, 34(6),
#' 2168-2181. \doi{10.1007/s11424-021-0111-7}.
#' @seealso \code{\link{centrality_improved_closeness}},
#' \code{\link{centrality_eigenvector}}, \code{\link{centrality_hub}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_semi_ia(to_undirected(regulation_net))
#' centrality_semi_ia(to_undirected(regulation_net), semi_ia_gamma = 0.5)
centrality_semi_ia <- function(x, ...) {
  df <- centrality(x, measures = "semi_ia", ...)
  stats::setNames(df$semi_ia, df$node)
}

#' Synthesize Centrality
#'
#' Synthesize centrality (SC; Liu et al. 2013) adds the normalised degree
#' and betweenness of a node and divides by its normalised farness:
#' \deqn{SC(v) = \frac{d_d(v) + d_b(v)}{d_c(v)}, \qquad d_d(v) = \frac{k_v}{n - 1}, \qquad d_b(v) = \frac{2 B(v)}{(n - 1)^2}, \qquad d_c(v) = \frac{\sum_i l(i, v)}{(n - 1)^2},}{SC(v) = (d_d(v) + d_b(v)) / d_c(v),  d_d(v) = k_v / (n - 1),  d_b(v) = 2 B(v) / (n - 1)^2,  d_c(v) = sum_i l(i, v) / (n - 1)^2,}
#' with \eqn{k} the degree, \eqn{B} the betweenness and \eqn{l} the
#' distance, 1000 for a pair that cannot reach each other.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' \eqn{B} is the betweenness \code{\link{centrality_betweenness}} gives on
#' an unweighted network, the share of shortest paths through \eqn{v}
#' summed over unordered pairs, so \eqn{2B} sums over ordered pairs as the
#' prose of the source's equation (7) does; its case formula says that the
#' path does not contain \eqn{v}, which contradicts that prose. The primes
#' of the source's equation (9), the "normalized" quantities, are taken as
#' equations (6)-(8) themselves, with no second rescaling. The Centrality
#' Zoo squares two terms and divides by the closeness; the source does
#' neither. The distance 1000 for an unreachable pair is the source's, so
#' disconnected networks have a value; an isolated node scores 0. On a
#' one-node network the measure warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA}. The source
#' prints no value that can be checked, so the implementation is checked
#' against the definition only. The score ranks nodes much like
#' \code{\link{centrality_topsis}} (Kendall correlation 0.88 to 0.97 on
#' four test networks, mean 0.94) and \code{\link{centrality_ahp}} (0.98 on
#' the karate club network). \code{normalized = TRUE} divides the scores by
#' their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Liu, H., Yu, X. and Lu, J. (2013). Identifying top-n opinion leaders on
#' local social network. IET International Conference on Smart and
#' Sustainable City 2013 (ICSSC 2013), 268-271. \doi{10.1049/cp.2013.1970}.
#' @seealso \code{\link{centrality_topsis}},
#' \code{\link{centrality_betweenness}},
#' \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_synthesize(regulation_net)
centrality_synthesize <- function(x, ...) {
  df <- centrality(x, measures = "synthesize", ...)
  stats::setNames(df$synthesize, df$node)
}

#' Inward Accessibility
#'
#' Inward accessibility (\enc{Travençolo}{Travencolo} and Costa 2008)
#' measures how evenly a node is reached by self-avoiding random walks of
#' \eqn{h} steps started at the other nodes:
#' \deqn{IA_h(i) = \frac{1}{N - 1} \exp\Bigl(-\sum_{j \ne i} q_{ji} \log q_{ji}\Bigr), \qquad q_{ji} = \frac{P_h(j \to i)}{N - 1},}{IA_h(i) = exp(-sum_{j != i} q_ji log q_ji) / (N - 1),  q_ji = P_h(j -> i) / (N - 1),}
#' with \eqn{P_h(j \to i)}{P_h(j -> i)} the probability that a walk started
#' at \eqn{j} is at \eqn{i} after \eqn{h} steps and \eqn{N} the number of
#' nodes.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored. A
#' self-avoiding walk steps to a neighbor it has not visited, each equally
#' likely, and stops when there is none. \code{ia_steps}, the number of
#' steps \eqn{h}, has no default, because the source gives none: without it
#' the measure warns (class \code{cograph_undefined_measure}) and returns
#' \code{NA}. The source leaves the treatment of a stopped walk to the
#' application. \code{ia_stuck = "vanish"}, the default, removes it from
#' the analysis, as the source's own application does; \code{"stay"} keeps
#' it at its last node. The probabilities are computed exactly by
#' enumerating every walk, the limit of the sampled walks of the source.
#' Their number grows exponentially with \eqn{h}; past
#' \code{enumeration_limit} walk steps the measure warns and returns
#' \code{NA}. The logarithm is natural. A node that no walk reaches scores
#' \eqn{1/(N - 1)}, the floor of the measure, and 1 is the largest score.
#' On a one-node network the measure warns and returns \code{NA}. The
#' source defines the measure but computes no value of it; its Fig. 2
#' prints 18 diversity entropies of the same walks, which the walk
#' enumeration reproduces. With \code{ia_steps = 2} the score ranks nodes
#' closest to \code{nwrank} (mean Kendall correlation 0.89 on four test
#' networks) and \code{density_centrality} (0.86). A \code{ia_steps} that
#' is not a whole number of at least one, or a \code{ia_stuck} other than
#' \code{"vanish"} and \code{"stay"}, raises a \code{cograph_bad_parameter}
#' error. \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ia_steps} (the number of steps, required), \code{ia_stuck}
#' (\code{\"vanish\"} or \code{\"stay\"}) and \code{enumeration_limit}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' \enc{Travençolo}{Travencolo}, B. A. N. and da F. Costa, L. (2008).
#' Accessibility in complex networks. Physics Letters A, 373(1), 89-95.
#' \doi{10.1016/j.physleta.2008.10.069}.
#' @seealso \code{\link{centrality_access_information}},
#' \code{\link{centrality_random_walk}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_inward_accessibility(regulation_net, ia_steps = 2)
#' centrality_inward_accessibility(regulation_net, ia_steps = 2, ia_stuck = "stay")
centrality_inward_accessibility <- function(x, ...) {
  df <- centrality(x, measures = "inward_accessibility", ...)
  stats::setNames(df$inward_accessibility, df$node)
}

#' Improved Entropy-Based Centrality
#'
#' The improved entropy-based centrality of Ni et al. (2020) adds a node's
#' direct influence, from the spread of its tie weights and of its
#' neighbors' degrees, and its indirect influence on the nodes two steps
#' away:
#' \deqn{I_i = \tfrac{1}{2} DI_i + \tfrac{1}{2} II_i, \qquad DI_i = \tfrac{1}{2} H(p_{i\cdot}) + \tfrac{1}{2} H(T_{i\cdot}), \qquad T_{ij} = \frac{k_j^{\beta}}{\sum_{l \in N(i)} k_l^{\beta}},}{I_i = DI_i / 2 + II_i / 2,  DI_i = H(p_i.) / 2 + H(T_i.) / 2,  T_ij = k_j^beta / sum_{l in N(i)} k_l^beta,}
#' with \eqn{H} the entropy to base 10 over the neighbors,
#' \eqn{p_{ij}}{p_ij} the share of the weight of \eqn{i} on its tie to
#' \eqn{j} and \eqn{k} the degree. \eqn{II_i} averages, over the nodes two
#' steps away, \eqn{DI_i} times the mean \eqn{DI} of the neighbors they
#' share with \eqn{i}.
#'
#' @details
#' The measure uses the edge weights. With \code{weighted = FALSE}, or on
#' an unweighted network, \eqn{H(p_{i\cdot}) = \log_{10} k_i}{H(p_i.) =
#' log10(k_i)}. The source defines it for undirected networks; when the
#' weighted adjacency matrix is not symmetric the measure warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA}. Loops are
#' ignored. \code{ie_beta = 2} and the mixing weights of 1/2 are the
#' source's basic values (its section 5); the mixing weights are fixed. A
#' node two steps away shares a neighbor with \eqn{i} and is not adjacent
#' to it. A node with no such node has indirect influence 0, where the
#' source leaves 0/0; an isolated node scores 0. The source prints no value
#' of the measure, so the implementation is checked against the definition
#' only. This is the measure the Centrality Zoo describes as the improved
#' entropy-based centrality; the Zoo's entry also cites Peng et al. (2017),
#' whose different measure is \code{\link{centrality_entropy_influence}}.
#' On four test networks the score ranks nodes closest to \code{ked} and
#' \code{lnc} (mean Kendall correlation 0.93); with the measure of Peng et
#' al. it has 0.85 to 0.93. A \code{ie_beta} that is not a single finite
#' number raises a \code{cograph_bad_parameter} error, and negative weights
#' a \code{cograph_negative_weights} error. \code{normalized = TRUE}
#' divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{ie_beta} (the exponent of the neighbor degree, default 2).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ni, C., Yang, J. and Kong, D. (2020). Sequential seeding strategy for
#' social influence diffusion with improved entropy-based centrality.
#' Physica A: Statistical Mechanics and its Applications, 545, 123659.
#' \doi{10.1016/j.physa.2019.123659}.
#' @seealso \code{\link{centrality_entropy_influence}},
#' \code{\link{centrality_entropy}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_improved_entropy(to_undirected(regulation_net))
#' centrality_improved_entropy(to_undirected(regulation_net), ie_beta = 0)
centrality_improved_entropy <- function(x, ...) {
  df <- centrality(x, measures = "improved_entropy", ...)
  stats::setNames(df$improved_entropy, df$node)
}
