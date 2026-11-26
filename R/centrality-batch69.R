#' RCFB; `NA` with a warning on a directed or asymmetric input
#' @keywords internal
#' @noRd
calculate_rcfb <- function(cg, weights = NULL, epsilon = 0) {
  w <- .cg_candidate_adjacency(cg, weights, "rcfb")
  diag(w) <- 0
  n <- nrow(w)
  if (n == 0L) return(numeric(0))
  if (!all(w == t(w))) {
    .cg_warn_undefined("`rcfb` has no value on this input, so its column is ",
                       "NA: the source defines it for undirected networks, ",
                       "and this one has an arc without its reverse or with ",
                       "a different weight.")
    return(rep(NA_real_, n))
  }
  .cg_check_rcfb_epsilon(epsilon, w)
  .cg_rcfb(w, epsilon)
}

#' @keywords internal
#' @noRd
calculate_m_centrality <- function(cg) .cg_m_centrality(.cg_skeleton(cg))

#' Wksd; `NA` with a warning without both weights
#' @keywords internal
#' @noRd
calculate_wksd <- function(cg, alpha = NULL, mu = NULL) {
  .cg_check_wksd_args(alpha, mu)
  b <- .cg_skeleton(cg)
  if (is.null(alpha) || is.null(mu)) {
    .cg_warn_undefined("`wksd` has no value on this input, so its column is ",
                       "NA: it needs both weights `wksd_alpha` and ",
                       "`wksd_mu`, which have no default because the source ",
                       "tunes them per network.")
    return(rep(NA_real_, nrow(b)))
  }
  .cg_wksd(b, alpha, mu)
}

#' @keywords internal
#' @noRd
calculate_mapping_entropy_betweenness <- function(cg) {
  .cg_mapping_entropy_betweenness(.cg_skeleton(cg))
}

#' Resized Current-Flow Betweenness
#'
#' RCFB (Agryzkov et al. 2019) joins every node to a virtual target by a
#' link of conductance \eqn{\varepsilon}{epsilon}, lets one unit of current
#' enter at each node \eqn{s} in turn, and credits the current of every
#' edge to its end of higher potential:
#' \deqn{b_i = \frac{1}{n} \sum_{s} \sum_{j:\, u^{(s)}_i > u^{(s)}_j} q_{ij} \bigl(u^{(s)}_i - u^{(s)}_j\bigr), \qquad u^{(s)} = (\varepsilon I + L)^{-1} e_s,}{b_i = (1 / n) sum_s sum_{j: u_i(s) > u_j(s)} q_ij (u_i(s) - u_j(s)),  u(s) = (epsilon I + L)^-1 e_s,}
#' with \eqn{q_{ij}}{q_ij} the conductance (weight) of the edge \eqn{ij},
#' \eqn{L} the weighted Laplacian and \eqn{n} the number of nodes.
#'
#' @details
#' The source defines the measure on undirected networks with edge weights
#' as conductances; a weight is read as a conductance (for a resistance,
#' give its reciprocal) and loops are ignored. A network with an arc whose
#' reverse is missing or has another weight has no value: the measure warns
#' (class \code{cograph_undefined_measure}) and returns \code{NA}. The
#' source bounds \eqn{\varepsilon}{epsilon} above by one over the sum of
#' the edge resistances (its equation 14) and gives no value.
#' \code{rcfb_epsilon = 0}, the default, takes the limit \eqn{\varepsilon
#' \to 0}{epsilon -> 0} (owner decision 2026-11-26), in which the potential
#' differences are those of the Laplacian pseudo-inverse; on the Krackhardt
#' kite \eqn{\varepsilon = 10^{-6}}{epsilon = 1e-6} is within
#' \eqn{10^{-6}}{1e-6} of it. A positive \code{rcfb_epsilon} at or above
#' the bound raises a \code{cograph_bad_parameter} error. The network need
#' not be connected: the current of a source stays in its component, and an
#' isolated node scores 0.
#'
#' The loop of the source's Algorithm 2 credits every edge, also those at
#' the source, so a source is credited with the current it sends out; that
#' is the second option of its equation (19), \eqn{I_s = x_s - x_s/n}{I_s =
#' x_s - x_s / n}, and it is what is computed (a builder choice). The
#' caption of Algorithm 2 names the first option, \eqn{I_s = 0}{I_s = 0};
#' in the limit that reading subtracts \eqn{(1 - 1/n_c)/n}{(1 - 1/n_c) / n}
#' from every score, \eqn{n_c} the size of the node's component, so it
#' ranks the nodes of one component in the same order. Algorithm 2 also
#' omits the conductance of equation (18), which matters only on a weighted
#' network; the current includes it here. The source prints no score. The
#' implementation reproduces the values a source audit computed on the
#' Krackhardt kite in the limit (10 of 10) and its distances to the limit
#' at \eqn{\varepsilon = 10^{-4}}{epsilon = 1e-4} and \eqn{10^{-6}}{1e-6};
#' the audit's ten values at \eqn{\varepsilon = 1}{epsilon = 1}, which is
#' above the bound, are reproduced by the internal computation, which this
#' function does not let reach that value. On four test networks the score
#' ranks nodes closest to \code{\link{centrality_current_flow_betweenness}}
#' (mean Kendall correlation 0.958, 0.931 at the least) and
#' \code{\link{centrality_godfather}} (0.87); with
#' \code{\link{centrality_betweenness}} the correlation is 0.83 to 0.87.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{rcfb_epsilon} (default 0, the limit) and the edge weights
#' (\code{weighted}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Agryzkov, T., Tortosa, L. and Vicent, J. F. (2019). A variant of the
#' current flow betweenness centrality and its application in urban
#' networks. Applied Mathematics and Computation, 347, 600-615.
#' \doi{10.1016/j.amc.2018.11.032}.
#' @seealso \code{\link{centrality_current_flow_betweenness}},
#' \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_rcfb(to_undirected(regulation_net))
#' centrality_rcfb(to_undirected(regulation_net), weighted = FALSE, rcfb_epsilon = 1e-4)
centrality_rcfb <- function(x, ...) {
  df <- centrality(x, measures = "rcfb", ...)
  stats::setNames(df$rcfb, df$node)
}

#' M-Centrality
#'
#' M-centrality (Ibnoulouafi et al. 2018) mixes the coreness of a node with
#' a local variation of degree among its neighbors, by weights taken from
#' the entropy of the two terms over the network:
#' \deqn{M_i = \mu K_s(i) + (1 - \mu) \Delta D_i, \qquad \Delta D_i = k_i \sum_{j \in N(i)} \frac{|k_j - k_i|}{\sum_{l \in N(i)} k_l}, \qquad \mu = \frac{1 - E_1}{2 - E_1 - E_2},}{M_i = mu Ks(i) + (1 - mu) DeltaD_i,  DeltaD_i = k_i sum_{j in N(i)} |k_j - k_i| / sum_{l in N(i)} k_l,  mu = (1 - E_1) / (2 - E_1 - E_2),}
#' with \eqn{k} the degree, \eqn{K_s}{Ks} the coreness and \eqn{E_1},
#' \eqn{E_2} the entropies of the shares \eqn{K_s(j)/\sum K_s}{Ks(j) / sum
#' Ks} and \eqn{\Delta D_j / \sum \Delta D}{DeltaD_j / sum DeltaD}, divided
#' by \eqn{\ln n}{ln n}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The absolute value covers the whole fraction of the source's equation
#' (7); the Centrality Zoo prints the term without it, which makes it
#' negative for hubs. When the weights of equation (11) have no value the
#' two terms are weighted 1/2 each (owner decision 2026-11-26): on a
#' regular network, and on any network where every node's neighbors share
#' its degree, \eqn{\Delta D}{DeltaD} is 0 at every node and its shares are
#' 0/0; the same applies when both entropies are 1, on a network without
#' edges and on a one-node network. With equal weights such a network
#' scores half its coreness. An isolated node has an empty neighbor sum,
#' \eqn{\Delta D = 0}{DeltaD = 0}, and coreness 0. The source prints no
#' score; the implementation reproduces the values and the weight \eqn{\mu
#' = 0.37263}{mu = 0.37263} that two source audits computed independently
#' on the Krackhardt kite (10 of 10). On four test networks no existing
#' measure has a mean Kendall correlation above 0.56 with the score
#' (\code{\link{centrality_nwrank}} 0.555); with the degree the correlation
#' is 0.00 to 0.84 and with the coreness -0.09 to 0.72. \code{normalized =
#' TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ibnoulouafi, A., El Haziti, M. and Cherifi, H. (2018). M-centrality:
#' identifying key nodes based on global position and local degree
#' variation. Journal of Statistical Mechanics: Theory and Experiment,
#' 2018(7), 073407. \doi{10.1088/1742-5468/aace08}.
#' @seealso \code{\link{centrality_coreness}},
#' \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_m_centrality(regulation_net)
centrality_m_centrality <- function(x, ...) {
  df <- centrality(x, measures = "m_centrality", ...)
  stats::setNames(df$m_centrality, df$node)
}

#' Weighted K-Shell Degree Neighborhood
#'
#' The weighted k-shell degree neighborhood (Wksd; Namtirtha et al. 2018,
#' 2020) weights each edge by the product of a mix of degree and k-shell
#' index at its two ends, and scores a node by the sum of the weights of
#' its edges:
#' \deqn{Wksd(i) = \sum_{j \in N(i)} (\alpha k_i + \mu k_s(i)) (\alpha k_j + \mu k_s(j)),}{Wksd(i) = sum_{j in N(i)} (alpha k_i + mu ks(i)) (alpha k_j + mu ks(j)),}
#' with \eqn{k} the degree and \eqn{k_s}{ks} the k-shell index.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The two original papers were not read (they are not open access); the
#' formula is the one three later papers state alike, two of them read from
#' the page (Shetty and Bhattacharjee 2025, equations 3-4; Lu and Dong
#' 2020, equations 2.5-2.6). They stop at the single sum above; a second
#' neighborhood sum in the originals cannot be ruled out. \code{wksd_alpha}
#' and \code{wksd_mu} are \eqn{\alpha}{alpha} and \eqn{\mu}{mu}. The source
#' tunes them per network, and the papers that restate it use different
#' pairs (Lu and Dong 0.9 and 0.2 on five of eight networks, 0.2 and 0.9 on
#' one), so they have no default: without both the measure warns (class
#' \code{cograph_undefined_measure}) and returns \code{NA} (owner decision
#' 2026-11-26). Each must lie in \eqn{[0, 1]}, the range those papers give,
#' or a \code{cograph_bad_parameter} error is raised. With \eqn{\mu = 0}{mu
#' = 0} the score is \eqn{\alpha^2 k_i \sum_{j \in N(i)} k_j}{alpha^2 k_i
#' sum_{j in N(i)} k_j}. An isolated node scores 0.
#' \code{\link{centrality_wksdn}} is Maji et al.'s additive variant of the
#' same idea, without parameters. No paper read prints a score; the
#' implementation reproduces the 35 values a source audit computed on two
#' small networks at three settings. On four test networks, with
#' \code{wksd_alpha = 0.2} and \code{wksd_mu = 0.9} the score ranks nodes
#' closest to \code{\link{centrality_wksdn}} (mean Kendall correlation
#' 0.972) and \code{\link{centrality_lnc}} (0.970), with correlations of
#' 0.99 or more on single networks: 0.998 with \code{wksdn} and with
#' \code{lnc}, 0.993 with \code{\link{centrality_gls}}, 0.990 with
#' \code{\link{centrality_laplacian}} and with
#' \code{\link{centrality_modified_expected_force}}. With \code{wksd_alpha
#' = 0.9} and \code{wksd_mu = 0.2} it ranks them closest to
#' \code{laplacian} (0.974), \code{\link{centrality_ked}} (0.968) and
#' \code{wksdn} (0.964), with 0.999 with \code{\link{centrality_ksgc}} on a
#' network with a single k-shell and 0.998 with \code{laplacian}, 0.996
#' with \code{lnc} and 0.992 with \code{ked} and \code{wksdn} on another.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#' uses \code{wksd_alpha} and \code{wksd_mu} (both required).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Namtirtha, A., Dutta, A. and Dutta, B. (2018). Weighted kshell degree
#' neighborhood method: an approach independent of completeness of global
#' network structure for identifying the influential spreaders. In 2018
#' 10th International Conference on Communication Systems and Networks
#' (COMSNETS), 81-88. \doi{10.1109/COMSNETS.2018.8328183}.
#'
#' Namtirtha, A., Dutta, A. and Dutta, B. (2020). Weighted kshell degree
#' neighborhood: a new method for identifying the influential spreaders
#' from a variety of complex network connectivity structures. Expert
#' Systems with Applications, 139, 112859.
#' \doi{10.1016/j.eswa.2019.112859}.
#'
#' Shetty, R. D. and Bhattacharjee, S. (2025). Pertinence of contact
#' duration as edge feature for epidemic spread analysis. Scientific
#' Reports, 15, 10703. \doi{10.1038/s41598-025-94637-3}.
#'
#' Lu, P. and Dong, C. (2020). EMH: extended mixing H-index centrality for
#' identification important users in social networks based on neighborhood
#' diversity. arXiv:2003.03585.
#' @seealso \code{\link{centrality_wksdn}},
#' \code{\link{centrality_coreness}}, \code{\link{centrality_degree}},
#' \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_wksd(regulation_net, wksd_alpha = 0.2, wksd_mu = 0.9)
#' centrality_wksd(regulation_net, wksd_alpha = 0.9, wksd_mu = 0.2)
centrality_wksd <- function(x, ...) {
  df <- centrality(x, measures = "wksd", ...)
  stats::setNames(df$wksd, df$node)
}

#' Mapping Entropy Betweenness
#'
#' Mapping entropy betweenness (MEB; Gialampoukidis et al. 2016) is mapping
#' entropy with the normalised betweenness in place of the degree
#' centrality:
#' \deqn{MEB_i = -BC_i \sum_{j \in N(i),\, BC_j > 0} \log BC_j, \qquad BC_i = \frac{2\, b_i}{(N - 1)(N - 2)},}{MEB_i = -BC_i sum_{j in N(i), BC_j > 0} log BC_j,  BC_i = 2 b_i / ((N - 1)(N - 2)),}
#' with \eqn{b_i} the betweenness of \eqn{i} (the shortest-path share over
#' unordered pairs of other nodes) and \eqn{N} the number of nodes.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored. A
#' neighbor of betweenness 0 has \eqn{\log 0}{log 0}; the source does not
#' say what to do, and such neighbors are left out of the sum (owner
#' decision 2026-11-26). The normalisation is by the number of pairs of
#' other nodes, as the Centrality Zoo transcribes the source, so \eqn{BC
#' \le 1}{BC <= 1} and the score is never negative. The logarithm is
#' natural, as in \code{\link{centrality_mapping_entropy}}; another base
#' rescales every score by one constant. A node of betweenness 0 scores 0,
#' so does every node of a network with fewer than three nodes, and a
#' neighbor of betweenness 1 (the centre of a star) adds 0. The source
#' paper was read for an earlier source audit of this package but was not
#' available for this implementation; the formula is the one that audit
#' recorded and the one the Centrality Zoo prints. No score is printed, so
#' the implementation is checked against the definition only. On four test
#' networks the score ranks nodes closest to
#' \code{\link{centrality_delta_betweenness}} (mean Kendall correlation
#' 0.956), \code{\link{centrality_relative_entropy}} (0.949) and
#' \code{\link{centrality_proximal_betweenness}} (0.948); with
#' \code{\link{centrality_betweenness}} the correlation is 0.92 to 0.96 and
#' with \code{\link{centrality_mapping_entropy}} 0.71 to 0.87.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#' \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Gialampoukidis, I., Kalpakis, G., Tsikrika, T., Vrochidis, S. and
#' Kompatsiaris, I. (2016). Key player identification in terrorism-related
#' social media networks using centrality measures. In 2016 European
#' Intelligence and Security Informatics Conference (EISIC), 112-115.
#' \doi{10.1109/EISIC.2016.029}.
#' @seealso \code{\link{centrality_mapping_entropy}},
#' \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mapping_entropy_betweenness(regulation_net)
centrality_mapping_entropy_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "mapping_entropy_betweenness", ...)
  stats::setNames(df$mapping_entropy_betweenness, df$node)
}
