# ===========================================================================
# Batch 7 — Centrality Zoo comparison batch
#
# igraph-facing calculators (thin glue over the base-R kernels in
# R/kernels-batch7.R) and the exported one-measure verbs.
# ===========================================================================

#' Distance entropy calculator
#' @keywords internal
#' @noRd
calculate_distance_entropy <- function(cg, mode = "all", hop_mat = NULL) {
  if (cg$n == 0L) return(numeric(0))
  hop_mat <- hop_mat %||% .cg_hop_distances(cg, mode)
  .cg_distance_entropy(hop_mat)
}

#' Local dimension calculator
#' @keywords internal
#' @noRd
calculate_local_dimension <- function(cg, mode = "all", hop_mat = NULL) {
  if (cg$n == 0L) return(numeric(0))
  hop_mat <- hop_mat %||% .cg_hop_distances(cg, mode)
  .cg_local_dimension(hop_mat)
}

#' Local information dimensionality calculator
#' @keywords internal
#' @noRd
calculate_local_information_dimension <- function(cg, mode = "all",
                                                  hop_mat = NULL) {
  if (cg$n == 0L) return(numeric(0))
  hop_mat <- hop_mat %||% .cg_hop_distances(cg, mode)
  .cg_local_information_dimension(hop_mat, n_total = cg$n)
}

#' Modularity vitality calculator
#'
#' Follows the community-measure convention: a missing partition warns and
#' returns `NA`; a partition of the wrong length is a contract violation.
#' @keywords internal
#' @noRd
calculate_modularity_vitality <- function(cg, weights = NULL,
                                          membership = NULL) {
  n <- cg$n
  if (n == 0L) return(numeric(0))
  if (is.null(membership)) {
    .cg_warn_no_membership("modularity_vitality")
    return(rep(NA_real_, n))
  }
  if (length(membership) != n || anyNA(membership)) {
    msg <- sprintf("`membership` needs one non-missing label per node (%d), %s",
                   n, sprintf("got length %d", length(membership)))
    stop(errorCondition(msg, class = "cograph_bad_membership", call = NULL))
  }
  .cg_modularity_vitality(.cg_path_matrix(cg, weights), membership)
}

#' Neighborhood connectivity calculator
#' @keywords internal
#' @noRd
calculate_neighborhood_connectivity <- function(cg, mode = "all") {
  if (cg$n == 0L) return(numeric(0))
  .cg_neighborhood_connectivity(.cg_path_matrix(cg, NULL), mode)
}

# ---------------------------------------------------------------------------
# Exported one-measure verbs
# ---------------------------------------------------------------------------

#' Distance Entropy
#'
#' Distance entropy (Stella and De Domenico 2018) is the Shannon entropy of
#' the distribution of hop distances from a node to the nodes it reaches,
#' scaled by the logarithm of the number of distance values in its range.
#' With \eqn{p_k}{p_k} the share of reachable nodes at distance \eqn{k}, and
#' \eqn{m_i}{m_i} and \eqn{M_i}{M_i} the smallest and largest distance,
#' \deqn{h_i = -\frac{1}{\log(M_i - m_i + 1)}
#'   \sum_{k = m_i}^{M_i} p_k \log p_k.}{
#'   h_i = -1 / log(M_i - m_i + 1) sum_{k = m_i}^{M_i} p_k log p_k.}
#'
#' @details
#' Distances are hop counts, so edge weights are ignored. On a directed
#' network \code{mode} sets the direction of the paths. Scores lie between 0
#' and 1. A node whose reachable nodes all lie at one distance scores 0, and
#' a node that reaches no other node returns \code{NaN}. The source divides
#' by \eqn{\log(M_i - m_i)}{log(M_i - m_i)}, which is zero when the range
#' holds two distances. The implementation divides by
#' \eqn{\log(M_i - m_i + 1)}{log(M_i - m_i + 1)}, so a uniform distribution
#' scores 1.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Stella, M., & De Domenico, M. (2018). Distance entropy
#'   cartography characterises centrality in complex networks. Entropy,
#'   20(4), 268.
#' @seealso \code{\link{centrality_local_dimension}},
#'   \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_distance_entropy(regulation_net)
centrality_distance_entropy <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "distance_entropy", mode = mode, ...)
  stats::setNames(df[[paste0("distance_entropy_", mode)]], df$node)
}

#' Local Dimension
#'
#' The local dimension (Silva and Costa 2013; Pu et al. 2014) is the growth
#' exponent of the ball around a node. With \eqn{B_i(r)}{B_i(r)} the number
#' of nodes within \eqn{r} hops of \eqn{i}, the node itself included, it is
#' the least-squares slope of \eqn{\ln B_i(r)}{ln B_i(r)} on
#' \eqn{\ln r}{ln r} over
#' \eqn{r = 1, \ldots, d_{\max}(i)}{r = 1, ..., d_max(i)}:
#' \deqn{D_i = \frac{d \ln B_i(r)}{d \ln r}.}{D_i = d ln B_i(r) / d ln r.}
#'
#' @details
#' Distances are hop counts, so edge weights are ignored. On a directed
#' network \code{mode} sets the direction of the paths. A node that reaches
#' most of the network in a few hops has a small exponent, so lower values
#' mark more influential nodes. A node with a single radius returns the
#' discretized derivative
#' \eqn{r\, n_i(r) / B_i(r)}{r n_i(r) / B_i(r)} at \eqn{r = 1}, where
#' \eqn{n_i(r)}{n_i(r)} counts the nodes at distance exactly \eqn{r}. A node
#' that reaches no other node returns \code{NaN}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Silva, F. N., & Costa, L. da F. (2013). Local dimension of complex
#'   networks. arXiv:1209.2476.
#'
#' Pu, J., Chen, X., Wei, D., Liu, Q., & Deng, Y. (2014). Identifying
#'   influential nodes based on local dimension. EPL, 107(1), 10010.
#'
#' Wen, T., & Jiang, W. (2019). Identifying influential nodes based on fuzzy
#'   local dimension in complex networks. Chaos, Solitons & Fractals, 119,
#'   332-342.
#' @seealso \code{\link{centrality_local_information_dimension}},
#'   \code{\link{centrality_local_dimension_fixed}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_local_dimension(regulation_net)
centrality_local_dimension <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "local_dimension", mode = mode, ...)
  stats::setNames(df[[paste0("local_dimension_", mode)]], df$node)
}

#' Local Information Dimensionality
#'
#' Local information dimensionality (Wen and Deng 2020) weights the local
#' dimension by information. With \eqn{p_i(l) = B_i(l) / N}{p_i(l) = B_i(l) / N}
#' the share of the network within \eqn{l} hops of \eqn{i}, the node
#' included, and box information
#' \eqn{I_i(l) = -p_i(l) \ln p_i(l)}{I_i(l) = -p_i(l) ln p_i(l)}, the measure
#' is minus the least-squares slope of \eqn{I_i(l)}{I_i(l)} on
#' \eqn{\ln l}{ln l} for
#' \eqn{l = 1, \ldots, \lceil d_{\max}(i) / 2 \rceil}{
#'   l = 1, ..., ceiling(d_max(i) / 2)}:
#' \deqn{D^I_i = -\frac{d I_i(l)}{d \ln l}.}{DI_i = -d I_i(l) / d ln l.}
#'
#' @details
#' Distances are hop counts, so edge weights are ignored. On a directed
#' network \code{mode} sets the direction of the paths. Higher values mark
#' more influential nodes. A node with a single box size returns the
#' discretized derivative of the source,
#' \eqn{l (1 + \ln p_i(l))\, n_i(l) / N}{l (1 + ln p_i(l)) n_i(l) / N}. A
#' node that reaches no other node returns \code{NaN}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Wen, T., & Deng, Y. (2020). Identification of influencers in
#'   complex networks by local information dimensionality. Information
#'   Sciences, 512, 549-562.
#' @seealso \code{\link{centrality_local_dimension}},
#'   \code{\link{centrality_local_dimension_fixed}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_local_information_dimension(regulation_net)
centrality_local_information_dimension <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "local_information_dimension", mode = mode,
                   ...)
  stats::setNames(df[[paste0("local_information_dimension_", mode)]],
                  df$node)
}

#' Modularity Vitality
#'
#' Modularity vitality (Magelinski, Bartulovic and Carley 2021) is the drop
#' in Newman modularity of a fixed partition \eqn{C} when a node is deleted
#' and the remaining nodes keep their communities:
#' \deqn{V_Q(i) = Q(G, C) - Q(G - i, C \setminus \{i\}).}{
#'   V_Q(i) = Q(G, C) - Q(G - i, C without i).}
#' Positive values mark community hubs and negative values mark bridges
#' between communities.
#'
#' @details
#' Edge weights are used. A directed network uses the Leicht-Newman
#' directed modularity, and the values equal those obtained by deleting
#' each node and recomputing \code{igraph::modularity()}. Self-loops enter
#' the modularity, and \code{loops = FALSE} drops them. A node whose deletion
#' leaves a graph with no edges returns \code{NaN}. Without
#' \code{membership} the function raises a warning of classes \code{cograph_bad_membership} and
#' \code{cograph_undefined_measure} and returns
#' \code{NA} for every node. A \code{membership} that is not one
#' non-missing label per node raises an error of class
#' \code{cograph_bad_membership}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Community labels, one per node (integer, factor or
#'   character), for example from \code{\link{detect_communities}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#'   uses \code{weighted} (use edge weights, default \code{TRUE}) and
#'   \code{loops} (keep self-loops, default \code{TRUE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Magelinski, T., Bartulovic, M., & Carley, K. M. (2021).
#'   Measuring node contribution to community structure with modularity
#'   vitality. IEEE Transactions on Network Science and Engineering, 8(1),
#'   707-723.
#' @seealso \code{\link{centrality_participation}},
#'   \code{\link{centrality_within_module_z}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_modularity_vitality(regulation_net,
#'                                membership = rep(1:2, each = 5))
centrality_modularity_vitality <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "modularity_vitality",
                   membership = membership, ...)
  stats::setNames(df$modularity_vitality, df$node)
}

#' Neighborhood Connectivity
#'
#' Neighborhood connectivity (Maslov and Sneppen 2002) is the mean degree of
#' the neighbors of a node, the average neighbor degree reported by
#' Cytoscape:
#' \deqn{C_{NC}(i) = \frac{1}{k_i} \sum_{j \in N(i)} k_j.}{
#'   C_NC(i) = (1 / k_i) sum_{j in N(i)} k_j.}
#'
#' @details
#' Edge weights are ignored. Under \code{mode = "out"} the out-degrees of the
#' out-neighbors are averaged, and under \code{mode = "in"} the in-degrees of
#' the in-neighbors. Self-loops change the degrees, and \code{loops = FALSE}
#' drops them. Isolated nodes score 0. High values mark nodes attached to
#' hubs.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#'   uses \code{loops} (keep self-loops, default \code{TRUE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Maslov, S., & Sneppen, K. (2002). Specificity and stability
#'   in topology of protein networks. Science, 296(5569), 910-913.
#' @seealso \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_neighborhood_connectivity(regulation_net)
centrality_neighborhood_connectivity <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "neighborhood_connectivity", mode = mode, ...)
  stats::setNames(df[[paste0("neighborhood_connectivity_", mode)]], df$node)
}
