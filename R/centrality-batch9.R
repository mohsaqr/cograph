# ===========================================================================
# Batch 9 — remaining Centrality Zoo measures with pinned definitions
#
# igraph-facing calculators over R/kernels-batch9.R and the exported verbs.
# ===========================================================================

#' Undirected simple neighbor matrix by mode, with membership validation
#' @keywords internal
#' @noRd
.cg_community_input <- function(cg, membership, mode, what) {
  n <- cg$n
  if (is.null(membership)) {
    .cg_warn_no_membership(what)
    return(NULL)
  }
  if (length(membership) != n || anyNA(membership)) {
    msg <- sprintf("`membership` needs one non-missing label per node (%d), %s",
                   n, sprintf("got length %d", length(membership)))
    stop(errorCondition(msg, class = "cograph_bad_membership", call = NULL))
  }
  b <- .cg_path_matrix(cg, NULL)
  nb <- switch(mode, all = (b + t(b)) != 0, out = b != 0, "in" = t(b) != 0)
  nb <- nb & (row(nb) != col(nb))
  storage.mode(nb) <- "numeric"
  nb
}

#' Community-aware batch 9 calculators
#' @keywords internal
#' @noRd
calculate_community_based <- function(cg, membership = NULL, mode = "all") {
  n <- cg$n
  if (n == 0L) return(numeric(0))
  nb <- .cg_community_input(cg, membership, mode, "community_based")
  if (is.null(nb)) return(rep(NA_real_, n))
  .cg_community_based(nb, membership)
}

#' @keywords internal
#' @noRd
calculate_comm_centrality <- function(cg, membership = NULL, mode = "all",
                                      r = "max_intra") {
  ok_r <- identical(r, "max_intra") ||
    (is.numeric(r) && length(r) == 1L && is.finite(r) && r > 0)
  if (!ok_r) {
    stop(errorCondition(
      "`comm_r` must be \"max_intra\" or a single positive number",
      class = "cograph_bad_parameter", call = NULL))
  }
  n <- cg$n
  if (n == 0L) return(numeric(0))
  nb <- .cg_community_input(cg, membership, mode, "comm_centrality")
  if (is.null(nb)) return(rep(NA_real_, n))
  .cg_comm_centrality(nb, membership, r = r)
}

#' @keywords internal
#' @noRd
calculate_community_mediator <- function(cg, membership = NULL, mode = "all") {
  n <- cg$n
  if (n == 0L) return(numeric(0))
  nb <- .cg_community_input(cg, membership, mode, "community_mediator")
  if (is.null(nb)) return(rep(NA_real_, n))
  .cg_community_mediator(nb, membership)
}

#' Dimension-family batch 9 calculators
#' @keywords internal
#' @noRd
calculate_local_dimension_fixed <- function(cg, mode = "all", r = 2,
                                            hop_mat = NULL) {
  if (!is.numeric(r) || length(r) != 1L || !is.finite(r) || r < 1) {
    stop(errorCondition(
      "`ld_radius` must be a single number of at least 1: it is a ball radius in hops",
      class = "cograph_bad_parameter", call = NULL))
  }
  if (cg$n == 0L) return(numeric(0))
  .cg_local_dimension_fixed(hop_mat %||% .cg_hop_distances(cg, mode), r = r)
}

#' @keywords internal
#' @noRd
calculate_fuzzy_local_dimension <- function(cg, mode = "all", hop_mat = NULL) {
  if (cg$n == 0L) return(numeric(0))
  .cg_fuzzy_local_dimension(hop_mat %||% .cg_hop_distances(cg, mode))
}

#' @keywords internal
#' @noRd
calculate_local_volume_dimension <- function(cg, mode = "all",
                                             hop_mat = NULL) {
  if (cg$n == 0L) return(numeric(0))
  b <- .cg_path_matrix(cg, NULL)
  nb <- .cg_edge_indicator(b)
  deg <- switch(mode, all = rowSums(pmax(nb, t(nb))), out = rowSums(nb),
                "in" = colSums(nb))
  .cg_local_volume_dimension(hop_mat %||% .cg_hop_distances(cg, mode), deg)
}

#' Community-Based Centralities
#'
#' Three measures that score a node from a partition \code{membership}.
#' Community-based centrality (Zhao et al. 2015) is
#' \eqn{CbC(i) = \sum_w d_{iw} S_w / N}{CbC(i) = sum_w d_iw S_w / N}, where
#' \eqn{d_{iw}}{d_iw} counts the links of \eqn{i} into community \eqn{w} of
#' size \eqn{S_w}{S_w}. Comm centrality (Gupta, Singh and Cherifi 2016)
#' combines intra-community degree \eqn{k^{in}}{k_in} and inter-community
#' degree \eqn{k^{out}}{k_out}:
#' \deqn{CC(i) = (1 + \mu_C) \frac{k^{in}_i}{\max_{j \in C} k^{in}_j} R
#'   + (1 - \mu_C)
#'   \left(\frac{k^{out}_i}{\max_{j \in C} k^{out}_j} R\right)^2,}{
#'   CC(i) = (1 + mu_C) (k_in(i) / max_{j in C} k_in(j)) R
#'   + (1 - mu_C) ((k_out(i) / max_{j in C} k_out(j)) R)^2,}
#' with \eqn{\mu_C}{mu_C} the mean inter-link fraction in the community of
#' \eqn{i}. Community-based mediator centrality (Tulu, Hou and Younas 2018)
#' is \eqn{CbM(i) = H_i\, d_i / \sum_j d_j}{CbM(i) = H_i d_i / sum_j d_j},
#' with \eqn{H_i}{H_i} the base-2 entropy of the links of \eqn{i} over the
#' communities.
#'
#' @details
#' Edge weights and self-loops are ignored. Under \code{mode = "out"} or
#' \code{mode = "in"} only out-links or in-links count, and the default
#' ignores direction. Higher values mark more central nodes in all three.
#' The default \code{comm_r = "max_intra"} sets \eqn{R} to the largest
#' intra-community degree of each community, the choice the source
#' recommends. The prose of Gupta et al. writes \eqn{\mu_C}{mu_C} where their
#' equation has \eqn{1 + \mu_C}{1 + mu_C}, and the equation is implemented.
#' Nodes
#' linked to one community only score 0 on the mediator measure. Without
#' \code{membership} each function raises a warning of classes \code{cograph_bad_membership} and
#' \code{cograph_undefined_measure} and
#' returns \code{NA}. A \code{membership} that is not one non-missing label per node
#' raises an error of class \code{cograph_bad_membership}, and an invalid
#' \code{comm_r} raises \code{cograph_bad_parameter}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Community labels, one per node, for example from
#'   \code{\link{detect_communities}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   or \code{"in"}.
#' @param comm_r Scale \eqn{R} of Comm centrality: \code{"max_intra"}
#'   (default) or a single positive number applied to every community.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhao, Z., Wang, X., Zhang, W., & Zhu, Z. (2015). A community-based
#'   approach to identifying influential spreaders. Entropy, 17(4),
#'   2228-2252.
#'
#' Gupta, N., Singh, A., & Cherifi, H. (2016). Centrality measures for
#'   networks with community structure. Physica A, 452, 46-59.
#'
#' Tulu, M. M., Hou, R., & Younas, T. (2018). Identifying influential nodes
#'   based on community structure to speed up the dissemination of
#'   information in complex network. IEEE Access, 6, 7390-7401.
#' @seealso \code{\link{centrality_community_hub_bridge}},
#'   \code{\link{centrality_participation}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_community_based(regulation_net, membership = rep(1:2, each = 5))
#' centrality_comm_centrality(regulation_net, membership = rep(1:2, each = 5))
#' centrality_community_mediator(regulation_net,
#'                               membership = rep(1:2, each = 5))
centrality_community_based <- function(x, membership = NULL, mode = "all",
                                       ...) {
  df <- centrality(x, measures = "community_based", mode = mode,
                   membership = membership, ...)
  stats::setNames(df[[paste0("community_based_", mode)]], df$node)
}

#' @rdname centrality_community_based
#' @export
centrality_comm_centrality <- function(x, membership = NULL, mode = "all",
                                       comm_r = "max_intra", ...) {
  df <- centrality(x, measures = "comm_centrality", mode = mode,
                   membership = membership, comm_r = comm_r, ...)
  stats::setNames(df[[paste0("comm_centrality_", mode)]], df$node)
}

#' @rdname centrality_community_based
#' @export
centrality_community_mediator <- function(x, membership = NULL, mode = "all",
                                          ...) {
  df <- centrality(x, measures = "community_mediator", mode = mode,
                   membership = membership, ...)
  stats::setNames(df[[paste0("community_mediator_", mode)]], df$node)
}

#' Fixed-Radius, Fuzzy and Volume Local Dimensions
#'
#' Three members of the local-dimension family, with the center node counted
#' in its own ball. The fixed-radius local dimension (Silva and Costa 2013)
#' is
#' \eqn{D_i(r) = r\, n_i(r) / B_i(r)}{D_i(r) = r n_i(r) / B_i(r)} at one
#' radius \eqn{r}, where \eqn{n_i(r)}{n_i(r)} counts the nodes at distance
#' \eqn{r} and \eqn{B_i(r)}{B_i(r)} those within it. The fuzzy local
#' dimension (Wen and Jiang 2019) is the slope of \eqn{\log N_i(r)}{log N_i(r)}
#' on \eqn{\log r}{log r} for the fuzzy ball
#' \deqn{N_i(r) = \frac{\sum_{d_{ij} \le r} e^{-d_{ij}^2 / r^2}}
#'   {|\{j : d_{ij} \le r\}|}.}{
#'   N_i(r) = sum_{d_ij <= r} exp(-d_ij^2 / r^2) / |{j : d_ij <= r}|.}
#' The local volume dimension (Li and Deng 2021) is the slope of
#' \eqn{\ln V_i(l)}{ln V_i(l)} on \eqn{\ln l}{ln l} for the volume
#' \eqn{V_i(l) = \sum_{d_{ij} \le l} k_j}{V_i(l) = sum_{d_ij <= l} k_j}.
#'
#' @details
#' Distances are hop counts, so edge weights are ignored. On a directed
#' network \code{mode} sets the direction of the paths. The fixed-radius
#' form is a structural descriptor, and nodes whose eccentricity is below
#' the radius score 0. Larger fuzzy dimensions and smaller volume dimensions
#' mark more influential nodes. The two regression forms return \code{NaN}
#' for a node with fewer than two radii. The volume dimension follows a
#' later preprint of the authors and the Centrality Zoo entry. A
#' \code{ld_radius} that is not a single number of at least 1 raises an
#' error of class \code{cograph_bad_parameter}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   or \code{"in"}.
#' @param ld_radius Radius \eqn{r} for \code{local_dimension_fixed}, in hops
#'   (default 2).
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Silva, F. N., & Costa, L. da F. (2013). Local dimension of complex
#'   networks. arXiv:1209.2476.
#'
#' Wen, T., & Jiang, W. (2019). Identifying influential nodes based on fuzzy
#'   local dimension in complex networks. Chaos, Solitons & Fractals, 119,
#'   332-342.
#'
#' Li, H., & Deng, Y. (2021). Local volume dimension: A novel approach for
#'   important nodes identification in complex networks. International
#'   Journal of Modern Physics B, 35(5), 2150069.
#' @seealso \code{\link{centrality_local_dimension}},
#'   \code{\link{centrality_local_information_dimension}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_local_dimension_fixed(regulation_net)
#' centrality_fuzzy_local_dimension(regulation_net)
#' centrality_local_volume_dimension(regulation_net)
centrality_local_dimension_fixed <- function(x, mode = "all", ld_radius = 2,
                                             ...) {
  df <- centrality(x, measures = "local_dimension_fixed", mode = mode,
                   ld_radius = ld_radius, ...)
  stats::setNames(df[[paste0("local_dimension_fixed_", mode)]], df$node)
}

#' @rdname centrality_local_dimension_fixed
#' @export
centrality_fuzzy_local_dimension <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "fuzzy_local_dimension", mode = mode, ...)
  stats::setNames(df[[paste0("fuzzy_local_dimension_", mode)]], df$node)
}

#' @rdname centrality_local_dimension_fixed
#' @export
centrality_local_volume_dimension <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "local_volume_dimension", mode = mode, ...)
  stats::setNames(df[[paste0("local_volume_dimension_", mode)]], df$node)
}

# ---------------------------------------------------------------------------
# VoteRank variants, node contraction, two-way random-walk betweenness
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
calculate_wvoterank <- function(cg, weights = NULL) {
  if (cg$n == 0L) return(numeric(0))
  .cg_wvoterank(.cg_path_matrix(cg, weights))
}

#' @keywords internal
#' @noRd
calculate_enrenew <- function(cg, depth = 2L) {
  if (cg$n == 0L) return(numeric(0))
  b <- .cg_path_matrix(cg, NULL)
  b <- pmax(b, t(b))
  .cg_enrenew(b, .cg_distances(b, "all"), depth = as.integer(depth))
}

#' @keywords internal
#' @noRd
calculate_voterank_plus <- function(cg, lambda = 0.1) {
  if (cg$n == 0L) return(numeric(0))
  .cg_voterank_plus(.cg_path_matrix(cg, NULL), lambda = lambda)
}

#' @keywords internal
#' @noRd
calculate_node_contraction <- function(cg, improved = FALSE, rho = 5) {
  if (cg$n == 0L) return(numeric(0))
  nb <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  if (improved) .cg_improved_node_contraction(nb, rho = rho)
  else .cg_node_contraction(nb)
}

#' @keywords internal
#' @noRd
calculate_two_way_rw <- function(cg, weights = NULL) {
  if (cg$n == 0L) return(numeric(0))
  .cg_two_way_rw(.cg_path_matrix(cg, weights))
}

#' WVoteRank, EnRenew and VoteRank++
#'
#' Three spreader-selection procedures in the VoteRank family that elect
#' one node per round. WVoteRank (Sun et al. 2019) scores a node by
#' \eqn{s_v = \sqrt{k_v \sum_{u \in N(v)} va_u w_{vu}}}{
#'   s_v = sqrt(k_v sum_{u in N(v)} va_u w_vu)}
#' and lowers the ability of the neighbors of a winner by
#' \eqn{1 / \langle w \rangle}{1 / <w>}, the inverse of the average strength.
#' EnRenew (Guo et al. 2020) elects the largest neighbor entropy
#' \eqn{E_v = -\sum_{u \in N(v)} p_{uv} \ln p_{uv}}{
#'   E_v = -sum_{u in N(v)} p_uv ln p_uv},
#' \eqn{p_{uv} = k_u / \sum_{l \in N(v)} k_l}{p_uv = k_u / sum_{l in N(v)} k_l},
#' and scales the entropy terms within \code{enrenew_depth} steps by
#' \eqn{1 - 1 / (2^{d-1} \ln \langle k \rangle)}{1 - 1 / (2^(d-1) ln <k>)}.
#' VoteRank++ (Liu et al. 2021) starts from ability
#' \eqn{\ln(1 + k_i / k_{\max})}{ln(1 + k_i / k_max)}, splits votes in
#' proportion to degree, and multiplies abilities by \eqn{\lambda}{lambda}
#' one step from a winner and by \eqn{\sqrt{\lambda}}{sqrt(lambda)} two
#' steps away.
#'
#' @details
#' Elections continue until every node is placed. The first elected scores 1
#' and the last \eqn{1/n}{1/n}, so scores lie in \eqn{(0, 1]}{(0, 1]}, and
#' ties go to the lowest node index. Direction and self-loops are ignored.
#' WVoteRank uses edge weights, and with unit weights it is VoteRank with a
#' square-root score. The other two ignore weights. The renewal factor of
#' EnRenew is negative when \eqn{\langle k \rangle < e}{<k> < e}. EnRenew
#' follows the article where the released code of the authors differs from
#' it. VoteRank++ follows the released code of the authors, including the
#' exclusion of elected nodes from the vote-share denominator.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param enrenew_depth Renewal radius \eqn{l} for EnRenew (default 2).
#' @param voterank_lambda Suppression factor \eqn{\lambda}{lambda} for
#'   VoteRank++ (default 0.1).
#' @param ... Further arguments to \code{\link{centrality}}. WVoteRank uses
#'   \code{weighted} (use edge weights, default \code{TRUE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Sun, H.-L., Chen, D.-B., He, J.-L., & Ch'ng, E. (2019). A voting
#'   approach to uncover multiple influential spreaders on weighted
#'   networks. Physica A, 519, 303-312.
#'
#' Guo, C., Yang, L., Guo, X., Pan, J., & Chen, X. (2020). Influential
#'   nodes identification in complex networks via information entropy.
#'   Entropy, 22(2), 242.
#'
#' Liu, P., Li, L., Fang, S., & Yao, Y. (2021). Identifying influential
#'   nodes in social networks: A voting approach. Chaos, Solitons &
#'   Fractals, 152, 111309.
#' @seealso \code{\link{centrality_voterank}},
#'   \code{\link{centrality_ncvoterank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_wvoterank(regulation_net)
#' centrality_enrenew(regulation_net)
#' centrality_voterank_plus(regulation_net)
centrality_wvoterank <- function(x, ...) {
  df <- centrality(x, measures = "wvoterank", ...)
  stats::setNames(df$wvoterank, df$node)
}

#' @rdname centrality_wvoterank
#' @export
centrality_enrenew <- function(x, enrenew_depth = 2, ...) {
  df <- centrality(x, measures = "enrenew", enrenew_depth = enrenew_depth, ...)
  stats::setNames(df$enrenew, df$node)
}

#' @rdname centrality_wvoterank
#' @export
centrality_voterank_plus <- function(x, voterank_lambda = 0.1, ...) {
  df <- centrality(x, measures = "voterank_plus",
                   voterank_lambda = voterank_lambda, ...)
  stats::setNames(df$voterank_plus, df$node)
}

#' Node Contraction Centrality
#'
#' Node contraction importance (Tan, Wu and Deng 2006, as restated by Wang
#' et al. 2011) compares the agglomeration
#' \eqn{\partial(G) = 1 / (N \bar{L})}{d(G) = 1 / (N L)} of a network,
#' with \eqn{\bar{L}}{L} the mean shortest-path length, before and after the
#' node is merged with all its neighbors into one node:
#' \deqn{IMC(v) = 1 - \frac{\partial(G)}{\partial(G_v)}.}{
#'   IMC(v) = 1 - d(G) / d(G_v).}
#' The improved form adds the same score of the edges of the node computed
#' on the line graph,
#' \eqn{IIMC(v) = \alpha\, IMC(v) + \beta \sum_{e \ni v} IMC_{L(G)}(e)}{
#'   IIMC(v) = alpha IMC(v) + beta sum_{e at v} IMC_L(G)(e)},
#' with \eqn{\alpha + \beta = 1}{alpha + beta = 1}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights and loops are ignored. Higher values mark
#' more important nodes. The sources assume a connected network. On a
#' disconnected network the mean path length is taken over the mutually
#' reachable ordered pairs. An isolated node scores 0, and a node whose
#' contraction leaves no pair of mutually reachable nodes returns
#' \code{NaN}. The Centrality Zoo describes the contracted graph as the
#' graph with the node removed. The sources define it by contraction, which
#' is implemented here.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param contraction_rho Ratio \eqn{\alpha / \beta}{alpha / beta} for the
#'   improved form (default 5).
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Tan, Y.-J., Wu, J., & Deng, H.-Z. (2006). Evaluation method for node
#'   importance based on node contraction in complex networks. Systems
#'   Engineering: Theory & Practice, 26(11), 79-83.
#' @seealso \code{\link{centrality_closeness_vitality}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_node_contraction(regulation_net)
#' centrality_node_contraction_improved(regulation_net)
centrality_node_contraction <- function(x, ...) {
  df <- centrality(x, measures = "node_contraction", ...)
  stats::setNames(df$node_contraction, df$node)
}

#' @rdname centrality_node_contraction
#' @export
centrality_node_contraction_improved <- function(x, contraction_rho = 5, ...) {
  df <- centrality(x, measures = "node_contraction_improved",
                   contraction_rho = contraction_rho, ...)
  stats::setNames(df$node_contraction_improved, df$node)
}

#' Two-Way Random Walk Betweenness
#'
#' Two-way random walk betweenness (Curado et al. 2022) counts how often a
#' node lies on the strongest two-way route between a pair. For every
#' unordered pair \eqn{(i, j)} the two-step transfer
#' \eqn{P_{itj} = w_{it} w_{tj} / (d_i d_j)}{P_itj = w_it w_tj / (d_i d_j)}
#' is combined into
#' \eqn{T_{ij}[t, k] = P_{itj} P_{jki}}{T_ij[t, k] = P_itj P_jki}, the
#' diagonal is dropped, and the largest entry credits one count to \eqn{t}
#' and one to \eqn{k}. The score of a node is its total count over all
#' pairs.
#'
#' @details
#' Edge weights are used, and direction and self-loops are ignored. Ties in
#' the maximum go to the first entry in row-major order. Nodes that never
#' lie on a winning two-way route score 0. The source divides the transfer
#' by \eqn{d_i d_j}{d_i d_j}, where a random-walk probability would divide
#' by \eqn{d_i d_t}{d_i d_t}. The formula is implemented as printed.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#'   uses \code{weighted} (use edge weights, default \code{TRUE}).
#' @return A named numeric vector with one count per node, in input node
#'   order.
#' @references Curado, M., Rodriguez, R., Tortosa, L., & Vicent, J. F.
#'   (2022). A new centrality measure in dense networks based on two-way
#'   random walk betweenness. Applied Mathematics and Computation, 412,
#'   126560.
#' @seealso \code{\link{centrality_current_flow_betweenness}},
#'   \code{\link{centrality_betweenness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_two_way_rw(regulation_net)
centrality_two_way_rw <- function(x, ...) {
  df <- centrality(x, measures = "two_way_rw", ...)
  stats::setNames(df$two_way_rw, df$node)
}

# ---------------------------------------------------------------------------
# Simple local measures, coreness variants, geodesic k-path
# ---------------------------------------------------------------------------

#' Neighbor matrix by mode with loops dropped
#' @keywords internal
#' @noRd
.cg_mode_neighbours <- function(cg, mode) {
  a <- .cg_edge_indicator(.cg_path_matrix(cg, NULL))
  switch(mode, all = pmax(a, t(a)), out = a, "in" = t(a))
}

#' @keywords internal
#' @noRd
calculate_heatmap <- function(cg, mode = "all", hop_mat = NULL) {
  if (cg$n == 0L) return(numeric(0))
  .cg_heatmap(.cg_mode_neighbours(cg, mode),
              hop_mat %||% .cg_hop_distances(cg, mode))
}

#' @keywords internal
#' @noRd
calculate_flow_coefficient <- function(cg) {
  if (cg$n == 0L) return(numeric(0))
  .cg_flow_coefficient(.cg_path_matrix(cg, NULL))
}

#' @keywords internal
#' @noRd
calculate_local_entropy <- function(cg, mode = "all") {
  if (cg$n == 0L) return(numeric(0))
  nb <- .cg_mode_neighbours(cg, mode)
  .cg_local_entropy(nb, rowSums(nb))
}

#' @keywords internal
#' @noRd
calculate_weighted_h_index <- function(cg, mode = "all") {
  if (cg$n == 0L) return(integer(0))
  nb <- .cg_mode_neighbours(cg, mode)
  .cg_weighted_h_index(nb, rowSums(nb))
}

#' @keywords internal
#' @noRd
calculate_redundancy <- function(cg) {
  if (cg$n == 0L) return(numeric(0))
  .cg_redundancy(.cg_mode_neighbours(cg, "all"))
}

#' @keywords internal
#' @noRd
calculate_weighted_kshell <- function(cg, weights = NULL, alpha = 1, beta = 1) {
  if (cg$n == 0L) return(integer(0))
  .cg_weighted_kshell(.cg_path_matrix(cg, weights), alpha = alpha, beta = beta)
}

#' @keywords internal
#' @noRd
calculate_renewed_coreness <- function(cg, threshold = 2) {
  if (cg$n == 0L) return(integer(0))
  .cg_renewed_coreness(.cg_mode_neighbours(cg, "all"), threshold = threshold)
}

#' @keywords internal
#' @noRd
calculate_geodesic_kpath <- function(cg, mode = "all", k = 3, hop_mat = NULL) {
  if (cg$n == 0L) return(numeric(0))
  .cg_geodesic_kpath(.cg_mode_neighbours(cg, mode),
                     hop_mat %||% .cg_hop_distances(cg, mode), k = k)
}

#' Heatmap, Flow Coefficient, Local Entropy, Weighted h-index and Redundancy
#'
#' Five local measures. Heatmap centrality (Duron 2020) is the farness of a
#' node minus the mean farness of its neighbors,
#' \eqn{C(v) = f(v) - \frac{1}{k_v} \sum_{u \in N(v)} f(u)}{
#'   C(v) = f(v) - (1 / k_v) sum_{u in N(v)} f(u)},
#' with \eqn{f} the sum of hop distances to the reachable nodes. The flow
#' coefficient (Honey et al. 2007) is the fraction of ordered pairs of
#' distinct neighbors joined by a two-step path through the node and by no
#' direct link, as in the Brain Connectivity Toolbox. Local entropy (Nie et
#' al. 2016) is
#' \eqn{-\sum_{j \in N(i)} k_j \ln k_j}{-sum_{j in N(i)} k_j ln k_j}. The
#' weighted h-index (Gao et al. 2019) is the h-index of the multiset in
#' which each neighbor \eqn{j} contributes the value \eqn{k_i k_j} repeated
#' \eqn{k_j} times. Redundancy (Burt 1992; Borgatti 1997) is the mean degree
#' of the neighbors of a node within its ego network.
#'
#' @details
#' Edge weights are ignored by all five. Heatmap, local entropy and the
#' weighted h-index follow \code{mode}. Redundancy ignores direction, and
#' the flow coefficient uses the direction of the links. On an undirected
#' network the flow coefficient of a node with at least two neighbors equals
#' one minus its clustering coefficient, and redundancy equals degree minus
#' effective size. Lower values mark more
#' central nodes for heatmap and local entropy. Isolated nodes return
#' \code{NaN} for heatmap and 0 for local entropy, and nodes with fewer than
#' two neighbors score 0 on the flow coefficient. The formula for local
#' entropy follows the Centrality Zoo and the survey of Omar and Plapper
#' (2021), which agree.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order. The weighted h-index is an integer vector.
#' @references
#' Duron, C. (2020). Heatmap centrality: A new measure to identify super-
#'   spreader nodes in scale-free networks. PLOS ONE, 15(7), e0235690.
#'
#' Honey, C. J., Kotter, R., Breakspear, M., & Sporns, O. (2007). Network
#'   structure of cerebral cortex shapes functional connectivity on multiple
#'   time scales. PNAS, 104(24), 10240-10245.
#'
#' Nie, T., Guo, Z., Zhao, K., & Lu, Z.-M. (2016). Using mapping entropy to
#'   identify node centrality in complex networks. Physica A, 453, 290-297.
#'
#' Gao, L., Yu, S., Li, M., Shen, Z., & Gao, Z. (2019). Weighted h-index
#'   for identifying influential spreaders. Symmetry, 11(10), 1263.
#'
#' Borgatti, S. P. (1997). Structural holes: Unpacking Burt's redundancy
#'   measures. Connections, 20(1), 35-38.
#' @seealso \code{\link{centrality_effective_size}},
#'   \code{\link{centrality_transitivity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_heatmap(regulation_net)
#' centrality_flow_coefficient(regulation_net)
#' centrality_local_entropy(regulation_net)
#' centrality_weighted_h_index(regulation_net)
#' centrality_redundancy(regulation_net)
centrality_heatmap <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "heatmap", mode = mode, ...)
  stats::setNames(df[[paste0("heatmap_", mode)]], df$node)
}

#' @rdname centrality_heatmap
#' @export
centrality_flow_coefficient <- function(x, ...) {
  df <- centrality(x, measures = "flow_coefficient", ...)
  stats::setNames(df$flow_coefficient, df$node)
}

#' @rdname centrality_heatmap
#' @export
centrality_local_entropy <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "local_entropy", mode = mode, ...)
  stats::setNames(df[[paste0("local_entropy_", mode)]], df$node)
}

#' @rdname centrality_heatmap
#' @export
centrality_weighted_h_index <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "weighted_h_index", mode = mode, ...)
  stats::setNames(df[[paste0("weighted_h_index_", mode)]], df$node)
}

#' @rdname centrality_heatmap
#' @export
centrality_redundancy <- function(x, ...) {
  df <- centrality(x, measures = "redundancy", ...)
  stats::setNames(df$redundancy, df$node)
}

#' Weighted k-shell, Renewed Coreness and Geodesic k-path
#'
#' Three shell and path counts. The weighted k-shell (Garas, Schweitzer and
#' Havlin 2012) runs the k-shell decomposition on the generalized degree
#' \deqn{k'_i = \left(k_i^{\alpha} s_i^{\beta}\right)^{1 / (\alpha + \beta)},}{
#'   k'_i = (k_i^alpha s_i^beta)^(1 / (alpha + beta)),}
#' with strength \eqn{s_i}{s_i} after the weight normalization of the
#' source. Renewed coreness (Liu, Tang, Zhou and Do 2015) gives each link
#' the diffusion importance
#' \eqn{D_{ij} = (|N(j) \setminus N[i]| + |N(i) \setminus N[j]|) / 2}{
#'   D_ij = (|N(j) - N[i]| + |N(i) - N[j]|) / 2},
#' removes the links below \code{renewed_threshold}, and returns the k-core
#' number of the residual graph. Geodesic k-path centrality (Borgatti and
#' Everett 2006) counts the shortest paths of length at most \code{kpath_k}
#' that start at the node, with multiplicity.
#'
#' @details
#' The weighted k-shell uses edge weights, and with unit weights it equals
#' the k-core number. Isolated nodes score 0. Renewed coreness ignores
#' weights, and a clique with no outside links scores 0. Both ignore
#' direction. Geodesic k-path ignores weights and follows \code{mode}. The
#' Centrality Zoo transcribes the diffusion importance with open
#' neighborhoods, which is off by one, and the closed neighborhoods of the
#' source are used here. \code{centiserve::geokpath} counts the nodes within
#' \eqn{k} hops, which is the vertex-disjoint variant of Borgatti and
#' Everett.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param wks_alpha,wks_beta Exponents of degree and strength in the
#'   weighted k-shell (default 1 and 1).
#' @param renewed_threshold Diffusion-importance threshold for renewed
#'   coreness (default 2).
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   or \code{"in"}.
#' @param kpath_k Maximum path length for geodesic k-path (default 3).
#' @param ... Further arguments to \code{\link{centrality}}. The weighted
#'   k-shell uses \code{weighted} (use edge weights, default \code{TRUE}).
#' @return A named numeric vector with one score per node, in input node
#'   order. The weighted k-shell and renewed coreness are integer vectors.
#' @references
#' Garas, A., Schweitzer, F., & Havlin, S. (2012). A k-shell decomposition
#'   method for weighted networks. New Journal of Physics, 14, 083030.
#'
#' Liu, Y., Tang, M., Zhou, T., & Do, Y. (2015). Improving the accuracy of
#'   the k-shell method by removing redundant links: From a perspective of
#'   spreading dynamics. Scientific Reports, 5, 13172. \doi{10.1038/srep13172}.
#'
#' Borgatti, S. P., & Everett, M. G. (2006). A graph-theoretic perspective
#'   on centrality. Social Networks, 28(4), 466-484.
#' @seealso \code{\link{centrality_coreness}}, \code{\link{centrality_s_shell}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_weighted_kshell(regulation_net)
#' centrality_renewed_coreness(regulation_net)
#' centrality_geodesic_kpath(regulation_net)
centrality_weighted_kshell <- function(x, wks_alpha = 1, wks_beta = 1, ...) {
  df <- centrality(x, measures = "weighted_kshell", wks_alpha = wks_alpha,
                   wks_beta = wks_beta, ...)
  stats::setNames(df$weighted_kshell, df$node)
}

#' @rdname centrality_weighted_kshell
#' @export
centrality_renewed_coreness <- function(x, renewed_threshold = 2, ...) {
  df <- centrality(x, measures = "renewed_coreness",
                   renewed_threshold = renewed_threshold, ...)
  stats::setNames(df$renewed_coreness, df$node)
}

#' @rdname centrality_weighted_kshell
#' @export
centrality_geodesic_kpath <- function(x, mode = "all", kpath_k = 3, ...) {
  df <- centrality(x, measures = "geodesic_kpath", mode = mode,
                   kpath_k = kpath_k, ...)
  stats::setNames(df[[paste0("geodesic_kpath_", mode)]], df$node)
}

# ---------------------------------------------------------------------------
# Measure metadata
# ---------------------------------------------------------------------------

#' Measures whose cost grows steeply with network size
#'
#' Held back from `centrality(type = "all")`. Measured on an 81-node graph,
#' `infection` alone took 611 seconds while every other measure together
#' took about five; the next three are superlinear by construction.
#' `fragmentation` re-solves all-pairs shortest paths once per node (60 s at
#' n = 200) and `epc` is a Monte Carlo estimate whose default 1000 runs cost
#' 8 s at the same size -- and whose value moves between calls unless
#' `epc_seed` is set, which a default tier should not do.
#'
#' @return Character vector of measure names.
#' @keywords internal
#' @noRd
.cg_costly_measures <- function() {
  c("controlrank", "extended_local_bridging", "bridging_capital", "linerank",
    "random_walk_decay", "infection",
    "two_way_rw", "node_contraction_improved",
    "entropy_variation_betweenness", "fragmentation", "epc", "mcc",
    "dynamical_importance", "resistance_curvature", "exogenous",
    "rsp_betweenness", "iec", "trust_pagerank")
}
