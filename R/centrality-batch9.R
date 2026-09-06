# ===========================================================================
# Batch 9 — remaining Centrality Zoo measures with pinned definitions
#
# igraph-facing calculators over R/kernels-batch9.R and the exported verbs.
# ===========================================================================

#' Undirected simple neighbour matrix by mode, with membership validation
#' @keywords internal
#' @noRd
.cg_community_input <- function(g, membership, mode, what) {
  n <- igraph::vcount(g)
  if (is.null(membership)) {
    warning(what, " requires membership; returning NA", call. = FALSE)
    return(NULL)
  }
  if (length(membership) != n || anyNA(membership)) {
    msg <- sprintf("`membership` needs one non-missing label per node (%d), %s",
                   n, sprintf("got length %d", length(membership)))
    stop(errorCondition(msg, class = "cograph_bad_membership", call = NULL))
  }
  b <- .cg_path_matrix(g, NULL)
  nb <- switch(mode, all = (b + t(b)) != 0, out = b != 0, "in" = t(b) != 0)
  nb <- nb & (row(nb) != col(nb))
  storage.mode(nb) <- "numeric"
  nb
}

#' Community-aware batch 9 calculators
#' @keywords internal
#' @noRd
calculate_community_based <- function(g, membership = NULL, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0L) return(numeric(0))
  nb <- .cg_community_input(g, membership, mode, "community_based")
  if (is.null(nb)) return(rep(NA_real_, n))
  .cg_community_based(nb, membership)
}

#' @keywords internal
#' @noRd
calculate_comm_centrality <- function(g, membership = NULL, mode = "all",
                                      r = "max_intra") {
  n <- igraph::vcount(g)
  if (n == 0L) return(numeric(0))
  nb <- .cg_community_input(g, membership, mode, "comm_centrality")
  if (is.null(nb)) return(rep(NA_real_, n))
  .cg_comm_centrality(nb, membership, r = r)
}

#' @keywords internal
#' @noRd
calculate_community_mediator <- function(g, membership = NULL, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0L) return(numeric(0))
  nb <- .cg_community_input(g, membership, mode, "community_mediator")
  if (is.null(nb)) return(rep(NA_real_, n))
  .cg_community_mediator(nb, membership)
}

#' Dimension-family batch 9 calculators
#' @keywords internal
#' @noRd
calculate_local_dimension_fixed <- function(g, mode = "all", r = 2,
                                            hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_local_dimension_fixed(hop_mat %||% .cg_hop_distances(g, mode), r = r)
}

#' @keywords internal
#' @noRd
calculate_fuzzy_local_dimension <- function(g, mode = "all", hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_fuzzy_local_dimension(hop_mat %||% .cg_hop_distances(g, mode))
}

#' @keywords internal
#' @noRd
calculate_local_volume_dimension <- function(g, mode = "all",
                                             hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  b <- .cg_path_matrix(g, NULL)
  nb <- .cg_edge_indicator(b)
  deg <- switch(mode, all = rowSums(pmax(nb, t(nb))), out = rowSums(nb),
                "in" = colSums(nb))
  .cg_local_volume_dimension(hop_mat %||% .cg_hop_distances(g, mode), deg)
}

#' Community-Based Centrality, Comm Centrality and Community-Based Mediator
#'
#' Three community-aware measures that need a partition (\code{membership}).
#'
#' \describe{
#'   \item{\code{community_based} (Zhao, Wang, Zhang & Zhu 2015)}{
#'     \eqn{CbC(i) = \sum_w d_{iw} S_w / N}: every link of \eqn{i} counts
#'     the size \eqn{S_w} of the community it lands in. No parameters.
#'     Reproduces Table 1 of the paper and Table 1 of Tulu et al. (2018).}
#'   \item{\code{comm_centrality} (Gupta, Singh & Cherifi 2016)}{
#'     \deqn{CC(i) = (1 + \mu_C)\, \frac{k^{in}_i}{\max_{j \in C} k^{in}_j} R
#'       + (1 - \mu_C) \left(\frac{k^{out}_i}{\max_{j \in C} k^{out}_j}
#'       R\right)^2,}
#'     where \eqn{k^{in}, k^{out}} are the intra- and inter-community
#'     degrees, \eqn{\mu_C} the mean inter-link fraction in \eqn{i}'s
#'     community, and \eqn{R} a scale. The default \code{comm_r =
#'     "max_intra"} is the paper's recommended \eqn{R = \max_{j \in C}
#'     k^{in}_j} per community; a number applies one global \eqn{R}. The
#'     equation uses \eqn{1 + \mu_C} although the paper's prose says
#'     \eqn{\mu_C}; the equation is implemented. A community without intra
#'     (inter) links contributes 0 through that term.}
#'   \item{\code{community_mediator} (Tulu, Hou & Younas 2018)}{
#'     \eqn{CbM(i) = H_i \, d_i / \sum_j d_j}, with \eqn{H_i} the base-2
#'     Shannon entropy of \eqn{i}'s link distribution over the communities.
#'     Nodes linked to one community only score 0. Base 2 is what
#'     reproduces the paper's Table 1.}
#' }
#' Higher = more central in all three. Under \code{mode = "out"} or
#' \code{"in"} only out- or in-links count; edge weights are ignored.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param membership Community labels, one per node. Required; without it
#'   the function warns and returns \code{NA}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"},
#'   or \code{"in"}.
#' @param comm_r Scale \eqn{R} of Comm centrality: \code{"max_intra"}
#'   (default) or a positive number.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node.
#'
#' @section Conditions:
#' Raises an error of class \code{cograph_bad_membership} when
#' \code{membership} is not one non-missing label per node.
#'
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
#'
#' @seealso \code{\link{centrality_community_hub_bridge}},
#'   \code{\link{centrality_participation}}.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_community_based(adj, membership = c(1, 1, 1, 2, 2, 2))
#' centrality_comm_centrality(adj, membership = c(1, 1, 1, 2, 2, 2))
#' centrality_community_mediator(adj, membership = c(1, 1, 1, 2, 2, 2))
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
#' Three further members of the local-dimension family, all computed from
#' hop counts (edge weights are ignored) with the centre node counted in
#' its own ball, as in \code{\link{centrality_local_dimension}}.
#'
#' \describe{
#'   \item{\code{local_dimension_fixed} (Silva & Costa 2013)}{The
#'     discretised estimator \eqn{D_i(r) = r\, n_i(r) / B_i(r)} at one
#'     radius \code{ld_radius} (default 2), where \eqn{n_i(r)} is the ring
#'     at distance \eqn{r} and \eqn{B_i(r)} the ball within it. A
#'     structural descriptor rather than an importance ranking; nodes with
#'     eccentricity below the radius score 0. The paper defines a curve in
#'     \eqn{r} and fixes \eqn{r} per figure; the Zoo lists this fixed-radius
#'     form separately from Pu et al.'s regression form.}
#'   \item{\code{fuzzy_local_dimension} (Wen & Jiang 2019)}{Fuzzy ball
#'     \eqn{N_i(r) = \sum_{d_{ij} \le r} e^{-d_{ij}^2 / r^2} / |\{j : d_{ij}
#'     \le r\}|} for \eqn{r = 1, \ldots, d_{\max}(i)}; the measure is the
#'     slope of \eqn{\log N_i(r)} on \eqn{\log r}. Larger = more
#'     influential. Reproduces Table 1 of the paper (Krackhardt kite) and
#'     its karate-club top ten in order.}
#'   \item{\code{local_volume_dimension} (Li & Deng 2021)}{Volume
#'     \eqn{V_i(l) = \sum_{d_{ij} \le l} k_j}, \eqn{l = 1, \ldots,
#'     ecc(i)}; the measure is the slope of \eqn{\ln V_i(l)} on \eqn{\ln
#'     l}. Smaller = more important. The article is closed access; the
#'     definition follows the authors' own later preprint and the Zoo
#'     entry, and no published per-node values exist to check against.}
#' }
#' The two regression measures return \code{NaN} for a node with fewer
#' than two radii.
#'
#' @inheritParams centrality_distance_entropy
#' @param ld_radius Radius \eqn{r} for \code{local_dimension_fixed}.
#'   Default 2.
#'
#' @return Named numeric vector, one value per node.
#'
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
#'
#' @seealso \code{\link{centrality_local_dimension}},
#'   \code{\link{centrality_local_information_dimension}}.
#'
#' @export
#' @examples
#' path5 <- matrix(0, 5, 5)
#' path5[cbind(1:4, 2:5)] <- 1; path5 <- path5 + t(path5)
#' rownames(path5) <- colnames(path5) <- LETTERS[1:5]
#' centrality_local_dimension_fixed(path5)
#' centrality_fuzzy_local_dimension(path5)
#' centrality_local_volume_dimension(path5)
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
calculate_wvoterank <- function(g, weights = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_wvoterank(.cg_path_matrix(g, weights))
}

#' @keywords internal
#' @noRd
calculate_enrenew <- function(g, depth = 2L) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  b <- .cg_path_matrix(g, NULL)
  b <- pmax(b, t(b))
  .cg_enrenew(b, .cg_distances(b, "all"), depth = as.integer(depth))
}

#' @keywords internal
#' @noRd
calculate_voterank_plus <- function(g, lambda = 0.1) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_voterank_plus(.cg_path_matrix(g, NULL), lambda = lambda)
}

#' @keywords internal
#' @noRd
calculate_node_contraction <- function(g, improved = FALSE, rho = 5) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  nb <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  if (improved) .cg_improved_node_contraction(nb, rho = rho)
  else .cg_node_contraction(nb)
}

#' @keywords internal
#' @noRd
calculate_two_way_rw <- function(g, weights = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_two_way_rw(.cg_path_matrix(g, weights))
}

#' WVoteRank, EnRenew and VoteRank++
#'
#' Three further spreader-selection procedures in the VoteRank family. All
#' three elect one node per round until every node is placed and return
#' the election order as a score, 1 for the first elected down to
#' \eqn{1 / n}; ties go to the lowest node index. Direction and self-loops
#' are ignored.
#'
#' \describe{
#'   \item{\code{wvoterank} (Sun, Chen, He & Ch'ng 2019)}{VoteRank for
#'     weighted graphs: \eqn{s_v = \sqrt{k_v \sum_{u \in N(v)} va_u w_{vu}}}.
#'     After an election the winner's ability is 0 and its neighbours lose
#'     \eqn{1 / \langle w \rangle}, where \eqn{\langle w \rangle} is the
#'     average strength (the paper's Figure 1 pins strength, not degree).
#'     Uses edge weights; with unit weights it is VoteRank with a
#'     square-root score. Reproduces all sixty numbers of the paper's
#'     Figure 1.}
#'   \item{\code{enrenew} (Guo, Yang, Guo, Pan & Chen 2020)}{Entropy-based
#'     selection: \eqn{E_v = \sum_{u \in N(v)} -p_{uv} \ln p_{uv}} with
#'     \eqn{p_{uv} = k_u / \sum_{l \in N(v)} k_l}; after electing the
#'     largest \eqn{E}, every entropy term flowing outward to depth
#'     \eqn{d \le l} is scaled by \eqn{1 - 1 / (2^{d-1} \ln \langle k
#'     \rangle)}, with \eqn{l} = \code{enrenew_depth} (default 2).
#'     Reproduces the paper's Figure 1. The authors' released code differs
#'     from the paper in several ways; the paper is implemented. Note the
#'     factor turns negative when \eqn{\langle k \rangle < e}.}
#'   \item{\code{voterank_plus} (Liu, Li, Fang & Yao 2021)}{Initial ability
#'     \eqn{\ln(1 + k_i / k_{\max})}, degree-proportional vote shares over
#'     unelected neighbours, score \eqn{\sqrt{k_i \sum_j va_j w_{j \to i}}},
#'     and after an election abilities are multiplied by \eqn{\lambda}
#'     one step away and \eqn{\sqrt{\lambda}} two steps away
#'     (\code{voterank_lambda}, default 0.1). The article is closed access;
#'     the implementation matches the authors' released code exactly,
#'     including its exclusion of elected nodes from the vote-share
#'     denominator.}
#' }
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param enrenew_depth Renewal radius \eqn{l} for \code{enrenew}. Default 2.
#' @param voterank_lambda Suppression factor \eqn{\lambda} for
#'   \code{voterank_plus}. Default 0.1.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector in \eqn{(0, 1]}, one score per node.
#'
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
#'
#' @seealso \code{\link{centrality_voterank}},
#'   \code{\link{centrality_ncvoterank}}.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_wvoterank(adj)
#' centrality_enrenew(adj)
#' centrality_voterank_plus(adj)
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

#' Node Contraction Centrality (IMC and IIMC)
#'
#' Tan, Wu and Deng's (2006) node-contraction importance, as restated by
#' Wang et al. (2011). The agglomeration (cohesion) of a graph is
#' \eqn{\partial(G) = 1 / (N \bar{L})}, with \eqn{\bar{L}} the mean
#' shortest-path length over ordered pairs; contracting a node merges it
#' with all its neighbours into one node, and
#' \deqn{IMC(v) = 1 - \partial(G) / \partial(G_v).}
#' The improved form (\code{node_contraction_improved}) adds the same score
#' of the node's edges computed on the line graph:
#' \eqn{IIMC(v) = \alpha\, IMC(v) + \beta \sum_{e \ni v} IMC_{L(G)}(e)},
#' with \eqn{\alpha / \beta = 5} (\code{contraction_rho}) and
#' \eqn{\alpha + \beta = 1}, the normalisation that reproduces the paper's
#' Table 1. Higher = more important. Both reproduce Table 1 of Wang et al.
#' (2011). The Zoo entry describes the contracted graph as the graph with
#' the node removed; the sources define it by contraction, which is what
#' is implemented.
#'
#' On a disconnected graph the mean path length is taken over the
#' mutually reachable ordered pairs (a cograph choice; the sources assume
#' connected graphs). Direction, weights and loops are ignored. Cost is
#' one all-pairs computation per node, so \eqn{O(n^2 (n + m))}; the
#' improved form does the same on the line graph, \eqn{O(m^2 (m + m'))}.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param contraction_rho Ratio \eqn{\alpha / \beta} for the improved form.
#'   Default 5.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node.
#'
#' @references
#' Tan, Y.-J., Wu, J., & Deng, H.-Z. (2006). Evaluation method for node
#'   importance based on node contraction in complex networks. Systems
#'   Engineering: Theory & Practice, 26(11), 79-83.
#'
#' Wang, J., Li, C., & Xia, C. (2011). Improved centrality indicators to
#'   characterize the nodal spreading capability in complex networks.
#'   Procedia Engineering, 15, 3304-3308.
#'
#' @seealso \code{\link{centrality_closeness_vitality}}.
#'
#' @export
#' @examples
#' path5 <- matrix(0, 5, 5)
#' path5[cbind(1:4, 2:5)] <- 1; path5 <- path5 + t(path5)
#' rownames(path5) <- colnames(path5) <- LETTERS[1:5]
#' centrality_node_contraction(path5)
#' centrality_node_contraction_improved(path5)
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
#' Curado, Rodriguez, Tortosa and Vicent's (2022) counting measure. For
#' every unordered pair \eqn{(i, j)} the two-step transfer
#' \eqn{P_{itj} = w_{it} w_{tj} / (d_i d_j)} (zero when any two of the
#' three coincide) is combined into \eqn{T_{ij}[t, k] = P_{itj} P_{jki}},
#' the diagonal is dropped, and the single largest entry credits one count
#' to \eqn{t} and one to \eqn{k}. A node's score is its total count over
#' all pairs. Higher = more central; nodes never on a winning two-way
#' route score 0, so sparse tails are not ranked. Reproduces the paper's
#' toy example exactly, including every printed fraction.
#'
#' The paper's \eqn{P_{itj}} is not a random-walk probability (its
#' denominator is \eqn{d_i d_j}, not \eqn{d_i d_t}); it is implemented as
#' printed. Ties in the maximum go to the first entry in row-major order.
#' Edge weights are used; direction and loops are ignored. Cost is
#' \eqn{O(n^4)}: fine to a few hundred nodes, slow beyond.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of counts, one per node.
#'
#' @references Curado, M., Rodriguez, R., Tortosa, L., & Vicent, J. F.
#'   (2022). A new centrality measure in dense networks based on two-way
#'   random walk betweenness. Applied Mathematics and Computation, 412,
#'   126560.
#'
#' @seealso \code{\link{centrality_current_flow_betweenness}} for Newman's
#'   random-walk betweenness.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_two_way_rw(adj)
centrality_two_way_rw <- function(x, ...) {
  df <- centrality(x, measures = "two_way_rw", ...)
  stats::setNames(df$two_way_rw, df$node)
}

# ---------------------------------------------------------------------------
# Simple local measures, coreness variants, geodesic k-path
# ---------------------------------------------------------------------------

#' Neighbour matrix by mode with loops dropped
#' @keywords internal
#' @noRd
.cg_mode_neighbours <- function(g, mode) {
  a <- .cg_edge_indicator(.cg_path_matrix(g, NULL))
  switch(mode, all = pmax(a, t(a)), out = a, "in" = t(a))
}

#' @keywords internal
#' @noRd
calculate_heatmap <- function(g, mode = "all", hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_heatmap(.cg_mode_neighbours(g, mode),
              hop_mat %||% .cg_hop_distances(g, mode))
}

#' @keywords internal
#' @noRd
calculate_flow_coefficient <- function(g) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_flow_coefficient(.cg_path_matrix(g, NULL))
}

#' @keywords internal
#' @noRd
calculate_local_entropy <- function(g, mode = "all") {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  nb <- .cg_mode_neighbours(g, mode)
  .cg_local_entropy(nb, rowSums(nb))
}

#' @keywords internal
#' @noRd
calculate_weighted_h_index <- function(g, mode = "all") {
  if (igraph::vcount(g) == 0L) return(integer(0))
  nb <- .cg_mode_neighbours(g, mode)
  .cg_weighted_h_index(nb, rowSums(nb))
}

#' @keywords internal
#' @noRd
calculate_redundancy <- function(g) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_redundancy(.cg_mode_neighbours(g, "all"))
}

#' @keywords internal
#' @noRd
calculate_weighted_kshell <- function(g, weights = NULL, alpha = 1, beta = 1) {
  if (igraph::vcount(g) == 0L) return(integer(0))
  .cg_weighted_kshell(.cg_path_matrix(g, weights), alpha = alpha, beta = beta)
}

#' @keywords internal
#' @noRd
calculate_renewed_coreness <- function(g, threshold = 2) {
  if (igraph::vcount(g) == 0L) return(integer(0))
  .cg_renewed_coreness(.cg_mode_neighbours(g, "all"), threshold = threshold)
}

#' @keywords internal
#' @noRd
calculate_geodesic_kpath <- function(g, mode = "all", k = 3, hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_geodesic_kpath(.cg_mode_neighbours(g, mode),
                     hop_mat %||% .cg_hop_distances(g, mode), k = k)
}

#' Heatmap, Flow Coefficient, Local Entropy, Weighted h-index, Redundancy
#'
#' Five local measures.
#'
#' \describe{
#'   \item{\code{heatmap} (Duron 2020)}{Farness minus the mean farness of
#'     the neighbours, \eqn{C(v) = f(v) - \frac{1}{k_v} \sum_{u \in N(v)}
#'     f(u)}, with \eqn{f} the sum of hop distances to reachable nodes.
#'     **Lower is more central.** Isolates score \code{NaN}. Reproduces
#'     Table 1 of the paper.}
#'   \item{\code{flow_coefficient} (Honey et al. 2007)}{Among ordered pairs
#'     of distinct neighbours, the fraction joined by a two-step path
#'     through the node but not by a direct link, as implemented in the
#'     Brain Connectivity Toolbox. On an undirected graph it equals one
#'     minus the clustering coefficient; it carries new information only on
#'     directed graphs. Nodes with fewer than two neighbours score 0.}
#'   \item{\code{local_entropy} (Nie et al. 2016)}{\eqn{-\sum_{j \in N(i)}
#'     k_j \ln k_j}, as printed by the sources. Always non-positive and more
#'     negative for larger, denser neighbourhoods, so **lower is more
#'     central**; isolates score 0, the maximum. The original article is
#'     closed access; the formula is that of the Zoo and of Omar and
#'     Plapper's 2021 survey, which agree.}
#'   \item{\code{weighted_h_index} (Gao et al. 2019)}{h-index of the
#'     multiset in which each neighbour \eqn{j} contributes the topological
#'     weight \eqn{k_i k_j} repeated \eqn{k_j} times. Edge weights on the
#'     input play no role.}
#'   \item{\code{redundancy} (Burt 1992; Borgatti 1997)}{Mean degree of the
#'     node's neighbours within its ego network, \eqn{2 t_i / k_i}; equal to
#'     degree minus effective size. Higher = fewer structural holes.
#'     Reproduces Borgatti's worked example.}
#' }
#' \code{heatmap}, \code{local_entropy} and \code{weighted_h_index} follow
#' \code{mode}; the others ignore direction. Edge weights are ignored.
#'
#' @inheritParams centrality_distance_entropy
#'
#' @return Named numeric vector, one value per node.
#'
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
#'
#' @seealso \code{\link{centrality_effective_size}},
#'   \code{\link{centrality_transitivity}}.
#'
#' @export
#' @examples
#' star5 <- matrix(0, 5, 5)
#' star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
#' rownames(star5) <- colnames(star5) <- LETTERS[1:5]
#' centrality_heatmap(star5)
#' centrality_weighted_h_index(star5)
#' centrality_redundancy(star5)
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
#' \describe{
#'   \item{\code{weighted_kshell} (Garas, Schweitzer & Havlin 2012)}{k-shell
#'     decomposition on the generalised degree \eqn{k' = (k^\alpha
#'     s^\beta)^{1 / (\alpha + \beta)}} (\code{wks_alpha}, \code{wks_beta},
#'     both 1), after the paper's weight normalisation (divide by the mean,
#'     then by the minimum, round to the nearest integer). Integer
#'     thresholds label the shells, so unit weights give the k-core number
#'     and isolates score 0. Reproduces the paper's Figure 1 example and its
#'     Table 2 core size on the netscience network. Uses edge weights.}
#'   \item{\code{renewed_coreness} (Liu, Tang, Zhou & Do 2015)}{Each link
#'     gets the diffusion importance \eqn{D_{ij} = (|N(j) \setminus N[i]| +
#'     |N(i) \setminus N[j]|) / 2}; links below \code{renewed_threshold}
#'     (paper: 2) are removed and the k-core number of the residual graph is
#'     the renewed coreness. A clique with no outside links collapses to 0.
#'     Reproduces the paper's Figure 1 and all twelve percentages of its
#'     supplementary Table S1; the Zoo's transcription with open
#'     neighbourhoods is off by one.}
#'   \item{\code{geodesic_kpath} (Borgatti & Everett 2006)}{The number of
#'     shortest paths of length at most \code{kpath_k} (default 3) that
#'     start at the node, counted with multiplicity. Note that
#'     \code{centiserve::geokpath} counts nodes within \eqn{k} instead,
#'     which is the paper's vertex-disjoint variant and equals m-reach.}
#' }
#' \code{geodesic_kpath} follows \code{mode}; the other two ignore
#' direction.
#'
#' @inheritParams centrality_distance_entropy
#' @param wks_alpha,wks_beta Exponents of degree and strength in the
#'   weighted k-shell. Default 1 and 1.
#' @param renewed_threshold Diffusion-importance threshold. Default 2.
#' @param kpath_k Maximum path length. Default 3.
#'
#' @return Named numeric vector, one value per node.
#'
#' @references
#' Garas, A., Schweitzer, F., & Havlin, S. (2012). A k-shell decomposition
#'   method for weighted networks. New Journal of Physics, 14, 083030.
#'
#' Liu, Y., Tang, M., Zhou, T., & Do, Y. (2015). Improving the accuracy of
#'   the k-shell method by removing redundant links. Scientific Reports, 5,
#'   13172.
#'
#' Borgatti, S. P., & Everett, M. G. (2006). A graph-theoretic perspective
#'   on centrality. Social Networks, 28(4), 466-484.
#'
#' @seealso \code{\link{centrality_coreness}}, \code{\link{centrality_s_shell}},
#'   \code{\link{centrality_kreach}}.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_weighted_kshell(adj)
#' centrality_renewed_coreness(adj)
#' centrality_geodesic_kpath(adj, kpath_k = 2)
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
  c("infection", "two_way_rw", "node_contraction_improved",
    "entropy_variation_betweenness", "fragmentation", "epc")
}
