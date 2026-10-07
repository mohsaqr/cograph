# ===========================================================================
# Batch 8 — Centrality Zoo "on the way" batch
#
# igraph-facing calculators (thin glue over the base-R kernels in
# R/kernels-batch8.R) and the exported one-measure verbs.
# ===========================================================================

# ---------------------------------------------------------------------------
# Shapley value games (Michalak et al. 2013)
# ---------------------------------------------------------------------------

#' Shapley game calculator
#' @keywords internal
#' @noRd
calculate_shapley <- function(cg, game = 1L, k = 2, cutoff = 2,
                              hop_mat = NULL) {
  if (cg$n == 0L) return(numeric(0))
  b <- .cg_path_matrix(cg, NULL)
  d <- if (game == 3L) hop_mat %||% .cg_distances(b, "out") else NULL
  .cg_shapley(b, game = game, k = k, cutoff = cutoff, d = d)
}

#' Shapley Value Centrality
#'
#' Shapley value centrality (Michalak et al. 2013) is the Shapley value of
#' each node in a coalition game whose worth \eqn{v(C)} counts the nodes a
#' coalition \eqn{C} covers. Each game has an exact closed form. In game 1 a
#' coalition covers its members and their neighbors, and
#' \deqn{SV(v) = \sum_{u \in \{v\} \cup N(v)} \frac{1}{1 + k_u}.}{
#'   SV(v) = sum_{u in {v} + N(v)} 1 / (1 + k_u).}
#' In game 2 a coalition covers its members and the nodes with at least
#' \eqn{k} neighbors in it. In game 3 it covers the nodes within
#' \code{shapley_cutoff} hops of it, and \eqn{N(v)} is replaced by the set
#' of nodes within that distance.
#'
#' @details
#' The values of every game sum to the number of nodes. Game 2 with
#' \eqn{k = 1} and game 3 with cutoff 1 equal game 1. Degrees exclude
#' self-loops, and edge weights are ignored. On a directed network coverage
#' runs along out-edges and the denominators use in-degrees, the extension
#' the source states. Distances in game 3 are hop counts.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param shapley_k Neighbor threshold \eqn{k} for game 2 (default 2).
#' @param shapley_cutoff Hop cutoff for game 3 (default 2).
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one Shapley value per node, in input
#'   node order.
#' @references Michalak, T. P., Aadithya, K. V., Szczepanski, P. L.,
#'   Ravindran, B., & Jennings, N. R. (2013). Efficient computation of the
#'   Shapley value for game-theoretic network centrality. Journal of
#'   Artificial Intelligence Research, 46, 607-650.
#' @seealso \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_shapley_game1(regulation_net)
#' centrality_shapley_game2(regulation_net)
#' centrality_shapley_game3(regulation_net)
centrality_shapley_game1 <- function(x, ...) {
  df <- centrality(x, measures = "shapley_game1", ...)
  stats::setNames(df$shapley_game1, df$node)
}

#' @rdname centrality_shapley_game1
#' @export
centrality_shapley_game2 <- function(x, shapley_k = 2, ...) {
  df <- centrality(x, measures = "shapley_game2", shapley_k = shapley_k, ...)
  stats::setNames(df$shapley_game2, df$node)
}

#' @rdname centrality_shapley_game1
#' @export
centrality_shapley_game3 <- function(x, shapley_cutoff = 2, ...) {
  df <- centrality(x, measures = "shapley_game3",
                   shapley_cutoff = shapley_cutoff, ...)
  stats::setNames(df$shapley_game3, df$node)
}

# ---------------------------------------------------------------------------
# Access and hide information (Rosvall et al. 2005; Sneppen et al. 2005)
# ---------------------------------------------------------------------------

#' Search information calculators
#' @keywords internal
#' @noRd
calculate_search_information <- function(cg, what = c("access", "hide"),
                                         hop_mat = NULL) {
  what <- match.arg(what)
  if (cg$n == 0L) return(numeric(0))
  b <- .cg_path_matrix(cg, NULL)
  d <- hop_mat %||% .cg_distances(b, "out")
  s_mat <- .cg_search_information(b, d, directed = cg$directed)
  if (what == "access") {
    .cg_access_information(s_mat)
  } else {
    .cg_hide_information(s_mat)
  }
}

#' Access and Hide Information
#'
#' Access and hide information (Rosvall et al. 2005; Sneppen, Trusina and
#' Rosvall 2005) measure the number of bits a walker needs to follow a
#' shortest path without a map. The search information from \eqn{i} to
#' \eqn{j} sums over all shortest paths \eqn{p(i, j)}, with \eqn{k_i} the
#' degree of the source and \eqn{k_l - 1} the choices left at each
#' intermediate node:
#' \deqn{S(i \to j) = -\log_2 \sum_{p(i, j)} \frac{1}{k_i}
#'   \prod_{l \in p,\, l \ne i, j} \frac{1}{k_l - 1}.}{
#'   S(i -> j) = -log2 sum_{p(i, j)} (1 / k_i)
#'   prod_{l in p, l != i, j} 1 / (k_l - 1).}
#' Access information \eqn{A_i}{A_i} averages \eqn{S(i \to j)}{S(i -> j)}
#' over targets \eqn{j}, and hide information \eqn{H_i}{H_i} averages
#' \eqn{S(j \to i)}{S(j -> i)} over sources.
#'
#' @details
#' Distances are hop counts, so edge weights are ignored. On a directed
#' network every step uses the out-degree. The average runs over the nodes
#' a walker can reach, or be reached from, so values stay finite on a
#' disconnected network and equal the source's \eqn{1/N}{1/N} average on a
#' connected one. Hubs score high on access information and low on hide
#' information. On a star with five leaves the hub has access information
#' 1.93 bits and hide information 0, and each leaf has access information
#' 1.33 bits. The Centrality Zoo states the star case in reverse, and the
#' values above follow the formulas and the source papers.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, in bits.
#' @references
#' Rosvall, M., Trusina, A., Minnhagen, P., & Sneppen, K. (2005). Networks
#'   and cities: An information perspective. Physical Review Letters, 94,
#'   028701.
#'
#' Sneppen, K., Trusina, A., & Rosvall, M. (2005). Hide-and-seek on complex
#'   networks. Europhysics Letters, 69(5), 853-859.
#' @seealso \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_access_information(regulation_net)
#' centrality_hide_information(regulation_net)
centrality_access_information <- function(x, ...) {
  df <- centrality(x, measures = "access_information", ...)
  stats::setNames(df$access_information, df$node)
}

#' @rdname centrality_access_information
#' @export
centrality_hide_information <- function(x, ...) {
  df <- centrality(x, measures = "hide_information", ...)
  stats::setNames(df$hide_information, df$node)
}

# ---------------------------------------------------------------------------
# Rumor centrality (Shah & Zaman 2010, 2011)
# ---------------------------------------------------------------------------

#' Rumor centrality calculator
#' @keywords internal
#' @noRd
calculate_rumor <- function(cg, hop_mat = NULL) {
  if (cg$n == 0L) return(numeric(0))
  b <- .cg_path_matrix(cg, NULL)
  # The paper's setting is undirected; a directed input is read ignoring
  # direction, matching igraph's BFS with mode = "all".
  b <- pmax(b, t(b))
  d <- hop_mat %||% .cg_distances(b, "all")
  .cg_rumor(b, d)
}

#' Rumor Centrality
#'
#' Rumor centrality (Shah and Zaman 2010, 2011) is the maximum-likelihood
#' score for the source of a rumor that has spread to every node under the
#' susceptible-infected model. On a tree rooted at \eqn{v} it counts the
#' spreading orders that start at \eqn{v},
#' \deqn{R(v) = \frac{N!}{\prod_u T^v_u},}{R(v) = N! / prod_u T^v_u,}
#' where \eqn{T^v_u}{T^v_u} is the size of the subtree rooted at \eqn{u}. On
#' a general graph \eqn{R} is evaluated on the breadth-first tree rooted at
#' each node.
#'
#' @details
#' The value is returned as \eqn{\log R(v)}{log R(v)} with the natural
#' logarithm, because \eqn{N!} overflows beyond 170 nodes. \eqn{N} is the
#' size of the component of the node, so a disconnected network is scored
#' component by component and an isolated node scores 0. The breadth-first
#' tree attaches each node to the earliest discovered node of the previous
#' layer, scanning neighbors in node order. Direction, edge weights and
#' self-loops are ignored. Higher values mark more plausible origins.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, holding \eqn{\log R}{log R}.
#' @references
#' Shah, D., & Zaman, T. (2010). Detecting sources of computer viruses in
#'   networks: theory and experiment. ACM SIGMETRICS, 203-214.
#'
#' Shah, D., & Zaman, T. (2011). Rumors in a network: Who's the culprit?
#'   IEEE Transactions on Information Theory, 57(8), 5163-5181.
#' @seealso \code{\link{centrality_closeness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_rumor(regulation_net)
centrality_rumor <- function(x, ...) {
  df <- centrality(x, measures = "rumor", ...)
  stats::setNames(df$rumor, df$node)
}

# ---------------------------------------------------------------------------
# Community hub-bridge (Ghalmane, El Hassouni & Cherifi 2019)
# ---------------------------------------------------------------------------

#' Community hub-bridge calculator
#' @keywords internal
#' @noRd
calculate_community_hub_bridge <- function(cg, membership = NULL,
                                           mode = "all") {
  n <- cg$n
  if (n == 0L) return(numeric(0))
  if (is.null(membership)) {
    .cg_warn_no_membership("community_hub_bridge")
    return(rep(NA_real_, n))
  }
  if (length(membership) != n || anyNA(membership)) {
    msg <- sprintf("`membership` needs one non-missing label per node (%d), %s",
                   n, sprintf("got length %d", length(membership)))
    stop(errorCondition(msg, class = "cograph_bad_membership", call = NULL))
  }
  b <- .cg_path_matrix(cg, NULL)
  nb <- switch(mode,
               all = (b + t(b)) != 0,
               out = b != 0,
               "in" = t(b) != 0)
  nb <- nb & (row(nb) != col(nb))
  storage.mode(nb) <- "numeric"
  .cg_community_hub_bridge(nb, membership)
}

#' Community Hub-Bridge Centrality
#'
#' Community hub-bridge centrality (Ghalmane, El Hassouni and Cherifi 2019)
#' scores nodes that are hubs inside their community and bridges between
#' communities:
#' \deqn{CHB(i) = |C_i|\, k^{intra}_i + NNC_i\, k^{inter}_i,}{
#'   CHB(i) = |C_i| k_intra(i) + NNC_i k_inter(i),}
#' where \eqn{|C_i|}{|C_i|} is the size of the community of \eqn{i},
#' \eqn{k^{intra}_i}{k_intra(i)} and \eqn{k^{inter}_i}{k_inter(i)} its
#' numbers of links inside and outside that community, and
#' \eqn{NNC_i}{NNC_i} the number of other communities it links to.
#'
#' @details
#' Edge weights and self-loops are ignored. Under \code{mode = "out"} or
#' \code{mode = "in"} only out-links or in-links count, and the default
#' ignores direction. This is the raw form of the original article. Later
#' work by the same group uses a normalized variant with the same name.
#' Without \code{membership} the function raises a warning of classes \code{cograph_bad_membership} and
#' \code{cograph_undefined_measure}
#' and returns \code{NA} for every node. A \code{membership} that is not one
#' non-missing label per node raises an error of class
#' \code{cograph_bad_membership}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param membership Community labels, one per node, for example from
#'   \code{\link{detect_communities}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"}
#'   or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Ghalmane, Z., El Hassouni, M., & Cherifi, H. (2019).
#'   Immunization of networks with non-overlapping community structure.
#'   Social Network Analysis and Mining, 9, 45.
#' @seealso \code{\link{centrality_community_based}},
#'   \code{\link{centrality_modularity_vitality}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_community_hub_bridge(regulation_net,
#'                                 membership = rep(1:2, each = 5))
centrality_community_hub_bridge <- function(x, membership = NULL,
                                            mode = "all", ...) {
  df <- centrality(x, measures = "community_hub_bridge", mode = mode,
                   membership = membership, ...)
  stats::setNames(df[[paste0("community_hub_bridge_", mode)]], df$node)
}

# ---------------------------------------------------------------------------
# Entropy variation (Ai 2017)
# ---------------------------------------------------------------------------

#' Entropy variation calculators
#' @keywords internal
#' @noRd
calculate_entropy_variation <- function(cg, of = c("degree", "betweenness"),
                                        mode = "all") {
  of <- match.arg(of)
  n <- cg$n
  if (n == 0L) return(numeric(0))
  if (of == "degree") {
    b <- .cg_path_matrix(cg, NULL)
    # An undirected graph has one degree; "all" reads it (as 2k, which the
    # normalization in the entropy cancels).
    if (!cg$directed) mode <- "all"
    return(.cg_entropy_variation_degree(b, mode))
  }
  # Betweenness is recomputed on each deletion graph, as in the author's
  # code; the paper's networks are unweighted, so weights are ignored.
  b <- .cg_mode_weights(.cg_path_matrix(cg, NULL),
                        if (cg$directed) "out" else "all")
  f <- .cg_betweenness(b, n, cg$directed)
  .cg_entropy_variation(f, function(i) {
    .cg_betweenness(b[-i, -i, drop = FALSE], n - 1L, cg$directed)
  })
}

#' Entropy Variation
#'
#' Entropy variation (Ai 2017) is the change in the Shannon entropy of a
#' node-level distribution \eqn{f} when a node and its links are removed:
#' \deqn{EnV_f(i) = I_f(G) - I_f(G - i), \qquad
#'   I_f(G) = -\sum_j p_j \ln p_j, \quad p_j = \frac{f(j)}{\sum_l f(l)}.}{
#'   EnV_f(i) = I_f(G) - I_f(G - i), I_f(G) = -sum_j p_j ln p_j,
#'   p_j = f(j) / sum_l f(l).}
#' The distribution \eqn{f} is the degree or the betweenness. Higher values
#' mark more important nodes.
#'
#' @details
#' The entropy uses the natural logarithm, as in the code of the author, so
#' scores are in nats. The difference is signed, and a negative value means
#' that removing the node evens out the distribution. Edge weights are
#' ignored in both variants. For the degree variant \code{mode} selects the
#' in-, out- or total degree on a directed network, and self-loops count
#' toward the degree, and \code{loops = FALSE} drops them. The betweenness
#' variant ignores \code{mode} and self-loops. A deletion that leaves every
#' value of \eqn{f} at zero, such as betweenness on a clique, has entropy 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param of Distribution: \code{"degree"} (default) or
#'   \code{"betweenness"}.
#' @param mode For the degree variant on directed networks: \code{"all"}
#'   (default, in plus out), \code{"out"} or \code{"in"}.
#' @param ... Further arguments to \code{\link{centrality}}. The degree
#'   variant uses \code{loops} (keep self-loops, default \code{TRUE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Ai, X. (2017). Node importance ranking of complex networks
#'   with entropy variation. Entropy, 19(7), 303.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_distance_entropy}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_entropy_variation(regulation_net)
centrality_entropy_variation <- function(x, of = c("degree", "betweenness"),
                                         mode = "all", ...) {
  of <- match.arg(of)
  measure <- paste0("entropy_variation_", of)
  df <- centrality(x, measures = measure, mode = mode, ...)
  col <- if (of == "degree") paste0(measure, "_", mode) else measure
  stats::setNames(df[[col]], df$node)
}

# ---------------------------------------------------------------------------
# s-shell index (Liu, Tang, Do & Hui 2017)
# ---------------------------------------------------------------------------

#' s-shell calculator
#' @keywords internal
#' @noRd
calculate_s_shell <- function(cg, a = 0.5) {
  if (!is.numeric(a) || length(a) != 1L || !is.finite(a) || a < 0) {
    stop(errorCondition(
      "`s_shell_a` must be a single non-negative number: it is the exponent on the link strengths",
      class = "cograph_bad_parameter", call = NULL))
  }
  if (cg$n == 0L) return(integer(0))
  .cg_s_shell(.cg_path_matrix(cg, NULL), a = a)
}

#' s-shell Index
#'
#' The s-shell index (Liu, Tang, Do and Hui 2017) peels the network like the
#' k-shell decomposition but by a strength computed from the topology.
#' Each link gets the asymmetric weight
#' \deqn{w_{ij} = 1 + (k_i\, k^{out}_j)^a,}{w_ij = 1 + (k_i k_out(j))^a,}
#' where \eqn{k^{out}_j}{k_out(j)} counts the neighbors of \eqn{j} outside
#' the closed neighborhood of \eqn{i}, and the strength of \eqn{i} is
#' \eqn{s_i = \sum_{j \in N(i)} w_{ij}}{s_i = sum_{j in N(i)} w_ij}. At each
#' step the nodes at the minimum remaining strength are removed, the
#' removals cascade, and the removed nodes receive the next shell index.
#'
#' @details
#' Direction, edge weights and self-loops are ignored. The index is an
#' ordinal counter starting at 1 for the outermost shell, so values are
#' comparable within a network only. Higher indices mark more central
#' nodes. Isolated nodes form shell 1 on their own and shift every other
#' shell up by one. With \eqn{a = 0} the shells are the dense ranks of the
#' k-core numbers. A \code{s_shell_a} that is not a single non-negative
#' number raises an error of class \code{cograph_bad_parameter}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param s_shell_a Exponent \eqn{a} of the link weights (default 0.5, the
#'   value the source recommends).
#' @param ... Further arguments to \code{\link{centrality}}.
#' @return A named integer vector with one shell index per node, in input
#'   node order.
#' @references Liu, Y., Tang, M., Do, Y., & Hui, P. M. (2017). Accurate
#'   ranking of influential spreaders in networks based on dynamically
#'   asymmetric link weights. Physical Review E, 96(2), 022323.
#' @seealso \code{\link{centrality_coreness}},
#'   \code{\link{centrality_weighted_kshell}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_s_shell(regulation_net)
centrality_s_shell <- function(x, s_shell_a = 0.5, ...) {
  df <- centrality(x, measures = "s_shell", s_shell_a = s_shell_a, ...)
  stats::setNames(df$s_shell, df$node)
}

# ---------------------------------------------------------------------------
# DegreeDiscountIC, SingleDiscount (Chen, Wang & Yang 2009), NCVoteRank
# (Kumar & Panda 2020)
# ---------------------------------------------------------------------------

#' Greedy seed-selection calculators
#' @keywords internal
#' @noRd
calculate_degree_discount <- function(cg, p = 0.01, single = FALSE) {
  if (cg$n == 0L) return(numeric(0))
  .cg_degree_discount(.cg_path_matrix(cg, NULL), p = p, single = single)
}

#' @keywords internal
#' @noRd
calculate_ncvoterank <- function(cg, theta = 0.5) {
  if (cg$n == 0L) return(numeric(0))
  # The paper's setting is an undirected simple graph: direction and loops
  # are dropped before the k-shell decomposition and the voting.
  nb <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  .cg_ncvoterank(nb, ks = .cg_coreness(nb, cg$n, directed = FALSE),
                 theta = theta)
}

#' DegreeDiscountIC and SingleDiscount
#'
#' DegreeDiscountIC and SingleDiscount (Chen, Wang and Yang 2009) select
#' spreaders for the independent-cascade model one at a time by the largest
#' discounted degree. After each selection every unselected neighbor
#' \eqn{v} of the new seed gains one selected neighbor \eqn{t_v}{t_v}, and
#' its DegreeDiscountIC degree becomes
#' \deqn{dd_v = d_v - 2 t_v - (d_v - t_v)\, t_v\, p,}{
#'   dd_v = d_v - 2 t_v - (d_v - t_v) t_v p,}
#' with propagation probability \eqn{p}. SingleDiscount uses
#' \eqn{d_v - t_v}{d_v - t_v}.
#'
#' @details
#' Every node is placed, and the selection order is returned as a score.
#' The first node selected scores 1 and the last \eqn{1/n}{1/n}, so scores
#' lie in \eqn{(0, 1]}{(0, 1]}. Ties are broken by node order, which the
#' source leaves open. Direction, edge weights and self-loops are ignored.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param discount_p Propagation probability \eqn{p} for DegreeDiscountIC
#'   (default 0.01).
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Chen, W., Wang, Y., & Yang, S. (2009). Efficient influence
#'   maximization in social networks. Proceedings of the 15th ACM SIGKDD
#'   International Conference on Knowledge Discovery and Data Mining,
#'   199-208.
#' @seealso \code{\link{centrality_voterank}},
#'   \code{\link{centrality_ncvoterank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_degree_discount(regulation_net)
#' centrality_single_discount(regulation_net)
centrality_degree_discount <- function(x, discount_p = 0.01, ...) {
  df <- centrality(x, measures = "degree_discount", discount_p = discount_p,
                   ...)
  stats::setNames(df$degree_discount, df$node)
}

#' @rdname centrality_degree_discount
#' @export
centrality_single_discount <- function(x, ...) {
  df <- centrality(x, measures = "single_discount", ...)
  stats::setNames(df$single_discount, df$node)
}

#' NCVoteRank
#'
#' NCVoteRank (Kumar and Panda 2020) is VoteRank (Zhang et al. 2016) with
#' the voting ability of each voter weighted by its neighborhood coreness.
#' A node collects the score
#' \deqn{s_u = \sum_{v \in N(u)} va_v \,[\theta + (1 - \theta)\, nc_v], \qquad
#'   nc_v = \frac{\sum_{w \in N(v)} ks(w)}{\max_j \sum_{w \in N(j)} ks(w)},}{
#'   s_u = sum_{v in N(u)} va_v [theta + (1 - theta) nc_v],
#'   nc_v = sum_{w in N(v)} ks(w) / max_j sum_{w in N(j)} ks(w),}
#' with \eqn{ks} the k-shell index. The top scorer is elected, its ability
#' drops to 0, its neighbors lose \eqn{1/\langle k \rangle}{1/<k>} and the
#' nodes two steps away lose \eqn{1/(2\langle k \rangle)}{1/(2<k>)}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights and loops are ignored. Elections continue
#' until every node is placed. The first elected scores 1 and the last
#' \eqn{1/n}{1/n}, so scores lie in \eqn{(0, 1]}{(0, 1]}. The definition
#' follows the Centrality Zoo and three independent restatements of the
#' article, and the scaling of the coreness by its maximum follows Yu et
#' al. (2020). With \eqn{\theta = 1}{theta = 1} and no two-step weakening the
#' procedure is VoteRank.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ncvote_theta Weight \eqn{\theta}{theta} of the plain vote
#'   (default 0.5).
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Kumar, S., & Panda, B. S. (2020). Identifying influential nodes in
#'   social networks: Neighborhood coreness based voting approach.
#'   Physica A, 553, 124215.
#'
#' Zhang, J.-X., Chen, D.-B., Dong, Q., & Zhao, Z.-D. (2016). Identifying
#'   a set of influential spreaders in complex networks. Scientific
#'   Reports, 6, 27823.
#' @seealso \code{\link{centrality_voterank}},
#'   \code{\link{centrality_wvoterank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ncvoterank(regulation_net)
centrality_ncvoterank <- function(x, ncvote_theta = 0.5, ...) {
  df <- centrality(x, measures = "ncvoterank", ncvote_theta = ncvote_theta,
                   ...)
  stats::setNames(df$ncvoterank, df$node)
}
