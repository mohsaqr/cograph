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
calculate_shapley <- function(g, game = 1L, k = 2, cutoff = 2,
                              hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  b <- .cg_path_matrix(g, NULL)
  d <- if (game == 3L) hop_mat %||% .cg_distances(b, "out") else NULL
  .cg_shapley(b, game = game, k = k, cutoff = cutoff, d = d)
}

#' Shapley Value Centrality (Games 1, 2 and 3)
#'
#' Game-theoretic centrality of Michalak, Aadithya, Szczepanski, Ravindran
#' and Jennings (2013): the Shapley value of each node in a coalition game
#' whose worth \eqn{v(C)} is the number of nodes a coalition \eqn{C}
#' "covers". Each game has a closed form, so the values are exact and cost
#' linear time.
#'
#' \describe{
#'   \item{Game 1 (\code{shapley_game1})}{\eqn{v(C)} = nodes in \eqn{C} or
#'     adjacent to it. \eqn{SV(v) = \sum_{u \in \{v\} \cup N(v)}
#'     1 / (1 + k_u)}.}
#'   \item{Game 2 (\code{shapley_game2})}{\eqn{v(C)} = nodes in \eqn{C} or
#'     with at least \eqn{k} neighbours in \eqn{C}.
#'     \eqn{SV(v) = \min(1, k / (1 + k_v)) + \sum_{u \in N(v)}
#'     \max(0, (k_u - k + 1) / (k_u (1 + k_u)))}. With \eqn{k = 1} this is
#'     game 1. Threshold via \code{shapley_k} (default 2).}
#'   \item{Game 3 (\code{shapley_game3})}{\eqn{v(C)} = nodes within
#'     \code{shapley_cutoff} hops of \eqn{C} (default 2).
#'     \eqn{SV(v) = \sum_{u \in \{v\} \cup N_d(v)} 1 / (1 + |N_d(u)|)},
#'     where \eqn{N_d(u)} is the set of nodes within \eqn{d} hops of
#'     \eqn{u}. With cutoff 1 this is game 1.}
#' }
#'
#' Values in every game sum to the number of nodes (efficiency). Higher
#' values mark nodes whose presence adds more coverage to a typical
#' coalition. Degrees exclude self-loops, as in the paper. On a directed
#' graph the coverage runs along out-edges and the denominators use
#' in-degrees (the paper's stated extension); distances for game 3 are hop
#' counts, so edge weights are ignored.
#'
#' Validated against exact Shapley values obtained by enumerating every
#' coalition on random graphs of up to eight nodes, including graphs with
#' isolates, self-loops and several components.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param shapley_k Neighbour threshold \eqn{k} for game 2. Default 2.
#' @param shapley_cutoff Hop cutoff for game 3. Default 2.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one Shapley value per node.
#'
#' @references Michalak, T. P., Aadithya, K. V., Szczepanski, P. L.,
#'   Ravindran, B., & Jennings, N. R. (2013). Efficient computation of the
#'   Shapley value for game-theoretic network centrality. Journal of
#'   Artificial Intelligence Research, 46, 607-650.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' star5 <- matrix(0, 5, 5)
#' star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
#' rownames(star5) <- colnames(star5) <- LETTERS[1:5]
#' centrality_shapley_game1(star5)
#' centrality_shapley_game2(star5, shapley_k = 2)
#' centrality_shapley_game3(star5, shapley_cutoff = 1)
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
calculate_search_information <- function(g, what = c("access", "hide"),
                                         hop_mat = NULL) {
  what <- match.arg(what)
  if (igraph::vcount(g) == 0L) return(numeric(0))
  b <- .cg_path_matrix(g, NULL)
  d <- hop_mat %||% .cg_distances(b, "out")
  s_mat <- .cg_search_information(b, d, directed = igraph::is_directed(g))
  if (what == "access") {
    .cg_access_information(s_mat)
  } else {
    .cg_hide_information(s_mat)
  }
}

#' Access and Hide Information
#'
#' Search-information centralities of Rosvall, Trusina, Minnhagen and
#' Sneppen (2005) and Sneppen, Trusina and Rosvall (2005). A walker who
#' knows only the shortest paths from \eqn{i} to \eqn{j} but has no map
#' must be told which link to take at each step; the number of bits needed
#' is
#' \deqn{S(i \to j) = -\log_2 \sum_{p \in \{p(i, j)\}} \frac{1}{k_i}
#'   \prod_{l \in p,\, l \ne i, j} \frac{1}{k_l - 1},}
#' summed over all shortest paths, with \eqn{k_i} the degree of the source
#' and \eqn{k_l - 1} the choices left at each intermediate node (the link
#' the walker arrived on is excluded). Then
#' \deqn{A_i = \frac{1}{N} \sum_j S(i \to j), \qquad
#'   H_i = \frac{1}{N} \sum_j S(j \to i),}
#' with \eqn{S(i \to i) = 0}.
#'
#' **Access information** \eqn{A_i}: how many bits it costs, on average,
#' to reach the rest of the network from \eqn{i}. A low value means the
#' node reaches others with few decisions. Hubs score *high*: a walker
#' leaving a hub has many links to choose from (on a star with five leaves
#' the hub scores 1.93 bits, a leaf 1.33). **Hide information**
#' \eqn{H_i}: how many bits it costs the rest of the network to find
#' \eqn{i}. High values mark hidden, peripheral nodes; hubs score low (the
#' star hub scores 0). The encyclopedia's prose states the star case the
#' other way round; the formulas and the source papers give the values
#' above.
#'
#' On a directed graph every step uses the out-degree, \eqn{1 / k^{out}}.
#' On a disconnected graph the average runs over the nodes a walker can
#' actually reach (or be reached from), so values stay finite; on a
#' connected graph this is exactly the paper's \eqn{1 / N}. Distances are
#' hop counts; edge weights are ignored. Cost is
#' \eqn{O(N (N + M))} with an \eqn{N \times N} matrix in memory.
#'
#' Validated against an independent enumeration of all shortest paths and
#' against the worked values in both papers (star and complete bipartite
#' graphs).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node, in bits.
#'
#' @references
#' Rosvall, M., Trusina, A., Minnhagen, P., & Sneppen, K. (2005). Networks
#'   and cities: An information perspective. Physical Review Letters, 94,
#'   028701.
#'
#' Sneppen, K., Trusina, A., & Rosvall, M. (2005). Hide-and-seek on complex
#'   networks. Europhysics Letters, 69(5), 853-859.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' star5 <- matrix(0, 5, 5)
#' star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
#' rownames(star5) <- colnames(star5) <- LETTERS[1:5]
#' centrality_access_information(star5)
#' centrality_hide_information(star5)
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
calculate_rumor <- function(g, hop_mat = NULL) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  b <- .cg_path_matrix(g, NULL)
  # The paper's setting is undirected; a directed input is read ignoring
  # direction, matching igraph's BFS with mode = "all".
  b <- pmax(b, t(b))
  d <- hop_mat %||% .cg_distances(b, "all")
  .cg_rumor(b, d)
}

#' Rumor Centrality
#'
#' Shah and Zaman's (2010, 2011) maximum-likelihood score for the source of
#' a rumor that has spread under the susceptible-infected model to every
#' node. On a tree,
#' \deqn{R(v) = \frac{N!}{\prod_{u} T^v_u},}
#' where \eqn{T^v_u} is the number of nodes in the subtree rooted at
#' \eqn{u} when the tree is rooted at \eqn{v}: the number of spreading
#' orders that could have started at \eqn{v}. On a general graph the paper
#' evaluates \eqn{R} on the breadth-first tree rooted at each node (its
#' eq. 24). Higher values mark nodes that are more plausible origins, which
#' in practice are nodes near the centre of the network.
#'
#' The value is returned as \eqn{\log R(v)} (natural log) because
#' \eqn{N!} overflows beyond 170 nodes; rankings and differences are
#' unchanged. \eqn{N} is the size of the node's component, so a
#' disconnected graph is scored component by component and an isolate
#' scores 0. The breadth-first tree attaches each node to the earliest
#' discovered node of the previous layer, scanning neighbours in label
#' order; the paper does not fix a tie rule, and this one reproduces its
#' Figure 3. Direction and edge weights are ignored.
#'
#' Validated on trees against a brute-force count of spreading orders and
#' against the worked examples in the paper.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, \eqn{\log R} per node.
#'
#' @references
#' Shah, D., & Zaman, T. (2010). Detecting sources of computer viruses in
#'   networks: theory and experiment. ACM SIGMETRICS, 203-214.
#'
#' Shah, D., & Zaman, T. (2011). Rumors in a network: Who's the culprit?
#'   IEEE Transactions on Information Theory, 57(8), 5163-5181.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' path5 <- matrix(0, 5, 5)
#' path5[cbind(1:4, 2:5)] <- 1; path5 <- path5 + t(path5)
#' rownames(path5) <- colnames(path5) <- LETTERS[1:5]
#' exp(centrality_rumor(path5))   # spreading orders from each node
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
calculate_community_hub_bridge <- function(g, membership = NULL,
                                           mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0L) return(numeric(0))
  if (is.null(membership)) {
    warning("community_hub_bridge requires membership; returning NA",
            call. = FALSE)
    return(rep(NA_real_, n))
  }
  if (length(membership) != n || anyNA(membership)) {
    msg <- sprintf("`membership` needs one non-missing label per node (%d), %s",
                   n, sprintf("got length %d", length(membership)))
    stop(errorCondition(msg, class = "cograph_bad_membership", call = NULL))
  }
  b <- .cg_path_matrix(g, NULL)
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
#' Ghalmane, El Hassouni and Cherifi's (2019) score for nodes that are both
#' hubs inside their community and bridges between communities:
#' \deqn{CHB(i) = |C_i| \, k^{intra}_i + NNC_i \, k^{inter}_i,}
#' where \eqn{|C_i|} is the number of nodes in \eqn{i}'s own community,
#' \eqn{k^{intra}_i} and \eqn{k^{inter}_i} its numbers of links inside and
#' outside that community, and \eqn{NNC_i} the number of *other*
#' communities it is linked to (eqs. 2 to 4 of the paper). Higher values
#' mark nodes whose removal both fragments their community and cuts links
#' between communities. A normalised variant with the same name exists in
#' later work by the same group; this is the original raw form.
#'
#' Under \code{mode = "out"} or \code{"in"} only out- or in-links count;
#' the default ignores direction. Edge weights are ignored.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param membership Community labels, one per node. Required; without it
#'   the function warns and returns \code{NA}. Obtain one from
#'   \code{\link{detect_communities}}.
#' @param mode For directed networks: \code{"all"} (default), \code{"out"},
#'   or \code{"in"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node.
#'
#' @section Conditions:
#' Raises an error of class \code{cograph_bad_membership} when
#' \code{membership} is not one non-missing label per node.
#'
#' @references Ghalmane, Z., El Hassouni, M., & Cherifi, H. (2019).
#'   Immunization of networks with non-overlapping community structure.
#'   Social Network Analysis and Mining, 9, 45.
#'
#' @seealso \code{\link{centrality_modularity_vitality}},
#'   \code{\link{centrality_participation}}.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_community_hub_bridge(adj, membership = c(1, 1, 1, 2, 2, 2))
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
calculate_entropy_variation <- function(g, of = c("degree", "betweenness"),
                                        mode = "all") {
  of <- match.arg(of)
  n <- igraph::vcount(g)
  if (n == 0L) return(numeric(0))
  if (of == "degree") {
    b <- .cg_path_matrix(g, NULL)
    # An undirected graph has one degree; "all" reads it (as 2k, which the
    # normalisation in the entropy cancels).
    if (!igraph::is_directed(g)) mode <- "all"
    return(.cg_entropy_variation_degree(b, mode))
  }
  # Betweenness is recomputed on each deletion graph, as in the author's
  # code; the paper's networks are unweighted, so weights are ignored.
  h <- if ("weight" %in% igraph::edge_attr_names(g)) {
    igraph::delete_edge_attr(g, "weight")
  } else {
    g
  }
  f <- igraph::betweenness(h)
  .cg_entropy_variation(f, function(i) {
    igraph::betweenness(igraph::delete_vertices(h, i))
  })
}

#' Entropy Variation
#'
#' Ai's (2017) vitality measure: the change in the Shannon entropy of a
#' node-level distribution when a node and its links are removed,
#' \deqn{EnV_f(i) = I_f(G) - I_f(G - i), \qquad
#'   I_f(G) = -\sum_j p_j \log p_j, \quad p_j = \frac{f(j)}{\sum_l f(l)},}
#' with \eqn{f} the degree (\code{"entropy_variation_degree"}, in-, out- or
#' total degree by \code{mode}) or the betweenness
#' (\code{"entropy_variation_betweenness"}). Natural logarithm, as in the
#' author's code. The difference is signed: a positive value means the
#' remaining network is less even without the node, a negative value that
#' removing it evens the distribution out. Higher = more important.
#'
#' The degree variant is computed in closed form. The betweenness variant
#' recomputes betweenness once per node and costs \eqn{O(n \cdot nm)}; it
#' ignores edge weights. Self-loops are counted as igraph counts them. When
#' a deletion leaves every \eqn{f} at zero (for instance betweenness on a
#' clique) that entropy is taken as 0.
#'
#' Validated against the author's own R code path
#' (\code{iCalEnV()} from the paper's repository) to \eqn{10^{-15}} and
#' against the quantiles of Table 2 of the paper on its 4234-node
#' Snake Idioms network.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param of Which distribution: \code{"degree"} (default) or
#'   \code{"betweenness"}.
#' @param mode For the degree variant on directed networks: \code{"all"}
#'   (default, in + out), \code{"out"}, or \code{"in"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector, one value per node, in nats.
#'
#' @references Ai, X. (2017). Node importance ranking of complex networks
#'   with entropy variation. Entropy, 19(7), 303.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' star5 <- matrix(0, 5, 5)
#' star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
#' rownames(star5) <- colnames(star5) <- LETTERS[1:5]
#' centrality_entropy_variation(star5)
#' centrality_entropy_variation(star5, of = "betweenness")
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
calculate_s_shell <- function(g, a = 0.5) {
  if (igraph::vcount(g) == 0L) return(integer(0))
  .cg_s_shell(.cg_path_matrix(g, NULL), a = a)
}

#' s-shell Index
#'
#' Liu, Tang, Do and Hui's (2017) strength-based generalisation of k-shell
#' for identifying spreaders. Each link is given an asymmetric weight from
#' the topology alone,
#' \deqn{w_{ij} = 1 + (k_i \, k^{out}_j)^a,}
#' where \eqn{k^{out}_j} is the number of \eqn{j}'s neighbours that lie
#' outside \eqn{i}'s closed neighbourhood (links that lead a spreading
#' process to new territory), and each node's strength is
#' \eqn{s_i = \sum_{j \in N(i)} w_{ij}}. The graph is then peeled like a
#' k-shell but by strength: the minimum remaining strength is the
#' threshold, everything at or below it is removed (neighbours lose the
#' corresponding \eqn{w_{ji}}), removals cascade until the threshold holds,
#' and the removed nodes receive the next shell index. Higher index = more
#' central. With \eqn{a = 0} the shells are the dense ranks of the k-core
#' numbers.
#'
#' The index is an ordinal counter (1 = outermost shell), not a strength
#' value, so it is not comparable across graphs. Isolates form shell 1 on
#' their own, shifting every other shell up by one, as the paper's rule
#' implies. Direction, edge weights and self-loops are ignored. The paper's
#' robust default is \eqn{a = 0.5}.
#'
#' Validated against the shell peeled at each threshold being exactly the
#' complement of the maximal subgraph in which every node keeps strength
#' above the threshold (brute force over all vertex subsets), and against
#' k-core dense ranks at \eqn{a = 0}.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param s_shell_a Exponent \eqn{a} of the link weights. Default 0.5.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named integer vector of shell indices, one per node.
#'
#' @references Liu, Y., Tang, M., Do, Y., & Hui, P. M. (2017). Accurate
#'   ranking of influential spreaders in networks based on dynamically
#'   asymmetric link weights. Physical Review E, 96(2), 022323.
#'
#' @seealso \code{\link{centrality_coreness}} for the k-shell index.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 2, 1, 3, 4, 5), c(2, 3, 3, 4, 5, 6))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_s_shell(adj)
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
calculate_degree_discount <- function(g, p = 0.01, single = FALSE) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  .cg_degree_discount(.cg_path_matrix(g, NULL), p = p, single = single)
}

#' @keywords internal
#' @noRd
calculate_ncvoterank <- function(g, theta = 0.5) {
  if (igraph::vcount(g) == 0L) return(numeric(0))
  h <- igraph::as_undirected(igraph::simplify(g, remove.loops = TRUE),
                             mode = "collapse")
  .cg_ncvoterank(.cg_path_matrix(h, NULL), ks = igraph::coreness(h),
                 theta = theta)
}

#' DegreeDiscountIC and SingleDiscount Rankings
#'
#' Chen, Wang and Yang's (2009) degree-discount heuristics for choosing
#' spreaders under the independent-cascade model. Nodes are selected one
#' at a time by the largest *discounted* degree; after each selection every
#' unselected neighbour \eqn{v} of the new seed counts one more selected
#' neighbour, \eqn{t_v}, and its discounted degree becomes
#' \deqn{dd_v = d_v - 2 t_v - (d_v - t_v)\, t_v\, p}
#' for DegreeDiscountIC (Algorithm 4 of the paper, with propagation
#' probability \eqn{p}, default 0.01), or simply \eqn{d_v - t_v} for
#' SingleDiscount, where each neighbour of a new seed discounts its degree
#' by one. Every node is placed, so the result is a full ranking, returned
#' as a score: the first node selected scores 1, the last \eqn{1 / n}.
#'
#' Ties are broken by node order, which the paper does not specify.
#' Direction, edge weights and self-loops are ignored, as in the paper's
#' setting. Validated against an independent implementation of the
#' algorithm and against the reference code of the influence-maximization
#' literature on the karate club graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param discount_p Propagation probability \eqn{p} for DegreeDiscountIC.
#'   Default 0.01.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector in \eqn{(0, 1]}, one score per node.
#'
#' @references Chen, W., Wang, Y., & Yang, S. (2009). Efficient influence
#'   maximization in social networks. Proceedings of the 15th ACM SIGKDD
#'   International Conference on Knowledge Discovery and Data Mining,
#'   199-208.
#'
#' @seealso \code{\link{centrality_voterank}} for the voting-based
#'   alternative.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_degree_discount(adj)
#' centrality_single_discount(adj)
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
#' Kumar and Panda's (2020) neighbourhood-coreness VoteRank. As in
#' VoteRank, every node votes for its neighbours with its voting ability,
#' the top scorer is elected, and the abilities around it are weakened;
#' here each voter's ability is additionally weighted by its neighbourhood
#' coreness,
#' \deqn{s_u = \sum_{v \in N(u)} va_v \,[\theta + (1 - \theta)\, nc_v],
#'   \qquad nc_v = \frac{\sum_{w \in N(v)} ks(w)}
#'   {\max_j \sum_{w \in N(j)} ks(w)},}
#' with \eqn{ks} the k-shell index (Bae & Kim 2014) and \eqn{\theta = 0.5}.
#' After an election the winner's ability drops to 0, its neighbours lose
#' \eqn{1 / \langle k \rangle} and the nodes two steps away lose
#' \eqn{1 / (2 \langle k \rangle)}. Elections continue until every node is
#' placed, as in \code{\link{centrality_voterank}}; the first elected
#' scores 1, the last \eqn{1 / n}.
#'
#' **Provenance.** The original Physica A article could not be obtained;
#' this definition follows the Centrality Zoo encyclopedia (Shvydun 2025)
#' and three independent restatements (Yu et al. 2020, Li et al. 2022,
#' Zhu et al. 2023), which agree on the voter-side coreness weighting.
#' The scaling of the coreness term by its maximum follows Yu et al., who
#' state the coreness is normalised without giving the form. With
#' \eqn{\theta = 1} and no two-hop weakening the procedure is exactly
#' VoteRank, which is reproduced against \code{networkx.voterank}.
#' Defined for undirected graphs; direction, weights and loops are ignored.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna
#'   object).
#' @param ncvote_theta Weight \eqn{\theta} of the plain vote. Default 0.5.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector in \eqn{(0, 1]}, one score per node.
#'
#' @references
#' Kumar, S., & Panda, B. S. (2020). Identifying influential nodes in
#'   social networks: Neighborhood coreness based voting approach.
#'   Physica A, 553, 124215.
#'
#' Zhang, J.-X., Chen, D.-B., Dong, Q., & Zhao, Z.-D. (2016). Identifying
#'   a set of influential spreaders in complex networks. Scientific
#'   Reports, 6, 27823.
#'
#' @seealso \code{\link{centrality_voterank}}.
#'
#' @export
#' @examples
#' adj <- matrix(0, 6, 6)
#' adj[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
#' adj <- adj + t(adj)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#' centrality_ncvoterank(adj)
centrality_ncvoterank <- function(x, ncvote_theta = 0.5, ...) {
  df <- centrality(x, measures = "ncvoterank", ncvote_theta = ncvote_theta,
                   ...)
  stats::setNames(df$ncvoterank, df$node)
}
