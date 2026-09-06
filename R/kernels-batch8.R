# ===========================================================================
# Batch 8 kernels — Centrality Zoo "on the way" batch
#
# Pure base-R kernels on matrices. Each one is validated in
# tests/testthat/test-centrality-batch8.R against an exact brute-force
# definition (Shapley coalitions, spreading orders, s-core by definition,
# all shortest paths) and against independent Python references written
# from the source papers.
#
# Conventions shared with R/kernels-batch7.R: `b` is an adjacency matrix
# (any non-zero entry is an edge, diagonal ignored where the paper counts
# degree without self-loops), `d` a hop-distance matrix (0 diagonal, Inf
# unreachable). Nothing here touches igraph.
# ===========================================================================

#' Off-diagonal edge indicator
#'
#' The Shapley games count degree without self-loops (Michalak et al.
#' 2013), so the diagonal is masked in a local copy; the caller's matrix is
#' never modified.
#'
#' @param b Adjacency matrix.
#' @return Numeric 0/1 matrix; `[i, j] = 1` when `i -> j` is an edge, `i != j`.
#' @keywords internal
#' @noRd
.cg_edge_indicator <- function(b) {
  nb <- (b != 0) & (row(b) != col(b))
  storage.mode(nb) <- "numeric"
  nb
}

#' Shapley-value centrality, games 1 to 3 (Michalak et al. 2013)
#'
#' Closed forms for the Shapley value of three coalition games on a graph.
#' `N(v)` is the out-neighbourhood of `v` and `deg(u)` the in-degree of
#' `u`; on an undirected graph both reduce to the paper's formulas.
#'
#' * Game 1: `v(C)` = nodes in `C` or adjacent to `C`.
#'   `SV(v) = sum_{u in {v} + N(v)} 1 / (1 + deg(u))`.
#' * Game 2: `v(C)` = nodes in `C` or with at least `k` neighbours in `C`.
#'   `SV(v) = min(1, k / (1 + deg(v))) +`
#'   `sum_{u in N(v)} max(0, (deg(u) - k + 1) / (deg(u) (1 + deg(u))))`.
#' * Game 3: `v(C)` = nodes within `cutoff` hops of `C`.
#'   `SV(v) = sum_{u in {v} + N_d(v)} 1 / (1 + ext(u))`, where `N_d(v)` is
#'   the set of nodes within `cutoff` hops of `v` and `ext(u)` the number
#'   of nodes from which `u` is within `cutoff` hops.
#'
#' @param b Adjacency matrix (games 1 and 2).
#' @param game Integer 1, 2 or 3.
#' @param k Threshold for game 2 (default 2, the paper's smallest setting).
#' @param cutoff Hop cutoff for game 3 (default 2).
#' @param d Hop-distance matrix (game 3 only), `d[i, j]` = hops from `i` to `j`.
#' @return Numeric vector, one Shapley value per node. Values sum to the
#'   number of nodes (efficiency) in every game.
#' @keywords internal
#' @noRd
.cg_shapley <- function(b, game = 1L, k = 2, cutoff = 2, d = NULL) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (game == 3L) {
    stopifnot("`d` must be a hop-distance matrix for game 3" = is.matrix(d))
    reach <- (d <= cutoff) & (row(d) != col(d))
    storage.mode(reach) <- "numeric"
    ext <- colSums(reach)
    share <- 1 / (1 + ext)
    return(as.numeric(share + reach %*% share))
  }
  nb <- .cg_edge_indicator(b)
  deg <- colSums(nb)
  if (game == 1L) {
    share <- 1 / (1 + deg)
    return(as.numeric(share + nb %*% share))
  }
  # game 2: an out-neighbour u always has deg(u) >= 1, but the guard keeps
  # the vector finite for nodes nobody points at.
  own <- pmin(1, k / (1 + deg))
  gain <- ifelse(deg > 0, pmax(0, (deg - k + 1) / (deg * (1 + deg))), 0)
  as.numeric(own + nb %*% gain)
}

#' Exact Shapley value by coalition enumeration (test oracle)
#'
#' Enumerates every coalition of the other nodes and averages the marginal
#' contribution with the Shapley weights. Exponential in `n`; for tests
#' with `n <= 8` only.
#'
#' @param b Adjacency matrix. @param game,k,cutoff,d As in `.cg_shapley()`.
#' @return Numeric vector of exact Shapley values.
#' @keywords internal
#' @noRd
.cg_shapley_exact <- function(b, game = 1L, k = 2, cutoff = 2, d = NULL) {
  n <- nrow(b)
  nb <- .cg_edge_indicator(b)
  reach <- if (game == 3L) (d <= cutoff) & (row(d) != col(d)) else NULL
  value <- function(coal) {
    if (length(coal) == 0L) return(0)
    inside <- seq_len(n) %in% coal
    hits <- function(m) colSums(m[coal, , drop = FALSE])
    covered <- switch(as.character(game),
                      "1" = inside | hits(nb) > 0,
                      "2" = inside | hits(nb) >= k,
                      "3" = inside | hits(reach) > 0)
    sum(covered)
  }
  vapply(seq_len(n), function(i) {
    others <- setdiff(seq_len(n), i)
    sizes <- 0:length(others)
    sum(vapply(sizes, function(r) {
      w <- factorial(r) * factorial(n - r - 1) / factorial(n)
      coals <- if (r == 0L) list(integer(0)) else
        utils::combn(others, r, simplify = FALSE)
      w * sum(vapply(coals, function(cc) value(c(cc, i)) - value(cc),
                     numeric(1L)))
    }, numeric(1L)))
  }, numeric(1L))
}

# ---------------------------------------------------------------------------
# Search information: access and hide (Rosvall et al. 2005; Sneppen et al. 2005)
# ---------------------------------------------------------------------------

#' Search information matrix
#'
#' `S[s, t] = -log2 sum_{shortest paths p} P(p)`, where a walker with no
#' map leaves the source with probability `1 / k_s` per link and each
#' intermediate node with probability `1 / (k_u - 1)` (the link it came in
#' on is excluded). On a directed graph every step uses `1 / k_out`. The
#' path-probability mass is accumulated layer by layer over the
#' shortest-path DAG, so no path is enumerated.
#'
#' @param b Adjacency matrix. @param d Hop-distance matrix.
#' @param directed Whether `b` is directed.
#' @return Numeric matrix in bits; `0` on the diagonal, `Inf` where `t`
#'   is unreachable from `s`.
#' @keywords internal
#' @noRd
.cg_search_information <- function(b, d, directed = FALSE) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(matrix(numeric(0), 0L, 0L))
  nb <- .cg_edge_indicator(b)
  k <- rowSums(nb)
  at_source <- ifelse(k > 0, 1 / k, 0)
  at_middle <- if (directed) at_source else ifelse(k > 1, 1 / (k - 1), 0)
  rows <- vapply(seq_len(n), function(s) {
    ds <- d[s, ]
    mass <- numeric(n)
    mass[s] <- 1
    ecc <- max(0, ds[is.finite(ds)])
    # Layer l of the BFS DAG receives mass only from layer l - 1, so the
    # layers must be visited in order; that dependence is the loop.
    for (l in seq_len(ecc)) {
      prev <- which(ds == l - 1)
      cur <- which(ds == l)
      f <- if (l == 1L) at_source[prev] else at_middle[prev]
      mass[cur] <- as.numeric(crossprod(nb[prev, cur, drop = FALSE],
                                        mass[prev] * f))
    }
    out <- -log2(mass)
    out[s] <- 0
    out
  }, numeric(n))
  t(rows)
}

#' Access information: mean search information from a node
#'
#' Average of `S[i, j]` over the nodes `j` the walker can reach from `i`
#' (including `i` itself, which costs 0 bits), so the divisor is `N` on a
#' connected graph and the reachable-set size otherwise. Low values mean
#' the node reaches the rest of the network with few decisions.
#'
#' @param s_mat Search information matrix.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_access_information <- function(s_mat) {
  finite <- is.finite(s_mat)
  rowSums(ifelse(finite, s_mat, 0)) / rowSums(finite)
}

#' Hide information: mean search information towards a node
#'
#' Average of `S[j, i]` over the nodes `j` that can reach `i`. High values
#' mean the node is hard to find from the rest of the network.
#'
#' @param s_mat Search information matrix.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_hide_information <- function(s_mat) {
  finite <- is.finite(s_mat)
  colSums(ifelse(finite, s_mat, 0)) / colSums(finite)
}

# ---------------------------------------------------------------------------
# Rumor centrality (Shah & Zaman 2010, 2011)
# ---------------------------------------------------------------------------

#' Rumor centrality on each node's BFS tree, log scale
#'
#' For a tree rooted at `v`, `R(v) = N! / prod_u T_u^v`, with `T_u^v` the
#' size of the subtree below `u` (eq. 10 of Shah & Zaman 2011): the number
#' of spreading orders that start at `v`. On a general graph the paper
#' evaluates it on the breadth-first tree rooted at `v` (their eq. 24).
#' The BFS is first-in-first-out with neighbours scanned in label order,
#' so a node hangs from the earliest-discovered node of the previous layer
#' (the paper leaves the tie rule open; this is the rule its Figure 3
#' follows). `N` is the size of `v`'s component. Returned on the natural
#' log scale because `N!` overflows past 170 nodes; an isolate scores 0.
#'
#' @param b Adjacency matrix. @param d Hop-distance matrix.
#' @return Numeric vector of `log R(v)`; higher = more plausible source.
#' @keywords internal
#' @noRd
.cg_rumor <- function(b, d) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  nb <- .cg_edge_indicator(b)
  vapply(seq_len(n), function(v) {
    ds <- d[v, ]
    ecc <- max(0, ds[is.finite(ds)])
    comp_size <- sum(is.finite(ds))
    size <- rep(1, n)
    parent <- rep(NA_integer_, n)
    order_of <- rep(NA_integer_, n)
    order_of[v] <- 1L
    prev <- v
    layers <- vector("list", ecc)
    # BFS layers are built from the previous layer's visiting order and the
    # subtree sizes are then summed from the deepest layer up; both are
    # sequential over depth, hence the two loops.
    for (l in seq_len(ecc)) {
      cur <- which(ds == l)
      first <- vapply(cur, function(w) {
        cand <- prev[nb[prev, w] != 0]
        cand[which.min(order_of[cand])]
      }, integer(1L))
      parent[cur] <- first
      cur <- cur[order(order_of[first], cur)]
      order_of[cur] <- max(order_of, na.rm = TRUE) + seq_along(cur)
      layers[[l]] <- cur
      prev <- cur
    }
    for (l in rev(seq_len(ecc))) {
      cur <- layers[[l]]
      added <- rowsum(size[cur], parent[cur])
      size[as.integer(rownames(added))] <- size[as.integer(rownames(added))] +
        as.numeric(added)
    }
    lgamma(comp_size + 1) - sum(log(size[is.finite(ds)]))
  }, numeric(1L))
}

# ---------------------------------------------------------------------------
# Community hub-bridge (Ghalmane, El Hassouni & Cherifi 2019)
# ---------------------------------------------------------------------------

#' Community hub-bridge score
#'
#' `CHB(i) = |C(i)| * k_intra(i) + NNC(i) * k_inter(i)` (eqs. 2-4 of
#' Ghalmane et al. 2019): the intra-community degree weighted by the raw
#' size of the node's own community, plus the inter-community degree
#' weighted by the number of *other* communities the node touches.
#'
#' @param nb 0/1 neighbour matrix (`nb[i, j] = 1` when `j` is a neighbour
#'   of `i` under the caller's mode).
#' @param membership Community labels, one per node.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_community_hub_bridge <- function(nb, membership) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  comm <- as.integer(factor(membership))
  k <- max(comm)
  own <- matrix(0, n, k)
  own[cbind(seq_len(n), comm)] <- 1
  links <- nb %*% own            # links from i into each community
  intra <- links[cbind(seq_len(n), comm)]
  inter <- rowSums(links) - intra
  touched <- rowSums(links > 0) - (intra > 0)  # other communities reached
  size <- colSums(own)[comm]
  as.numeric(size * intra + touched * inter)
}

# ---------------------------------------------------------------------------
# Entropy variation (Ai 2017)
# ---------------------------------------------------------------------------

#' Shannon entropy of a non-negative information function over nodes
#'
#' `I_f = -sum_i p_i log p_i`, `p_i = f_i / sum f` (eqs. 4-5 of Ai 2017),
#' natural log, `0 log 0 = 0`. When `sum f = 0` there is no distribution;
#' the entropy is taken as 0 (the author's code returns `NaN` there).
#'
#' @param f Non-negative numeric vector.
#' @return A single number in nats.
#' @keywords internal
#' @noRd
.cg_entropy_of <- function(f) {
  total <- sum(f)
  if (!is.finite(total) || total <= 0) return(0)
  p <- f[f > 0] / total
  -sum(p * log(p))
}

#' Entropy variation from a set of post-removal information vectors
#'
#' `EnV(i) = I_f(G) - I_f(G - i)` (eq. 10 of Ai 2017), signed, so nodes
#' whose removal makes the remaining distribution *more* even score
#' negative.
#'
#' @param f Information function on the full graph.
#' @param f_removed Function of a node index returning the information
#'   function of the graph with that node deleted (length `n - 1`).
#' @return Numeric vector; higher = more important.
#' @keywords internal
#' @noRd
.cg_entropy_variation <- function(f, f_removed) {
  n <- length(f)
  if (n == 0L) return(numeric(0))
  base <- .cg_entropy_of(f)
  base - vapply(seq_len(n), function(i) .cg_entropy_of(f_removed(i)),
                numeric(1L))
}

#' Degree-based entropy variation, closed form
#'
#' Deleting node `i` removes it and lowers every other node's degree by
#' the links it shared with `i`, so no graph is rebuilt: `f_removed(i)` is
#' `f[-i] - delta[-i, i]`. Loops are counted as igraph counts them (once on
#' each of in- and out-degree), and undirected graphs use the total degree.
#'
#' @param b Adjacency matrix. @param mode `"all"`, `"out"` or `"in"`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_entropy_variation_degree <- function(b, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  bin <- (b != 0) * 1
  k_out <- rowSums(bin)
  k_in <- colSums(bin)
  f <- switch(mode, all = k_out + k_in, out = k_out, "in" = k_in)
  # delta[j, i]: how much node j's degree drops when node i is deleted
  delta <- switch(mode, all = bin + t(bin), out = bin, "in" = t(bin))
  .cg_entropy_variation(f, function(i) f[-i] - delta[-i, i])
}

# ---------------------------------------------------------------------------
# s-shell index (Liu, Tang, Do & Hui 2017)
# ---------------------------------------------------------------------------

#' Asymmetric link weights and node strengths of Liu et al. (2017)
#'
#' `w_ij = 1 + (k_i k_j^out(i))^a`, where `k_j^out(i)` counts the
#' neighbours of `j` outside `i`'s closed neighbourhood (eq. 3), and
#' `s_i = sum_{j in N(i)} w_ij` (eq. 4). Self-loops are ignored.
#'
#' @param b Adjacency matrix. @param a Exponent, default 0.5.
#' @return List with `w` (n x n, zero off the edges) and `s`.
#' @keywords internal
#' @noRd
.cg_asymmetric_weights <- function(b, a = 0.5) {
  nb <- .cg_edge_indicator(b)
  nb <- pmax(nb, t(nb))
  k <- rowSums(nb)
  common <- nb %*% nb
  # For an edge i -- j, j's out-reaching links are its k_j links minus the
  # ones into i's neighbourhood and minus the link back to i.
  k_out <- sweep(-common, 2, k, "+") - 1
  w <- nb * (1 + (outer(k, rep(1, length(k))) * pmax(k_out, 0))^a)
  list(w = w, s = rowSums(w))
}

#' s-shell decomposition (Liu et al. 2017, Sec. III.B)
#'
#' Peels the graph by strength the way k-shell peels it by degree: the
#' minimum remaining strength `s_m` is the threshold, every node at or
#' below it is removed (neighbours lose `w_ji`), the removal is repeated
#' until nothing is at or below `s_m`, and the removed nodes get the next
#' shell index. With `a = 0` the shells are the dense ranks of k-core.
#'
#' @param b Adjacency matrix. @param a Exponent, default 0.5.
#' @param tol Tolerance for the `<= s_m` test on accumulated sums.
#' @return Integer vector of shell indices; 1 is the outermost shell.
#' @keywords internal
#' @noRd
.cg_s_shell <- function(b, a = 0.5, tol = 1e-9) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(integer(0))
  aw <- .cg_asymmetric_weights(b, a)
  w <- aw$w
  s <- aw$s
  shell <- rep(NA_integer_, n)
  remaining <- rep(TRUE, n)
  index <- 0L
  # Shells are peeled one after another, and inside a shell removals
  # cascade until the strengths settle; both are sequential by definition.
  while (any(remaining)) {
    index <- index + 1L
    s_m <- min(s[remaining])
    repeat {
      drop <- remaining & s <= s_m + tol
      if (!any(drop)) break
      shell[drop] <- index
      remaining[drop] <- FALSE
      if (!any(remaining)) break
      s[remaining] <- s[remaining] -
        rowSums(w[remaining, drop, drop = FALSE])
    }
  }
  shell
}

# ---------------------------------------------------------------------------
# Greedy seed-selection rankings: DegreeDiscountIC, SingleDiscount
# (Chen, Wang & Yang 2009) and NCVoteRank (Kumar & Panda 2020)
# ---------------------------------------------------------------------------

#' Convert a selection order into a centrality score
#'
#' Same convention as `.cg_voterank()`: the first node selected scores 1,
#' the last selected `1 / m`, and a node never selected 0.
#'
#' @param rank Integer vector, position in the selection order (0 = never).
#' @return Numeric vector in `[0, 1]`.
#' @keywords internal
#' @noRd
.cg_rank_score <- function(rank) {
  m <- max(rank)
  if (m == 0) return(rep(0, length(rank)))
  ifelse(rank > 0, (m + 1 - rank) / m, 0)
}

#' Index of the largest admissible score, ties to the lowest index
#'
#' Scores within `tol` of the maximum count as tied, so floating-point
#' noise from summation order cannot decide an election.
#'
#' @param score Numeric vector. @param admissible Logical mask.
#' @param tol Tolerance (1e-9).
#' @return An index.
#' @keywords internal
#' @noRd
.cg_argmax_tied <- function(score, admissible, tol = 1e-9) {
  top <- max(score[admissible])
  which(admissible & score >= top - tol)[1L]
}

#' Undirected simple neighbour matrix (direction and loops dropped)
#' @keywords internal
#' @noRd
.cg_undirected_view <- function(b) {
  nb <- .cg_edge_indicator(b)
  pmax(nb, t(nb))
}

#' DegreeDiscountIC and SingleDiscount (Chen, Wang & Yang 2009, Alg. 4)
#'
#' Repeatedly select the node with the largest discounted degree `dd`;
#' each unselected neighbour `v` of the selected node then counts one more
#' selected neighbour (`t_v`) and its discounted degree becomes
#' `d_v - 2 t_v - (d_v - t_v) t_v p` (DegreeDiscountIC, line 12 of the
#' algorithm) or `d_v - t_v` (SingleDiscount: "each neighbor of a newly
#' selected seed discounts its degree by one"). Ties go to the lowest node
#' index; the paper gives no tie rule. Every node is eventually selected,
#' so the output is a complete ranking.
#'
#' @param b Adjacency matrix. @param p Propagation probability (0.01).
#' @param single `TRUE` for SingleDiscount.
#' @return Numeric score vector from `.cg_rank_score()`.
#' @keywords internal
#' @noRd
.cg_degree_discount <- function(b, p = 0.01, single = FALSE) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  nb <- .cg_undirected_view(b)
  d <- rowSums(nb)
  dd <- d
  t_sel <- numeric(n)
  selected <- rep(FALSE, n)
  rank <- integer(n)
  # Each selection changes the discounted degrees that decide the next one.
  for (r in seq_len(n)) {
    u <- .cg_argmax_tied(dd, !selected)
    selected[u] <- TRUE
    rank[u] <- r
    v <- which(nb[u, ] != 0 & !selected)
    t_sel[v] <- t_sel[v] + 1
    dd[v] <- if (single) d[v] - t_sel[v] else
      d[v] - 2 * t_sel[v] - (d[v] - t_sel[v]) * t_sel[v] * p
  }
  .cg_rank_score(rank)
}

#' NCVoteRank (Kumar & Panda 2020), as restated by the Centrality Zoo
#'
#' VoteRank whose voters weight their ability by neighbourhood coreness:
#' node `u` scores `sum_{v in N(u)} va_v (theta + (1 - theta) nc_v)`, where
#' `nc_v = sum_{w in N(v)} ks(w)` (Bae & Kim 2014) scaled by its maximum.
#' The top scorer is elected, its ability drops to 0, its neighbours lose
#' `1 / <k>` and the nodes at distance two lose `1 / (2 <k>)`. Elections
#' continue until every node is placed (ties to the lowest index), matching
#' `.cg_voterank()`.
#'
#' @param b Adjacency matrix. @param ks k-core index per node.
#' @param theta Weight of the plain vote (0.5). @param two_hop Whether
#'   nodes at distance two are weakened.
#' @param return_scores Return the n x n matrix of scores before each
#'   election instead (row = round); for tie diagnostics.
#' @return Numeric score vector from `.cg_rank_score()`.
#' @keywords internal
#' @noRd
.cg_ncvoterank <- function(b, ks, theta = 0.5, two_hop = TRUE,
                           return_scores = FALSE) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  nb <- .cg_undirected_view(b)
  k_avg <- max(mean(rowSums(nb)), .Machine$double.eps)
  f1 <- 1 / k_avg
  f2 <- 1 / (2 * k_avg)
  nc <- as.numeric(nb %*% ks)
  if (max(nc) > 0) nc <- nc / max(nc)
  weight <- theta + (1 - theta) * nc
  va <- rep(1, n)
  selected <- rep(FALSE, n)
  rank <- integer(n)
  scores <- matrix(NA_real_, n, n)
  # Each election suppresses the abilities that decide the next one.
  for (r in seq_len(n)) {
    score <- as.numeric(nb %*% (va * weight))
    scores[r, ] <- ifelse(selected, NA, score)
    u <- .cg_argmax_tied(score, !selected)
    selected[u] <- TRUE
    rank[u] <- r
    va[u] <- 0
    one <- nb[u, ] != 0
    va[one] <- pmax(0, va[one] - f1)
    if (two_hop) {
      two <- colSums(nb[one, , drop = FALSE]) > 0 & !one
      two[u] <- FALSE
      va[two] <- pmax(0, va[two] - f2)
    }
  }
  if (return_scores) return(scores)
  .cg_rank_score(rank)
}
