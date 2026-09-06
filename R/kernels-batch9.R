# ===========================================================================
# Batch 9 kernels — remaining Centrality Zoo measures with pinned definitions
#
# Pure base-R kernels on matrices; validated in
# tests/testthat/test-centrality-batch9.R against published tables, hand
# values and brute-force definitions, and against independent Python
# references (local_testing_and_equivalence/batch9/). Conventions as in
# R/kernels-batch7.R and R/kernels-batch8.R.
# ===========================================================================

# ---------------------------------------------------------------------------
# Community-aware measures
# ---------------------------------------------------------------------------

#' Community link counts: how many links each node has into each community
#'
#' @param nb 0/1 neighbour matrix (`nb[i, j] = 1` when `j` is a neighbour of
#' `i`).
#' @param membership Community labels, one per node.
#' @return List with `links` (n x K), `own` (n x K indicator), `comm`
#'   (integer labels), `size` (K), `deg` (n), `intra` (n), `inter` (n).
#' @keywords internal
#' @noRd
.cg_community_links <- function(nb, membership) {
  n <- nrow(nb)
  comm <- as.integer(factor(membership))
  k <- max(comm)
  own <- matrix(0, n, k)
  own[cbind(seq_len(n), comm)] <- 1
  links <- nb %*% own
  intra <- links[cbind(seq_len(n), comm)]
  deg <- rowSums(nb)
  list(links = links, own = own, comm = comm, size = colSums(own),
       deg = deg, intra = intra, inter = deg - intra)
}

#' Community-based centrality (Zhao, Wang, Zhang & Zhu 2015), eq. 1
#'
#' `CbC(i) = sum_w d_iw * S_w / N`: every link weighted by the size of the
#' community it lands in. No parameters.
#'
#' @inheritParams .cg_community_links
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_community_based <- function(nb, membership) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  cl <- .cg_community_links(nb, membership)
  as.numeric(cl$links %*% cl$size) / n
}

#' Comm centrality (Gupta, Singh & Cherifi 2016), eqs. 3-4
#'
#' `CC(i) = (1 + mu_C) (k_in / max_C k_in) R + (1 - mu_C) ((k_out / max_C
#' k_out) R)^2`,
#' `mu_C` the mean inter-link fraction of `i`'s community. `R` defaults to
#' the paper's recommended per-community `max_C k_in`; a number applies one
#' global `R`. Communities without intra (inter) links contribute 0 through
#' that term, and an isolate scores 0.
#'
#' @inheritParams .cg_community_links
#' @param r `"max_intra"` or a positive number.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_comm_centrality <- function(nb, membership, r = "max_intra") {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  cl <- .cg_community_links(nb, membership)
  groups <- factor(cl$comm, levels = seq_len(max(cl$comm)))
  max_in <- as.numeric(tapply(cl$intra, groups, max))
  max_out <- as.numeric(tapply(cl$inter, groups, max))
  frac <- ifelse(cl$deg > 0, cl$inter / cl$deg, 0)
  mu <- as.numeric(tapply(frac, groups, mean))
  r_c <- if (identical(r, "max_intra")) {
    max_in
  } else {
    rep(as.numeric(r), max(cl$comm))
  }
  mi <- max_in[cl$comm]
  mo <- max_out[cl$comm]
  in_term <- ifelse(mi > 0, cl$intra / mi * r_c[cl$comm], 0)
  out_term <- ifelse(mo > 0, cl$inter / mo * r_c[cl$comm], 0)
  as.numeric((1 + mu[cl$comm]) * in_term + (1 - mu[cl$comm]) * out_term^2)
}

#' Community-based mediator (Tulu, Hou & Younas 2018), eqs. 9-12
#'
#' Shannon entropy (base 2) of a node's link distribution over the
#' communities, times its share of the total degree:
#' `CbM(i) = H_i * d_i / sum_j d_j`, `H_i = -sum_k p_ik log2 p_ik`,
#' `p_ik = d_i(C_k) / d_i`. A node whose links all stay inside one
#' community scores 0; the base-2 log is what reproduces the paper's
#' Table 1.
#'
#' @inheritParams .cg_community_links
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_community_mediator <- function(nb, membership) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  cl <- .cg_community_links(nb, membership)
  p <- cl$links / pmax(cl$deg, 1)
  h <- -rowSums(ifelse(p > 0, p * log2(pmax(p, .Machine$double.xmin)), 0))
  total <- sum(cl$deg)
  if (total <= 0) return(rep(0, n))
  as.numeric(h * cl$deg / total)
}

# ---------------------------------------------------------------------------
# Local-dimension descendants (hop-distance matrix `d`)
# ---------------------------------------------------------------------------

#' Silva-Costa local dimension at a fixed radius (their eq. 4)
#'
#' `D_i(r) = r n_i(r) / B_i(r)`, with `n_i(r)` the ring at distance `r` and
#' `B_i(r)` the ball within `r` (centre included). Nodes whose eccentricity
#' is below `r` have an empty ring and score 0, as the paper states for
#' `r -> Inf`.
#'
#' @param d Hop-distance matrix. @param r Radius (default 2).
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_local_dimension_fixed <- function(d, r = 2) {
  n <- nrow(d)
  if (is.null(n) || n == 0L) return(numeric(0))
  vapply(seq_len(n), function(i) {
    rings <- .cg_ring_counts(d[i, ], i)
    if (length(rings) < r) return(0)
    ball <- 1 + sum(rings[seq_len(r)])
    r * rings[r] / ball
  }, numeric(1L))
}

#' Fuzzy local dimension (Wen & Jiang 2019)
#'
#' Fuzzy ball `N_i(r) = sum_{d_ij <= r} exp(-d_ij^2 / r^2) / |{j : d_ij <= r}|`
#' (centre included, its term equal to 1), for `r = 1, ..., d_max(i)`;
#' the measure is the OLS slope of `log N_i(r)` on `log r`. Larger = more
#' influential. `NaN` when fewer than two radii exist.
#'
#' @param d Hop-distance matrix.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_fuzzy_local_dimension <- function(d) {
  n <- nrow(d)
  if (is.null(n) || n == 0L) return(numeric(0))
  vapply(seq_len(n), function(i) {
    di <- d[i, ]
    di <- di[is.finite(di)]
    r_max <- max(di)
    if (r_max < 2) return(NaN)
    fuzzy <- vapply(seq_len(r_max), function(r) {
      inside <- di[di <= r]
      sum(exp(-inside^2 / r^2)) / length(inside)
    }, numeric(1L))
    .cg_ols_slope(log(seq_len(r_max)), log(fuzzy))
  }, numeric(1L))
}

#' Local volume dimension (Li & Deng 2021)
#'
#' Volume `V_i(l) = sum_{d_ij <= l} k_j` (degrees of every node within `l`
#' hops, centre included), `l = 1, ..., ecc(i)`; the measure is the OLS
#' slope of `ln V_i(l)` on `ln l`. Smaller = more important. `NaN` when
#' fewer than two radii exist.
#'
#' @param d Hop-distance matrix. @param deg Degree vector.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_local_volume_dimension <- function(d, deg) {
  n <- nrow(d)
  if (is.null(n) || n == 0L) return(numeric(0))
  vapply(seq_len(n), function(i) {
    di <- d[i, ]
    ok <- is.finite(di)
    l_max <- max(di[ok])
    if (l_max < 2) return(NaN)
    volume <- vapply(seq_len(l_max), function(l) sum(deg[ok & di <= l]),
                     numeric(1L))
    if (any(volume <= 0)) return(NaN)
    .cg_ols_slope(log(seq_len(l_max)), log(volume))
  }, numeric(1L))
}

# ---------------------------------------------------------------------------
# VoteRank variants
# ---------------------------------------------------------------------------

#' WVoteRank (Sun, Chen, He & Ch'ng 2019), eq. 2 and Figure 1
#'
#' Weighted VoteRank: node `v` scores `sqrt(k_v * sum_{u in N(v)} va_u w_vu)`,
#' the top scorer is elected, its ability drops to 0 and its neighbours
#' lose `1 / <w>`, with `<w>` the average strength `2 sum w / n` (the
#' paper's worked Figure 1 pins the strength, not the degree). Elections
#' continue until every node is placed; ties go to the lowest index.
#'
#' @param w Symmetric weight matrix (diagonal ignored).
#' @return Numeric score vector from `.cg_rank_score()`.
#' @keywords internal
#' @noRd
.cg_wvoterank <- function(w, return_scores = FALSE) {
  n <- nrow(w)
  if (is.null(n) || n == 0L) return(numeric(0))
  w <- w * (row(w) != col(w))
  w <- pmax(w, t(w))
  nb <- (w != 0) * 1
  deg <- rowSums(nb)
  avg_strength <- sum(w) / n
  discount <- if (avg_strength > 0) 1 / avg_strength else 0
  va <- rep(1, n)
  selected <- rep(FALSE, n)
  rank <- integer(n)
  scores <- matrix(NA_real_, n, n)
  # Each election changes the abilities that decide the next one.
  for (r in seq_len(n)) {
    acc <- as.numeric(w %*% va)
    score <- ifelse(acc > 0, sqrt(deg * acc), 0)
    scores[r, ] <- ifelse(selected, NA, score)
    u <- .cg_argmax_tied(score, !selected)
    selected[u] <- TRUE
    rank[u] <- r
    va[u] <- 0
    hit <- nb[u, ] != 0
    va[hit] <- pmax(0, va[hit] - discount)
  }
  if (return_scores) return(scores)
  .cg_rank_score(rank)
}

#' EnRenew (Guo, Yang, Guo, Pan & Chen 2020), eq. 1 and Algorithm 1
#'
#' Each node's spreading ability is the entropy its neighbours supply,
#' `E_v = sum_{u in N(v)} -p_uv log p_uv`, `p_uv = k_u / sum_{l in N(v)} k_l`.
#' The largest `E` is elected; then, for depths `1..l` around it, every
#' entropy term flowing from a node at depth `d - 1` to a node at depth `d`
#' is scaled by `1 - 1 / (2^{d-1} log <k>)`. Natural log, as the paper's
#' Figure 1 numbers require. Elections continue until every node is placed.
#'
#' @param b Adjacency matrix. @param d Hop-distance matrix.
#' @param depth Renewal radius `l` (paper default 2).
#' @param return_scores Return the n x n matrix of scores before each
#'   election instead (row = election round); for tie diagnostics.
#' @return Numeric score vector from `.cg_rank_score()`.
#' @keywords internal
#' @noRd
.cg_enrenew <- function(b, d, depth = 2L, return_scores = FALSE) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  nb <- .cg_undirected_view(b)
  deg <- rowSums(nb)
  k_avg <- sum(deg) / n
  e_k <- if (k_avg > 0) log(k_avg) else 1
  # h[v, u] = entropy neighbour u supplies to v
  nb_sum <- as.numeric(nb %*% deg)
  p <- nb * rep(deg, each = n) / pmax(nb_sum, .Machine$double.eps)
  h <- ifelse(p > 0, -p * log(pmax(p, .Machine$double.xmin)), 0)
  selected <- rep(FALSE, n)
  rank <- integer(n)
  factors <- 1 - 1 / (2^(seq_len(depth) - 1) * e_k)
  scores <- matrix(NA_real_, n, n)
  # Each election renews the entropies that decide the next one.
  for (r in seq_len(n)) {
    e <- rowSums(h)
    scores[r, ] <- ifelse(selected, NA, e)
    u <- .cg_argmax_tied(e, !selected)
    selected[u] <- TRUE
    rank[u] <- r
    for (l in seq_len(depth)) {
      inner <- d[u, ] == l - 1
      outer_l <- d[u, ] == l
      if (!any(outer_l)) break
      h[outer_l, inner] <- h[outer_l, inner] * factors[l]
    }
  }
  if (return_scores) return(scores)
  .cg_rank_score(rank)
}

#' VoteRank++ (Liu, Li, Fang & Yao 2021), as in the authors' code
#'
#' Initial ability `va_i = ln(1 + k_i / k_max)`; voter `j` splits its vote
#' among its not-yet-elected neighbours in proportion to their degree,
#' `w_{j -> i} = k_i / sum_{x in N(j), unelected} k_x`; node `i` scores
#' `sqrt(k_i sum_{j in N(i)} va_j w_{j -> i})`. After an election the
#' winner's ability is 0, its neighbours' abilities are multiplied by
#' `lambda` and the unelected nodes two steps away by `sqrt(lambda)`.
#'
#' @param b Adjacency matrix. @param lambda Suppression factor (0.1).
#' @return Numeric score vector from `.cg_rank_score()`.
#' @keywords internal
#' @noRd
.cg_voterank_plus <- function(b, lambda = 0.1, return_scores = FALSE) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  nb <- .cg_undirected_view(b)
  deg <- rowSums(nb)
  k_max <- max(deg)
  if (k_max == 0) return(.cg_rank_score(seq_len(n)))
  va <- log(1 + deg / k_max)
  selected <- rep(FALSE, n)
  rank <- integer(n)
  scores <- matrix(NA_real_, n, n)
  # Each election changes both the abilities and the vote shares.
  for (r in seq_len(n)) {
    live <- !selected
    denom <- as.numeric(nb %*% (deg * live))   # per voter
    # each voter's ability per unit of live neighbour degree
    share <- ifelse(denom > 0, va / denom, 0)
    acc <- deg * as.numeric(nb %*% share) * live
    score <- ifelse(acc > 0, sqrt(deg * acc), 0)
    scores[r, ] <- ifelse(selected, NA, score)
    u <- .cg_argmax_tied(score, !selected)
    selected[u] <- TRUE
    rank[u] <- r
    va[u] <- 0
    one <- nb[u, ] != 0
    va[one] <- va[one] * lambda
    two <- colSums(nb[one, , drop = FALSE]) > 0 & !one & !selected
    va[two] <- va[two] * sqrt(lambda)
  }
  if (return_scores) return(scores)
  .cg_rank_score(rank)
}

# ---------------------------------------------------------------------------
# Node contraction (Tan, Wu & Deng 2006; Wang et al. 2011)
# ---------------------------------------------------------------------------

#' Agglomeration (cohesion) of a graph: `1 / (N * L)`
#'
#' `L` is the mean shortest-path length over ordered pairs. On a
#' disconnected graph the sources leave it undefined; the mean is taken
#' over the mutually reachable ordered pairs (a cograph choice). A single
#' node has agglomeration 1 by the paper's convention.
#'
#' @param nb 0/1 symmetric adjacency matrix.
#' @return A single number.
#' @keywords internal
#' @noRd
.cg_agglomeration <- function(nb) {
  n <- nrow(nb)
  if (n <= 1L) return(1)
  d <- .cg_distances(nb, "all")
  off <- row(d) != col(d) & is.finite(d)
  if (!any(off)) return(0)
  1 / (n * mean(d[off]))
}

#' Node contraction centrality (IMC)
#'
#' `IMC(v) = 1 - agglomeration(G) / agglomeration(G contracted at v)`
#' (eq. 2 of Wang et al. 2011), where contracting `v` merges `v` and all
#' its neighbours into one node, leaving `N - k_v` nodes; a contracted
#' graph of one node has agglomeration 1. Reproduces Table 1 of the paper.
#'
#' @param nb 0/1 symmetric adjacency matrix.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_node_contraction <- function(nb) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  base <- .cg_agglomeration(nb)
  if (base <= 0) return(rep(NaN, n))
  vapply(seq_len(n), function(v) {
    group <- nb[v, ] != 0
    group[v] <- TRUE
    rest <- which(!group)
    if (length(rest) == 0L) return(1 - base)
    merged_row <- as.numeric(colSums(nb[group, rest, drop = FALSE]) > 0)
    m <- rbind(cbind(nb[rest, rest, drop = FALSE], merged_row),
               c(merged_row, 0))
    contracted <- .cg_agglomeration(m)
    if (contracted <= 0) return(NaN)
    1 - base / contracted
  }, numeric(1L))
}

#' Line-graph adjacency of a simple undirected graph
#'
#' @param nb 0/1 symmetric adjacency matrix.
#' @return List with `edges` (m x 2 index matrix) and `adj` (m x m 0/1).
#' @keywords internal
#' @noRd
.cg_line_graph <- function(nb) {
  edges <- which(nb != 0 & upper.tri(nb), arr.ind = TRUE)
  m <- nrow(edges)
  inc <- matrix(0, nrow(nb), m)
  inc[cbind(edges[, 1], seq_len(m))] <- 1
  inc[cbind(edges[, 2], seq_len(m))] <- 1
  adj <- (crossprod(inc) > 0) * 1
  diag(adj) <- 0
  list(edges = edges, adj = adj)
}

#' Improved node contraction (IIMC), Wang et al. (2011) eq. 4
#'
#' `IIMC(v) = alpha IMC(v) + beta sum_{e incident to v} IMC_L(e)`, with
#' `IMC_L` the node-contraction score of the edge in the line graph. The
#' paper fixes `alpha / beta = 5`; `alpha + beta = 1` is the normalisation
#' that reproduces its Table 1.
#'
#' @param nb 0/1 symmetric adjacency matrix. @param rho `alpha / beta`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_improved_node_contraction <- function(nb, rho = 5) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  alpha <- rho / (rho + 1)
  beta <- 1 / (rho + 1)
  node_term <- .cg_node_contraction(nb)
  lg <- .cg_line_graph(nb)
  if (nrow(lg$edges) == 0L) return(alpha * node_term)
  edge_score <- .cg_node_contraction(lg$adj)
  inc <- matrix(0, n, nrow(lg$edges))
  inc[cbind(lg$edges[, 1], seq_len(nrow(lg$edges)))] <- 1
  inc[cbind(lg$edges[, 2], seq_len(nrow(lg$edges)))] <- 1
  alpha * node_term + beta * as.numeric(inc %*% edge_score)
}

# ---------------------------------------------------------------------------
# Two-way random walk betweenness (Curado, Rodriguez, Tortosa & Vicent 2022)
# ---------------------------------------------------------------------------

#' 2RW betweenness counters
#'
#' For every unordered pair `(i, j)`, `T[t, k] = P_itj P_jki` with the
#' paper's `P_itj = w_it w_tj / (d_i d_j)` (zero when any two of `i, t, j`
#' coincide), the diagonal of `T` is zeroed and the single largest entry
#' (first in row-major order) credits one count to `t` and one to `k`.
#' Reproduces the paper's toy example counters. Cost is `O(n^4)`.
#'
#' @param w Symmetric weight matrix (diagonal ignored).
#' @return Numeric vector of counts.
#' @keywords internal
#' @noRd
.cg_two_way_rw <- function(w) {
  n <- nrow(w)
  if (is.null(n) || n == 0L) return(numeric(0))
  w <- w * (row(w) != col(w))
  w <- pmax(w, t(w))
  d <- rowSums(w)
  counts <- numeric(n)
  pair <- function(i, j) {
    if (d[i] <= 0 || d[j] <= 0) return(NULL)
    p_ij <- w[i, ] * w[, j] / (d[i] * d[j])   # over t
    p_ji <- w[j, ] * w[, i] / (d[j] * d[i])   # over k
    p_ij[c(i, j)] <- 0
    p_ji[c(i, j)] <- 0
    tt <- outer(p_ij, p_ji)
    diag(tt) <- 0
    if (max(tt) <= 0) return(NULL)
    idx <- which.max(t(tt))                    # first maximum in row-major
    k <- (idx - 1) %% n + 1
    t_node <- (idx - 1) %/% n + 1
    c(t_node, k)
  }
  # Every unordered pair contributes at most one winning (t, k).
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      win <- pair(i, j)
      if (!is.null(win)) counts[win] <- counts[win] + 1
    }
  }
  counts
}

# ---------------------------------------------------------------------------
# Simple local measures
# ---------------------------------------------------------------------------

#' Heatmap centrality (Duron 2020)
#'
#' Farness minus the mean farness of the neighbours,
#' `C(v) = f(v) - mean_{u in N(v)} f(u)`, with `f(v)` the sum of hop
#' distances from `v` to the nodes it can reach. Lower (more negative) is
#' more central; an isolate has no neighbours and scores `NaN`.
#' Reproduces Table 1 of the paper.
#'
#' @param nb 0/1 neighbour matrix (rows = node, columns = its neighbours).
#' @param d Hop-distance matrix in the same direction.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_heatmap <- function(nb, d) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  farness <- rowSums(ifelse(is.finite(d), d, 0))
  deg <- rowSums(nb)
  ifelse(deg > 0, farness - as.numeric(nb %*% farness) / deg, NaN)
}

#' Flow coefficient (Honey, Kotter, Breakspear & Sporns 2007), BCT form
#'
#' Among the ordered pairs `(j, k)` of distinct neighbours of `v` (in- or
#' out-neighbours), the fraction joined by a two-step path `j -> v -> k`
#' but not by a direct link `j -> k`. Equal to `1 - clustering` on an
#' undirected graph; distinct only on directed graphs. Matches the Brain
#' Connectivity Toolbox `flow_coef_bd` exactly.
#'
#' @param b Adjacency matrix (direction kept).
#' @return Numeric vector in `[0, 1]`; 0 for fewer than two neighbours.
#' @keywords internal
#' @noRd
.cg_flow_coefficient <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  a <- .cg_edge_indicator(b)
  nb_any <- pmax(a, t(a))
  vapply(seq_len(n), function(v) {
    nbrs <- which(nb_any[v, ] != 0)
    m <- length(nbrs)
    if (m < 2L) return(0)
    two_step <- outer(a[nbrs, v] != 0, a[v, nbrs] != 0)
    direct <- a[nbrs, nbrs] != 0
    diag(two_step) <- FALSE
    sum(two_step & !direct) / (m * (m - 1))
  }, numeric(1L))
}

#' Local entropy (Nie, Guo, Zhao & Lu 2016)
#'
#' `LE(i) = -sum_{j in N(i)} k_j log k_j`, natural log, as printed by the
#' sources. Always non-positive; more negative for larger, denser
#' neighbourhoods. Isolates score 0.
#'
#' @param nb 0/1 neighbour matrix. @param deg Degree vector to weight by.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_local_entropy <- function(nb, deg) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  term <- ifelse(deg > 0, deg * log(pmax(deg, 1)), 0)
  -as.numeric(nb %*% term)
}

#' h-index of a multiset
#' @keywords internal
#' @noRd
.cg_h_of <- function(values) {
  if (length(values) == 0L) return(0L)
  s <- sort(values, decreasing = TRUE)
  sum(s >= seq_along(s))
}

#' Weighted h-index (Gao, Yu, Li, Shen & Gao 2019), eq. 3
#'
#' Topological link weights `w_ij = k_i k_j`; the h-index is taken over
#' the multiset in which each neighbour's weight is repeated `k_j` times.
#' Input edge weights play no role.
#'
#' @param nb 0/1 neighbour matrix. @param deg Degree vector.
#' @return Integer vector.
#' @keywords internal
#' @noRd
.cg_weighted_h_index <- function(nb, deg) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(integer(0))
  vapply(seq_len(n), function(i) {
    js <- which(nb[i, ] != 0)
    .cg_h_of(rep(deg[i] * deg[js], times = deg[js]))
  }, integer(1L))
}

#' Redundancy (Burt 1992; Borgatti 1997): mean degree of ego's alters
#'
#' `2 t_i / k_i`, with `t_i` the number of links among `i`'s neighbours;
#' 0 for fewer than two neighbours. Equal to degree minus effective size.
#' Higher = fewer structural holes.
#'
#' @param nb 0/1 symmetric neighbour matrix.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_redundancy <- function(nb) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  deg <- rowSums(nb)
  among <- rowSums((nb %*% nb) * nb)   # 2 t_i: ordered neighbour pairs linked
  ifelse(deg > 1, among / deg, 0)
}

# ---------------------------------------------------------------------------
# Coreness variants and geodesic k-path
# ---------------------------------------------------------------------------

#' Integer-threshold peeling on a generalised degree (Garas et al. 2012)
#'
#' For `t = 0, 1, 2, ...` delete every remaining node whose generalised
#' degree is at most `t`, recomputing after each deletion round, and label
#' it `t`. With the plain degree this is the k-core number.
#'
#' @param nb 0/1 symmetric neighbour matrix.
#' @param gen Function of (remaining indicator) returning the generalised
#'   degree of every node on the remaining subgraph.
#' @return Integer vector of shell labels.
#' @keywords internal
#' @noRd
.cg_peel_threshold <- function(nb, gen) {
  n <- nrow(nb)
  shell <- rep(NA_integer_, n)
  remaining <- rep(TRUE, n)
  t <- 0L
  # Thresholds rise one at a time and deletions cascade within each; both
  # are sequential by definition.
  while (any(remaining)) {
    repeat {
      gd <- gen(remaining)
      doomed <- remaining & gd <= t + 1e-12
      if (!any(doomed)) break
      shell[doomed] <- t
      remaining[doomed] <- FALSE
      if (!any(remaining)) break
    }
    t <- t + 1L
  }
  shell
}

#' Weighted k-shell (Garas, Schweitzer & Havlin 2012)
#'
#' Weights are normalised by their mean, divided by their minimum and
#' rounded to the nearest integer (section 3 of the paper); the
#' generalised degree is `k' = (k^alpha s^beta)^(1 / (alpha + beta))` on
#' the remaining subgraph, and the graph is peeled by integer thresholds.
#' Unit weights give the ordinary k-core number. Reproduces the paper's
#' Figure 1 example.
#'
#' @param w Symmetric weight matrix. @param alpha,beta Exponents (1, 1).
#' @return Integer vector of shell labels.
#' @keywords internal
#' @noRd
.cg_weighted_kshell <- function(w, alpha = 1, beta = 1) {
  n <- nrow(w)
  if (is.null(n) || n == 0L) return(integer(0))
  w <- w * (row(w) != col(w))
  w <- pmax(w, t(w))
  nb <- (w != 0) * 1
  vals <- w[nb != 0]
  if (length(vals)) {
    scaled <- vals / mean(vals)
    scaled <- scaled / min(scaled)
    w[nb != 0] <- floor(scaled + 0.5)
  }
  gen <- function(remaining) {
    keep <- nb * remaining
    keep <- keep * rep(remaining, each = n)
    k <- rowSums(keep)
    s <- rowSums(w * keep)
    ifelse(k > 0, (k^alpha * s^beta)^(1 / (alpha + beta)), 0)
  }
  .cg_peel_threshold(nb, gen)
}

#' Renewed coreness (Liu, Tang, Zhou & Do 2015)
#'
#' Each link gets a diffusion importance
#' `D_ij = (|N(j) \ N[i]| + |N(i) \ N[j]|) / 2` (closed neighbourhoods,
#' as the paper's Figure 1 requires); links with `D_ij` below the
#' threshold (paper: 2) are removed and the k-core number of the residual
#' graph, with isolates at 0, is the renewed coreness.
#'
#' @param nb 0/1 symmetric neighbour matrix. @param threshold Default 2.
#' @return Integer vector.
#' @keywords internal
#' @noRd
.cg_renewed_coreness <- function(nb, threshold = 2) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(integer(0))
  common <- nb %*% nb                  # shared neighbours of i and j
  deg <- rowSums(nb)
  # Links of j that leave i's closed neighbourhood: j's degree less the
  # shared neighbours and less the link back to i.
  leave <- sweep(-common, 2, deg, "+") - 1
  d_ij <- (leave + t(leave)) / 2
  residual <- nb * (d_ij >= threshold)
  gen <- function(remaining) {
    keep <- residual * remaining
    rowSums(keep * rep(remaining, each = n))
  }
  .cg_peel_threshold(residual, gen)
}

#' Geodesic k-path centrality (Borgatti & Everett 2006)
#'
#' The number of shortest paths of length at most `k` that start at the
#' node, counted with multiplicity: `sum_{0 < d(i, j) <= k} sigma(i, j)`.
#'
#' @param nb 0/1 neighbour matrix in walking direction. @param d Matching
#'   hop-distance matrix. @param k Maximum length (default 3).
#' @return Numeric vector of path counts.
#' @keywords internal
#' @noRd
.cg_geodesic_kpath <- function(nb, d, k = 3) {
  n <- nrow(nb)
  if (is.null(n) || n == 0L) return(numeric(0))
  vapply(seq_len(n), function(i) {
    di <- d[i, ]
    sigma <- numeric(n)
    sigma[i] <- 1
    depth <- min(k, max(0, di[is.finite(di)]))
    # Shortest-path counts propagate layer by layer.
    for (l in seq_len(depth)) {
      prev <- which(di == l - 1)
      cur <- which(di == l)
      sigma[cur] <- as.numeric(crossprod(nb[prev, cur, drop = FALSE],
                                         sigma[prev]))
    }
    sum(sigma[di > 0 & di <= k])
  }, numeric(1L))
}
