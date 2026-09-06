# ===========================================================================
# Batch 10 kernels — node measures other packages expose and cograph did not
#
# Each one closes a row of docs/CENTRALITY-CROSS-COVERAGE.md: local
# efficiency (brainGraph), s-core (Eidsaa & Almaas), distance-weighted
# fragmentation (keyplayer), the k-path census (sna) and the edge percolated
# component (centiserve, CINNA). Pure base-R kernels on matrices, validated
# in tests/testthat/test-centrality-batch10.R against those packages and
# against brute-force definitions. Conventions as in R/kernels-batch7.R.
# ===========================================================================

#' Local efficiency (Latora & Marchiori 2001, eq. 6)
#'
#' The global efficiency of the subgraph induced on a node's neighbours, the
#' node itself excluded: `E_loc(i) = mean over ordered pairs (j, l) of
#' neighbours of 1 / d_jl` measured *inside* that subgraph. A node with
#' fewer than two neighbours scores 0, since the average is over an empty
#' set of pairs. Detours through the rest of the graph do not count, which
#' is what makes the measure a local fault-tolerance statistic: it asks how
#' well a node's neighbourhood still communicates when the node is gone.
#'
#' @param m Numeric matrix. Row `i` holds the edges leaving `i`; symmetric
#'   for an undirected graph.
#' @return Numeric vector, one value per node.
#' @keywords internal
#' @noRd
.cg_local_efficiency <- function(m) {
  n <- nrow(m)
  if (is.null(n) || n == 0L) return(numeric(0))
  vapply(seq_len(n), function(i) {
    nbrs <- which(m[i, ] != 0)
    nbrs <- nbrs[nbrs != i]
    k <- length(nbrs)
    if (k < 2L) return(0)
    d <- .cg_distances(m[nbrs, nbrs, drop = FALSE], "out")
    off <- row(d) != col(d) & is.finite(d) & d > 0
    if (!any(off)) return(0)
    sum(1 / d[off]) / (k * (k - 1))
  }, numeric(1L))
}

#' s-core index (Eidsaa & Almaas 2013)
#'
#' The weighted generalisation of the k-core: the `s`-core is the maximal
#' subgraph in which every node has strength at least `s`, and a node's
#' s-core index is the largest `s` whose core still contains it. The peeling
#' below realises that definition directly -- prune everything below the
#' current smallest strength, repeat until the survivors are stable, then
#' raise the threshold to the new smallest strength. Unit weights make
#' strength equal degree, so the index reduces exactly to the k-core number.
#'
#' `brainGraph::s_core()` returns the peeling *round* rather than the
#' threshold, and short-circuits to `igraph::coreness()` on an unweighted
#' graph; the two agree on unweighted input and disagree on weighted input.
#'
#' @param m Numeric matrix of edge weights, symmetric.
#' @return Numeric vector of s-core indices; isolates score 0.
#' @keywords internal
#' @noRd
.cg_s_core <- function(m) {
  n <- nrow(m)
  if (is.null(n) || n == 0L) return(numeric(0))
  diag(m) <- 0
  alive <- colSums(m) > 0
  idx <- numeric(n)
  while (any(alive)) {
    s <- colSums(m)
    thr <- min(s[alive])
    repeat {
      drop <- alive & colSums(m) <= thr
      if (!any(drop)) break
      idx[drop] <- thr
      alive[drop] <- FALSE
      m[drop, ] <- 0
      m[, drop] <- 0
    }
  }
  idx
}

#' Distance-weighted fragmentation (Borgatti 2006, eq. 4)
#'
#' How badly the network falls apart when one node is taken out:
#' `F_d(v) = 1 - (sum over ordered pairs of the remaining nodes of 1 / d_ij)
#' / ((n - 1)(n - 2))`, distances measured after deleting `v`. Unreachable
#' pairs contribute nothing, so a node whose removal disconnects the network
#' scores high, and a node in a clique scores near 0. Higher is more
#' disruptive.
#'
#' @param m Numeric matrix. Row `i` holds the edges leaving `i`.
#' @param mode Direction for the distances, one of `"all"`, `"out"`, `"in"`.
#' @return Numeric vector; `NaN` when fewer than three nodes remain.
#' @keywords internal
#' @noRd
.cg_fragmentation <- function(m, mode = "all") {
  n <- nrow(m)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (n < 3L) return(rep(NaN, n))
  denom <- (n - 1) * (n - 2)
  vapply(seq_len(n), function(v) {
    d <- .cg_distances(m[-v, -v, drop = FALSE], mode)
    off <- row(d) != col(d) & is.finite(d) & d > 0
    1 - sum(1 / d[off]) / denom
  }, numeric(1L))
}

#' k-path census (Sade 1989; sna::kpath.census)
#'
#' The number of simple paths of length at most `k` that the node lies on,
#' endpoints included. On an undirected graph each path is counted once, in
#' the orientation whose first node has the smaller index; on a directed one
#' every direction counts separately. Length 1 therefore reproduces degree.
#'
#' Enumeration is exhaustive, so the cost grows with the branching factor to
#' the power `k`; the default `k = 3` is what the sna census uses.
#'
#' @param a 0/1 matrix. `a[i, j] = 1` when the path may step from `i` to `j`.
#' @param k Maximum path length.
#' @param directed Whether the two orientations of a path are distinct.
#' @return Numeric vector of path counts.
#' @keywords internal
#' @noRd
.cg_kpath_counts <- function(a, k = 3, directed = FALSE) {
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(numeric(0))
  counts <- numeric(n)
  nbrs <- lapply(seq_len(n), function(i) which(a[i, ] != 0))
  extend <- function(path) {
    len <- length(path) - 1L
    if (len >= 1L && (directed || path[1L] < path[len + 1L])) {
      counts[path] <<- counts[path] + 1
    }
    if (len >= k) return(NULL)
    nxt <- setdiff(nbrs[[path[len + 1L]]], path)
    lapply(nxt, function(v) extend(c(path, v)))
    NULL
  }
  lapply(seq_len(n), function(v) extend(v))
  counts
}

#' Connected-component label of every node
#'
#' Label propagation to a fixed point: each node takes the smallest label in
#' its closed neighbourhood until nothing moves, which leaves every
#' component labelled by its smallest member.
#'
#' @param a Logical or 0/1 symmetric matrix.
#' @return Integer vector of component labels.
#' @keywords internal
#' @noRd
.cg_component_labels <- function(a) {
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(integer(0))
  keep <- a != 0
  lab <- seq_len(n)
  repeat {
    spread <- matrix(lab, n, n, byrow = TRUE)
    spread[!keep] <- .Machine$integer.max
    nxt <- pmin(lab, apply(spread, 1L, min))
    if (identical(nxt, lab)) break
    lab <- nxt
  }
  lab
}

#' Edge percolated component (Lin et al. 2008)
#'
#' Bond percolation averaged over `runs` realisations: each edge survives
#' independently with probability `1 - threshold`, and the node's score is
#' the mean size of the component it lands in, as a share of the network.
#' The value is therefore in `[1/n, 1]` and does not move when `runs`
#' changes. cytoHubba and `centiserve::epc()` divide the same total by the
#' node count alone, so their number is `runs` times this one.
#'
#' The result is a Monte Carlo estimate: two calls agree only when the
#' random stream does.
#'
#' @param a 0/1 symmetric matrix.
#' @param threshold Edge removal probability. Default 0.5.
#' @param runs Number of realisations. Default 1000.
#' @return Numeric vector of expected component shares.
#' @keywords internal
#' @noRd
.cg_epc <- function(a, threshold = 0.5, runs = 1000) {
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(numeric(0))
  ends <- which(a != 0 & upper.tri(a), arr.ind = TRUE)
  m <- nrow(ends)
  if (m == 0L) return(rep(1 / n, n))
  total <- Reduce(`+`, lapply(seq_len(runs), function(r) {
    live <- stats::runif(m) >= threshold
    sub <- matrix(FALSE, n, n)
    if (any(live)) {
      e <- ends[live, , drop = FALSE]
      sub[e] <- TRUE
      sub[e[, c(2L, 1L), drop = FALSE]] <- TRUE
    }
    lab <- .cg_component_labels(sub)
    tabulate(lab, nbins = n)[lab]
  }), numeric(n))
  total / (runs * n)
}
