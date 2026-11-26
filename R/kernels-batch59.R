# Three measures defined on all simple paths or all simple cycles of a
# network. Their cost grows exponentially with the size and density of the
# network, so every enumeration counts its steps and stops with a classed
# condition once `limit` steps were taken.

#' Signal that an enumeration passed its step limit
#' @return Does not return.
#' @keywords internal
#' @noRd
.cg_stop_enumeration <- function() {
  stop(errorCondition("enumeration limit reached",
                      class = "cograph_enumeration_limit", call = NULL))
}

#' Validate the step limit of an enumeration
#' @param limit Proposed limit.
#' @return `limit`, invisibly.
#' @keywords internal
#' @noRd
.cg_check_enumeration_limit <- function(limit) {
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) ||
        limit < 1) {
    .cg_stop_bad_parameter(
      "`enumeration_limit` must be a single number of at least one, or Inf")
  }
  invisible(limit)
}

#' All cycle betweenness (Zhou, Liang, Zhao and Zhang 2018)
#'
#' Equation (10), page 7, of Zhou, Liang, Zhao and Zhang (2018), "Cycle
#' based network centrality", *Scientific Reports* 8:11749:
#'
#' ```
#' C_A(i) = sum_{k = 3}^{n} alpha^(k - 2) s_k(i),      alpha in (0, 1)
#' ```
#'
#' with `s_k(i)` the number of simple cycles of `k` edges through `i`. A
#' cycle is counted once, whatever its starting node and direction, as the
#' paper's Example 3 requires (its Fig. 1 has exactly three cycles).
#'
#' Each cycle is found from its lowest-numbered node by a depth-first
#' search through higher-numbered nodes only, once in each direction, and
#' is credited with half its weight each time.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param alpha Discount per edge, a single number in `(0, 1)`.
#' @param limit Largest number of search steps.
#' @return Numeric vector; raises `cograph_enumeration_limit` when the
#'   search is cut short.
#' @keywords internal
#' @noRd
.cg_all_cycle_betweenness <- function(b, alpha = 0.5, limit = 1e6) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha <= 0 || alpha >= 1) {
    .cg_stop_bad_parameter(
      "`cycle_alpha` must be a single number strictly between 0 and 1")
  }
  .cg_check_enumeration_limit(limit)
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  neighbors <- lapply(seq_len(n), \(v) which(b[v, ] != 0))
  score <- numeric(n)
  steps <- 0
  extend <- function(start, path, on_path) {
    steps <<- steps + 1
    if (steps > limit) .cg_stop_enumeration()
    here <- path[length(path)]
    # Depth-first enumeration: each branch extends the one before it and
    # all of them add to the same score vector.
    for (nxt in neighbors[[here]]) {
      if (nxt == start && length(path) >= 3L) {
        score[path] <<- score[path] + alpha^(length(path) - 2) / 2
      } else if (nxt > start && !on_path[nxt]) {
        on_path[nxt] <- TRUE
        extend(start, c(path, nxt), on_path)
        on_path[nxt] <- FALSE
      }
    }
  }
  for (start in seq_len(n)) {
    on_path <- logical(n)
    on_path[start] <- TRUE
    extend(start, start, on_path)
  }
  score
}

#' Transportation centrality (Piraveenan and Saripada 2023)
#'
#' Equations (8)-(10) of Piraveenan and Saripada (2023), *IEEE Access*
#' 11:142214-142234 (read in the early-access version):
#'
#' ```
#' TC(v)    = 1 / ((N - 1)(N - 2)) sum_{s != v != t} Q^v_{s,t}
#' Q^v_{s,t} = sum_{i in P^v_{s,t}} exp(-beta C_i) / sum_{j in P_{s,t}} exp(-beta C_j)
#' ```
#'
#' with `P_{s,t}` all simple paths from `s` to `t`, `P^v_{s,t}` those
#' through `v` and `C` the cost of a path, here its number of edges, as in
#' the paper's worked example. The pairs are ordered. A pair without a
#' path adds nothing and the divisor stays `(N - 1)(N - 2)`, the
#' convention of the authors' program; the paper does not address it.
#'
#' Every prefix of a depth-first path from `s` is a simple path from `s`
#' to its last node, so one search per source visits each simple path
#' exactly once.
#'
#' @param a Binary adjacency matrix with a zero diagonal, `a[i, j]` for the
#'   arc `i -> j`.
#' @param beta Sensitivity to path cost, a single finite nonnegative
#'   number.
#' @param limit Largest number of search steps.
#' @return Numeric vector, `NA` when the network has fewer than three
#'   nodes; raises `cograph_enumeration_limit` when the search is cut
#'   short.
#' @keywords internal
#' @noRd
.cg_transportation <- function(a, beta = 0.1, limit = 1e6) {
  if (!is.numeric(beta) || length(beta) != 1L || !is.finite(beta) ||
        beta < 0) {
    .cg_stop_bad_parameter(
      "`transport_beta` must be a single finite nonnegative number")
  }
  .cg_check_enumeration_limit(limit)
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (n < 3L) return(rep(NA_real_, n))
  neighbors <- lapply(seq_len(n), \(v) which(a[v, ] != 0))
  total <- numeric(n)
  steps <- 0
  through <- NULL
  all_paths <- NULL
  extend <- function(path, on_path) {
    steps <<- steps + 1
    if (steps > limit) .cg_stop_enumeration()
    here <- path[length(path)]
    # Depth-first enumeration with shared accumulators, as above.
    for (nxt in neighbors[[here]]) {
      if (!on_path[nxt]) {
        weight <- exp(-beta * length(path))
        all_paths[nxt] <<- all_paths[nxt] + weight
        inner <- path[-1L]
        through[nxt, inner] <<- through[nxt, inner] + weight
        on_path[nxt] <- TRUE
        extend(c(path, nxt), on_path)
        on_path[nxt] <- FALSE
      }
    }
  }
  for (source in seq_len(n)) {
    through <- matrix(0, n, n)
    all_paths <- numeric(n)
    on_path <- logical(n)
    on_path[source] <- TRUE
    extend(source, on_path)
    reached <- all_paths > 0
    total <- total + colSums(through[reached, , drop = FALSE] /
                               all_paths[reached])
  }
  total / ((n - 1) * (n - 2))
}

#' Path-transfer centrality (Tutzauer 2007)
#'
#' Tutzauer (2007), *Social Networks* 29(2):249-265. The original was not
#' read; the definition is the one Oggier, Phetsouvanh and Datta (2019),
#' *PeerJ Computer Science* 5:e220, restate in their equation (1) and
#' their Figs. 1 and 2. A flow starts at `u` and never visits a node
#' twice. At a node with `m` unvisited out-neighbors it stops with
#' probability `1 / (m + 1)` and moves to each of them with the same
#' probability. With `p_uv` the probability that it stops at `v`,
#'
#' ```
#' C_H(u) = - sum_v p_uv log2 p_uv.
#' ```
#'
#' @param a Binary adjacency matrix with a zero diagonal, `a[i, j]` for the
#'   arc `i -> j`.
#' @param limit Largest number of search steps.
#' @return Numeric vector of entropies in bits; raises
#'   `cograph_enumeration_limit` when the search is cut short.
#' @keywords internal
#' @noRd
.cg_path_transfer <- function(a, limit = 1e6) {
  .cg_check_enumeration_limit(limit)
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(numeric(0))
  neighbors <- lapply(seq_len(n), \(v) which(a[v, ] != 0))
  steps <- 0
  stopped <- NULL
  extend <- function(here, on_path, probability) {
    steps <<- steps + 1
    if (steps > limit) .cg_stop_enumeration()
    open <- neighbors[[here]][!on_path[neighbors[[here]]]]
    share <- probability / (length(open) + 1)
    stopped[here] <<- stopped[here] + share
    # Depth-first enumeration with a shared accumulator, as above.
    for (nxt in open) {
      on_path[nxt] <- TRUE
      extend(nxt, on_path, share)
      on_path[nxt] <- FALSE
    }
  }
  vapply(seq_len(n), \(origin) {
    stopped <<- numeric(n)
    on_path <- logical(n)
    on_path[origin] <- TRUE
    extend(origin, on_path, 1)
    positive <- stopped[stopped > 0]
    -sum(positive * log2(positive))
  }, numeric(1))
}
