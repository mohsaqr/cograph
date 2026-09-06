# ===========================================================================
# Random-walk and role-based centralities
# ===========================================================================

#' Stationary distribution of a directed transition matrix
#'
#' Taken as the eigenvector whose eigenvalue is nearest 1, the same selection
#' SALSA uses. A linear solve for the null space gives a different vector
#' whenever the chain is not irreducible, so the choice is not cosmetic.
#'
#' @param p Row-stochastic transition matrix. @param n Vertex count.
#' @return Numeric vector summing to 1, or `NULL` if it cannot be found.
#' @keywords internal
#' @noRd
.cg_stationary <- function(p, n) {
  e <- tryCatch(eigen(t(p)), error = function(e) NULL)
  if (is.null(e)) return(NULL)
  idx <- which.min(abs(Re(e$values) - 1))
  pi_v <- abs(Re(e$vectors[, idx]))
  s <- sum(pi_v)
  if (!(s > 0) || !is.finite(s)) return(NULL)
  pi_v / s
}

#' Mean first-passage-time centralities
#'
#' Markov, random-walk and second-order centrality all fall out of the same
#' fundamental matrix. A disconnected graph has no stationary distribution to
#' speak of, so all three are `NaN` rather than computed per component.
#'
#' @param w Weight matrix. @param n Vertex count. @param directed Whether directed.
#' @return A list with `markov`, `random_walk` and `second_order`.
#' @keywords internal
#' @noRd
.cg_mfpt <- function(w, n, directed) {
  nan <- rep(NaN, n)
  if (n <= 1L) return(list(markov = nan, random_walk = nan, second_order = nan))
  a <- (w != 0) * 1
  diag(a) <- 0
  if (.cg_n_components(a) > 1L)
    return(list(markov = nan, random_walk = nan, second_order = nan))
  deg <- rowSums(a)
  deg[deg == 0] <- 1
  p <- a / deg
  pi_v <- if (directed) .cg_stationary(p, n) else deg / sum(deg)
  if (is.null(pi_v) || any(!is.finite(pi_v)))
    return(list(markov = nan, random_walk = nan, second_order = nan))
  wmat <- matrix(pi_v, n, n, byrow = TRUE)
  z <- tryCatch(solve(diag(1, n, n) - p + wmat), error = function(e) NULL)
  if (is.null(z)) return(list(markov = nan, random_walk = nan, second_order = nan))
  zjj <- matrix(diag(z), n, n, byrow = TRUE)
  mfpt <- (zjj - z) / matrix(pi_v, n, n, byrow = TRUE)
  mfpt[matrix(pi_v, n, n, byrow = TRUE) <= 1e-15] <- 0
  diag(mfpt) <- 0
  rw_dist <- (mfpt + t(mfpt)) / 2
  diag(rw_dist) <- 0
  rs <- rowSums(rw_dist)
  random_walk <- ifelse(rs > 0, 1 / rs, NA_real_)
  col_means <- colMeans(mfpt)
  markov <- ifelse(col_means > 0, 1 / col_means, NA_real_)
  second_order <- vapply(seq_len(n), function(j) {
    times <- mfpt[seq_len(n) != j, j]
    times <- times[times > 0]
    if (length(times) < 2L) NaN else stats::sd(times)
  }, numeric(1L))
  list(markov = markov, random_walk = random_walk, second_order = second_order)
}

#' Gould-Fernandez brokerage roles
#'
#' Counts open two-paths `a -> v -> c` with no `a -> c` shortcut, classified
#' by how the three group memberships line up.
#'
#' The roles are defined by the direction of the two-path, so an undirected
#' graph has no brokerage structure to report.
#'
#' @param b Binary adjacency matrix.
#' @param membership Integer community labels, or `NULL`.
#' @param role One of `"coordinator"`, `"itinerant"`, `"representative"`,
#'   `"gatekeeper"`, `"liaison"`.
#' @param directed Whether the graph is directed.
#' @return Numeric vector; `NaN` without a partition, `NA` when undirected.
#' @keywords internal
#' @noRd
.cg_brokerage <- function(b, membership, role, directed = TRUE) {
  n <- nrow(b)
  if (!directed) return(rep(NA_real_, n))
  if (is.null(membership)) return(rep(NaN, n))
  bb <- (b != 0)
  out <- numeric(n)
  for (v in seq_len(n)) {
    a_set <- which(bb[, v])
    c_set <- which(bb[v, ])
    if (length(a_set) == 0L || length(c_set) == 0L) next
    pairs <- expand.grid(a = a_set, c = c_set, KEEP.OUT.ATTRS = FALSE)
    pairs <- pairs[pairs$a != pairs$c, , drop = FALSE]
    if (nrow(pairs) == 0L) next
    open <- !bb[cbind(pairs$a, pairs$c)]
    pairs <- pairs[open, , drop = FALSE]
    if (nrow(pairs) == 0L) next
    ga <- membership[pairs$a]; gv <- membership[v]; gc <- membership[pairs$c]
    r <- ifelse(ga == gv & gv == gc, "coordinator",
         ifelse(ga == gc & ga != gv, "itinerant",
         ifelse(ga == gv, "representative",
         ifelse(gv == gc, "gatekeeper", "liaison"))))
    out[v] <- sum(r == role)
  }
  out
}

#' Non-backtracking (Hashimoto) centrality
#'
#' The dominant eigenvector of the edge-to-edge non-backtracking operator,
#' folded back onto vertices. Built on directed edge pairs, so an undirected
#' graph is first split into its two arcs.
#'
#' @param w Weight matrix. @param n Vertex count. @param directed Whether directed.
#' @return Numeric vector scaled to a maximum of 1.
#' @keywords internal
#' @noRd
.cg_nonbacktracking <- function(w, n, directed) {
  if (n == 0L) return(numeric(0))
  if (n == 1L) return(1)
  a <- (w != 0) * 1
  diag(a) <- 0
  idx <- which(a != 0, arr.ind = TRUE)
  # With no edges the non-backtracking operator is empty and has no dominant
  # eigenvector; there is nothing to report.
  if (nrow(idx) == 0L) return(rep(NA_real_, n))
  if (directed) {
    edges <- idx
  } else {
    upper <- idx[idx[, 1L] < idx[, 2L], , drop = FALSE]
    # Reversed arcs are appended as a block, matching the reference ordering.
    edges <- rbind(upper, upper[, c(2L, 1L), drop = FALSE])
  }
  m <- nrow(edges)
  if (m == 0L) return(rep(0, n))
  bmat <- outer(seq_len(m), seq_len(m), function(x, y)
    as.numeric(edges[x, 2L] == edges[y, 1L] & edges[x, 1L] != edges[y, 2L]))
  e <- eigen(bmat)
  v <- abs(Re(e$vectors[, .cg_dominant_index(e$values)]))
  res <- numeric(n)
  for (k in seq_len(m)) res[edges[k, 1L]] <- res[edges[k, 1L]] + v[k]
  mx <- max(res)
  if (mx > 0) res / mx else res
}
