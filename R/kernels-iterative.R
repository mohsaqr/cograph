# ===========================================================================
# Iterative and peeling centralities, dependency-free
# ===========================================================================
# Power iterations and k-core peeling. igraph's scaling conventions are
# reproduced exactly: eigenvector, hub and authority are all rescaled so the
# largest absolute value is 1.

#' Index of the dominant eigenvalue
#'
#' Maximum modulus, tie-broken by largest real part. Neither half is
#' sufficient alone: on a signed matrix the spectral radius can be negative,
#' so selecting by real part is wrong; on a *periodic* directed graph (a
#' 3-cycle has eigenvalues 1, w, w^2, all of modulus 1) selecting by modulus
#' alone can land on a complex eigenvalue, so the tie-break is what recovers
#' the real Perron root.
#'
#' @param values Eigenvalue vector, possibly complex.
#' @return An integer index.
#' @keywords internal
#' @noRd
.cg_dominant_index <- function(values) {
  mods <- Mod(values)
  top <- max(mods)
  cand <- which(mods >= top - 1e-9 * max(top, 1))
  cand[which.max(Re(values)[cand])]
}

#' Rescale so the largest absolute entry is 1, sign-fixed positive
#' @param v Numeric vector.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_unit_max <- function(v) {
  i <- which.max(abs(v))
  if (length(i) == 0L) return(v)
  if (v[i] < 0) v <- -v
  m <- abs(v[i])
  if (m > 1e-15) v / m else v
}

#' PageRank by power iteration
#'
#' Dangling vertices redistribute their mass uniformly, which is igraph's
#' behaviour and the reason a sink does not silently drain the ranking.
#'
#' @param w Weight matrix. @param n Vertex count.
#' @param damping Damping factor. @param max_iter Iteration cap.
#' @param tol L1 convergence tolerance.
#' @param personalized Optional reset distribution; uniform when `NULL`.
#' @return Numeric vector summing to 1.
#' @keywords internal
#' @noRd
.cg_pagerank <- function(w, n, damping = 0.85, max_iter = 1000L, tol = 1e-14,
                         personalized = NULL) {
  if (n == 0L) return(numeric(0))
  # A negative weight makes the random walk meaningless and the iteration
  # diverges rather than failing; the reference refuses outright, so do the
  # same instead of returning a confident 1e137.
  if (any(w < 0, na.rm = TRUE)) {
    stop(errorCondition(
      "PageRank needs non-negative weights; found a negative edge.",
      class = "cograph_negative_weights", call = NULL))
  }
  reset <- if (is.null(personalized)) rep(1 / n, n) else {
    p <- as.numeric(personalized)
    if (length(p) != n || any(p < 0) || sum(p) <= 0) {
      stop(errorCondition(
        "`personalized` must be a non-negative vector of length n with a positive sum.",
        class = "cograph_bad_input", call = NULL))
    }
    p / sum(p)
  }
  outdeg <- rowSums(w)
  live <- outdeg > 0
  pr <- rep(1 / n, n)
  # Each sweep depends on the previous vector, so iteration is sequential.
  for (iter in seq_len(max_iter)) {
    dangling <- sum(pr[!live])
    new_pr <- (1 - damping) * reset + damping * dangling * reset
    if (any(live)) {
      contrib <- pr[live] / outdeg[live]
      new_pr <- new_pr + damping * as.numeric(crossprod(w[live, , drop = FALSE], contrib))
    }
    converged <- sum(abs(new_pr - pr)) < tol
    pr <- new_pr
    if (converged) break
  }
  pr
}

#' Eigenvector centrality by eigendecomposition
#'
#' Power iteration is not safe here: on a bipartite graph (a star, say) it
#' oscillates between the two sides and never settles, so a converged-looking
#' answer can be badly wrong. A direct eigendecomposition has no such failure
#' mode, which is why igraph reaches for ARPACK rather than iterating.
#'
#' @param w Weight matrix. @param n Vertex count.
#' @return Numeric vector scaled to a maximum of 1.
#' @keywords internal
#' @noRd
.cg_eigenvector <- function(w, n) {
  if (n == 0L) return(numeric(0))
  if (all(w == 0)) return(rep(1, n))
  sym <- isSymmetric(unname(w))
  # A^T v = lambda v: incoming edges confer standing, matching igraph.
  e <- eigen(if (sym) w else t(w), symmetric = sym)
  k <- .cg_dominant_index(e$values)
  v <- Re(e$vectors[, k])
  v[abs(v) < 1e-12] <- 0
  .cg_unit_max(v)
}

#' HITS hub and authority scores
#'
#' Hubs are the dominant eigenvector of \eqn{A A^T} and authorities of
#' \eqn{A^T A}; taking them from an eigendecomposition avoids the same
#' bipartite oscillation that defeats power iteration.
#'
#' @param w Weight matrix. @param n Vertex count.
#' @return A list with `hub` and `authority`, each scaled to a maximum of 1.
#' @keywords internal
#' @noRd
.cg_hits <- function(w, n) {
  if (n == 0L) return(list(hub = numeric(0), authority = numeric(0)))
  if (all(w == 0)) return(list(hub = rep(1, n), authority = rep(1, n)))
  dom <- function(m) {
    e <- eigen(m, symmetric = TRUE)
    v <- Re(e$vectors[, which.max(Re(e$values))])
    v[abs(v) < 1e-12] <- 0
    .cg_unit_max(v)
  }
  list(hub = dom(tcrossprod(w)), authority = dom(crossprod(w)))
}

#' Coreness by k-core peeling
#'
#' Matches `igraph::coreness(mode = )`. Under `"all"` on a directed graph a
#' reciprocated dyad contributes to the degree twice, which is why the
#' directed branch adds both directions rather than collapsing first.
#' Direction must be supplied, not inferred: a complete directed graph has a
#' symmetric adjacency.
#'
#' @param b Binary adjacency matrix with a zero diagonal.
#' @param n Vertex count.
#' @param directed Whether the graph is directed.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_coreness <- function(b, n, directed = FALSE, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  if (n == 0L) return(numeric(0))
  bb <- (b != 0) * 1
  diag(bb) <- 0
  deg <- .cg_degree(bb, directed, mode)
  core <- numeric(n)
  removed <- rep(FALSE, n)
  left <- n
  # Peeling is inherently sequential: each removal changes the degrees that
  # decide the next removal.
  while (left > 0L) {
    mn <- min(deg[!removed])
    repeat {
      take <- which(!removed & deg <= mn)
      if (length(take) == 0L) break
      for (i in take) {
        if (removed[i]) next
        removed[i] <- TRUE
        core[i] <- mn
        left <- left - 1L
        alive <- !removed
        # Losing vertex i costs j whichever incident arcs this mode counts.
        loss <- switch(mode,
          out = (bb[alive, i] != 0),                 # j -> i disappears
          `in` = (bb[i, alive] != 0),                # i -> j disappears
          all = if (!directed) (bb[i, alive] != 0)
                else (bb[i, alive] != 0) + (bb[alive, i] != 0))
        deg[alive] <- deg[alive] - loss
      }
    }
  }
  core
}
