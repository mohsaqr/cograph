# ===========================================================================
# Local and neighbourhood centralities, dependency-free
# ===========================================================================

#' Degree and strength under igraph's mode and loop semantics
#'
#' Two conventions have to be honoured at once:
#'
#' * For a directed graph `mode = "all"` is in + out, so a reciprocated dyad
#'   counts twice. Direction is passed in rather than inferred, because a
#'   complete directed graph has a symmetric adjacency.
#' * A **self-loop counts twice** whenever both of its endpoints are being
#'   counted -- undirected degree, and directed `mode = "all"` -- but once
#'   under `"out"` or `"in"`. Treating the diagonal as an ordinary entry
#'   undercounts every vertex carrying a loop.
#'
#' @param b Binary adjacency matrix (the diagonal is read, not assumed zero).
#' @param directed Whether the graph is directed.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @param loops Whether self-loops are counted at all.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_degree <- function(b, directed, mode = c("all", "out", "in"),
                       loops = TRUE) {
  mode <- match.arg(mode)
  bin <- (b != 0) * 1
  loop <- if (loops) diag(bin) else rep(0, nrow(bin))
  off <- bin
  diag(off) <- 0
  out <- rowSums(off)
  inn <- colSums(off)
  if (!directed) return(out + 2 * loop)
  switch(mode,
    out = out + loop,
    `in` = inn + loop,
    all = out + inn + 2 * loop)
}

#' @param w Weight matrix (the diagonal is read, not assumed zero).
#' @rdname dot-cg_degree
#' @keywords internal
#' @noRd
.cg_strength <- function(w, directed, mode = c("all", "out", "in"),
                         loops = TRUE) {
  mode <- match.arg(mode)
  loop <- if (loops) diag(w) else rep(0, nrow(w))
  off <- w
  diag(off) <- 0
  out <- rowSums(off)
  inn <- colSums(off)
  if (!directed) return(out + 2 * loop)
  switch(mode,
    out = out + loop,
    `in` = inn + loop,
    all = out + inn + 2 * loop)
}

#' Burt's constraint
#'
#' igraph folds the graph to undirected by summing both directions before
#' forming the proportional weights. An isolate has no ego network at all, so
#' it is reported as `NaN` rather than 0.
#'
#' @param w Weight matrix. @param n Vertex count.
#' @return Numeric vector.
#' @references Burt, R. S. (1992). *Structural Holes*. Harvard University Press.
#' @keywords internal
#' @noRd
.cg_constraint <- function(w, n) {
  if (n == 0L) return(numeric(0))
  uw <- w + t(w)
  diag(uw) <- 0
  strength <- rowSums(uw)
  p <- ifelse(strength > 0, 1, 0) * (uw / ifelse(strength > 0, strength, 1))
  # p[i, q] %*% p[q, j] with q equal to i or j excluded from the inner sum.
  indirect <- p %*% p
  vapply(seq_len(n), function(i) {
    if (strength[i] == 0) return(NaN)
    j_set <- which(p[i, ] != 0)
    j_set <- j_set[j_set != i]
    if (length(j_set) == 0L) return(0)
    total <- vapply(j_set, function(j) {
      ind <- indirect[i, j] - p[i, i] * p[i, j] - p[i, j] * p[j, j]
      p[i, j] + ind
    }, numeric(1L))
    sum(total^2)
  }, numeric(1L))
}

#' Leverage centrality (Joyce et al. 2010)
#'
#' Degrees and the neighbour set are both taken at `mode`; using the
#' undirected reading for either would silently answer a different question
#' on a directed graph.
#'
#' @param w Weight matrix. @param n Vertex count.
#' @param directed Whether directed. @param mode One of `"all"`, `"out"`, `"in"`.
#' @return Numeric vector; `NaN` for isolates.
#' @keywords internal
#' @noRd
.cg_leverage <- function(w, n, directed = FALSE, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  if (n == 0L) return(numeric(0))
  b <- (w != 0) * 1
  diag(b) <- 0
  deg <- .cg_degree(b, directed, mode)
  adj <- if (!directed) ((b + t(b)) != 0)
         else switch(mode, out = b != 0, `in` = t(b) != 0,
                     all = ((b + t(b)) != 0))
  vapply(seq_len(n), function(i) {
    if (deg[i] == 0) return(NaN)
    j_set <- which(adj[i, ])
    j_set <- j_set[j_set != i]
    if (length(j_set) == 0L) return(NaN)
    denom <- deg[i] + deg[j_set]
    mean(ifelse(denom == 0, 0, (deg[i] - deg[j_set]) / denom))
  }, numeric(1L))
}

#' Local transitivity (clustering coefficient)
#'
#' The denominator uses igraph's `mode = "all"` degree, so on a directed
#' graph a reciprocated dyad counts twice. That is why a vertex with a single
#' reciprocated neighbour scores 0 rather than `NaN`: it has degree two, and
#' therefore a triple that simply is not closed. Triangles are counted over
#' the distinct neighbour set.
#'
#' @param b Binary adjacency matrix. @param n Vertex count.
#' @param directed Whether the graph is directed.
#' @return Numeric vector; `NaN` where the vertex has fewer than two edges.
#' @keywords internal
#' @noRd
.cg_local_transitivity <- function(b, n, directed = FALSE) {
  if (n == 0L) return(numeric(0))
  bb <- (b != 0) * 1
  diag(bb) <- 0
  u <- ((bb + t(bb)) != 0) * 1
  diag(u) <- 0
  # Directed: in + out, so a reciprocated dyad counts twice. Undirected: the
  # neighbour count. Taking out-degree here would let an asymmetric matrix
  # read as undirected produce a coefficient above 1.
  k <- if (directed) rowSums(bb) + colSums(bb) else rowSums(u)
  vapply(seq_len(n), function(i) {
    if (k[i] < 2L) return(NaN)
    nbs <- which(u[i, ] != 0)
    if (length(nbs) < 2L) return(0)
    sub <- u[nbs, nbs, drop = FALSE]
    2 * (sum(sub) / 2) / (k[i] * (k[i] - 1))
  }, numeric(1L))
}
