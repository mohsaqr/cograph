# Native graph context for the centrality surface.
#
# `.cg_graph()` turns any supported input into a dense, labelled weight
# matrix plus a canonical edge list, without igraph for matrix,
# cograph_network, tna, netobject and edge-list input. Every centrality
# kernel reads from this object. `.cg_igraph()` is the only sanctioned way
# to obtain an igraph object from it; it exists so the remaining igraph
# dependence can be found by setting `options(cograph.forbid_igraph = TRUE)`.
#
# Conventions reproduced from igraph::graph_from_adjacency_matrix(): edges
# are ordered row-major over non-zero cells (upper triangle including the
# diagonal when undirected); an undirected reading of an asymmetric matrix
# takes the element-wise maximum; a loop is one edge whose weight sits on
# the diagonal and which degree-type measures count twice.

#' Build the native graph context
#'
#' @param x Matrix, cograph_network, tna, network, igraph or edge-list data
#'   frame.
#' @param directed NULL to detect, or TRUE/FALSE to force.
#' @param loops Keep self-loops? FALSE zeroes the diagonal.
#' @param simplify How duplicate edges (only possible from edge-list,
#'   cograph_network or igraph input) are combined: "sum", "mean", "max",
#'   "min". FALSE or "none" also sum them: a dense matrix cannot hold
#'   parallel edges.
#' @return An environment of class `cg_graph` with fields `n`, `directed`,
#'   `labels`, `has_names`, `w` (weights), `b` (binary), `edges` (integer
#'   matrix, canonical order), `weights` (numeric vector in that order, or
#'   NULL for unweighted igraph input), `weighted`.
#' @keywords internal
#' @noRd
.cg_graph <- function(x, directed = NULL, loops = TRUE, simplify = "sum") {
  stopifnot(
    "`directed` must be NULL or a single logical" =
      is.null(directed) || (is.logical(directed) && length(directed) == 1L && !is.na(directed)),
    "`loops` must be a single logical" = is.logical(loops) && length(loops) == 1L && !is.na(loops)
  )
  src <- .cg_graph_source(x, directed)
  acc <- .cg_accumulate(src$from, src$to, src$weight, src$n, src$directed, simplify)
  w <- acc$w
  if (!loops) diag(w) <- 0
  labels <- src$labels %||% as.character(seq_len(src$n))
  if (src$n > 0L) dimnames(w) <- list(labels, labels)
  .cg_graph_from_matrix(w, src$directed, labels, src$has_names, src$weighted)
}

.cg_graph_from_matrix <- function(w, directed, labels, has_names, weighted = TRUE) {
  n <- nrow(w)
  present <- is.na(w) | w != 0
  nz <- which(present, arr.ind = TRUE)
  if (!directed && nrow(nz)) nz <- nz[nz[, 1L] <= nz[, 2L], , drop = FALSE]
  if (nrow(nz)) nz <- nz[order(nz[, 1L], nz[, 2L]), , drop = FALSE]
  edges <- matrix(as.integer(nz), ncol = 2L, dimnames = NULL)
  ctx <- new.env(parent = emptyenv())
  ctx$n <- n
  ctx$directed <- directed
  ctx$labels <- labels
  ctx$has_names <- has_names
  # The matrices carry no dimnames: labels live in `labels`, and kernels
  # then return unnamed vectors exactly as the igraph-backed code did.
  dimnames(w) <- NULL
  ctx$w <- w
  ctx$b <- unname(present * 1)
  ctx$edges <- edges
  ctx$weighted <- weighted
  ctx$weights <- if (weighted && nrow(edges)) w[edges] else if (weighted) numeric(0) else NULL
  ctx$cache <- new.env(parent = emptyenv())
  class(ctx) <- "cg_graph"
  ctx
}

# Resolve any input to (from, to, weight, n, labels, directed, weighted).
.cg_graph_source <- function(x, directed) {
  if (inherits(x, "igraph")) {
    if (!requireNamespace("igraph", quietly = TRUE)) { # nocov start
      stop("Package 'igraph' is required for igraph input", call. = FALSE)
    } # nocov end
    el <- igraph::as_edgelist(x, names = FALSE)
    weighted <- igraph::is_weighted(x)
    wv <- if (weighted) as.numeric(igraph::E(x)$weight) else rep(1, nrow(el))
    src_dir <- igraph::is_directed(x)
    labels <- igraph::vertex_attr(x, "name")
    return(.cg_resolve_direction(el[, 1L], el[, 2L], wv, igraph::vcount(x), labels,
                                 src_dir, directed, weighted))
  }
  if (inherits(x, "cograph_network")) {
    edges <- get_edges(x)
    nodes <- get_nodes(x)
    return(.cg_resolve_direction(edges$from, edges$to, as.numeric(edges$weight),
                                 nrow(nodes), as.character(nodes$label),
                                 isTRUE(x$directed), directed, TRUE))
  }
  if (is.data.frame(x)) {
    parsed <- parse_edgelist(x, directed = directed)
    return(.cg_resolve_direction(parsed$edges$from, parsed$edges$to,
                                 as.numeric(parsed$edges$weight),
                                 nrow(parsed$nodes), as.character(parsed$nodes$name),
                                 parsed$directed, NULL, TRUE))
  }
  if (inherits(x, "network")) {
    if (!requireNamespace("network", quietly = TRUE)) { # nocov start
      stop("Package 'network' is required for network input", call. = FALSE)
    } # nocov end
    attrname <- if ("weight" %in% network::list.edge.attributes(x)) "weight" else NULL
    m <- network::as.matrix.network(x, matrix.type = "adjacency", attrname = attrname)
    storage.mode(m) <- "double"
    labels <- network::network.vertex.names(x)
    if (is.null(labels) || all(is.na(labels))) labels <- NULL
    dimnames(m) <- NULL
    src_dir <- if (!is.null(directed)) directed else network::is.directed(x)
    return(.cg_matrix_source(m, labels, src_dir, directed))
  }
  if (inherits(x, "tna")) {
    m <- x$weights
    labels <- x$labels %||% rownames(m)
    dimnames(m) <- NULL
    return(.cg_matrix_source(m, labels, NULL, directed))
  }
  if (is.matrix(x)) {
    stopifnot("`x` must be a square numeric matrix" = is.numeric(x) && nrow(x) == ncol(x))
    labels <- rownames(x)
    m <- unname(x)
    storage.mode(m) <- "double"
    return(.cg_matrix_source(m, labels, NULL, directed))
  }
  stop("x must be a matrix, data.frame edge list, igraph, network, cograph_network, or tna object",
       call. = FALSE)
}

# A matrix is already a collapsed graph: read its non-zero cells as edges.
# `src_dir` NULL means "detect by symmetry"; `directed` is the user override.
.cg_matrix_source <- function(m, labels, src_dir, directed) {
  n <- nrow(m)
  if (anyNA(m)) stop("adjacency matrix contains NA", call. = FALSE)
  detected <- if (!is.null(src_dir)) src_dir else (n > 0L && !isSymmetric(m))
  want <- if (!is.null(directed)) directed else detected
  if (!want && detected) {
    # igraph reads an asymmetric matrix as undirected with IGRAPH_ADJ_MAX.
    m <- pmax(m, t(m))
  }
  nz <- which(m != 0, arr.ind = TRUE)
  if (!want && nrow(nz)) {
    # Undirected: one edge per unordered pair, read from the upper triangle.
    nz <- nz[nz[, 1L] <= nz[, 2L], , drop = FALSE]
  }
  list(from = nz[, 1L], to = nz[, 2L], weight = m[nz], n = n, labels = labels,
       has_names = !is.null(labels), directed = want, weighted = TRUE)
}

# Edge-list sources: apply the direction override the way to_igraph() did
# (as_undirected collapse sums, as.directed mutual mirrors).
.cg_resolve_direction <- function(from, to, weight, n, labels, src_dir, directed, weighted) {
  want <- if (!is.null(directed)) directed else src_dir
  if (want && !src_dir) {
    keep <- from != to
    from2 <- c(from, to[keep]); to2 <- c(to, from[keep]); weight2 <- c(weight, weight[keep])
    from <- from2; to <- to2; weight <- weight2
  }
  list(from = as.integer(from), to = as.integer(to), weight = weight, n = n,
       labels = labels, has_names = !is.null(labels), directed = want, weighted = weighted)
}

# Accumulate an edge list into a dense matrix, combining duplicates.
.cg_accumulate <- function(from, to, weight, n, directed, simplify) {
  w <- matrix(0, n, n)
  if (!length(from)) return(list(w = w))
  # Non-finite weights are kept so that each measure raises its own
  # validation error, as before the port.
  if (!directed) {
    lo <- pmin(from, to); hi <- pmax(from, to)
    from <- lo; to <- hi
  }
  key <- (to - 1) * n + from
  if (anyDuplicated(key) > 0L) {
    # A dense weight matrix holds one value per cell, so parallel edges are
    # always combined. FALSE / "none" fall back to summation, which is what
    # the old adapters did when assembling an adjacency from a multigraph.
    if (isFALSE(simplify) || identical(simplify, "none")) simplify <- "sum"
    simplify <- match.arg(simplify, c("sum", "mean", "max", "min"))
    comb <- switch(simplify, sum = sum, mean = mean, max = max, min = min)
    acc <- vapply(split(weight, key), comb, numeric(1))
    w[as.numeric(names(acc))] <- acc
  } else {
    w[key] <- weight
  }
  if (!directed) {
    tw <- t(w)
    lower <- lower.tri(w)
    w[lower] <- tw[lower]
  }
  list(w = w)
}

#' Sanctioned bridge to igraph during the removal
#'
#' Builds (once, cached) the igraph object that the old code path would have
#' seen. Refuses when `options(cograph.forbid_igraph = TRUE)`, which is how
#' the test suite lists every measure still depending on igraph.
#' @keywords internal
#' @noRd
.cg_igraph <- function(ctx) {
  if (inherits(ctx, "igraph")) return(ctx)
  stopifnot("expected a cg_graph context" = inherits(ctx, "cg_graph"))
  if (isTRUE(getOption("cograph.forbid_igraph"))) {
    stop(errorCondition(
      "igraph reached from the native centrality path",
      class = "cograph_igraph_leak", call = NULL))
  }
  if (!is.null(ctx$cache$g)) return(ctx$cache$g)
  if (!requireNamespace("igraph", quietly = TRUE)) { # nocov start
    stop("Package 'igraph' is required for this measure", call. = FALSE)
  } # nocov end
  g <- igraph::graph_from_adjacency_matrix(
    ctx$w, mode = if (ctx$directed) "directed" else "undirected", weighted = TRUE)
  if (!ctx$weighted) g <- igraph::delete_edge_attr(g, "weight")
  if (ctx$has_names) igraph::V(g)$name <- ctx$labels
  ctx$cache$g <- g
  g
}

#' Memoise a per-context computation
#' @keywords internal
#' @noRd
.cg_memo <- function(ctx, key, expr) {
  if (!inherits(ctx, "cg_graph")) return(expr)
  if (!is.null(ctx$cache[[key]])) return(ctx$cache[[key]])
  val <- expr
  ctx$cache[[key]] <- val
  val
}

#' Dense matrix of the path weights in force
#'
#' `centrality()` carries path weights as a vector in canonical edge order,
#' possibly inverted for path-based measures. This assembles them into the
#' matrix the kernels expect. Accepts an igraph object for the code that has
#' not been ported yet.
#' @param g A `cg_graph` context or an igraph object.
#' @param weights Edge weight vector in canonical order, or NULL for unweighted.
#' @return A numeric adjacency matrix.
#' @keywords internal
#' @noRd
.cg_path_matrix <- function(g, weights = NULL) {
  if (inherits(g, "cg_graph")) {
    n <- g$n
    m <- matrix(0, n, n)
    if (n == 0L || nrow(g$edges) == 0L) return(m)
    w <- if (is.null(weights)) rep(1, nrow(g$edges)) else as.numeric(weights)
    stopifnot("weights must match the edge count" = length(w) == nrow(g$edges))
    m[g$edges] <- w
    if (!g$directed) m[g$edges[, 2:1, drop = FALSE]] <- w
    return(m)
  }
  n <- igraph::vcount(g)
  m <- matrix(0, n, n)
  if (n == 0L || igraph::ecount(g) == 0L) return(m)
  el <- igraph::as_edgelist(g, names = FALSE)
  w <- if (is.null(weights)) rep(1, nrow(el)) else as.numeric(weights)
  key <- (el[, 2L] - 1L) * n + el[, 1L]
  acc <- vapply(split(w, key), max, numeric(1))
  m[as.numeric(names(acc))] <- acc
  if (!igraph::is_directed(g)) {
    keep <- m > t(m)
    m[!keep] <- t(m)[!keep]
  }
  m
}

#' Hop-distance matrix under a mode (memoised on a context)
#' @keywords internal
#' @noRd
.cg_hop_distances <- function(g, mode = "all") {
  .cg_memo(g, paste0("hop_", mode), .cg_distances(.cg_path_matrix(g, NULL), mode))
}

#' Print method so a context does not dump its environment
#' @export
#' @return `x`, invisibly.
#' @noRd
print.cg_graph <- function(x, ...) {
  cat(sprintf("<cg_graph: %d nodes, %d edges, %s%s>\n", x$n, nrow(x$edges),
              if (x$directed) "directed" else "undirected",
              if (x$weighted) ", weighted" else ""))
  invisible(x)
}

#' Guard for the one measure family deliberately left on igraph
#' @keywords internal
#' @noRd
.cg_need_igraph <- function(what) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(errorCondition(
      sprintf("`%s` requires the igraph package, which is not installed", what),
      class = "cograph_needs_igraph", call = NULL))
  }
  invisible(TRUE)
}
