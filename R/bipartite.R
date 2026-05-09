#' Project Bipartite Network to One-Mode
#'
#' Projects a two-mode (bipartite/incidence) network into a one-mode adjacency
#' matrix. Row-mode projection yields a matrix of shared-column connections
#' among row nodes; column-mode projection does the converse.
#'
#' @param x An incidence matrix (rows = type 1 nodes, columns = type 2 nodes)
#'   where non-zero entries indicate connections. Can also be a data.frame with
#'   columns \code{type1}, \code{type2}, and optionally \code{weight}.
#' @param mode Character. \code{"rows"} (default) projects onto row nodes
#'   (result: n_rows x n_rows). \code{"columns"} projects onto column nodes
#'   (result: n_cols x n_cols).
#' @param method Character. Projection method:
#'   \describe{
#'     \item{\code{"sum"}}{Weighted projection: \code{A \%*\% t(A)} (rows) or
#'       \code{t(A) \%*\% A} (columns). Edge weight equals sum of shared
#'       connection-weight products.}
#'     \item{\code{"binary"}}{Co-occurrence count: binarize A first, then
#'       compute overlap. Edge weight equals number of shared connections.}
#'     \item{\code{"jaccard"}}{Jaccard similarity: shared / (total_i + total_j
#'       - shared) for each pair.}
#'     \item{\code{"cosine"}}{Cosine similarity: dot product of row (or column)
#'       vectors divided by the product of their norms.}
#'     \item{\code{"newman"}}{Newman's weighted projection (Newman 2001): each
#'       shared affiliation contributes \code{1 / (d_k - 1)} where \code{d_k}
#'       is the degree of the shared node. Gives more weight to connections
#'       through exclusive affiliations.}
#'   }
#' @param ... Additional arguments (currently unused).
#'
#' @return A square adjacency matrix with row and column names preserved from
#'   the input. Diagonal is set to 0 (no self-loops).
#'
#' @details
#' For the Newman projection, affiliations shared with only one node of the
#' focal type (\code{d_k = 1}) are skipped, since \code{1 / (d_k - 1)} is
#' undefined. This follows the convention in Newman (2001).
#'
#' @references
#' Newman, M. E. J. (2001). Scientific collaboration networks. II. Shortest
#' paths, weighted networks, and centrality. \emph{Physical Review E}, 64(1),
#' 016132.
#'
#' @seealso \code{\link{is_bipartite}}, \code{\link{plot_heatmap}}
#' @export
#' @examples
#' # Incidence matrix: 4 students x 3 courses
#' inc <- matrix(c(1, 1, 0,
#'                 1, 0, 1,
#'                 0, 1, 1,
#'                 1, 1, 1), 4, 3, byrow = TRUE)
#' rownames(inc) <- paste0("S", 1:4)
#' colnames(inc) <- paste0("C", 1:3)
#'
#' # Student co-enrollment (weighted)
#' cograph::project_bipartite(inc, mode = "rows", method = "sum")
#'
#' # Course overlap (Jaccard similarity)
#' cograph::project_bipartite(inc, mode = "columns", method = "jaccard")
#'
#' # Newman's weighted projection
#' cograph::project_bipartite(inc, mode = "rows", method = "newman")
project_bipartite <- function(x,
                              mode = "rows",
                              method = "sum",
                              ...) {

  mode <- match.arg(mode, c("rows", "columns"))
  method <- match.arg(method, c("sum", "binary", "jaccard", "cosine", "newman"))

  # Convert data.frame input to incidence matrix
  if (is.data.frame(x)) {
    x <- .df_to_incidence(x)
  }

  stopifnot(is.matrix(x), is.numeric(x))

  # For column projection, transpose so the rest of the code projects "rows"
  if (mode == "columns") {
    x <- t(x)
  }

  result <- switch(method,
    sum     = .project_sum(x),
    binary  = .project_binary(x),
    jaccard = .project_jaccard(x),
    cosine  = .project_cosine(x),
    newman  = .project_newman(x)
  )

  # Zero diagonal (no self-loops)
  diag(result) <- 0

  result
}


# ============================================================================
# Projection methods
# ============================================================================

#' Weighted sum projection
#' @noRd
.project_sum <- function(a) {
  result <- a %*% t(a)
  .set_dimnames(result, rownames(a))
}


#' Binary co-occurrence projection
#' @noRd
.project_binary <- function(a) {
  b <- (a > 0) * 1
  result <- b %*% t(b)
  .set_dimnames(result, rownames(a))
}


#' Jaccard similarity projection
#' @noRd
.project_jaccard <- function(a) {
  b <- (a > 0) * 1
  shared <- b %*% t(b)
  # Row sums = number of connections per row node
  row_totals <- rowSums(b)
  n <- nrow(b)
  # total_i + total_j for each pair
  total_matrix <- outer(row_totals, row_totals, "+")
  # Jaccard = shared / (total_i + total_j - shared)
  denom <- total_matrix - shared
  result <- ifelse(denom > 0, shared / denom, 0)
  .set_dimnames(result, rownames(a))
}


#' Cosine similarity projection
#' @noRd
.project_cosine <- function(a) {
  # Dot products
  dot <- a %*% t(a)
  # Norms
  norms <- sqrt(rowSums(a^2))
  # Outer product of norms
  norm_matrix <- outer(norms, norms, "*")
  result <- ifelse(norm_matrix > 0, dot / norm_matrix, 0)
  .set_dimnames(result, rownames(a))
}


#' Newman weighted projection
#' @noRd
.project_newman <- function(a) {
  b <- (a > 0) * 1
  n_rows <- nrow(b)
  # Degree of each column node (shared-side degree)
  col_degrees <- colSums(b)
  # Weight for each column node: 1 / (d_k - 1), skipping d_k <= 1
  col_weights <- ifelse(col_degrees > 1, 1 / (col_degrees - 1), 0)
  # Weighted projection: B %*% diag(w) %*% t(B)
  # Efficiently: (B * col_weights[col]) %*% t(B) via sweep
  weighted_b <- sweep(b, 2, col_weights, `*`)
  result <- weighted_b %*% t(b)
  .set_dimnames(result, rownames(a))
}


# ============================================================================
# Helper: convert data.frame to incidence matrix
# ============================================================================

#' Convert edge data.frame to incidence matrix
#' @noRd
.df_to_incidence <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("type1", "type2") %in% names(df))
  )

  rows <- unique(df$type1)
  cols <- unique(df$type2)
  mat <- matrix(0, nrow = length(rows), ncol = length(cols),
                dimnames = list(rows, cols))

  has_weight <- "weight" %in% names(df)
  weights <- if (has_weight) df$weight else rep(1, nrow(df))

  # Aggregate duplicate pairs by summing weights
  agg <- stats::aggregate(weights ~ df$type1 + df$type2, FUN = sum)
  mat[cbind(match(agg[[1]], rows), match(agg[[2]], cols))] <- agg[[3]]

  mat
}


#' Set symmetric dimnames on a square matrix
#' @noRd
.set_dimnames <- function(m, nms) {
  if (!is.null(nms)) {
    rownames(m) <- nms
    colnames(m) <- nms
  }
  m
}


# ============================================================================
# Bipartite check
# ============================================================================

#' Check if a Matrix Could Be Bipartite
#'
#' Tests whether a matrix could represent a bipartite incidence matrix.
#' A non-square matrix is considered bipartite by default. For square matrices,
#' checks whether the corresponding graph has bipartite structure (i.e., nodes
#' can be partitioned into two groups with edges only between groups).
#'
#' @param x A numeric matrix.
#'
#' @return Logical. \code{TRUE} if the matrix could represent a bipartite
#'   network, \code{FALSE} otherwise.
#'
#' @details
#' For non-square matrices, returns \code{TRUE} since they naturally represent
#' two-mode data (rows and columns are distinct node types).
#'
#' For square matrices, the function checks whether the corresponding
#' undirected graph is bipartite by attempting a two-coloring via
#' \code{igraph::bipartite_mapping()} when igraph is available. Without igraph,
#' it uses a BFS-based two-coloring algorithm.
#'
#' @export
#' @examples
#' # Non-square matrix is bipartite
#' inc <- matrix(c(1, 0, 1, 1, 1, 0), 2, 3)
#' cograph::is_bipartite(inc)
#'
#' # Square bipartite-compatible adjacency
#' adj <- matrix(c(0, 0, 1, 1,
#'                 0, 0, 1, 0,
#'                 1, 1, 0, 0,
#'                 1, 0, 0, 0), 4, 4, byrow = TRUE)
#' cograph::is_bipartite(adj)
#'
#' # Non-bipartite (triangle)
#' tri <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' cograph::is_bipartite(tri)
is_bipartite <- function(x) {
  stopifnot(is.matrix(x), is.numeric(x))

  # Non-square matrices are bipartite by nature
  if (nrow(x) != ncol(x)) return(TRUE)

  # Square matrix: check if the graph has bipartite structure
  if (requireNamespace("igraph", quietly = TRUE)) {
    # Symmetrize for undirected check
    adj <- (x > 0 | t(x) > 0) * 1
    diag(adj) <- 0
    g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
    bm <- igraph::bipartite_mapping(g)
    return(bm$res)
  }

  # Fallback: BFS-based two-coloring
  .is_bipartite_bfs(x)
}


#' BFS-based bipartite check (no igraph dependency)
#' @noRd
.is_bipartite_bfs <- function(x) {
  n <- nrow(x)
  if (n == 0L) return(TRUE)

  # Symmetrize and binarize
  adj <- (x > 0 | t(x) > 0) * 1
  diag(adj) <- 0

  color <- rep(NA_integer_, n)

  # Check each connected component
  uncolored <- which(is.na(color))
  while (length(uncolored) > 0L) {
    start <- uncolored[1L]
    color[start] <- 0L
    queue <- start

    while (length(queue) > 0L) {
      node <- queue[1L]
      queue <- queue[-1L]
      neighbors <- which(adj[node, ] > 0)
      unvisited <- neighbors[is.na(color[neighbors])]
      color[unvisited] <- 1L - color[node]
      queue <- c(queue, unvisited)
      # Check already-colored neighbors for conflict
      visited <- neighbors[!is.na(color[neighbors])]
      if (any(color[visited] == color[node])) return(FALSE)
    }

    uncolored <- which(is.na(color))
  }

  TRUE
}
