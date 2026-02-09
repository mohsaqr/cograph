#' @title Matrix Input Parsing
#' @description Functions for parsing adjacency/weight matrices.
#' @name input-matrix
NULL

#' Parse Adjacency/Weight Matrix
#'
#' Convert an adjacency or weight matrix to internal network format.
#'
#' @param m A square matrix. Symmetric matrices are treated as undirected,
#'   asymmetric as directed. Values represent edge weights.
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @return List with nodes, edges, directed, and weights components.
#' @noRd
parse_matrix <- function(m, directed = NULL) {
  # Validate input
  if (!is.matrix(m)) {
    stop("Input must be a matrix", call. = FALSE)
  }
  if (!is.numeric(m)) {
    stop("Matrix must be numeric", call. = FALSE)
  }
  if (nrow(m) != ncol(m)) {
    stop("Matrix must be square", call. = FALSE)
  }

  n <- nrow(m)

  # Get node labels from dimnames
  labels <- rownames(m)
  if (is.null(labels)) {
    labels <- colnames(m)
  }
  if (is.null(labels)) {
    labels <- as.character(seq_len(n))
  }

  # Auto-detect directed
  if (is.null(directed)) {
    directed <- !is_symmetric_matrix(m)
  }

  # Extract edges
  if (directed) {
    # For directed: all non-zero entries
    idx <- which(m != 0, arr.ind = TRUE)
    from <- idx[, 1]
    to <- idx[, 2]
    weight <- m[idx]
  } else {
    # For undirected: upper triangle only (avoid duplicates)
    idx <- which(upper.tri(m) & m != 0, arr.ind = TRUE)
    from <- idx[, 1]
    to <- idx[, 2]
    weight <- m[idx]
  }

  # Create data structures
  nodes <- create_nodes_df(n, labels)
  edges <- create_edges_df(from, to, weight, directed)

  list(
    nodes = nodes,
    edges = edges,
    directed = directed,
    weights = weight
  )
}
