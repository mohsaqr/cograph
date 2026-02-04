#' @title tna Input Parsing
#' @description Functions for parsing tna objects.
#' @name input-tna
NULL

#' Parse tna Object
#'
#' Convert a tna object to internal network format.
#' tna objects are simple lists with $weights (matrix), $labels, and $inits.
#'
#' @param tna_obj A tna object (list with weights matrix).
#' @param directed Logical. Force directed interpretation. NULL uses TRUE (tna networks are directed).
#' @return List with nodes, edges, directed, and weights components.
#' @noRd
parse_tna <- function(tna_obj, directed = NULL) {
  # Validate input
  if (!inherits(tna_obj, "tna")) {
    stop("Input must be a tna object", call. = FALSE)
  }

  # tna networks are always directed (transition matrices)
  if (is.null(directed)) {
    directed <- TRUE
  }

  # Get the weights matrix
  x <- tna_obj$weights

  # Get number of nodes and labels
  n <- nrow(x)
  labels <- tna_obj$labels
  if (is.null(labels) || all(is.na(labels))) {
    labels <- as.character(seq_len(n))
  }

  # Extract edges from matrix
  edge_idx <- which(x != 0, arr.ind = TRUE)
  if (nrow(edge_idx) == 0) {
    from_idx <- integer(0)
    to_idx <- integer(0)
    weight_vals <- numeric(0)
  } else {
    from_idx <- edge_idx[, 1]
    to_idx <- edge_idx[, 2]
    weight_vals <- x[edge_idx]
  }

  # Create data structures
  nodes <- create_nodes_df(n, labels)
  edges <- create_edges_df(from_idx, to_idx, weight_vals, directed)

  # Store initial probabilities as node attribute (for donut visualization)
  if (!is.null(tna_obj$inits)) {
    nodes$inits <- as.numeric(tna_obj$inits)
  }

  list(
    nodes = nodes,
    edges = edges,
    directed = directed,
    weights = weight_vals
  )
}
