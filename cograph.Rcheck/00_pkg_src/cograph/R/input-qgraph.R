#' @title qgraph Input Parsing
#' @description Functions for parsing qgraph objects.
#' @name input-qgraph
NULL

#' Parse qgraph Object
#'
#' Convert a qgraph object to internal network format.
#'
#' @param q A qgraph object from the qgraph package.
#' @param directed Logical. Force directed interpretation. NULL uses qgraph's setting.
#' @return List with nodes, edges, directed, and weights components.
#' @noRd
parse_qgraph <- function(q, directed = NULL) {
 # Check if qgraph is available
  if (!requireNamespace("qgraph", quietly = TRUE)) {
    stop("Package 'qgraph' is required for qgraph input. ",
         "Please install it with: install.packages('qgraph')",
         call. = FALSE)
  }

  # Validate input
  if (!inherits(q, "qgraph") && is.null(q$Arguments)) {
    stop("Input must be a qgraph object", call. = FALSE)
  }

  # Get edge list
  el <- q$Edgelist

  # Get directedness
  if (is.null(directed)) {
    if (!is.null(el$directed)) {
      directed <- any(el$directed)
    } else {
      # Fall back to checking matrix symmetry
      input_mat <- q$Arguments$input
      directed <- if (!is.null(input_mat) && is.matrix(input_mat)) {
        !isSymmetric(input_mat)
      } else {
        FALSE
      }
    }
  }

  # Get node information
  ga_nodes <- q$graphAttributes$Nodes
  if (!is.null(ga_nodes$names)) {
    labels <- ga_nodes$names
    n <- length(labels)
  } else if (!is.null(ga_nodes$labels)) {
    labels <- ga_nodes$labels
    n <- length(labels)
  } else {
    # Infer from input matrix or edge list
    input_mat <- q$Arguments$input
    if (!is.null(input_mat) && is.matrix(input_mat)) {
      n <- nrow(input_mat)
    } else {
      n <- max(c(el$from, el$to))
    }
    labels <- as.character(seq_len(n))
  }

  # Get edges
  if (is.null(el) || length(el$from) == 0) {
    from_idx <- integer(0)
    to_idx <- integer(0)
    weight_vals <- numeric(0)
  } else {
    from_idx <- el$from
    to_idx <- el$to
    weight_vals <- if (!is.null(el$weight)) el$weight else rep(1, length(el$from))
  }

  # Create data structures
  nodes <- create_nodes_df(n, labels)
  edges <- create_edges_df(from_idx, to_idx, weight_vals, directed)

  # Add layout if available
  if (!is.null(q$layout) && is.matrix(q$layout) && nrow(q$layout) == n) {
    nodes$x <- q$layout[, 1]
    nodes$y <- q$layout[, 2]
  }

  list(
    nodes = nodes,
    edges = edges,
    directed = directed,
    weights = weight_vals
  )
}
