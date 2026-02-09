#' @title Edge List Input Parsing
#' @description Functions for parsing edge list data frames.
#' @name input-edgelist
NULL

#' Parse Edge List Data Frame
#'
#' Convert an edge list data frame to internal network format.
#'
#' @param df A data frame with columns for source (from) and target (to) nodes.
#'   Optional weight column. Column names are auto-detected.
#' @param directed Logical. Is the network directed? Default TRUE.
#' @return List with nodes, edges, directed, and weights components.
#' @noRd
parse_edgelist <- function(df, directed = NULL) {
  # Validate input
  if (!is.data.frame(df)) {
    stop("Input must be a data frame", call. = FALSE)
  }
  if (nrow(df) == 0) {
    stop("Edge list cannot be empty", call. = FALSE)
  }

  # Auto-detect column names
  col_names <- tolower(names(df))

  # Find source column
  from_col <- which(col_names %in% c("from", "source", "src", "v1", "node1", "i"))[1]
  if (is.na(from_col)) from_col <- 1

  # Find target column
  to_col <- which(col_names %in% c("to", "target", "tgt", "v2", "node2", "j"))[1]
  if (is.na(to_col)) to_col <- 2

  # Find weight column
  weight_col <- which(col_names %in% c("weight", "w", "value", "strength"))[1]
  has_weight <- !is.na(weight_col)

  # Extract columns
  from_vals <- df[[from_col]]
  to_vals <- df[[to_col]]

  if (has_weight) {
    weight_vals <- as.numeric(df[[weight_col]])
  } else {
    weight_vals <- rep(1, nrow(df))
  }

  # Get unique nodes
  all_nodes <- unique(c(as.character(from_vals), as.character(to_vals)))
  n <- length(all_nodes)

  # Create node ID mapping
  node_map <- setNames(seq_len(n), all_nodes)

  # Convert to numeric indices
  from_idx <- as.integer(node_map[as.character(from_vals)])
  to_idx <- as.integer(node_map[as.character(to_vals)])

  # Auto-detect directed if not specified
  if (is.null(directed)) {
    # Check for bidirectional edges
    edge_pairs <- paste(pmin(from_idx, to_idx), pmax(from_idx, to_idx), sep = "-")
    directed <- length(edge_pairs) != length(unique(edge_pairs))
    if (!directed) {
      # Also check if same edge appears twice with different directions
      edge_dir <- paste(from_idx, to_idx, sep = "->")
      edge_rev <- paste(to_idx, from_idx, sep = "->")
      directed <- any(edge_dir %in% edge_rev)
    }
  }

  # Create data structures
  nodes <- create_nodes_df(n, all_nodes)
  edges <- create_edges_df(from_idx, to_idx, weight_vals, directed)

  list(
    nodes = nodes,
    edges = edges,
    directed = directed,
    weights = weight_vals
  )
}
