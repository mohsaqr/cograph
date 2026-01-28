#' @title Input Parsing Functions
#' @description Functions for parsing network input into internal format.
#' @name input-parse
NULL

#' Parse Network Input
#'
#' Automatically detects input type and converts to internal format.
#'
#' @param input Network input: matrix, data.frame (edge list), or igraph object.
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @return List with nodes, edges, directed, and weights components.
#' @keywords internal
parse_input <- function(input, directed = NULL) {
  # Detect input type
  if (is.matrix(input)) {
    parse_matrix(input, directed = directed)
  } else if (is.data.frame(input)) {
    parse_edgelist(input, directed = directed)
  } else if (inherits(input, "igraph")) {
    parse_igraph(input, directed = directed)
  } else if (inherits(input, "network")) {
    parse_statnet(input, directed = directed)
  } else if (inherits(input, "qgraph")) {
    parse_qgraph(input, directed = directed)
  } else if (is.list(input) && !is.null(input$edges)) {
    # Already parsed format
    input
  } else {
    stop("Unsupported input type. Expected matrix, data.frame, igraph, network, or qgraph object.",
         call. = FALSE)
  }
}

#' Detect if Matrix is Symmetric
#'
#' @param m A matrix.
#' @param tol Tolerance for comparison.
#' @return Logical.
#' @keywords internal
is_symmetric_matrix <- function(m, tol = .Machine$double.eps^0.5) {
  if (!is.matrix(m)) return(FALSE)
  if (nrow(m) != ncol(m)) return(FALSE)
  isTRUE(all.equal(m, t(m), tolerance = tol, check.attributes = FALSE))
}

#' Create Node Data Frame
#'
#' @param n Number of nodes.
#' @param labels Optional node labels.
#' @param names Optional node names for legend (defaults to labels).
#' @return Data frame with node information.
#' @keywords internal
create_nodes_df <- function(n, labels = NULL, names = NULL) {
  if (is.null(labels)) {
    labels <- as.character(seq_len(n))
  }

  if (is.null(names)) {
    names <- labels
  }

  data.frame(
    id = seq_len(n),
    label = labels,
    name = names,
    x = NA_real_,
    y = NA_real_,
    stringsAsFactors = FALSE
  )
}

#' Create Edge Data Frame
#'
#' @param from Vector of source node indices.
#' @param to Vector of target node indices.
#' @param weight Vector of edge weights.
#' @param directed Logical. Is the network directed?
#' @return Data frame with edge information.
#' @keywords internal
create_edges_df <- function(from, to, weight = NULL, directed = FALSE) {
  if (is.null(weight)) {
    weight <- rep(1, length(from))
  }

  data.frame(
    from = from,
    to = to,
    weight = weight,
    stringsAsFactors = FALSE
  )
}

#' Detect Duplicate Edges in Undirected Network
#'
#' For undirected networks, checks if any node pair has multiple edges
#' (e.g., both A->B and B->A in an edge list).
#'
#' @param edges Edge data frame with from, to, and optionally weight columns.
#' @return List with has_duplicates (logical) and info (list of duplicate details).
#' @keywords internal
detect_duplicate_edges <- function(edges) {
  if (is.null(edges) || nrow(edges) == 0) {
    return(list(has_duplicates = FALSE, info = NULL))
  }

  # Create canonical keys (lower index first)
  keys <- paste(pmin(edges$from, edges$to), pmax(edges$from, edges$to), sep = "-")
  dup_keys <- keys[duplicated(keys)]

  if (length(dup_keys) == 0) {
    return(list(has_duplicates = FALSE, info = NULL))
  }

  # Build info about duplicates
  info <- lapply(unique(dup_keys), function(k) {
    idx <- which(keys == k)
    list(
      nodes = as.numeric(strsplit(k, "-")[[1]]),
      count = length(idx),
      weights = if ("weight" %in% names(edges)) edges$weight[idx] else rep(1, length(idx))
    )
  })

  list(has_duplicates = TRUE, info = info)
}

#' Aggregate Duplicate Edges
#'
#' For undirected networks, aggregates multiple edges between the same node pair
#' using the specified method.
#'
#' @param edges Edge data frame with from, to, and optionally weight columns.
#' @param method Aggregation method: "sum", "mean", "first", "max", "min",
#'   or a custom function.
#' @return Deduplicated edge data frame.
#' @keywords internal
aggregate_duplicate_edges <- function(edges, method = "mean") {
  if (is.null(edges) || nrow(edges) == 0) {
    return(edges)
  }

  keys <- paste(pmin(edges$from, edges$to), pmax(edges$from, edges$to), sep = "-")

  agg_fn <- if (is.function(method)) {
    method
  } else {
    switch(method,
      "sum" = sum,
      "mean" = mean,
      "first" = function(x) x[1],
      "max" = max,
      "min" = min,
      stop("Unknown aggregation method: ", method,
           ". Use 'sum', 'mean', 'first', 'max', 'min', or a custom function.",
           call. = FALSE)
    )
  }

  # Aggregate by key
  unique_keys <- unique(keys)
  result <- do.call(rbind, lapply(unique_keys, function(k) {
    idx <- which(keys == k)
    row <- edges[idx[1], , drop = FALSE]
    # Ensure canonical order (lower index first)
    row$from <- min(edges$from[idx[1]], edges$to[idx[1]])
    row$to <- max(edges$from[idx[1]], edges$to[idx[1]])
    if ("weight" %in% names(row)) {
      row$weight <- agg_fn(edges$weight[idx])
    }
    row
  }))
  rownames(result) <- NULL
  result
}
