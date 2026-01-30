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
  } else if (inherits(input, "tna")) {
    parse_tna(input, directed = directed)
  } else if (is.list(input) && !is.null(input$edges)) {
    # Already parsed format
    input
  } else {
    stop("Unsupported input type. Expected matrix, data.frame, igraph, network, qgraph, or tna object.",
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
#' Identifies edges that appear multiple times between the same pair of nodes.
#' For undirected networks, edges A\code{->}B and B\code{->}A are considered duplicates.
#' For directed networks, only identical from\code{->}to pairs are duplicates.
#'
#' @param edges Data frame with \code{from} and \code{to} columns (and optionally
#'   \code{weight}).
#'
#' @details
#' This function is useful for cleaning network data before visualization.
#' Duplicate edges can arise from:
#' \itemize{
#'   \item Data collection errors (same edge recorded twice)
#'   \item Combining multiple data sources
#'   \item Converting from formats that allow multi-edges
#'   \item Edge lists that include both A\code{->}B and B\code{->}A for undirected networks
#' }
#'
#' The function creates canonical keys by sorting node pairs (lower index first),
#' so edges 1\code{->}2 and 2\code{->}1 map to the same key "1-2" in undirected mode.
#'
#' @return A list with two components:
#' \describe{
#'   \item{has_duplicates}{Logical indicating whether any duplicates were found.}
#'   \item{info}{A list of duplicate details, where each element contains:
#'     \code{nodes} (the node pair), \code{count} (number of edges), and
#'     \code{weights} (vector of weights if available).}
#' }
#'
#' @seealso \code{\link{aggregate_duplicate_edges}} for combining duplicates into
#'   single edges
#'
#' @examples
#' \dontrun{
#' # Create edges with duplicates
#' edges <- data.frame(
#'   from = c(1, 1, 2, 2, 3),
#'   to = c(2, 2, 3, 1, 1),
#'   weight = c(0.5, 0.3, 0.4, 0.6, 0.2)
#' )
#'
#' # Detect duplicates (undirected: 1-2 appears 3 times, 1-3 appears 2 times)
#' result <- detect_duplicate_edges(edges)
#' result$has_duplicates
#' # [1] TRUE
#'
#' # View duplicate details
#' result$info[[1]]
#' # $nodes: 1, 2
#' # $count: 3
#' # $weights: 0.5, 0.3, 0.6
#' }
#'
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
#' Combines duplicate edges by aggregating their weights using a specified
#' function (sum, mean, max, min, or first).
#'
#' @param edges Data frame with \code{from}, \code{to}, and \code{weight} columns.
#' @param method Aggregation method: \code{"sum"} (default), \code{"mean"},
#'   \code{"max"}, \code{"min"}, \code{"first"}, or a custom function that
#'   takes a numeric vector and returns a single value.
#'
#' @details
#' ## Aggregation Methods
#' \describe{
#'   \item{\strong{sum}}{Total weight of all duplicate edges. Useful for frequency
#'     counts or when edges represent additive quantities (e.g., number of emails).}
#'   \item{\strong{mean}}{Average weight. Useful for averaging multiple measurements
#'     or when duplicates represent repeated observations.}
#'   \item{\strong{max}}{Maximum weight. Useful for finding the strongest connection
#'     or most recent value.
#'   }
#'   \item{\strong{min}}{Minimum weight. Useful for the most conservative estimate
#'     or earliest value.}
#'   \item{\strong{first}}{Keep first occurrence. Useful for preserving original
#'     order or when duplicates are erroneous.}
#' }
#'
#' The output edge list uses canonical node ordering (lower index first for
#' undirected networks), ensuring consistent from/to assignment.
#'
#' @return A deduplicated data frame with the same columns as the input, where
#'   each node pair appears only once with its aggregated weight.
#'
#' @seealso \code{\link{detect_duplicate_edges}} for identifying duplicates before
#'   aggregation
#'
#' @examples
#' \dontrun{
#' # Create edges with duplicates
#' edges <- data.frame(
#'   from = c(1, 1, 2),
#'   to = c(2, 2, 3),
#'   weight = c(0.5, 0.3, 0.4)
#' )
#'
#' # Aggregate by sum (0.5 + 0.3 = 0.8)
#' aggregate_duplicate_edges(edges, method = "sum")
#' #   from to weight
#' # 1    1  2    0.8
#' # 2    2  3    0.4
#'
#' # Aggregate by mean (average: 0.4)
#' aggregate_duplicate_edges(edges, method = "mean")
#' #   from to weight
#' # 1    1  2    0.4
#' # 2    2  3    0.4
#'
#' # Use custom aggregation function
#' aggregate_duplicate_edges(edges, method = function(x) sqrt(sum(x^2)))
#' }
#'
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
