#' @title tna Input Parsing
#' @description Functions for parsing tna objects.
#' @name input-tna
NULL

#' Parse tna Object
#'
#' Convert a tna object to internal network format.
#' When the tna object contains raw sequence data ($data), each individual
#' transition is preserved as a separate edge in the edge list (temporal
#' multigraph). Use simplify to aggregate into a weighted simple graph.
#'
#' @param tna_obj A tna object (list with weights matrix).
#' @param directed Logical. Force directed interpretation. NULL uses TRUE
#'   (tna networks are directed).
#' @param simplify Logical or character. If FALSE (default), every transition
#'   from $data is a separate edge. If TRUE or a string ("sum", "mean", "max",
#'   "min"), duplicate edges are aggregated. When $data is NULL, falls back
#'   to the weight matrix (always simplified).
#' @return List with nodes, edges, directed, weights_matrix, and tna components.
#' @noRd
parse_tna <- function(tna_obj, directed = NULL, simplify = FALSE) {
  # Validate input
  if (!inherits(tna_obj, "tna")) {
    stop("Input must be a tna object", call. = FALSE)
  }

  # Get the weights matrix
  x <- tna_obj$weights

  # Determine directedness
  if (is.null(directed)) {
    if (!is.null(tna_obj$directed)) {
      directed <- tna_obj$directed
    } else if (!is.null(attr(tna_obj, "directed"))) {
      directed <- attr(tna_obj, "directed")
    } else {
      directed <- !is_symmetric_matrix(x)
    }
  }

  # Get labels
  n <- nrow(x)
  labels <- tna_obj$labels
  if (is.null(labels) || all(is.na(labels))) {
    labels <- as.character(seq_len(n))
  }

  # Build edges: prefer raw transitions from $data, fall back to weight matrix
  if (!is.null(tna_obj$data) && is.matrix(tna_obj$data)) {
    edges <- .extract_tna_transitions(tna_obj$data, labels)
  } else {
    # No raw data — extract from aggregated weight matrix
    edges <- .edges_from_weight_matrix(x, directed)
  }

  # Simplify if requested
  if (!isFALSE(simplify)) {
    method <- if (isTRUE(simplify)) "sum" else simplify
    edges <- .simplify_tna_edges(edges, method, directed)
  }

  # Create node data frame
  nodes <- create_nodes_df(n, labels)

  # Store initial probabilities as node attribute (for donut visualization)
  if (!is.null(tna_obj$inits)) {
    nodes$inits <- as.numeric(tna_obj$inits)
  }

  # Extract colors from tna data if available
  if (!is.null(tna_obj$data)) {
    tna_colors <- attr(tna_obj$data, "colors")
    if (!is.null(tna_colors) && length(tna_colors) == n) {
      nodes$color <- tna_colors
    }
  }

  list(
    nodes = nodes,
    edges = edges,
    directed = directed,
    weights_matrix = x,
    tna = list(
      type = "tna",
      group_index = NULL,
      group_name = NULL
    )
  )
}

#' Extract Individual Transitions from tna Sequence Data
#'
#' Converts the raw sequence matrix ($data) into a temporal edge list.
#' Each consecutive pair of states within a session becomes one edge.
#'
#' @param data Matrix where rows = sessions, columns = time steps,
#'   values = integer state indices.
#' @param labels Character vector of state names.
#' @return Data frame with columns: from, to, weight, session, time.
#' @noRd
.extract_tna_transitions <- function(data, labels) {
  n_sessions <- nrow(data)
  n_cols <- ncol(data)

  # Pre-allocate — upper bound is n_sessions * (n_cols - 1)
  max_edges <- n_sessions * (n_cols - 1L)
  from_vec <- integer(max_edges)
  to_vec <- integer(max_edges)
  session_vec <- integer(max_edges)
  time_vec <- integer(max_edges)
  k <- 0L

  for (i in seq_len(n_sessions)) {
    row <- data[i, ]
    valid <- which(is.finite(row) & !is.na(row))
    if (length(valid) < 2L) next
    # Consecutive pairs within valid positions
    for (j in seq_len(length(valid) - 1L)) {
      k <- k + 1L
      from_vec[k] <- row[valid[j]]
      to_vec[k] <- row[valid[j + 1L]]
      session_vec[k] <- i
      time_vec[k] <- j
    }
  }

  # Trim to actual size
  if (k == 0L) {
    return(data.frame(
      from = integer(0), to = integer(0), weight = numeric(0),
      session = integer(0), time = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  from_vec <- from_vec[seq_len(k)]
  to_vec <- to_vec[seq_len(k)]
  session_vec <- session_vec[seq_len(k)]
  time_vec <- time_vec[seq_len(k)]

  data.frame(
    from = from_vec,
    to = to_vec,
    weight = rep(1, k),
    session = session_vec,
    time = time_vec,
    stringsAsFactors = FALSE
  )
}

#' Extract Edges from Weight Matrix
#'
#' Fallback when $data is not available.
#'
#' @param x Weight matrix.
#' @param directed Logical.
#' @return Data frame with from, to, weight columns.
#' @noRd
.edges_from_weight_matrix <- function(x, directed) {
  if (directed) {
    edge_idx <- which(x != 0, arr.ind = TRUE)
  } else {
    edge_idx <- which(upper.tri(x) & x != 0, arr.ind = TRUE)
  }
  if (nrow(edge_idx) == 0) {
    return(data.frame(from = integer(0), to = integer(0),
                      weight = numeric(0), stringsAsFactors = FALSE))
  }
  data.frame(
    from = edge_idx[, 1],
    to = edge_idx[, 2],
    weight = x[edge_idx],
    stringsAsFactors = FALSE
  )
}

#' Simplify tna Edge List
#'
#' Aggregate duplicate edges using the specified method.
#'
#' @param edges Data frame with from, to, weight (and optionally session, time).
#' @param method Aggregation method: "sum", "mean", "max", "min".
#' @param directed Logical.
#' @return Simplified data frame with from, to, weight.
#' @noRd
.simplify_tna_edges <- function(edges, method = "sum", directed = TRUE) {
  if (nrow(edges) == 0) return(edges[, c("from", "to", "weight")])

  method <- match.arg(method, c("sum", "mean", "max", "min"))
  agg_fn <- switch(method, sum = sum, mean = mean, max = max, min = min)

  if (directed) {
    keys <- paste(edges$from, edges$to, sep = "->")
  } else {
    keys <- paste(pmin(edges$from, edges$to), pmax(edges$from, edges$to),
                  sep = "-")
  }

  unique_keys <- unique(keys)
  result <- do.call(rbind, lapply(unique_keys, function(k) {
    idx <- which(keys == k)
    data.frame(
      from = edges$from[idx[1]],
      to = edges$to[idx[1]],
      weight = agg_fn(edges$weight[idx]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(result) <- NULL
  result
}


#' Parse group_tna Object
#'
#' Convert a single group from a group_tna object to internal network format.
#' group_tna objects are named lists of tna objects.
#'
#' @param group_tna_obj A group_tna object (named list of tna objects).
#' @param i Index of the group to extract.
#' @param directed Logical. Force directed interpretation. NULL uses TRUE.
#' @param simplify Logical or character. Passed to parse_tna.
#' @return List with nodes, edges, directed, weights_matrix, and tna components.
#' @noRd
parse_group_tna <- function(group_tna_obj, i = 1, directed = NULL,
                            simplify = FALSE) {
  # Validate input
  if (!inherits(group_tna_obj, "group_tna")) {
    stop("Input must be a group_tna object", call. = FALSE)
  }

  if (i < 1 || i > length(group_tna_obj)) {
    stop("Index i must be between 1 and ", length(group_tna_obj), call. = FALSE)
  }

  # Extract the single tna object
  tna_obj <- group_tna_obj[[i]]
  group_name <- names(group_tna_obj)[i]

  # Parse using parse_tna
  parsed <- parse_tna(tna_obj, directed = directed, simplify = simplify)

  # Update tna metadata for group_tna context (minimal - no parent stored)
  parsed$tna$type <- "group_tna"
  parsed$tna$group_index <- i
  parsed$tna$group_name <- group_name

  parsed
}

# =============================================================================
# TNA Network Helper Functions
# =============================================================================

#' Check if Network is TNA-based
#'
#' Checks whether a cograph_network was created from a tna or group_tna object.
#'
#' @param x A cograph_network object.
#' @return Logical: TRUE if the network was created from a TNA object, FALSE otherwise.
#'
#' @seealso \code{\link{as_cograph}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(tna)
#' model <- tna(group_regulation)
#' net <- as_cograph(model)
#' is_tna_network(net)
#' #> TRUE
#'
#' # Non-TNA network
#' mat <- matrix(runif(25), 5, 5)
#' net2 <- as_cograph(mat)
#' is_tna_network(net2)
#' #> FALSE
#' }
is_tna_network <- function(x) {
  (inherits(x, "cograph_network") || inherits(x, "CographNetwork")) &&
    !is.null(x$meta$tna) &&
    !is.null(x$meta$tna$type)
}
