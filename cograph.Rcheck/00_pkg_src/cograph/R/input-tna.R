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

  # Determine directedness:
  # 1. Use explicit directed parameter if provided
  # 2. Otherwise read from tna object's $directed field or attribute
  # 3. Default to TRUE (standard tna networks are directed transition matrices)
  if (is.null(directed)) {
    if (!is.null(tna_obj$directed)) {
      directed <- tna_obj$directed
    } else if (!is.null(attr(tna_obj, "directed"))) {
      directed <- attr(tna_obj, "directed")
    } else {
      directed <- TRUE
    }
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
    weights = weight_vals,
    tna = list(
      model = tna_obj,
      type = "tna",
      group_index = NULL,
      group_name = NULL
    )
  )
}

#' Parse group_tna Object
#'
#' Convert a single group from a group_tna object to internal network format.
#' group_tna objects are named lists of tna objects.
#'
#' @param group_tna_obj A group_tna object (named list of tna objects).
#' @param i Index of the group to extract.
#' @param directed Logical. Force directed interpretation. NULL uses TRUE.
#' @return List with nodes, edges, directed, weights, and tna components.
#' @noRd
parse_group_tna <- function(group_tna_obj, i = 1, directed = NULL) {
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
  parsed <- parse_tna(tna_obj, directed = directed)

  # Update tna metadata for group_tna context

  parsed$tna$type <- "group_tna"
  parsed$tna$group_index <- i
  parsed$tna$group_name <- group_name
  parsed$tna$parent <- group_tna_obj

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
#' @return Logical: TRUE if the network contains a TNA model, FALSE otherwise.
#'
#' @seealso \code{\link{get_tna_model}}, \code{\link{as_cograph}}
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
    !is.null(x$tna) &&
    !is.null(x$tna$model)
}

#' Get Original TNA Model
#'
#' Extracts the original tna object from a cograph_network that was created
#' from a TNA model. This allows access to all TNA-specific fields and
#' attributes, including the sequence data and colors.
#'
#' @param x A cograph_network object created from a tna or group_tna object.
#' @return The original tna object with all its fields:
#'   \describe{
#'     \item{\code{$weights}}{Transition matrix (numeric matrix)}
#'     \item{\code{$inits}}{Initial state probabilities (named numeric vector)}
#'     \item{\code{$labels}}{State names (character vector)}
#'     \item{\code{$data}}{Sequence data (class tna_seq_data)}
#'   }
#'   The returned object also has attributes accessible via \code{attr()}:
#'   \describe{
#'     \item{\code{attr(, "type")}}{Model type ("relative", "frequency", etc.)}
#'     \item{\code{attr(, "scaling")}}{Scaling method applied}
#'     \item{\code{attr(, "params")}}{Model parameters}
#'   }
#'   The \code{$data} element has its own attributes:
#'   \describe{
#'     \item{\code{attr(data, "colors")}}{State colors assigned by tna}
#'   }
#'
#' @seealso \code{\link{is_tna_network}}, \code{\link{as_cograph}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(tna)
#' model <- tna(group_regulation)
#' net <- as_cograph(model)
#'
#' # Get original model
#' original <- get_tna_model(net)
#' class(original)
#' #> "tna"
#'
#' # Access TNA fields
#' original$weights   # transition matrix
#' original$inits     # initial probabilities
#' original$labels    # state names
#' original$data      # sequence data
#'
#' # Access attributes
#' attr(original, "type")              # "relative"
#' attr(original$data, "colors")       # state colors
#' }
get_tna_model <- function(x) {
  if (!is_tna_network(x)) {
    stop("Not a TNA network. Use is_tna_network() to check first.", call. = FALSE)
  }
  x$tna$model
}
