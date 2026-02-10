#' @title Validation Utility Functions
#' @description Helper functions for network validation (bootstrap, permutation, stability).
#' @name validation-utils
NULL

# =============================================================================
# Data Type Detection
# =============================================================================

#' Detect Data Type for Validation
#'
#' Identifies the data type and available validation methods for a network object.
#'
#' @param x A network object (tna, group_tna, cograph_network, or matrix).
#' @return A list with:
#'   \describe{
#'     \item{type}{Character: "tna", "group_tna", "cograph_raw", or "matrix_only"}
#'     \item{has_raw_data}{Logical: whether raw data is available for resampling}
#'     \item{estimator}{Function or NULL: weight estimation function}
#'     \item{available_methods}{Character vector of available validation methods}
#'   }
#'
#' @keywords internal
#' @export
detect_data_type <- function(x) {
 if (inherits(x, "tna")) {

    has_data <- !is.null(x$data)
    list(
      type = "tna",
      has_raw_data = has_data,
      estimator = if (has_data) .tna_estimator else NULL,
      available_methods = if (has_data) {
        c("bootstrap", "permutation_test", "estimate_cs", "disparity_filter")
      } else {
        "disparity_filter"
      }
    )
  } else if (inherits(x, "group_tna")) {
    # Check if any group has data
    has_data <- any(vapply(x, function(g) !is.null(g$data), logical(1)))
    list(
      type = "group_tna",
      has_raw_data = has_data,
      estimator = if (has_data) .tna_estimator else NULL,
      available_methods = if (has_data) {
        c("bootstrap", "permutation_test", "estimate_cs", "disparity_filter")
      } else {
        "disparity_filter"
      }
    )
  } else if (inherits(x, "cograph_network")) {
    has_data <- !is.null(x$raw_data)
    has_estimator <- !is.null(x$estimator)
    list(
      type = "cograph_network",
      has_raw_data = has_data,
      estimator = if (has_estimator) x$estimator else NULL,
      available_methods = if (has_data && has_estimator) {
        c("bootstrap", "permutation_test", "estimate_cs", "disparity_filter")
      } else {
        "disparity_filter"
      }
    )
  } else if (is.matrix(x)) {
    list(
      type = "matrix_only",
      has_raw_data = FALSE,
      estimator = NULL,
      available_methods = "disparity_filter"
    )
  } else {
    stop("Unsupported object type: ", class(x)[1], call. = FALSE)
  }
}

# =============================================================================
# Raw Data Attachment
# =============================================================================

#' Set Raw Data for Bootstrap/Permutation
#'
#' Attaches raw data and an estimator function to a cograph_network object,
#' enabling bootstrap, permutation testing, and stability analysis.
#'
#' @param x A cograph_network object.
#' @param raw_data The raw data used to estimate the network weights.
#'   For sequence data, this should be the original sequences.
#'   For co-occurrence/correlation, this should be the original data matrix.
#' @param estimator A function that takes raw_data (or a subset) and returns
#'   a weight matrix. Signature: \code{function(data) -> matrix}.
#'
#' @return The modified cograph_network object with raw_data and estimator attached.
#'
#' @seealso \code{\link{get_raw_data}}, \code{\link{bootstrap}}, \code{\link{permutation_test}}
#'
#' @export
#'
#' @examples
#' # Create network from correlation matrix
#' data <- matrix(rnorm(500), nrow = 100, ncol = 5)
#' colnames(data) <- paste0("V", 1:5)
#' cor_mat <- cor(data)
#'
#' net <- as_cograph(cor_mat)
#'
#' # Attach raw data with correlation estimator
#' net <- set_raw_data(net, data, estimator = function(d) cor(d))
#'
#' # Now bootstrap is available
#' # result <- bootstrap(net, iter = 100)
set_raw_data <- function(x, raw_data, estimator) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }
  if (!is.function(estimator)) {
    stop("estimator must be a function", call. = FALSE)
  }

  x$raw_data <- raw_data
  x$estimator <- estimator
  x
}

#' Get Raw Data from Network
#'
#' Extracts the raw data attached to a cograph_network object.
#'
#' @param x A cograph_network object.
#' @return The raw data, or NULL if not attached.
#'
#' @seealso \code{\link{set_raw_data}}
#'
#' @export
get_raw_data <- function(x) {
  if (inherits(x, "tna")) {
    return(x$data)
  } else if (inherits(x, "cograph_network")) {
    # Check if it's a TNA network
    if (is_tna_network(x)) {
      return(x$tna$model$data)
    }
    return(x$raw_data)
  }
  NULL
}

#' Get Estimator Function from Network
#'
#' Extracts the weight estimation function from a network object.
#'
#' @param x A network object.
#' @return The estimator function, or NULL if not available.
#'
#' @keywords internal
#' @export
get_estimator <- function(x) {
  if (inherits(x, "tna") || inherits(x, "group_tna")) {
    return(.tna_estimator)
  } else if (inherits(x, "cograph_network")) {
    if (is_tna_network(x)) {
      return(.tna_estimator)
    }
    return(x$estimator)
  }
  NULL
}

# =============================================================================
# Weight Extraction
# =============================================================================
#' Extract Weight Matrix from Network Object
#'
#' Internal function to extract the weight matrix from various network types.
#'
#' @param x A network object (tna, group_tna element, cograph_network, or matrix).
#' @return A numeric weight matrix.
#'
#' @keywords internal
.extract_weights <- function(x) {
  if (inherits(x, "tna")) {
    return(x$weights)
  } else if (inherits(x, "cograph_network")) {
    if (is_tna_network(x)) {
      return(x$tna$model$weights)
    }
    # Convert edge list back to matrix
    return(to_matrix(x))
  } else if (is.matrix(x)) {
    return(x)
  }
  stop("Cannot extract weights from object of class: ", class(x)[1], call. = FALSE)
}

#' Extract Initial Probabilities from Network
#'
#' Internal function to extract initial state probabilities from TNA networks.
#'
#' @param x A network object.
#' @return Named numeric vector of initial probabilities, or NULL.
#'
#' @keywords internal
.extract_inits <- function(x) {
  if (inherits(x, "tna")) {
    return(x$inits)
  } else if (inherits(x, "cograph_network") && is_tna_network(x)) {
    return(x$tna$model$inits)
  }
  NULL
}

# =============================================================================
# TNA-Specific Estimator
# =============================================================================

#' TNA Weight Estimator
#'
#' Internal estimator function for TNA sequence data.
#' Matches TNA's internal weight computation exactly.
#'
#' @param data TNA sequence data (tna_seq_data class).
#' @param type Weight type ("relative", "frequency", etc.).
#' @param scaling Scaling method.
#' @return Weight matrix.
#'
#' @keywords internal
.tna_estimator <- function(data, type = "relative", scaling = "none") {
  # This requires tna package
  if (!requireNamespace("tna", quietly = TRUE)) {
    stop("Package 'tna' is required for TNA estimation", call. = FALSE)
  }

  # Create tna model from data
  model <- tna::tna(data, type = type, scaling = scaling)
  model$weights
}

# =============================================================================
# Compute Weights (TNA-Compatible)
# =============================================================================

#' Compute Weights from Transition Data
#'
#' Fast vectorized weight computation matching TNA's internal algorithm.
#' Works with 3D transition arrays (n_sequences x n_states x n_states).
#'
#' @param trans 3D array of transitions [n_sequences, n_states, n_states].
#' @param type Weight type: "relative" (default), "frequency".
#' @param scaling Scaling method: "none" (default), "row", "column".
#' @param a Number of states (nodes).
#' @return Weight matrix [n_states x n_states].
#'
#' @keywords internal
.compute_weights <- function(trans, type = "relative", scaling = "none", a = NULL) {
  if (is.null(a)) {
    a <- dim(trans)[2]
  }

  # Sum transitions across all sequences
  counts <- apply(trans, c(2, 3), sum, na.rm = TRUE)

  if (type == "relative") {
    # Row-normalize to get transition probabilities
    row_sums <- .rowSums(counts, m = a, n = a)
    row_sums[row_sums == 0] <- 1  # Avoid division by zero
    weights <- counts / row_sums
  } else if (type == "frequency") {
    weights <- counts
  } else {
    weights <- counts
  }

  weights
}

# =============================================================================
# Transition Array Extraction
# =============================================================================

#' Extract Transition Array from TNA Data
#'
#' Converts TNA sequence data to a 3D transition array for bootstrap/permutation.
#'
#' @param data TNA sequence data.
#' @return 3D array [n_sequences, n_states, n_states].
#'
#' @keywords internal
.extract_transitions <- function(data) {
  if (!requireNamespace("tna", quietly = TRUE)) {
    stop("Package 'tna' is required", call. = FALSE)
  }

  # Get sequences and states
  if (inherits(data, "tna_seq_data")) {
    seqs <- data
  } else {
    stop("Expected tna_seq_data object", call. = FALSE
    )
  }

  # Get unique states
  states <- attr(seqs, "labels")
  if (is.null(states)) {
    states <- sort(unique(unlist(seqs)))
  }
  a <- length(states)
  n <- length(seqs)

  # Create 3D transition array
  trans <- array(0L, dim = c(n, a, a))

  # Fill transition counts per sequence
  for (i in seq_len(n)) {
    seq_i <- seqs[[i]]
    if (length(seq_i) < 2) next

    for (j in seq_len(length(seq_i) - 1)) {
      from_state <- match(seq_i[j], states)
      to_state <- match(seq_i[j + 1], states)
      if (!is.na(from_state) && !is.na(to_state)) {
        trans[i, from_state, to_state] <- trans[i, from_state, to_state] + 1L
      }
    }
  }

  attr(trans, "states") <- states
  trans
}

# =============================================================================
# Matrix Conversion Helpers
# =============================================================================

#' Convert Network to Matrix
#'
#' Extracts or reconstructs the adjacency/weight matrix from a network object.
#'
#' @param x A network object (cograph_network, tna, or matrix).
#' @return A numeric weight matrix.
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C")
#' net <- as_cograph(mat)
#' to_matrix(net)
to_matrix <- function(x, directed = NULL) {
  if (is.matrix(x)) {
    return(x)
  }

  if (inherits(x, "tna")) {
    return(x$weights)
  }

  if (inherits(x, "cograph_network")) {
    if (is_tna_network(x)) {
      return(x$tna$model$weights)
    }

    # Reconstruct from edge list
    n <- x$n_nodes
    labels <- x$labels
    mat <- matrix(0, nrow = n, ncol = n)
    rownames(mat) <- colnames(mat) <- labels

    if (x$n_edges > 0) {
      for (i in seq_along(x$from)) {
        mat[x$from[i], x$to[i]] <- x$weight[i]
      }
    }
    return(mat)
  }

  stop("Cannot convert to matrix: ", class(x)[1], call. = FALSE)
}

# =============================================================================
# Centrality Computation
# =============================================================================

#' Compute Centrality Measures
#'
#' Computes centrality measures for a weight matrix.
#' Matches TNA's centrality computation.
#'
#' @param weights Weight matrix.
#' @param measures Character vector of measures to compute.
#'   Options: "InStrength", "OutStrength", "Betweenness".
#' @param loops Logical. Include self-loops in strength calculation.
#' @param normalize Logical. Normalize centrality values.
#' @return Data frame with state and centrality columns.
#'
#' @keywords internal
.compute_centrality <- function(weights, measures = c("InStrength", "OutStrength", "Betweenness"),
                                loops = FALSE, normalize = FALSE) {
  a <- nrow(weights)
  labels <- rownames(weights)
  if (is.null(labels)) labels <- as.character(seq_len(a))

  result <- data.frame(state = labels, stringsAsFactors = FALSE)

  if (!loops) {
    diag(weights) <- 0
  }

  if ("InStrength" %in% measures) {
    result$InStrength <- .colSums(weights, m = a, n = a)
    if (normalize && max(result$InStrength) > 0) {
      result$InStrength <- result$InStrength / max(result$InStrength)
    }
  }

  if ("OutStrength" %in% measures) {
    result$OutStrength <- .rowSums(weights, m = a, n = a)
    if (normalize && max(result$OutStrength) > 0) {
      result$OutStrength <- result$OutStrength / max(result$OutStrength)
    }
  }

  if ("Betweenness" %in% measures) {
    # Simple betweenness approximation (for full implementation, use igraph)
    if (requireNamespace("igraph", quietly = TRUE)) {
      # igraph requires positive weights for betweenness
      # Use absolute values for weighted betweenness
      abs_weights <- abs(weights)
      # Convert to distance (higher weight = shorter distance)
      max_w <- max(abs_weights[abs_weights > 0], na.rm = TRUE)
      if (is.finite(max_w) && max_w > 0) {
        dist_weights <- max_w - abs_weights + 0.001  # Small offset to avoid zero
        dist_weights[abs_weights == 0] <- 0
      } else {
        dist_weights <- abs_weights
      }
      g <- igraph::graph_from_adjacency_matrix(dist_weights, mode = "directed", weighted = TRUE)
      result$Betweenness <- igraph::betweenness(g, directed = TRUE, normalized = normalize)
    } else {
      # Fallback: use degree-based approximation
      in_str <- if ("InStrength" %in% names(result)) result$InStrength else .colSums(abs(weights), m = a, n = a)
      out_str <- if ("OutStrength" %in% names(result)) result$OutStrength else .rowSums(abs(weights), m = a, n = a)
      result$Betweenness <- (in_str + out_str) / 2
      if (normalize && max(result$Betweenness) > 0) {
        result$Betweenness <- result$Betweenness / max(result$Betweenness)
      }
    }
  }

  result
}

# =============================================================================
# Validation Helpers
# =============================================================================

#' Check if Validation Method is Available
#'
#' @param x Network object.
#' @param method Validation method name.
#' @return Logical.
#'
#' @keywords internal
.check_validation_available <- function(x, method) {
  info <- detect_data_type(x)
  if (!method %in% info$available_methods) {
    if (!info$has_raw_data) {
      stop(
        method, "() requires raw data for resampling.\n",
        "For cograph_network objects, use set_raw_data() to attach data and an estimator.\n",
        "For matrix-only input, only disparity_filter() is available.",
        call. = FALSE
      )
    }
    stop(method, "() is not available for this object type.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Create Edge Summary Data Frame
#'
#' Creates a data frame summarizing edge-level statistics.
#'
#' @param weights Original weight matrix.
#' @param weights_mean Mean weights from bootstrap.
#' @param weights_sd SD of weights from bootstrap.
#' @param ci_lower Lower CI bound.
#' @param ci_upper Upper CI bound.
#' @param p_values P-values per edge.
#' @param level Significance level.
#' @return Data frame with edge statistics.
#'
#' @keywords internal
.create_edge_summary <- function(weights, weights_mean, weights_sd,
                                 ci_lower, ci_upper, p_values, level) {
  a <- nrow(weights)
  labels <- rownames(weights)
  if (is.null(labels)) labels <- as.character(seq_len(a))

  # Find all non-zero edges
  edge_idx <- which(weights != 0, arr.ind = TRUE)

  if (nrow(edge_idx) == 0) {
    return(data.frame(
      from = character(0),
      to = character(0),
      weight = numeric(0),
      mean = numeric(0),
      sd = numeric(0),
      ci_lower = numeric(0),
      ci_upper = numeric(0),
      p_value = numeric(0),
      significant = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    from = labels[edge_idx[, 1]],
    to = labels[edge_idx[, 2]],
    weight = weights[edge_idx],
    mean = weights_mean[edge_idx],
    sd = weights_sd[edge_idx],
    ci_lower = ci_lower[edge_idx],
    ci_upper = ci_upper[edge_idx],
    p_value = p_values[edge_idx],
    significant = p_values[edge_idx] < level,
    stringsAsFactors = FALSE
  )
}
