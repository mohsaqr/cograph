#' @title splot Parameter Vectorization Helpers
#' @description Functions for resolving and vectorizing splot() parameters.
#' @name splot-params
#' @keywords internal
NULL

#' Resolve Edge Colors
#'
#' Determines edge colors based on weights, explicit colors, or defaults.
#'
#' @param edges Edge data frame with from, to, weight columns.
#' @param edge.color User-specified edge color(s) or NULL.
#' @param posCol Color for positive weights.
#' @param negCol Color for negative weights.
#' @param default_col Default color when no weight.
#' @return Vector of colors for each edge.
#' @keywords internal
resolve_edge_colors <- function(edges, edge.color = NULL, posCol = "#2E7D32",
                                negCol = "#C62828", default_col = "gray50") {
  m <- nrow(edges)
  if (m == 0) return(character(0))

  if (!is.null(edge.color)) {
    # User-specified colors
    return(recycle_to_length(edge.color, m))
  }

  # Color by weight sign
  if ("weight" %in% names(edges)) {
    weights <- edges$weight
    colors <- ifelse(
      weights > 0, posCol,
      ifelse(weights < 0, negCol, default_col)
    )
    return(colors)
  }

  # Default
  rep(default_col, m)
}

#' Resolve Edge Widths
#'
#' Determines edge widths based on weights or explicit values.
#' Supports multiple scaling modes, two-tier cutoff, and output range specification.
#'
#' @param edges Edge data frame.
#' @param edge.width User-specified width(s) or NULL.
#' @param esize Base edge size. NULL uses adaptive sizing based on n_nodes.
#' @param n_nodes Number of nodes (for adaptive esize calculation).
#' @param directed Whether network is directed.
#' @param maximum Maximum weight for scaling (NULL for auto).
#' @param minimum Minimum weight threshold.
#' @param cut Two-tier cutoff. NULL = auto (75th pct), 0 = disabled.
#' @param edge_width_range Output width range c(min, max).
#' @param edge_scale_mode Scaling mode: "linear", "log", "sqrt", "rank".
#' @param scaling Scaling mode for constants: "default" or "legacy".
#' @param base_width Legacy: Base width value.
#' @param scale_factor Legacy: Width scaling factor.
#' @return Vector of widths for each edge.
#' @keywords internal
resolve_edge_widths <- function(edges,
                                edge.width = NULL,
                                esize = NULL,
                                n_nodes = NULL,
                                directed = FALSE,
                                maximum = NULL,
                                minimum = 0,
                                cut = NULL,
                                edge_width_range = NULL,
                                edge_scale_mode = NULL,
                                scaling = "default",
                                base_width = NULL,
                                scale_factor = NULL) {
  m <- nrow(edges)
  if (m == 0) return(numeric(0))

  # Handle "weight" string: signals to use weight-based scaling
  if (identical(edge.width, "weight")) {
    edge.width <- NULL
  }

  # If explicit widths provided, use them directly
  if (!is.null(edge.width) && is.numeric(edge.width)) {
    return(recycle_to_length(edge.width, m))
  }

  # Get scale constants
  scale <- get_scale_constants(scaling)

  # Use defaults from scale constants if not specified
  if (is.null(edge_width_range)) {
    edge_width_range <- scale$edge_width_range
  }
  if (is.null(edge_scale_mode)) {
    edge_scale_mode <- scale$edge_scale_mode
  }

  # Scale by weight if available
  if ("weight" %in% names(edges)) {
    return(scale_edge_widths(
      weights = edges$weight,
      esize = esize,
      n_nodes = n_nodes,
      directed = directed,
      mode = edge_scale_mode,
      maximum = maximum,
      minimum = minimum,
      cut = cut,
      range = edge_width_range
    ))
  }

  # Default width when no weights - use scale constants
  rep(scale$edge_width_default, m)
}

#' Resolve Node Sizes
#'
#' Converts vsize parameter to user coordinate sizes.
#'
#' @param vsize User-specified node size(s).
#' @param n Number of nodes.
#' @param default_size Default size if NULL (uses scale constants if NULL).
#' @param scale_factor Scale factor to apply (uses scale constants if NULL).
#' @param scaling Scaling mode: "default" or "legacy".
#' @return Vector of node sizes.
#' @keywords internal
resolve_node_sizes <- function(vsize, n, default_size = NULL, scale_factor = NULL,
                               scaling = "default") {
  scale <- get_scale_constants(scaling)

  # Use scale constants if not explicitly provided
  if (is.null(default_size)) {
    default_size <- scale$node_default
  }
  if (is.null(scale_factor)) {
    scale_factor <- scale$node_factor
  }

  if (is.null(vsize)) {
    vsize <- default_size
  }

  sizes <- recycle_to_length(vsize, n)

  # Convert to user coordinates (qgraph-style sizing)
  sizes * scale_factor
}

#' Resolve Centrality-Based Node Sizes
#'
#' Calculates node sizes based on centrality measures.
#'
#' @param x Network object (igraph, matrix, cograph_network, etc.)
#' @param scale_by Centrality measure name or list with measure and parameters.
#'   Valid measures: "degree", "strength", "betweenness", "closeness",
#'   "eigenvector", "pagerank", "authority", "hub", "eccentricity",
#'   "coreness", "constraint", "harmonic".
#' @param size_range Numeric vector of length 2: c(min_size, max_size).
#'   Default c(2, 8).
#' @param n Number of nodes (for validation).
#' @param scaling Scaling mode: "default" or "legacy".
#' @return Named list with 'sizes' (vector of node sizes) and 'values' (raw centrality values).
#' @keywords internal
resolve_centrality_sizes <- function(x, scale_by, size_range = c(2, 8), n = NULL,
                                     scaling = "default") {
  if (is.null(scale_by)) {
    return(NULL)
  }

  scale <- get_scale_constants(scaling)

  # Parse scale_by argument
  if (is.character(scale_by)) {
    measure <- scale_by
    params <- list()
  } else if (is.list(scale_by)) {
    measure <- scale_by$measure %||% scale_by[[1]]
    params <- scale_by[setdiff(names(scale_by), "measure")]
  } else {
    stop("scale_nodes_by must be a character string or list", call. = FALSE)
  }

  # Valid centrality measures
  valid_measures <- c("degree", "strength", "betweenness", "closeness",
                      "eigenvector", "pagerank", "authority", "hub",
                      "eccentricity", "coreness", "constraint", "transitivity",
                      "harmonic", "diffusion", "leverage", "kreach",
                      "resilience")

  measure <- match.arg(tolower(measure), valid_measures)

  # Build centrality call
  cent_args <- c(list(x = x, measures = measure), params)

  # Calculate centrality
  cent_result <- tryCatch({
    do.call(centrality, cent_args)
  }, error = function(e) {
    stop("Failed to calculate ", measure, " centrality: ", e$message, call. = FALSE)
  })

  # Extract the values (column name depends on measure and mode)
  value_cols <- setdiff(names(cent_result), "node")
  if (length(value_cols) == 0) {
    stop("No centrality values returned for measure: ", measure, call. = FALSE)
  }
  values <- cent_result[[value_cols[1]]]

  # Handle NA/NaN values
  values[is.na(values) | is.nan(values)] <- 0

  # Handle all-zero or constant values
  val_range <- range(values, na.rm = TRUE)
  if (val_range[1] == val_range[2]) {
    # All same value - use middle of size range
    sizes <- rep(mean(size_range), length(values))
  } else {
    # Scale to size range
    normalized <- (values - val_range[1]) / (val_range[2] - val_range[1])
    sizes <- size_range[1] + normalized * (size_range[2] - size_range[1])
  }

  # Apply scale factor
  sizes <- sizes * scale$node_factor

  list(
    sizes = sizes,
    values = values,
    measure = measure,
    labels = cent_result$node
  )
}

#' Resolve Label Sizes
#'
#' Determines label sizes, either independent (new default) or coupled to node size (legacy).
#'
#' @param label_size User-specified label size(s) or NULL.
#' @param node_size_usr Node sizes in user coordinates (for legacy coupled mode).
#' @param n Number of nodes.
#' @param scaling Scaling mode: "default" or "legacy".
#' @return Vector of label sizes (cex values).
#' @keywords internal
resolve_label_sizes <- function(label_size, node_size_usr, n, scaling = "default") {
  scale <- get_scale_constants(scaling)

  if (!is.null(label_size)) {
    # User explicitly specified - use as-is
    return(recycle_to_length(label_size, n))
  }

  if (scale$label_coupled) {
    # Legacy mode: couple to node size (original behavior)
    # vsize_usr * 8, capped at 1
    return(pmin(1, node_size_usr * 8))
  }

 # New default: independent label size
  rep(scale$label_default, n)
}

#' Resolve Node Colors
#'
#' Determines node colors from various inputs.
#'
#' @param color User-specified color(s) or NULL.
#' @param n Number of nodes.
#' @param nodes Node data frame (for group coloring).
#' @param groups Group assignments for color mapping.
#' @param default_col Default node color.
#' @return Vector of colors for each node.
#' @keywords internal
resolve_node_colors <- function(color, n, nodes = NULL, groups = NULL,
                                default_col = "#4A90D9") {
  if (!is.null(color)) {
    return(recycle_to_length(color, n))
  }

  # Color by groups if provided
  if (!is.null(groups)) {
    unique_groups <- unique(groups)
    n_groups <- length(unique_groups)
    palette <- grDevices::rainbow(n_groups, s = 0.7, v = 0.9)
    colors <- palette[match(groups, unique_groups)]
    return(colors)
  }

  # Color from node data if available
  if (!is.null(nodes) && "color" %in% names(nodes)) {
    return(nodes$color)
  }

  rep(default_col, n)
}

#' Resolve Labels
#'
#' Determines node labels from various inputs.
#'
#' @param labels User-specified labels: TRUE, FALSE, character vector, or NULL.
#' @param nodes Node data frame.
#' @param n Number of nodes.
#' @return Character vector of labels (or NULL for no labels).
#' @keywords internal
resolve_labels <- function(labels, nodes, n) {
  if (is.null(labels) || identical(labels, FALSE)) {
    return(NULL)
  }

  if (identical(labels, TRUE)) {
    # Use node labels from data or indices
    if (!is.null(nodes) && "label" %in% names(nodes)) {
      return(as.character(nodes$label))
    }
    return(as.character(seq_len(n)))
  }

  # User-provided labels
  recycle_to_length(as.character(labels), n)
}

#' Resolve Edge Labels
#'
#' Determines edge labels from various inputs.
#'
#' @param edge.labels User-specified labels: TRUE, FALSE, character vector, or NULL.
#' @param edges Edge data frame.
#' @param m Number of edges.
#' @return Character vector of labels (or NULL for no labels).
#' @keywords internal
resolve_edge_labels <- function(edge.labels, edges, m) {
  if (is.null(edge.labels) || identical(edge.labels, FALSE)) {
    return(NULL)
  }

  if (identical(edge.labels, TRUE)) {
    # Use weights as labels if available
    if (!is.null(edges) && "weight" %in% names(edges)) {
      return(as.character(round(edges$weight, 2)))
    }
    return(rep("", m))
  }

  # User-provided labels
  recycle_to_length(as.character(edge.labels), m)
}

#' Resolve Shape Parameter
#'
#' Converts shape specification to vector of shape names.
#'
#' @param shape Shape specification.
#' @param n Number of nodes.
#' @return Character vector of shape names.
#' @keywords internal
resolve_shapes <- function(shape, n) {
  if (is.null(shape)) {
    shape <- "circle"
  }
  recycle_to_length(shape, n)
}

#' Resolve Curvature Parameter
#'
#' Determines edge curvatures, handling reciprocal edges.
#'
#' @param curve User-specified curvature(s).
#' @param edges Edge data frame.
#' @param curveScale Logical: scale curvature for reciprocal edges?
#' @param default_curve Default curvature for reciprocal edges.
#' @return Vector of curvatures.
#' @keywords internal
resolve_curvatures <- function(curve, edges, curveScale = TRUE,
                               default_curve = 0.2) {
  m <- nrow(edges)
  if (m == 0) return(numeric(0))

  curves <- recycle_to_length(curve, m)

  if (!curveScale) {
    return(curves)
  }

  # Identify reciprocal edges and apply default curvature
  for (i in seq_len(m)) {
    from_i <- edges$from[i]
    to_i <- edges$to[i]

    if (from_i == to_i) next  # Skip self-loops

    # Check for reciprocal
    for (j in seq_len(m)) {
      if (j != i && edges$from[j] == to_i && edges$to[j] == from_i) {
        # Found reciprocal - apply curvature if not already set
        if (curves[i] == 0) {
          curves[i] <- default_curve
        }
        break
      }
    }
  }

  curves
}

#' Resolve Loop Rotation
#'
#' Determines rotation angle for self-loops.
#'
#' @param loopRotation User-specified rotation(s) or NULL.
#' @param edges Edge data frame.
#' @param layout Layout coordinates (to auto-calculate optimal rotation).
#' @return Vector of rotation angles in radians.
#' @keywords internal
resolve_loop_rotation <- function(loopRotation, edges, layout = NULL) {
  m <- nrow(edges)
  if (m == 0) return(numeric(0))

  # Find self-loops
  is_loop <- edges$from == edges$to

  if (is.null(loopRotation)) {
    # Default: loop at top (pi/2)
    rotations <- rep(pi/2, m)

    # If layout provided, point away from center
    if (!is.null(layout)) {
      center_x <- mean(layout[, 1], na.rm = TRUE)
      center_y <- mean(layout[, 2], na.rm = TRUE)

      for (i in which(is_loop)) {
        node_idx <- edges$from[i]
        node_x <- layout[node_idx, 1]
        node_y <- layout[node_idx, 2]

        # Angle away from center
        rotations[i] <- atan2(node_y - center_y, node_x - center_x)
      }
    }

    return(rotations)
  }

  recycle_to_length(loopRotation, m)
}

#' Filter Edges by Weight Threshold
#'
#' Removes edges below the minimum weight threshold.
#'
#' @param edges Edge data frame.
#' @param minimum Minimum absolute weight to include.
#' @return Filtered edge data frame.
#' @keywords internal
filter_edges_by_weight <- function(edges, minimum = 0) {
  if (minimum == 0 || !"weight" %in% names(edges)) {
    return(edges)
  }

  edges[abs(edges$weight) >= minimum, , drop = FALSE]
}

#' Get Edge Rendering Order
#'
#' Returns indices for rendering edges from weakest to strongest.
#'
#' @param edges Edge data frame.
#' @param priority Optional numeric vector of edge priorities. Higher = on top.
#' @return Integer vector of indices.
#' @keywords internal
get_edge_order <- function(edges, priority = NULL) {
  n <- nrow(edges)
  if (n == 0) return(integer(0))

  # If priority provided, use it as primary sort key
  if (!is.null(priority)) {
    # Sort by priority first (low to high), then by weight
    weights <- if ("weight" %in% names(edges)) abs(edges$weight) else rep(0, n)
    return(order(priority, weights))
  }

  # Default: order by weight (weakest first)
  if (!"weight" %in% names(edges)) {
    return(seq_len(n))
  }

  order(abs(edges$weight))
}

#' Get Node Rendering Order
#'
#' Returns indices for rendering nodes from largest to smallest.
#'
#' @param sizes Vector of node sizes.
#' @return Integer vector of indices.
#' @keywords internal
get_node_order <- function(sizes) {
  order(sizes, decreasing = TRUE)
}
