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
#'   "coreness", "constraint", "harmonic". Also accepts directional shorthands:
#'   "indegree", "outdegree", "instrength", "outstrength", "incloseness",
#'   "outcloseness", "inharmonic", "outharmonic", "ineccentricity",
#'   "outeccentricity".
#' @param size_range Numeric vector of length 2: c(min_size, max_size).
#'   Default c(2, 8).
#' @param n Number of nodes (for validation).
#' @param scaling Scaling mode: "default" or "legacy".
#' @param scale_exp Dampening exponent applied to normalized centrality values
#'   before mapping to size range. Default 1 (linear).
#' @return Named list with 'sizes' (vector of node sizes) and 'values' (raw centrality values).
#' @keywords internal
resolve_centrality_sizes <- function(x, scale_by, size_range = c(2, 8), n = NULL,
                                     scaling = "default", scale_exp = 1) {
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

  # Map directional shorthands to (measure, mode) pairs
  directional_map <- list(
    indegree        = list(measure = "degree",       mode = "in"),
    outdegree       = list(measure = "degree",       mode = "out"),
    instrength      = list(measure = "strength",     mode = "in"),
    outstrength     = list(measure = "strength",     mode = "out"),
    incloseness     = list(measure = "closeness",    mode = "in"),
    outcloseness    = list(measure = "closeness",    mode = "out"),
    inharmonic      = list(measure = "harmonic",     mode = "in"),
    outharmonic     = list(measure = "harmonic",     mode = "out"),
    ineccentricity  = list(measure = "eccentricity", mode = "in"),
    outeccentricity = list(measure = "eccentricity", mode = "out")
  )

  measure_lower <- tolower(measure)
  if (measure_lower %in% names(directional_map)) {
    mapped <- directional_map[[measure_lower]]
    measure <- mapped$measure
    params$mode <- mapped$mode
  } else {
    measure <- match.arg(measure_lower, valid_measures)
  }

  # Build centrality call
  cent_args <- c(list(x = x, measures = measure), params)

  # Calculate centrality
  cent_result <- tryCatch({
    do.call(centrality, cent_args)
  }, error = function(e) {
    stop("Failed to calculate ", measure, " centrality: ", e$message, call. = FALSE) # nocov
  })

  # Extract the values (column name depends on measure and mode)
  value_cols <- setdiff(names(cent_result), "node")
  if (length(value_cols) == 0) { # nocov start
    stop("No centrality values returned for measure: ", measure, call. = FALSE)
  } # nocov end
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
    normalized <- normalized^scale_exp
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
    # Use node labels from data or indices (priority: labels > label > indices)
    if (!is.null(nodes)) {
      if ("labels" %in% names(nodes)) {
        return(as.character(nodes$labels))
      } else if ("label" %in% names(nodes)) {
        return(as.character(nodes$label))
      }
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
    # Guard against length mismatch (e.g., from weight rounding)
    if (length(priority) != n) {
      priority <- rep_len(priority, n)
    }
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

#' Check and Handle Duplicate Edges
#'
#' Detects duplicate edges in undirected networks and either errors with
#' guidance or aggregates them per the user's \code{edge_duplicates} setting.
#'
#' @param edges Edge data frame.
#' @param directed Logical: is the network directed?
#' @param edge_duplicates Aggregation method (NULL to error, or "sum"/"mean"/etc).
#' @return Possibly aggregated edge data frame.
#' @keywords internal
check_duplicate_edges <- function(edges, directed, edge_duplicates) {
  if (directed || is.null(edges) || nrow(edges) == 0) return(edges)

  dup_check <- detect_duplicate_edges(edges, directed = directed)
  if (!dup_check$has_duplicates) return(edges)

  if (is.null(edge_duplicates)) {
    dup_msg <- vapply(dup_check$info, function(d) {
      sprintf("  - Nodes %d-%d: %d edges (weights: %s)",
              d$nodes[1], d$nodes[2], d$count,
              paste(round(d$weights, 2), collapse = ", "))
    }, character(1))
    stop("Found ", length(dup_check$info), " duplicate edge pair(s) in undirected network:\n",
         paste(dup_msg, collapse = "\n"), "\n\n",
         "Specify how to handle with edge_duplicates parameter:\n",
         "  edge_duplicates = \"sum\"   # Sum weights\n",
         "  edge_duplicates = \"mean\"  # Average weights\n",
         "  edge_duplicates = \"first\" # Keep first edge\n",
         "  edge_duplicates = \"max\"   # Keep max weight\n",
         "  edge_duplicates = \"min\"   # Keep min weight\n",
         call. = FALSE)
  }

  aggregate_duplicate_edges(edges, edge_duplicates, directed = directed)
}

#' Compute Plot Limits
#'
#' Calculates xlim/ylim accounting for node radii, self-loop extents,
#' and layout margin padding.
#'
#' @param layout_mat Two-column layout matrix.
#' @param vsize_usr Node radii in user coordinates.
#' @param layout_margin Fractional margin padding.
#' @param edges Edge data frame.
#' @param n_edges Number of edges.
#' @param loop_rotations Per-edge loop rotation angles.
#' @return List with \code{xlim} and \code{ylim}.
#' @keywords internal
compute_plot_limits <- function(layout_mat, vsize_usr, layout_margin,
                                edges, n_edges, loop_rotations) {
  x_range <- range(layout_mat[, 1], na.rm = TRUE)
  y_range <- range(layout_mat[, 2], na.rm = TRUE)
  x_margin <- diff(x_range) * layout_margin
  y_margin <- diff(y_range) * layout_margin

  # Expand to encompass node radii at boundary nodes
  x_lo <- min(layout_mat[, 1] - vsize_usr, na.rm = TRUE)
  x_hi <- max(layout_mat[, 1] + vsize_usr, na.rm = TRUE)
  y_lo <- min(layout_mat[, 2] - vsize_usr, na.rm = TRUE)
  y_hi <- max(layout_mat[, 2] + vsize_usr, na.rm = TRUE)

  # Expand for self-loops
  if (n_edges > 0) {
    self_loop_idx <- which(edges$from == edges$to)
    if (length(self_loop_idx) > 0) {
      loop_extent <- 2.52
      ni <- edges$from[self_loop_idx]
      r <- vsize_usr[ni] * loop_extent
      rot <- loop_rotations[self_loop_idx]
      lx <- layout_mat[ni, 1] + r * cos(rot)
      ly <- layout_mat[ni, 2] + r * sin(rot)
      lr <- vsize_usr[ni] * 0.8
      x_lo <- min(x_lo, lx - lr)
      x_hi <- max(x_hi, lx + lr)
      y_lo <- min(y_lo, ly - lr)
      y_hi <- max(y_hi, ly + lr)
    }
  }

  list(
    xlim = c(min(x_range[1] - x_margin, x_lo), max(x_range[2] + x_margin, x_hi)),
    ylim = c(min(y_range[1] - y_margin, y_lo), max(y_range[2] + y_margin, y_hi))
  )
}

#' Resolve Edge Styles
#'
#' Converts edge style strings to numeric lty values and adjusts
#' edge widths for dotted style (30% reduction).
#'
#' @param edge_style Edge style specification (character or numeric).
#' @param edge_widths Numeric vector of edge widths.
#' @param n_edges Number of edges.
#' @return List with \code{ltys} (numeric lty vector) and \code{edge_widths}.
#' @keywords internal
resolve_edge_styles <- function(edge_style, edge_widths, n_edges) {
  edge_styles_raw <- recycle_to_length(edge_style, n_edges)
  ltys <- vapply(edge_styles_raw, function(s) {
    if (is.character(s)) {
      switch(s,
        "solid" = 1, "dashed" = 2, "dotted" = 3,
        "dotdash" = 4, "longdash" = 5, "twodash" = 6, 1)
    } else {
      s
    }
  }, numeric(1))
  # Dotted edges: reduce width by 30% to avoid overly thick appearance
  edge_widths[ltys == 3] <- edge_widths[ltys == 3] * 0.7
  list(ltys = ltys, edge_widths = edge_widths)
}

#' Resolve Donut Parameters
#'
#' Normalizes donut/pie overlay parameters into the canonical format
#' expected by render_nodes_splot(). Handles donut_fill auto-enable,
#' list conversion, empty-value NA replacement, color encoding (1/2/n-color),
#' shape inheritance, and border vectorization.
#'
#' @param donut_fill User-specified donut fill values.
#' @param donut_values Raw donut values (deprecated path).
#' @param donut_color Donut color specification (1, 2, or n colors).
#' @param donut_colors Deprecated donut colors (list).
#' @param donut_bg_color Background color.
#' @param donut_shape Donut base shape override.
#' @param donut_border_color Border color.
#' @param donut_outer_border_color Outer border color.
#' @param donut_line_type Line type.
#' @param donut_empty Logical: render empty rings?
#' @param shapes Resolved node shapes vector.
#' @param n_nodes Number of nodes.
#' @return Named list of effective_* parameters.
#' @keywords internal
resolve_donut_params <- function(donut_fill, donut_values, donut_color,
                                 donut_colors, donut_bg_color, donut_shape,
                                 donut_border_color, donut_outer_border_color,
                                 donut_line_type, donut_empty, shapes, n_nodes) {
  # Auto-enable donut fill when node_shape is "donut" but no fill specified
  if (is.null(donut_fill) && is.null(donut_values)) {
    if (any(shapes == "donut")) {
      donut_fill <- ifelse(shapes == "donut", 1.0, NA)
    }
  }

  # Handle donut_fill: convert to list format
  effective_donut_values <- donut_values
  if (!is.null(donut_fill)) {
    if (!is.list(donut_fill)) {
      fill_vec <- recycle_to_length(donut_fill, n_nodes)
      effective_donut_values <- as.list(fill_vec)
    } else {
      effective_donut_values <- donut_fill
    }
  }

  # Replace NA values with 0 when donut_empty = TRUE
  if (donut_empty && !is.null(effective_donut_values)) {
    na_idx <- which(vapply(effective_donut_values,
      function(v) length(v) == 1 && is.na(v), logical(1)))
    effective_donut_values[na_idx] <- lapply(na_idx, function(i) 0)
  }

  # Resolve donut colors (donut_color > donut_colors priority)
  effective_donut_colors <- NULL
  effective_bg_color <- donut_bg_color

  if (!is.null(donut_color)) {
    if (is.list(donut_color) && length(donut_color) == 2 * n_nodes) {
      effective_donut_colors <- as.list(donut_color[seq(1, 2 * n_nodes, by = 2)])
    } else if (length(donut_color) == 2) {
      effective_donut_colors <- as.list(rep(donut_color[1], n_nodes))
      effective_bg_color <- donut_color[2]
    } else if (length(donut_color) == 1) {
      effective_donut_colors <- as.list(rep(donut_color, n_nodes))
    } else {
      cols <- recycle_to_length(donut_color, n_nodes)
      effective_donut_colors <- as.list(cols)
    }
  } else if (!is.null(donut_colors)) {
    effective_donut_colors <- donut_colors
  } else if (any(shapes == "donut") || !is.null(effective_donut_values)) {
    effective_donut_colors <- as.list(rep("maroon", n_nodes))
  }

  # Resolve donut shapes (inherit from node_shape)
  valid_donut_base_shapes <- c("circle", "square", "hexagon", "triangle",
                               "diamond", "pentagon")
  if (is.null(donut_shape) || identical(donut_shape, "circle")) {
    effective_donut_shapes <- ifelse(
      shapes %in% valid_donut_base_shapes, shapes, "circle")
  } else {
    effective_donut_shapes <- recycle_to_length(donut_shape, n_nodes)
  }

  # Vectorize border params
  effective_donut_border_color <- if (!is.null(donut_border_color)) {
    recycle_to_length(donut_border_color, n_nodes)
  } else {
    NULL
  }

  effective_donut_outer_border_color <- if (!is.null(donut_outer_border_color)) {
    recycle_to_length(donut_outer_border_color, n_nodes)
  } else {
    NULL
  }

  effective_donut_line_type <- recycle_to_length(donut_line_type, n_nodes)

  list(
    donut_values = effective_donut_values,
    donut_colors = effective_donut_colors,
    bg_color = effective_bg_color,
    donut_shapes = effective_donut_shapes,
    donut_border_color = effective_donut_border_color,
    donut_outer_border_color = effective_donut_outer_border_color,
    donut_line_type = effective_donut_line_type
  )
}

#' Compute Edge Curvatures
#'
#' Determines per-edge curvature values based on reciprocal-edge detection,
#' curve mode, and layout geometry.
#'
#' @param curvature User-specified curvature scalar or vector.
#' @param curves Curve mode: FALSE, TRUE/"mutual", or "force".
#' @param edges Edge data frame with from/to columns.
#' @param layout_mat Two-column layout matrix.
#' @return List with \code{curves_vec}, \code{is_reciprocal}.
#' @keywords internal
compute_edge_curvatures <- function(curvature, curves, edges, layout_mat) {
  n_edges <- nrow(edges)
  if (n_edges == 0) {
    return(list(curves_vec = numeric(0), is_reciprocal = logical(0))) # nocov
  }

  # Identify reciprocal pairs via hash-set lookup (O(n))
  edge_keys <- paste(edges$from, edges$to, sep = "-")
  reverse_keys <- paste(edges$to, edges$from, sep = "-")
  is_reciprocal <- reverse_keys %in% edge_keys & edges$from != edges$to

  # Curve magnitude
  if (length(curvature) == 1 && curvature == 0) {
    curve_magnitudes <- rep(0.175, n_edges)
  } else {
    curve_magnitudes <- abs(recycle_to_length(curvature, n_edges))
  }

  curves_vec <- rep(0, n_edges)
  center_x <- mean(layout_mat[, 1])
  center_y <- mean(layout_mat[, 2])
  per_edge_curvature <- length(curvature) > 1

  # Helper: compute outward sign for a reciprocal edge pair
  outward_sign <- function(from_idx, to_idx) {
    lo <- min(from_idx, to_idx)
    hi <- max(from_idx, to_idx)
    dx_canon <- layout_mat[hi, 1] - layout_mat[lo, 1]
    dy_canon <- layout_mat[hi, 2] - layout_mat[lo, 2]
    perp_x <- -dy_canon
    perp_y <- dx_canon
    mid_x <- (layout_mat[from_idx, 1] + layout_mat[to_idx, 1]) / 2
    mid_y <- (layout_mat[from_idx, 2] + layout_mat[to_idx, 2]) / 2
    test_x <- mid_x + perp_x * 0.1
    test_y <- mid_y + perp_y * 0.1
    dist_pos <- sqrt((test_x - center_x)^2 + (test_y - center_y)^2)
    dist_orig <- sqrt((mid_x - center_x)^2 + (mid_y - center_y)^2)
    if (dist_pos > dist_orig) 1 else -1
  }

  if (per_edge_curvature) {
    # Per-edge curvature vector: apply sign for reciprocal separation
    non_self <- edges$from != edges$to & curve_magnitudes != 0
    idx <- which(non_self)
    curves_vec[idx] <- vapply(idx, function(i) {
      if (is_reciprocal[i]) {
        outward_sign(edges$from[i], edges$to[i]) * curve_magnitudes[i]
      } else {
        curve_magnitudes[i]
      }
    }, numeric(1))
  } else if (identical(curves, TRUE) || identical(curves, "mutual")) {
    # Curve reciprocal edges in opposite directions
    recip_idx <- which(is_reciprocal)
    curves_vec[recip_idx] <- vapply(recip_idx, function(i) {
      outward_sign(edges$from[i], edges$to[i]) * curve_magnitudes[i]
    }, numeric(1))
  } else if (identical(curves, "force")) {
    # Curve all non-self-loop edges
    non_self <- edges$from != edges$to
    curves_vec[non_self] <- curve_magnitudes[non_self]
  }
  # curves = FALSE: curves_vec stays at 0

  list(curves_vec = curves_vec, is_reciprocal = is_reciprocal)
}
