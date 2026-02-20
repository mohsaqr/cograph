#' @title Transition Flow Visualization
#' @description Alluvial/Sankey diagram for visualizing transitions between
#'   two categorical states, such as cluster membership changes.
#' @name plot-transitions
NULL

#' Plot Transitions Between States
#'
#' Creates an elegant alluvial/Sankey diagram showing how items flow from
#' one set of categories to another. Useful for visualizing cluster
#' transitions, state changes, or any categorical mapping.
#'
#' @param x Input data in one of several formats:
#'   \itemize{
#'     \item A transition matrix (rows = from, cols = to, values = counts)
#'     \item Two vectors: pass \code{before} as x and \code{after} as second argument
#'       (contingency table computed automatically, like chi-square)
#'     \item A 2-column data frame (raw observations; table computed automatically)
#'     \item A data frame with columns: from, to, count
#'     \item A list of matrices for multi-step transitions
#'   }
#' @param from_title Title for the left column. Default "From". For multi-step,
#'   use a vector of titles (e.g., c("T1", "T2", "T3", "T4")).
#' @param to_title Title for the right column. Default "To". Ignored for multi-step.
#' @param from_colors Colors for left-side nodes. Default uses palette.
#' @param to_colors Colors for right-side nodes. Default uses palette.
#' @param flow_fill Fill color for flows. Default "#888888" (grey). Ignored if flow_color_by is set.
#' @param flow_alpha Alpha transparency for flows. Default 0.4.
#' @param flow_color_by Color flows by "source", "destination", or NULL (use flow_fill). Default NULL.
#' @param flow_border Border color for flows. Default NA (no border).
#' @param flow_border_width Line width for flow borders. Default 0.5.
#' @param node_width Width of node rectangles (0-1 scale). Default 0.08.
#' @param node_border Border color for nodes. Default NA (no border).
#' @param node_spacing Vertical spacing between nodes (0-1 scale). Default 0.02.
#' @param label_size Size of node labels. Default 3.5.
#' @param label_position Position of node labels: "beside" (default), "inside", "above", "below", "outside".
#' @param label_halo Logical: add white halo around labels for readability? Default TRUE.
#' @param title_size Size of column titles. Default 5.
#' @param curve_strength Controls bezier curve shape (0-1). Default 0.6.
#' @param show_values Logical: show transition counts on flows? Default FALSE.
#' @param value_position Position of flow values: "center", "origin", "destination",
#'   "outside_origin", "outside_destination". Default "center".
#' @param value_size Size of value labels on flows. Default 3.
#' @param value_color Color of value labels. Default "black".
#' @param show_totals Logical: show total counts on nodes? Default FALSE.
#' @param total_size Size of total labels. Default 4.
#' @param total_color Color of total labels. Default "white".
#' @param conserve_flow Logical: should left and right totals match? Default TRUE.
#'   When FALSE, each side scales independently (allows for "lost" or "gained" items).
#' @param min_flow Minimum flow value to display. Default 0 (show all).
#' @param column_gap Horizontal spread of columns (0-1). Default 1 uses full width.
#'   Use smaller values (e.g., 0.6) to bring columns closer together.
#' @param track_individuals Logical: draw individual lines instead of aggregated flows?
#'   Default FALSE. When TRUE, each row in the data frame becomes a separate line.
#' @param line_alpha Alpha for individual tracking lines. Default 0.3.
#' @param line_width Width of individual tracking lines. Default 0.5.
#' @param jitter_amount Vertical jitter for individual lines (0-1). Default 0.8.
#' @param proportional_nodes Logical: size nodes proportionally to counts? Default TRUE.
#' @param node_label_format Format string for node labels with \code{{state}} and
#'   \code{{count}} placeholders. Default NULL (plain state name).
#'   Example: \code{"{state} (n={count})"}.
#' @param bundle_size Controls line bundling for large datasets. Default NULL (no bundling).
#'   Integer >= 2: each drawn line represents that many cases.
#'   Numeric in (0,1): reduce to this fraction of original lines
#'   (e.g., 0.15 keeps ~15\% of lines).
#' @param bundle_legend Logical: show annotation when bundling is active? Default TRUE.
#'
#' @return A ggplot2 object.
#'
#' @details
#' The function creates smooth bezier curves connecting nodes from the left
#' column to the right column. Flow width is proportional to the transition
#' count. Nodes are sized proportionally to their total flow.
#'
#' @examples
#' \dontrun{
#' # From a transition matrix
#' mat <- matrix(c(50, 10, 5, 15, 40, 10, 5, 20, 30), 3, 3, byrow = TRUE)
#' rownames(mat) <- c("Light", "Resource", "Intense")
#' colnames(mat) <- c("Light", "PBL", "Resource")
#' plot_transitions(mat, from_title = "Time 1", to_title = "Time 2")
#'
#' # From two vectors (like chi-square) - auto-computes contingency table
#' before <- c("A", "A", "B", "B", "A", "C", "B", "C")
#' after <- c("X", "Y", "X", "Z", "X", "Y", "Z", "X")
#' plot_transitions(before, after)
#'
#' # From a 2-column data frame
#' df <- data.frame(time1 = before, time2 = after)
#' plot_transitions(df, from_title = "Time 1", to_title = "Time 2")
#'
#' # Multi-step transitions (list of matrices)
#' plot_transitions(list(mat1, mat2, mat3),
#'   from_title = c("T1", "T2", "T3", "T4"),
#'   show_totals = TRUE
#' )
#'
#' # Custom colors
#' plot_transitions(mat,
#'   from_colors = c("#FFD166", "#06D6A0", "#9D4EDD"),
#'   to_colors = c("#FFD166", "#EF476F", "#06D6A0")
#' )
#' }
#'
#' @import ggplot2
#' @export
plot_transitions <- function(x,
                             from_title = "From",
                             to_title = "To",
                             from_colors = NULL,
                             to_colors = NULL,
                             flow_fill = "#888888",
                             flow_alpha = 0.4,
                             flow_color_by = NULL,
                             flow_border = NA,
                             flow_border_width = 0.5,
                             node_width = 0.08,
                             node_border = NA,
                             node_spacing = 0.02,
                             label_size = 3.5,
                             label_position = c("beside", "inside", "above", "below", "outside"),
                             label_halo = TRUE,
                             title_size = 5,
                             curve_strength = 0.6,
                             show_values = FALSE,
                             value_position = c("center", "origin", "destination", "outside_origin", "outside_destination"),
                             value_size = 3,
                             value_color = "black",
                             show_totals = FALSE,
                             total_size = 4,
                             total_color = "white",
                             conserve_flow = TRUE,
                             min_flow = 0,
                             column_gap = 1,
                             track_individuals = FALSE,
                             line_alpha = 0.3,
                             line_width = 0.5,
                             jitter_amount = 0.8,
                             proportional_nodes = TRUE,
                             node_label_format = NULL,
                             bundle_size = NULL,
                             bundle_legend = TRUE) {

  label_position <- match.arg(label_position)
  value_position <- match.arg(value_position)

  # Handle multi-step transitions (list of matrices)
  if (is.list(x) && !is.data.frame(x)) {
    return(.plot_transitions_multi(
      x, titles = from_title, colors = from_colors,
      flow_fill = flow_fill, flow_alpha = flow_alpha,
      flow_color_by = flow_color_by,
      flow_border = flow_border, flow_border_width = flow_border_width,
      node_width = node_width, node_border = node_border,
      node_spacing = node_spacing, label_size = label_size,
      label_position = label_position, label_halo = label_halo,
      title_size = title_size,
      curve_strength = curve_strength, show_values = show_values,
      value_position = value_position, value_size = value_size,
      value_color = value_color, show_totals = show_totals,
      total_size = total_size, total_color = total_color,
      min_flow = min_flow, column_gap = column_gap
    ))
  }

  # Handle two vectors input (like chi-square: compute contingency table)
  # If x is a character/factor vector and from_title is also a vector of same length,
  # treat them as before/after observations
  if (is.vector(x) && !is.matrix(x) && length(x) > 2 &&
      is.vector(from_title) && length(from_title) == length(x)) {
    # x is "from" vector, from_title is "to" vector
    to_vec <- from_title
    x <- table(x, to_vec)
    from_title <- "From"
    to_title <- "To"
  }

  # Handle individual tracking mode
  if (track_individuals && is.data.frame(x) && ncol(x) >= 2 &&
      !all(c("from", "to", "count") %in% names(x))) {
    # Use column names as titles if not provided
    if (is.null(from_title) || identical(from_title, "From")) {
      from_title <- names(x)
    }
    return(.plot_individual_tracks(
      x, titles = from_title, colors = from_colors,
      flow_color_by = flow_color_by,
      node_width = node_width, node_border = node_border,
      node_spacing = node_spacing, label_size = label_size,
      label_position = label_position, label_halo = label_halo,
      title_size = title_size,
      curve_strength = curve_strength,
      line_alpha = line_alpha, line_width = line_width,
      jitter_amount = jitter_amount,
      show_totals = show_totals, total_size = total_size,
      total_color = total_color, column_gap = column_gap,
      proportional_nodes = proportional_nodes,
      node_label_format = node_label_format,
      bundle_size = bundle_size, bundle_legend = bundle_legend
    ))
  }

  # Convert input to standard format
  if (is.matrix(x) || inherits(x, "table")) {
    trans_df <- .matrix_to_trans_df(as.matrix(x))
  } else if (is.data.frame(x)) {
    # Check if it's a multi-column data frame (raw data for multiple time points)
    if (ncol(x) >= 3 && !all(c("from", "to", "count") %in% names(x))) {
      # Compute transition matrices between consecutive columns
      matrices <- list()
      for (i in seq_len(ncol(x) - 1)) {
        tab <- table(x[[i]], x[[i + 1]])
        matrices[[i]] <- as.matrix(tab)
      }
      # Use column names as titles if available
      if (is.null(from_title) || identical(from_title, "From")) {
        from_title <- names(x)
      }
      # Call multi-step function
      return(.plot_transitions_multi(
        matrices, titles = from_title, colors = from_colors,
        flow_fill = flow_fill, flow_alpha = flow_alpha,
        flow_color_by = flow_color_by,
        flow_border = flow_border, flow_border_width = flow_border_width,
        node_width = node_width, node_border = node_border,
        node_spacing = node_spacing, label_size = label_size,
        label_position = label_position, label_halo = label_halo,
        title_size = title_size,
        curve_strength = curve_strength, show_values = show_values,
        value_position = value_position, value_size = value_size,
        value_color = value_color, show_totals = show_totals,
        total_size = total_size, total_color = total_color,
        min_flow = min_flow, column_gap = column_gap
      ))
    } else if (ncol(x) == 2 && !all(c("from", "to", "count") %in% names(x))) {
      # Compute contingency table from two columns
      tab <- table(x[[1]], x[[2]])
      trans_df <- .matrix_to_trans_df(as.matrix(tab))
    } else if (all(c("from", "to", "count") %in% names(x))) {
      trans_df <- x
    } else {
      stop("Data frame must have 2+ columns (raw data) or columns: from, to, count", call. = FALSE)
    }
  } else {
    stop("x must be a matrix, data frame, two vectors, or list of matrices", call. = FALSE)
  }

  # Filter by min_flow
  if (min_flow > 0) {
    trans_df <- trans_df[trans_df$count >= min_flow, ]
  }

  # Get unique states
  from_states <- unique(trans_df$from)
  to_states <- unique(trans_df$to)
  n_from <- length(from_states)
  n_to <- length(to_states)

  # Default colors
  if (is.null(from_colors)) {
    from_colors <- .default_transition_palette(n_from)
  }
  if (is.null(to_colors)) {
    to_colors <- .default_transition_palette(n_to)
  }

  # Ensure colors are named
  if (is.null(names(from_colors))) {
    names(from_colors) <- from_states
  }
  if (is.null(names(to_colors))) {
    names(to_colors) <- to_states
  }

  # Calculate node sizes (total flow in/out)
  from_totals <- tapply(trans_df$count, trans_df$from, sum)
  to_totals <- tapply(trans_df$count, trans_df$to, sum)

  # Normalize to 0-1 scale
  if (conserve_flow) {
    # Both sides use same total for proportions
    total_flow <- sum(trans_df$count)
    from_heights <- as.numeric(from_totals) / total_flow
    to_heights <- as.numeric(to_totals) / total_flow
  } else {
    # Each side scales independently
    from_heights <- as.numeric(from_totals) / sum(from_totals)
    to_heights <- as.numeric(to_totals) / sum(to_totals)
  }

  # Scale heights to use available space (leaving room for spacing)
  available_height <- 1 - (max(n_from, n_to) - 1) * node_spacing
  from_heights <- from_heights * available_height
  to_heights <- to_heights * available_height

  # Calculate node positions
  from_nodes <- .calculate_node_positions(from_states, from_heights, node_spacing)
  to_nodes <- .calculate_node_positions(to_states, to_heights, node_spacing)

  # X positions
  x_left <- 0
  x_right <- 1

  # Build flow polygons
  flow_data <- .build_flow_polygons(
    trans_df, from_nodes, to_nodes,
    x_left, x_right, node_width, curve_strength, value_position
  )
  flow_polys <- flow_data$polys
  flow_centers <- flow_data$centers

  # Build node rectangles
  node_rects <- .build_node_rects(
    from_nodes, to_nodes, from_colors, to_colors,
    x_left, x_right, node_width, from_totals, to_totals
)

  # Create plot
  p <- ggplot() +
    # Flows (draw first, behind nodes)
    geom_polygon(
      data = flow_polys,
      aes(x = x, y = y, group = id),
      fill = flow_fill,
      alpha = flow_alpha,
      color = if (is.na(flow_border)) NA else flow_border,
      linewidth = flow_border_width
    ) +
    # Nodes
    geom_rect(
      data = node_rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color),
      color = if (is.na(node_border)) NA else node_border
    ) +
    scale_fill_identity()

  # Halo offsets for text readability
  halo_off <- 0.004
  from_data <- node_rects[node_rects$side == "from", ]
  to_data <- node_rects[node_rects$side == "to", ]

  # Add node labels based on position
  if (label_position == "beside") {
    # Labels beside nodes (outside)
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p +
              geom_text(data = from_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                        hjust = 0, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off) +
              geom_text(data = to_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                        hjust = 1, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off)
          }
        }
      }
    }
    p <- p +
      geom_text(data = from_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                hjust = 0, size = label_size) +
      geom_text(data = to_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                hjust = 1, size = label_size)

  } else if (label_position == "inside") {
    # Labels inside nodes (no halo needed - white on colored)
    p <- p +
      geom_text(data = from_data, aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
                hjust = 0.5, size = label_size, color = "white", fontface = "bold") +
      geom_text(data = to_data, aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
                hjust = 0.5, size = label_size, color = "white", fontface = "bold")

  } else if (label_position == "above") {
    # Labels above nodes
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p +
              geom_text(data = from_data, aes(x = (xmin + xmax) / 2, y = ymax + 0.02, label = label),
                        hjust = 0.5, vjust = 0, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off) +
              geom_text(data = to_data, aes(x = (xmin + xmax) / 2, y = ymax + 0.02, label = label),
                        hjust = 0.5, vjust = 0, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off)
          }
        }
      }
    }
    p <- p +
      geom_text(data = from_data, aes(x = (xmin + xmax) / 2, y = ymax + 0.02, label = label),
                hjust = 0.5, vjust = 0, size = label_size) +
      geom_text(data = to_data, aes(x = (xmin + xmax) / 2, y = ymax + 0.02, label = label),
                hjust = 0.5, vjust = 0, size = label_size)

  } else if (label_position == "below") {
    # Labels below nodes
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p +
              geom_text(data = from_data, aes(x = (xmin + xmax) / 2, y = ymin - 0.02, label = label),
                        hjust = 0.5, vjust = 1, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off) +
              geom_text(data = to_data, aes(x = (xmin + xmax) / 2, y = ymin - 0.02, label = label),
                        hjust = 0.5, vjust = 1, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off)
          }
        }
      }
    }
    p <- p +
      geom_text(data = from_data, aes(x = (xmin + xmax) / 2, y = ymin - 0.02, label = label),
                hjust = 0.5, vjust = 1, size = label_size) +
      geom_text(data = to_data, aes(x = (xmin + xmax) / 2, y = ymin - 0.02, label = label),
                hjust = 0.5, vjust = 1, size = label_size)

  } else if (label_position == "outside") {
    # Labels on outer edges (far left for from, far right for to)
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p +
              geom_text(data = from_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                        hjust = 1, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off) +
              geom_text(data = to_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                        hjust = 0, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off)
          }
        }
      }
    }
    p <- p +
      geom_text(data = from_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                hjust = 1, size = label_size) +
      geom_text(data = to_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                hjust = 0, size = label_size)
  }

  # Add totals on nodes if requested
  if (show_totals) {
    p <- p +
      geom_text(
        data = node_rects,
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = total),
        size = total_size, color = total_color, fontface = "bold"
      )
  }

  # Column titles with halo
  title_y <- max(node_rects$ymax) + 0.08
  title_x_left <- x_left + node_width / 2
  title_x_right <- x_right - node_width / 2

  if (label_halo) {
    for (dx in c(-1, 0, 1)) {
      for (dy in c(-1, 0, 1)) {
        if (dx != 0 || dy != 0) {
          p <- p +
            annotate("text", x = title_x_left + dx * halo_off, y = title_y + dy * halo_off,
                     label = from_title, size = title_size, fontface = "bold", color = "white") +
            annotate("text", x = title_x_right + dx * halo_off, y = title_y + dy * halo_off,
                     label = to_title, size = title_size, fontface = "bold", color = "white")
        }
      }
    }
  }
  p <- p +
    annotate("text", x = title_x_left, y = title_y,
             label = from_title, size = title_size, fontface = "bold") +
    annotate("text", x = title_x_right, y = title_y,
             label = to_title, size = title_size, fontface = "bold")

  # Add value labels on flows if requested
  if (show_values && nrow(flow_centers) > 0) {
    p <- p + geom_text(
      data = flow_centers,
      aes(x = x, y = y, label = value),
      size = value_size,
      color = value_color
    )
  }

  p <- p +
    # Theme
    coord_cartesian(xlim = c(-0.15, 1.15), ylim = c(min(node_rects$ymin) - 0.1, max(node_rects$ymax) + 0.15), clip = "off") +
    theme_void() +
    theme(
      plot.margin = margin(20, 20, 20, 20)
    )

  p
}


# =============================================================================
# Helper Functions
# =============================================================================

#' Convert transition matrix to data frame
#' @noRd
.matrix_to_trans_df <- function(mat) {
  from_names <- rownames(mat)
  to_names <- colnames(mat)

  if (is.null(from_names)) from_names <- paste0("From_", seq_len(nrow(mat)))
  if (is.null(to_names)) to_names <- paste0("To_", seq_len(ncol(mat)))

  # Build data frame
  df <- expand.grid(from = from_names, to = to_names, stringsAsFactors = FALSE)
  df$count <- as.vector(mat)

  # Remove zero flows
  df <- df[df$count > 0, ]
  df
}

#' Calculate vertical positions for nodes
#' @noRd
.calculate_node_positions <- function(states, heights, spacing) {
  n <- length(states)
  positions <- data.frame(
    state = states,
    height = heights,
    stringsAsFactors = FALSE
  )

  # Calculate total height needed
 total_height <- sum(heights) + (n - 1) * spacing

  # Center vertically around 0.5
  start_top <- 0.5 + total_height / 2

  positions$top <- NA
  positions$bottom <- NA

  current_top <- start_top
  for (i in seq_len(n)) {
    positions$top[i] <- current_top
    positions$bottom[i] <- current_top - heights[i]
    current_top <- positions$bottom[i] - spacing
  }

  positions
}

#' Build flow polygon coordinates using bezier curves
#' @noRd
.build_flow_polygons <- function(trans_df, from_nodes, to_nodes,
                                  x_left, x_right, node_width, curve_strength,
                                  value_position = "center") {
  polys <- list()
  centers <- list()
  poly_id <- 1

  # Track current position within each node for stacking flows
  from_current <- setNames(from_nodes$top, from_nodes$state)
  to_current <- setNames(to_nodes$top, to_nodes$state)

  # Total flows for calculating proportions
  from_totals <- tapply(trans_df$count, trans_df$from, sum)
  to_totals <- tapply(trans_df$count, trans_df$to, sum)

  for (i in seq_len(nrow(trans_df))) {
    from_state <- trans_df$from[i]
    to_state <- trans_df$to[i]
    count <- trans_df$count[i]

    # Calculate flow heights proportional to node heights
    from_idx <- which(from_nodes$state == from_state)
    to_idx <- which(to_nodes$state == to_state)

    from_node_height <- from_nodes$height[from_idx]
    to_node_height <- to_nodes$height[to_idx]

    flow_height_from <- from_node_height * (count / from_totals[from_state])
    flow_height_to <- to_node_height * (count / to_totals[to_state])

    # Starting positions (top of current available space)
    y_from_top <- from_current[from_state]
    y_from_bottom <- y_from_top - flow_height_from

    y_to_top <- to_current[to_state]
    y_to_bottom <- y_to_top - flow_height_to

    # Update current positions
    from_current[from_state] <- y_from_bottom
    to_current[to_state] <- y_to_bottom

    # X coordinates
    x_from <- x_left + node_width
    x_to <- x_right - node_width

    # Build bezier polygon
    poly <- .create_bezier_ribbon(
      x_from, y_from_top, y_from_bottom,
      x_to, y_to_top, y_to_bottom,
      curve_strength
    )
    poly$id <- poly_id
    poly$from_state <- from_state
    poly$to_state <- to_state
    polys[[poly_id]] <- poly

    # Store point for value label based on value_position
    if (value_position == "origin") {
      label_x <- x_from + 0.03
      label_y <- (y_from_top + y_from_bottom) / 2
    } else if (value_position == "destination") {
      label_x <- x_to - 0.03
      label_y <- (y_to_top + y_to_bottom) / 2
    } else if (value_position == "outside_origin") {
      label_x <- x_from - node_width - 0.02
      label_y <- (y_from_top + y_from_bottom) / 2
    } else if (value_position == "outside_destination") {
      label_x <- x_to + node_width + 0.02
      label_y <- (y_to_top + y_to_bottom) / 2
    } else {
      # center - use bezier midpoint
      t_mid <- 0.5
      mid_y_top <- (1-t_mid)^3 * y_from_top + 3*(1-t_mid)^2*t_mid * y_from_top +
                   3*(1-t_mid)*t_mid^2 * y_to_top + t_mid^3 * y_to_top
      mid_y_bottom <- (1-t_mid)^3 * y_from_bottom + 3*(1-t_mid)^2*t_mid * y_from_bottom +
                      3*(1-t_mid)*t_mid^2 * y_to_bottom + t_mid^3 * y_to_bottom
      label_x <- (x_from + x_to) / 2
      label_y <- (mid_y_top + mid_y_bottom) / 2
    }

    centers[[poly_id]] <- data.frame(
      id = poly_id,
      x = label_x,
      y = label_y,
      value = count,
      stringsAsFactors = FALSE
    )

    poly_id <- poly_id + 1
  }

  list(
    polys = do.call(rbind, polys),
    centers = do.call(rbind, centers)
  )
}

#' Create bezier ribbon polygon
#' @noRd
.create_bezier_ribbon <- function(x0, y0_top, y0_bottom,
                                   x1, y1_top, y1_bottom,
                                   strength, n_points = 50) {
  t <- seq(0, 1, length.out = n_points)

  # Cubic bezier with two control points for S-curve

  # Control points are horizontally offset but keep source/target y values
  # This creates the characteristic "flat exit, curved middle, flat entry" look
  cp1_x <- x0 + (x1 - x0) * strength
  cp2_x <- x1 - (x1 - x0) * strength

  # Top edge: cubic bezier from (x0, y0_top) to (x1, y1_top)
  # P0 = (x0, y0_top), P1 = (cp1_x, y0_top), P2 = (cp2_x, y1_top), P3 = (x1, y1_top)
  top_x <- (1-t)^3 * x0 + 3*(1-t)^2*t * cp1_x + 3*(1-t)*t^2 * cp2_x + t^3 * x1
  top_y <- (1-t)^3 * y0_top + 3*(1-t)^2*t * y0_top + 3*(1-t)*t^2 * y1_top + t^3 * y1_top

  # Bottom edge: same curve shape but for bottom coordinates
  bottom_x <- (1-t)^3 * x0 + 3*(1-t)^2*t * cp1_x + 3*(1-t)*t^2 * cp2_x + t^3 * x1
  bottom_y <- (1-t)^3 * y0_bottom + 3*(1-t)^2*t * y0_bottom + 3*(1-t)*t^2 * y1_bottom + t^3 * y1_bottom

  # Combine: top edge left-to-right, then bottom edge right-to-left
  data.frame(
    x = c(top_x, rev(bottom_x)),
    y = c(top_y, rev(bottom_y))
  )
}

#' Build node rectangle data
#' @noRd
.build_node_rects <- function(from_nodes, to_nodes, from_colors, to_colors,
                               x_left, x_right, node_width,
                               from_totals = NULL, to_totals = NULL) {
  # From nodes (left side)
  from_rects <- data.frame(
    xmin = x_left,
    xmax = x_left + node_width,
    ymin = from_nodes$bottom,
    ymax = from_nodes$top,
    label = from_nodes$state,
    color = from_colors[from_nodes$state],
    total = if (!is.null(from_totals)) as.numeric(from_totals[from_nodes$state]) else NA,
    side = "from",
    stringsAsFactors = FALSE
  )

  # To nodes (right side)
  to_rects <- data.frame(
    xmin = x_right - node_width,
    xmax = x_right,
    ymin = to_nodes$bottom,
    ymax = to_nodes$top,
    label = to_nodes$state,
    color = to_colors[to_nodes$state],
    total = if (!is.null(to_totals)) as.numeric(to_totals[to_nodes$state]) else NA,
    side = "to",
    stringsAsFactors = FALSE
  )

  rbind(from_rects, to_rects)
}

#' Default color palette for transitions
#' @noRd
.default_transition_palette <- function(n) {
 # Elegant, distinct colors
  palette <- c(
    "#FFD166",  # Yellow/gold
    "#06D6A0",  # Teal/mint
    "#9D4EDD",  # Purple
    "#EF476F",  # Coral/red
    "#118AB2",  # Blue
    "#073B4C",
    "#F78C6B",  # Orange
    "#83C5BE"   # Sage
  )

  if (n <= length(palette)) {
    return(palette[seq_len(n)])
  }

  # Generate more colors if needed
  colorRampPalette(palette)(n)
}

#' Add halo text layers to a ggplot
#' @noRd
.add_halo_text <- function(p, data, aes_mapping, size, color = "black",
                            hjust = 0.5, vjust = 0.5, fontface = "plain",
                            halo_color = "white", halo_expand = 0.003) {
  # Draw white halo by offsetting text in 8 directions
  offsets <- list(
    c(-1, -1), c(0, -1), c(1, -1),
    c(-1, 0),            c(1, 0),
    c(-1, 1),  c(0, 1),  c(1, 1)
  )

  for (off in offsets) {
    p <- p + geom_text(
      data = data,
      mapping = aes_mapping,
      size = size,
      color = halo_color,
      hjust = hjust,
      vjust = vjust,
      fontface = fontface,
      nudge_x = off[1] * halo_expand,
      nudge_y = off[2] * halo_expand
    )
  }

  # Draw main text on top
  p <- p + geom_text(
    data = data,
    mapping = aes_mapping,
    size = size,
    color = color,
    hjust = hjust,
    vjust = vjust,
    fontface = fontface
  )

  p
}

#' Multi-step transitions helper
#' @noRd
.plot_transitions_multi <- function(matrices, titles, colors,
                                     flow_fill, flow_alpha, flow_color_by = NULL,
                                     flow_border, flow_border_width,
                                     node_width, node_border, node_spacing,
                                     label_size, label_position, label_halo = TRUE,
                                     title_size,
                                     curve_strength, show_values, value_position,
                                     value_size, value_color, show_totals,
                                     total_size, total_color, min_flow, column_gap = 1) {

  n_steps <- length(matrices)
  n_columns <- n_steps + 1

  # Get all unique states across all matrices
  all_states <- unique(c(
    unlist(lapply(matrices, rownames)),
    unlist(lapply(matrices, colnames))
  ))
  n_states <- length(all_states)

  # Default colors if not provided
  if (is.null(colors)) {
    colors <- .default_transition_palette(n_states)
    names(colors) <- all_states
  }
  if (is.null(names(colors))) {
    names(colors) <- all_states
  }

  # Default titles
  if (length(titles) < n_columns) {
    titles <- paste0("T", seq_len(n_columns))
  }

  # Calculate x positions for each column (centered, respecting column_gap)
  x_start <- (1 - column_gap) / 2
  x_end <- 1 - x_start
  x_positions <- seq(x_start, x_end, length.out = n_columns)

  # Calculate node positions for each column
  # First, get totals for each state at each time point
  column_totals <- list()

  # First column: row sums of first matrix
  column_totals[[1]] <- rowSums(matrices[[1]])

  # Middle columns: col sums of prev = row sums of next (should match)
  for (i in seq_len(n_steps - 1)) {
    column_totals[[i + 1]] <- colSums(matrices[[i]])
  }

  # Last column: col sums of last matrix
  column_totals[[n_columns]] <- colSums(matrices[[n_steps]])

  # Calculate heights for each column
  max_total <- max(sapply(column_totals, sum))

  column_nodes <- list()
  for (col in seq_len(n_columns)) {
    totals <- column_totals[[col]]
    states <- names(totals)
    heights <- as.numeric(totals) / max_total

    # Scale to available space
    available_height <- 1 - (length(states) - 1) * node_spacing
    heights <- heights * available_height

    column_nodes[[col]] <- .calculate_node_positions(states, heights, node_spacing)
  }

  # Build all flow polygons
  all_polys <- list()
  all_centers <- list()
  poly_offset <- 0

  for (step in seq_len(n_steps)) {
    mat <- matrices[[step]]
    trans_df <- .matrix_to_trans_df(mat)

    if (min_flow > 0) {
      trans_df <- trans_df[trans_df$count >= min_flow, ]
    }

    from_nodes <- column_nodes[[step]]
    to_nodes <- column_nodes[[step + 1]]

    # For centered nodes, adjust x positions so flows connect to node edges
    # Nodes span from (x_pos - node_width/2) to (x_pos + node_width/2)
    x_left <- x_positions[step] - node_width / 2
    x_right <- x_positions[step + 1] + node_width / 2

    flow_data <- .build_flow_polygons(
      trans_df, from_nodes, to_nodes,
      x_left, x_right, node_width, curve_strength, value_position
    )

    if (!is.null(flow_data$polys)) {
      flow_data$polys$id <- flow_data$polys$id + poly_offset
      flow_data$centers$id <- flow_data$centers$id + poly_offset
      all_polys[[step]] <- flow_data$polys
      all_centers[[step]] <- flow_data$centers
      poly_offset <- max(flow_data$polys$id)
    }
  }

  flow_polys <- do.call(rbind, all_polys)
  flow_centers <- do.call(rbind, all_centers)

  # Build all node rectangles
  all_rects <- list()
  for (col in seq_len(n_columns)) {
    nodes <- column_nodes[[col]]
    x_pos <- x_positions[col]
    totals <- column_totals[[col]]

    rects <- data.frame(
      xmin = x_pos - node_width / 2,
      xmax = x_pos + node_width / 2,
      ymin = nodes$bottom,
      ymax = nodes$top,
      label = nodes$state,
      color = colors[nodes$state],
      total = as.numeric(totals[nodes$state]),
      side = ifelse(col == 1, "from", ifelse(col == n_columns, "to", "middle")),
      col = col,
      x_pos = x_pos,
      stringsAsFactors = FALSE
    )
    all_rects[[col]] <- rects
  }

  node_rects <- do.call(rbind, all_rects)

  # Assign flow colors based on flow_color_by
  if (!is.null(flow_color_by) && flow_color_by %in% c("source", "destination")) {
    if (flow_color_by == "source") {
      flow_polys$flow_color <- colors[flow_polys$from_state]
    } else {
      flow_polys$flow_color <- colors[flow_polys$to_state]
    }
  } else {
    flow_polys$flow_color <- flow_fill
  }

  # Create plot
  p <- ggplot() +
    # Flows
    geom_polygon(
      data = flow_polys,
      aes(x = x, y = y, group = id, fill = flow_color),
      alpha = flow_alpha,
      color = if (is.na(flow_border)) NA else flow_border,
      linewidth = flow_border_width
    ) +
    # Nodes
    geom_rect(
      data = node_rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color),
      color = if (is.na(node_border)) NA else node_border
    ) +
    scale_fill_identity()

  # Halo offsets for text readability
  halo_off <- 0.004

  # Add labels based on position
  if (label_position == "beside") {
    left_data <- node_rects[node_rects$col == 1, ]
    right_data <- node_rects[node_rects$col == n_columns, ]

    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p +
              geom_text(data = left_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                        hjust = 0, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off) +
              geom_text(data = right_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                        hjust = 1, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off)
          }
        }
      }
    }
    p <- p +
      geom_text(data = left_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                hjust = 0, size = label_size) +
      geom_text(data = right_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                hjust = 1, size = label_size)

  } else if (label_position == "outside") {
    left_data <- node_rects[node_rects$col == 1, ]
    right_data <- node_rects[node_rects$col == n_columns, ]

    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p +
              geom_text(data = left_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                        hjust = 1, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off) +
              geom_text(data = right_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                        hjust = 0, size = label_size, color = "white",
                        nudge_x = dx * halo_off, nudge_y = dy * halo_off)
          }
        }
      }
    }
    p <- p +
      geom_text(data = left_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                hjust = 1, size = label_size) +
      geom_text(data = right_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                hjust = 0, size = label_size)

  } else if (label_position == "above") {
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p + geom_text(
              data = node_rects,
              aes(x = x_pos, y = ymax + 0.02, label = label),
              hjust = 0.5, vjust = 0, size = label_size, color = "white",
              nudge_x = dx * halo_off, nudge_y = dy * halo_off
            )
          }
        }
      }
    }
    p <- p + geom_text(
      data = node_rects,
      aes(x = x_pos, y = ymax + 0.02, label = label),
      hjust = 0.5, vjust = 0, size = label_size
    )
  } else if (label_position == "below") {
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p + geom_text(
              data = node_rects,
              aes(x = x_pos, y = ymin - 0.02, label = label),
              hjust = 0.5, vjust = 1, size = label_size, color = "white",
              nudge_x = dx * halo_off, nudge_y = dy * halo_off
            )
          }
        }
      }
    }
    p <- p + geom_text(
      data = node_rects,
      aes(x = x_pos, y = ymin - 0.02, label = label),
      hjust = 0.5, vjust = 1, size = label_size
    )
  } else if (label_position == "inside") {
    # Inside labels don't need halo (white on colored background)
    p <- p + geom_text(
      data = node_rects,
      aes(x = x_pos, y = (ymin + ymax) / 2, label = label),
      hjust = 0.5, size = label_size * 0.8, color = "white", fontface = "bold"
    )
  }

  # Add totals
  if (show_totals) {
    p <- p + geom_text(
      data = node_rects,
      aes(x = x_pos, y = (ymin + ymax) / 2, label = total),
      size = total_size, color = total_color, fontface = "bold"
    )
  }

  # Add values on flows
  if (show_values && nrow(flow_centers) > 0) {
    p <- p + geom_text(
      data = flow_centers,
      aes(x = x, y = y, label = value),
      size = value_size, color = value_color
    )
  }

  # Add column titles with halo
  title_y <- max(node_rects$ymax) + 0.04
  for (col in seq_len(n_columns)) {
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p + annotate("text",
              x = x_positions[col] + dx * halo_off, y = title_y + dy * halo_off,
              label = titles[col], size = title_size, fontface = "bold", color = "white"
            )
          }
        }
      }
    }
    p <- p + annotate("text",
      x = x_positions[col], y = title_y,
      label = titles[col], size = title_size, fontface = "bold"
    )
  }

  # Theme
  p <- p +
    coord_cartesian(
      xlim = c(-0.15, 1.15),
      ylim = c(min(node_rects$ymin) - 0.05, max(node_rects$ymax) + 0.1),
      clip = "off"
    ) +
    theme_void() +
    theme(plot.margin = margin(20, 20, 20, 20))

  p
}

#' Plot individual tracking lines
#' @noRd
.plot_individual_tracks <- function(df, titles, colors,
                                     flow_color_by = NULL,
                                     node_width, node_border, node_spacing,
                                     label_size, label_position, label_halo = TRUE,
                                     title_size,
                                     curve_strength, line_alpha, line_width,
                                     jitter_amount, show_totals, total_size,
                                     total_color, column_gap = 1,
                                     proportional_nodes = TRUE,
                                     node_label_format = NULL,
                                     bundle_size = NULL,
                                     bundle_legend = TRUE) {

  n_columns <- ncol(df)
  n_individuals <- nrow(df)

  # Get all unique states
  all_states <- unique(unlist(lapply(df, as.character)))
  n_states <- length(all_states)

  # Default colors
  if (is.null(colors)) {
    colors <- .default_transition_palette(n_states)
    names(colors) <- all_states
  }
  if (is.null(names(colors))) {
    names(colors) <- all_states
  }

  # Default titles
  if (length(titles) < n_columns) {
    titles <- names(df)
  }

  # Calculate x positions
  x_start <- (1 - column_gap) / 2
  x_end <- 1 - x_start
  x_positions <- seq(x_start, x_end, length.out = n_columns)

  # Calculate node sizes based on counts at each time point
  column_totals <- list()
  for (col in seq_len(n_columns)) {
    column_totals[[col]] <- table(df[[col]])
  }

  # Calculate node heights
  max_total <- max(sapply(column_totals, sum))
  column_nodes <- list()

  for (col in seq_len(n_columns)) {
    totals <- column_totals[[col]]
    states <- names(totals)
    n_states_col <- length(states)

    if (proportional_nodes) {
      # Heights proportional to counts
      heights <- as.numeric(totals) / max_total
    } else {
      # Equal heights for all states
      heights <- rep(1 / n_states_col, n_states_col)
    }

    available_height <- 1 - (n_states_col - 1) * node_spacing
    heights <- heights * available_height

    column_nodes[[col]] <- .calculate_node_positions(states, heights, node_spacing)
  }

  # Build individual line data with ORDERED positions (no crossing within nodes)
  # For each segment, compute y-positions so lines don't cross unnecessarily

  line_data <- list()

  # Pre-compute all individual trajectories
  trajectories <- lapply(seq_len(n_individuals), function(i) {
    as.character(unlist(df[i, ]))
  })

  # Line bundling: aggregate trajectories when bundle_size is set
  bundled_weights <- rep(1L, length(trajectories))
  cases_per_line <- 1L
  if (!is.null(bundle_size)) {
    # Compute path strings for each individual
    path_strings <- vapply(trajectories, paste, character(1), collapse = "->")
    path_counts <- table(path_strings)

    # Compute cases_per_line from bundle_size
    if (bundle_size > 0 && bundle_size < 1) {
      # Fraction mode: keep this fraction of original lines
      # e.g., 0.15 with 4500 individuals → target ~675 lines
      target_lines <- max(1L, round(n_individuals * bundle_size))
      n_unique <- length(path_counts)
      cases_per_line <- max(1L, round(n_individuals / target_lines))
    } else {
      # Integer mode: each line represents bundle_size cases
      cases_per_line <- max(1L, as.integer(bundle_size))
    }

    # Build reduced trajectory list
    # Paths with count < cases_per_line/2 are dropped (rounded to 0 lines)
    new_trajectories <- list()
    new_weights <- integer(0)
    for (path_name in names(path_counts)) {
      path_count <- as.integer(path_counts[path_name])
      lines_to_draw <- round(path_count / cases_per_line)
      if (lines_to_draw < 1L) next
      # Find first occurrence of this path to get the trajectory
      first_idx <- which(path_strings == path_name)[1]
      traj <- trajectories[[first_idx]]
      weight_per_line <- path_count / lines_to_draw
      for (k in seq_len(lines_to_draw)) {
        new_trajectories[[length(new_trajectories) + 1]] <- traj
        new_weights <- c(new_weights, weight_per_line)
      }
    }
    # Fall back to top paths if everything got dropped
    if (length(new_trajectories) == 0) {
      # Keep the single most common path
      top_path <- names(sort(path_counts, decreasing = TRUE))[1]
      first_idx <- which(path_strings == top_path)[1]
      new_trajectories <- list(trajectories[[first_idx]])
      new_weights <- as.integer(path_counts[top_path])
    }
    trajectories <- new_trajectories
    bundled_weights <- new_weights
    n_individuals <- length(trajectories)
  }

  # For each segment, compute proper alluvial ordering
  for (seg in seq_len(n_columns - 1)) {
    from_nodes <- column_nodes[[seg]]
    to_nodes <- column_nodes[[seg + 1]]

    # Get from/to states for all individuals in this segment
    seg_data <- data.frame(
      individual = seq_len(n_individuals),
      from_state = sapply(trajectories, `[`, seg),
      to_state = sapply(trajectories, `[`, seg + 1),
      first_state = sapply(trajectories, `[`, 1),
      last_state = sapply(trajectories, `[`, n_columns),
      stringsAsFactors = FALSE
    )

    # Pre-calculate destination positions: for each destination, stack sources
    dest_positions <- list()
    for (ts in unique(seg_data$to_state)) {
      to_idx <- which(to_nodes$state == ts)
      to_top <- to_nodes$top[to_idx]
      to_bottom <- to_nodes$bottom[to_idx]
      to_height <- to_top - to_bottom

      # Get all sources going to this destination, ordered
      sources_to_ts <- seg_data[seg_data$to_state == ts, ]
      source_order <- unique(from_nodes$state)  # Use node order for consistency

      current_top <- to_top
      for (fs in source_order) {
        n_from_fs <- sum(sources_to_ts$from_state == fs)
        if (n_from_fs > 0) {
          total_to_ts <- nrow(sources_to_ts)
          section_height <- to_height * (n_from_fs / total_to_ts)
          dest_positions[[paste0(fs, "->", ts)]] <- list(
            top = current_top,
            bottom = current_top - section_height,
            count = n_from_fs
          )
          current_top <- current_top - section_height
        }
      }
    }

    # For each from_state, order by to_state and assign positions
    for (fs in unique(seg_data$from_state)) {
      from_idx <- which(from_nodes$state == fs)
      from_top <- from_nodes$top[from_idx]
      from_bottom <- from_nodes$bottom[from_idx]

      # Get individuals starting from this state
      mask <- seg_data$from_state == fs
      sub_data <- seg_data[mask, ]

      # Order by to_state (so lines going to same destination are grouped)
      sub_data <- sub_data[order(sub_data$to_state), ]
      n_sub <- nrow(sub_data)

      # Assign evenly spaced y positions within the source node
      if (n_sub > 1) {
        from_y_positions <- from_top - (seq_len(n_sub) - 0.5) / n_sub * (from_top - from_bottom)
      } else {
        from_y_positions <- (from_top + from_bottom) / 2
      }

      # Track position within each destination section
      dest_counters <- list()

      for (j in seq_len(n_sub)) {
        ind <- sub_data$individual[j]
        ts <- sub_data$to_state[j]
        from_y <- from_y_positions[j]

        # Get destination section for this source->dest pair
        key <- paste0(fs, "->", ts)
        dest_sec <- dest_positions[[key]]

        # Track position within this destination section
        if (is.null(dest_counters[[key]])) dest_counters[[key]] <- 0
        dest_counters[[key]] <- dest_counters[[key]] + 1
        pos_in_section <- dest_counters[[key]]

        # Calculate y position within section (evenly spaced)
        section_height <- dest_sec$top - dest_sec$bottom
        if (dest_sec$count > 1) {
          to_y <- dest_sec$top - (pos_in_section - 0.5) / dest_sec$count * section_height
        } else {
          to_y <- (dest_sec$top + dest_sec$bottom) / 2
        }

        # X positions
        x_from <- x_positions[seg] + node_width / 2
        x_to <- x_positions[seg + 1] - node_width / 2

        # Create bezier curve
        t <- seq(0, 1, length.out = 20)
        cp1_x <- x_from + (x_to - x_from) * curve_strength
        cp2_x <- x_to - (x_to - x_from) * curve_strength

        bezier_x <- (1-t)^3 * x_from + 3*(1-t)^2*t * cp1_x + 3*(1-t)*t^2 * cp2_x + t^3 * x_to
        bezier_y <- (1-t)^3 * from_y + 3*(1-t)^2*t * from_y + 3*(1-t)*t^2 * to_y + t^3 * to_y

        line_data[[length(line_data) + 1]] <- data.frame(
          x = bezier_x,
          y = bezier_y,
          individual = ind,
          segment = seg,
          group = paste0(ind, "_", seg),
          from_state = fs,
          to_state = ts,
          first_state = sub_data$first_state[j],
          last_state = sub_data$last_state[j],
          bundled_weight = bundled_weights[ind],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  lines_df <- do.call(rbind, line_data)

  # Assign colors based on flow_color_by
  if (!is.null(flow_color_by)) {
    if (flow_color_by == "source") {
      lines_df$line_color <- colors[lines_df$from_state]
    } else if (flow_color_by == "destination") {
      lines_df$line_color <- colors[lines_df$to_state]
    } else if (flow_color_by == "first") {
      lines_df$line_color <- colors[lines_df$first_state]
    } else if (flow_color_by == "last") {
      lines_df$line_color <- colors[lines_df$last_state]
    } else {
      lines_df$line_color <- "#888888"
    }
  } else {
    lines_df$line_color <- "#888888"
  }

  # Build node rectangles
  all_rects <- list()
  for (col in seq_len(n_columns)) {
    nodes <- column_nodes[[col]]
    x_pos <- x_positions[col]
    totals <- column_totals[[col]]

    rects <- data.frame(
      xmin = x_pos - node_width / 2,
      xmax = x_pos + node_width / 2,
      ymin = nodes$bottom,
      ymax = nodes$top,
      label = nodes$state,
      color = colors[nodes$state],
      total = as.numeric(totals[nodes$state]),
      col = col,
      x_pos = x_pos,
      stringsAsFactors = FALSE
    )
    all_rects[[col]] <- rects
  }

  node_rects <- do.call(rbind, all_rects)

  # Apply node_label_format if specified
  if (!is.null(node_label_format)) {
    node_rects$label <- mapply(function(state, count) {
      out <- gsub("{state}", state, node_label_format, fixed = TRUE)
      gsub("{count}", count, out, fixed = TRUE)
    }, node_rects$label, node_rects$total, USE.NAMES = FALSE)
  }

  # Create plot
  if (!is.null(bundle_size)) {
    # Scale line widths by bundled_weight
    lw_min <- line_width
    lw_max <- line_width * 2
    w_range <- range(lines_df$bundled_weight)
    if (w_range[1] == w_range[2]) {
      lines_df$lw <- line_width
    } else {
      lines_df$lw <- lw_min + (lines_df$bundled_weight - w_range[1]) /
        (w_range[2] - w_range[1]) * (lw_max - lw_min)
    }
    # Auto-boost alpha for bundled plots: fewer lines need higher opacity
    # If user kept default (0.3), boost to 0.9; otherwise respect their choice
    bundled_alpha <- if (line_alpha <= 0.3) 0.9 else min(1, line_alpha + 0.3)
    p <- ggplot() +
      geom_path(
        data = lines_df,
        aes(x = x, y = y, group = group, color = line_color, linewidth = lw),
        alpha = bundled_alpha
      ) +
      scale_linewidth_identity() +
      scale_color_identity()
  } else {
    p <- ggplot() +
      # Individual lines (draw first, behind nodes)
      geom_path(
        data = lines_df,
        aes(x = x, y = y, group = group, color = line_color),
        alpha = line_alpha,
        linewidth = line_width
      ) +
      scale_color_identity()
  }

  p <- p +
    # Nodes
    geom_rect(
      data = node_rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color),
      color = if (is.na(node_border)) NA else node_border
    ) +
    scale_fill_identity()

  # Halo offsets for text readability
  halo_off <- 0.004

  # Add labels for ALL columns with full position support
  if (label_position == "beside" || label_position == "outside") {
    # First/last columns: beside/outside positioning as before
    # Intermediate columns: fall back to "above" positioning
    left_data <- node_rects[node_rects$col == 1, ]
    right_data <- node_rects[node_rects$col == n_columns, ]
    mid_data <- node_rects[node_rects$col > 1 & node_rects$col < n_columns, ]

    if (label_position == "beside") {
      if (label_halo) {
        for (dx in c(-1, 0, 1)) {
          for (dy in c(-1, 0, 1)) {
            if (dx != 0 || dy != 0) {
              p <- p +
                geom_text(data = left_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                          hjust = 0, size = label_size, color = "white",
                          nudge_x = dx * halo_off, nudge_y = dy * halo_off) +
                geom_text(data = right_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                          hjust = 1, size = label_size, color = "white",
                          nudge_x = dx * halo_off, nudge_y = dy * halo_off)
            }
          }
        }
      }
      p <- p +
        geom_text(data = left_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                  hjust = 0, size = label_size) +
        geom_text(data = right_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                  hjust = 1, size = label_size)
    } else {
      # "outside": left column labels to the left, right column to the right
      if (label_halo) {
        for (dx in c(-1, 0, 1)) {
          for (dy in c(-1, 0, 1)) {
            if (dx != 0 || dy != 0) {
              p <- p +
                geom_text(data = left_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                          hjust = 1, size = label_size, color = "white",
                          nudge_x = dx * halo_off, nudge_y = dy * halo_off) +
                geom_text(data = right_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                          hjust = 0, size = label_size, color = "white",
                          nudge_x = dx * halo_off, nudge_y = dy * halo_off)
            }
          }
        }
      }
      p <- p +
        geom_text(data = left_data, aes(x = xmin - 0.02, y = (ymin + ymax) / 2, label = label),
                  hjust = 1, size = label_size) +
        geom_text(data = right_data, aes(x = xmax + 0.02, y = (ymin + ymax) / 2, label = label),
                  hjust = 0, size = label_size)
    }

    # Intermediate columns: use "above" positioning
    if (nrow(mid_data) > 0) {
      if (label_halo) {
        for (dx in c(-1, 0, 1)) {
          for (dy in c(-1, 0, 1)) {
            if (dx != 0 || dy != 0) {
              p <- p + geom_text(
                data = mid_data,
                aes(x = x_pos, y = ymax + 0.02, label = label),
                hjust = 0.5, vjust = 0, size = label_size, color = "white",
                nudge_x = dx * halo_off, nudge_y = dy * halo_off
              )
            }
          }
        }
      }
      p <- p + geom_text(data = mid_data,
        aes(x = x_pos, y = ymax + 0.02, label = label),
        hjust = 0.5, vjust = 0, size = label_size)
    }

  } else if (label_position == "above") {
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p + geom_text(
              data = node_rects,
              aes(x = x_pos, y = ymax + 0.02, label = label),
              hjust = 0.5, vjust = 0, size = label_size, color = "white",
              nudge_x = dx * halo_off, nudge_y = dy * halo_off
            )
          }
        }
      }
    }
    p <- p + geom_text(
      data = node_rects,
      aes(x = x_pos, y = ymax + 0.02, label = label),
      hjust = 0.5, vjust = 0, size = label_size
    )

  } else if (label_position == "below") {
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p + geom_text(
              data = node_rects,
              aes(x = x_pos, y = ymin - 0.02, label = label),
              hjust = 0.5, vjust = 1, size = label_size, color = "white",
              nudge_x = dx * halo_off, nudge_y = dy * halo_off
            )
          }
        }
      }
    }
    p <- p + geom_text(
      data = node_rects,
      aes(x = x_pos, y = ymin - 0.02, label = label),
      hjust = 0.5, vjust = 1, size = label_size
    )

  } else if (label_position == "inside") {
    p <- p + geom_text(
      data = node_rects,
      aes(x = x_pos, y = (ymin + ymax) / 2, label = label),
      hjust = 0.5, size = label_size * 0.8, color = "white", fontface = "bold"
    )
  }

  # Add totals
  if (show_totals) {
    p <- p + geom_text(
      data = node_rects,
      aes(x = x_pos, y = (ymin + ymax) / 2, label = total),
      size = total_size, color = total_color, fontface = "bold"
    )
  }

  # Add titles with halo
  title_y <- max(node_rects$ymax) + 0.04
  for (col in seq_len(n_columns)) {
    if (label_halo) {
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          if (dx != 0 || dy != 0) {
            p <- p + annotate("text",
              x = x_positions[col] + dx * halo_off, y = title_y + dy * halo_off,
              label = titles[col], size = title_size, fontface = "bold", color = "white"
            )
          }
        }
      }
    }
    p <- p + annotate("text",
      x = x_positions[col], y = title_y,
      label = titles[col], size = title_size, fontface = "bold"
    )
  }

  # Bundle legend annotation
  if (!is.null(bundle_size) && bundle_legend) {
    legend_y <- min(node_rects$ymin) - 0.04
    legend_text <- sprintf("Each line \u2248 %s cases", round(cases_per_line))
    p <- p + annotate("text",
      x = 0.5, y = legend_y,
      label = legend_text, size = 3, color = "grey50", fontface = "italic"
    )
  }

  # Theme
  y_bottom <- min(node_rects$ymin) - 0.05
  if (!is.null(bundle_size) && bundle_legend) {
    y_bottom <- min(node_rects$ymin) - 0.08
  }
  p <- p +
    coord_cartesian(
      xlim = c(-0.15, 1.15),
      ylim = c(y_bottom, max(node_rects$ymax) + 0.1),
      clip = "off"
    ) +
    theme_void() +
    theme(plot.margin = margin(20, 20, 20, 20))

  p
}


# =============================================================================
# Convenience Aliases
# =============================================================================

#' Plot Alluvial Diagram
#'
#' Creates an alluvial (Sankey) diagram showing aggregated flows between states.
#' This is an alias for \code{plot_transitions()} with aggregated flows (default).
#'
#' @inheritParams plot_transitions
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # From a transition matrix
#' mat <- matrix(c(50, 10, 5, 15, 40, 10), 2, 3)
#' rownames(mat) <- c("A", "B")
#' colnames(mat) <- c("X", "Y", "Z")
#' plot_alluvial(mat)
#'
#' # From a data frame with multiple time points
#' df <- data.frame(T1 = c("A","A","B"), T2 = c("X","Y","X"), T3 = c("P","P","Q"))
#' plot_alluvial(df, flow_color_by = "source")
#' }
#'
#' @seealso \code{\link{plot_transitions}}, \code{\link{plot_trajectories}}
#' @export
plot_alluvial <- function(x,
                          from_title = "From",
                          to_title = "To",
                          from_colors = NULL,
                          to_colors = NULL,
                          flow_fill = "#888888",
                          flow_alpha = 0.4,
                          flow_color_by = NULL,
                          flow_border = NA,
                          flow_border_width = 0.5,
                          node_width = 0.08,
                          node_border = NA,
                          node_spacing = 0.02,
                          label_size = 3.5,
                          label_position = c("beside", "inside", "above", "below", "outside"),
                          label_halo = TRUE,
                          title_size = 5,
                          curve_strength = 0.6,
                          show_values = FALSE,
                          value_position = c("center", "origin", "destination", "outside_origin", "outside_destination"),
                          value_size = 3,
                          value_color = "black",
                          show_totals = FALSE,
                          total_size = 4,
                          total_color = "white",
                          conserve_flow = TRUE,
                          min_flow = 0,
                          column_gap = 1) {

  plot_transitions(
    x = x,
    from_title = from_title,
    to_title = to_title,
    from_colors = from_colors,
    to_colors = to_colors,
    flow_fill = flow_fill,
    flow_alpha = flow_alpha,
    flow_color_by = flow_color_by,
    flow_border = flow_border,
    flow_border_width = flow_border_width,
    node_width = node_width,
    node_border = node_border,
    node_spacing = node_spacing,
    label_size = label_size,
    label_position = label_position,
    label_halo = label_halo,
    title_size = title_size,
    curve_strength = curve_strength,
    show_values = show_values,
    value_position = value_position,
    value_size = value_size,
    value_color = value_color,
    show_totals = show_totals,
    total_size = total_size,
    total_color = total_color,
    conserve_flow = conserve_flow,
    min_flow = min_flow,
    column_gap = column_gap,
    track_individuals = FALSE
  )
}


#' Plot Individual Trajectories
#'
#' Creates an alluvial-style diagram where each individual's trajectory is shown
#' as a separate line. This is an alias for \code{plot_transitions()} with
#' \code{track_individuals = TRUE}.
#'
#' @inheritParams plot_transitions
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Track individual trajectories across time points
#' df <- data.frame(
#'   Baseline = c("Light", "Light", "Intense", "Resource"),
#'   Week4 = c("Light", "Intense", "Intense", "Light"),
#'   Week8 = c("Resource", "Intense", "Light", "Light")
#' )
#'
#' # Color by starting state
#' plot_trajectories(df, flow_color_by = "first")
#'
#' # Color by ending state
#' plot_trajectories(df, flow_color_by = "last")
#' }
#'
#' @seealso \code{\link{plot_transitions}}, \code{\link{plot_alluvial}}
#' @export
plot_trajectories <- function(x,
                              from_title = NULL,
                              from_colors = NULL,
                              flow_color_by = "first",
                              node_width = 0.08,
                              node_border = NA,
                              node_spacing = 0.02,
                              label_size = 3.5,
                              label_position = c("beside", "inside", "above", "below", "outside"),
                              label_halo = TRUE,
                              title_size = 5,
                              curve_strength = 0.6,
                              line_alpha = 0.3,
                              line_width = 0.5,
                              jitter_amount = 0.8,
                              show_totals = FALSE,
                              total_size = 4,
                              total_color = "white",
                              column_gap = 1,
                              proportional_nodes = TRUE,
                              node_label_format = NULL,
                              bundle_size = NULL,
                              bundle_legend = TRUE) {

  plot_transitions(
    x = x,
    from_title = from_title,
    from_colors = from_colors,
    flow_color_by = flow_color_by,
    node_width = node_width,
    node_border = node_border,
    node_spacing = node_spacing,
    label_size = label_size,
    label_position = label_position,
    label_halo = label_halo,
    title_size = title_size,
    curve_strength = curve_strength,
    line_alpha = line_alpha,
    line_width = line_width,
    jitter_amount = jitter_amount,
    show_totals = show_totals,
    total_size = total_size,
    total_color = total_color,
    column_gap = column_gap,
    track_individuals = TRUE,
    proportional_nodes = proportional_nodes,
    node_label_format = node_label_format,
    bundle_size = bundle_size,
    bundle_legend = bundle_legend
  )
}
