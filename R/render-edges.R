#' @title Edge Rendering
#' @description Functions for rendering edges using grid graphics.
#' @name render-edges
#' @keywords internal
NULL

#' Render All Edges
#'
#' Create grid grobs for all edges in the network.
#'
#' @param network A SonnetNetwork object.
#' @return A grid gList of edge grobs.
#' @keywords internal
render_edges_grid <- function(network) {
  nodes <- network$get_nodes()
  edges <- network$get_edges()
  aes <- network$get_edge_aes()
  node_aes <- network$get_node_aes()
  theme <- network$get_theme()

  if (is.null(edges) || nrow(edges) == 0) return(grid::gList())

  m <- nrow(edges)
  n <- nrow(nodes)

  # Resolve edge widths using the enhanced scaling system
  if (!is.null(aes$width)) {
    # Explicit widths provided
    widths <- recycle_to_length(aes$width, m)
  } else if ("weight" %in% names(edges)) {
    # Use weight-based scaling with new system
    widths <- scale_edge_widths(
      weights = edges$weight,
      esize = aes$esize,
      n_nodes = n,
      directed = network$is_directed,
      mode = if (!is.null(aes$edge_scale_mode)) aes$edge_scale_mode else "linear",
      maximum = aes$maximum,
      minimum = 0,
      cut = aes$cut,
      range = if (!is.null(aes$edge_width_range)) aes$edge_width_range else c(0.5, 4)
    )
  } else {
    # No weights, use default
    widths <- recycle_to_length(theme$get("edge_width"), m)
  }

  # Apply width_scale if provided (additional multiplier)
  if (!is.null(aes$width_scale)) {
    widths <- widths * aes$width_scale
  }

  # Color resolution
  if (!is.null(aes$color)) {
    colors <- recycle_to_length(aes$color, m)
  } else {
    # Default: color by weight sign
    pos_col <- if (!is.null(aes$positive_color)) aes$positive_color else theme$get("edge_positive_color")
    neg_col <- if (!is.null(aes$negative_color)) aes$negative_color else theme$get("edge_negative_color")
    default_col <- theme$get("edge_color")
    colors <- if (!is.null(edges$weight)) {
      ifelse(edges$weight > 0, pos_col, ifelse(edges$weight < 0, neg_col, default_col))
    } else {
      rep(default_col, m)
    }
  }

  alphas <- recycle_to_length(
    if (!is.null(aes$alpha)) aes$alpha else 0.8,
    m
  )

  # Apply cut threshold for transparency: edges below cut are faded
  if (!is.null(aes$cut) && aes$cut > 0 && "weight" %in% names(edges)) {
    cut_val <- aes$cut
    abs_weights <- abs(edges$weight)
    # Edges below cut get reduced alpha (pale/faded)
    below_cut <- abs_weights < cut_val
    if (any(below_cut)) {
      # Scale alpha: edges at 0 get 20% of normal alpha, edges near cut get full alpha
      fade_factor <- ifelse(below_cut, 0.2 + 0.8 * (abs_weights / cut_val), 1)
      alphas <- alphas * fade_factor
    }
  }

  styles <- recycle_to_length(
    if (!is.null(aes$style)) aes$style else "solid",
    m
  )
  curvatures <- recycle_to_length(
    if (!is.null(aes$curvature)) aes$curvature else 0,
    m
  )

  # Get curves mode:
  # TRUE (default) = single edges straight, reciprocal edges curve as ellipse (opposite directions)
  # FALSE = all straight
  # "force" = all curved (reciprocals opposite, singles curved)
  curves_mode <- if (!is.null(aes$curves)) aes$curves else TRUE

  # Handle curve modes
  if (!identical(curves_mode, FALSE)) {
    # Identify reciprocal pairs
    is_reciprocal <- rep(FALSE, m)
    for (i in seq_len(m)) {
      from_i <- edges$from[i]
      to_i <- edges$to[i]
      if (from_i == to_i) next
      for (j in seq_len(m)) {
        if (j != i && edges$from[j] == to_i && edges$to[j] == from_i) {
          is_reciprocal[i] <- TRUE
          break
        }
      }
    }

    # Curve reciprocal edges with direction based on network center (qgraph-style)
    # Increased from 0.18 to 0.5 for better visibility
    default_curve <- 0.25

    # Calculate network center for curve direction
    center_x <- mean(nodes$x)
    center_y <- mean(nodes$y)

    for (i in seq_len(m)) {
      if (is_reciprocal[i] && curvatures[i] == 0) {
        from_i <- edges$from[i]
        to_i <- edges$to[i]

        # Calculate edge midpoint
        mid_x <- (nodes$x[from_i] + nodes$x[to_i]) / 2
        mid_y <- (nodes$y[from_i] + nodes$y[to_i]) / 2

        # Calculate perpendicular direction (for curve)
        dx <- nodes$x[to_i] - nodes$x[from_i]
        dy <- nodes$y[to_i] - nodes$y[from_i]

        # Perpendicular vector (rotated 90 degrees)
        perp_x <- -dy
        perp_y <- dx

        # Check if positive curve moves toward or away from center
        test_x <- mid_x + perp_x * 0.1
        test_y <- mid_y + perp_y * 0.1
        dist_to_center_pos <- sqrt((test_x - center_x)^2 + (test_y - center_y)^2)
        dist_to_center_orig <- sqrt((mid_x - center_x)^2 + (mid_y - center_y)^2)

        # Both edges curve OUTWARD (away from center), on opposite sides
        # The perpendicular naturally reverses for the reverse edge, so same logic works
        curvatures[i] <- if (dist_to_center_pos > dist_to_center_orig) default_curve else -default_curve
      }
    }

    # For "force" mode, also curve non-reciprocal edges
    if (identical(curves_mode, "force")) {
      for (i in seq_len(m)) {
        from_i <- edges$from[i]
        to_i <- edges$to[i]
        if (is_reciprocal[i] || from_i == to_i) next
        if (curvatures[i] == 0) {
          curvatures[i] <- default_curve
        }
      }
    }
  }

  # Arrow settings
  # Default arrow_size uses unified scale constant (0.02)
  # This is already in the correct format - soplot converts user input via scale$arrow_factor
  show_arrows <- if (!is.null(aes$show_arrows)) aes$show_arrows else network$is_directed
  arrow_size <- if (!is.null(aes$arrow_size)) aes$arrow_size else SONNET_SCALE$arrow_factor

  # Bidirectional arrow settings
  bidirectionals <- recycle_to_length(
    if (!is.null(aes$bidirectional)) aes$bidirectional else FALSE,
    m
  )

  # Loop rotation settings
  loop_rotations <- recycle_to_length(
    if (!is.null(aes$loop_rotation)) aes$loop_rotation else pi/2,
    m
  )

  # Curve shape and pivot settings
  curve_shapes <- recycle_to_length(
    if (!is.null(aes$curve_shape)) aes$curve_shape else 0,
    m
  )
  curve_pivots <- recycle_to_length(
    if (!is.null(aes$curve_pivot)) aes$curve_pivot else 0.5,
    m
  )

  # Node sizes for endpoint calculation
  node_sizes <- recycle_to_length(
    if (!is.null(node_aes$size)) node_aes$size else 0.05,
    nrow(nodes)
  )

  # Get aspect ratio correction for proper edge endpoints
  vp_width <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  vp_height <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  min_dim <- min(vp_width, vp_height)
  x_scale <- min_dim / vp_width
  y_scale <- min_dim / vp_height

  # CI underlay parameters
  edge_ci <- if (!is.null(aes$ci)) recycle_to_length(aes$ci, m) else NULL
  edge_ci_scale <- if (!is.null(aes$ci_scale)) aes$ci_scale else 2.0
  edge_ci_alpha <- if (!is.null(aes$ci_alpha)) aes$ci_alpha else 0.15
  edge_ci_color <- if (!is.null(aes$ci_color) && !is.na(aes$ci_color)) {
    recycle_to_length(aes$ci_color, m)
  } else NULL
  edge_ci_style <- if (!is.null(aes$ci_style)) aes$ci_style else 2
  edge_ci_arrows <- if (!is.null(aes$ci_arrows)) aes$ci_arrows else FALSE

  # Create edge grobs
  grobs <- list()

  for (i in seq_len(m)) {
    from_idx <- edges$from[i]
    to_idx <- edges$to[i]

    x1 <- nodes$x[from_idx]
    y1 <- nodes$y[from_idx]
    x2 <- nodes$x[to_idx]
    y2 <- nodes$y[to_idx]

    # Adjust color with alpha
    edge_col <- adjust_alpha(colors[i], alphas[i])

    # Line type
    lty <- switch(styles[i],
      solid = 1,
      dashed = 2,
      dotted = 3,
      longdash = 5,
      twodash = 6,
      1
    )

    # Adjust line width for dotted style (reduce by 30% to avoid overly thick appearance)
    cur_width <- widths[i]
    if (lty == 3) {  # dotted
      cur_width <- cur_width * 0.7
    }

    # Handle self-loops
    if (from_idx == to_idx) {
      # PASS 1: Draw CI underlay for self-loop (if edge_ci provided)
      if (!is.null(edge_ci) && !is.na(edge_ci[i]) && edge_ci[i] > 0) {
        underlay_width <- cur_width * (1 + edge_ci[i] * edge_ci_scale)
        underlay_col <- if (!is.null(edge_ci_color)) edge_ci_color[i] else colors[i]
        underlay_col <- adjust_alpha(underlay_col, edge_ci_alpha)
        underlay_lty <- edge_ci_style

        grobs[[length(grobs) + 1]] <- draw_self_loop(
          x1, y1, node_sizes[from_idx], underlay_col, underlay_width, underlay_lty,
          rotation = loop_rotations[i]
        )
      }

      # PASS 2: Draw main self-loop
      grobs[[length(grobs) + 1]] <- draw_self_loop(
        x1, y1, node_sizes[from_idx], edge_col, cur_width, lty,
        rotation = loop_rotations[i]
      )
      next
    }

    # Calculate endpoints (offset by node radius, with aspect correction)
    start_pt <- edge_endpoint(x1, y1, x2, y2, node_sizes[from_idx],
                              x_scale = x_scale, y_scale = y_scale)
    end_pt <- edge_endpoint(x2, y2, x1, y1, node_sizes[to_idx],
                            x_scale = x_scale, y_scale = y_scale)

    # PASS 1: Draw CI underlay (if edge_ci provided)
    if (!is.null(edge_ci) && !is.na(edge_ci[i]) && edge_ci[i] > 0) {
      underlay_width <- cur_width * (1 + edge_ci[i] * edge_ci_scale)
      underlay_col <- if (!is.null(edge_ci_color)) edge_ci_color[i] else colors[i]
      underlay_col <- adjust_alpha(underlay_col, edge_ci_alpha)
      underlay_lty <- edge_ci_style

      if (curvatures[i] != 0) {
        grobs[[length(grobs) + 1]] <- draw_curved_edge(
          start_pt$x, start_pt$y, end_pt$x, end_pt$y,
          curvatures[i], underlay_col, underlay_width, underlay_lty,
          edge_ci_arrows, arrow_size, FALSE,
          curve_shapes[i], curve_pivots[i],
          x_scale = x_scale, y_scale = y_scale
        )
      } else {
        grobs[[length(grobs) + 1]] <- draw_straight_edge(
          start_pt$x, start_pt$y, end_pt$x, end_pt$y,
          underlay_col, underlay_width, underlay_lty, edge_ci_arrows, arrow_size,
          FALSE,
          x_scale = x_scale, y_scale = y_scale
        )
      }
    }

    # PASS 2: Draw main edge
    if (curvatures[i] != 0) {
      # Curved edge
      grobs[[length(grobs) + 1]] <- draw_curved_edge(
        start_pt$x, start_pt$y, end_pt$x, end_pt$y,
        curvatures[i], edge_col, cur_width, lty,
        show_arrows, arrow_size, bidirectionals[i],
        curve_shapes[i], curve_pivots[i],
        x_scale = x_scale, y_scale = y_scale
      )
    } else {
      # Straight edge
      grobs[[length(grobs) + 1]] <- draw_straight_edge(
        start_pt$x, start_pt$y, end_pt$x, end_pt$y,
        edge_col, cur_width, lty, show_arrows, arrow_size,
        bidirectionals[i],
        x_scale = x_scale, y_scale = y_scale
      )
    }
  }

  do.call(grid::gList, grobs)
}

#' Draw Straight Edge
#' @keywords internal
draw_straight_edge <- function(x1, y1, x2, y2, color, width, lty,
                                show_arrow, arrow_size, bidirectional = FALSE,
                                x_scale = 1, y_scale = 1) {
  grobs <- list()

  # Calculate angle with aspect correction
  dx <- (x2 - x1) / x_scale
  dy <- (y2 - y1) / y_scale
  angle <- atan2(dy, dx)

  # Draw line from start to end (arrow overlays the end)
  grobs[[1]] <- grid::segmentsGrob(
    x0 = grid::unit(x1, "npc"),
    y0 = grid::unit(y1, "npc"),
    x1 = grid::unit(x2, "npc"),
    y1 = grid::unit(y2, "npc"),
    gp = grid::gpar(col = color, lwd = width, lty = lty)
  )

  # Draw arrow at target (tip at endpoint)
  if (show_arrow && arrow_size > 0) {
    arrow_pts <- arrow_points(x2, y2, angle, arrow_size,
                              x_scale = x_scale, y_scale = y_scale)
    grobs[[2]] <- grid::polygonGrob(
      x = grid::unit(arrow_pts$x, "npc"),
      y = grid::unit(arrow_pts$y, "npc"),
      gp = grid::gpar(fill = color, col = color)
    )
  }

  # Draw arrow at source if bidirectional (tip at start point)
  if (bidirectional && arrow_size > 0) {
    dx_back <- (x1 - x2) / x_scale
    dy_back <- (y1 - y2) / y_scale
    angle_back <- atan2(dy_back, dx_back)
    arrow_pts_back <- arrow_points(x1, y1, angle_back, arrow_size,
                                   x_scale = x_scale, y_scale = y_scale)
    grobs[[length(grobs) + 1]] <- grid::polygonGrob(
      x = grid::unit(arrow_pts_back$x, "npc"),
      y = grid::unit(arrow_pts_back$y, "npc"),
      gp = grid::gpar(fill = color, col = color)
    )
  }

  do.call(grid::gList, grobs)
}

#' Draw Curved Edge
#' @keywords internal
draw_curved_edge <- function(x1, y1, x2, y2, curvature, color, width, lty,
                              show_arrow, arrow_size, bidirectional = FALSE,
                              curve_shape = 0, curve_pivot = 0.5,
                              x_scale = 1, y_scale = 1) {
  grobs <- list()

  # Calculate control point with shape and pivot adjustments
  ctrl <- curve_control_point(x1, y1, x2, y2, curvature,
                               pivot = curve_pivot, shape = curve_shape)

  # Generate bezier points
  pts <- bezier_points(x1, y1, ctrl$x, ctrl$y, x2, y2, n = 50)
  n <- nrow(pts)

  # Draw curve (arrows overlay the ends)
  grobs[[1]] <- grid::linesGrob(
    x = grid::unit(pts$x, "npc"),
    y = grid::unit(pts$y, "npc"),
    gp = grid::gpar(col = color, lwd = width, lty = lty)
  )

  # Draw arrow at target (tip at curve end, angle follows curve direction)
  if (show_arrow && arrow_size > 0) {
    # Calculate angle with aspect correction
    dx <- (pts$x[n] - pts$x[n-1]) / x_scale
    dy <- (pts$y[n] - pts$y[n-1]) / y_scale
    angle <- atan2(dy, dx)
    arrow_pts <- arrow_points(x2, y2, angle, arrow_size,
                              x_scale = x_scale, y_scale = y_scale)
    grobs[[2]] <- grid::polygonGrob(
      x = grid::unit(arrow_pts$x, "npc"),
      y = grid::unit(arrow_pts$y, "npc"),
      gp = grid::gpar(fill = color, col = color)
    )
  }

  # Draw arrow at source if bidirectional
  if (bidirectional && arrow_size > 0) {
    dx_back <- (pts$x[1] - pts$x[2]) / x_scale
    dy_back <- (pts$y[1] - pts$y[2]) / y_scale
    angle_back <- atan2(dy_back, dx_back)
    arrow_pts_back <- arrow_points(x1, y1, angle_back, arrow_size,
                                   x_scale = x_scale, y_scale = y_scale)
    grobs[[length(grobs) + 1]] <- grid::polygonGrob(
      x = grid::unit(arrow_pts_back$x, "npc"),
      y = grid::unit(arrow_pts_back$y, "npc"),
      gp = grid::gpar(fill = color, col = color)
    )
  }

  do.call(grid::gList, grobs)
}

#' Draw Self-Loop
#' @param x,y Node center coordinates.
#' @param node_size Node radius.
#' @param color Loop color.
#' @param width Loop line width.
#' @param lty Loop line type.
#' @param rotation Angle in radians for loop direction (default: pi/2 = top).
#' @keywords internal
draw_self_loop <- function(x, y, node_size, color, width, lty, rotation = pi/2) {
  # Get aspect ratio correction
  vp_width <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  vp_height <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  min_dim <- min(vp_width, vp_height)
  x_scale <- min_dim / vp_width
  y_scale <- min_dim / vp_height

  # Loop parameters
  loop_angle <- pi/8  # Angle spread for loop attachment points
  # rotation is now a parameter (default: pi/2 = top of node)

  # Points where loop attaches to node edge
  left_angle <- rotation + loop_angle
  right_angle <- rotation - loop_angle

  left_x <- x + (node_size * x_scale) * cos(left_angle)
  left_y <- y + (node_size * y_scale) * sin(left_angle)

  right_x <- x + (node_size * x_scale) * cos(right_angle)
  right_y <- y + (node_size * y_scale) * sin(right_angle)

  # Loop center point (outside the node) - larger = further out
  loop_dist <- node_size * 2.2
  center_x <- x + (loop_dist * x_scale) * cos(rotation)
  center_y <- y + (loop_dist * y_scale) * sin(rotation)

  # Create smooth bezier-like curve through control points
  # Control points for the loop
  ctrl1_x <- left_x + (loop_dist * 0.8 * x_scale) * cos(rotation + pi/6)
  ctrl1_y <- left_y + (loop_dist * 0.8 * y_scale) * sin(rotation + pi/6)

  ctrl2_x <- right_x + (loop_dist * 0.8 * x_scale) * cos(rotation - pi/6)
  ctrl2_y <- right_y + (loop_dist * 0.8 * y_scale) * sin(rotation - pi/6)

  # Generate smooth curve using bezier-like interpolation
  t_vals <- seq(0, 1, length.out = 40)
  curve_x <- numeric(length(t_vals))
  curve_y <- numeric(length(t_vals))

  for (i in seq_along(t_vals)) {
    t <- t_vals[i]
    # Cubic bezier formula
    curve_x[i] <- (1-t)^3 * left_x + 3*(1-t)^2*t * ctrl1_x +
                  3*(1-t)*t^2 * ctrl2_x + t^3 * right_x
    curve_y[i] <- (1-t)^3 * left_y + 3*(1-t)^2*t * ctrl1_y +
                  3*(1-t)*t^2 * ctrl2_y + t^3 * right_y
  }

  grobs <- list()

  # Draw the loop curve
  grobs[[1]] <- grid::linesGrob(
    x = grid::unit(curve_x, "npc"),
    y = grid::unit(curve_y, "npc"),
    gp = grid::gpar(col = color, lwd = width, lty = lty)
  )

  # Add arrowhead at the end (pointing into node)
  n <- length(curve_x)
  grobs[[2]] <- grid::linesGrob(
    x = grid::unit(curve_x[(n-3):n], "npc"),
    y = grid::unit(curve_y[(n-3):n], "npc"),
    arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
    gp = grid::gpar(col = color, lwd = width, fill = color)
  )

  do.call(grid::gList, grobs)
}

#' Render Edge Labels
#'
#' Create grid grobs for edge labels with background, borders, and styling.
#'
#' @param network A SonnetNetwork object.
#' @return A grid gList of label grobs.
#' @keywords internal
render_edge_labels_grid <- function(network) {
  nodes <- network$get_nodes()
  edges <- network$get_edges()
  aes <- network$get_edge_aes()
  theme <- network$get_theme()

  if (is.null(edges) || nrow(edges) == 0) return(grid::gList())

  m <- nrow(edges)

  # Check for template-based labels first
  has_template <- !is.null(aes$label_template) ||
                  (!is.null(aes$label_style) && aes$label_style != "none")

  if (has_template) {
    # Use template-based labels
    edge_weights <- if ("weight" %in% names(edges)) edges$weight else NULL
    labels <- build_edge_labels_from_template(
      template = aes$label_template,
      style = if (!is.null(aes$label_style)) aes$label_style else "none",
      weights = edge_weights,
      ci_lower = aes$ci_lower,
      ci_upper = aes$ci_upper,
      p_values = aes$label_p,
      stars = aes$label_stars,
      digits = if (!is.null(aes$label_digits)) aes$label_digits else 2,
      p_digits = if (!is.null(aes$label_p_digits)) aes$label_p_digits else 3,
      p_prefix = if (!is.null(aes$label_p_prefix)) aes$label_p_prefix else "p=",
      ci_format = if (!is.null(aes$label_ci_format)) aes$label_ci_format else "bracket",
      oneline = TRUE,
      n = m
    )
  } else if (!is.null(aes$labels)) {
    # Use standard labels
    labels <- recycle_to_length(aes$labels, m)
  } else {
    return(grid::gList())
  }

  if (is.null(labels)) return(grid::gList())

  # Vectorize edge label parameters (strict: length 1 or m)
  label_sizes <- expand_param(
    if (!is.null(aes$label_size)) aes$label_size else 8,
    m, "edge_label_size"
  )
  label_colors <- expand_param(
    if (!is.null(aes$label_color)) aes$label_color else "gray30",
    m, "edge_label_color"
  )

  # Label position along edge (0 = at source, 0.5 = midpoint, 1 = at target)
  # Default 0.65 = slightly closer to target (strict vectorization)
  label_positions <- expand_param(
    if (!is.null(aes$label_position)) aes$label_position else 0.65,
    m, "edge_label_position"
  )
  # Label offset perpendicular to edge - default 0 (on the edge line)
  label_offsets <- expand_param(
    if (!is.null(aes$label_offset)) aes$label_offset else 0,
    m, "edge_label_offset"
  )

  # New styling options - vectorize (strict)
  label_bgs <- expand_param(
    if (!is.null(aes$label_bg)) aes$label_bg else "white",
    m, "edge_label_bg"
  )
  label_bg_padding <- if (!is.null(aes$label_bg_padding)) aes$label_bg_padding else 0.3

  # Vectorize fontface with string-to-number conversion (strict)
  label_fontface_raw <- expand_param(
    if (!is.null(aes$label_fontface)) aes$label_fontface else "plain",
    m, "edge_label_fontface"
  )
  label_fontfaces <- sapply(label_fontface_raw, function(ff) {
    if (is.character(ff)) {
      switch(ff,
        "plain" = 1,
        "bold" = 2,
        "italic" = 3,
        "bold.italic" = 4,
        1  # default
      )
    } else {
      ff
    }
  })
  label_border <- aes$label_border  # NULL, "rect", "rounded", "circle"
  label_border_color <- if (!is.null(aes$label_border_color)) aes$label_border_color else "gray50"
  label_underline <- if (!is.null(aes$label_underline)) aes$label_underline else FALSE

  # Shadow options (strict vectorization)
  label_shadows <- expand_param(
    if (!is.null(aes$label_shadow)) aes$label_shadow else FALSE,
    m, "edge_label_shadow"
  )
  label_shadow_colors <- expand_param(
    if (!is.null(aes$label_shadow_color)) aes$label_shadow_color else "gray40",
    m, "edge_label_shadow_color"
  )
  label_shadow_offsets <- expand_param(
    if (!is.null(aes$label_shadow_offset)) aes$label_shadow_offset else 0.5,
    m, "edge_label_shadow_offset"
  )
  label_shadow_alphas <- expand_param(
    if (!is.null(aes$label_shadow_alpha)) aes$label_shadow_alpha else 0.5,
    m, "edge_label_shadow_alpha"
  )

  # Get curvature for positioning
  curvatures <- recycle_to_length(
    if (!is.null(aes$curvature)) aes$curvature else 0,
    m
  )

  # Get curves mode and apply same logic as render_edges_grid
  # TRUE (default) = reciprocal edges curve as ellipse, singles straight
  # FALSE = all straight; "force" = all curved
  curves_mode <- if (!is.null(aes$curves)) aes$curves else TRUE

  if (!identical(curves_mode, FALSE)) {
    # Identify reciprocal pairs
    is_reciprocal <- rep(FALSE, m)
    for (i in seq_len(m)) {
      from_i <- edges$from[i]
      to_i <- edges$to[i]
      if (from_i == to_i) next
      for (j in seq_len(m)) {
        if (j != i && edges$from[j] == to_i && edges$to[j] == from_i) {
          is_reciprocal[i] <- TRUE
          break
        }
      }
    }

    # Curve reciprocal edges with direction based on network center (qgraph-style)
    default_curve <- 0.25

    # Calculate network center for curve direction
    center_x <- mean(nodes$x)
    center_y <- mean(nodes$y)

    for (i in seq_len(m)) {
      if (is_reciprocal[i] && curvatures[i] == 0) {
        from_i <- edges$from[i]
        to_i <- edges$to[i]

        # Calculate edge midpoint
        mid_x <- (nodes$x[from_i] + nodes$x[to_i]) / 2
        mid_y <- (nodes$y[from_i] + nodes$y[to_i]) / 2

        # Calculate perpendicular direction (for curve)
        dx <- nodes$x[to_i] - nodes$x[from_i]
        dy <- nodes$y[to_i] - nodes$y[from_i]

        # Perpendicular vector (rotated 90 degrees)
        perp_x <- -dy
        perp_y <- dx

        # Check if positive curve moves toward or away from center
        test_x <- mid_x + perp_x * 0.1
        test_y <- mid_y + perp_y * 0.1
        dist_to_center_pos <- sqrt((test_x - center_x)^2 + (test_y - center_y)^2)
        dist_to_center_orig <- sqrt((mid_x - center_x)^2 + (mid_y - center_y)^2)

        # Both edges curve OUTWARD (away from center), on opposite sides
        curvatures[i] <- if (dist_to_center_pos > dist_to_center_orig) default_curve else -default_curve
      }
    }

    # For "force" mode, also curve non-reciprocal edges
    if (identical(curves_mode, "force")) {
      for (i in seq_len(m)) {
        from_i <- edges$from[i]
        to_i <- edges$to[i]
        if (is_reciprocal[i] || from_i == to_i) next
        if (curvatures[i] == 0) {
          curvatures[i] <- default_curve
        }
      }
    }
  }

  # Get curve pivot for label positioning on curves
  curve_pivots <- recycle_to_length(
    if (!is.null(aes$curve_pivot)) aes$curve_pivot else 0.5,
    m
  )

  # Get node sizes for edge endpoint calculation
  node_aes <- network$get_node_aes()
  node_sizes <- recycle_to_length(
    if (!is.null(node_aes$size)) node_aes$size else 0.05,
    nrow(nodes)
  )

  grobs <- vector("list", m)
  for (i in seq_len(m)) {
    from_idx <- edges$from[i]
    to_idx <- edges$to[i]

    # Skip self-loops for labels (would need special handling)
    if (from_idx == to_idx) {
      grobs[[i]] <- grid::nullGrob()
      next
    }

    # Use actual edge endpoints (same as edge rendering)
    node_x1 <- nodes$x[from_idx]
    node_y1 <- nodes$y[from_idx]
    node_x2 <- nodes$x[to_idx]
    node_y2 <- nodes$y[to_idx]

    start_pt <- edge_endpoint(node_x1, node_y1, node_x2, node_y2, node_sizes[from_idx])
    end_pt <- edge_endpoint(node_x2, node_y2, node_x1, node_y1, node_sizes[to_idx])

    x1 <- start_pt$x
    y1 <- start_pt$y
    x2 <- end_pt$x
    y2 <- end_pt$y

    # Position along edge (per-edge value from vectorized label_positions)
    pos <- label_positions[i]

    # Calculate perpendicular direction
    dx <- x2 - x1
    dy <- y2 - y1
    len <- sqrt(dx^2 + dy^2)

    if (len == 0) {
      grobs[[i]] <- grid::nullGrob()
      next
    }

    # Perpendicular unit vector (rotated 90 degrees)
    perp_x <- -dy / len
    perp_y <- dx / len

    # If edge is curved, position label along the curve
    curv <- curvatures[i]
    if (curv != 0) {
      # Get control point
      pivot <- curve_pivots[i]
      ctrl <- curve_control_point(x1, y1, x2, y2, curv, pivot = pivot, shape = 0)

      # Position along bezier curve at 'pos'
      t <- pos
      # Quadratic bezier formula
      x <- (1 - t)^2 * x1 + 2 * (1 - t) * t * ctrl$x + t^2 * x2
      y <- (1 - t)^2 * y1 + 2 * (1 - t) * t * ctrl$y + t^2 * y2
    } else {
      # Straight edge
      x <- x1 + pos * (x2 - x1)
      y <- y1 + pos * (y2 - y1)
    }

    # Apply user-specified offset (per-edge value from vectorized label_offsets)
    offset <- label_offsets[i]
    if (offset != 0) {
      x <- x + offset * perp_x
      y <- y + offset * perp_y
    }

    # Create label grob with styling
    label_grobs <- list()

    # Calculate text dimensions for background/border
    # Get per-edge label styling
    cur_label_size <- label_sizes[i]
    cur_label_color <- label_colors[i]
    cur_label_bg <- label_bgs[i]
    cur_fontface_num <- label_fontfaces[i]

    text_width <- grid::convertWidth(
      grid::stringWidth(as.character(labels[i])),
      "npc", valueOnly = TRUE
    ) * (cur_label_size / 12)  # Scale by font size (smaller)
    text_height <- grid::convertHeight(
      grid::stringHeight(as.character(labels[i])),
      "npc", valueOnly = TRUE
    ) * (cur_label_size / 12)

    # Add padding (smaller halo)
    pad_w <- text_width * label_bg_padding * 0.5
    pad_h <- text_height * label_bg_padding * 0.5
    bg_width <- text_width + pad_w * 2
    bg_height <- text_height + pad_h * 2

    # Draw background (white by default)
    if (!is.na(cur_label_bg) && !is.null(cur_label_bg)) {
      if (!is.null(label_border) && label_border == "circle") {
        # Circle background (tight fit)
        radius <- max(bg_width, bg_height) / 2 * 0.9
        label_grobs[[length(label_grobs) + 1]] <- grid::circleGrob(
          x = grid::unit(x, "npc"),
          y = grid::unit(y, "npc"),
          r = grid::unit(radius, "npc"),
          gp = grid::gpar(fill = cur_label_bg, col = label_border_color, lwd = 0.5)
        )
      } else if (!is.null(label_border) && label_border == "rounded") {
        # Rounded rectangle background (tight fit)
        label_grobs[[length(label_grobs) + 1]] <- grid::roundrectGrob(
          x = grid::unit(x, "npc"),
          y = grid::unit(y, "npc"),
          width = grid::unit(bg_width * 1.1, "npc"),
          height = grid::unit(bg_height * 1.2, "npc"),
          r = grid::unit(0.2, "npc"),
          gp = grid::gpar(fill = cur_label_bg, col = label_border_color, lwd = 0.5)
        )
      } else {
        # Rectangle background (tight fit, default)
        border_col <- if (!is.null(label_border) && label_border == "rect") label_border_color else NA
        label_grobs[[length(label_grobs) + 1]] <- grid::rectGrob(
          x = grid::unit(x, "npc"),
          y = grid::unit(y, "npc"),
          width = grid::unit(bg_width * 1.1, "npc"),
          height = grid::unit(bg_height * 1.2, "npc"),
          gp = grid::gpar(fill = cur_label_bg, col = border_col, lwd = 0.5)
        )
      }
    }

    # Draw shadow text first (if enabled) - per-edge shadow settings
    if (label_shadows[i]) {
      # Calculate shadow offset in NPC units (points to NPC conversion)
      shadow_offset_npc <- label_shadow_offsets[i] * 0.002
      shadow_col <- adjust_alpha(label_shadow_colors[i], label_shadow_alphas[i])

      label_grobs[[length(label_grobs) + 1]] <- grid::textGrob(
        label = labels[i],
        x = grid::unit(x + shadow_offset_npc, "npc"),
        y = grid::unit(y - shadow_offset_npc, "npc"),
        gp = grid::gpar(fontsize = cur_label_size, col = shadow_col, fontface = cur_fontface_num)
      )
    }

    # Draw main text
    label_grobs[[length(label_grobs) + 1]] <- grid::textGrob(
      label = labels[i],
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      gp = grid::gpar(fontsize = cur_label_size, col = cur_label_color, fontface = cur_fontface_num)
    )

    # Draw underline if requested
    if (label_underline) {
      underline_y <- y - text_height * 0.6
      label_grobs[[length(label_grobs) + 1]] <- grid::segmentsGrob(
        x0 = grid::unit(x - text_width / 2, "npc"),
        y0 = grid::unit(underline_y, "npc"),
        x1 = grid::unit(x + text_width / 2, "npc"),
        y1 = grid::unit(underline_y, "npc"),
        gp = grid::gpar(col = cur_label_color, lwd = 0.8)
      )
    }

    grobs[[i]] <- do.call(grid::gList, label_grobs)
  }

  do.call(grid::gList, grobs)
}
