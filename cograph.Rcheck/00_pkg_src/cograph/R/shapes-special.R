#' @title Special Node Shapes
#' @description Special node shape drawing functions (ellipse, heart, star, pie).
#' @name shapes-special
#' @keywords internal
NULL

#' Draw Ellipse Node
#' @keywords internal
draw_ellipse <- function(x, y, size, fill, border_color, border_width,
                         alpha = 1, aspect = 0.6, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Ellipse as polygon approximation
  n_points <- 50
  angles <- seq(0, 2*pi, length.out = n_points + 1)[-1]
  xs <- x + size * cos(angles)
  ys <- y + size * aspect * sin(angles)

  grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(
      fill = fill_col,
      col = border_col,
      lwd = border_width
    )
  )
}

#' Draw Heart Node
#' @keywords internal
draw_heart <- function(x, y, size, fill, border_color, border_width,
                       alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Heart shape using parametric equations
  n_points <- 100
  t <- seq(0, 2*pi, length.out = n_points)

  # Heart parametric equations
  hx <- 16 * sin(t)^3
  hy <- 13 * cos(t) - 5 * cos(2*t) - 2 * cos(3*t) - cos(4*t)

  # Normalize and scale
  hx <- hx / max(abs(hx))
  hy <- hy / max(abs(hy))

  xs <- x + size * 0.8 * hx
  ys <- y + size * 0.8 * hy

  grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(
      fill = fill_col,
      col = border_col,
      lwd = border_width
    )
  )
}

#' Draw Star Node
#' @keywords internal
draw_star <- function(x, y, size, fill, border_color, border_width,
                      alpha = 1, n_points = 5, inner_ratio = 0.4, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Alternating outer and inner points
  n_vertices <- n_points * 2
  angles <- seq(pi/2, pi/2 + 2*pi * (1 - 1/n_vertices), length.out = n_vertices)
  radii <- rep(c(size, size * inner_ratio), n_points)

  xs <- x + radii * cos(angles)
  ys <- y + radii * sin(angles)

  grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(
      fill = fill_col,
      col = border_col,
      lwd = border_width
    )
  )
}

#' Draw Pie Node
#'
#' Draw a pie chart node with multiple segments.
#'
#' @param pie_border_width Border width for pie segments (optional, defaults to border_width * 0.5).
#' @param default_color Fallback color when colors is NULL and there's a single segment.
#' @keywords internal
draw_pie <- function(x, y, size, fill, border_color, border_width,
                     alpha = 1, values = NULL, colors = NULL,
                     pie_border_width = NULL, default_color = NULL, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Use specific pie_border_width if provided, else default
  segment_border <- if (!is.null(pie_border_width)) pie_border_width else border_width * 0.5

  # If no values, draw a simple circle
  if (is.null(values) || length(values) <= 1) {
    # Use default_color if provided
    actual_fill <- if (!is.null(default_color)) default_color else fill
    return(draw_circle(x, y, size, actual_fill, border_color, border_width, alpha, ...))
  }

  # Normalize values to proportions
  props <- values / sum(values)

  # Default colors if not provided
  if (is.null(colors)) {
    if (!is.null(default_color) && length(values) == 1) {
      colors <- adjust_alpha(default_color, alpha)
    } else {
      colors <- grDevices::rainbow(length(values), alpha = alpha)
    }
  } else {
    colors <- sapply(colors, adjust_alpha, alpha = alpha)
  }

  # Create pie slices
  grobs <- list()
  start_angle <- pi/2

  for (i in seq_along(props)) {
    end_angle <- start_angle - 2 * pi * props[i]

    # Create arc
    n_points <- max(20, ceiling(50 * props[i]))
    angles <- seq(start_angle, end_angle, length.out = n_points)

    xs <- c(x, x + size * cos(angles), x)
    ys <- c(y, y + size * sin(angles), y)

    # Use NA for border if segment_border is 0 or very small
    seg_col <- if (!is.null(segment_border) && segment_border > 0.1) border_col else NA

    grobs[[i]] <- grid::polygonGrob(
      x = grid::unit(xs, "npc"),
      y = grid::unit(ys, "npc"),
      gp = grid::gpar(
        fill = colors[i],
        col = seg_col,
        lwd = segment_border
      )
    )

    start_angle <- end_angle
  }

  # Add outer border
  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    r = grid::unit(size, "npc"),
    gp = grid::gpar(
      fill = NA,
      col = border_col,
      lwd = border_width
    )
  )

  do.call(grid::gList, grobs)
}

#' Draw Polygon Donut Node
#'
#' Draws a donut ring on a polygon shape where segments follow polygon edges.
#' The fill shows a proportion (0-1) as filled segments starting from the top vertex.
#'
#' @param x,y Node center coordinates (NPC units).
#' @param size Node radius (NPC units).
#' @param fill Fill color for the donut ring.
#' @param border_color Border color.
#' @param border_width Border line width.
#' @param alpha Transparency (0-1).
#' @param values Single numeric value (0-1) specifying fill proportion.
#'   0.1 = 10% filled, 0.5 = 50% filled, 1.0 = full ring.
#' @param colors Override fill color (optional).
#' @param inner_ratio Ratio of inner to outer radius (0-1). Default 0.5.
#' @param bg_color Background color for unfilled portion. Default "gray90".
#' @param donut_shape Base polygon shape: "circle", "square", "hexagon", "triangle", "diamond", "pentagon".
#' @param show_value Logical: show value in center? Default FALSE.
#' @param value_size Font size for center value.
#' @param value_color Color for center value text.
#' @param value_fontface Font face for center value.
#' @param value_fontfamily Font family for center value.
#' @param value_digits Decimal places for value display.
#' @param value_prefix Text before value.
#' @param value_suffix Text after value.
#' @param value_format Custom format function.
#' @param donut_border_width Border width for donut ring (NULL = use border_width).
#' @keywords internal
draw_polygon_donut <- function(x, y, size, fill, border_color, border_width,
                               alpha = 1, values = NULL, colors = NULL,
                               inner_ratio = 0.5, bg_color = "gray90",
                               donut_shape = "square",
                               show_value = TRUE, value_size = 8, value_color = "black",
                               value_fontface = "bold", value_fontfamily = "sans",
                               value_digits = 2, value_prefix = "", value_suffix = "",
                               value_format = NULL, donut_border_width = NULL, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)
  bg_col <- adjust_alpha(bg_color, alpha)

  ring_border <- if (!is.null(donut_border_width)) donut_border_width else border_width

  # Get outer polygon vertices
  outer <- get_donut_base_vertices(donut_shape, x, y, size)

  # Get inner polygon vertices
  inner <- inset_polygon_vertices(outer, inner_ratio)

  n_verts <- length(outer$x)
  grobs <- list()
  center_value <- NULL

  # Helper to draw a ring segment between two vertex pairs
  draw_ring_segment <- function(idx_start, idx_end, segment_col) {
    seg_x <- c(outer$x[idx_start], outer$x[idx_end], inner$x[idx_end], inner$x[idx_start])
    seg_y <- c(outer$y[idx_start], outer$y[idx_end], inner$y[idx_end], inner$y[idx_start])

    grid::polygonGrob(
      x = grid::unit(seg_x, "npc"),
      y = grid::unit(seg_y, "npc"),
      gp = grid::gpar(fill = segment_col, col = NA)
    )
  }

  if (is.null(values) || length(values) == 0) {
    values <- 1
    if (is.null(colors)) colors <- fill_col
  }

  if (length(values) == 1) {
    # Progress donut
    prop <- max(0, min(1, values))
    center_value <- prop

    # Draw background ring
    for (i in seq_len(n_verts)) {
      i_next <- if (i == n_verts) 1 else i + 1
      grobs[[length(grobs) + 1]] <- draw_ring_segment(i, i_next, bg_col)
    }

    # Draw filled portion
    if (prop > 0) {
      segment_col <- if (!is.null(colors)) colors[1] else fill_col
      filled_verts <- max(1, round(prop * n_verts))

      for (i in seq_len(filled_verts)) {
        i_next <- if (i == n_verts) 1 else i + 1
        grobs[[length(grobs) + 1]] <- draw_ring_segment(i, i_next, segment_col)
      }
    }
  } else {
    # Multi-segment donut
    props <- values / sum(values)
    n_seg <- length(props)

    if (is.null(colors)) {
      colors <- grDevices::rainbow(n_seg, s = 0.7, v = 0.9)
    }
    colors <- recycle_to_length(colors, n_seg)

    vert_idx <- 1
    for (seg in seq_len(n_seg)) {
      seg_verts <- max(1, round(props[seg] * n_verts))
      seg_col <- adjust_alpha(colors[seg], alpha)

      for (j in seq_len(seg_verts)) {
        if (vert_idx > n_verts) break
        i_next <- if (vert_idx == n_verts) 1 else vert_idx + 1
        grobs[[length(grobs) + 1]] <- draw_ring_segment(vert_idx, i_next, seg_col)
        vert_idx <- vert_idx + 1
      }
    }
  }

  # Outer border
  grobs[[length(grobs) + 1]] <- grid::polygonGrob(
    x = grid::unit(outer$x, "npc"),
    y = grid::unit(outer$y, "npc"),
    gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
  )

  # Inner border and fill (center uses node fill color)
  grobs[[length(grobs) + 1]] <- grid::polygonGrob(
    x = grid::unit(inner$x, "npc"),
    y = grid::unit(inner$y, "npc"),
    gp = grid::gpar(fill = fill_col, col = border_col, lwd = ring_border)
  )

  # Value text in center
  if (show_value && !is.null(center_value)) {
    if (!is.null(value_format) && is.function(value_format)) {
      formatted_value <- value_format(center_value)
    } else {
      formatted_value <- round(center_value, value_digits)
    }
    label_text <- paste0(value_prefix, formatted_value, value_suffix)

    fontface_num <- switch(value_fontface,
      "plain" = 1, "bold" = 2, "italic" = 3, "bold.italic" = 4, 2
    )

    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = label_text,
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      gp = grid::gpar(
        fontsize = value_size, col = value_color,
        fontface = fontface_num, fontfamily = value_fontfamily
      )
    )
  }

  do.call(grid::gList, grobs)
}

#' Draw Donut Node
#'
#' Draw a donut chart node showing a fill proportion (0-1) as an arc.
#' The fill starts from 12 o'clock (top) and fills clockwise.
#'
#' @param x,y Node center coordinates (NPC units).
#' @param size Node radius (NPC units).
#' @param fill Fill color for the donut ring.
#' @param border_color Border color.
#' @param border_width Border line width.
#' @param alpha Transparency (0-1).
#' @param values Single numeric value (0-1) specifying fill proportion.
#'   0.1 = 10% filled arc, 0.5 = 50% filled, 1.0 = full ring.
#' @param colors Override fill color (optional).
#' @param inner_ratio Ratio of inner to outer radius (0-1). Default 0.5.
#' @param bg_color Background color for unfilled portion. Default "gray90".
#' @param show_value Logical: show value in center? Default FALSE.
#' @param value_size Font size for center value.
#' @param value_color Color for center value text.
#' @param value_fontface Font face for center value.
#' @param value_fontfamily Font family for center value.
#' @param value_digits Decimal places for value display.
#' @param value_prefix Text before value (e.g., "$").
#' @param value_suffix Text after value (e.g., "%").
#' @param value_format Custom format function (overrides digits).
#' @param donut_border_width Border width for donut ring (NULL = use border_width).
#' @keywords internal
draw_donut <- function(x, y, size, fill, border_color, border_width,
                       alpha = 1, values = NULL, colors = NULL,
                       inner_ratio = 0.5, bg_color = "gray90",
                       show_value = TRUE, value_size = 8, value_color = "black",
                       value_fontface = "bold", value_fontfamily = "sans",
                       value_digits = 2, value_prefix = "", value_suffix = "",
                       value_format = NULL, donut_border_width = NULL, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)
  bg_col <- adjust_alpha(bg_color, alpha)

  # Use specific donut_border_width if provided, else default to border_width
  ring_border <- if (!is.null(donut_border_width)) donut_border_width else border_width

  outer_r <- size
  inner_r <- size * inner_ratio

  grobs <- list()
  center_value <- NULL

  # Use symbols() approach - convert to grob after drawing
  # This ensures proper aspect ratio handling

  # Get viewport dimensions to match circleGrob's radius calculation
  # circleGrob with NPC units uses min(width, height) as reference
  vp_width <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  vp_height <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)

  # Calculate scaling factors to match circleGrob behavior
  min_dim <- min(vp_width, vp_height)
  x_scale <- min_dim / vp_width
  y_scale <- min_dim / vp_height

  # Helper function to create pie wedge coordinates for a ring segment
  # Uses same scaling as circleGrob for perfect alignment
  make_ring_coords <- function(start_ang, end_ang, outer_radius, inner_radius, cx, cy, n_pts = 100) {
    angles <- seq(start_ang, end_ang, length.out = n_pts)

    # Apply same scaling as circleGrob
    outer_x <- cx + (outer_radius * x_scale) * cos(angles)
    outer_y <- cy + (outer_radius * y_scale) * sin(angles)

    inner_x <- cx + (inner_radius * x_scale) * cos(rev(angles))
    inner_y <- cy + (inner_radius * y_scale) * sin(rev(angles))

    list(x = c(outer_x, inner_x), y = c(outer_y, inner_y))
  }

  # Handle single value case
  if (is.null(values) || length(values) == 1) {
    prop <- if (is.null(values)) 1 else values[1]
    prop <- max(0, min(1, prop))
    center_value <- prop

    # Inset factor to keep fill inside border
    inset <- 0.97

    # 1. Draw background ring (full circle) - slightly inside the border
    bg_coords <- make_ring_coords(0, 2 * pi, outer_r * inset, inner_r / inset, x, y, 200)
    grobs[[length(grobs) + 1]] <- grid::polygonGrob(
      x = grid::unit(bg_coords$x, "npc"),
      y = grid::unit(bg_coords$y, "npc"),
      gp = grid::gpar(fill = bg_col, col = NA)
    )

    # 2. Draw filled portion (from 12 o'clock clockwise)
    if (prop > 0) {
      start_ang <- pi / 2
      end_ang <- pi / 2 - 2 * pi * prop
      n_pts <- max(100, ceiling(300 * prop))
      fill_coords <- make_ring_coords(start_ang, end_ang, outer_r * inset, inner_r / inset, x, y, n_pts)
      grobs[[length(grobs) + 1]] <- grid::polygonGrob(
        x = grid::unit(fill_coords$x, "npc"),
        y = grid::unit(fill_coords$y, "npc"),
        gp = grid::gpar(fill = fill_col, col = NA)
      )
    }

    # 3. Fill inner hole (center uses node fill color)
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      r = grid::unit(inner_r, "npc"),
      gp = grid::gpar(fill = fill_col, col = NA)
    )

    # 4. Redraw borders for clean edges
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      r = grid::unit(outer_r, "npc"),
      gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
    )
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      r = grid::unit(inner_r, "npc"),
      gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
    )

  } else {
    # Multiple values: donut with segments
    props <- values / sum(values)

    if (is.null(colors)) {
      colors <- grDevices::rainbow(length(values), alpha = alpha)
    } else {
      colors <- sapply(colors, adjust_alpha, alpha = alpha)
    }

    # Inset factor to keep fill inside border
    inset <- 0.97

    # Draw arc segments
    start_ang <- pi / 2
    for (i in seq_along(props)) {
      end_ang <- start_ang - 2 * pi * props[i]
      n_pts <- max(50, ceiling(150 * props[i]))
      seg_coords <- make_ring_coords(start_ang, end_ang, outer_r * inset, inner_r / inset, x, y, n_pts)
      grobs[[length(grobs) + 1]] <- grid::polygonGrob(
        x = grid::unit(seg_coords$x, "npc"),
        y = grid::unit(seg_coords$y, "npc"),
        gp = grid::gpar(fill = colors[i], col = NA)
      )
      start_ang <- end_ang
    }

    # Fill inner hole (center uses node fill color)
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      r = grid::unit(inner_r, "npc"),
      gp = grid::gpar(fill = fill_col, col = NA)
    )

    # Draw borders
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      r = grid::unit(outer_r, "npc"),
      gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
    )
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      r = grid::unit(inner_r, "npc"),
      gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
    )
  }

  # Add outer border
  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    r = grid::unit(outer_r, "npc"),
    gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
  )

  # Add inner border
  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    r = grid::unit(inner_r, "npc"),
    gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
  )

  # Add value text in center (for single value donut)
  if (show_value && !is.null(center_value)) {
    # Format the value
    if (!is.null(value_format) && is.function(value_format)) {
      formatted_value <- value_format(center_value)
    } else {
      formatted_value <- round(center_value, value_digits)
    }
    label_text <- paste0(value_prefix, formatted_value, value_suffix)

    # Convert fontface string to numeric
    fontface_num <- switch(value_fontface,
      "plain" = 1,
      "bold" = 2,
      "italic" = 3,
      "bold.italic" = 4,
      2  # default to bold
    )

    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = label_text,
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      gp = grid::gpar(
        fontsize = value_size,
        col = value_color,
        fontface = fontface_num,
        fontfamily = value_fontfamily
      )
    )
  }

  do.call(grid::gList, grobs)
}

#' Draw Donut with Inner Pie Node
#'
#' Draw a node with an outer donut ring showing a proportion and an inner
#' pie chart with multiple segments.
#'
#' @param pie_border_width Border width for pie segments (optional).
#' @param donut_border_width Border width for donut ring (optional).
#' @keywords internal
draw_donut_pie <- function(x, y, size, fill, border_color, border_width,
                           alpha = 1, donut_value = NULL, pie_values = NULL,
                           pie_colors = NULL, inner_ratio = 0.5,
                           bg_color = "gray90", pie_border_width = NULL,
                           donut_border_width = NULL, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)
  bg_col <- adjust_alpha(bg_color, alpha)

  # Use specific border widths if provided
  ring_border <- if (!is.null(donut_border_width)) donut_border_width else border_width
  pie_segment_border <- if (!is.null(pie_border_width)) pie_border_width else border_width * 0.5

  outer_r <- size
  inner_r <- size * inner_ratio

  grobs <- list()

  # Get viewport dimensions for aspect ratio correction
 vp_width <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  vp_height <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  min_dim <- min(vp_width, vp_height)
  x_scale <- min_dim / vp_width
  y_scale <- min_dim / vp_height

  # Helper function for ring coordinates
  make_ring_coords <- function(start_ang, end_ang, outer_radius, inner_radius, cx, cy, n_pts = 100) {
    angles <- seq(start_ang, end_ang, length.out = n_pts)
    outer_x <- cx + (outer_radius * x_scale) * cos(angles)
    outer_y <- cy + (outer_radius * y_scale) * sin(angles)
    inner_x <- cx + (inner_radius * x_scale) * cos(rev(angles))
    inner_y <- cy + (inner_radius * y_scale) * sin(rev(angles))
    list(x = c(outer_x, inner_x), y = c(outer_y, inner_y))
  }

  # Helper function for pie slice coordinates
  make_pie_coords <- function(start_ang, end_ang, radius, cx, cy, n_pts = 50) {
    angles <- seq(start_ang, end_ang, length.out = n_pts)
    xs <- c(cx, cx + (radius * x_scale) * cos(angles), cx)
    ys <- c(cy, cy + (radius * y_scale) * sin(angles), cy)
    list(x = xs, y = ys)
  }

  inset <- 0.97

  # 1. Draw outer donut ring (background)
  bg_coords <- make_ring_coords(0, 2 * pi, outer_r * inset, inner_r / inset, x, y, 200)
  grobs[[length(grobs) + 1]] <- grid::polygonGrob(
    x = grid::unit(bg_coords$x, "npc"),
    y = grid::unit(bg_coords$y, "npc"),
    gp = grid::gpar(fill = bg_col, col = NA)
  )

  # 2. Draw donut filled portion (if donut_value provided)
  donut_prop <- if (is.null(donut_value)) 1 else max(0, min(1, donut_value))
  if (donut_prop > 0) {
    start_ang <- pi / 2
    end_ang <- pi / 2 - 2 * pi * donut_prop
    n_pts <- max(100, ceiling(300 * donut_prop))
    fill_coords <- make_ring_coords(start_ang, end_ang, outer_r * inset, inner_r / inset, x, y, n_pts)
    grobs[[length(grobs) + 1]] <- grid::polygonGrob(
      x = grid::unit(fill_coords$x, "npc"),
      y = grid::unit(fill_coords$y, "npc"),
      gp = grid::gpar(fill = fill_col, col = NA)
    )
  }

  # 3. Draw inner pie chart
  pie_radius <- inner_r * 0.95
  if (!is.null(pie_values) && length(pie_values) > 0) {
    props <- pie_values / sum(pie_values)

    if (is.null(pie_colors)) {
      pie_colors <- grDevices::rainbow(length(pie_values), alpha = alpha)
    } else {
      pie_colors <- sapply(pie_colors, adjust_alpha, alpha = alpha)
      pie_colors <- rep(pie_colors, length.out = length(pie_values))
    }

    start_ang <- pi / 2
    for (i in seq_along(props)) {
      end_ang <- start_ang - 2 * pi * props[i]
      n_pts <- max(30, ceiling(100 * props[i]))
      pie_coords <- make_pie_coords(start_ang, end_ang, pie_radius, x, y, n_pts)
      grobs[[length(grobs) + 1]] <- grid::polygonGrob(
        x = grid::unit(pie_coords$x, "npc"),
        y = grid::unit(pie_coords$y, "npc"),
        gp = grid::gpar(fill = pie_colors[i], col = NA)
      )
      start_ang <- end_ang
    }
  } else {
    # No pie values - fill inner with white
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      r = grid::unit(pie_radius, "npc"),
      gp = grid::gpar(fill = "white", col = NA)
    )
  }

  # 4. Draw borders
  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
    r = grid::unit(outer_r, "npc"),
    gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
  )
  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
    r = grid::unit(inner_r, "npc"),
    gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
  )

  do.call(grid::gList, grobs)
}

#' Draw Double Donut with Inner Pie Node
#'
#' Draw a node with two concentric donut rings and an optional inner pie chart.
#' From outside to inside: outer donut ring, inner donut ring, center pie.
#'
#' @param pie_border_width Border width for pie segments (optional).
#' @param donut_border_width Border width for donut rings (optional).
#' @keywords internal
draw_double_donut_pie <- function(x, y, size, fill, border_color, border_width,
                                  alpha = 1, donut_values = NULL, donut_colors = NULL,
                                  donut2_values = NULL, donut2_colors = NULL,
                                  pie_values = NULL, pie_colors = NULL,
                                  outer_inner_ratio = 0.7, inner_inner_ratio = 0.4,
                                  bg_color = "gray90", pie_border_width = NULL,
                                  donut_border_width = NULL, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)
  bg_col <- adjust_alpha(bg_color, alpha)

  # Use specific border widths if provided
  ring_border <- if (!is.null(donut_border_width)) donut_border_width else border_width
  pie_segment_border <- if (!is.null(pie_border_width)) pie_border_width else border_width * 0.5

  # Define radii for the three layers
  outer_r <- size
  mid_r <- size * outer_inner_ratio
  inner_r <- size * inner_inner_ratio

  grobs <- list()

  # Get viewport dimensions for aspect ratio correction
  vp_width <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  vp_height <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  min_dim <- min(vp_width, vp_height)
  x_scale <- min_dim / vp_width
  y_scale <- min_dim / vp_height

  # Helper function for ring coordinates
  make_ring_coords <- function(start_ang, end_ang, r_outer, r_inner, cx, cy, n_pts = 100) {
    angles <- seq(start_ang, end_ang, length.out = n_pts)
    outer_x <- cx + (r_outer * x_scale) * cos(angles)
    outer_y <- cy + (r_outer * y_scale) * sin(angles)
    inner_x <- cx + (r_inner * x_scale) * cos(rev(angles))
    inner_y <- cy + (r_inner * y_scale) * sin(rev(angles))
    list(x = c(outer_x, inner_x), y = c(outer_y, inner_y))
  }

  # Helper function for pie slice coordinates
  make_pie_coords <- function(start_ang, end_ang, radius, cx, cy, n_pts = 50) {
    angles <- seq(start_ang, end_ang, length.out = n_pts)
    xs <- c(cx, cx + (radius * x_scale) * cos(angles), cx)
    ys <- c(cy, cy + (radius * y_scale) * sin(angles), cy)
    list(x = xs, y = ys)
  }

  inset <- 0.97

  # Helper to draw donut ring (handles both progress and segmented)
  draw_donut_ring_grid <- function(values, colors, r_outer, r_inner) {
    if (is.null(values)) {
      # Fill with background
      bg_coords <- make_ring_coords(0, 2 * pi, r_outer * inset, r_inner / inset, x, y, 200)
      return(list(grid::polygonGrob(
        x = grid::unit(bg_coords$x, "npc"),
        y = grid::unit(bg_coords$y, "npc"),
        gp = grid::gpar(fill = bg_col, col = NA)
      )))
    }

    ring_grobs <- list()

    if (length(values) == 1) {
      # Progress donut - draw background then filled portion
      bg_coords <- make_ring_coords(0, 2 * pi, r_outer * inset, r_inner / inset, x, y, 200)
      ring_grobs[[length(ring_grobs) + 1]] <- grid::polygonGrob(
        x = grid::unit(bg_coords$x, "npc"),
        y = grid::unit(bg_coords$y, "npc"),
        gp = grid::gpar(fill = bg_col, col = NA)
      )

      prop <- max(0, min(1, values))
      if (prop > 0) {
        fill_c <- if (!is.null(colors)) adjust_alpha(colors[1], alpha) else fill_col
        start_ang <- pi / 2
        end_ang <- pi / 2 - 2 * pi * prop
        n_pts <- max(100, ceiling(300 * prop))
        fill_coords <- make_ring_coords(start_ang, end_ang, r_outer * inset, r_inner / inset, x, y, n_pts)
        ring_grobs[[length(ring_grobs) + 1]] <- grid::polygonGrob(
          x = grid::unit(fill_coords$x, "npc"),
          y = grid::unit(fill_coords$y, "npc"),
          gp = grid::gpar(fill = fill_c, col = NA)
        )
      }
    } else {
      # Segmented donut
      props <- values / sum(values)

      if (is.null(colors)) {
        colors <- grDevices::rainbow(length(values), alpha = alpha)
      } else {
        colors <- sapply(colors, adjust_alpha, alpha = alpha)
        colors <- rep(colors, length.out = length(values))
      }

      start_ang <- pi / 2
      for (i in seq_along(props)) {
        end_ang <- start_ang - 2 * pi * props[i]
        n_pts <- max(50, ceiling(150 * props[i]))
        seg_coords <- make_ring_coords(start_ang, end_ang, r_outer * inset, r_inner / inset, x, y, n_pts)
        ring_grobs[[length(ring_grobs) + 1]] <- grid::polygonGrob(
          x = grid::unit(seg_coords$x, "npc"),
          y = grid::unit(seg_coords$y, "npc"),
          gp = grid::gpar(fill = colors[i], col = NA)
        )
        start_ang <- end_ang
      }
    }

    ring_grobs
  }

  # 1. Draw outer donut ring
  grobs <- c(grobs, draw_donut_ring_grid(donut_values, donut_colors, outer_r, mid_r))

  # 2. Draw inner donut ring
  grobs <- c(grobs, draw_donut_ring_grid(donut2_values, donut2_colors, mid_r, inner_r))

  # 3. Draw center pie (if values provided)
  pie_radius <- inner_r * 0.95
  if (!is.null(pie_values) && length(pie_values) > 0) {
    props <- pie_values / sum(pie_values)

    if (is.null(pie_colors)) {
      pie_colors <- grDevices::rainbow(length(pie_values), alpha = alpha)
    } else {
      pie_colors <- sapply(pie_colors, adjust_alpha, alpha = alpha)
      pie_colors <- rep(pie_colors, length.out = length(pie_values))
    }

    start_ang <- pi / 2
    for (i in seq_along(props)) {
      end_ang <- start_ang - 2 * pi * props[i]
      n_pts <- max(30, ceiling(100 * props[i]))
      pie_coords <- make_pie_coords(start_ang, end_ang, pie_radius, x, y, n_pts)
      grobs[[length(grobs) + 1]] <- grid::polygonGrob(
        x = grid::unit(pie_coords$x, "npc"),
        y = grid::unit(pie_coords$y, "npc"),
        gp = grid::gpar(fill = pie_colors[i], col = NA)
      )
      start_ang <- end_ang
    }
  } else {
    # No pie values - fill inner with white
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      r = grid::unit(pie_radius, "npc"),
      gp = grid::gpar(fill = "white", col = NA)
    )
  }

  # 4. Draw all borders
  # Outer border
  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
    r = grid::unit(outer_r, "npc"),
    gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
  )
  # Middle border (between outer and inner donut)
  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
    r = grid::unit(mid_r, "npc"),
    gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
  )
  # Inner border (between inner donut and pie)
  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
    r = grid::unit(inner_r, "npc"),
    gp = grid::gpar(fill = NA, col = border_col, lwd = ring_border)
  )

  do.call(grid::gList, grobs)
}

#' Draw Neural Node
#'
#' Circle with small connection circles around the perimeter (neuron-like).
#'
#' @param n_connections Number of connection points around perimeter.
#' @keywords internal
draw_neural <- function(x, y, size, fill, border_color, border_width,
                        alpha = 1, n_connections = 6, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Get viewport dimensions for aspect correction
  vp_width <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  vp_height <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  min_dim <- min(vp_width, vp_height)
  x_scale <- min_dim / vp_width
  y_scale <- min_dim / vp_height

  grobs <- list()

  # Main center circle
  grobs[[1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    r = grid::unit(size * 0.6, "npc"),
    gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
  )

  # Connection circles around perimeter
  conn_radius <- size * 0.15
  angles <- seq(0, 2 * pi * (1 - 1/n_connections), length.out = n_connections)

  for (i in seq_along(angles)) {
    cx <- x + (size * 0.85 * x_scale) * cos(angles[i])
    cy <- y + (size * 0.85 * y_scale) * sin(angles[i])

    # Line from center to connection
    grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
      x0 = grid::unit(x, "npc"),
      y0 = grid::unit(y, "npc"),
      x1 = grid::unit(cx, "npc"),
      y1 = grid::unit(cy, "npc"),
      gp = grid::gpar(col = border_col, lwd = border_width * 0.5)
    )

    # Connection circle
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(cx, "npc"),
      y = grid::unit(cy, "npc"),
      r = grid::unit(conn_radius, "npc"),
      gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width * 0.7)
    )
  }

  do.call(grid::gList, grobs)
}

#' Draw Chip Node
#'
#' Square with pins extending from all edges (processor/IC chip).
#'
#' @param pins_per_side Number of pins per side.
#' @keywords internal
draw_chip <- function(x, y, size, fill, border_color, border_width,
                      alpha = 1, pins_per_side = 3, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  grobs <- list()

  # Main body (square with corner notch)
  body_size <- size * 0.7
  notch_size <- body_size * 0.15

  # Create notched square polygon
  xs <- c(
    x - body_size, x - body_size + notch_size, x + body_size, x + body_size, x - body_size
  )
  ys <- c(
    y - body_size, y + body_size, y + body_size, y - body_size, y - body_size
  )

  grobs[[1]] <- grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
  )

  # Draw pins on each side
  pin_length <- size * 0.2
  pin_width <- body_size * 0.8 / (pins_per_side * 2 - 1)

  # Helper to draw pins
  draw_pins <- function(side) {
    pin_grobs <- list()
    for (i in seq_len(pins_per_side)) {
      offset <- (i - (pins_per_side + 1) / 2) * (body_size * 1.5 / pins_per_side)

      if (side == "top") {
        px <- x + offset
        py <- y + body_size
        p_xs <- c(px - pin_width/2, px + pin_width/2, px + pin_width/2, px - pin_width/2)
        p_ys <- c(py, py, py + pin_length, py + pin_length)
      } else if (side == "bottom") {
        px <- x + offset
        py <- y - body_size
        p_xs <- c(px - pin_width/2, px + pin_width/2, px + pin_width/2, px - pin_width/2)
        p_ys <- c(py, py, py - pin_length, py - pin_length)
      } else if (side == "left") {
        px <- x - body_size
        py <- y + offset
        p_xs <- c(px, px, px - pin_length, px - pin_length)
        p_ys <- c(py - pin_width/2, py + pin_width/2, py + pin_width/2, py - pin_width/2)
      } else {  # right
        px <- x + body_size
        py <- y + offset
        p_xs <- c(px, px, px + pin_length, px + pin_length)
        p_ys <- c(py - pin_width/2, py + pin_width/2, py + pin_width/2, py - pin_width/2)
      }

      pin_grobs[[i]] <- grid::polygonGrob(
        x = grid::unit(p_xs, "npc"),
        y = grid::unit(p_ys, "npc"),
        gp = grid::gpar(fill = border_col, col = border_col, lwd = 0.5)
      )
    }
    pin_grobs
  }

  grobs <- c(grobs, draw_pins("top"), draw_pins("bottom"),
             draw_pins("left"), draw_pins("right"))

  do.call(grid::gList, grobs)
}

#' Draw Robot Node
#'
#' Rounded square with antenna and eyes (robot head).
#'
#' @keywords internal
draw_robot <- function(x, y, size, fill, border_color, border_width,
                       alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  grobs <- list()

  # Robot head (rounded rectangle)
  head_w <- size * 0.8
  head_h <- size * 0.7

  grobs[[1]] <- grid::roundrectGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y - size * 0.1, "npc"),
    width = grid::unit(head_w * 2, "npc"),
    height = grid::unit(head_h * 2, "npc"),
    r = grid::unit(0.2, "npc"),
    gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
  )

  # Antenna stem
  antenna_base_y <- y + head_h - size * 0.1
  grobs[[2]] <- grid::segmentsGrob(
    x0 = grid::unit(x, "npc"),
    y0 = grid::unit(antenna_base_y, "npc"),
    x1 = grid::unit(x, "npc"),
    y1 = grid::unit(y + size, "npc"),
    gp = grid::gpar(col = border_col, lwd = border_width)
  )

  # Antenna ball
  grobs[[3]] <- grid::circleGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y + size + size * 0.08, "npc"),
    r = grid::unit(size * 0.08, "npc"),
    gp = grid::gpar(fill = border_col, col = border_col)
  )

  # Eyes (two circles)
  eye_y <- y
  eye_radius <- size * 0.12

  grobs[[4]] <- grid::circleGrob(
    x = grid::unit(x - head_w * 0.4, "npc"),
    y = grid::unit(eye_y, "npc"),
    r = grid::unit(eye_radius, "npc"),
    gp = grid::gpar(fill = "white", col = border_col, lwd = border_width * 0.7)
  )

  grobs[[5]] <- grid::circleGrob(
    x = grid::unit(x + head_w * 0.4, "npc"),
    y = grid::unit(eye_y, "npc"),
    r = grid::unit(eye_radius, "npc"),
    gp = grid::gpar(fill = "white", col = border_col, lwd = border_width * 0.7)
  )

  # Mouth (horizontal line)
  grobs[[6]] <- grid::segmentsGrob(
    x0 = grid::unit(x - head_w * 0.3, "npc"),
    y0 = grid::unit(y - head_h * 0.4, "npc"),
    x1 = grid::unit(x + head_w * 0.3, "npc"),
    y1 = grid::unit(y - head_h * 0.4, "npc"),
    gp = grid::gpar(col = border_col, lwd = border_width)
  )

  do.call(grid::gList, grobs)
}

#' Draw Brain Node
#'
#' Simplified brain outline using overlapping curves.
#'
#' @keywords internal
draw_brain <- function(x, y, size, fill, border_color, border_width,
                       alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Brain shape using overlapping lobes
  n_pts <- 80
  t <- seq(0, 2 * pi, length.out = n_pts)

  # Create irregular brain-like shape
  r <- size * (0.7 + 0.15 * sin(3 * t) + 0.1 * sin(5 * t) + 0.05 * cos(7 * t))
  xs <- x + r * cos(t)
  ys <- y + r * sin(t) * 0.85  # Slightly flattened

  grobs <- list()

  # Main brain shape
  grobs[[1]] <- grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
  )

  # Central fissure (dividing line)
  grobs[[2]] <- grid::segmentsGrob(
    x0 = grid::unit(x, "npc"),
    y0 = grid::unit(y + size * 0.6, "npc"),
    x1 = grid::unit(x, "npc"),
    y1 = grid::unit(y - size * 0.5, "npc"),
    gp = grid::gpar(col = border_col, lwd = border_width * 0.5)
  )

  do.call(grid::gList, grobs)
}

#' Draw Network Node
#'
#' Interconnected nodes pattern (mini network inside).
#'
#' @keywords internal
draw_network <- function(x, y, size, fill, border_color, border_width,
                         alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Get viewport dimensions for aspect correction
  vp_width <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  vp_height <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  min_dim <- min(vp_width, vp_height)
  x_scale <- min_dim / vp_width
  y_scale <- min_dim / vp_height

  grobs <- list()

  # Outer boundary circle
  grobs[[1]] <- grid::circleGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    r = grid::unit(size, "npc"),
    gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
  )

  # Mini nodes inside (pentagon arrangement)
  n_nodes <- 5
  inner_r <- size * 0.55
  node_r <- size * 0.12
  angles <- seq(pi/2, pi/2 + 2 * pi * (1 - 1/n_nodes), length.out = n_nodes)

  node_x <- x + (inner_r * x_scale) * cos(angles)
  node_y <- y + (inner_r * y_scale) * sin(angles)

  # Draw edges between nodes
  for (i in seq_len(n_nodes)) {
    for (j in seq_len(n_nodes)) {
      if (i < j) {
        grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
          x0 = grid::unit(node_x[i], "npc"),
          y0 = grid::unit(node_y[i], "npc"),
          x1 = grid::unit(node_x[j], "npc"),
          y1 = grid::unit(node_y[j], "npc"),
          gp = grid::gpar(col = border_col, lwd = border_width * 0.5)
        )
      }
    }
  }

  # Draw mini nodes
  for (i in seq_len(n_nodes)) {
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x = grid::unit(node_x[i], "npc"),
      y = grid::unit(node_y[i], "npc"),
      r = grid::unit(node_r, "npc"),
      gp = grid::gpar(fill = "white", col = border_col, lwd = border_width * 0.7)
    )
  }

  do.call(grid::gList, grobs)
}

#' Draw Database Node
#'
#' Cylinder shape (data storage).
#'
#' @keywords internal
draw_database <- function(x, y, size, fill, border_color, border_width,
                          alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  grobs <- list()

  cyl_width <- size * 0.8
  cyl_height <- size * 1.2
  ellipse_h <- size * 0.25

  n_pts <- 50
  angles <- seq(0, pi, length.out = n_pts)
  angles_full <- seq(0, 2 * pi, length.out = n_pts * 2)

  # Bottom ellipse
  bottom_y <- y - cyl_height / 2

  # Cylinder body (rectangle)
  body_xs <- c(x - cyl_width, x + cyl_width, x + cyl_width, x - cyl_width)
  body_ys <- c(bottom_y, bottom_y, y + cyl_height / 2, y + cyl_height / 2)

  grobs[[1]] <- grid::polygonGrob(
    x = grid::unit(body_xs, "npc"),
    y = grid::unit(body_ys, "npc"),
    gp = grid::gpar(fill = fill_col, col = NA)
  )

  # Bottom ellipse (lower half visible)
  bottom_x <- x + cyl_width * cos(angles)
  bottom_y_pts <- bottom_y + ellipse_h * sin(angles) * (-1)

  grobs[[2]] <- grid::linesGrob(
    x = grid::unit(bottom_x, "npc"),
    y = grid::unit(bottom_y_pts, "npc"),
    gp = grid::gpar(col = border_col, lwd = border_width)
  )

  # Top ellipse (full)
  top_y <- y + cyl_height / 2
  top_x <- x + cyl_width * cos(angles_full)
  top_y_pts <- top_y + ellipse_h * sin(angles_full)

  grobs[[3]] <- grid::polygonGrob(
    x = grid::unit(top_x, "npc"),
    y = grid::unit(top_y_pts, "npc"),
    gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
  )

  # Side lines
  grobs[[4]] <- grid::segmentsGrob(
    x0 = grid::unit(x - cyl_width, "npc"),
    y0 = grid::unit(bottom_y, "npc"),
    x1 = grid::unit(x - cyl_width, "npc"),
    y1 = grid::unit(y + cyl_height / 2, "npc"),
    gp = grid::gpar(col = border_col, lwd = border_width)
  )

  grobs[[5]] <- grid::segmentsGrob(
    x0 = grid::unit(x + cyl_width, "npc"),
    y0 = grid::unit(bottom_y, "npc"),
    x1 = grid::unit(x + cyl_width, "npc"),
    y1 = grid::unit(y + cyl_height / 2, "npc"),
    gp = grid::gpar(col = border_col, lwd = border_width)
  )

  do.call(grid::gList, grobs)
}

#' Draw Cloud Node
#'
#' Cloud shape (cloud computing).
#'
#' @keywords internal
draw_cloud <- function(x, y, size, fill, border_color, border_width,
                       alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Cloud made of overlapping circles
  n_pts <- 100
  t <- seq(0, 2 * pi, length.out = n_pts)

  # Bumpy cloud shape
  r <- size * (0.65 + 0.2 * sin(4 * t) + 0.1 * sin(6 * t))
  xs <- x + r * cos(t)
  ys <- y + r * sin(t) * 0.6 + size * 0.1  # Flattened and raised

  grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
  )
}

#' Draw Gear Node
#'
#' Gear/cog shape (processing/automation).
#'
#' @param n_teeth Number of gear teeth.
#' @keywords internal
draw_gear <- function(x, y, size, fill, border_color, border_width,
                      alpha = 1, n_teeth = 8, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  grobs <- list()

  # Gear parameters
  outer_r <- size
  inner_r <- size * 0.65
  tooth_height <- size * 0.25
  center_r <- size * 0.25

  # Generate gear teeth
  n_pts_per_tooth <- 8
  n_total <- n_teeth * n_pts_per_tooth
  angles <- seq(0, 2 * pi, length.out = n_total + 1)[-1]

  gear_x <- numeric(n_total)
  gear_y <- numeric(n_total)

  for (i in seq_len(n_total)) {
    tooth_idx <- (i - 1) %/% n_pts_per_tooth
    pos_in_tooth <- (i - 1) %% n_pts_per_tooth

    if (pos_in_tooth < 2 || pos_in_tooth >= 6) {
      # Valley
      r <- inner_r
    } else {
      # Tooth
      r <- inner_r + tooth_height
    }

    gear_x[i] <- x + r * cos(angles[i])
    gear_y[i] <- y + r * sin(angles[i])
  }

  # Main gear body
  grobs[[1]] <- grid::polygonGrob(
    x = grid::unit(gear_x, "npc"),
    y = grid::unit(gear_y, "npc"),
    gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
  )

  # Center hole
  grobs[[2]] <- grid::circleGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    r = grid::unit(center_r, "npc"),
    gp = grid::gpar(fill = "white", col = border_col, lwd = border_width * 0.7)
  )

  do.call(grid::gList, grobs)
}

#' Draw Cross/Plus Node
#' @keywords internal
draw_cross <- function(x, y, size, fill, border_color, border_width,
                       alpha = 1, thickness = 0.3, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Cross shape
  t <- size * thickness  # Half thickness
  s <- size  # Half size

  # Horizontal bar
  xs1 <- c(x - s, x + s, x + s, x - s)
  ys1 <- c(y - t, y - t, y + t, y + t)

  # Vertical bar
  xs2 <- c(x - t, x + t, x + t, x - t)
  ys2 <- c(y - s, y - s, y + s, y + s)

  grid::gList(
    grid::polygonGrob(
      x = grid::unit(xs1, "npc"),
      y = grid::unit(ys1, "npc"),
      gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
    ),
    grid::polygonGrob(
      x = grid::unit(xs2, "npc"),
      y = grid::unit(ys2, "npc"),
      gp = grid::gpar(fill = fill_col, col = border_col, lwd = border_width)
    )
  )
}
