#' @title Base R Node Rendering
#' @description Node drawing functions for splot() using base R graphics.
#' @name splot-nodes
#' @keywords internal
NULL

#' Draw a Single Node
#'
#' Renders a node at the specified position with given aesthetics.
#'
#' @param x,y Node center coordinates.
#' @param size Node radius in user coordinates.
#' @param size2 Secondary size (for ellipse height).
#' @param shape Node shape name.
#' @param col Fill color.
#' @param border.col Border color.
#' @param border.width Border line width.
#' @param ... Additional parameters.
#' @keywords internal
draw_node_base <- function(x, y, size, size2 = NULL, shape = "circle",
                           col = "#4A90D9", border.col = "#2C5AA0",
                           border.width = 1, ...) {
  if (is.null(size2)) size2 <- size

  if (shape == "circle") {
    # Use symbols() for perfect circles
    graphics::symbols(
      x = x, y = y,
      circles = size,
      inches = FALSE,
      add = TRUE,
      fg = border.col,
      bg = col,
      lwd = border.width
    )

  } else if (shape == "square") {
    # Square using rect()
    graphics::rect(
      xleft = x - size,
      ybottom = y - size,
      xright = x + size,
      ytop = y + size,
      col = col,
      border = border.col,
      lwd = border.width
    )

  } else if (shape == "rectangle" || shape == "ellipse") {
    # Use polygon for ellipse/rectangle
    verts <- get_shape_vertices(shape, x, y, size, size2)
    graphics::polygon(
      x = verts$x,
      y = verts$y,
      col = col,
      border = border.col,
      lwd = border.width
    )

  } else if (shape == "neural") {
    draw_neural_node_base(x, y, size, col, border.col, border.width, ...)

  } else if (shape == "chip") {
    draw_chip_node_base(x, y, size, col, border.col, border.width, ...)

  } else if (shape == "robot") {
    draw_robot_node_base(x, y, size, col, border.col, border.width)

  } else if (shape == "network") {
    draw_network_node_base(x, y, size, col, border.col, border.width)

  } else if (shape == "database") {
    draw_database_node_base(x, y, size, col, border.col, border.width)

  } else if (startsWith(shape, "_") && !is.null(get_svg_shape(shape))) {
    # Custom SVG shape
    svg_data <- get_svg_shape(shape)
    draw_svg_shape_base(x, y, size, svg_data, col, border.col, border.width)

  } else {
    # All other shapes via polygon (including gear, cloud, brain)
    verts <- get_shape_vertices(shape, x, y, size, size2, ...)
    graphics::polygon(
      x = verts$x,
      y = verts$y,
      col = col,
      border = border.col,
      lwd = border.width
    )
  }
}

#' Draw Pie Chart Node
#'
#' Renders a node as a pie chart with multiple colored segments.
#' The pie is drawn slightly inside the node boundary to leave room for arrows.
#'
#' @param x,y Node center coordinates.
#' @param size Node radius.
#' @param values Numeric vector of values (will be normalized to proportions).
#' @param colors Vector of colors for each segment.
#' @param default_color Fallback color when colors is NULL and values length is 1.
#' @param border.col Border color.
#' @param border.width Border line width.
#' @param pie_border.width Border width for pie slice dividers (NULL = use border.width).
#' @keywords internal
draw_pie_node_base <- function(x, y, size, values, colors = NULL,
                               default_color = NULL,
                               border.col = "black", border.width = 1,
                               pie_border.width = NULL) {
  if (is.null(values) || length(values) == 0) {
    return(invisible())
  }

  # Draw outer boundary circle first (arrows will touch this)
  angles <- seq(0, 2 * pi, length.out = 100)
  graphics::polygon(
    x = x + size * cos(angles),
    y = y + size * sin(angles),
    col = "white",
    border = border.col,
    lwd = border.width
 )

  # Pie is drawn inside the boundary
  pie_size <- size * 0.92

  # Normalize to proportions
  props <- values / sum(values)
  n <- length(props)

  # Default colors - use default_color if provided and single segment
  if (is.null(colors)) {
    if (!is.null(default_color) && n == 1) {
      colors <- default_color
    } else {
      colors <- grDevices::rainbow(n, s = 0.7, v = 0.9)
    }
  }
  colors <- recycle_to_length(colors, n)

  # Use separate pie border width if provided
  slice_border_width <- if (!is.null(pie_border.width)) pie_border.width else border.width

  # Draw slices
  start_angle <- pi / 2  # Start at top
  n_points <- 50

  for (i in seq_len(n)) {
    if (props[i] <= 0) next

    end_angle <- start_angle - 2 * pi * props[i]

    # Create slice polygon
    angles <- seq(start_angle, end_angle, length.out = max(3, ceiling(n_points * props[i])))

    xs <- c(x, x + pie_size * cos(angles), x)
    ys <- c(y, y + pie_size * sin(angles), y)

    graphics::polygon(
      x = xs, y = ys,
      col = colors[i],
      border = NA
    )

    start_angle <- end_angle
  }

  # Draw slice dividers (if more than one slice)
  if (n > 1) {
    start_angle <- pi / 2
    for (i in seq_len(n)) {
      if (props[i] <= 0) next
      end_angle <- start_angle - 2 * pi * props[i]
      # Draw radial line at slice boundary
      graphics::lines(
        x = c(x, x + pie_size * cos(start_angle)),
        y = c(y, y + pie_size * sin(start_angle)),
        col = border.col,
        lwd = slice_border_width
      )
      start_angle <- end_angle
    }
  }
}

#' Draw Polygon Donut Node (Base R)
#'
#' Renders a donut on a polygon shape where segments follow polygon edges.
#' The donut shows a fill proportion (0-1) as filled segments starting from the top.
#'
#' @param x,y Node center coordinates.
#' @param size Outer radius.
#' @param values Single numeric value (0-1) specifying fill proportion.
#'   0.1 = 10% filled, 0.5 = 50% filled, 1.0 = full ring.
#' @param colors Fill color for the donut ring.
#' @param default_color Fallback color when colors is NULL.
#' @param inner_ratio Ratio of inner to outer radius (0-1). Default 0.5.
#' @param bg_color Background color for unfilled portion. Default "gray90".
#' @param donut_shape Base polygon shape: "square", "hexagon", "triangle", etc.
#' @param border.col Border color.
#' @param border.width Border line width.
#' @param donut_border.width Border width for donut ring (NULL = use border.width).
#' @param show_value Logical: show value in center? Default FALSE.
#' @param value_cex Text size for center value.
#' @param value_col Text color for center value.
#' @param value_fontface Font face for center value.
#' @param value_fontfamily Font family for center value.
#' @param value_digits Decimal places for value display.
#' @param value_prefix Text before value (e.g., "$").
#' @param value_suffix Text after value (e.g., "%").
#' @keywords internal
draw_polygon_donut_node_base <- function(x, y, size, values, colors = NULL,
                                         default_color = NULL,
                                         inner_ratio = 0.5, bg_color = "gray90",
                                         center_color = "white",
                                         donut_shape = "square",
                                         border.col = "black", border.width = 1,
                                         donut_border.width = NULL,
                                         outer_border.col = NULL,
                                         border.lty = 1,
                                         show_value = TRUE, value_cex = 0.8,
                                         value_col = "black",
                                         value_fontface = "bold", value_fontfamily = "sans",
                                         value_digits = 2, value_prefix = "",
                                         value_suffix = "") {
  ring_border_width <- if (!is.null(donut_border.width)) donut_border.width else border.width

  # Get outer polygon vertices
  outer <- get_donut_base_vertices(donut_shape, x, y, size)

  # Get inner polygon vertices
  inner <- inset_polygon_vertices(outer, inner_ratio)

  n_verts <- length(outer$x)
  center_value <- NULL

  # Helper to draw a ring segment
  draw_ring_segment <- function(idx_start, idx_end, segment_col) {
    seg_x <- c(outer$x[idx_start], outer$x[idx_end], inner$x[idx_end], inner$x[idx_start])
    seg_y <- c(outer$y[idx_start], outer$y[idx_end], inner$y[idx_end], inner$y[idx_start])
    graphics::polygon(seg_x, seg_y, col = segment_col, border = NA)
  }

  if (is.null(values) || length(values) == 0) {
    values <- 1
    if (is.null(colors)) colors <- if (!is.null(default_color)) default_color else "#4A90D9"
  }

  if (length(values) == 1) {
    # Progress donut
    prop <- max(0, min(1, values))
    center_value <- prop

    # Draw background ring
    for (i in seq_len(n_verts)) {
      i_next <- if (i == n_verts) 1 else i + 1
      draw_ring_segment(i, i_next, bg_color)
    }

    # Draw filled portion
    if (prop > 0) {
      segment_col <- if (!is.null(colors)) colors[1] else "maroon"
      filled_verts <- max(1, round(prop * n_verts))

      for (i in seq_len(filled_verts)) {
        i_next <- if (i == n_verts) 1 else i + 1
        draw_ring_segment(i, i_next, segment_col)
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

      for (j in seq_len(seg_verts)) {
        if (vert_idx > n_verts) break
        i_next <- if (vert_idx == n_verts) 1 else vert_idx + 1
        draw_ring_segment(vert_idx, i_next, colors[seg])
        vert_idx <- vert_idx + 1
      }
    }
  }

  # Outer boundary border (double border feature)
  if (!is.null(outer_border.col)) {
    # Scale up the outer polygon slightly for the double border
    outer_boundary <- list(
      x = x + (outer$x - x) / 0.92,  # Match the 0.92 scaling factor from circular donut
      y = y + (outer$y - y) / 0.92
    )
    graphics::polygon(outer_boundary$x, outer_boundary$y, col = NA,
                      border = outer_border.col, lwd = border.width, lty = border.lty)
  }

  # Outer border
  graphics::polygon(outer$x, outer$y, col = NA, border = border.col,
                    lwd = ring_border_width, lty = border.lty)

  # Inner border and fill (center of donut)
  graphics::polygon(inner$x, inner$y, col = center_color, border = border.col,
                    lwd = ring_border_width, lty = border.lty)

  # Show value in center
  if (show_value && !is.null(center_value)) {
    formatted_value <- round(center_value, value_digits)
    label_text <- paste0(value_prefix, formatted_value, value_suffix)

    fontface_num <- switch(value_fontface,
      "plain" = 1, "bold" = 2, "italic" = 3, "bold.italic" = 4, 2
    )

    graphics::text(
      x = x, y = y,
      labels = label_text,
      cex = value_cex,
      col = value_col,
      font = fontface_num,
      family = value_fontfamily
    )
  }
}

#' Draw Donut Chart Node
#'
#' Renders a node as a donut chart with an inner hole.
#' The donut shows a fill proportion (0-1) as an arc starting from 12 o'clock.
#'
#' @param x,y Node center coordinates.
#' @param size Outer radius.
#' @param values Single numeric value (0-1) specifying fill proportion.
#'   0.1 = 10% filled arc, 0.5 = 50% filled, 1.0 = full ring.
#' @param colors Fill color for the donut ring.
#' @param default_color Fallback color when colors is NULL.
#' @param inner_ratio Ratio of inner to outer radius (0-1). Default 0.5.
#' @param bg_color Background color for unfilled portion. Default "gray90".
#' @param border.col Border color.
#' @param border.width Border line width.
#' @param donut_border.width Border width for donut ring (NULL = use border.width).
#' @param show_value Logical: show value in center? Default FALSE.
#' @param value_cex Text size for center value.
#' @param value_col Text color for center value.
#' @param value_fontface Font face for center value ("plain", "bold", "italic", "bold.italic").
#' @param value_fontfamily Font family for center value ("sans", "serif", "mono").
#' @param value_digits Decimal places for value display.
#' @param value_prefix Text before value (e.g., "$").
#' @param value_suffix Text after value (e.g., "%").
#' @keywords internal
draw_donut_node_base <- function(x, y, size, values, colors = NULL,
                                 default_color = NULL,
                                 inner_ratio = 0.5, bg_color = "gray90",
                                 center_color = "white",
                                 border.col = "black", border.width = 1,
                                 donut_border.width = NULL,
                                 outer_border.col = NULL,
                                 border.lty = 1,
                                 show_value = TRUE, value_cex = 0.8,
                                 value_col = "black",
                                 value_fontface = "bold", value_fontfamily = "sans",
                                 value_digits = 2, value_prefix = "",
                                 value_suffix = "") {
  # Use separate donut border width if provided
  ring_border_width <- if (!is.null(donut_border.width)) donut_border.width else border.width

  n_points <- 100

  # Draw outer boundary circle first (arrows will touch this)
  angles <- seq(0, 2 * pi, length.out = n_points)
  graphics::polygon(
    x = x + size * cos(angles),
    y = y + size * sin(angles),
    col = "white",
    border = NA,
    lwd = border.width
  )

  # Donut content is drawn inside the boundary
  content_size <- size * 0.92
  outer_r <- content_size
  inner_r <- content_size * inner_ratio

  # Helper to draw ring segment
  draw_ring_segment <- function(start_ang, end_ang, outer_r, inner_r, col) {
    n_pts <- max(10, ceiling(abs(end_ang - start_ang) / (2 * pi) * n_points))
    angles <- seq(start_ang, end_ang, length.out = n_pts)

    # Outer arc
    outer_x <- x + outer_r * cos(angles)
    outer_y <- y + outer_r * sin(angles)

    # Inner arc (reversed)
    inner_x <- x + inner_r * cos(rev(angles))
    inner_y <- y + inner_r * sin(rev(angles))

    graphics::polygon(
      x = c(outer_x, inner_x),
      y = c(outer_y, inner_y),
      col = col,
      border = NA
    )
  }

  center_value <- NULL

  if (length(values) == 1) {
    # Single value: progress donut
    prop <- max(0, min(1, values))
    center_value <- prop

    # Draw background ring
    draw_ring_segment(0, 2 * pi, outer_r, inner_r, bg_color)

    # Draw filled portion
    if (prop > 0) {
      start_ang <- pi / 2
      end_ang <- pi / 2 - 2 * pi * prop
      fill_col <- if (!is.null(colors)) colors[1] else if (!is.null(default_color)) default_color else "maroon"
      draw_ring_segment(start_ang, end_ang, outer_r, inner_r, fill_col)
    }

  } else {
    # Multiple values: segmented donut
    props <- values / sum(values)
    n <- length(props)

    if (is.null(colors)) {
      if (!is.null(default_color) && n == 1) {
        colors <- default_color
      } else {
        colors <- grDevices::rainbow(n, s = 0.7, v = 0.9)
      }
    }
    colors <- recycle_to_length(colors, n)

    start_ang <- pi / 2
    for (i in seq_len(n)) {
      if (props[i] <= 0) next

      end_ang <- start_ang - 2 * pi * props[i]
      draw_ring_segment(start_ang, end_ang, outer_r, inner_r, colors[i])
      start_ang <- end_ang
    }
  }

  # Fill inner hole (center of donut)
  angles <- seq(0, 2 * pi, length.out = n_points)
  graphics::polygon(
    x = x + inner_r * cos(angles),
    y = y + inner_r * sin(angles),
    col = center_color,
    border = NA
  )

  # Draw borders
  # Outer boundary border (double border feature - at full node size)
  if (!is.null(outer_border.col)) {
    graphics::lines(
      x = x + size * cos(seq(0, 2*pi, length.out = n_points)),
      y = y + size * sin(seq(0, 2*pi, length.out = n_points)),
      col = outer_border.col,
      lwd = border.width,
      lty = border.lty
    )
  }
  # Donut ring outer border (at content_size)
  graphics::lines(
    x = x + outer_r * cos(seq(0, 2*pi, length.out = n_points)),
    y = y + outer_r * sin(seq(0, 2*pi, length.out = n_points)),
    col = border.col,
    lwd = ring_border_width,
    lty = border.lty
  )
  # Donut ring inner border (inner hole)
  graphics::lines(
    x = x + inner_r * cos(seq(0, 2*pi, length.out = n_points)),
    y = y + inner_r * sin(seq(0, 2*pi, length.out = n_points)),
    col = border.col,
    lwd = ring_border_width,
    lty = border.lty
  )

  # Show value in center
  if (show_value && !is.null(center_value)) {
    # Format the value
    formatted_value <- round(center_value, value_digits)
    label_text <- paste0(value_prefix, formatted_value, value_suffix)

    # Convert fontface string to numeric
    fontface_num <- switch(value_fontface,
      "plain" = 1,
      "bold" = 2,
      "italic" = 3,
      "bold.italic" = 4,
      2  # default to bold
    )

    graphics::text(
      x = x, y = y,
      labels = label_text,
      cex = value_cex,
      col = value_col,
      font = fontface_num,
      family = value_fontfamily
    )
  }
}

#' Draw Donut with Inner Pie
#'
#' Renders a node with outer donut ring and inner pie chart.
#'
#' @param x,y Node center coordinates.
#' @param size Outer radius.
#' @param donut_value Single value (0-1) for donut progress.
#' @param donut_color Fill color for donut ring.
#' @param pie_values Numeric vector for pie segments.
#' @param pie_colors Vector of colors for pie segments.
#' @param pie_default_color Default color for pie when pie_colors is NULL.
#' @param inner_ratio Ratio of inner to outer radius.
#' @param bg_color Background color.
#' @param border.col Border color.
#' @param border.width Border line width.
#' @param pie_border.width Border width for pie slice dividers (NULL = use border.width * 0.5).
#' @param donut_border.width Border width for donut ring (NULL = use border.width).
#' @keywords internal
draw_donut_pie_node_base <- function(x, y, size, donut_value = 1,
                                     donut_color = "#4A90D9",
                                     pie_values = NULL, pie_colors = NULL,
                                     pie_default_color = NULL,
                                     inner_ratio = 0.5, bg_color = "gray90",
                                     border.col = "black", border.width = 1,
                                     pie_border.width = NULL,
                                     donut_border.width = NULL) {
  # Use separate border widths if provided
  ring_border_width <- if (!is.null(donut_border.width)) donut_border.width else border.width
  pie_slice_border <- if (!is.null(pie_border.width)) pie_border.width else border.width * 0.5
  n_points <- 100

  # Draw outer boundary circle first (arrows will touch this)
  angles <- seq(0, 2 * pi, length.out = n_points)
  graphics::polygon(
    x = x + size * cos(angles),
    y = y + size * sin(angles),
    col = "white",
    border = border.col,
    lwd = border.width
  )

  # Content is drawn inside the boundary
  content_size <- size * 0.92
  outer_r <- content_size
  inner_r <- content_size * inner_ratio

  # Helper to draw ring segment
  draw_ring_segment <- function(start_ang, end_ang, outer_r, inner_r, col) {
    n_pts <- max(10, ceiling(abs(end_ang - start_ang) / (2 * pi) * n_points))
    angles <- seq(start_ang, end_ang, length.out = n_pts)

    outer_x <- x + outer_r * cos(angles)
    outer_y <- y + outer_r * sin(angles)
    inner_x <- x + inner_r * cos(rev(angles))
    inner_y <- y + inner_r * sin(rev(angles))

    graphics::polygon(
      x = c(outer_x, inner_x),
      y = c(outer_y, inner_y),
      col = col,
      border = NA
    )
  }

  # Draw donut ring background
  draw_ring_segment(0, 2 * pi, outer_r, inner_r, bg_color)

  # Draw donut filled portion
  donut_prop <- max(0, min(1, donut_value))
  if (donut_prop > 0) {
    start_ang <- pi / 2
    end_ang <- pi / 2 - 2 * pi * donut_prop
    draw_ring_segment(start_ang, end_ang, outer_r, inner_r, donut_color)
  }

  # Draw inner pie
  pie_r <- inner_r * 0.95

  if (!is.null(pie_values) && length(pie_values) > 0) {
    props <- pie_values / sum(pie_values)
    n <- length(props)

    if (is.null(pie_colors)) {
      if (!is.null(pie_default_color) && n == 1) {
        pie_colors <- pie_default_color
      } else {
        pie_colors <- grDevices::rainbow(n, s = 0.7, v = 0.9)
      }
    }
    pie_colors <- recycle_to_length(pie_colors, n)

    start_ang <- pi / 2
    for (i in seq_len(n)) {
      if (props[i] <= 0) next

      end_ang <- start_ang - 2 * pi * props[i]
      n_pts <- max(3, ceiling(50 * props[i]))
      angles <- seq(start_ang, end_ang, length.out = n_pts)

      xs <- c(x, x + pie_r * cos(angles), x)
      ys <- c(y, y + pie_r * sin(angles), y)

      graphics::polygon(x = xs, y = ys, col = pie_colors[i], border = NA)
      start_ang <- end_ang
    }

    # Draw pie slice dividers (if more than one slice)
    if (n > 1) {
      start_ang <- pi / 2
      for (i in seq_len(n)) {
        if (props[i] <= 0) next
        end_ang <- start_ang - 2 * pi * props[i]
        graphics::lines(
          x = c(x, x + pie_r * cos(start_ang)),
          y = c(y, y + pie_r * sin(start_ang)),
          col = border.col,
          lwd = pie_slice_border
        )
        start_ang <- end_ang
      }
    }
  } else {
    # Fill inner with white
    angles <- seq(0, 2 * pi, length.out = n_points)
    graphics::polygon(
      x = x + pie_r * cos(angles),
      y = y + pie_r * sin(angles),
      col = "white",
      border = NA
    )
  }

  # Draw borders
  angles <- seq(0, 2 * pi, length.out = n_points)
  graphics::lines(x = x + outer_r * cos(angles), y = y + outer_r * sin(angles),
                  col = border.col, lwd = ring_border_width)
  graphics::lines(x = x + inner_r * cos(angles), y = y + inner_r * sin(angles),
                  col = border.col, lwd = ring_border_width)
}

#' Draw Double Donut with Inner Pie
#'
#' Renders a node with two concentric donut rings and an optional inner pie chart.
#' From outside to inside: outer donut ring, inner donut ring, center pie.
#'
#' @param x,y Node center coordinates.
#' @param size Outer radius.
#' @param donut_values Values for outer donut ring (vector for segments, or single 0-1 for progress).
#' @param donut_colors Colors for outer donut segments.
#' @param donut2_values Values for inner donut ring (vector for segments, or single 0-1 for progress).
#' @param donut2_colors Colors for inner donut segments.
#' @param pie_values Numeric vector for center pie segments.
#' @param pie_colors Vector of colors for pie segments.
#' @param pie_default_color Default color for pie when pie_colors is NULL.
#' @param outer_inner_ratio Where outer donut ends (inner radius as ratio of outer radius). Default 0.7.
#' @param inner_inner_ratio Where inner donut ends (inner radius as ratio of outer radius). Default 0.4.
#' @param bg_color Background color for unfilled portions.
#' @param border.col Border color.
#' @param border.width Border line width.
#' @param pie_border.width Border width for pie slice dividers.
#' @param donut_border.width Border width for donut rings.
#' @keywords internal
draw_double_donut_pie_node_base <- function(x, y, size,
                                            donut_values = NULL, donut_colors = NULL,
                                            donut2_values = NULL, donut2_colors = NULL,
                                            pie_values = NULL, pie_colors = NULL,
                                            pie_default_color = NULL,
                                            outer_inner_ratio = 0.7,
                                            inner_inner_ratio = 0.4,
                                            bg_color = "gray90",
                                            border.col = "black", border.width = 1,
                                            pie_border.width = NULL,
                                            donut_border.width = NULL) {
  # Use separate border widths if provided
  ring_border_width <- if (!is.null(donut_border.width)) donut_border.width else border.width
  pie_slice_border <- if (!is.null(pie_border.width)) pie_border.width else border.width * 0.5
  n_points <- 100

  # Draw outer boundary circle first (arrows will touch this)
  angles <- seq(0, 2 * pi, length.out = n_points)
  graphics::polygon(
    x = x + size * cos(angles),
    y = y + size * sin(angles),
    col = "white",
    border = border.col,
    lwd = border.width
  )

  # Content is drawn inside the boundary
  content_size <- size * 0.92

  # Define radii for the three layers (scaled down)
  outer_r <- content_size                        # Outermost edge of content
  mid_r <- content_size * outer_inner_ratio      # Between outer and inner donut
  inner_r <- content_size * inner_inner_ratio    # Inner edge of inner donut / outer edge of pie

  # Helper to draw ring segment
  draw_ring_segment <- function(start_ang, end_ang, r_outer, r_inner, col) {
    n_pts <- max(10, ceiling(abs(end_ang - start_ang) / (2 * pi) * n_points))
    angles <- seq(start_ang, end_ang, length.out = n_pts)

    outer_x <- x + r_outer * cos(angles)
    outer_y <- y + r_outer * sin(angles)
    inner_x <- x + r_inner * cos(rev(angles))
    inner_y <- y + r_inner * sin(rev(angles))

    graphics::polygon(
      x = c(outer_x, inner_x),
      y = c(outer_y, inner_y),
      col = col,
      border = NA
    )
  }

  # Helper to draw donut ring (handles both progress and segmented)
  draw_donut_ring <- function(values, colors, default_color, r_outer, r_inner) {
    if (is.null(values)) return()

    if (length(values) == 1) {
      # Progress donut - draw background then filled portion
      draw_ring_segment(0, 2 * pi, r_outer, r_inner, bg_color)
      prop <- max(0, min(1, values))
      if (prop > 0) {
        fill_col <- if (!is.null(colors)) colors[1] else if (!is.null(default_color)) default_color else "#4A90D9"
        start_ang <- pi / 2
        end_ang <- pi / 2 - 2 * pi * prop
        draw_ring_segment(start_ang, end_ang, r_outer, r_inner, fill_col)
      }
    } else {
      # Segmented donut
      props <- values / sum(values)
      n <- length(props)

      if (is.null(colors)) {
        colors <- grDevices::rainbow(n, s = 0.7, v = 0.9)
      }
      colors <- recycle_to_length(colors, n)

      start_ang <- pi / 2
      for (i in seq_len(n)) {
        if (props[i] <= 0) next
        end_ang <- start_ang - 2 * pi * props[i]
        draw_ring_segment(start_ang, end_ang, r_outer, r_inner, colors[i])
        start_ang <- end_ang
      }
    }
  }

  # 1. Draw outer donut ring (if values provided)
  if (!is.null(donut_values)) {
    draw_donut_ring(donut_values, donut_colors, NULL, outer_r, mid_r)
  } else {
    # Fill with background if no outer donut
    draw_ring_segment(0, 2 * pi, outer_r, mid_r, bg_color)
  }

  # 2. Draw inner donut ring (if values provided)
  if (!is.null(donut2_values)) {
    draw_donut_ring(donut2_values, donut2_colors, NULL, mid_r, inner_r)
  } else {
    # Fill with background if no inner donut
    draw_ring_segment(0, 2 * pi, mid_r, inner_r, bg_color)
  }

  # 3. Draw center pie (if values provided)
  pie_r <- inner_r * 0.95
  if (!is.null(pie_values) && length(pie_values) > 0) {
    props <- pie_values / sum(pie_values)
    n <- length(props)

    if (is.null(pie_colors)) {
      if (!is.null(pie_default_color) && n == 1) {
        pie_colors <- pie_default_color
      } else {
        pie_colors <- grDevices::rainbow(n, s = 0.7, v = 0.9)
      }
    }
    pie_colors <- recycle_to_length(pie_colors, n)

    start_ang <- pi / 2
    for (i in seq_len(n)) {
      if (props[i] <= 0) next

      end_ang <- start_ang - 2 * pi * props[i]
      n_pts <- max(3, ceiling(50 * props[i]))
      angles <- seq(start_ang, end_ang, length.out = n_pts)

      xs <- c(x, x + pie_r * cos(angles), x)
      ys <- c(y, y + pie_r * sin(angles), y)

      graphics::polygon(x = xs, y = ys, col = pie_colors[i], border = NA)
      start_ang <- end_ang
    }

    # Draw pie slice dividers (if more than one slice)
    if (n > 1) {
      start_ang <- pi / 2
      for (i in seq_len(n)) {
        if (props[i] <= 0) next
        end_ang <- start_ang - 2 * pi * props[i]
        graphics::lines(
          x = c(x, x + pie_r * cos(start_ang)),
          y = c(y, y + pie_r * sin(start_ang)),
          col = border.col,
          lwd = pie_slice_border
        )
        start_ang <- end_ang
      }
    }
  } else {
    # Fill center with white
    angles <- seq(0, 2 * pi, length.out = n_points)
    graphics::polygon(
      x = x + pie_r * cos(angles),
      y = y + pie_r * sin(angles),
      col = "white",
      border = NA
    )
  }

  # 4. Draw all borders
  angles <- seq(0, 2 * pi, length.out = n_points)
  # Outer border
  graphics::lines(x = x + outer_r * cos(angles), y = y + outer_r * sin(angles),
                  col = border.col, lwd = ring_border_width)
  # Middle border (between outer and inner donut)
  graphics::lines(x = x + mid_r * cos(angles), y = y + mid_r * sin(angles),
                  col = border.col, lwd = ring_border_width)
  # Inner border (between inner donut and pie)
  graphics::lines(x = x + inner_r * cos(angles), y = y + inner_r * sin(angles),
                  col = border.col, lwd = ring_border_width)
}

#' Draw Node Label
#'
#' Renders a text label at or near a node.
#'
#' @param x,y Label position coordinates.
#' @param label Text to display.
#' @param cex Character expansion factor.
#' @param col Text color.
#' @param font Font face (1=plain, 2=bold, 3=italic, 4=bold italic).
#' @param family Font family ("sans", "serif", "mono").
#' @param hjust Horizontal justification (0=left, 0.5=center, 1=right).
#' @param vjust Vertical justification (0=bottom, 0.5=center, 1=top).
#' @param srt String rotation angle in degrees.
#' @param pos Position relative to point (NULL=centered, 1=below, 2=left, 3=above, 4=right).
#' @param offset Offset distance when using pos.
#' @keywords internal
draw_node_label_base <- function(x, y, label, cex = 1, col = "black",
                                 font = 1, family = "sans",
                                 hjust = 0.5, vjust = 0.5, srt = 0,
                                 pos = NULL, offset = 0.5) {
  if (is.null(label) || is.na(label) || label == "") {
    return(invisible())
  }

  # Convert hjust/vjust to adj parameter
  adj <- c(hjust, vjust)

  graphics::text(
    x = x, y = y,
    labels = label,
    cex = cex,
    col = col,
    font = font,
    family = family,
    adj = adj,
    srt = srt,
    pos = pos,
    offset = offset
  )
}

#' Draw Neural Node (Base R)
#'
#' Circle with small connection circles around perimeter.
#'
#' @keywords internal
draw_neural_node_base <- function(x, y, size, col, border.col, border.width,
                                  n_connections = 6) {
  # Main center circle
  graphics::symbols(
    x = x, y = y,
    circles = size * 0.6,
    inches = FALSE, add = TRUE,
    fg = border.col, bg = col, lwd = border.width
  )

  # Connection circles around perimeter
  conn_radius <- size * 0.15
  angles <- seq(0, 2 * pi * (1 - 1/n_connections), length.out = n_connections)

  for (i in seq_along(angles)) {
    cx <- x + size * 0.85 * cos(angles[i])
    cy <- y + size * 0.85 * sin(angles[i])

    # Line from center to connection
    graphics::lines(c(x, cx), c(y, cy), col = border.col, lwd = border.width * 0.5)

    # Connection circle
    graphics::symbols(
      x = cx, y = cy,
      circles = conn_radius,
      inches = FALSE, add = TRUE,
      fg = border.col, bg = col, lwd = border.width * 0.7
    )
  }
}

#' Draw Chip Node (Base R)
#'
#' Square with pins extending from all edges.
#'
#' @keywords internal
draw_chip_node_base <- function(x, y, size, col, border.col, border.width,
                                pins_per_side = 3) {
  body_size <- size * 0.7
  pin_length <- size * 0.2
  pin_width <- body_size * 0.8 / (pins_per_side * 2 - 1)

  # Main body (square with corner notch)
  notch_size <- body_size * 0.15
  xs <- c(x - body_size, x - body_size + notch_size, x + body_size, x + body_size, x - body_size)
  ys <- c(y - body_size, y + body_size, y + body_size, y - body_size, y - body_size)
  graphics::polygon(xs, ys, col = col, border = border.col, lwd = border.width)

  # Draw pins
  for (side in c("top", "bottom", "left", "right")) {
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
      } else {
        px <- x + body_size
        py <- y + offset
        p_xs <- c(px, px, px + pin_length, px + pin_length)
        p_ys <- c(py - pin_width/2, py + pin_width/2, py + pin_width/2, py - pin_width/2)
      }

      graphics::polygon(p_xs, p_ys, col = border.col, border = border.col)
    }
  }
}

#' Draw Robot Node (Base R)
#'
#' Rounded square with antenna and eyes.
#'
#' @keywords internal
draw_robot_node_base <- function(x, y, size, col, border.col, border.width) {
  head_w <- size * 0.8
  head_h <- size * 0.7

  # Robot head (rectangle with rounded-ish appearance)
  graphics::rect(
    xleft = x - head_w, ybottom = y - head_h - size * 0.1,
    xright = x + head_w, ytop = y + head_h - size * 0.1,
    col = col, border = border.col, lwd = border.width
  )

  # Antenna
  antenna_base_y <- y + head_h - size * 0.1
  graphics::lines(c(x, x), c(antenna_base_y, y + size), col = border.col, lwd = border.width)
  graphics::symbols(
    x = x, y = y + size + size * 0.08,
    circles = size * 0.08,
    inches = FALSE, add = TRUE, fg = border.col, bg = border.col
  )

  # Eyes
  eye_y <- y
  eye_radius <- size * 0.12
  graphics::symbols(
    x = c(x - head_w * 0.4, x + head_w * 0.4),
    y = c(eye_y, eye_y),
    circles = rep(eye_radius, 2),
    inches = FALSE, add = TRUE,
    fg = border.col, bg = "white", lwd = border.width * 0.7
  )

  # Mouth
  graphics::lines(
    c(x - head_w * 0.3, x + head_w * 0.3),
    c(y - head_h * 0.4, y - head_h * 0.4),
    col = border.col, lwd = border.width
  )
}

#' Draw Network Node (Base R)
#'
#' Interconnected nodes pattern.
#'
#' @keywords internal
draw_network_node_base <- function(x, y, size, col, border.col, border.width) {
  # Outer boundary
  graphics::symbols(
    x = x, y = y,
    circles = size,
    inches = FALSE, add = TRUE,
    fg = border.col, bg = col, lwd = border.width
  )

  # Mini nodes (pentagon arrangement)
  n_nodes <- 5
  inner_r <- size * 0.55
  node_r <- size * 0.12
  angles <- seq(pi/2, pi/2 + 2 * pi * (1 - 1/n_nodes), length.out = n_nodes)

  node_x <- x + inner_r * cos(angles)
  node_y <- y + inner_r * sin(angles)

  # Edges
  for (i in seq_len(n_nodes)) {
    for (j in seq_len(n_nodes)) {
      if (i < j) {
        graphics::lines(
          c(node_x[i], node_x[j]), c(node_y[i], node_y[j]),
          col = border.col, lwd = border.width * 0.5
        )
      }
    }
  }

  # Nodes
  graphics::symbols(
    x = node_x, y = node_y,
    circles = rep(node_r, n_nodes),
    inches = FALSE, add = TRUE,
    fg = border.col, bg = "white", lwd = border.width * 0.7
  )
}

#' Draw Database Node (Base R)
#'
#' Cylinder shape.
#'
#' @keywords internal
draw_database_node_base <- function(x, y, size, col, border.col, border.width) {
  cyl_width <- size * 0.8
  cyl_height <- size * 1.2
  ellipse_h <- size * 0.25

  n_pts <- 50
  bottom_y <- y - cyl_height / 2
  top_y <- y + cyl_height / 2

  # Body
  graphics::rect(
    xleft = x - cyl_width, ybottom = bottom_y,
    xright = x + cyl_width, ytop = top_y,
    col = col, border = NA
  )

  # Side lines
  graphics::lines(c(x - cyl_width, x - cyl_width), c(bottom_y, top_y), col = border.col, lwd = border.width)
  graphics::lines(c(x + cyl_width, x + cyl_width), c(bottom_y, top_y), col = border.col, lwd = border.width)

  # Bottom ellipse (lower arc)
  angles <- seq(0, pi, length.out = n_pts)
  bottom_x <- x + cyl_width * cos(angles)
  bottom_y_pts <- bottom_y + ellipse_h * sin(angles) * (-1)
  graphics::lines(bottom_x, bottom_y_pts, col = border.col, lwd = border.width)

  # Top ellipse
  angles_full <- seq(0, 2 * pi, length.out = n_pts * 2)
  top_x <- x + cyl_width * cos(angles_full)
  top_y_pts <- top_y + ellipse_h * sin(angles_full)
  graphics::polygon(top_x, top_y_pts, col = col, border = border.col, lwd = border.width)
}

#' Render All Nodes
#'
#' Renders all nodes in the network.
#'
#' @param layout Matrix with x, y columns.
#' @param vsize Vector of node sizes.
#' @param vsize2 Vector of secondary sizes (for ellipse).
#' @param shape Vector of shape names.
#' @param color Vector of fill colors.
#' @param border.color Vector of border colors.
#' @param border.width Vector of border widths.
#' @param pie List of pie value vectors (one per node) or NULL.
#' @param pieColor List of pie color vectors or NULL.
#' @param donut List of donut values or NULL.
#' @param donutColor List of donut color vectors or NULL.
#' @param labels Vector of labels or NULL.
#' @param label.cex Vector of label sizes.
#' @param label.color Vector of label colors.
#' @keywords internal
render_nodes_base <- function(layout, vsize, vsize2 = NULL, shape = "circle",
                              color = "#4A90D9", border.color = "#2C5AA0",
                              border.width = 1, pie = NULL, pieColor = NULL,
                              donut = NULL, donutColor = NULL,
                              labels = NULL, label.cex = 1, label.color = "black") {
  n <- nrow(layout)
  if (n == 0) return(invisible())

  # Vectorize all parameters
  vsize <- recycle_to_length(vsize, n)
  vsize2 <- if (!is.null(vsize2)) recycle_to_length(vsize2, n) else vsize
  shape <- recycle_to_length(shape, n)
  color <- recycle_to_length(color, n)
  border.color <- recycle_to_length(border.color, n)
  border.width <- recycle_to_length(border.width, n)
  label.cex <- recycle_to_length(label.cex, n)
  label.color <- recycle_to_length(label.color, n)

  # Render order: largest to smallest
  order_idx <- get_node_order(vsize)

  for (i in order_idx) {
    x <- layout[i, 1]
    y <- layout[i, 2]

    # Check for pie/donut
    has_pie <- !is.null(pie) && length(pie) >= i && !is.null(pie[[i]]) && length(pie[[i]]) > 0
    has_donut <- !is.null(donut) && length(donut) >= i && !is.null(donut[[i]])

    if (has_donut && has_pie) {
      # Donut with inner pie
      donut_val <- if (length(donut[[i]]) == 1) donut[[i]] else 1
      donut_col <- if (!is.null(donutColor) && length(donutColor) >= i) donutColor[[i]][1] else color[i]
      pie_vals <- pie[[i]]
      pie_cols <- if (!is.null(pieColor) && length(pieColor) >= i) pieColor[[i]] else NULL

      draw_donut_pie_node_base(
        x, y, vsize[i],
        donut_value = donut_val,
        donut_color = donut_col,
        pie_values = pie_vals,
        pie_colors = pie_cols,
        border.col = border.color[i],
        border.width = border.width[i]
      )

    } else if (has_donut) {
      # Donut only
      donut_vals <- donut[[i]]
      donut_cols <- if (!is.null(donutColor) && length(donutColor) >= i) donutColor[[i]] else color[i]

      draw_donut_node_base(
        x, y, vsize[i],
        values = donut_vals,
        colors = donut_cols,
        border.col = border.color[i],
        border.width = border.width[i]
      )

    } else if (has_pie) {
      # Pie only
      pie_vals <- pie[[i]]
      pie_cols <- if (!is.null(pieColor) && length(pieColor) >= i) pieColor[[i]] else NULL

      draw_pie_node_base(
        x, y, vsize[i],
        values = pie_vals,
        colors = pie_cols,
        border.col = border.color[i],
        border.width = border.width[i]
      )

    } else {
      # Standard node
      draw_node_base(
        x, y, vsize[i], vsize2[i],
        shape = shape[i],
        col = color[i],
        border.col = border.color[i],
        border.width = border.width[i]
      )
    }
  }

  # Render labels (all at once, on top of nodes)
  if (!is.null(labels)) {
    for (i in seq_len(n)) {
      if (!is.null(labels[i]) && !is.na(labels[i]) && labels[i] != "") {
        draw_node_label_base(
          layout[i, 1], layout[i, 2],
          label = labels[i],
          cex = label.cex[i],
          col = label.color[i]
        )
      }
    }
  }
}
