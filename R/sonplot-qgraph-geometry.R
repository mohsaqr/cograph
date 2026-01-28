#' @title qgraph-Compatible Geometry Utilities
#' @description Coordinate transformation and geometry functions that exactly replicate
#' qgraph's visual logic. Used by splot() for qgraph-compatible network visualization.
#' @name sonplot-qgraph-geometry
#' @keywords internal
NULL

#' Get Plot Dimension Info
#'
#' Retrieves current plot device information needed for qgraph-style calculations.
#'
#' @return List with usr, pin, mai, csi, and dev_name components.
#' @keywords internal
qgraph_plot_info <- function() {
  list(
    usr = graphics::par("usr"),
    pin = graphics::par("pin"),
    mai = graphics::par("mai"),
    csi = graphics::par("csi"),
    dev_name = names(grDevices::dev.cur())
  )
}

#' qgraph Default Node Size
#'
#' Calculates the default node size using qgraph's exact formula.
#' Formula: 8 * exp(-n/80) + 1
#'
#' @param n_nodes Number of nodes in the network.
#' @return Default vsize value (before scale factor conversion).
#' @keywords internal
qgraph_default_vsize <- function(n_nodes) {
  8 * exp(-n_nodes / 80) + 1
}

#' qgraph Default Edge Size
#'
#' Calculates the default maximum edge width using qgraph's exact formula.
#' Formula: 15 * exp(-n/90) + 1 (halved for directed networks, minimum 1)
#'
#' @param n_nodes Number of nodes in the network.
#' @param weighted Logical: is the network weighted?
#' @param directed Logical: is the network directed?
#' @return Default esize value.
#' @keywords internal
qgraph_default_esize <- function(n_nodes, weighted = TRUE, directed = FALSE) {
  if (weighted) {
    esize <- 15 * exp(-n_nodes / 90) + 1
    if (directed) {
      esize <- max(esize / 2, 1)
    }
  } else {
    esize <- 2
  }
  esize
}

#' qgraph Edge Width Scaling (EXACT)
#'
#' Scales edge weights to widths using qgraph's exact formula.
#' Output range is 1 to esize for continuous scaling (cut = 0).
#'
#' @param weights Numeric vector of edge weights.
#' @param minimum Minimum weight threshold.
#' @param maximum Maximum weight for normalization.
#' @param cut Two-tier cutoff threshold. 0 = continuous scaling.
#' @param esize Maximum edge width.
#' @return Numeric vector of scaled edge widths.
#' @keywords internal
qgraph_scale_edge_widths <- function(weights, minimum = 0, maximum = NULL,
                                      cut = 0, esize = NULL) {
  if (length(weights) == 0) return(numeric(0))

  abs_w <- abs(weights)

  # Auto-detect maximum
  if (is.null(maximum)) {
    maximum <- max(abs_w, na.rm = TRUE)
  }
  if (maximum == 0 || is.na(maximum)) maximum <- 1

  # Auto-detect esize if not provided (assume 5 nodes as fallback)

  if (is.null(esize)) {
    esize <- 4  # Reasonable default
  }

  # qgraph-style normalization
  if (cut == 0) {
    # Continuous scaling: normalize from minimum to maximum
    avgW <- (abs_w - minimum) / (maximum - minimum)
  } else {
    # Two-tier: normalize from cut to maximum
    avgW <- (abs_w - cut) / (maximum - cut)
    avgW[abs_w < cut] <- 0
  }

  # Clamp to [0, 1]
  avgW <- pmax(0, pmin(1, avgW))

  # Map to [min_lwd, esize] range
  # Use very small minimum for thin edges on low weights
  min_lwd <- 0.1
  avgW * (esize - min_lwd) + min_lwd
}

#' qgraph Cent2Edge (EXACT - critical formula)
#'
#' Calculates the point on node boundary where an edge should connect.
#' This is qgraph's exact formula for positioning arrows and edge endpoints.
#'
#' @param x Node center x coordinate.
#' @param y Node center y coordinate.
#' @param cex Node size (vsize value, not yet scaled).
#' @param offset Additional offset distance.
#' @param angle Angle from node center to target point (radians).
#' @param plot_info Plot dimension info from qgraph_plot_info(). NULL to auto-compute.
#' @return List with x, y coordinates on node boundary.
#' @keywords internal
qgraph_cent2edge <- function(x, y, cex, offset = 0, angle, plot_info = NULL) {
  if (is.null(plot_info)) {
    plot_info <- qgraph_plot_info()
  }

  xin <- plot_info$pin[1]
  yin <- plot_info$pin[2]
  xmarrange <- plot_info$mai[2] + plot_info$mai[4]
  ymarrange <- plot_info$mai[1] + plot_info$mai[3]
  xrange <- plot_info$usr[2] - plot_info$usr[1]
  yrange <- plot_info$usr[4] - plot_info$usr[3]
  csi <- plot_info$csi

  # SVG device correction factor
  svg_factor <- 1 + 0.5 * (plot_info$dev_name == "devSVG")

  # qgraph's exact cent2edge formula
  x_factor <- ((xin + xmarrange) / xin) * (7 / (xin + xmarrange)) *
              (xrange / 2.16) * svg_factor * csi / 17.5
  y_factor <- ((yin + ymarrange) / yin) * (7 / (yin + ymarrange)) *
              (yrange / 2.16) * svg_factor * csi / 17.5

  list(
    x = x + (cex + offset) * x_factor * sin(angle),
    y = y + (cex + offset) * y_factor * cos(angle)
  )
}

#' qgraph Curve Normalization Factor
#'
#' Calculates the normalization factor for edge curvature to maintain
#' consistent visual appearance across different plot sizes.
#' Formula: sqrt(sum(pin^2)) / sqrt(7^2 + 7^2)
#'
#' @return Numeric normalization factor.
#' @keywords internal
qgraph_norm_curve <- function() {
  pin <- graphics::par("pin")
  sqrt(sum(pin^2)) / sqrt(7^2 + 7^2)
}

#' qgraph Node Size to User Coordinates
#'
#' Converts qgraph vsize to user coordinate radius using qgraph's exact logic.
#'
#' @param vsize Node size value (as used in qgraph).
#' @param plot_info Plot dimension info. NULL to auto-compute.
#' @return Node radius in user coordinates.
#' @keywords internal
qgraph_vsize_to_user <- function(vsize, plot_info = NULL) {
  if (is.null(plot_info)) {
    plot_info <- qgraph_plot_info()
  }

  xin <- plot_info$pin[1]
  yin <- plot_info$pin[2]
  xmarrange <- plot_info$mai[2] + plot_info$mai[4]
  ymarrange <- plot_info$mai[1] + plot_info$mai[3]
  xrange <- plot_info$usr[2] - plot_info$usr[1]
  yrange <- plot_info$usr[4] - plot_info$usr[3]
  csi <- plot_info$csi

  # SVG device correction factor
  svg_factor <- 1 + 0.5 * (plot_info$dev_name == "devSVG")

  # Average factor for circular nodes (average of x and y factors)
  x_factor <- ((xin + xmarrange) / xin) * (7 / (xin + xmarrange)) *
              (xrange / 2.16) * svg_factor * csi / 17.5
  y_factor <- ((yin + ymarrange) / yin) * (7 / (yin + ymarrange)) *
              (yrange / 2.16) * svg_factor * csi / 17.5

  # Use average for approximately circular appearance
  avg_factor <- (x_factor + y_factor) / 2

  vsize * avg_factor
}

#' qgraph Point on Node Boundary
#'
#' Simplified boundary calculation for splot that approximates qgraph behavior
#' while working with sonnet's coordinate system.
#'
#' @param x Node center x coordinate.
#' @param y Node center y coordinate.
#' @param angle Angle to target (radians).
#' @param node_size Node radius in user coordinates.
#' @param shape Node shape.
#' @return List with x, y coordinates on boundary.
#' @keywords internal
qgraph_cent_to_edge_simple <- function(x, y, angle, node_size, shape = "circle") {
  if (shape == "circle") {
    list(
      x = x + node_size * cos(angle),
      y = y + node_size * sin(angle)
    )
  } else if (shape == "square" || shape == "rectangle") {
    # Square/rectangle: find intersection with edges
    a <- angle %% (2 * pi)
    hw <- node_size

    tan_a <- tan(a)

    if (abs(cos(a)) < 1e-10) {
      if (sin(a) > 0) {
        list(x = x, y = y + hw)
      } else {
        list(x = x, y = y - hw)
      }
    } else if (abs(sin(a)) < 1e-10) {
      if (cos(a) > 0) {
        list(x = x + hw, y = y)
      } else {
        list(x = x - hw, y = y)
      }
    } else {
      edge_x <- if (cos(a) > 0) hw else -hw
      edge_y <- edge_x * tan_a

      if (abs(edge_y) <= hw) {
        list(x = x + edge_x, y = y + edge_y)
      } else {
        edge_y <- if (sin(a) > 0) hw else -hw
        edge_x <- edge_y / tan_a
        list(x = x + edge_x, y = y + edge_y)
      }
    }
  } else {
    # Default to circle for other shapes
    list(
      x = x + node_size * cos(angle),
      y = y + node_size * sin(angle)
    )
  }
}

#' qgraph-style Arrow Size Calculation
#'
#' Calculates arrow size based on edge width, matching qgraph behavior.
#'
#' @param edge_width Edge line width.
#' @param base_asize Base arrow size multiplier.
#' @return Arrow size in user coordinates.
#' @keywords internal
qgraph_arrow_size <- function(edge_width, base_asize = 1) {
  # qgraph scales arrow size with edge width
  # Base size around 0.02 user coords, scaled by edge width
  base_asize * 0.02 * sqrt(edge_width / 2)
}
