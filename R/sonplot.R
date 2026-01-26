#' @title qgraph-Compatible Network Plotting
#' @description Network visualization that exactly replicates qgraph's visual logic.
#' @name sonplot
NULL

#' Plot Network with qgraph-Compatible Visuals
#'
#' Creates a network visualization using base R graphics with visual output
#' that exactly replicates qgraph's appearance. Uses qgraph's formulas for
#' node sizing, edge width scaling, and boundary calculations while maintaining
#' sonnet's snake_case parameter naming convention.
#'
#' @param x Network input. Can be:
#'   - A square numeric matrix (adjacency/weight matrix)
#'   - A data frame with edge list (from, to, optional weight columns)
#'   - An igraph object
#'   - A sonnet_network object
#' @param layout Layout algorithm: "circle", "spring", "groups", or a matrix
#'   of x,y coordinates, or an igraph layout function. Also supports igraph
#'   two-letter codes: "kk", "fr", "drl", "mds", "ni", etc.
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @param seed Random seed for deterministic layouts. Default 42.
#' @param theme Theme name: "classic", "dark", "minimal", "colorblind", etc.
#'
#' @section Node Aesthetics:
#' @param node_size Node size(s). NULL uses qgraph adaptive formula: 8*exp(-n/80)+1.
#'   Single value or vector.
#' @param node_size2 Secondary node size for ellipse/rectangle height.
#' @param node_shape Node shape(s): "circle", "square", "triangle", "diamond",
#'   "pentagon", "hexagon", "star", "heart", "ellipse", "cross", or any custom
#'   SVG shape registered with register_svg_shape().
#' @param node_svg Custom SVG for nodes: path to SVG file OR inline SVG string.
#' @param svg_preserve_aspect Logical: maintain SVG aspect ratio? Default TRUE.
#' @param node_fill Node fill color(s).
#' @param node_border_color Node border color(s).
#' @param node_border_width Node border width(s).
#' @param node_alpha Node transparency (0-1). Default 1.
#' @param labels Node labels: TRUE (use node names/indices), FALSE (none),
#'   or character vector.
#' @param label_size Label character expansion factor.
#' @param label_color Label text color.
#' @param label_position Label position: "center", "above", "below", "left", "right".
#' @param label_fontface Font face for labels: "plain", "bold", "italic", "bold.italic". Default "plain".
#' @param label_fontfamily Font family for labels: "sans", "serif", "mono". Default "sans".
#' @param label_hjust Horizontal justification (0=left, 0.5=center, 1=right). Default 0.5.
#' @param label_vjust Vertical justification (0=bottom, 0.5=center, 1=top). Default 0.5.
#' @param label_angle Text rotation angle in degrees. Default 0.
#'
#' @section Pie/Donut Nodes:
#' @param pie_values List of numeric vectors for pie chart nodes.
#' @param pie_colors List of color vectors for pie segments.
#' @param pie_border_width Border width for pie slice dividers. NULL uses node_border_width.
#' @param donut_fill Numeric value (0-1) for donut fill proportion.
#' @param donut_values Deprecated. Use donut_fill.
#' @param donut_color Fill color(s) for the donut ring.
#' @param donut_colors Deprecated. Use donut_color instead.
#' @param donut_border_color Border color for donut rings. NULL uses node_border_color.
#' @param donut_border_width Border width for donut rings. NULL uses node_border_width.
#' @param donut_inner_ratio Inner radius ratio for donut (0-1). Default 0.5.
#' @param donut_bg_color Background color for unfilled donut portion.
#' @param donut_shape Base shape for donut: "circle", "square", "hexagon", etc.
#' @param donut_show_value Logical: show value in donut center? Default FALSE.
#' @param donut_value_size Font size for donut center value.
#' @param donut_value_color Color for donut center value.
#' @param donut_value_fontface Font face for donut center value: "plain", "bold", etc. Default "bold".
#' @param donut_value_fontfamily Font family for donut center value. Default "sans".
#' @param donut_value_digits Decimal places for donut center value. Default 2.
#' @param donut_value_prefix Text before donut center value. Default "".
#' @param donut_value_suffix Text after donut center value. Default "".
#' @param donut2_values List of values for inner donut ring (for double donut).
#' @param donut2_colors List of color vectors for inner donut ring segments.
#' @param donut2_inner_ratio Inner radius ratio for inner donut ring. Default 0.4.
#'
#' @section Edge Aesthetics:
#' @param edge_color Edge color(s). If NULL, uses positive_color/negative_color.
#' @param edge_width Edge width(s). If NULL, scales by weight using qgraph formula.
#' @param esize Base edge size for weight scaling. NULL uses qgraph adaptive formula:
#'   15*exp(-n/90)+1 (halved for directed networks).
#' @param edge_width_range Output width range as c(min, max) for weight-based scaling.
#'   Default c(0.5, 4). Edges are scaled to fit within this range.
#' @param edge_scale_mode Scaling mode for edge weights: "linear" (default, qgraph-style),
#'   "log", "sqrt", or "rank".
#' @param cut qgraph-style cutoff: 0 = continuous scaling (default), positive = threshold.
#' @param minimum Minimum weight threshold. Edges below this are hidden.
#' @param maximum Maximum weight for scaling. NULL for auto.
#' @param edge_alpha Edge transparency (0-1). Default 0.8.
#' @param curvature Edge curvature. 0 for straight, positive/negative for curves.
#' @param curves Curve mode: TRUE = reciprocal edges curved, FALSE = all straight,
#'   "force" = all edges curved.
#' @param arrow_size Arrow head size.
#' @param show_arrows Logical or vector: show arrows on directed edges?
#' @param positive_color Color for positive weights.
#' @param negative_color Color for negative weights.
#' @param edge_start_style Style for the start segment of edges: "solid" (default),
#'   "dashed", or "dotted". Use dashed/dotted to indicate edge direction.
#' @param edge_start_length Fraction of edge length for styled start segment (0-0.5).
#'   Default 0.15.
#'
#' @section Edge CI Underlays:
#' @param edge_ci Numeric vector of CI widths (0-1 scale). Larger values = more uncertainty.
#' @param edge_ci_scale Width multiplier for underlay thickness. Default 2.
#' @param edge_ci_alpha Transparency for underlay (0-1). Default 0.15.
#' @param edge_ci_color Underlay color. NA (default) uses main edge color.
#' @param edge_ci_style Line type for underlay: 1=solid, 2=dashed, 3=dotted. Default 2.
#' @param edge_ci_arrows Logical: show arrows on underlay? Default FALSE.
#'
#' @section Edge Labels:
#' @param edge_labels Edge labels: TRUE (show weights), FALSE (none), or character vector.
#' @param edge_label_size Edge label size.
#' @param edge_label_color Edge label text color.
#' @param edge_label_bg Edge label background color.
#' @param edge_label_position Position along edge (0=start, 0.5=middle, 1=end). Default 0.5.
#' @param edge_label_offset Perpendicular offset for edge labels (0 = on line).
#' @param edge_label_fontface Font face: 1=plain, 2=bold, 3=italic.
#' @param edge_label_shadow Logical: enable drop shadow for edge labels? Default FALSE.
#' @param edge_label_shadow_color Color for edge label shadow. Default "gray40".
#' @param edge_label_shadow_offset Offset distance for shadow. Default 0.5.
#' @param edge_label_shadow_alpha Transparency for shadow (0-1). Default 0.5.
#'
#' @section Edge Label Templates:
#' @param edge_label_style Preset style: "none", "estimate", "full", "range", "stars".
#' @param edge_label_template Template with placeholders: \{est\}, \{range\}, \{low\}, \{up\}, \{p\}, \{stars\}.
#' @param edge_label_digits Decimal places for estimates. Default 2.
#' @param edge_label_oneline Logical: single line format? Default TRUE.
#' @param edge_label_ci_format CI format: "bracket" for [low, up] or "dash" for low-up.
#' @param edge_ci_lower Numeric vector of lower CI bounds for labels.
#' @param edge_ci_upper Numeric vector of upper CI bounds for labels.
#' @param edge_label_p Numeric vector of p-values for edges.
#' @param edge_label_p_digits Decimal places for p-values. Default 3.
#' @param edge_label_p_prefix Prefix for p-values. Default "p=".
#' @param edge_label_stars Stars for labels: character vector, TRUE (compute from p),
#'   or numeric (treated as p-values).
#'
#' @section Plot Settings:
#' @param title Plot title.
#' @param title_size Title font size.
#' @param margins Margins as c(bottom, left, top, right).
#' @param background Background color.
#' @param rescale Logical: rescale layout to [-1, 1]?
#' @param layout_scale Scale factor for layout. >1 expands (spreads nodes apart),
#'   <1 contracts (brings nodes closer). Default 1.
#' @param layout_margin Margin around layout as fraction of range. Default 0.15.
#' @param aspect Logical: maintain aspect ratio?
#' @param usePCH Logical: use points() for simple circles (faster). Default FALSE.
#' @param scaling Scaling mode: "default" for qgraph-matched scaling, or "legacy".
#'
#' @section Legend:
#' @param legend Logical: show legend?
#' @param legend_position Position: "topright", "topleft", "bottomright", "bottomleft".
#' @param legend_size Legend text size.
#' @param legend_edge_colors Logical: show positive/negative edge colors in legend?
#' @param legend_node_sizes Logical: show node size scale in legend?
#' @param groups Group assignments for node coloring/legend.
#' @param node_names Alternative names for legend (separate from labels).
#'
#' @section Output:
#' @param filetype Output format: "default" (screen), "png", "pdf", "svg", "jpeg", "tiff".
#' @param filename Output filename (without extension).
#' @param width Output width in inches.
#' @param height Output height in inches.
#' @param res Resolution in DPI for raster outputs. Default 600.
#' @param ... Additional arguments passed to layout functions.
#'
#' @return Invisibly returns the sonnet_network object.
#'
#' @export
#'
#' @examples
#' # Basic network from adjacency matrix
#' adj <- matrix(c(0, 1, 1, 0,
#'                 0, 0, 1, 1,
#'                 0, 0, 0, 1,
#'                 0, 0, 0, 0), 4, 4, byrow = TRUE)
#' sonplot(adj)
#'
#' # Weighted network with automatic sizing
#' w_adj <- matrix(c(0, 0.5, -0.3, 0,
#'                   0.8, 0, 0.4, -0.2,
#'                   0, 0, 0, 0.6,
#'                   0, 0, 0, 0), 4, 4, byrow = TRUE)
#' sonplot(w_adj)
#'
#' # Compare node sizes with qgraph
#' # For n=4 nodes: 8*exp(-4/80)+1 = 8.61
#' # sonplot uses same formula, so visuals should match qgraph
#'
sonplot <- function(
    x,
    layout = "spring",
    directed = NULL,
    seed = 42,
    theme = NULL,

    # Node aesthetics (sonnet syntax)
    node_size = NULL,
    node_size2 = NULL,
    node_shape = "circle",
    node_svg = NULL,
    svg_preserve_aspect = TRUE,
    node_fill = NULL,
    node_border_color = NULL,
    node_border_width = 1,
    node_alpha = 1,
    labels = TRUE,
    label_size = NULL,
    label_color = "black",
    label_position = "center",
    label_fontface = "plain",
    label_fontfamily = "sans",
    label_hjust = 0.5,
    label_vjust = 0.5,
    label_angle = 0,

    # Pie/Donut
    pie_values = NULL,
    pie_colors = NULL,
    pie_border_width = NULL,
    donut_fill = NULL,
    donut_values = NULL,
    donut_color = NULL,
    donut_colors = NULL,
    donut_border_color = NULL,
    donut_border_width = NULL,
    donut_inner_ratio = 0.5,
    donut_bg_color = "gray90",
    donut_shape = "circle",
    donut_show_value = FALSE,
    donut_value_size = 0.8,
    donut_value_color = "black",
    donut_value_fontface = "bold",
    donut_value_fontfamily = "sans",
    donut_value_digits = 2,
    donut_value_prefix = "",
    donut_value_suffix = "",
    donut2_values = NULL,
    donut2_colors = NULL,
    donut2_inner_ratio = 0.4,

    # Edge aesthetics (sonnet syntax, qgraph behavior)
    edge_color = NULL,
    edge_width = NULL,
    esize = NULL,
    edge_width_range = c(0.5, 4),
    edge_scale_mode = "linear",
    cut = 0,
    minimum = 0,
    maximum = NULL,
    edge_alpha = 0.8,
    edge_labels = FALSE,
    edge_label_size = 0.8,
    edge_label_color = "gray30",
    edge_label_bg = "white",
    edge_label_position = 0.5,
    edge_label_offset = 0,
    edge_label_fontface = 1,
    edge_label_shadow = FALSE,
    edge_label_shadow_color = "gray40",
    edge_label_shadow_offset = 0.5,
    edge_label_shadow_alpha = 0.5,
    edge_style = 1,
    curvature = 0,
    curve_scale = TRUE,
    curve_shape = 0,
    curve_pivot = 0.5,
    curves = TRUE,
    arrow_size = 1,
    show_arrows = TRUE,
    bidirectional = FALSE,
    loop_rotation = NULL,

    # Edge Start Style (for direction clarity)
    edge_start_style = "solid",
    edge_start_length = 0.15,

    # Edge CI Underlays
    edge_ci = NULL,
    edge_ci_scale = 2.0,
    edge_ci_alpha = 0.15,
    edge_ci_color = NA,
    edge_ci_style = 2,
    edge_ci_arrows = FALSE,

    # Edge Label Templates
    edge_label_style = "none",
    edge_label_template = NULL,
    edge_label_digits = 2,
    edge_label_oneline = TRUE,
    edge_label_ci_format = "bracket",
    edge_ci_lower = NULL,
    edge_ci_upper = NULL,
    edge_label_p = NULL,
    edge_label_p_digits = 3,
    edge_label_p_prefix = "p=",
    edge_label_stars = NULL,

    # Weight handling
    threshold = 0,
    positive_color = "#2E7D32",
    negative_color = "#C62828",

    # Plot settings
    title = NULL,
    title_size = 1.2,
    margins = c(0.1, 0.1, 0.1, 0.1),
    background = "white",
    rescale = TRUE,
    layout_scale = 1,
    layout_margin = 0.15,
    aspect = TRUE,
    usePCH = FALSE,
    scaling = "default",

    # Legend
    legend = FALSE,
    legend_position = "topright",
    legend_size = 0.8,
    legend_edge_colors = TRUE,
    legend_node_sizes = FALSE,
    groups = NULL,
    node_names = NULL,

    # Output
    filetype = "default",
    filename = "sonplot",
    width = 7,
    height = 7,
    res = 600,
    ...
) {

  # ============================================
  # 1. INPUT PROCESSING
  # ============================================

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Convert to sonnet_network if needed
  network <- ensure_sonnet_network(x, layout = layout, seed = seed, ...)

  # Apply theme if specified
  if (!is.null(theme)) {
    th <- get_theme(theme)
    if (!is.null(th)) {
      # Extract theme colors
      if (is.null(node_fill)) node_fill <- th$get("node_fill")
      if (is.null(node_border_color)) node_border_color <- th$get("node_border_color")
      if (is.null(background)) background <- th$get("background")
      if (label_color == "black") label_color <- th$get("label_color")
      if (positive_color == "#2E7D32") positive_color <- th$get("edge_positive_color")
      if (negative_color == "#C62828") negative_color <- th$get("edge_negative_color")
    }
  }

  nodes <- network$network$get_nodes()
  edges <- network$network$get_edges()
  layout_coords <- network$network$get_layout()

  n_nodes <- nrow(nodes)
  n_edges <- if (!is.null(edges)) nrow(edges) else 0

  # Determine if directed
  if (is.null(directed)) {
    directed <- network$network$is_directed
  }

  # Determine if weighted
  weighted <- !is.null(edges) && "weight" %in% names(edges) &&
              any(edges$weight != 1, na.rm = TRUE)

  # ============================================
  # 2. LAYOUT HANDLING
  # ============================================

  if (is.null(layout_coords)) {
    stop("Layout coordinates not available", call. = FALSE)
  }

  layout_mat <- as.matrix(layout_coords[, c("x", "y")])

  # Rescale to [-1, 1]
  if (rescale) {
    layout_mat <- as.matrix(rescale_layout(layout_mat, mar = 0.1))
  }

  # Apply layout scale (expand/contract around center)
  if (layout_scale != 1) {
    center <- colMeans(layout_mat)
    layout_mat <- t(t(layout_mat - center) * layout_scale + center)
  }

  # ============================================
  # 3. QGRAPH-STYLE NODE SIZING
  # ============================================

  # Get scale constants for current scaling mode
  scale <- get_scale_constants(scaling)

  # Node sizes (qgraph-style, using scale constants)
  vsize_usr <- resolve_node_sizes(node_size, n_nodes, scaling = scaling)
  vsize2_usr <- if (!is.null(node_size2)) {
    resolve_node_sizes(node_size2, n_nodes, scaling = scaling)
  } else {
    vsize_usr
  }

  # Node shapes - handle custom SVG if provided
  if (!is.null(node_svg)) {
    # Register SVG as a temporary shape
    temp_svg_name <- paste0("_sonplot_svg_", format(Sys.time(), "%H%M%S"))
    tryCatch({
      register_svg_shape(temp_svg_name, node_svg)
      node_shape <- temp_svg_name
    }, error = function(e) {
      warning("Failed to register SVG shape: ", e$message, ". Using default shape.",
              call. = FALSE)
    })
  }
  shapes <- resolve_shapes(node_shape, n_nodes)

  # Node colors
  node_colors <- resolve_node_colors(node_fill, n_nodes, nodes, groups)

  # Apply alpha to node colors
  if (node_alpha < 1) {
    node_colors <- sapply(node_colors, function(c) adjust_alpha(c, node_alpha))
  }

  # Border colors
  if (is.null(node_border_color)) {
    node_border_color <- sapply(node_colors, function(c) {
      tryCatch(adjust_brightness(c, -0.3), error = function(e) "black")
    })
  }
  border_colors <- recycle_to_length(node_border_color, n_nodes)

  # Border widths
  border_widths <- recycle_to_length(node_border_width, n_nodes)

  # Labels
  node_labels <- resolve_labels(labels, nodes, n_nodes)

  # Label sizes (using new decoupled system)
  label_cex <- resolve_label_sizes(label_size, vsize_usr, n_nodes, scaling = scaling)
  label_colors <- recycle_to_length(label_color, n_nodes)

  # ============================================
  # 4. QGRAPH-STYLE EDGE PROCESSING
  # ============================================

  # Use minimum threshold or explicit threshold
  effective_threshold <- max(threshold, minimum)

  if (n_edges > 0) {
    # Filter by minimum weight (threshold)
    edges <- filter_edges_by_weight(edges, effective_threshold)
    n_edges <- nrow(edges)
  }

  # ============================================
  # 4a. QGRAPH-STYLE DOUBLE CURVED EDGES FOR UNDIRECTED NETWORKS
  # ============================================
  # For undirected networks with curves enabled, duplicate edges to create
  # double curved visual representation (like qgraph)
  if (n_edges > 0 && !directed && (identical(curves, TRUE) || identical(curves, "mutual"))) {
    # Duplicate each edge with reversed direction
    reverse_edges <- edges
    reverse_edges$from <- edges$to
    reverse_edges$to <- edges$from
    edges <- rbind(edges, reverse_edges)
    n_edges <- nrow(edges)
  }

  if (n_edges > 0) {
    # Edge colors
    edge_colors <- resolve_edge_colors(edges, edge_color, positive_color, negative_color)

    # Apply edge alpha
    if (edge_alpha < 1) {
      edge_colors <- sapply(edge_colors, function(c) adjust_alpha(c, edge_alpha))
    }

    # Apply cut threshold for transparency: edges below cut are faded
    if (cut > 0 && "weight" %in% names(edges)) {
      abs_weights <- abs(edges$weight)
      below_cut <- abs_weights < cut
      if (any(below_cut)) {
        # Scale alpha: edges at 0 get 20% of normal alpha, edges near cut get full alpha
        fade_factor <- ifelse(below_cut, 0.2 + 0.8 * (abs_weights / cut), 1)
        edge_colors <- mapply(function(col, fade) {
          if (fade < 1) adjust_alpha(col, fade) else col
        }, edge_colors, fade_factor, SIMPLIFY = TRUE, USE.NAMES = FALSE)
      }
    }

    # Edge widths (using resolve_edge_widths for proper scaling)
    edge_widths <- resolve_edge_widths(
      edges = edges,
      edge.width = edge_width,
      esize = esize,
      n_nodes = n_nodes,
      directed = directed,
      maximum = maximum,
      minimum = effective_threshold,
      cut = cut,
      edge_width_range = edge_width_range,
      edge_scale_mode = edge_scale_mode,
      scaling = scaling
    )

    # Line types
    ltys <- recycle_to_length(edge_style, n_edges)

    # Handle curves mode:
    # FALSE = all straight
    # TRUE or "mutual" = only reciprocal edges curved (opposite directions)
    # "force" = all edges curved
    #
    # curvature parameter sets the MAGNITUDE of curves (default 0.25)
    # curves parameter controls WHICH edges get curved
    is_reciprocal <- rep(FALSE, n_edges)

    # Identify reciprocal pairs
    for (i in seq_len(n_edges)) {
      from_i <- edges$from[i]
      to_i <- edges$to[i]
      if (from_i == to_i) next
      for (j in seq_len(n_edges)) {
        if (j != i && edges$from[j] == to_i && edges$to[j] == from_i) {
          is_reciprocal[i] <- TRUE
          break
        }
      }
    }

    # Curve magnitude (user-specified or default 0.25)
    curve_magnitude <- if (curvature == 0) 0.25 else abs(curvature)

    # Initialize curves vector to 0 (straight)
    curves_vec <- rep(0, n_edges)

    # Calculate network center for curve direction
    center_x <- mean(layout_mat[, 1])
    center_y <- mean(layout_mat[, 2])

    if (identical(curves, TRUE) || identical(curves, "mutual")) {
      # Curve reciprocal edges with direction based on network center
      for (i in seq_len(n_edges)) {
        if (is_reciprocal[i]) {
          # Calculate edge midpoint
          from_idx <- edges$from[i]
          to_idx <- edges$to[i]
          mid_x <- (layout_mat[from_idx, 1] + layout_mat[to_idx, 1]) / 2
          mid_y <- (layout_mat[from_idx, 2] + layout_mat[to_idx, 2]) / 2

          # Calculate perpendicular direction (for curve)
          dx <- layout_mat[to_idx, 1] - layout_mat[from_idx, 1]
          dy <- layout_mat[to_idx, 2] - layout_mat[from_idx, 2]

          # Perpendicular vector (rotated 90 degrees)
          perp_x <- -dy
          perp_y <- dx

          # Check if positive curve moves toward or away from center
          test_x <- mid_x + perp_x * 0.1
          test_y <- mid_y + perp_y * 0.1
          dist_to_center_pos <- sqrt((test_x - center_x)^2 + (test_y - center_y)^2)
          dist_to_center_orig <- sqrt((mid_x - center_x)^2 + (mid_y - center_y)^2)

          # Lower index node gets the curve pointing AWAY from center (outer curve)
          # Higher index node gets the curve pointing TOWARD center (inner curve)
          if (edges$from[i] < edges$to[i]) {
            curves_vec[i] <- if (dist_to_center_pos > dist_to_center_orig) curve_magnitude else -curve_magnitude
          } else {
            curves_vec[i] <- if (dist_to_center_pos < dist_to_center_orig) curve_magnitude else -curve_magnitude
          }
        }
      }
    } else if (identical(curves, "force")) {
      # Curve all edges with the specified magnitude
      for (i in seq_len(n_edges)) {
        if (edges$from[i] == edges$to[i]) next  # Skip self-loops
        curves_vec[i] <- curve_magnitude
      }
    }
    # If curves = FALSE, curves_vec stays at 0 (straight edges)

    curve_pivots <- recycle_to_length(curve_pivot, n_edges)
    curve_shapes <- recycle_to_length(curve_shape, n_edges)

    # Arrows
    if (is.logical(show_arrows) && length(show_arrows) == 1) {
      arrows_vec <- rep(directed && show_arrows, n_edges)
    } else {
      arrows_vec <- recycle_to_length(show_arrows, n_edges)
    }

    # Arrow size (using scale constants for consistency)
    asize_scaled <- arrow_size * scale$arrow_factor
    arrow_sizes <- recycle_to_length(asize_scaled, n_edges)

    # Bidirectional
    bidirectionals <- recycle_to_length(bidirectional, n_edges)

    # Loop rotation
    loop_rotations <- resolve_loop_rotation(loop_rotation, edges, layout_mat)

    # Edge labels - check for template system first
    if (!is.null(edge_label_template) || edge_label_style != "none") {
      # Use template-based labels
      edge_weights <- if ("weight" %in% names(edges)) edges$weight else NULL
      edge_labels_vec <- build_edge_labels_from_template(
        template = edge_label_template,
        style = edge_label_style,
        weights = edge_weights,
        ci_lower = edge_ci_lower,
        ci_upper = edge_ci_upper,
        p_values = edge_label_p,
        stars = edge_label_stars,
        digits = edge_label_digits,
        p_digits = edge_label_p_digits,
        p_prefix = edge_label_p_prefix,
        ci_format = edge_label_ci_format,
        oneline = edge_label_oneline,
        n = n_edges
      )
    } else {
      # Use standard edge labels
      edge_labels_vec <- resolve_edge_labels(edge_labels, edges, n_edges)
    }

    # CI underlay parameters
    edge_ci_vec <- if (!is.null(edge_ci)) recycle_to_length(edge_ci, n_edges) else NULL
    edge_ci_colors <- if (!is.null(edge_ci_vec)) {
      if (is.na(edge_ci_color)) {
        # Use main edge colors
        edge_colors
      } else {
        recycle_to_length(edge_ci_color, n_edges)
      }
    } else NULL
  }

  # ============================================
  # 5. DEVICE SETUP
  # ============================================

  # Handle file output
  if (filetype != "default") {
    full_filename <- paste0(filename, ".", filetype)

    if (filetype == "png") {
      grDevices::png(full_filename, width = width, height = height,
                     units = "in", res = res)
    } else if (filetype == "pdf") {
      grDevices::pdf(full_filename, width = width, height = height)
    } else if (filetype == "svg") {
      grDevices::svg(full_filename, width = width, height = height)
    } else if (filetype == "jpeg" || filetype == "jpg") {
      grDevices::jpeg(full_filename, width = width, height = height,
                      units = "in", res = res, quality = 100)
    } else if (filetype == "tiff") {
      grDevices::tiff(full_filename, width = width, height = height,
                      units = "in", res = res, compression = "lzw")
    } else {
      stop("Unknown filetype: ", filetype, call. = FALSE)
    }

    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Set up plot area
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)

  # Margins
  title_space <- if (!is.null(title)) 0.5 else 0
  graphics::par(mar = c(margins[1], margins[2], margins[3] + title_space, margins[4]))

  # Calculate plot limits
  x_range <- range(layout_mat[, 1], na.rm = TRUE)
  y_range <- range(layout_mat[, 2], na.rm = TRUE)

  # Add margin to limits
  x_margin <- diff(x_range) * layout_margin
  y_margin <- diff(y_range) * layout_margin

  xlim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  ylim <- c(y_range[1] - y_margin, y_range[2] + y_margin)

  # Create plot
  graphics::plot(
    1, type = "n",
    xlim = xlim,
    ylim = ylim,
    axes = FALSE,
    ann = FALSE,
    asp = if (aspect) 1 else NA,
    xaxs = "i", yaxs = "i"
  )

  # Background
  if (!is.null(background) && background != "transparent") {
    graphics::rect(
      xleft = xlim[1] - 1, ybottom = ylim[1] - 1,
      xright = xlim[2] + 1, ytop = ylim[2] + 1,
      col = background, border = NA
    )
  }

  # Title
  if (!is.null(title)) {
    graphics::title(main = title, cex.main = title_size)
  }

  # ============================================
  # 6. RENDER EDGES
  # ============================================

  if (n_edges > 0) {
    render_edges_splot(
      edges = edges,
      layout = layout_mat,
      node_sizes = vsize_usr,
      shapes = shapes,
      edge_color = edge_colors,
      edge_width = edge_widths,
      edge_style = ltys,
      curvature = curves_vec,
      curve_shape = curve_shapes,
      curve_pivot = curve_pivots,
      show_arrows = arrows_vec,
      arrow_size = arrow_sizes,
      bidirectional = bidirectionals,
      loop_rotation = loop_rotations,
      edge_labels = edge_labels_vec,
      edge_label_size = edge_label_size,
      edge_label_color = edge_label_color,
      edge_label_bg = edge_label_bg,
      edge_label_position = edge_label_position,
      edge_label_offset = edge_label_offset,
      edge_label_fontface = edge_label_fontface,
      edge_label_shadow = edge_label_shadow,
      edge_label_shadow_color = edge_label_shadow_color,
      edge_label_shadow_offset = edge_label_shadow_offset,
      edge_label_shadow_alpha = edge_label_shadow_alpha,
      # CI underlay parameters
      edge_ci = edge_ci_vec,
      edge_ci_scale = edge_ci_scale,
      edge_ci_alpha = edge_ci_alpha,
      edge_ci_color = edge_ci_colors,
      edge_ci_style = edge_ci_style,
      edge_ci_arrows = edge_ci_arrows,
      is_reciprocal = is_reciprocal,
      # Edge start style parameters
      edge_start_style = edge_start_style,
      edge_start_length = edge_start_length
    )
  }

  # ============================================
  # 7. RENDER NODES
  # ============================================

  # Auto-enable donut fill when node_shape is "donut" but no fill specified
  if (is.null(donut_fill) && is.null(donut_values)) {
    if (any(shapes == "donut")) {
      # Create per-node fill: 1.0 for donut nodes, NA for others
      donut_fill <- ifelse(shapes == "donut", 1.0, NA)
    }
  }

  # Handle donut_fill: convert to list format if provided
  effective_donut_values <- donut_values
  if (!is.null(donut_fill)) {
    if (!is.list(donut_fill)) {
      fill_vec <- recycle_to_length(donut_fill, n_nodes)
      effective_donut_values <- as.list(fill_vec)
    } else {
      effective_donut_values <- donut_fill
    }
  }

  # Handle donut_color (new simplified API) and donut_colors (deprecated)
  effective_donut_colors <- NULL
  effective_bg_color <- donut_bg_color

  if (!is.null(donut_color)) {
    if (is.list(donut_color) && length(donut_color) == 2 * n_nodes) {
      # List with 2×n_nodes: per-node (fill, bg) pairs - extract odd indices for fill
      effective_donut_colors <- as.list(donut_color[seq(1, 2 * n_nodes, by = 2)])
    } else if (length(donut_color) == 2) {
      # Two colors: fill + background for ALL nodes
      effective_donut_colors <- as.list(rep(donut_color[1], n_nodes))
      effective_bg_color <- donut_color[2]
    } else if (length(donut_color) == 1) {
      # Single color: fill for all nodes
      effective_donut_colors <- as.list(rep(donut_color, n_nodes))
    } else {
      # Multiple colors (not 2): treat as per-node fill colors
      cols <- recycle_to_length(donut_color, n_nodes)
      effective_donut_colors <- as.list(cols)
    }
  } else if (!is.null(donut_colors)) {
    # Deprecated: use old donut_colors parameter
    effective_donut_colors <- donut_colors
  } else if (any(shapes == "donut") || !is.null(effective_donut_values)) {
    # Default fill color: light gray when donuts are being used
    effective_donut_colors <- as.list(rep("lightgray", n_nodes))
  }

  # Determine effective donut shapes - inherit from node_shape by default
  if (is.null(donut_shape) || identical(donut_shape, "circle")) {
    # Inherit from node_shape, replacing special donut shapes with "circle"
    special_donut_shapes <- c("donut", "donut_pie", "double_donut_pie")
    effective_donut_shapes <- ifelse(shapes %in% special_donut_shapes, "circle", shapes)
  } else {
    # User explicitly set donut_shape - vectorize and use it
    effective_donut_shapes <- recycle_to_length(donut_shape, n_nodes)
  }

  # Vectorize donut_border_color for per-node support
  effective_donut_border_color <- if (!is.null(donut_border_color)) {
    recycle_to_length(donut_border_color, n_nodes)
  } else {
    NULL
  }

  render_nodes_splot(
    layout = layout_mat,
    node_size = vsize_usr,
    node_size2 = vsize2_usr,
    node_shape = shapes,
    node_fill = node_colors,
    node_border_color = border_colors,
    node_border_width = border_widths,
    pie_values = pie_values,
    pie_colors = pie_colors,
    pie_border_width = pie_border_width,
    donut_values = effective_donut_values,
    donut_colors = effective_donut_colors,
    donut_border_color = effective_donut_border_color,
    donut_border_width = donut_border_width,
    donut_inner_ratio = donut_inner_ratio,
    donut_bg_color = effective_bg_color,
    donut_shape = effective_donut_shapes,
    donut_show_value = donut_show_value,
    donut_value_size = donut_value_size,
    donut_value_color = donut_value_color,
    donut_value_fontface = donut_value_fontface,
    donut_value_fontfamily = donut_value_fontfamily,
    donut_value_digits = donut_value_digits,
    donut_value_prefix = donut_value_prefix,
    donut_value_suffix = donut_value_suffix,
    donut2_values = donut2_values,
    donut2_colors = donut2_colors,
    donut2_inner_ratio = donut2_inner_ratio,
    labels = node_labels,
    label_size = label_cex,
    label_color = label_colors,
    label_position = label_position,
    label_fontface = label_fontface,
    label_fontfamily = label_fontfamily,
    label_hjust = label_hjust,
    label_vjust = label_vjust,
    label_angle = label_angle,
    usePCH = usePCH
  )

  # ============================================
  # 8. LEGEND
  # ============================================

  if (legend) {
    # Determine if we have positive/negative weighted edges
    has_pos_edges <- FALSE
    has_neg_edges <- FALSE
    if (n_edges > 0 && "weight" %in% names(edges)) {
      has_pos_edges <- any(edges$weight > 0, na.rm = TRUE)
      has_neg_edges <- any(edges$weight < 0, na.rm = TRUE)
    }

    render_legend_splot(
      groups = groups,
      node_names = node_names,
      nodes = nodes,
      node_colors = node_colors,
      position = legend_position,
      cex = legend_size,
      show_edge_colors = legend_edge_colors,
      positive_color = positive_color,
      negative_color = negative_color,
      has_pos_edges = has_pos_edges,
      has_neg_edges = has_neg_edges,
      show_node_sizes = legend_node_sizes,
      node_size = vsize_usr
    )
  }

  # ============================================
  # 9. RETURN
  # ============================================

  invisible(network)
}
