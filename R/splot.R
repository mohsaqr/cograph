#' @title Base R Graphics Network Plotting
#' @description Network visualization using base R graphics (similar to qgraph).
#' @name splot
NULL

#' Plot Network with Base R Graphics
#'
#' Creates a network visualization using base R graphics functions (polygon,
#' lines, xspline, etc.) instead of grid graphics. This provides better
#' performance for large networks and uses the same snake_case parameter names
#' as soplot() for consistency.
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
#' @param node_size Node size(s). Single value or vector. Default 3.
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
#' @param pie_values List of numeric vectors for pie chart nodes. Each element
#'   corresponds to a node and contains values for pie segments.
#' @param pie_colors List of color vectors for pie segments.
#' @param pie_border_width Border width for pie slice dividers. NULL uses node_border_width.
#' @param donut_fill Numeric value (0-1) for donut fill proportion. This is the
#'   qgraph-style API: 0.1 = 10% filled, 0.5 = 50% filled, 1.0 = fully filled.
#'   Can be a single value (all nodes) or vector (per-node values).
#' @param donut_values Deprecated. Use donut_fill for simple fill proportion.
#' @param donut_color Fill color(s) for the donut ring.
#'   Single color sets fill for all nodes.
#'   Two colors set fill and background for all nodes.
#'   More than 2 colors set per-node fill colors (recycled to n_nodes).
#'   Default: "maroon" fill, "gray90" background when node_shape="donut".
#' @param donut_colors Deprecated. Use donut_color instead.
#' @param donut_border_color Border color for donut rings. NULL uses node_border_color.
#' @param donut_border_width Border width for donut rings. NULL uses node_border_width.
#' @param donut_outer_border_color Color for outer boundary border (enables double border).
#'   NULL (default) shows single border. Set to a color for double border effect.
#'   Can be scalar or per-node vector.
#' @param donut_border_lty Line type for donut borders (1=solid, 2=dashed, 3=dotted, etc.).
#'   Can be scalar (applies to all) or per-node vector.
#' @param donut_inner_ratio Inner radius ratio for donut (0-1). Default 0.5.
#' @param donut_bg_color Background color for unfilled donut portion.
#' @param donut_shape Base shape for donut: "circle", "square", "hexagon", "triangle",
#'   "diamond", "pentagon". Can be a single value or per-node vector.
#'   Default inherits from node_shape (e.g., hexagon nodes get hexagon donuts).
#'   Set explicitly to override (e.g., donut_shape = "hexagon" for hexagon donuts
#'   on all nodes regardless of node_shape).
#' @param donut_show_value Logical: show value in donut center? Default FALSE.
#' @param donut_value_size Font size for donut center value.
#' @param donut_value_color Color for donut center value.
#' @param donut_value_fontface Font face for donut center value: "plain", "bold", "italic", "bold.italic". Default "bold".
#' @param donut_value_fontfamily Font family for donut center value: "sans", "serif", "mono". Default "sans".
#' @param donut_value_digits Decimal places for donut center value. Default 2.
#' @param donut_value_prefix Text before donut center value (e.g., "$"). Default "".
#' @param donut_value_suffix Text after donut center value (e.g., "%"). Default "".
#' @param donut_empty Logical: render empty donut rings for NA values? Default TRUE.
#' @param donut2_values List of values for inner donut ring (for double donut).
#' @param donut2_colors List of color vectors for inner donut ring segments.
#' @param donut2_inner_ratio Inner radius ratio for inner donut ring. Default 0.4.
#'
#' @param edge_color Edge color(s). If NULL, uses positive_color/negative_color based on weight.
#' @param edge_width Edge width(s). If NULL, scales by weight using esize and edge_width_range.
#' @param esize Base edge size for weight scaling. NULL (default) uses adaptive sizing
#'   based on network size: `15 * exp(-n_nodes/90) + 1`. For directed networks, this
#'   is halved. Larger values = thicker edges overall.
#' @param edge_width_range Output width range as c(min, max) for weight-based scaling.
#'   Default c(0.5, 4). Edges are scaled to fit within this range.
#' @param edge_scale_mode Scaling mode for edge weights: "linear" (default, qgraph-style),
#'   "log" (logarithmic for wide weight ranges), "sqrt" (moderate compression),
#'   or "rank" (equal visual spacing regardless of weight distribution).
#' @param cut Two-tier cutoff for edge width scaling. NULL (default) = auto-calculate
#'   as 75th percentile of weights (qgraph behavior). 0 = disabled (continuous scaling).
#'   Positive number = manual threshold. Edges below cut get minimal width variation.
#' @param edge_alpha Edge transparency (0-1). Default 0.8.
#' @param edge_labels Edge labels: TRUE (show weights), FALSE (none),
#'   or character vector.
#' @param edge_label_size Edge label size.
#' @param edge_label_color Edge label text color.
#' @param edge_label_bg Edge label background color.
#' @param edge_label_position Position along edge (0-1).
#' @param edge_label_offset Perpendicular offset for edge labels (0 = on line, positive = above).
#' @param edge_label_fontface Font face: 1=plain, 2=bold, 3=italic.
#' @param edge_label_shadow Logical: enable drop shadow for edge labels? Default FALSE.
#' @param edge_label_shadow_color Color for edge label shadow. Default "gray40".
#' @param edge_label_shadow_offset Offset distance for shadow in points. Default 0.5.
#' @param edge_label_shadow_alpha Transparency for shadow (0-1). Default 0.5.
#' @param edge_style Line type(s): 1=solid, 2=dashed, 3=dotted, etc.
#' @param curvature Edge curvature. 0 for straight, positive/negative for curves.
#' @param curve_scale Logical: auto-curve reciprocal edges?
#' @param curve_shape Spline tension (-1 to 1). Default 0.
#' @param curve_pivot Position along edge for curve control point (0-1).
#' @param curves Curve mode: TRUE (default) = single edges straight, reciprocal edges
#'   curve as ellipse (two opposing curves); FALSE = all straight; "force" = all curved.
#' @param arrow_size Arrow head size.
#' @param show_arrows Logical or vector: show arrows on directed edges?
#' @param bidirectional Logical or vector: show arrows at both ends?
#' @param loop_rotation Angle(s) in radians for self-loop direction.
#' @param edge_start_style Style for the start segment of edges: "solid" (default),
#'   "dashed", or "dotted". Use dashed/dotted to indicate edge direction (source node).
#' @param edge_start_length Fraction of edge length for the styled start segment (0-0.5).
#'   Default 0.15 (15% of edge). Only applies when edge_start_style is not "solid".
#'
#' @param edge_ci Numeric vector of CI widths (0-1 scale). Larger values = more uncertainty.
#' @param edge_ci_scale Width multiplier for underlay thickness. Default 2.
#' @param edge_ci_alpha Transparency for underlay (0-1). Default 0.15.
#' @param edge_ci_color Underlay color. NA (default) uses main edge color.
#' @param edge_ci_style Line type for underlay: 1=solid, 2=dashed, 3=dotted. Default 2.
#' @param edge_ci_arrows Logical: show arrows on underlay? Default FALSE.
#'
#' @param edge_label_style Preset style: "none", "estimate", "full", "range", "stars".
#' @param edge_label_template Template with placeholders: \{est\}, \{range\}, \{low\}, \{up\}, \{p\}, \{stars\}.
#'   Overrides edge_label_style if provided.
#' @param edge_label_digits Decimal places for estimates. Default 2.
#' @param edge_label_oneline Logical: single line format? Default TRUE.
#' @param edge_label_ci_format CI format: "bracket" for `[low, up]` or "dash" for `low-up`.
#' @param edge_ci_lower Numeric vector of lower CI bounds for labels.
#' @param edge_ci_upper Numeric vector of upper CI bounds for labels.
#' @param edge_label_p Numeric vector of p-values for edges.
#' @param edge_label_p_digits Decimal places for p-values. Default 3.
#' @param edge_label_p_prefix Prefix for p-values. Default "p=".
#' @param edge_label_stars Stars for labels: character vector, TRUE (compute from p),
#'   or numeric (treated as p-values).
#'
#' @param weight_digits Number of decimal places to round edge weights to before
#'   plotting. Edges that round to zero are automatically removed. Default 2.
#'   Set NULL to disable rounding.
#' @param threshold Minimum absolute weight to display.
#' @param minimum Alias for threshold (qgraph compatibility). Uses max of threshold and minimum.
#' @param maximum Maximum weight for scaling. NULL for auto.
#' @param positive_color Color for positive weights.
#' @param negative_color Color for negative weights.
#' @param edge_duplicates How to handle duplicate edges in undirected networks.
#'   NULL (default) = stop with error listing duplicates. Options: "sum", "mean",
#'   "first", "max", "min", or a custom aggregation function.
#'
#' @param title Plot title.
#' @param title_size Title font size.
#' @param margins Margins as c(bottom, left, top, right).
#' @param background Background color.
#' @param rescale Logical: rescale layout to -1 to 1 range?
#' @param layout_scale Scale factor for layout. >1 expands (spreads nodes apart),
#'   <1 contracts (brings nodes closer). Use "auto" to automatically scale based
#'   on node count (compact for small networks, expanded for large). Default 1.
#' @param layout_margin Margin around the layout as fraction of range. Default 0.15.
#'   Set to 0 for no extra margin (tighter fit). Affects white space around nodes.
#' @param aspect Logical: maintain aspect ratio?
#' @param usePCH Logical: use points() for simple circles (faster). Default FALSE.
#' @param scaling Scaling mode: "default" for qgraph-matched scaling where node_size=6
#'   looks similar to qgraph vsize=6, or "legacy" to preserve pre-v2.0 behavior.
#'
#' @param legend Logical: show legend?
#' @param legend_position Position: "topright", "topleft", "bottomright", "bottomleft".
#' @param legend_size Legend text size.
#' @param legend_edge_colors Logical: show positive/negative edge colors in legend?
#' @param legend_node_sizes Logical: show node size scale in legend?
#' @param groups Group assignments for node coloring/legend.
#' @param node_names Alternative names for legend (separate from labels).
#'
#' @param filetype Output format: "default" (screen), "png", "pdf", "svg", "jpeg", "tiff".
#' @param filename Output filename (without extension).
#' @param width Output width in inches.
#' @param height Output height in inches.
#' @param res Resolution in DPI for raster outputs (PNG, JPEG, TIFF). Default 600.
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
#' splot(adj)
#'
#' # With curved edges
#' splot(adj, curvature = 0.2)
#'
#' # Weighted network with colors
#' w_adj <- matrix(c(0, 0.5, -0.3, 0,
#'                   0.8, 0, 0.4, -0.2,
#'                   0, 0, 0, 0.6,
#'                   0, 0, 0, 0), 4, 4, byrow = TRUE)
#' splot(w_adj, positive_color = "darkgreen", negative_color = "red")
#'
#' # Pie chart nodes
#' splot(adj, pie_values = list(c(1,2,3), c(2,2), c(1,1,1,1), c(3,1)))
#'
#' # Circle layout with labels
#' splot(adj, layout = "circle", labels = c("A", "B", "C", "D"))
#'
splot <- function(
    x,
    layout = "spring",
    directed = NULL,
    seed = 42,
    theme = NULL,

    # Node aesthetics
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
    donut_colors = NULL,  # Deprecated: use donut_color
    donut_border_color = NULL,
    donut_border_width = NULL,
    donut_outer_border_color = NULL,
    donut_border_lty = 1,
    donut_inner_ratio = 0.8,
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
    donut_empty = TRUE,
    donut2_values = NULL,
    donut2_colors = NULL,
    donut2_inner_ratio = 0.4,

    # Edge aesthetics
    edge_color = NULL,
    edge_width = NULL,
    esize = NULL,
    edge_width_range = c(0.1, 4),
    edge_scale_mode = "linear",
    cut = NULL,
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
    weight_digits = 2,
    threshold = 0,
    minimum = 0,
    maximum = NULL,
    positive_color = "#2E7D32",
    negative_color = "#C62828",
    edge_duplicates = NULL,

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
    filename = "splot",
    width = 7,
    height = 7,
    res = 600,
    ...
) {

  # ============================================
  # 1. INPUT PROCESSING
  # ============================================

  # Handle tna objects directly
  if (inherits(x, "tna")) {
    tna_params <- from_tna(x, engine = "splot", plot = FALSE)
    # User-supplied args override tna defaults
    call_args <- tna_params
    call_args$layout <- layout
    call_args$seed <- seed
    call_args$theme <- theme
    user_args <- as.list(match.call(expand.dots = FALSE))[-1]
    user_args$x <- NULL  # already set via tna_params$x
    dots <- list(...)
    for (nm in names(user_args)) {
      val <- eval(user_args[[nm]], envir = parent.frame())
      if (!is.null(val)) call_args[[nm]] <- val
    }
    for (nm in names(dots)) {
      call_args[[nm]] <- dots[[nm]]
    }
    return(do.call(splot, call_args))
  }

  # Round matrix weights to filter near-zero edges globally
  if (is.matrix(x) && !is.null(weight_digits)) {
    x <- round(x, weight_digits)
  }

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
      if (length(label_color) == 1 && label_color == "black") label_color <- th$get("label_color")
      if (length(positive_color) == 1 && positive_color == "#2E7D32") positive_color <- th$get("edge_positive_color")
      if (length(negative_color) == 1 && negative_color == "#C62828") negative_color <- th$get("edge_negative_color")
    }
  }

  nodes <- network$network$get_nodes()
  edges <- network$network$get_edges()
  layout_coords <- network$network$get_layout()

  # (oval layout uses elliptical spacing but nodes remain circular via aspect=TRUE)

  n_nodes <- nrow(nodes)
  n_edges <- if (!is.null(edges)) nrow(edges) else 0

  # Determine if directed
  if (is.null(directed)) {
    directed <- network$network$is_directed
  }

  # Check for duplicate edges in undirected networks
  if (!directed && !is.null(edges) && nrow(edges) > 0) {
    dup_check <- detect_duplicate_edges(edges)
    if (dup_check$has_duplicates) {
      if (is.null(edge_duplicates)) {
        # Build error message
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
      edges <- aggregate_duplicate_edges(edges, edge_duplicates)
      n_edges <- nrow(edges)
      # Update the network object with deduplicated edges
      network$network$set_edges(edges)
    }
  }

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
  # Handle "auto" scaling based on node count
  if (identical(layout_scale, "auto")) {
    # Auto-scale formula:
    # - Small networks (<10): compact (0.8-0.9)
    # - Medium networks (10-30): normal (0.9-1.1)
    # - Large networks (>30): expanded (1.1-1.4)
    layout_scale <- 0.7 + 0.7 * (1 - exp(-n_nodes / 25))
  }

  if (is.numeric(layout_scale) && layout_scale != 1) {
    center <- colMeans(layout_mat)
    layout_mat <- t(t(layout_mat - center) * layout_scale + center)
  }

  # ============================================
  # 3. PARAMETER VECTORIZATION
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

  # Node shapes
  # Handle custom SVG if provided
  if (!is.null(node_svg)) {
    # Register SVG as a temporary shape
    temp_svg_name <- paste0("_splot_svg_", format(Sys.time(), "%H%M%S"))
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
  # 4. EDGE PROCESSING
  # ============================================

  # Use minimum threshold or explicit threshold
  effective_threshold <- max(threshold, minimum)

  if (n_edges > 0) {
    # Filter by minimum weight (threshold)
    orig_n_edges <- n_edges
    orig_weights <- edges$weight
    edges <- filter_edges_by_weight(edges, effective_threshold)
    n_edges <- nrow(edges)

    # Subset edge_labels to match filtered edges
    if (n_edges < orig_n_edges && is.character(edge_labels) && length(edge_labels) == orig_n_edges) {
      keep_idx <- which(abs(orig_weights) >= effective_threshold)
      edge_labels <- edge_labels[keep_idx]
    }
  }

  # ============================================
  # EDGE CURVING BEHAVIOR
  # ============================================
  # curves = TRUE (default): single edges straight, reciprocal edges curved
  # curves = "force": all edges curved
  # curves = FALSE: all edges straight
  #
  # NOTE: We no longer duplicate edges for undirected networks.
  # Only edges with actual reciprocal pairs (A→B AND B→A) will curve.

  if (n_edges > 0) {
    # Edge colors
    edge_colors <- resolve_edge_colors(edges, edge_color, positive_color, negative_color)

    # Apply edge alpha
    if (edge_alpha < 1) {
      edge_colors <- sapply(edge_colors, function(c) adjust_alpha(c, edge_alpha))
    }

    # Apply cut threshold for transparency: edges below cut are faded
    if (!is.null(cut) && cut > 0 && "weight" %in% names(edges)) {
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

    # Edge widths
    edge_widths <- resolve_edge_widths(
      edges = edges,
      edge.width = edge_width,
      esize = esize,
      n_nodes = n_nodes,
      directed = directed,
      maximum = maximum,
      minimum = threshold,
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
    # "force" = all edges curved (reciprocals opposite, singles inward)
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
    curve_magnitude <- if (curvature == 0) 0.175 else abs(curvature)

    # Initialize curves vector to 0 (straight)
    curves_vec <- rep(0, n_edges)

    # Calculate network center for curve direction
    center_x <- mean(layout_mat[, 1])
    center_y <- mean(layout_mat[, 2])

    if (identical(curves, TRUE) || identical(curves, "mutual")) {
      # Curve reciprocal edges in opposite directions.
      # Use canonical ordering (lower node index first) so both edges in a pair
      # compute the same perpendicular reference, then assign opposite signs.
      for (i in seq_len(n_edges)) {
        if (is_reciprocal[i]) {
          from_idx <- edges$from[i]
          to_idx <- edges$to[i]

          # Canonical direction: always compute perp from lower-index to higher-index node
          lo <- min(from_idx, to_idx)
          hi <- max(from_idx, to_idx)
          dx_canon <- layout_mat[hi, 1] - layout_mat[lo, 1]
          dy_canon <- layout_mat[hi, 2] - layout_mat[lo, 2]

          # Perpendicular vector (consistent for both edges in the pair)
          perp_x <- -dy_canon
          perp_y <- dx_canon

          # Check if positive perp moves outward from center
          mid_x <- (layout_mat[from_idx, 1] + layout_mat[to_idx, 1]) / 2
          mid_y <- (layout_mat[from_idx, 2] + layout_mat[to_idx, 2]) / 2
          test_x <- mid_x + perp_x * 0.1
          test_y <- mid_y + perp_y * 0.1
          dist_to_center_pos <- sqrt((test_x - center_x)^2 + (test_y - center_y)^2)
          dist_to_center_orig <- sqrt((mid_x - center_x)^2 + (mid_y - center_y)^2)
          outward_sign <- if (dist_to_center_pos > dist_to_center_orig) 1 else -1

          # Both edges get the same sign. The renderer computes perp from the
          # edge's own from->to direction, which flips for hi->lo vs lo->hi.
          # Same sign + flipped perp = opposite curve directions.
          curves_vec[i] <- outward_sign * curve_magnitude
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

  # Set up plot area - only save/restore parameters we modify
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)

  # Margins
  title_space <- if (!is.null(title)) 0.5 else 0
  graphics::par(mar = c(margins[1], margins[2], margins[3] + title_space, margins[4]))

  # Calculate plot limits
  x_range <- range(layout_mat[, 1], na.rm = TRUE)
  y_range <- range(layout_mat[, 2], na.rm = TRUE)

  # Add margin to limits (configurable via layout_margin parameter)
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
  # donut_fill takes precedence over donut_values for the new simplified API
  effective_donut_values <- donut_values
  if (!is.null(donut_fill)) {
    # Convert donut_fill to list format for internal use
    if (!is.list(donut_fill)) {
      fill_vec <- recycle_to_length(donut_fill, n_nodes)
      effective_donut_values <- as.list(fill_vec)
    } else {
      effective_donut_values <- donut_fill
    }
  }

  # When donut_empty = TRUE, replace NA values with 0 so empty rings still render
  if (donut_empty && !is.null(effective_donut_values)) {
    for (di in seq_along(effective_donut_values)) {
      if (length(effective_donut_values[[di]]) == 1 && is.na(effective_donut_values[[di]])) {
        effective_donut_values[[di]] <- 0
      }
    }
  }

  # Handle donut_color (new simplified API) and donut_colors (deprecated)
  # Priority: donut_color > donut_colors
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
    effective_donut_colors <- as.list(rep("maroon", n_nodes))
  }

  # Determine effective donut shapes - inherit from node_shape by default
  # If donut_shape is NULL or "circle" (default), inherit from node_shape
  # Otherwise, use the explicitly set donut_shape
  valid_donut_base_shapes <- c("circle", "square", "hexagon", "triangle", "diamond", "pentagon")
  if (is.null(donut_shape) || identical(donut_shape, "circle")) {
    # Inherit from node_shape, but only if it's a valid donut base shape
    # donut, donut_pie, double_donut_pie and custom SVG shapes default to "circle"
    special_donut_shapes <- c("donut", "donut_pie", "double_donut_pie")
    effective_donut_shapes <- ifelse(
      shapes %in% valid_donut_base_shapes,
      shapes,
      "circle"  # Default for SVG shapes and special shapes
    )
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

  # Vectorize donut_outer_border_color for per-node support (double border feature)
  effective_donut_outer_border_color <- if (!is.null(donut_outer_border_color)) {
    recycle_to_length(donut_outer_border_color, n_nodes)
  } else {
    NULL
  }

  # Vectorize donut_border_lty for per-node support
  effective_donut_border_lty <- recycle_to_length(donut_border_lty, n_nodes)

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
    donut_outer_border_color = effective_donut_outer_border_color,
    donut_border_lty = effective_donut_border_lty,
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


#' Render Edges for splot
#' @keywords internal
render_edges_splot <- function(edges, layout, node_sizes, shapes,
                               edge_color, edge_width, edge_style, curvature,
                               curve_shape, curve_pivot, show_arrows, arrow_size,
                               bidirectional, loop_rotation, edge_labels,
                               edge_label_size, edge_label_color, edge_label_bg,
                               edge_label_position, edge_label_offset = 0,
                               edge_label_fontface,
                               edge_label_shadow = FALSE, edge_label_shadow_color = "gray40",
                               edge_label_shadow_offset = 0.5, edge_label_shadow_alpha = 0.5,
                               edge_ci = NULL, edge_ci_scale = 2.0,
                               edge_ci_alpha = 0.15, edge_ci_color = NULL,
                               edge_ci_style = 2, edge_ci_arrows = FALSE,
                               is_reciprocal = NULL,
                               edge_start_style = "solid", edge_start_length = 0.15) {

  m <- nrow(edges)
  if (m == 0) return(invisible())

  n <- nrow(layout)

  # Calculate network center for inward curve direction
  center_x <- mean(layout[, 1])
  center_y <- mean(layout[, 2])

  # Get render order (weakest to strongest)
  order_idx <- get_edge_order(edges)

  # Storage for label positions
  label_positions <- vector("list", m)

  # Convert edge_start_style to numeric lty
  start_lty <- switch(edge_start_style,
    "solid" = 1,
    "dashed" = 2,
    "dotted" = 3,
    1  # default to solid
  )
  start_fraction <- if (start_lty == 1) 0 else edge_start_length

  # Helper function to calculate curve direction (bend INWARD toward center)
  calc_curve_direction <- function(curve_val, start_x, start_y, end_x, end_y) {
    if (curve_val > 1e-6) {
      mid_x <- (start_x + end_x) / 2
      mid_y <- (start_y + end_y) / 2
      dx <- end_x - start_x
      dy <- end_y - start_y
      to_center_x <- center_x - mid_x
      to_center_y <- center_y - mid_y

      # Perpendicular to edge direction (same as draw_curved_edge_base)
      # Clockwise rotation: (dx, dy) -> (dy, -dx)
      len <- sqrt(dx^2 + dy^2)
      if (len < 1e-10) return(curve_val)
      px <- dy / len
      py <- -dx / len

      # Dot product: positive = perpendicular points toward center
      dot <- px * to_center_x + py * to_center_y

      if (dot < 0) -abs(curve_val) else abs(curve_val)
    } else {
      curve_val
    }
  }

  for (i in order_idx) {
    from_idx <- edges$from[i]
    to_idx <- edges$to[i]

    x1 <- layout[from_idx, 1]
    y1 <- layout[from_idx, 2]
    x2 <- layout[to_idx, 1]
    y2 <- layout[to_idx, 2]

    # Self-loop
    if (from_idx == to_idx) {
      # PASS 1: Draw CI underlay for self-loop (if edge_ci provided)
      if (!is.null(edge_ci) && !is.na(edge_ci[i]) && edge_ci[i] > 0) {
        underlay_width <- edge_width[i] * (1 + edge_ci[i] * edge_ci_scale)
        underlay_col <- if (!is.null(edge_ci_color)) edge_ci_color[i] else edge_color[i]
        underlay_col <- adjust_alpha(underlay_col, edge_ci_alpha)

        draw_self_loop_base(
          x1, y1, node_sizes[from_idx],
          col = underlay_col,
          lwd = underlay_width,
          lty = edge_ci_style,
          rotation = loop_rotation[i],
          arrow = edge_ci_arrows,
          asize = arrow_size[i]
        )
      }

      # PASS 2: Draw main self-loop
      draw_self_loop_base(
        x1, y1, node_sizes[from_idx],
        col = edge_color[i],
        lwd = edge_width[i],
        lty = edge_style[i],
        rotation = loop_rotation[i],
        arrow = show_arrows[i],
        asize = arrow_size[i]
      )

      # Label position for self-loop
      loop_dist <- node_sizes[from_idx] * 2.5
      label_positions[[i]] <- list(
        x = x1 + loop_dist * cos(loop_rotation[i]),
        y = y1 + loop_dist * sin(loop_rotation[i])
      )
      next
    }

    # Calculate edge endpoints
    angle_to <- splot_angle(x1, y1, x2, y2)
    angle_from <- splot_angle(x2, y2, x1, y1)

    start <- cent_to_edge(x1, y1, angle_to, node_sizes[from_idx], NULL, shapes[from_idx])
    end <- cent_to_edge(x2, y2, angle_from, node_sizes[to_idx], NULL, shapes[to_idx])

    # Determine curve direction
    # For reciprocal edges, use pre-computed curvature directly (preserves opposite directions)
    # For non-reciprocal edges, apply inward curve direction adjustment
    if (!is.null(is_reciprocal) && is_reciprocal[i]) {
      curve_i <- curvature[i]
    } else {
      curve_i <- calc_curve_direction(curvature[i], start$x, start$y, end$x, end$y)
    }

    # PASS 1: Draw CI underlay (if edge_ci provided)
    if (!is.null(edge_ci) && !is.na(edge_ci[i]) && edge_ci[i] > 0) {
      underlay_width <- edge_width[i] * (1 + edge_ci[i] * edge_ci_scale)
      underlay_col <- if (!is.null(edge_ci_color)) edge_ci_color[i] else edge_color[i]
      underlay_col <- adjust_alpha(underlay_col, edge_ci_alpha)

      if (abs(curve_i) > 1e-6) {
        draw_curved_edge_base(
          start$x, start$y, end$x, end$y,
          curve = curve_i,
          curvePivot = curve_pivot[i],
          col = underlay_col,
          lwd = underlay_width,
          lty = edge_ci_style,
          arrow = edge_ci_arrows,
          asize = arrow_size[i],
          bidirectional = FALSE
        )
      } else {
        draw_straight_edge_base(
          start$x, start$y, end$x, end$y,
          col = underlay_col,
          lwd = underlay_width,
          lty = edge_ci_style,
          arrow = edge_ci_arrows,
          asize = arrow_size[i],
          bidirectional = FALSE
        )
      }
    }

    # PASS 2: Draw main edge
    if (abs(curve_i) > 1e-6) {
      draw_curved_edge_base(
        start$x, start$y, end$x, end$y,
        curve = curve_i,
        curvePivot = curve_pivot[i],
        col = edge_color[i],
        lwd = edge_width[i],
        lty = edge_style[i],
        arrow = show_arrows[i],
        asize = arrow_size[i],
        bidirectional = bidirectional[i],
        start_lty = start_lty,
        start_fraction = start_fraction
      )
    } else {
      draw_straight_edge_base(
        start$x, start$y, end$x, end$y,
        col = edge_color[i],
        lwd = edge_width[i],
        lty = edge_style[i],
        arrow = show_arrows[i],
        asize = arrow_size[i],
        bidirectional = bidirectional[i],
        start_lty = start_lty,
        start_fraction = start_fraction
      )
    }

    # Store label position (use adjusted curve for correct position)
    label_positions[[i]] <- get_edge_label_position(
      start$x, start$y, end$x, end$y,
      position = edge_label_position,
      curve = curve_i,
      curvePivot = curve_pivot[i],
      label_offset = edge_label_offset
    )
  }

  # Draw edge labels
  if (!is.null(edge_labels)) {
    for (i in seq_len(m)) {
      if (!is.null(edge_labels[i]) && !is.na(edge_labels[i]) && edge_labels[i] != "") {
        pos <- label_positions[[i]]
        draw_edge_label_base(
          pos$x, pos$y,
          label = edge_labels[i],
          cex = edge_label_size,
          col = edge_label_color,
          bg = edge_label_bg,
          font = edge_label_fontface,
          shadow = edge_label_shadow,
          shadow_color = edge_label_shadow_color,
          shadow_offset = edge_label_shadow_offset,
          shadow_alpha = edge_label_shadow_alpha
        )
      }
    }
  }
}


#' Render Nodes for splot
#'
#' @param donut_values List of values for donut chart. Each element is a single
#'   numeric (0-1) representing fill proportion for that node.
#' @keywords internal
render_nodes_splot <- function(layout, node_size, node_size2, node_shape, node_fill,
                               node_border_color, node_border_width, pie_values, pie_colors,
                               pie_border_width, donut_values, donut_colors,
                               donut_border_color, donut_border_width,
                               donut_outer_border_color = NULL, donut_border_lty = 1,
                               donut_inner_ratio, donut_bg_color, donut_shape,
                               donut_show_value, donut_value_size, donut_value_color,
                               donut_value_fontface = "bold", donut_value_fontfamily = "sans",
                               donut_value_digits = 2, donut_value_prefix = "",
                               donut_value_suffix = "",
                               donut2_values, donut2_colors, donut2_inner_ratio,
                               labels, label_size, label_color, label_position,
                               label_fontface = "plain", label_fontfamily = "sans",
                               label_hjust = 0.5, label_vjust = 0.5, label_angle = 0,
                               usePCH = FALSE) {

  n <- nrow(layout)
  if (n == 0) return(invisible())

  # Render order: largest to smallest
  order_idx <- get_node_order(node_size)

  for (i in order_idx) {
    x <- layout[i, 1]
    y <- layout[i, 2]

    # Check for pie/donut/donut2
    has_pie <- !is.null(pie_values) && length(pie_values) >= i && !is.null(pie_values[[i]]) && length(pie_values[[i]]) > 0
    # Check for donut: either node_shape is "donut" OR donut_values has a valid (non-NA) value
    has_donut <- (node_shape[i] == "donut") ||
                 (!is.null(donut_values) && length(donut_values) >= i &&
                  !is.null(donut_values[[i]]) && length(donut_values[[i]]) > 0 && !anyNA(donut_values[[i]]))
    has_donut2 <- !is.null(donut2_values) && length(donut2_values) >= i && !is.null(donut2_values[[i]])

    if (has_donut2 || (has_donut && has_pie)) {
      # Double donut with optional inner pie
      # Or single donut with pie - both use the layered drawing approach
      if (has_donut2) {
        # Double donut case
        donut_vals <- if (has_donut) donut_values[[i]] else NULL
        donut_cols <- if (!is.null(donut_colors) && length(donut_colors) >= i) donut_colors[[i]] else NULL
        donut2_vals <- donut2_values[[i]]
        donut2_cols <- if (!is.null(donut2_colors) && length(donut2_colors) >= i) donut2_colors[[i]] else NULL
        pie_vals <- if (has_pie) pie_values[[i]] else NULL
        pie_cols <- if (!is.null(pie_colors) && length(pie_colors) >= i) pie_colors[[i]] else NULL

        draw_double_donut_pie_node_base(
          x, y, node_size[i],
          donut_values = donut_vals,
          donut_colors = donut_cols,
          donut2_values = donut2_vals,
          donut2_colors = donut2_cols,
          pie_values = pie_vals,
          pie_colors = pie_cols,
          pie_default_color = node_fill[i],
          outer_inner_ratio = donut_inner_ratio,
          inner_inner_ratio = donut2_inner_ratio,
          bg_color = donut_bg_color,
          border.col = node_border_color[i],
          border.width = node_border_width[i],
          pie_border.width = pie_border_width,
          donut_border.width = donut_border_width
        )
      } else {
        # Single donut with pie
        donut_val <- if (length(donut_values[[i]]) == 1) donut_values[[i]] else 1
        donut_col <- if (!is.null(donut_colors) && length(donut_colors) >= i) donut_colors[[i]][1] else node_fill[i]
        pie_vals <- pie_values[[i]]
        pie_cols <- if (!is.null(pie_colors) && length(pie_colors) >= i) pie_colors[[i]] else NULL

        draw_donut_pie_node_base(
          x, y, node_size[i],
          donut_value = donut_val,
          donut_color = donut_col,
          pie_values = pie_vals,
          pie_colors = pie_cols,
          pie_default_color = node_fill[i],
          inner_ratio = donut_inner_ratio,
          bg_color = donut_bg_color,
          border.col = node_border_color[i],
          border.width = node_border_width[i],
          pie_border.width = pie_border_width,
          donut_border.width = donut_border_width
        )
      }

    } else if (has_donut) {
      # Donut only
      # Get donut value, defaulting to 1.0 if node_shape is "donut" but no explicit value
      donut_vals <- if (!is.null(donut_values) && length(donut_values) >= i &&
                        !is.null(donut_values[[i]]) && length(donut_values[[i]]) > 0 && !anyNA(donut_values[[i]])) {
        donut_values[[i]]
      } else {
        1.0  # Default to full ring when node_shape is "donut" but no explicit value
      }
      donut_cols <- if (!is.null(donut_colors) && length(donut_colors) >= i) donut_colors[[i]] else NULL

      # Get per-node donut shape (donut_shape is now a vector)
      current_donut_shape <- if (length(donut_shape) >= i) donut_shape[i] else "circle"

      # Determine effective donut border color (use donut_border_color[i] if set, else node_border_color)
      effective_donut_border_col <- if (!is.null(donut_border_color) && length(donut_border_color) >= i) {
        donut_border_color[i]
      } else {
        node_border_color[i]
      }

      # Get per-node outer border color (for double border feature)
      effective_outer_border_col <- if (!is.null(donut_outer_border_color) && length(donut_outer_border_color) >= i) {
        donut_outer_border_color[i]
      } else {
        NULL
      }

      # Get per-node border line type
      effective_border_lty <- if (length(donut_border_lty) >= i) donut_border_lty[i] else 1

      if (current_donut_shape != "circle") {
        # Use polygon donut for non-circular shapes
        draw_polygon_donut_node_base(
          x, y, node_size[i],
          values = donut_vals,
          colors = donut_cols,
          default_color = node_fill[i],
          inner_ratio = donut_inner_ratio,
          bg_color = donut_bg_color,
          center_color = node_fill[i],
          donut_shape = current_donut_shape,
          border.col = effective_donut_border_col,
          border.width = node_border_width[i],
          donut_border.width = donut_border_width,
          outer_border.col = effective_outer_border_col,
          border.lty = effective_border_lty,
          show_value = donut_show_value,
          value_cex = donut_value_size,
          value_col = donut_value_color,
          value_fontface = donut_value_fontface,
          value_fontfamily = donut_value_fontfamily,
          value_digits = donut_value_digits,
          value_prefix = donut_value_prefix,
          value_suffix = donut_value_suffix
        )
      } else {
        # Use circular donut (default)
        draw_donut_node_base(
          x, y, node_size[i],
          values = donut_vals,
          colors = donut_cols,
          default_color = node_fill[i],
          inner_ratio = donut_inner_ratio,
          bg_color = donut_bg_color,
          center_color = node_fill[i],
          border.col = effective_donut_border_col,
          border.width = node_border_width[i],
          donut_border.width = donut_border_width,
          outer_border.col = effective_outer_border_col,
          border.lty = effective_border_lty,
          show_value = donut_show_value,
          value_cex = donut_value_size,
          value_col = donut_value_color,
          value_fontface = donut_value_fontface,
          value_fontfamily = donut_value_fontfamily,
          value_digits = donut_value_digits,
          value_prefix = donut_value_prefix,
          value_suffix = donut_value_suffix
        )
      }

    } else if (has_pie) {
      # Pie only
      pie_vals <- pie_values[[i]]
      pie_cols <- if (!is.null(pie_colors) && length(pie_colors) >= i) pie_colors[[i]] else NULL

      draw_pie_node_base(
        x, y, node_size[i],
        values = pie_vals,
        colors = pie_cols,
        default_color = node_fill[i],
        border.col = node_border_color[i],
        border.width = node_border_width[i],
        pie_border.width = pie_border_width
      )

    } else {
      # Standard node
      if (usePCH && node_shape[i] == "circle") {
        # Fast point-based rendering
        graphics::points(x, y, pch = 21, cex = node_size[i] * 20,
                         bg = node_fill[i], col = node_border_color[i], lwd = node_border_width[i])
      } else {
        draw_node_base(
          x, y, node_size[i], node_size2[i],
          shape = node_shape[i],
          col = node_fill[i],
          border.col = node_border_color[i],
          border.width = node_border_width[i]
        )
      }
    }
  }

  # Render labels
  if (!is.null(labels)) {
    for (i in seq_len(n)) {
      if (!is.null(labels[i]) && !is.na(labels[i]) && labels[i] != "") {
        lx <- layout[i, 1]
        ly <- layout[i, 2]

        # Adjust position based on label_position
        offset <- node_size[i] * 1.2
        pos <- NULL

        if (label_position == "above") {
          ly <- ly + offset
        } else if (label_position == "below") {
          ly <- ly - offset
        } else if (label_position == "left") {
          lx <- lx - offset
        } else if (label_position == "right") {
          lx <- lx + offset
        }
        # "center" - no offset

        # Convert fontface string to numeric
        fontface_num <- switch(label_fontface,
          "plain" = 1,
          "bold" = 2,
          "italic" = 3,
          "bold.italic" = 4,
          1
        )

        draw_node_label_base(
          lx, ly,
          label = labels[i],
          cex = label_size[i],
          col = label_color[i],
          font = fontface_num,
          family = label_fontfamily,
          hjust = label_hjust,
          vjust = label_vjust,
          srt = label_angle
        )
      }
    }
  }
}


#' Render Legend for splot
#'
#' Renders a comprehensive legend showing node groups, edge weight colors,
#' and optionally node sizes.
#'
#' @param groups Group assignments for nodes.
#' @param node_names Names for legend entries.
#' @param nodes Node data frame.
#' @param node_colors Vector of node colors.
#' @param position Legend position.
#' @param cex Text size.
#' @param show_edge_colors Logical: show positive/negative edge color legend?
#' @param positive_color Positive edge color.
#' @param negative_color Negative edge color.
#' @param has_pos_edges Logical: are there positive weighted edges?
#' @param has_neg_edges Logical: are there negative weighted edges?
#' @param show_node_sizes Logical: show node size legend?
#' @param node_size Vector of node sizes.
#' @keywords internal
render_legend_splot <- function(groups, node_names, nodes, node_colors,
                                position = "topright", cex = 0.8,
                                show_edge_colors = FALSE,
                                positive_color = "#2E7D32", negative_color = "#C62828",
                                has_pos_edges = FALSE, has_neg_edges = FALSE,
                                show_node_sizes = FALSE, node_size = NULL) {

  n <- length(node_colors)

  # Collect all legend components
  legend_labels <- character(0)
  legend_colors <- character(0)
  legend_pch <- integer(0)
  legend_lty <- integer(0)
  legend_lwd <- numeric(0)
  legend_pt_cex <- numeric(0)

  # =========================================
  # 1. NODE GROUPS (filled squares)
  # =========================================
  if (!is.null(groups)) {
    unique_groups <- unique(groups)

    # Get color for each group (first node of that group)
    group_colors <- sapply(unique_groups, function(g) {
      idx <- which(groups == g)[1]
      node_colors[idx]
    })

    group_labels <- if (!is.null(node_names)) {
      sapply(unique_groups, function(g) {
        idx <- which(groups == g)[1]
        if (length(node_names) >= idx) node_names[idx] else as.character(g)
      })
    } else {
      as.character(unique_groups)
    }

    legend_labels <- c(legend_labels, group_labels)
    legend_colors <- c(legend_colors, group_colors)
    legend_pch <- c(legend_pch, rep(22, length(unique_groups)))  # filled square
    legend_lty <- c(legend_lty, rep(NA, length(unique_groups)))
    legend_lwd <- c(legend_lwd, rep(NA, length(unique_groups)))
    legend_pt_cex <- c(legend_pt_cex, rep(2, length(unique_groups)))
  }

  # =========================================
  # 2. EDGE COLORS (lines)
  # =========================================
  if (show_edge_colors && (has_pos_edges || has_neg_edges)) {
    # Add separator if we have groups
    if (length(legend_labels) > 0) {
      legend_labels <- c(legend_labels, "")
      legend_colors <- c(legend_colors, NA)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 0)
      legend_lwd <- c(legend_lwd, NA)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }

    if (has_pos_edges) {
      legend_labels <- c(legend_labels, "Positive")
      legend_colors <- c(legend_colors, positive_color)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 1)
      legend_lwd <- c(legend_lwd, 2)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }

    if (has_neg_edges) {
      legend_labels <- c(legend_labels, "Negative")
      legend_colors <- c(legend_colors, negative_color)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 1)
      legend_lwd <- c(legend_lwd, 2)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }
  }

  # =========================================
  # 3. NODE SIZES (circles of different sizes)
  # =========================================
  if (show_node_sizes && !is.null(node_size) && length(unique(node_size)) > 1) {
    # Add separator
    if (length(legend_labels) > 0) {
      legend_labels <- c(legend_labels, "")
      legend_colors <- c(legend_colors, NA)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 0)
      legend_lwd <- c(legend_lwd, NA)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }

    # Show min, median, max sizes
    size_range <- range(node_size)
    size_med <- median(node_size)
    size_vals <- c(size_range[1], size_med, size_range[2])
    size_labels <- c(
      paste0("Small (", round(size_range[1], 1), ")"),
      paste0("Medium (", round(size_med, 1), ")"),
      paste0("Large (", round(size_range[2], 1), ")")
    )

    # Scale for legend display
    scale_factor <- 15  # Adjust for visual appearance
    size_cex <- size_vals * scale_factor

    legend_labels <- c(legend_labels, size_labels)
    legend_colors <- c(legend_colors, rep("gray50", 3))
    legend_pch <- c(legend_pch, rep(21, 3))  # filled circle
    legend_lty <- c(legend_lty, rep(NA, 3))
    legend_lwd <- c(legend_lwd, rep(NA, 3))
    legend_pt_cex <- c(legend_pt_cex, size_cex)
  }

  # =========================================
  # Draw legend if we have entries
  # =========================================
  if (length(legend_labels) == 0) {
    return(invisible())
  }

  # Replace NA colors with transparent for proper rendering
  legend_colors[is.na(legend_colors)] <- "transparent"

  # Determine which elements to show
  has_points <- any(!is.na(legend_pch) & legend_pch > 0)
  has_lines <- any(!is.na(legend_lty) & legend_lty > 0)

  # Build legend
  graphics::legend(
    position,
    legend = legend_labels,
    col = legend_colors,
    pch = if (has_points) legend_pch else NULL,
    lty = if (has_lines) legend_lty else NULL,
    lwd = if (has_lines) legend_lwd else NULL,
    pt.cex = if (has_points) legend_pt_cex else NULL,
    pt.bg = if (has_points) legend_colors else NULL,
    bty = "o",
    bg = "white",
    cex = cex,
    seg.len = 1.5
  )
}
