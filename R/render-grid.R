#' @title Grid Rendering
#' @description Main grid-based rendering functions.
#' @name render-grid
NULL

#' Plot Sonnet Network
#'
#' Main plotting function for Sonnet networks. Renders the network visualization
#' using grid graphics. Accepts all node and edge aesthetic parameters.
#'
#' @param network A sonnet_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param title Optional plot title.
#' @param title_size Title font size.
#' @param margins Plot margins as c(bottom, left, top, right).
#' @param layout_margin Margin around the network layout (proportion of viewport). Default 0.15.
#' @param newpage Logical. Start a new graphics page? Default TRUE.
#' @param layout Layout algorithm. Built-in: "circle", "spring", "groups", "grid",
#'   "random", "star", "bipartite". igraph (2-letter): "kk" (Kamada-Kawai),
#'   "fr" (Fruchterman-Reingold), "drl", "mds", "ni" (nicely), "tr" (tree), etc.
#'   Can also pass a coordinate matrix or igraph layout function directly.
#' @param theme Theme name: "classic", "dark", "minimal", etc.
#' @param seed Random seed for deterministic layouts. Default 42. Set NULL for random.
#' @param labels Node labels. Can be a character vector to set custom labels.
#' @param weight_digits Number of decimal places to round edge weights to before
#'   plotting. Edges that round to zero are automatically removed. Default 2.
#'   Set NULL to disable rounding.
#' @param threshold Minimum absolute edge weight to display. Edges with
#'   abs(weight) < threshold are hidden. Similar to qgraph's threshold.
#' @param maximum Maximum edge weight for width scaling. Weights above this
#'   are capped. Similar to qgraph's maximum parameter.
#'
#' @param node_size Node size.
#' @param node_shape Node shape: "circle", "square", "triangle", "diamond",
#'   "ellipse", "heart", "star", "pie", "donut", "cross".
#' @param node_fill Node fill color.
#' @param node_border_color Node border color.
#' @param node_border_width Node border width.
#' @param node_alpha Node transparency (0-1).
#' @param label_size Node label text size.
#' @param label_color Node label text color.
#' @param label_position Label position: "center", "above", "below", "left", "right".
#' @param show_labels Logical. Show node labels?
#' @param pie_values For pie/donut/donut_pie nodes: list or matrix of values for segments.
#'   For donut with single value (0-1), shows that proportion filled.
#' @param pie_colors For pie/donut/donut_pie nodes: colors for pie segments.
#' @param pie_border_width Border width for pie chart segments.
#' @param donut_values For donut_pie nodes: vector of values (0-1) for outer ring proportion.
#' @param donut_border_width Border width for donut ring.
#' @param donut_inner_ratio For donut nodes: inner radius ratio (0-1). Default 0.5.
#' @param donut_bg_color For donut nodes: background color for unfilled portion.
#' @param donut_show_value For donut nodes: show value in center? Default FALSE.
#' @param donut_value_size For donut nodes: font size for center value.
#' @param donut_value_color For donut nodes: color for center value text.
#' @param donut_fill Numeric value (0-1) for donut fill proportion. This is the
#'   simplified API for creating donut charts. Can be a single value or vector per node.
#' @param donut_color Fill color(s) for the donut ring. Simplified API:
#'   single color for fill, or c(fill, background) for both.
#' @param donut_colors Deprecated. Use donut_color instead.
#' @param donut_shape Base shape for donut: "circle", "square", "hexagon", "triangle",
#'   "diamond", "pentagon". Default inherits from node_shape.
#' @param donut_value_fontface Font face for donut center value: "plain", "bold",
#'   "italic", "bold.italic". Default "bold".
#' @param donut_value_fontfamily Font family for donut center value. Default "sans".
#' @param donut_value_digits Decimal places for donut center value. Default 2.
#' @param donut_value_prefix Text before donut center value (e.g., "$"). Default "".
#' @param donut_value_suffix Text after donut center value (e.g., "%"). Default "".
#' @param donut2_values List of values for inner donut ring (for double donut).
#' @param donut2_colors List of color vectors for inner donut ring segments.
#' @param donut2_inner_ratio Inner radius ratio for inner donut ring. Default 0.4.
#'
#' @param edge_width Edge width. If NULL, scales by weight using edge_size and edge_width_range.
#' @param edge_size Base edge size for weight scaling. NULL (default) uses adaptive sizing
#'   based on network size: `15 * exp(-n_nodes/90) + 1`. Larger values = thicker edges.
#' @param esize Deprecated. Use `edge_size` instead.
#' @param edge_width_range Output width range as c(min, max) for weight-based scaling.
#'   Default c(0.5, 4). Edges are scaled to fit within this range.
#' @param edge_scale_mode Scaling mode for edge weights: "linear" (default),
#'   "log" (for wide weight ranges), "sqrt" (moderate compression),
#'   or "rank" (equal visual spacing).
#' @param edge_cutoff Two-tier cutoff for edge width scaling. NULL (default) = auto 75th percentile.
#'   0 = disabled. Positive number = manual threshold.
#' @param cut Deprecated. Use `edge_cutoff` instead.
#' @param edge_width_scale Scale factor for edge widths. Values > 1 make edges thicker.
#' @param edge_color Edge color.
#' @param edge_alpha Edge transparency (0-1).
#' @param edge_style Line style: "solid", "dashed", "dotted".
#' @param curvature Edge curvature amount.
#' @param arrow_size Size of arrow heads.
#' @param show_arrows Logical. Show arrows?
#' @param edge_positive_color Color for positive edge weights.
#' @param positive_color Deprecated. Use `edge_positive_color` instead.
#' @param edge_negative_color Color for negative edge weights.
#' @param negative_color Deprecated. Use `edge_negative_color` instead.
#' @param edge_duplicates How to handle duplicate edges in undirected networks.
#'   NULL (default) = stop with error listing duplicates. Options: "sum", "mean",
#'   "first", "max", "min", or a custom aggregation function.
#' @param edge_labels Edge labels. Can be TRUE to show weights, or a vector.
#' @param edge_label_size Edge label text size.
#' @param edge_label_color Edge label text color.
#' @param edge_label_position Position along edge (0 = source, 0.5 = middle, 1 = target).
#' @param edge_label_offset Perpendicular offset from edge line.
#' @param edge_label_bg Background color for edge labels (default "white"). Set to NA for transparent.
#' @param edge_label_fontface Font face: "plain", "bold", "italic", "bold.italic".
#' @param edge_label_border Border style: NULL, "rect", "rounded", "circle".
#' @param edge_label_border_color Border color for label border.
#' @param edge_label_underline Logical. Underline the label text?
#' @param bidirectional Logical. Show arrows at both ends of edges?
#' @param loop_rotation Angle in radians for self-loop direction (default: pi/2 = top).
#' @param curve_shape Spline tension for curved edges (-1 to 1, default: 0).
#' @param curve_pivot Pivot position along edge for curve control point (0-1, default: 0.5).
#' @param curves Curve mode: TRUE (default) = single edges straight, reciprocal edges
#'   curve as ellipse (two opposing curves); FALSE = all straight; "force" = all curved.
#' @param node_names Alternative names for legend (separate from display labels).
#' @param legend Logical. Show legend?
#' @param legend_position Legend position: "topright", "topleft", "bottomright", "bottomleft".
#' @param scaling Scaling mode: "default" for qgraph-matched scaling where node_size=6
#'   looks similar to qgraph vsize=6, or "legacy" to preserve pre-v2.0 behavior.
#'
#' @details
#' ## soplot vs splot
#' \code{soplot()} uses grid graphics while \code{splot()} uses base R graphics.
#' Both accept the same parameters and produce visually similar output. Choose based on:
#' \itemize{
#'   \item \strong{soplot}: Better for integration with ggplot2, combining plots,
#'     and publication-quality vector graphics.
#'   \item \strong{splot}: Better for large networks (faster rendering), interactive
#'     exploration, and traditional R workflows.
#' }
#'
#' ## Edge Curve Behavior
#' Edge curving is controlled by the \code{curves} and \code{curvature} parameters:
#' \describe{
#'   \item{\strong{curves = FALSE}}{All edges are straight lines.}
#'   \item{\strong{curves = TRUE}}{(Default) Reciprocal edge pairs (A\code{->}B and
#'     B\code{->}A) curve in opposite directions to form a visual ellipse. Single
#'     edges remain straight.}
#'   \item{\strong{curves = "force"}}{All edges curve inward toward the network center.}
#' }
#'
#' ## Weight Scaling Modes (edge_scale_mode)
#' Controls how edge weights map to visual widths:
#' \describe{
#'   \item{\strong{linear}}{Width proportional to weight. Best for similar-magnitude weights.}
#'   \item{\strong{log}}{Logarithmic scaling. Best for weights spanning orders of magnitude.}
#'   \item{\strong{sqrt}}{Square root scaling. Moderate compression for skewed data.}
#'   \item{\strong{rank}}{Rank-based scaling. Equal visual spacing regardless of values.}
#' }
#'
#' ## Donut Visualization
#' The donut system visualizes proportions (0-1) as filled rings around nodes:
#' \describe{
#'   \item{\strong{donut_fill}}{Proportion filled (0-1). Can be scalar or per-node vector.}
#'   \item{\strong{donut_color}}{Fill color. Single color, c(fill, bg), or per-node vector.}
#'   \item{\strong{donut_shape}}{Base shape: "circle", "square", "hexagon", etc.}
#'   \item{\strong{donut_show_value}}{Show numeric value in center.}
#' }
#'
#' @return Invisible NULL. Called for side effect of drawing.
#'
#' @seealso
#' \code{\link{splot}} for base R graphics rendering (alternative engine),
#' \code{\link{sonnet}} for creating network objects,
#' \code{\link{sn_nodes}} for node customization,
#' \code{\link{sn_edges}} for edge customization,
#' \code{\link{sn_layout}} for layout algorithms,
#' \code{\link{sn_theme}} for visual themes,
#' \code{\link{from_qgraph}} and \code{\link{from_tna}} for converting external objects
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' # With sonnet()
#' sonnet(adj) |> soplot()
#'
#' # Direct matrix input with all options
#' adj |> soplot(
#'   layout = "circle",
#'   node_fill = "steelblue",
#'   node_size = 0.08,
#'   edge_width = 2
#' )
soplot <- function(network, title = NULL, title_size = 14,
                      margins = c(0.05, 0.05, 0.1, 0.05),
                      layout_margin = 0.15,
                      newpage = TRUE,
                      # Layout and theme
                      layout = NULL,
                      theme = NULL,
                      seed = 42,
                      # Node labels
                      labels = NULL,
                      # Edge filtering/scaling
                      threshold = NULL,
                      maximum = NULL,
                      # Node aesthetics
                      node_size = NULL,
                      node_shape = NULL,
                      node_fill = NULL,
                      node_border_color = NULL,
                      node_border_width = NULL,
                      node_alpha = NULL,
                      # Node labels
                      label_size = NULL,
                      label_color = NULL,
                      label_position = NULL,
                      show_labels = NULL,
                      # Pie/donut chart nodes
                      pie_values = NULL,
                      pie_colors = NULL,
                      pie_border_width = NULL,
                      donut_values = NULL,
                      donut_border_width = NULL,
                      donut_inner_ratio = NULL,
                      donut_bg_color = NULL,
                      donut_show_value = NULL,
                      donut_value_size = NULL,
                      donut_value_color = NULL,
                      # NEW donut parameters for feature parity with splot
                      donut_fill = NULL,
                      donut_color = NULL,
                      donut_colors = NULL,  # Deprecated: use donut_color
                      donut_shape = "circle",
                      donut_value_fontface = "bold",
                      donut_value_fontfamily = "sans",
                      donut_value_digits = 2,
                      donut_value_prefix = "",
                      donut_value_suffix = "",
                      donut2_values = NULL,
                      donut2_colors = NULL,
                      donut2_inner_ratio = 0.4,
                      # Edge aesthetics
                      edge_width = NULL,
                      edge_size = NULL,
                      esize = NULL,  # Deprecated: use edge_size
                      edge_width_range = NULL,
                      edge_scale_mode = "linear",
                      edge_cutoff = NULL,
                      cut = NULL,  # Deprecated: use edge_cutoff
                      edge_width_scale = NULL,
                      edge_color = NULL,
                      edge_alpha = NULL,
                      edge_style = NULL,
                      curvature = NULL,
                      arrow_size = NULL,
                      show_arrows = NULL,
                      edge_positive_color = NULL,
                      positive_color = NULL,  # Deprecated: use edge_positive_color
                      edge_negative_color = NULL,
                      negative_color = NULL,  # Deprecated: use edge_negative_color
                      edge_duplicates = NULL,
                      # Edge labels
                      edge_labels = NULL,
                      edge_label_size = NULL,
                      edge_label_color = NULL,
                      edge_label_position = NULL,
                      edge_label_offset = NULL,
                      edge_label_bg = NULL,
                      edge_label_fontface = NULL,
                      edge_label_border = NULL,
                      edge_label_border_color = NULL,
                      edge_label_underline = NULL,
                      # Advanced edge options
                      bidirectional = NULL,
                      loop_rotation = NULL,
                      curve_shape = NULL,
                      curve_pivot = NULL,
                      curves = NULL,
                      # Legend options
                      node_names = NULL,
                      legend = FALSE,
                      legend_position = "topright",
                      # Scaling mode
                      scaling = "default",
                      weight_digits = 2) {


  # Handle tna objects directly
  if (inherits(network, "tna")) {
    tna_params <- from_tna(network, engine = "soplot", plot = FALSE)
    call_args <- tna_params
    # from_tna returns $x; soplot expects $network
    call_args$network <- call_args$x
    call_args$x <- NULL
    call_args$layout <- layout
    call_args$seed <- seed
    call_args$theme <- theme
    # Apply user overrides
    user_args <- as.list(match.call(expand.dots = FALSE))[-1]
    user_args$network <- NULL
    for (nm in names(user_args)) {
      val <- eval(user_args[[nm]], envir = parent.frame())
      if (!is.null(val)) call_args[[nm]] <- val
    }
    # Filter to accepted soplot params
    accepted <- names(formals(soplot))
    call_args <- call_args[intersect(names(call_args), accepted)]
    return(do.call(soplot, call_args))
  }

  # ============================================
  # HANDLE DEPRECATED PARAMETERS
  # ============================================
  # Detect which arguments were explicitly provided by the user
  explicit_args <- names(match.call())

  # For params with NULL defaults, simple check works
  edge_size <- handle_deprecated_param(edge_size, esize, "edge_size", "esize")
  edge_cutoff <- handle_deprecated_param(edge_cutoff, cut, "edge_cutoff", "cut")

  # For params with non-NULL defaults, use new_val_was_set to check if user explicitly set them
  edge_positive_color <- handle_deprecated_param(
    edge_positive_color, positive_color,
    "edge_positive_color", "positive_color",
    new_val_was_set = "edge_positive_color" %in% explicit_args
  )
  edge_negative_color <- handle_deprecated_param(
    edge_negative_color, negative_color,
    "edge_negative_color", "negative_color",
    new_val_was_set = "edge_negative_color" %in% explicit_args
  )

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Get scale constants for current scaling mode
  scale <- get_scale_constants(scaling)

  # Two-letter igraph layout codes
  igraph_codes <- c("kk", "fr", "drl", "mds", "go", "tr", "st", "gr", "rd", "ni", "ci", "lgl", "sp")

  # Determine effective layout
  effective_layout <- layout %||% "spring"

  # Round matrix weights to filter near-zero edges globally
  if (is.matrix(network) && !is.null(weight_digits)) {
    network <- round(network, weight_digits)
  }

  # Auto-convert matrix/data.frame/igraph to sonnet_network
  network <- ensure_sonnet_network(network, layout = effective_layout, seed = seed)

  # Check for duplicate edges in undirected networks
  net <- network$network
  directed <- net$is_directed
  edges <- net$get_edges()
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
      net$set_edges(edges)
    }
  }

  # Apply custom node labels if provided
  if (!is.null(labels)) {
    net <- network$network
    nodes_df <- net$get_nodes()
    if (length(labels) != nrow(nodes_df)) {
      stop("labels length (", length(labels), ") must match number of nodes (",
           nrow(nodes_df), ")", call. = FALSE)
    }
    nodes_df$label <- labels
    net$set_nodes(nodes_df)
  }

  # Apply threshold - filter out weak edges
 if (!is.null(threshold)) {
    net <- network$network
    edges_df <- net$get_edges()
    if (!is.null(edges_df) && nrow(edges_df) > 0 && !is.null(edges_df$weight)) {
      keep <- abs(edges_df$weight) >= threshold
      edges_df <- edges_df[keep, , drop = FALSE]
      net$set_edges(edges_df)
    }
  }

  # Apply layout if specified
 if (!is.null(layout)) {
    network <- sn_layout(network, layout)
  }

  # Apply theme if specified
  if (!is.null(theme)) {
    network <- sn_theme(network, theme)
  }

  # ============================================
  # DONUT PROCESSING (for feature parity with splot)
  # ============================================

  # Get node count for processing
  n_nodes <- nrow(network$network$get_nodes())

  # Get shapes for processing
  shapes <- recycle_to_length(node_shape %||% "circle", n_nodes)

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

  # Convert node_size using scale constants (qgraph-style to NPC)
  # If node_size is provided, convert it; otherwise let render_nodes_grid use default
  effective_node_size <- if (!is.null(node_size)) {
    # Convert from qgraph-style units to NPC coordinates
    node_size * scale$soplot_node_factor
  } else {
    # Use default from scale constants, converted to NPC
    scale$node_default * scale$soplot_node_factor
  }

  # Apply node aesthetics if any specified
  node_aes <- list(
    size = effective_node_size,
    shape = node_shape,
    fill = node_fill,
    border_color = node_border_color,
    border_width = node_border_width,
    alpha = node_alpha,
    label_size = label_size,
    label_color = label_color,
    label_position = label_position,
    show_labels = show_labels,
    pie_values = pie_values,
    pie_colors = pie_colors,
    pie_border_width = pie_border_width,
    # Use processed donut values for feature parity with splot
    donut_values = effective_donut_values,
    donut_colors = effective_donut_colors,
    donut_border_width = donut_border_width,
    donut_inner_ratio = donut_inner_ratio,
    donut_bg_color = effective_bg_color,
    donut_shape = effective_donut_shapes,
    donut_show_value = donut_show_value,
    donut_value_size = donut_value_size,
    donut_value_color = donut_value_color,
    # NEW donut value formatting parameters
    donut_value_fontface = donut_value_fontface,
    donut_value_fontfamily = donut_value_fontfamily,
    donut_value_digits = donut_value_digits,
    donut_value_prefix = donut_value_prefix,
    donut_value_suffix = donut_value_suffix,
    # Double donut parameters
    donut2_values = donut2_values,
    donut2_colors = donut2_colors,
    donut2_inner_ratio = donut2_inner_ratio,
    node_names = node_names
  )
  node_aes <- node_aes[!sapply(node_aes, is.null)]
  if (length(node_aes) > 0) {
    network <- do.call(sn_nodes, c(list(network = network), node_aes))
  }

  # Convert arrow_size using scale constants for consistency with splot
  effective_arrow_size <- if (!is.null(arrow_size)) {
    arrow_size * scale$arrow_factor
  } else {
    NULL  # Let render_edges_grid use default
  }

  # Apply edge aesthetics if any specified
  edge_aes <- list(
    width = edge_width,
    edge_size = edge_size,
    edge_width_range = edge_width_range,
    edge_scale_mode = edge_scale_mode,
    edge_cutoff = edge_cutoff,
    width_scale = edge_width_scale,
    color = edge_color,
    alpha = edge_alpha,
    style = edge_style,
    curvature = curvature,
    arrow_size = effective_arrow_size,
    show_arrows = show_arrows,
    edge_positive_color = edge_positive_color,
    edge_negative_color = edge_negative_color,
    maximum = maximum,
    labels = edge_labels,
    label_size = edge_label_size,
    label_color = edge_label_color,
    label_position = edge_label_position,
    label_offset = edge_label_offset,
    label_bg = edge_label_bg,
    label_fontface = edge_label_fontface,
    label_border = edge_label_border,
    label_border_color = edge_label_border_color,
    label_underline = edge_label_underline,
    bidirectional = bidirectional,
    loop_rotation = loop_rotation,
    curve_shape = curve_shape,
    curve_pivot = curve_pivot,
    curves = curves
  )
  edge_aes <- edge_aes[!sapply(edge_aes, is.null)]
  if (length(edge_aes) > 0) {
    network <- do.call(sn_edges, c(list(network = network), edge_aes))
  }

  net <- network$network
  th <- net$get_theme()

  # Rescale layout coordinates to [0.1, 0.9] range (same as splot)
  # This ensures consistent rendering between soplot and splot
  nodes <- net$get_nodes()
  if (!is.null(nodes) && nrow(nodes) > 0 && !is.null(nodes$x) && !is.null(nodes$y)) {
    x <- nodes$x
    y <- nodes$y

    # Handle single node case
    if (nrow(nodes) == 1) {
      nodes$x <- 0.5
      nodes$y <- 0.5
    } else {
      # Rescale to [0.1, 0.9] range
      x_range <- range(x, na.rm = TRUE)
      y_range <- range(y, na.rm = TRUE)

      # Uniform scaling to preserve aspect ratio
      margin <- layout_margin
      max_range <- max(diff(x_range), diff(y_range))
      if (max_range > 1e-10) {
        x_center <- mean(x_range)
        y_center <- mean(y_range)
        nodes$x <- 0.5 + (x - x_center) / max_range * (1 - 2 * margin)
        nodes$y <- 0.5 + (y - y_center) / max_range * (1 - 2 * margin)
      } else {
        nodes$x <- rep(0.5, nrow(nodes))
        nodes$y <- rep(0.5, nrow(nodes))
      }
    }

    net$set_nodes(nodes)
  }

  if (newpage) {
    grid::grid.newpage()
  }

  # Draw background
  bg_color <- if (!is.null(th)) th$get("background") else "white"
  grid::grid.rect(gp = grid::gpar(fill = bg_color, col = NA))

  # Create viewport with margins
  vp <- grid::viewport(
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.5, "npc"),
    width = grid::unit(1 - margins[2] - margins[4], "npc"),
    height = grid::unit(1 - margins[1] - margins[3], "npc")
  )
  grid::pushViewport(vp)

  # Render edges first (behind nodes)
  edge_grobs <- render_edges_grid(net)
  grid::grid.draw(edge_grobs)

  # Render edge labels
  edge_label_grobs <- render_edge_labels_grid(net)
  grid::grid.draw(edge_label_grobs)

  # Render nodes
  node_grobs <- render_nodes_grid(net)
  grid::grid.draw(node_grobs)

  # Render node labels
  label_grobs <- render_node_labels_grid(net)
  grid::grid.draw(label_grobs)

  # Render legend if requested
  if (isTRUE(legend)) {
    legend_grobs <- render_legend_grid(net, position = legend_position)
    grid::grid.draw(legend_grobs)
  }

  grid::popViewport()

  # Draw title if provided
  if (!is.null(title)) {
    title_col <- if (!is.null(th)) th$get("title_color") else "black"
    # Position title within the top margin, ensuring it's visible
    # Use at least 0.02 from the top edge to prevent clipping
    title_y <- 1 - max(margins[3] / 2, 0.02)
    grid::grid.text(
      title,
      x = grid::unit(0.5, "npc"),
      y = grid::unit(title_y, "npc"),
      gp = grid::gpar(fontsize = title_size, col = title_col, fontface = "bold")
    )
  }

  # Store all plot parameters in the network object
  plot_params <- list(
    title = title, title_size = title_size, margins = margins,
    layout = effective_layout, theme = theme, seed = seed, scaling = scaling,
    labels = labels, threshold = threshold, maximum = maximum,
    node_size = node_size, node_shape = node_shape, node_fill = node_fill,
    node_border_color = node_border_color, node_border_width = node_border_width,
    node_alpha = node_alpha, label_size = label_size, label_color = label_color,
    label_position = label_position, show_labels = show_labels,
    pie_values = pie_values, pie_colors = pie_colors, pie_border_width = pie_border_width,
    donut_fill = donut_fill, donut_values = donut_values,
    donut_color = donut_color, donut_colors = donut_colors,
    donut_border_width = donut_border_width,
    donut_inner_ratio = donut_inner_ratio, donut_bg_color = donut_bg_color,
    donut_shape = donut_shape,
    donut_show_value = donut_show_value, donut_value_size = donut_value_size,
    donut_value_color = donut_value_color,
    donut_value_fontface = donut_value_fontface,
    donut_value_fontfamily = donut_value_fontfamily,
    donut_value_digits = donut_value_digits,
    donut_value_prefix = donut_value_prefix,
    donut_value_suffix = donut_value_suffix,
    donut2_values = donut2_values, donut2_colors = donut2_colors,
    donut2_inner_ratio = donut2_inner_ratio,
    edge_width = edge_width, edge_size = edge_size,
    edge_width_range = edge_width_range, edge_scale_mode = edge_scale_mode,
    edge_cutoff = edge_cutoff, edge_width_scale = edge_width_scale, edge_color = edge_color,
    edge_alpha = edge_alpha, edge_style = edge_style,
    curvature = curvature, arrow_size = arrow_size, show_arrows = show_arrows,
    edge_positive_color = edge_positive_color, edge_negative_color = edge_negative_color,
    edge_labels = edge_labels, edge_label_size = edge_label_size,
    edge_label_color = edge_label_color, edge_label_position = edge_label_position,
    edge_label_offset = edge_label_offset,
    bidirectional = bidirectional, loop_rotation = loop_rotation,
    curve_shape = curve_shape, curve_pivot = curve_pivot,
    node_names = node_names, legend = legend, legend_position = legend_position
  )
  # Remove NULL values
  plot_params <- plot_params[!sapply(plot_params, is.null)]
  net$set_plot_params(plot_params)

  # Store layout coordinates
  net$set_layout_info(list(
    name = effective_layout,
    seed = seed,
    coords = net$get_layout()
  ))

  # Re-create wrapper with updated data
  invisible(as_sonnet_network(net))
}

#' Create Grid Grob Tree
#'
#' Create a complete grid grob tree for the network (without drawing).
#'
#' @param network A sonnet_network object.
#' @param title Optional plot title.
#' @return A grid gTree object.
#' @keywords internal
create_grid_grob <- function(network, title = NULL) {
  if (!inherits(network, "sonnet_network")) {
    stop("network must be a sonnet_network object", call. = FALSE)
  }

  net <- network$network
  theme <- net$get_theme()

  # Background
  bg_color <- if (!is.null(theme)) theme$get("background") else "white"
  bg_grob <- grid::rectGrob(gp = grid::gpar(fill = bg_color, col = NA))

  # Edge grobs
  edge_grobs <- render_edges_grid(net)

  # Node grobs
  node_grobs <- render_nodes_grid(net)

  # Label grobs
  label_grobs <- render_node_labels_grid(net)

  # Edge label grobs
  edge_label_grobs <- render_edge_labels_grid(net)

  # Combine all
  children <- grid::gList(bg_grob, edge_grobs, edge_label_grobs,
                          node_grobs, label_grobs)

  # Add title if provided
  if (!is.null(title)) {
    title_col <- if (!is.null(theme)) theme$get("title_color") else "black"
    title_grob <- grid::textGrob(
      title,
      x = grid::unit(0.5, "npc"),
      y = grid::unit(0.95, "npc"),
      gp = grid::gpar(fontsize = 14, col = title_col, fontface = "bold")
    )
    children <- grid::gList(children, title_grob)
  }

  grid::gTree(children = children, name = "sonnet_plot")
}

#' Render Legend
#'
#' Create grid grobs for the network legend.
#'
#' @param network A SonnetNetwork object.
#' @param position Legend position: "topright", "topleft", "bottomright", "bottomleft".
#' @return A grid gList of legend grobs.
#' @keywords internal
render_legend_grid <- function(network, position = "topright") {
  nodes <- network$get_nodes()
  aes <- network$get_node_aes()
  theme <- network$get_theme()

  if (is.null(nodes) || nrow(nodes) == 0) return(grid::gList())

  n <- nrow(nodes)

  # Get names for legend (use node_names aesthetic if provided, otherwise node name/label)
  if (!is.null(aes$node_names)) {
    legend_names <- recycle_to_length(aes$node_names, n)
  } else if (!is.null(nodes$name)) {
    legend_names <- nodes$name
  } else {
    legend_names <- nodes$label
  }

  # Get fill colors
  fills <- recycle_to_length(
    if (!is.null(aes$fill)) aes$fill else "#4A90D9",
    n
  )

  # Get unique name-color pairs (to avoid duplicate legend entries)
  legend_data <- data.frame(
    name = legend_names,
    fill = fills,
    stringsAsFactors = FALSE
  )
  legend_data <- unique(legend_data)

  n_items <- nrow(legend_data)
  if (n_items == 0) return(grid::gList())

  # Legend styling
  swatch_size <- 0.02  # Size of color swatch
  text_size <- 8       # Text size
  item_height <- 0.04  # Height per item
  padding <- 0.02      # Padding from edge
  spacing <- 0.01      # Space between swatch and text

  # Calculate legend dimensions
  legend_height <- n_items * item_height + padding
  legend_width <- 0.15  # Fixed width

  # Calculate position based on legend_position parameter
  if (position == "topright") {
    x_start <- 1 - padding - legend_width
    y_start <- 1 - padding
  } else if (position == "topleft") {
    x_start <- padding
    y_start <- 1 - padding
  } else if (position == "bottomright") {
    x_start <- 1 - padding - legend_width
    y_start <- padding + legend_height
  } else if (position == "bottomleft") {
    x_start <- padding
    y_start <- padding + legend_height
  } else {
    # Default to topright
    x_start <- 1 - padding - legend_width
    y_start <- 1 - padding
  }

  grobs <- list()

  # Optional: Add legend background
  bg_color <- if (!is.null(theme)) theme$get("background") else "white"
  grobs[[1]] <- grid::rectGrob(
    x = grid::unit(x_start - padding/2, "npc"),
    y = grid::unit(y_start - legend_height/2 + padding/2, "npc"),
    width = grid::unit(legend_width + padding, "npc"),
    height = grid::unit(legend_height + padding, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(fill = adjustcolor(bg_color, alpha.f = 0.9),
                    col = "gray70", lwd = 0.5)
  )

  # Draw each legend item
  for (i in seq_len(n_items)) {
    y_pos <- y_start - (i - 0.5) * item_height

    # Color swatch
    grobs[[length(grobs) + 1]] <- grid::rectGrob(
      x = grid::unit(x_start, "npc"),
      y = grid::unit(y_pos, "npc"),
      width = grid::unit(swatch_size, "npc"),
      height = grid::unit(swatch_size, "npc"),
      just = c("left", "center"),
      gp = grid::gpar(fill = legend_data$fill[i], col = "gray50", lwd = 0.5)
    )

    # Text label
    text_color <- if (!is.null(theme)) theme$get("label_color") else "black"
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = legend_data$name[i],
      x = grid::unit(x_start + swatch_size + spacing, "npc"),
      y = grid::unit(y_pos, "npc"),
      just = c("left", "center"),
      gp = grid::gpar(fontsize = text_size, col = text_color)
    )
  }

  do.call(grid::gList, grobs)
}

#' @rdname soplot
#' @export
sn_render <- soplot
