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
#'   - A CographNetwork or cograph_network object
#'   - A tna object (from tna package)
#'   - A group_tna object (list of tna objects from tna package).
#'     Use parameter `i` to select a specific group, or omit to plot all groups.
#' @param layout Layout algorithm: "oval" (default), "circle", "spring",
#'   "groups", or a matrix of x,y coordinates, or an igraph layout function.
#'   Also supports igraph two-letter codes: "kk", "fr", "drl", "mds", "ni",
#'   etc.
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @param seed Random seed for deterministic layouts. Default 42.
#' @param theme Theme name: "classic", "dark", "minimal", "colorblind", etc.
#'
#' @param node_size Node size(s). Single value or vector. Default NULL, which
#'   resolves to 7 with default scaling.
#' @param node_size2 Secondary node size for ellipse/rectangle height.
#' @param scale_nodes_by Scale node sizes by a centrality measure. Can be:
#'   \itemize{
#'     \item A measure name: "degree", "strength", "betweenness", "closeness",
#'       "eigenvector", "pagerank", "authority", "hub", "harmonic", etc.
#'     \item A directional shorthand: "indegree", "outdegree", "instrength",
#'       "outstrength", "incloseness", "outcloseness", "inharmonic",
#'       "outharmonic", "ineccentricity", "outeccentricity".
#'     \item A list with measure and parameters: list("pagerank", damping = 0.9)
#'   }
#'   When used, node_size is ignored. Use node_size_range to control the
#'   min/max size. Default NULL (no centrality scaling).
#' @param node_size_range Size range for centrality-based scaling. Numeric
#'   vector c(min_size, max_size). Default c(2, 8).
#' @param scale_nodes_scale Dampening exponent for centrality-based sizing.
#'   Values < 1 compress differences (e.g., 0.5 applies square root), values > 1
#'   exaggerate differences. Default 1 (linear).
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
#'   corresponds to a node and contains values for pie segments. If a simple
#'   numeric vector with values between 0 and 1 is provided (e.g., centrality scores),
#'   it is automatically converted to donut_fill for convenience.
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
#' @param donut_inner_border_color Color for the inner boundary (where the
#'   donut meets its hole). NULL (default) uses `donut_border_color`.
#'   Can be scalar or per-node vector.
#' @param donut_inner_border_width Width for the inner boundary border.
#'   NULL (default) uses `donut_border_width`. Can be scalar or per-node vector.
#' @param donut_line_type Line type for donut borders: "solid", "dashed", "dotted", or
#'   numeric (1=solid, 2=dashed, 3=dotted). Can be scalar or per-node vector.
#' @param donut_border_lty Deprecated. Use `donut_line_type` instead.
#' @param donut_inner_ratio Inner radius ratio for donut (0-1). Default 0.8.
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
#' @param edge_color Edge color(s). If NULL, uses edge_positive_color/edge_negative_color based on weight.
#' @param edge_width Edge width(s). If NULL, scales by weight using edge_size and edge_width_range.
#' @param edge_size Maximum edge size for weight scaling. NULL (default) uses
#'   the upper bound of \code{edge_width_range}. Larger values = thicker edges
#'   overall.
#' @param esize Deprecated. Use `edge_size` instead.
#' @param edge_width_range Output width range as c(min, max) for weight-based scaling.
#'   Default c(0.1, 4). Edges are scaled to fit within this range unless
#'   \code{edge_size} supplies the maximum.
#' @param edge_scale_mode Scaling mode for edge weights: "linear" (default, qgraph-style),
#'   "log" (logarithmic for wide weight ranges), "sqrt" (moderate compression),
#'   or "rank" (equal visual spacing regardless of weight distribution).
#' @param edge_cutoff Optional cutoff for edge emphasis. NULL (default) or 0
#'   disables cutoff fading. Positive values fade edges whose absolute weights
#'   are below the cutoff; width scaling remains continuous.
#' @param cut Deprecated. Use `edge_cutoff` instead.
#' @param edge_alpha Edge transparency (0-1). Default 0.8.
#' @param edge_labels Edge labels: TRUE (show weights), FALSE (none),
#'   or character vector.
#' @param edge_label_size Edge label size.
#' @param edge_label_color Edge label text color.
#' @param edge_label_bg Edge label background color.
#' @param edge_label_position Position along edge (0-1).
#' @param edge_label_offset Perpendicular offset for edge labels (0 = on line, positive = above).
#' @param edge_label_fontface Font face: "plain", "bold", "italic", "bold.italic".
#' @param edge_label_shadow Logical: enable drop shadow for edge labels? Default FALSE.
#' @param edge_label_shadow_color Color for edge label shadow. Default "gray40".
#' @param edge_label_shadow_offset Offset distance for shadow in points. Default 0.5.
#' @param edge_label_shadow_alpha Transparency for shadow (0-1). Default 0.5.
#' @param edge_label_halo Logical: enable white halo/outline around edge labels for
#'   readability over dark edges? Default TRUE. When TRUE, overrides shadow settings.
#' @param edge_style Line type(s): 1=solid, 2=dashed, 3=dotted, etc.
#' @param curvature Edge curvature. 0 for straight, positive/negative for curves.
#' @param curve_scale Reserved for future curve scaling; currently not used.
#' @param curve_shape Spline tension (-1 to 1). Default 0.
#' @param curve_pivot Position along edge for curve control point (0-1).
#' @param curves Curve mode: TRUE (default) = single edges straight, reciprocal edges
#'   curve as ellipse (two opposing curves); FALSE = all straight; "force" = all curved.
#' @param arrow_size Arrow head size.
#' @param arrow_angle Arrow head angle in radians. Default pi/6 (30 degrees).
#' @param show_arrows Logical or vector: show arrows on directed edges?
#' @param show Dispatch-only placeholder used by method dispatch (e.g.,
#'   \code{splot.tna_disparity}). Not intended for direct use.
#' @param bidirectional Logical or vector: show arrows at both ends?
#' @param loop_rotation Angle(s) in radians for self-loop direction.
#' @param edge_start_style Style for the start segment of edges: "solid" (default),
#'   "dashed", or "dotted". Use dashed/dotted to indicate edge direction (source node).
#' @param edge_start_length Fraction of edge length for the styled start segment (0-0.5).
#'   Default 0.15 (15% of edge). Only applies when edge_start_style is not "solid".
#' @param edge_start_dot_density Pattern for dotted start segments. A two-character string
#'   where the first digit is dot length and second is gap length (in line width units).
#'   Default "12" (1 unit dot, 2 units gap). Use "11" for tighter dots, "13" for more spacing.
#'   Only applies when edge_start_style = "dotted".
#'
#' @param edge_ci Numeric vector of CI widths (0-1 scale). Larger values = more uncertainty.
#' @param edge_ci_scale Width multiplier for underlay thickness. Default 2.
#' @param edge_ci_alpha Transparency for underlay (0-1). Default 0.15.
#' @param edge_ci_color Underlay color. NA (default) uses main edge color.
#' @param edge_ci_style Line type for underlay: 1=solid, 2=dashed, 3=dotted. Default 2.
#' @param edge_ci_arrows Logical: show arrows on underlay? Default FALSE.
#' @param edge_priority Numeric vector of edge priorities. Higher values render on top.
#'   Useful for ensuring significant edges appear above non-significant ones.
#'
#' @param edge_label_style Preset style: "none", "estimate", "full", "range", "stars".
#' @param edge_label_template Template with placeholders: \{est\}, \{range\}, \{low\}, \{up\}, \{p\}, \{stars\}.
#'   Overrides edge_label_style if provided.
#' @param edge_label_digits Decimal places for estimates. Default 2.
#' @param edge_label_leading_zero Logical: show leading zero for values < 1? Default TRUE.
#'   Set to FALSE to display ".5" instead of "0.5".
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
#' @param edge_positive_color Color for positive weights.
#' @param positive_color Deprecated. Use `edge_positive_color` instead.
#' @param edge_negative_color Color for negative weights.
#' @param negative_color Deprecated. Use `edge_negative_color` instead.
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
#' @param use_pch Logical: use points() for simple circles (faster). Default FALSE.
#' @param usePCH Deprecated. Use `use_pch` instead.
#' @param scaling Scaling mode: "default" for qgraph-matched scaling where node_size=6
#'   looks similar to qgraph vsize=6, or "legacy" to preserve pre-v2.0 behavior.
#' @param align_panels Logical. If \code{TRUE}, forces a uniform symmetric
#'   plot box (\code{c(-layout_scale, layout_scale)} on each axis) so two
#'   networks plotted side-by-side in a \code{par(mfrow)} grid render at
#'   identical absolute scales — useful for bootstrap panels, comparison
#'   grids with networks of different node counts, or any case where
#'   visual-size parity across panels matters more than canvas fill.
#'   Default \code{FALSE} uses dynamic, layout-driven bounds (the
#'   pre-2.1.x behaviour) which renders tighter on the canvas. The
#'   per-node loop-reservation pad in \code{compute_plot_limits} runs
#'   regardless, so networks with different self-loop patterns stay
#'   centered consistently in either mode.
#'
#' @param legend Logical: show legend?
#' @param legend_position Position: "topright", "topleft", "bottomright", "bottomleft".
#' @param legend_size Legend text size.
#' @param legend_edge_colors Logical: show positive/negative edge colors in legend?
#' @param legend_node_sizes Logical: show node size scale in legend?
#' @param groups Group assignments for node coloring/legend.
#' @param node_names Alternative names for legend (separate from labels).
#' @param tna_styling Logical or NULL. If \code{TRUE}, applies TNA visual defaults
#'   (oval layout, TNA color palette, edge labels as estimates, dotted edge starts,
#'   etc.) as a base layer. Any explicitly provided argument overrides the TNA default.
#'   If \code{FALSE}, no TNA styling is applied. If \code{NULL} (default),
#'   automatically set to \code{TRUE} when \code{x} is a tna object, \code{FALSE}
#'   otherwise. Can be used with any input type (matrix, igraph, cograph_network).
#' @param psych_styling Logical or NULL. Undirected counterpart of `tna_styling`.
#'   If \code{TRUE}, applies psychometric-network defaults (spring layout,
#'   Okabe-Ito palette, no arrows, thin edges) as a base layer. If \code{NULL}
#'   (default), `splot.netobject` auto-enables it on correlation-family input
#'   (glasso, cor, pcor, ising) and on the undirected constituents of
#'   `net_mlvar`. Explicit user args always win.
#' @param i Group index or name when x is a group_tna object. If NULL (default),
#'   plots all groups in a grid. If specified (e.g., i = 1 or i = "Treatment"),
#'   plots only that group.
#'
#' @param filetype Output format: "default" (screen), "png", "pdf", "svg", "jpeg", "tiff".
#' @param filename Output filename (without extension).
#' @param width Output width in inches.
#' @param height Output height in inches.
#' @param res Resolution in DPI for raster outputs (PNG, JPEG, TIFF). Default 600.
#' @param ... Additional arguments passed to layout functions.
#'   One ride-along worth calling out: \code{combined} (default
#'   \code{TRUE}). When \code{x} is a multi-panel input (a
#'   \code{group_tna}, \code{group_tna_bootstrap},
#'   \code{group_tna_permutation}, \code{net_permutation_group}, or any
#'   class routed to a \code{splot.*} method that draws multiple panels
#'   such as \code{splot.net_mlvar} with \code{type = "all"}),
#'   \code{combined = FALSE} skips the internal
#'   \code{graphics::par(mfrow = ...)} grid so the caller can drive
#'   layout explicitly via \code{\link{panel_layout}()} or
#'   \code{graphics::layout()}. For single-network inputs (a single
#'   \code{tna}, \code{netobject}, matrix, etc.) \code{combined} has no
#'   effect — there is no panel grid to gate.
#'
#' @details
#' ## Edge Curve Behavior
#' Edge curving is controlled by three parameters that interact:
#' \describe{
#'   \item{\strong{curves}}{Mode for automatic curving. \code{FALSE} = all straight,
#'     \code{TRUE} (default) = curve only reciprocal edge pairs as an ellipse,
#'     \code{"force"} = curve all edges inward toward network center.}
#'   \item{\strong{curvature}}{Manual curvature amount (0-1 typical). Sets the
#'     magnitude of curves. Default 0 uses automatic 0.175 for curved edges.
#'     Positive values curve edges; the direction is automatically determined.
#'   }
#'   \item{\strong{curve_scale}}{Not currently used; reserved for future scaling.}
#' }
#'
#' For reciprocal edges (A\code{->}B and B\code{->}A both exist), the edges curve
#' in opposite directions to form a visual ellipse, making bidirectional
#' relationships clear.
#'
#' ## Weight Scaling Modes (edge_scale_mode)
#' Controls how edge weights are mapped to visual widths:
#' \describe{
#'   \item{\strong{linear} (default)}{Width proportional to weight. Best when
#'     weights are similar in magnitude.}
#'   \item{\strong{log}}{Logarithmic scaling. Best when weights span multiple
#'     orders of magnitude (e.g., 0.01 to 100).}
#'   \item{\strong{sqrt}}{Square root scaling. Moderate compression, good for
#'     moderately skewed distributions.}
#'
#'   \item{\strong{rank}}{Rank-based scaling. Ignores actual values; uses relative
#'     ordering. All edges get equal visual spacing regardless of weight distribution.}
#' }
#'
#' ## Donut vs Pie vs Double Donut
#' Three ways to show additional data on nodes:
#' \describe{
#'   \item{\strong{Donut (donut_fill)}}{Single ring showing a proportion (0-1).
#'     Ideal for completion rates, probabilities, or any single metric per node.
#'     Use \code{donut_color} for fill color and \code{donut_bg_color} for unfilled portion.}
#'   \item{\strong{Pie (pie_values)}}{Multiple colored segments showing category
#'     breakdown. Ideal for composition data. Values are normalized to sum to 1.
#'     Use \code{pie_colors} for segment colors.}
#'   \item{\strong{Double Donut (donut2_values)}}{Two concentric rings for comparing
#'     two metrics per node. Outer ring uses \code{donut_fill}/\code{donut_color},
#'     inner ring uses \code{donut2_values}/\code{donut2_colors}.}
#' }
#'
#' ## CI Underlay System
#' Confidence interval underlays draw a wider, semi-transparent edge behind the
#' main edge to visualize uncertainty:
#' \describe{
#'   \item{\strong{edge_ci}}{Vector of CI widths (0-1 scale). Larger = more uncertainty.}
#'   \item{\strong{edge_ci_scale}}{Multiplier for underlay width relative to main edge.
#'     Default 2 means underlay is twice as wide as main edge at CI=1.}
#'   \item{\strong{edge_ci_alpha}}{Transparency of underlay (0-1). Default 0.15.}
#'   \item{\strong{edge_ci_style}}{Line type: 1=solid, 2=dashed (default), 3=dotted.}
#' }
#'
#' ## Edge Label Templates
#' For statistical output, use templates to format complex labels:
#' \describe{
#'   \item{\strong{edge_label_template}}{Template string with placeholders:
#'     \code{\{est\}} for estimate/weight, \code{\{low\}}/\code{\{up\}} for CI bounds,
#'     \code{\{range\}} for formatted range, \code{\{p\}} for p-value, \code{\{stars\}}
#'     for significance stars.}
#'   \item{\strong{edge_label_style}}{Preset styles: \code{"estimate"} (weight only),
#'     \code{"full"} (estimate + CI), \code{"range"} (CI only), \code{"stars"} (significance).}
#' }
#'
#' @return Invisibly returns the cograph_network object.
#'
#' @seealso
#' \code{\link{soplot}} for grid graphics rendering (alternative engine),
#' \code{\link{cograph}} for creating network objects,
#' \code{\link{sn_nodes}} for node customization,
#' \code{\link{sn_edges}} for edge customization,
#' \code{\link{sn_layout}} for layout algorithms,
#' \code{\link{sn_theme}} for visual themes,
#' \code{\link{from_qgraph}} and \code{\link{from_tna}} for converting external objects
#'
#' @export
#'
#' @examples
#' # Basic directed network
#' adj <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1,
#'                 0, 0, 0, 1, 0, 0, 0, 0), 4, 4, byrow = TRUE)
#' splot(adj, layout = "circle", labels = c("A", "B", "C", "D"))
#'
#' # Weighted network with signed edges
#' w_adj <- matrix(c(0, .5, -.3, 0, .8, 0, .4, -.2,
#'                   0, 0, 0, .6, 0, 0, 0, 0), 4, 4, byrow = TRUE)
#' splot(w_adj, edge_positive_color = "darkgreen", edge_negative_color = "red")
#'
#' @export
splot <- function(
    x,
    layout = "oval",
    directed = NULL,
    seed = 42,
    theme = NULL,

    # Node aesthetics
    node_size = NULL,
    node_size2 = NULL,
    scale_nodes_by = NULL,
    node_size_range = c(2, 8),
    scale_nodes_scale = 1,
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
    donut_inner_border_color = NULL,
    donut_inner_border_width = NULL,
    donut_outer_border_color = NULL,
    donut_line_type = "solid",
    donut_border_lty = NULL,  # Deprecated: use donut_line_type
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
    edge_size = NULL,
    esize = NULL,  # Deprecated: use edge_size
    edge_width_range = c(0.1, 4),
    edge_scale_mode = "linear",
    edge_cutoff = NULL,
    cut = NULL,  # Deprecated: use edge_cutoff
    edge_alpha = 0.8,
    edge_labels = FALSE,
    edge_label_size = 0.8,
    edge_label_color = "gray30",
    edge_label_bg = NA,
    edge_label_position = 0.5,
    edge_label_offset = 0,
    edge_label_fontface = "plain",
    edge_label_shadow = FALSE,
    edge_label_shadow_color = "gray40",
    edge_label_shadow_offset = 0.5,
    edge_label_shadow_alpha = 0.5,
    edge_label_halo = TRUE,
    edge_style = 1,
    curvature = 0,
    curve_scale = TRUE,
    curve_shape = 0,
    curve_pivot = 0.5,
    curves = TRUE,
    arrow_size = 1,
    arrow_angle = pi/6,
    show_arrows = TRUE,
    bidirectional = FALSE,
    loop_rotation = NULL,

    # Dispatch-only placeholder: prevents R's partial-argument matching from
    # binding a caller's `show = ...` (intended for splot.tna_disparity) to
    # `show_arrows`. Defaults to NULL here; actual handling lives in
    # splot.tna_disparity, which receives `show` via .collect_dispatch_args.
    show = NULL,

    # Edge Start Style (for direction clarity)
    edge_start_style = "solid",
    edge_start_length = 0.15,
    edge_start_dot_density = "12",

    # Edge CI Underlays
    edge_ci = NULL,
    edge_ci_scale = 2.0,
    edge_ci_alpha = 0.15,
    edge_ci_color = NA,
    edge_ci_style = 2,
    edge_ci_arrows = FALSE,
    edge_priority = NULL,

    # Edge Label Templates
    edge_label_style = "none",
    edge_label_template = NULL,
    edge_label_digits = 2,
    edge_label_oneline = TRUE,
    edge_label_ci_format = "bracket",
    edge_label_leading_zero = TRUE,
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
    edge_positive_color = "#2E7D32",
    positive_color = NULL,  # Deprecated: use edge_positive_color
    edge_negative_color = "#C62828",
    negative_color = NULL,  # Deprecated: use edge_negative_color
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
    use_pch = FALSE,
    usePCH = NULL,  # Deprecated: use use_pch
    scaling = "default",
    align_panels = FALSE,

    # Legend
    legend = FALSE,
    legend_position = "topright",
    legend_size = 0.8,
    legend_edge_colors = TRUE,
    legend_node_sizes = FALSE,
    groups = NULL,
    node_names = NULL,

    # TNA styling
    tna_styling = NULL,
    # Psych network styling
    psych_styling = NULL,

    # Group selection (for group_tna)
    i = NULL,

    # Output
    filetype = "default",
    filename = file.path(tempdir(), "splot"),
    width = 7,
    height = 7,
    res = 600,
    ...
) {

  # ============================================
  # 1. INPUT PROCESSING
  # ============================================

  # --- Collect explicitly-provided user args (for dispatch forwarding) ---
  # match.call only captures args the user actually typed, not defaults
  .user_explicit <- as.list(match.call(expand.dots = FALSE))[-1]
  .user_explicit$x <- NULL
  .dots <- list(...)

  # Translate qgraph-style args for tna-family objects (early, before any dispatch)
  if (inherits(x, c("tna", "group_tna", "tna_bootstrap", "group_tna_bootstrap",
                     "tna_permutation", "group_tna_permutation"))) {
    .dots <- .translate_qgraph_dots(.dots)
  }

  # Evaluate user-explicit args once from local scope (safe, no re-eval of AST)
  # Exclude "..." — those are already captured in .dots
  .user_args <- mget(setdiff(names(.user_explicit), "..."), envir = environment())

  # Handle tna objects directly
  if (inherits(x, "tna")) {
    tna_params <- from_tna(x, engine = "splot", plot = FALSE)
    # tna_styling is implicitly TRUE for tna objects unless user said FALSE
    if (identical(tna_styling, FALSE)) {
      # Strip visual defaults, keep only structural data
      structural <- c("x", "labels", "directed", "weight_digits",
                      "donut_fill", "donut_inner_ratio", "donut_empty")
      tna_params <- tna_params[intersect(names(tna_params), structural)]
    }
    call_args <- .collect_dispatch_args(.user_args, .dots, base = tna_params)
    call_args$tna_styling <- NULL  # consumed; don't pass to recursive call
    return(do.call(splot, call_args))
  }

  # Handle list-of-plottables: any named list of first-class plottables.
  # Covers group_tna (list of tna), group_tna_bootstrap (list of tna_bootstrap),
  # group_tna_permutation (list of tna_permutation), and net_permutation_group
  # (list of net_permutation). Lay out in a grid (or plot a single one via
  # `i = "GroupName"` / `i = index`) and recurse into splot() per element,
  # which routes to the right renderer based on the element's class.
  if (inherits(x, c("group_tna", "group_tna_bootstrap",
                    "group_tna_permutation", "net_permutation_group"))) {
    n_groups <- length(x)
    group_names <- names(x)
    if (is.null(group_names)) group_names <- paste0("Group ", seq_len(n_groups))

    # Build forwarded args: everything the user explicitly provided except x and i
    fwd_args <- .collect_dispatch_args(.user_args, .dots, skip = c("x", "i"))

    # If i is specified, plot just that group
    if (!is.null(i)) {
      # Resolve group index
      if (is.character(i)) {
        idx <- match(i, group_names)
        if (is.na(idx)) {
          stop("Group '", i, "' not found. Available groups: ",
               paste(group_names, collapse = ", "), call. = FALSE)
        }
      } else {
        idx <- as.integer(i)
        if (idx < 1 || idx > n_groups) {
          stop("Group index ", idx, " out of range. Available: 1 to ", n_groups, call. = FALSE)
        }
      }

      # Set title to group name if not provided
      if (is.null(fwd_args$title)) fwd_args$title <- group_names[idx]

      return(do.call(splot, c(list(x = x[[idx]]), fwd_args)))
    }

    # No i specified: plot all groups in a grid.
    # `combined` rides in via ...; pull it out before recursing so splot()'s
    # main signature doesn't see an unknown argument.
    combined <- if (is.null(fwd_args$combined)) TRUE else isTRUE(fwd_args$combined)
    fwd_args$combined <- NULL

    if (combined) {
      n_cols <- ceiling(sqrt(n_groups))
      n_rows <- ceiling(n_groups / n_cols)
      old_par <- graphics::par(mfrow = c(n_rows, n_cols), mar = c(1, 1, 2, 1))
      on.exit(graphics::par(old_par), add = TRUE)
    }

    for (idx in seq_len(n_groups)) {
      grp_fwd <- fwd_args
      grp_fwd$title <- if (is.null(fwd_args$title)) {
        group_names[idx]
      } else {
        paste(fwd_args$title, "-", group_names[idx])
      }
      do.call(splot, c(list(x = x[[idx]]), grp_fwd))
    }

    return(invisible(NULL))
  }

  # ============================================
  # HANDLE cluster_summary / mcml
  # ============================================

  # Handle cluster_summary / mcml objects -> dispatch to plot_mcml
  if (inherits(x, c("cluster_summary", "mcml"))) {
    return(do.call(plot_mcml, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Dispatch to specialized methods for bootstrap objects
  if (inherits(x, "tna_bootstrap")) {
    return(do.call(splot.tna_bootstrap, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Dispatch to specialized methods for permutation test objects
  if (inherits(x, "tna_permutation")) {
    return(do.call(splot.tna_permutation, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # (group_tna_permutation is handled earlier by the generic list-of-plottables
  # branch; splot.group_tna_permutation / plot_group_permutation remain
  # exported for direct calls.)

  # Dispatch for tna disparity filter results
  if (inherits(x, "tna_disparity")) {
    return(do.call(splot.tna_disparity, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Dispatch for tna::communities() results — plot base model with community colors
  if (inherits(x, "tna_communities")) {
    return(do.call(splot.tna_communities, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Dispatch for cograph detect_communities() results
  if (inherits(x, "cograph_communities")) {
    return(do.call(splot.cograph_communities, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Nestimate: base netobject — apply directed/undirected styling defaults
  if (inherits(x, "netobject")) {
    return(do.call(splot.netobject, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Nestimate: bootstrap object
  if (inherits(x, "net_bootstrap")) {
    return(do.call(splot.net_bootstrap, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # (net_permutation_group is handled earlier by the generic list-of-plottables
  # branch — no dedicated splot.net_permutation_group function exists.)

  # Nestimate: permutation test object
  if (inherits(x, "net_permutation")) {
    return(do.call(splot.net_permutation, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Nestimate: group bootstrap object
  if (inherits(x, "net_bootstrap_group")) {
    return(do.call(plot_net_bootstrap_group, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Nestimate: glasso bootstrap object
  if (inherits(x, "boot_glasso")) {
    return(do.call(splot.boot_glasso, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Nestimate: centrality stability object
  if (inherits(x, "net_stability")) {
    return(do.call(plot_net_stability, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Nestimate: multilevel VAR (temporal / contemporaneous / between)
  # Must come before netobject_group — net_mlvar inherits from it
  if (inherits(x, "net_mlvar")) {
    return(do.call(splot.net_mlvar, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Nestimate: group of netobjects
  if (inherits(x, "netobject_group")) {
    return(do.call(plot_netobject_group, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Nestimate: multilevel netobject
  if (inherits(x, "netobject_ml")) {
    return(do.call(plot_netobject_ml, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # Nestimate: mixed wtna (transition + co-occurrence)
  if (inherits(x, "wtna_mixed")) {
    return(do.call(splot.wtna_mixed, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
  }

  # ============================================
  # HANDLE DEPRECATED PARAMETERS
  # ============================================
  # Detect which arguments were explicitly provided by the user
  explicit_args <- names(.user_explicit)

  # For params with NULL defaults, simple check works
  edge_size <- handle_deprecated_param(edge_size, esize, "edge_size", "esize")
  edge_cutoff <- handle_deprecated_param(edge_cutoff, cut, "edge_cutoff", "cut")

  # For params with non-NULL defaults, use new_val_was_set to check if user explicitly set them
  use_pch <- handle_deprecated_param(
    use_pch, usePCH, "use_pch", "usePCH",
    new_val_was_set = "use_pch" %in% explicit_args
  )
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
  donut_line_type <- handle_deprecated_param(
    donut_line_type, donut_border_lty,
    "donut_line_type", "donut_border_lty",
    new_val_was_set = "donut_line_type" %in% explicit_args
  )

  # Convert edge_label_fontface to numeric if string (for backwards compat with renderers)
  edge_label_fontface_num <- fontface_to_numeric(edge_label_fontface)

  # ============================================
  # APPLY TNA STYLING DEFAULTS
  # ============================================
  # tna_styling = TRUE applies TNA visual defaults as a base layer.
  # Any user-explicit arg always wins. NULL defaults are filled;

  # non-NULL defaults are only overridden if the user didn't specify them.
  if (isTRUE(tna_styling)) {
    # Detect directedness for TNA defaults (matrix or network)
    .tna_dir <- if (!is.null(directed)) {
      directed
    } else if (is.matrix(x)) {
      !is_symmetric_matrix(x)
    } else {
      TRUE
    }
    .tna_n <- if (is.matrix(x)) nrow(x) else NULL
    .tna_defs <- .tna_style_defaults(.tna_n, .tna_dir)

    # Parameters with NULL defaults — fill if user didn't set them
    if (is.null(node_fill) && !is.null(.tna_defs$node_fill))
      node_fill <- .tna_defs$node_fill
    if (is.null(node_size))
      node_size <- .tna_defs$node_size
    if (is.null(edge_color))
      edge_color <- .tna_defs$edge_color

    # Parameters with non-NULL defaults — only override if user didn't explicitly set
    if (!"layout" %in% explicit_args)
      layout <- .tna_defs$layout
    if (!"edge_label_style" %in% explicit_args)
      edge_label_style <- .tna_defs$edge_label_style
    if (!"edge_label_leading_zero" %in% explicit_args)
      edge_label_leading_zero <- .tna_defs$edge_label_leading_zero
    if (!"edge_label_size" %in% explicit_args)
      edge_label_size <- .tna_defs$edge_label_size
    if (!"edge_label_position" %in% explicit_args)
      edge_label_position <- .tna_defs$edge_label_position
    if (!"minimum" %in% explicit_args)
      minimum <- .tna_defs$minimum

    # Directed-only defaults
    if (isTRUE(.tna_dir)) {
      if (!"arrow_size" %in% explicit_args && !is.null(.tna_defs$arrow_size))
        arrow_size <- .tna_defs$arrow_size
      if (!"edge_start_length" %in% explicit_args && !is.null(.tna_defs$edge_start_length))
        edge_start_length <- .tna_defs$edge_start_length
      if (!"edge_start_style" %in% explicit_args && !is.null(.tna_defs$edge_start_style))
        edge_start_style <- .tna_defs$edge_start_style
    }
  }

  # ============================================
  # APPLY PSYCH STYLING DEFAULTS
  # ============================================
  if (isTRUE(psych_styling)) {
    .psych_n <- if (is.matrix(x)) nrow(x) else NULL
    .psych_defs <- .psych_style_defaults(.psych_n)

    if (is.null(node_fill) && !is.null(.psych_defs$node_fill))
      node_fill <- .psych_defs$node_fill
    if (is.null(node_size))
      node_size <- .psych_defs$node_size
    if (!"layout" %in% explicit_args)
      layout <- .psych_defs$layout
    if (!"directed" %in% explicit_args)
      directed <- .psych_defs$directed
    if (!"show_arrows" %in% explicit_args)
      show_arrows <- .psych_defs$show_arrows
    if (!"edge_style" %in% explicit_args)
      edge_style <- .psych_defs$edge_style
    if (!"edge_label_style" %in% explicit_args)
      edge_label_style <- .psych_defs$edge_label_style
    if (!"edge_label_leading_zero" %in% explicit_args)
      edge_label_leading_zero <- .psych_defs$edge_label_leading_zero
    if (!"edge_label_size" %in% explicit_args)
      edge_label_size <- .psych_defs$edge_label_size
    if (!"edge_label_position" %in% explicit_args)
      edge_label_position <- .psych_defs$edge_label_position
    if (!"minimum" %in% explicit_args)
      minimum <- .psych_defs$minimum
    if (!"donut_bg_color" %in% explicit_args)
      donut_bg_color <- .psych_defs$donut_bg_color
    if (is.null(donut_border_width))
      donut_border_width <- .psych_defs$donut_border_width
    if (is.null(donut_inner_border_color))
      donut_inner_border_color <- .psych_defs$donut_inner_border_color
    if (is.null(donut_inner_border_width))
      donut_inner_border_width <- .psych_defs$donut_inner_border_width
  }

  # Round matrix weights to filter near-zero edges globally
  if (is.matrix(x) && !is.null(weight_digits)) {
    x <- round(x, weight_digits)
  }

  # Set seed for deterministic layouts, restoring RNG state on exit
  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }

  # Convert to cograph_network if needed
  network <- ensure_cograph_network(x, layout = layout, seed = seed, directed = directed, ...)

  # Apply theme if specified
  if (!is.null(theme)) {
    th <- get_theme(theme)
    if (!is.null(th)) {
      # Extract theme colors
      if (is.null(node_fill)) node_fill <- th$get("node_fill")
      if (is.null(node_border_color)) node_border_color <- th$get("node_border_color")
      if (is.null(background)) background <- th$get("background")
      # Use explicit_args (built earlier from match.call) to detect "user did
      # not pass this", rather than value-equality against the signature
      # default — value-equality silently overrides users who pass the default
      # literal explicitly together with a theme.
      if (!"label_color" %in% explicit_args) label_color <- th$get("label_color")
      if (!"edge_positive_color" %in% explicit_args) edge_positive_color <- th$get("edge_positive_color")
      if (!"edge_negative_color" %in% explicit_args) edge_negative_color <- th$get("edge_negative_color")
    }
  }

  # Extract network data using getter functions
  # This handles all formats: new list format, old attr format, and R6 wrapper
  nodes <- get_nodes(network)
  edges <- get_edges(network)
  is_net_directed <- is_directed(network)

  # Get layout coordinates from nodes if available
  if ("x" %in% names(nodes) && !all(is.na(nodes$x))) {
    layout_coords <- data.frame(x = nodes$x, y = nodes$y)
  } else {
    layout_coords <- NULL # nocov
  }

  # (oval layout uses elliptical spacing but nodes remain circular via aspect=TRUE)

  n_nodes <- nrow(nodes)
  n_edges <- if (!is.null(edges)) nrow(edges) else 0

  # Determine if directed
  if (is.null(directed)) {
    directed <- is_net_directed
  }

  # Check for duplicate edges in undirected networks
  edges <- check_duplicate_edges(edges, directed, edge_duplicates)
  n_edges <- nrow(edges)
  if (!is.null(network)) network$edges <- edges

  # ============================================
  # 2. LAYOUT HANDLING
  # ============================================

  if (is.null(layout_coords)) { # nocov start
    stop("Layout coordinates not available", call. = FALSE)
  } # nocov end

  layout_mat <- as.matrix(layout_coords[, c("x", "y")])

  # Rescale to [-1, 1]
  if (rescale) {
    layout_mat <- as.matrix(rescale_layout(layout_mat, mar = 0.1,
                                            keep_aspect = aspect))
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
  # 2b. AUTO-CONVERT pie_values VECTOR TO donut_fill
  # ============================================

  # If pie_values is a numeric vector (not list) with values in [0,1],
  # treat it as donut_fill instead (single proportion per node)
  if (!is.null(pie_values) && is.numeric(pie_values) && !is.list(pie_values)) {
    if (all(pie_values >= 0 & pie_values <= 1, na.rm = TRUE)) {
      donut_fill <- pie_values
      pie_values <- NULL
    }
  }

  # ============================================
  # 3. PARAMETER VECTORIZATION
  # ============================================

  # Get scale constants for current scaling mode
  scale <- get_scale_constants(scaling)

  # Node sizes (qgraph-style, using scale constants)
  # Check for centrality-based scaling first
  centrality_info <- NULL
  if (!is.null(scale_nodes_by)) {
    centrality_info <- resolve_centrality_sizes(
      x = x,
      scale_by = scale_nodes_by,
      size_range = node_size_range,
      n = n_nodes,
      scaling = scaling,
      scale_exp = scale_nodes_scale
    )
    vsize_usr <- centrality_info$sizes
  } else {
    vsize_usr <- resolve_node_sizes(node_size, n_nodes, scaling = scaling)
  }

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

  # Vectorize node_alpha
  node_alphas <- recycle_to_length(node_alpha, n_nodes)

  # Apply alpha to node colors (skip if all alpha=1)
  if (any(node_alphas < 1)) {
    node_colors <- mapply(function(col, alpha) {
      if (alpha < 1) adjust_alpha(col, alpha) else col
    }, node_colors, node_alphas, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  }

  # Border colors (compute on unique colors to avoid redundant col2rgb calls)
  if (is.null(node_border_color)) {
    unique_cols <- unique(node_colors)
    darkened <- setNames(vapply(unique_cols, function(c) {
      tryCatch(adjust_brightness(c, -0.3), error = function(e) "black")
    }, character(1)), unique_cols)
    node_border_color <- unname(darkened[node_colors])
  }
  border_colors <- recycle_to_length(node_border_color, n_nodes)

  # Border widths
  border_widths <- recycle_to_length(node_border_width, n_nodes)

  # Labels
  node_labels <- resolve_labels(labels, nodes, n_nodes)

  # Device-dependent visual scale: reserve the per-draw env now so inner
  # helpers can retrieve it. The actual scale computation is DEFERRED until
  # after `graphics::plot()` is called further down, because before plot.new
  # `par("pin")` is still reflecting the previous plot (or default margins)
  # rather than the real plot region about to be drawn. Setting an identity
  # placeholder here keeps `.get_current_visual_scale()` safe to call from
  # any code path that runs between now and the post-plot refresh.
  visual_scale <- .identity_visual_scale()
  .set_current_visual_scale(visual_scale)
  on.exit(.clear_current_visual_scale(), add = TRUE)

  # Per-node label colours (vectorised). Actual label cex is resolved after
  # plot.new so par("pin") is accurate.
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

    # Subset all per-edge vectors to match filtered edge count
    if (n_edges < orig_n_edges) {
      keep_idx <- which(abs(orig_weights) >= effective_threshold)
      .subset_if_per_edge <- function(v) {
        if (!is.null(v) && length(v) == orig_n_edges) v[keep_idx] else v
      }
      if (is.character(edge_labels) && length(edge_labels) == orig_n_edges)
        edge_labels <- edge_labels[keep_idx]
      edge_style             <- .subset_if_per_edge(edge_style)
      edge_color             <- .subset_if_per_edge(edge_color)
      edge_width             <- .subset_if_per_edge(edge_width)
      edge_priority          <- .subset_if_per_edge(edge_priority)
      edge_ci                <- .subset_if_per_edge(edge_ci)
      edge_ci_alpha          <- .subset_if_per_edge(edge_ci_alpha)
      edge_ci_scale          <- .subset_if_per_edge(edge_ci_scale)
      edge_ci_color          <- .subset_if_per_edge(edge_ci_color)
      edge_label_fontface    <- .subset_if_per_edge(edge_label_fontface)
      edge_label_position    <- .subset_if_per_edge(edge_label_position)
      edge_label_p           <- .subset_if_per_edge(edge_label_p)
      edge_ci_lower          <- .subset_if_per_edge(edge_ci_lower)
      edge_ci_upper          <- .subset_if_per_edge(edge_ci_upper)
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
    edge_colors <- resolve_edge_colors(edges, edge_color, edge_positive_color, edge_negative_color)

    # Vectorize edge_alpha and apply to edge colors (skip if all alpha=1)
    edge_alphas <- recycle_to_length(edge_alpha, n_edges)
    if (any(edge_alphas < 1)) {
      edge_colors <- mapply(function(col, alpha) {
        if (alpha < 1) adjust_alpha(col, alpha) else col
      }, edge_colors, edge_alphas, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    }

    # Apply edge_cutoff threshold for transparency: edges below cutoff are faded
    if (!is.null(edge_cutoff) && edge_cutoff > 0 && "weight" %in% names(edges)) {
      abs_weights <- abs(edges$weight)
      below_cutoff <- abs_weights < edge_cutoff
      if (any(below_cutoff)) {
        # Scale alpha: edges at 0 get 20% of normal alpha, edges near cutoff get full alpha
        fade_factor <- ifelse(below_cutoff, 0.2 + 0.8 * (abs_weights / edge_cutoff), 1)
        edge_colors <- mapply(function(col, fade) {
          if (fade < 1) adjust_alpha(col, fade) else col
        }, edge_colors, fade_factor, SIMPLIFY = TRUE, USE.NAMES = FALSE)
      }
    }

    # Edge widths are resolved post-plot (below) so par("pin") is valid.
    # We still compute curvatures here because they don't depend on device.

    # Compute per-edge curvatures (reciprocal detection + direction)
    curve_result <- compute_edge_curvatures(curvature, curves, edges, layout_mat)
    curves_vec <- curve_result$curves_vec
    is_reciprocal <- curve_result$is_reciprocal

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
        leading_zero = edge_label_leading_zero,
        n = n_edges
      )
    } else {
      # Use standard edge labels
      edge_labels_vec <- resolve_edge_labels(edge_labels, edges, n_edges)
    }

    # CI underlay parameters
    edge_ci_vec <- if (!is.null(edge_ci)) recycle_to_length(edge_ci, n_edges) else NULL
    edge_ci_colors <- if (!is.null(edge_ci_vec)) {
      if (length(edge_ci_color) == 1 && is.na(edge_ci_color)) {
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

  # Margins - ensure title has adequate space
  # Default margins[3] (top) is 0.1 which is too small for titles
  # Add extra space proportional to title_size when title is provided
  title_space <- if (!is.null(title)) max(1.5, title_size * 1.2) else 0
  graphics::par(mar = c(margins[1], margins[2],
                        margins[3] + title_space, margins[4]))

  # Calculate plot limits accounting for node radii, self-loops, and margins
  # When the layout was auto-rescaled (rescale = TRUE, the default),
  # rescale_layout fits it inside [-0.9, 0.9] with aspect preserved, so
  # the plot area should anchor to a consistent [-1, 1] box regardless
  # of layout shape. This keeps node pixel sizes stable across different
  # seeds / algorithms / imported qgraph layouts, fixing the long-standing
  # "different layout -> different apparent node size" surprise.
  # `align_panels = TRUE` opts into cf525b30's fixed-bounds box, which
  # forces consistent xlim/ylim across panels regardless of layout
  # extremity. Default FALSE = dynamic bounds (pre-cf525b30) for tighter
  # rendering. The all-nodes loop-reservation in compute_plot_limits
  # (lines ~619-632) runs regardless and keeps loop-presence-driven
  # centering consistent across panels even on the dynamic path.
  fixed_bounds <- if (isTRUE(rescale) && isTRUE(align_panels)) {
    b <- layout_scale %||% 1
    c(-b, b, -b, b)
  } else NULL
  lims <- compute_plot_limits(layout_mat, vsize_usr, layout_margin,
                              edges, n_edges, loop_rotations,
                              fixed_bounds = fixed_bounds)
  xlim <- lims$xlim
  ylim <- lims$ylim

  # Reserve native whitespace for the legend by expanding `xlim` on the
  # appropriate side (qgraph's GLratio idiom). Doing it in user-coordinates
  # rather than via `par("mar")` keeps the legend-band expansion in the
  # same units as `inset`, so a small positive inset places the legend
  # cleanly inside the expanded region rather than pushing it off the PNG.
  # With `asp = 1`, R preserves visual circularity by widening the physical
  # plot box and compensating `ylim` — nodes stay round.
  legend_xlim_expansion <- 0
  if (isTRUE(legend)) {
    x_span <- xlim[2] - xlim[1]
    pos <- legend_position
    if (identical(pos, "topright") || identical(pos, "bottomright") ||
        identical(pos, "right")) {
      xlim[2] <- xlim[2] + x_span * 0.25
      legend_xlim_expansion <- x_span * 0.25
    } else if (identical(pos, "topleft") || identical(pos, "bottomleft") ||
               identical(pos, "left")) {
      xlim[1] <- xlim[1] - x_span * 0.25
      legend_xlim_expansion <- x_span * 0.25
    }
  }

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

  # Compute visual_scale NOW — par("pin") is accurate for the freshly-opened
  # plot region. Labels and edges are resolved from this scale below so
  # they share the same pin-based anchor as the node user-coordinates.
  visual_scale <- .resolve_visual_scale(scaling)
  .set_current_visual_scale(visual_scale)

  # Label sizes (qgraph-style invariant: label cex tracks node_size so the
  # node-to-label ratio is locked by construction). Device compensation
  # is applied uniformly via visual_scale so label pixel size tracks pin.
  label_cex <- resolve_label_sizes(label_size, vsize_usr, n_nodes,
                                   scaling = scaling,
                                   visual_scale = visual_scale,
                                   node_size = node_size)

  # Default edge_label_size is a fixed fraction of node label cex so the
  # node-to-edge-label ratio stays constant across canvases. User-explicit
  # values skip the coupling and receive the (capped) visual_scale
  # compensation instead — that logic lives here rather than in
  # render_edges_splot so the final cex is produced in one place.
  if (!("edge_label_size" %in% explicit_args)) {
    edge_label_size <- mean(label_cex) * EDGE_LABEL_NODE_CEX_FRACTION
  } else {
    .vs_mult_edge <- visual_scale$scale %||% 1
    if (is.finite(.vs_mult_edge) && .vs_mult_edge > 0) {
      .vs_mult_edge <- pmin(pmax(.vs_mult_edge, EDGE_LABEL_SCALE_CAP[1]),
                            EDGE_LABEL_SCALE_CAP[2])
      edge_label_size <- edge_label_size * .vs_mult_edge
    }
  }

  # Edge widths (visual_scale multiplies the mapped lwd so absolute widths
  # track the canvas; weight-to-width rank mapping is preserved). Line
  # types / dotted-width adjustment applied after.
  if (n_edges > 0) {
    edge_widths <- resolve_edge_widths(
      edges = edges,
      edge.width = edge_width,
      esize = edge_size,
      n_nodes = n_nodes,
      directed = directed,
      maximum = maximum,
      minimum = threshold,
      cut = edge_cutoff,
      edge_width_range = edge_width_range,
      edge_scale_mode = edge_scale_mode,
      scaling = scaling,
      visual_scale = visual_scale
    )
    es <- resolve_edge_styles(edge_style, edge_widths, n_edges)
    ltys <- es$ltys
    edge_widths <- es$edge_widths
  }

  # Background
  if (!is.null(background) && background != "transparent") {
    graphics::rect(
      xleft = xlim[1] - 1, ybottom = ylim[1] - 1,
      xright = xlim[2] + 1, ytop = ylim[2] + 1,
      col = background, border = NA
    )
  }

  # Title — scaled by the same uniform visual_scale multiplier as labels and
  # legend so title-to-plot ratio is stable across canvases.
  if (!is.null(title)) {
    graphics::title(main = title,
                    cex.main = title_size *
                      (visual_scale$scale %||% visual_scale$text %||% 1))
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
      arrow_angle = arrow_angle,
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
      edge_label_halo = edge_label_halo,
      # CI underlay parameters
      edge_ci = edge_ci_vec,
      edge_ci_scale = edge_ci_scale,
      edge_ci_alpha = edge_ci_alpha,
      edge_ci_color = edge_ci_colors,
      edge_ci_style = edge_ci_style,
      edge_ci_arrows = edge_ci_arrows,
      edge_priority = edge_priority,
      is_reciprocal = is_reciprocal,
      # Edge start style parameters
      edge_start_style = edge_start_style,
      edge_start_length = edge_start_length,
      edge_start_dot_density = edge_start_dot_density
    )
  }

  # ============================================
  # 7. RENDER NODES
  # ============================================

  # Resolve donut parameters
  dp <- resolve_donut_params(
    donut_fill = donut_fill, donut_values = donut_values,
    donut_color = donut_color, donut_colors = donut_colors,
    donut_bg_color = donut_bg_color, donut_shape = donut_shape,
    donut_border_color = donut_border_color,
    donut_outer_border_color = donut_outer_border_color,
    donut_line_type = donut_line_type, donut_empty = donut_empty,
    shapes = shapes, n_nodes = n_nodes
  )

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
    donut_values = dp$donut_values,
    donut_colors = dp$donut_colors,
    donut_border_color = dp$donut_border_color,
    donut_border_width = donut_border_width,
    donut_inner_border_color = donut_inner_border_color,
    donut_inner_border_width = donut_inner_border_width,
    donut_outer_border_color = dp$donut_outer_border_color,
    donut_line_type = dp$donut_line_type,
    donut_inner_ratio = donut_inner_ratio,
    donut_bg_color = dp$bg_color,
    donut_shape = dp$donut_shapes,
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
    use_pch = use_pch
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
      positive_color = edge_positive_color,
      negative_color = edge_negative_color,
      has_pos_edges = has_pos_edges,
      has_neg_edges = has_neg_edges,
      show_node_sizes = legend_node_sizes,
      node_size = vsize_usr,
      visual_scale = visual_scale
    )
  }

  # ============================================
  # 9. RETURN
  # ============================================

  # Attach the actual plot-space coordinates (post-rescale, post-layout_scale)
  # so downstream helpers like overlay_communities() can place annotations at
  # true node positions without reconstructing splot's internal transform.
  # network$nodes$x / $y remain in the original layout-coord space (used by
  # tests and non-rendering callers); plot_x / plot_y are the coords that
  # actually appeared on the device.
  if (!is.null(network$nodes) && nrow(network$nodes) == nrow(layout_mat)) {
    network$nodes$plot_x <- layout_mat[, 1]
    network$nodes$plot_y <- layout_mat[, 2]

    # Port the layout across plots: stash the rendered coord matrix on
    # $meta$layout so a caller can do
    #   p2 <- splot(other_graph, layout = p$meta$layout)
    # and get identical positions without manually reassembling plot_x/y.
    rendered_coords <- layout_mat
    rownames(rendered_coords) <- network$nodes$name
    colnames(rendered_coords) <- c("x", "y")
    existing <- network$meta$layout %||% list()
    network$meta$layout <- modifyList(
      if (is.list(existing)) existing else list(name = existing),
      list(coords = rendered_coords,
           rescale = FALSE,
           layout_scale = 1)
    )

    # Stash resolved per-node render params so the returned object is
    # self-sufficient for replot. Column names deliberately mirror splot
    # argument names (node_size, node_fill, node_shape, ...).
    if (exists("vsize_usr", inherits = FALSE))
      network$nodes$node_size  <- vsize_usr
    if (exists("node_colors", inherits = FALSE))
      network$nodes$node_fill  <- node_colors
    if (exists("shapes", inherits = FALSE) && length(shapes) == nrow(network$nodes))
      network$nodes$node_shape <- shapes
    if (exists("label_cex", inherits = FALSE))
      network$nodes$label_size <- label_cex
  }

  # Same for edges — stash resolved widths, colors, styles.
  if (!is.null(network$edges) && nrow(network$edges) == n_edges && n_edges > 0) {
    if (exists("edge_widths", inherits = FALSE))
      network$edges$edge_size  <- edge_widths
    if (exists("edge_colors", inherits = FALSE))
      network$edges$edge_color <- edge_colors
    if (exists("ltys", inherits = FALSE))
      network$edges$edge_style <- ltys
    if (exists("curves_vec", inherits = FALSE))
      network$edges$curve      <- curves_vec
  }

  # Attach device geometry so the visual-sweep harness (and external callers)
  # can compute label/node pixel ratios without re-running the plot. Captured
  # while the device is still open — both scales require live par("pin").
  attr(network, "cograph.visual_scale") <- visual_scale
  if (exists("vsize_usr", inherits = FALSE)) {
    ux <- tryCatch(get_x_scale(), error = function(e) NA_real_)
    uy <- tryCatch(get_y_scale(), error = function(e) NA_real_)
    if (is.finite(ux) && is.finite(uy)) {
      # Representative diameter: 2 * median node radius, in inches (mean of
      # x- and y-inch scales so it is rotation-independent).
      attr(network, "cograph.node_diam_in") <-
        2 * stats::median(vsize_usr, na.rm = TRUE) * mean(c(ux, uy))
    }
  }

  invisible(network)
}


#' Render Edges for splot
#' @keywords internal
render_edges_splot <- function(edges, layout, node_sizes, shapes,
                               edge_color, edge_width, edge_style, curvature,
                               curve_shape, curve_pivot, show_arrows, arrow_size,
                               arrow_angle = pi/6, bidirectional, loop_rotation, edge_labels,
                               edge_label_size, edge_label_color, edge_label_bg,
                               edge_label_position, edge_label_offset = 0,
                               edge_label_fontface,
                               edge_label_shadow = FALSE, edge_label_shadow_color = "gray40",
                               edge_label_shadow_offset = 0.5, edge_label_shadow_alpha = 0.5,
                               edge_label_halo = TRUE,
                               edge_ci = NULL, edge_ci_scale = 2.0,
                               edge_ci_alpha = 0.15, edge_ci_color = NULL,
                               edge_ci_style = 2, edge_ci_arrows = FALSE,
                               edge_priority = NULL,
                               is_reciprocal = NULL,
                               edge_start_style = "solid", edge_start_length = 0.15,
                               edge_start_dot_density = "12") {

  m <- nrow(edges)
  if (m == 0) return(invisible())

  n <- nrow(layout)

  # Calculate network center for inward curve direction
  center_x <- mean(layout[, 1])
  center_y <- mean(layout[, 2])

  # Get render order (weakest to strongest, low priority to high priority)
  order_idx <- get_edge_order(edges, priority = edge_priority)

  # Expand CI parameters to per-edge vectors
  edge_ci_scales <- expand_param(edge_ci_scale, m, "edge_ci_scale")
  edge_ci_alphas <- expand_param(edge_ci_alpha, m, "edge_ci_alpha")
  edge_ci_arrows_vec <- expand_param(edge_ci_arrows, m, "edge_ci_arrows")

  # Storage for label positions
  label_positions <- vector("list", m)

  # Validate and convert edge_start_style to lty value

  # Accepts string values ("solid", "dashed", "dotted") or numeric (1, 2, 3)
  if (is.numeric(edge_start_style)) {
    if (!edge_start_style %in% c(1, 2, 3)) {
      warning("edge_start_style numeric value should be 1 (solid), 2 (dashed), or 3 (dotted). ",
              "Got: ", edge_start_style, ". Using solid.", call. = FALSE)
      start_lty <- 1
    } else if (edge_start_style == 3) {
      # Dotted: use custom density pattern
      start_lty <- edge_start_dot_density
    } else {
      start_lty <- edge_start_style
    }
  } else {
    valid_styles <- c("solid", "dashed", "dotted")
    if (!edge_start_style %in% valid_styles) {
      stop("edge_start_style must be one of: ", paste(valid_styles, collapse = ", "),
           ", or numeric 1-3. Got: '", edge_start_style, "'", call. = FALSE)
    }
    start_lty <- switch(edge_start_style,
      "solid" = 1,
      "dashed" = 2,
      "dotted" = edge_start_dot_density  # Use custom density pattern
    )
  }
  start_fraction <- if (identical(start_lty, 1) || identical(start_lty, 1L)) 0 else edge_start_length

  # Helper function to calculate curve direction (bend INWARD toward center)
  calc_curve_direction <- function(curve_val, start_x, start_y, end_x, end_y) {
    # Defensive check: ensure all coordinates are valid scalars
    if (length(start_x) == 0 || length(start_y) == 0 ||
        length(end_x) == 0 || length(end_y) == 0 ||
        any(is.na(c(start_x, start_y, end_x, end_y)))) {
      return(if (length(curve_val) > 0) curve_val else 0) # nocov
    }

    if (length(curve_val) == 0 || is.na(curve_val)) { # nocov start
      return(0)
    } # nocov end

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
      if (length(len) == 0 || is.na(len) || len < 1e-10) return(curve_val) # nocov
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

    # Skip invalid edges (NA or out-of-bounds indices)
    if (length(from_idx) == 0 || length(to_idx) == 0 ||
        is.na(from_idx) || is.na(to_idx) ||
        from_idx < 1 || to_idx < 1 ||
        from_idx > n || to_idx > n) {
      next
    }

    x1 <- layout[from_idx, 1]
    y1 <- layout[from_idx, 2]
    x2 <- layout[to_idx, 1]
    y2 <- layout[to_idx, 2]

    # Skip if coordinates are invalid
    if (length(x1) == 0 || length(y1) == 0 ||
        length(x2) == 0 || length(y2) == 0 ||
        any(is.na(c(x1, y1, x2, y2)))) {
      next
    }

    # Self-loop
    if (from_idx == to_idx) {
      # PASS 1: Draw CI underlay for self-loop (if edge_ci provided)
      if (!is.null(edge_ci) && !is.na(edge_ci[i]) && edge_ci[i] > 0) {
        underlay_width <- edge_width[i] * (1 + edge_ci[i] * edge_ci_scales[i])
        underlay_col <- if (!is.null(edge_ci_color)) edge_ci_color[i] else edge_color[i]
        underlay_col <- adjust_alpha(underlay_col, edge_ci_alphas[i])

        draw_self_loop_base(
          x1, y1, node_sizes[from_idx],
          col = underlay_col,
          lwd = underlay_width,
          lty = edge_ci_style,
          rotation = loop_rotation[i],
          arrow = edge_ci_arrows_vec[i],
          asize = arrow_size[i],
          arrow_angle = arrow_angle
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
        asize = arrow_size[i],
        arrow_angle = arrow_angle
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

    # For reciprocal edges, shift snap points perpendicular to the edge
    if (!is.null(is_reciprocal) && is_reciprocal[i]) {
      dx <- x2 - x1
      dy <- y2 - y1
      len <- sqrt(dx^2 + dy^2)
      if (len > 1e-10) {
        px <- -dy / len
        py <- dx / len
        shift <- node_sizes[from_idx] * 0.12 * sign(curvature[i])
        start$x <- start$x + px * shift
        start$y <- start$y + py * shift
        shift_end <- node_sizes[to_idx] * 0.12 * sign(curvature[i])
        end$x <- end$x + px * shift_end
        end$y <- end$y + py * shift_end
      }
    }

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
      underlay_width <- edge_width[i] * (1 + edge_ci[i] * edge_ci_scales[i])
      underlay_col <- if (!is.null(edge_ci_color)) edge_ci_color[i] else edge_color[i]
      underlay_col <- adjust_alpha(underlay_col, edge_ci_alphas[i])

      if (abs(curve_i) > 1e-6) {
        draw_curved_edge_base(
          start$x, start$y, end$x, end$y,
          curve = curve_i,
          curvePivot = curve_pivot[i],
          col = underlay_col,
          lwd = underlay_width,
          lty = edge_ci_style,
          arrow = edge_ci_arrows_vec[i],
          asize = arrow_size[i],
          bidirectional = FALSE,
          arrow_angle = arrow_angle
        )
      } else {
        draw_straight_edge_base(
          start$x, start$y, end$x, end$y,
          col = underlay_col,
          lwd = underlay_width,
          lty = edge_ci_style,
          arrow = edge_ci_arrows_vec[i],
          asize = arrow_size[i],
          bidirectional = FALSE,
          arrow_angle = arrow_angle
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
        start_fraction = start_fraction,
        arrow_angle = arrow_angle
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
        start_fraction = start_fraction,
        arrow_angle = arrow_angle
      )
    }

    # Store edge start/end and curve info for label positioning
    label_positions[[i]] <- list(
      start_x = start$x, start_y = start$y,
      end_x = end$x, end_y = end$y,
      curve = curve_i,
      curvePivot = curve_pivot[i]
    )
  }

  # Draw edge labels
  if (!is.null(edge_labels)) {
    # Vectorize edge label parameters (strict: length 1 or m). The caller
    # (splot.R) has already applied the harmony coupling (fraction of node
    # label cex for auto-defaults, or visual_scale with EDGE_LABEL_SCALE_CAP
    # for user-explicit values), so `edge_label_size` arrives as final cex.
    edge_label_sizes <- expand_param(edge_label_size, m, "edge_label_size")
    edge_label_colors <- expand_param(edge_label_color, m, "edge_label_color")
    edge_label_bgs <- expand_param(edge_label_bg, m, "edge_label_bg")
    edge_label_positions_vec <- expand_param(edge_label_position, m, "edge_label_position")
    edge_label_offsets <- expand_param(edge_label_offset, m, "edge_label_offset")
    edge_label_shadows <- expand_param(edge_label_shadow, m, "edge_label_shadow")
    edge_label_shadow_colors <- expand_param(edge_label_shadow_color, m, "edge_label_shadow_color")
    edge_label_shadow_offsets <- expand_param(edge_label_shadow_offset, m, "edge_label_shadow_offset")
    edge_label_shadow_alphas <- expand_param(edge_label_shadow_alpha, m, "edge_label_shadow_alpha")

    # Apply halo effect if enabled (overrides shadow settings)
    edge_label_halos <- expand_param(edge_label_halo, m, "edge_label_halo")
    for (i in seq_len(m)) {
      if (isTRUE(edge_label_halos[i])) {
        edge_label_shadows[i] <- "halo"
        edge_label_shadow_colors[i] <- "white"
        edge_label_shadow_alphas[i] <- 1.0
        if (edge_label_shadow_offsets[i] < 0.5) {
          edge_label_shadow_offsets[i] <- 0.6
        }
      }
    }

    # Handle edge_label_fontface - convert strings to numbers if needed
    edge_label_fontfaces <- expand_param(edge_label_fontface, m, "edge_label_fontface")
    edge_label_fontfaces <- vapply(edge_label_fontfaces, fontface_to_numeric, numeric(1))

    for (i in seq_len(m)) {
      if (!is.null(edge_labels[i]) && !is.na(edge_labels[i]) && edge_labels[i] != "") {
        edge_info <- label_positions[[i]]
        # Self-loops have x, y directly; regular edges have start_x, start_y, etc.
        if (!is.null(edge_info$x) && !is.null(edge_info$y)) {
          # Self-loop: use stored position directly
          pos <- list(x = edge_info$x, y = edge_info$y)
        } else {
          # Regular edge: compute position
          pos <- get_edge_label_position(
            edge_info$start_x, edge_info$start_y,
            edge_info$end_x, edge_info$end_y,
            position = edge_label_positions_vec[i],
            curve = edge_info$curve,
            curvePivot = edge_info$curvePivot,
            label_offset = edge_label_offsets[i]
          )
        }
        draw_edge_label_base(
          pos$x, pos$y,
          label = edge_labels[i],
          cex = edge_label_sizes[i],
          col = edge_label_colors[i],
          bg = edge_label_bgs[i],
          font = edge_label_fontfaces[i],
          shadow = edge_label_shadows[i],
          shadow_color = edge_label_shadow_colors[i],
          shadow_offset = edge_label_shadow_offsets[i],
          shadow_alpha = edge_label_shadow_alphas[i]
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
                               donut_inner_border_color = NULL, donut_inner_border_width = NULL,
                               donut_outer_border_color = NULL, donut_line_type = "solid",
                               donut_inner_ratio, donut_bg_color, donut_shape,
                               donut_show_value, donut_value_size, donut_value_color,
                               donut_value_fontface = "bold", donut_value_fontfamily = "sans",
                               donut_value_digits = 2, donut_value_prefix = "",
                               donut_value_suffix = "",
                               donut2_values, donut2_colors, donut2_inner_ratio,
                               labels, label_size, label_color, label_position,
                               label_fontface = "plain", label_fontfamily = "sans",
                               label_hjust = 0.5, label_vjust = 0.5, label_angle = 0,
                               use_pch = FALSE) {

  n <- nrow(layout)
  if (n == 0) return(invisible())

  # Vectorize donut parameters (strict: length 1 or n)
  donut_inner_ratios <- expand_param(donut_inner_ratio, n, "donut_inner_ratio")
  donut_bg_colors <- expand_param(donut_bg_color, n, "donut_bg_color")
  donut_show_values <- expand_param(donut_show_value, n, "donut_show_value")
  donut_value_sizes <- expand_param(donut_value_size, n, "donut_value_size")
  donut_value_colors <- expand_param(donut_value_color, n, "donut_value_color")
  donut_value_fontfaces <- expand_param(donut_value_fontface, n, "donut_value_fontface")
  donut_value_fontfamilies <- expand_param(donut_value_fontfamily, n, "donut_value_fontfamily")

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
          outer_inner_ratio = donut_inner_ratios[i],
          inner_inner_ratio = donut2_inner_ratio,
          bg_color = donut_bg_colors[i],
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

        # Get per-node donut shape
        current_donut_shape <- if (length(donut_shape) >= i) donut_shape[i] else "circle"

        if (current_donut_shape != "circle") {
          # Use polygon donut with pie for non-circular shapes
          draw_polygon_donut_pie_node_base(
            x, y, node_size[i],
            donut_value = donut_val,
            donut_color = donut_col,
            donut_shape = current_donut_shape,
            pie_values = pie_vals,
            pie_colors = pie_cols,
            pie_default_color = node_fill[i],
            inner_ratio = donut_inner_ratios[i],
            bg_color = donut_bg_colors[i],
            border.col = node_border_color[i],
            border.width = node_border_width[i],
            pie_border.width = pie_border_width,
            donut_border.width = donut_border_width
          )
        } else {
          # Use circular donut with pie (default)
          draw_donut_pie_node_base(
            x, y, node_size[i],
            donut_value = donut_val,
            donut_color = donut_col,
            pie_values = pie_vals,
            pie_colors = pie_cols,
            pie_default_color = node_fill[i],
            inner_ratio = donut_inner_ratios[i],
            bg_color = donut_bg_colors[i],
            border.col = node_border_color[i],
            border.width = node_border_width[i],
            pie_border.width = pie_border_width,
            donut_border.width = donut_border_width
          )
        }
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
      effective_border_lty <- if (length(donut_line_type) >= i) donut_line_type[i] else "solid"

      if (current_donut_shape != "circle") {
        # Use polygon donut for non-circular shapes
        draw_polygon_donut_node_base(
          x, y, node_size[i],
          values = donut_vals,
          colors = donut_cols,
          default_color = node_fill[i],
          inner_ratio = donut_inner_ratios[i],
          bg_color = donut_bg_colors[i],
          center_color = node_fill[i],
          donut_shape = current_donut_shape,
          border.col = effective_donut_border_col,
          border.width = node_border_width[i],
          donut_border.width = donut_border_width,
          inner_border.col = donut_inner_border_color,
          inner_border.width = donut_inner_border_width,
          outer_border.col = effective_outer_border_col,
          border.lty = effective_border_lty,
          show_value = donut_show_values[i],
          value_cex = donut_value_sizes[i],
          value_col = donut_value_colors[i],
          value_fontface = donut_value_fontfaces[i],
          value_fontfamily = donut_value_fontfamilies[i],
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
          inner_ratio = donut_inner_ratios[i],
          bg_color = donut_bg_colors[i],
          center_color = node_fill[i],
          border.col = effective_donut_border_col,
          border.width = node_border_width[i],
          donut_border.width = donut_border_width,
          inner_border.col = donut_inner_border_color,
          inner_border.width = donut_inner_border_width,
          outer_border.col = effective_outer_border_col,
          border.lty = effective_border_lty,
          show_value = donut_show_values[i],
          value_cex = donut_value_sizes[i],
          value_col = donut_value_colors[i],
          value_fontface = donut_value_fontfaces[i],
          value_fontfamily = donut_value_fontfamilies[i],
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
      if (use_pch && node_shape[i] == "circle") {
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
    # Vectorize label parameters (strict: length 1 or n)
    label_angles <- expand_param(label_angle, n, "label_angle")
    label_positions <- expand_param(label_position, n, "label_position")
    label_fontfaces <- expand_param(label_fontface, n, "label_fontface")
    label_fontfamilies <- expand_param(label_fontfamily, n, "label_fontfamily")
    label_hjusts <- expand_param(label_hjust, n, "label_hjust")
    label_vjusts <- expand_param(label_vjust, n, "label_vjust")

    for (i in seq_len(n)) {
      if (!is.null(labels[i]) && !is.na(labels[i]) && labels[i] != "") {
        lx <- layout[i, 1]
        ly <- layout[i, 2]

        # Adjust position based on per-node label_position
        offset <- node_size[i] * 1.2

        if (label_positions[i] == "above") {
          ly <- ly + offset
        } else if (label_positions[i] == "below") {
          ly <- ly - offset
        } else if (label_positions[i] == "left") {
          lx <- lx - offset
        } else if (label_positions[i] == "right") {
          lx <- lx + offset
        }
        # "center" - no offset

        # Convert fontface string to numeric (per-node)
        fontface_num <- fontface_to_numeric(label_fontfaces[i])

        draw_node_label_base(
          lx, ly,
          label = labels[i],
          cex = label_size[i],
          col = label_color[i],
          font = fontface_num,
          family = label_fontfamilies[i],
          hjust = label_hjusts[i],
          vjust = label_vjusts[i],
          srt = label_angles[i]
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
                                show_node_sizes = FALSE, node_size = NULL,
                                visual_scale = NULL) {

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
    group_colors <- vapply(unique_groups, function(g) {
      idx <- which(groups == g)[1]
      node_colors[idx]
    }, character(1))

    group_labels <- if (!is.null(node_names)) {
      vapply(unique_groups, function(g) {
        idx <- which(groups == g)[1]
        if (length(node_names) >= idx) node_names[idx] else as.character(g)
      }, character(1))
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

    # Semantic point-size multiplier for the node-size legend swatches. Device
    # compensation is applied inside .render_legend_base via visual_scale$point.
    scale_factor <- 15
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

  # The legend lands in the native whitespace we reserved via xlim
  # expansion (see splot.R where xlim[2] is widened for right-positioned
  # legends). A small positive inset keeps the legend a comfortable
  # distance from the inner plot edge. xpd = FALSE because the legend
  # now sits inside the plot box, not in the margin.
  inset_val <- c(0.02, 0.02)

  # Delegate to the shared helper so cex/pt.cex/lwd pick up device compensation
  # via visual_scale. Preserves the pt.bg = legend_colors pairing and the
  # has_points / has_lines gates.
  .render_legend_base(
    legend = legend_labels,
    col = legend_colors,
    pch = if (has_points) legend_pch else NULL,
    lty = if (has_lines) legend_lty else NULL,
    lwd = if (has_lines) legend_lwd else NULL,
    pt.cex = if (has_points) legend_pt_cex else NULL,
    pt.bg = if (has_points) legend_colors else NULL,
    position = position,
    cex = cex,
    bty = "o",
    bg = "white",
    seg.len = 1.5,
    inset = inset_val,
    xpd = FALSE,
    visual_scale = visual_scale
  )
}

#' Collect user-explicit args for dispatch forwarding
#'
#' Merges evaluated user args and dots, optionally starting from a base list.
#' Used by splot() to forward all user-provided parameters across dispatch
#' boundaries (bootstrap, permutation, cluster, etc.).
#'
#' @param user_args Named list of evaluated user-explicit args.
#' @param dots The ... args (already evaluated).
#' @param skip Character vector of arg names to exclude (default "x").
#' @param base Optional base list to merge on top of (e.g., tna_params).
#' @return Named list of args ready for do.call().
#' @noRd
.collect_dispatch_args <- function(user_args, dots, skip = "x", base = list()) {
  nms <- setdiff(names(user_args), skip)
  result <- base
  result[nms] <- user_args[nms]
  result[names(dots)] <- dots
  result
}
