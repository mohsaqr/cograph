#' @title Edge Aesthetics
#' @description Functions for setting edge aesthetic properties.
#' @name aes-edges
NULL

#' Set Edge Aesthetics
#'
#' Customize the visual appearance of edges in a network plot.
#'
#' @param network A sonnet_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param width Edge width. Can be a single value, vector (per-edge), or "weight".
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
#' @param color Edge color. Can be a single color, vector, or "weight" for
#'   automatic coloring based on edge weights.
#' @param edge_positive_color Color for positive edge weights.
#' @param positive_color Deprecated. Use `edge_positive_color` instead.
#' @param edge_negative_color Color for negative edge weights.
#' @param negative_color Deprecated. Use `edge_negative_color` instead.
#' @param alpha Edge transparency (0-1).
#' @param style Line style: "solid", "dashed", "dotted", "longdash", "twodash".
#' @param curvature Edge curvature amount (0 = straight).
#' @param arrow_size Size of arrow heads for directed networks.
#' @param show_arrows Logical. Show arrows? Default TRUE for directed networks.
#' @param maximum Maximum edge weight for scaling width. Weights above this are
#'   capped. Similar to qgraph's maximum parameter.
#' @param width_scale Scale factor for edge widths. Values > 1 make edges thicker,
#'   values < 1 make them thinner. Applied after all other width calculations.
#' @param labels Edge labels. Can be TRUE (show weights), a vector, or column name.
#' @param label_size Edge label text size.
#' @param label_color Edge label text color.
#' @param label_position Position along edge (0 = source, 0.5 = middle, 1 = target).
#' @param label_offset Perpendicular offset from edge line.
#' @param label_bg Background color for edge labels (default "white"). Set to NA for transparent.
#' @param label_bg_padding Padding around label text as proportion of text size (default 0.3).
#' @param label_fontface Font face: "plain", "bold", "italic", "bold.italic" (default "plain").
#' @param label_border Border style: NULL (none), "rect", "rounded", "circle" (default NULL).
#' @param label_border_color Border color for label border (default "gray50").
#' @param label_underline Logical. Underline the label text? (default FALSE).
#' @param label_shadow Logical. Enable drop shadow for labels? (default FALSE).
#' @param label_shadow_color Color for label shadow (default "gray40").
#' @param label_shadow_offset Offset distance for shadow in points (default 0.5).
#' @param label_shadow_alpha Transparency for shadow (0-1, default 0.5).
#' @param bidirectional Logical. Show arrows at both ends of edges?
#' @param loop_rotation Angle in radians for self-loop direction (default: pi/2 = top).
#' @param curve_shape Spline tension for curved edges (-1 to 1, default: 0).
#' @param curve_pivot Pivot position along edge for curve control point (0-1, default: 0.5).
#' @param curves Curve mode: FALSE (straight edges), "mutual" (only curve reciprocal pairs),
#'   or "force" (curve all edges). Default FALSE.
#' @param ci Numeric vector of CI widths (0-1 scale). Larger values = more uncertainty.
#' @param ci_scale Width multiplier for CI underlay thickness. Default 2.
#' @param ci_alpha Transparency for CI underlay (0-1). Default 0.15.
#' @param ci_color CI underlay color. NA (default) uses main edge color.
#' @param ci_style Line type for CI underlay: 1=solid, 2=dashed, 3=dotted. Default 2.
#' @param ci_arrows Logical: show arrows on CI underlay? Default FALSE.
#' @param ci_lower Numeric vector of lower CI bounds for labels.
#' @param ci_upper Numeric vector of upper CI bounds for labels.
#' @param label_style Preset style: "none", "estimate", "full", "range", "stars".
#' @param label_template Template with placeholders: \{est\}, \{range\}, \{low\}, \{up\}, \{p\}, \{stars\}.
#' @param label_digits Decimal places for estimates in template. Default 2.
#' @param label_ci_format CI format: "bracket" for `[low, up]` or "dash" for `low-up`.
#' @param label_p Numeric vector of p-values for edges.
#' @param label_p_digits Decimal places for p-values. Default 3.
#' @param label_p_prefix Prefix for p-values. Default "p=".
#' @param label_stars Stars for labels: character vector, TRUE (compute from p),
#'   or numeric (treated as p-values).
#'
#' @details
#' ## Vectorization
#' Most aesthetic parameters can be specified as:
#' \itemize{
#'   \item \strong{Single value}: Applied to all edges
#'   \item \strong{Vector}: Per-edge values (must match edge count)
#'   \item \strong{"weight"}: Special value for \code{width} and \code{color} that
#'     auto-maps from edge weights
#' }
#'
#' ## Weight-Based Styling
#' When \code{color = "weight"}, edges are colored by sign:
#' \itemize{
#'   \item Positive weights use \code{edge_positive_color} (default: green)
#'   \item Negative weights use \code{edge_negative_color} (default: red)
#' }
#'
#' When \code{width = "weight"}, edge widths scale with absolute weight values,
#' respecting the \code{maximum} parameter if set.
#'
#' ## Edge Label Templates
#' For statistical output (e.g., regression coefficients with CIs), use templates:
#' \itemize{
#'   \item \code{label_template = "\{est\}"}: Show estimate only
#'   \item \code{label_template = "\{est\} [\{low\}, \{up\}]"}: Estimate with CI
#'   \item \code{label_template = "\{est\}\{stars\}"}: Estimate with significance
#' }
#'
#' Preset styles via \code{label_style}:
#' \itemize{
#'   \item \code{"estimate"}: Weight/estimate only
#'   \item \code{"full"}: Estimate + CI in brackets
#'   \item \code{"range"}: CI range only
#'   \item \code{"stars"}: Significance stars
#' }
#'
#' ## CI Underlays
#' Visualize uncertainty by drawing a wider, semi-transparent edge behind:
#' \itemize{
#'   \item \code{ci}: Vector of CI widths (0-1 scale)
#'   \item \code{ci_scale}: Width multiplier (default 2)
#'   \item \code{ci_alpha}: Transparency (default 0.15)
#' }
#'
#' @return Modified sonnet_network object that can be piped to further customization
#'   functions or plotting functions.
#'
#' @seealso
#' \code{\link{sn_nodes}} for node customization,
#' \code{\link{sonnet}} for network creation,
#' \code{\link{splot}} and \code{\link{soplot}} for plotting,
#' \code{\link{sn_layout}} for layout algorithms,
#' \code{\link{sn_theme}} for visual themes
#'
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, -0.5, 1, 0, 1, -0.5, 1, 0), nrow = 3)
#'
#' # Basic: auto-style by weight
#' sonnet(adj) |>
#'   sn_edges(width = "weight", color = "weight")
#'
#' # Direct matrix input (auto-converted)
#' adj |> sn_edges(width = 2, color = "gray50")
#'
#' # Custom positive/negative colors
#' sonnet(adj) |>
#'   sn_edges(
#'     color = "weight",
#'     edge_positive_color = "darkblue",
#'     edge_negative_color = "darkred"
#'   ) |>
#'   splot()
#'
#' # Edge labels showing weights
#' sonnet(adj) |>
#'   sn_edges(labels = TRUE, label_size = 0.8) |>
#'   splot()
#'
#' # Statistical output with CI template
#' # Suppose we have estimates, lower/upper CI bounds
#' estimates <- c(0.5, -0.3, 0.8)
#' ci_lo <- c(0.2, -0.6, 0.5)
#' ci_hi <- c(0.8, -0.1, 1.1)
#'
#' \dontrun{
#' sonnet(adj) |>
#'   sn_edges(
#'     label_template = "{est} [{low}, {up}]",
#'     ci_lower = ci_lo,
#'     ci_upper = ci_hi,
#'     label_digits = 2
#'   ) |>
#'   splot()
#' }
#'
#' # Curved edges for reciprocal pairs
#' sonnet(adj) |>
#'   sn_edges(curves = "mutual", curvature = 0.3) |>
#'   splot()
sn_edges <- function(network,
                     width = NULL,
                     edge_size = NULL,
                     esize = NULL,  # Deprecated: use edge_size
                     edge_width_range = NULL,
                     edge_scale_mode = NULL,
                     edge_cutoff = NULL,
                     cut = NULL,  # Deprecated: use edge_cutoff
                     color = NULL,
                     edge_positive_color = NULL,
                     positive_color = NULL,  # Deprecated: use edge_positive_color
                     edge_negative_color = NULL,
                     negative_color = NULL,  # Deprecated: use edge_negative_color
                     alpha = NULL,
                     style = NULL,
                     curvature = NULL,
                     arrow_size = NULL,
                     show_arrows = NULL,
                     maximum = NULL,
                     width_scale = NULL,
                     labels = NULL,
                     label_size = NULL,
                     label_color = NULL,
                     label_position = NULL,
                     label_offset = NULL,
                     label_bg = NULL,
                     label_bg_padding = NULL,
                     label_fontface = NULL,
                     label_border = NULL,
                     label_border_color = NULL,
                     label_underline = NULL,
                     label_shadow = NULL,
                     label_shadow_color = NULL,
                     label_shadow_offset = NULL,
                     label_shadow_alpha = NULL,
                     bidirectional = NULL,
                     loop_rotation = NULL,
                     curve_shape = NULL,
                     curve_pivot = NULL,
                     curves = NULL,
                     # CI underlay parameters
                     ci = NULL,
                     ci_scale = NULL,
                     ci_alpha = NULL,
                     ci_color = NULL,
                     ci_style = NULL,
                     ci_arrows = NULL,
                     # Label template parameters
                     ci_lower = NULL,
                     ci_upper = NULL,
                     label_style = NULL,
                     label_template = NULL,
                     label_digits = NULL,
                     label_ci_format = NULL,
                     label_p = NULL,
                     label_p_digits = NULL,
                     label_p_prefix = NULL,
                     label_stars = NULL) {

  # Handle deprecated parameters
  edge_size <- handle_deprecated_param(edge_size, esize, "edge_size", "esize")
  edge_cutoff <- handle_deprecated_param(edge_cutoff, cut, "edge_cutoff", "cut")
  edge_positive_color <- handle_deprecated_param(edge_positive_color, positive_color,
                                                  "edge_positive_color", "positive_color")
  edge_negative_color <- handle_deprecated_param(edge_negative_color, negative_color,
                                                  "edge_negative_color", "negative_color")

  # Auto-convert matrix/data.frame/igraph to sonnet_network
  network <- ensure_sonnet_network(network)

  # Clone the network to maintain immutability
  new_net <- network$network$clone_network()

  # Get edge count for validation
  edges_df <- new_net$get_edges()
  m <- if (is.null(edges_df)) 0 else nrow(edges_df)

  # Build aesthetics list
  aes <- list()

  if (!is.null(width)) {
    if (identical(width, "weight") && !is.null(edges_df$weight)) {
      # Scale width by weight, respecting maximum if set
      weights_for_scale <- abs(edges_df$weight)
      if (!is.null(maximum)) {
        weights_for_scale <- pmin(weights_for_scale, maximum)
      }
      aes$width <- scale_edge_widths_simple(weights_for_scale, maximum = maximum)
    } else {
      aes$width <- resolve_aesthetic(width, edges_df, m)
    }
  }

  if (!is.null(maximum)) {
    aes$maximum <- maximum
  }

  # Edge width scaling parameters
  if (!is.null(edge_size)) {
    aes$esize <- edge_size
  }

  if (!is.null(edge_width_range)) {
    aes$edge_width_range <- edge_width_range
  }

  if (!is.null(edge_scale_mode)) {
    valid_modes <- c("linear", "log", "sqrt", "rank")
    if (!edge_scale_mode %in% valid_modes) {
      stop("edge_scale_mode must be one of: ", paste(valid_modes, collapse = ", "),
           call. = FALSE)
    }
    aes$edge_scale_mode <- edge_scale_mode
  }

  if (!is.null(edge_cutoff)) {
    aes$cut <- edge_cutoff
  }

  if (!is.null(width_scale)) {
    aes$width_scale <- width_scale
  }

  if (!is.null(color)) {
    if (identical(color, "weight") && !is.null(edges_df$weight)) {
      # Color by weight sign: positive = green, negative = red
      current_aes <- new_net$get_edge_aes()
      pos_col <- if (!is.null(edge_positive_color)) edge_positive_color else current_aes$positive_color
      neg_col <- if (!is.null(edge_negative_color)) edge_negative_color else current_aes$negative_color
      aes$color <- ifelse(edges_df$weight >= 0, pos_col, neg_col)
    } else {
      aes$color <- resolve_aesthetic(color, edges_df, m)
    }
  }

  if (!is.null(edge_positive_color)) {
    aes$positive_color <- edge_positive_color
  }

  if (!is.null(edge_negative_color)) {
    aes$negative_color <- edge_negative_color
  }

  if (!is.null(alpha)) {
    validate_range(alpha, 0, 1, "alpha")
    aes$alpha <- resolve_aesthetic(alpha, edges_df, m)
  }

  if (!is.null(style)) {
    valid_styles <- c("solid", "dashed", "dotted", "longdash", "twodash")
    style_vals <- resolve_aesthetic(style, edges_df, m)
    if (!all(style_vals %in% valid_styles)) {
      stop("style must be one of: ", paste(valid_styles, collapse = ", "),
           call. = FALSE)
    }
    aes$style <- style_vals
  }

  if (!is.null(curvature)) {
    aes$curvature <- resolve_aesthetic(curvature, edges_df, m)
  }

  if (!is.null(arrow_size)) {
    aes$arrow_size <- arrow_size
  }

  if (!is.null(show_arrows)) {
    aes$show_arrows <- show_arrows
  }

  if (!is.null(labels)) {
    if (isTRUE(labels) && !is.null(edges_df$weight)) {
      # Auto-show weights as labels
      aes$labels <- round(edges_df$weight, 2)
    } else if (!isTRUE(labels) && !isFALSE(labels)) {
      aes$labels <- resolve_aesthetic(labels, edges_df, m)
    }
  }

  if (!is.null(label_size)) {
    aes$label_size <- label_size
  }

  if (!is.null(label_color)) {
    aes$label_color <- label_color
  }

  if (!is.null(label_position)) {
    aes$label_position <- label_position
  }

  if (!is.null(label_offset)) {
    aes$label_offset <- label_offset
  }

  if (!is.null(label_bg)) {
    aes$label_bg <- label_bg
  }

  if (!is.null(label_bg_padding)) {
    aes$label_bg_padding <- label_bg_padding
  }

  if (!is.null(label_fontface)) {
    valid_faces <- c("plain", "bold", "italic", "bold.italic")
    if (!label_fontface %in% valid_faces) {
      stop("label_fontface must be one of: ", paste(valid_faces, collapse = ", "),
           call. = FALSE)
    }
    aes$label_fontface <- label_fontface
  }

  if (!is.null(label_border)) {
    valid_borders <- c("rect", "rounded", "circle")
    if (!label_border %in% valid_borders) {
      stop("label_border must be one of: ", paste(valid_borders, collapse = ", "),
           call. = FALSE)
    }
    aes$label_border <- label_border
  }

  if (!is.null(label_border_color)) {
    aes$label_border_color <- label_border_color
  }

  if (!is.null(label_underline)) {
    aes$label_underline <- label_underline
  }

  if (!is.null(label_shadow)) {
    aes$label_shadow <- label_shadow
  }

  if (!is.null(label_shadow_color)) {
    aes$label_shadow_color <- label_shadow_color
  }

  if (!is.null(label_shadow_offset)) {
    aes$label_shadow_offset <- label_shadow_offset
  }

  if (!is.null(label_shadow_alpha)) {
    validate_range(label_shadow_alpha, 0, 1, "label_shadow_alpha")
    aes$label_shadow_alpha <- label_shadow_alpha
  }

  if (!is.null(bidirectional)) {
    aes$bidirectional <- resolve_aesthetic(bidirectional, edges_df, m)
  }

  if (!is.null(loop_rotation)) {
    aes$loop_rotation <- resolve_aesthetic(loop_rotation, edges_df, m)
  }

  if (!is.null(curve_shape)) {
    aes$curve_shape <- resolve_aesthetic(curve_shape, edges_df, m)
  }

  if (!is.null(curve_pivot)) {
    aes$curve_pivot <- resolve_aesthetic(curve_pivot, edges_df, m)
  }

  if (!is.null(curves)) {
    if (!isFALSE(curves) && !curves %in% c("mutual", "force")) {
      stop("curves must be FALSE, 'mutual', or 'force'", call. = FALSE)
    }
    aes$curves <- curves
  }

  # CI underlay parameters
  if (!is.null(ci)) {
    aes$ci <- resolve_aesthetic(ci, edges_df, m)
  }

  if (!is.null(ci_scale)) {
    aes$ci_scale <- ci_scale
  }

  if (!is.null(ci_alpha)) {
    validate_range(ci_alpha, 0, 1, "ci_alpha")
    aes$ci_alpha <- ci_alpha
  }

  if (!is.null(ci_color)) {
    aes$ci_color <- ci_color
  }

  if (!is.null(ci_style)) {
    aes$ci_style <- ci_style
  }

  if (!is.null(ci_arrows)) {
    aes$ci_arrows <- ci_arrows
  }

  # Label template parameters
  if (!is.null(ci_lower)) {
    aes$ci_lower <- resolve_aesthetic(ci_lower, edges_df, m)
  }

  if (!is.null(ci_upper)) {
    aes$ci_upper <- resolve_aesthetic(ci_upper, edges_df, m)
  }

  if (!is.null(label_style)) {
    valid_styles <- c("none", "estimate", "full", "range", "stars")
    if (!label_style %in% valid_styles) {
      stop("label_style must be one of: ", paste(valid_styles, collapse = ", "),
           call. = FALSE)
    }
    aes$label_style <- label_style
  }

  if (!is.null(label_template)) {
    aes$label_template <- label_template
  }

  if (!is.null(label_digits)) {
    aes$label_digits <- label_digits
  }

  if (!is.null(label_ci_format)) {
    valid_formats <- c("bracket", "dash")
    if (!label_ci_format %in% valid_formats) {
      stop("label_ci_format must be one of: ", paste(valid_formats, collapse = ", "),
           call. = FALSE)
    }
    aes$label_ci_format <- label_ci_format
  }

  if (!is.null(label_p)) {
    aes$label_p <- resolve_aesthetic(label_p, edges_df, m)
  }

  if (!is.null(label_p_digits)) {
    aes$label_p_digits <- label_p_digits
  }

  if (!is.null(label_p_prefix)) {
    aes$label_p_prefix <- label_p_prefix
  }

  if (!is.null(label_stars)) {
    aes$label_stars <- label_stars
  }

  # Apply aesthetics
  new_net$set_edge_aes(aes)

  # Return wrapped object
  as_sonnet_network(new_net)
}

#' Scale Edge Widths (Simple Version)
#'
#' Simple linear edge width scaling used by sn_edges() when width="weight".
#' For the full-featured version with multiple modes and cut parameter,
#' see scale_edge_widths() in scale-constants.R.
#'
#' @param values Numeric values to scale.
#' @param range Target width range (min, max).
#' @param maximum Optional maximum value for scaling. If provided, this value
#'   maps to the max of range, and values above it are capped.
#' @return Scaled width values.
#' @keywords internal
scale_edge_widths_simple <- function(values, range = c(0.5, 3), maximum = NULL) {
  if (all(is.na(values))) return(rep(mean(range), length(values)))

  # Use maximum as the upper bound if provided
  if (!is.null(maximum)) {
    val_min <- 0
    val_max <- maximum
    # Cap values at maximum
    values <- pmin(values, maximum)
  } else {
    val_min <- min(values, na.rm = TRUE)
    val_max <- max(values, na.rm = TRUE)
  }

  if (val_max == val_min) {
    return(rep(mean(range), length(values)))
  }

  # Linear scaling
  scaled <- (values - val_min) / (val_max - val_min)
  range[1] + scaled * diff(range)
}

#' Map Edge Colors by Weight
#'
#' Map edge colors based on weight values.
#'
#' @param weights Numeric weight values.
#' @param positive_color Color for positive weights.
#' @param negative_color Color for negative weights.
#' @param zero_color Color for zero weights.
#' @return Character vector of colors.
#' @keywords internal
map_edge_colors <- function(weights, positive_color = "#2E7D32",
                            negative_color = "#C62828",
                            zero_color = "gray50") {
  colors <- character(length(weights))
  colors[weights > 0] <- positive_color
  colors[weights < 0] <- negative_color
  colors[weights == 0] <- zero_color
  colors[is.na(weights)] <- zero_color
  colors
}
