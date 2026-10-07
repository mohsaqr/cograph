#' @title Node Aesthetics
#' @description Functions for setting node aesthetic properties.
#' @name aes-nodes
#' @keywords internal
#' @noRd
NULL

#' Set Node Aesthetics
#'
#' Sets the visual properties of the nodes in a network plot. Every argument
#' except \code{network} defaults to \code{NULL}, which leaves the current setting unchanged; the
#' defaults stated below are the values used at render time.
#'
#' @param network A cograph_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param size Node size. Can be a single value, vector (per-node), or column name.
#' @param shape Node shape. One of the shapes listed by \code{\link{list_shapes}()}
#'   (for example "circle", "square", "triangle", "diamond", "pentagon",
#'   "hexagon", "ellipse", "heart", "star", "pie", "donut", "cross",
#'   "rectangle"), or a custom SVG shape registered with
#'   \code{\link{register_svg_shape}()}.
#' @param node_svg Custom SVG for the node shape: a path to an SVG file or an
#'   inline SVG string. It is registered as a temporary shape and replaces
#'   \code{shape}.
#' @param svg_preserve_aspect Logical: maintain the SVG aspect ratio? The value
#'   is stored with the network, but the renderer always preserves the aspect
#'   ratio, so it has no effect at present.
#' @param fill Node fill color. Can be a single color, vector, or column name.
#' @param border_color Node border color.
#' @param border_width Node border width.
#' @param alpha Node opacity in \[0, 1\]. Values outside this range raise an error.
#' @param label_size Label text size.
#' @param label_color Label text color.
#' @param label_position Label position: "center" (render default), "above",
#'   "below", "left" or "right".
#' @param show_labels Logical. Show node labels? Default TRUE.
#' @param pie_values For pie shape: list or matrix of values for pie segments.
#'   Each element corresponds to a node and contains values for its segments.
#' @param pie_colors For pie shape: colors for pie segments.
#' @param pie_border_width Border width for pie chart nodes.
#' @param donut_fill For donut shape: numeric value (0-1) specifying fill proportion.
#'   A value of 0.5 fills half of the ring and 1 fills the whole ring.
#'   Can be a single value (all nodes) or vector (per-node values).
#' @param donut_values Deprecated. Use \code{donut_fill}. Ignored when
#'   \code{donut_fill} is supplied.
#' @param donut_color For donut shape: fill color(s) for the donut ring.
#'   Single color sets fill for all nodes.
#'   Two colors set fill and background for all nodes.
#'   More than 2 colors set per-node fill colors (recycled to n_nodes).
#'   The render default is a "maroon" fill on a "gray90" background.
#' @param donut_colors Deprecated. Use \code{donut_color}. Ignored when
#'   \code{donut_color} is supplied.
#' @param donut_border_width Border width for donut chart nodes.
#' @param donut_inner_ratio For donut shape: inner radius ratio (0-1). Default 0.5.
#' @param donut_bg_color For donut shape: background color for unfilled portion.
#' @param donut_shape For donut: base shape for ring ("circle", "square",
#'   "hexagon", "triangle", "diamond", "pentagon"). Default NULL, which inherits
#'   the ring shape from the node's own shape (hexagon nodes get hexagon
#'   donuts); set it explicitly to override that for every node.
#' @param donut_show_value For donut shape: show value in center? Default FALSE.
#' @param donut_value_size For donut shape: font size for center value.
#' @param donut_value_color For donut shape: color for center value text.
#' @param donut_value_fontface For donut shape: font face for center value ("plain", "bold", "italic", "bold.italic"). Default "bold".
#' @param donut_value_fontfamily For donut shape: font family for center value ("sans", "serif", "mono"). Default "sans".
#' @param donut_value_digits For donut shape: decimal places for value display. Default 2.
#' @param donut_value_prefix For donut shape: text before value (e.g., "$"). Default "".
#' @param donut_value_suffix For donut shape: text after value (e.g., "%"). Default "".
#' @param donut_value_format For donut shape: a function that formats the
#'   center value. It replaces \code{donut_value_digits}. A non-function raises
#'   an error.
#' @param donut2_values For double donut: list of values for inner donut ring.
#' @param donut2_colors For double donut: colors for inner donut ring segments.
#' @param donut2_inner_ratio For double donut: inner radius ratio for inner donut ring. Default 0.4.
#' @param label_fontface Font face for node labels: "plain", "bold", "italic", "bold.italic". Default "plain".
#' @param label_fontfamily Font family for node labels: "sans", "serif", "mono", or system font. Default "sans".
#' @param label_hjust Horizontal justification for node labels (0=left, 0.5=center, 1=right). Default 0.5.
#' @param label_vjust Vertical justification for node labels (0=bottom, 0.5=center, 1=top). Default 0.5.
#' @param label_angle Text rotation angle in degrees for node labels. Default 0.
#' @param node_names Alternative names for legend (separate from display labels).
#'
#' @details
#' ## Vectorization
#' The arguments \code{size}, \code{shape}, \code{fill}, \code{border_color},
#' \code{border_width}, \code{alpha}, \code{label_size}, \code{label_color},
#' \code{label_position} and \code{node_names} accept a single value, which is
#' applied to all nodes, or a per-node vector. A vector of another length is
#' recycled to the number of nodes without a warning. A single string that
#' matches a column of the node data frame is replaced by that column.
#'
#' ## Donut Charts
#' A donut node shows one proportion in \[0, 1\] per node. \code{donut_fill}
#' sets the proportion, \code{donut_color} the ring color, \code{donut_shape}
#' the base shape of the ring, and \code{donut_show_value = TRUE} prints the
#' value in the center.
#'
#' @return The input as a \code{cograph_network} object, with the supplied
#'   settings merged into its node aesthetics. It can be piped to further
#'   customization or plotting functions.
#'
#' @seealso
#' \code{\link{sn_edges}} for edge customization,
#' \code{\link{cograph}} for network creation,
#' \code{\link{splot}} and \code{\link{soplot}} for plotting,
#' \code{\link{sn_layout}} for layout algorithms,
#' \code{\link{sn_theme}} for visual themes
#'
#' @export
#'
#' @examples
#' cograph(regulation_net) |>
#'   sn_nodes(fill = "steelblue", shape = "square") |>
#'   splot()
sn_nodes <- function(network,
                     size = NULL,
                     shape = NULL,
                     node_svg = NULL,
                     svg_preserve_aspect = NULL,
                     fill = NULL,
                     border_color = NULL,
                     border_width = NULL,
                     alpha = NULL,
                     label_size = NULL,
                     label_color = NULL,
                     label_position = NULL,
                     show_labels = NULL,
                     pie_values = NULL,
                     pie_colors = NULL,
                     pie_border_width = NULL,
                     donut_fill = NULL,
                     donut_values = NULL,
                     donut_color = NULL,
                     donut_colors = NULL,  # Deprecated: use donut_color
                     donut_border_width = NULL,
                     donut_inner_ratio = NULL,
                     donut_bg_color = NULL,
                     donut_shape = NULL,
                     donut_show_value = NULL,
                     donut_value_size = NULL,
                     donut_value_color = NULL,
                     donut_value_fontface = NULL,
                     donut_value_fontfamily = NULL,
                     donut_value_digits = NULL,
                     donut_value_prefix = NULL,
                     donut_value_suffix = NULL,
                     donut_value_format = NULL,
                     donut2_values = NULL,
                     donut2_colors = NULL,
                     donut2_inner_ratio = NULL,
                     label_fontface = NULL,
                     label_fontfamily = NULL,
                     label_hjust = NULL,
                     label_vjust = NULL,
                     label_angle = NULL,
                     node_names = NULL) {

  # Auto-convert matrix/data.frame/igraph to cograph_network
  network <- ensure_cograph_network(network)

  # Get node count for validation
  n <- n_nodes(network)
  nodes_df <- get_nodes(network)

  # Build aesthetics list
  aes <- list()

  if (!is.null(size)) {
    aes$size <- resolve_aesthetic(size, nodes_df, n)
  }

  if (!is.null(shape)) {
    aes$shape <- resolve_aesthetic(shape, nodes_df, n)
  }

  if (!is.null(node_svg)) {
    aes$node_svg <- node_svg
    # Register as temporary SVG shape if not already registered
    if (!is.null(node_svg) && is.character(node_svg)) {
      if (requireNamespace("digest", quietly = TRUE)) {
        temp_name <- paste0("_temp_svg_", substr(digest::digest(node_svg), 1, 8))
      } else { # nocov start
        hash_val <- sum(utf8ToInt(substr(node_svg, 1, 200))) %% 1e8
        temp_name <- paste0("_temp_svg_", formatC(as.integer(hash_val), width = 8, flag = "0"))
      } # nocov end
      if (!temp_name %in% list_svg_shapes()) {
        tryCatch(
          register_svg_shape(temp_name, node_svg),
          error = function(e) warning("Failed to register SVG: ", e$message, call. = FALSE)
        )
      }
      aes$shape <- temp_name
    }
  }

  if (!is.null(svg_preserve_aspect)) {
    aes$svg_preserve_aspect <- svg_preserve_aspect
  }

  if (!is.null(fill)) {
    aes$fill <- resolve_aesthetic(fill, nodes_df, n)
  }

  if (!is.null(border_color)) {
    aes$border_color <- resolve_aesthetic(border_color, nodes_df, n)
  }

  if (!is.null(border_width)) {
    aes$border_width <- resolve_aesthetic(border_width, nodes_df, n)
  }

  if (!is.null(alpha)) {
    validate_range(alpha, 0, 1, "alpha")
    aes$alpha <- resolve_aesthetic(alpha, nodes_df, n)
  }

  if (!is.null(label_size)) {
    aes$label_size <- resolve_aesthetic(label_size, nodes_df, n)
  }

  if (!is.null(label_color)) {
    aes$label_color <- resolve_aesthetic(label_color, nodes_df, n)
  }

  if (!is.null(label_position)) {
    valid_pos <- c("center", "above", "below", "left", "right")
    if (!all(label_position %in% valid_pos)) {
      stop("label_position must be one of: ", paste(valid_pos, collapse = ", "),
           call. = FALSE)
    }
    aes$label_position <- resolve_aesthetic(label_position, nodes_df, n)
  }

  if (!is.null(show_labels)) {
    aes$show_labels <- show_labels
  }

  if (!is.null(pie_values)) {
    aes$pie_values <- pie_values
  }

  if (!is.null(pie_colors)) {
    aes$pie_colors <- pie_colors
  }

  if (!is.null(pie_border_width)) {
    aes$pie_border_width <- pie_border_width
  }

  if (!is.null(donut_fill)) {
    # donut_fill is the new simplified API - single value (0-1) for fill proportion
    # Convert to list format for internal use
    aes$donut_fill <- donut_fill
    # Also set donut_values for backwards compatibility with drawing functions
    aes$donut_values <- donut_fill
  } else if (!is.null(donut_values)) {
    aes$donut_values <- donut_values
  }

  if (!is.null(donut_color)) {
    aes$donut_color <- donut_color
  } else if (!is.null(donut_colors)) {
    # Deprecated: use donut_colors as fallback
    aes$donut_color <- donut_colors
  }

  if (!is.null(donut_border_width)) {
    aes$donut_border_width <- donut_border_width
  }

  if (!is.null(donut_inner_ratio)) {
    aes$donut_inner_ratio <- donut_inner_ratio
  }

  if (!is.null(donut_bg_color)) {
    aes$donut_bg_color <- donut_bg_color
  }

  if (!is.null(donut_shape)) {
    valid_shapes <- c("circle", "square", "hexagon", "triangle", "diamond", "pentagon")
    # Handle vectorized donut_shape (can be per-node)
    if (!all(donut_shape %in% valid_shapes)) {
      invalid <- unique(donut_shape[!donut_shape %in% valid_shapes])
      stop("donut_shape must be one of: ", paste(valid_shapes, collapse = ", "),
           ". Invalid values: ", paste(invalid, collapse = ", "),
           call. = FALSE)
    }
    aes$donut_shape <- donut_shape
  }

  if (!is.null(donut_show_value)) {
    aes$donut_show_value <- donut_show_value
  }

  if (!is.null(donut_value_size)) {
    aes$donut_value_size <- donut_value_size
  }

  if (!is.null(donut_value_color)) {
    aes$donut_value_color <- donut_value_color
  }

  if (!is.null(donut_value_fontface)) {
    valid_faces <- c("plain", "bold", "italic", "bold.italic")
    if (!donut_value_fontface %in% valid_faces) {
      stop("donut_value_fontface must be one of: ", paste(valid_faces, collapse = ", "),
           call. = FALSE)
    }
    aes$donut_value_fontface <- donut_value_fontface
  }

  if (!is.null(donut_value_fontfamily)) {
    aes$donut_value_fontfamily <- donut_value_fontfamily
  }

  if (!is.null(donut_value_digits)) {
    aes$donut_value_digits <- donut_value_digits
  }

  if (!is.null(donut_value_prefix)) {
    aes$donut_value_prefix <- donut_value_prefix
  }

  if (!is.null(donut_value_suffix)) {
    aes$donut_value_suffix <- donut_value_suffix
  }

  if (!is.null(donut_value_format)) {
    if (!is.function(donut_value_format)) {
      stop("donut_value_format must be a function", call. = FALSE)
    }
    aes$donut_value_format <- donut_value_format
  }

  if (!is.null(donut2_values)) {
    aes$donut2_values <- donut2_values
  }

  if (!is.null(donut2_colors)) {
    aes$donut2_colors <- donut2_colors
  }

  if (!is.null(donut2_inner_ratio)) {
    aes$donut2_inner_ratio <- donut2_inner_ratio
  }

  if (!is.null(label_fontface)) {
    valid_faces <- c("plain", "bold", "italic", "bold.italic")
    if (!label_fontface %in% valid_faces) {
      stop("label_fontface must be one of: ", paste(valid_faces, collapse = ", "),
           call. = FALSE)
    }
    aes$label_fontface <- label_fontface
  }

  if (!is.null(label_fontfamily)) {
    aes$label_fontfamily <- label_fontfamily
  }

  if (!is.null(label_hjust)) {
    aes$label_hjust <- label_hjust
  }

  if (!is.null(label_vjust)) {
    aes$label_vjust <- label_vjust
  }

  if (!is.null(label_angle)) {
    aes$label_angle <- label_angle
  }

  if (!is.null(node_names)) {
    aes$node_names <- resolve_aesthetic(node_names, nodes_df, n)
  }

  # Apply aesthetics to network (merge with existing)
  if (is.null(network$node_aes)) {
    network$node_aes <- list()
  }
  network$node_aes <- utils::modifyList(network$node_aes, aes)

  # Return modified network
  network
}

#' Map Node Colors by Group
#'
#' Helper function to map node colors based on group membership.
#'
#' @param groups Vector of group assignments.
#' @param palette Color palette (function or character vector).
#' @return Character vector of colors.
#' @keywords internal
#' @noRd
map_node_colors <- function(groups, palette = NULL) {
  groups <- as.factor(groups)
  n_groups <- length(levels(groups))

  if (is.null(palette)) {
    colors <- palette_colorblind(n_groups)
  } else if (is.function(palette)) {
    colors <- palette(n_groups)
  } else {
    colors <- rep(palette, length.out = n_groups)
  }

  colors[as.integer(groups)]
}

#' Scale Node Sizes
#'
#' Scale node sizes based on a numeric variable.
#'
#' @param values Numeric values to scale.
#' @param range Target size range (min, max).
#' @return Scaled size values.
#' @keywords internal
#' @noRd
scale_node_sizes <- function(values, range = c(0.03, 0.1)) {
  if (all(is.na(values))) return(rep(mean(range), length(values)))

  val_range <- range(values, na.rm = TRUE)

  if (diff(val_range) == 0) {
    return(rep(mean(range), length(values)))
  }

  # Linear scaling
  scaled <- (values - val_range[1]) / diff(val_range)
  range[1] + scaled * diff(range)
}
