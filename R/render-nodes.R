#' @title Node Rendering
#' @description Functions for rendering nodes using grid graphics.
#' @name render-nodes
#' @keywords internal
NULL

#' Render All Nodes
#'
#' Create grid grobs for all nodes in the network.
#'
#' @param network A SonnetNetwork object.
#' @return A grid gList of node grobs.
#' @keywords internal
render_nodes_grid <- function(network) {
  nodes <- network$get_nodes()
  aes <- network$get_node_aes()
  theme <- network$get_theme()
  n <- nrow(nodes)

  if (n == 0) return(grid::gList())

  # Resolve aesthetics to per-node values
  sizes <- recycle_to_length(
    if (!is.null(aes$size)) aes$size else theme$get("node_border_width") * 0.01 + 0.04,
    n
  )
  shapes <- recycle_to_length(
    if (!is.null(aes$shape)) aes$shape else "circle",
    n
  )
  fills <- recycle_to_length(
    if (!is.null(aes$fill)) aes$fill else theme$get("node_fill"),
    n
  )
  border_colors <- recycle_to_length(
    if (!is.null(aes$border_color)) aes$border_color else theme$get("node_border"),
    n
  )
  border_widths <- recycle_to_length(
    if (!is.null(aes$border_width)) aes$border_width else theme$get("node_border_width"),
    n
  )
  alphas <- recycle_to_length(
    if (!is.null(aes$alpha)) aes$alpha else 1,
    n
  )

  # Create grobs for each node
  grobs <- vector("list", n)
  for (i in seq_len(n)) {
    shape_fn <- get_shape(shapes[i])
    if (is.null(shape_fn)) {
      shape_fn <- get_shape("circle")
    }

    # Additional arguments for special shapes
    extra_args <- list()

    # Check if this node should render as a donut (based on donut_values)
    has_donut_value <- !is.null(aes$donut_values) && length(aes$donut_values) >= i &&
                       !is.null(aes$donut_values[[i]]) && !is.na(aes$donut_values[[i]])

    # If donut_values is set for this node, render as donut (override shape)
    if (has_donut_value && !shapes[i] %in% c("donut", "polygon_donut", "pie", "donut_pie", "double_donut_pie")) {
      # Determine donut shape from aes$donut_shape
      effective_donut_shape <- if (!is.null(aes$donut_shape)) {
        if (length(aes$donut_shape) >= i) aes$donut_shape[i] else aes$donut_shape[1]
      } else {
        "circle"
      }

      # Use polygon_donut for non-circle shapes, regular donut for circles
      if (effective_donut_shape != "circle") {
        shape_fn <- get_shape("polygon_donut")
        extra_args$donut_shape <- effective_donut_shape
      } else {
        shape_fn <- get_shape("donut")
      }

      # Pass donut value
      extra_args$values <- aes$donut_values[[i]]

      # Pass donut colors
      if (!is.null(aes$donut_colors)) {
        if (is.list(aes$donut_colors) && length(aes$donut_colors) >= i) {
          extra_args$colors <- aes$donut_colors[[i]]
        } else if (!is.list(aes$donut_colors)) {
          extra_args$colors <- aes$donut_colors[1]
        }
      }

      # Pass all donut parameters
      if (!is.null(aes$donut_inner_ratio)) extra_args$inner_ratio <- aes$donut_inner_ratio
      if (!is.null(aes$donut_bg_color)) extra_args$bg_color <- aes$donut_bg_color
      if (!is.null(aes$donut_show_value)) extra_args$show_value <- aes$donut_show_value
      if (!is.null(aes$donut_value_size)) extra_args$value_size <- aes$donut_value_size
      if (!is.null(aes$donut_value_color)) extra_args$value_color <- aes$donut_value_color
      if (!is.null(aes$donut_value_fontface)) extra_args$value_fontface <- aes$donut_value_fontface
      if (!is.null(aes$donut_value_fontfamily)) extra_args$value_fontfamily <- aes$donut_value_fontfamily
      if (!is.null(aes$donut_value_digits)) extra_args$value_digits <- aes$donut_value_digits
      if (!is.null(aes$donut_value_prefix)) extra_args$value_prefix <- aes$donut_value_prefix
      if (!is.null(aes$donut_value_suffix)) extra_args$value_suffix <- aes$donut_value_suffix
      if (!is.null(aes$donut_border_width)) extra_args$donut_border_width <- aes$donut_border_width
    }

    if (shapes[i] %in% c("pie", "donut") && !is.null(aes$pie_values)) {
      if (is.list(aes$pie_values)) {
        extra_args$values <- aes$pie_values[[i]]
      } else if (is.matrix(aes$pie_values)) {
        extra_args$values <- aes$pie_values[i, ]
      } else if (is.numeric(aes$pie_values)) {
        # Single values per node (for donut)
        extra_args$values <- aes$pie_values[i]
      }
      if (!is.null(aes$pie_colors)) {
        extra_args$colors <- aes$pie_colors
      }
    }
    # Pie-specific border width
    if (shapes[i] == "pie" && !is.null(aes$pie_border_width)) {
      extra_args$pie_border_width <- aes$pie_border_width
    }
    # Donut-specific parameters
    if (shapes[i] == "donut" || shapes[i] == "polygon_donut") {
      # Pass donut_values as values (for explicit donut shapes)
      if (!is.null(aes$donut_values) && length(aes$donut_values) >= i &&
          !is.null(aes$donut_values[[i]]) && !is.na(aes$donut_values[[i]])) {
        extra_args$values <- aes$donut_values[[i]]
      } else {
        # Default to 1.0 (fully filled) for explicit donut shapes without values
        extra_args$values <- 1.0
      }
      # Pass donut_colors as colors
      if (!is.null(aes$donut_colors)) {
        if (is.list(aes$donut_colors) && length(aes$donut_colors) >= i) {
          extra_args$colors <- aes$donut_colors[[i]]
        } else if (!is.list(aes$donut_colors)) {
          extra_args$colors <- aes$donut_colors[1]
        }
      }
      if (!is.null(aes$donut_inner_ratio)) {
        extra_args$inner_ratio <- aes$donut_inner_ratio
      }
      if (!is.null(aes$donut_bg_color)) {
        extra_args$bg_color <- aes$donut_bg_color
      }
      if (!is.null(aes$donut_shape)) {
        # Handle vectorized donut_shape
        if (length(aes$donut_shape) >= i) {
          extra_args$donut_shape <- aes$donut_shape[i]
        } else {
          extra_args$donut_shape <- aes$donut_shape[1]
        }
      }
      if (!is.null(aes$donut_show_value)) {
        extra_args$show_value <- aes$donut_show_value
      }
      if (!is.null(aes$donut_value_size)) {
        extra_args$value_size <- aes$donut_value_size
      }
      if (!is.null(aes$donut_value_color)) {
        extra_args$value_color <- aes$donut_value_color
      }
      if (!is.null(aes$donut_value_fontface)) {
        extra_args$value_fontface <- aes$donut_value_fontface
      }
      if (!is.null(aes$donut_value_fontfamily)) {
        extra_args$value_fontfamily <- aes$donut_value_fontfamily
      }
      if (!is.null(aes$donut_value_digits)) {
        extra_args$value_digits <- aes$donut_value_digits
      }
      if (!is.null(aes$donut_value_prefix)) {
        extra_args$value_prefix <- aes$donut_value_prefix
      }
      if (!is.null(aes$donut_value_suffix)) {
        extra_args$value_suffix <- aes$donut_value_suffix
      }
      if (!is.null(aes$donut_value_format)) {
        extra_args$value_format <- aes$donut_value_format
      }
      if (!is.null(aes$donut_border_width)) {
        extra_args$donut_border_width <- aes$donut_border_width
      }
    }
    # Donut+Pie combined shape parameters
    if (shapes[i] == "donut_pie") {
      # Donut value (outer ring proportion)
      if (!is.null(aes$donut_values)) {
        if (length(aes$donut_values) >= i) {
          extra_args$donut_value <- aes$donut_values[i]
        }
      }
      # Pie values (inner segments)
      if (!is.null(aes$pie_values)) {
        if (is.list(aes$pie_values)) {
          extra_args$pie_values <- aes$pie_values[[i]]
        } else if (is.matrix(aes$pie_values)) {
          extra_args$pie_values <- aes$pie_values[i, ]
        }
      }
      if (!is.null(aes$pie_colors)) {
        extra_args$pie_colors <- aes$pie_colors
      }
      if (!is.null(aes$donut_inner_ratio)) {
        extra_args$inner_ratio <- aes$donut_inner_ratio
      }
      if (!is.null(aes$donut_bg_color)) {
        extra_args$bg_color <- aes$donut_bg_color
      }
      # Border width parameters
      if (!is.null(aes$pie_border_width)) {
        extra_args$pie_border_width <- aes$pie_border_width
      }
      if (!is.null(aes$donut_border_width)) {
        extra_args$donut_border_width <- aes$donut_border_width
      }
    }
    # Double donut + pie shape parameters
    if (shapes[i] == "double_donut_pie") {
      # Outer donut values
      if (!is.null(aes$donut_values)) {
        if (is.list(aes$donut_values)) {
          extra_args$donut_values <- aes$donut_values[[i]]
        } else if (length(aes$donut_values) >= i) {
          extra_args$donut_values <- aes$donut_values[i]
        }
      }
      if (!is.null(aes$donut_colors)) {
        if (is.list(aes$donut_colors)) {
          extra_args$donut_colors <- aes$donut_colors[[i]]
        } else {
          extra_args$donut_colors <- aes$donut_colors
        }
      }
      # Inner donut values
      if (!is.null(aes$donut2_values)) {
        if (is.list(aes$donut2_values)) {
          extra_args$donut2_values <- aes$donut2_values[[i]]
        } else if (length(aes$donut2_values) >= i) {
          extra_args$donut2_values <- aes$donut2_values[i]
        }
      }
      if (!is.null(aes$donut2_colors)) {
        if (is.list(aes$donut2_colors)) {
          extra_args$donut2_colors <- aes$donut2_colors[[i]]
        } else {
          extra_args$donut2_colors <- aes$donut2_colors
        }
      }
      # Pie values (inner segments)
      if (!is.null(aes$pie_values)) {
        if (is.list(aes$pie_values)) {
          extra_args$pie_values <- aes$pie_values[[i]]
        } else if (is.matrix(aes$pie_values)) {
          extra_args$pie_values <- aes$pie_values[i, ]
        }
      }
      if (!is.null(aes$pie_colors)) {
        extra_args$pie_colors <- aes$pie_colors
      }
      if (!is.null(aes$donut_inner_ratio)) {
        extra_args$outer_inner_ratio <- aes$donut_inner_ratio
      }
      if (!is.null(aes$donut2_inner_ratio)) {
        extra_args$inner_inner_ratio <- aes$donut2_inner_ratio
      }
      if (!is.null(aes$donut_bg_color)) {
        extra_args$bg_color <- aes$donut_bg_color
      }
      # Border width parameters
      if (!is.null(aes$pie_border_width)) {
        extra_args$pie_border_width <- aes$pie_border_width
      }
      if (!is.null(aes$donut_border_width)) {
        extra_args$donut_border_width <- aes$donut_border_width
      }
    }

    grobs[[i]] <- do.call(shape_fn, c(list(
      x = nodes$x[i],
      y = nodes$y[i],
      size = sizes[i],
      fill = fills[i],
      border_color = border_colors[i],
      border_width = border_widths[i],
      alpha = alphas[i]
    ), extra_args))
  }

  do.call(grid::gList, grobs)
}

#' Render Node Labels
#'
#' Create grid grobs for node labels.
#'
#' @param network A SonnetNetwork object.
#' @return A grid gList of label grobs.
#' @keywords internal
render_node_labels_grid <- function(network) {
  nodes <- network$get_nodes()
  aes <- network$get_node_aes()
  theme <- network$get_theme()
  n <- nrow(nodes)

  if (n == 0) return(grid::gList())

  # Check if labels should be shown
  show_labels <- if (!is.null(aes$show_labels)) aes$show_labels else TRUE
  if (!show_labels) return(grid::gList())

  # Resolve aesthetics
  labels <- if (!is.null(nodes$label)) nodes$label else as.character(seq_len(n))
  sizes <- recycle_to_length(
    if (!is.null(aes$size)) aes$size else 0.05,
    n
  )
  label_sizes <- recycle_to_length(
    if (!is.null(aes$label_size)) aes$label_size else theme$get("label_size"),
    n
  )
  label_colors <- recycle_to_length(
    if (!is.null(aes$label_color)) aes$label_color else theme$get("label_color"),
    n
  )
  positions <- recycle_to_length(
    if (!is.null(aes$label_position)) aes$label_position else "center",
    n
  )

  # Label font properties
  label_fontfaces <- recycle_to_length(
    if (!is.null(aes$label_fontface)) aes$label_fontface else "plain",
    n
  )
  label_fontfamilies <- recycle_to_length(
    if (!is.null(aes$label_fontfamily)) aes$label_fontfamily else "sans",
    n
  )
  label_hjusts <- recycle_to_length(
    if (!is.null(aes$label_hjust)) aes$label_hjust else 0.5,
    n
  )
  label_vjusts <- recycle_to_length(
    if (!is.null(aes$label_vjust)) aes$label_vjust else 0.5,
    n
  )
  label_angles <- recycle_to_length(
    if (!is.null(aes$label_angle)) aes$label_angle else 0,
    n
  )

  # Create label grobs
  grobs <- vector("list", n)
  for (i in seq_len(n)) {
    # Calculate label position
    x <- nodes$x[i]
    y <- nodes$y[i]

    offset <- sizes[i] + 0.02

    switch(positions[i],
      above = { y <- y + offset },
      below = { y <- y - offset },
      left = { x <- x - offset },
      right = { x <- x + offset }
    )

    # Convert fontface string to numeric
    fontface_num <- switch(label_fontfaces[i],
      "plain" = 1,
      "bold" = 2,
      "italic" = 3,
      "bold.italic" = 4,
      1
    )

    grobs[[i]] <- grid::textGrob(
      label = labels[i],
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      hjust = label_hjusts[i],
      vjust = label_vjusts[i],
      rot = label_angles[i],
      gp = grid::gpar(
        fontsize = label_sizes[i],
        col = label_colors[i],
        fontface = fontface_num,
        fontfamily = label_fontfamilies[i]
      )
    )
  }

  do.call(grid::gList, grobs)
}
