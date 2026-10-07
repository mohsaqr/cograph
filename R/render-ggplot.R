#' @title ggplot2 Conversion
#' @description \code{\link{sn_ggplot}()} converts a network to a ggplot object.
#' @name render-ggplot
#' @return \code{sn_ggplot()} returns a ggplot object representing the network.
#' @examples
#' sn_ggplot(regulation_net)
NULL

#' Convert Network to ggplot2
#'
#' Builds a ggplot object of a network that can be modified and combined with
#' other ggplot2 layers. Nodes are points with labels and edges are straight
#' segments, with arrows when the network is directed. Edges with positive
#' weights are green and edges with negative weights are red. The plot uses
#' fixed default aesthetics and does not apply styling set with the
#' \code{sn_*} functions or a theme.
#'
#' @param network A \code{cograph_network} object, matrix, data frame edge
#'   list, igraph, statnet network, qgraph or tna object. A network without
#'   layout coordinates receives a spring layout computed with seed 42.
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' sn_ggplot(regulation_net, title = "Regulation")
sn_ggplot <- function(network, title = NULL) {
  # Auto-convert matrix/data.frame/igraph to cograph_network
  network <- ensure_cograph_network(network)

  nodes <- get_nodes(network)
  edges <- get_edges(network)

  n <- nrow(nodes)
  m <- if (is.null(edges)) 0 else nrow(edges)

  # Theme defaults
  bg_color <- "white"

  # Resolve node aesthetics using defaults
  node_sizes <- recycle_to_length(0.05, n)
  node_fills <- recycle_to_length("#4A90D9", n)
  node_borders <- recycle_to_length("#2C5AA0", n)
  node_border_widths <- recycle_to_length(1, n)
  node_alphas <- recycle_to_length(1, n)
  node_shapes <- recycle_to_length("circle", n)

  # Map shapes to ggplot2 shapes
  shape_map <- c(
    circle = 21, square = 22, triangle = 24, diamond = 23,
    pentagon = 21, hexagon = 21, ellipse = 21, star = 8,
    cross = 3, plus = 3
  )
  gg_shapes <- vapply(node_shapes, function(s) {
    if (s %in% names(shape_map)) shape_map[[s]] else 21 # nocov
  }, numeric(1))

  # Build node data frame
  node_df <- data.frame(
    x = nodes$x,
    y = nodes$y,
    label = if (!is.null(nodes$label)) nodes$label else seq_len(n),
    size = node_sizes * 100,  # Scale for ggplot
    fill = node_fills,
    border = node_borders,
    border_width = node_border_widths,
    alpha = node_alphas,
    shape = gg_shapes,
    stringsAsFactors = FALSE
  )

  # Build edge data frame
  if (m > 0) {
    # Resolve edge aesthetics using defaults
    edge_widths <- recycle_to_length(1, m)

    pos_col <- "#2E7D32"
    neg_col <- "#C62828"
    default_col <- "gray50"
    edge_colors <- if (!is.null(edges$weight)) {
      ifelse(edges$weight > 0, pos_col, ifelse(edges$weight < 0, neg_col, default_col))
    } else {
      rep(default_col, m)
    }

    edge_alphas <- recycle_to_length(0.8, m)

    edge_df <- data.frame(
      x = nodes$x[edges$from],
      y = nodes$y[edges$from],
      xend = nodes$x[edges$to],
      yend = nodes$y[edges$to],
      width = edge_widths,
      color = edge_colors,
      alpha = edge_alphas,
      stringsAsFactors = FALSE
    )
  } else {
    edge_df <- NULL
  }

  # Create base plot
  p <- ggplot2::ggplot()

  # Add edges
  if (!is.null(edge_df) && nrow(edge_df) > 0) {
    show_arrows <- is_directed(network)

    if (show_arrows) {
      p <- p + ggplot2::geom_segment(
        data = edge_df,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
        color = edge_df$color,
        linewidth = edge_df$width,
        alpha = edge_df$alpha,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed")
      )
    } else {
      p <- p + ggplot2::geom_segment(
        data = edge_df,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
        color = edge_df$color,
        linewidth = edge_df$width,
        alpha = edge_df$alpha
      )
    }
  }

  # Add nodes
  p <- p + ggplot2::geom_point(
    data = node_df,
    ggplot2::aes(x = .data$x, y = .data$y),
    fill = node_df$fill,
    color = node_df$border,
    size = node_df$size,
    stroke = node_df$border_width,
    alpha = node_df$alpha,
    shape = node_df$shape
  )

  # Add labels
  show_labels <- TRUE
  if (show_labels) {
    label_size <- 10
    label_color <- "black"

    p <- p + ggplot2::geom_text(
      data = node_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      color = label_color,
      size = label_size / 3  # Convert to ggplot2 sizing
    )
  }

  # Apply theme
  p <- p +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA)
    )

  # Add title
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  p
}
