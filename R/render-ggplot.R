#' @title ggplot2 Conversion
#' @description Convert Cograph network to ggplot2 object.
#' @name render-ggplot
NULL

#' Convert Network to ggplot2
#'
#' Convert a Cograph network visualization to a ggplot2 object for further
#' customization and composability.
#'
#' @param network A cograph_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param title Optional plot title.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' # With cograph()
#' p <- cograph(adj) |> sn_ggplot()
#' print(p)
#'
#' # Direct matrix input
#' p <- adj |> sn_ggplot()
#'
#' # Further customization
#' p + ggplot2::labs(title = "My Network")
sn_ggplot <- function(network, title = NULL) {
  # Auto-convert matrix/data.frame/igraph to cograph_network
  network <- ensure_cograph_network(network)

  nodes <- get_nodes(network)
  edges <- get_edges(network)
  node_aes <- network$node_aes
  edge_aes <- network$edge_aes
  theme <- network$theme

  n <- nrow(nodes)
  m <- if (is.null(edges)) 0 else nrow(edges)

  # Theme defaults
  bg_color <- if (!is.null(theme)) theme$get("background") else "white"

  # Resolve node aesthetics
  node_sizes <- recycle_to_length(
    if (!is.null(node_aes$size)) node_aes$size else 0.05,
    n
  )
  node_fills <- recycle_to_length(
    if (!is.null(node_aes$fill)) node_aes$fill else
      if (!is.null(theme)) theme$get("node_fill") else "#4A90D9",
    n
  )
  node_borders <- recycle_to_length(
    if (!is.null(node_aes$border_color)) node_aes$border_color else
      if (!is.null(theme)) theme$get("node_border") else "#2C5AA0",
    n
  )
  node_border_widths <- recycle_to_length(
    if (!is.null(node_aes$border_width)) node_aes$border_width else
      if (!is.null(theme)) theme$get("node_border_width") else 1,
    n
  )
  node_alphas <- recycle_to_length(
    if (!is.null(node_aes$alpha)) node_aes$alpha else 1,
    n
  )
  node_shapes <- recycle_to_length(
    if (!is.null(node_aes$shape)) node_aes$shape else "circle",
    n
  )

  # Map shapes to ggplot2 shapes
  shape_map <- c(
    circle = 21, square = 22, triangle = 24, diamond = 23,
    pentagon = 21, hexagon = 21, ellipse = 21, star = 8,
    cross = 3, plus = 3
  )
  gg_shapes <- sapply(node_shapes, function(s) {
    if (s %in% names(shape_map)) shape_map[[s]] else 21
  })

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
    # Resolve edge aesthetics
    edge_widths <- recycle_to_length(
      if (!is.null(edge_aes$width)) edge_aes$width else
        if (!is.null(theme)) theme$get("edge_width") else 1,
      m
    )

    if (!is.null(edge_aes$color)) {
      edge_colors <- recycle_to_length(edge_aes$color, m)
    } else {
      pos_col <- if (!is.null(edge_aes$positive_color)) edge_aes$positive_color else
        if (!is.null(theme)) theme$get("edge_positive_color") else "#2E7D32"
      neg_col <- if (!is.null(edge_aes$negative_color)) edge_aes$negative_color else
        if (!is.null(theme)) theme$get("edge_negative_color") else "#C62828"
      default_col <- if (!is.null(theme)) theme$get("edge_color") else "gray50"
      edge_colors <- if (!is.null(edges$weight)) {
        ifelse(edges$weight > 0, pos_col, ifelse(edges$weight < 0, neg_col, default_col))
      } else {
        rep(default_col, m)
      }
    }

    edge_alphas <- recycle_to_length(
      if (!is.null(edge_aes$alpha)) edge_aes$alpha else 0.8,
      m
    )

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
    show_arrows <- if (!is.null(edge_aes$show_arrows)) edge_aes$show_arrows else is_directed(network)

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
  show_labels <- if (!is.null(node_aes$show_labels)) node_aes$show_labels else TRUE
  if (show_labels) {
    label_size <- if (!is.null(node_aes$label_size)) node_aes$label_size[1] else
      if (!is.null(theme)) theme$get("label_size") else 10
    label_color <- if (!is.null(node_aes$label_color)) node_aes$label_color[1] else
      if (!is.null(theme)) theme$get("label_color") else "black"

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
