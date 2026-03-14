# Shared helpers for plot_simplicial() and overlay_communities()

# =========================================================================
# State extraction
# =========================================================================

#' Extract state names from a network object
#' @noRd
.extract_blob_states <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "tna")) return(x$labels)
  if (inherits(x, "igraph")) {
    if (requireNamespace("igraph", quietly = TRUE)) {
      return(igraph::V(x)$name %||% paste0("S", seq_len(igraph::vcount(x))))
    }
  }
  if (inherits(x, "cograph_network")) return(x$nodes$label)
  if (is.matrix(x)) {
    states <- rownames(x)
    return(if (is.null(states)) paste0("S", seq_len(nrow(x))) else states)
  }
  stop("x must be a tna object, matrix, igraph, or cograph_network.")
}

# =========================================================================
# Layout
# =========================================================================

#' Compute circle or custom layout for blob plots
#' @noRd
.blob_layout <- function(states, labels, layout, n) {
  if (is.character(layout) && layout == "circle") {
    angles <- seq(pi / 2, pi / 2 - 2 * pi, length.out = n + 1)[seq_len(n)]
    R <- 5.5
    data.frame(
      x = R * cos(angles),
      y = R * sin(angles),
      label = labels,
      state = states,
      stringsAsFactors = FALSE
    )
  } else if (is.matrix(layout) || is.data.frame(layout)) {
    layout <- as.data.frame(layout)
    stopifnot(nrow(layout) == n, ncol(layout) >= 2)
    data.frame(
      x = as.numeric(layout[, 1]),
      y = as.numeric(layout[, 2]),
      label = labels,
      state = states,
      stringsAsFactors = FALSE
    )
  } else {
    stop("layout must be 'circle' or a matrix of coordinates.")
  }
}

# =========================================================================
# Geometry
# =========================================================================

#' Smooth blob polygon via padded convex hull + Laplacian smoothing
#' @noRd
.smooth_blob <- function(px, py, pad = 1.0, n_circle = 60L,
                         n_upsample = 800L, n_smooth_iter = 80L) {
  all_x <- all_y <- numeric(0)
  for (i in seq_along(px)) {
    a <- seq(0, 2 * pi, length.out = n_circle + 1L)[-(n_circle + 1L)]
    all_x <- c(all_x, px[i] + pad * cos(a))
    all_y <- c(all_y, py[i] + pad * sin(a))
  }
  hi <- grDevices::chull(all_x, all_y)
  hx <- all_x[hi]; hy <- all_y[hi]
  n_hull <- length(hx)
  hx <- c(hx, hx[1]); hy <- c(hy, hy[1])
  ux <- uy <- numeric(0)
  for (i in seq_len(n_hull)) {
    seg_n <- max(2L, round(n_upsample / n_hull))
    t_seq <- seq(0, 1, length.out = seg_n + 1L)[-(seg_n + 1L)]
    ux <- c(ux, hx[i] + t_seq * (hx[i + 1] - hx[i]))
    uy <- c(uy, hy[i] + t_seq * (hy[i + 1] - hy[i]))
  }
  n_pts <- length(ux)
  for (iter in seq_len(n_smooth_iter)) {
    nx <- ny <- numeric(n_pts)
    for (j in seq_len(n_pts)) {
      jp <- if (j == 1L) n_pts else j - 1L
      jn <- if (j == n_pts) 1L else j + 1L
      nx[j] <- (ux[jp] + ux[j] + ux[jn]) / 3
      ny[j] <- (uy[jp] + uy[j] + uy[jn]) / 3
    }
    ux <- nx; uy <- ny
  }
  data.frame(x = c(ux, ux[1]), y = c(uy, uy[1]))
}

#' Darken hex colors by a fraction
#' @noRd
.darken_colors <- function(cols, amount = 0.2) {
  vapply(cols, function(col) {
    rgb <- grDevices::col2rgb(col)[, 1] / 255
    darkened <- pmax(rgb * (1 - amount), 0)
    grDevices::rgb(darkened[1], darkened[2], darkened[3])
  }, character(1), USE.NAMES = FALSE)
}

# =========================================================================
# Default palettes
# =========================================================================

#' Default blob fill colors
#' @noRd
.blob_default_colors <- function() {
  c("#B0D4F1", "#A8D8A8", "#F0C8A0", "#D4B0F0",
    "#F0DFA0", "#C8E8E0", "#F0D4B0", "#E0C8E8",
    "#D4F0B0", "#F0B0B0")
}

#' Default blob linetype cycle
#' @noRd
.blob_default_linetypes <- function() {
  c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
}

# =========================================================================
# ggplot layer helpers
# =========================================================================

#' Add shadow layers to a ggplot
#' @noRd
.add_shadow <- function(p, blob, n_layers = 3L, offset = 0.04,
                         alpha = 0.008) {
  for (s in seq(n_layers, 1L, by = -1L)) {
    shadow_df <- blob
    shadow_df$x <- shadow_df$x + s * offset
    shadow_df$y <- shadow_df$y - s * offset
    p <- p + ggplot2::geom_polygon(
      data = shadow_df, ggplot2::aes(x = x, y = y),
      fill = "black", color = NA, alpha = alpha
    )
  }
  p
}

#' Base ggplot with void theme for blob plots
#' @noRd
.blob_base_plot <- function(xlim = c(-9, 9), ylim = c(-8.5, 8.5)) {
  ggplot2::ggplot() +
    ggplot2::coord_equal(clip = "off", xlim = xlim, ylim = ylim) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.title = ggplot2::element_text(
        hjust = 0.5, size = 18, face = "bold", color = "#2c3e50"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5, size = 11, color = "#7f8c8d",
        margin = ggplot2::margin(b = 15)
      )
    )
}

#' Add source/target colored nodes to a ggplot
#' @noRd
.add_pathway_nodes <- function(p, ndf, is_target, node_color, target_color,
                                ring_color, ring_border, node_size,
                                label_size) {
  ring_size <- node_size * 1.27
  p <- p + ggplot2::geom_point(
    data = ndf, ggplot2::aes(x = x, y = y),
    fill = ring_color, color = ring_border,
    size = ring_size, shape = 21, stroke = 1
  )

  src_df <- ndf[!is_target, , drop = FALSE]
  if (nrow(src_df) > 0L) {
    p <- p + ggplot2::geom_point(
      data = src_df, ggplot2::aes(x = x, y = y),
      fill = node_color, color = node_color,
      size = node_size, shape = 21, stroke = 0.5
    )
    p <- p + ggplot2::geom_text(
      data = src_df, ggplot2::aes(x = x, y = y, label = label),
      color = "white", fontface = "bold", size = label_size
    )
  }

  tgt_df <- ndf[is_target, , drop = FALSE]
  if (nrow(tgt_df) > 0L) {
    p <- p + ggplot2::geom_point(
      data = tgt_df, ggplot2::aes(x = x, y = y),
      fill = target_color, color = target_color,
      size = node_size, shape = 21, stroke = 0.5
    )
    p <- p + ggplot2::geom_text(
      data = tgt_df, ggplot2::aes(x = x, y = y, label = label),
      color = "white", fontface = "bold", size = label_size
    )
  }
  p
}
