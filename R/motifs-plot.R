# Motif visualization helpers
# Contains: .plot_motifs_bar, .plot_motifs_heatmap, .plot_motifs_network,
#           .draw_closed_arrow, .grid_arrow, .plot_triad_networks, .plot_motif_patterns

#' @noRd
.plot_motifs_bar <- function(df, colors, directed, size) {
  df$direction <- ifelse(df$z > 2, "over",
                         ifelse(df$z < -2, "under", "neutral"))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = motif, y = z, fill = direction)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_hline(yintercept = c(-2, 2), linetype = "dashed",
                        color = "#666666", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "#333333", linewidth = 0.3) +
    ggplot2::scale_fill_manual(
      values = c(over = colors[3], neutral = colors[2], under = colors[1]),
      labels = c(over = "Over-represented", neutral = "Not significant",
                 under = "Under-represented"),
      name = NULL
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("%d-Node Motif Analysis", size),
      subtitle = if (directed) "Directed network" else "Undirected network",
      x = NULL,
      y = "Z-score (vs null model)"
    ) +
    .motifs_ggplot_theme(12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  print(p)
  invisible(p)
}

#' @noRd
.plot_motifs_heatmap <- function(df, colors) {
  df$label <- sprintf("%d\n(%.1f)", df$count, df$expected)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = 1, y = motif, fill = z)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::scale_fill_gradient2(
      low = colors[1], mid = colors[2], high = colors[3],
      midpoint = 0, limits = c(-max(abs(df$z)), max(abs(df$z))),
      name = "Z-score"
    ) +
    ggplot2::labs(title = "Motif Frequencies", x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  print(p)
  invisible(p)
}

#' @noRd
.plot_motifs_network <- function(df, directed, size, colors) {
  if (!directed || size != 3) {
    message("Network visualization only available for directed 3-node motifs")
    return(.plot_motifs_bar(df, colors, directed, size))
  }

  triad_patterns <- .get_triad_patterns_visual()

  motifs_to_plot <- df$motif[df$motif %in% names(triad_patterns)]

  if (length(motifs_to_plot) == 0) {
    message("No standard triads found in results")
    return(.plot_motifs_bar(df, colors, directed, size))
  }

  n_plots <- length(motifs_to_plot)
  n_cols <- min(4, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(mfrow = c(n_rows, n_cols), mar = c(1, 1, 3, 1))

  for (motif_name in motifs_to_plot) {
    mat <- triad_patterns[[motif_name]]
    z <- df$z[df$motif == motif_name]
    count <- df$count[df$motif == motif_name]

    node_col <- if (z > 2) colors[3] else if (z < -2) colors[1] else "#999999"
    edge_col <- grDevices::adjustcolor(node_col, alpha.f = 0.7)

    g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")
    igraph::V(g)$color <- node_col
    igraph::V(g)$frame.color <- node_col
    igraph::V(g)$size <- 25
    igraph::E(g)$color <- edge_col
    igraph::E(g)$width <- 2
    igraph::E(g)$arrow.size <- 0.5

    coords <- matrix(c(-1, 0, 1, 0.5, 0.5, -0.8), ncol = 2, byrow = TRUE)

    igraph::plot.igraph(g, layout = coords, vertex.label = NA,
                        main = sprintf("%s\nn=%d, z=%.1f", motif_name, count, z))
  }

  invisible(NULL)
}

#' Draw arrow with closed/filled head
#' @noRd
.draw_closed_arrow <- function(x0, y0, x1, y1, col = "#800020", lwd = 2.5,
                                both = FALSE, head_length = 0.12, head_width = 0.08) {
  graphics::segments(x0, y0, x1, y1, col = col, lwd = lwd)

  dx <- x1 - x0
  dy <- y1 - y0
  len <- sqrt(dx^2 + dy^2)
  if (is.na(len) || len == 0) return()

  ux <- dx / len
  uy <- dy / len

  px <- -uy
  py <- ux

  # Arrow head at end (x1, y1)
  tip_x <- x1
  tip_y <- y1
  base_x <- x1 - head_length * ux
  base_y <- y1 - head_length * uy

  arrow_x <- c(tip_x, base_x + head_width * px, base_x - head_width * px)
  arrow_y <- c(tip_y, base_y + head_width * py, base_y - head_width * py)
  graphics::polygon(arrow_x, arrow_y, col = col, border = col)

  # Arrow head at start if mutual
  if (both) {
    tip_x <- x0
    tip_y <- y0
    base_x <- x0 + head_length * ux
    base_y <- y0 + head_length * uy

    arrow_x <- c(tip_x, base_x + head_width * px, base_x - head_width * px)
    arrow_y <- c(tip_y, base_y + head_width * py, base_y - head_width * py)
    graphics::polygon(arrow_x, arrow_y, col = col, border = col)
  }
}

#' Draw arrow head using grid
#' @noRd
.grid_arrow <- function(tip_x, tip_y, base_x, base_y, col) {
  dx <- tip_x - base_x
  dy <- tip_y - base_y
  len <- sqrt(dx^2 + dy^2)
  if (is.na(len) || len == 0) return()

  ux <- dx / len
  uy <- dy / len

  head_len <- 0.04
  head_wid <- 0.025

  ax <- tip_x - head_len * ux
  ay <- tip_y - head_len * uy

  px <- -uy
  py <- ux

  arrow_x <- c(tip_x, ax + head_wid * px, ax - head_wid * px)
  arrow_y <- c(tip_y, ay + head_wid * py, ay - head_wid * py)

  grid::grid.polygon(x = arrow_x, y = arrow_y,
                    gp = grid::gpar(fill = col, col = col))
}

#' Plot individual triads as network diagrams using grid graphics
#' @noRd
.plot_triad_networks <- function(x, n = 12, colors = c("#2166AC", "#B2182B"),
                                  res = 72, node_size = 5, label_size = 7,
                                  title_size = 7, stats_size = 5, ncol = 5,
                                  legend = TRUE, color = "#800020", spacing = 1, ...) {
  df <- utils::head(x$results, n)

  if (nrow(df) == 0) {
    message("No triads to plot")
    return(invisible(NULL))
  }

  n_plots <- nrow(df)
  n_cols <- min(ncol, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  triad_patterns <- .get_triad_patterns_visual()
  motif_color <- color

  grid::grid.newpage()

  legend_height <- grid::unit(2, "lines")

  grid::pushViewport(grid::viewport(
    layout = grid::grid.layout(
      nrow = n_rows + 1,
      ncol = n_cols,
      heights = grid::unit.c(rep(grid::unit(1, "null"), n_rows), legend_height)
    ),
    clip = "on"
  ))

  # Triangle coordinates (0-1 normalized within each cell)
  spread <- 0.32 / spacing
  tri_x <- c(0.5, 0.5 - spread, 0.5 + spread)
  tri_y <- c(0.5 + spread * 0.7, 0.5 - spread * 0.7, 0.5 - spread * 0.7)

  for (i in seq_len(n_plots)) {
    row <- ((i - 1) %/% n_cols) + 1
    col <- ((i - 1) %% n_cols) + 1

    triad_name <- df$triad[i]
    triad_type <- df$type[i]
    count <- df$observed[i]

    nodes <- trimws(strsplit(triad_name, " - ")[[1]])
    if (length(nodes) != 3) nodes <- c("A", "B", "C")
    nodes_short <- vapply(nodes, function(nm) substr(toupper(nm), 1, 3), character(1))

    mat <- triad_patterns[[triad_type]]
    if (is.null(mat)) mat <- matrix(0L, 3, 3)

    grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col, clip = "on"))

    # Title and stats
    if (x$params$significance && "z" %in% names(df)) {
      p_val <- df$p[i]
      p_str <- if (p_val < 0.001) "p<.001" else sprintf("p=%.2f", p_val)
      grid::grid.text(triad_type, x = 0.5, y = 0.94,
                     gp = grid::gpar(fontsize = title_size, fontface = "bold", col = motif_color))
      grid::grid.text(sprintf("n=%d z=%.1f %s", count, df$z[i], p_str),
                     x = 0.5, y = 0.08,
                     gp = grid::gpar(fontsize = stats_size, col = "#64748b"))
    } else {
      grid::grid.text(triad_type, x = 0.5, y = 0.94,
                     gp = grid::gpar(fontsize = title_size, fontface = "bold", col = motif_color))
      grid::grid.text(sprintf("n=%d", count), x = 0.5, y = 0.08,
                     gp = grid::gpar(fontsize = stats_size, col = "#64748b"))
    }

    # Draw edges first
    drawn_mutual <- matrix(FALSE, 3, 3)
    for (from in 1:3) {
      for (to in 1:3) {
        if (from != to && mat[from, to] == 1L) {
          is_mutual <- mat[to, from] == 1L
          if (is_mutual && drawn_mutual[from, to]) next

          x0 <- tri_x[from]; y0 <- tri_y[from]
          x1 <- tri_x[to]; y1 <- tri_y[to]

          dx <- x1 - x0; dy <- y1 - y0
          len <- sqrt(dx^2 + dy^2)
          shrink <- node_size * 0.025 + 0.02

          x0_adj <- x0 + shrink * dx / len
          y0_adj <- y0 + shrink * dy / len
          x1_adj <- x1 - shrink * dx / len
          y1_adj <- y1 - shrink * dy / len

          grid::grid.lines(x = c(x0_adj, x1_adj), y = c(y0_adj, y1_adj),
                          gp = grid::gpar(col = motif_color, lwd = 2))

          .grid_arrow(x1_adj, y1_adj, x0_adj, y0_adj, motif_color)

          if (is_mutual) {
            .grid_arrow(x0_adj, y0_adj, x1_adj, y1_adj, motif_color)
            drawn_mutual[from, to] <- TRUE
            drawn_mutual[to, from] <- TRUE
          }
        }
      }
    }

    # Draw nodes
    node_r <- grid::unit(node_size * 0.025, "npc")
    for (j in 1:3) {
      grid::grid.circle(x = tri_x[j], y = tri_y[j], r = node_r,
                       gp = grid::gpar(fill = "white", col = motif_color, lwd = 2))
      grid::grid.text(nodes_short[j], x = tri_x[j], y = tri_y[j],
                     gp = grid::gpar(fontsize = label_size, fontface = "bold", col = motif_color))
    }

    grid::popViewport()
  }

  # Legend
  if (legend) {
    all_nodes <- unique(unlist(lapply(df$triad, function(tr) {
      trimws(strsplit(tr, " - ")[[1]])
    })))

    if (length(all_nodes) <= 20 && length(all_nodes) > 0) {
      grid::pushViewport(grid::viewport(layout.pos.row = n_rows + 1, layout.pos.col = 1:n_cols))
      abbrev_map <- vapply(all_nodes, function(nm) {
        paste0(substr(toupper(nm), 1, 3), "=", nm)
      }, character(1))
      abbrev_map <- sort(abbrev_map)

      n_items <- length(abbrev_map)
      mid <- ceiling(n_items / 2)
      row1 <- paste(abbrev_map[1:mid], collapse = "  ")
      row2 <- if (mid < n_items) paste(abbrev_map[(mid + 1):n_items], collapse = "  ") else ""

      grid::grid.text(row1, x = 0.5, y = 0.65,
                     gp = grid::gpar(fontsize = 7, col = "#64748b"))
      if (nzchar(row2)) {
        grid::grid.text(row2, x = 0.5, y = 0.35,
                       gp = grid::gpar(fontsize = 7, col = "#64748b"))
      }
      grid::popViewport()
    }
  }

  grid::popViewport()  # layout viewport
  invisible(NULL)
}

#' Plot abstract MAN pattern diagrams
#' @noRd
.plot_motif_patterns <- function(x, n = 12, colors = c("#2166AC", "#B2182B"), ...) {
  type_counts <- x$type_summary
  type_counts <- type_counts[type_counts > 0]
  type_counts <- sort(type_counts, decreasing = TRUE)

  if (length(type_counts) > n) {
    type_counts <- type_counts[seq_len(n)]
  }

  triad_patterns <- .get_triad_patterns_visual()
  type_desc <- .get_man_descriptions()

  motifs_to_plot <- names(type_counts)
  n_plots <- length(motifs_to_plot)

  if (n_plots == 0) {
    message("No motif types to plot")
    return(invisible(NULL))
  }

  n_cols <- min(4, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(mfrow = c(n_rows, n_cols), mar = c(1, 1, 4, 1), bg = "white")

  # Node positions (triangle layout)
  coords <- matrix(c(
    0, 1,
    -0.866, -0.5,
    0.866, -0.5
  ), ncol = 2, byrow = TRUE)

  for (motif_name in motifs_to_plot) {
    count <- type_counts[motif_name]
    mat <- triad_patterns[[motif_name]]
    desc <- type_desc[motif_name]

    graphics::plot(NULL, xlim = c(-1.5, 1.5), ylim = c(-1.2, 1.5),
                   asp = 1, axes = FALSE, xlab = "", ylab = "")

    # Draw edges (arrows)
    edge_col <- "#444444"
    for (i in 1:3) {
      for (j in 1:3) {
        if (i != j && mat[i, j] == 1L) {
          is_mutual <- mat[j, i] == 1L

          x0 <- coords[i, 1]
          y0 <- coords[i, 2]
          x1 <- coords[j, 1]
          y1 <- coords[j, 2]

          dx <- x1 - x0
          dy <- y1 - y0
          len <- sqrt(dx^2 + dy^2)
          shrink <- 0.25 / len

          x0_adj <- x0 + dx * shrink
          y0_adj <- y0 + dy * shrink
          x1_adj <- x1 - dx * shrink
          y1_adj <- y1 - dy * shrink

          if (is_mutual && i < j) {
            offset <- 0.08
            perp_x <- -dy / len * offset
            perp_y <- dx / len * offset
            graphics::arrows(x0_adj + perp_x, y0_adj + perp_y,
                           x1_adj + perp_x, y1_adj + perp_y,
                           length = 0.12, lwd = 2.5, col = edge_col)
          } else if (is_mutual && i > j) {
            offset <- 0.08
            perp_x <- -dy / len * offset
            perp_y <- dx / len * offset
            graphics::arrows(x0_adj - perp_x, y0_adj - perp_y,
                           x1_adj - perp_x, y1_adj - perp_y,
                           length = 0.12, lwd = 2.5, col = edge_col)
          } else {
            graphics::arrows(x0_adj, y0_adj, x1_adj, y1_adj,
                           length = 0.12, lwd = 2.5, col = edge_col)
          }
        }
      }
    }

    # Draw nodes
    node_col <- colors[1]
    graphics::points(coords[, 1], coords[, 2], pch = 21, cex = 4,
                    bg = node_col, col = "white", lwd = 2)

    # Node labels
    graphics::text(coords[, 1], coords[, 2], c("A", "B", "C"),
                  col = "white", font = 2, cex = 1.1)

    # Title with count
    graphics::title(main = sprintf("%s: %s\nn = %s",
                                   motif_name, desc,
                                   format(count, big.mark = ",")),
                   cex.main = 1.1, line = 1)
  }

  invisible(NULL)
}
