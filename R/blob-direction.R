# =========================================================================
# Direction cues for per-pathway simplicial panels
# =========================================================================
#
# A simplex is a SET of vertices, so a blob alone cannot say which state
# came first. `plot_simplicial(dismantled = TRUE)` draws one panel per
# pathway and the pathway IS ordered (`pw$source` is the ordered prefix,
# `pw$target` the last state), so the traversal can be drawn.
#
# Three cues, ported from ladyna's `plotPathwayPanels()`
# (tna-js/src/analysis/simplicialPlot.ts) which is itself the engine behind
# the CarmNote notebooks:
#
#   1. a light-to-dark core ramp along the path (`.step_shade()`),
#   2. a ring whose gold peaks on the side facing the next state, and
#   3. an arrowhead just outside each node, aimed at its successor.
#
# All three are drawn in DATA units, not device millimeters. `geom_point()`
# sizes are in mm, so a point's radius in data units depends on the panel
# extent and the device — which makes "just outside the ring" unanswerable
# and a gradient ring unexpressible. Drawing ring and core as polygons fixes
# both, and matches the engine, where node and blob scale together per panel.

#' Light-to-dark step ramp along a pathway
#'
#' Port of ladyna's `stepShade()`. \code{t} is the node's position in the
#' pathway, 0 for the first state and 1 for the last: the first sits 62%
#' of the way to white, the last 15% below the base color. Vectorized
#' over \code{t}; \code{hex} is recycled.
#' @noRd
.step_shade <- function(hex, t) {
  stopifnot(
    "`t` must be numeric" = is.numeric(t),
    "`hex` must be a colour" = is.character(hex) && length(hex) >= 1L
  )
  if (length(t) == 0L) return(character(0))
  t <- pmin(1, pmax(0, ifelse(is.finite(t), t, 1)))
  rgbm <- grDevices::col2rgb(rep_len(hex, length(t)))
  to_white <- matrix(0.62 * (1 - t), nrow = 3L, ncol = length(t), byrow = TRUE)
  keep <- matrix(1 - 0.15 * t, nrow = 3L, ncol = length(t), byrow = TRUE)
  out <- (rgbm + (255 - rgbm) * to_white) * keep
  # pmin()/pmax() take attributes from their FIRST argument: with the
  # scalar first the matrix would come back as a bare vector.
  out <- pmin(pmax(round(out), 0), 255)
  grDevices::rgb(out[1L, ], out[2L, ], out[3L, ], maxColorValue = 255)
}

#' Linear blend between two colors
#'
#' \code{w} is the weight on \code{to}: 0 returns \code{from}, 1 returns
#' \code{to}. Vectorized over \code{w}.
#' @noRd
.blend_colors <- function(from, to, w) {
  stopifnot("`w` must be numeric" = is.numeric(w))
  if (length(w) == 0L) return(character(0))
  w <- pmin(1, pmax(0, w))
  a <- grDevices::col2rgb(from)[, 1L]
  b <- grDevices::col2rgb(to)[, 1L]
  v <- outer(a, 1 - w) + outer(b, w)
  v <- pmin(pmax(round(v), 0), 255)
  grDevices::rgb(v[1L, ], v[2L, ], v[3L, ], maxColorValue = 255)
}

#' Filled disc as a polygon, in data units
#' @noRd
.disc_polygon <- function(x, y, r, n = 64L) {
  a <- seq(0, 2 * pi, length.out = n + 1L)[seq_len(n)]
  data.frame(x = x + r * cos(a), y = y + r * sin(a))
}

#' Ring annulus whose gold peaks on the side facing the next state
#'
#' The engine paints the ring with an SVG `linearGradient` running from
#' \code{(x - dx, y - dy)} (palest) to \code{(x + dx, y + dy)} (full
#' strength), where the axis points at the successor. A linear gradient
#' assigns each point a weight by its projection onto that axis, so sector
#' \code{theta} gets \code{w = (1 + cos(theta - angle)) / 2}. Discretizing
#' the annulus into \code{n_sector} wedges reproduces it; the chord error
#' at 48 wedges is under 0.1% of the radius.
#'
#' A terminal state has no successor: \code{angle} is \code{NA} and the
#' ring is painted flat, exactly as the engine does.
#' @noRd
.ring_sector_polys <- function(x, y, r_in, r_out, angle, ring_color,
                               n_sector = 48L) {
  stopifnot(
    "`r_out` must exceed `r_in`" = r_out > r_in,
    "`n_sector` must be at least 8" = n_sector >= 8L
  )
  edges <- seq(0, 2 * pi, length.out = n_sector + 1L)
  lo <- edges[seq_len(n_sector)]
  hi <- edges[-1L]
  mid <- (lo + hi) / 2
  w <- if (is.finite(angle)) (1 + cos(mid - angle)) / 2 else rep(1, n_sector)
  fills <- .blend_colors(.step_shade(ring_color, 0), ring_color, w)
  parts <- lapply(seq_len(n_sector), function(k) {
    a <- seq(lo[k], hi[k], length.out = 4L)
    data.frame(
      x = c(x + r_out * cos(a), x + r_in * cos(rev(a))),
      y = c(y + r_out * sin(a), y + r_in * sin(rev(a))),
      fill = fills[k],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, parts)
  out$sector <- rep(seq_len(n_sector), each = 8L)
  out
}

#' Arrowheads aimed at each node's successor
#'
#' One detached triangle per consecutive pair, sitting just outside the
#' source node's ring so it never crosses the label. Returns \code{NULL}
#' for a pathway with fewer than two states.
#' @noRd
.direction_arrow_polys <- function(px, py, r_out, size) {
  stopifnot("`px` and `py` must be the same length" = length(px) == length(py))
  n <- length(px)
  if (n < 2L) return(NULL)
  i <- seq_len(n - 1L)
  dx <- px[i + 1L] - px[i]
  dy <- py[i + 1L] - py[i]
  len <- sqrt(dx^2 + dy^2)
  keep <- is.finite(len) & len > 0
  if (!any(keep)) return(NULL)
  i <- i[keep]; dx <- dx[keep]; dy <- dy[keep]; len <- len[keep]
  off <- r_out + size * 0.95
  ax <- px[i] + dx / len * off
  ay <- py[i] + dy / len * off
  ang <- atan2(dy, dx)
  tri_x <- c(-size, size * 1.15, -size)
  tri_y <- c(-size, 0, size)
  parts <- lapply(seq_along(i), function(k) {
    ca <- cos(ang[k]); sa <- sin(ang[k])
    data.frame(
      x = ax[k] + tri_x * ca - tri_y * sa,
      y = ay[k] + tri_x * sa + tri_y * ca,
      step = k
    )
  })
  do.call(rbind, parts)
}

#' Draw one pathway's nodes with the traversal made visible
#'
#' Replaces `.add_pathway_nodes()` on the directed path. Order matters:
#' every ring and core is drawn before any label, because a ring painted
#' for a later node would otherwise slice through an earlier node's
#' overhanging label — the same reason the engine splits its node drawing
#' into a shell pass and a label pass.
#' @noRd
.add_directed_pathway_nodes <- function(p, ndf, node_color, target_color,
                                        ring_color, ring_border,
                                        node_radius, ring_radius,
                                        label_size,
                                        label_color = "#e8e8e8",
                                        target_label_color = NULL,
                                        label_halo = TRUE,
                                        label_halo_color = NULL,
                                        label_halo_width = 0.035,
                                        label_halo_alpha = 0.6,
                                        arrows = TRUE,
                                        step_shade = TRUE,
                                        ring_gradient = TRUE) {
  n <- nrow(ndf)
  stopifnot("`ndf` must have at least one node" = n >= 1L)
  last <- n
  # Position along the path, 0 for the first state and 1 for the last.
  tpos <- if (n > 1L) (seq_len(n) - 1L) / (n - 1L) else 1
  base <- ifelse(seq_len(n) == last, target_color, node_color)
  cores <- if (isTRUE(step_shade)) .step_shade(base, tpos) else base

  # --- rings: gradient wedges, then the border circle on top ---
  ring_layers <- unlist(lapply(seq_len(n), function(i) {
    ang <- if (isTRUE(ring_gradient) && i < n) {
      atan2(ndf$y[i + 1L] - ndf$y[i], ndf$x[i + 1L] - ndf$x[i])
    } else {
      NA_real_
    }
    list(
      ggplot2::geom_polygon(
        data = .ring_sector_polys(ndf$x[i], ndf$y[i], node_radius,
                                  ring_radius, ang, ring_color),
        # Each wedge strokes itself in its own fill: abutting antialiased
        # polygons otherwise leave a hairline seam on every sector boundary,
        # which reads as radial spokes across the ring.
        ggplot2::aes(x = x, y = y, group = sector, fill = fill,
                     colour = fill),
        linewidth = 0.3
      ),
      ggplot2::geom_polygon(
        data = .disc_polygon(ndf$x[i], ndf$y[i], ring_radius, 96L),
        ggplot2::aes(x = x, y = y),
        fill = NA, color = ring_border, linewidth = 0.35
      )
    )
  }), recursive = FALSE)
  p <- Reduce(`+`, ring_layers, init = p) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity()

  # --- cores ---
  core_layers <- lapply(seq_len(n), function(i) {
    ggplot2::geom_polygon(
      data = .disc_polygon(ndf$x[i], ndf$y[i], node_radius),
      ggplot2::aes(x = x, y = y),
      fill = cores[i], color = cores[i], linewidth = 0.2
    )
  })
  p <- Reduce(`+`, core_layers, init = p)

  # --- labels ---
  src_text_color <- label_color
  tgt_text_color <- target_label_color %||% label_color
  src_halo <- label_halo_color %||% .contrasting_text_color(src_text_color)
  tgt_halo <- label_halo_color %||% .contrasting_text_color(tgt_text_color)
  is_target <- seq_len(n) == last
  src_df <- ndf[!is_target, , drop = FALSE]
  if (nrow(src_df) > 0L) {
    p <- .add_text_with_halo(p, src_df, src_text_color, src_halo,
                             label_size, label_halo, label_halo_width,
                             label_halo_alpha)
  }
  tgt_df <- ndf[is_target, , drop = FALSE]
  if (nrow(tgt_df) > 0L) {
    p <- .add_text_with_halo(p, tgt_df, tgt_text_color, tgt_halo,
                             label_size, label_halo, label_halo_width,
                             label_halo_alpha)
  }

  # --- arrows, last, so they sit above the rings and the labels ---
  if (isTRUE(arrows)) {
    # 0.26 is the engine's own proportion: arrow 6px against a 23px ring.
    tri <- .direction_arrow_polys(ndf$x, ndf$y, ring_radius,
                                  size = ring_radius * 0.26)
    if (!is.null(tri)) {
      p <- p + ggplot2::geom_polygon(
        data = tri, ggplot2::aes(x = x, y = y, group = step),
        fill = "#374151", color = "#ffffff", linewidth = 0.3
      )
    }
  }
  p
}

#' In-figure legend strip for a dismantled simplicial grid
#'
#' The engine draws its legend INTO the figure rather than beside it, so
#' the explanation survives export. Same idea here: a grob returned for
#' \code{gridExtra::arrangeGrob(bottom = )}, so it sits under the grid and
#' travels with whatever the caller saves.
#'
#' Item widths are estimated from \code{nchar()} and laid out left to
#' right, which is what the engine's `simDrawLegend()` does — a text grob
#' cannot report its own width before the device knows the font.
#' @noRd
.simplicial_legend_grob <- function(node_color, target_color,
                                    ring_color, ring_border,
                                    direction = TRUE, ordered = TRUE) {
  items <- if (!isTRUE(ordered)) {
    # A set has no target and no traversal, so the legend must not name
    # either. It still has to say that the blob fill means nothing.
    list(
      list(fill = node_color, text = "every node is a member of the set"),
      list(fill = NA, text = "no target: the members are co-equal"),
      list(fill = NA, text = "blob fill = palette, no meaning")
    )
  } else if (isTRUE(direction)) {
    list(
      list(fill = target_color, text = "orange core = target (last step)"),
      list(fill = .step_shade(node_color, 0),
           text = "blue core = earlier steps (light to dark = first to last)"),
      list(fill = NA, text = "arrowheads point to the next state"),
      list(fill = NA, text = "repeated label = one slot per visit")
    )
  } else {
    list(
      list(fill = target_color, text = "orange core = target (last step)"),
      list(fill = node_color, text = "blue core = source step"),
      list(fill = NA, text = "repeated label = one slot per visit")
    )
  }

  # Lay out left to right in a 0-100 strip, budgeting width by character
  # count plus a swatch allowance for the items that carry one.
  widths <- vapply(items, function(it) {
    nchar(it$text) + if (is.na(it$fill)) 1 else 4
  }, numeric(1))
  total <- sum(widths) + length(items)
  starts <- c(0, cumsum(widths + 1)[-length(widths)]) / total * 100

  swatch <- do.call(rbind, lapply(seq_along(items), function(k) {
    if (is.na(items[[k]]$fill)) return(NULL)
    data.frame(x = starts[k] + 0.9, y = 0.5, fill = items[[k]]$fill,
               stringsAsFactors = FALSE)
  }))
  labs_df <- data.frame(
    x = starts + vapply(items, function(it) if (is.na(it$fill)) 0 else 2.1,
                        numeric(1)),
    y = 0.5,
    label_text = vapply(items, `[[`, character(1), "text"),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 100), ylim = c(0, 1),
                             expand = FALSE, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 6, 2, 6))
  if (!is.null(swatch)) {
    p <- p +
      ggplot2::geom_point(data = swatch, ggplot2::aes(x = x, y = y),
                          fill = ring_color, color = ring_border,
                          shape = 21, size = 4.2, stroke = 0.6) +
      # shape 19 takes `color`, so the core needs no fill/color split;
      # `color = NA` would count as a missing aesthetic and drop the row.
      ggplot2::geom_point(data = swatch,
                          ggplot2::aes(x = x, y = y, colour = fill),
                          shape = 19, size = 2.6) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_colour_identity()
  }
  p <- p + ggplot2::geom_text(
    data = labs_df, ggplot2::aes(x = x, y = y, label = label_text),
    hjust = 0, size = 2.5, color = "#555555"
  )
  ggplot2::ggplotGrob(p)
}
