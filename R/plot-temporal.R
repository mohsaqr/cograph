#' Temporal Network Prism (3D Glass Box)
#'
#' Displays a network at different time points as vertical planes inside a
#' 3D oblique-projection box, with time flowing left to right. Each network
#' plane extends into the depth of the box.
#'
#' @param x An edge list data frame with columns \code{from}, \code{to}, and
#'   a time column, OR a \code{cograph_network} (reads time from stored data),
#'   OR a named list of network objects.
#' @param time Character. Name of the time column.
#' @param slices Integer or NULL. Number of equal-width time bins. Default
#'   NULL uses unique time values.
#' @param cumulative Logical. If TRUE, edges accumulate. Default FALSE.
#' @param labels Character vector of layer labels. Default auto.
#' @param layout Character or matrix. Character values currently use a shared
#'   Fruchterman-Reingold/spring layout; a matrix supplies shared coordinates.
#'   Default \code{"spring"}.
#' @param node_size Numeric. Node size. Default 2.5.
#' @param node_color Character or vector. Node fill color. A single color
#'   applies to all layers, or a vector of length \code{n_layers} for
#'   per-layer colors. Default \code{"steelblue"}.
#' @param node_shape Integer. Point shape (\code{pch}). Default 21 (filled
#'   circle).
#' @param node_border Character. Node border color. Default \code{"gray30"}.
#' @param edge_color Character or vector. Edge color (single or per-layer).
#'   Default \code{"#E41A1C"}.
#' @param edge_width Numeric. Base edge width. Actual width scales by
#'   weight. Default 1.5.
#' @param edge_alpha Numeric. Edge transparency (0-1). Default 0.35.
#' @param plane_color Character or vector. Plane fill color (single or
#'   per-layer). Default \code{"gray92"}.
#' @param plane_alpha Numeric. Plane fill transparency (0-1). Default 0.2.
#' @param plane_border Character. Plane border color. Default
#'   \code{"gray60"}.
#' @param plane_lty Integer. Plane border line type. Default 2 (dashed).
#' @param box Logical. Draw 3D bounding box. Default TRUE.
#' @param box_color Character. Box edge color. Default \code{"gray40"}.
#' @param connections Logical. Draw lines connecting same nodes across
#'   planes. Default FALSE.
#' @param connection_color Character. Default \code{"gray50"}.
#' @param connection_alpha Numeric. Default 0.15.
#' @param minimum Numeric. Minimum edge weight to display. Default 0.
#' @param show_labels Logical. Default FALSE.
#' @param label_size Numeric. Label text size. Default 0.4.
#' @param title Character or NULL. Plot title. Default NULL.
#' @param angle Numeric vector of length 2: \code{c(dz_x, dz_y)} controlling
#'   the oblique projection shear. Default \code{c(1.0, 0.7)}.
#' @param seed Integer or NULL. Default 42.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisible list of adjacency matrices per layer.
#' @seealso \code{\link{plot_network_evolution}}, \code{\link{plot_mlna}}
#' @export
#' @examples
#' set.seed(1)
#' edges <- data.frame(
#'   from = sample(LETTERS[1:5], 30, replace = TRUE),
#'   to   = sample(LETTERS[1:5], 30, replace = TRUE),
#'   week = sample(1:3, 30, replace = TRUE))
#' cograph::plot_temporal(edges, time = "week")
plot_temporal <- function(x,
                          time = NULL,
                          slices = NULL,
                          cumulative = FALSE,
                          labels = NULL,
                          layout = "spring",
                          node_size = 2.5,
                          node_color = "steelblue",
                          node_shape = 21,
                          node_border = "gray30",
                          edge_color = "#E41A1C",
                          edge_width = 1.5,
                          edge_alpha = 0.35,
                          plane_color = "gray92",
                          plane_alpha = 0.2,
                          plane_border = "gray60",
                          plane_lty = 2,
                          box = TRUE,
                          box_color = "gray40",
                          connections = FALSE,
                          connection_color = "gray50",
                          connection_alpha = 0.15,
                          minimum = 0,
                          show_labels = FALSE,
                          label_size = 0.4,
                          title = NULL,
                          angle = c(1.0, 0.7),
                          seed = 42,
                          ...) {

  # --- Resolve input ---
  resolve <- .resolve_temporal_input(x, time, slices, cumulative, labels)
  mats <- resolve$mats
  labels <- resolve$labels
  all_nodes <- resolve$all_nodes
  nn <- length(all_nodes)
  n_layers <- length(mats)

  # Recycle per-layer colors
  node_color <- rep_len(node_color, n_layers)
  edge_color <- rep_len(edge_color, n_layers)
  plane_color <- rep_len(plane_color, n_layers)

  # --- Shared layout ---
  full_mat <- Reduce("+", mats)
  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }
  if (is.character(layout)) {
    g_full <- to_igraph(full_mat)
    shared_xy <- igraph::layout_with_fr(g_full)
  } else {
    shared_xy <- layout
  }
  .n01 <- function(v) {
    r <- range(v)
    if (diff(r) == 0) rep(0.5, length(v)) else (v - r[1]) / diff(r)
  }
  shared_xy[, 1] <- .n01(shared_xy[, 1])
  shared_xy[, 2] <- .n01(shared_xy[, 2])

  # --- Oblique cabinet projection ---
  box_x <- 12; box_y <- 6; box_z <- 3.5
  dz_x <- angle[1]; dz_y <- angle[2]

  .proj <- function(X, Y, Z) cbind(X + Z * dz_x, Y + Z * dz_y)

  max_w <- max(vapply(mats, function(m) max(abs(m), na.rm = TRUE), numeric(1)))
  if (max_w == 0) max_w <- 1

  # --- Plot setup ---
  box_corners <- .proj(
    c(0, box_x, box_x, 0, 0, box_x, box_x, 0),
    c(0, 0, box_y, box_y, 0, 0, box_y, box_y),
    c(0, 0, 0, 0, box_z, box_z, box_z, box_z)
  )
  xr <- range(box_corners[, 1]) + c(-0.5, 0.5)
  yr <- range(box_corners[, 2]) + c(-1.5, if (is.null(title)) 0.5 else 1.5)

  old_par <- graphics::par(mar = c(2, 0.5, if (is.null(title)) 0.5 else 2, 0.5),
                           bg = "white")
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = xr, ylim = yr)

  if (!is.null(title)) {
    graphics::title(main = title, col.main = "gray20", font.main = 2)
  }

  # --- Draw box ---
  if (box) {
    .seg <- function(x0, y0, z0, x1, y1, z1, ...) {
      p0 <- .proj(x0, y0, z0); p1 <- .proj(x1, y1, z1)
      graphics::segments(p0[1], p0[2], p1[1], p1[2], ...)
    }
    .poly <- function(xs, ys, zs, ...) {
      pts <- .proj(xs, ys, zs)
      graphics::polygon(pts[, 1], pts[, 2], ...)
    }

    bc <- grDevices::adjustcolor(box_color, 0.4)

    # Back face
    .poly(c(0, box_x, box_x, 0), c(0, 0, box_y, box_y),
          rep(box_z, 4), col = NA, border = bc, lwd = 0.6, lty = 3)

    # Depth edges
    .seg(0, 0, 0, 0, 0, box_z, col = bc, lwd = 0.6, lty = 3)
    .seg(box_x, 0, 0, box_x, 0, box_z, col = bc, lwd = 0.6, lty = 3)
    .seg(box_x, box_y, 0, box_x, box_y, box_z, col = bc, lwd = 0.6, lty = 3)
    .seg(0, box_y, 0, 0, box_y, box_z, col = bc, lwd = 0.6, lty = 3)

    # Front face
    .seg(0, 0, 0, box_x, 0, 0, col = box_color, lwd = 1)
    .seg(box_x, 0, 0, box_x, box_y, 0, col = box_color, lwd = 1)
    .seg(box_x, box_y, 0, 0, box_y, 0, col = box_color, lwd = 1)
    .seg(0, box_y, 0, 0, 0, 0, col = box_color, lwd = 1)
  }

  # --- Network planes ---
  plane_xs <- seq(box_x * 0.08, box_x * 0.92, length.out = n_layers)
  margin <- 0.35
  node_positions <- vector("list", n_layers)

  for (li in seq_len(n_layers)) {
    px <- plane_xs[li]
    mat <- mats[[li]]

    # Plane
    plane_pts <- .proj(rep(px, 4), c(0, 0, box_y, box_y), c(0, box_z, box_z, 0))
    graphics::polygon(plane_pts[, 1], plane_pts[, 2],
                      col = grDevices::adjustcolor(plane_color[li], plane_alpha),
                      border = NA)
    graphics::lines(c(plane_pts[, 1], plane_pts[1, 1]),
                    c(plane_pts[, 2], plane_pts[1, 2]),
                    col = grDevices::adjustcolor(plane_border, 0.4),
                    lwd = 0.7, lty = plane_lty)

    # Node positions
    node_y <- margin + shared_xy[, 2] * (box_y - 2 * margin)
    node_z <- margin + shared_xy[, 1] * (box_z - 2 * margin)
    node_screen <- .proj(rep(px, nn), node_y, node_z)
    node_positions[[li]] <- node_screen

    # Edges
    e_col <- grDevices::adjustcolor(edge_color[li], edge_alpha)
    for (i in seq_len(nn)) {
      for (j in seq_len(nn)) {
        w <- mat[i, j]
        if (!is.na(w) && w > minimum) {
          lwd <- 0.3 + edge_width * abs(w) / max_w
          if (i == j) {
            draw_self_loop_base(
              x = node_screen[i, 1], y = node_screen[i, 2],
              node_size = node_size * 0.02,
              col = e_col, lwd = lwd, arrow = FALSE
            )
          } else {
            graphics::segments(
              node_screen[i, 1], node_screen[i, 2],
              node_screen[j, 1], node_screen[j, 2],
              col = e_col, lwd = lwd
            )
          }
        }
      }
    }

    # Nodes — for non-fillable shapes (pch < 21), use col as the fill
    fillable <- node_shape >= 21 && node_shape <= 25
    graphics::points(node_screen[, 1], node_screen[, 2],
                     pch = node_shape, cex = node_size,
                     bg = if (fillable) grDevices::adjustcolor(node_color[li], 0.85) else NA,
                     col = if (fillable) grDevices::adjustcolor(node_border, 0.5) else
                       grDevices::adjustcolor(node_color[li], 0.85),
                     lwd = 0.5)

    if (show_labels) {
      graphics::text(node_screen[, 1], node_screen[, 2],
                     labels = all_nodes, cex = label_size, font = 2)
    }
  }

  # --- Connections ---
  if (connections && n_layers > 1) {
    for (li in seq_len(n_layers - 1)) {
      p1 <- node_positions[[li]]
      p2 <- node_positions[[li + 1]]
      graphics::segments(
        p1[, 1], p1[, 2], p2[, 1], p2[, 2],
        col = grDevices::adjustcolor(connection_color, connection_alpha),
        lwd = 0.3
      )
    }
  }

  # --- Time labels ---
  for (li in seq_len(n_layers)) {
    lbl_pos <- .proj(plane_xs[li], -0.5, 0)
    graphics::text(lbl_pos[1], lbl_pos[2], labels = labels[li],
                   cex = 0.85, col = "gray30", font = 2)
  }
  tl_pos <- .proj(box_x / 2, -1.3, 0)
  graphics::text(tl_pos[1], tl_pos[2],
                 labels = if (!is.null(time)) time else "time",
                 cex = 1.1, col = "gray20", font = 3)

  invisible(mats)
}


#' Resolve temporal input to list of adjacency matrices
#' @noRd
.resolve_temporal_input <- function(x, time, slices, cumulative, labels) {
  if (inherits(x, "cograph_network")) {
    raw <- x$data
    if (is.null(raw) || !is.data.frame(raw)) {
      stop("cograph_network has no stored edge data.", call. = FALSE)
    }
    x <- raw
  }

  if (is.data.frame(x)) {
    stopifnot(!is.null(time), time %in% names(x),
              all(c("from", "to") %in% names(x)))
    time_vals <- x[[time]]
    if (!is.null(slices)) {
      time_vals <- cut(as.numeric(time_vals), breaks = slices,
                       include.lowest = TRUE)
      x[[time]] <- time_vals
    }
    periods <- sort(unique(time_vals))
    if (is.null(labels)) labels <- as.character(periods)
    all_nodes <- sort(unique(c(x$from, x$to)))
    nn <- length(all_nodes)
    has_w <- "weight" %in% names(x)

    mats <- lapply(seq_along(periods), function(pi) {
      slice <- if (cumulative) {
        x[time_vals <= periods[pi], , drop = FALSE]
      } else {
        x[time_vals == periods[pi], , drop = FALSE]
      }
      mat <- matrix(0, nn, nn, dimnames = list(all_nodes, all_nodes))
      if (nrow(slice) > 0) {
        fi <- match(slice$from, all_nodes)
        ti <- match(slice$to, all_nodes)
        wt <- if (has_w) slice$weight else rep(1, nrow(slice))
        agg <- stats::aggregate(wt, by = list(fi, ti), FUN = sum)
        mat[cbind(agg[[1]], agg[[2]])] <- agg[[3]]
      }
      mat
    })
  } else if (is.list(x)) {
    mats <- lapply(x, function(net) {
      if (is.matrix(net)) net else to_matrix(net)
    })
    all_nodes <- rownames(mats[[1]])
    if (is.null(all_nodes)) all_nodes <- as.character(seq_len(nrow(mats[[1]])))
    if (is.null(labels)) labels <- paste0("T", seq_along(mats))
  } else {
    stop("x must be an edge list data.frame, cograph_network, or list.",
         call. = FALSE)
  }

  stopifnot(length(mats) >= 2, length(labels) == length(mats))
  list(mats = mats, labels = labels, all_nodes = all_nodes)
}
