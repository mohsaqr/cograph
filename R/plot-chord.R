#' Chord Diagram
#'
#' Draw a chord diagram where nodes are arcs on the outer ring and edges are
#' curved ribbons (chords) connecting them. Arc size is proportional to total
#' flow through each node and chord width is proportional to edge weight.
#'
#' @param x A weight matrix, \code{cograph_network}, \code{tna}, or
#'   \code{igraph} object.
#' @param directed Logical. If \code{NULL} (default), auto-detected from
#'   matrix symmetry.
#' @param segment_colors Colors for the outer ring segments. \code{NULL} uses
#'   a built-in vibrant palette.
#' @param segment_border_color Border color for segments.
#' @param segment_border_width Border width for segments.
#' @param segment_pad Gap between segments in radians.
#' @param segment_width Radial thickness of the outer ring as a fraction of
#'   the radius.
#' @param chord_color_by How to color chords: \code{"source"} (default),
#'   \code{"target"}, or a color vector of length matching the number of
#'   non-zero edges.
#' @param chord_alpha Alpha transparency for chords.
#' @param chord_border Border color for chords. \code{NA} for no border.
#' @param self_loop Logical. Show self-loop chords?
#' @param labels Node labels. \code{NULL} uses row names, \code{FALSE}
#'   suppresses labels.
#' @param label_size Text size multiplier for labels.
#' @param label_color Color for labels.
#' @param label_offset Radial offset of labels beyond the outer ring.
#' @param label_threshold Hide labels for nodes whose flow fraction is below
#'   this value.
#' @param threshold Minimum absolute weight to show a chord.
#' @param ticks Logical. Draw tick marks along the outer ring to indicate
#'   magnitude?
#' @param tick_interval Spacing between ticks in the same units as the weight
#'   matrix. \code{NULL} (default) auto-selects a nice interval.
#' @param tick_labels Logical. Show numeric labels at major ticks?
#' @param tick_size Text size multiplier for tick labels.
#' @param tick_color Color for tick marks and labels.
#' @param start_angle Starting angle in radians (default \code{pi/2}, top).
#' @param clockwise Logical. Lay out segments clockwise?
#' @param title Optional plot title.
#' @param title_size Text size multiplier for the title.
#' @param background Background color for the plot. \code{NULL} (default) uses
#'   the current device background.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns a list with components \code{segments} (data frame
#'   of segment angles and flows) and \code{chords} (data frame of chord
#'   endpoints and weights).
#'
#' @details
#' The diagram is drawn entirely with base R graphics using \code{polygon()}
#' for segments and chords, and \code{bezier_points()} for the curved ribbons.
#'
#' For directed networks, each segment is split into an outgoing half and an
#' incoming half so that chords attach to the correct side. For undirected
#' networks each edge is drawn once and the full segment arc is shared.
#'
#' @examples
#' # Weighted directed matrix
#' mat <- matrix(c(
#'    0, 25,  5, 15,
#'   10,  0, 20,  8,
#'    3, 18,  0, 30,
#'   20,  5, 10,  0
#' ), 4, 4, byrow = TRUE,
#' dimnames = list(c("A", "B", "C", "D"), c("A", "B", "C", "D")))
#'
#' plot_chord(mat)
#' plot_chord(mat, chord_alpha = 0.6, ticks = TRUE)
#'
#' \dontrun{
#' # TNA transition network
#' library(tna)
#' model <- tna(group_regulation)
#' plot_chord(model, ticks = TRUE, segment_width = 0.10)
#' }
#'
#' @export
plot_chord <- function(
    x,
    directed = NULL,
    segment_colors = NULL,
    segment_border_color = "white",
    segment_border_width = 1,
    segment_pad = 0.02,
    segment_width = 0.08,
    chord_color_by = "source",
    chord_alpha = 0.5,
    chord_border = NA,
    self_loop = TRUE,
    labels = NULL,
    label_size = 1,
    label_color = "black",
    label_offset = 0.05,
    label_threshold = 0,
    threshold = 0,
    ticks = FALSE,
    tick_interval = NULL,
    tick_labels = TRUE,
    tick_size = 0.6,
    tick_color = "grey30",
    start_angle = pi / 2,
    clockwise = TRUE,
    title = NULL,
    title_size = 1.2,
    background = NULL,
    ...
) {

  # --- Step 1: Extract and prepare matrix ---
  res <- .chord_extract_matrix(x, directed, threshold, self_loop)
  mat <- res$mat
  directed <- res$directed
  n <- nrow(mat)
  node_labels <- .chord_resolve_labels(labels, mat)

  # --- Step 2: Segment colours ---
  if (is.null(segment_colors)) {
    segment_colors <- .chord_default_palette(n)
  } else {
    segment_colors <- recycle_to_length(segment_colors, n)
  }

  # --- Step 3: Compute segments ---
  segs <- .chord_compute_segments(mat, directed, segment_pad, start_angle,
                                  clockwise)

  # --- Step 4: Compute chords ---
  chords <- .chord_compute_chords(mat, directed, segs)

  # --- Step 5: Resolve chord colours ---
  chord_cols <- .chord_resolve_colors(chords, chord_color_by,
                                      segment_colors, chord_alpha)

  # --- Step 6: Draw ---
  radius <- 1
  inner_r <- radius - segment_width

  old_par <- graphics::par(mar = c(1, 1, 2, 1), xpd = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  plot(NULL, xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3),
       asp = 1, axes = FALSE, xlab = "", ylab = "")

  if (!is.null(background)) {
    graphics::rect(-1.4, -1.4, 1.4, 1.4, col = background, border = NA)
  }

  # 6a. Chords (back to front: large first so small render on top)
  if (nrow(chords) > 0L) {
    ord <- order(chords$weight, decreasing = TRUE)
    lapply(ord, function(k) {
      ch <- chords[k, ]
      .chord_draw_ribbon(
        ch$from_start, ch$from_end,
        ch$to_start, ch$to_end,
        inner_r, chord_cols[k], chord_border
      )
    })
  }

  # 6b. Outer ring segments
  lapply(seq_len(n), function(i) {
    .chord_draw_segment(
      segs$start[i], segs$end[i],
      inner_r, radius,
      fill = segment_colors[i],
      border = segment_border_color,
      lwd = segment_border_width
    )
  })

  # 6c. Labels
  if (!is.null(node_labels)) {
    total_flow <- sum(segs$flow)
    .chord_draw_labels(
      segs, node_labels, radius, label_offset,
      label_size, label_color, label_threshold,
      total_flow
    )
  }

  # 6d. Tick marks
  if (ticks) {
    .chord_draw_ticks(segs, mat, directed, radius, tick_interval,
                      tick_labels, tick_size, tick_color)
  }

  # 6e. Title
  if (!is.null(title)) {
    graphics::title(main = title, line = 0.5, cex.main = title_size)
  }

  invisible(list(segments = segs, chords = chords))
}


# ============================================================================
# Internal Helpers
# ============================================================================

#' Default Vibrant Palette for Chord Diagrams
#'
#' Saturated Material Design-inspired colours that look good as filled arcs
#' and translucent chord ribbons.
#' @noRd
.chord_default_palette <- function(n) {
  base <- c(
    "#E53935",
    "#8E24AA",
    "#1E88E5",
    "#00897B",
    "#43A047",
    "#FFB300",
    "#F4511E",
    "#00ACC1",
    "#6D4C41",
    "#D81B60",
    "#3949AB",
    "#7CB342"
  )
  if (n <= length(base)) {
    base[seq_len(n)]
  } else {
    grDevices::colorRampPalette(base)(n)
  }
}


#' Extract and Prepare Weight Matrix for Chord Diagram
#' @noRd
.chord_extract_matrix <- function(x, directed, threshold, self_loop) {
  mat <- .extract_weights(x)
  stopifnot(is.matrix(mat), is.numeric(mat), nrow(mat) == ncol(mat))

  if (is.null(directed)) {
    directed <- !isSymmetric(unname(mat))
  }

  # Apply threshold
  mat[abs(mat) < threshold] <- 0

  # Remove self-loops if requested
  if (!self_loop) {
    diag(mat) <- 0
  }

  list(mat = mat, directed = directed)
}


#' Resolve Node Labels
#' @noRd
.chord_resolve_labels <- function(labels, mat) {
  if (is.logical(labels) && !labels) return(NULL)
  if (is.null(labels)) {
    labs <- rownames(mat)
    if (is.null(labs)) labs <- as.character(seq_len(nrow(mat)))
    return(labs)
  }
  as.character(labels)
}


#' Compute Segment Angles
#'
#' Allocates angular span per node proportional to total flow.
#' @noRd
.chord_compute_segments <- function(mat, directed, segment_pad, start_angle,
                                    clockwise) {
  n <- nrow(mat)

  # Flow per node: sum of all connections (count diagonal once)
  if (directed) {
    flow <- rowSums(abs(mat)) + colSums(abs(mat))
    flow <- flow - abs(diag(mat))
  } else {
    # Undirected: off-diagonal counted once per node via rowSums
    flow <- rowSums(abs(mat))
  }

  # Give zero-flow nodes a minimal arc so they remain visible
  total_flow <- sum(flow)
  if (total_flow == 0) {
    flow <- rep(1, n)
    total_flow <- n
  } else {
    min_arc <- 0.01
    flow <- pmax(flow, min_arc * total_flow / n)
    total_flow <- sum(flow)
  }

  avail <- 2 * pi - n * segment_pad
  arc_span <- (flow / total_flow) * avail

  # Build angle vectors
  starts <- numeric(n)
  ends <- numeric(n)
  cursor <- start_angle

  direction <- if (clockwise) -1 else 1

  for (i in seq_len(n)) {
    starts[i] <- cursor
    ends[i] <- cursor + direction * arc_span[i]
    cursor <- ends[i] + direction * segment_pad
  }

  data.frame(
    node   = seq_len(n),
    start  = starts,
    end    = ends,
    mid    = (starts + ends) / 2,
    flow   = flow,
    stringsAsFactors = FALSE
  )
}


#' Compute Chord Endpoints
#'
#' For each non-zero edge, compute the angular positions where the chord
#' attaches to the source and target segments.
#' @noRd
.chord_compute_chords <- function(mat, directed, segs) {
  n <- nrow(mat)

  # Pre-compute per-segment allocation: how much arc each edge occupies
  # Directed: segment split into outgoing (first half) and incoming (second)
  # Undirected: full segment shared

  chords_list <- list()
  idx <- 0L

  if (directed) {
    # Out-flow and in-flow per node
    out_total <- rowSums(abs(mat))
    in_total  <- colSums(abs(mat))

    # Each segment arc goes from segs$start to segs$end
    # First half = outgoing, second half = incoming
    seg_span <- segs$end - segs$start  # signed

    out_cursor <- segs$start
    in_cursor  <- segs$start + seg_span / 2

    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        w <- abs(mat[i, j])
        if (w == 0) next

        # Source (i) outgoing portion
        out_frac <- if (out_total[i] > 0) w / out_total[i] else 0
        from_arc <- out_frac * seg_span[i] / 2
        from_start <- out_cursor[i]
        from_end   <- out_cursor[i] + from_arc
        out_cursor[i] <- from_end

        # Target (j) incoming portion
        in_frac <- if (in_total[j] > 0) w / in_total[j] else 0
        to_arc <- in_frac * seg_span[j] / 2
        to_start <- in_cursor[j]
        to_end   <- in_cursor[j] + to_arc
        in_cursor[j] <- to_end

        idx <- idx + 1L
        chords_list[[idx]] <- data.frame(
          from = i, to = j,
          from_start = from_start, from_end = from_end,
          to_start = to_start, to_end = to_end,
          weight = w,
          stringsAsFactors = FALSE
        )
      }
    }
  } else {
    # Undirected: full segment, process i <= j only
    node_total <- rowSums(abs(mat))
    cursor <- segs$start

    for (i in seq_len(n)) {
      for (j in seq(i, n)) {
        w <- abs(mat[i, j])
        if (w == 0) next

        seg_span_i <- segs$end[i] - segs$start[i]
        seg_span_j <- segs$end[j] - segs$start[j]

        frac_i <- if (node_total[i] > 0) w / node_total[i] else 0
        frac_j <- if (node_total[j] > 0) w / node_total[j] else 0

        from_arc <- frac_i * seg_span_i
        to_arc   <- frac_j * seg_span_j

        from_start <- cursor[i]
        from_end   <- cursor[i] + from_arc
        cursor[i]  <- from_end

        to_start <- cursor[j]
        to_end   <- cursor[j] + to_arc
        cursor[j] <- to_end

        idx <- idx + 1L
        chords_list[[idx]] <- data.frame(
          from = i, to = j,
          from_start = from_start, from_end = from_end,
          to_start = to_start, to_end = to_end,
          weight = w,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(chords_list) == 0L) {
    return(data.frame(
      from = integer(0), to = integer(0),
      from_start = numeric(0), from_end = numeric(0),
      to_start = numeric(0), to_end = numeric(0),
      weight = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, chords_list)
}


#' Resolve Chord Colors
#' @noRd
.chord_resolve_colors <- function(chords, chord_color_by, segment_colors,
                                  chord_alpha) {
  nc <- nrow(chords)
  if (nc == 0L) return(character(0))

  if (is.character(chord_color_by) && length(chord_color_by) == 1L) {
    cols <- if (chord_color_by == "target") {
      segment_colors[chords$to]
    } else {
      # default: source
      segment_colors[chords$from]
    }
  } else {
    cols <- recycle_to_length(chord_color_by, nc)
  }

  vapply(cols, function(cc) adjustcolor(cc, alpha.f = chord_alpha),
         character(1), USE.NAMES = FALSE)
}


#' Draw an Outer Ring Segment (arc polygon)
#' @noRd
.chord_draw_segment <- function(angle_start, angle_end, inner_r, outer_r,
                                fill, border, lwd, n_pts = 100) {
  angles_fwd <- seq(angle_start, angle_end, length.out = n_pts)
  angles_rev <- rev(angles_fwd)

  xs <- c(outer_r * cos(angles_fwd), inner_r * cos(angles_rev))
  ys <- c(outer_r * sin(angles_fwd), inner_r * sin(angles_rev))

  graphics::polygon(xs, ys, col = fill, border = border, lwd = lwd)
}


#' Draw a Chord Ribbon
#'
#' Uses the same path construction as d3-chord: source arc, then a quadratic
#' Bezier from the END of the source arc to the START of the target arc
#' (control at origin), then target arc, then a second Bezier back.
#' The cross-connection of opposite arc corners naturally keeps the two
#' curves apart so ribbons stay thick through the centre.
#' @noRd
.chord_draw_ribbon <- function(from_start, from_end, to_start, to_end,
                               inner_r, fill, border, n_pts = 50) {

  # d3-chord path order:
  #   1. Source arc: from_start → from_end
  #   2. Bezier:     from_end   → to_start  (cross! control at origin)
  #   3. Target arc: to_start   → to_end
  #   4. Bezier:     to_end     → from_start (cross back, control at origin)

  src_arc <- seq(from_start, from_end, length.out = 20)

  bez1 <- bezier_points(
    inner_r * cos(from_end), inner_r * sin(from_end),
    0, 0,
    inner_r * cos(to_start), inner_r * sin(to_start),
    n = n_pts
  )

  tgt_arc <- seq(to_start, to_end, length.out = 20)

  bez2 <- bezier_points(
    inner_r * cos(to_end), inner_r * sin(to_end),
    0, 0,
    inner_r * cos(from_start), inner_r * sin(from_start),
    n = n_pts
  )

  xs <- c(inner_r * cos(src_arc), bez1$x,
          inner_r * cos(tgt_arc), bez2$x)
  ys <- c(inner_r * sin(src_arc), bez1$y,
          inner_r * sin(tgt_arc), bez2$y)

  graphics::polygon(xs, ys, col = fill, border = border)
}


#' Draw Rotated Labels Around the Ring
#' @noRd
.chord_draw_labels <- function(segs, node_labels, radius, label_offset,
                               label_size, label_color, label_threshold,
                               total_flow) {
  r <- radius + label_offset
  n <- nrow(segs)
  lapply(seq_len(n), function(i) {
    # Skip labels below threshold
    frac <- segs$flow[i] / total_flow
    if (frac < label_threshold) return(invisible(NULL))

    angle <- segs$mid[i]
    x <- r * cos(angle)
    y <- r * sin(angle)

    # Rotation: degrees from horizontal
    deg <- angle * 180 / pi

    # Flip text on left half for readability
    adj <- c(0, 0.5)
    if (cos(angle) < 0) {
      deg <- deg + 180
      adj <- c(1, 0.5)
    }

    graphics::text(x, y, node_labels[i],
                   srt = deg, adj = adj,
                   cex = label_size, col = label_color)
  })
  invisible()
}


#' Draw Tick Marks Along the Outer Ring
#'
#' Draws small radial tick marks at regular intervals along each segment to
#' indicate magnitude. Optionally labels every major tick with its value.
#' @noRd
.chord_draw_ticks <- function(segs, mat, directed, radius, tick_interval,
                              tick_labels, tick_size, tick_color) {
  n <- nrow(segs)
  total_flow <- sum(segs$flow)

  # Auto-select tick interval based on weight scale
  if (is.null(tick_interval)) {
    tick_interval <- .chord_nice_interval(max(segs$flow), mat)
  }

  # Major ticks every tick_interval; minor ticks at half that
  major <- tick_interval
  minor <- tick_interval / 2

  tick_len_major <- 0.03
  tick_len_minor <- 0.015

  lapply(seq_len(n), function(i) {
    seg_start <- segs$start[i]
    seg_end   <- segs$end[i]
    seg_flow  <- segs$flow[i]
    seg_span  <- seg_end - seg_start   # signed

    if (abs(seg_flow) < 1e-10) return(invisible(NULL))

    # Place ticks from 0 to seg_flow
    tick_vals <- seq(0, seg_flow, by = minor)
    # Remove last value if it's very close to seg_flow (avoid overlap at gap)
    if (length(tick_vals) > 1 &&
        abs(tick_vals[length(tick_vals)] - seg_flow) < minor * 0.1) {
      tick_vals <- tick_vals[-length(tick_vals)]
    }

    lapply(tick_vals, function(v) {
      frac  <- v / seg_flow
      angle <- seg_start + frac * seg_span
      is_major <- (abs(v %% major) < 1e-10) ||
                  (abs(v %% major - major) < 1e-10)
      tlen <- if (is_major) tick_len_major else tick_len_minor

      # Radial tick line from radius to radius + tlen
      x0 <- radius * cos(angle)
      y0 <- radius * sin(angle)
      x1 <- (radius + tlen) * cos(angle)
      y1 <- (radius + tlen) * sin(angle)
      graphics::segments(x0, y0, x1, y1, col = tick_color, lwd = 0.5)

      # Label major ticks
      if (is_major && tick_labels && v > 0) {
        lbl <- .chord_format_tick(v)
        lx  <- (radius + tlen + 0.015) * cos(angle)
        ly  <- (radius + tlen + 0.015) * sin(angle)
        deg <- angle * 180 / pi
        adj <- c(0, 0.5)
        if (cos(angle) < 0) {
          deg <- deg + 180
          adj <- c(1, 0.5)
        }
        graphics::text(lx, ly, lbl, srt = deg, adj = adj,
                       cex = tick_size, col = tick_color)
      }
    })
  })
  invisible()
}


#' Choose a Nice Tick Interval
#'
#' Detects the scale of the weight matrix (probability 0-1 vs integer) and
#' picks an interval that produces readable tick labels.
#' @noRd
.chord_nice_interval <- function(max_val, mat) {
  if (max_val <= 0) return(1)

  # Detect probability scale: all weights in [0, 1]
  vals <- abs(mat[mat != 0])
  if (length(vals) > 0 && max(vals) <= 1) {
    # Probability scale — use 0.1 or 0.2
    if (max_val <= 0.5) return(0.05)
    if (max_val <= 1.5) return(0.1)
    return(0.2)
  }

  # Integer / general scale
  raw <- max_val / 5
  mag <- 10^floor(log10(raw))
  residual <- raw / mag
  nice <- if (residual < 1.5) 1
          else if (residual < 3.5) 2
          else if (residual < 7.5) 5
          else 10
  nice * mag
}


#' Format Tick Label
#' @noRd
.chord_format_tick <- function(v) {
  if (abs(v) >= 1) {
    formatC(v, format = "fg", big.mark = "")
  } else {
    formatC(v, format = "fg", digits = 2, big.mark = "")
  }
}
