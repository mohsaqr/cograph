#' @title Forest Plot for Bootstrap Network Results
#' @description
#' A ggplot2-based forest plot for \code{net_bootstrap} and \code{boot_glasso}
#' objects. Each row is one network edge; horizontal bars span the confidence
#' interval and a filled square marks the point estimate. A dashed reference
#' line runs through zero.
#'
#' @name plot_bootstrap_forest
#' @keywords internal
NULL

# ── helpers ──────────────────────────────────────────────────────────────────

.p_stars <- function(p) {
  vapply(p, function(v) {
    if      (is.na(v))   ""
    else if (v < 0.001)  "***"
    else if (v < 0.01)   "**"
    else if (v < 0.05)   "*"
    else                 ""
  }, character(1))
}

# Extract long-form data frame from a net_bootstrap object.
# Returns columns: edge, estimate, ci_lower, ci_upper, cr_lower, cr_upper,
#                  p_value, sig, has_cr
.forest_df_net_bootstrap <- function(x, alpha) {
  alpha <- alpha %||% x$ci_level %||% 0.05

  mean_mat <- x$mean
  lo_mat   <- x$ci_lower
  hi_mat   <- x$ci_upper
  pv_mat   <- x$p_values
  crl_mat  <- x$cr_lower   # NULL when inference != "stability"
  crh_mat  <- x$cr_upper

  is_dir <- isTRUE(x$original$directed %||% x$model$directed %||% TRUE)
  nms    <- rownames(mean_mat) %||% as.character(seq_len(nrow(mean_mat)))

  if (is_dir) {
    keep <- which(mean_mat != 0, arr.ind = TRUE)
    keep <- keep[keep[, 1] != keep[, 2], , drop = FALSE]
    sep  <- " \u2192 "
  } else {
    keep <- which(upper.tri(mean_mat) & mean_mat != 0, arr.ind = TRUE)
    sep  <- " \u2014 "
  }
  if (nrow(keep) == 0) stop("No non-zero edges found in bootstrap results.")

  data.frame(
    edge     = paste0(nms[keep[, 1]], sep, nms[keep[, 2]]),
    estimate = mean_mat[keep],
    ci_lower = lo_mat[keep],
    ci_upper = hi_mat[keep],
    cr_lower = if (!is.null(crl_mat)) crl_mat[keep] else NA_real_,
    cr_upper = if (!is.null(crh_mat)) crh_mat[keep] else NA_real_,
    p_value  = pv_mat[keep],
    sig      = pv_mat[keep] < alpha,
    has_cr   = !is.null(crl_mat),
    stringsAsFactors = FALSE
  )
}

# Extract long-form data frame from a boot_glasso object.
# boot_glasso has no consistency range — cr columns set to NA.
.forest_df_boot_glasso <- function(x, alpha) {
  alpha  <- alpha %||% x$alpha %||% 0.05
  thresh <- 1 - alpha

  df <- x$edge_ci
  if (is.null(df) || nrow(df) == 0)
    stop("boot_glasso object has no edge CI data.")

  data.frame(
    edge     = df$edge,
    estimate = df$weight,
    ci_lower = df$ci_lower,
    ci_upper = df$ci_upper,
    cr_lower = NA_real_,
    cr_upper = NA_real_,
    p_value  = 1 - df$inclusion,
    sig      = df$inclusion >= thresh,
    has_cr   = FALSE,
    stringsAsFactors = FALSE
  )
}

# ── core plot builder ─────────────────────────────────────────────────────────

.build_forest_plot <- function(
    df,
    interval     = c("ci", "cr", "both"),
    show_nonsig  = TRUE,
    sort_by      = c("estimate", "significance", "name"),
    n_top        = NULL,
    sig_color    = "#2C6E8A",
    cr_color     = "#D4829A",
    nonsig_color = "#CCCCCC",
    ref_color    = "#555555",
    point_size   = 3,
    title        = NULL,
    subtitle     = NULL
) {
  interval <- match.arg(interval)
  sort_by  <- match.arg(sort_by)

  # If CR requested but not available, fall back silently to CI
  has_cr <- isTRUE(df$has_cr[1]) && !all(is.na(df$cr_lower))
  if (interval %in% c("cr", "both") && !has_cr) {
    message("Consistency range not available in this object; showing CI only.")
    interval <- "ci"
  }

  if (!show_nonsig) df <- df[df$sig, , drop = FALSE]
  if (nrow(df) == 0)
    stop("No significant edges to display. Use show_nonsig = TRUE to include all.")

  # Sort
  df <- switch(
    sort_by,
    estimate     = df[order(df$estimate), ],
    significance = df[order(df$p_value, decreasing = TRUE), ],
    name         = df[order(df$edge), ]
  )
  if (!is.null(n_top)) {
    all_sorted <- df[order(abs(df$estimate), decreasing = TRUE), ]
    df <- all_sorted[seq_len(min(n_top, nrow(all_sorted))), ]
    df <- df[order(df$estimate), ]
  }

  df$edge  <- factor(df$edge, levels = df$edge)
  df$color <- ifelse(df$sig, sig_color, nonsig_color)
  df$alpha <- ifelse(df$sig, 1, 0.45)   # non-sig edges are faded
  df$stars <- .p_stars(df$p_value)

  # CR bar color: sig = cr_color, nonsig = faded grey
  df$cr_col <- ifelse(df$sig, cr_color, nonsig_color)

  # Determine x-axis range across all intervals being shown
  bar_cols <- switch(
    interval,
    ci   = c("ci_lower", "ci_upper"),
    cr   = c("cr_lower", "cr_upper"),
    both = c("ci_lower", "ci_upper", "cr_lower", "cr_upper")
  )
  x_range <- range(unlist(df[, bar_cols]), na.rm = TRUE)
  x_pad   <- diff(x_range) * 0.18
  x_star  <- x_range[2] + diff(x_range) * 0.04
  x_lim   <- c(x_range[1] - x_pad * 0.4, x_range[2] + x_pad)

  # Build caption
  caption <- switch(
    interval,
    ci   = "Squares: point estimates  |  Bars: bootstrap CI  |  * p<0.05  ** p<0.01  *** p<0.001",
    cr   = "Squares: point estimates  |  Bars: consistency range  |  * p<0.05  ** p<0.01  *** p<0.001",
    both = paste0("\u25A0 = estimate  |  \u2014 blue: bootstrap CI  |  ",
                  "\u2014 amber: consistency range  |  * p<0.05  ** p<0.01  *** p<0.001")
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$edge)) +

    # Reference line
    ggplot2::geom_vline(
      xintercept = 0,
      linetype   = "dashed",
      colour     = ref_color,
      linewidth  = 0.45,
      alpha      = 0.6
    )

  # ---- interval layers -------------------------------------------------------

  if (interval %in% c("ci", "both")) {
    p <- p + ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$ci_lower, xmax = .data$ci_upper,
                   colour = .data$color, alpha = .data$alpha),
      height    = 0.28,
      linewidth = 0.65
    )
  }

  if (interval %in% c("cr", "both")) {
    p <- p + ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$cr_lower, xmax = .data$cr_upper,
                   colour = .data$cr_col,
                   alpha  = I(.data$alpha * 0.55)),
      height    = if (interval == "both") 0.10 else 0.22,
      linewidth = if (interval == "both") 0.90 else 0.55
    )
  }

  # ---- point estimates -------------------------------------------------------

  p <- p +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$color, alpha = .data$alpha),
      shape = 15,
      size  = point_size
    ) +

    # Significance stars
    ggplot2::geom_text(
      data = df[df$sig, , drop = FALSE],
      ggplot2::aes(x = x_star, label = .data$stars),
      hjust    = 0,
      size     = 3.2,
      colour   = sig_color,
      fontface = "bold"
    ) +

    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_x_continuous(limits = x_lim, expand = c(0, 0)) +

    ggplot2::labs(
      x        = "Edge Weight (Bootstrap Estimate)",
      y        = NULL,
      title    = title,
      subtitle = subtitle,
      caption  = caption
    ) +

    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        colour    = "#EBEBEB",
        linewidth = 0.4
      ),
      axis.text.y  = ggplot2::element_text(
        size   = 9,
        colour = "#333333",
        margin = ggplot2::margin(r = 4)
      ),
      axis.text.x  = ggplot2::element_text(size = 8.5, colour = "#555555"),
      axis.title.x = ggplot2::element_text(
        size   = 9,
        colour = "#555555",
        margin = ggplot2::margin(t = 6)
      ),
      plot.title    = ggplot2::element_text(
        size   = 12,
        face   = "bold",
        colour = "#1A1A1A",
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size   = 9.5,
        colour = "#666666",
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption  = ggplot2::element_text(
        size   = 7.5,
        colour = "#888888",
        hjust  = 0,
        margin = ggplot2::margin(t = 8)
      ),
      plot.margin       = ggplot2::margin(12, 16, 8, 12),
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "white", colour = NA)
    )

  p
}

# ── radial forest builder ─────────────────────────────────────────────────────

.build_radial_forest_plot <- function(
    df,
    interval     = c("ci", "cr", "both"),
    show_nonsig  = TRUE,
    n_top        = NULL,
    sig_color    = "#2C6E8A",
    cr_color     = "#D4829A",
    nonsig_color = "#CCCCCC",
    ring_color   = "#C8C8C8",
    median_color = "#AAAAAA",
    label_size   = 2.3,
    label_color  = NULL,       # NULL = inherit edge colour
    point_size   = 2,
    title        = NULL,
    subtitle     = NULL
) {
  interval <- match.arg(interval)

  has_cr <- isTRUE(df$has_cr[1]) && !all(is.na(df$cr_lower))
  if (interval %in% c("cr", "both") && !has_cr) {
    message("Consistency range not available; showing CI only.")
    interval <- "ci"
  }

  if (!show_nonsig) df <- df[df$sig, , drop = FALSE]
  if (nrow(df) == 0) stop("No edges to display.")

  # Sort alphabetically so edges fan evenly; n_top trims to largest by estimate
  df <- df[order(df$edge), ]
  if (!is.null(n_top)) {
    keep <- order(abs(df$estimate), decreasing = TRUE)[seq_len(min(n_top, nrow(df)))]
    df   <- df[sort(keep), ]   # preserve alphabetical order within top-N
  }

  n <- nrow(df)

  nonsig_plot  <- if (nonsig_color == "#CCCCCC") "#999999" else nonsig_color
  df$color     <- ifelse(df$sig, sig_color, nonsig_plot)
  df$alpha_val <- ifelse(df$sig, 1, 0.9)

  # Clockwise from top (pi/2 → pi/2 - 2*pi), one angle per edge
  angles   <- seq(pi / 2, pi / 2 - 2 * pi, length.out = n + 1)[seq_len(n)]
  df$angle <- angles

  # Scale: zoom to actual CI range so bars fill the annular zone.
  # Inner ring (r_inner) = min(ci_lower); outer ring (r=1) = max(ci_upper) + 5% pad.
  r_inner <- 0.58
  v_min   <- min(df$ci_lower, na.rm = TRUE)
  v_max   <- max(df$ci_upper, na.rm = TRUE) * 1.05
  to_r    <- function(v) {
    r_inner + pmin(pmax((v - v_min) / (v_max - v_min), 0), 1) * (1 - r_inner)
  }

  df$x_est <- to_r(df$estimate) * cos(angles)
  df$y_est <- to_r(df$estimate) * sin(angles)
  df$x_lo  <- to_r(df$ci_lower) * cos(angles)
  df$y_lo  <- to_r(df$ci_lower) * sin(angles)
  df$x_hi  <- to_r(df$ci_upper) * cos(angles)
  df$y_hi  <- to_r(df$ci_upper) * sin(angles)

  if (interval %in% c("cr", "both")) {
    df$x_crl  <- to_r(df$cr_lower) * cos(angles)
    df$y_crl  <- to_r(df$cr_lower) * sin(angles)
    df$x_crh  <- to_r(df$cr_upper) * cos(angles)
    df$y_crh  <- to_r(df$cr_upper) * sin(angles)
    df$cr_col <- ifelse(df$sig, cr_color, nonsig_color)
  }

  # Median ring radius
  r_median <- to_r(median(df$estimate, na.rm = TRUE))

  # Labels flush against the outer ring
  label_r  <- 1.03
  df$x_lab <- label_r * cos(angles)
  df$y_lab <- label_r * sin(angles)

  # Radially outward text: flip left half so it stays readable
  deg            <- angles * 180 / pi
  flip           <- cos(angles) < 0
  df$text_angle  <- ifelse(flip, deg + 180, deg)
  df$hjust       <- ifelse(flip, 1, 0)

  # Reference geometry
  theta_seq    <- seq(0, 2 * pi, length.out = 300)
  ring_inner   <- data.frame(x = r_inner   * cos(theta_seq), y = r_inner   * sin(theta_seq))
  ring_median  <- data.frame(x = r_median  * cos(theta_seq), y = r_median  * sin(theta_seq))
  ring_outer   <- data.frame(x = cos(theta_seq), y = sin(theta_seq))

  # Two faint grid rings (quartiles)
  q1_r <- to_r(quantile(df$estimate, 0.25, na.rm = TRUE))
  q3_r <- to_r(quantile(df$estimate, 0.75, na.rm = TRUE))
  grid_rings <- do.call(rbind, lapply(c(q1_r, q3_r), function(r) {
    data.frame(x = r * cos(theta_seq), y = r * sin(theta_seq), grp = r)
  }))

  # Grid labels at 3 o'clock
  grid_lab_df <- data.frame(
    x   = c(q1_r, r_median, q3_r) + 0.014,
    y   = 0.012,
    lab = as.character(round(c(
      quantile(df$estimate, 0.25, na.rm = TRUE),
      median(df$estimate,   na.rm = TRUE),
      quantile(df$estimate, 0.75, na.rm = TRUE)
    ), 2))
  )

  lim <- 1.52

  p <- ggplot2::ggplot() +
    # Q1 / Q3 faint grid rings
    ggplot2::geom_path(
      data = grid_rings,
      ggplot2::aes(x = x, y = y, group = grp),
      colour = "#EBEBEB", linewidth = 0.3
    ) +
    ggplot2::geom_path(
      data = ring_outer,
      ggplot2::aes(x = x, y = y),
      colour = ring_color, linewidth = 0.25
    ) +
    # Guide spokes: inner ring → outer ring
    ggplot2::geom_segment(
      data = df,
      ggplot2::aes(x = r_inner * cos(angle), y = r_inner * sin(angle),
                   xend = cos(angle), yend = sin(angle),
                   colour = color, alpha = I(alpha_val * 0.12)),
      linewidth = 0.35
    )

  if (interval %in% c("ci", "both")) {
    p <- p + ggplot2::geom_segment(
      data = df,
      ggplot2::aes(x = x_lo, y = y_lo, xend = x_hi, yend = y_hi,
                   colour = color, alpha = alpha_val),
      linewidth = 0.7, lineend = "round"
    )
  }

  if (interval %in% c("cr", "both")) {
    p <- p + ggplot2::geom_segment(
      data = df,
      ggplot2::aes(x = x_crl, y = y_crl, xend = x_crh, yend = y_crh,
                   colour = cr_col, alpha = I(alpha_val * 0.5)),
      linewidth = 0.65, lineend = "round"
    )
  }

  p <- p +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = x_est, y = y_est, colour = color, alpha = alpha_val),
      shape = 15, size = point_size * 0.45
    ) +
    # Median ring — slightly more prominent
    ggplot2::geom_path(
      data = ring_median,
      ggplot2::aes(x = x, y = y),
      colour = median_color, linewidth = 0.3, linetype = "dashed"
    ) +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(x = x_lab, y = y_lab, label = edge,
                   angle = text_angle, hjust = hjust,
                   colour = if (is.null(label_color)) color else label_color,
                   alpha  = alpha_val),
      size = label_size
    ) +
    ggplot2::geom_text(
      data = grid_lab_df,
      ggplot2::aes(x = x, y = y, label = lab),
      hjust = 0, size = label_size * 0.87, colour = median_color
    ) +
    # Inner ring (data minimum)
    ggplot2::geom_path(
      data = ring_inner,
      ggplot2::aes(x = x, y = y),
      colour = ring_color, linewidth = 0.25
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(clip = "off",
                         xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(
        size = 12, face = "bold", hjust = 0.5,
        colour = "#1A1A1A", margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9, hjust = 0.5,
        colour = "#666666", margin = ggplot2::margin(b = 8)
      ),
      plot.margin     = ggplot2::margin(20, 40, 20, 40),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  p
}

# ── grouped radial builder ───────────────────────────────────────────────────

.build_grouped_radial_plot <- function(
    df,
    interval     = c("ci", "cr", "both"),
    show_nonsig  = TRUE,
    n_top        = NULL,
    node_colors  = NULL,
    cr_color     = "#D4829A",
    ring_color   = "#C8C8C8",
    median_color = "#AAAAAA",
    label_size   = 2.9,
    label_color  = NULL,
    point_size   = 1.5,
    r_inner      = 0.38,
    r_outer      = 0.72,
    gap_rad      = 0.10,
    title        = NULL,
    subtitle     = NULL
) {
  interval <- match.arg(interval)

  has_cr <- isTRUE(df$has_cr[1]) && !all(is.na(df$cr_lower))
  if (interval %in% c("cr", "both") && !has_cr) {
    message("Consistency range not available; showing CI only.")
    interval <- "ci"
  }

  # Parse from / to out of "A → B" or "A — B"
  parts   <- strsplit(df$edge, " [\u2192\u2014] ")
  df$from <- vapply(parts, `[[`, character(1), 1L)
  df$to   <- vapply(parts, `[[`, character(1), 2L)

  if (!show_nonsig) df <- df[df$sig, , drop = FALSE]
  if (nrow(df) == 0) stop("No edges to display.")

  # Sort: within each from-node, alphabetically by to-node
  df <- df[order(df$from, df$to), ]

  if (!is.null(n_top)) {
    keep <- order(abs(df$estimate), decreasing = TRUE)[seq_len(min(n_top, nrow(df)))]
    df   <- df[sort(keep), ]
    df   <- df[order(df$from, df$to), ]
  }

  from_nodes <- unique(df$from)
  n_from     <- length(from_nodes)

  # Node colors: use supplied palette/named vector, or fall back to darkened Okabe-Ito
  oi <- c("#005A8E","#B87D00","#007B5A","#A84A00","#2A91C9","#A35284","#C4B800","#222222","#666666")
  if (is.null(node_colors)) {
    node_col <- setNames(oi[((seq_len(n_from) - 1L) %% length(oi)) + 1L], from_nodes)
  } else if (!is.null(names(node_colors))) {
    # Named vector — match by node name, fill missing with Okabe-Ito
    node_col <- node_colors[from_nodes]
    missing  <- is.na(node_col)
    if (any(missing))
      node_col[missing] <- oi[((which(missing) - 1L) %% length(oi)) + 1L]
    names(node_col) <- from_nodes
  } else {
    # Unnamed vector — assign in order, cycling if needed
    node_col <- setNames(
      node_colors[((seq_len(n_from) - 1L) %% length(node_colors)) + 1L],
      from_nodes
    )
  }

  df$color     <- node_col[df$from]
  df$alpha_val <- ifelse(df$sig, 1, 0.50)

  # Sector angles: clockwise from top, gap between sectors
  available   <- 2 * pi - gap_rad * n_from
  edge_counts <- vapply(from_nodes, function(n) sum(df$from == n), integer(1))
  sector_sz   <- (edge_counts / sum(edge_counts)) * available

  sector_start <- numeric(n_from)
  sector_start[1] <- pi / 2
  for (i in seq_len(n_from - 1L))
    sector_start[i + 1L] <- sector_start[i] - sector_sz[i] - gap_rad

  # Assign one angle per edge within its sector (clockwise)
  df$angle     <- NA_real_
  sector_mid   <- numeric(n_from)

  for (i in seq_along(from_nodes)) {
    node  <- from_nodes[i]
    idx   <- which(df$from == node)
    n_e   <- length(idx)
    s     <- sector_start[i]
    sz    <- sector_sz[i]
    sector_mid[i] <- s - sz / 2
    pad   <- sz * 0.08
    df$angle[idx] <- if (n_e == 1L) {
      s - sz / 2
    } else {
      seq(s - pad, s - sz + pad, length.out = n_e)
    }
  }

  angles <- df$angle

  # Radial scale: zoom to data range
  v_min   <- min(df$ci_lower, na.rm = TRUE)
  v_max   <- max(df$ci_upper, na.rm = TRUE) * 1.05
  to_r    <- function(v) r_inner + pmin(pmax((v - v_min) / (v_max - v_min), 0), 1) * (r_outer - r_inner)

  r_median <- to_r(median(df$estimate, na.rm = TRUE))

  df$x_est <- to_r(df$estimate) * cos(angles)
  df$y_est <- to_r(df$estimate) * sin(angles)
  df$x_lo  <- to_r(df$ci_lower) * cos(angles)
  df$y_lo  <- to_r(df$ci_lower) * sin(angles)
  df$x_hi  <- to_r(df$ci_upper) * cos(angles)
  df$y_hi  <- to_r(df$ci_upper) * sin(angles)

  if (interval %in% c("cr", "both")) {
    df$x_crl  <- to_r(df$cr_lower) * cos(angles)
    df$y_crl  <- to_r(df$cr_lower) * sin(angles)
    df$x_crh  <- to_r(df$cr_upper) * cos(angles)
    df$y_crh  <- to_r(df$cr_upper) * sin(angles)
    df$cr_col <- cr_color
  }

  # Outer (target) labels
  label_r   <- r_outer + 0.06
  df$x_lab  <- label_r * cos(angles)
  df$y_lab  <- label_r * sin(angles)
  deg       <- angles * 180 / pi
  flip      <- cos(angles) < 0
  df$text_angle <- ifelse(flip, deg + 180, deg)
  df$hjust      <- ifelse(flip, 1, 0)
  df$lab_col    <- if (is.null(label_color)) df$color else label_color

  # Inner (source) labels — tangential, at sector midpoints inside inner ring
  src_r   <- r_inner * 0.80
  src_df  <- data.frame(
    node        = from_nodes,
    angle       = sector_mid,
    x_lab       = src_r * cos(sector_mid),
    y_lab       = src_r * sin(sector_mid),
    color       = node_col[from_nodes],
    stringsAsFactors = FALSE
  )
  src_deg            <- sector_mid * 180 / pi
  src_flip           <- cos(sector_mid) < 0
  src_df$text_angle  <- ifelse(src_flip, src_deg + 90, src_deg - 90)

  # Reference geometry
  theta_seq   <- seq(0, 2 * pi, length.out = 300)
  ring_inner  <- data.frame(x = r_inner * cos(theta_seq), y = r_inner * sin(theta_seq))
  ring_median <- data.frame(x = r_median * cos(theta_seq), y = r_median * sin(theta_seq))
  ring_outer  <- data.frame(x = r_outer * cos(theta_seq), y = r_outer * sin(theta_seq))

  lim <- 1.52

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = ring_outer, ggplot2::aes(x = x, y = y),
      colour = ring_color, linewidth = 0.25
    ) +
    ggplot2::geom_path(
      data = ring_inner, ggplot2::aes(x = x, y = y),
      colour = ring_color, linewidth = 0.25
    ) +
    # Guide spokes
    ggplot2::geom_segment(
      data = df,
      ggplot2::aes(x = r_inner * cos(angle), y = r_inner * sin(angle),
                   xend = r_outer * cos(angle), yend = r_outer * sin(angle),
                   colour = color, alpha = I(alpha_val * 0.12)),
      linewidth = 0.3
    )

  if (interval %in% c("ci", "both")) {
    p <- p + ggplot2::geom_segment(
      data = df,
      ggplot2::aes(x = x_lo, y = y_lo, xend = x_hi, yend = y_hi,
                   colour = color, alpha = alpha_val),
      linewidth = 0.7, lineend = "round"
    )
  }

  if (interval %in% c("cr", "both")) {
    p <- p + ggplot2::geom_segment(
      data = df,
      ggplot2::aes(x = x_crl, y = y_crl, xend = x_crh, yend = y_crh,
                   colour = cr_col, alpha = I(alpha_val * 0.5)),
      linewidth = 0.45, lineend = "round"
    )
  }

  p <- p +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = x_est, y = y_est, colour = color, alpha = alpha_val),
      shape = 15, size = point_size * 0.45
    ) +
    ggplot2::geom_path(
      data = ring_median, ggplot2::aes(x = x, y = y),
      colour = median_color, linewidth = 0.25, linetype = "dashed"
    ) +
    # Target labels — outer ring, radial
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(x = x_lab, y = y_lab, label = to,
                   angle = text_angle, hjust = hjust,
                   colour = lab_col, alpha = alpha_val),
      size = label_size
    ) +
    # Source labels — inner ring, tangential, bold
    ggplot2::geom_text(
      data = src_df,
      ggplot2::aes(x = x_lab, y = y_lab, label = node,
                   angle = text_angle, colour = color),
      hjust = 0.5, size = label_size * 1.15, fontface = "bold"
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(clip = "off",
                         xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(
        size = 12, face = "bold", hjust = 0.5,
        colour = "#1A1A1A", margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9, hjust = 0.5,
        colour = "#666666", margin = ggplot2::margin(b = 8)
      ),
      plot.margin     = ggplot2::margin(20, 40, 20, 40),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  p
}

# ── exported S3 generics ──────────────────────────────────────────────────────

#' Forest Plot for Bootstrap Network Results
#'
#' Produces a ggplot2 forest plot where each row is one network edge, the
#' square marks the bootstrap mean estimate, and the horizontal bar spans the
#' selected interval. A dashed reference line runs through zero. Significant
#' edges are highlighted in colour; non-significant ones appear in grey (only
#' shown when \code{show_nonsig = TRUE}).
#'
#' For \code{net_bootstrap} objects from stability inference, both a bootstrap
#' confidence interval (\code{ci_lower}/\code{ci_upper}) and a consistency
#' range (\code{cr_lower}/\code{cr_upper}) are available. Use
#' \code{interval = "both"} to overlay both on the same plot.
#'
#' @param x A \code{net_bootstrap} or \code{boot_glasso} object.
#' @param alpha Significance threshold. Default: inherits from the object
#'   (\code{$ci_level} or \code{$alpha}), falling back to \code{0.05}.
#' @param interval Which interval to display: \code{"ci"} (bootstrap confidence
#'   interval, default), \code{"cr"} (consistency range, stability inference
#'   only), or \code{"both"} (CI as outer bar, CR as inner bar).
#' @param layout \code{"linear"} (default) draws the classic tall forest plot;
#'   \code{"radial"} arranges each edge as a spoke around a circle, with the
#'   inner ring at 0 and the outer ring at 1 (or the data maximum).
#' @param show_nonsig Logical: include non-significant edges (greyed out)?
#'   Default \code{TRUE}.
#' @param sort_by How to order edges on the y-axis (linear layout) or
#'   clockwise from top (radial layout):
#'   \code{"estimate"} (default, ascending), \code{"significance"} (most
#'   significant at top), or \code{"name"} (alphabetical).
#' @param n_top Integer: restrict to the \code{n_top} edges with the largest
#'   absolute estimate. Applied after significance filtering. Default \code{NULL}.
#' @param sig_color Colour for significant CI bars and points. Default \code{"#2C6E8A"} (teal-blue).
#' @param cr_color Colour for the consistency range bar (\code{interval = "cr"} or \code{"both"}).
#'   Default \code{"#D4820A"} (amber).
#' @param nonsig_color Colour for non-significant edges. Default \code{"#CCCCCC"}.
#' @param ring_color Colour for the reference rings (radial layout only). Default \code{"#C8C8C8"}.
#' @param median_color Colour for the dashed median ring (radial layout only). Default \code{"#AAAAAA"}.
#' @param label_size Text size for edge labels (radial layout only). Default \code{2.3}.
#' @param label_color Fixed colour for edge labels (radial layout only). \code{NULL} (default)
#'   inherits the edge colour (teal for significant, grey for non-significant).
#' @param point_size Size of the estimate square. Default \code{3} (linear) or \code{2} (radial).
#' @param title Plot title. Default \code{NULL}.
#' @param subtitle Plot subtitle. Default \code{NULL}.
#' @param ... Currently unused.
#'
#' @return A \code{ggplot} object.
#' @export
plot_bootstrap_forest <- function(x, ...) UseMethod("plot_bootstrap_forest")

#' @rdname plot_bootstrap_forest
#' @export
plot_bootstrap_forest.net_bootstrap <- function(
    x,
    alpha        = NULL,
    layout       = c("linear", "radial", "grouped"),
    interval     = c("ci", "cr", "both"),
    show_nonsig  = TRUE,
    sort_by      = c("estimate", "significance", "name"),
    n_top        = NULL,
    node_colors  = NULL,
    sig_color    = "#2C6E8A",
    cr_color     = "#D4829A",
    nonsig_color = "#CCCCCC",
    ring_color   = "#C8C8C8",
    median_color = "#AAAAAA",
    label_size   = 2.9,
    label_color  = NULL,
    point_size   = if (match.arg(layout) == "radial") 2 else 3,
    r_inner      = 0.38,
    r_outer      = 0.72,
    gap_rad      = 0.10,
    title        = NULL,
    subtitle     = NULL,
    ...
) {
  layout <- match.arg(layout)
  df     <- .forest_df_net_bootstrap(x, alpha)

  # Auto-read node colors from the original network object if not supplied
  if (is.null(node_colors) && layout == "grouped") {
    orig_nodes <- x$original$nodes
    if (!is.null(orig_nodes) && "color" %in% names(orig_nodes)) {
      nms         <- orig_nodes$name %||% orig_nodes$label %||% orig_nodes$id
      node_colors <- setNames(orig_nodes$color, nms)
    }
  }

  grouped_args <- list(
    df           = df,
    interval     = match.arg(interval),
    show_nonsig  = show_nonsig,
    n_top        = n_top,
    node_colors  = node_colors,
    cr_color     = cr_color,
    ring_color   = ring_color,
    median_color = median_color,
    label_size   = label_size,
    label_color  = label_color,
    point_size   = point_size,
    r_inner      = r_inner,
    r_outer      = r_outer,
    gap_rad      = gap_rad,
    title        = title,
    subtitle     = subtitle
  )
  if (layout == "grouped") {
    do.call(.build_grouped_radial_plot, grouped_args)
  } else if (layout == "radial") {
    .build_radial_forest_plot(
      df,
      interval     = match.arg(interval),
      show_nonsig  = show_nonsig,
      n_top        = n_top,
      sig_color    = sig_color,
      cr_color     = cr_color,
      nonsig_color = nonsig_color,
      ring_color   = ring_color,
      median_color = median_color,
      label_size   = label_size,
      label_color  = label_color,
      point_size   = point_size,
      title        = title,
      subtitle     = subtitle
    )
  } else {
    .build_forest_plot(
      df,
      interval     = match.arg(interval),
      show_nonsig  = show_nonsig,
      sort_by      = match.arg(sort_by),
      n_top        = n_top,
      sig_color    = sig_color,
      cr_color     = cr_color,
      nonsig_color = nonsig_color,
      point_size   = point_size,
      title        = title,
      subtitle     = subtitle
    )
  }
}

#' @rdname plot_bootstrap_forest
#' @export
plot_bootstrap_forest.boot_glasso <- function(
    x,
    alpha        = NULL,
    layout       = c("linear", "radial", "grouped"),
    interval     = c("ci", "cr", "both"),
    show_nonsig  = TRUE,
    sort_by      = c("estimate", "significance", "name"),
    n_top        = NULL,
    node_colors  = NULL,
    sig_color    = "#2C6E8A",
    cr_color     = "#D4829A",
    nonsig_color = "#CCCCCC",
    ring_color   = "#C8C8C8",
    median_color = "#AAAAAA",
    label_size   = 2.9,
    label_color  = NULL,
    point_size   = if (match.arg(layout) == "radial") 2 else 3,
    r_inner      = 0.38,
    r_outer      = 0.72,
    gap_rad      = 0.10,
    title        = NULL,
    subtitle     = NULL,
    ...
) {
  layout <- match.arg(layout)
  df     <- .forest_df_boot_glasso(x, alpha)
  grouped_args <- list(
    df           = df,
    interval     = match.arg(interval),
    show_nonsig  = show_nonsig,
    n_top        = n_top,
    node_colors  = node_colors,
    cr_color     = cr_color,
    ring_color   = ring_color,
    median_color = median_color,
    label_size   = label_size,
    label_color  = label_color,
    point_size   = point_size,
    r_inner      = r_inner,
    r_outer      = r_outer,
    gap_rad      = gap_rad,
    title        = title,
    subtitle     = subtitle
  )
  if (layout == "grouped") {
    do.call(.build_grouped_radial_plot, grouped_args)
  } else if (layout == "radial") {
    .build_radial_forest_plot(
      df,
      interval     = match.arg(interval),
      show_nonsig  = show_nonsig,
      n_top        = n_top,
      sig_color    = sig_color,
      cr_color     = cr_color,
      nonsig_color = nonsig_color,
      ring_color   = ring_color,
      median_color = median_color,
      label_size   = label_size,
      label_color  = label_color,
      point_size   = point_size,
      title        = title,
      subtitle     = subtitle
    )
  } else {
    .build_forest_plot(
      df,
      interval     = match.arg(interval),
      show_nonsig  = show_nonsig,
      sort_by      = match.arg(sort_by),
      n_top        = n_top,
      sig_color    = sig_color,
      cr_color     = cr_color,
      nonsig_color = nonsig_color,
      point_size   = point_size,
      title        = title,
      subtitle     = subtitle
    )
  }
}
