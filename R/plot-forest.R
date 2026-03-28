#' @title Forest Plot for Bootstrap Network Results
#' @description
#' A ggplot2-based forest plot for \code{net_bootstrap} and \code{boot_glasso}
#' objects. Each row is one network edge; horizontal bars span the confidence
#' interval and a filled square marks the point estimate. A dashed reference
#' line runs through zero.
#'
#' @name plot_bootstrap_forest
#' @keywords internal
#' @importFrom stats quantile
#' @importFrom utils head
NULL

utils::globalVariables(c(
  "alpha_val", "angle", "cr_col", "edge", "grp", "hjust",
  "lab", "lab_col", "linewidth", "lx", "ly", "name", "node",
  "text_angle", "to", "x_crh", "x_crl", "x_est", "x_hi", "x_lab", "x_lo",
  "y_crh", "y_crl", "y_est", "y_hi", "y_lab", "y_lo"
))

# -- helpers ------------------------------------------------------------------

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

# Extract long-form data frame from a tna_bootstrap object.
# Returns columns: edge, estimate, ci_lower, ci_upper, cr_lower, cr_upper,
#                  p_value, sig, has_cr
.forest_df_tna_bootstrap <- function(x, alpha) {
  alpha <- alpha %||% x$level %||% 0.05

  # tna_bootstrap uses different field names than net_bootstrap
  mean_mat <- x$weights_mean %||% x$weights_orig %||% x$model$weights
  lo_mat   <- x$ci_lower
  hi_mat   <- x$ci_upper
  pv_mat   <- x$p_values
  crl_mat  <- x$cr_lower
  crh_mat  <- x$cr_upper

  if (is.null(mean_mat)) {
    stop("Cannot find weight matrix in bootstrap object", call. = FALSE)
  }

  # TNA networks are always directed
  nms <- rownames(mean_mat) %||% x$model$labels %||%
         as.character(seq_len(nrow(mean_mat)))

  keep <- which(mean_mat != 0, arr.ind = TRUE)
  keep <- keep[keep[, 1] != keep[, 2], , drop = FALSE]
  sep  <- " \u2192 "

  if (nrow(keep) == 0) {
    stop("No non-zero edges found in bootstrap results.", call. = FALSE)
  }

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
# boot_glasso has no consistency range -- cr columns set to NA.
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

# -- core plot builder ---------------------------------------------------------

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

# -- radial forest builder -----------------------------------------------------

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

  # Clockwise from top (pi/2 -> pi/2 - 2*pi), one angle per edge
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

  lim <- 1.85

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
    # Guide spokes: inner ring -> outer ring
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
    # Median ring -- slightly more prominent
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

# -- grouped radial builder ---------------------------------------------------

.build_grouped_radial_plot <- function(
    df,
    interval     = c("ci", "cr", "both"),
    show_nonsig  = TRUE,
    n_top        = NULL,
    node_colors  = NULL,
    cr_color     = "#D4829A",
    ring_color   = "#C8C8C8",
    median_color = "#AAAAAA",
    label_size   = NULL,
    label_color  = NULL,
    point_size   = NULL,
    r_inner      = NULL,
    r_outer      = NULL,
    gap_rad      = NULL,
    label_offset = NULL,
    src_label_size = NULL,
    margins      = c(0.1, 0.1, 0.1, 0.1),
    scale        = 1,
    title        = NULL,
    subtitle     = NULL
) {
  interval <- match.arg(interval)

  has_cr <- isTRUE(df$has_cr[1]) && !all(is.na(df$cr_lower))
  if (interval %in% c("cr", "both") && !has_cr) {
    message("Consistency range not available; showing CI only.")
    interval <- "ci"
  }

  # Parse from / to out of "A -> B" or "A -- B"
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
  n_edges    <- nrow(df)

  # Adaptive sizing: scale labels and geometry for dense plots
  label_size     <- label_size     %||% if (n_edges > 60) 3.2 else if (n_edges > 30) 3.4 else 3.6
  point_size     <- point_size     %||% if (n_edges > 60) 1.0 else 1.5
  r_inner        <- r_inner        %||% if (n_from > 6) 0.10 else 0.12
  r_outer        <- r_outer        %||% 0.14
  gap_rad        <- gap_rad        %||% if (n_from > 6) 0.04 else 0.06
  label_offset   <- label_offset   %||% 0.005
  src_label_size <- src_label_size %||% (label_size * 0.80)

  # Apply scale factor to all visual sizes
  label_size     <- label_size * scale
  src_label_size <- src_label_size * scale
  point_size     <- point_size * scale

  # Node colors: use supplied palette/named vector, or fall back to darkened Okabe-Ito
  oi <- c("#005A8E","#B87D00","#007B5A","#A84A00","#2A91C9","#A35284","#C4B800","#222222","#666666")
  if (is.null(node_colors)) {
    node_col <- setNames(oi[((seq_len(n_from) - 1L) %% length(oi)) + 1L], from_nodes)
  } else if (!is.null(names(node_colors))) {
    # Named vector -- match by node name, fill missing with Okabe-Ito
    node_col <- node_colors[from_nodes]
    missing  <- is.na(node_col)
    if (any(missing))
      node_col[missing] <- oi[((which(missing) - 1L) %% length(oi)) + 1L]
    names(node_col) <- from_nodes
  } else {
    # Unnamed vector -- assign in order, cycling if needed
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
  label_r   <- r_outer + label_offset
  df$x_lab  <- label_r * cos(angles)
  df$y_lab  <- label_r * sin(angles)
  deg       <- angles * 180 / pi
  flip      <- cos(angles) < 0
  df$text_angle <- ifelse(flip, deg + 180, deg)
  df$hjust      <- ifelse(flip, 1, 0)
  df$lab_col    <- if (is.null(label_color)) df$color else label_color

  # Inner (source) labels -- tangential, at sector midpoints inside inner ring
  src_r   <- r_inner * 0.92
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

  # Margins: c(bottom, left, top, right) as fractions of the plot radius
  # Similar to splot margins -- controls whitespace around the radial plot
  mar_b <- margins[1]; mar_l <- margins[2]
  mar_t <- margins[3]; mar_r <- margins[4]

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
    # Target labels -- outer ring, radial
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(x = x_lab, y = y_lab, label = to,
                   angle = text_angle, hjust = hjust,
                   colour = lab_col, alpha = alpha_val),
      size = label_size
    ) +
    # Source labels -- inner ring, tangential, bold
    ggplot2::geom_text(
      data = src_df,
      ggplot2::aes(x = x_lab, y = y_lab, label = node,
                   angle = text_angle, colour = color),
      hjust = 0.5, size = src_label_size, fontface = "bold"
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(
        size = 12 * scale, face = "bold", hjust = 0.5,
        colour = "#1A1A1A", margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9 * scale, hjust = 0.5,
        colour = "#666666", margin = ggplot2::margin(b = 4)
      ),
      plot.margin     = ggplot2::margin(
        t = mar_t * 100, r = mar_r * 100,
        b = mar_b * 100, l = mar_l * 100
      ),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  p
}

# -- exported S3 generics ------------------------------------------------------

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
#' @param x A \code{tna_bootstrap} (from \code{tna::bootstrap}),
#'   \code{net_bootstrap}, or \code{boot_glasso} object.
#' @param alpha Significance threshold. Default: inherits from the object
#'   (\code{$ci_level} or \code{$alpha}), falling back to \code{0.05}.
#' @param interval Which interval to display: \code{"ci"} (bootstrap confidence
#'   interval, default), \code{"cr"} (consistency range, stability inference
#'   only), or \code{"both"} (CI as outer bar, CR as inner bar).
#' @param layout \code{"linear"} (default) draws the classic tall forest plot;
#'   \code{"circular"} arranges each edge as a spoke around a circle, with the
#'   inner ring at the data minimum and the outer ring at the data maximum.
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
#' @param r_inner Inner ring radius (grouped layout). Default \code{NULL} (auto).
#' @param r_outer Outer ring radius (grouped layout). Default \code{NULL} (auto).
#' @param gap_rad Gap in radians between sectors (grouped layout). Default \code{NULL} (auto).
#' @param label_offset Distance between outer ring and labels (grouped layout). Default \code{NULL} (auto).
#' @param src_label_size Text size for source node labels in the center (grouped layout).
#'   Default \code{NULL} (auto, \code{label_size * 0.80}).
#' @param margins Margins as \code{c(bottom, left, top, right)} fractions (grouped layout).
#'   Default \code{c(0.1, 0.1, 0.1, 0.1)}.
#' @param scale Scaling factor applied to all text and point sizes (grouped layout).
#'   Default \code{1}. Use values > 1 for high-DPI output, < 1 for small devices.
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
    layout       = c("linear", "circular", "grouped"),
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
    label_size   = NULL,
    label_color  = NULL,
    point_size   = NULL,
    r_inner      = NULL,
    r_outer      = NULL,
    gap_rad      = NULL,
    label_offset   = NULL,
    src_label_size = NULL,
    margins      = c(0.1, 0.1, 0.1, 0.1),
    scale        = 1,
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

  # Defaults for non-grouped layouts when NULL
  if (layout != "grouped") {
    label_size <- label_size %||% 2.9
    point_size <- point_size %||% if (layout == "circular") 2 else 3
  }

  grouped_args <- list(
    df             = df,
    interval       = match.arg(interval),
    show_nonsig    = show_nonsig,
    n_top          = n_top,
    node_colors    = node_colors,
    cr_color       = cr_color,
    ring_color     = ring_color,
    median_color   = median_color,
    label_size     = label_size,
    label_color    = label_color,
    point_size     = point_size,
    r_inner        = r_inner,
    r_outer        = r_outer,
    gap_rad        = gap_rad,
    label_offset   = label_offset,
    src_label_size = src_label_size,
    margins        = margins,
    scale          = scale,
    title          = title,
    subtitle       = subtitle
  )
  if (layout == "grouped") {
    do.call(.build_grouped_radial_plot, grouped_args)
  } else if (layout == "circular") {
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
#' @method plot_bootstrap_forest tna_bootstrap
#' @export
plot_bootstrap_forest.tna_bootstrap <- function(
    x,
    alpha        = NULL,
    layout       = c("linear", "circular", "grouped"),
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
    label_size   = NULL,
    label_color  = NULL,
    point_size   = NULL,
    r_inner      = NULL,
    r_outer      = NULL,
    gap_rad      = NULL,
    label_offset   = NULL,
    src_label_size = NULL,
    margins      = c(0.1, 0.1, 0.1, 0.1),
    scale        = 1,
    title        = NULL,
    subtitle     = NULL,
    ...
) {
  layout <- match.arg(layout)
  df     <- .forest_df_tna_bootstrap(x, alpha)

  # Auto-read node colors from the tna model if not supplied
  if (is.null(node_colors) && layout == "grouped") {
    if (!is.null(x$model$colors)) {
      nms         <- x$model$labels %||% rownames(x$weights_mean) %||%
                     as.character(seq_along(x$model$colors))
      node_colors <- setNames(x$model$colors, nms)
    }
  }

  # Defaults for non-grouped layouts when NULL
  if (layout != "grouped") {
    label_size <- label_size %||% 2.9
    point_size <- point_size %||% if (layout == "circular") 2 else 3
  }

  grouped_args <- list(
    df             = df,
    interval       = match.arg(interval),
    show_nonsig    = show_nonsig,
    n_top          = n_top,
    node_colors    = node_colors,
    cr_color       = cr_color,
    ring_color     = ring_color,
    median_color   = median_color,
    label_size     = label_size,
    label_color    = label_color,
    point_size     = point_size,
    r_inner        = r_inner,
    r_outer        = r_outer,
    gap_rad        = gap_rad,
    label_offset   = label_offset,
    src_label_size = src_label_size,
    margins        = margins,
    scale          = scale,
    title          = title,
    subtitle       = subtitle
  )
  if (layout == "grouped") {
    do.call(.build_grouped_radial_plot, grouped_args)
  } else if (layout == "circular") {
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
    layout       = c("linear", "circular", "grouped"),
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
    label_size   = NULL,
    label_color  = NULL,
    point_size   = NULL,
    r_inner      = NULL,
    r_outer      = NULL,
    gap_rad      = NULL,
    label_offset   = NULL,
    src_label_size = NULL,
    margins      = c(0.1, 0.1, 0.1, 0.1),
    scale        = 1,
    title        = NULL,
    subtitle     = NULL,
    ...
) {
  layout <- match.arg(layout)
  df     <- .forest_df_boot_glasso(x, alpha)
  # Defaults for non-grouped layouts when NULL
  if (layout != "grouped") {
    label_size <- label_size %||% 2.9
    point_size <- point_size %||% if (layout == "circular") 2 else 3
  }

  grouped_args <- list(
    df             = df,
    interval       = match.arg(interval),
    show_nonsig    = show_nonsig,
    n_top          = n_top,
    node_colors    = node_colors,
    cr_color       = cr_color,
    ring_color     = ring_color,
    median_color   = median_color,
    label_size     = label_size,
    label_color    = label_color,
    point_size     = point_size,
    r_inner        = r_inner,
    r_outer        = r_outer,
    gap_rad        = gap_rad,
    label_offset   = label_offset,
    src_label_size = src_label_size,
    margins        = margins,
    scale          = scale,
    title          = title,
    subtitle       = subtitle
  )
  if (layout == "grouped") {
    do.call(.build_grouped_radial_plot, grouped_args)
  } else if (layout == "circular") {
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
plot_bootstrap_forest.net_bootstrap_group <- function(
    x,
    layout       = c("linear", "circular"),
    interval     = c("ci", "cr", "both"),
    show_nonsig  = TRUE,
    n_top        = NULL,
    all_edges    = FALSE,
    pos_color    = NULL,
    title        = NULL,
    subtitle     = NULL,
    label_size   = 2.8,
    ...
) {
  layout   <- match.arg(layout)
  interval <- match.arg(interval)

  # Circular multi-group: fall back to first group
  if (layout == "circular") {
    sub_note <- paste0(
      subtitle %||% "",
      if (!is.null(subtitle)) "  |  " else "",
      "Note: circular layout shows first group only"
    )
    return(plot_bootstrap_forest(x[[1L]], layout = "circular",
                                 interval = interval,
                                 show_nonsig = show_nonsig,
                                 n_top = n_top,
                                 title = title,
                                 subtitle = sub_note,
                                 label_size = label_size,
                                 ...))
  }

  # Extract per-group data frames
  grp_names <- names(x)
  if (is.null(grp_names)) grp_names <- paste0("Group", seq_along(x))

  df_list <- lapply(x, function(boot) .forest_df_net_bootstrap(boot, alpha = NULL))
  names(df_list) <- grp_names

  # Determine edge universe
  edge_sets <- lapply(df_list, function(d) d$edge)
  if (all_edges) {
    edge_universe <- Reduce(union, edge_sets)
  } else {
    edge_universe <- Reduce(intersect, edge_sets)
  }
  if (length(edge_universe) == 0L)
    stop("No edges in common across groups. Use all_edges = TRUE to show the union.",
         call. = FALSE)

  # Filter / align each group df to the edge universe
  df_list <- lapply(df_list, function(d) {
    d <- d[d$edge %in% edge_universe, , drop = FALSE]
    # Fill missing rows with NA (for union case)
    missing_edges <- setdiff(edge_universe, d$edge)
    if (length(missing_edges) > 0L) {
      na_rows <- data.frame(
        edge     = missing_edges,
        estimate = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        cr_lower = NA_real_,
        cr_upper = NA_real_,
        p_value  = NA_real_,
        sig      = FALSE,
        has_cr   = FALSE,
        stringsAsFactors = FALSE
      )
      d <- rbind(d, na_rows)
    }
    d
  })

  # Combine with group column
  combined <- do.call(rbind, Map(function(d, nm) {
    d$group <- nm
    d
  }, df_list, grp_names))

  # Sort edges by mean estimate across groups (ascending)
  mean_by_edge <- tapply(combined$estimate, combined$edge, mean, na.rm = TRUE)
  edge_order   <- names(sort(mean_by_edge))
  combined$edge <- factor(combined$edge, levels = edge_order)

  if (!show_nonsig) {
    # Keep an edge if it is significant in at least one group
    sig_edges <- unique(combined$edge[!is.na(combined$sig) & combined$sig])
    combined  <- combined[combined$edge %in% sig_edges, , drop = FALSE]
    if (nrow(combined) == 0L)
      stop("No significant edges. Use show_nonsig = TRUE.", call. = FALSE)
  }

  if (!is.null(n_top)) {
    abs_mean  <- abs(mean_by_edge[levels(combined$edge)])
    top_edges <- names(sort(abs_mean, decreasing = TRUE))[seq_len(min(n_top, length(abs_mean)))]
    combined  <- combined[combined$edge %in% top_edges, , drop = FALSE]
    # Rebuild factor levels in sorted order
    remaining_means <- mean_by_edge[top_edges]
    combined$edge   <- factor(as.character(combined$edge),
                               levels = names(sort(remaining_means)))
  }

  # Okabe-Ito darkened palette for groups
  oi        <- c("#005A8E","#B87D00","#007B5A","#A84A00","#2A91C9","#A35284",
                 "#C4B800","#222222","#666666")
  grp_colors <- setNames(oi[seq_along(x)], grp_names)

  combined$grp_col  <- grp_colors[combined$group]
  combined$group_f  <- factor(combined$group, levels = grp_names)

  # Determine which CI columns to use
  ci_lo_col <- switch(interval,
    ci   = "ci_lower",
    cr   = "cr_lower",
    both = "ci_lower"
  )
  ci_hi_col <- switch(interval,
    ci   = "ci_upper",
    cr   = "cr_upper",
    both = "ci_upper"
  )

  p <- ggplot2::ggplot(combined,
         ggplot2::aes(x = .data$estimate, y = .data$edge,
                      colour = .data$group_f,
                      group  = .data$group_f)) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed",
      colour = "#555555", linewidth = 0.45, alpha = 0.6
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = .data[[ci_lo_col]],
        xmax = .data[[ci_hi_col]]
      ),
      height    = 0.28,
      linewidth = 0.65,
      position  = ggplot2::position_dodge(0.6),
      na.rm     = TRUE
    ) +
    ggplot2::geom_point(
      shape    = 15,
      size     = 3,
      position = ggplot2::position_dodge(0.6),
      na.rm    = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = grp_colors,
      name   = "Group"
    ) +
    ggplot2::labs(
      x        = "Edge Weight (Bootstrap Estimate)",
      y        = NULL,
      title    = title,
      subtitle = subtitle,
      caption  = "Squares: point estimates  |  Bars: bootstrap CI  |  Colour: group"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        colour    = "#EBEBEB",
        linewidth = 0.4
      ),
      axis.text.y   = ggplot2::element_text(
        size   = 9,
        colour = "#333333",
        margin = ggplot2::margin(r = 4)
      ),
      axis.text.x   = ggplot2::element_text(size = 8.5, colour = "#555555"),
      axis.title.x  = ggplot2::element_text(
        size   = 9, colour = "#555555",
        margin = ggplot2::margin(t = 6)
      ),
      legend.position = "top",
      legend.title    = ggplot2::element_text(size = 9, face = "bold"),
      legend.text     = ggplot2::element_text(size = 9),
      plot.title    = ggplot2::element_text(
        size   = 12, face = "bold", colour = "#1A1A1A",
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size   = 9.5, colour = "#666666",
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption  = ggplot2::element_text(
        size   = 7.5, colour = "#888888",
        hjust  = 0, margin = ggplot2::margin(t = 8)
      ),
      plot.margin      = ggplot2::margin(12, 16, 8, 12),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  p
}

# -- edge difference forest ----------------------------------------------------

# Compute pairwise bootstrap difference data frame from a boot_glasso object.
.forest_df_edge_diff <- function(x, alpha, nonzero_only = FALSE) {
  alpha    <- alpha %||% x$alpha %||% 0.05
  boot_mat <- x$boot_edges
  p_mat    <- x$edge_diff_p
  if (is.null(boot_mat))
    stop("No boot_edges \u2014 re-run boot_glasso().", call. = FALSE)
  if (is.null(p_mat))
    stop("No edge_diff_p \u2014 re-run boot_glasso().", call. = FALSE)

  # Optionally restrict to edges present in the original network
  if (nonzero_only) {
    orig <- x$original_pcor
    if (!is.null(orig)) {
      orig_upper <- orig[upper.tri(orig)]
      nz_names   <- colnames(boot_mat)[orig_upper != 0]
    } else {
      # Fallback: edges with non-trivial mean bootstrap weight
      edge_means <- colMeans(boot_mat, na.rm = TRUE)
      threshold  <- max(abs(edge_means)) * 0.10
      nz_names   <- colnames(boot_mat)[abs(edge_means) >= threshold]
    }
    if (length(nz_names) == 0L) stop("No non-zero edges found.", call. = FALSE)
    keep     <- match(nz_names, colnames(boot_mat))
    boot_mat <- boot_mat[, keep, drop = FALSE]
    p_mat    <- p_mat[keep, keep, drop = FALSE]
  }

  edge_nms <- colnames(boot_mat)
  pairs    <- which(upper.tri(p_mat), arr.ind = TRUE)
  if (nrow(pairs) == 0L) stop("No edge pairs found.", call. = FALSE)

  diff_mat <- boot_mat[, pairs[, 1L], drop = FALSE] -
              boot_mat[, pairs[, 2L], drop = FALSE]

  data.frame(
    edge     = paste0(edge_nms[pairs[, 1L]], " vs ", edge_nms[pairs[, 2L]]),
    edge1    = edge_nms[pairs[, 1L]],
    edge2    = edge_nms[pairs[, 2L]],
    estimate = colMeans(diff_mat, na.rm = TRUE),
    ci_lower = apply(diff_mat, 2L, quantile, alpha / 2,       na.rm = TRUE),
    ci_upper = apply(diff_mat, 2L, quantile, 1 - alpha / 2,   na.rm = TRUE),
    p_value  = p_mat[pairs],
    sig      = p_mat[pairs] < alpha,
    stringsAsFactors = FALSE
  )
}

# Linear forest for edge differences
.build_edge_diff_linear <- function(
    df,
    show_nonsig  = FALSE,
    sort_by      = c("estimate", "significance", "name"),
    n_top        = NULL,
    pos_color    = "#C0392B",
    neg_color    = "#2C6E8A",
    nonsig_color = "#AAAAAA",
    point_size   = 3,
    title        = NULL,
    subtitle     = NULL
) {
  sort_by <- match.arg(sort_by)
  if (!show_nonsig) df <- df[df$sig, , drop = FALSE]
  if (nrow(df) == 0L)
    stop("No significant edge differences. Use show_nonsig = TRUE.", call. = FALSE)

  df <- switch(sort_by,
    estimate     = df[order(df$estimate), ],
    significance = df[order(df$p_value, decreasing = TRUE), ],
    name         = df[order(df$edge), ]
  )
  if (!is.null(n_top)) {
    tmp <- df[order(abs(df$estimate), decreasing = TRUE), ]
    df  <- tmp[seq_len(min(n_top, nrow(tmp))), ]
    df  <- df[order(df$estimate), ]
  }

  df$edge  <- factor(df$edge, levels = df$edge)
  df$color <- ifelse(!df$sig, nonsig_color,
                     ifelse(df$estimate > 0, pos_color, neg_color))
  df$alpha <- ifelse(df$sig, 1, 0.4)
  df$stars <- .p_stars(df$p_value)

  xr     <- range(c(df$ci_lower, df$ci_upper, 0), na.rm = TRUE)
  x_pad  <- diff(xr) * 0.18
  x_star <- xr[2] + diff(xr) * 0.04
  x_lim  <- c(xr[1] - x_pad * 0.4, xr[2] + x_pad)

  caption <- paste0(
    "[=] = mean bootstrap difference  |  Bars: 95% bootstrap CI  |  ",
    "Red: edge_1 > edge_2  |  Blue: edge_1 < edge_2  |  ",
    "* p<0.05  ** p<0.01  *** p<0.001"
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$edge)) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed",
      colour = "#444444", linewidth = 0.55, alpha = 0.7
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$ci_lower, xmax = .data$ci_upper,
                   colour = .data$color, alpha = .data$alpha),
      height = 0.28, linewidth = 0.65
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$color, alpha = .data$alpha),
      shape = 15, size = point_size
    ) +
    ggplot2::geom_text(
      data = df[nchar(df$stars) > 0L, , drop = FALSE],
      ggplot2::aes(x = x_star, label = .data$stars),
      hjust = 0, size = 3.2, colour = "#333333", fontface = "bold"
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_x_continuous(limits = x_lim, expand = c(0, 0)) +
    ggplot2::labs(
      x        = "Bootstrap Weight Difference (edge_1 - edge_2)",
      y        = NULL,
      title    = title,
      subtitle = subtitle,
      caption  = caption
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#EBEBEB", linewidth = 0.4),
      axis.text.y  = ggplot2::element_text(size = 9, colour = "#333333",
                                            margin = ggplot2::margin(r = 4)),
      axis.text.x  = ggplot2::element_text(size = 8.5, colour = "#555555"),
      axis.title.x = ggplot2::element_text(size = 9, colour = "#555555",
                                            margin = ggplot2::margin(t = 6)),
      plot.title    = ggplot2::element_text(size = 12, face = "bold",
                                             colour = "#1A1A1A",
                                             margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 9.5, colour = "#666666",
                                             margin = ggplot2::margin(b = 8)),
      plot.caption  = ggplot2::element_text(size = 7.5, colour = "#888888",
                                             hjust = 0,
                                             margin = ggplot2::margin(t = 8)),
      plot.margin      = ggplot2::margin(12, 16, 8, 12),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  p
}

# Circular forest for edge differences -- dashed reference ring at 0
.build_edge_diff_circular <- function(
    df,
    show_nonsig  = FALSE,
    n_top        = NULL,
    pos_color    = "#C0392B",
    neg_color    = "#2C6E8A",
    nonsig_color = "#AAAAAA",
    ring_color   = "#C8C8C8",
    label_size   = 2.3,
    label_color  = NULL,
    point_size   = 2,
    r_inner      = 0.38,
    r_outer      = 0.72,
    title        = NULL,
    subtitle     = NULL
) {
  if (!show_nonsig) df <- df[df$sig, , drop = FALSE]
  if (nrow(df) == 0L)
    stop("No significant edge differences. Use show_nonsig = TRUE.", call. = FALSE)

  df <- df[order(df$edge), ]
  if (!is.null(n_top)) {
    keep <- order(abs(df$estimate), decreasing = TRUE)[seq_len(min(n_top, nrow(df)))]
    df   <- df[sort(keep), ]
  }

  n <- nrow(df)
  df$color     <- ifelse(!df$sig, nonsig_color,
                          ifelse(df$estimate > 0, pos_color, neg_color))
  df$alpha_val <- ifelse(df$sig, 1, 0.4)

  angles   <- seq(pi / 2, pi / 2 - 2 * pi, length.out = n + 1L)[seq_len(n)]
  df$angle <- angles

  # Scale always includes 0
  v_min  <- min(c(df$ci_lower, 0), na.rm = TRUE)
  v_max  <- max(c(df$ci_upper, 0), na.rm = TRUE)
  v_pad  <- (v_max - v_min) * 0.05
  v_min  <- v_min - v_pad
  v_max  <- v_max + v_pad
  to_r   <- function(v) r_inner + pmin(pmax((v - v_min) / (v_max - v_min), 0), 1) * (r_outer - r_inner)
  r_zero <- to_r(0)

  df$x_est <- to_r(df$estimate) * cos(angles)
  df$y_est <- to_r(df$estimate) * sin(angles)
  df$x_lo  <- to_r(df$ci_lower) * cos(angles)
  df$y_lo  <- to_r(df$ci_lower) * sin(angles)
  df$x_hi  <- to_r(df$ci_upper) * cos(angles)
  df$y_hi  <- to_r(df$ci_upper) * sin(angles)

  label_r       <- r_outer + 0.06
  df$x_lab      <- label_r * cos(angles)
  df$y_lab      <- label_r * sin(angles)
  deg           <- angles * 180 / pi
  flip          <- cos(angles) < 0
  df$text_angle <- ifelse(flip, deg + 180, deg)
  df$hjust      <- ifelse(flip, 1, 0)
  df$lab_col    <- if (is.null(label_color)) df$color else label_color

  theta_seq  <- seq(0, 2 * pi, length.out = 300)
  ring_inner <- data.frame(x = r_inner * cos(theta_seq), y = r_inner * sin(theta_seq))
  ring_outer <- data.frame(x = r_outer * cos(theta_seq), y = r_outer * sin(theta_seq))
  ring_zero  <- data.frame(x = r_zero  * cos(theta_seq), y = r_zero  * sin(theta_seq))

  lim <- 1.52

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = ring_outer, ggplot2::aes(x = x, y = y),
                       colour = ring_color, linewidth = 0.25) +
    ggplot2::geom_path(data = ring_inner, ggplot2::aes(x = x, y = y),
                       colour = ring_color, linewidth = 0.25) +
    ggplot2::geom_path(data = ring_zero, ggplot2::aes(x = x, y = y),
                       colour = "#555555", linewidth = 0.5, linetype = "dashed") +
    ggplot2::geom_segment(
      data = df,
      ggplot2::aes(x = r_inner * cos(angle), y = r_inner * sin(angle),
                   xend = r_outer * cos(angle), yend = r_outer * sin(angle),
                   colour = color, alpha = I(alpha_val * 0.12)),
      linewidth = 0.3
    ) +
    ggplot2::geom_segment(
      data = df,
      ggplot2::aes(x = x_lo, y = y_lo, xend = x_hi, yend = y_hi,
                   colour = color, alpha = alpha_val),
      linewidth = 0.7, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = x_est, y = y_est, colour = color, alpha = alpha_val),
      shape = 15, size = point_size * 0.45
    ) +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(x = x_lab, y = y_lab, label = edge,
                   angle = text_angle, hjust = hjust,
                   colour = lab_col, alpha = alpha_val),
      size = label_size
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(clip = "off", xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5,
                                             colour = "#1A1A1A",
                                             margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 9, hjust = 0.5,
                                             colour = "#666666",
                                             margin = ggplot2::margin(b = 8)),
      plot.margin     = ggplot2::margin(20, 40, 20, 40),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  p
}

#' Forest Plot for Bootstrap Edge Differences
#'
#' Visualises pairwise edge weight differences from a \code{boot_glasso} object.
#' Each row (linear) or spoke (circular) is one edge pair; the CI bar spans the
#' bootstrap CI of the difference; a dashed line/ring marks zero.
#' Red = first edge larger; blue = second edge larger.
#'
#' @param x A \code{boot_glasso} object with \code{$boot_edges} and
#'   \code{$edge_diff_p}.
#' @param alpha Significance threshold. Default: inherits from object.
#' @param layout \code{"linear"} (default), \code{"circular"}, or
#'   \code{"chord"}.  The chord layout places all edge names on a unit circle
#'   and connects significant pairs with bezier arcs; arc width and colour
#'   encode the mean bootstrap difference.
#' @param show_nonsig Include non-significant pairs? Default \code{FALSE}.
#' @param nonzero_only If \code{TRUE}, restrict to edges that are non-zero in
#'   the original network (identified via \code{$original_pcor}). Useful for
#'   EBICglasso results where many edges are regularised to exactly zero.
#'   Default \code{FALSE}.
#' @param sort_by \code{"estimate"} (default), \code{"significance"}, or
#'   \code{"name"} (linear only).
#' @param n_top Restrict to top N pairs by absolute difference.
#' @param pos_color Colour when edge1 > edge2. Default crimson.
#' @param neg_color Colour when edge1 < edge2. Default teal.
#' @param nonsig_color Colour for non-significant pairs.
#' @param ring_color Ring colour (circular/chord). Default light grey.
#' @param label_size Text size. Default \code{2.3}.
#' @param label_color Fixed label colour (\code{NULL} = inherit).
#' @param point_size Size of estimate square (linear/circular).
#' @param r_inner Inner ring radius (circular). Default \code{0.38}.
#' @param r_outer Outer ring radius (circular). Default \code{0.72}.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param ... Currently unused.
#'
#' @return A \code{ggplot} object.
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE)
#' set.seed(1)
#' data1 <- as.data.frame(matrix(rnorm(60), 20, 3, dimnames = list(NULL, c("A","B","C"))))
#' bg <- Nestimate::boot_glasso(data1, iter = 50, centrality = c("strength", "expected_influence"))
#' plot_edge_diff_forest(bg)
#' @export
plot_edge_diff_forest <- function(x, ...) UseMethod("plot_edge_diff_forest")

#' @rdname plot_edge_diff_forest
#' @export
plot_edge_diff_forest.boot_glasso <- function(
    x,
    alpha        = NULL,
    layout       = c("linear", "circular", "chord", "tile"),
    show_nonsig  = FALSE,
    nonzero_only = FALSE,
    sort_by      = c("estimate", "significance", "name"),
    n_top        = NULL,
    pos_color    = "#C0392B",
    neg_color    = "#2C6E8A",
    nonsig_color = "#AAAAAA",
    ring_color   = "#C8C8C8",
    label_size   = 2.3,
    label_color  = NULL,
    point_size   = if (match.arg(layout) == "circular") 2 else 3,
    r_inner      = 0.38,
    r_outer      = 0.72,
    title        = NULL,
    subtitle     = NULL,
    ...
) {
  layout <- match.arg(layout)

  if (layout == "tile") {
    return(.build_edge_diff_tile(x,
      pos_color = pos_color, neg_color = neg_color,
      title = title, subtitle = subtitle))
  }

  df <- .forest_df_edge_diff(x, alpha, nonzero_only = nonzero_only)

  if (layout == "chord") {
    .build_edge_diff_chord(
      df,
      show_nonsig  = show_nonsig,
      n_top        = n_top,
      pos_color    = pos_color,
      neg_color    = neg_color,
      nonsig_color = nonsig_color,
      ring_color   = ring_color,
      label_size   = label_size,
      label_color  = label_color,
      title        = title,
      subtitle     = subtitle
    )
  } else if (layout == "circular") {
    .build_edge_diff_circular(
      df,
      show_nonsig  = show_nonsig,
      n_top        = n_top,
      pos_color    = pos_color,
      neg_color    = neg_color,
      nonsig_color = nonsig_color,
      ring_color   = ring_color,
      label_size   = label_size,
      label_color  = label_color,
      point_size   = point_size,
      r_inner      = r_inner,
      r_outer      = r_outer,
      title        = title,
      subtitle     = subtitle
    )
  } else {
    .build_edge_diff_linear(
      df,
      show_nonsig  = show_nonsig,
      sort_by      = match.arg(sort_by),
      n_top        = n_top,
      pos_color    = pos_color,
      neg_color    = neg_color,
      nonsig_color = nonsig_color,
      point_size   = point_size,
      title        = title,
      subtitle     = subtitle
    )
  }
}

# ---- Chord arc diagram for edge differences ----------------------------------

.build_edge_diff_chord <- function(
    df,
    show_nonsig  = FALSE,
    n_top        = NULL,
    pos_color    = "#C0392B",
    neg_color    = "#2C6E8A",
    nonsig_color = "#AAAAAA",
    ring_color   = "#C8C8C8",
    label_size   = 2.5,
    label_color  = NULL,
    title        = NULL,
    subtitle     = NULL
) {
  # --- keep all edges for ring, filter only display arcs ---------------------
  all_edges <- sort(unique(c(df$edge1, df$edge2)))
  N         <- length(all_edges)

  df_arcs <- if (!show_nonsig) df[df$sig, , drop = FALSE] else df
  if (!is.null(n_top)) {
    df_arcs <- df_arcs[order(abs(df_arcs$estimate), decreasing = TRUE), , drop = FALSE]
    df_arcs <- head(df_arcs, n_top)
  }
  if (nrow(df_arcs) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = title, subtitle = "No pairs to display.") +
        ggplot2::theme_void()
    )
  }

  # --- per-node degree (# significant connections) and total |estimate| -----
  sig_df      <- df[df$sig, , drop = FALSE]
  node_degree <- vapply(all_edges, function(e)
    sum(sig_df$edge1 == e | sig_df$edge2 == e), integer(1L))
  node_total  <- vapply(all_edges, function(e)
    sum(abs(df$estimate[df$edge1 == e | df$edge2 == e])), numeric(1L))
  if (sum(node_total) == 0) node_total[] <- 1

  # --- angular layout: arc width ~ total |estimate|, gaps between nodes -----
  gap_each  <- 0.08                              # radians gap per node
  total_gap <- gap_each * N
  total_arc <- 2 * pi - total_gap
  arc_w     <- node_total / sum(node_total) * total_arc

  # Clockwise from top: start angles decrease
  starts <- numeric(N)
  starts[1L] <- pi / 2
  for (i in seq(2L, N)) starts[i] <- starts[i - 1L] - arc_w[i - 1L] - gap_each
  ends <- starts - arc_w
  mids <- (starts + ends) / 2

  # --- thick arc segment polygons -------------------------------------------
  r_in  <- 0.86
  r_out <- 1.00
  n_arc <- 60L

  # Node colour by degree (light -> dark blue)
  max_deg  <- max(node_degree, 1L)
  deg_ramp <- grDevices::colorRamp(c("#AED6F1", "#1A5276"))
  node_col <- vapply(node_degree, function(d) {
    t <- d / max_deg
    m <- deg_ramp(t)
    grDevices::rgb(m[1L], m[2L], m[3L], maxColorValue = 255)
  }, character(1L))

  seg_list <- lapply(seq_len(N), function(i) {
    theta <- seq(starts[i], ends[i], length.out = n_arc)
    xs    <- c(r_in  * cos(theta), rev(r_out * cos(theta)))
    ys    <- c(r_in  * sin(theta), rev(r_out * sin(theta)))
    data.frame(x = xs, y = ys, node = i, fill = node_col[i],
               stringsAsFactors = FALSE)
  })
  seg_df <- do.call(rbind, seg_list)

  # --- colour ramp for ribbons (neg -> white -> pos) --------------------------
  diff_max <- max(abs(df_arcs$estimate), na.rm = TRUE)
  if (diff_max == 0) diff_max <- 1
  cramp    <- grDevices::colorRamp(c(neg_color, "#FFFFFF", pos_color))

  arc_col_fn <- function(est) {
    t   <- pmin(pmax((est / diff_max + 1) / 2, 0), 1)
    m   <- cramp(t)
    grDevices::rgb(m[, 1L], m[, 2L], m[, 3L], maxColorValue = 255)
  }

  # --- bezier ribbons from midpoint of each node segment --------------------
  n_pts <- 100L
  t_seq <- seq(0, 1, length.out = n_pts)

  arc_list <- lapply(seq_len(nrow(df_arcs)), function(i) {
    row  <- df_arcs[i, , drop = FALSE]
    idx1 <- match(row$edge1, all_edges)
    idx2 <- match(row$edge2, all_edges)
    x0   <- r_in * cos(mids[idx1]);  y0 <- r_in * sin(mids[idx1])
    x2   <- r_in * cos(mids[idx2]);  y2 <- r_in * sin(mids[idx2])
    bx   <- (1 - t_seq)^2 * x0 + t_seq^2 * x2
    by   <- (1 - t_seq)^2 * y0 + t_seq^2 * y2
    col   <- if (row$sig) arc_col_fn(row$estimate) else nonsig_color
    lwd   <- if (row$sig) 0.3 + 3.5 * abs(row$estimate) / diff_max else 0.3
    alpha <- if (row$sig) 0.80 else 0.18
    data.frame(x = bx, y = by, group = i,
               color = col, linewidth = lwd, alpha = alpha,
               stringsAsFactors = FALSE)
  })
  arc_df <- do.call(rbind, arc_list)

  # --- labels outside ring --------------------------------------------------
  label_r <- 1.14
  lab_df  <- data.frame(
    name        = all_edges,
    lx          = label_r * cos(mids),
    ly          = label_r * sin(mids),
    text_angle  = ifelse(cos(mids) < -0.01, mids * 180 / pi + 180, mids * 180 / pi),
    hjust       = ifelse(cos(mids) < -0.01, 1, 0),
    stringsAsFactors = FALSE
  )

  # --- legend gradient bar --------------------------------------------------
  leg_n  <- 200L
  leg_df <- data.frame(
    x     = seq(-0.55, 0.55, length.out = leg_n),
    y     = -1.44,
    color = arc_col_fn(seq(-diff_max, diff_max, length.out = leg_n))
  )

  lim <- 1.58

  # --- plot: arcs first (bottom), segments on top ---------------------------
  p <- ggplot2::ggplot() +
    # Ribbons (drawn under node segments)
    ggplot2::geom_path(
      data = arc_df,
      ggplot2::aes(x = x, y = y, group = group,
                   colour = color, alpha = alpha, linewidth = linewidth)
    ) +
    # Node arc segments (thick, coloured by degree)
    ggplot2::geom_polygon(
      data = seg_df,
      ggplot2::aes(x = x, y = y, group = node, fill = fill),
      colour = "white", linewidth = 0.3
    ) +
    # Labels
    ggplot2::geom_text(
      data = lab_df,
      ggplot2::aes(x = lx, y = ly, label = name,
                   angle = text_angle, hjust = hjust),
      colour = if (!is.null(label_color)) label_color else "#2A2A2A",
      size   = label_size
    ) +
    # Legend gradient
    ggplot2::geom_tile(
      data = leg_df,
      ggplot2::aes(x = x, y = y, fill = color),
      height = 0.045, width = 1.1 / leg_n + 0.001
    ) +
    ggplot2::annotate("text", x = -0.62, y = -1.44,
      label = sprintf("-%.2f", diff_max),
      hjust = 1, size = 2.5, colour = neg_color) +
    ggplot2::annotate("text", x =  0.62, y = -1.44,
      label = sprintf("+%.2f", diff_max),
      hjust = 0, size = 2.5, colour = pos_color) +
    ggplot2::annotate("text", x = 0, y = -1.52,
      label = "Mean edge difference",
      hjust = 0.5, size = 2.3, colour = "#666666") +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_linewidth_identity() +
    ggplot2::coord_equal(clip = "off",
                         xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(
        size = 12, face = "bold", hjust = 0.5, colour = "#1A1A1A",
        margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(
        size = 9, hjust = 0.5, colour = "#666666",
        margin = ggplot2::margin(b = 8)),
      plot.margin     = ggplot2::margin(20, 40, 20, 40),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  p
}

# ---- Tile heatmap for edge differences ---------------------------------------

.build_edge_diff_tile <- function(
    x,
    order     = c("sample", "id"),
    pos_color = "#C0392B",
    neg_color = "#2C6E8A",
    title     = NULL,
    subtitle  = NULL
) {
  if (is.null(x$edge_diff_p))
    stop("No edge_diff_p \u2014 re-run boot_glasso().", call. = FALSE)

  order      <- match.arg(order)
  p_mat      <- x$edge_diff_p
  alpha      <- x$alpha %||% 0.05
  boot_mat   <- x$boot_edges
  edge_names <- colnames(p_mat)
  n_e        <- length(edge_names)

  edge_means    <- colMeans(boot_mat, na.rm = TRUE)
  mean_diff_mat <- outer(edge_means, edge_means, `-`)
  rownames(mean_diff_mat) <- colnames(mean_diff_mat) <- edge_names

  edge_weights <- x$edge_ci$weight
  names(edge_weights) <- x$edge_ci$edge

  ordered_names <- if (order == "sample")
    edge_names[order(abs(edge_weights[edge_names]))] else sort(edge_names)

  grid     <- expand.grid(edge1 = ordered_names, edge2 = ordered_names,
                           stringsAsFactors = FALSE)
  idx1     <- match(grid$edge1, ordered_names)
  idx2     <- match(grid$edge2, ordered_names)
  is_upper <- idx1 < idx2
  is_diag  <- idx1 == idx2

  grid$p_value   <- p_mat[cbind(grid$edge1, grid$edge2)]
  grid$mean_diff <- mean_diff_mat[cbind(grid$edge1, grid$edge2)]
  grid$sig       <- grid$p_value < alpha

  grid$fill_val              <- ifelse(is_upper | is_diag, grid$mean_diff, NA_real_)
  grid$fill_val[is_diag]     <- 0
  grid$alpha_val             <- 0
  grid$alpha_val[is_upper &  grid$sig] <- 1
  grid$alpha_val[is_upper & !grid$sig] <- 0.08
  grid$alpha_val[is_diag]              <- 0.06

  grid$stars <- ""
  grid$stars[is_upper & grid$sig] <- vapply(
    grid$p_value[is_upper & grid$sig],
    function(p) if (p < 0.001) "***" else if (p < 0.01) "**" else "*",
    character(1L)
  )
  grid$label <- ifelse(is_diag, sprintf("%.2f", edge_weights[grid$edge1]), "")

  grid$edge1 <- factor(grid$edge1, levels = ordered_names)
  grid$edge2 <- factor(grid$edge2, levels = rev(ordered_names))

  label_size <- if (n_e <= 10) 3.2 else if (n_e <= 20) 2.5 else 1.8
  star_size  <- label_size * 0.80

  ut      <- which(upper.tri(p_mat), arr.ind = TRUE)
  n_sig   <- sum(p_mat[ut] < alpha)
  n_pairs <- nrow(ut)
  auto_sub <- sprintf(
    "%d of %d pairs significantly different (p < %s)  |  Red: row > col  |  Blue: col > row",
    n_sig, n_pairs, alpha
  )

  diff_max <- max(abs(grid$fill_val), na.rm = TRUE)
  if (diff_max == 0) diff_max <- 0.1

  ggplot2::ggplot(grid, ggplot2::aes(x = .data$edge1, y = .data$edge2)) +
    ggplot2::geom_tile(fill = "white", colour = "#F0F0F0", linewidth = 0.25) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = .data$fill_val, alpha = .data$alpha_val),
      colour = "white", linewidth = 0.25
    ) +
    ggplot2::geom_text(
      data = grid[is_upper & grid$sig, ],
      ggplot2::aes(label = .data$stars),
      size = star_size, colour = "white", fontface = "bold"
    ) +
    ggplot2::geom_text(
      data = grid[is_diag, ],
      ggplot2::aes(label = .data$label),
      size = label_size * 0.88, colour = "#444444"
    ) +
    ggplot2::scale_fill_gradient2(
      low = neg_color, high = pos_color, mid = "white", midpoint = 0,
      limits = c(-diff_max, diff_max), na.value = "transparent",
      name = "Mean\ndifference"
    ) +
    ggplot2::scale_alpha_identity() +
    ggplot2::labs(
      title    = title %||% "Bootstrap Edge Difference Test",
      subtitle = subtitle %||% auto_sub,
      x = NULL, y = NULL
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(
        angle = 45, hjust = 1, vjust = 1,
        size = label_size * 2.8, colour = "#333333"),
      axis.text.y      = ggplot2::element_text(
        size = label_size * 2.8, colour = "#333333"),
      plot.title       = ggplot2::element_text(
        size = 13, face = "bold", colour = "#1A1A1A",
        margin = ggplot2::margin(b = 4)),
      plot.subtitle    = ggplot2::element_text(
        size = 9, colour = "#666666",
        margin = ggplot2::margin(b = 10)),
      legend.title     = ggplot2::element_text(size = 8.5, colour = "#444444"),
      legend.text      = ggplot2::element_text(size = 8, colour = "#555555"),
      legend.key.width = ggplot2::unit(0.5, "cm"),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin      = ggplot2::margin(12, 12, 12, 12)
    )
}
