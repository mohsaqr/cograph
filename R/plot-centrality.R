# =============================================================================
# Centrality visualization -- line, bar, lollipop, pyramid, heatmap
# =============================================================================

utils::globalVariables(c(
  "node", "measure", "value", "z_value", "rank", "cluster",
  "side", "group_label", "freq", "val_label", "highlight",
  "xmin", "xmax", "ymin", "ymax", "pval", "pval_label",
  "plot_value", "x1", "x2", "x_key", "y_key", "text_x",
  "yidx", "text_label"
))


# --- internal helpers --------------------------------------------------------

.centrality_df <- function(x, measures, ...) {
  # Accept a centrality() data frame directly, or a network input.
  if (is.data.frame(x) && "node" %in% names(x)) {
    df <- x
    if (!is.null(measures)) {
      missing_cols <- setdiff(measures, names(df))
      if (length(missing_cols)) {
        stop("Measures not in supplied data frame: ",
             paste(missing_cols, collapse = ", "), call. = FALSE)
      }
      df <- df[, c("node", measures), drop = FALSE]
    }
    return(df)
  }
  if (is.null(measures)) measures <- c("degree", "strength", "betweenness",
                                       "closeness", "eigenvector")
  centrality(x, measures = measures, ...)
}

.centrality_long <- function(df) {
  meas <- setdiff(names(df), c("node", "cluster"))
  out <- data.frame(
    node = rep(df$node, length(meas)),
    measure = rep(meas, each = nrow(df)),
    value = unlist(df[, meas, drop = FALSE], use.names = FALSE),
    stringsAsFactors = FALSE
  )
  if ("cluster" %in% names(df)) {
    out$cluster <- rep(df$cluster, length(meas))
  }
  out$measure <- factor(out$measure, levels = meas)
  out
}

.zscore_by_measure <- function(long) {
  mean_by <- stats::aggregate(value ~ measure, data = long, FUN = mean,
                               na.rm = TRUE)
  sd_by <- stats::aggregate(value ~ measure, data = long, FUN = stats::sd,
                             na.rm = TRUE)
  names(mean_by)[2] <- "m"; names(sd_by)[2] <- "s"
  long <- merge(long, mean_by, by = "measure", sort = FALSE)
  long <- merge(long, sd_by, by = "measure", sort = FALSE)
  long$z_value <- ifelse(long$s > 0, (long$value - long$m) / long$s, 0)
  long$m <- long$s <- NULL
  long
}

.normalize_by_measure <- function(long) {
  # Scale each measure to [0, 1]: (x - min) / (max - min). Zero-range
  # columns map to 0.5 rather than NaN so the plot stays interpretable.
  min_by <- stats::aggregate(value ~ measure, data = long, FUN = min,
                              na.rm = TRUE)
  max_by <- stats::aggregate(value ~ measure, data = long, FUN = max,
                              na.rm = TRUE)
  names(min_by)[2] <- "mn"; names(max_by)[2] <- "mx"
  long <- merge(long, min_by, by = "measure", sort = FALSE)
  long <- merge(long, max_by, by = "measure", sort = FALSE)
  rng <- long$mx - long$mn
  long$norm_value <- ifelse(rng > 0, (long$value - long$mn) / rng, 0.5)
  long$mn <- long$mx <- NULL
  long
}

# Extract per-node colors from network-like inputs. Returns a named
# character vector keyed by node name, or NULL if no colors are present.
.extract_node_colors <- function(x) {
  if (is.character(x)) return(NULL)  # palette name passed, not a graph

  # CographNetwork R6
  if (inherits(x, "R6") && isTRUE(try(is.function(x$get_nodes), silent = TRUE))) {
    nodes <- try(x$get_nodes(), silent = TRUE)
    if (!inherits(nodes, "try-error") && is.data.frame(nodes)) {
      name_col <- intersect(c("name", "label", "id"), names(nodes))[1]
      fill_col <- intersect(c("node_fill", "fill", "color", "col"),
                            names(nodes))[1]
      if (!is.na(name_col) && !is.na(fill_col)) {
        return(stats::setNames(as.character(nodes[[fill_col]]),
                               as.character(nodes[[name_col]])))
      }
    }
  }

  # S3 cograph_network
  if (inherits(x, "cograph_network")) {
    nodes <- x$nodes
    if (is.data.frame(nodes)) {
      name_col <- intersect(c("name", "label", "id"), names(nodes))[1]
      fill_col <- intersect(c("node_fill", "fill", "color", "col"),
                            names(nodes))[1]
      if (!is.na(name_col) && !is.na(fill_col)) {
        return(stats::setNames(as.character(nodes[[fill_col]]),
                               as.character(nodes[[name_col]])))
      }
    }
  }

  # igraph
  if (inherits(x, "igraph")) {
    col <- igraph::vertex_attr(x, "color")
    nm  <- igraph::vertex_attr(x, "name")
    if (!is.null(col) && !is.null(nm)) {
      return(stats::setNames(as.character(col), as.character(nm)))
    }
  }

  NULL
}

# Given user input for node colors (NULL | named vector | unnamed vector |
# palette name), a set of nodes, and an optional source network, return
# a named character vector keyed by node name.
.resolve_node_colors <- function(node_colors, nodes, source = NULL) {
  if (is.null(node_colors)) {
    from_net <- .extract_node_colors(source)
    if (!is.null(from_net)) {
      hit <- from_net[nodes]
      if (all(!is.na(hit))) return(stats::setNames(unname(hit), nodes))
    }
    # Fallback: cycle cograph palette
    return(stats::setNames(.pick_palette(length(nodes)), nodes))
  }

  if (is.character(node_colors) && length(node_colors) == 1L) {
    # A single palette name
    pal <- switch(node_colors,
                  "cograph" = .pick_palette(length(nodes)),
                  "okabe"   = rep(c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                                    "#0072B2", "#D55E00", "#CC79A7", "#000000"),
                                  length.out = length(nodes)),
                  "viridis" = if (requireNamespace("viridisLite", quietly = TRUE))
                                viridisLite::viridis(length(nodes))
                              else .pick_palette(length(nodes)),
                  rep(node_colors, length.out = length(nodes)))
    return(stats::setNames(pal, nodes))
  }

  if (!is.null(names(node_colors))) {
    hit <- node_colors[nodes]
    missing <- is.na(hit)
    if (any(missing)) {
      fill <- .pick_palette(sum(missing))
      hit[missing] <- fill
    }
    return(stats::setNames(unname(hit), nodes))
  }

  # Unnamed vector: apply by position
  stats::setNames(rep(node_colors, length.out = length(nodes)), nodes)
}

.pretty_measure <- function(s) {
  # "expected_influence_1" -> "Expected Influence 1"
  words <- strsplit(s, "_")
  vapply(words, function(w) {
    paste(toupper(substring(w, 1, 1)), substring(w, 2), sep = "",
          collapse = " ")
  }, character(1))
}

# Modern palette for the centrality family. Teal-gold split used across
# the rest of cograph's defaults; falls back to Okabe-Ito when there are
# more groups than this palette has colors.
.cograph_centrality_palette <- c(
  "#4FC3F7",  # sky (matches plot_htna default)
  "#FBB550",  # warm gold
  "#66BB6A",  # leaf
  "#AB47BC",  # amethyst
  "#EF5350",  # coral
  "#26A69A",  # deep teal
  "#FFCA28",  # mustard
  "#5C6BC0"   # indigo
)

.pick_palette <- function(n) {
  base <- .cograph_centrality_palette
  if (n <= length(base)) return(base[seq_len(n)])
  # Extend with Okabe-Ito (colorblind-friendly)
  okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
             "#D55E00", "#CC79A7", "#000000")
  rep(c(base, okabe), length.out = n)
}

.theme_centrality <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x  = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor    = ggplot2::element_blank(),
      panel.grid.major.y  = ggplot2::element_blank(),
      axis.ticks          = ggplot2::element_blank(),
      axis.text           = ggplot2::element_text(color = "grey25"),
      axis.title          = ggplot2::element_text(color = "grey25"),
      strip.text          = ggplot2::element_text(face = "bold", color = "grey15",
                                                  hjust = 0, size = base_size),
      strip.background    = ggplot2::element_blank(),
      plot.title          = ggplot2::element_text(face = "bold",
                                                  color = "grey15",
                                                  size = base_size + 2),
      plot.subtitle       = ggplot2::element_text(color = "grey45",
                                                  size = base_size - 1),
      legend.position     = "right",
      plot.margin         = ggplot2::margin(10, 12, 10, 10)
    )
}


# ============================================================================
# plot_centrality -- single-network visualization
# ============================================================================

#' Plot Centrality
#'
#' Publication-quality visualization of one or more centrality measures.
#' Accepts the data frame from \code{\link{centrality}} directly or any
#' network input.
#'
#' Four styles are available:
#' \describe{
#'   \item{\code{"line"}}{Faceted line view with one panel per measure.
#'     Nodes are ordered along the requested orientation and connected within
#'     each measure.}
#'   \item{\code{"bar"}}{Horizontal bars, one facet per measure. Best for
#'     reading individual measure values.}
#'   \item{\code{"lollipop"}}{Like \code{"bar"} but with a dot at the tip.
#'     Softer visual weight; useful on dense grids.}
#'   \item{\code{"dot"}}{Dot-only variant of the lollipop style.}
#' }
#'
#' @param x Output of \code{\link{centrality}}, or any network input
#'   (matrix, igraph, cograph_network, tna, netobject).
#' @param measures Character vector of measure names. Default pulls the
#'   classical five (degree, strength, betweenness, closeness, eigenvector)
#'   when \code{x} is a network; default \code{NULL} keeps all columns
#'   when \code{x} is already a centrality data frame.
#' @param style Character: "line" (default), "bar", "lollipop", or "dot".
#' @param orientation Character: "horizontal" (default, nodes on y-axis) or
#'   "vertical" (nodes on x-axis).
#' @param scale Character: "raw" (default, native units; in the "line"
#'   style this forces free y-axis per measure via faceting), "normalized" (\[0, 1\] 
#'   within measure), "z" (standardized within measure), or "rank"
#'   (1..n, highest value = 1).
#' @param order_by Character. For "bar"/"lollipop": which measure sorts
#'   nodes. Defaults to the first measure. Use \code{"alpha"} for
#'   alphabetical. For "line", this also controls node ordering unless
#'   \code{"alpha"} is requested.
#' @param top_n Optional integer to keep only the top-N nodes (by
#'   \code{order_by}). Useful for large graphs.
#' @param highlight Optional integer: highlight the top-N bars/lines per
#'   measure in full color; mute the rest. Default 0 (no highlighting).
#' @param cluster Optional named vector or data-frame column mapping each
#'   node to a cluster/community. Colors nodes by cluster when supplied.
#' @param palette Character or vector. \code{"cograph"} (default) uses
#'   cograph's teal-gold-leaf palette; \code{"okabe"} uses Okabe-Ito;
#'   \code{"viridis"} uses viridis; or supply a character vector of colors.
#' @param ncol For faceted styles ("bar", "lollipop"): number of columns.
#'   Default \code{NULL} chooses sensibly based on measure count.
#' @param title Plot title. Default NULL.
#' @param subtitle Plot subtitle. Default NULL.
#' @param ... Passed to \code{\link{centrality}} when \code{x} is a network.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0,0, 1,0,1,1,0, 1,1,0,1,1, 0,1,1,0,1, 0,0,1,1,0),
#'               5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' plot_centrality(adj)
#' plot_centrality(adj, style = "bar", highlight = 2)
plot_centrality <- function(x,
                            measures = NULL,
                            style = c("line", "bar", "lollipop", "dot"),
                            orientation = c("horizontal", "vertical"),
                            scale = c("raw", "normalized", "z", "rank"),
                            order_by = NULL,
                            top_n = NULL,
                            highlight = 0L,
                            cluster = NULL,
                            palette = "cograph",
                            ncol = NULL,
                            title = NULL,
                            subtitle = NULL,
                            ...) {
  style <- match.arg(style)
  orientation <- match.arg(orientation)
  scale <- match.arg(scale)

  df <- .centrality_df(x, measures, ...)
  if ("cluster" %in% names(df) && is.null(cluster)) {
    # already present
  } else if (!is.null(cluster)) {
    if (is.character(cluster) && length(cluster) == 1 && cluster %in% names(df)) {
      names(df)[names(df) == cluster] <- "cluster"
    } else if (is.atomic(cluster) && length(cluster) == nrow(df)) {
      df$cluster <- cluster
    } else if (is.atomic(cluster) && !is.null(names(cluster))) {
      df$cluster <- unname(cluster[df$node])
    }
  }

  meas <- setdiff(names(df), c("node", "cluster"))
  if (length(meas) == 0L) {
    stop("No centrality columns found. Did you pass an empty data frame?",
         call. = FALSE)
  }

  # Apply top_n before factoring nodes so the rank is honored.
  if (!is.null(top_n)) {
    sort_col <- if (is.null(order_by) || identical(order_by, "alpha")) meas[1] else order_by
    ord <- order(-df[[sort_col]])
    df <- df[ord[seq_len(min(top_n, nrow(df)))], , drop = FALSE]
  }

  long <- .centrality_long(df)

  # Apply scaling transform
  if (scale == "z") {
    long <- .zscore_by_measure(long)
    long$plot_value <- long$z_value
    value_label <- "z-score"
  } else if (scale == "normalized") {
    long <- .normalize_by_measure(long)
    long$plot_value <- long$norm_value
    value_label <- "normalized"
  } else if (scale == "rank") {
    long$plot_value <- stats::ave(-long$value, long$measure,
                           FUN = function(v) rank(v, ties.method = "min"))
    value_label <- "rank"
  } else {
    long$plot_value <- long$value
    value_label <- "raw"
  }

  # Pretty measure labels for facets/legend
  long$measure <- factor(.pretty_measure(as.character(long$measure)),
                         levels = .pretty_measure(meas))

  if (style == "line") {
    return(.plot_centrality_line_facet(long, df, meas, orientation, scale,
                                       value_label, order_by, highlight,
                                       ncol, title, subtitle))
  }
  .plot_centrality_bars(long, df, meas, style, orientation, scale,
                        value_label, order_by, palette, highlight, ncol,
                        title, subtitle)
}


# qgraph-style faceted line: one facet per measure, within each facet a
# line connects points in node order. Horizontal orientation (default):
# nodes on y-axis, value on x-axis. Vertical: nodes on x-axis, value on y.
.plot_centrality_line_facet <- function(long, df, meas, orientation, scale,
                                        value_label, order_by, highlight,
                                        ncol, title, subtitle) {
  # Determine node sort order
  if (is.null(order_by) || identical(order_by, meas[1])) {
    order_col <- meas[1]
  } else if (identical(order_by, "alpha")) {
    order_col <- NA
  } else if (order_by %in% meas) {
    order_col <- order_by
  } else {
    order_col <- meas[1]
  }
  node_order <- if (is.na(order_col)) sort(df$node)
                else df$node[order(df[[order_col]])]
  long$node <- factor(long$node, levels = node_order)

  if (is.null(ncol)) {
    n_meas <- length(meas)
    ncol <- if (n_meas <= 3) n_meas else if (n_meas <= 8) 3 else 4
  }
  facet_scales <- if (scale == "raw") {
    if (orientation == "horizontal") "free_x" else "free_y"
  } else "fixed"

  accent <- .cograph_centrality_palette[1]

  x_title <- NULL; y_title <- NULL
  value_title <- switch(value_label,
                        "z-score"    = "Centrality (z-score)",
                        "normalized" = "Centrality (0-1)",
                        "rank"       = "Rank (1 = highest)",
                        "Centrality")

  if (orientation == "horizontal") {
    p <- ggplot2::ggplot(long, ggplot2::aes(x = plot_value, y = node,
                                            group = 1)) +
      ggplot2::geom_line(color = accent, linewidth = 0.9, alpha = 0.9,
                         orientation = "y") +
      ggplot2::geom_point(color = accent, size = 2.8)
    x_title <- value_title
  } else {
    # Vertical: nodes on x-axis
    p <- ggplot2::ggplot(long, ggplot2::aes(x = node, y = plot_value,
                                            group = 1)) +
      ggplot2::geom_line(color = accent, linewidth = 0.9, alpha = 0.9) +
      ggplot2::geom_point(color = accent, size = 2.8)
    y_title <- value_title
  }

  p <- p + ggplot2::facet_wrap(~ measure, ncol = ncol, scales = facet_scales) +
    ggplot2::labs(x = x_title, y = y_title, title = title,
                  subtitle = subtitle) +
    .theme_centrality()

  if (orientation == "horizontal") {
    p + ggplot2::theme(
      axis.text.y = ggplot2::element_text(family = "mono", size = 9,
                                           color = "grey20"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92",
                                                  linewidth = 0.3)
    )
  } else {
    p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9,
                                           color = "grey20"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92",
                                                  linewidth = 0.3)
    )
  }
}


# Legacy overlay (parallel-coordinates across all nodes with one line
# per measure) retained for internal use; not the default any more.
.plot_centrality_line <- function(long, value_label, scale, palette,
                                  highlight, title, subtitle) {
  # Nodes on x, plot_value on y. When a shared scale is meaningful
  # (normalized / z / rank) we overlay all measures as separate colored
  # lines; when raw, we facet per measure so free y-scales keep each
  # measure readable.
  long$node <- factor(long$node, levels = sort(unique(long$node)))
  n_meas <- length(levels(long$measure))

  cols <- if (is.character(palette) && length(palette) == 1L) {
    switch(palette,
           "cograph" = .pick_palette(n_meas),
           "okabe"   = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                          "#0072B2", "#D55E00", "#CC79A7", "#000000")[seq_len(n_meas)],
           "viridis" = if (requireNamespace("viridisLite", quietly = TRUE))
                        viridisLite::viridis(n_meas)
                       else .pick_palette(n_meas),
           .pick_palette(n_meas))
  } else {
    rep(palette, length.out = n_meas)
  }

  y_title <- switch(value_label,
                    "z-score"    = "Centrality (z-score)",
                    "normalized" = "Centrality (0-1)",
                    "rank"       = "Rank (1 = highest)",
                    "Centrality")

  if (scale == "raw") {
    # Facet per measure; single color (each panel is one measure already)
    p <- ggplot2::ggplot(long, ggplot2::aes(x = node, y = plot_value,
                                            group = 1)) +
      ggplot2::geom_line(color = .cograph_centrality_palette[1],
                         linewidth = 0.9, alpha = 0.9) +
      ggplot2::geom_point(color = .cograph_centrality_palette[1],
                          size = 2.4, alpha = 0.95) +
      ggplot2::facet_wrap(~ measure, scales = "free_y",
                          ncol = if (n_meas <= 3) n_meas else 3) +
      ggplot2::labs(x = NULL, y = y_title, title = title,
                    subtitle = subtitle) +
      .theme_centrality() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1,
                                             color = "grey25")
      )
    return(p)
  }

  # Overlay (shared y because scales are comparable)
  p <- ggplot2::ggplot(long, ggplot2::aes(x = node, y = plot_value,
                                          color = measure, group = measure)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey85", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.85) +
    ggplot2::geom_point(size = 2.6, alpha = 0.95) +
    ggplot2::scale_color_manual(values = cols, name = NULL) +
    ggplot2::labs(x = NULL, y = y_title, title = title, subtitle = subtitle) +
    .theme_centrality() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1,
                                           color = "grey25"),
      legend.position = "top",
      legend.justification = "left"
    )

  if (identical(value_label, "rank")) {
    p <- p + ggplot2::scale_y_reverse()
  }
  p
}


.plot_centrality_bars <- function(long, df, meas, style, orientation, scale,
                                  value_label, order_by, palette, highlight,
                                  ncol, title, subtitle) {
  # Decide sort column
  if (is.null(order_by) || identical(order_by, meas[1])) {
    order_col <- meas[1]
  } else if (identical(order_by, "alpha")) {
    order_col <- NA
  } else {
    if (!order_by %in% meas) {
      stop("order_by must be 'alpha' or one of: ",
           paste(meas, collapse = ", "), call. = FALSE)
    }
    order_col <- order_by
  }

  node_order <- if (is.na(order_col)) sort(df$node)
                else df$node[order(df[[order_col]])]
  long$node <- factor(long$node, levels = node_order)

  # Highlight styling
  if (isTRUE(highlight > 0)) {
    top_by_meas <- do.call(rbind, lapply(split(long, long$measure), function(d) {
      d[order(-d$plot_value)[seq_len(min(highlight, nrow(d)))], , drop = FALSE]
    }))
    long$highlight <- paste(long$node, long$measure) %in%
                      paste(top_by_meas$node, top_by_meas$measure)
  } else {
    long$highlight <- TRUE
  }

  # Color: cluster if present, else single accent
  has_cluster <- "cluster" %in% names(long)
  base_color <- .cograph_centrality_palette[1]
  if (has_cluster) {
    n_clu <- length(unique(long$cluster))
    cols <- .pick_palette(n_clu)
    color_aes <- ggplot2::aes(fill = factor(cluster))
  } else {
    color_aes <- NULL
  }

  # Compute ncol default
  if (is.null(ncol)) {
    n_meas <- length(meas)
    ncol <- if (n_meas <= 3) n_meas else if (n_meas <= 8) 3 else 4
  }

  # Build plot. orientation "horizontal" puts nodes on y-axis (the classic
  # Cleveland layout); "vertical" puts nodes on x-axis (qgraph-style).
  horiz <- identical(orientation, "horizontal")

  # Free scales when scale = "raw" -- free the value axis, not the node axis
  facet_scales <- if (scale == "raw") {
    if (horiz) "free_x" else "free_y"
  } else "fixed"

  if (style == "bar") {
    base_aes <- if (horiz) ggplot2::aes(x = plot_value, y = node)
                else       ggplot2::aes(x = node, y = plot_value)
    bar_layer <- if (has_cluster) {
      ggplot2::geom_col(ggplot2::aes(fill = factor(cluster), alpha = highlight),
                       width = 0.72, color = NA)
    } else {
      ggplot2::geom_col(ggplot2::aes(alpha = highlight),
                       width = 0.72, color = NA, fill = base_color)
    }
    p <- ggplot2::ggplot(long, base_aes) + bar_layer
  } else if (style == "lollipop") {
    if (horiz) {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = plot_value, y = node)) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, xend = plot_value,
                                           yend = node, alpha = highlight),
                              color = "grey70", linewidth = 0.55)
    } else {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = node, y = plot_value)) +
        ggplot2::geom_segment(ggplot2::aes(x = node, xend = node,
                                           y = 0, yend = plot_value,
                                           alpha = highlight),
                              color = "grey70", linewidth = 0.55)
    }
    if (has_cluster) {
      p <- p + ggplot2::geom_point(ggplot2::aes(color = factor(cluster),
                                                alpha = highlight),
                                    size = 3.2)
    } else {
      p <- p + ggplot2::geom_point(ggplot2::aes(alpha = highlight),
                                    size = 3.2, color = base_color)
    }
  } else {
    # dot: no segment
    base_aes <- if (horiz) ggplot2::aes(x = plot_value, y = node)
                else       ggplot2::aes(x = node, y = plot_value)
    p <- ggplot2::ggplot(long, base_aes)
    if (has_cluster) {
      p <- p + ggplot2::geom_point(ggplot2::aes(color = factor(cluster),
                                                alpha = highlight),
                                    size = 3.6)
    } else {
      p <- p + ggplot2::geom_point(ggplot2::aes(alpha = highlight),
                                    size = 3.6, color = base_color)
    }
  }

  p <- p +
    ggplot2::facet_wrap(~ measure, ncol = ncol, scales = facet_scales) +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35),
                                guide = "none")

  if (has_cluster) {
    p <- p + ggplot2::scale_fill_manual(values = cols, name = "Cluster") +
      ggplot2::scale_color_manual(values = cols, name = "Cluster")
  }

  value_axis_title <- switch(value_label,
                              "z-score"    = "Centrality (z-score)",
                              "normalized" = "Centrality (0-1)",
                              "rank"       = "Rank (1 = highest)",
                              "Centrality")
  if (identical(value_label, "z-score")) {
    p <- p + if (horiz) ggplot2::geom_vline(xintercept = 0, color = "grey85",
                                            linewidth = 0.4)
             else       ggplot2::geom_hline(yintercept = 0, color = "grey85",
                                            linewidth = 0.4)
  }

  p <- if (horiz) {
    p + ggplot2::labs(x = value_axis_title, y = NULL, title = title,
                      subtitle = subtitle)
  } else {
    p + ggplot2::labs(x = NULL, y = value_axis_title, title = title,
                      subtitle = subtitle)
  }

  p <- p + .theme_centrality()

  if (horiz) {
    p + ggplot2::theme(
      axis.text.y = ggplot2::element_text(family = "mono", size = 9,
                                           color = "grey20"),
      panel.grid.major.x = ggplot2::element_line(color = "grey92",
                                                  linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_blank()
    )
  } else {
    p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 9,
                                           color = "grey20"),
      panel.grid.major.y = ggplot2::element_line(color = "grey92",
                                                  linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank()
    )
  }
}


# ============================================================================
# plot_centrality_compare -- back-to-back pyramid for two networks
# ============================================================================

#' Plot Centrality Comparison
#'
#' Compare a centrality measure across two or more groups using stacked,
#' faceted, grouped, dumbbell, line, or two-group pyramid layouts. The
#' \code{"pyramid"} style is a back-to-back horizontal bar chart for exactly
#' two groups.
#'
#' @param ... Two or more centrality data frames (from
#'   \code{\link{centrality}}) or network inputs. Names are used as
#'   group labels when \code{group_labels} is NULL.
#' @param measure Character, a single centrality measure to compare. If
#'   NULL, the first shared measure is used.
#' @param style Character: \code{"stacked"} (default), \code{"facet"},
#'   \code{"grouped"}, \code{"dumbbell"}, \code{"line"}, or
#'   \code{"pyramid"} (2 groups only).
#' @param group_labels Character vector with one label per group. Default
#'   \code{c("Group 1", "Group 2", ...)}.
#' @param group_colors Character vector of colors, one per group.
#'   Default cycles through the cograph palette.
#' @param node_colors Optional. Either a named character vector mapping
#'   node name to color, an unnamed vector of colors applied in node
#'   order, or the name of a palette (\code{"cograph"}, \code{"okabe"},
#'   \code{"viridis"}). Used by \code{style = "facet"}.
#' @param sort_by \code{"max"} (default) ranks nodes by highest value
#'   across groups; \code{"delta"} by range; \code{"first"} by first
#'   group; \code{"alpha"} alphabetically.
#' @param top_n Show top N nodes (by \code{sort_by}). Default: all.
#' @param scale \code{"raw"} (default, native values on each side) or
#'   \code{"normalized"} (\[0, 1\] within each side before plotting).
#' @param show_values Logical. Print the value inside each bar.
#'   Default TRUE.
#' @param size_by_value Logical. For \code{"dumbbell"} style, scale dot
#'   size by centrality value. Default FALSE.
#' @param size_range Numeric vector of length 2 giving the min and max
#'   dot size (mm) when \code{size_by_value = TRUE}. Default
#'   \code{c(2, 9)}.
#' @param orientation Character: \code{"horizontal"} (default, nodes on
#'   y-axis) or \code{"vertical"} (nodes on x-axis).
#' @param ncol Number of facet columns for \code{style = "facet"}.
#'   Default NULL chooses automatically.
#' @param title Plot title.
#' @param subtitle Plot subtitle. Auto-generated when NULL.
#' @param centrality_args Named list of additional arguments passed to
#'   \code{\link{centrality}} when inputs are networks.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' set.seed(1)
#' m1 <- matrix(runif(25), 5, 5); diag(m1) <- 0
#' m2 <- matrix(runif(25), 5, 5); diag(m2) <- 0
#' rownames(m1) <- colnames(m1) <- LETTERS[1:5]
#' rownames(m2) <- colnames(m2) <- LETTERS[1:5]
#' plot_centrality_compare(m1, m2, measure = "strength",
#'                         group_labels = c("Pre", "Post"))
plot_centrality_compare <- function(...,
                                    measure = NULL,
                                    style = c("stacked", "facet", "grouped",
                                              "dumbbell", "line", "pyramid"),
                                    group_labels = NULL,
                                    group_colors = NULL,
                                    node_colors = NULL,
                                    sort_by = c("max", "delta", "first", "alpha"),
                                    top_n = NULL,
                                    scale = c("raw", "normalized"),
                                    show_values = TRUE,
                                    size_by_value = FALSE,
                                    size_range = c(2, 9),
                                    orientation = c("horizontal", "vertical"),
                                    ncol = NULL,
                                    title = NULL,
                                    subtitle = NULL,
                                    centrality_args = list()) {
  style <- match.arg(style)
  scale <- match.arg(scale)
  sort_by <- match.arg(sort_by)
  orientation <- match.arg(orientation)

  # Collect networks from ... -- each unnamed arg is a network; names (if any)
  # become the group labels.
  nets <- list(...)
  if (length(nets) < 2L) {
    stop("plot_centrality_compare needs at least 2 networks/data frames.",
         call. = FALSE)
  }
  if (style == "pyramid" && length(nets) != 2L) {
    stop("style = 'pyramid' is for exactly 2 groups; got ",
         length(nets),
         ". Use style = 'stacked', 'grouped', 'dumbbell', or 'facet' for 3+ groups.",
         call. = FALSE)
  }

  supplied_names <- names(nets)
  if (is.null(group_labels)) {
    group_labels <- if (is.null(supplied_names) || any(supplied_names == "")) {
      paste("Group", seq_along(nets))
    } else {
      supplied_names
    }
  }
  stopifnot(length(group_labels) == length(nets))

  # Colors: default to cograph palette cycling through n groups
  if (is.null(group_colors)) {
    group_colors <- .pick_palette(length(nets))
  }
  stopifnot(length(group_colors) == length(nets))
  names(group_colors) <- group_labels

  # Resolve per-group centrality data frames
  dfs <- lapply(nets, function(n) {
    do.call(.centrality_df,
            c(list(x = n, measures = measure), centrality_args))
  })

  # Resolve measure against shared columns in all frames
  shared_measures <- Reduce(intersect,
                            lapply(dfs,
                                   function(d) setdiff(names(d),
                                                       c("node", "cluster"))))
  if (length(shared_measures) == 0L) {
    stop("No shared centrality measures across the supplied inputs.",
         call. = FALSE)
  }
  if (is.null(measure)) {
    measure <- shared_measures[1]
  } else if (!measure %in% shared_measures) {
    candidate <- grep(paste0("^", measure, "(_all|_in|_out)?$"),
                      shared_measures, value = TRUE)
    if (length(candidate) == 1L) {
      measure <- candidate
    } else if (length(candidate) > 1L) {
      stop("Ambiguous measure '", measure, "'. Matches: ",
           paste(candidate, collapse = ", "), call. = FALSE)
    } else {
      stop("Measure '", measure, "' not in all inputs. Shared: ",
           paste(shared_measures, collapse = ", "), call. = FALSE)
    }
  }

  # Align on shared node names (intersection across all groups)
  shared_nodes <- Reduce(intersect, lapply(dfs, function(d) d$node))
  if (length(shared_nodes) == 0L) {
    stop("Inputs share no node names -- cannot align.", call. = FALSE)
  }

  # Build long-form data: node x group x value
  long_list <- lapply(seq_along(dfs), function(i) {
    d <- dfs[[i]]
    d <- d[match(shared_nodes, d$node), , drop = FALSE]
    data.frame(
      node  = shared_nodes,
      group = factor(group_labels[i], levels = group_labels),
      value = d[[measure]],
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, long_list)

  if (scale == "normalized") {
    long <- do.call(rbind, lapply(split(long, long$group), function(d) {
      r <- range(d$value, na.rm = TRUE)
      d$value <- if (diff(r) > 0) (d$value - r[1]) / diff(r) else 0.5
      d
    }))
  }

  # Node ordering -- computed on the long data so it applies across groups
  wide_vals <- stats::reshape(
    long, direction = "wide", idvar = "node", timevar = "group",
    v.names = "value"
  )
  val_cols <- grep("^value\\.", names(wide_vals), value = TRUE)

  ord <- switch(
    sort_by,
    "max"   = order(-apply(wide_vals[, val_cols, drop = FALSE], 1,
                           max, na.rm = TRUE)),
    "delta" = order(-(apply(wide_vals[, val_cols, drop = FALSE], 1,
                            max, na.rm = TRUE) -
                      apply(wide_vals[, val_cols, drop = FALSE], 1,
                            min, na.rm = TRUE))),
    "first" = order(-wide_vals[, val_cols[1]]),
    "alpha" = order(wide_vals$node)
  )
  node_order <- wide_vals$node[ord]
  if (!is.null(top_n)) node_order <- node_order[seq_len(min(top_n,
                                                            length(node_order)))]
  long <- long[long$node %in% node_order, , drop = FALSE]
  long$node <- factor(long$node, levels = rev(node_order))

  if (is.null(title)) {
    title <- sprintf("Centrality comparison: %s", .pretty_measure(measure))
  }

  value_axis_title <- switch(scale,
                              "normalized" = sprintf("%s (0-1)",
                                                      .pretty_measure(measure)),
                              .pretty_measure(measure))

  # Resolve per-node colors for the facet style -- colors encode node
  # identity and are consistent across panels.
  resolved_node_colors <- NULL
  if (style == "facet") {
    resolved_node_colors <- .resolve_node_colors(
      node_colors,
      nodes = as.character(node_order),
      source = nets[[1]]
    )
  }

  switch(
    style,
    "facet"    = .cc_facet(long, resolved_node_colors, value_axis_title,
                           orientation, show_values, ncol, title, subtitle),
    "grouped"  = .cc_grouped(long, group_colors, value_axis_title,
                             orientation, show_values, title, subtitle),
    "stacked"  = .cc_stacked(long, group_colors, value_axis_title,
                             orientation, show_values, title, subtitle),
    "dumbbell" = .cc_dumbbell_n(long, group_colors, value_axis_title,
                                orientation, show_values, size_by_value,
                                size_range, title, subtitle),
    "line"     = .cc_line(long, group_colors, value_axis_title,
                          orientation, show_values, title, subtitle),
    "pyramid"  = .cc_pyramid_dispatch(long, node_order, group_labels,
                                      group_colors, value_axis_title,
                                      show_values, title, subtitle)
  )
}


# ---- compare style: facet (one panel per group, node-colored bars) --------
# Each panel shows all nodes as horizontal bars, independently sorted by
# value within the panel. Colors encode node identity (consistent across
# panels) so the reader can track "which node moved where" across groups.
.cc_facet <- function(long, node_colors, axis_title, orientation,
                      show_values, ncol, title, subtitle) {
  horiz <- identical(orientation, "horizontal")

  # Per-facet node ordering: within each group, rank nodes high-to-low so
  # the highest-value node appears at the TOP of the panel (reference-image
  # convention). ggplot2 maps the first factor level to the BOTTOM of a
  # discrete axis, so we sort ASCENDING here -- then the factor level at
  # the top is the highest-value node. An axis label formatter strips
  # the "___group" suffix at render time.
  long <- long[order(long$group, long$value), , drop = FALSE]
  long$y_key <- paste0(long$node, "___", long$group)
  long$y_key <- factor(long$y_key, levels = unique(long$y_key))

  if (isTRUE(show_values)) {
    long$label <- sprintf("%.3g", long$value)
  }

  strip_group <- function(x) sub("___.*$", "", x)

  # Fallback node colors if auto-resolve returned NULL (shouldn't happen --
  # .resolve_node_colors always returns something -- but defensive).
  if (is.null(node_colors)) {
    uniq <- unique(as.character(long$node))
    node_colors <- stats::setNames(.pick_palette(length(uniq)), uniq)
  }

  if (horiz) {
    p <- ggplot2::ggplot(long,
                         ggplot2::aes(x = value, y = y_key, fill = node)) +
      ggplot2::geom_col(width = 0.75, color = NA) +
      ggplot2::scale_y_discrete(labels = strip_group) +
      ggplot2::facet_wrap(~ group, scales = "free_y", ncol = ncol) +
      ggplot2::labs(x = axis_title, y = NULL, title = title,
                    subtitle = subtitle)

    if (isTRUE(show_values)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = label),
        hjust = -0.15, size = 2.6, color = "grey30"
      )
    }
  } else {
    # Vertical: put the group factor on the x-axis, value on y.
    long$x_key <- long$y_key
    p <- ggplot2::ggplot(long,
                         ggplot2::aes(x = x_key, y = value, fill = node)) +
      ggplot2::geom_col(width = 0.75, color = NA) +
      ggplot2::scale_x_discrete(labels = strip_group) +
      ggplot2::facet_wrap(~ group, scales = "free_x", ncol = ncol) +
      ggplot2::labs(x = NULL, y = axis_title, title = title,
                    subtitle = subtitle)

    if (isTRUE(show_values)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = label),
        vjust = -0.6, size = 2.6, color = "grey30"
      )
    }
  }

  p <- p +
    ggplot2::scale_fill_manual(values = node_colors, guide = "none") +
    .theme_centrality(base_size = 12) +
    ggplot2::theme(
      strip.text          = ggplot2::element_text(face = "bold",
                                                  color = "grey15",
                                                  hjust = 0.5, size = 13),
      strip.background    = ggplot2::element_blank(),
      panel.grid.minor    = ggplot2::element_blank(),
      plot.title          = ggplot2::element_text(face = "bold", size = 14,
                                                  color = "grey15"),
      plot.subtitle       = ggplot2::element_text(color = "grey45")
    )

  if (horiz) {
    p + ggplot2::theme(
      axis.text.y         = ggplot2::element_text(size = 10,
                                                  color = "grey20"),
      panel.grid.major.y  = ggplot2::element_blank(),
      panel.grid.major.x  = ggplot2::element_line(color = "grey92",
                                                  linewidth = 0.3)
    )
  } else {
    p + ggplot2::theme(
      axis.text.x         = ggplot2::element_text(angle = 35, hjust = 1,
                                                  size = 10,
                                                  color = "grey20"),
      panel.grid.major.x  = ggplot2::element_blank(),
      panel.grid.major.y  = ggplot2::element_line(color = "grey92",
                                                  linewidth = 0.3)
    )
  }
}


# Pyramid dispatch: 2 groups only (enforced upstream). Flipped/mirrored
# back-to-back layout, shared x-axis.
.cc_pyramid_dispatch <- function(long, node_order, group_labels, group_colors,
                                 axis_title, show_values, title, subtitle) {
  gl <- group_labels[1]; gl_next <- group_labels[2]
  dat <- data.frame(
    node  = node_order,
    left  = long$value[long$group == gl][match(node_order,
              long$node[long$group == gl])],
    right = long$value[long$group == gl_next][match(node_order,
              long$node[long$group == gl_next])],
    stringsAsFactors = FALSE
  )
  dat$node <- factor(dat$node, levels = rev(node_order))
  p <- .cc_pyramid(dat, group_labels, group_colors[group_labels], axis_title,
                   show_values, title = NULL, subtitle = NULL)
  if (!is.null(title) || !is.null(subtitle)) {
    p <- p + ggplot2::labs(title = title, subtitle = subtitle)
  }
  p
}


# ---- compare style: stacked ------------------------------------------------
# One horizontal bar per node, split into two adjacent colored segments
# for the two groups. Segments are drawn adjacent (not mirrored), so the
# total bar length is the sum of both group values -- common when both
# values are non-negative centralities.
.cc_stacked <- function(long, group_colors, axis_title, orientation,
                        show_values, title, subtitle) {
  horiz <- identical(orientation, "horizontal")
  # Attach label/text-color columns up front so geom_text can see them.
  if (isTRUE(show_values)) {
    long$label <- sprintf("%.3g", long$value)
    long$text_col <- vapply(as.character(long$group),
                            function(g) .contrast_text_color(group_colors[g]),
                            character(1))
  }

  base_aes <- if (horiz) ggplot2::aes(y = node, x = value, fill = group)
              else       ggplot2::aes(x = node, y = value, fill = group)
  p <- ggplot2::ggplot(long, base_aes) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE),
                      width = 0.72, color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = group_colors, name = NULL)

  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = label, group = group),
      position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
      size = 2.9, fontface = "bold",
      color = long$text_col
    )
  }

  p <- if (horiz) {
    p + ggplot2::labs(x = axis_title, y = NULL, title = title,
                      subtitle = subtitle)
  } else {
    p + ggplot2::labs(x = NULL, y = axis_title, title = title,
                      subtitle = subtitle)
  }

  p <- p + .theme_centrality(base_size = 12) +
    ggplot2::theme(
      legend.position    = "top",
      legend.justification = "left",
      panel.grid.minor   = ggplot2::element_blank()
    )
  if (horiz) {
    p + ggplot2::theme(
      axis.text.y        = ggplot2::element_text(family = "mono", size = 9,
                                                 color = "grey20"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92",
                                                 linewidth = 0.3)
    )
  } else {
    p + ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 35, hjust = 1,
                                                 size = 9, color = "grey20"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92",
                                                 linewidth = 0.3)
    )
  }
}


# ---- compare style: grouped (dodged) ---------------------------------------
.cc_grouped <- function(long, group_colors, axis_title, orientation,
                        show_values, title, subtitle) {
  horiz <- identical(orientation, "horizontal")
  if (isTRUE(show_values)) long$label <- sprintf("%.3g", long$value)

  base_aes <- if (horiz) ggplot2::aes(y = node, x = value, fill = group)
              else       ggplot2::aes(x = node, y = value, fill = group)
  p <- ggplot2::ggplot(long, base_aes) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72),
                      width = 0.65, color = NA) +
    ggplot2::scale_fill_manual(values = group_colors, name = NULL)

  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = label),
      position = ggplot2::position_dodge(width = 0.72),
      hjust = if (horiz) -0.12 else 0.5,
      vjust = if (horiz) 0.5 else -0.5,
      size = 2.7, color = "grey25"
    )
  }

  p <- if (horiz) {
    p + ggplot2::labs(x = axis_title, y = NULL, title = title,
                      subtitle = subtitle)
  } else {
    p + ggplot2::labs(x = NULL, y = axis_title, title = title,
                      subtitle = subtitle)
  }

  p <- p + .theme_centrality(base_size = 12) +
    ggplot2::theme(legend.position    = "top",
                   legend.justification = "left",
                   panel.grid.minor   = ggplot2::element_blank())
  if (horiz) {
    p + ggplot2::theme(
      axis.text.y        = ggplot2::element_text(family = "mono", size = 9,
                                                 color = "grey20"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92",
                                                 linewidth = 0.3)
    )
  } else {
    p + ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 35, hjust = 1,
                                                 size = 9, color = "grey20"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92",
                                                 linewidth = 0.3)
    )
  }
}


# ---- compare style: dumbbell -----------------------------------------------
.cc_dumbbell_n <- function(long, group_colors, axis_title, orientation,
                           show_values, size_by_value, size_range,
                           title, subtitle) {
  horiz <- identical(orientation, "horizontal")

  # Per-node connecting trajectory: segments linking consecutive groups
  # (T1->T2, T2->T3, ...), which for N groups produces (N-1) line segments.
  # This shows the trajectory across groups even when min/max aren't the
  # endpoints in group order.
  group_levels <- levels(long$group)
  seg_df <- do.call(rbind, lapply(split(long, long$node), function(d) {
    d <- d[match(group_levels, as.character(d$group)), , drop = FALSE]
    d <- d[!is.na(d$value), , drop = FALSE]
    if (nrow(d) < 2L) return(NULL)
    data.frame(
      node = d$node[-nrow(d)],
      x1   = d$value[-nrow(d)],
      x2   = d$value[-1L],
      stringsAsFactors = FALSE
    )
  }))
  seg_df$node <- factor(seg_df$node, levels = levels(long$node))

  if (isTRUE(show_values)) {
    long$label <- sprintf("%.3g", long$value)
    # Text color chosen for contrast with the dot's fill (white on dark
    # dots, dark on light dots) so in-bubble labels stay readable.
    long$text_col <- vapply(as.character(long$group),
                            function(g) .contrast_text_color(group_colors[g]),
                            character(1))
  }

  # Points are sized by centrality value when size_by_value = TRUE. The
  # aesthetic comes from `value`; `scale_size()` maps the observed range
  # into the user's `size_range` (mm) so the largest point is readable
  # and the smallest stays visible.
  pt_aes <- if (horiz) {
    if (isTRUE(size_by_value))
      ggplot2::aes(x = value, y = node, color = group, size = value)
    else
      ggplot2::aes(x = value, y = node, color = group)
  } else {
    if (isTRUE(size_by_value))
      ggplot2::aes(x = node, y = value, color = group, size = value)
    else
      ggplot2::aes(x = node, y = value, color = group)
  }

  if (horiz) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = seg_df,
                            ggplot2::aes(x = x1, xend = x2,
                                         y = node, yend = node),
                            color = "grey70", linewidth = 0.7)
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = seg_df,
                            ggplot2::aes(x = node, xend = node,
                                         y = x1, yend = x2),
                            color = "grey70", linewidth = 0.7)
  }
  # When labels are shown inside dots, default fixed-size dots need to be
  # big enough to enclose the text. Bump from 3.8 to 6.5 mm.
  default_pt_size <- if (isTRUE(show_values)) 6.5 else 3.8
  if (isTRUE(size_by_value)) {
    p <- p +
      ggplot2::geom_point(data = long, pt_aes, alpha = 0.95) +
      ggplot2::scale_size(range = size_range, name = "Value",
                          guide = ggplot2::guide_legend(
                            override.aes = list(color = "grey30")))
  } else {
    p <- p + ggplot2::geom_point(data = long, pt_aes, size = default_pt_size)
  }
  p <- p + ggplot2::scale_color_manual(values = group_colors, name = NULL)

  if (isTRUE(show_values)) {
    # Labels sit INSIDE the dots, colored for contrast against the dot's
    # fill. Use a slightly smaller size so the text fits even with the
    # default non-value-sized dots.
    if (horiz) {
      p <- p + ggplot2::geom_text(
        data = long,
        ggplot2::aes(x = value, y = node, label = label),
        color = long$text_col,
        size = 2.5, fontface = "bold", show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = long,
        ggplot2::aes(x = node, y = value, label = label),
        color = long$text_col,
        size = 2.5, fontface = "bold", show.legend = FALSE
      )
    }
  }

  p <- if (horiz) {
    p + ggplot2::labs(x = axis_title, y = NULL, title = title,
                      subtitle = subtitle)
  } else {
    p + ggplot2::labs(x = NULL, y = axis_title, title = title,
                      subtitle = subtitle)
  }

  # Legend sits below the plot, horizontally, to keep the plotting area
  # uncluttered.
  p <- p + .theme_centrality(base_size = 12) +
    ggplot2::theme(
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.box         = "horizontal",
      legend.margin      = ggplot2::margin(4, 0, 0, 0),
      legend.key         = ggplot2::element_rect(fill = NA, color = NA),
      panel.grid.minor   = ggplot2::element_blank()
    )
  if (horiz) {
    p + ggplot2::theme(
      axis.text.y        = ggplot2::element_text(family = "mono", size = 9,
                                                 color = "grey20"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92",
                                                 linewidth = 0.3)
    )
  } else {
    p + ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 35, hjust = 1,
                                                 size = 9, color = "grey20"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92",
                                                 linewidth = 0.3)
    )
  }
}


# ---- compare style: line (one line per group across nodes) ----------------
# Each group becomes a colored polyline connecting its per-node values.
# Orientation follows the rest of the compare family: "horizontal" puts
# nodes on y (value on x, lines run vertically through the plot);
# "vertical" puts nodes on x (value on y, conventional line plot).
.cc_line <- function(long, group_colors, axis_title, orientation,
                     show_values, title, subtitle) {
  horiz <- identical(orientation, "horizontal")

  if (isTRUE(show_values)) long$label <- sprintf("%.3g", long$value)

  if (horiz) {
    p <- ggplot2::ggplot(long,
                         ggplot2::aes(x = value, y = node,
                                      color = group, group = group)) +
      ggplot2::geom_line(linewidth = 0.9, alpha = 0.9,
                         orientation = "y") +
      ggplot2::geom_point(size = 3.2)
  } else {
    p <- ggplot2::ggplot(long,
                         ggplot2::aes(x = node, y = value,
                                      color = group, group = group)) +
      ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
      ggplot2::geom_point(size = 3.2)
  }

  p <- p + ggplot2::scale_color_manual(values = group_colors, name = NULL)

  if (isTRUE(show_values)) {
    if (horiz) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = label), vjust = -0.8, size = 2.6,
        fontface = "bold", show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = label), vjust = -0.8, size = 2.6,
        fontface = "bold", show.legend = FALSE
      )
    }
  }

  p <- if (horiz) {
    p + ggplot2::labs(x = axis_title, y = NULL, title = title,
                      subtitle = subtitle)
  } else {
    p + ggplot2::labs(x = NULL, y = axis_title, title = title,
                      subtitle = subtitle)
  }

  p <- p + .theme_centrality(base_size = 12) +
    ggplot2::theme(
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.key         = ggplot2::element_rect(fill = NA, color = NA),
      panel.grid.minor   = ggplot2::element_blank()
    )

  if (horiz) {
    p + ggplot2::theme(
      axis.text.y        = ggplot2::element_text(family = "mono", size = 9,
                                                 color = "grey20"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92",
                                                 linewidth = 0.3)
    )
  } else {
    p + ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 35, hjust = 1,
                                                 size = 9,
                                                 color = "grey20"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92",
                                                 linewidth = 0.3)
    )
  }
}


# ---- compare style: pyramid (back-to-back, 2 groups only) -----------------
# Kept as an explicit opt-in; colors the two sides by group, not by node.
.cc_pyramid <- function(dat, group_labels, group_colors, axis_title,
                        show_values, title, subtitle) {
  y_num <- as.numeric(dat$node)
  bar_hw <- 0.40
  x_max <- max(abs(c(dat$left, dat$right)), na.rm = TRUE)
  gutter <- if (x_max > 0) x_max * 0.04 else 0.04

  rect_df <- rbind(
    data.frame(yidx = y_num, ymin = y_num - bar_hw, ymax = y_num + bar_hw,
               xmin = -dat$left - gutter, xmax = -gutter,
               value = dat$left,
               group = factor(group_labels[1], levels = group_labels),
               stringsAsFactors = FALSE),
    data.frame(yidx = y_num, ymin = y_num - bar_hw, ymax = y_num + bar_hw,
               xmin = gutter, xmax = dat$right + gutter,
               value = dat$right,
               group = factor(group_labels[2], levels = group_labels),
               stringsAsFactors = FALSE)
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = rect_df,
                       ggplot2::aes(xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax,
                                    fill = group),
                       color = "white", linewidth = 0.25) +
    ggplot2::scale_fill_manual(values = group_colors, name = NULL)

  if (isTRUE(show_values)) {
    rect_df$text_x <- (rect_df$xmin + rect_df$xmax) / 2
    rect_df$text_label <- sprintf("%.3g", rect_df$value)
    rect_df$text_col <- vapply(as.character(rect_df$group),
                               function(g) .contrast_text_color(group_colors[g]),
                               character(1))
    p <- p + ggplot2::geom_text(
      data = rect_df,
      ggplot2::aes(x = text_x, y = yidx, label = text_label),
      size = 3, color = rect_df$text_col, fontface = "bold"
    )
  }

  if (is.null(subtitle)) {
    subtitle <- sprintf("%s  \u2190\u2190    \u2192\u2192  %s",
                        group_labels[1], group_labels[2])
  }

  p +
    ggplot2::scale_y_continuous(breaks = y_num,
                                labels = as.character(dat$node)) +
    ggplot2::scale_x_continuous(
      labels = function(v) format(abs(v), scientific = FALSE, trim = TRUE)
    ) +
    ggplot2::labs(x = axis_title, y = NULL, title = title,
                  subtitle = subtitle) +
    .theme_centrality(base_size = 12) +
    ggplot2::theme(
      axis.text.y        = ggplot2::element_text(family = "mono", size = 9,
                                                 color = "grey20"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "top",
      legend.justification = "left",
      plot.title         = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle      = ggplot2::element_text(hjust = 0.5, color = "grey45")
    )
}


# Pick a legible text color (black or white) given a background hex string.
# Uses WCAG relative-luminance (sRGB gamma ~2.2 approximation).
.contrast_text_color <- function(hex) {
  if (is.na(hex) || !nzchar(hex)) return("grey20")
  rgb_vals <- tryCatch(grDevices::col2rgb(hex)[, 1] / 255,
                       error = function(e) c(0.5, 0.5, 0.5))
  lum <- sum(c(0.299, 0.587, 0.114) * rgb_vals ^ 2.2)
  if (lum > 0.35) "grey15" else "white"
}


# ============================================================================
# plot_centrality_heatmap -- nodes x measures
# ============================================================================

#' Plot Centrality Heatmap
#'
#' Heatmap of nodes (rows) by centrality measures (columns), z-standardized
#' within measure so the diverging palette is meaningful. Optional row
#' clustering groups nodes with similar centrality profiles.
#'
#' @param x Centrality data frame (from \code{\link{centrality}}) or a
#'   network input.
#' @param measures Character vector of measure names.
#' @param cluster_rows Logical. Hierarchically cluster rows so nodes with
#'   similar profiles are adjacent. Default TRUE.
#' @param order_by If \code{cluster_rows = FALSE}, optionally the name of
#'   a measure to sort rows by (descending). Default: first measure.
#' @param show_values Logical. Print z-scores in cells. Default FALSE.
#' @param value_digits Decimal places for cell values. Default 1.
#' @param low,mid,high Color stops for the diverging scale. Defaults to
#'   blue -> white -> red.
#' @param limits Numeric c(min, max) z-score range. Values outside are
#'   squished to the endpoints. Default c(-2.5, 2.5).
#' @param title,subtitle Plot title and subtitle.
#' @param ... Passed to \code{\link{centrality}} when \code{x} is a
#'   network.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0,0, 1,0,1,1,0, 1,1,0,1,1, 0,1,1,0,1, 0,0,1,1,0),
#'               5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' plot_centrality_heatmap(adj)
plot_centrality_heatmap <- function(x,
                                    measures = NULL,
                                    cluster_rows = TRUE,
                                    order_by = NULL,
                                    show_values = FALSE,
                                    value_digits = 1L,
                                    low = "#2171B5",
                                    mid = "white",
                                    high = "#CB181D",
                                    limits = c(-2.5, 2.5),
                                    title = NULL,
                                    subtitle = "z-scored within measure",
                                    ...) {
  df <- .centrality_df(x, measures, ...)
  meas <- setdiff(names(df), c("node", "cluster"))
  if (length(meas) == 0L) {
    stop("No centrality columns to plot.", call. = FALSE)
  }

  # Build z-scored matrix
  mat <- as.matrix(df[, meas, drop = FALSE])
  rownames(mat) <- df$node
  z <- scale(mat)
  # Guard against zero-variance columns
  z[, apply(mat, 2, stats::sd) == 0] <- 0

  # Row ordering
  if (isTRUE(cluster_rows) && nrow(z) > 2L) {
    # Euclidean + average linkage on the node profile vectors
    hc <- stats::hclust(stats::dist(z), method = "average")
    row_order <- rownames(z)[hc$order]
  } else {
    sort_col <- if (!is.null(order_by) && order_by %in% meas) order_by else meas[1]
    row_order <- rownames(z)[order(-mat[, sort_col])]
  }

  long <- data.frame(
    node = rep(rownames(z), length(meas)),
    measure = rep(meas, each = nrow(z)),
    z = as.numeric(z),
    raw = as.numeric(mat),
    stringsAsFactors = FALSE
  )
  long$node <- factor(long$node, levels = rev(row_order))
  long$measure <- factor(.pretty_measure(as.character(long$measure)),
                         levels = .pretty_measure(meas))
  # Add label + contrast color up front so geom_text can see them via aes().
  if (isTRUE(show_values)) {
    long$txt_col <- ifelse(abs(long$z) > max(limits) * 0.5,
                           "grey15", "grey35")
    long$label <- formatC(long$raw, digits = value_digits, format = "f")
  }

  p <- ggplot2::ggplot(long, ggplot2::aes(x = measure, y = node, fill = z)) +
    ggplot2::geom_tile(color = "grey75", linewidth = 0.5)

  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = label),
      size = 2.9, color = long$txt_col
    )
  }

  p +
    ggplot2::scale_fill_gradient2(
      low = low, mid = mid, high = high,
      midpoint = 0, limits = limits,
      oob = scales::squish,
      name = "z-score",
      guide = ggplot2::guide_colorbar(
        title.position = "left", title.vjust = 1,
        barheight = grid::unit(0.45, "cm"),
        barwidth  = grid::unit(6, "cm"),
        frame.colour = "grey70", ticks.colour = "grey40"
      )
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    .theme_centrality() +
    ggplot2::theme(
      panel.grid         = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(angle = 35, hjust = 1,
                                                 face = "bold",
                                                 color = "grey20"),
      axis.text.y        = ggplot2::element_text(family = "mono", size = 9),
      axis.ticks         = ggplot2::element_blank(),
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.title       = ggplot2::element_text(face = "bold",
                                                 color = "grey20"),
      legend.text        = ggplot2::element_text(color = "grey30"),
      plot.title         = ggplot2::element_text(face = "bold",
                                                 color = "grey15",
                                                 margin = ggplot2::margin(b = 4)),
      plot.subtitle      = ggplot2::element_text(color = "grey45",
                                                 margin = ggplot2::margin(b = 10))
    )
}
