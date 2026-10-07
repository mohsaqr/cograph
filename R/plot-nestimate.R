#' @title Nestimate Plotting Methods
#' @description Plot methods for Nestimate network objects, including
#'   \code{netobject}, \code{boot_glasso}, \code{wtna_mixed},
#'   \code{netobject_group}, \code{netobject_ml},
#'   \code{net_bootstrap_group}, and \code{net_stability}.
#'   No Nestimate import is needed — dispatch is via \code{inherits()} class-name checking only.
#' @name plot-nestimate
#' @keywords internal
#' @noRd
NULL

#' @details
#' \code{splot.netobject()} plots a \code{netobject} from Nestimate. Networks
#' estimated by a transition-type method (\code{"relative"},
#' \code{"frequency"}, \code{"attention"}, \code{"co_occurrence"},
#' \code{"wtna"}, \code{"wtna_cooccurrence"}, \code{"entropy"}) get
#' \code{tna_styling = TRUE}, and other methods, such as correlation and
#' partial correlation networks, get \code{psych_styling = TRUE}. When the
#' method is not recorded, directed networks get the TNA styling. Arguments
#' supplied by the caller override these defaults.
#'
#' @rdname splot
#' @export
splot.netobject <- function(x, ...) {
  args <- list(...)
  if (is.null(args$labels)) args$labels <- x$nodes$label %||% rownames(x$weights)

  # Auto-suppress ".00" tails on integer-valued matrices (counts/frequencies).
  if (is.null(args$weight_digits)) {
    nz <- x$weights[x$weights != 0]
    if (length(nz) > 0 && all(nz == floor(nz))) {
      args$weight_digits <- 0L
      if (is.null(args$edge_label_digits)) args$edge_label_digits <- 0L
    }
  }

  # Sequence-based TNA family uses TNA styling (oval layout, palette, etc.);
  # correlation-family (glasso/cor/pcor/ising) uses psych styling. Direction
  # alone isn't the right signal — build_cna and wtna cooccurrence are
  # undirected TNA-family networks and still belong in oval layout with no
  # arrows. When $method is missing (legacy mocks, hand-built netobjects),
  # fall back to direction: directed -> TNA, undirected -> psych.
  # edge_betweenness: a routing network that inherits its source's
  # directedness (Nestimate preserves x$directed) — a directed one drawn with
  # psych styling would lose arrows and one triangle of each asymmetric pair,
  # while one derived from a correlation-family source belongs in the psych
  # look. Style it like its SOURCE network when Nestimate recorded it
  # ($edge_betweenness$source_method), falling back to direction: undirected
  # TNA-family sources (co_occurrence, wtna_cooccurrence) then keep the same
  # TNA look their source network gets.
  # entropy: Nestimate's entropy_network() — the TNA transition network
  # re-weighted with per-edge entropy contributions; it inherits the source
  # network's fields and belongs in the same TNA look for comparability.
  tna_methods <- c("relative", "frequency", "attention",
                   "co_occurrence", "wtna", "wtna_cooccurrence", "entropy")
  use_tna <- if (identical(x$method, "edge_betweenness")) {
    src <- x[["edge_betweenness"]][["source_method"]]
    if (!is.null(src)) src %in% tna_methods || isTRUE(x$directed)
    else isTRUE(x$directed)
  } else if (!is.null(x$method)) {
    x$method %in% tna_methods
  } else {
    isTRUE(x$directed)
  }

  if (use_tna) {
    if (!is.null(x$initial) && is.null(args$donut_fill)) {
      args$donut_fill  <- as.numeric(x$initial)
      args$donut_empty <- args$donut_empty %||% FALSE
    }
    if (is.null(args$tna_styling)) args$tna_styling <- TRUE
  } else {
    if (!is.null(x$predictability) && is.null(args$donut_fill)) {
      args$donut_fill  <- as.numeric(x$predictability)
      args$donut_empty <- args$donut_empty %||% FALSE
    }
    if (is.null(args$psych_styling)) args$psych_styling <- TRUE
  }

  do.call(splot, c(list(x = x$weights), args))
}

#' @details
#' \code{splot.boot_glasso()} plots the partial-correlation network of a
#' \code{boot_glasso} object from Nestimate, with the bootstrap inclusion
#' probability of each edge mapped to its opacity.
#'
#' @param use_thresholded Logical. Plot \code{$thresholded_pcor} of a
#'   \code{boot_glasso} object, or \code{$original_pcor} when \code{FALSE}.
#'   Default \code{TRUE}.
#' @param show_inclusion Logical. Map the inclusion probability of each edge
#'   of a \code{boot_glasso} object to its opacity, from 0.2 to 1. Default
#'   \code{TRUE}.
#' @param inclusion_threshold Numeric. Minimum inclusion probability of a
#'   plotted \code{boot_glasso} edge. \code{NULL} (default) uses
#'   \code{1 - x$alpha}, or 0.95 when \code{x$alpha} is absent.
#'
#' @rdname splot
#' @export
splot.boot_glasso <- function(x,
                              use_thresholded     = TRUE,
                              show_inclusion      = TRUE,
                              inclusion_threshold = NULL,
                              edge_positive_color = "#2E7D32",
                              edge_negative_color = "#C62828",
                              ...) {
  # Build inclusion probability matrix (vectorized)
  n <- x$p
  inclusion_matrix <- matrix(0, n, n, dimnames = list(x$nodes, x$nodes))

  if (!is.null(x$edge_ci) && nrow(x$edge_ci) > 0) {
    edge_parts <- strsplit(x$edge_ci$edge, " -- ")
    from_nodes <- vapply(edge_parts, `[[`, character(1), 1)
    to_nodes   <- vapply(edge_parts, `[[`, character(1), 2)
    from_idx   <- match(from_nodes, x$nodes)
    to_idx     <- match(to_nodes, x$nodes)
    valid      <- !is.na(from_idx) & !is.na(to_idx)
    if (any(valid)) {
      inclusion_matrix[cbind(from_idx[valid], to_idx[valid])] <- x$edge_ci$inclusion[valid]
      inclusion_matrix[cbind(to_idx[valid],   from_idx[valid])] <- x$edge_ci$inclusion[valid]
    }
  }

  # Select weights and apply inclusion threshold
  weights       <- if (use_thresholded) x$thresholded_pcor else x$original_pcor
  eff_threshold <- inclusion_threshold %||% (1 - (x$alpha %||% 0.05))
  weights       <- weights * (inclusion_matrix >= eff_threshold)

  args    <- list(...)
  n_nodes <- nrow(weights)

  if (is.null(args$layout))       args$layout       <- "spring"
  if (is.null(args$directed))     args$directed     <- FALSE
  if (is.null(args$show_arrows))  args$show_arrows  <- FALSE
  if (is.null(args$labels))       args$labels       <- x$nodes
  if (is.null(args$node_size))    args$node_size    <- 7

  args$edge_positive_color <- edge_positive_color
  args$edge_negative_color <- edge_negative_color

  # Pre-round to match splot's internal rounding so edge_alpha vector length
  # matches splot's internal edge count (same fix as in splot.net_bootstrap)
  wd <- args$weight_digits %||% 2
  if (!is.null(wd)) weights <- round(weights, wd)

  # Scale edge alpha by inclusion probability
  if (show_inclusion) {
    edge_idx    <- which(weights != 0, arr.ind = TRUE)
    n_edges     <- nrow(edge_idx)
    if (n_edges > 0) {
      # Vectorized: map inclusion [0,1] to alpha [0.2, 1.0]
      edge_alphas     <- 0.2 + 0.8 * inclusion_matrix[edge_idx]
      args$edge_alpha <- edge_alphas
    }
  }

  do.call(splot, c(list(x = weights), args))
}


#' @details
#' \code{splot.wtna_mixed()} plots a \code{wtna_mixed} object from
#' \code{Nestimate::wtna(..., method = "both")}, either as one overlaid
#' network or as two panels.
#'
#' @param type For a \code{net_mlvar} object, the network to plot:
#'   \code{"temporal"} or \code{"t"} (default), \code{"contemporaneous"} or
#'   \code{"c"}, \code{"between"} or \code{"b"}, or \code{"all"} or
#'   \code{"a"} for a 1 x 3 panel, matched without regard to case. For a
#'   \code{wtna_mixed} object, \code{"overlay"} (default) plots both
#'   networks on one canvas with \code{\link{plot_mixed_network}}, the
#'   co-occurrences as straight undirected edges and the transitions as curved
#'   directed edges, and \code{"group"} plots each network in its own panel.
#'
#' @rdname splot
#' @export
splot.wtna_mixed <- function(x, type = c("overlay", "group"), ...) {
  type <- match.arg(type)
  if (type == "overlay") {
    args <- list(...)
    if (is.null(args$initial) && !is.null(x$transition$initial))
      args$initial <- x$transition$initial
    do.call(plot_mixed_network, c(
      list(sym_matrix  = x$cooccurrence$weights,
           asym_matrix = x$transition$weights),
      args
    ))
  } else {
    group <- structure(
      list(Transition = x$transition, `Co-occurrence` = x$cooccurrence),
      group_col = "network_type",
      class = "netobject_group"
    )
    splot(group, ...)
  }
  invisible(x)
}


#' @rdname plot-results
#' @export
plot_netobject_group <- function(x,
                                 nrow         = NULL,
                                 ncol         = NULL,
                                 common_scale = TRUE,
                                 title_prefix = NULL,
                                 combined     = TRUE,
                                 ...) {
  n_groups    <- length(x)
  group_names <- names(x) %||% paste0("Group ", seq_len(n_groups))

  if (n_groups == 0) {
    message("No groups to display")
    return(invisible(NULL))
  }

  # Common scale: compute before early-return so single-group path honours it too
  max_abs <- NULL
  if (common_scale) {
    all_w   <- unlist(lapply(x, function(e) abs(e$weights)))
    max_abs <- max(all_w, na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- NULL # nocov
  }

  if (n_groups == 1) {
    args <- list(...)
    if (is.null(args$title)) {
      panel_title <- if (!is.null(title_prefix)) paste0(title_prefix, group_names[1]) else group_names[1]
      args$title  <- panel_title
    }
    if (!is.null(max_abs)) args$maximum <- max_abs
    return(do.call(splot, c(list(x = x[[1]]), args)))
  }

  if (combined) {
    if (is.null(ncol)) ncol <- ceiling(sqrt(n_groups))
    if (is.null(nrow)) nrow <- ceiling(n_groups / ncol)
    old_par <- graphics::par(mfrow = c(nrow, ncol), mar = c(2, 2, 3, 1))
    on.exit(graphics::par(old_par), add = TRUE)
  }

  for (k in seq_len(n_groups)) {
    panel_title <- if (!is.null(title_prefix)) paste0(title_prefix, group_names[k]) else group_names[k]
    args        <- list(...)
    if (is.null(args$title))    args$title   <- panel_title
    if (!is.null(max_abs))      args$maximum <- max_abs
    do.call(splot, c(list(x = x[[k]]), args))
  }

  invisible(x)
}

#' @noRd
#' @export
plot.netobject_group <- function(x, ...) plot_netobject_group(x, ...)


#' @rdname plot-results
#' @export
plot_netobject_ml <- function(x,
                              layout       = NULL,
                              common_scale = TRUE,
                              titles       = c("Between-person", "Within-person"),
                              combined     = TRUE,
                              ...) {
  if (is.null(x$between)) stop("net_ml object missing $between", call. = FALSE)
  if (is.null(x$within))  stop("net_ml object missing $within",  call. = FALSE)
  if (length(titles) < 2) stop("titles must have length >= 2", call. = FALSE)

  max_abs <- NULL
  if (common_scale) {
    max_abs <- max(abs(x$between$weights), abs(x$within$weights), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- NULL # nocov
  }

  layout_alg <- layout %||% "oval"

  if (combined) {
    old_par <- graphics::par(mfrow = c(1, 2), mar = c(2, 2, 3, 1))
    on.exit(graphics::par(old_par), add = TRUE)
  }

  for (side in 1:2) {
    net  <- if (side == 1) x$between else x$within
    args <- list(...)
    args$title  <- titles[side]
    args$layout <- layout_alg
    if (!is.null(max_abs)) args$maximum <- max_abs
    do.call(splot, c(list(x = net), args))
  }

  invisible(x)
}

#' @noRd
#' @export
plot.netobject_ml <- function(x, ...) plot_netobject_ml(x, ...)


#' @rdname plot-results
#' @export
plot_net_bootstrap_group <- function(x,
                                     nrow         = NULL,
                                     ncol         = NULL,
                                     common_scale = TRUE,
                                     combined     = TRUE,
                                     ...) {
  n_groups    <- length(x)
  group_names <- names(x) %||% paste0("Group ", seq_len(n_groups))

  if (n_groups == 0) {
    message("No groups to display")
    return(invisible(NULL))
  }

  max_abs <- NULL
  if (common_scale) {
    all_w <- unlist(lapply(x, function(bs) abs(bs$original$weights)))
    max_abs <- max(all_w, na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- NULL # nocov
  }

  if (n_groups == 1) {
    args <- list(...)
    if (is.null(args$title)) args$title <- group_names[1]
    if (!is.null(max_abs))   args$maximum <- max_abs
    return(do.call(splot, c(list(x = x[[1]]), args)))
  }

  if (combined) {
    if (is.null(ncol)) ncol <- ceiling(sqrt(n_groups))
    if (is.null(nrow)) nrow <- ceiling(n_groups / ncol)
    old_par <- graphics::par(mfrow = c(nrow, ncol), mar = c(2, 2, 3, 1))
    on.exit(graphics::par(old_par), add = TRUE)
  }

  for (k in seq_len(n_groups)) {
    args <- list(...)
    if (is.null(args$title)) args$title <- group_names[k]
    if (!is.null(max_abs))   args$maximum <- max_abs
    do.call(splot, c(list(x = x[[k]]), args))
  }

  invisible(x)
}

#' @noRd
#' @export
plot.net_bootstrap_group <- function(x, ...) plot_net_bootstrap_group(x, ...)


#' @rdname plot-results
#' @export
plot_net_stability <- function(x, ...) {
  measures   <- x$measures
  drop_prop  <- x$drop_prop
  threshold  <- x$threshold %||% 0.7
  n_measures <- length(measures)

  # Set up colors
  cols <- if (n_measures <= 8) {
    grDevices::palette.colors(n_measures, "R4")
  } else {
    grDevices::rainbow(n_measures)
  }

  # Compute mean correlation at each drop proportion
  plot(NULL, xlim = range(drop_prop), ylim = c(0, 1),
       xlab = "Proportion of cases dropped",
       ylab = "Mean correlation with original",
       main = "Centrality Stability", ...)

  for (i in seq_along(measures)) {
    corr_mat <- x$correlations[[measures[i]]]
    # corr_mat is iter x length(drop_prop) matrix
    mean_corrs <- colMeans(corr_mat, na.rm = TRUE)
    graphics::lines(drop_prop, mean_corrs, col = cols[i], lwd = 2)
    graphics::points(drop_prop, mean_corrs, col = cols[i], pch = 16, cex = 0.8)
  }

  # Threshold line
  graphics::abline(h = threshold, lty = 2, col = "gray50")
  graphics::text(max(drop_prop), threshold, paste("threshold =", threshold),
                 adj = c(1, -0.5), cex = 0.8, col = "gray50")

  # CS-coefficient labels
  cs_vals <- x$cs
  cs_text <- paste0(names(cs_vals), " CS=", round(cs_vals, 2))
  graphics::legend("bottomleft", legend = cs_text, col = cols, lwd = 2,
                   bty = "n", cex = 0.8)

  invisible(x)
}

# NOTE: cograph deliberately does NOT register an S3 `plot.net_stability`
# method. Nestimate (the data layer that produces `net_stability` objects)
# ships its own ggplot `plot.net_stability` with confidence-interval ribbons,
# which is the canonical rendering. Registering one here too created an
# S3 dispatch clash (last-loaded wins). The base-graphics rendering remains
# available on demand via `plot_net_stability()`.
