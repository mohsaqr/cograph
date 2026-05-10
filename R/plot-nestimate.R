#' @title Nestimate Plotting Methods
#' @description Plot methods for Nestimate network objects, including
#'   \code{netobject}, \code{boot_glasso}, \code{wtna_mixed},
#'   \code{netobject_group}, \code{netobject_ml},
#'   \code{net_bootstrap_group}, and \code{net_stability}.
#'   No Nestimate import is needed — dispatch is via \code{inherits()} class-name checking only.
#' @name plot-nestimate
#' @keywords internal
NULL

#' Plot a Nestimate netobject
#'
#' Applies TNA-compatible styling defaults before delegating to \code{splot()}:
#' directed networks get oval layout, coloured nodes, and sized arrows;
#' undirected networks get spring layout with no arrows or dashes.
#' All parameters can be overridden by the caller.
#'
#' @param x A \code{netobject} (from Nestimate).
#' @param ... Additional arguments passed to \code{splot()}.
#'
#' @return Invisibly returns the plot.
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
  tna_methods <- c("relative", "frequency", "attention",
                   "co_occurrence", "wtna", "wtna_cooccurrence")
  use_tna <- if (!is.null(x$method)) {
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

#' Plot Nestimate GLASSO Bootstrap Results
#'
#' Visualizes \code{boot_glasso} objects from the Nestimate package.
#' Plots a partial-correlation network with edge inclusion probabilities
#' mapped to edge transparency.
#'
#' @param x A \code{boot_glasso} object (from Nestimate).
#' @param use_thresholded Logical: use \code{$thresholded_pcor}? If FALSE, uses
#'   \code{$original_pcor}. Default TRUE.
#' @param show_inclusion Logical: scale edge alpha by inclusion probability?
#'   Default TRUE.
#' @param inclusion_threshold Numeric: minimum inclusion probability to show an edge.
#'   Default \code{1 - x$alpha} (i.e. the complement of the alpha level).
#' @param edge_positive_color Color for positive partial correlations. Default \code{"#2E7D32"}.
#' @param edge_negative_color Color for negative partial correlations. Default \code{"#C62828"}.
#' @param ... Additional arguments passed to \code{splot()}.
#'
#' @return Invisibly returns the plot.
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


#' Plot a Mixed Window TNA Object
#'
#' Plot a \code{wtna_mixed} object either as a single overlaid network or as
#' two separate group panels.
#'
#' @param x A \code{wtna_mixed} object (from Nestimate \code{wtna(..., method = "both")}).
#' @param type Character. \code{"overlay"} (default) renders both networks on a
#'   single canvas via \code{\link{plot_mixed_network}} — co-occurrence as straight
#'   undirected edges, transitions as curved directed arrows.
#'   \code{"group"} plots each component as a separate panel.
#' @param ... Additional arguments passed to \code{\link{plot_mixed_network}}
#'   (\code{type = "overlay"}) or \code{\link{splot}} (\code{type = "group"}).
#'
#' @return Invisibly returns \code{x}.
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


#' Plot a Group of Nestimate netobjects
#'
#' Creates a multi-panel plot for a \code{netobject_group} list, one panel per group.
#' Mirrors \code{plot_group_permutation()} in structure.
#'
#' @param x A \code{netobject_group} object (named list of netobjects).
#' @param nrow Integer: number of rows in the panel grid. Auto-computed if NULL.
#' @param ncol Integer: number of columns in the panel grid. Auto-computed if NULL.
#' @param common_scale Logical: use the same maximum weight across all panels? Default TRUE.
#' @param title_prefix Character: optional prefix added before each group name in panel titles.
#' @param combined Logical: when TRUE (default), arrange the panels in an
#'   internal grid via \code{graphics::par(mfrow=...)}. Set to FALSE to draw
#'   each panel into the active device without altering \code{par()}, e.g.
#'   when laying panels out yourself with \code{\link{panel_layout}()}.
#' @param ... Additional arguments passed to \code{splot()}.
#'
#' @return Invisibly returns \code{x}.
#' @examples
#' mat <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
#' colnames(mat) <- rownames(mat) <- c("A", "B", "C")
#' net1 <- as_cograph(mat)
#' net2 <- as_cograph(mat * 0.5)
#' grp <- structure(list(G1 = net1, G2 = net2), class = c("netobject_group", "list"))
#' plot_netobject_group(grp)
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

#' @rdname plot_netobject_group
#' @export
plot.netobject_group <- function(x, ...) plot_netobject_group(x, ...)


#' Plot a Multilevel Nestimate netobject
#'
#' Creates a side-by-side plot for a \code{netobject_ml} object, showing the
#' between-person and within-person networks.
#'
#' @param x A \code{netobject_ml} object with \code{$between} and \code{$within} networks.
#' @param layout Character: layout algorithm. Default \code{"oval"} (deterministic).
#' @param common_scale Logical: use the same maximum weight for both panels? Default TRUE.
#' @param titles Character vector of length 2: panel titles. Default
#'   \code{c("Between-person", "Within-person")}.
#' @param combined Logical: when TRUE (default), draws both panels in an
#'   internal 1 x 2 grid. Set to FALSE to render into a layout the caller
#'   already configured (e.g. via \code{\link{panel_layout}()}).
#' @param ... Additional arguments passed to \code{splot()}.
#'
#' @return Invisibly returns \code{x}.
#' @examples
#' mat <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
#' colnames(mat) <- rownames(mat) <- c("A", "B", "C")
#' btw <- as_cograph(mat)
#' wth <- as_cograph(mat * 0.6)
#' ml <- structure(list(between = btw, within = wth), class = c("netobject_ml", "list"))
#' plot_netobject_ml(ml)
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

#' @rdname plot_netobject_ml
#' @export
plot.netobject_ml <- function(x, ...) plot_netobject_ml(x, ...)


#' Plot a Group Bootstrap Result
#'
#' Plots each cluster's \code{net_bootstrap} in a grid, routing every panel
#' through \code{splot.net_bootstrap} so significance styling (solid vs
#' dashed edges) is preserved. Earlier versions extracted \code{bs$original}
#' per cluster and handed plain netobjects to \code{splot()}, which
#' dispatches to \code{splot.netobject} — that path has no concept of
#' significance, so every edge rendered identically.
#'
#' @param x A \code{net_bootstrap_group} object (list of \code{net_bootstrap}).
#' @param nrow,ncol Grid dimensions. Defaults to auto-computed square layout.
#' @param common_scale Logical: use the same maximum weight across panels? Default TRUE.
#' @param combined Logical: when TRUE (default), arrange panels in an internal
#'   grid via \code{graphics::par(mfrow=...)}. Set to FALSE to draw into a
#'   layout the caller already configured (e.g. via \code{\link{panel_layout}()}).
#' @param ... Additional arguments passed to \code{splot.net_bootstrap}
#'   (e.g. \code{display = "significant"}, \code{show_stars = FALSE}).
#'
#' @return Invisibly returns \code{x}.
#' @export
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE)
#' set.seed(1)
#' seqs <- data.frame(T1 = sample(c("A","B","C"), 30, replace = TRUE),
#'                    T2 = sample(c("A","B","C"), 30, replace = TRUE))
#' grp <- Nestimate::cluster_network(seqs, k = 2)
#' gbs <- Nestimate::bootstrap_network(grp, iter = 10)
#' plot_net_bootstrap_group(gbs)
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

#' @rdname plot_net_bootstrap_group
#' @export
plot.net_bootstrap_group <- function(x, ...) plot_net_bootstrap_group(x, ...)


#' Plot Centrality Stability Results
#'
#' Visualizes the centrality stability analysis from a \code{net_stability}
#' object. Shows how centrality correlations drop as cases are removed.
#'
#' @param x A \code{net_stability} object (from \code{Nestimate::centrality_stability}).
#' @param ... Additional graphical arguments.
#'
#' @return Invisibly returns \code{x}.
#' @export
#' @examplesIf requireNamespace("Nestimate", quietly = TRUE)
#' set.seed(1)
#' seqs <- data.frame(T1 = sample(c("A","B","C"), 30, replace = TRUE),
#'                    T2 = sample(c("A","B","C"), 30, replace = TRUE))
#' net <- Nestimate::build_network(seqs, method = "tna")
#' cs <- Nestimate::centrality_stability(net, iter = 10)
#' plot_net_stability(cs)
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

#' @rdname plot_net_stability
#' @export
plot.net_stability <- function(x, ...) plot_net_stability(x, ...)
