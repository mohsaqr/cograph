#' @title Permutation Test Plotting
#' @description Plot permutation test results from tna::permutation_test().
#'   Visualizes network comparison with styling to distinguish
#'   significant from non-significant edge differences.
#' @name plot-permutation
#' @keywords internal
NULL

#' @rdname plot_permutation
#' @export
splot.tna_permutation <- function(x, ...) {
  plot_permutation(x, ...)
}

#' @rdname plot_group_permutation
#' @export
splot.group_tna_permutation <- function(x, ...) {
  plot_group_permutation(x, ...)
}

#' Plot Permutation Test Results
#'
#' Visualizes permutation test results with styling to distinguish
#' significant from non-significant edge differences. Works with tna_permutation
#' objects from the tna package.
#'
#' @param x A tna_permutation object (from tna::permutation_test).
#' @param show_nonsig Logical: show non-significant edges? Default FALSE (only significant shown).
#' @param edge_positive_color Color for positive differences (x > y). Default "#009900" (green).
#' @param edge_negative_color Color for negative differences (x < y). Default "#C62828" (red).
#' @param edge_nonsig_color Color for non-significant edges. Default "#888888" (grey).
#' @param edge_nonsig_style Line style for non-significant edges (2=dashed). Default 2.
#' @param show_stars Logical: show significance stars (*, **, ***) on edges? Default TRUE.
#' @param show_effect Logical: show effect size in parentheses for significant edges? Default FALSE.
#' @param edge_nonsig_alpha Alpha for non-significant edges. Default 0.4.
#' @param ... Additional arguments passed to splot().
#'
#' @return Invisibly returns the plot.
#'
#' @details
#' The function expects a tna_permutation object containing:
#' \itemize{
#'   \item \code{edges$diffs_true}: Matrix of actual edge differences (x - y)
#'   \item \code{edges$diffs_sig}: Matrix of significant differences only
#'   \item \code{edges$stats}: Data frame with edge_name, diff_true, effect_size, p_value
#' }
#'
#' Edge styling:
#' \itemize{
#'   \item Significant positive: solid green, bold labels with stars
#'   \item Significant negative: solid red, bold labels with stars
#'   \item Non-significant (when show_nonsig=TRUE): dashed grey, plain labels, lower alpha
#' }
#'
#' @examples
#' \dontrun{
#' # Create a mock tna_permutation object with synthetic data
#' set.seed(42)
#' diffs <- matrix(c(0, 0.15, -0.1, -0.2, 0, 0.05, 0.1, -0.05, 0), 3, 3)
#' rownames(diffs) <- colnames(diffs) <- c("A", "B", "C")
#' diffs_sig <- diffs
#' diffs_sig[abs(diffs) < 0.1] <- 0
#' perm <- list(
#'   edges = list(
#'     diffs_true = diffs,
#'     diffs_sig = diffs_sig,
#'     stats = data.frame(
#'       edge_name = c("A -> B", "A -> C", "B -> A", "B -> C", "C -> A", "C -> B"),
#'       diff_true = c(0.15, -0.1, -0.2, 0.05, 0.1, -0.05),
#'       effect_size = c(2.1, -1.5, -2.8, 0.4, 1.2, -0.3),
#'       p_value = c(0.01, 0.04, 0.001, 0.3, 0.02, 0.5)
#'     )
#'   )
#' )
#' attr(perm, "level") <- 0.05
#' attr(perm, "labels") <- c("A", "B", "C")
#' class(perm) <- c("tna_permutation", "list")
#' plot_permutation(perm)
#' }
#'
#' @export
plot_permutation <- function(x,
                                  show_nonsig = FALSE,
                                  edge_positive_color = "#009900",
                                  edge_negative_color = "#C62828",
                                  edge_nonsig_color = "#888888",
                                  edge_nonsig_style = 2,
                                  show_stars = TRUE,
                                  show_effect = FALSE,
                                  edge_nonsig_alpha = 0.4,
                                  ...) {
  level <- attr(x, "level") %||% 0.05
  labels <- attr(x, "labels")

  # Get difference matrices

  diffs_true <- x$edges$diffs_true
  diffs_sig <- x$edges$diffs_sig
  edge_stats <- x$edges$stats

  if (is.null(diffs_true)) {
    stop("Cannot find edge differences in permutation object", call. = FALSE)
  }

  # Get weights based on show_nonsig
  weights <- if (show_nonsig) diffs_true else diffs_sig

  # Build args list
  args <- list(...)

  # Translate qgraph-style vsize to node_size
  if (!is.null(args$vsize) && is.null(args$node_size)) {
    args$node_size <- args$vsize
    args$vsize <- NULL
  }

  n_nodes <- nrow(weights)

  # Apply same rounding as splot to match edge counts
  weight_digits <- args$weight_digits %||% 2
  weights       <- round(weights, weight_digits)

  # Default layout
  if (is.null(args$layout)) args$layout <- "oval"

  # Labels
  if (is.null(args$labels) && !is.null(labels)) {
    args$labels <- labels
  }
  if (is.null(args$labels)) {
    args$labels <- rownames(weights)
  }

  # Default styling
  if (is.null(args$edge_labels)) args$edge_labels <- TRUE
  if (is.null(args$edge_label_size)) args$edge_label_size <- 0.6
  if (is.null(args$edge_label_position)) args$edge_label_position <- 0.35
  if (is.null(args$edge_label_halo)) args$edge_label_halo <- TRUE
  if (is.null(args$node_size)) args$node_size <- 7
  if (is.null(args$arrow_size)) args$arrow_size <- 0.61
  if (is.null(args$edge_label_leading_zero)) args$edge_label_leading_zero <- FALSE

  # Compute edge indices for non-zero edges
  edge_idx <- which(weights != 0, arr.ind = TRUE)
  n_edges <- nrow(edge_idx)

  if (n_edges == 0) {
    message("No edges to display")
    return(invisible(NULL))
  }

  # Build p-value matrix from stats if needed
  p_matrix <- NULL
  effect_matrix <- NULL

  if (!is.null(edge_stats)) {
    # Reconstruct matrices from stats data frame
    p_matrix <- matrix(1, n_nodes, n_nodes)
    effect_matrix <- matrix(0, n_nodes, n_nodes)

    # Get row/column names with null fallback
    row_names <- rownames(diffs_true)
    col_names <- colnames(diffs_true)
    if (is.null(row_names) || is.null(col_names)) {
      row_names <- seq_len(nrow(diffs_true))
      col_names <- seq_len(ncol(diffs_true))
    }

    # edge_stats has edge_name like "A -> B"
    for (k in seq_len(nrow(edge_stats))) {
      # Parse edge name
      edge_name <- edge_stats$edge_name[k]
      parts <- strsplit(edge_name, " -> ")[[1]]
      if (length(parts) == 2) {
        from_idx <- which(row_names == parts[1])
        to_idx <- which(col_names == parts[2])
        if (length(from_idx) == 1 && length(to_idx) == 1) {
          p_matrix[from_idx, to_idx] <- edge_stats$p_value[k]
          effect_matrix[from_idx, to_idx] <- edge_stats$effect_size[k]
        }
      }
    }
  }

  # Build per-edge vectors (like bootstrap does)
  sig_mask <- diffs_sig != 0

  if (show_nonsig && n_edges > 0) {
    # Show all edges with styling for sig vs non-sig
    edge_colors <- character(n_edges)
    edge_styles <- numeric(n_edges)
    edge_fontfaces <- numeric(n_edges)
    edge_alphas <- numeric(n_edges)

    for (k in seq_len(n_edges)) {
      i <- edge_idx[k, 1]
      j <- edge_idx[k, 2]
      diff_val <- weights[i, j]

      if (sig_mask[i, j]) {
        # Significant edge
        edge_colors[k] <- if (diff_val > 0) edge_positive_color else edge_negative_color
        edge_styles[k] <- 1  # solid
        edge_fontfaces[k] <- 2  # bold
        edge_alphas[k] <- 1
      } else {
        # Non-significant edge
        edge_colors[k] <- edge_nonsig_color
        edge_styles[k] <- edge_nonsig_style
        edge_fontfaces[k] <- 1  # plain
        edge_alphas[k] <- edge_nonsig_alpha
      }
    }

    args$edge_color <- edge_colors
    args$edge_style <- edge_styles
    args$edge_label_fontface <- edge_fontfaces
    args$edge_alpha <- edge_alphas

  } else {
    # Default: show only significant edges
    args$edge_positive_color <- edge_positive_color
    args$edge_negative_color <- edge_negative_color
    args$edge_label_fontface <- 2  # bold
  }

  # Build custom edge labels with optional effect size
  if (n_edges > 0 && (show_stars || show_effect)) {
    edge_labels_custom <- character(n_edges)

    for (k in seq_len(n_edges)) {
      i <- edge_idx[k, 1]
      j <- edge_idx[k, 2]

      # Format weight (remove leading zero)
      w <- weights[i, j]
      w_str <- sub("^0\\.", ".", sprintf("%.2f", w))
      w_str <- sub("^-0\\.", "-.", w_str)

      # Add stars if requested
      stars_str <- ""
      if (show_stars && !is.null(p_matrix)) {
        stars_str <- get_significance_stars(p_matrix[i, j])
      }

      # Add effect size if requested, not NA, and edge is significant
      effect_str <- ""
      if (show_effect && !is.null(effect_matrix) && sig_mask[i, j]) {
        eff <- effect_matrix[i, j]
        if (!is.na(eff) && is.finite(eff)) {
          effect_str <- sprintf(" (%.1f)", abs(eff))
        }
      }

      edge_labels_custom[k] <- paste0(w_str, stars_str, effect_str)
    }

    args$edge_labels <- edge_labels_custom
  }

  # Edges are scaled by weight by default (splot default behavior)
  # No need to set edge_width - let splot handle it

  # Title
  if (is.null(args$title)) {
    args$title <- if (show_nonsig) {
      "Permutation Test: All Differences"
    } else {
      "Permutation Test: Significant Differences"
    }
  }

  # Node colors from tna model
  node_colors <- attr(x, "colors")
  if (!is.null(node_colors) && is.null(args$node_fill)) {
    args$node_fill <- node_colors
  }

  do.call(splot, c(list(x = weights), args))
}


#' Plot Group Permutation Test Results
#'
#' Visualizes all pairwise permutation test results from a group_tna object.
#' Creates a multi-panel plot with one panel per comparison.
#'
#' @param x A group_tna_permutation object (from tna::permutation_test on group_tna).
#' @param i Index or name of specific comparison to plot. NULL for all.
#' @param ... Additional arguments passed to plot_permutation().
#'
#' @return Invisibly returns NULL.
#'
#' @examples
#' \dontrun{
#' # Create a mock group_tna_permutation object
#' set.seed(42)
#' d1 <- matrix(c(0, 0.2, -0.1, -0.2, 0, 0.1, 0.1, -0.1, 0), 3, 3)
#' rownames(d1) <- colnames(d1) <- c("A", "B", "C")
#' d1_sig <- d1
#' d1_sig[abs(d1) < 0.15] <- 0
#' perm1 <- list(edges = list(diffs_true = d1, diffs_sig = d1_sig, stats = NULL))
#' attr(perm1, "labels") <- c("A", "B", "C")
#' class(perm1) <- c("tna_permutation", "list")
#' gperm <- list("G1 vs. G2" = perm1)
#' class(gperm) <- c("group_tna_permutation", "list")
#' plot_group_permutation(gperm)
#' }
#'
#' @export
plot_group_permutation <- function(x, i = NULL, ...) {
  # Strip `title` from `...` so we can re-inject a per-panel title without
  # R's argument matcher seeing `title` twice. If the user supplied one,
  # compose it as `"user_title - pair_name"` so both pieces of information
  # survive into each panel; otherwise the pair name alone becomes the title.
  dots <- list(...)
  user_title <- dots$title
  dots$title <- NULL
  compose_title <- function(pair) {
    if (is.null(user_title)) pair else paste(user_title, "-", pair)
  }

  if (!is.null(i)) {
    # Plot single comparison
    elem <- x[[i]]
    if (is.null(elem)) {
      stop("Invalid index i=", i, call. = FALSE)
    }
    pair_title <- if (is.character(i)) i else names(x)[i]
    return(do.call(plot_permutation,
                   c(list(elem, title = compose_title(pair_title)), dots)))
  }

  # Plot all comparisons
  n_pairs <- length(x)
  if (n_pairs == 0) {
    message("No comparisons to display")
    return(invisible(NULL))
  }

  # Calculate grid layout
  ncol <- ceiling(sqrt(n_pairs))
  nrow <- ceiling(n_pairs / ncol)

  # Set up multi-panel plot
  old_par <- graphics::par(mfrow = c(nrow, ncol), mar = c(2, 2, 3, 1))
  on.exit(graphics::par(old_par), add = TRUE)

  pair_names <- names(x)
  for (k in seq_len(n_pairs)) {
    pair_title <- pair_names[k] %||% paste("Comparison", k)
    do.call(plot_permutation,
            c(list(x[[k]], title = compose_title(pair_title)), dots))
  }

  invisible(NULL)
}


#' Plot Nestimate Permutation Test Results
#'
#' Visualizes \code{net_permutation} objects from the Nestimate package.
#' Differs from \code{plot_permutation}: p_values and effect_size are already
#' p×p matrices (no edge-name parsing needed), and \code{directed} comes from
#' \code{x$x$directed}.
#'
#' @param x A \code{net_permutation} object (from Nestimate).
#' @param show_nonsig Logical: show non-significant edges? Default FALSE.
#' @param show_effect Logical: show effect size in parentheses? Default FALSE.
#' @param edge_positive_color Color for positive differences. Default \code{"#009900"}.
#' @param edge_negative_color Color for negative differences. Default \code{"#C62828"}.
#' @param edge_nonsig_color Color for non-significant edges. Default \code{"#888888"}.
#' @param edge_nonsig_style Line style for non-significant edges. Default 2L.
#' @param show_stars Logical: show significance stars? Default TRUE.
#' @param ... Additional arguments passed to \code{splot()}.
#'
#' @return Invisibly returns the plot.
#' @rdname splot
#' @export
splot.net_permutation <- function(x,
                                  show_nonsig         = FALSE,
                                  show_effect         = FALSE,
                                  edge_positive_color = "#009900",
                                  edge_negative_color = "#C62828",
                                  edge_nonsig_color   = "#888888",
                                  edge_nonsig_style   = 2L,
                                  show_stars          = TRUE,
                                  ...) {
  sig_level     <- x$alpha %||% 0.05
  diffs_true    <- x$diff
  diffs_sig     <- x$diff_sig
  p_matrix      <- x$p_values
  effect_matrix <- x$effect_size
  is_directed   <- isTRUE(x$x$directed)
  labels        <- x$x$nodes$label %||% rownames(diffs_true)

  if (is.null(diffs_true)) stop("Cannot find diff matrix in net_permutation object", call. = FALSE)

  weights_display <- if (show_nonsig) diffs_true else diffs_sig
  args            <- list(...)

  # Translate qgraph-style vsize to node_size
  if (!is.null(args$vsize) && is.null(args$node_size)) {
    args$node_size <- args$vsize
    args$vsize <- NULL
  }

  n_nodes         <- nrow(weights_display)

  # Round to match splot's internal weight_digits (default 2), so edge_idx
  # is consistent with the edge count splot sees when building the plot.
  weight_digits    <- args$weight_digits %||% 2
  weights_display  <- round(weights_display, weight_digits)

  if (is.null(args$layout))  args$layout  <- if (is_directed) "oval" else "spring"
  if (is.null(args$labels))  args$labels  <- labels
  if (is.null(args$directed)) args$directed <- is_directed
  if (is.null(args$show_arrows)) args$show_arrows <- is_directed

  if (is.null(args$edge_labels))             args$edge_labels             <- TRUE
  if (is.null(args$edge_label_size))         args$edge_label_size         <- 0.6
  if (is.null(args$edge_label_position))     args$edge_label_position     <- 0.35
  if (is.null(args$edge_label_halo))         args$edge_label_halo         <- TRUE
  if (is.null(args$node_size))               args$node_size               <- 7
  if (is.null(args$arrow_size))              args$arrow_size              <- 0.61
  if (is.null(args$node_fill))               args$node_fill               <- tna_color_palette(n_nodes)
  if (is.null(args$edge_label_leading_zero)) args$edge_label_leading_zero <- FALSE

  # For undirected networks splot only processes upper-triangle edges,
  # so per-edge arrays must use the same index set.
  if (is_directed) {
    edge_idx <- which(weights_display != 0, arr.ind = TRUE)
  } else {
    edge_idx <- which(weights_display != 0 & upper.tri(weights_display), arr.ind = TRUE)
  }
  n_edges  <- nrow(edge_idx)

  if (n_edges == 0) {
    message("No edges to display")
    return(invisible(NULL))
  }

  sig_mask <- if (!is.null(diffs_sig)) diffs_sig != 0 else matrix(FALSE, n_nodes, n_nodes)

  if (show_nonsig) {
    edge_colors    <- character(n_edges)
    edge_styles    <- numeric(n_edges)
    edge_fontfaces <- numeric(n_edges)
    edge_alphas    <- numeric(n_edges)

    for (k in seq_len(n_edges)) {
      i <- edge_idx[k, 1]; j <- edge_idx[k, 2]
      dv <- weights_display[i, j]
      if (sig_mask[i, j]) {
        edge_colors[k]    <- if (dv > 0) edge_positive_color else edge_negative_color
        edge_styles[k]    <- 1
        edge_fontfaces[k] <- 2
        edge_alphas[k]    <- 1
      } else {
        edge_colors[k]    <- edge_nonsig_color
        edge_styles[k]    <- edge_nonsig_style
        edge_fontfaces[k] <- 1
        edge_alphas[k]    <- 0.4
      }
    }
    args$edge_color          <- edge_colors
    args$edge_style          <- edge_styles
    args$edge_label_fontface <- edge_fontfaces
    args$edge_alpha          <- edge_alphas
  } else {
    args$edge_positive_color <- edge_positive_color
    args$edge_negative_color <- edge_negative_color
    args$edge_label_fontface <- 2
  }

  if (n_edges > 0 && (show_stars || show_effect)) {
    edge_labels_custom <- character(n_edges)
    for (k in seq_len(n_edges)) {
      i  <- edge_idx[k, 1]; j <- edge_idx[k, 2]
      w  <- weights_display[i, j]
      ws <- sub("^0\\.", ".", sprintf("%.2f", w))
      ws <- sub("^-0\\.", "-.", ws)

      stars_str <- ""
      if (show_stars && !is.null(p_matrix)) {
        stars_str <- get_significance_stars(p_matrix[i, j])
      }

      effect_str <- ""
      if (show_effect && !is.null(effect_matrix) && sig_mask[i, j]) {
        eff <- effect_matrix[i, j]
        if (!is.na(eff) && is.finite(eff)) effect_str <- sprintf(" (%.1f)", abs(eff))
      }

      edge_labels_custom[k] <- paste0(ws, stars_str, effect_str)
    }
    args$edge_labels <- edge_labels_custom
  }

  if (is.null(args$title)) {
    args$title <- if (show_nonsig) "Permutation Test: All Differences" else "Permutation Test: Significant Differences"
  }

  do.call(splot, c(list(x = weights_display), args))
}
