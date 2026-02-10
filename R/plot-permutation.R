#' @title Permutation Test Plotting
#' @description Plot permutation test results from tna::permutation_test().
#'   Visualizes network comparison with styling to distinguish
#'   significant from non-significant edge differences.
#' @name plot-permutation
NULL

# =============================================================================
# S3 Methods for tna_permutation (from tna package)
# =============================================================================

#' @export
print.tna_permutation <- function(x, ...) {
  level <- attr(x, "level") %||% 0.05
  n_sig <- sum(x$edges$diffs_sig != 0)
  n_total <- sum(x$edges$diffs_true != 0)
  cat("TNA Permutation Test (level=", level, ")\n", sep = "")
  cat("Significant edge differences:", n_sig, "/", n_total, "\n")
  if (!is.null(x$centralities)) {
    n_cent_sig <- sum(x$centralities$diffs_sig[, -1] != 0)
    n_cent_total <- sum(x$centralities$diffs_true[, -1] != 0)
    cat("Significant centrality differences:", n_cent_sig, "/", n_cent_total, "\n")
  }
  invisible(x)
}

#' @export
print.group_tna_permutation <- function(x, ...) {
  cat("Group TNA Permutation Tests (", length(x), " comparisons)\n", sep = "")
  for (nm in names(x)) {
    n_sig <- sum(x[[nm]]$edges$diffs_sig != 0)
    n_total <- sum(x[[nm]]$edges$diffs_true != 0)
    cat("  ", nm, ": ", n_sig, "/", n_total, " significant edges\n", sep = "")
  }
  invisible(x)
}

#' @export
plot.tna_permutation <- function(x, ...) {
  plot_permutation(x, ...)
}

#' @export
plot.group_tna_permutation <- function(x, ...) {
  plot_group_permutation(x, ...)
}

#' @export
splot.tna_permutation <- function(x, ...) {
  plot_permutation(x, ...)
}

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
#' # Permutation test between two TNA models (requires tna package)
#' library(tna)
#' perm <- permutation_test(model1, model2, iter = 1000)
#'
#' # Plot significant edges only (default)
#' plot_permutation(perm)
#'
#' # Show effect sizes
#' plot_permutation(perm, show_effect = TRUE)
#'
#' # Include non-significant edges
#' plot_permutation(perm, show_nonsig = TRUE)
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
  n_nodes <- nrow(weights)

  # Apply same rounding as splot to match edge counts
  weight_digits <- args$weight_digits
  if (is.null(weight_digits)) weight_digits <- 2  # splot default
  if (!is.null(weight_digits)) {
    weights <- round(weights, weight_digits)
  }

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

    # edge_stats has edge_name like "A -> B"
    for (k in seq_len(nrow(edge_stats))) {
      # Parse edge name
      edge_name <- edge_stats$edge_name[k]
      parts <- strsplit(edge_name, " -> ")[[1]]
      if (length(parts) == 2) {
        from_idx <- which(rownames(diffs_true) == parts[1])
        to_idx <- which(colnames(diffs_true) == parts[2])
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
        p <- p_matrix[i, j]
        if (!is.na(p)) {
          if (p < 0.001) stars_str <- "***"
          else if (p < 0.01) stars_str <- "**"
          else if (p < 0.05) stars_str <- "*"
        }
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
#' library(tna)
#' mod <- group_tna(data, group = groups)
#' perm <- permutation_test(mod, iter = 1000)
#'
#' # Plot all comparisons
#' plot_group_permutation(perm)
#'
#' # Plot specific comparison
#' plot_group_permutation(perm, i = "A vs. B")
#' }
#'
#' @export
plot_group_permutation <- function(x, i = NULL, ...) {
  if (!is.null(i)) {
    # Plot single comparison
    elem <- x[[i]]
    if (is.null(elem)) {
      stop("Invalid index i=", i, call. = FALSE)
    }
    title <- if (is.character(i)) i else names(x)[i]
    return(plot_permutation(elem, title = title, ...))
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
  on.exit(graphics::par(old_par))

  pair_names <- names(x)
  for (k in seq_len(n_pairs)) {
    title <- pair_names[k] %||% paste("Comparison", k)
    plot_permutation(x[[k]], title = title, ...)
  }

  invisible(NULL)
}

# Null coalescing operator (if not defined elsewhere)
`%||%` <- function(a, b) if (is.null(a)) b else a
