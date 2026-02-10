#' @title Permutation Test Plotting Methods
#' @description Plot methods for permutation test results from tna::permutation_test().
#'   These methods visualize network comparison with styling to distinguish
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
  splot.tna_permutation(x, ...)
}

#' @export
plot.group_tna_permutation <- function(x, ...) {
  splot.group_tna_permutation(x, ...)
}

#' Plot Permutation Test Results
#'
#' Visualizes permutation test results with styling to distinguish
#' significant from non-significant edge differences. Works with tna_permutation
#' objects from the tna package.
#'
#' @param x A tna_permutation object (from tna::permutation_test).
#' @param display Display mode:
#'   \itemize{
#'     \item "styled" (default): All edges with styling to distinguish significant/non-significant
#'     \item "significant": Only significant edge differences
#'     \item "full": All edge differences without significance styling
#'   }
#' @param pos_color Color for positive differences (x > y). Default "#009900" (green).
#' @param neg_color Color for negative differences (x < y). Default "#C62828" (red).
#' @param color_nonsig Color for non-significant edges in styled mode. Default "#888888" (grey).
#' @param edge_style_sig Line style for significant edges (1=solid). Default 1.
#' @param edge_style_nonsig Line style for non-significant edges (2=dashed). Default 2.
#' @param show_stars Logical: show significance stars (*, **, ***) on edges? Default TRUE.
#' @param show_effect Logical: show effect size in labels? Default FALSE.
#' @param alpha_nonsig Alpha for non-significant edges. Default 0.4.
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
#' Edge styling in "styled" mode:
#' \itemize{
#'   \item Significant positive: solid green, bold labels with stars
#'   \item Significant negative: solid red, bold labels with stars
#'   \item Non-significant: dashed grey, plain labels, lower alpha
#' }
#'
#' @examples
#' \dontrun{
#' # Permutation test between two TNA models (requires tna package)
#' library(tna)
#' perm <- permutation_test(model1, model2, iter = 1000)
#'
#' # Plot with default styling
#' splot(perm)
#'
#' # Show only significant differences
#' splot(perm, display = "significant")
#'
#' # Show effect sizes
#' splot(perm, show_effect = TRUE)
#' }
#'
#' @export
splot.tna_permutation <- function(x,
                                  display = c("styled", "significant", "full"),
                                  pos_color = "#009900",
                                  neg_color = "#C62828",
                                  color_nonsig = "#888888",
                                  edge_style_sig = 1,
                                  edge_style_nonsig = 2,
                                  show_stars = TRUE,
                                  show_effect = FALSE,
                                  alpha_nonsig = 0.4,
                                  ...) {
  display <- match.arg(display)

  level <- attr(x, "level") %||% 0.05
  labels <- attr(x, "labels")

  # Get difference matrices

  diffs_true <- x$edges$diffs_true
  diffs_sig <- x$edges$diffs_sig
  edge_stats <- x$edges$stats

  if (is.null(diffs_true)) {
    stop("Cannot find edge differences in permutation object", call. = FALSE)
  }

  # Get weights based on display mode
  weights <- switch(display,
    significant = diffs_sig,
    diffs_true
  )

  # Build args list
  args <- list(...)
  n_nodes <- nrow(weights)

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
  if (is.null(args$edge_label_position)) args$edge_label_position <- 0.5
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

  # Use matrix-based styling (splot handles per-edge conversion internally)
  # This avoids edge index ordering mismatches

  if (display == "styled" && n_edges > 0) {
    # Create edge color matrix: sig edges get pos/neg colors, non-sig get grey
    sig_mask <- diffs_sig != 0
    color_matrix <- matrix(color_nonsig, n_nodes, n_nodes)
    color_matrix[sig_mask & weights > 0] <- pos_color
    color_matrix[sig_mask & weights < 0] <- neg_color

    # Create style matrix: sig=solid, non-sig=dashed
    style_matrix <- matrix(edge_style_nonsig, n_nodes, n_nodes)
    style_matrix[sig_mask] <- edge_style_sig

    # Create alpha matrix
    alpha_matrix <- matrix(alpha_nonsig, n_nodes, n_nodes)
    alpha_matrix[sig_mask] <- 1

    args$edge_color <- color_matrix
    args$edge_style <- style_matrix
    args$edge_alpha <- alpha_matrix

  } else if (display == "significant") {
    # For "significant" mode, use pos/neg colors based on direction
    args$edge_positive_color <- pos_color
    args$edge_negative_color <- neg_color
  } else {
    # Full mode - use standard pos/neg coloring
    args$edge_positive_color <- pos_color
    args$edge_negative_color <- neg_color
  }

  # Stars for significance - pass matrix form
  if (show_stars && n_edges > 0 && !is.null(p_matrix)) {
    args$edge_label_p <- p_matrix
    args$edge_label_stars <- TRUE
    args$edge_label_template <- "{est}{stars}"
  }

  # Title
  if (is.null(args$title)) {
    args$title <- switch(display,
      styled = "Permutation Test: Network Difference",
      significant = "Permutation Test: Significant Differences",
      full = "Permutation Test: All Differences"
    )
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
#' @param ... Additional arguments passed to splot.tna_permutation().
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
#' splot(perm)
#'
#' # Plot specific comparison
#' splot(perm, i = "A vs. B")
#' }
#'
#' @export
splot.group_tna_permutation <- function(x, i = NULL, ...) {
  if (!is.null(i)) {
    # Plot single comparison
    elem <- x[[i]]
    if (is.null(elem)) {
      stop("Invalid index i=", i, call. = FALSE)
    }
    title <- if (is.character(i)) i else names(x)[i]
    return(splot.tna_permutation(elem, title = title, ...))
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
    splot.tna_permutation(x[[k]], title = title, ...)
  }

  invisible(NULL)
}

# Null coalescing operator (if not defined elsewhere)
`%||%` <- function(a, b) if (is.null(a)) b else a
