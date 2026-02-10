#' @title Bootstrap Plotting Methods
#' @description Plot methods for bootstrap analysis results from tna::bootstrap().
#'   These methods visualize bootstrapped TNA models with styling to distinguish
#'   significant from non-significant edges.
#' @name plot-bootstrap
NULL

# =============================================================================
# S3 Methods for tna_bootstrap (from tna package)
# =============================================================================

#' @export
print.tna_bootstrap <- function(x, ...) {
 method <- x$method %||% "bootstrap"
  iter <- x$iter %||% x$R %||% NA
  cat("TNA Bootstrap Analysis (", method, ", iter=", iter, ")\n", sep = "")
  if (!is.null(x$weights_sig) && !is.null(x$weights_orig)) {
    cat("Significant edges:", sum(x$weights_sig != 0), "/", sum(x$weights_orig != 0), "\n")
  }
  invisible(x)
}

#' @export
plot.tna_bootstrap <- function(x, ...) {
  splot.tna_bootstrap(x, ...)
}

#' Plot Bootstrap Results
#'
#' Visualizes bootstrap analysis results with styling to distinguish
#' significant from non-significant edges. Works with tna_bootstrap objects
#' from the tna package.
#'
#' @param x A tna_bootstrap object (from tna::bootstrap).
#' @param display Display mode:
#'   \itemize{
#'     \item "styled" (default): All edges with styling to distinguish significant/non-significant
#'     \item "significant": Only significant edges
#'     \item "full": All edges without significance styling
#'     \item "ci": Show CI bands on edges
#'   }
#' @param edge_style_sig Line style for significant edges (1=solid). Default 1.
#' @param edge_style_nonsig Line style for non-significant edges (2=dashed). Default 2.
#' @param color_nonsig Color for non-significant edges. Default "#888888" (grey).
#' @param show_ci Logical: overlay CI bands on edges? Default FALSE.
#' @param show_stars Logical: show significance stars (*, **, ***) on edges? Default TRUE.
#' @param width_by Optional: "cr_lower" to scale edge width by lower consistency range bound.
#' @param inherit_style Logical: inherit colors/layout from original TNA model? Default TRUE.
#' @param ... Additional arguments passed to splot().
#'
#' @return Invisibly returns the plot.
#'
#' @details
#' The function expects a tna_bootstrap object containing:
#' \itemize{
#'   \item \code{weights} or \code{weights_orig}: Original weight matrix
#'   \item \code{weights_sig}: Significant weights only (optional)
#'   \item \code{p_values}: P-value matrix
#'   \item \code{ci_lower}, \code{ci_upper}: Confidence interval bounds
#'   \item \code{level}: Significance level (default 0.05)
#'   \item \code{model}: Original TNA model for styling inheritance
#' }
#'
#' Edge styling in "styled" mode:
#' \itemize{
#'   \item Significant edges: solid dark blue, bold labels with stars, rendered on top
#'   \item Non-significant edges: dashed pink, plain labels, rendered behind
#' }
#'
#' @examples
#' \dontrun{
#' # Bootstrap a TNA model (requires tna package)
#' library(tna)
#' model <- tna(sequences)
#' boot <- bootstrap(model, iter = 1000)
#'
#' # Plot with default styling
#' splot(boot)
#'
#' # Show only significant edges
#' splot(boot, display = "significant")
#'
#' # Show CI bands
#' splot(boot, display = "ci")
#' }
#'
#' @export
splot.tna_bootstrap <- function(x,
                                display = c("styled", "significant", "full", "ci"),
                                edge_style_sig = 1,
                                edge_style_nonsig = 2,
                                color_nonsig = "#888888",
                                show_ci = FALSE,
                                show_stars = TRUE,
                                width_by = NULL,
                                inherit_style = TRUE,
                                ...) {
  display <- match.arg(display)

  # Handle different tna_bootstrap structures
  level <- x$level %||% 0.05

  # Get original weights (tna uses $weights, older versions might use $weights_orig)
  weights_orig <- x$weights_orig %||% x$weights %||% x$model$weights
  if (is.null(weights_orig)) {
    stop("Cannot find weight matrix in bootstrap object", call. = FALSE)
  }

  # Get significant weights if available, otherwise compute from p_values
  weights_sig <- x$weights_sig
  if (is.null(weights_sig) && !is.null(x$p_values)) {
    weights_sig <- weights_orig * (x$p_values < level)
  }

  # Get weights based on display mode
  weights <- switch(display,
    significant = weights_sig %||% weights_orig,
    full = weights_orig,
    weights_orig
  )

  # Build args list
  args <- list(...)
  n_nodes <- nrow(weights)

  # TNA edge color
  tna_edge_color <- "#003355"

  # Inherit styling from original model if available
  if (inherit_style && !is.null(x$model)) {
    if (is.null(args$layout)) args$layout <- "oval"

    # Labels from model
    if (is.null(args$labels)) {
      args$labels <- x$model$labels %||% rownames(weights)
    }

    # Node colors
    if (is.null(args$node_fill)) {
      model_colors <- attr(x$model$data, "colors") %||% x$model$colors
      args$node_fill <- model_colors %||% tna_color_palette(n_nodes)
    }

    # Donut charts for initial state distribution
    if (is.null(args$donut_fill) && !is.null(x$model$inits)) {
      args$donut_fill <- as.numeric(x$model$inits)
      if (is.null(args$donut_inner_ratio)) args$donut_inner_ratio <- 0.8
    }
  }

  # Compute edge indices
  edge_idx <- which(weights != 0, arr.ind = TRUE)
  n_edges <- nrow(edge_idx)

  # Default TNA styling
  if (is.null(args$edge_color)) args$edge_color <- tna_edge_color
  if (is.null(args$edge_labels)) args$edge_labels <- TRUE
  if (is.null(args$edge_label_size)) args$edge_label_size <- 0.6
  if (is.null(args$edge_label_position)) args$edge_label_position <- 0.7
  if (is.null(args$edge_label_halo)) args$edge_label_halo <- TRUE
  if (is.null(args$node_size)) args$node_size <- 7
  if (is.null(args$arrow_size)) args$arrow_size <- 0.61
  if (is.null(args$edge_label_leading_zero)) args$edge_label_leading_zero <- FALSE

  # For styled mode: differentiate sig/non-sig edges
  if (display == "styled" && n_edges > 0 && !is.null(x$p_values)) {
    sig_mask <- x$p_values < level

    # Build per-edge arrays
    edge_styles <- numeric(n_edges)
    edge_colors <- character(n_edges)
    edge_fontfaces <- numeric(n_edges)
    edge_priorities <- numeric(n_edges)
    edge_label_positions <- numeric(n_edges)
    ci_vals <- numeric(n_edges)
    ci_colors <- character(n_edges)
    ci_scales <- numeric(n_edges)
    ci_alphas <- numeric(n_edges)

    # CI width for underlays
    ci_width <- if (!is.null(x$ci_upper) && !is.null(x$ci_lower)) {
      x$ci_upper - x$ci_lower
    } else {
      matrix(1, nrow(weights), ncol(weights))
    }
    max_ci <- max(ci_width, na.rm = TRUE)

    for (k in seq_len(n_edges)) {
      i <- edge_idx[k, 1]
      j <- edge_idx[k, 2]

      if (sig_mask[i, j]) {
        # Significant edge - solid blue, bold, no underlay
        edge_styles[k] <- edge_style_sig
        edge_colors[k] <- tna_edge_color
        edge_fontfaces[k] <- 2
        edge_priorities[k] <- 1
        edge_label_positions[k] <- 0.7
        ci_vals[k] <- 0
        ci_colors[k] <- NA
        ci_scales[k] <- 0
        ci_alphas[k] <- 0
      } else {
        # Non-significant edge - dashed pink, same-width underlay
        edge_styles[k] <- edge_style_nonsig
        edge_colors[k] <- "#E091AA"
        edge_fontfaces[k] <- 1
        edge_priorities[k] <- 0
        edge_label_positions[k] <- 0.4
        ci_vals[k] <- 0.5
        ci_colors[k] <- "#E091AA"
        ci_scales[k] <- 1.0  # Same width as edge
        ci_alphas[k] <- 0.4
      }
    }

    args$edge_style <- edge_styles
    args$edge_color <- edge_colors
    args$edge_label_fontface <- edge_fontfaces
    args$edge_label_position <- edge_label_positions
    args$edge_priority <- edge_priorities
    args$edge_ci <- ci_vals
    args$edge_ci_alpha <- ci_alphas
    args$edge_ci_scale <- ci_scales
    args$edge_ci_color <- ci_colors
    args$edge_ci_style <- 1
  }

  # Stars for significance
  if (show_stars && n_edges > 0 && !is.null(x$p_values)) {
    args$edge_label_p <- x$p_values[edge_idx]
    args$edge_label_stars <- TRUE
    args$edge_label_template <- "{est}{stars}"
  }

  # CI labels (add CI bounds to label template)
  if ((show_ci || display == "ci") && n_edges > 0 &&
      !is.null(x$ci_lower) && !is.null(x$ci_upper)) {
    args$edge_ci_lower <- x$ci_lower[edge_idx]
    args$edge_ci_upper <- x$ci_upper[edge_idx]
    args$edge_label_template <- "{est}{stars} [{low}, {up}]"

    # For CI mode: thickness reflects relative uncertainty
    if (display == "ci" && !is.null(x$p_values)) {
      sig_mask <- x$p_values < level
      ci_width <- x$ci_upper - x$ci_lower

      # Relative uncertainty = CI width / edge weight (coefficient of variation)
      # This is scale-invariant: works same for weights 0.1 or 1000
      rel_uncertainty <- ci_width / (abs(weights_orig) + 1e-10)

      # Cap extreme values and normalize to 0-1 range
      rel_uncertainty[!is.finite(rel_uncertainty)] <- 0
      rel_uncertainty <- pmin(rel_uncertainty, 10)  # Cap at 10x relative uncertainty
      max_rel <- max(rel_uncertainty[weights_orig != 0], na.rm = TRUE)
      if (!is.finite(max_rel) || max_rel == 0) max_rel <- 1

      ci_vals <- numeric(n_edges)
      ci_colors <- character(n_edges)
      ci_scales <- numeric(n_edges)
      ci_alphas <- numeric(n_edges)

      for (k in seq_len(n_edges)) {
        i <- edge_idx[k, 1]
        j <- edge_idx[k, 2]

        # Normalize to 0.2-1.0 range, capped
        rel_unc <- rel_uncertainty[i, j] / max_rel
        ci_vals[k] <- pmin(pmax(rel_unc, 0.2), 1.0)

        if (sig_mask[i, j]) {
          ci_colors[k] <- tna_edge_color
          ci_scales[k] <- 1.0
          ci_alphas[k] <- 0.25
        } else {
          # Non-sig: thicker underlay based on relative uncertainty
          ci_colors[k] <- "#E091AA"
          ci_scales[k] <- 12.0
          ci_alphas[k] <- 0.4
        }
      }

      args$edge_ci <- ci_vals
      args$edge_ci_alpha <- ci_alphas
      args$edge_ci_scale <- ci_scales
      args$edge_ci_color <- ci_colors
      args$edge_ci_style <- 1
    }
  }

  # Width scaling by cr_lower
  if (!is.null(width_by) && width_by == "cr_lower" && !is.null(x$cr_lower)) {
    weights <- x$cr_lower
    edge_idx_cr <- which(weights != 0, arr.ind = TRUE)
    cr_vals <- abs(weights[edge_idx_cr])
    cr_max <- max(cr_vals, na.rm = TRUE)
    if (cr_max > 0) {
      args$edge_width <- 0.5 + (cr_vals / cr_max) * 3.5
    }
    args$edge_style <- 1
    args$edge_color <- tna_edge_color
    args$edge_label_fontface <- NULL
    args$edge_label_template <- NULL
    args$edge_label_stars <- NULL
    args$edge_label_p <- NULL
  }

  do.call(splot, c(list(x = weights), args))
}

# Null coalescing operator (if not defined elsewhere)
`%||%` <- function(a, b) if (is.null(a)) b else a
