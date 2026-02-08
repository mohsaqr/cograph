#' @title Bootstrap Analysis for Network Validation
#' @name bootstrap
NULL

#' Bootstrap Analysis
#'
#' @param x A tna, group_tna, or cograph_network object.
#' @param iter Number of bootstrap iterations (default 1000).
#' @param level Significance level (default 0.05).
#' @param method "stability" or "threshold".
#' @param threshold Threshold for method="threshold".
#' @param consistency_range Range for method="stability". Default c(0.75, 1.25).
#' @param ... Additional arguments.
#'
#' @return A cograph_boot object.
#' @export
bootstrap <- function(x, iter = 1000, level = 0.05, method = "stability",
                      threshold, consistency_range = c(0.75, 1.25), ...) {
 UseMethod("bootstrap")
}

#' @export
bootstrap.default <- function(x, ...) {
  stop(
    "bootstrap() requires a tna or cograph_network object.\n",
    "Use as_cograph() to convert matrices/edge lists, then set_raw_data() to attach data.",
    call. = FALSE
  )
}

#' @export
bootstrap.tna <- function(x, iter = 1000, level = 0.05, method = "stability",
                          threshold, consistency_range = c(0.75, 1.25), ...) {
  method <- match.arg(method, c("stability", "threshold"))

  if (is.null(x$data)) {
    stop("TNA object has no data. Bootstrap requires $data.", call. = FALSE)
  }

  # Use TNA internals
  d <- x$data
  type <- attr(x, "type") %||% "relative"
  scaling <- attr(x, "scaling") %||% "none"
  params <- attr(x, "params")

  # Get transition array via TNA
  model <- tna:::initialize_model(d, type, scaling, params, transitions = TRUE)
  trans <- model$trans
  alphabet <- attr(d, "alphabet")
  n <- nrow(d)
  a <- length(alphabet)

  # Original weights
  weights <- tna:::compute_weights(trans, type, scaling, a)
  dimnames(weights) <- list(alphabet, alphabet)

  # Default threshold
  if (missing(threshold)) {
    threshold <- unname(stats::quantile(weights, probs = 0.1))
  }

  # Pre-allocate
  weights_boot <- array(0, dim = c(iter, a, a))
  p_values <- matrix(0, a, a)
  idx <- seq_len(n)

  # Bootstrap loop (TNA-matching)
  if (method == "stability") {
    for (i in seq_len(iter)) {
      trans_boot <- trans[sample(idx, n, replace = TRUE), , , drop = FALSE]
      weights_boot[i, , ] <- tna:::compute_weights(trans_boot, type, scaling, a)
      p_values[] <- p_values +
        1L * (weights_boot[i, , ] <= weights * consistency_range[1]) +
        1L * (weights_boot[i, , ] >= weights * consistency_range[2])
    }
  } else {
    for (i in seq_len(iter)) {
      trans_boot <- trans[sample(idx, n, replace = TRUE), , , drop = FALSE]
      weights_boot[i, , ] <- tna:::compute_weights(trans_boot, type, scaling, a)
      p_values[] <- p_values + 1L * (weights_boot[i, , ] < threshold)
    }
  }

  # Bias-corrected p-values
  p_values <- (p_values + 1) / (iter + 1)

  # Summary stats
  weights_mean <- apply(weights_boot, c(2, 3), mean, na.rm = TRUE)
  weights_sd <- apply(weights_boot, c(2, 3), stats::sd, na.rm = TRUE)
  ci_lower <- apply(weights_boot, c(2, 3), stats::quantile, probs = level / 2, na.rm = TRUE)
  ci_upper <- apply(weights_boot, c(2, 3), stats::quantile, probs = 1 - level / 2, na.rm = TRUE)

  # Significant weights
  weights_sig <- (p_values < level) * weights

  # Set dimnames
  dimnames(p_values) <- list(alphabet, alphabet)
  dimnames(weights_mean) <- list(alphabet, alphabet)
  dimnames(weights_sd) <- list(alphabet, alphabet)
  dimnames(weights_sig) <- list(alphabet, alphabet)
  dimnames(ci_lower) <- list(alphabet, alphabet)
  dimnames(ci_upper) <- list(alphabet, alphabet)

  # Summary data frame
  weights_vec <- as.vector(weights)
  summary_df <- data.frame(
    from = rep(alphabet, times = a),
    to = rep(alphabet, each = a),
    weight = weights_vec,
    p_value = as.vector(p_values),
    sig = as.vector(p_values < level),
    cr_lower = as.vector(weights * consistency_range[1]),
    cr_upper = as.vector(weights * consistency_range[2]),
    ci_lower = as.vector(ci_lower),
    ci_upper = as.vector(ci_upper)
  )[weights_vec > 0, ]

  structure(
    list(
      weights_orig = weights,
      weights_sig = weights_sig,
      weights_mean = weights_mean,
      weights_sd = weights_sd,
      p_values = p_values,
      cr_lower = weights * consistency_range[1],
      cr_upper = weights * consistency_range[2],
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      summary = summary_df,
      iter = iter,
      level = level,
      method = method,
      consistency_range = consistency_range,
      model = x  # Store original TNA model for styling
    ),
    class = "cograph_boot"
  )
}

#' @export
bootstrap.group_tna <- function(x, iter = 1000, level = 0.05, method = "stability",
                                threshold, consistency_range = c(0.75, 1.25),
                                i = 1, ...) {
  if (i < 1 || i > length(x)) {
    stop("Index i must be between 1 and ", length(x), call. = FALSE)
  }
  result <- bootstrap.tna(x[[i]], iter, level, method, threshold, consistency_range, ...)
  result$group_name <- names(x)[i]
  result
}

#' @export
bootstrap.cograph_network <- function(x, iter = 1000, level = 0.05, method = "stability",
                                      threshold, consistency_range = c(0.75, 1.25),
                                      seed = NULL, ...) {
  if (is_tna_network(x)) {
    return(bootstrap.tna(x$tna$model, iter, level, method, threshold, consistency_range, ...))
  }

  if (is.null(x$raw_data) || is.null(x$estimator)) {
    stop(
      "Bootstrap requires raw case-level data to resample.\n",
      "Options:\n",
      "
- Use set_raw_data(net, data, estimator) to attach raw data\n",
      "
- Use disparity_filter() for edge significance without raw data\n",
      "See ?set_raw_data for details.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) set.seed(seed)

  method <- match.arg(method, c("stability", "threshold"))
  weights <- to_matrix(x)
  a <- nrow(weights)
  labels <- rownames(weights) %||% x$labels
  raw_data <- x$raw_data
  estimator <- x$estimator
  n <- nrow(raw_data)

  if (missing(threshold)) threshold <- unname(stats::quantile(weights, probs = 0.1))

  weights_boot <- array(0, dim = c(iter, a, a))
  p_values <- matrix(0, a, a)
  idx <- seq_len(n)

  if (method == "stability") {
    cr_lower <- pmin(weights * consistency_range[1], weights * consistency_range[2])
    cr_upper <- pmax(weights * consistency_range[1], weights * consistency_range[2])
    for (i in seq_len(iter)) {
      data_boot <- raw_data[sample(idx, n, replace = TRUE), , drop = FALSE]
      weights_boot[i, , ] <- estimator(data_boot)
      p_values[] <- p_values +
        1L * (weights_boot[i, , ] < cr_lower | weights_boot[i, , ] > cr_upper)
    }
  } else {
    for (i in seq_len(iter)) {
      data_boot <- raw_data[sample(idx, n, replace = TRUE), , drop = FALSE]
      weights_boot[i, , ] <- estimator(data_boot)
      p_values[] <- p_values + 1L * (weights_boot[i, , ] < threshold)
    }
  }

  p_values <- (p_values + 1) / (iter + 1)
  weights_mean <- apply(weights_boot, c(2, 3), mean, na.rm = TRUE)
  weights_sd <- apply(weights_boot, c(2, 3), stats::sd, na.rm = TRUE)
  ci_lower <- apply(weights_boot, c(2, 3), stats::quantile, probs = level / 2, na.rm = TRUE)
  ci_upper <- apply(weights_boot, c(2, 3), stats::quantile, probs = 1 - level / 2, na.rm = TRUE)
  weights_sig <- (p_values < level) * weights

  dimnames(p_values) <- dimnames(weights_mean) <- dimnames(weights_sd) <- list(labels, labels)
  dimnames(weights_sig) <- dimnames(ci_lower) <- dimnames(ci_upper) <- list(labels, labels)

  # Compute proper bounds for output (handle negative weights)
  out_cr_lower <- pmin(weights * consistency_range[1], weights * consistency_range[2])
  out_cr_upper <- pmax(weights * consistency_range[1], weights * consistency_range[2])
  dimnames(out_cr_lower) <- dimnames(out_cr_upper) <- list(labels, labels)

  # Build summary data frame
  weights_vec <- as.vector(weights)
  summary_df <- data.frame(
    from = rep(labels, times = a),
    to = rep(labels, each = a),
    weight = weights_vec,
    mean = as.vector(weights_mean),
    sd = as.vector(weights_sd),
    ci_lower = as.vector(ci_lower),
    ci_upper = as.vector(ci_upper),
    p_value = as.vector(p_values),
    significant = as.vector(p_values < level)
  )[weights_vec != 0, ]

  structure(
    list(
      weights_orig = weights,
      weights_sig = weights_sig,
      weights_mean = weights_mean,
      weights_sd = weights_sd,
      p_values = p_values,
      cr_lower = out_cr_lower,
      cr_upper = out_cr_upper,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      summary = summary_df,
      iter = iter,
      level = level,
      method = method,
      consistency_range = consistency_range,
      threshold = threshold,
      network = x  # Store original cograph_network for styling
    ),
    class = "cograph_boot"
  )
}

#' @export
print.cograph_boot <- function(x, ...) {
  cat("Bootstrap Analysis (", x$method, ", iter=", x$iter, ")\n", sep = "")
  cat("Significant edges:", sum(x$weights_sig != 0), "/", sum(x$weights_orig != 0), "\n")
  invisible(x)
}

#' @export
plot.cograph_boot <- function(x, ...) {
  splot(x$weights_sig, ...)
}

#' Plot Bootstrap Results
#'
#' Visualizes bootstrap analysis results with styling to distinguish
#' significant from non-significant edges. Uses the same styling as
#' splot() for TNA models.
#'
#' @param x A cograph_boot object.
#' @param display Display mode:
#'   \itemize{
#'     \item "styled" (default): All edges with styling to distinguish significant/non-significant
#'     \item "significant": Only significant edges
#'     \item "full": All edges without significance styling
#'     \item "ci": Show CI bands on edges
#'   }
#' @param edge_style_sig Line style for significant edges (1=solid). Default 1.
#' @param edge_style_nonsig Line style for non-significant edges (2=dashed). Default 2.
#' @param color_nonsig Color for non-significant edges. Default "#FF8C00" (orange).
#' @param show_ci Logical: overlay CI bands on edges? Default FALSE.
#' @param show_stars Logical: show significance stars (*, **, ***) on edges? Default TRUE.
#' @param width_by Optional: "cr_lower" to scale edge width by lower consistency range bound.
#' @param inherit_style Logical: inherit colors/layout from original TNA model? Default TRUE.
#' @param ... Additional arguments passed to splot().
#'
#' @return Invisibly returns the plot.
#' @export
splot.cograph_boot <- function(x,
                                display = c("styled", "significant", "full", "ci"),
                                edge_style_sig = 1,
                                edge_style_nonsig = 2,
                                color_nonsig = "#888888",  # Grey
                                show_ci = FALSE,
                                show_stars = TRUE,
                                width_by = NULL,
                                inherit_style = TRUE,
                                ...) {
  display <- match.arg(display)

  # Handle tna_bootstrap objects (may not have $level)
  level <- if (!is.null(x$level)) x$level else 0.05

  # Get weights based on display mode
  weights <- switch(display,
    significant = x$weights_sig,
    full = x$weights_orig,
    x$weights_orig  # styled and ci
  )

  # Build args list
  args <- list(...)
  n_nodes <- nrow(weights)

  # TNA edge color (same as from_tna uses)
  tna_edge_color <- "#003355"

  # Inherit styling from original model if available
  if (inherit_style) {
    # Check for TNA model (from bootstrap.tna)
    if (!is.null(x$model)) {
      if (is.null(args$layout)) args$layout <- "oval"
      if (is.null(args$labels) && !is.null(x$model$labels)) {
        args$labels <- x$model$labels
      }
      # Use model colors if available, otherwise tna_color_palette
      if (is.null(args$node_fill)) {
        if (!is.null(x$model$colors)) {
          args$node_fill <- x$model$colors
        } else {
          args$node_fill <- tna_color_palette(n_nodes)
        }
      }
      # Donut charts for initial state distribution
      if (is.null(args$donut_fill) && !is.null(x$model$inits)) {
        args$donut_fill <- as.numeric(x$model$inits)
        if (is.null(args$donut_inner_ratio)) args$donut_inner_ratio <- 0.8
      }
    }
    # Check for cograph_network (from bootstrap.cograph_network)
    if (!is.null(x$network)) {
      if (is.null(args$layout)) args$layout <- "spring"
      if (is.null(args$labels) && !is.null(x$network$labels)) {
        args$labels <- x$network$labels
      }
      if (is.null(args$node_fill)) {
        if (!is.null(x$network$node_fill)) {
          args$node_fill <- x$network$node_fill
        } else {
          args$node_fill <- tna_color_palette(n_nodes)
        }
      }
    }
  }

  # Compute edge indices from the SAME weights matrix passed to splot
  # This ensures edge ordering matches splot's internal edge list
  a <- nrow(weights)
  edge_idx <- which(weights != 0, arr.ind = TRUE)
  n_edges <- nrow(edge_idx)

  # Default TNA styling for ALL modes
  if (is.null(args$edge_color)) args$edge_color <- tna_edge_color
  if (is.null(args$edge_labels)) args$edge_labels <- TRUE
  if (is.null(args$edge_label_size)) args$edge_label_size <- 0.6
  if (is.null(args$edge_label_position)) args$edge_label_position <- 0.7
  if (is.null(args$node_size)) args$node_size <- 7
  if (is.null(args$arrow_size)) args$arrow_size <- 0.61
  # No leading zero for cleaner labels
  if (is.null(args$edge_label_leading_zero)) args$edge_label_leading_zero <- FALSE

  # For styled mode: differentiate sig/non-sig edges
  if (display == "styled" && n_edges > 0) {
    sig_mask <- x$p_values < level

    # Build per-edge arrays matching splot's edge order
    edge_styles <- numeric(n_edges)
    edge_colors <- character(n_edges)
    edge_fontfaces <- numeric(n_edges)
    edge_priorities <- numeric(n_edges)
    edge_label_positions <- numeric(n_edges)
    ci_vals <- numeric(n_edges)
    ci_colors <- character(n_edges)
    ci_scales <- numeric(n_edges)
    ci_alphas <- numeric(n_edges)

    ci_width <- x$ci_upper - x$ci_lower
    max_ci <- max(ci_width, na.rm = TRUE)

    for (k in seq_len(n_edges)) {
      i <- edge_idx[k, 1]
      j <- edge_idx[k, 2]
      # CI underlay for ALL edges
      ci_vals[k] <- pmax(ci_width[i, j] / max_ci, 0.3)  # minimum width

      if (sig_mask[i, j]) {
        # Significant edge - render on top
        edge_styles[k] <- edge_style_sig
        edge_colors[k] <- tna_edge_color
        edge_fontfaces[k] <- 2  # bold
        edge_priorities[k] <- 1  # high priority = on top
        edge_label_positions[k] <- 0.7  # closer to target
        ci_colors[k] <- tna_edge_color  # blue underlay
        ci_scales[k] <- 1.0  # tight
        ci_alphas[k] <- 0.35
      } else {
        # Non-significant edge - pink
        edge_styles[k] <- edge_style_nonsig
        edge_colors[k] <- "#E091AA"  # soft pink
        edge_fontfaces[k] <- 1  # plain
        edge_priorities[k] <- 0  # low priority = behind
        edge_label_positions[k] <- 0.4  # offset to reduce overlap
        ci_colors[k] <- "#E091AA"  # pink underlay
        ci_scales[k] <- 3.0  # bigger for visibility
        ci_alphas[k] <- 0.3
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
  if (show_stars && n_edges > 0) {
    args$edge_label_p <- x$p_values[edge_idx]
    args$edge_label_stars <- TRUE
    args$edge_label_template <- "{est}{stars}"
  }

  # CI labels and underlays
  if ((show_ci || display == "ci") && n_edges > 0) {
    args$edge_ci_lower <- x$ci_lower[edge_idx]
    args$edge_ci_upper <- x$ci_upper[edge_idx]
    args$edge_label_template <- "{est}{stars} [{low}, {up}]"

    ci_width <- x$ci_upper - x$ci_lower
    max_ci <- max(ci_width, na.rm = TRUE)
    if (max_ci > 0) {
      args$edge_ci <- ci_width[edge_idx] / max_ci
      args$edge_ci_alpha <- 0.35
      args$edge_ci_scale <- 2.5
      args$edge_ci_color <- "#800000"
      args$edge_ci_style <- 1
    }
  }

  # Width scaling by cr_lower
  if (!is.null(width_by) && width_by == "cr_lower") {
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

# Null coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a
