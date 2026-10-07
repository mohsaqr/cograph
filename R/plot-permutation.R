#' @title Permutation Test Plotting
#' @description Plot permutation test results from tna::permutation_test().
#'   Visualizes network comparison with styling to distinguish
#'   significant from non-significant edge differences.
#' @name plot-permutation
#' @keywords internal
#' @noRd
NULL

#' @rdname plot-results
#' @export
splot.tna_permutation <- function(x, ...) {
  plot_permutation(x, ...)
}

#' @rdname plot-results
#' @export
splot.group_tna_permutation <- function(x, ...) {
  plot_group_permutation(x, ...)
}

#' @rdname plot-results
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
  edge_labels_user <- "edge_labels" %in% names(args)
  labels_enabled <- !identical(args[["edge_labels"]], FALSE)
  template_labels <- !is.null(args[["edge_label_template"]]) ||
    (!is.null(args[["edge_label_style"]]) && !identical(args[["edge_label_style"]], "none"))

  # Translate qgraph-style vsize to node_size
  if (!is.null(args[["vsize"]]) && is.null(args[["node_size"]])) {
    args$node_size <- args$vsize
    args$vsize <- NULL
  }

  n_nodes <- nrow(weights)

  # Apply same rounding as splot to match edge counts
  weight_digits <- args[["weight_digits"]] %||% 2
  weights       <- round(weights, weight_digits)

  # Default layout
  if (is.null(args[["layout"]])) args[["layout"]] <- "oval"

  # Labels
  if (is.null(args[["labels"]]) && !is.null(labels)) {
    args$labels <- labels
  }
  if (is.null(args[["labels"]])) {
    args$labels <- rownames(weights)
  }

  # Default styling
  if (is.null(args[["edge_labels"]])) args$edge_labels <- TRUE
  if (is.null(args[["edge_label_size"]])) args$edge_label_size <- 0.6
  if (is.null(args[["edge_label_position"]])) args$edge_label_position <- 0.35
  if (is.null(args[["edge_label_halo"]])) args$edge_label_halo <- TRUE
  if (is.null(args[["node_size"]])) args$node_size <- 7
  if (is.null(args[["arrow_size"]])) args$arrow_size <- 0.61
  if (is.null(args[["edge_label_leading_zero"]])) args$edge_label_leading_zero <- FALSE

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

  if (labels_enabled && n_edges > 0 && !is.null(p_matrix)) {
    args$edge_label_p <- p_matrix[edge_idx]
    p_diff_mat <- .aligned_p_difference(x$p_difference, weights)
    if (!is.null(p_diff_mat) && is.null(args[["edge_label_p_diff"]])) {
      args$edge_label_p_diff <- p_diff_mat[edge_idx]
    }
    if (template_labels && show_stars) {
      args$edge_label_stars <- TRUE
    }
  }

  # Build custom edge labels with optional effect size. Leave explicit label
  # vectors/templates alone so users can show p-values or suppress labels.
  if (labels_enabled && !template_labels && n_edges > 0 &&
      (show_stars || show_effect) &&
      (!edge_labels_user || isTRUE(args[["edge_labels"]]))) {
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
  if (is.null(args[["title"]])) {
    args[["title"]] <- if (show_nonsig) {
      "Permutation Test: All Differences"
    } else {
      "Permutation Test: Significant Differences"
    }
  }

  # Node colors from tna model
  node_colors <- attr(x, "colors")
  if (!is.null(node_colors) && is.null(args[["node_fill"]])) {
    args$node_fill <- node_colors
  }

  do.call(splot, c(list(x = weights), args))
}


#' @rdname plot-results
#' @export
plot_group_permutation <- function(x, i = NULL, combined = TRUE, ...) {
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

  if (combined) {
    # Calculate grid layout
    ncol <- ceiling(sqrt(n_pairs))
    nrow <- ceiling(n_pairs / ncol)

    # Set up multi-panel plot
    old_par <- graphics::par(mfrow = c(nrow, ncol), mar = c(2, 2, 3, 1))
    on.exit(graphics::par(old_par), add = TRUE)
  }

  pair_names <- names(x)
  for (k in seq_len(n_pairs)) {
    pair_title <- pair_names[k] %||% paste("Comparison", k)
    do.call(plot_permutation,
            c(list(x[[k]], title = compose_title(pair_title)), dots))
  }

  invisible(NULL)
}


#' @details
#' \code{splot.net_permutation()} plots the edge differences of a
#' \code{net_permutation} object from Nestimate, with significant
#' differences highlighted. The direction of the network is taken from
#' \code{x$x$directed}.
#'
#' @param show_nonsig Logical. Show non-significant edges of a
#'   \code{net_permutation} plot. Default \code{FALSE}.
#' @param show_effect Logical. Show the effect size in parentheses in the edge
#'   labels of a \code{net_permutation} plot. Default \code{FALSE}.
#' @param edge_nonsig_color Color of non-significant edges. Default
#'   \code{"#888888"}.
#' @param edge_nonsig_style Line type of non-significant edges. Default 2.
#' @param show_stars Logical. Show significance stars in the edge labels of a
#'   \code{net_bootstrap} or \code{net_permutation} plot. Default
#'   \code{TRUE}.
#'
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
  edge_labels_user <- "edge_labels" %in% names(args)
  labels_enabled <- !identical(args[["edge_labels"]], FALSE)
  template_labels <- !is.null(args[["edge_label_template"]]) ||
    (!is.null(args[["edge_label_style"]]) && !identical(args[["edge_label_style"]], "none"))

  # Translate qgraph-style vsize to node_size
  if (!is.null(args[["vsize"]]) && is.null(args[["node_size"]])) {
    args$node_size <- args$vsize
    args$vsize <- NULL
  }

  n_nodes         <- nrow(weights_display)

  # Round to match splot's internal weight_digits (default 2), so edge_idx
  # is consistent with the edge count splot sees when building the plot.
  weight_digits    <- args[["weight_digits"]] %||% 2
  weights_display  <- round(weights_display, weight_digits)

  if (is.null(args[["layout"]]))  args[["layout"]]  <- if (is_directed) "oval" else "spring"
  if (is.null(args[["labels"]]))  args$labels  <- labels
  if (is.null(args[["directed"]])) args$directed <- is_directed
  if (is.null(args[["show_arrows"]])) args$show_arrows <- is_directed

  if (is.null(args[["edge_labels"]]))             args$edge_labels             <- TRUE
  if (is.null(args[["edge_label_size"]]))         args$edge_label_size         <- 0.6
  if (is.null(args[["edge_label_position"]]))     args$edge_label_position     <- 0.35
  if (is.null(args[["edge_label_halo"]]))         args$edge_label_halo         <- TRUE
  if (is.null(args[["node_size"]]))               args$node_size               <- 7
  if (is.null(args[["arrow_size"]]))              args$arrow_size              <- 0.61
  if (is.null(args[["node_fill"]]))               args$node_fill               <- tna_color_palette(n_nodes)
  if (is.null(args[["edge_label_leading_zero"]])) args$edge_label_leading_zero <- FALSE

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

  if (labels_enabled && n_edges > 0 && !is.null(p_matrix)) {
    args$edge_label_p <- p_matrix[edge_idx]
    p_diff_mat <- .aligned_p_difference(x$p_difference, weights_display)
    if (!is.null(p_diff_mat) && is.null(args[["edge_label_p_diff"]])) {
      args$edge_label_p_diff <- p_diff_mat[edge_idx]
    }
    if (template_labels && show_stars) {
      args$edge_label_stars <- TRUE
    }
  }

  if (labels_enabled && !template_labels && n_edges > 0 &&
      (show_stars || show_effect) &&
      (!edge_labels_user || isTRUE(args[["edge_labels"]]))) {
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

  if (is.null(args[["title"]])) {
    args[["title"]] <- if (show_nonsig) "Permutation Test: All Differences" else "Permutation Test: Significant Differences"
  }

  do.call(splot, c(list(x = weights_display), args))
}

#' Validate and align a p_difference matrix to a reference weights matrix
#'
#' Returns a matrix in the reference's node order, or NULL when p_difference
#' is absent or unusable (not a matrix, wrong dimensions). Positional
#' edge-index subsetting on the result is then guaranteed valid; when both
#' matrices carry full dimnames over the same names, the values are
#' re-ordered by name so a producer may store p_difference in any node order.
#'
#' @param p_diff Candidate probability-of-difference matrix (or anything).
#' @param ref Reference weights matrix whose node order edge_idx follows.
#' @return A dim-matched matrix or NULL.
#' @noRd
.aligned_p_difference <- function(p_diff, ref) {
  if (!is.matrix(p_diff) || !identical(dim(p_diff), dim(ref))) {
    return(NULL)
  }

  rn <- rownames(ref)
  cn <- colnames(ref)
  if (!is.null(rn) && !is.null(cn) &&
      !anyDuplicated(rn) && !anyDuplicated(cn) &&
      !is.null(rownames(p_diff)) && !is.null(colnames(p_diff)) &&
      all(rn %in% rownames(p_diff)) && all(cn %in% colnames(p_diff))) {
    return(p_diff[rn, cn, drop = FALSE])
  }

  p_diff
}
