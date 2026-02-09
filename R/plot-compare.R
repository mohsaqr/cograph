#' @title Network Comparison Plots
#' @description Visualize differences between two networks.
#' @name plot-compare
NULL

#' Plot Network Difference
#'
#' Plots the difference between two networks (x - y) using splot.
#' Positive differences (x > y) are shown in green, negative (x < y) in red.
#' Optionally displays node-level differences (e.g., initial probabilities)
#' as donut charts.
#'
#' @param x First network: matrix, CographNetwork, tna, igraph object,
#'   OR a group_tna object. For group_tna with 2 groups, compares them directly.
#'   For more groups, plots all pairwise comparisons (or specify i, j).
#' @param y Second network: same type as x. Ignored if x is group_tna.
#' @param i Index/name of first group when x is group_tna. NULL for all pairs.
#' @param j Index/name of second group when x is group_tna. NULL for all pairs.
#' @param pos_color Color for positive differences (x > y). Default "#009900" (green).
#' @param neg_color Color for negative differences (x < y). Default "#C62828" (red).
#' @param labels Node labels. NULL uses rownames or defaults.
#' @param title Plot title. NULL for auto-generated title.
#' @param inits_x Node values for x (e.g., initial probabilities). NULL to auto-extract from tna.
#' @param inits_y Node values for y. NULL to auto-extract from tna.
#' @param show_inits Logical: show node differences as donuts? Default TRUE if inits available.
#' @param donut_inner_ratio Inner radius ratio for donut (0-1). Default 0.8.
#' @param force Logical: force plotting when more than 4 groups (many comparisons). Default FALSE.
#' @param ... Additional arguments passed to splot().
#'
#' @return Invisibly returns a list with difference matrix and inits difference.
#'
#' @details
#' The function computes element-wise subtraction of the weight matrices.
#' Edge colors indicate direction of difference:
#' - Green edges: x has higher weight than y
#' - Red edges: y has higher weight than x
#'
#' When initial probabilities (inits) are provided or extracted from tna objects,
#' nodes display donut charts showing the absolute difference, colored by direction:
#' - Green donut: x has higher initial probability
#' - Red donut: y has higher initial probability
#'
#' For lists of networks (e.g., group_tna), specify which elements to compare
#' using i and j parameters.
#'
#' @examples
#' \dontrun{
#' # Compare two adjacency matrices
#' m1 <- matrix(runif(25), 5, 5)
#' m2 <- matrix(runif(25), 5, 5)
#' plot_compare(m1, m2)
#'
#' # With node-level differences (e.g., initial probabilities)
#' inits1 <- c(0.3, 0.2, 0.2, 0.15, 0.15)
#' inits2 <- c(0.1, 0.4, 0.2, 0.2, 0.1)
#' plot_compare(m1, m2, inits_x = inits1, inits_y = inits2)
#'
#' # Compare tna objects (auto-extracts inits)
#' # plot_compare(tna_model1, tna_model2)
#'
#' # Compare group_tna (auto-detects, plots all pairs)
#' # plot_compare(group_tna_model)
#' # plot_compare(group_tna_model, i = 1, j = 2)  # specific pair
#' }
#'
#' @export
plot_compare <- function(x, y = NULL,
                         i = NULL,
                         j = NULL,
                         pos_color = "#009900",
                         neg_color = "#C62828",
                         labels = NULL,
                         title = NULL,
                         inits_x = NULL,
                         inits_y = NULL,
                         show_inits = NULL,
                         donut_inner_ratio = 0.8,
                         force = FALSE,
                         ...) {

  # Handle group_tna object (tna package integration)
  if (inherits(x, "group_tna")) {
    n_groups <- length(x)

    if (n_groups < 2) {
      stop("group_tna must contain at least 2 groups to compare")
    }

    # If i and j not specified, compare all pairs or just the two groups
    if (is.null(i) && is.null(j)) {
      if (n_groups == 2) {
        # Exactly 2 groups: compare them directly
        i <- 1L
        j <- 2L
      } else {
        # More than 2 groups: plot all pairwise comparisons
        n_pairs <- n_groups * (n_groups - 1) / 2

        if (n_groups > 4 && !force) {
          stop("group_tna has ", n_groups, " groups (", n_pairs, " pairwise comparisons). ",
               "Use force = TRUE to plot all comparisons, or specify i and j for a single pair.")
        }

        # Plot all pairs
        return(.plot_compare_all_pairs(x, pos_color, neg_color, labels,
                                       show_inits, donut_inner_ratio, ...))
      }
    }

    # Default i, j if only one specified
    if (is.null(i)) i <- 1L
    if (is.null(j)) j <- 2L

    # Extract groups i and j
    x_elem <- x[[i]]
    y_elem <- x[[j]]

    if (is.null(x_elem) || is.null(y_elem)) {
      stop("Invalid group indices i=", i, " or j=", j)
    }

    # Auto-generate title with group names
    if (is.null(title)) {
      nm <- names(x)
      if (!is.null(nm)) {
        name_i <- if (is.character(i)) i else nm[i]
        name_j <- if (is.character(j)) j else nm[j]
        if (!is.na(name_i) && !is.na(name_j)) {
          title <- paste0("Difference (", name_i, " - ", name_j, ")")
        }
      }
    }

    x <- x_elem
    y <- y_elem
  }

  # Handle plain list of networks
  else if (is.list(x) && !inherits(x, c("tna", "CographNetwork", "igraph"))) {
    if (length(x) < 2) {
      stop("List must contain at least 2 networks to compare")
    }

    # Default to first two if not specified
    if (is.null(i)) i <- 1L
    if (is.null(j)) j <- 2L

    x_elem <- x[[i]]
    y_elem <- x[[j]]

    if (is.null(x_elem) || is.null(y_elem)) {
      stop("Invalid indices i=", i, " or j=", j)
    }

    if (is.null(title)) {
      nm <- names(x)
      if (!is.null(nm)) {
        name_i <- if (is.character(i)) i else nm[i]
        name_j <- if (is.character(j)) j else nm[j]
        if (!is.na(name_i) && !is.na(name_j)) {
          title <- paste0("Difference (", name_i, " - ", name_j, ")")
        }
      }
    }

    x <- x_elem
    y <- y_elem
  }

  # Validate y is provided

  if (is.null(y)) {
    stop("y is required (or x must be a list with at least 2 elements)")
  }

  # Extract weight matrices
  x_mat <- .extract_weights(x)
  y_mat <- .extract_weights(y)

  # Auto-extract inits from tna objects
  if (is.null(inits_x)) inits_x <- .extract_inits(x)
  if (is.null(inits_y)) inits_y <- .extract_inits(y)

  # Validate dimensions
  if (!identical(dim(x_mat), dim(y_mat))) {
    stop("x and y must have the same dimensions")
  }

  # Check labels match
  x_labels <- rownames(x_mat)
  y_labels <- rownames(y_mat)

  if (!is.null(x_labels) && !is.null(y_labels)) {
    if (!identical(x_labels, y_labels)) {
      stop("x and y must have the same node labels")
    }
  }

  # Compute difference
  diff_mat <- x_mat - y_mat

  # Preserve labels
  if (!is.null(x_labels)) {
    rownames(diff_mat) <- x_labels
    colnames(diff_mat) <- x_labels
  }

  # Set labels
  if (is.null(labels)) {
    labels <- rownames(diff_mat)
    if (is.null(labels)) {
      labels <- seq_len(nrow(diff_mat))
    }
  }

  # Auto title
  if (is.null(title)) {
    title <- "Network Difference (x - y)"
  }

  # Handle inits/donut display
  donut_args <- list()
  inits_diff <- NULL

  has_inits <- !is.null(inits_x) && !is.null(inits_y)
  if (is.null(show_inits)) show_inits <- has_inits

  if (show_inits && has_inits) {
    # Validate inits lengths
    n_nodes <- nrow(diff_mat)
    if (length(inits_x) != n_nodes || length(inits_y) != n_nodes) {
      warning("inits_x/inits_y length doesn't match number of nodes, ignoring")
    } else {
      # Compute inits difference
      inits_diff <- inits_x - inits_y

      # Donut fill = absolute difference (capped at 1)
      donut_fill <- pmin(abs(inits_diff), 1)

      # Donut color = direction (green if x > y, red if x < y)
      donut_colors <- ifelse(inits_diff >= 0, pos_color, neg_color)

      donut_args <- list(
        node_shape = "donut",
        donut_fill = donut_fill,
        donut_color = donut_colors,
        donut_inner_ratio = donut_inner_ratio
      )
    }
  }

  # Merge donut args with user args (user args take precedence)
  extra_args <- list(...)
  plot_args <- c(
    list(
      x = diff_mat,
      layout = "oval",
      edge_positive_color = pos_color,
      edge_negative_color = neg_color,
      labels = labels,
      title = title
    ),
    donut_args
  )

  # User args override defaults
  for (nm in names(extra_args)) {
    plot_args[[nm]] <- extra_args[[nm]]
  }

  # Plot with splot
  do.call(splot, plot_args)

  invisible(list(
    weights = diff_mat,
    inits = inits_diff
  ))
}


#' Plot Comparison Heatmap
#'
#' Creates a heatmap visualization comparing two networks.
#'
#' @param x First network: matrix, CographNetwork, tna, or igraph object.
#' @param y Second network: same type as x. NULL to plot just x.
#' @param type What to display: "difference" (x - y), "x", or "y".
#' @param name_x Label for first network in title. Default "x".
#' @param name_y Label for second network in title. Default "y".
#' @param low_color Color for low/negative values. Default "blue".
#' @param mid_color Color for zero/middle values. Default "white".
#' @param high_color Color for high/positive values. Default "red".
#' @param limits Color scale limits. NULL for auto. Use c(-1, 1) for normalized.
#' @param show_values Logical: display values in cells? Default FALSE.
#' @param value_size Text size for cell values. Default 3.
#' @param digits Decimal places for cell values. Default 2.
#' @param title Plot title. NULL for auto-generated.
#' @param xlab X-axis label. Default "Target".
#' @param ylab Y-axis label. Default "Source".
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' m1 <- matrix(runif(25), 5, 5)
#' m2 <- matrix(runif(25), 5, 5)
#' rownames(m1) <- colnames(m1) <- LETTERS[1:5]
#' rownames(m2) <- colnames(m2) <- LETTERS[1:5]
#'
#' # Difference heatmap
#' plot_comparison_heatmap(m1, m2)
#'
#' # Show just one network
#' plot_comparison_heatmap(m1, type = "x")
#'
#' # With cell values
#' plot_comparison_heatmap(m1, m2, show_values = TRUE)
#' }
#'
#' @export
plot_comparison_heatmap <- function(x, y = NULL,
                                    type = c("difference", "x", "y"),
                                    name_x = "x",
                                    name_y = "y",
                                    low_color = "blue",
                                    mid_color = "white",
                                    high_color = "red",
                                    limits = NULL,
                                    show_values = FALSE,
                                    value_size = 3,
                                    digits = 2,
                                    title = NULL,
                                    xlab = "Target",
                                    ylab = "Source") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for heatmap. Install with: install.packages('ggplot2')")
  }

  type <- match.arg(type)

  # Extract weight matrices
  x_mat <- .extract_weights(x)

  if (type == "difference" || type == "y") {
    if (is.null(y)) {
      stop("y is required for type = '", type, "'")
    }
    y_mat <- .extract_weights(y)

    if (!identical(dim(x_mat), dim(y_mat))) {
      stop("x and y must have the same dimensions")
    }
  }

  # Get the matrix to display
  mat <- switch(type,
    "x" = x_mat,
    "y" = y_mat,
    "difference" = x_mat - y_mat
  )

  # Auto title
  if (is.null(title)) {
    title <- switch(type,
      "x" = paste0("Heatmap: ", name_x),
      "y" = paste0("Heatmap: ", name_y),
      "difference" = paste0("Difference Heatmap (", name_x, " - ", name_y, ")")
    )
  }

  # Get labels
  row_labels <- rownames(mat)
  col_labels <- colnames(mat)

  if (is.null(row_labels)) row_labels <- seq_len(nrow(mat))
  if (is.null(col_labels)) col_labels <- seq_len(ncol(mat))

  # Convert to long format
  df <- expand.grid(
    source = row_labels,
    target = col_labels,
    stringsAsFactors = FALSE
  )
  df$value <- as.vector(mat)

  # Preserve factor order
  df$source <- factor(df$source, levels = rev(row_labels))
  df$target <- factor(df$target, levels = col_labels)

  # Build plot
  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = .data$target,
    y = .data$source,
    fill = .data$value
  )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_gradient2(
      low = low_color,
      mid = mid_color,
      high = high_color,
      midpoint = 0,
      limits = limits,
      na.value = "grey50",
      name = "Value"
    ) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = ggplot2::element_text(size = 9),
      plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed()

  # Add cell values if requested
  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = round(.data$value, digits)),
      size = value_size,
      color = "black"
    )
  }

  p
}


#' Extract Weight Matrix from Network Object
#'
#' Internal helper to extract adjacency/weight matrix from various input types.
#'
#' @param x Network object (matrix, CographNetwork, tna, igraph, or list with $weights).
#' @return A numeric matrix.
#' @keywords internal
.extract_weights <- function(x) {
  if (is.matrix(x)) {
    return(x)
  }

  if (inherits(x, "CographNetwork")) {
    return(x$get_adjacency())
  }

  if (inherits(x, "tna")) {
    return(x$weights)
  }

  if (inherits(x, "igraph")) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("Package 'igraph' required for igraph objects")
    }
    return(igraph::as_adjacency_matrix(x, attr = "weight", sparse = FALSE))
  }

  # Handle list-like objects with $weights (e.g., elements of group_tna)
  if (is.list(x) && !is.null(x$weights) && is.matrix(x$weights)) {
    return(x$weights)
  }

  stop("x must be a matrix, CographNetwork, tna, or igraph object")
}


#' Extract Initial Probabilities from Network Object
#'
#' Internal helper to extract initial probabilities (inits) from tna objects.
#'
#' @param x Network object.
#' @return A numeric vector of initial probabilities, or NULL if not available.
#' @keywords internal
.extract_inits <- function(x) {
  if (inherits(x, "tna")) {
    return(x$inits)
  }

  # Handle list-like objects with $inits (e.g., elements of group_tna)
  if (is.list(x) && !is.null(x$inits)) {
    return(x$inits)
  }

  NULL
}


#' Plot All Pairwise Comparisons
#'
#' Internal helper to plot all pairwise comparisons from a group_tna object.
#'
#' @param x A group_tna object.
#' @param pos_color Color for positive differences.
#' @param neg_color Color for negative differences.
#' @param labels Node labels.
#' @param show_inits Show donut inits.
#' @param donut_inner_ratio Donut inner ratio.
#' @param ... Additional arguments passed to splot().
#' @return Invisibly returns list of comparison results.
#' @keywords internal
.plot_compare_all_pairs <- function(x, pos_color, neg_color, labels,
                                    show_inits, donut_inner_ratio, ...) {
  n_groups <- length(x)
  nm <- names(x)
  if (is.null(nm)) nm <- seq_len(n_groups)

  # Generate all pairs
  pairs <- utils::combn(n_groups, 2)
  n_pairs <- ncol(pairs)

  # Calculate grid layout
  ncol <- ceiling(sqrt(n_pairs))
  nrow <- ceiling(n_pairs / ncol)

  # Set up multi-panel plot
  old_par <- graphics::par(mfrow = c(nrow, ncol), mar = c(2, 2, 3, 1))
  on.exit(graphics::par(old_par))

  results <- list()

  for (k in seq_len(n_pairs)) {
    i <- pairs[1, k]
    j <- pairs[2, k]

    title <- paste0(nm[i], " - ", nm[j])

    # Extract networks
    x_net <- x[[i]]
    y_net <- x[[j]]

    # Extract weights and inits
    x_mat <- .extract_weights(x_net)
    y_mat <- .extract_weights(y_net)
    x_inits <- .extract_inits(x_net)
    y_inits <- .extract_inits(y_net)

    # Compute difference
    diff_mat <- x_mat - y_mat

    # Set labels
    plot_labels <- labels
    if (is.null(plot_labels)) {
      plot_labels <- rownames(diff_mat)
      if (is.null(plot_labels)) {
        plot_labels <- seq_len(nrow(diff_mat))
      }
    }

    # Handle inits/donut display
    donut_args <- list()
    inits_diff <- NULL

    has_inits <- !is.null(x_inits) && !is.null(y_inits)
    do_show_inits <- if (is.null(show_inits)) has_inits else show_inits

    if (do_show_inits && has_inits) {
      n_nodes <- nrow(diff_mat)
      if (length(x_inits) == n_nodes && length(y_inits) == n_nodes) {
        inits_diff <- x_inits - y_inits
        donut_fill <- pmin(abs(inits_diff), 1)
        donut_colors <- ifelse(inits_diff >= 0, pos_color, neg_color)

        donut_args <- list(
          node_shape = "donut",
          donut_fill = donut_fill,
          donut_color = donut_colors,
          donut_inner_ratio = donut_inner_ratio
        )
      }
    }

    # Build plot args
    extra_args <- list(...)
    plot_args <- c(
      list(
        x = diff_mat,
        layout = "oval",
        edge_positive_color = pos_color,
        edge_negative_color = neg_color,
        labels = plot_labels,
        title = title
      ),
      donut_args
    )

    for (arg_nm in names(extra_args)) {
      plot_args[[arg_nm]] <- extra_args[[arg_nm]]
    }

    # Plot
    do.call(splot, plot_args)

    results[[paste0(nm[i], "_vs_", nm[j])]] <- list(
      weights = diff_mat,
      inits = inits_diff
    )
  }

  invisible(results)
}
