#' @title Network Comparison Plots
#' @description Visualize differences between two networks.
#' @name plot-compare
#' @keywords internal
#' @noRd
NULL

#' Plot Network Difference
#'
#' Plots the difference between two networks (x - y) with \code{\link{splot}}.
#' Positive differences (x > y) are shown in \code{pos_color} and negative
#' differences (x < y) in \code{neg_color}. Node-level differences, such as
#' initial probabilities, can be shown as donut charts.
#'
#' @param x First network: matrix, \code{cograph_network}, \code{tna},
#'   \code{igraph}, list with a matrix \code{weights} component, plain list of
#'   networks, or \code{group_tna}. For a \code{group_tna} with two groups,
#'   the groups are compared directly. With more groups and \code{i} and
#'   \code{j} both \code{NULL}, all pairwise comparisons are plotted.
#' @param y Second network, of the same type as \code{x}. Ignored when \code{x}
#'   is a list or \code{group_tna}.
#' @param i Index or name of the first group when \code{x} is a
#'   \code{group_tna} or a plain list. \code{NULL} selects the first element,
#'   except that all pairs are plotted for a \code{group_tna} of more than two
#'   groups when \code{j} is also \code{NULL}.
#' @param j Index or name of the second group, with the same rules as
#'   \code{i}. \code{NULL} selects the second element.
#' @param pos_color Color for positive differences (x > y).
#' @param neg_color Color for negative differences (x < y).
#' @param labels Node labels. \code{NULL} uses the row names of the weight
#'   matrix, or node indices when there are none.
#' @param title Plot title. \code{NULL} uses \code{"Network Difference (x - y)"},
#'   or the two group names when they are available.
#' @param inits_x Node values for x (e.g., initial probabilities). \code{NULL}
#'   extracts them from a tna object.
#' @param inits_y Node values for y. \code{NULL} extracts them from a tna
#'   object.
#' @param show_inits Logical. Show node differences as donuts? \code{NULL}
#'   (default) shows them when node values are available for both networks.
#' @param donut_inner_ratio Inner radius ratio for donut (0-1).
#' @param difference Logical. If \code{TRUE}, \code{x} is treated as an
#'   already-subtracted difference network and \code{y} is ignored with a
#'   warning. A \code{tna_comparison} object (from \code{tna::compare()}) or
#'   a \code{netdifference} object is detected automatically and its
#'   difference matrix is used.
#' @param force Logical. Plot all pairs for a \code{group_tna} with more than
#'   four groups. Without it, such input stops with an error.
#' @param combined Logical. When \code{TRUE} (default) and \code{x} is a
#'   multi-group input that triggers all-pairs plotting, panels are arranged in
#'   a grid via \code{graphics::par(mfrow = ...)}. \code{FALSE} plots into a
#'   layout the caller has already configured (e.g. via
#'   \code{\link{panel_layout}()}). It has no effect for a single pair.
#' @param ... Additional arguments passed to \code{\link{splot}}. They
#'   override the defaults set here (\code{layout = "oval"},
#'   \code{minimum = 0} and the TNA or psychometric styling preset).
#'
#' @return Invisibly, a list with elements \code{weights} (the element-wise
#'   difference matrix \code{x - y}) and \code{inits} (the node-value
#'   difference, or \code{NULL} when no node values were available). For the
#'   \code{group_tna} all-pairs path, a named list of such lists with one
#'   element per pair, named \code{"<group_i>_vs_<group_j>"}.
#'
#' @details
#' The weight matrices are subtracted element-wise. Both networks must have
#' the same dimensions and, when present, the same node labels; otherwise the
#' function stops with an error. A directed difference (or tna input) is
#' styled with the TNA preset and an undirected difference with the
#' psychometric preset.
#'
#' When node values (inits) are given or extracted from tna objects, each node
#' is shown as a donut whose filled fraction is the absolute difference,
#' capped at 1, colored \code{pos_color} when x is higher and
#' \code{neg_color} when y is higher.
#'
#' \code{plot_compare()} is an alias of \code{plot_difference()}.
#'
#' @examples
#' plot_difference(regulation_net, t(regulation_net))
#'
#' @seealso \code{\link{plot_compare}}, \code{\link{plot_comparison_heatmap}}
#' @export
plot_difference <- function(x, y = NULL,
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
                         combined = TRUE,
                         # `difference` is new in 2.4.x and must stay AFTER
                         # `combined`: through the released 2.3.6 signature the
                         # 14th positional argument is `combined`, and
                         # plot_compare() forwards positionally via `...`.
                         # Inserting ahead of it silently rebinds a caller's
                         # 14th argument to `difference`, which makes the
                         # function treat `x` as a pre-computed difference and
                         # discard `y`. Append new arguments; never insert.
                         difference = FALSE,
                         ...) {

  # Consume a pre-computed difference: a tna_comparison object (uses its
  # $difference_matrix) or, with difference = TRUE, x is treated as the already
  # subtracted matrix/network. Modeled as x - 0 so the whole downstream
  # pipeline (styling, sign coloring) is reused unchanged.
  p_diff_matrix <- NULL
  if (inherits(x, c("tna_comparison", "netdifference")) ||
      (is.list(x) && is.matrix(x$difference_matrix))) {
    # netdifference carries the DISPLAY matrix in $weights (e.g. only the
    # supported differences when coerced with significant_only = TRUE) and the
    # full difference in $difference_matrix; prefer the display matrix.
    # Keep the probability-of-difference matrix (Bayesian coercions) for the
    # {p_diff} label placeholder before x is reduced to a plain matrix.
    if (is.list(x) && is.matrix(x$p_difference)) p_diff_matrix <- x$p_difference
    x <- if (inherits(x, "netdifference") && is.matrix(x$weights)) {
      x$weights
    } else {
      x$difference_matrix
    }
    difference <- TRUE
  }
  if (isTRUE(difference)) {
    if (!is.null(y)) {
      warning("'difference = TRUE': 'y' is ignored; 'x' is used as the ",
              "difference network.", call. = FALSE)
    }
    x <- .extract_weights(x)
    y <- matrix(0, nrow(x), ncol(x), dimnames = dimnames(x))
  }

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
                                       show_inits, donut_inner_ratio,
                                       combined = combined, ...))
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

  # Handle plain list of networks. Exclude network objects that are themselves
  # S3 lists (cograph_network covers psychnet, Nestimate netobject, etc.) so a
  # single such network is not mistaken for a list of networks to compare.
  else if (is.list(x) &&
           !inherits(x, c("tna", "CographNetwork", "cograph_network", "igraph"))) {
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

  # Track TNA input for styling defaults (after group_tna/list resolution)
  is_tna_input <- inherits(x, "tna")

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

  # Translate qgraph-style vsize to node_size
  if (!is.null(extra_args$vsize) && is.null(extra_args$node_size)) {
    extra_args$node_size <- extra_args$vsize
    extra_args$vsize <- NULL
  }

  plot_args <- c(
    list(
      x = diff_mat,
      layout = "oval",
      edge_positive_color = pos_color,
      edge_negative_color = neg_color,
      labels = labels,
      title = title,
      # Show all difference edges: the style presets below default
      # `minimum = 0.01`, which would silently hide small differences on a
      # plot whose whole purpose is differences. User `minimum` still wins.
      minimum = 0
    ),
    donut_args
  )

  # Style the difference like a proper network instead of bare default nodes:
  # the TNA look for a directed difference, the psychometric (Okabe-Ito) look
  # for an undirected one. The presets supply the node size (qgraph scale, which
  # splot transforms) and a per-node palette; edge_color is dropped so the
  # sign-based positive/negative edge colors are kept.
  n_states <- nrow(diff_mat)
  diff_directed <- is_tna_input ||
    !isTRUE(all.equal(unname(diff_mat), unname(t(diff_mat)), tolerance = 1e-8))

  if (diff_directed) {
    style_defaults <- .tna_style_defaults(n_nodes = n_states, directed = TRUE)
    # Prefer the tna object's own state colors when available.
    tna_colors <- if (is_tna_input && !is.null(x$data)) attr(x$data, "colors") else NULL
    if (!is.null(tna_colors)) style_defaults$node_fill <- tna_colors
  } else {
    style_defaults <- .psych_style_defaults(n_nodes = n_states)
  }
  style_defaults$edge_color <- NULL  # keep sign-based pos/neg edge colours

  for (nm in names(style_defaults)) {
    if (is.null(plot_args[[nm]])) {
      plot_args[[nm]] <- style_defaults[[nm]]
    }
  }

  # User args override defaults
  for (nm in names(extra_args)) {
    plot_args[[nm]] <- extra_args[[nm]]
  }

  # Bayesian coercions: expose the probability of the difference to the
  # {p_diff} template placeholder (matrix form — splot indexes it per edge).
  # Check names(extra_args), not is.null(): an explicit edge_label_p_diff =
  # NULL from the user must suppress the auto-forward (R list NULL trap —
  # assigning NULL deleted the element, so is.null() alone can't see it).
  if (!is.null(p_diff_matrix) &&
      !("edge_label_p_diff" %in% names(extra_args)) &&
      is.null(plot_args[["edge_label_p_diff"]])) {
    plot_args$edge_label_p_diff <- p_diff_matrix
  }

  # Plot with splot
  do.call(splot, plot_args)

  invisible(list(
    weights = diff_mat,
    inits = inits_diff
  ))
}

#' Plot Network Difference (alias of plot_difference)
#'
#' \code{plot_compare()} is an alias of \code{\link{plot_difference}()} and
#' calls the same implementation. \code{tna::plot_compare()} calls it by name.
#' \code{plot_difference()} is the preferred name.
#'
#' @param x First network (see \code{\link{plot_difference}}).
#' @param ... Arguments passed to \code{\link{plot_difference}}.
#' @return Invisibly, the value of \code{\link{plot_difference}}.
#' @seealso \code{\link{plot_difference}}
#' @examples
#' plot_compare(regulation_net, t(regulation_net))
#' @export
plot_compare <- function(x, ...) {
  plot_difference(x, ...)
}


#' Plot Comparison Heatmap
#'
#' Plots a heatmap of the difference between two weight matrices, or of
#' either matrix alone. Rows are source nodes and columns are target nodes.
#'
#' @param x First network: matrix, \code{cograph_network}, \code{tna},
#'   \code{igraph}, or list with a matrix \code{weights} component.
#' @param y Second network, of the same type and dimensions as \code{x}.
#'   Required for \code{type = "difference"} and \code{type = "y"}; it may
#'   be \code{NULL} for \code{type = "x"}.
#' @param type What to display: \code{"difference"} (x - y), \code{"x"}, or
#'   \code{"y"}.
#' @param name_x Label for the first network in the default title.
#' @param name_y Label for the second network in the default title.
#' @param low_color Color for low (negative) values.
#' @param mid_color Color for zero.
#' @param high_color Color for high (positive) values.
#' @param limits Color scale limits. \code{NULL} uses the data range. Use
#'   \code{c(-1, 1)} for normalized values.
#' @param show_values Logical. Display values in cells?
#' @param value_size Text size for cell values.
#' @param digits Decimal places for cell values.
#' @param title Plot title. \code{NULL} builds one from \code{type},
#'   \code{name_x} and \code{name_y}.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#'
#' @return A ggplot object. The color scale is a diverging gradient with its
#'   midpoint at 0.
#'
#' @examples
#' plot_comparison_heatmap(regulation_net, t(regulation_net))
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

  if (!requireNamespace("ggplot2", quietly = TRUE)) { # nocov start
    stop("Package 'ggplot2' required for heatmap. Install with: install.packages('ggplot2')")
  } # nocov end

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
#' @noRd
.extract_weights <- function(x) {
  if (is.matrix(x)) {
    return(x)
  }

  # Handle S3 cograph_network
  if (inherits(x, "cograph_network")) {
    return(to_matrix(x))
  }

  # Handle R6 CographNetwork
  if (inherits(x, "CographNetwork")) {
    return(x$get_adjacency())
  }

  if (inherits(x, "tna")) {
    return(x$weights)
  }

  if (inherits(x, "igraph")) {
    if (!requireNamespace("igraph", quietly = TRUE)) { # nocov start
      stop("Package 'igraph' required for igraph objects")
    } # nocov end
    return(igraph::as_adjacency_matrix(x, attr = "weight", sparse = FALSE))
  }

  # Handle list-like objects with $weights (e.g., elements of group_tna)
  if (is.list(x) && !is.null(x$weights) && is.matrix(x$weights)) {
    return(x$weights)
  }

  stop("x must be a matrix, cograph_network, tna, or igraph object")
}


#' Extract Initial Probabilities from Network Object
#'
#' Internal helper to extract initial probabilities (inits) from tna objects.
#'
#' @param x Network object.
#' @return A numeric vector of initial probabilities, or NULL if not available.
#' @keywords internal
#' @noRd
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
#' @param combined Logical: when TRUE (default), arrange the pairwise panels
#'   in an internal grid via \code{graphics::par(mfrow=...)}, restored on exit.
#' @param ... Additional arguments passed to splot().
#' @return Invisibly returns a named list of comparison results, one element
#'   per pair (named \code{"<group_i>_vs_<group_j>"}), each a list with
#'   \code{weights} and \code{inits}.
#' @keywords internal
#' @noRd
.plot_compare_all_pairs <- function(x, pos_color, neg_color, labels,
                                    show_inits, donut_inner_ratio,
                                    combined = TRUE, ...) {
  n_groups <- length(x)
  nm <- names(x)
  if (is.null(nm)) nm <- seq_len(n_groups)

  # Generate all pairs
  pairs <- utils::combn(n_groups, 2)
  n_pairs <- ncol(pairs)

  if (combined) {
    # Calculate grid layout
    ncol <- ceiling(sqrt(n_pairs))
    nrow <- ceiling(n_pairs / ncol)

    # Set up multi-panel plot
    old_par <- graphics::par(mfrow = c(nrow, ncol), mar = c(2, 2, 3, 1))
    on.exit(graphics::par(old_par), add = TRUE)
  }

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

    # Apply TNA visual defaults when inputs are TNA objects
    if (inherits(x_net, "tna")) {
      n_st <- nrow(diff_mat)
      tna_cols <- if (!is.null(x_net$data)) attr(x_net$data, "colors") else NULL
      if (is.null(tna_cols)) tna_cols <- tna_color_palette(n_st)

      tna_defs <- .tna_style_defaults(directed = TRUE)
      tna_defs$edge_labels <- TRUE
      tna_defs$node_fill <- tna_cols
      for (dnm in names(tna_defs)) {
        if (is.null(plot_args[[dnm]])) {
          plot_args[[dnm]] <- tna_defs[[dnm]]
        }
      }
    }

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
