#' @title Disparity Filter for Network Backbone Extraction
#' @description
#' Implements the disparity filter (Serrano et al., 2009) to extract
#' the statistically significant backbone of a weighted network.
#' @name disparity
NULL

#' Disparity Filter
#'
#' Extracts the statistically significant backbone of a weighted network
#' using the disparity filter method (Serrano, Boguna, & Vespignani, 2009).
#'
#' The disparity filter identifies edges that carry a disproportionate
#' fraction of a node's total weight, based on a null model where weights
#' are distributed uniformly at random.
#'
#' @param x A weight matrix, tna object, or cograph_network.
#' @param level Significance level (default 0.05). Lower values result in
#'   a sparser backbone (fewer edges retained).
#' @param ... Additional arguments (currently unused).
#'
#' @return A binary matrix (0/1) indicating significant edges,
#'   or a filtered weight matrix if \code{return_weights = TRUE}.
#'
#' @details
#' For each node \eqn{i} with degree \eqn{k_i}, and each edge \eqn{(i,j)}

#' with normalized weight \eqn{p_{ij} = w_{ij} / s_i} (where \eqn{s_i} is
#' the node's strength), the p-value is:
#'
#' \deqn{p = (1 - p_{ij})^{(k_i - 1)}}
#'
#' Edges are significant if \eqn{p < level} for either endpoint.
#'
#' @references
#' Serrano, M. A., Boguna, M., & Vespignani, A. (2009).
#' Extracting the multiscale backbone of complex weighted networks.
#' Proceedings of the National Academy of Sciences, 106(16), 6483-6488.
#'
#' @seealso \code{\link{bootstrap}} for bootstrap-based significance testing
#'
#' @export
#'
#' @examples
#' # Create a weighted network
#' mat <- matrix(c(
#'   0.0, 0.5, 0.1, 0.0,
#'   0.3, 0.0, 0.4, 0.1,
#'   0.1, 0.2, 0.0, 0.5,
#'   0.0, 0.1, 0.3, 0.0
#' ), nrow = 4, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
#'
#' # Extract backbone at 5% significance level
#' backbone <- disparity_filter(mat, level = 0.05)
#' backbone
#'
#' # More stringent filter (1% level)
#' backbone_strict <- disparity_filter(mat, level = 0.01)
disparity_filter <- function(x, level = 0.05, ...) {
  UseMethod("disparity_filter")
}

#' @rdname disparity_filter
#' @export
disparity_filter.default <- function(x, level = 0.05, ...) {
  if (!is.matrix(x)) {
    stop("x must be a matrix, tna object, or cograph_network", call. = FALSE)
  }
  .disparity_filter_matrix(x, level = level)
}

#' @rdname disparity_filter
#' @export
disparity_filter.matrix <- function(x, level = 0.05, ...) {
  .disparity_filter_matrix(x, level = level)
}

#' @rdname disparity_filter
#' @export
disparity_filter.tna <- function(x, level = 0.05, ...) {
  mat <- x$weights
  sig <- .disparity_filter_matrix(mat, level = level)

  # Return as tna_disparity object
  structure(
    list(
      significant = sig,
      weights_orig = mat,
      weights_filtered = mat * sig,
      level = level,
      n_edges_orig = sum(mat != 0),
      n_edges_filtered = sum(sig != 0)
    ),
    class = c("tna_disparity", "list")
  )
}

#' @rdname disparity_filter
#' @export
disparity_filter.cograph_network <- function(x, level = 0.05, ...) {
  mat <- to_matrix(x)
  sig <- .disparity_filter_matrix(mat, level = level)

  structure(
    list(
      significant = sig,
      weights_orig = mat,
      weights_filtered = mat * sig,
      level = level,
      n_edges_orig = sum(mat != 0),
      n_edges_filtered = sum(sig != 0)
    ),
    class = c("tna_disparity", "list")
  )
}

# =============================================================================
# Core Disparity Filter Implementation (TNA-matching)
# =============================================================================

#' Disparity Filter Core Implementation
#'
#' Exact implementation matching TNA package.
#' Uses fast .rowSums/.colSums for performance.
#'
#' @param mat Weight matrix.
#' @param level Significance level.
#' @return Binary significance matrix.
#'
#' @keywords internal
.disparity_filter_matrix <- function(mat, level = 0.05) {
  d <- dim(mat)[2]

  # Create binary adjacency (edge indicator)
  idx_mat <- 1L * (mat > 0)

  # ===== Outgoing edges (row-based) =====
  # Normalized edge weights: p_ij = w_ij / strength_i
  row_sums <- .rowSums(mat, m = d, n = d)
  row_sums[row_sums == 0] <- 1  # Avoid division by zero
  out_edges <- mat / row_sums

  # Out-degree per node
  out_degree <- .rowSums(idx_mat, m = d, n = d)

  # P-value: (1 - p_ij)^(k_i - 1)
  out_p_values <- (1 - out_edges)^(out_degree - 1)

  # ===== Incoming edges (column-based) =====
  # Normalized edge weights for columns
  col_sums <- .colSums(mat, m = d, n = d)
  col_sums[col_sums == 0] <- 1
  in_edges <- t(t(mat) / col_sums)

  # In-degree per node
  in_degree <- .colSums(idx_mat, m = d, n = d)

  # P-value for incoming
  in_p_values <- t((1 - t(in_edges))^(in_degree - 1))

  # ===== Combine: edge significant if significant from either direction =====
  p_values <- pmin(out_p_values, in_p_values)

  # Binary significance indicator
  sig <- 1L * (p_values < level)
  diag(sig) <- 0L  # No self-loops in backbone

  # Preserve dimnames
  dimnames(sig) <- dimnames(mat)

  sig
}

# =============================================================================
# Print Method
# =============================================================================

#' @export
print.tna_disparity <- function(x, ...) {
  cat("Disparity Filter Result\n")
  cat("=======================\n")
  cat("Significance level:", x$level, "\n")
  cat("Original edges:", x$n_edges_orig, "\n")
  cat("Filtered edges:", x$n_edges_filtered, "\n")
  cat("Reduction:", sprintf("%.1f%%",
    100 * (1 - x$n_edges_filtered / max(x$n_edges_orig, 1))), "\n")
  invisible(x)
}

# =============================================================================
# Plot Method
# =============================================================================

#' Plot Disparity Filter Result
#'
#' @param x A tna_disparity object.
#' @param type Plot type: "backbone" (default) or "comparison".
#' @param ... Additional arguments passed to splot.
#'
#' @export
plot.tna_disparity <- function(x, type = c("backbone", "comparison"), ...) {
  type <- match.arg(type)

  if (type == "backbone") {
    # Plot filtered network
    splot(x$weights_filtered, ...)
  } else {
    # Side-by-side comparison
    oldpar <- par(mfrow = c(1, 2))
    on.exit(par(oldpar))

    splot(x$weights_orig, title = "Original", ...)
    splot(x$weights_filtered, title = "Backbone", ...)
  }
}

# =============================================================================
# splot Method for tna_disparity
# =============================================================================

#' Plot Disparity Results with splot
#'
#' @param x A tna_disparity object.
#' @param show What to display: "styled" (default), "backbone", "full".
#' @param edge_style_sig Line style for backbone edges. Default 1 (solid).
#' @param edge_style_nonsig Line style for non-backbone edges. Default 2 (dashed).
#' @param alpha_nonsig Alpha for non-backbone edges. Default 0.3.
#' @param ... Additional arguments passed to splot.
#'
#' @export
splot.tna_disparity <- function(x, show = c("styled", "backbone", "full"),
                                edge_style_sig = 1, edge_style_nonsig = 2,
                                alpha_nonsig = 0.3, ...) {
  show <- match.arg(show)

  if (show == "backbone") {
    splot(x$weights_filtered, ...)
  } else if (show == "full") {
    splot(x$weights_orig, ...)
  } else {
    # Styled: solid=backbone, dashed=non-backbone
    weights <- x$weights_orig
    sig_mask <- x$significant == 1
    a <- nrow(weights)

    # Build edge styles
    edge_styles <- matrix(edge_style_nonsig, a, a)
    edge_styles[sig_mask] <- edge_style_sig

    # Build edge alphas
    edge_alphas <- matrix(alpha_nonsig, a, a)
    edge_alphas[sig_mask] <- 1

    # Convert to edge vectors
    edge_idx <- which(weights != 0, arr.ind = TRUE)
    if (nrow(edge_idx) > 0) {
      style_vec <- edge_styles[edge_idx]
      alpha_vec <- edge_alphas[edge_idx]

      args <- list(...)
      args$edge_style <- style_vec
      args$edge_alpha <- alpha_vec

      do.call(splot, c(list(x = weights), args))
    } else {
      splot(weights, ...)
    }
  }
}
