#' @title Weight Wrangling Verbs
#' @description Verbs that change edge weights: thresholding, binarising,
#'   symmetrising, normalising and inverting.
#' @name wrangle-weights
#' @keywords internal
NULL

# =============================================================================
# threshold_edges()
# =============================================================================

#' Threshold Edges by Weight, Count, Proportion or Density
#'
#' Keeps the edges that satisfy every criterion supplied. This is the network
#' equivalent of qgraph's \code{minimum}/\code{cut} arguments and of
#' \code{tna::prune()}, except that it returns a network rather than a plot
#' setting, so the thresholded network can be analysed, not only drawn.
#'
#' @param x Network input: cograph_network, matrix, igraph, network, tna, or
#'   an edge-list data frame.
#' @param minimum Numeric. Keep edges whose weight is at least this value.
#' @param maximum Numeric. Keep edges whose weight is at most this value.
#' @param proportion Numeric in (0, 1]. Keep this fraction of the edges, the
#'   strongest first.
#' @param density Numeric in (0, 1]. Keep as many of the strongest edges as
#'   gives this density (edges as a fraction of the possible edges).
#' @param top Integer. Keep this many edges, the strongest first.
#' @param absolute Logical. Compare \code{abs(weight)} rather than the signed
#'   weight. Default TRUE, which is what correlation and partial-correlation
#'   networks need. \code{minimum}/\code{maximum} and the ranking used by
#'   \code{proportion}, \code{density} and \code{top} both follow this flag.
#' @param keep_isolates Logical. Keep nodes that end up with no edges? Default
#'   TRUE. Set FALSE, or call \code{\link{remove_isolates}()}, to drop them.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @details
#' When several criteria are given they are combined with AND: for example
#' \code{threshold_edges(x, minimum = 0.2, top = 20)} keeps the twenty
#' strongest edges among those of weight at least 0.2.
#'
#' Ties at the cut point are all kept, so \code{top = 10} can return more than
#' ten edges when the tenth and eleventh weights are equal. This is deliberate:
#' breaking ties on edge order would make the result depend on how the network
#' was built.
#'
#' @return A \code{cograph_network} with the surviving edges, or the input
#'   format when \code{keep_format = TRUE}. Every node is kept unless
#'   \code{keep_isolates = FALSE}.
#'
#' @seealso \code{\link{binarize}}, \code{\link{filter_edges}},
#'   \code{\link{disparity_filter}}, \code{\link{remove_isolates}}
#'
#' @references
#' Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., &
#' Borsboom, D. (2012). qgraph: Network visualizations of relationships in
#' psychometric data. \emph{Journal of Statistical Software}, 48(4), 1--18.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' threshold_edges(adj, minimum = 0.5)
#' threshold_edges(adj, top = 2)
#' threshold_edges(adj, density = 0.5)
threshold_edges <- function(x, minimum = NULL, maximum = NULL,
                            proportion = NULL, density = NULL, top = NULL,
                            absolute = TRUE, keep_isolates = TRUE,
                            keep_format = FALSE, directed = NULL) {
  stopifnot("`absolute` must be TRUE or FALSE" = is.logical(absolute) && length(absolute) == 1L)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  edges <- get_edges(net)

  if (nrow(edges) == 0L) {
    warning("Network has no edges", call. = FALSE)
    return(.finish_result(net, x, input_class, keep_format))
  }

  score <- if (absolute) abs(edges$weight) else edges$weight
  keep <- rep(TRUE, nrow(edges))

  if (!is.null(minimum)) {
    .check_scalar_number(minimum, "minimum")
    keep <- keep & score >= minimum
  }
  if (!is.null(maximum)) {
    .check_scalar_number(maximum, "maximum")
    keep <- keep & score <= maximum
  }

  n_target <- .threshold_target_count(net, nrow(edges), proportion, density, top)
  if (!is.null(n_target)) {
    keep <- keep & .top_by_score(score, n_target, keep)
  }

  kept_edges <- edges[keep, , drop = FALSE]
  result <- .update_cograph_edges(net, kept_edges, keep_isolates = keep_isolates)

  if (nrow(kept_edges) == 0L) {
    warning("Threshold removed all edges.", call. = FALSE)
  } else if (isTRUE(keep_isolates)) {
    .warn_new_isolates(edges, kept_edges, n_nodes(net))
  }

  .finish_result(result, x, input_class, keep_format)
}

#' How many edges a proportion / density / top request asks for
#' @noRd
.threshold_target_count <- function(net, n_edges_total, proportion, density, top) {
  targets <- c()

  if (!is.null(proportion)) {
    .check_scalar_number(proportion, "proportion", lower = 0, upper = 1)
    targets <- c(targets, ceiling(proportion * n_edges_total))
  }
  if (!is.null(density)) {
    .check_scalar_number(density, "density", lower = 0, upper = 1)
    n <- n_nodes(net)
    possible <- if (isTRUE(net$directed)) n * (n - 1) else n * (n - 1) / 2
    targets <- c(targets, ceiling(density * possible))
  }
  if (!is.null(top)) {
    .check_scalar_number(top, "top", lower = 0)
    targets <- c(targets, as.integer(top))
  }

  if (length(targets) == 0L) NULL else min(targets)
}

#' Keep the `n` highest scores among the currently kept edges, ties included
#' @noRd
.top_by_score <- function(score, n, eligible) {
  if (n <= 0L) {
    return(rep(FALSE, length(score)))
  }
  candidates <- score[eligible]
  if (length(candidates) <= n) {
    return(rep(TRUE, length(score)))
  }
  # Ties at the cut point are kept: an edge order tie-break would make the
  # result depend on how the network happened to be built.
  cutoff <- sort(candidates, decreasing = TRUE)[n]
  score >= cutoff
}

# =============================================================================
# binarize()
# =============================================================================

#' Binarize Edge Weights
#'
#' Replaces every surviving weight with 1, dropping edges at or below the
#' threshold. The network equivalent of \code{sna::event2dichot()}.
#'
#' @param x Network input.
#' @param threshold Numeric. Edges whose weight exceeds this value are kept and
#'   set to 1. Default 0, which keeps every existing edge.
#' @param absolute Logical. Compare \code{abs(weight)}. Default TRUE, so a
#'   correlation network keeps its strong negative edges.
#' @param signed Logical. If TRUE, negative edges become \code{-1} rather than
#'   \code{1}, preserving the sign of the association. Default FALSE.
#' @param keep_isolates Logical. Keep nodes that end up with no edges? Default TRUE.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} whose weights are all 1 (or \code{-1} when
#'   \code{signed = TRUE}), or the input format when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{threshold_edges}}, \code{\link{normalize_weights}}
#'
#' @references
#' Butts, C. T. (2008). Social network analysis with sna.
#' \emph{Journal of Statistical Software}, 24(6), 1--51.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' binarize(adj, threshold = 0.45)
binarize <- function(x, threshold = 0, absolute = TRUE, signed = FALSE,
                     keep_isolates = TRUE, keep_format = FALSE,
                     directed = NULL) {
  .check_scalar_number(threshold, "threshold")
  stopifnot(
    "`absolute` must be TRUE or FALSE" = is.logical(absolute) && length(absolute) == 1L,
    "`signed` must be TRUE or FALSE" = is.logical(signed) && length(signed) == 1L
  )

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  edges <- get_edges(net)

  if (nrow(edges) == 0L) {
    warning("Network has no edges", call. = FALSE)
    return(.finish_result(net, x, input_class, keep_format))
  }

  score <- if (absolute) abs(edges$weight) else edges$weight
  keep <- score > threshold
  kept <- edges[keep, , drop = FALSE]
  kept$weight <- if (signed) sign(kept$weight) else rep(1, nrow(kept))

  result <- .update_cograph_edges(net, kept, keep_isolates = keep_isolates)

  if (nrow(kept) == 0L) {
    warning("Binarize removed all edges.", call. = FALSE)
  } else if (isTRUE(keep_isolates)) {
    .warn_new_isolates(edges, kept, n_nodes(net))
  }

  .finish_result(result, x, input_class, keep_format)
}

# =============================================================================
# symmetrize()
# =============================================================================

#' Symmetrize a Directed Network
#'
#' Combines each pair of opposite arcs into one undirected edge. The result is
#' an undirected network, so measures that branch on directedness see the
#' change.
#'
#' @param x Network input.
#' @param method How to combine \code{w[i, j]} and \code{w[j, i]}:
#'   \describe{
#'     \item{\code{"max"}}{(default) the larger of the two, sna's "weak" rule}
#'     \item{\code{"min"}}{the smaller of the two, sna's "strong" rule}
#'     \item{\code{"mean"}}{their average}
#'     \item{\code{"sum"}}{their total}
#'     \item{\code{"upper"}}{take the upper triangle and mirror it}
#'     \item{\code{"lower"}}{take the lower triangle and mirror it}
#'   }
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. Directedness to read the input with;
#'   the result is always undirected.
#'
#' @return An undirected \code{cograph_network}, or the input format when
#'   \code{keep_format = TRUE}. The weight matrix satisfies
#'   \code{isSymmetric()}.
#'
#' @seealso \code{\link{to_undirected}}, \code{\link{normalize_weights}}
#'
#' @references
#' Butts, C. T. (2008). Social network analysis with sna.
#' \emph{Journal of Statistical Software}, 24(6), 1--51.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, 0,
#'                 .2, 0, .7,
#'                 0, .1, 0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#'
#' symmetrize(adj, method = "max")
#' symmetrize(adj, method = "mean")
symmetrize <- function(x, method = c("max", "min", "mean", "sum", "upper", "lower"),
                       keep_format = FALSE, directed = NULL) {
  method <- match.arg(method)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  m <- to_matrix(net)

  sym <- switch(method,
    max = pmax(m, t(m)),
    min = pmin(m, t(m)),
    mean = (m + t(m)) / 2,
    sum = m + t(m),
    upper = .mirror_triangle(m, "upper"),
    lower = .mirror_triangle(m, "lower")
  )

  # `sum` doubles the diagonal, which would silently double every self-loop.
  if (method == "sum") {
    diag(sym) <- diag(m)
  }

  .finish_result(.network_from_matrix(net, sym, directed = FALSE),
                 x, input_class, keep_format)
}

#' Mirror one triangle of a square matrix onto the other
#' @noRd
.mirror_triangle <- function(m, which = c("upper", "lower")) {
  which <- match.arg(which)
  keep <- if (which == "upper") upper.tri(m) else lower.tri(m)
  out <- matrix(0, nrow(m), ncol(m))
  out[keep] <- m[keep]
  out <- out + t(out)
  diag(out) <- diag(m)
  out
}

# =============================================================================
# normalize_weights()
# =============================================================================

#' Normalize Edge Weights
#'
#' Rescales the weight matrix. Row normalisation is what turns a transition
#' count matrix into the transition probabilities that TNA models use.
#'
#' @param x Network input.
#' @param method How to rescale:
#'   \describe{
#'     \item{\code{"row"}}{(default) each row sums to 1}
#'     \item{\code{"column"}}{each column sums to 1}
#'     \item{\code{"max"}}{divide by the largest absolute weight}
#'     \item{\code{"sum"}}{divide by the total of all weights}
#'     \item{\code{"minmax"}}{rescale the non-zero weights to \[0, 1\]}
#'   }
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @details
#' A row (or column, or the whole matrix) whose total is zero is left at zero
#' rather than producing \code{NaN}: there is nothing to distribute. Rows with
#' a zero total are reported in a \code{cograph_zero_norm} warning so that the
#' zeros are a stated result rather than a silent one.
#'
#' Row and column normalisation are meaningful on directed networks. On an
#' undirected network they still work but break symmetry, so the result is
#' returned as directed.
#'
#' @return A \code{cograph_network} with rescaled weights, or the input format
#'   when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{binarize}}, \code{\link{invert_weights}},
#'   \code{\link{symmetrize}}
#'
#' @export
#' @examples
#' counts <- matrix(c(0, 3, 1,
#'                    2, 0, 4,
#'                    5, 1, 0), 3, 3, byrow = TRUE)
#' rownames(counts) <- colnames(counts) <- c("A", "B", "C")
#'
#' normalize_weights(counts, method = "row")
#' normalize_weights(counts, method = "max")
normalize_weights <- function(x, method = c("row", "column", "max", "sum", "minmax"),
                              keep_format = FALSE, directed = NULL) {
  method <- match.arg(method)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  m <- to_matrix(net)

  if (length(m) == 0L) {
    warning("Network has no nodes", call. = FALSE)
    return(.finish_result(net, x, input_class, keep_format))
  }

  normalized <- switch(method,
    row = .scale_margin(m, 1L),
    column = .scale_margin(m, 2L),
    max = .scale_by_total(m, max(abs(m)), "the largest absolute weight"),
    sum = .scale_by_total(m, sum(m), "the total weight"),
    minmax = .scale_minmax(m)
  )

  # Row and column normalisation are not symmetric operations.
  result_directed <- if (method %in% c("row", "column")) TRUE else NULL

  .finish_result(.network_from_matrix(net, normalized, directed = result_directed),
                 x, input_class, keep_format)
}

#' Scale rows (margin 1) or columns (margin 2) to sum to one
#' @noRd
.scale_margin <- function(m, margin) {
  totals <- if (margin == 1L) rowSums(m) else colSums(m)
  zero <- totals == 0
  if (any(zero)) {
    warning(warningCondition(
      paste0(sum(zero), " ", if (margin == 1L) "row" else "column",
             "(s) sum to zero and were left unchanged."),
      class = "cograph_zero_norm"))
  }
  # Divide only where there is something to divide by; sweep() would put NaN
  # into the empty rows.
  totals[zero] <- 1
  if (margin == 1L) m / totals else t(t(m) / totals)
}

#' Divide by a scalar, guarding a zero denominator
#'
#' Named `.scale_by_total`, not `.scale_by`: visual-scale.R already owns that
#' name, and package-internal helpers share one namespace.
#' @noRd
.scale_by_total <- function(m, denominator, what) {
  if (denominator == 0) {
    warning(warningCondition(
      paste0("Cannot normalize by ", what, ": it is zero. Weights left unchanged."),
      class = "cograph_zero_norm"))
    return(m)
  }
  m / denominator
}

#' Rescale the non-zero weights to [0, 1]
#' @noRd
.scale_minmax <- function(m) {
  nz <- m != 0
  if (!any(nz)) {
    return(m)
  }
  lo <- min(m[nz])
  hi <- max(m[nz])
  if (isTRUE(all.equal(lo, hi))) {
    out <- matrix(0, nrow(m), ncol(m))
    out[nz] <- 1
    return(out)
  }
  out <- matrix(0, nrow(m), ncol(m))
  out[nz] <- (m[nz] - lo) / (hi - lo)
  # An edge that lands exactly on the minimum would otherwise be deleted.
  out[nz & out == 0] <- .Machine$double.eps
  out
}

# =============================================================================
# invert_weights()
# =============================================================================

#' Invert Edge Weights (Similarity to Distance and Back)
#'
#' Turns strong ties into short distances, which is what path-based measures
#' need when the weights are similarities rather than costs.
#'
#' @param x Network input.
#' @param method How to invert:
#'   \describe{
#'     \item{\code{"reciprocal"}}{(default) \code{1 / w}. The standard
#'       similarity-to-distance map; requires non-zero weights, which every
#'       stored edge has.}
#'     \item{\code{"max_minus"}}{\code{max(w) - w}. The strongest edge becomes
#'       zero and is therefore dropped; a \code{cograph_edges_dropped} warning
#'       says how many.}
#'     \item{\code{"reflect"}}{\code{max(w) + min(w) - w}. Reverses the order
#'       of the weights while keeping every edge, so no edge is lost.}
#'   }
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} with inverted weights, or the input format
#'   when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{normalize_weights}}, \code{\link{shortest_paths}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' invert_weights(adj)
#' invert_weights(adj, method = "reflect")
invert_weights <- function(x, method = c("reciprocal", "max_minus", "reflect"),
                           keep_format = FALSE, directed = NULL) {
  method <- match.arg(method)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  edges <- get_edges(net)

  if (nrow(edges) == 0L) {
    warning("Network has no edges", call. = FALSE)
    return(.finish_result(net, x, input_class, keep_format))
  }

  w <- edges$weight
  inverted <- switch(method,
    reciprocal = 1 / w,
    max_minus = max(w) - w,
    reflect = max(w) + min(w) - w
  )

  dropped <- sum(inverted == 0)
  if (dropped > 0L) {
    warning(warningCondition(
      paste0(dropped, " edge(s) inverted to weight zero and were dropped. ",
             "Use method = \"reflect\" to keep every edge."),
      class = "cograph_edges_dropped"))
  }

  edges$weight <- inverted
  result <- .update_cograph_edges(net, edges[inverted != 0, , drop = FALSE],
                                  keep_isolates = TRUE)

  .finish_result(result, x, input_class, keep_format)
}
