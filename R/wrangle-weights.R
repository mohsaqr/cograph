#' @title Weight Wrangling Verbs
#' @description Verbs that change edge weights: thresholding, binarizing,
#'   symmetrizing, normalizing and inverting.
#' @name wrangle-weights
#' @keywords internal
#' @noRd
NULL

# =============================================================================
# threshold_edges()
# =============================================================================

#' Threshold Edges by Weight, Count, Proportion or Density
#'
#' Keeps the edges that satisfy every criterion supplied. The operation
#' corresponds to qgraph's \code{minimum} argument and to
#' \code{tna::prune()}. The result is a network that can be analysed and
#' plotted.
#'
#' @param x Network input: cograph_network, matrix, igraph, network, tna, or
#'   an edge-list data frame.
#' @param minimum Numeric. Keep edges whose weight is at least this value.
#' @param maximum Numeric. Keep edges whose weight is at most this value.
#' @param proportion Numeric in (0, 1]. Keep this fraction of the edges, the
#'   strongest first.
#' @param density Numeric in (0, 1]. Keep as many of the strongest edges as
#'   gives this density (edges as a fraction of the possible edges).
#' @param top Non-negative integer. Keep this many edges, the strongest first.
#'   \code{top = 0} removes every edge.
#' @param absolute Logical. Compare \code{abs(weight)} instead of the signed
#'   weight. Default TRUE, which suits correlation and partial-correlation
#'   networks. The \code{minimum} and \code{maximum} comparisons and the
#'   ranking used by \code{proportion}, \code{density} and \code{top} all
#'   follow this flag.
#' @param keep_isolates Logical. Keep nodes that end up with no edges? Default
#'   TRUE. Set FALSE, or call \code{\link{remove_isolates}()}, to drop them.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @details
#' Several criteria are combined with AND. For example,
#' \code{threshold_edges(x, minimum = 0.2, top = 20)} keeps the twenty
#' strongest edges among those of weight at least 0.2. When \code{proportion},
#' \code{density} and \code{top} are combined, the smallest of the implied
#' edge counts is used.
#'
#' Ties at the cut point are all kept, so \code{top = 10} can return more than
#' ten edges when the tenth and eleventh weights are equal. The result
#' therefore does not depend on the order in which the edges are stored.
#'
#' @return A \code{cograph_network} with the surviving edges, or the input
#'   format when \code{keep_format = TRUE}. Every node is kept unless
#'   \code{keep_isolates = FALSE}; nodes the threshold stranded are reported in
#'   a \code{cograph_isolates_created} warning. A non-finite \code{minimum} or
#'   \code{maximum}, a \code{proportion} or \code{density} outside (0, 1],
#'   and a negative or fractional \code{top} raise a
#'   \code{cograph_bad_selection} error.
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
#' threshold_edges(regulation_net, minimum = 0.1)
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
  }
  if (isTRUE(keep_isolates)) {
    # Also when every edge went: the nodes are kept, so they are newly
    # isolated and the documented classed warning has to fire.
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
    if (proportion == 0) {
      .stop_bad_selection("`proportion` must be greater than 0; 0 would keep no edges.")
    }
    targets <- c(targets, ceiling(proportion * n_edges_total))
  }
  if (!is.null(density)) {
    .check_scalar_number(density, "density", lower = 0, upper = 1)
    if (density == 0) {
      .stop_bad_selection("`density` must be greater than 0; 0 would keep no edges.")
    }
    n <- n_nodes(net)
    possible <- if (isTRUE(net$directed)) n * (n - 1) else n * (n - 1) / 2
    targets <- c(targets, ceiling(density * possible))
  }
  if (!is.null(top)) {
    .check_count(top, "top", min = 0)
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
#' Replaces every surviving weight with 1 and drops edges whose weight is at
#' or below the threshold. The operation corresponds to
#' \code{sna::event2dichot()} with an absolute threshold.
#'
#' @param x Network input.
#' @param threshold Numeric. Edges whose weight exceeds this value are kept and
#'   set to 1. Default 0, which keeps every existing edge.
#' @param absolute Logical. Compare \code{abs(weight)}. Default TRUE, so a
#'   correlation network keeps its strong negative edges.
#' @param signed Logical. If TRUE, negative edges become \code{-1}, which
#'   preserves the sign of the association. Default FALSE.
#' @param keep_isolates Logical. Keep nodes that end up with no edges? Default TRUE.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} whose weights are all \code{1} (or, when
#'   \code{signed = TRUE}, \code{1} for a positive edge and \code{-1} for a
#'   negative one), or the input format when \code{keep_format = TRUE}. Nodes
#'   left without edges are kept and reported in a
#'   \code{cograph_isolates_created} warning, unless
#'   \code{keep_isolates = FALSE}.
#'
#' @seealso \code{\link{threshold_edges}}, \code{\link{normalize_weights}}
#'
#' @references
#' Butts, C. T. (2008). Social network analysis with sna.
#' \emph{Journal of Statistical Software}, 24(6), 1--51.
#'
#' @export
#' @examples
#' binarize(regulation_net, threshold = 0.1)
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
  }
  if (isTRUE(keep_isolates)) {
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
#' an undirected network. Self-loops are kept unchanged.
#'
#' @param x Network input.
#' @param method How to combine \code{w[i, j]} and \code{w[j, i]}:
#'   \describe{
#'     \item{\code{"max"}}{(default) the larger of the two; on a binary
#'       network this is sna's "weak" rule}
#'     \item{\code{"min"}}{the smaller of the two}
#'     \item{\code{"mean"}}{their average}
#'     \item{\code{"sum"}}{their total}
#'     \item{\code{"mutual"}}{keep only reciprocated pairs, taking the smaller
#'       weight; on a binary network this is sna's "strong" rule}
#'     \item{\code{"upper"}}{take the upper triangle and mirror it}
#'     \item{\code{"lower"}}{take the lower triangle and mirror it}
#'   }
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. Directedness to read the input with;
#'   the result is always undirected.
#'
#' @details
#' \code{"max"}, \code{"min"}, \code{"mean"} and \code{"sum"} combine two
#' values only where both arcs exist. An unreciprocated edge keeps its own
#' weight. In a signed network an unreciprocated negative edge is therefore
#' kept. \code{"mutual"} keeps only reciprocated edges.
#'
#' @return An undirected \code{cograph_network}, or the input format when
#'   \code{keep_format = TRUE}. The weight matrix satisfies
#'   \code{isSymmetric()}. Zero is how this representation stores "no edge", so
#'   any pair whose combined weight is exactly zero disappears: every
#'   unreciprocated arc under \code{method = "mutual"}, and a cancelling pair
#'   under \code{"sum"}. A \code{cograph_edges_dropped} warning says how many.
#'
#' @seealso \code{\link{to_undirected}}, \code{\link{normalize_weights}}
#'
#' @references
#' Butts, C. T. (2008). Social network analysis with sna.
#' \emph{Journal of Statistical Software}, 24(6), 1--51.
#'
#' @export
#' @examples
#' symmetrize(regulation_net, method = "mean")
symmetrize <- function(x, method = c("max", "min", "mean", "sum", "mutual",
                                    "upper", "lower"),
                       keep_format = FALSE, directed = NULL) {
  method <- match.arg(method)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  m <- to_matrix(net)

  sym <- switch(method,
    upper = .mirror_triangle(m, "upper"),
    lower = .mirror_triangle(m, "lower"),
    mutual = pmin(m, t(m)) * ((m != 0) & (t(m) != 0)),
    # Presence is carried separately from weight, so an unreciprocated
    # negative edge is not deleted by comparison with a structural zero.
    .combine_arcs(m, t(m), method)
  )

  # A self-loop is a single arc and must not be combined with its transpose.
  diag(sym) <- diag(m)

  .warn_cancelled_edges(m, t(m), sym)

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
#' Rescales the edge weights. Row normalization turns a transition count
#' matrix into the transition probabilities used by TNA models.
#'
#' @param x Network input.
#' @param method How to rescale:
#'   \describe{
#'     \item{\code{"row"}}{(default) each row sums to 1}
#'     \item{\code{"column"}}{each column sums to 1}
#'     \item{\code{"max"}}{divide by the largest absolute weight}
#'     \item{\code{"sum"}}{divide by the sum of the weight matrix, in which
#'       each undirected edge appears twice}
#'     \item{\code{"minmax"}}{rescale the non-zero weights to \[0, 1\]}
#'   }
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @details
#' A row or column whose total is zero is left at zero. Zero totals, and a zero
#' denominator for \code{"max"} or \code{"sum"}, raise a
#' \code{cograph_zero_norm} warning.
#'
#' \code{"minmax"} maps the weakest edge to \code{.Machine$double.eps}. A
#' weight of exactly 0 would remove the edge, because 0 stores "no edge". When
#' all weights are equal they all become 1.
#'
#' \code{"max"}, \code{"sum"} and \code{"minmax"} rescale each edge
#' independently and keep any extra edge columns. \code{"row"} and
#' \code{"column"} scale an edge by a total that differs at its two endpoints.
#' They break symmetry, so an undirected input is returned as a directed
#' network.
#'
#' @return A \code{cograph_network} with rescaled weights, or the input format
#'   when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{binarize}}, \code{\link{invert_weights}},
#'   \code{\link{symmetrize}}
#'
#' @export
#' @examples
#' normalize_weights(regulation_net, method = "row")
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
  .check_finite_weights(m, "normalize_weights")

  if (method %in% c("max", "sum", "minmax")) {
    # These rescale each edge independently of every other, so the edge set is
    # unchanged and the edge table (with any extra columns) can be kept.
    edges <- get_edges(net)
    scaled <- switch(method,
      max = .scale_by_total(edges$weight, max(abs(m)), "the largest absolute weight"),
      sum = .scale_by_total(edges$weight, sum(m), "the total weight"),
      minmax = .rescale_minmax(edges$weight)
    )
    edges$weight <- scaled
    return(.finish_result(.rebuild_network(net, edges = .drop_zero_edges(edges)),
                          x, input_class, keep_format))
  }

  # Row and column normalization scale an edge by a total that differs at each
  # endpoint, so they break symmetry and the result is directed.
  normalized <- .scale_margin(m, if (method == "row") 1L else 2L)

  .finish_result(.network_from_matrix(net, normalized, directed = TRUE),
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

#' Rescale a weight vector to the unit interval
#'
#' The edge at the minimum maps to `.Machine$double.eps` rather than to 0,
#' because 0 is how this representation stores "no edge" and mapping to it
#' would delete the weakest edge instead of scaling it. This is documented on
#' `normalize_weights()`.
#'
#' @noRd
.rescale_minmax <- function(w) {
  if (length(w) == 0L) {
    return(w)
  }
  lo <- min(w)
  hi <- max(w)
  if (isTRUE(all.equal(lo, hi))) {
    return(rep(1, length(w)))
  }
  out <- (w - lo) / (hi - lo)
  out[out == 0] <- .Machine$double.eps
  out
}

# =============================================================================
# invert_weights()
# =============================================================================

#' Invert Edge Weights (Similarity to Distance and Back)
#'
#' Turns strong ties into short distances. Path-based measures treat weights
#' as costs, so similarity weights are inverted before such measures are
#' computed.
#'
#' @param x Network input.
#' @param method How to invert:
#'   \describe{
#'     \item{\code{"reciprocal"}}{(default) \code{1 / w}, the standard
#'       similarity-to-distance map.}
#'     \item{\code{"max_minus"}}{\code{max(w) - w}. The strongest edge becomes
#'       zero and is therefore dropped; a \code{cograph_edges_dropped} warning
#'       says how many.}
#'     \item{\code{"reflect"}}{\code{max(w) + min(w) - w}. Reverses the order
#'       of the weights and keeps every edge when all weights are positive.}
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
#' invert_weights(regulation_net, method = "reciprocal")
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
