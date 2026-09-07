#' Expected Force from three-vertex cluster multiplicities
#' @keywords internal
#' @noRd
calculate_expected_force <- function(g, modified = FALSE, exf_alpha = 2) {
  if (modified && (!is.numeric(exf_alpha) || length(exf_alpha) != 1L ||
                     !is.finite(exf_alpha) || exf_alpha <= 1)) {
    stop("exf_alpha must be a finite number greater than one.", call. = FALSE)
  }
  a <- .cg_path_matrix(g, NULL)
  diag(a) <- 0
  n <- nrow(a)
  degree <- rowSums(a)
  neighbors <- .cg_adjlist(a, directed = TRUE, mode = "out")
  result <- numeric(n)
  for (s in seq_len(n)) {
    first <- neighbors[[s]]
    candidates <- sort(setdiff(unique(c(first, unlist(neighbors[first]))), s))
    if (length(candidates) < 2L) next
    counts <- numeric(3L * n)
    for (j in seq_len(length(candidates) - 1L)) {
      u <- candidates[j]
      v <- candidates[(j + 1L):length(candidates)]
      # Two independent seed transmissions can occur in either order.
      # Each directed chain supplies one additional event sequence.
      multiplicity <- 2 * a[s, u] * a[s, v] +
        a[s, u] * a[u, v] + a[s, v] * a[v, u]
      keep <- multiplicity > 0
      if (!any(keep)) next
      v <- v[keep]
      multiplicity <- multiplicity[keep]
      # Sum outward degrees, removing the six possible internal arcs.
      boundary <- degree[s] + degree[u] + degree[v] -
        a[s, u] - a[u, s] - a[s, v] - a[v, s] - a[u, v] - a[v, u]
      # Histogram stores event-sequence counts, not just distinct clusters.
      counts <- counts + tabulate(rep(boundary, multiplicity),
                                  nbins = length(counts))
    }
    d <- which(counts > 0)
    if (!length(d)) next
    total <- sum(d * counts[d])
    probability <- d / total
    result[s] <- sum(counts[d] * probability * log(total / d))
  }
  if (modified) {
    positive <- degree > 0
    # Avoid overflowing alpha * degree before taking its logarithm.
    factor <- log(exf_alpha) + log(degree[positive])
    result[positive] <- result[positive] * factor
  }
  result
}

#' Expected Force centrality
#'
#' Computes Lawyer's Expected Force after exactly two transmission events
#' without recovery. For each seed, enumerate ordered sequences of two
#' infected-to-susceptible edge transmissions. Each sequence produces a
#' three-node infected cluster with D outgoing edges to susceptible nodes.
#' Normalize these D values across all sequences and take their Shannon
#' entropy using natural logarithms (Lawyer 2015, equation 1).
#'
#' Different event orders or transmitting parents remain distinct even
#' when they infect the same three nodes. A seed and two adjacent neighbors
#' of an undirected triangle form four sequences, not one. Boundary edges
#' are counted individually even when they reach the same susceptible node.
#' This is not entropy over distinct infected sets or over boundary-degree
#' categories, and is not a probability-weighted epidemic simulation.
#'
#' Uses the simple unweighted graph, retaining direction. In directed
#' graphs, only outgoing infected-to-susceptible arcs transmit or contribute
#' boundary degree, following the paper's directed extension. Loops and
#' duplicate arcs are removed after generic processing. Weights, mode,
#' inversion and cutoff do not affect the result. The weighted extension
#' and horizons other than two events are outside this implementation.
#'
#' Zero-degree outcomes use the zero-log-zero entropy limit. If no sequence
#' can perform two transmissions, or every resulting cluster has zero
#' onward force, cograph returns zero. The latter is an explicit extension
#' of the paper's undefined all-zero normalization, not author-code parity.
#' Isolates and components of at most three nodes therefore score zero.
#' A single positive-force outcome also has entropy zero. Empty input
#' returns no scores. The measure is local and does not establish epidemic
#' probability, outbreak size or predictive accuracy on the supplied graph.
#'
#' Native computation groups three-node clusters by boundary degree while
#' preserving their event multiplicities. Worst-case time is O(n cubed),
#' memory O(n squared), including dense graph preparation. Scores remain
#' independent between components before maximum normalization.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#'   \code{normalized = TRUE} divides by the maximum score; all-zero
#'   results remain zero.
#' @return Named numeric vector in input node order.
#' @references Lawyer, G. (2015). Understanding the influence of all nodes
#'   in a network. Scientific Reports, 5, 8665. Equations 1 and 2 and
#'   the directed extension in the Weighted graphs section.
#'   \doi{10.1038/srep08665}.
#' @seealso \code{\link{centrality_modified_expected_force}} for degree
#'   adjustment. \code{\link{centrality_expected}} computes a different
#'   quantity, the sum of neighbor degrees.
#' @export
#' @examples
#' centrality_expected_force(igraph::make_graph("Zachary"))
centrality_expected_force <- function(x, ...) {
  df <- centrality(x, measures = "expected_force", ...)
  stats::setNames(df$expected_force, df$node)
}

#' Modified Expected Force centrality
#'
#' Multiplies the two-event Expected Force by the logarithm of alpha times
#' seed degree (Lawyer 2015, equation 2). Alpha defaults to two, as in the
#' paper, and must be finite and strictly greater than one. Directed input
#' uses outgoing degree, consistent with the outgoing transmission process.
#' An isolate scores zero without evaluating the logarithm of zero.
#' All graph, event-counting and zero-force conventions of
#' \code{\link{centrality_expected_force}} apply. Native log addition avoids
#' overflow when alpha times degree cannot be represented.
#'
#' @inheritParams centrality_expected_force
#' @param exf_alpha Degree rescaling factor, default two, finite and greater
#'   than one. The paper motivates small values; larger finite values are
#'   permitted by the formula without a predictive-performance claim.
#' @return Named numeric vector in input node order.
#' @references Lawyer, G. (2015). Understanding the influence of all nodes
#'   in a network. Scientific Reports, 5, 8665, equation 2.
#'   \doi{10.1038/srep08665}.
#' @export
#' @examples
#' centrality_modified_expected_force(igraph::make_graph("Zachary"))
# nolint start: object_length_linter.
centrality_modified_expected_force <- function(x, exf_alpha = 2, ...) {
  df <- centrality(x, measures = "modified_expected_force",
                   exf_alpha = exf_alpha, ...)
  stats::setNames(df$modified_expected_force, df$node)
}
# nolint end: object_length_linter.
