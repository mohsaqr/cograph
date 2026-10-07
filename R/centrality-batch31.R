#' Expected Force from three-vertex cluster multiplicities
#' @keywords internal
#' @noRd
calculate_expected_force <- function(cg, modified = FALSE, exf_alpha = 2) {
  if (modified && (!is.numeric(exf_alpha) || length(exf_alpha) != 1L ||
                     !is.finite(exf_alpha) || exf_alpha <= 1)) {
    stop("exf_alpha must be a finite number greater than one.", call. = FALSE)
  }
  a <- .cg_path_matrix(cg, NULL)
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

#' Expected Force Centrality
#'
#' The Expected Force (Lawyer 2015, equation 1) is the entropy of the
#' onward spreading potential after two transmission events from a seed
#' node. Each ordered sequence of two transmissions gives an infected
#' cluster of three nodes with \eqn{D_k}{D_k} edges to susceptible nodes,
#' and with natural logarithms
#' \deqn{ExF_i = -\sum_k \frac{D_k}{\sum_l D_l}
#'   \log \frac{D_k}{\sum_l D_l}.}{
#'   ExF_i = -sum_k (D_k / sum_l D_l) log(D_k / sum_l D_l).}
#'
#' @details
#' The measure is computed on the simple unweighted network with direction
#' kept, so weights, loops and parallel edges are ignored. In a directed
#' network only outgoing arcs transmit and count toward \eqn{D_k}{D_k}.
#' Different orders of the same two infections count as distinct
#' sequences. A node that cannot start two transmissions scores zero, so
#' isolated nodes and nodes of components with at most three nodes score
#' zero. When every cluster has no edge to a susceptible node the entropy
#' is undefined, and the score is set to zero.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Lawyer, G. (2015). Understanding the influence of all nodes in a network.
#'   Scientific Reports, 5, 8665. \doi{10.1038/srep08665}.
#' @seealso \code{\link{centrality_modified_expected_force}},
#'   \code{\link{centrality_expected}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_expected_force(regulation_net)
centrality_expected_force <- function(x, ...) {
  df <- centrality(x, measures = "expected_force", ...)
  stats::setNames(df$expected_force, df$node)
}

#' Modified Expected Force Centrality
#'
#' The modified Expected Force (Lawyer 2015, equation 2) multiplies the
#' Expected Force of a node by the logarithm of its scaled degree
#' \eqn{\alpha d_i}{alpha d_i}:
#' \deqn{ExF^{\alpha}_i = \log(\alpha d_i) \, ExF_i.}{
#'   ExF^alpha_i = log(alpha d_i) ExF_i.}
#'
#' @details
#' On a directed network \eqn{d_i}{d_i} is the out-degree, in line with the
#' outgoing transmission process. An isolated node scores zero. The input
#' handling and zero conventions of \code{\link{centrality_expected_force}}
#' apply. An \code{exf_alpha} that is not a finite number greater than one
#' raises an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param exf_alpha Degree scaling factor \eqn{\alpha}{alpha}, a finite
#'   number greater than one. Default 2, as in the paper.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Lawyer, G. (2015). Understanding the influence of all nodes in a network.
#'   Scientific Reports, 5, 8665. \doi{10.1038/srep08665}.
#' @seealso \code{\link{centrality_expected_force}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_modified_expected_force(regulation_net)
# nolint start: object_length_linter.
centrality_modified_expected_force <- function(x, exf_alpha = 2, ...) {
  df <- centrality(x, measures = "modified_expected_force",
                   exf_alpha = exf_alpha, ...)
  stats::setNames(df$modified_expected_force, df$node)
}
# nolint end: object_length_linter.
