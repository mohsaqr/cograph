#' ControlRank from grounded symmetric Laplacians
#' @keywords internal
#' @noRd
calculate_controlrank <- function(g, weights = NULL, normalized = FALSE) {
  a <- .cg_candidate_adjacency(g, weights, "controlrank")
  diag(a) <- 0
  n <- nrow(a)
  if (n <= 1L || !any(a > 0)) return(numeric(n))
  symmetric <- isSymmetric(a, tol = 0, check.attributes = FALSE)
  # An ungrounded component has an exact zero mode. Do not normalize
  # roundoff-sized eigenvalues into spurious scores on disconnected graphs.
  if (symmetric && .cg_n_components(a > 0) > 1L) return(numeric(n))
  scale <- max(a)
  positive <- a > 0
  a <- a / scale
  if (any(a[positive] == 0)) {
    stop("controlrank weight range exceeds double precision", call. = FALSE)
  }
  laplacian <- -(a / 2 + t(a) / 2)
  diag(laplacian) <- rowSums(a)
  result <- vapply(seq_len(n), function(i) {
    minor <- laplacian[-i, -i, drop = FALSE]
    blocks <- split(seq_len(n - 1L), .cg_component_labels(minor < 0))
    minima <- vapply(blocks, function(nodes) {
      block <- minor[nodes, nodes, drop = FALSE]
      if (length(nodes) == 1L) return(block[1, 1])
      # An intact Laplacian block has an exact constant zero mode.
      if (all(rowSums(block) == 0)) return(0)
      min(eigen(block, symmetric = TRUE, only.values = TRUE)$values)
    }, numeric(1))
    value <- min(minima)
    # Connected symmetric inputs have strictly positive grounded spectra.
    # Reject unresolved positive modes rather than inventing zeros or ranks.
    if (symmetric && value <= 64 * .Machine$double.eps *
          max(rowSums(abs(minor)))) {
      stop("controlrank grounded spectrum is unresolved in double precision",
           call. = FALSE)
    }
    value
  }, numeric(1))
  if (normalized && max(result) > 0) return(result / max(result))
  result <- result * scale
  if (any(!is.finite(result))) {
    stop("controlrank exceeds finite double precision; use normalized = TRUE",
         call. = FALSE)
  }
  result
}

#' ControlRank centrality
#'
#' Zhou, Yu and Lu's ControlRank is the smallest eigenvalue after deleting
#' a node's row and column from the symmetric part of the graph Laplacian.
#' With \eqn{L = D-A}, this is
#' \eqn{CR_i = \lambda_{\min}(((L+L^T)/2)_{-i,-i})}.
#' D retains the original graph's degrees: the Laplacian is not recomputed
#' on the vertex-deleted graph. Larger values receive higher rank.
#'
#' Uses finite nonnegative interaction weights. For directed input,
#' \eqn{A_{ij}} denotes an arc from i to j and D contains outgoing strengths.
#' This fixes the row-Laplacian orientation explicitly; transpose the input
#' to use incoming strengths. Symmetrizing L preserves its diagonal, so
#' this is different from constructing a Laplacian of the undirected
#' projection. Directed scores can be negative and are not clipped.
#' For matrix inputs with very small weights, supply \code{directed = TRUE}
#' explicitly (or use a directed igraph object): the shared input parser's
#' approximate symmetry detection can otherwise infer an undirected graph.
#'
#' Loops are removed and zero weights are absent connections. Parallel
#' weights follow the generic simplify rule; remaining parallel edges sum.
#' With \code{weighted = FALSE}, each remaining edge contributes one.
#' Mode, weight inversion for shortest paths and cutoff are ignored.
#'
#' Connected undirected graphs with at least two nodes have positive
#' scores. Disconnected undirected graphs score zero for every node because
#' at least one component remains ungrounded. Empty graphs return no scores;
#' singletons return zero as an explicit extension of the undefined empty
#' minor. The source excludes isolates; the matrix formula here also applies
#' to disconnected directed graphs, whose scores may remain negative.
#'
#' This implements the spectral index, not a controller simulation, a
#' finite-feedback convergence rate, or an optimization over controller
#' sets. In particular, no general directed stability guarantee is inferred
#' from these scores. The paper's multi-node selection problem is separate.
#'
#' Dense eigensolves take O(n to the fourth) time and O(n squared) memory;
#' this measure is marked costly and excluded from the default all tier.
#' Disconnected blocks are solved separately, preserving isolated zeros
#' before normalization. Global scaling avoids intermediate overflow.
#' Unrepresentable weight ranges and unresolved positive spectra raise errors.
#' Signed directed scores near zero can retain floating-point roundoff;
#' very small raw scores can underflow. Uniform weight scaling multiplies
#' raw scores by the same factor.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#'   \code{normalized = TRUE} divides by the maximum if it is positive;
#'   otherwise raw scores are retained. This package normalization is
#'   optional and is not part of the published definition.
#' @return Named numeric vector in input node order.
#' @references Zhou, J., Yu, X. and Lu, J.-A. (2019; online 2018).
#'   Node Importance in Controlled Complex Networks. IEEE Transactions on
#'   Circuits and Systems II: Express Briefs, 66(3), 437-441.
#'   Section III-C, Theorem 3; Figure 1 and section IV-A.
#'   \doi{10.1109/TCSII.2018.2845940}.
#' @export
#' @examples
#' centrality_controlrank(igraph::make_ring(5))
#' centrality_controlrank(igraph::make_star(6, mode = "undirected"))
centrality_controlrank <- function(x, ...) {
  df <- centrality(x, measures = "controlrank", ...)
  stats::setNames(df$controlrank, df$node)
}
