#' ControlRank from grounded symmetric Laplacians
#' @keywords internal
#' @noRd
calculate_controlrank <- function(cg, weights = NULL, normalized = FALSE) {
  a <- .cg_candidate_adjacency(cg, weights, "controlrank")
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

#' ControlRank Centrality
#'
#' ControlRank (Zhou, Yu and Lu 2019) is the smallest eigenvalue of the
#' symmetric part of the graph Laplacian \eqn{L = D - A}{L = D - A} after
#' the row and column of the node are deleted:
#' \deqn{CR_i = \lambda_{\min}\left(\left(\frac{L + L^T}{2}\right)_{-i,-i}
#'   \right).}{
#'   CR_i = lambda_min(((L + t(L)) / 2)[-i, -i]).}
#' Larger values rank higher.
#'
#' @details
#' Edge weights must be finite and nonnegative, \code{weighted = FALSE}
#' gives every edge weight one, and loops are removed. In a directed
#' network \eqn{A_{ij}}{A_ij} is the arc from \eqn{i}{i} to \eqn{j}{j},
#' \eqn{D}{D} holds the out-strengths and scores can be negative. On a
#' connected undirected network with at least two nodes all scores are
#' positive, and on a disconnected undirected network every score is zero.
#' A single node scores zero. With \code{normalized = TRUE} the scores are
#' divided by their maximum when it is positive, so negative directed
#' scores stay negative. For matrix input with very small weights, set
#' \code{directed = TRUE} explicitly, because symmetry detection is
#' approximate. A weight range beyond double precision or an unresolved
#' spectrum raises an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{weighted} (use edge weights, default \code{TRUE}) and
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhou, J., Yu, X. and Lu, J.-A. (2019). Node Importance in Controlled
#'   Complex Networks. IEEE Transactions on Circuits and Systems II: Express
#'   Briefs, 66(3), 437-441. \doi{10.1109/TCSII.2018.2845940}.
#' @seealso \code{\link{centrality_laplacian}},
#'   \code{\link{centrality_spectralrank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_controlrank(regulation_net)
centrality_controlrank <- function(x, ...) {
  df <- centrality(x, measures = "controlrank", ...)
  stats::setNames(df$controlrank, df$node)
}
