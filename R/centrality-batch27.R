#' X-degree on the simple undirected skeleton
#' @keywords internal
#' @noRd
calculate_x_degree <- function(cg) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  neighbors <- .cg_adjlist(b, directed = FALSE)
  excess <- as.numeric(lengths(neighbors)) - 1
  vapply(neighbors, function(nodes) {
    z <- excess[nodes]
    if (length(z) < 2L) return(0)
    # Twice the sum over unordered pairs, without subtracting large squares.
    2 * sum(z[-1L] * head(cumsum(z), -1L))
  }, numeric(1))
}

#' X-degree centrality
#'
#' Computes Torres et al.'s X-degree (equation 3.15):
#' \deqn{Xdeg(i) = (\sum_{j\in N(i)}(d_j-1))^2
#'                   - \sum_{j\in N(i)}(d_j-1)^2.}
#' Degrees are measured in the original simple undirected graph. The score
#' counts oriented nonbacktracking walks of four edges whose middle vertex
#' is i. Walks can revisit a vertex provided they do not immediately reverse
#' an edge. It is also the sum of entries of the paper's matrix DFE, where
#' D, F and E are blocks of the nonbacktracking matrix around i.
#'
#' Uses the simple undirected skeleton: direction, weights, mode, inversion
#' and cutoff do not affect results. Loops are removed and parallel edges
#' count once. This projection is a cograph convention extending the
#' published simple, unweighted, undirected domain. Isolates and leaves
#' score zero; every vertex of a star also scores zero. Empty graphs return
#' no scores. Disconnected components are independent before maximum
#' normalization. These cases follow directly from the local formula.
#'
#' Native arithmetic accumulates nonnegative pair products instead of
#' subtracting two squares. Aggregation takes O(n+m) time after neighbor
#' construction; the current dense skeleton conversion uses O(n squared)
#' time and memory. This is a score on the supplied graph, not the paper's
#' iterative node-removal immunization algorithm. Agreement with the author
#' function and matrix definition does not establish immunization efficacy,
#' exact eigendrop prediction or an unconditional spectral upper bound.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#'   \code{normalized = TRUE} divides scores by their maximum; an all-zero
#'   result stays zero.
#' @return Named numeric vector in input node order.
#' @references Torres, L., Chan, K. S., Tong, H., & Eliassi-Rad, T. (2021).
#'   Nonbacktracking Eigenvalues under Node Removal: X-Centrality and
#'   Targeted Immunization. SIAM Journal on Mathematics of Data Science,
#'   3(2), 656-675. Proposition 3.8, equation 3.15.
#'   \doi{10.1137/20M1352132}.
#' @export
#' @examples
#' centrality_x_degree(igraph::make_graph("Zachary"))
centrality_x_degree <- function(x, ...) {
  df <- centrality(x, measures = "x_degree", ...)
  stats::setNames(df$x_degree, df$node)
}
