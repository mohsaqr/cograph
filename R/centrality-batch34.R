#' Finite neighbor propagation of closed-neighborhood degree volume
#' @keywords internal
#' @noRd
calculate_ninl <- function(cg, ninl_order = 3, ninl_radius = NULL,
                           normalized = FALSE) {
  if (!is.numeric(ninl_order) || length(ninl_order) != 1L ||
        !is.finite(ninl_order) || ninl_order < 0 ||
        ninl_order != floor(ninl_order) || ninl_order > 2^53 - 1) {
    stop("ninl_order must be an integer between 0 and 2^53 - 1.",
         call. = FALSE)
  }
  if (!is.null(ninl_radius) &&
        (!is.numeric(ninl_radius) || length(ninl_radius) != 1L ||
           is.na(ninl_radius) || ninl_radius < 0 ||
           (is.finite(ninl_radius) && ninl_radius != floor(ninl_radius)))) {
    stop("ninl_radius must be NULL, a nonnegative integer, or Inf.",
         call. = FALSE)
  }
  a <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(a) <- 0
  degree <- rowSums(a)
  if (!length(degree) || !any(degree > 0)) return(as.numeric(degree))
  distances <- .cg_distances(a)
  if (is.null(ninl_radius)) {
    ninl_radius <- ceiling(mean(distances[upper.tri(distances)]))
  }
  value <- as.numeric((is.finite(distances) &
                         distances <= ninl_radius) %*% degree)
  # Positive global rescaling commutes with every remaining matrix product.
  # Stepwise propagation avoids exponent-amplified roundoff in matrix powers.
  rescale <- function(z) {
    if (any(!is.finite(z))) {
      stop("NINL exceeds finite double precision; use normalized = TRUE.",
           call. = FALSE)
    }
    if (normalized && any(z > 0)) z <- z / max(z)
    z
  }
  value <- rescale(value)
  previous <- NULL
  while (ninl_order > 0) {
    following <- rescale(as.numeric(a %*% value))
    ninl_order <- ninl_order - 1
    if (identical(following, value)) return(following)
    if (identical(following, previous)) {
      return(if (ninl_order %% 2 == 0) following else value)
    }
    previous <- value
    value <- following
  }
  value
}

#' Node and Neighbor Layer Information Centrality
#'
#' NINL (Zhu and Wang 2021) starts each node with the sum of the degrees
#' \eqn{k_j}{k_j} in its closed neighborhood of radius \eqn{r}{r} and then,
#' for \eqn{p}{p} iterations, replaces every score by the sum of the
#' previous scores of its neighbors:
#' \deqn{NINL^{(p)} = A^p \, NINL^{(0)}, \qquad
#'   NINL^{(0)}_i = \sum_{j : d(i,j) \le r} k_j.}{
#'   NINL(p) = A^p NINL(0), NINL(0)_i = sum_{j: d(i,j) <= r} k_j.}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The default radius is the ceiling of the average shortest-path length,
#' as in the paper. On a disconnected network that average is infinite, so
#' the default radius covers the whole component of each node. Isolated
#' nodes score zero, and \code{ninl_order = 0} returns the initial degree
#' sums. With \code{normalized = TRUE} the scores are rescaled at every
#' iteration, and on a bipartite network they can alternate between
#' iterations. An invalid \code{ninl_order} or \code{ninl_radius}, and raw
#' scores that overflow, raise an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ninl_order Number of iterations \eqn{p}{p}, a nonnegative
#'   integer. Default 3, as in the paper.
#' @param ninl_radius Radius \eqn{r}{r}. \code{NULL} (default) uses the
#'   automatic radius of the paper, a nonnegative integer fixes the hop
#'   radius, and \code{Inf} includes every reachable node.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhu, J. and Wang, L. (2021). Identifying Influential Nodes in Complex
#'   Networks Based on Node Itself and Neighbor Layer Information. Symmetry,
#'   13, 1570. \doi{10.3390/sym13091570}.
#' @seealso \code{\link{centrality_semilocal}},
#'   \code{\link{centrality_eigenvector}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ninl(regulation_net)
centrality_ninl <- function(x, ninl_order = 3, ninl_radius = NULL, ...) {
  df <- centrality(x, measures = "ninl", ninl_order = ninl_order,
                   ninl_radius = ninl_radius, ...)
  stats::setNames(df$ninl, df$node)
}
