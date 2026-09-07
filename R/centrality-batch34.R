#' Finite neighbor propagation of closed-neighborhood degree volume
#' @keywords internal
#' @noRd
calculate_ninl <- function(g, ninl_order = 3, ninl_radius = NULL,
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
  a <- .cg_undirected_view(.cg_path_matrix(g, NULL))
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

#' Node and Neighbor Layer Information centrality
#'
#' Zhu and Wang's NINL initializes each node with the sum of original-graph
#' degrees in its closed radius-r neighborhood. The paper sets r to the
#' ceiling of the graph's average shortest-path length. Each iteration then
#' replaces every node's score by the sum of its neighbors' previous scores:
#' NINL-p = A^p NINL-0. The paper uses p = 3; zero iterations returns the
#' initial degree volume. Repeated vertices and edges in these walks count.
#'
#' Uses simple undirected unweighted topology: either arc creates an edge;
#' loops and parallel edges are removed. Weights, mode, inversion and cutoff
#' are ignored. This does not claim a directed or weighted NINL definition.
#'
#' The mean path length includes all distinct vertex pairs. For disconnected
#' graphs it is infinite, so the automatic radius includes every reachable
#' node in each component. This is an explicit cograph extension of the
#' paper's connected example; unreachable nodes never enter the degree sum.
#' Isolates score zero and empty graphs return no scores. A supplied radius
#' is an explicit generalization of the paper's automatic-radius rule.
#'
#' Stepwise propagation evaluates the requested finite iteration count,
#' without assuming convergence to eigenvector centrality. Exact repeated
#' floating-point states of period one or two allow the remaining iterations
#' to be skipped while preserving parity. No tolerance-based convergence
#' cutoff is used. Normalized scores can alternate on bipartite graphs.
#' Dense distance calculation and propagation take O(n cubed + p n squared)
#' time and O(n squared) memory; very large orders can be slow if no exact
#' repeated state occurs. Raw overflow raises an error. With maximum
#' normalization, global rescaling after every step avoids overflow;
#' extremely small relative scores can still underflow in double precision.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ninl_order Nonnegative integer iteration count, default 3.
#'   At most \code{2^53 - 1}, the consecutive-integer precision of doubles.
#' @param ninl_radius \code{NULL} for the source-defined automatic radius,
#'   or a nonnegative integer hop radius, or \code{Inf} for all reachable
#'   nodes. Radius zero uses the focal node's degree alone.
#' @param ... Additional arguments to \code{\link{centrality}}.
#'   \code{normalized = TRUE} divides by the maximum score; all-zero scores
#'   remain zero. Normalization is optional and is not part of the raw
#'   definition in the original paper.
#' @return Named numeric vector in input node order.
#' @references Zhu, J. and Wang, L. (2021). Identifying Influential Nodes
#'   in Complex Networks Based on Node Itself and Neighbor Layer Information.
#'   Symmetry, 13, 1570. Section 2.1, equations 1-2 and Table 1.
#'   \doi{10.3390/sym13091570}.
#' @export
#' @examples
#' centrality_ninl(igraph::make_graph("Zachary"))
#' centrality_ninl(igraph::make_star(5, mode = "undirected"), ninl_order = 2)
centrality_ninl <- function(x, ninl_order = 3, ninl_radius = NULL, ...) {
  df <- centrality(x, measures = "ninl", ninl_order = ninl_order,
                   ninl_radius = ninl_radius, ...)
  stats::setNames(df$ninl, df$node)
}
