#' Simplify a Network
#'
#' Removes self-loops and (where representable) merges duplicate
#' (multi-)edges, similar to \code{igraph::simplify()}.
#'
#' The extent of simplification depends on the input representation:
#' \itemize{
#'   \item \code{matrix} and \code{tna}: edges are stored as an n x n weight
#'     matrix. Each cell (i, j) is unique by construction, so duplicate-edge
#'     merging is a no-op regardless of \code{remove_multiple} /
#'     \code{edge_attr_comb}; only self-loops (the diagonal) can be removed.
#'     Convert to \code{cograph_network} or \code{igraph} first if you need
#'     true duplicate aggregation.
#'   \item \code{cograph_network}: duplicate edges in the edge-list are
#'     merged via \code{aggregate_duplicate_edges()} using
#'     \code{edge_attr_comb}.
#'   \item \code{igraph}: delegates to \code{igraph::simplify()}.
#' }
#'
#' @param x Network input (matrix, cograph_network, igraph, tna object).
#' @param remove_loops Logical. Remove self-loops (diagonal entries)?
#' @param remove_multiple Logical. Merge duplicate edges?
#'   No-op for matrix/tna inputs (see Details).
#' @param edge_attr_comb How to combine weights of duplicate edges:
#'   \code{"sum"}, \code{"mean"}, \code{"max"}, \code{"min"},
#'   \code{"first"}, or a custom function. Ignored for matrix/tna inputs.
#' @param ... Additional arguments (currently unused).
#'
#' @return The simplified network in the same format as the input.
#'
#' @seealso \code{\link{filter_edges}} for conditional edge removal,
#'   \code{\link{centrality}} which has its own \code{simplify} parameter
#'
#' @export
#' @examples
#' # Matrix with self-loops
#' mat <- matrix(c(0.5, 0.3, 0, 0.3, 0.2, 0.4, 0, 0.4, 0.1), 3, 3)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C")
#' simplify(mat)
#'
#' # Edge list with duplicates
#' edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3), weight = c(0.3, 0.7, 0.5))
#' net <- cograph(edges, layout = NULL)
#' simplify(net)
#' simplify(net, edge_attr_comb = "sum")
simplify <- function(x, remove_loops, remove_multiple, edge_attr_comb, ...) {
  UseMethod("simplify")
}

#' @rdname simplify
#' @export
simplify.matrix <- function(x, remove_loops = TRUE, remove_multiple = TRUE,
                            edge_attr_comb = "mean", ...) {
  # An n x n weight matrix cannot hold duplicate (i, j) entries, so
  # remove_multiple / edge_attr_comb are no-ops here (see @details).
  if (remove_loops) diag(x) <- 0
  x
}

#' @rdname simplify
#' @export
simplify.cograph_network <- function(x, remove_loops = TRUE,
                                     remove_multiple = TRUE,
                                     edge_attr_comb = "mean", ...) {
  edges <- get_edges(x)
  directed <- isTRUE(x$directed)

  if (!is.null(edges) && nrow(edges) > 0) {
    if (remove_loops) {
      edges <- edges[edges$from != edges$to, , drop = FALSE]
    }
    if (remove_multiple) {
      edges <- aggregate_duplicate_edges(edges, method = edge_attr_comb,
                                         directed = directed)
    }
    x$edges <- edges
  }

  if (!is.null(x$weights) && is.matrix(x$weights) && remove_loops) {
    diag(x$weights) <- 0
  }

  x
}

#' @rdname simplify
#' @export
simplify.igraph <- function(x, remove_loops = TRUE, remove_multiple = TRUE,
                            edge_attr_comb = "mean", ...) {
  igraph::simplify(x,
    remove.multiple = remove_multiple,
    remove.loops = remove_loops,
    edge.attr.comb = list(weight = edge_attr_comb, "ignore")
  )
}

#' @rdname simplify
#' @export
simplify.tna <- function(x, remove_loops = TRUE, remove_multiple = TRUE,
                         edge_attr_comb = "mean", ...) {
  # tna objects carry weights as an n x n matrix: no duplicates are
  # representable, so remove_multiple / edge_attr_comb are no-ops
  # (see @details). Only the diagonal can be zeroed.
  if (!is.null(x$weights) && is.matrix(x$weights) && remove_loops) {
    diag(x$weights) <- 0
  }
  x
}

#' @rdname simplify
#' @export
simplify.default <- function(x, remove_loops = TRUE, remove_multiple = TRUE,
                             edge_attr_comb = "mean", ...) {
  stop("Cannot simplify object of class ", paste(class(x), collapse = "/"),
       call. = FALSE)
}
