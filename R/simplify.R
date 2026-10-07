#' Simplify a Network
#'
#' Removes self-loops and (where representable) merges duplicate
#' (multi-)edges, similar to \code{igraph::simplify()}.
#'
#' The extent of simplification depends on the input representation:
#' \itemize{
#'   \item \code{matrix} and \code{tna}: edges are stored as an n x n weight
#'     matrix. Each cell (i, j) is unique by construction, so duplicate-edge
#'     merging has no effect and \code{remove_multiple} and
#'     \code{edge_attr_comb} are ignored. Only self-loops (the diagonal) are
#'     removed. Duplicate aggregation requires a \code{cograph_network} or
#'     \code{igraph} input.
#'   \item \code{cograph_network}: duplicate edges in the edge list are
#'     merged, and their weights are combined with \code{edge_attr_comb}.
#'   \item \code{igraph}: delegates to \code{igraph::simplify()}. The
#'     \code{weight} attribute is combined with \code{edge_attr_comb} and
#'     other edge attributes are dropped.
#' }
#'
#' @param x Network input (matrix, cograph_network, igraph, tna object).
#' @param remove_loops Logical. Remove self-loops (diagonal entries)?
#'   Default \code{TRUE}.
#' @param remove_multiple Logical. Merge duplicate edges? Default
#'   \code{TRUE}. Ignored for matrix and tna inputs (see Details).
#' @param edge_attr_comb How to combine weights of duplicate edges:
#'   \code{"sum"}, \code{"mean"} (default), \code{"max"}, \code{"min"},
#'   \code{"first"}, or a custom function. Ignored for matrix and tna inputs.
#' @param ... Additional arguments (currently unused).
#'
#' @return The simplified network, in the same format and class as the input
#'   (matrix in / matrix out, \code{cograph_network} in / \code{cograph_network}
#'   out, and so on). The default method raises an error for any other class.
#'
#' @seealso \code{\link{filter_edges}} for conditional edge removal,
#'   \code{\link{centrality}} which has its own \code{simplify} parameter
#'
#' @export
#' @examples
#' # igraph also exports simplify(); qualify the call when both are loaded.
#' cograph::simplify(cograph(student_interactions), edge_attr_comb = "sum")
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
    n_before <- nrow(edges)
    if (remove_multiple) {
      edges <- aggregate_duplicate_edges(edges, method = edge_attr_comb,
                                         directed = directed)
    }
    x$edges <- edges
    # Merged duplicates change the weights, so the stored matrix must be
    # rebuilt from the merged edge table or it keeps the pre-merge values.
    if (is.matrix(x$weights) && nrow(edges) < n_before) {
      x$weights <- .network_weight_matrix(as.character(get_nodes(x)$label),
                                          edges, directed)
    }
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
