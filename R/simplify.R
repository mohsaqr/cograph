#' Simplify a Network
#'
#' Removes self-loops and merges duplicate (multi-)edges, similar to
#' \code{igraph::simplify()}. Works on matrices, cograph_network, igraph,
#' and tna objects.
#'
#' @param x Network input (matrix, cograph_network, igraph, tna object).
#' @param remove_loops Logical. Remove self-loops (diagonal entries)?
#' @param remove_multiple Logical. Merge duplicate edges?
#' @param edge_attr_comb How to combine weights of duplicate edges:
#'   \code{"sum"}, \code{"mean"}, \code{"max"}, \code{"min"},
#'   \code{"first"}, or a custom function.
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
  if (remove_loops) diag(x) <- 0
  x
}

#' @rdname simplify
#' @export
simplify.cograph_network <- function(x, remove_loops = TRUE,
                                     remove_multiple = TRUE,
                                     edge_attr_comb = "mean", ...) {
  edges <- get_edges(x)

  if (!is.null(edges) && nrow(edges) > 0) {
    if (remove_loops) {
      edges <- edges[edges$from != edges$to, , drop = FALSE]
    }
    if (remove_multiple) {
      edges <- aggregate_duplicate_edges(edges, method = edge_attr_comb)
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
