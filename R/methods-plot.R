#' @title Plot Methods
#' @description S3 plot methods for Cograph objects.
#' @name methods-plot
NULL

#' Plot cograph_network Object
#'
#' @param x A cograph_network object.
#' @param ... Additional arguments passed to sn_render.
#' @return Invisible x.
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- cograph(adj)
#' plot(net)
plot.cograph_network <- function(x, ...) {
  sn_render(x, ...)
  invisible(x)
}

#' Summary of cograph_network Object
#'
#' @param object A cograph_network object.
#' @param ... Ignored.
#' @return A list with network summary information (invisibly).
#' @export
summary.cograph_network <- function(object, ...) {
  nodes <- get_nodes(object)
  edges <- get_edges(object)
  nn <- n_nodes(object)
  ne <- n_edges(object)
  directed <- is_directed(object)

  cat("Cograph Network Summary\n")
  cat("======================\n\n")

  cat("Structure:\n")
  cat("  Nodes:", nn, "\n")
  cat("  Edges:", ne, "\n")
  cat("  Type:", if (directed) "Directed" else "Undirected", "\n")

  if (!is.null(edges) && nrow(edges) > 0) {
    cat("\nEdge Statistics:\n")
    if (!is.null(edges$weight)) {
      cat("  Min weight:", round(min(edges$weight), 3), "\n")
      cat("  Max weight:", round(max(edges$weight), 3), "\n")
      cat("  Mean weight:", round(mean(edges$weight), 3), "\n")
      n_pos <- sum(edges$weight > 0)
      n_neg <- sum(edges$weight < 0)
      if (n_neg > 0) {
        cat("  Positive edges:", n_pos, "\n")
        cat("  Negative edges:", n_neg, "\n")
      }
    }
  }

  if (nn > 0) {
    cat("\nNode Labels:\n")
    labels <- get_labels(object)
    if (length(labels) > 10) {
      cat("  ", paste(labels[1:10], collapse = ", "), ", ...\n")
    } else {
      cat("  ", paste(labels, collapse = ", "), "\n")
    }
  }

  has_layout <- !is.null(object$layout)
  cat("\nLayout:", if (has_layout) "computed" else "not computed", "\n")

  theme <- object$theme
  cat("Theme:", if (is.null(theme)) "none" else theme$name, "\n")

  weighted <- !is.null(edges$weight) && any(edges$weight != 1)

  invisible(list(
    n_nodes = nn,
    n_edges = ne,
    directed = directed,
    weighted = weighted,
    has_layout = has_layout
  ))
}
