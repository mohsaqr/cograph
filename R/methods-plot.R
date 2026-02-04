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
  net <- object$network
  nodes <- net$get_nodes()
  edges <- net$get_edges()

  cat("Cograph Network Summary\n")
  cat("======================\n\n")

  cat("Structure:\n")
  cat("  Nodes:", net$n_nodes, "\n")
  cat("  Edges:", net$n_edges, "\n")
  cat("  Type:", if (net$is_directed) "Directed" else "Undirected", "\n")

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

  if (net$n_nodes > 0) {
    cat("\nNode Labels:\n")
    labels <- net$node_labels
    if (length(labels) > 10) {
      cat("  ", paste(labels[1:10], collapse = ", "), ", ...\n")
    } else {
      cat("  ", paste(labels, collapse = ", "), "\n")
    }
  }

  cat("\nLayout:", if (is.null(net$get_layout())) "not computed" else "computed", "\n")

  theme <- net$get_theme()
  cat("Theme:", if (is.null(theme)) "none" else theme$name, "\n")

  invisible(list(
    n_nodes = net$n_nodes,
    n_edges = net$n_edges,
    directed = net$is_directed,
    weighted = net$has_weights,
    has_layout = !is.null(net$get_layout())
  ))
}
