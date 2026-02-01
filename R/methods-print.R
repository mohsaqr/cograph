#' @title Print Methods
#' @description S3 print methods for Sonnet objects.
#' @name methods-print
NULL

#' Print sonnet_network Object
#'
#' @param x A sonnet_network object.
#' @param ... Ignored.
#' @return Invisible x.
#' @export
print.sonnet_network <- function(x, ...) {
  # Handle new list-based format (has n_nodes as list element)
  if (!is.null(x$n_nodes)) {
    n <- x$n_nodes
    e <- x$n_edges
    dir <- x$directed
    dir_str <- if (isTRUE(dir)) "directed" else "undirected"

    cat("Sonnet network:", n, "nodes,", e, "edges (", dir_str, ")\n", sep = " ")

    # Show weight range if there are edges
    if (e > 0 && !is.null(x$weight)) {
      w_range <- range(x$weight, na.rm = TRUE)
      if (w_range[1] != w_range[2]) {
        cat("Weights:", round(w_range[1], 3), "to", round(w_range[2], 3), "\n")
      } else {
        cat("Weights:", round(w_range[1], 3), "(all equal)\n")
      }
    }

    # Show layout status
    nodes_df <- x$nodes
    has_layout <- !is.null(nodes_df) && "x" %in% names(nodes_df) && !all(is.na(nodes_df$x))
    cat("Layout:", if (has_layout) "set" else "none", "\n")

    return(invisible(x))
  }

  # Handle old attr-based format (for backward compatibility)
  if (!is.null(attr(x, "n_nodes"))) {
    n <- attr(x, "n_nodes")
    e <- attr(x, "n_edges")
    dir <- attr(x, "directed")
    dir_str <- if (isTRUE(dir)) "directed" else "undirected"

    cat("Sonnet network:", n, "nodes,", e, "edges (", dir_str, ")\n", sep = " ")

    # Show weight range if there are edges
    if (e > 0 && !is.null(x$weight)) {
      w_range <- range(x$weight, na.rm = TRUE)
      if (w_range[1] != w_range[2]) {
        cat("Weights:", round(w_range[1], 3), "to", round(w_range[2], 3), "\n")
      } else {
        cat("Weights:", round(w_range[1], 3), "(all equal)\n")
      }
    }

    # Show layout status
    nodes_df <- attr(x, "nodes")
    has_layout <- !is.null(nodes_df) && "x" %in% names(nodes_df) && !all(is.na(nodes_df$x))
    cat("Layout:", if (has_layout) "set" else "none", "\n")

    return(invisible(x))
  }

  # Handle old R6 wrapper format
  net <- x$network
  if (!is.null(net) && inherits(net, "SonnetNetwork")) {
    cat("Sonnet Network\n")
    cat("==============\n")
    cat("Nodes:", net$n_nodes, "\n")
    cat("Edges:", net$n_edges, "\n")
    cat("Directed:", net$is_directed, "\n")
    cat("Weighted:", net$has_weights, "\n")

    layout <- net$get_layout()
    cat("Layout:", if (is.null(layout)) "not computed" else "computed", "\n")

    theme <- net$get_theme()
    cat("Theme:", if (is.null(theme)) "none" else theme$name, "\n")

    cat("\nUse plot() or sn_render() to visualize\n")
    cat("Use sn_ggplot() to convert to ggplot2\n")

    return(invisible(x))
  }

  # Fallback
  cat("Sonnet network object\n")
  invisible(x)
}
