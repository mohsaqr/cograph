#' @title Print Methods
#' @description S3 print methods for Cograph objects.
#' @name methods-print
NULL

#' Print cograph_network Object
#'
#' @param x A cograph_network object.
#' @param ... Ignored.
#' @return Invisible x.
#' @export
print.cograph_network <- function(x, ...) {
  n <- n_nodes(x)
  e <- n_edges(x)
  dir_str <- if (is_directed(x)) "directed" else "undirected"

  cat("Cograph network:", n, "nodes,", e, "edges (", dir_str, ")\n", sep = " ")

  # Show weight range if there are edges
  if (e > 0) {
    w <- get_edges(x)$weight
    w_range <- range(w, na.rm = TRUE)
    if (w_range[1] != w_range[2]) {
      cat("Weights:", round(w_range[1], 3), "to", round(w_range[2], 3), "\n")
    } else {
      cat("Weights:", round(w_range[1], 3), "(all equal)\n")
    }
  }

  # Show source
  src <- x$meta$source
  if (!is.null(src) && src != "unknown") {
    cat("Source:", src, "\n")
  }

  # Show layout status
  nodes <- get_nodes(x)
  has_layout <- "x" %in% names(nodes) && !all(is.na(nodes$x))
  cat("Layout:", if (has_layout) "set" else "none", "\n")

  # Show data if present
  if (!is.null(x$data)) {
    d <- x$data
    dim_str <- if (!is.null(dim(d))) {
      paste0("(", paste(dim(d), collapse = " x "), ")")
    } else {
      paste0("(length ", length(d), ")")
    }
    cat("Data:", class(d)[1], dim_str, "\n")
  }

  invisible(x)
}
