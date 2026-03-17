#' @title Print Methods
#' @description S3 print methods for Cograph objects.
#' @name methods-print
#' @keywords internal
NULL

#' Print cograph_network Object
#'
#' @param x A cograph_network object.
#' @param ... Ignored.
#' @return The input object \code{x}, invisibly.
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- cograph(adj)
#' print(net)
#'
#' @export
print.cograph_network <- function(x, ...) {
  nn <- n_nodes(x)
  ne <- n_edges(x)
  directed <- is_directed(x)
  dir_str <- if (directed) "directed" else "undirected"

  # ---- Header ----
  tna_meta <- x$meta$tna
  method <- tna_meta$method
  src <- x$meta$source %||% "unknown"
  if (!is.null(method)) {
    method_labels <- c(
      relative      = "Transition Network (relative probabilities)",
      frequency     = "Transition Network (frequency counts)",
      co_occurrence = "Co-occurrence Network",
      glasso        = "Partial Correlation Network (EBICglasso)",
      pcor          = "Partial Correlation Network (unregularised)",
      cor           = "Correlation Network",
      ising         = "Ising Model Network",
      attention     = "Attention Network (decay-weighted transitions)"
    )
    header <- if (method %in% names(method_labels)) method_labels[[method]]
              else sprintf("Network (method: %s)", method)
    cat(header, " [", dir_str, "]\n", sep = "")
  } else {
    cat("Cograph network:", nn, "nodes,", ne, "edges (", dir_str, ")\n")
  }

  # ---- Source ----
  if (!is.null(src) && src != "unknown") {
    cat("Source:", src, "\n")
  }

  # ---- Data ----
  if (!is.null(x$data)) {
    d <- x$data
    dim_str <- if (!is.null(dim(d))) {
      paste0("(", paste(dim(d), collapse = " x "), ")")
    } else {
      paste0("(length ", length(d), ")")
    }
    cat("Data:", class(d)[1], dim_str, "\n")
  }

  # ---- Nodes ----
  labels <- get_labels(x)
  cat(sprintf("  Nodes (%d): %s\n", nn,
              if (nn <= 8) paste(labels, collapse = ", ")
              else paste(c(labels[1:6],
                           sprintf("... +%d more", nn - 6)),
                         collapse = ", ")))

  # ---- Network structure from weights matrix ----
  mat <- x$weights
  if (!is.null(mat) && is.matrix(mat)) {
    max_possible <- if (directed) nn * (nn - 1) else nn * (nn - 1) / 2
    if (directed) {
      nz <- mat[mat != 0 & row(mat) != col(mat)]
    } else {
      nz <- mat[upper.tri(mat) & mat != 0]
    }
    n_edges_mat <- length(nz)
    density <- if (max_possible > 0) n_edges_mat / max_possible else 0
    cat(sprintf("  Edges: %d / %d (density: %.1f%%)\n",
                n_edges_mat, as.integer(max_possible), density * 100))

    if (length(nz) > 0) {
      has_neg <- any(nz < 0)
      if (has_neg) {
        cat(sprintf("  Weights: [%.3f, %.3f]  |  +%d / -%d edges\n",
                    min(nz), max(nz), sum(nz > 0), sum(nz < 0)))
      } else {
        cat(sprintf("  Weights: [%.3f, %.3f]  |  mean: %.3f\n",
                    min(nz), max(nz), mean(nz)))
      }

      # Top edges
      if (directed) {
        idx <- which(mat != 0 & row(mat) != col(mat), arr.ind = TRUE)
      } else {
        idx <- which(upper.tri(mat) & mat != 0, arr.ind = TRUE)
      }
      if (nrow(idx) > 0) {
        w <- mat[idx]
        top_k <- min(5L, nrow(idx))
        ord <- order(abs(w), decreasing = TRUE)[seq_len(top_k)]
        cat("  Strongest edges:\n")
        arrow <- if (directed) " -> " else " -- "
        for (j in ord) {
          cat(sprintf("    %s%s%s  %.3f\n",
                      labels[idx[j, 1]], arrow, labels[idx[j, 2]], w[j]))
        }
      }
    }

    # Self-loops
    diag_vals <- diag(mat)
    n_self <- sum(diag_vals != 0)
    if (n_self > 0) {
      cat(sprintf("  Self-loops: %d  |  range: [%.3f, %.3f]\n",
                  n_self, min(diag_vals[diag_vals != 0]),
                  max(diag_vals[diag_vals != 0])))
    }
  } else if (ne > 0) {
    # No weights matrix — use edge list summary
    w <- get_edges(x)$weight
    w_range <- range(w, na.rm = TRUE)
    if (w_range[1] != w_range[2]) {
      cat("Weights:", round(w_range[1], 3), "to", round(w_range[2], 3), "\n")
    } else {
      cat("Weights:", round(w_range[1], 3), "(all equal)\n")
    }
  }

  # ---- Layout ----
  nodes <- get_nodes(x)
  has_layout <- "x" %in% names(nodes) && !all(is.na(nodes$x))
  cat("Layout:", if (has_layout) "set" else "none", "\n")

  # ---- Groups ----
  grp <- x$node_groups
  if (!is.null(grp) && is.data.frame(grp) && nrow(grp) > 0) {
    grp_col <- intersect(c("layer", "cluster", "group"), names(grp))[1]
    if (!is.na(grp_col)) {
      n_grp <- length(unique(grp[[grp_col]]))
      cat(sprintf("  Groups: %d (%s)\n", n_grp, grp_col))
    }
  }

  invisible(x)
}
