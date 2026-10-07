#' @title Target and Saqr Layouts
#' @description Focal-node flow layouts: `target` (ported from qgraph's
#'   `flow()`) and `saqr` (ported from the Dynalytics Desktop
#'   transition-network viewer).
#' @name layout-target-saqr
#' @keywords internal
#' @noRd
NULL

#' Extract node count, labels, and a directed weight matrix from a network.
#'
#' Works for both the R6 \code{CographNetwork} (the form the layout registry
#' passes) and an S3 \code{cograph_network}.
#'
#' @param network A \code{CographNetwork} or \code{cograph_network} object.
#' @return List with \code{n}, \code{labels}, and the \code{n x n} directed
#'   weight matrix \code{W} (rows = source, cols = target).
#' @keywords internal
#' @noRd
.target_saqr_extract <- function(network) {
  is_r6 <- inherits(network, "R6")
  nodes <- if (is_r6) network$get_nodes() else get_nodes(network)
  edges <- if (is_r6) network$get_edges() else get_edges(network)
  n <- if (is_r6) network$n_nodes else n_nodes(network)

  labels <- if (!is.null(nodes) && !is.null(nodes$label)) {
    as.character(nodes$label)
  } else {
    as.character(seq_len(n))
  }

  W <- matrix(0, nrow = n, ncol = n)
  if (!is.null(edges) && nrow(edges) > 0) {
    w <- if (!is.null(edges$weight)) edges$weight else rep(1, nrow(edges))
    idx <- cbind(as.integer(edges$from), as.integer(edges$to))
    W[idx] <- w
  }

  list(n = n, labels = labels, W = W)
}

#' Target Layout (focal-node, topological)
#'
#' Adapts the \code{flow()} layout of qgraph. One node of interest (the
#' \code{target}) is placed alone, and every other node is placed in successive
#' levels ordered by its unweighted graph distance (number of hops) from the
#' target. Edge direction is ignored. The layout shows how the target node
#' connects to the rest of the network.
#'
#' Weights are binarized for layering, so only connectivity matters. Nodes
#' that cannot be reached from the target are placed in one extra level after
#' the last. qgraph raises an error for such nodes.
#'
#' @param network A \code{CographNetwork} or \code{cograph_network} object.
#' @param target Node of interest, given as a label (character) or 1-based
#'   index. When \code{NULL} (default) the node with the most neighbors is
#'   used. A label that is not found or an index out of range raises an
#'   error.
#' @param horizontal Logical. If \code{TRUE} (default) levels flow left to right
#'   with the target node on the left; if \code{FALSE} they flow top to bottom.
#' @param equalize Logical. If \code{TRUE} (default) nodes are evenly spaced
#'   within each level.
#' @param ... Additional arguments (ignored).
#' @return Data frame with \code{x}, \code{y} coordinates, one row per node.
#'
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' layout_target(CographNetwork$new(regulation_net), target = "Plan")
#'
#' @export
layout_target <- function(network, target = NULL, horizontal = TRUE,
                          equalize = TRUE, ...) {
  info <- .target_saqr_extract(network)
  n <- info$n
  labels <- info$labels
  W <- info$W

  if (n == 0) return(data.frame(x = numeric(0), y = numeric(0)))  # nocov
  if (n == 1) return(data.frame(x = 0.5, y = 0.5))

  # Binarized, undirected adjacency (qgraph layers on connectivity, not weight).
  adj <- (W != 0)
  adj <- adj | t(adj)
  diag(adj) <- FALSE

  # Resolve the target node.
  if (is.null(target)) {
    target_idx <- which.max(rowSums(adj))
  } else if (is.character(target)) {
    target_idx <- match(target[1], labels)
    if (is.na(target_idx)) {
      stop("target layout: 'target' label not found: ", target[1],
           call. = FALSE)
    }
  } else {
    target_idx <- as.integer(target[1])
    if (target_idx < 1 || target_idx > n) {
      stop("target layout: 'target' index out of range", call. = FALSE)
    }
  }

  # BFS hop distance from the target node (weights = NA forces unweighted).
  g <- igraph::graph_from_adjacency_matrix(adj * 1, mode = "undirected",
                                           diag = FALSE)
  dist <- as.numeric(igraph::distances(g, v = target_idx, weights = NA))

  # Disconnected nodes go into one extra trailing level.
  finite_max <- suppressWarnings(max(dist[is.finite(dist)]))
  if (!is.finite(finite_max)) finite_max <- 0
  dist[!is.finite(dist)] <- finite_max + 1

  # Even spacing within each level (qgraph's "equalize").
  place_level <- function(lv) {
    idx <- which(dist == lv)
    k <- length(idx)
    pos <- if (equalize) {
      seq(0, 1, length.out = k + 2)[-c(1, k + 2)]
    } else if (k == 1) {
      0.5
    } else {
      seq(0, 1, length.out = k)
    }
    data.frame(node = idx, pos = pos)
  }
  spread <- do.call(rbind, lapply(sort(unique(dist)), place_level))

  secondary <- numeric(n)
  secondary[spread$node] <- spread$pos
  primary <- dist  # level = distance from focal node

  if (horizontal) {
    coords <- data.frame(x = primary, y = secondary)
  } else {
    coords <- data.frame(x = secondary, y = -primary)
  }
  coords
}

#' Saqr Layout (Start/End transition flow)
#'
#' Places the nodes of a directed transition network in rows between a Start
#' and an End node (Saqr et al., LAK25). The Start node is alone on the top
#' row and the End node, when present, is alone on the bottom row. The other
#' nodes are ranked by the weight of the edge they receive from Start, with
#' the strongest nearest Start. They are split into two middle rows when there
#' are at most 10 of them and into three rows otherwise. A sine envelope
#' narrows the rows near Start and End, which gives the layout a lens shape.
#' The first middle row is offset in a zig-zag pattern.
#'
#' @param network A \code{CographNetwork} or \code{cograph_network} object.
#' @param start Label of the Start node (default \code{"Start"}). When the
#'   label is not found, the node with the largest sum of outgoing weights is
#'   used.
#' @param end Label of the End node (default \code{"End"}). The End row is
#'   omitted when the label is not found.
#' @param jitter Numeric in \code{[0, 1]}. Zig-zag amount applied to the first
#'   middle row, as a fraction of the row spacing (default 0.32).
#' @param ... Additional arguments (ignored).
#' @return Data frame with \code{x}, \code{y} coordinates, one row per node.
#'
#' @examples
#' layout_saqr(CographNetwork$new(regulation_net), start = "Explore",
#'   end = "Share")
#'
#' @export
layout_saqr <- function(network, start = "Start", end = "End",
                        jitter = 0.32, ...) {
  info <- .target_saqr_extract(network)
  n <- info$n
  labels <- info$labels
  W <- info$W

  if (n == 0) return(data.frame(x = numeric(0), y = numeric(0)))  # nocov
  if (n == 1) return(data.frame(x = 0.5, y = 0.5))

  # Outgoing weight per node, excluding self-loops.
  out_sum <- rowSums(W) - diag(W)

  start_idx <- if (!is.null(start)) match(start[1], labels) else NA_integer_
  if (is.na(start_idx)) start_idx <- which.max(out_sum)

  end_idx <- if (!is.null(end)) match(end[1], labels) else NA_integer_
  has_end <- !is.na(end_idx)

  # Middle nodes ranked by outgoing weight from Start (strongest first).
  middle <- setdiff(seq_len(n), c(start_idx, if (has_end) end_idx))
  middle <- middle[order(-W[start_idx, middle], middle)]

  nm <- length(middle)
  if (nm == 0) {
    mid_layers <- list()
  } else if (nm <= 10) {
    cut <- ceiling(nm / 2)
    mid_layers <- list(middle[seq_len(cut)], middle[-seq_len(cut)])
  } else {
    cut1 <- ceiling(nm / 3)
    cut2 <- ceiling(nm * 2 / 3)
    mid_layers <- list(middle[seq_len(cut1)],
                       middle[(cut1 + 1):cut2],
                       middle[(cut2 + 1):nm])
  }
  mid_layers <- Filter(function(l) length(l) > 0, mid_layers)

  total_rows <- 1L + length(mid_layers) + as.integer(has_end)

  # Row y (0 = Start on top); flipped because cograph's y-axis points up.
  row_y <- function(ri) if (total_rows == 1) 0.5 else 1 - ri / (total_rows - 1)
  row_spacing <- if (total_rows > 1) 1 / (total_rows - 1) else 1
  jitter_amt <- row_spacing * jitter

  # Lens envelope: sine narrows rows near the Start/End poles.
  envelope <- function(ri) {
    if (total_rows > 1) sin(pi * ri / (total_rows - 1)) else 1
  }

  # Horizontal spread across the enveloped width of a row.
  spread_row <- function(layer, ri) {
    size <- length(layer)
    pis <- seq_len(size) - 1L
    xs <- if (size == 1) {
      0.5
    } else {
      0.5 + (pis / (size - 1) - 0.5) * 2 * (0.5 * envelope(ri))
    }
    data.frame(node = layer, x = xs, ri = ri, pis = pis)
  }

  # Place all middle rows; ri runs 1, 2, (3).
  mid_df <- if (length(mid_layers) > 0) {
    rows <- lapply(seq_along(mid_layers), function(li) {
      df <- spread_row(mid_layers[[li]], li)
      # Zig-zag jitter only on the first middle row.
      df$y <- row_y(li) + if (li == 1) {
        ifelse(df$pis %% 2 == 0, jitter_amt, -jitter_amt)
      } else {
        0
      }
      df[, c("node", "x", "y")]
    })
    do.call(rbind, rows)
  } else {
    NULL
  }

  x <- rep(0.5, n)
  y <- rep(0.5, n)

  x[start_idx] <- 0.5
  y[start_idx] <- row_y(0)
  if (!is.null(mid_df)) {
    x[mid_df$node] <- mid_df$x
    y[mid_df$node] <- mid_df$y
  }
  if (has_end) {
    x[end_idx] <- 0.5
    y[end_idx] <- row_y(total_rows - 1)
  }

  data.frame(x = x, y = y)
}
