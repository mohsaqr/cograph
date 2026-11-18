# =============================================================================
# splot(isolates = FALSE): draw a network without its unconnected nodes
# =============================================================================

#' Is `x` one network that isolates can be dropped from?
#' @param x Any splot() input.
#' @return TRUE for a matrix, igraph, statnet network, tna model or
#'   `cograph_network` (a Nestimate netobject included).
#' @keywords internal
#' @noRd
.is_single_network <- function(x) {
  !.is_network_group(x) &&
    (is.matrix(x) || inherits(x, c("igraph", "network", "tna",
                                   "cograph_network")))
}

#' Which nodes have at least one tie (self-loops count)
#' @param x A single network.
#' @return Logical, one per node, in node order.
#' @keywords internal
#' @noRd
.tied_nodes <- function(x) {
  w <- if (inherits(x, c("tna", "cograph_network")) && is.matrix(x$weights)) {
    x$weights
  } else {
    to_matrix(x)
  }
  w[is.na(w)] <- 0
  rowSums(w != 0) + colSums(w != 0) > 0
}

#' Restrict one network to a node subset, keeping its class
#'
#' A Nestimate netobject carries fields a plain `cograph_network` does not
#' (`method`, `initial`, `frequency_matrix`, ...). splot() styles by them, so
#' they are carried over: square per-node matrices and per-node vectors are
#' subset, the counts recomputed, everything else copied.
#'
#' @param x A single network.
#' @param keep Logical, one per node.
#' @return `x` restricted to the nodes in `keep`, in its own format.
#' @keywords internal
#' @noRd
.keep_nodes_in_format <- function(x, keep) {
  if (!inherits(x, "cograph_network")) {
    return(remove_isolates(x, keep_format = TRUE))
  }
  n <- length(keep)
  out <- .rebuild_network(x, nodes_keep = which(keep))
  extra <- setdiff(names(x), names(out))
  out[extra] <- lapply(x[extra], function(field) {
    if (is.matrix(field) && all(dim(field) == n)) {
      field[keep, keep, drop = FALSE]
    } else if (is.atomic(field) && length(field) == n && n > 1L) {
      field[keep]
    } else {
      field
    }
  })
  if ("n_nodes" %in% extra) out$n_nodes <- sum(keep)
  if ("n_edges" %in% extra) out$n_edges <- nrow(out$edges)
  class(out) <- class(x)
  out
}

#' Restrict per-node arguments to the nodes that stay
#'
#' An argument given for the full node set (one value per node, or a layout
#' with one row per node) follows the nodes that are kept. Arguments of any
#' other length are left as they are.
#'
#' @param args Named list of splot() arguments.
#' @param keep Logical, one per original node.
#' @return `args`, with per-node entries subset.
#' @keywords internal
#' @noRd
.keep_node_arguments <- function(args, keep) {
  n <- length(keep)
  per_node <- grepl("^(node_|label_|donut_|pie_)|^(labels|groups|layout|vsize|color|shape|predictability)$",
                    names(args))
  args[per_node] <- lapply(args[per_node], function(value) {
    if ((is.matrix(value) || is.data.frame(value)) && nrow(value) == n) {
      value[keep, , drop = FALSE]
    } else if (!is.matrix(value) && !is.data.frame(value) &&
               !is.function(value) && length(value) == n && n > 1L) {
      value[keep]
    } else {
      value
    }
  })
  args
}

#' Apply `isolates = FALSE` to a single network and its per-node arguments
#'
#' @param x A single network.
#' @param user_args,dots The argument lists splot() forwards.
#' @return List with `x`, `user_args` and `dots`. When no node has a tie the
#'   network is returned whole with a `cograph_empty_network` warning.
#' @keywords internal
#' @noRd
.splot_hide_isolates <- function(x, user_args, dots) {
  keep <- .tied_nodes(x)
  if (!any(keep)) {
    warning(warningCondition(
      "`isolates = FALSE`: no node has a tie, so every node is drawn",
      class = "cograph_empty_network", call = NULL))
  } else if (!all(keep)) {
    x <- .keep_nodes_in_format(x, keep)
    user_args <- .keep_node_arguments(user_args, keep)
    dots <- .keep_node_arguments(dots, keep)
  }
  list(x = x, user_args = user_args, dots = dots)
}
