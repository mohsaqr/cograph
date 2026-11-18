# =============================================================================
# Group network objects (one network per group)
# =============================================================================
#
# `netobject_group` (Nestimate, including `net_mlvar`) and `group_tna` (tna)
# are named lists holding one network per group. The analytic verbs take them
# whole and return one tidy data frame with a leading `group` column.

#' Is `x` a group network object?
#' @param x Any object.
#' @return TRUE for a `netobject_group` or a tna group model.
#' @keywords internal
#' @noRd
.is_network_group <- function(x) {
  inherits(x, c("netobject_group", "group_tna", "group_ctna", "group_ftna",
                "group_atna"))
}

#' Stop when a one-network function hands a group object on
#'
#' `centrality()` and `network_summary()` return one row set per group. A
#' cograph function built for one network (the `centrality_*()` shortcuts,
#' `reaching_global()`, the centrality plots) would read those stacked rows
#' as one network and return pooled numbers, so it stops here instead.
#'
#' @param caller The function that called the verb, or NULL at top level.
#' @return NULL, invisibly; raises `cograph_group_input` when `caller` is a
#'   cograph function.
#' @keywords internal
#' @noRd
.stop_if_group_passed_on <- function(caller) {
  if (is.function(caller) && identical(environment(caller), topenv())) {
    stop(errorCondition(paste0(
      "this function takes one network, and `x` holds one network per group. ",
      "Use centrality() or network_summary() on the group object, or pass ",
      "one group."),
      class = "cograph_group_input", call = NULL))
  }
  invisible(NULL)
}

#' The networks of a group object, each on its own nodes
#'
#' Group builders give every group the node set of the whole data, so a node
#' that belongs to another group is carried as an isolate. With
#' `shared_nodes = FALSE` a node is kept in a group when it has a tie there
#' (self-loops count) or occurs in the group's own data, and dropped otherwise.
#'
#' @param x A group network object.
#' @param shared_nodes Logical. Keep the common node set in every group.
#' @return Named list of networks, one per group.
#' @keywords internal
#' @noRd
.group_members <- function(x, shared_nodes = FALSE) {
  if (!isTRUE(shared_nodes) && !isFALSE(shared_nodes)) {
    .cg_stop_bad_parameter("`shared_nodes` must be TRUE or FALSE")
  }
  members <- as.list(unclass(x))
  if (length(members) == 0L) {
    stop(errorCondition("the group network object holds no network",
                        class = "cograph_empty_input", call = NULL))
  }
  if (is.null(names(members))) {
    names(members) <- as.character(seq_along(members))
  }
  if (shared_nodes) {
    return(members)
  }

  # A node belongs to a group when it has a tie there or occurs in the group's
  # own data (a member who took part without forming a tie).
  tied <- lapply(members, function(net) {
    w <- net$weights
    w[is.na(w)] <- 0
    rowSums(w != 0) + colSums(w != 0) > 0 | .nodes_in_data(net)
  })
  empty <- names(members)[!vapply(tied, any, logical(1))]
  if (length(empty) > 0L) {
    warning(warningCondition(
      sprintf("group %s has no node of its own, so it keeps the shared node set",
              paste(empty, collapse = ", ")),
      class = "cograph_empty_group", call = NULL))
  }
  # A group that already uses every node is passed on untouched.
  Map(function(net, keep) {
    if (all(keep) || !any(keep)) net else .drop_untied_nodes(net, keep)
  }, members, tied)
}

#' Which nodes of a group member occur in that member's own data
#'
#' A member built from sequences keeps its slice of the data: a data frame or
#' character matrix of states (Nestimate), or an integer-coded matrix with a
#' `labels` attribute (tna). A member built from variables (correlation
#' networks) has its nodes as the data's columns.
#'
#' @param net A tna model or a `cograph_network`.
#' @return Logical, one per node, in the order of the weight matrix. All FALSE
#'   when the member carries no data.
#' @keywords internal
#' @noRd
.nodes_in_data <- function(net) {
  labels <- rownames(net$weights) %||% net$labels %||% net$nodes$label
  data <- net$data
  if (is.null(labels) || is.null(data)) {
    return(logical(nrow(net$weights)))
  }
  coded <- attr(data, "labels")
  seen <- if (is.numeric(data) && !is.null(coded)) {
    coded[unique(as.vector(data[!is.na(data)]))]
  } else if (is.data.frame(data)) {
    unique(unlist(lapply(data, as.character), use.names = FALSE))
  } else {
    unique(as.character(data))
  }
  labels %in% c(seen, colnames(data))
}

#' Drop the nodes of one group member that are absent from it
#'
#' The node set is read from the weight matrix, the one field a tna model and
#' a `cograph_network` share. A tna model keeps its class (and with it the
#' tna conventions of the analytic verbs) and its sequence data.
#'
#' @param net A tna model or a `cograph_network`.
#' @param keep Logical, one per node: belongs to this group.
#' @return `net` restricted to the nodes in `keep`.
#' @keywords internal
#' @noRd
.drop_untied_nodes <- function(net, keep) {
  if (!inherits(net, "tna")) {
    return(.rebuild_network(net, nodes_keep = which(keep)))
  }
  net$weights <- net$weights[keep, keep, drop = FALSE]
  if (!is.null(net$labels)) net$labels <- net$labels[keep]
  if (!is.null(net$inits)) net$inits <- net$inits[keep]
  net
}

#' Run a verb on every network of a group object and stack the rows
#'
#' Re-evaluates the caller's own call once per group, with `x` replaced by
#' that group's network, so every argument reaches the verb exactly as the
#' caller gave it.
#'
#' @param x A group network object.
#' @param fn The verb (a function returning a data frame).
#' @param call The verb's `match.call()`.
#' @param envir The verb's calling frame, where the arguments are evaluated.
#' @param shared_nodes See `.group_members()`.
#' @return A data frame: the verb's rows for every group, with a leading
#'   `group` column and default row names.
#' @keywords internal
#' @noRd
.map_network_group <- function(x, fn, call, envir, shared_nodes = FALSE) {
  members <- .group_members(x, shared_nodes)
  call[[1L]] <- fn
  call$shared_nodes <- NULL
  parts <- lapply(members, function(net) {
    call$x <- net
    eval(call, envir)
  })
  columns <- unique(unlist(lapply(parts, names), use.names = FALSE))
  stopifnot("the group column name `group` is already a result column" =
              !"group" %in% columns)
  # A measure that one group lacks is NA there, never a silently dropped column.
  parts <- Map(function(part, group) {
    part[setdiff(columns, names(part))] <- NA
    cbind(data.frame(group = rep(group, nrow(part)), stringsAsFactors = FALSE),
          part[columns])
  }, parts, names(parts))
  result <- do.call(rbind, parts)
  rownames(result) <- NULL
  result
}
