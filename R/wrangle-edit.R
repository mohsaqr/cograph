#' @title Network Editing Verbs
#' @description Verbs that add, remove, mutate or combine nodes and edges.
#' @name wrangle-edit
#' @keywords internal
NULL

# =============================================================================
# Nodes
# =============================================================================

#' Add Nodes to a Network
#'
#' @param x Network input.
#' @param labels Character vector of labels for the new nodes.
#' @param ... Named vectors of node attributes for the new nodes, each of
#'   length 1 (recycled) or \code{length(labels)}. Columns the network does not
#'   already have are created and filled with \code{NA} for the existing nodes.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} with the new nodes appended (isolated until
#'   edges are added), or the input format when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{remove_nodes}}, \code{\link{add_edges}},
#'   \code{\link{mutate_nodes}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 0), 2, 2)
#' rownames(adj) <- colnames(adj) <- c("A", "B")
#'
#' add_nodes(adj, labels = c("C", "D"))
#' add_nodes(adj, labels = "C", group = "new")
add_nodes <- function(x, labels, ..., keep_format = FALSE, directed = NULL) {
  if (!is.character(labels) || length(labels) == 0L) {
    .stop_bad_selection("`labels` must be a non-empty character vector.")
  }

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  nodes <- get_nodes(net)

  clash <- intersect(labels, nodes$label)
  if (length(clash) > 0L) {
    .stop_bad_selection("These labels are already in the network: ",
                        paste(clash, collapse = ", "), ".")
  }
  if (anyDuplicated(labels) > 0L) {
    .stop_bad_selection("`labels` contains duplicates: ",
                        paste(unique(labels[duplicated(labels)]), collapse = ", "), ".")
  }

  new_rows <- create_nodes_df(length(labels), labels)
  attrs <- list(...)
  if (length(attrs) > 0L) {
    new_rows[names(attrs)] <- lapply(attrs, .recycle_to, n = length(labels))
  }

  combined <- .rbind_fill(nodes, new_rows)
  combined$id <- seq_len(nrow(combined))
  rownames(combined) <- NULL

  result <- net
  result$nodes <- combined
  result$weights <- .network_weight_matrix(as.character(combined$label),
                                           get_edges(net), isTRUE(net$directed))

  .finish_result(result, x, input_class, keep_format)
}

#' Recycle a value to length n, or fail loudly
#' @noRd
.recycle_to <- function(value, n) {
  if (length(value) == n) {
    return(value)
  }
  if (length(value) == 1L) {
    return(rep(value, n))
  }
  .stop_bad_selection("Attribute of length ", length(value),
                      " cannot be used for ", n, " node(s).")
}

#' Row-bind two data frames, filling missing columns with NA
#' @noRd
.rbind_fill <- function(a, b) {
  missing_in_b <- setdiff(names(a), names(b))
  missing_in_a <- setdiff(names(b), names(a))
  if (length(missing_in_b) > 0L) {
    b[missing_in_b] <- lapply(a[missing_in_b], function(col) rep(col[NA_integer_], nrow(b)))
  }
  if (length(missing_in_a) > 0L) {
    a[missing_in_a] <- lapply(b[missing_in_a], function(col) rep(col[NA_integer_], nrow(a)))
  }
  rbind(a, b[names(a)])
}

#' Remove Nodes from a Network
#'
#' @param x Network input.
#' @param nodes Node labels or indices to remove.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} without those nodes and without any edge
#'   that touched them, or the input format when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{add_nodes}}, \code{\link{filter_nodes}},
#'   \code{\link{remove_isolates}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#'
#' remove_nodes(adj, nodes = "B")
remove_nodes <- function(x, nodes, keep_format = FALSE, directed = NULL) {
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  node_df <- get_nodes(net)
  drop <- .resolve_node_selection(node_df, nodes, "nodes")
  keep <- setdiff(seq_len(nrow(node_df)), drop)

  if (length(keep) == 0L) {
    warning("Every node was removed.", call. = FALSE)
    return(.finish_result(.empty_cograph_network(net$directed, meta = net$meta),
                          x, input_class, keep_format))
  }

  .finish_result(.rebuild_network(net, nodes_keep = keep),
                 x, input_class, keep_format)
}

# =============================================================================
# Edges
# =============================================================================

#' Add Edges to a Network
#'
#' @param x Network input.
#' @param from Source nodes, by label or index.
#' @param to Target nodes, by label or index. The same length as \code{from}.
#' @param weight Numeric weight for the new edges, length 1 or
#'   \code{length(from)}. Default 1.
#' @param ... Named vectors of extra edge attributes, length 1 or
#'   \code{length(from)}.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} with the new edges, or the input format
#'   when \code{keep_format = TRUE}. An edge that already exists has its weight
#'   replaced, and a \code{cograph_edges_replaced} warning says how many.
#'
#' @note When the igraph package is attached it masks this function with
#'   \code{igraph::add_edges()}, which takes an igraph object. Use
#'   \code{cograph::add_edges()} to be explicit.
#'
#' @seealso \code{\link{remove_edges}}, \code{\link{add_nodes}},
#'   \code{\link{bind_networks}}
#'
#' @export
#' @examples
#' adj <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
#' adj["A", "B"] <- adj["B", "A"] <- 1
#'
#' add_edges(adj, from = "B", to = "C", weight = 0.5)
add_edges <- function(x, from, to, weight = 1, ..., keep_format = FALSE,
                      directed = NULL) {
  if (length(from) != length(to)) {
    .stop_bad_selection("`from` and `to` must be the same length; got ",
                        length(from), " and ", length(to), ".")
  }
  if (length(from) == 0L) {
    .stop_bad_selection("`from` and `to` must name at least one edge.")
  }

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  node_df <- get_nodes(net)

  from_idx <- .resolve_node_selection_ordered(node_df, from, "from")
  to_idx <- .resolve_node_selection_ordered(node_df, to, "to")

  new_edges <- data.frame(
    from = from_idx,
    to = to_idx,
    weight = .recycle_to(as.numeric(weight), length(from_idx))
  )
  attrs <- list(...)
  if (length(attrs) > 0L) {
    new_edges[names(attrs)] <- lapply(attrs, .recycle_to, n = nrow(new_edges))
  }

  existing <- get_edges(net)
  duplicate <- .edge_key(new_edges, isTRUE(net$directed)) %in%
    .edge_key(existing, isTRUE(net$directed))
  if (any(duplicate)) {
    warning(warningCondition(
      paste0(sum(duplicate), " edge(s) already existed and had their weight replaced."),
      class = "cograph_edges_replaced"))
    existing <- existing[!(.edge_key(existing, isTRUE(net$directed)) %in%
                             .edge_key(new_edges, isTRUE(net$directed))), , drop = FALSE]
  }

  combined <- .rbind_fill(existing, new_edges)
  rownames(combined) <- NULL

  .finish_result(.rebuild_network(net, edges = combined),
                 x, input_class, keep_format)
}

#' A comparable key per edge, direction-aware
#' @noRd
.edge_key <- function(edges, directed) {
  if (is.null(edges) || nrow(edges) == 0L) {
    return(character(0))
  }
  if (directed) {
    paste(edges$from, edges$to, sep = "->")
  } else {
    paste(pmin(edges$from, edges$to), pmax(edges$from, edges$to), sep = "--")
  }
}

#' Remove Edges from a Network
#'
#' @param x Network input.
#' @param from Source nodes, by label or index.
#' @param to Target nodes, by label or index. The same length as \code{from}.
#' @param keep_isolates Logical. Keep nodes that end up with no edges? Default
#'   TRUE, matching \code{\link{filter_edges}}.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} without those edges, or the input format
#'   when \code{keep_format = TRUE}. Named pairs that carry no edge are
#'   reported in a \code{cograph_no_such_edge} warning.
#'
#' @seealso \code{\link{add_edges}}, \code{\link{filter_edges}},
#'   \code{\link{remove_isolates}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#'
#' remove_edges(adj, from = "A", to = "B")
remove_edges <- function(x, from, to, keep_isolates = TRUE,
                         keep_format = FALSE, directed = NULL) {
  if (length(from) != length(to)) {
    .stop_bad_selection("`from` and `to` must be the same length; got ",
                        length(from), " and ", length(to), ".")
  }

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  node_df <- get_nodes(net)
  edges <- get_edges(net)

  target <- data.frame(
    from = .resolve_node_selection_ordered(node_df, from, "from"),
    to = .resolve_node_selection_ordered(node_df, to, "to")
  )
  is_dir <- isTRUE(net$directed)
  target_keys <- .edge_key(target, is_dir)
  drop <- .edge_key(edges, is_dir) %in% target_keys

  absent <- setdiff(target_keys, .edge_key(edges, is_dir))
  if (length(absent) > 0L) {
    warning(warningCondition(
      paste0(length(absent), " named pair(s) carry no edge: ",
             paste(absent, collapse = ", "), "."),
      class = "cograph_no_such_edge"))
  }

  kept <- edges[!drop, , drop = FALSE]
  result <- .update_cograph_edges(net, kept, keep_isolates = keep_isolates)
  if (isTRUE(keep_isolates)) {
    .warn_new_isolates(edges, kept, n_nodes(net))
  }

  .finish_result(result, x, input_class, keep_format)
}

# =============================================================================
# Attribute mutation
# =============================================================================

#' Add or Change Node Attributes
#'
#' Evaluates expressions against the node table, with the same centrality and
#' structural vocabulary that \code{\link{select_nodes}()} offers, and stores
#' the results as node columns.
#'
#' @param x Network input.
#' @param ... Named expressions, for example \code{hub = degree > 3} or
#'   \code{score = pagerank * 100}. Available names are the existing node
#'   columns plus every measure and predicate listed under
#'   \code{\link{select_nodes}}.
#' @param keep_format Logical. Return the input format when TRUE. Note that
#'   only igraph and cograph_network formats can carry node attributes; a
#'   matrix cannot, and the new columns are lost.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} whose node table has the new columns, or
#'   the input format when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{mutate_edges}}, \code{\link{select_nodes}},
#'   \code{\link{centrality}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1,
#'                 1, 0, 1, 0,
#'                 1, 1, 0, 0,
#'                 1, 0, 0, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' as.data.frame(mutate_nodes(adj, deg = degree, hub = degree >= 3),
#'               what = "nodes")
mutate_nodes <- function(x, ..., keep_format = FALSE, directed = NULL) {
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  nodes <- get_nodes(net)

  dots <- substitute(list(...))[-1]
  .check_named_expressions(dots, "mutate_nodes")

  eval_env <- .build_filter_env(
    nodes,
    .node_filter_vars(.cg_graph(net), .detect_needed_variables(dots)),
    parent.frame()
  )
  values <- .evaluate_mutations(dots, eval_env, nrow(nodes))

  nodes[names(values)] <- values
  result <- net
  result$nodes <- nodes

  .finish_result(result, x, input_class, keep_format)
}

#' Add or Change Edge Attributes
#'
#' @param x Network input.
#' @param ... Named expressions evaluated against the edge table, with the same
#'   metrics and predicates \code{\link{select_edges}()} offers, for example
#'   \code{strong = abs_weight > 0.5} or \code{scaled = weight / max(weight)}.
#' @param community Community detection method used when an expression refers
#'   to \code{same_community}, \code{from_community} or \code{to_community}.
#'   Default \code{"louvain"}.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} whose edge table has the new columns, or
#'   the input format when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{mutate_nodes}}, \code{\link{select_edges}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' as.data.frame(mutate_edges(adj, strong = weight > 0.5))
mutate_edges <- function(x, ..., community = "louvain", keep_format = FALSE,
                         directed = NULL) {
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  edges <- get_edges(net)
  nodes <- get_nodes(net)

  dots <- substitute(list(...))[-1]
  .check_named_expressions(dots, "mutate_edges")

  needed <- .detect_needed_edge_variables(dots)
  metrics <- .compute_lazy_edge_metrics(.cg_graph(net), edges, nodes, needed, community)
  eval_env <- .build_filter_env(edges, metrics, parent.frame())
  values <- .evaluate_mutations(dots, eval_env, nrow(edges))

  edges[names(values)] <- values
  result <- .rebuild_network(net, edges = edges)

  .finish_result(result, x, input_class, keep_format)
}

#' Every mutation must be named, or the new column has no name
#' @noRd
.check_named_expressions <- function(dots, fn) {
  if (length(dots) == 0L) {
    .stop_bad_selection(fn, "() needs at least one named expression.")
  }
  nms <- names(dots)
  if (is.null(nms) || any(nms == "")) {
    .stop_bad_selection("Every expression passed to ", fn,
                        "() must be named, as in `deg = degree`.")
  }
  invisible(nms)
}

#' Evaluate mutation expressions to columns of length n
#' @noRd
.evaluate_mutations <- function(dots, env, n) {
  stats::setNames(lapply(names(dots), function(nm) {
    value <- eval(dots[[nm]], envir = env)
    if (length(value) == 1L) {
      value <- rep(value, n)
    }
    if (length(value) != n) {
      .stop_bad_selection("`", nm, "` produced ", length(value),
                          " value(s) but the table has ", n, " row(s).")
    }
    # Later expressions can see earlier ones, as in dplyr::mutate().
    assign(nm, value, envir = env)
    value
  }), names(dots))
}

# =============================================================================
# Combining networks
# =============================================================================

#' Combine Two Networks
#'
#' Aligns two networks on node labels and combines their edges.
#'
#' @param x,y Network inputs.
#' @param method How to combine the edge sets:
#'   \describe{
#'     \item{\code{"union"}}{(default) every edge of either network, over the
#'       union of the node sets}
#'     \item{\code{"intersection"}}{only edges present in both, over the nodes
#'       common to both}
#'     \item{\code{"difference"}}{edges of \code{x} that are not in \code{y},
#'       over the nodes of \code{x}}
#'   }
#' @param weight How to combine the weights of an edge present in both:
#'   \code{"sum"} (default), \code{"mean"}, \code{"max"}, \code{"min"}, or
#'   \code{"first"} (keep \code{x}'s weight).
#' @param keep_format Logical. Return \code{x}'s format when TRUE.
#' @param directed Logical or NULL. If NULL (default), the result is directed
#'   when either input is.
#'
#' @return A \code{cograph_network} over the combined node set, or \code{x}'s
#'   format when \code{keep_format = TRUE}. Nodes are ordered with \code{x}'s
#'   first, then any node only \code{y} has.
#'
#' @seealso \code{\link{add_edges}}, \code{\link{plot_difference}}
#'
#' @export
#' @examples
#' a <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
#' a["A", "B"] <- a["B", "A"] <- 1
#' b <- matrix(0, 3, 3, dimnames = list(c("B", "C", "D"), c("B", "C", "D")))
#' b["B", "C"] <- b["C", "B"] <- 2
#'
#' bind_networks(a, b)
#' bind_networks(a, b, method = "difference")
bind_networks <- function(x, y, method = c("union", "intersection", "difference"),
                          weight = c("sum", "mean", "max", "min", "first"),
                          keep_format = FALSE, directed = NULL) {
  method <- match.arg(method)
  weight <- match.arg(weight)

  input_class <- .detect_input_class(x)
  net_x <- as_cograph(x, directed = directed)
  net_y <- as_cograph(y, directed = directed)

  labels_x <- get_labels(net_x)
  labels_y <- get_labels(net_y)

  labels <- switch(method,
    union = c(labels_x, setdiff(labels_y, labels_x)),
    intersection = intersect(labels_x, labels_y),
    difference = labels_x
  )
  if (length(labels) == 0L) {
    .stop_bad_selection("The two networks share no nodes.")
  }

  is_dir <- if (!is.null(directed)) isTRUE(directed) else
    isTRUE(net_x$directed) || isTRUE(net_y$directed)

  mx <- .align_matrix(to_matrix(net_x), labels)
  my <- .align_matrix(to_matrix(net_y), labels)
  if (is_dir) {
    mx <- .as_directed_matrix(mx, net_x)
    my <- .as_directed_matrix(my, net_y)
  }

  combined <- switch(method,
    union = .combine_weights(mx, my, weight),
    intersection = .combine_weights(mx, my, weight) * ((mx != 0) & (my != 0)),
    difference = mx * (my == 0)
  )

  nodes <- data.frame(
    id = seq_along(labels),
    label = labels,
    name = labels,
    x = NA_real_,
    y = NA_real_,
    stringsAsFactors = FALSE
  )
  skeleton <- .create_cograph_network(
    nodes = nodes,
    edges = data.frame(from = integer(0), to = integer(0), weight = numeric(0)),
    directed = is_dir,
    meta = list(source = net_x$meta$source %||% "unknown"),
    weights = combined
  )

  .finish_result(.network_from_matrix(skeleton, combined, directed = is_dir),
                 x, input_class, keep_format)
}

#' Place a weight matrix into a larger, label-indexed frame
#' @noRd
.align_matrix <- function(m, labels) {
  out <- matrix(0, length(labels), length(labels),
                dimnames = list(labels, labels))
  shared <- intersect(labels, rownames(m))
  if (length(shared) > 0L) {
    out[shared, shared] <- m[shared, shared, drop = FALSE]
  }
  out
}

#' An undirected network read as directed means arcs in both directions
#' @noRd
.as_directed_matrix <- function(m, net) {
  if (isTRUE(net$directed)) m else pmax(m, t(m))
}

#' Combine two aligned weight matrices
#' @noRd
.combine_weights <- function(a, b, how) {
  switch(how,
    sum = a + b,
    max = pmax(a, b),
    min = { both <- (a != 0) & (b != 0); ifelse(both, pmin(a, b), a + b) },
    first = { a_missing <- a == 0; a * !a_missing + b * a_missing },
    mean = {
      counts <- (a != 0) + (b != 0)
      totals <- a + b
      counts[counts == 0] <- 1
      totals / counts
    }
  )
}
