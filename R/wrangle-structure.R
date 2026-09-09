#' @title Structural Network Wrangling Verbs
#' @description Verbs that change the shape of a network rather than its
#'   weights: directedness, node contraction, components, cores.
#' @name wrangle-structure
#' @keywords internal
NULL

#' Remove Isolated Nodes
#'
#' Drops every node with no edges. Filtering edges deliberately keeps nodes
#' (see \code{\link{filter_edges}}), so this is the explicit way to prune the
#' isolates a filter left behind.
#'
#' @param x Network input: cograph_network, matrix, igraph, network, tna, or
#'   an edge-list data frame.
#' @param keep_format Logical. If TRUE, matrix, igraph, statnet network and tna
#'   inputs are returned in that format. Default FALSE returns a
#'   cograph_network.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} with the isolated nodes removed (or the
#'   input format when \code{keep_format = TRUE}). Node order is otherwise
#'   preserved and edge indices are remapped to the new node numbering.
#'
#' @seealso \code{\link{filter_edges}}, \code{\link{split_components}},
#'   \code{\link{filter_nodes}}
#'
#' @export
#' @examples
#' adj <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' adj["A", "B"] <- adj["B", "A"] <- 1
#'
#' # C and D have no edges
#' remove_isolates(adj)
remove_isolates <- function(x, keep_format = FALSE, directed = NULL) {
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  connected <- .connected_nodes(get_edges(net))

  if (length(connected) == 0L) {
    return(.finish_result(.empty_cograph_network(net$directed, meta = net$meta),
                          x, input_class, keep_format))
  }

  .finish_result(.rebuild_network(net, nodes_keep = connected),
                 x, input_class, keep_format)
}

# =============================================================================
# Directedness
# =============================================================================

#' Convert a Directed Network to Undirected
#'
#' Collapses each pair of opposite arcs into one undirected edge. The
#' counterpart of \code{igraph::as_undirected()} and tidygraph's
#' \code{to_undirected()}.
#'
#' @param x Network input.
#' @param method How to combine \code{w[i, j]} and \code{w[j, i]}:
#'   \code{"max"} (default), \code{"sum"}, \code{"mean"}, \code{"min"}, or
#'   \code{"mutual"} (keep only reciprocated pairs, taking the minimum weight).
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. Directedness to read the input with.
#'
#' @return An undirected \code{cograph_network}, or the input format when
#'   \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{to_directed}}, \code{\link{symmetrize}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, 0,
#'                 .2, 0, .7,
#'                 0, 0, 0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#'
#' to_undirected(adj, method = "sum")
#' to_undirected(adj, method = "mutual")
to_undirected <- function(x, method = c("max", "sum", "mean", "min", "mutual"),
                          keep_format = FALSE, directed = NULL) {
  method <- match.arg(method)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  m <- to_matrix(net)

  combined <- switch(method,
    max = pmax(m, t(m)),
    min = pmin(m, t(m)),
    mean = (m + t(m)) / 2,
    sum = { s <- m + t(m); diag(s) <- diag(m); s },
    mutual = { both <- (m != 0) & (t(m) != 0); pmin(m, t(m)) * both }
  )

  .finish_result(.network_from_matrix(net, combined, directed = FALSE),
                 x, input_class, keep_format)
}

#' Convert an Undirected Network to Directed
#'
#' @param x Network input.
#' @param mode \code{"mutual"} (default) creates an arc in both directions for
#'   every undirected edge; \code{"arbitrary"} keeps one arc per edge, running
#'   from the lower node index to the higher.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. Directedness to read the input with.
#'
#' @return A directed \code{cograph_network}, or the input format when
#'   \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{to_undirected}}, \code{\link{reverse_edges}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0,
#'                 1, 0, 1,
#'                 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#'
#' to_directed(adj)
#' to_directed(adj, mode = "arbitrary")
to_directed <- function(x, mode = c("mutual", "arbitrary"),
                        keep_format = FALSE, directed = NULL) {
  mode <- match.arg(mode)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  m <- to_matrix(net)

  out <- if (mode == "mutual") {
    pmax(m, t(m))
  } else {
    lower <- m
    lower[lower.tri(lower)] <- 0
    lower
  }

  .finish_result(.network_from_matrix(net, out, directed = TRUE),
                 x, input_class, keep_format)
}

#' Reverse Edge Direction
#'
#' Transposes the weight matrix, so every arc runs the other way. TNA users
#' reach for this to look at where transitions came from rather than where they
#' went.
#'
#' @param x Network input.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} with every edge reversed, or the input
#'   format when \code{keep_format = TRUE}. An undirected network is returned
#'   unchanged, with a \code{cograph_no_effect} warning.
#'
#' @seealso \code{\link{to_directed}}, \code{\link{to_undirected}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, 0,
#'                 0, 0, .7,
#'                 0, 0, 0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#'
#' reverse_edges(adj)
reverse_edges <- function(x, keep_format = FALSE, directed = NULL) {
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)

  if (!isTRUE(net$directed)) {
    warning(warningCondition(
      "The network is undirected; reversing its edges changes nothing.",
      class = "cograph_no_effect"))
    return(.finish_result(net, x, input_class, keep_format))
  }

  .finish_result(.network_from_matrix(net, t(to_matrix(net))),
                 x, input_class, keep_format)
}

# =============================================================================
# Components, cores and trees
# =============================================================================

#' Split a Network into Its Connected Components
#'
#' @param x Network input.
#' @param min_size Integer. Drop components smaller than this. Default 1
#'   (keep all, including isolated nodes).
#' @param keep_format Logical. Return each component in the input format.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A list of \code{cograph_network} objects, one per component, ordered
#'   from largest to smallest and named \code{"component_1"},
#'   \code{"component_2"}, and so on. Components are weakly connected, matching
#'   \code{igraph::components(mode = "weak")}.
#'
#' @seealso \code{\link{select_component}}, \code{\link{remove_isolates}}
#'
#' @export
#' @examples
#' adj <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
#' adj["A", "B"] <- adj["B", "A"] <- 1
#' adj["C", "D"] <- adj["D", "C"] <- 1
#'
#' parts <- split_components(adj)
#' length(parts)
split_components <- function(x, min_size = 1L, keep_format = FALSE,
                             directed = NULL) {
  .check_scalar_number(min_size, "min_size", lower = 0)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  cg <- .cg_graph(net)
  comp <- .cg_components_numbered(cg$b)

  order_by_size <- order(comp$csize, decreasing = TRUE)
  wanted <- order_by_size[comp$csize[order_by_size] >= min_size]

  parts <- lapply(wanted, function(id) {
    .finish_result(.rebuild_network(net, nodes_keep = which(comp$membership == id)),
                   x, input_class, keep_format)
  })
  stats::setNames(parts, paste0("component_", seq_along(parts)))
}

#' Select the k-Core of a Network
#'
#' The k-core is the maximal subgraph in which every node has degree at least
#' \code{k}, found by repeatedly removing nodes of degree below \code{k}.
#'
#' @param x Network input.
#' @param k Integer. The core number.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} holding the k-core, or the input format
#'   when \code{keep_format = TRUE}. An empty network when no node reaches
#'   coreness \code{k}.
#'
#' @seealso \code{\link{select_nodes}}, \code{\link{centrality}}
#'
#' @references
#' Seidman, S. B. (1983). Network structure and minimum degree.
#' \emph{Social Networks}, 5(3), 269--287.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1,
#'                 1, 0, 1, 0,
#'                 1, 1, 0, 0,
#'                 1, 0, 0, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' select_k_core(adj, k = 2)
select_k_core <- function(x, k, keep_format = FALSE, directed = NULL) {
  .check_scalar_number(k, "k", lower = 0)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  cg <- .cg_graph(net)
  core <- .cg_coreness_loops(cg$b, cg$n, cg$directed, "all")
  keep <- which(core >= k)

  if (length(keep) == 0L) {
    warning("No node reaches coreness ", k, ".", call. = FALSE)
    return(.finish_result(.empty_cograph_network(net$directed, meta = net$meta),
                          x, input_class, keep_format))
  }

  .finish_result(.rebuild_network(net, nodes_keep = keep),
                 x, input_class, keep_format)
}

#' Minimum or Maximum Spanning Tree
#'
#' Prim's algorithm on each connected component, so a disconnected network
#' yields a spanning forest.
#'
#' @param x Network input.
#' @param weights \code{"weight"} (default) uses the edge weights as costs;
#'   \code{"none"} treats every edge as cost 1.
#' @param maximum Logical. Find the maximum spanning tree instead of the
#'   minimum. Default FALSE. Set TRUE when the weights are similarities.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. Directedness to read the input with; the
#'   tree itself is undirected.
#'
#' @return An undirected \code{cograph_network} holding the spanning tree (or
#'   forest), or the input format when \code{keep_format = TRUE}. Every node is
#'   kept.
#'
#' @seealso \code{\link{disparity_filter}}, \code{\link{threshold_edges}}
#'
#' @references
#' Prim, R. C. (1957). Shortest connection networks and some generalizations.
#' \emph{Bell System Technical Journal}, 36(6), 1389--1401.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' spanning_tree(adj)
#' spanning_tree(adj, maximum = TRUE)
spanning_tree <- function(x, weights = c("weight", "none"), maximum = FALSE,
                          keep_format = FALSE, directed = NULL) {
  weights <- match.arg(weights)
  stopifnot("`maximum` must be TRUE or FALSE" = is.logical(maximum) && length(maximum) == 1L)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  m <- to_matrix(net)
  m <- pmax(m, t(m))
  diag(m) <- 0

  cost <- if (weights == "none") (m != 0) * 1 else m
  if (maximum) cost[m != 0] <- -cost[m != 0]

  tree <- .prim_forest(m != 0, cost)
  chosen <- matrix(0, nrow(m), ncol(m))
  chosen[tree] <- m[tree]
  chosen <- pmax(chosen, t(chosen))

  .finish_result(.network_from_matrix(net, chosen, directed = FALSE),
                 x, input_class, keep_format)
}

#' Prim's algorithm over every component
#'
#' @param present Logical adjacency matrix.
#' @param cost Numeric matrix of edge costs, read where `present` is TRUE.
#' @return A two-column integer matrix of the chosen (from, to) pairs.
#' @noRd
.prim_forest <- function(present, cost) {
  n <- nrow(present)
  if (n < 2L) {
    return(matrix(integer(0), ncol = 2L))
  }
  cost[!present] <- Inf

  in_tree <- rep(FALSE, n)
  edges_from <- integer(0)
  edges_to <- integer(0)

  # One iteration per node: Prim's frontier is inherently sequential, there is
  # no vectorised form. The inner work is a vectorised column scan.
  repeat {
    if (all(in_tree)) break
    if (!any(in_tree)) {
      in_tree[which(!in_tree)[1]] <- TRUE
      next
    }
    frontier <- cost[in_tree, !in_tree, drop = FALSE]
    if (all(is.infinite(frontier))) {
      # The current component is finished; start the next one.
      in_tree[which(!in_tree)[1]] <- TRUE
      next
    }
    pick <- arrayInd(which.min(frontier), dim(frontier))
    from <- which(in_tree)[pick[1, 1]]
    to <- which(!in_tree)[pick[1, 2]]
    edges_from <- c(edges_from, from)
    edges_to <- c(edges_to, to)
    in_tree[to] <- TRUE
  }

  cbind(edges_from, edges_to)
}

#' Complement of a Network
#'
#' Every pair of distinct nodes that is not joined in \code{x} is joined in the
#' complement, and vice versa.
#'
#' @param x Network input.
#' @param weight Numeric. Weight to give the new edges. Default 1.
#' @param loops Logical. Include self-loops in the complement. Default FALSE.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} holding the complement, or the input format
#'   when \code{keep_format = TRUE}. Directedness is preserved.
#'
#' @seealso \code{\link{to_undirected}}, \code{\link{binarize}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0,
#'                 1, 0, 0,
#'                 0, 0, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#'
#' complement_network(adj)
complement_network <- function(x, weight = 1, loops = FALSE,
                               keep_format = FALSE, directed = NULL) {
  .check_scalar_number(weight, "weight")
  stopifnot("`loops` must be TRUE or FALSE" = is.logical(loops) && length(loops) == 1L)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  m <- to_matrix(net)

  comp <- (m == 0) * weight
  if (!loops) diag(comp) <- 0

  .finish_result(.network_from_matrix(net, comp), x, input_class, keep_format)
}

# =============================================================================
# Node-level restructuring
# =============================================================================

#' Contract Nodes into Groups
#'
#' Replaces each group of nodes with a single node whose edges aggregate the
#' edges of its members. The counterpart of \code{igraph::contract()} and
#' tidygraph's \code{to_contracted()}, and the network form of what
#' \code{\link{summarize_clusters}()} computes inside an analysis object.
#'
#' @param x Network input.
#' @param groups Group assignment. Either a vector with one entry per node (in
#'   node order), or a named list mapping group name to node labels.
#' @param weight How to aggregate the weights of the edges that fall between
#'   two groups: \code{"sum"} (default), \code{"mean"}, \code{"max"} or
#'   \code{"min"}.
#' @param loops Logical. Keep the within-group edges as self-loops on the
#'   contracted node. Default FALSE.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} with one node per group, labelled by group
#'   name, or the input format when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{summarize_clusters}}, \code{\link{detect_communities}},
#'   \code{\link{split_components}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 0,
#'                 1, 0, 0, 1,
#'                 1, 0, 0, 1,
#'                 0, 1, 1, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' contract_nodes(adj, groups = c("left", "left", "right", "right"))
contract_nodes <- function(x, groups, weight = c("sum", "mean", "max", "min"),
                           loops = FALSE, keep_format = FALSE, directed = NULL) {
  weight <- match.arg(weight)
  stopifnot("`loops` must be TRUE or FALSE" = is.logical(loops) && length(loops) == 1L)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  labels <- get_labels(net)
  membership <- .resolve_group_membership(groups, labels)

  group_names <- levels(membership)
  k <- length(group_names)
  m <- to_matrix(net)

  # Aggregate the n x n matrix down to k x k, one group pair at a time.
  totals <- .aggregate_by_group(m, membership, k, weight)
  if (!loops) diag(totals) <- 0

  nodes <- data.frame(
    id = seq_len(k),
    label = group_names,
    name = group_names,
    x = NA_real_,
    y = NA_real_,
    stringsAsFactors = FALSE
  )
  contracted <- .create_cograph_network(
    nodes = nodes,
    edges = data.frame(from = integer(0), to = integer(0), weight = numeric(0)),
    directed = isTRUE(net$directed),
    meta = net$meta,
    weights = totals,
    data = net$data
  )

  .finish_result(.network_from_matrix(contracted, totals), x, input_class, keep_format)
}

#' Turn a group specification into a factor with one entry per node
#' @noRd
.resolve_group_membership <- function(groups, labels) {
  n <- length(labels)

  if (is.list(groups) && !is.data.frame(groups)) {
    if (is.null(names(groups))) {
      .stop_bad_selection("`groups` given as a list must be named.")
    }
    assignment <- rep(NA_character_, n)
    # One pass per group: assign every member of that group at once.
    invisible(lapply(names(groups), function(g) {
      idx <- match(as.character(groups[[g]]), labels)
      if (anyNA(idx)) {
        .stop_bad_selection("`groups` names nodes that are not in the network: ",
                            paste(as.character(groups[[g]])[is.na(idx)], collapse = ", "), ".")
      }
      assignment[idx] <<- g
    }))
    if (anyNA(assignment)) {
      .stop_bad_selection("`groups` leaves ", sum(is.na(assignment)),
                          " node(s) unassigned: ",
                          paste(labels[is.na(assignment)], collapse = ", "), ".")
    }
    return(factor(assignment, levels = names(groups)))
  }

  if (length(groups) != n) {
    .stop_bad_selection("`groups` must have one entry per node (", n,
                        "); got ", length(groups), ".")
  }
  if (anyNA(groups)) {
    .stop_bad_selection("`groups` contains NA.")
  }
  factor(groups)
}

#' Aggregate a weight matrix down to group level
#' @noRd
.aggregate_by_group <- function(m, membership, k, how) {
  idx <- as.integer(membership)
  # rowsum() sums within groups down the rows; doing it twice, with a
  # transpose between, gives the k x k block sums in two vectorised passes.
  block_sum <- function(mat) {
    t(rowsum(t(rowsum(mat, idx, reorder = TRUE)), idx, reorder = TRUE))
  }

  sums <- block_sum(m)
  if (how == "sum") {
    return(sums)
  }
  counts <- block_sum((m != 0) * 1)

  if (how == "mean") {
    out <- sums / counts
    out[counts == 0] <- 0
    return(out)
  }

  # max and min need the extreme over the block, which rowsum cannot give.
  extreme <- if (how == "max") max else min
  out <- matrix(0, k, k)
  cells <- which(counts > 0, arr.ind = TRUE)
  if (nrow(cells) > 0L) {
    out[cells] <- vapply(seq_len(nrow(cells)), function(i) {
      block <- m[idx == cells[i, 1L], idx == cells[i, 2L], drop = FALSE]
      nz <- block[block != 0]
      if (length(nz) == 0L) 0 else extreme(nz)
    }, numeric(1))
  }
  out
}

#' Reorder the Nodes of a Network
#'
#' Changes the order the nodes are stored in, which is the order plotting
#' functions lay them out in. The network itself is unchanged.
#'
#' @param x Network input.
#' @param order Node labels or indices, in the wanted order, or one of
#'   \code{"label"}, \code{"degree"}, \code{"strength"} to sort by. Sorting by
#'   a measure is descending.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} with the nodes in the requested order and
#'   edge indices remapped, or the input format when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{rename_nodes}}, \code{\link{select_nodes}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1,
#'                 1, 0, 1, 0,
#'                 1, 1, 0, 0,
#'                 1, 0, 0, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' get_labels(reorder_nodes(adj, order = "degree"))
#' get_labels(reorder_nodes(adj, order = c("D", "C", "B", "A")))
reorder_nodes <- function(x, order, keep_format = FALSE, directed = NULL) {
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  nodes <- get_nodes(net)
  n <- nrow(nodes)

  new_order <- if (is.character(order) && length(order) == 1L &&
                   order %in% c("label", "degree", "strength")) {
    cg <- .cg_graph(net)
    switch(order,
      label = base::order(nodes$label),
      degree = base::order(.cg_degree(cg$b, cg$directed, "all"), decreasing = TRUE),
      strength = base::order(.cg_strength(cg$w, cg$directed, "all"), decreasing = TRUE)
    )
  } else {
    .resolve_node_selection_ordered(nodes, order, "order")
  }

  if (length(new_order) != n) {
    .stop_bad_selection("`order` must list every node exactly once (", n,
                        "); got ", length(new_order), ".")
  }

  .finish_result(.reindex_network(net, new_order), x, input_class, keep_format)
}

#' Resolve a node selection, keeping the caller's order
#'
#' `.resolve_node_selection()` sorts, because a *set* of nodes has no order.
#' Reordering needs the order the caller gave.
#' @noRd
.resolve_node_selection_ordered <- function(nodes, selection, arg) {
  if (is.character(selection)) {
    idx <- match(selection, nodes$label)
    if (anyNA(idx)) {
      .stop_bad_selection("`", arg, "` names nodes that are not in the network: ",
                          paste(selection[is.na(idx)], collapse = ", "), ".")
    }
    return(idx)
  }
  .validate_indices(selection, nrow(nodes), arg)
  as.integer(selection)
}

#' Permute a network's nodes into a new order
#' @noRd
.reindex_network <- function(net, new_order) {
  nodes <- get_nodes(net)[new_order, , drop = FALSE]
  nodes$id <- seq_along(new_order)
  rownames(nodes) <- NULL

  edges <- get_edges(net)
  if (nrow(edges) > 0L) {
    edges$from <- match(edges$from, new_order)
    edges$to <- match(edges$to, new_order)
    rownames(edges) <- NULL
  }

  .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = net$directed,
    meta = net$meta,
    weights = .network_weight_matrix(as.character(nodes$label), edges,
                                     isTRUE(net$directed)),
    data = net$data,
    node_groups = net$node_groups
  )
}

#' Rename Nodes
#'
#' @param x Network input.
#' @param from Character vector of current labels, or a named character vector
#'   mapping old label to new (in which case \code{to} is not used).
#' @param to Character vector of new labels, the same length as \code{from}.
#' @param keep_format Logical. Return the input format when TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @return A \code{cograph_network} with the renamed nodes, or the input format
#'   when \code{keep_format = TRUE}. Labels not named in \code{from} are left
#'   alone.
#'
#' @seealso \code{\link{reorder_nodes}}, \code{\link{set_nodes}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 0), 2, 2)
#' rownames(adj) <- colnames(adj) <- c("A", "B")
#'
#' get_labels(rename_nodes(adj, from = "A", to = "Alpha"))
#' get_labels(rename_nodes(adj, from = c(A = "Alpha", B = "Beta")))
rename_nodes <- function(x, from, to = NULL, keep_format = FALSE,
                         directed = NULL) {
  if (is.null(to)) {
    if (is.null(names(from))) {
      .stop_bad_selection("Give either `from` and `to`, or a named vector as `from`.")
    }
    to <- unname(from)
    from <- names(from)
  }
  if (length(from) != length(to)) {
    .stop_bad_selection("`from` and `to` must be the same length; got ",
                        length(from), " and ", length(to), ".")
  }

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  nodes <- get_nodes(net)

  idx <- match(as.character(from), nodes$label)
  if (anyNA(idx)) {
    .stop_bad_selection("`from` names nodes that are not in the network: ",
                        paste(as.character(from)[is.na(idx)], collapse = ", "), ".")
  }

  nodes$label[idx] <- as.character(to)
  if ("name" %in% names(nodes)) {
    nodes$name[idx] <- as.character(to)
  }
  if (anyDuplicated(nodes$label) > 0L) {
    .stop_bad_selection("Renaming would give two nodes the same label: ",
                        paste(unique(nodes$label[duplicated(nodes$label)]), collapse = ", "), ".")
  }

  renamed <- net
  renamed$nodes <- nodes
  renamed$weights <- .network_weight_matrix(as.character(nodes$label),
                                            get_edges(net), isTRUE(net$directed))
  if (!is.null(renamed$node_groups) && "node" %in% names(renamed$node_groups)) {
    map <- stats::setNames(as.character(to), as.character(from))
    hit <- renamed$node_groups$node %in% names(map)
    renamed$node_groups$node[hit] <- map[renamed$node_groups$node[hit]]
  }

  .finish_result(renamed, x, input_class, keep_format)
}
