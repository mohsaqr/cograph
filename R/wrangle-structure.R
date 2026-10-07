#' @title Structural Network Wrangling Verbs
#' @description Verbs that change the shape of a network rather than its
#'   weights: directedness, node contraction, components, cores.
#' @name wrangle-structure
#' @keywords internal
#' @noRd
NULL

#' Remove Isolated Nodes
#'
#' Removes every node with no edges. Edge filters such as
#' \code{\link{filter_edges}} keep all nodes, and this function removes the
#' isolates that such a filter leaves behind.
#'
#' @param x Network input: cograph_network, matrix, igraph, network, tna, or
#'   an edge-list data frame.
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return A \code{cograph_network} with the isolated nodes removed (or the
#'   input format when \code{keep_format = TRUE}). The remaining nodes keep
#'   their order, and edge indices are remapped to the new node numbering.
#'
#' @seealso \code{\link{filter_edges}}, \code{\link{split_components}},
#'   \code{\link{filter_nodes}}
#'
#' @export
#' @examples
#' remove_isolates(threshold_edges(regulation_net, minimum = 0.3))
remove_isolates <- function(x, keep_format = FALSE, directed = NULL) {
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  connected <- .connected_nodes(get_edges(net))

  if (length(connected) == 0L) {
    return(.finish_result(.empty_cograph_network(net$directed, meta = net$meta, data = net$data),
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
#' Collapses each pair of opposite arcs into one undirected edge, as
#' \code{igraph::as_undirected()} and tidygraph's \code{to_undirected()} do.
#'
#' @param x Network input.
#' @param method How to combine \code{w[i, j]} and \code{w[j, i]}. One of
#'   \code{"max"} (default), \code{"sum"}, \code{"mean"}, \code{"min"} or
#'   \code{"mutual"}. The first four combine the two weights when both arcs
#'   exist, and an arc without a reverse arc keeps its own weight.
#'   \code{"mutual"} keeps only reciprocated pairs, at the smaller of the two
#'   weights.
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return An undirected \code{cograph_network}, or the input format when
#'   \code{keep_format = TRUE}. Self-loops keep their weight. A weight of zero
#'   means no edge, so a pair whose combined weight is exactly zero is dropped.
#'   Under \code{method = "mutual"} this applies to every unreciprocated arc,
#'   and under \code{"sum"} to a pair of opposite weights that cancel. Dropped
#'   edges raise a \code{cograph_edges_dropped} warning.
#'
#' @seealso \code{\link{to_directed}}, \code{\link{symmetrize}}
#'
#' @export
#' @examples
#' to_undirected(regulation_net, method = "sum")
to_undirected <- function(x, method = c("max", "sum", "mean", "min", "mutual"),
                          keep_format = FALSE, directed = NULL) {
  method <- match.arg(method)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  m <- to_matrix(net)

  combined <- if (method == "mutual") {
    both <- (m != 0) & (t(m) != 0)
    pmin(m, t(m)) * both
  } else {
    .combine_arcs(m, t(m), method)
  }
  # A self-loop is one arc, not a reciprocated pair: it must not be combined
  # with its own transpose.
  diag(combined) <- diag(m)

  .warn_cancelled_edges(m, t(m), combined)

  .finish_result(.network_from_matrix(net, combined, directed = FALSE),
                 x, input_class, keep_format)
}

#' Convert an Undirected Network to Directed
#'
#' @param x Network input.
#' @param mode \code{"mutual"} (default) creates an arc in both directions for
#'   every undirected edge. \code{"arbitrary"} keeps one arc per edge, running
#'   from the lower node index to the higher. For a directed input,
#'   \code{"mutual"} gives both arcs of a pair the larger of the two weights,
#'   and \code{"arbitrary"} drops every arc from a higher to a lower index.
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return A directed \code{cograph_network}, or the input format when
#'   \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{to_undirected}}, \code{\link{reverse_edges}}
#'
#' @export
#' @examples
#' to_directed(to_undirected(regulation_net))
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
#' Swaps the endpoints of every edge, which transposes the weight matrix. In a
#' transition network the reversed arcs show where each transition came from.
#' Additional edge columns are kept.
#'
#' @param x Network input.
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return A \code{cograph_network} with every edge reversed, or the input
#'   format when \code{keep_format = TRUE}. An undirected network is returned
#'   unchanged, with a \code{cograph_no_effect} warning.
#'
#' @seealso \code{\link{to_directed}}, \code{\link{to_undirected}}
#'
#' @export
#' @examples
#' reverse_edges(regulation_net)
reverse_edges <- function(x, keep_format = FALSE, directed = NULL) {
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)

  if (!isTRUE(net$directed)) {
    warning(warningCondition(
      "The network is undirected; reversing its edges changes nothing.",
      class = "cograph_no_effect"))
    return(.finish_result(net, x, input_class, keep_format))
  }

  # Swap the endpoints in the edge table rather than transposing the matrix:
  # the edge set is unchanged, so extra edge columns have no reason to be lost.
  edges <- get_edges(net)
  swapped <- edges
  swapped$from <- edges$to
  swapped$to <- edges$from

  .finish_result(.rebuild_network(net, edges = swapped),
                 x, input_class, keep_format)
}

# =============================================================================
# Components, cores and trees
# =============================================================================

#' Split a Network into Its Connected Components
#'
#' @param x Network input.
#' @param min_size Integer. Components with fewer nodes are dropped. Default 1
#'   keeps every component, including isolated nodes.
#' @param keep_format Logical. If TRUE, each component of a matrix, igraph,
#'   statnet network or tna input is returned in that format. An edge-list
#'   data frame or a qgraph object gives \code{cograph_network} components
#'   with a \code{cograph_no_format_roundtrip} warning. Default FALSE.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return A list of \code{cograph_network} objects, one per component, ordered
#'   from largest to smallest and named \code{"component_1"},
#'   \code{"component_2"}, and so on. Components are weakly connected, matching
#'   \code{igraph::components(mode = "weak")}. A network with no nodes gives an
#'   empty list and a warning.
#'
#' @seealso \code{\link{select_component}}, \code{\link{remove_isolates}}
#'
#' @export
#' @examples
#' split_components(threshold_edges(regulation_net, minimum = 0.3))
split_components <- function(x, min_size = 1L, keep_format = FALSE,
                             directed = NULL) {
  .check_count(min_size, "min_size", min = 0)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)

  if (n_nodes(net) == 0L) {
    warning("Network has no nodes", call. = FALSE)
    return(list())
  }

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
#' Degree is the total degree, which is in-degree plus out-degree in a
#' directed network. A self-loop adds 2 to the degree of its node.
#'
#' @param x Network input.
#' @param k A single non-negative whole number. The core number.
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return A \code{cograph_network} holding the k-core, or the input format
#'   when \code{keep_format = TRUE}. When no node reaches coreness \code{k},
#'   the result is an empty network and a warning is raised.
#'
#' @seealso \code{\link{select_nodes}}, \code{\link{centrality}}
#'
#' @references
#' Seidman, S. B. (1983). Network structure and minimum degree.
#' \emph{Social Networks}, 5(3), 269--287.
#'
#' @export
#' @examples
#' select_k_core(regulation_net, k = 2)
select_k_core <- function(x, k, keep_format = FALSE, directed = NULL) {
  .check_count(k, "k", min = 0)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  cg <- .cg_graph(net)
  core <- .cg_coreness_loops(cg$b, cg$n, cg$directed, "all")
  keep <- which(core >= k)

  if (length(keep) == 0L) {
    warning("No node reaches coreness ", k, ".", call. = FALSE)
    return(.finish_result(.empty_cograph_network(net$directed, meta = net$meta, data = net$data),
                          x, input_class, keep_format))
  }

  .finish_result(.rebuild_network(net, nodes_keep = keep),
                 x, input_class, keep_format)
}

#' Minimum or Maximum Spanning Tree
#'
#' Computes a spanning tree with Prim's algorithm on each connected component,
#' so a disconnected network yields a spanning forest. A directed network is
#' symmetrized first, each pair taking the larger of its two arc weights.
#' Self-loops are ignored. Missing or infinite weights raise a
#' \code{cograph_bad_selection} error.
#'
#' @param x Network input.
#' @param weights \code{"weight"} (default) uses the edge weights as costs;
#'   \code{"none"} treats every edge as cost 1.
#' @param maximum Logical. Find the maximum spanning tree instead of the
#'   minimum. Default FALSE. Set TRUE when the weights are similarities.
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input. The tree itself is undirected.
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
#' spanning_tree(regulation_net, maximum = TRUE)
spanning_tree <- function(x, weights = c("weight", "none"), maximum = FALSE,
                          keep_format = FALSE, directed = NULL) {
  weights <- match.arg(weights)
  stopifnot("`maximum` must be TRUE or FALSE" = is.logical(maximum) && length(maximum) == 1L)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  m <- to_matrix(net)
  .check_finite_weights(m, "spanning_tree")
  m <- .combine_arcs(m, t(m), "max")
  diag(m) <- 0

  cost <- if (weights == "none") (m != 0) * 1 else m
  if (maximum) cost[m != 0] <- -cost[m != 0]

  tree <- .prim_forest(m != 0, cost)
  chosen <- matrix(0, nrow(m), ncol(m))
  # Mirror by assignment, not by pmax(): a selected negative weight compared
  # with the structural zero of the empty transpose would come back as zero,
  # deleting exactly the edges Prim just chose.
  if (nrow(tree) > 0L) {
    chosen[tree] <- m[tree]
    chosen[tree[, c(2L, 1L), drop = FALSE]] <- m[tree]
  }

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
  # no vectorized form. The inner work is a vectorized column scan.
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
#' complement, and every joined pair is absent from it.
#'
#' @param x Network input.
#' @param weight Numeric. Weight of every edge in the complement. Default 1. A
#'   weight of zero means no edge, so \code{weight = 0} raises a
#'   \code{cograph_bad_selection} error.
#' @param loops Logical. Include self-loops in the complement. Default FALSE.
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return A \code{cograph_network} holding the complement, or the input format
#'   when \code{keep_format = TRUE}. Directedness is preserved.
#'
#' @seealso \code{\link{to_undirected}}, \code{\link{binarize}}
#'
#' @export
#' @examples
#' complement_network(threshold_edges(regulation_net, minimum = 0.2))
complement_network <- function(x, weight = 1, loops = FALSE,
                               keep_format = FALSE, directed = NULL) {
  .check_scalar_number(weight, "weight")
  if (weight == 0) {
    # Zero is how this representation stores "no edge", so a complement of
    # weight zero would contain nothing at all.
    .stop_bad_selection("`weight` must not be zero: zero means 'no edge', so ",
                        "the complement would be empty.")
  }
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
#' edges of its members. The operation corresponds to \code{igraph::contract()}
#' and tidygraph's \code{to_contracted()}, and it is the network form of the
#' aggregation computed by \code{\link{summarize_clusters}()}.
#'
#' @param x Network input.
#' @param groups Group assignment. Either a vector with one entry per node, in
#'   node order, or a named list mapping each group name to node labels. A list
#'   must assign every node. Malformed input raises a
#'   \code{cograph_bad_selection} error.
#' @param weight How to aggregate the weights of the edges that fall between
#'   two groups. One of \code{"sum"} (default), \code{"mean"}, \code{"max"}
#'   or \code{"min"}. Within-group edges are aggregated the same way when
#'   \code{loops = TRUE}.
#' @param loops Logical. Keep the within-group edges as self-loops on the
#'   contracted node. Default FALSE.
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return A \code{cograph_network} with one node per group, labeled by group
#'   name, or the input format when \code{keep_format = TRUE}. The groups
#'   follow the factor levels of a vector (sorted for a character vector) or
#'   the order of a list.
#'
#' @seealso \code{\link{summarize_clusters}}, \code{\link{detect_communities}},
#'   \code{\link{split_components}}
#'
#' @export
#' @examples
#' contract_nodes(regulation_net, groups = rep(c("Plan", "Act"), each = 5))
contract_nodes <- function(x, groups, weight = c("sum", "mean", "max", "min"),
                           loops = FALSE, keep_format = FALSE, directed = NULL) {
  weight <- match.arg(weight)
  stopifnot("`loops` must be TRUE or FALSE" = is.logical(loops) && length(loops) == 1L)

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  labels <- get_labels(net)

  if (length(labels) == 0L) {
    warning("Network has no nodes", call. = FALSE)
    return(.finish_result(.empty_cograph_network(net$directed, meta = net$meta, data = net$data),
                          x, input_class, keep_format))
  }

  membership <- .resolve_group_membership(groups, labels)

  group_names <- levels(membership)
  k <- length(group_names)

  # Aggregate the *edge table*, not the matrix. An undirected network stores
  # one row per unordered pair but a symmetric matrix holds each of them
  # twice, so summing matrix blocks counts every within-group edge twice.
  totals <- .aggregate_edges_by_group(get_edges(net), as.integer(membership), k,
                                      weight, isTRUE(net$directed))
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

#' Aggregate an edge table to group level
#'
#' One row per group pair. For an undirected network the group pair is
#' canonicalized (low index first) so that A-B and B-A land in the same cell,
#' and the result is mirrored once at the end.
#'
#' @param edges Edge table with `from`, `to`, `weight` in node-index space.
#' @param idx Integer group index per node.
#' @param k Number of groups.
#' @param how `"sum"`, `"mean"`, `"max"` or `"min"`.
#' @param directed Logical.
#' @return A k x k numeric matrix.
#' @noRd
.aggregate_edges_by_group <- function(edges, idx, k, how, directed) {
  out <- matrix(0, k, k)
  if (k == 0L || is.null(edges) || nrow(edges) == 0L) {
    return(out)
  }

  g_from <- idx[edges$from]
  g_to <- idx[edges$to]
  if (!directed) {
    lo <- pmin(g_from, g_to)
    hi <- pmax(g_from, g_to)
    g_from <- lo
    g_to <- hi
  }

  cell <- (g_to - 1L) * k + g_from
  fn <- switch(how, sum = sum, mean = mean, max = max, min = min)
  agg <- tapply(as.numeric(edges$weight), cell, fn)
  out[as.integer(names(agg))] <- as.numeric(agg)

  if (!directed) {
    # Only the canonical (low, high) cells were written; mirror them once.
    filled <- which(out != 0, arr.ind = TRUE)
    off <- filled[filled[, 1L] != filled[, 2L], , drop = FALSE]
    if (nrow(off) > 0L) {
      out[off[, c(2L, 1L), drop = FALSE]] <- out[off]
    }
  }
  out
}

#' Reorder the Nodes of a Network
#'
#' Changes the order in which the nodes are stored, which is the order in which
#' plotting functions place them. The edges and their weights are unchanged.
#'
#' @param x Network input.
#' @param order Node labels or indices, in the wanted order, or one of
#'   \code{"label"}, \code{"degree"} or \code{"strength"}. \code{"label"}
#'   sorts alphabetically, and the two measures sort in decreasing order. A
#'   vector that does not name every node exactly once raises a
#'   \code{cograph_bad_selection} error.
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return A \code{cograph_network} with the nodes in the requested order and
#'   edge indices remapped, or the input format when \code{keep_format = TRUE}.
#'
#' @seealso \code{\link{rename_nodes}}, \code{\link{select_nodes}}
#'
#' @export
#' @examples
#' reorder_nodes(regulation_net, order = "degree")
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

  if (length(new_order) != n || !identical(sort(as.integer(new_order)), seq_len(n))) {
    # Length alone is not enough: c("A", "A", "B") on a three-node network is
    # the right length but duplicates one node and drops another, which
    # produces duplicate labels or an NA subscript during the rebuild.
    .stop_bad_selection("`order` must be a permutation naming every node ",
                        "exactly once (", n, " nodes); got ", length(new_order),
                        " entr", if (length(new_order) == 1L) "y" else "ies",
                        " covering ", length(unique(new_order)), " node(s).")
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
#' @param keep_format Logical. If TRUE, a matrix, igraph, statnet network or
#'   tna input is returned in its own format. An edge-list data frame or a
#'   qgraph object is returned as a \code{cograph_network} with a
#'   \code{cograph_no_format_roundtrip} warning. Default FALSE returns a
#'   \code{cograph_network}.
#' @param directed Logical or NULL. Directedness used to read the input. NULL
#'   (default) detects it from the input.
#'
#' @return A \code{cograph_network} with the renamed nodes, or the input format
#'   when \code{keep_format = TRUE}. Labels not named in \code{from} are
#'   unchanged. A \code{cograph_bad_selection} error is raised when
#'   \code{from} names a node that is not in the network or when the renaming
#'   would give two nodes the same label.
#'
#' @seealso \code{\link{reorder_nodes}}, \code{\link{set_nodes}}
#'
#' @export
#' @examples
#' rename_nodes(regulation_net, from = "Plan", to = "Planning")
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
