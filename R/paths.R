#' Compute Shortest Path Distances
#'
#' Computes shortest path distances between nodes in a network. Supports
#' all-pairs, single-source, and point-to-point queries.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param from Character or numeric node identifier(s) for the source. If NULL
#'   (default), compute distances from all nodes.
#' @param to Character or numeric node identifier(s) for the target. If NULL
#'   (default), compute distances to all nodes.
#' @param weights Edge weight handling: NULL (default) auto-detects from edge
#'   attributes, NA forces unweighted distances, or a numeric vector of custom
#'   weights.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Depends on the query:
#' \itemize{
#'   \item If both \code{from} and \code{to} are NULL: a full distance matrix
#'     (all pairs)
#'   \item If \code{from} is a single node and \code{to} is NULL: a named
#'     numeric vector of distances from that node to all others
#'   \item If \code{from} is multiple nodes and \code{to} is NULL: a matrix
#'     with rows for each source
#'   \item If both \code{from} and \code{to} are single nodes: a single numeric
#'     value
#'   \item Otherwise: a matrix of distances between the specified node sets
#' }
#'
#' @details
#' Uses \code{igraph::distances()} internally. For weighted networks, edge
#' weights are used as distances by default. Pass \code{weights = NA} to
#' ignore weights and treat all edges as having unit distance.
#'
#' Note: \code{igraph::distances()} with \code{weights = NULL} automatically
#' uses edge weight attributes if present. To force unweighted computation,
#' pass \code{weights = NA} explicitly.
#'
#' @export
#' @examples
#' # All-pairs distances
#' adj <- matrix(c(
#'   0, 1, 0, 0,
#'   1, 0, 1, 0,
#'   0, 1, 0, 1,
#'   0, 0, 1, 0
#' ), 4, 4)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:4]
#' cograph::shortest_paths(adj)
#'
#' # Single source to all
#' cograph::shortest_paths(adj, from = "A")
#'
#' # Point-to-point
#' cograph::shortest_paths(adj, from = "A", to = "D")
#'
#' @seealso \code{\link{k_shortest_paths}}, \code{\link{network_summary}}
shortest_paths <- function(x,
                           from = NULL,
                           to = NULL,
                           weights = NULL,
                           directed = NULL,
                           ...) {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for shortest_paths()", call. = FALSE)
  }

  # Convert input to igraph
  g <- to_igraph(x, directed = directed, ...)

  n <- igraph::vcount(g)
  node_names <- igraph::V(g)$name
  if (is.null(node_names)) {
    node_names <- as.character(seq_len(n))
    igraph::V(g)$name <- node_names
  }

  # Resolve from/to node identifiers to igraph vertex sequences
  v_from <- if (is.null(from)) igraph::V(g) else .resolve_nodes(g, from)
  v_to <- if (is.null(to)) igraph::V(g) else .resolve_nodes(g, to)

  # Build arguments for igraph::distances
  dist_args <- list(
    graph = g,
    v = v_from,
    to = v_to,
    mode = if (igraph::is_directed(g)) "out" else "all"
  )

  # Handle weights: NULL -> auto, NA -> unweighted, numeric -> custom
  if (is.null(weights)) {
    # Let igraph auto-detect (uses edge weights if present)
    dist_args$weights <- NULL
  } else if (identical(weights, NA)) {
    dist_args$weights <- NA
  } else {
    stopifnot(is.numeric(weights))
    dist_args$weights <- weights
  }

  d <- do.call(igraph::distances, dist_args)

  # Determine return type based on query shape
  single_from <- !is.null(from) && length(from) == 1L
  single_to <- !is.null(to) && length(to) == 1L

  if (single_from && single_to) {
    # Single value
    return(as.numeric(d[1, 1]))
  }

  if (single_from && is.null(to)) {
    # Named vector: single source to all
    result <- as.numeric(d[1, ])
    names(result) <- colnames(d)
    return(result)
  }

  # Matrix result (all-pairs, multi-source, or multi-target)
  d
}


#' Find K Shortest Loopless Paths (Yen's Algorithm)
#'
#' Computes up to \code{k} shortest loopless paths between two nodes using
#' Yen's algorithm. Each path is a sequence of distinct nodes from source to
#' target.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param from Character or numeric node identifier for the source node.
#' @param to Character or numeric node identifier for the target node.
#' @param k Integer; number of shortest paths to find. Default 3.
#' @param weights Edge weight handling: NULL (default) auto-detects from edge
#'   attributes, NA forces unweighted distances, or a numeric vector of custom
#'   weights.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A list with class "cograph_k_paths" containing:
#' \describe{
#'   \item{paths}{List of up to \code{k} character vectors, each containing node
#'     names in path order}
#'   \item{distances}{Numeric vector of path lengths (sum of edge weights or hop
#'     count)}
#'   \item{from}{Source node name}
#'   \item{to}{Target node name}
#'   \item{k}{Number of paths requested}
#' }
#'
#' @details
#' Yen's algorithm finds the k shortest loopless (simple) paths in a graph.
#' It works by:
#' \enumerate{
#'   \item Finding the shortest path via Dijkstra's algorithm
#'   \item For each subsequent path, systematically exploring deviations from
#'     previously found paths by temporarily removing edges, finding spur paths,
#'     and selecting the shortest candidate
#' }
#'
#' The algorithm may return fewer than \code{k} paths if fewer distinct loopless
#' paths exist between the two nodes.
#'
#' @references
#' Yen, J.Y. (1971). Finding the K shortest loopless paths in a network.
#' \emph{Management Science}, 17(11), 712-716.
#' \doi{10.1287/mnsc.17.11.712}
#'
#' @export
#' @examples
#' # Find 3 shortest paths in a small network
#' adj <- matrix(c(
#'   0, 1, 1, 0, 0,
#'   0, 0, 1, 1, 0,
#'   0, 0, 0, 1, 1,
#'   0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 0
#' ), 5, 5, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' kp <- cograph::k_shortest_paths(adj, from = "A", to = "E", k = 3)
#' kp
#'
#' @seealso \code{\link{shortest_paths}}
k_shortest_paths <- function(x,
                             from,
                             to,
                             k = 3,
                             weights = NULL,
                             directed = NULL,
                             ...) {

  stopifnot(
    length(from) == 1L,
    length(to) == 1L,
    is.numeric(k),
    length(k) == 1L,
    k >= 1L
  )
  k <- as.integer(k)

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for k_shortest_paths()", call. = FALSE)
  }

  # Convert input to igraph
  g <- to_igraph(x, directed = directed, ...)

  node_names <- igraph::V(g)$name
  if (is.null(node_names)) {
    node_names <- as.character(seq_len(igraph::vcount(g)))
    igraph::V(g)$name <- node_names
  }

  is_dir <- igraph::is_directed(g)

  # Resolve source and target
  v_from <- .resolve_nodes(g, from)
  v_to <- .resolve_nodes(g, to)
  from_name <- node_names[as.integer(v_from)]
  to_name <- node_names[as.integer(v_to)]

  # Determine edge weight vector for distance calculations
  edge_weights <- .resolve_path_weights(g, weights)

  # --- Yen's Algorithm ---

  # Step 1: Find the shortest path
  sp <- igraph::shortest_paths(
    g, from = v_from, to = v_to,
    mode = if (is_dir) "out" else "all",
    weights = edge_weights,
    output = "vpath"
  )
  first_path_vs <- as.integer(sp$vpath[[1]])

  if (length(first_path_vs) == 0L) {
    # No path exists
    result <- list(
      paths = list(),
      distances = numeric(0),
      from = from_name,
      to = to_name,
      k = k
    )
    class(result) <- "cograph_k_paths"
    return(result)
  }

  found_paths <- list(node_names[first_path_vs])
  found_distances <- .path_distance(g, first_path_vs, edge_weights)

  # Candidate pool: list of (path, distance) not yet accepted
  candidates <- list()  # each element: list(path = char_vec, dist = numeric)

  # Step 2: Find paths 2..k
  path_idx <- 1L
  while (path_idx < k) {
    prev_path <- found_paths[[path_idx]]
    prev_path_idx <- match(prev_path, node_names)

    # For each spur node in the previous path (except the last node)
    spur_candidates <- lapply(seq_along(prev_path_idx[-length(prev_path_idx)]),
                              function(spur_pos) {
      spur_node <- prev_path_idx[spur_pos]
      root_path <- prev_path_idx[seq_len(spur_pos)]

      # Create a modified graph: remove edges that overlap with found paths
      g_mod <- g
      edges_to_remove <- integer(0)

      # Remove edges from spur node that are used by any found path sharing
      # the same root
      vapply(found_paths, function(fp) {
        fp_idx <- match(fp, node_names)
        if (length(fp_idx) >= spur_pos + 1L &&
            identical(fp_idx[seq_len(spur_pos)], root_path)) {
          # Remove the edge from spur_node to next node in this found path
          eid <- .get_edge_id(g_mod, fp_idx[spur_pos], fp_idx[spur_pos + 1L],
                              is_dir)
          if (length(eid) > 0L && eid > 0L) {
            edges_to_remove <<- c(edges_to_remove, eid)
          }
        }
        TRUE
      }, logical(1))

      # Remove root path nodes (except spur node) from the graph
      nodes_to_remove <- setdiff(root_path[-length(root_path)],
                                 integer(0))
      # Mark them by removing all their edges
      if (length(nodes_to_remove) > 0L) {
        incident_edges <- unlist(lapply(nodes_to_remove, function(v) {
          as.integer(igraph::incident(g_mod, v, mode = "all"))
        }))
        edges_to_remove <- unique(c(edges_to_remove, incident_edges))
      }

      if (length(edges_to_remove) > 0L) {
        edges_to_remove <- unique(edges_to_remove)
        # Recompute weights after removal
        mod_weights <- edge_weights
        if (!is.null(mod_weights) && !identical(mod_weights, NA)) {
          mod_weights <- mod_weights[-edges_to_remove]
        }
        g_mod <- igraph::delete_edges(g_mod, edges_to_remove)
      } else {
        mod_weights <- edge_weights
      }

      # Find shortest path from spur to target in modified graph
      spur_sp <- tryCatch(
        suppressWarnings(igraph::shortest_paths(
          g_mod, from = spur_node, to = v_to,
          mode = if (is_dir) "out" else "all",
          weights = mod_weights,
          output = "vpath"
        )),
        error = function(e) NULL
      )

      if (is.null(spur_sp) || length(as.integer(spur_sp$vpath[[1]])) == 0L) {
        return(NULL)
      }

      spur_path_vs <- as.integer(spur_sp$vpath[[1]])

      # Combine root + spur (root includes spur node, spur starts at spur)
      total_path_vs <- c(root_path[-length(root_path)], spur_path_vs)

      # Check for loops (must be loopless)
      if (anyDuplicated(total_path_vs)) return(NULL)

      total_path <- node_names[total_path_vs]
      total_dist <- .path_distance(g, total_path_vs, edge_weights)

      list(path = total_path, dist = total_dist)
    })

    # Add valid spur candidates to the pool
    valid <- Filter(Negate(is.null), spur_candidates)
    candidates <- c(candidates, valid)

    # Remove duplicates from candidates
    if (length(candidates) > 0L) {
      path_keys <- vapply(candidates, function(cand) {
        paste(cand$path, collapse = "->")
      }, character(1))
      candidates <- candidates[!duplicated(path_keys)]

      # Also remove any that match already-found paths
      found_keys <- vapply(found_paths, function(fp) {
        paste(fp, collapse = "->")
      }, character(1))
      keep <- !vapply(candidates, function(cand) {
        paste(cand$path, collapse = "->") %in% found_keys
      }, logical(1))
      candidates <- candidates[keep]
    }

    if (length(candidates) == 0L) break

    # Select the shortest candidate
    cand_dists <- vapply(candidates, function(cand) cand$dist, numeric(1))
    best_idx <- which.min(cand_dists)

    found_paths <- c(found_paths, list(candidates[[best_idx]]$path))
    found_distances <- c(found_distances, candidates[[best_idx]]$dist)

    # Remove selected candidate from pool
    candidates <- candidates[-best_idx]

    path_idx <- path_idx + 1L
  }

  result <- list(
    paths = found_paths,
    distances = found_distances,
    from = from_name,
    to = to_name,
    k = k
  )
  class(result) <- "cograph_k_paths"
  result
}


# =============================================================================
# Internal Helpers
# =============================================================================

#' Resolve node identifiers to igraph vertex indices
#' @param g An igraph graph
#' @param nodes Character or numeric node identifiers
#' @return igraph vertex sequence
#' @keywords internal
#' @noRd
.resolve_nodes <- function(g, nodes) {
  node_names <- igraph::V(g)$name
  if (is.character(nodes) && !is.null(node_names)) {
    missing <- setdiff(nodes, node_names)
    if (length(missing) > 0L) {
      stop("Node(s) not found in graph: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    return(match(nodes, node_names))
  }
  if (is.numeric(nodes)) {
    n <- igraph::vcount(g)
    invalid <- nodes[nodes < 1L | nodes > n]
    if (length(invalid) > 0L) {
      stop("Node index(es) out of range: ",
           paste(invalid, collapse = ", "), call. = FALSE)
    }
    return(as.integer(nodes))
  }
  stop("'nodes' must be character names or numeric indices", call. = FALSE)
}


#' Resolve weight specification for igraph path functions
#' @param g An igraph graph
#' @param weights NULL, NA, or numeric vector
#' @return Appropriate weights argument for igraph functions
#' @keywords internal
#' @noRd
.resolve_path_weights <- function(g, weights) {
  if (is.null(weights)) {
    # Auto-detect: use edge weight attribute if present
    if ("weight" %in% igraph::edge_attr_names(g)) {
      return(igraph::E(g)$weight)
    }
    return(NULL)
  }
  if (identical(weights, NA)) {
    return(NA)
  }
  stopifnot(is.numeric(weights))
  weights
}


#' Compute total distance along a path given vertex indices
#' @param g An igraph graph
#' @param path_vs Integer vector of vertex indices forming the path
#' @param edge_weights Weight vector or NULL/NA
#' @return Numeric total path distance
#' @keywords internal
#' @noRd
.path_distance <- function(g, path_vs, edge_weights) {
  if (length(path_vs) < 2L) return(0)

  # Get edge IDs along the path
  n_edges <- length(path_vs) - 1L
  is_dir <- igraph::is_directed(g)

  eids <- vapply(seq_len(n_edges), function(i) {
    as.integer(.get_edge_id(g, path_vs[i], path_vs[i + 1L], is_dir))
  }, integer(1))

  if (any(eids == 0L)) return(Inf)

  if (is.null(edge_weights) || identical(edge_weights, NA)) {
    return(as.numeric(n_edges))
  }

  sum(edge_weights[eids])
}


#' Get edge ID between two vertices
#' @param g An igraph graph
#' @param from_v Integer vertex index
#' @param to_v Integer vertex index
#' @param directed Logical
#' @return Integer edge ID, or 0L if no edge exists
#' @keywords internal
#' @noRd
.get_edge_id <- function(g, from_v, to_v, directed) {
  eid <- igraph::get_edge_ids(g, c(from_v, to_v), directed = directed)
  if (length(eid) == 0L) return(0L)
  eid[1]
}


# =============================================================================
# Print Method
# =============================================================================

#' @method print cograph_k_paths
#' @export
#' @noRd
print.cograph_k_paths <- function(x, ...) {
  cat("K Shortest Paths\n")
  cat("================\n")
  cat("  From:", x$from, "\n")
  cat("  To:", x$to, "\n")
  cat("  Requested:", x$k, "paths\n")
  cat("  Found:", length(x$paths), "paths\n")

  if (length(x$paths) > 0L) {
    cat("\n")
    vapply(seq_along(x$paths), function(i) {
      cat(sprintf("  Path %d (distance = %.4g): %s\n",
                  i, x$distances[i],
                  paste(x$paths[[i]], collapse = " -> ")))
      TRUE
    }, logical(1))
  }

  invisible(x)
}
