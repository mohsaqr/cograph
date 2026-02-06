#' @title igraph Input Parsing
#' @description Functions for parsing igraph objects.
#' @name input-igraph
NULL

#' Parse igraph Object
#'
#' Convert an igraph object to internal network format.
#'
#' @param g An igraph object.
#' @param directed Logical. Force directed interpretation. NULL uses igraph's setting.
#' @return List with nodes, edges, directed, and weights components.
#' @noRd
parse_igraph <- function(g, directed = NULL) {
  # Check if igraph is available
 if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for igraph input. ",
         "Please install it with: install.packages('igraph')",
         call. = FALSE)
  }

  # Validate input
  if (!inherits(g, "igraph")) {
    stop("Input must be an igraph object", call. = FALSE)
  }

  # Get directedness
  if (is.null(directed)) {
    directed <- igraph::is_directed(g)
  }

  # Get number of nodes
  n <- igraph::vcount(g)

  # Get node labels
  labels <- igraph::V(g)$name
  if (is.null(labels) || all(is.na(labels))) {
    labels <- as.character(seq_len(n))
  }

  # Get edges
  edge_list <- igraph::as_edgelist(g, names = FALSE)
  from_idx <- edge_list[, 1]
  to_idx <- edge_list[, 2]

  # Get edge weights
  if ("weight" %in% igraph::edge_attr_names(g)) {
    weight_vals <- igraph::E(g)$weight
  } else {
    weight_vals <- rep(1, igraph::ecount(g))
  }

  # Create data structures
  nodes <- create_nodes_df(n, labels)
  edges <- create_edges_df(from_idx, to_idx, weight_vals, directed)

  # Add any additional vertex attributes
  v_attrs <- igraph::vertex_attr_names(g)
  for (attr in v_attrs) {
    if (attr != "name") {
      nodes[[attr]] <- igraph::vertex_attr(g, attr)
    }
  }

  # Add any additional edge attributes
  e_attrs <- igraph::edge_attr_names(g)
  for (attr in e_attrs) {
    if (attr != "weight") {
      edges[[attr]] <- igraph::edge_attr(g, attr)
    }
  }

  list(
    nodes = nodes,
    edges = edges,
    directed = directed,
    weights = weight_vals
  )
}

#' Apply igraph Layout Function
#'
#' Apply an igraph layout function to a CographNetwork.
#'
#' @param network A CographNetwork object.
#' @param layout_fn An igraph layout function (e.g., igraph::layout_nicely).
#' @param ... Additional arguments passed to the layout function.
#' @return Data frame with x, y coordinates.
#' @noRd
apply_igraph_layout <- function(network, layout_fn, ...) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for igraph layouts. ",
         "Please install it with: install.packages('igraph')",
         call. = FALSE)
  }

  # Convert network to igraph
  g <- network_to_igraph(network)

  # Apply layout function
  coords <- layout_fn(g, ...)

  # Normalize to 0.1-0.9 range
  coords <- normalize_coords(coords)

  data.frame(x = coords[, 1], y = coords[, 2])
}

#' Apply igraph Layout by Name
#'
#' Apply an igraph layout by its name string.
#'
#' @param network A CographNetwork object.
#' @param layout_name Layout name (e.g., "layout_nicely", "kk", "fr").
#' @param seed Random seed for deterministic layouts. Default 42.
#' @param ... Additional arguments passed to the layout function.
#' @return Data frame with x, y coordinates.
#' @noRd
apply_igraph_layout_by_name <- function(network, layout_name, seed = 42, ...) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for igraph layouts. ",
         "Please install it with: install.packages('igraph')",
         call. = FALSE)
  }

  # Map common names to igraph functions
  layout_map <- list(
    # Two-letter aliases (primary)
    "kk" = igraph::layout_with_kk,
    "fr" = igraph::layout_with_fr,
    "drl" = igraph::layout_with_drl,
    "lgl" = igraph::layout_with_lgl,
    "mds" = igraph::layout_with_mds,
    "go" = igraph::layout_with_graphopt,
    "tr" = igraph::layout_as_tree,
    "st" = igraph::layout_as_star,
    "gr" = igraph::layout_on_grid,
    "rd" = igraph::layout_randomly,
    "sp" = igraph::layout_on_sphere,
    "ni" = igraph::layout_nicely,
    "ci" = igraph::layout_in_circle,
    # Full names
    "layout_nicely" = igraph::layout_nicely,
    "layout_with_fr" = igraph::layout_with_fr,
    "layout_with_kk" = igraph::layout_with_kk,
    "layout_with_drl" = igraph::layout_with_drl,
    "layout_with_lgl" = igraph::layout_with_lgl,
    "layout_with_mds" = igraph::layout_with_mds,
    "layout_with_graphopt" = igraph::layout_with_graphopt,
    "layout_in_circle" = igraph::layout_in_circle,
    "layout_as_star" = igraph::layout_as_star,
    "layout_as_tree" = igraph::layout_as_tree,
    "layout_on_grid" = igraph::layout_on_grid,
    "layout_randomly" = igraph::layout_randomly,
    "layout_on_sphere" = igraph::layout_on_sphere,
    # Short aliases with igraph_ prefix
    "igraph_nicely" = igraph::layout_nicely,
    "igraph_fr" = igraph::layout_with_fr,
    "igraph_kk" = igraph::layout_with_kk,
    "igraph_drl" = igraph::layout_with_drl,
    "igraph_lgl" = igraph::layout_with_lgl,
    "igraph_mds" = igraph::layout_with_mds,
    "igraph_graphopt" = igraph::layout_with_graphopt,
    "igraph_circle" = igraph::layout_in_circle,
    "igraph_star" = igraph::layout_as_star,
    "igraph_tree" = igraph::layout_as_tree,
    "igraph_grid" = igraph::layout_on_grid,
    "igraph_random" = igraph::layout_randomly,
    "igraph_sphere" = igraph::layout_on_sphere
  )

  layout_fn <- layout_map[[layout_name]]
  if (is.null(layout_fn)) {
    available <- c("kk", "fr", "drl", "mds", "go", "tr", "st", "gr", "rd", "ni", "ci")
    stop("Unknown igraph layout: ", layout_name,
         "\nAvailable (2-letter): ", paste(available, collapse = ", "), call. = FALSE)
  }

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  apply_igraph_layout(network, layout_fn, ...)
}

#' Convert CographNetwork to igraph
#'
#' Convert a CographNetwork object to an igraph object for layout computation.
#' Handles both old R6 CographNetwork format and new lightweight cograph_network format.
#'
#' @param network A CographNetwork or cograph_network object.
#' @return An igraph object.
#' @noRd
network_to_igraph <- function(network) {
  # Handle both old R6 format and new lightweight format
  if (inherits(network, "CographNetwork")) {
    # Old R6 format with methods
    edges <- network$get_edges()
    n <- network$n_nodes
    is_dir <- network$is_directed
    nodes <- network$get_nodes()
  } else if (inherits(network, "cograph_network")) {
    # New lightweight format - use getter functions
    edges <- get_edges(network)
    n <- n_nodes(network)
    is_dir <- is_directed(network)
    nodes <- get_nodes(network)
  } else {
    stop("network must be a CographNetwork or cograph_network object", call. = FALSE)
  }

  if (is.null(edges) || nrow(edges) == 0) {
    # Empty graph
    g <- igraph::make_empty_graph(n, directed = is_dir)
  } else {
    # Create edge list
    edge_mat <- as.matrix(edges[, c("from", "to")])
    g <- igraph::graph_from_edgelist(edge_mat, directed = is_dir)

    # Add weights if present
    if (!is.null(edges$weight)) {
      igraph::E(g)$weight <- edges$weight
    }
  }

  # Add node labels
  if (!is.null(nodes$label)) {
    igraph::V(g)$name <- nodes$label
  }

  g
}

#' Normalize Coordinates
#'
#' Normalize layout coordinates to 0.1-0.9 range.
#'
#' @param coords Matrix of x, y coordinates.
#' @return Normalized coordinate matrix.
#' @noRd
normalize_coords <- function(coords) {
  # Handle single node case
  if (nrow(coords) == 1) {
    return(matrix(c(0.5, 0.5), nrow = 1))
  }

  # Normalize each dimension
  for (i in 1:2) {
    rng <- range(coords[, i], na.rm = TRUE)
    if (diff(rng) > 0) {
      coords[, i] <- 0.1 + 0.8 * (coords[, i] - rng[1]) / diff(rng)
    } else {
      coords[, i] <- 0.5
    }
  }

  coords
}
