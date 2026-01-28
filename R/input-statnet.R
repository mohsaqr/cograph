#' @title Statnet Network Input Parsing
#' @description Functions for parsing statnet network objects.
#' @name input-statnet
NULL

#' Parse Statnet Network Object
#'
#' Convert a statnet network object to internal network format.
#'
#' @param net A network object from the statnet/network package.
#' @param directed Logical. Force directed interpretation. NULL uses network's setting.
#' @return List with nodes, edges, directed, and weights components.
#' @noRd
parse_statnet <- function(net, directed = NULL) {
  # Check if network package is available
  if (!requireNamespace("network", quietly = TRUE)) {
    stop("Package 'network' is required for statnet network input. ",
         "Please install it with: install.packages('network')",
         call. = FALSE)
  }

  # Validate input
  if (!inherits(net, "network")) {
    stop("Input must be a network object", call. = FALSE)
  }

  # Get directedness
  if (is.null(directed)) {
    directed <- network::is.directed(net)
  }

  # Get number of nodes
  n <- network::network.size(net)

  # Get node labels
  labels <- network::network.vertex.names(net)
  if (is.null(labels) || all(is.na(labels))) {
    labels <- as.character(seq_len(n))
  }

  # Get edges as matrix
  edge_matrix <- network::as.edgelist(net)

  if (is.null(edge_matrix) || nrow(edge_matrix) == 0) {
    # Empty network
    from_idx <- integer(0)
    to_idx <- integer(0)
    weight_vals <- numeric(0)
  } else {
    from_idx <- edge_matrix[, 1]
    to_idx <- edge_matrix[, 2]

    # Get edge weights
    edge_attrs <- network::list.edge.attributes(net)
    if ("weight" %in% edge_attrs) {
      weight_vals <- network::get.edge.value(net, "weight")
    } else {
      weight_vals <- rep(1, nrow(edge_matrix))
    }
  }

  # Create data structures
  nodes <- create_nodes_df(n, labels)
  edges <- create_edges_df(from_idx, to_idx, weight_vals, directed)

  # Add additional vertex attributes
  v_attrs <- network::list.vertex.attributes(net)
  for (attr in v_attrs) {
    if (!attr %in% c("vertex.names", "na")) {
      nodes[[attr]] <- network::get.vertex.attribute(net, attr)
    }
  }

  # Add additional edge attributes
  if (nrow(edges) > 0) {
    e_attrs <- network::list.edge.attributes(net)
    for (attr in e_attrs) {
      if (!attr %in% c("weight", "na")) {
        edges[[attr]] <- network::get.edge.value(net, attr)
      }
    }
  }

  list(
    nodes = nodes,
    edges = edges,
    directed = directed,
    weights = weight_vals
  )
}
