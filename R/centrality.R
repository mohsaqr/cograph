#' Calculate Network Centrality Measures
#'
#' Computes centrality measures for nodes in a network and returns a tidy
#' data frame. Accepts matrices, igraph objects, cograph_network, or tna objects.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object)
#' @param measures Which measures to calculate. Default "all" calculates all
#'   available measures. Can be a character vector of measure names:
#'   "degree", "strength", "betweenness", "closeness", "eigenvector",
#'   "pagerank", "authority", "hub", "eccentricity", "coreness",
#'   "constraint", "transitivity", "harmonic", "diffusion", "leverage",
#'   "kreach", "resilience", "alpha", "power", "subgraph", "laplacian",
#'   "load", "current_flow_closeness", "current_flow_betweenness", "voterank".
#' @param mode For directed networks: "all", "in", or "out". Affects degree,
#'   strength, closeness, eccentricity, coreness, and harmonic centrality.
#' @param normalized Logical. Normalize values to 0-1 range by dividing by max.
#'   For closeness, this is passed directly to igraph (proper normalization).
#' @param weighted Logical. Use edge weights if available. Default TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param loops Logical. If TRUE (default), keep self-loops. Set to FALSE to
#'   remove them before calculation.
#' @param simplify How to combine multiple edges between the same node pair.
#'   Options: "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
#'   multiple edges.
#' @param digits Integer or NULL. Round all numeric columns to this many
#'   decimal places. Default NULL (no rounding).
#' @param sort_by Character or NULL. Column name to sort results by
#'   (descending order). Default NULL (original node order).
#' @param cutoff Maximum path length to consider for betweenness and closeness.
#'   Default -1 (no limit). Set to a positive value for faster computation
#'   on large networks at the cost of accuracy.
#' @param invert_weights Logical or NULL. For path-based measures (betweenness,
#'   closeness, harmonic, eccentricity, kreach), should weights be inverted so
#'   that higher weights mean shorter paths? Default NULL which auto-detects:
#'   TRUE for tna objects (transition probabilities), FALSE otherwise (matching
#'   igraph/sna). Set explicitly to TRUE for strength/frequency weights (qgraph
#'   style) or FALSE for distance/cost weights.
#' @param alpha Numeric. Exponent for weight transformation when \code{invert_weights = TRUE}.
#'   Distance is computed as \code{1 / weight^alpha}. Default 1. Higher values
#'   increase the influence of weight differences on path lengths.
#' @param damping PageRank damping factor. Default 0.85. Must be between 0 and 1.
#' @param personalized Named numeric vector for personalized PageRank.
#'   Default NULL (standard PageRank). Values should sum to 1.
#' @param transitivity_type Type of transitivity to calculate: "local" (default),
#'   "global", "undirected", "localundirected", "barrat" (weighted), or "weighted".
#' @param isolates How to handle isolate nodes in transitivity calculation:
#'   "nan" (default) returns NaN, "zero" returns 0.
#' @param lambda Diffusion scaling factor for diffusion centrality. Default 1.
#' @param k Path length parameter for geodesic k-path centrality. Default 3.
#' @param ... Additional arguments (currently unused)
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{node}: Node labels/names
#'     \item One column per measure, with mode suffix for directional measures
#'       (e.g., \code{degree_in}, \code{closeness_all})
#'   }
#'
#' @details
#' The following centrality measures are available:
#' \describe{
#'   \item{degree}{Count of edges (supports mode: in/out/all)}
#'   \item{strength}{Weighted degree (supports mode: in/out/all)}
#'   \item{betweenness}{Shortest path centrality}
#'   \item{closeness}{Inverse distance centrality (supports mode: in/out/all)}
#'   \item{eigenvector}{Influence-based centrality}
#'   \item{pagerank}{Random walk centrality (supports damping and personalization)}
#'   \item{authority}{HITS authority score}
#'   \item{hub}{HITS hub score}
#'   \item{eccentricity}{Maximum distance to other nodes (supports mode)}
#'   \item{coreness}{K-core membership (supports mode: in/out/all)}
#'   \item{constraint}{Burt's constraint (structural holes)}
#'   \item{transitivity}{Local clustering coefficient (supports multiple types)}
#'   \item{harmonic}{Harmonic centrality - handles disconnected graphs better
#'     than closeness (supports mode: in/out/all)}
#'   \item{diffusion}{Diffusion degree centrality - sum of scaled degrees of
#'     node and its neighbors (supports mode: in/out/all, lambda scaling)}
#'   \item{leverage}{Leverage centrality - measures influence over neighbors
#'     based on relative degree differences (supports mode: in/out/all)}
#'   \item{kreach}{Geodesic k-path centrality - count of nodes reachable
#'     within distance k (supports mode: in/out/all, k parameter)}
#'   \item{resilience}{Resilience centrality - minimum number of nodes that
#'     must be removed to disconnect this node from the network}
#'   \item{alpha}{Alpha/Katz centrality - influence via paths, penalized by
#'     distance. Similar to eigenvector but includes exogenous contribution}
#'   \item{power}{Bonacich power centrality - measures influence based on
#'     connections to other influential nodes}
#'   \item{subgraph}{Subgraph centrality - participation in closed loops/walks,
#'     weighting shorter loops more heavily}
#'   \item{laplacian}{Laplacian centrality - drop in Laplacian energy when
#'     node is removed. Higher = more important}
#'   \item{load}{Load centrality - fraction of all shortest paths through node,
#'     similar to betweenness but weights paths by 1/count}
#'   \item{current_flow_closeness}{Information centrality - closeness based on
#'     electrical current flow (requires connected graph)}
#'   \item{current_flow_betweenness}{Random walk betweenness - betweenness based
#'     on current flow rather than shortest paths (requires connected graph)}
#'   \item{voterank}{VoteRank - identifies influential spreaders via iterative
#'     voting mechanism. Returns normalized rank (1 = most influential)}
#' }
#'
#' @export
#' @examples
#' # Basic usage with matrix
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality(adj)
#'
#' # Specific measures
#' centrality(adj, measures = c("degree", "betweenness"))
#'
#' # Directed network with normalization
#' centrality(adj, mode = "in", normalized = TRUE)
#'
#' # Sort by pagerank
#' centrality(adj, sort_by = "pagerank", digits = 3)
#'
#' # PageRank with custom damping
#' centrality(adj, measures = "pagerank", damping = 0.9)
#'
#' # Harmonic centrality (better for disconnected graphs)
#' centrality(adj, measures = "harmonic")
#'
#' # Global transitivity
#' centrality(adj, measures = "transitivity", transitivity_type = "global")
centrality <- function(x, measures = "all", mode = "all",
                       normalized = FALSE, weighted = TRUE,
                       directed = NULL, loops = TRUE, simplify = "sum",
                       digits = NULL, sort_by = NULL,
                       cutoff = -1, invert_weights = NULL, alpha = 1,
                       damping = 0.85, personalized = NULL,
                       transitivity_type = "local", isolates = "nan",
                       lambda = 1, k = 3, ...) {

  # Auto-detect invert_weights based on input type

  # tna objects have transition probabilities (strengths), so invert for path-based measures
  is_tna_input <- inherits(x, c("tna", "group_tna", "ctna", "ftna", "atna",
                                 "group_ctna", "group_ftna", "group_atna"))
  if (is.null(invert_weights)) {
    invert_weights <- is_tna_input
  }

  # Validate mode
  mode <- match.arg(mode, c("all", "in", "out"))

  # Validate new parameters
  transitivity_type <- match.arg(
    transitivity_type,
    c("local", "global", "undirected", "localundirected", "barrat", "weighted")
  )
  isolates <- match.arg(isolates, c("nan", "zero"))

  if (damping < 0 || damping > 1) {
    stop("damping must be between 0 and 1", call. = FALSE)
  }

  # Convert input to igraph (pass directed for override)
  g <- to_igraph(x, directed = directed)

  # Handle loops (remove if loops = FALSE)
  if (!loops && igraph::any_loop(g)) {
    g <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
  }

  # Handle multiple edges (only call simplify if there are actual multiples)
  if (!isFALSE(simplify) && !identical(simplify, "none") && igraph::any_multiple(g)) {
    simplify <- match.arg(simplify, c("sum", "mean", "max", "min"))
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE,
                          edge.attr.comb = list(weight = simplify, "ignore"))
  }

  # Define which measures support mode parameter
  mode_measures <- c("degree", "strength", "closeness", "eccentricity",
                     "coreness", "harmonic", "diffusion", "leverage", "kreach",
                     "alpha", "power")
  no_mode_measures <- c("betweenness", "eigenvector", "pagerank",
                        "authority", "hub", "constraint", "transitivity",
                        "resilience", "subgraph", "laplacian", "load",
                        "current_flow_closeness", "current_flow_betweenness",
                        "voterank")
  all_measures <- c(mode_measures, no_mode_measures)

  # Resolve measures
  if (identical(measures, "all")) {
    measures <- all_measures
  } else {
    invalid <- setdiff(measures, all_measures)
    if (length(invalid) > 0) {
      stop("Unknown measures: ", paste(invalid, collapse = ", "),
           "\nAvailable: ", paste(all_measures, collapse = ", "), call. = FALSE)
    }
  }

  # Get node labels
  labels <- if (!is.null(igraph::V(g)$name)) {
    igraph::V(g)$name
  } else {
    as.character(seq_len(igraph::vcount(g)))
  }

  # Calculate each measure
  results <- list(node = labels)
  weights <- if (weighted && !is.null(igraph::E(g)$weight)) {
    igraph::E(g)$weight
  } else {
    NULL
  }

  # Path-based measures need inverted weights (higher weight = shorter path)
  # Following qgraph's approach: distance = 1 / weight^alpha
  path_based_measures <- c("betweenness", "closeness", "harmonic",
                           "eccentricity", "kreach", "load")
  needs_path_weights <- any(measures %in% path_based_measures)

  weights_for_paths <- weights
  if (!is.null(weights) && invert_weights && needs_path_weights) {
    # Invert weights: distance = 1 / weight^alpha (qgraph/tna style)
    weights_for_paths <- 1 / (weights ^ alpha)
    # Handle zeros/infinities
    weights_for_paths[!is.finite(weights_for_paths)] <- .Machine$double.xmax
    reason <- if (is_tna_input) "tna object detected" else "invert_weights=TRUE"
    message("Note: Weights inverted (1/w^", alpha, ") for path-based measures (",
            reason, "). Higher weights = shorter paths.")
  }

  # Pre-calculate HITS scores if needed (avoid computing twice)
  hits_result <- NULL
  if (any(c("authority", "hub") %in% measures)) {
    hits_result <- igraph::hits_scores(g, weights = weights)
  }

  for (m in measures) {
    # Use inverted weights for path-based measures, original for others
    measure_weights <- if (m %in% path_based_measures) weights_for_paths else weights

    # Calculate value
    value <- calculate_measure(
      g, m, mode, measure_weights, normalized,
      cutoff = cutoff, damping = damping, personalized = personalized,
      transitivity_type = transitivity_type, isolates = isolates,
      hits_result = hits_result, lambda = lambda, k = k
    )

    # Normalize if requested (except for closeness which is handled by igraph)
    if (normalized && m != "closeness") {
      max_val <- max(value, na.rm = TRUE)
      if (!is.na(max_val) && max_val > 0) {
        value <- value / max_val
      }
    }

    # Column name with mode suffix for directional measures
    col_name <- if (m %in% mode_measures) paste0(m, "_", mode) else m
    results[[col_name]] <- value
  }

  df <- as.data.frame(results, stringsAsFactors = FALSE)

  # Round if digits specified
  if (!is.null(digits)) {
    num_cols <- sapply(df, is.numeric)
    df[num_cols] <- lapply(df[num_cols], round, digits = digits)
  }

  # Sort if sort_by specified
  if (!is.null(sort_by)) {
    if (!sort_by %in% names(df)) {
      stop("sort_by column '", sort_by, "' not found in results", call. = FALSE)
    }
    df <- df[order(df[[sort_by]], decreasing = TRUE), ]
    rownames(df) <- NULL
  }

  df
}

#' Calculate diffusion centrality (vectorized)
#'
#' Fast vectorized implementation of diffusion degree centrality.
#' For each node, sums the scaled degrees of itself and its neighbors.
#'
#' @param g igraph object
#' @param mode "all", "in", or "out" for directed graphs
#' @param lambda Scaling factor applied to degrees. Default 1.
#' @return Numeric vector of diffusion centrality values
#' @noRd
calculate_diffusion <- function(g, mode = "all", lambda = 1) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

 # Get scaled degrees
  d <- igraph::degree(g, mode = mode) * lambda

  # Get adjacency matrix (sparse for efficiency)
  adj <- igraph::as_adjacency_matrix(g, sparse = TRUE)

  # Calculate neighbor sum based on mode
  # neighborhood() with order=1 includes the node itself plus neighbors
  # For mode="out": neighbors are nodes this node points TO
  # For mode="in": neighbors are nodes that point TO this node
  # For mode="all": all neighbors (both directions)

  if (igraph::is_directed(g)) {
    if (mode == "out") {
      # Out-neighbors: nodes I point to (row i, columns with 1s)
      neighbor_sum <- as.numeric(adj %*% d)
    } else if (mode == "in") {
      # In-neighbors: nodes that point to me (column i, rows with 1s)
      neighbor_sum <- as.numeric(Matrix::t(adj) %*% d)
    } else {
      # All neighbors: combine both directions
      # Use logical OR to avoid double-counting mutual edges
      adj_undirected <- adj | Matrix::t(adj)
      neighbor_sum <- as.numeric(adj_undirected %*% d)
    }
  } else {
    # Undirected: adjacency matrix is symmetric
    neighbor_sum <- as.numeric(adj %*% d)
  }

  # Result is own degree + sum of neighbor degrees
  d + neighbor_sum
}

#' Calculate leverage centrality (vectorized)
#'
#' Fast vectorized implementation of leverage centrality.
#' Measures how much a node influences its neighbors based on relative degrees.
#' Formula: l_i = (1/k_i) * sum_j((k_i - k_j) / (k_i + k_j)) for neighbors j
#'
#' @param g igraph object
#' @param mode "all", "in", or "out" for directed graphs
#' @param loops Logical; whether to count loop edges
#' @return Numeric vector of leverage centrality values
#' @noRd
calculate_leverage <- function(g, mode = "all", loops = TRUE) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  # Get degrees
  k <- igraph::degree(g, mode = mode, loops = loops)

  # Get adjacency matrix
  adj <- igraph::as_adjacency_matrix(g, sparse = TRUE)

  # For directed graphs with specific mode, use appropriate adjacency
  if (igraph::is_directed(g)) {
    if (mode == "in") {
      adj <- Matrix::t(adj)
    } else if (mode == "all") {
      adj <- adj | Matrix::t(adj)
    }
  }

  # Vectorized calculation
  # For each node i, we need: mean over neighbors j of (k_i - k_j)/(k_i + k_j)
  # Using matrix operations:
  # - k_i - k_j for all pairs: outer subtraction
  # - k_i + k_j for all pairs: outer addition
  # - Select only neighbors using adjacency matrix

  result <- numeric(n)

  for (i in seq_len(n)) {
    if (k[i] == 0) {
      result[i] <- NaN
      next
    }

    # Get neighbor indices
    neighbors_i <- which(adj[i, ] != 0)

    if (length(neighbors_i) == 0) {
      result[i] <- NaN
      next
    }

    k_neighbors <- k[neighbors_i]

    # Calculate leverage: mean of (k_i - k_j) / (k_i + k_j)
    numerator <- k[i] - k_neighbors
    denominator <- k[i] + k_neighbors

    # Handle division by zero (when k_i = k_j = 0)
    ratios <- ifelse(denominator == 0, 0, numerator / denominator)
    result[i] <- mean(ratios)
  }

  result
}

#' Calculate geodesic k-path centrality (vectorized)
#'
#' Fast vectorized implementation of geodesic k-path centrality.
#' Counts neighbors that are on a geodesic path less than or equal to k away.
#'
#' @param g igraph object
#' @param mode "all", "in", or "out" for directed graphs
#' @param weights Edge weights (NULL for unweighted)
#' @param k Maximum path length. Default 3.
#' @return Numeric vector of kreach centrality values
#' @noRd
calculate_kreach <- function(g, mode = "all", weights = NULL, k = 3) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  if (k <= 0) {
    stop("The k parameter must be greater than 0.", call. = FALSE)
  }

  # Get shortest path matrix
  sp <- igraph::distances(g, mode = mode, weights = weights)

  # Count nodes within distance k (excluding self)
  # rowSums counts how many entries are <= k, subtract 1 for self
  as.integer(rowSums(sp <= k, na.rm = TRUE) - 1)
}

#' Calculate resilience centrality
#'
#' Measures how many nodes must be removed before a node becomes disconnected
#' from the rest of the network. This is based on vertex-disjoint paths.
#' Higher values = more resilient/robust nodes (harder to isolate).
#'
#' For each node, this calculates the minimum number of OTHER nodes that need
#' to be removed to disconnect it from at least one other node it was
#' previously connected to.
#'
#' @param g igraph object
#' @return Numeric vector of resilience centrality values
#' @noRd
calculate_resilience <- function(g) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (n == 1) return(0L)

  # For each node, find its minimum vertex connectivity to any neighbor
  # This represents how many nodes must be removed to cut at least one connection
  result <- integer(n)

  for (v in seq_len(n)) {
    # Get neighbors of v
    neighbors_v <- as.integer(igraph::neighbors(g, v, mode = "all"))

    if (length(neighbors_v) == 0) {
      # Already isolated
      result[v] <- 0L
      next
    }

    # For each neighbor, count vertex-disjoint paths (excluding direct edge)
    # The resilience is the minimum across all neighbors
    min_resilience <- Inf

    for (u in neighbors_v) {
      # Count vertex-disjoint paths from v to u
      # This equals the vertex connectivity between them
      # vertex_connectivity counts internal vertices on disjoint paths

      # For adjacent nodes, we want paths that don't use the direct edge
      # Remove the edge temporarily and check connectivity
      edge_id <- igraph::get.edge.ids(g, c(v, u))

      if (length(edge_id) > 0 && edge_id[1] > 0) {
        # Remove direct edge and count alternative paths
        g_temp <- igraph::delete_edges(g, edge_id[1])
        conn <- tryCatch({
          # Count vertex-disjoint paths in graph without direct edge
          igraph::vertex_connectivity(g_temp, source = v, target = u)
        }, error = function(e) {
          0L
        })
      } else {
        conn <- 0L
      }

      min_resilience <- min(min_resilience, conn)
    }

    # Add 1 because the direct edge also counts as a path
    # Actually, resilience = number of vertex-disjoint paths - 1
    # (removing that many internal nodes breaks all but one path)
    result[v] <- as.integer(min_resilience)
  }

  result
}

#' Calculate Laplacian centrality
#'
#' Measures the drop in Laplacian energy when a node is removed.
#' Higher values indicate more important nodes.
#'
#' @param g igraph object
#' @param weights Edge weights (NULL for unweighted)
#' @param normalized Whether to normalize by max value
#' @return Numeric vector of Laplacian centrality values
#' @noRd
calculate_laplacian <- function(g, weights = NULL, normalized = FALSE) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (n == 1) return(0)

  # Get Laplacian matrix
  L <- igraph::laplacian_matrix(g, weights = weights, sparse = FALSE)

  # Calculate Laplacian energy of full graph (sum of squared eigenvalues)
  eig_full <- eigen(L, symmetric = TRUE, only.values = TRUE)$values
  energy_full <- sum(eig_full^2)

  # For each node, calculate energy drop when removed
  result <- numeric(n)
  for (v in seq_len(n)) {
    # Remove node v (delete row and column)
    L_reduced <- L[-v, -v, drop = FALSE]
    if (nrow(L_reduced) == 0) {
      result[v] <- energy_full
    } else {
      eig_reduced <- eigen(L_reduced, symmetric = TRUE, only.values = TRUE)$values
      energy_reduced <- sum(eig_reduced^2)
      result[v] <- energy_full - energy_reduced
    }
  }

  if (normalized && max(result) > 0) {
    result <- result / max(result)
  }

  result
}

#' Calculate load centrality
#'
#' Similar to betweenness but counts fraction of all shortest paths through node.
#' Based on Newman's variant where paths are weighted by 1/number_of_paths.
#'
#' @param g igraph object
#' @param weights Edge weights (NULL for unweighted)
#' @param directed Whether to consider edge direction
#' @return Numeric vector of load centrality values
#' @noRd
calculate_load <- function(g, weights = NULL, directed = TRUE) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (n == 1) return(0)

  # Get all shortest paths
  sp_lengths <- igraph::distances(g, mode = if (directed) "out" else "all",
                                   weights = weights)

  load <- numeric(n)

  for (s in seq_len(n)) {
    for (t in seq_len(n)) {
      if (s == t) next
      if (is.infinite(sp_lengths[s, t])) next

      # Get all shortest paths from s to t
      paths <- igraph::all_shortest_paths(g, from = s, to = t,
                                           mode = if (directed) "out" else "all",
                                           weights = weights)$res

      if (length(paths) == 0) next

      n_paths <- length(paths)

      # Count how many paths go through each intermediate node
      for (path in paths) {
        path_nodes <- as.integer(path)
        # Exclude source and target
        intermediate <- path_nodes[-c(1, length(path_nodes))]
        for (v in intermediate) {
          load[v] <- load[v] + 1 / n_paths
        }
      }
    }
  }

  load
}

#' Calculate current-flow closeness centrality (information centrality)
#'
#' Based on electrical current flow through the network.
#' Uses the pseudoinverse of the Laplacian matrix.
#'
#' @param g igraph object
#' @param weights Edge weights (NULL for unweighted)
#' @return Numeric vector of current-flow closeness values
#' @noRd
calculate_current_flow_closeness <- function(g, weights = NULL) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (n <= 1) return(rep(NA_real_, n))

  # Must be connected for current flow
  if (!igraph::is_connected(g, mode = "weak")) {
    warning("Graph is not connected; current-flow closeness undefined for disconnected nodes")
    return(rep(NA_real_, n))
  }

  # Get Laplacian matrix
  L <- igraph::laplacian_matrix(g, weights = weights, sparse = FALSE)

  # Compute Moore-Penrose pseudoinverse
  # L+ = (L - J/n)^-1 + J/n where J is all-ones matrix
  J <- matrix(1, n, n)
  L_tilde <- L - J / n

  # Use SVD for pseudoinverse (more stable)
  svd_result <- svd(L_tilde)
  tol <- max(dim(L_tilde)) * max(svd_result$d) * .Machine$double.eps
  positive <- svd_result$d > tol
  if (sum(positive) == 0) {
    return(rep(NA_real_, n))
  }

  L_pinv <- svd_result$v[, positive, drop = FALSE] %*%
    diag(1 / svd_result$d[positive], nrow = sum(positive)) %*%
    t(svd_result$u[, positive, drop = FALSE])

  # Current-flow closeness for node i is n / sum of effective resistances
  # Effective resistance R_ij = L+_ii + L+_jj - 2*L+_ij
  diag_L_pinv <- diag(L_pinv)

  result <- numeric(n)
  for (i in seq_len(n)) {
    total_resistance <- 0
    for (j in seq_len(n)) {
      if (i != j) {
        R_ij <- diag_L_pinv[i] + diag_L_pinv[j] - 2 * L_pinv[i, j]
        total_resistance <- total_resistance + R_ij
      }
    }
    result[i] <- (n - 1) / total_resistance
  }

  result
}

#' Calculate current-flow betweenness centrality
#'
#' Betweenness based on current flow rather than shortest paths.
#' Measures the amount of current passing through each node.
#'
#' @param g igraph object
#' @param weights Edge weights (NULL for unweighted, treated as conductances)
#' @return Numeric vector of current-flow betweenness values
#' @noRd
calculate_current_flow_betweenness <- function(g, weights = NULL) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (n <= 2) return(rep(0, n))

  # Must be connected
  if (!igraph::is_connected(g, mode = "weak")) {
    warning("Graph is not connected; current-flow betweenness undefined")
    return(rep(NA_real_, n))
  }

  # Get Laplacian (with weights as conductances)
  L <- igraph::laplacian_matrix(g, weights = weights, sparse = FALSE)

  # Pseudoinverse of Laplacian
  J <- matrix(1, n, n)
  L_tilde <- L - J / n

  svd_result <- svd(L_tilde)
  tol <- max(dim(L_tilde)) * max(svd_result$d) * .Machine$double.eps
  positive <- svd_result$d > tol

  if (sum(positive) == 0) {
    return(rep(NA_real_, n))
  }

  L_pinv <- svd_result$v[, positive, drop = FALSE] %*%
    diag(1 / svd_result$d[positive], nrow = sum(positive)) %*%
    t(svd_result$u[, positive, drop = FALSE])

  # Calculate throughput for each node
  betweenness <- numeric(n)

  for (s in seq_len(n)) {
    for (t in seq_len(n)) {
      if (s >= t) next  # Only consider each pair once

      # Current at node k when unit current flows from s to t
      # I_k = |L+_ks - L+_kt| for k != s, t
      for (k in seq_len(n)) {
        if (k == s || k == t) next
        current_through_k <- abs(L_pinv[k, s] - L_pinv[k, t])
        betweenness[k] <- betweenness[k] + current_through_k
      }
    }
  }

  # Normalize by number of pairs
  betweenness <- betweenness * 2 / ((n - 1) * (n - 2))

  betweenness
}

#' Calculate VoteRank centrality
#'
#' Iteratively finds influential spreaders by voting mechanism.
#' Each iteration selects the node with most votes, then reduces voting
#' power of its neighbors.
#'
#' @param g igraph object
#' @param directed Whether to consider edge direction
#' @return Numeric vector with rank order (1 = most influential, higher = less)
#' @noRd
calculate_voterank <- function(g, directed = TRUE)
{
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (n == 1) return(1)


  # Initialize voting ability for all nodes
  avg_degree <- mean(igraph::degree(g, mode = "all"))
  if (avg_degree == 0) avg_degree <- 1

  voting_ability <- rep(1, n)
  selected <- logical(n)
  rank_order <- rep(NA_integer_, n)
  rank <- 1

  for (iter in seq_len(n)) {
    # Calculate votes for each unselected node
    votes <- numeric(n)

    for (v in which(!selected)) {
      # Get in-neighbors (nodes that vote for v)
      if (directed) {
        voters <- as.integer(igraph::neighbors(g, v, mode = "in"))
      } else {
        voters <- as.integer(igraph::neighbors(g, v, mode = "all"))
      }

      # Sum voting ability of neighbors that haven't been selected
      votes[v] <- sum(voting_ability[voters[!selected[voters]]])
    }

    # Select node with maximum votes
    candidates <- which(!selected)
    if (length(candidates) == 0) break

    votes_candidates <- votes[candidates]
    if (all(votes_candidates == 0)) {
      # No more votes, assign remaining ranks arbitrarily
      remaining <- which(!selected)
      rank_order[remaining] <- seq(rank, length.out = length(remaining))
      break
    }

    # Winner is candidate with max votes
    winner <- candidates[which.max(votes_candidates)]
    selected[winner] <- TRUE
    rank_order[winner] <- rank
    rank <- rank + 1

    # Reduce voting ability of winner's neighbors
    if (directed) {
      neighbors_of_winner <- as.integer(igraph::neighbors(g, winner, mode = "out"))
    } else {
      neighbors_of_winner <- as.integer(igraph::neighbors(g, winner, mode = "all"))
    }

    for (nb in neighbors_of_winner) {
      voting_ability[nb] <- max(0, voting_ability[nb] - 1 / avg_degree)
    }
  }

  # Convert rank to centrality (lower rank = higher centrality)
  # Return inverse rank so higher values = more central
  max_rank <- max(rank_order, na.rm = TRUE)
  (max_rank + 1 - rank_order) / max_rank
}

#' Calculate a single centrality measure
#' @noRd
calculate_measure <- function(g, measure, mode, weights, normalized,
                              cutoff, damping, personalized,
                              transitivity_type, isolates,
                              hits_result = NULL, lambda = 1, k = 3) {
  directed <- igraph::is_directed(g)

  value <- switch(measure,
    # Measures that support mode
    "degree" = igraph::degree(g, mode = mode),
    "strength" = igraph::strength(g, mode = mode, weights = weights),
    "closeness" = igraph::closeness(
      g, mode = mode, weights = weights, normalized = normalized, cutoff = cutoff
    ),
    "eccentricity" = igraph::eccentricity(g, mode = mode),
    "coreness" = igraph::coreness(g, mode = mode),
    "harmonic" = igraph::harmonic_centrality(
      g, mode = mode, weights = weights, normalized = normalized, cutoff = cutoff
    ),
    "diffusion" = calculate_diffusion(g, mode = mode, lambda = lambda),
    "leverage" = calculate_leverage(g, mode = mode),
    "kreach" = calculate_kreach(g, mode = mode, weights = weights, k = k),
    "alpha" = igraph::alpha_centrality(
      g, weights = weights, exo = 1,
      tol = 1e-07, loops = FALSE, sparse = TRUE
    ),
    "power" = igraph::power_centrality(
      g, exponent = 1, rescale = FALSE, tol = 1e-07, loops = FALSE, sparse = TRUE
    ),

    # Measures without mode
    "resilience" = calculate_resilience(g),
    "subgraph" = igraph::subgraph_centrality(g, diag = FALSE),
    "laplacian" = calculate_laplacian(g, weights = weights, normalized = normalized),
    "load" = calculate_load(g, weights = weights, directed = directed),
    "current_flow_closeness" = calculate_current_flow_closeness(g, weights = weights),
    "current_flow_betweenness" = calculate_current_flow_betweenness(g, weights = weights),
    "voterank" = calculate_voterank(g, directed = directed),
    "betweenness" = igraph::betweenness(
      g, weights = weights, directed = directed, cutoff = cutoff
    ),
    "eigenvector" = igraph::eigen_centrality(
      g, weights = weights, directed = directed
    )$vector,
    "pagerank" = igraph::page_rank(
      g, weights = weights, directed = directed,
      damping = damping, personalized = personalized
    )$vector,
    "authority" = hits_result$authority,
    "hub" = hits_result$hub,
    "constraint" = igraph::constraint(g, weights = weights),
    "transitivity" = igraph::transitivity(
      g, type = transitivity_type, isolates = isolates
    ),

    stop("Unknown measure: ", measure, call. = FALSE)
  )

  # Remove names to ensure consistent output
  unname(value)
}

#' @rdname centrality
#' @export
centrality_degree <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "degree", mode = mode, ...)
  col <- paste0("degree_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_strength <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "strength", mode = mode, ...)
  col <- paste0("strength_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "betweenness", ...)
  stats::setNames(df$betweenness, df$node)
}

#' @rdname centrality
#' @export
centrality_closeness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "closeness", mode = mode, ...)
  col <- paste0("closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_eigenvector <- function(x, ...) {
  df <- centrality(x, measures = "eigenvector", ...)
  stats::setNames(df$eigenvector, df$node)
}

#' @rdname centrality
#' @export
centrality_pagerank <- function(x, damping = 0.85, personalized = NULL, ...) {
  df <- centrality(x, measures = "pagerank",
                   damping = damping, personalized = personalized, ...)
  stats::setNames(df$pagerank, df$node)
}

#' @rdname centrality
#' @export
centrality_authority <- function(x, ...) {
  df <- centrality(x, measures = "authority", ...)
  stats::setNames(df$authority, df$node)
}

#' @rdname centrality
#' @export
centrality_hub <- function(x, ...) {
  df <- centrality(x, measures = "hub", ...)
  stats::setNames(df$hub, df$node)
}

#' @rdname centrality
#' @export
centrality_eccentricity <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "eccentricity", mode = mode, ...)
  col <- paste0("eccentricity_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_coreness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "coreness", mode = mode, ...)
  col <- paste0("coreness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_constraint <- function(x, ...) {
  df <- centrality(x, measures = "constraint", ...)
  stats::setNames(df$constraint, df$node)
}

#' @rdname centrality
#' @export
centrality_transitivity <- function(x, transitivity_type = "local",
                                    isolates = "nan", ...) {
  df <- centrality(x, measures = "transitivity",
                   transitivity_type = transitivity_type, isolates = isolates, ...)
  stats::setNames(df$transitivity, df$node)
}

#' @rdname centrality
#' @export
centrality_harmonic <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "harmonic", mode = mode, ...)
  col <- paste0("harmonic_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_diffusion <- function(x, mode = "all", lambda = 1, ...) {
  df <- centrality(x, measures = "diffusion", mode = mode, lambda = lambda, ...)
  col <- paste0("diffusion_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_leverage <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "leverage", mode = mode, ...)
  col <- paste0("leverage_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_kreach <- function(x, mode = "all", k = 3, ...) {
  df <- centrality(x, measures = "kreach", mode = mode, k = k, ...)
  col <- paste0("kreach_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_resilience <- function(x, ...) {
  df <- centrality(x, measures = "resilience", ...)
  stats::setNames(df$resilience, df$node)
}
