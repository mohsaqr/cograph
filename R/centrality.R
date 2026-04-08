#' Calculate Network Centrality Measures
#'
#' Computes centrality measures for nodes in a network and returns a tidy
#' data frame. Accepts matrices, igraph objects, cograph_network, or tna objects.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object)
#' @param measures Which measures to calculate. Default "all" calculates all
#'   available measures (87 total). Can be a character vector of measure names.
#'   **Core** (igraph-backed): "degree", "strength", "betweenness", "closeness",
#'   "eigenvector", "pagerank", "authority", "hub", "eccentricity", "coreness",
#'   "constraint", "transitivity", "harmonic", "alpha", "power", "subgraph".
#'   **Native**: "diffusion", "leverage", "kreach", "laplacian", "load",
#'   "current_flow_closeness", "current_flow_betweenness", "voterank",
#'   "percolation".
#'   **Distance-based**: "radiality", "lin", "decay", "residual_closeness",
#'   "dangalchev", "generalized_closeness", "harary", "average_distance",
#'   "barycenter", "wiener", "closeness_vitality".
#'   **Spectral/walk**: "communicability", "communicability_betweenness",
#'   "random_walk".
#'   **Path-based**: "stress", "flow_betweenness".
#'   **Local/neighborhood**: "lobby", "entropy", "semilocal", "clusterrank",
#'   "bottleneck", "centroid", "mnc", "dmnc", "lac", "topological_coefficient",
#'   "bridging", "local_bridging", "effective_size", "diversity",
#'   "cross_clique", "markov".
#'   **Influence**: "integration", "expected", "gilschmidt".
#'   **Directed-only**: "salsa", "leaderrank", "trophic_level", "pairwisedis",
#'   "prestige_domain", "prestige_domain_proximity".
#'   **Community-aware** (require \code{membership}): "participation",
#'   "within_module_z", "gateway", "brokerage_coordinator",
#'   "brokerage_itinerant", "brokerage_representative",
#'   "brokerage_gatekeeper", "brokerage_liaison" (the last 5 also require
#'   a directed graph; see \code{\link{centrality_brokerage_coordinator}}).
#'   **Zoo (batch 2)**: "gravity", "collective_influence", "local_hindex",
#'   "hindex_strength", "onion", "second_order", "infection", "nonbacktracking",
#'   "spanning_tree".
#'   **Classical (batch 3, reference-validated)**: "katz" (Katz 1953),
#'   "hubbell" (Hubbell 1965), "information" (Stephenson-Zelen 1989),
#'   "reaching_local" (Mones et al. 2012). See \code{\link{centrality_katz}},
#'   \code{\link{centrality_hubbell}}, \code{\link{centrality_information}},
#'   \code{\link{centrality_pairwisedis}}, \code{\link{centrality_reaching_local}}.
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
#' @param states Named numeric vector of percolation states (0-1) for percolation
#'   centrality. Each value represents how "activated" or "infected" a node is.
#'   Default NULL (all nodes get state 1, equivalent to betweenness).
#' @param decay_parameter Numeric. Decay parameter for decay and generalized
#'   closeness centrality. Default 0.5. Must be between 0 and 1.
#' @param dmnc_epsilon Numeric. Epsilon exponent for DMNC (Density of Maximum
#'   Neighborhood Component). Default 1.7 as recommended by Lin et al. (2008).
#'   centiserve uses 1.67 (four-community assumption). Must be between 1 and 2.
#' @param membership Integer vector of community assignments (one per node) for
#'   community-aware measures: participation, within_module_z, gateway.
#'   Default NULL. Required when requesting these measures.
#' @param katz_alpha Attenuation factor for Katz centrality. Must satisfy
#'   \eqn{\alpha < 1 / \rho(A)}. Default 0.1 (matches centiserve and NetworkX
#'   conventions). Only used when \code{"katz"} is in \code{measures}.
#' @param hubbell_weight Weight factor \eqn{w} for Hubbell centrality. Must
#'   satisfy \eqn{w \cdot \rho(W) \le 1} for solvability. Default 0.5. Only
#'   used when \code{"hubbell"} is in \code{measures}.
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
#'   \item{alpha}{Alpha/Katz centrality - influence via paths, penalized by
#'     distance. Similar to eigenvector but includes exogenous contribution}
#'   \item{power}{Bonacich power centrality - measures influence based on
#'     connections to other influential nodes}
#'   \item{subgraph}{Subgraph centrality - participation in closed loops/walks,
#'     weighting shorter loops more heavily}
#'   \item{laplacian}{Laplacian centrality using Qi et al. (2012) local formula.
#'     Matches NetworkX and centiserve::laplacian()}
#'   \item{load}{Load centrality - fraction of all shortest paths through node,
#'     similar to betweenness but weights paths by 1/count}
#'   \item{current_flow_closeness}{Information centrality - closeness based on
#'     electrical current flow (requires connected graph)}
#'   \item{current_flow_betweenness}{Random walk betweenness - betweenness based
#'     on current flow rather than shortest paths (requires connected graph)}
#'   \item{voterank}{VoteRank - identifies influential spreaders via iterative
#'     voting mechanism. Returns normalized rank (1 = most influential)}
#'   \item{percolation}{Percolation centrality - importance for spreading processes.
#'     Uses node states (0-1) to weight paths. When all states equal, equivalent
#'     to betweenness. Useful for epidemic/information spreading analysis.}
#'   \item{radiality}{Radiality centrality (centiserve). Sum of (diam + 1 - d)
#'     normalized by n-1.}
#'   \item{lin}{Lin's centrality. Reachable nodes squared divided by sum of
#'     distances.}
#'   \item{decay}{Decay centrality. Sum of delta^d for parameter delta.}
#'   \item{residual_closeness}{Residual closeness. Sum of 1/2^d.}
#'   \item{dangalchev}{Dangalchev closeness (alias for residual closeness).}
#'   \item{generalized_closeness}{Generalized closeness. Sum of alpha^d.}
#'   \item{harary}{Harary centrality. Sum of 1/d^2 for all reachable pairs.}
#'   \item{average_distance}{Average distance (centiserve). Sum of distances /
#'     (n+1).}
#'   \item{barycenter}{Barycenter centrality. 1 / sum of distances.}
#'   \item{wiener}{Wiener index. Total sum of shortest path distances from node.}
#'   \item{closeness_vitality}{Closeness vitality. Drop in Wiener index when
#'     node removed.}
#'   \item{communicability}{Total communicability. Row sums of matrix exponential.}
#'   \item{communicability_betweenness}{Communicability betweenness. Fraction of
#'     communicability through each node.}
#'   \item{random_walk}{Random walk centrality. Inverse sum of random walk
#'     distances (requires connected graph).}
#'   \item{stress}{Stress centrality. Number of shortest paths through node.}
#'   \item{flow_betweenness}{Flow betweenness. Max-flow based betweenness.}
#'   \item{lobby}{Lobby index (h-index of neighborhood).}
#'   \item{entropy}{Graph entropy centrality. Entropy change on node removal.}
#'   \item{semilocal}{Semi-local centrality. Triple-nested neighborhood sum.}
#'   \item{clusterrank}{ClusterRank. Clustering coefficient times neighbor
#'     degree sum.}
#'   \item{bottleneck}{Bottleneck centrality. Count of shortest path trees where
#'     node is critical.}
#'   \item{centroid}{Centroid value. Minimum f(v,i) across all nodes.}
#'   \item{mnc}{Maximum Neighborhood Component size.}
#'   \item{dmnc}{Density of Maximum Neighborhood Component.}
#'   \item{topological_coefficient}{Topological coefficient. Shared neighbor
#'     ratio.}
#'   \item{bridging}{Bridging centrality. Betweenness times bridging
#'     coefficient.}
#'   \item{local_bridging}{Local bridging. (1/degree) times bridging
#'     coefficient.}
#'   \item{effective_size}{Burt's effective size. Degree minus redundancy.}
#'   \item{diversity}{Diversity centrality. Shannon entropy of edge weight
#'     distribution.}
#'   \item{cross_clique}{Cross-clique connectivity. Count of cliques containing
#'     node.}
#'   \item{markov}{Markov centrality. Inverse mean first passage time
#'     (requires connected graph).}
#'   \item{integration}{Integration centrality. Distance-based influence.}
#'   \item{expected}{Expected centrality. Sum of neighbor degrees.}
#'   \item{gilschmidt}{Gil-Schmidt power index. Sum of 1/d normalized by n-1.}
#'   \item{salsa}{SALSA authority scores (directed graphs only).}
#'   \item{leaderrank}{LeaderRank. PageRank with ground node
#'     (directed graphs only).}
#'   \item{participation}{Participation coefficient. Diversity of inter-community
#'     connections (requires \code{membership}).}
#'   \item{within_module_z}{Within-module degree z-score. Intra-community
#'     connectivity (requires \code{membership}).}
#'   \item{gateway}{Gateway coefficient. Inter-community brokerage weighted by
#'     centrality (requires \code{membership}).}
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
                       lambda = 1, k = 3, states = NULL,
                       decay_parameter = 0.5, dmnc_epsilon = 1.7,
                       membership = NULL,
                       katz_alpha = 0.1, hubbell_weight = 0.5,
                       ...) {

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
                     "alpha", "power",
                     # Extended mode measures
                     "radiality", "lin", "decay", "residual_closeness",
                     "dangalchev", "generalized_closeness", "harary",
                     "average_distance", "barycenter", "wiener",
                     "lobby", "entropy", "semilocal", "clusterrank",
                     "bottleneck", "centroid", "mnc", "dmnc", "lac",
                     "closeness_vitality",
                     "integration", "expected", "gilschmidt",
                     # Community-aware mode measures
                     "participation", "within_module_z", "gateway",
                     # Zoo batch 2 — mode measures
                     "gravity", "collective_influence", "local_hindex",
                     "hindex_strength", "onion",
                     # Batch 3 — mode measures
                     "reaching_local")
  no_mode_measures <- c("betweenness", "eigenvector", "pagerank",
                        "authority", "hub", "constraint", "transitivity",
                        "subgraph", "laplacian", "load",
                        "current_flow_closeness", "current_flow_betweenness",
                        "voterank", "percolation",
                        # Extended no-mode measures
                        "stress", "flow_betweenness",
                        "communicability", "communicability_betweenness",
                        "random_walk",
                        "topological_coefficient", "bridging",
                        "local_bridging", "effective_size",
                        "diversity", "cross_clique", "markov",
                        # Directed-only measures
                        "salsa", "leaderrank", "trophic_level",
                        # Zoo batch 2 — no-mode measures
                        "second_order", "infection", "nonbacktracking",
                        "spanning_tree",
                        # Batch 3 — classical measures with reference validation
                        "katz", "hubbell", "information", "pairwisedis",
                        # Batch 4 — directed prestige family (Wasserman-Faust / sna)
                        "prestige_domain", "prestige_domain_proximity",
                        # Batch 5 — Gould-Fernandez brokerage (5 roles)
                        "brokerage_coordinator", "brokerage_itinerant",
                        "brokerage_representative", "brokerage_gatekeeper",
                        "brokerage_liaison")
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
                           "eccentricity", "kreach", "load",
                           "radiality", "lin", "decay", "residual_closeness",
                           "dangalchev", "generalized_closeness", "harary",
                           "average_distance", "barycenter", "wiener",
                           "closeness_vitality", "centroid", "stress",
                           "flow_betweenness", "integration", "gilschmidt",
                           "markov")
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
      hits_result = hits_result, lambda = lambda, k = k, states = states,
      decay_parameter = decay_parameter, dmnc_epsilon = dmnc_epsilon,
      membership = membership,
      katz_alpha = katz_alpha, hubbell_weight = hubbell_weight
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

    if (length(neighbors_i) == 0) { # nocov start
      result[i] <- NaN
      next
    } # nocov end

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
  # Qi et al. (2012) local formula: deg² + deg + 2 * Σ(neighbor_degrees)
  # Matches NetworkX and centiserve::laplacian()
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (n == 1) return(0)

  result <- numeric(n)
  for (v in seq_len(n)) {
    deg_v <- igraph::degree(g, v)
    neighbors <- igraph::neighbors(g, v)
    sum_neighbor_deg <- sum(igraph::degree(g, neighbors))
    result[v] <- deg_v^2 + deg_v + 2 * sum_neighbor_deg
  }

  if (normalized && max(result) > 0) {
    result <- result / max(result)
  }

  result
}

#' Calculate load centrality
#'
#' Goh et al.'s load centrality as implemented in sna::loadcent.
#' Uses Brandes-style algorithm where flow is divided equally among
#' shortest-path predecessors. Matches sna::loadcent().
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

  # sna convention: transpose directed graphs before computing load
  if (directed && igraph::is_directed(g)) {
    g <- igraph::reverse_edges(g)
  }
  mode <- if (directed) "out" else "all"
  load <- numeric(n)

  # Pre-build incoming neighbor list with edge weights for predecessor checks
  el <- igraph::as_edgelist(g, names = FALSE)
  if (is.null(weights)) {
    edge_w <- rep(1, nrow(el))
  } else {
    edge_w <- weights
  }

  # For each node w, store matrix of (predecessor_v, edge_weight)
  # In directed mode: predecessor is el[,1] for target el[,2]
  # In undirected mode: both directions
  incoming <- vector("list", n)
  for (i in seq_len(nrow(el))) {
    incoming[[el[i, 2]]] <- rbind(incoming[[el[i, 2]]], c(el[i, 1], edge_w[i]))
    if (!directed) {
      incoming[[el[i, 1]]] <- rbind(incoming[[el[i, 1]]], c(el[i, 2], edge_w[i]))
    }
  }

  for (s in seq_len(n)) {
    # Get distances from source
    # Use NA to force unweighted when weights not provided (NULL = auto-detect)
    dist_weights <- if (is.null(weights)) NA else weights
    dist_s <- igraph::distances(g, v = s, mode = mode, weights = dist_weights)[1, ]

    # Find predecessors using actual edge weights (not hardcoded 1)
    sigma <- numeric(n)
    sigma[s] <- 1
    pred <- vector("list", n)

    # Process reachable nodes in distance order
    reachable <- which(!is.infinite(dist_s) & seq_len(n) != s)
    ordered_nodes <- reachable[order(dist_s[reachable])]

    for (w in ordered_nodes) {
      inc <- incoming[[w]]
      if (is.null(inc)) next # nocov
      if (is.null(dim(inc))) inc <- matrix(inc, nrow = 1) # nocov
      for (r in seq_len(nrow(inc))) {
        v <- inc[r, 1]
        ew <- inc[r, 2]
        # Check if edge v->w lies on a shortest path from s
        if (abs(dist_s[w] - dist_s[v] - ew) < 1e-10) {
          sigma[w] <- sigma[w] + sigma[v]
          pred[[w]] <- c(pred[[w]], v)
        }
      }
    }

    # Accumulation phase (reverse distance order, load-style)
    # Only reachable nodes (+ source) carry unit load
    delta <- numeric(n)
    delta[c(s, ordered_nodes)] <- 1
    for (w in rev(ordered_nodes)) {
      if (length(pred[[w]]) > 0) {
        flow_per_pred <- delta[w] / length(pred[[w]])
        for (v in pred[[w]]) {
          delta[v] <- delta[v] + flow_per_pred
        }
      }
    }

    load <- load + delta
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
  if (sum(positive) == 0) return(rep(NA_real_, n)) # nocov

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

  # Must be connected and undirected
  if (!igraph::is_connected(g, mode = "weak")) {
    warning("Graph is not connected; current-flow betweenness undefined")
    return(rep(NA_real_, n))
  }

  # Get adjacency matrix (for edge weights)
  if (is.null(weights)) {
    A <- igraph::as_adjacency_matrix(g, sparse = FALSE)
  } else {
    A <- igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
  }

  # Get Laplacian
  L <- igraph::laplacian_matrix(g, weights = weights, sparse = FALSE)

  # Pseudoinverse of Laplacian
  J <- matrix(1, n, n)
  L_tilde <- L - J / n

  svd_result <- svd(L_tilde)
  tol <- max(dim(L_tilde)) * max(svd_result$d) * .Machine$double.eps
  positive <- svd_result$d > tol

  if (sum(positive) == 0) return(rep(NA_real_, n)) # nocov

  L_pinv <- svd_result$v[, positive, drop = FALSE] %*%
    diag(1 / svd_result$d[positive], nrow = sum(positive)) %*%
    t(svd_result$u[, positive, drop = FALSE])

  # Calculate throughput for each node using Brandes & Fleischer algorithm
  # For each source-target pair, compute current through each node
  # Throughput = (1/2) * sum of |current| on incident edges
  betweenness <- numeric(n)

  for (s in seq_len(n)) {
    for (t in seq_len(n)) {
      if (s >= t) next  # Only consider each pair once

      # Potential at each node: p_v = L+_vs - L+_vt
      potential <- L_pinv[, s] - L_pinv[, t]

      # For each node v, compute throughput = (1/2) * sum |w_vu * (p_v - p_u)|
      for (v in seq_len(n)) {
        if (v == s || v == t) next
        throughput <- 0
        for (u in seq_len(n)) {
          if (A[v, u] > 0) {  # Edge exists
            edge_current <- A[v, u] * (potential[v] - potential[u])
            throughput <- throughput + abs(edge_current)
          }
        }
        betweenness[v] <- betweenness[v] + throughput / 2
      }
    }
  }

  # Normalize: 2 / ((n-1)(n-2)) matches NetworkX normalized=TRUE
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
    if (length(candidates) == 0) break # nocov

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

#' Calculate percolation centrality
#'
#' Measures node importance for percolation/spreading processes using Brandes algorithm.
#' Each node has a "percolation state" (0-1) representing how activated/infected it is.
#' When all states are 1, this equals betweenness centrality.
#'
#' @param g igraph object
#' @param states Named numeric vector of percolation states (0-1) for each node.
#'   If NULL, all nodes get state 1 (equivalent to betweenness).
#' @param weights Edge weights (NULL for unweighted)
#' @param directed Whether to respect edge direction
#' @return Numeric vector of percolation centrality values
#' @references
#' Piraveenan, M., Prokopenko, M., & Hossain, L. (2013).
#' Percolation centrality: Quantifying graph-theoretic impact of nodes during percolation in networks.
#' @noRd
calculate_percolation <- function(g, states = NULL, weights = NULL, directed = TRUE) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (n <= 2) return(rep(0, n))

  mode <- if (directed) "out" else "all"

  # Get node names/indices
  node_names <- igraph::V(g)$name
  if (is.null(node_names)) node_names <- seq_len(n)

  # Initialize percolation states (default all 1.0)
  if (is.null(states)) {
    states <- rep(1.0, n)
  } else {
    if (!is.null(names(states))) {
      states <- states[as.character(node_names)]
    }
    if (length(states) != n) {
      stop("states vector length must match number of nodes", call. = FALSE)
    }
    states[is.na(states)] <- 1.0
    states <- pmax(0, pmin(1, states))
  }

  # Total percolation state
  p_sigma_x_t <- sum(states)
  if (p_sigma_x_t == 0) {
    return(rep(0, n))
  }

  # Initialize centrality
  percolation <- numeric(n)

  # Pre-build incoming neighbor list with edge weights for predecessor checks
  el <- igraph::as_edgelist(g, names = FALSE)
  if (is.null(weights)) {
    edge_w <- rep(1, nrow(el))
  } else {
    edge_w <- weights
  }
  incoming <- vector("list", n)
  for (i in seq_len(nrow(el))) {
    incoming[[el[i, 2]]] <- rbind(incoming[[el[i, 2]]], c(el[i, 1], edge_w[i]))
    if (!directed) {
      incoming[[el[i, 1]]] <- rbind(incoming[[el[i, 1]]], c(el[i, 2], edge_w[i]))
    }
  }

  # Brandes-style algorithm for each source
  for (s in seq_len(n)) {
    if (states[s] == 0) next

    # Distances from source
    dist_weights <- if (is.null(weights)) NA else weights
    dist_s <- igraph::distances(g, v = s, mode = mode, weights = dist_weights)[1, ]

    # Find predecessors using actual edge weights
    sigma <- numeric(n)
    sigma[s] <- 1
    pred <- vector("list", n)

    reachable <- which(!is.infinite(dist_s) & seq_len(n) != s)
    ordered_nodes <- reachable[order(dist_s[reachable])]

    for (w in ordered_nodes) {
      inc <- incoming[[w]]
      if (is.null(inc)) next # nocov
      if (is.null(dim(inc))) inc <- matrix(inc, nrow = 1) # nocov
      for (r in seq_len(nrow(inc))) {
        v <- inc[r, 1]
        ew <- inc[r, 2]
        if (abs(dist_s[w] - dist_s[v] - ew) < 1e-10) {
          sigma[w] <- sigma[w] + sigma[v]
          pred[[w]] <- c(pred[[w]], v)
        }
      }
    }

    # Accumulation phase (Brandes algorithm, reverse distance order)
    delta <- numeric(n)

    for (w in rev(ordered_nodes)) {
      if (sigma[w] > 0) {
        coeff <- (1 + delta[w]) / sigma[w]
        for (v in pred[[w]]) {
          delta[v] <- delta[v] + sigma[v] * coeff
        }
      }
      # Percolation weight: states[s] / (total - states[w])
      denom <- p_sigma_x_t - states[w]
      if (denom > 0) {
        pw_s_w <- states[s] / denom
        percolation[w] <- percolation[w] + delta[w] * pw_s_w
      }
    }
  }

  # Normalize by (n-2)
  if (n > 2) {
    percolation <- percolation / (n - 2)
  }

  percolation
}

#' Calculate a single centrality measure
#' @noRd
calculate_measure <- function(g, measure, mode, weights, normalized,
                              cutoff, damping, personalized,
                              transitivity_type, isolates,
                              hits_result = NULL, lambda = 1, k = 3,
                              states = NULL, decay_parameter = 0.5,
                              dmnc_epsilon = 1.7,
                              membership = NULL,
                              katz_alpha = 0.1, hubbell_weight = 0.5) {
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
    "subgraph" = igraph::subgraph_centrality(g, diag = FALSE),
    "laplacian" = calculate_laplacian(g, weights = weights, normalized = normalized),
    "load" = calculate_load(g, weights = weights, directed = directed),
    "current_flow_closeness" = calculate_current_flow_closeness(g, weights = weights),
    "current_flow_betweenness" = calculate_current_flow_betweenness(g, weights = weights),
    "voterank" = calculate_voterank(g, directed = directed),
    "percolation" = calculate_percolation(g, states = states, weights = weights, directed = directed),
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

    # Extended measures — distance-based closeness variants
    "radiality" = calculate_radiality(g, mode = mode, weights = weights),
    "lin" = calculate_lin(g, mode = mode, weights = weights),
    "decay" = calculate_decay(g, mode = mode, weights = weights,
                              decay_parameter = decay_parameter),
    "residual_closeness" = calculate_residual_closeness(g, mode = mode,
                                                        weights = weights),
    "dangalchev" = calculate_dangalchev(g, mode = mode, weights = weights),
    "generalized_closeness" = calculate_generalized_closeness(
      g, mode = mode, weights = weights, alpha = decay_parameter
    ),
    "harary" = calculate_harary(g, mode = mode, weights = weights),
    "average_distance" = calculate_average_distance(g, mode = mode,
                                                    weights = weights),
    "barycenter" = calculate_barycenter(g, mode = mode, weights = weights),
    "wiener" = calculate_wiener(g, mode = mode, weights = weights),
    "closeness_vitality" = calculate_closeness_vitality(g, mode = mode,
                                                        weights = weights),

    # Extended measures — spectral/walk-based
    "communicability" = calculate_communicability(g),
    "communicability_betweenness" = calculate_communicability_betweenness(g),
    "random_walk" = calculate_random_walk(g),

    # Extended measures — path-based
    "stress" = calculate_stress(g, weights = weights, directed = directed),
    "flow_betweenness" = calculate_flow_betweenness(g, weights = weights,
                                                    directed = directed),

    # Extended measures — local/neighborhood
    "lobby" = calculate_lobby(g, mode = mode),
    "entropy" = calculate_entropy(g, mode = mode),
    "semilocal" = calculate_semilocal(g, mode = mode),
    "clusterrank" = calculate_clusterrank(g, mode = mode),
    "bottleneck" = calculate_bottleneck(g, mode = mode),
    "centroid" = calculate_centroid(g, mode = mode, weights = weights),
    "mnc" = calculate_mnc(g, mode = mode),
    "dmnc" = calculate_dmnc(g, mode = mode, epsilon = dmnc_epsilon),
    "lac" = calculate_lac(g, mode = mode),
    "topological_coefficient" = calculate_topological_coefficient(g),
    "bridging" = calculate_bridging(g, weights = weights, directed = directed),
    "local_bridging" = calculate_local_bridging(g),
    "effective_size" = calculate_effective_size(g),
    "diversity" = calculate_diversity(g, weights = weights),
    "cross_clique" = calculate_cross_clique(g),
    "markov" = calculate_markov(g),

    # Extended measures — with mode support
    "integration" = calculate_integration(g, mode = mode),
    "expected" = calculate_expected(g, mode = mode),
    "gilschmidt" = calculate_gilschmidt(g, mode = mode),

    # Zoo batch 2 — mode measures
    "gravity" = calculate_gravity(g, mode = mode),
    "collective_influence" = calculate_collective_influence(g, mode = mode),
    "local_hindex" = as.numeric(calculate_local_hindex(g, mode = mode)),
    "hindex_strength" = as.numeric(calculate_hindex_strength(g, mode = mode)),
    "onion" = as.numeric(calculate_onion(g)),

    # Zoo batch 2 — no-mode measures
    "second_order" = calculate_second_order(g),
    "infection" = calculate_infection(g),
    "nonbacktracking" = calculate_nonbacktracking(g),
    "spanning_tree" = calculate_spanning_tree(g),

    # Directed-only measures
    "salsa" = calculate_salsa(g),
    "leaderrank" = calculate_leaderrank(g),
    "trophic_level" = calculate_trophic_level(g),

    # Community-aware measures (require membership parameter)
    "participation" = calculate_participation(g, membership = membership,
                                              mode = mode),
    "within_module_z" = calculate_within_module_z(g, membership = membership,
                                                   mode = mode),
    "gateway" = calculate_gateway(g, membership = membership, mode = mode),

    # Batch 3 — classical measures with reference-package validation
    "katz" = calculate_katz(g, weights = weights, alpha = katz_alpha),
    "hubbell" = calculate_hubbell(g, weights = weights,
                                  weightfactor = hubbell_weight),
    "information" = calculate_information(g, weights = weights),
    "pairwisedis" = calculate_pairwisedis(g),
    "reaching_local" = calculate_reaching_local(g, mode = mode,
                                                weights = weights),

    # Batch 4 — directed prestige family (Wasserman-Faust / sna)
    "prestige_domain" = calculate_prestige_domain(g),
    "prestige_domain_proximity" = calculate_prestige_domain_proximity(g),

    # Batch 5 — Gould-Fernandez brokerage (5 roles)
    "brokerage_coordinator"    = calculate_brokerage(g, membership, "coordinator"),
    "brokerage_itinerant"      = calculate_brokerage(g, membership, "itinerant"),
    "brokerage_representative" = calculate_brokerage(g, membership, "representative"),
    "brokerage_gatekeeper"     = calculate_brokerage(g, membership, "gatekeeper"),
    "brokerage_liaison"        = calculate_brokerage(g, membership, "liaison"),

    stop("Unknown measure: ", measure, call. = FALSE)
  )

  # Remove names to ensure consistent output
  unname(value)
}

#' Degree Centrality
#'
#' Number of edges connected to each node. For directed networks,
#' \code{centrality_indegree} counts incoming edges and
#' \code{centrality_outdegree} counts outgoing edges.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{normalized}, \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of degree values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_strength}} for the weighted version.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_degree(adj)
centrality_degree <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "degree", mode = mode, ...)
  col <- paste0("degree_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_degree
#' @export
centrality_indegree <- function(x, ...) {
  df <- centrality(x, measures = "degree", mode = "in", ...)
  stats::setNames(df$degree_in, df$node)
}

#' @rdname centrality_degree
#' @export
centrality_outdegree <- function(x, ...) {
  df <- centrality(x, measures = "degree", mode = "out", ...)
  stats::setNames(df$degree_out, df$node)
}

#' Strength Centrality (Weighted Degree)
#'
#' Sum of edge weights connected to each node. For directed networks,
#' \code{centrality_instrength} sums incoming weights and
#' \code{centrality_outstrength} sums outgoing weights.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of strength values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_degree}} for the unweighted version.
#'
#' @export
#' @examples
#' mat <- matrix(c(0, .5, .3, .5, 0, .8, .3, .8, 0), 3, 3)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C")
#' centrality_strength(mat)
centrality_strength <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "strength", mode = mode, ...)
  col <- paste0("strength_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_strength
#' @export
centrality_instrength <- function(x, ...) {
  df <- centrality(x, measures = "strength", mode = "in", ...)
  stats::setNames(df$strength_in, df$node)
}

#' @rdname centrality_strength
#' @export
centrality_outstrength <- function(x, ...) {
  df <- centrality(x, measures = "strength", mode = "out", ...)
  stats::setNames(df$strength_out, df$node)
}

#' Betweenness Centrality
#'
#' Fraction of shortest paths passing through each node. Nodes with high
#' betweenness act as bridges connecting different parts of the network.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{normalized}, \code{weighted}, \code{directed}, \code{cutoff},
#'   \code{invert_weights}).
#'
#' @return Named numeric vector of betweenness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_load}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_betweenness(adj)
centrality_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "betweenness", ...)
  stats::setNames(df$betweenness, df$node)
}

#' Closeness Centrality
#'
#' Inverse of the average shortest path distance from a node to all others.
#' For directed networks, \code{centrality_incloseness} and
#' \code{centrality_outcloseness} measure incoming and outgoing closeness.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_harmonic}} for a variant that handles disconnected
#'   graphs.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_closeness(adj)
centrality_closeness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "closeness", mode = mode, ...)
  col <- paste0("closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_closeness
#' @export
centrality_incloseness <- function(x, ...) {
  df <- centrality(x, measures = "closeness", mode = "in", ...)
  stats::setNames(df$closeness_in, df$node)
}

#' @rdname centrality_closeness
#' @export
centrality_outcloseness <- function(x, ...) {
  df <- centrality(x, measures = "closeness", mode = "out", ...)
  stats::setNames(df$closeness_out, df$node)
}

#' Eigenvector Centrality
#'
#' Influence-based centrality where a node's score depends on the scores
#' of its neighbors. Nodes connected to other high-scoring nodes get
#' higher scores.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of eigenvector centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_pagerank}} for a random walk variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_eigenvector(adj)
centrality_eigenvector <- function(x, ...) {
  df <- centrality(x, measures = "eigenvector", ...)
  stats::setNames(df$eigenvector, df$node)
}

#' PageRank Centrality
#'
#' Random walk centrality measuring node importance. Simulates a random
#' walker that follows edges with probability \code{damping} and jumps to a
#' random node with probability \code{1 - damping}.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param damping Damping factor (probability of following an edge). Default 0.85.
#' @param personalized Named numeric vector for personalized PageRank.
#'   Values should sum to 1. Default \code{NULL} (uniform).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of PageRank values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_eigenvector}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_pagerank(adj)
#' centrality_pagerank(adj, damping = 0.9)
centrality_pagerank <- function(x, damping = 0.85, personalized = NULL, ...) {
  df <- centrality(x, measures = "pagerank",
                   damping = damping, personalized = personalized, ...)
  stats::setNames(df$pagerank, df$node)
}

#' HITS Authority and Hub Scores
#'
#' Kleinberg's HITS algorithm. \code{centrality_authority} scores nodes
#' pointed to by good hubs. \code{centrality_hub} scores nodes that point
#' to good authorities.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of authority or hub scores.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_authority(adj)
#' centrality_hub(adj)
centrality_authority <- function(x, ...) {
  df <- centrality(x, measures = "authority", ...)
  stats::setNames(df$authority, df$node)
}

#' @rdname centrality_authority
#' @export
centrality_hub <- function(x, ...) {
  df <- centrality(x, measures = "hub", ...)
  stats::setNames(df$hub, df$node)
}

#' Eccentricity
#'
#' Maximum shortest path distance from a node to any other node.
#' For directed networks, \code{centrality_ineccentricity} and
#' \code{centrality_outeccentricity} use incoming and outgoing paths.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of eccentricity values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_eccentricity(adj)
centrality_eccentricity <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "eccentricity", mode = mode, ...)
  col <- paste0("eccentricity_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_eccentricity
#' @export
centrality_ineccentricity <- function(x, ...) {
  df <- centrality(x, measures = "eccentricity", mode = "in", ...)
  stats::setNames(df$eccentricity_in, df$node)
}

#' @rdname centrality_eccentricity
#' @export
centrality_outeccentricity <- function(x, ...) {
  df <- centrality(x, measures = "eccentricity", mode = "out", ...)
  stats::setNames(df$eccentricity_out, df$node)
}

#' K-Core Decomposition (Coreness)
#'
#' Assigns each node to its maximum k-core. A k-core is a maximal subgraph
#' where every node has at least k connections within the subgraph.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of coreness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_coreness(adj)
centrality_coreness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "coreness", mode = mode, ...)
  col <- paste0("coreness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Burt's Constraint
#'
#' Network constraint measuring the extent to which a node's connections are
#' redundant. Low constraint indicates access to structural holes (brokerage
#' opportunities).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of constraint values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_constraint(adj)
centrality_constraint <- function(x, ...) {
  df <- centrality(x, measures = "constraint", ...)
  stats::setNames(df$constraint, df$node)
}

#' Local Transitivity (Clustering Coefficient)
#'
#' Proportion of triangles around each node relative to the number of
#' possible triangles. Measures how tightly clustered a node's neighborhood is.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param transitivity_type Type of transitivity: \code{"local"} (default),
#'   \code{"global"}, \code{"undirected"}, \code{"localundirected"},
#'   \code{"barrat"} (weighted), or \code{"weighted"}.
#' @param isolates How to handle isolate nodes: \code{"nan"} (default) or
#'   \code{"zero"}.
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of transitivity values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_transitivity(adj)
centrality_transitivity <- function(x, transitivity_type = "local",
                                    isolates = "nan", ...) {
  df <- centrality(x, measures = "transitivity",
                   transitivity_type = transitivity_type, isolates = isolates, ...)
  stats::setNames(df$transitivity, df$node)
}

#' Harmonic Centrality
#'
#' Sum of inverse shortest path distances to all other nodes. Unlike closeness,
#' harmonic centrality handles disconnected graphs naturally (unreachable nodes
#' contribute 0 instead of making the measure undefined).
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of harmonic centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_closeness}} for the traditional variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_harmonic(adj)
centrality_harmonic <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "harmonic", mode = mode, ...)
  col <- paste0("harmonic_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality_harmonic
#' @export
centrality_inharmonic <- function(x, ...) {
  df <- centrality(x, measures = "harmonic", mode = "in", ...)
  stats::setNames(df$harmonic_in, df$node)
}

#' @rdname centrality_harmonic
#' @export
centrality_outharmonic <- function(x, ...) {
  df <- centrality(x, measures = "harmonic", mode = "out", ...)
  stats::setNames(df$harmonic_out, df$node)
}

#' Diffusion Centrality
#'
#' Sum of scaled degrees of a node and its neighbors, measuring the node's
#' potential for spreading information through the network.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param lambda Scaling factor for neighbor contributions. Default 1.
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of diffusion centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_diffusion(adj)
centrality_diffusion <- function(x, mode = "all", lambda = 1, ...) {
  df <- centrality(x, measures = "diffusion", mode = mode, lambda = lambda, ...)
  col <- paste0("diffusion_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Leverage Centrality
#'
#' Measures a node's influence over its neighbors based on relative degree
#' differences. Positive values indicate the node has more connections than
#' its average neighbor.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of leverage centrality values (range -1 to 1).
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_leverage(adj)
centrality_leverage <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "leverage", mode = mode, ...)
  col <- paste0("leverage_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Geodesic K-Path Centrality
#'
#' Count of nodes reachable within shortest path distance \code{k}. Measures
#' how many nodes a given node can reach quickly.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param k Maximum path length. Default 3.
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}, \code{invert_weights}).
#'
#' @return Named numeric vector of k-reach centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_kreach(adj, k = 2)
centrality_kreach <- function(x, mode = "all", k = 3, ...) {
  df <- centrality(x, measures = "kreach", mode = mode, k = k, ...)
  col <- paste0("kreach_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Alpha (Katz) Centrality
#'
#' Influence via all paths penalized by distance. Similar to eigenvector
#' centrality but includes an exogenous contribution, making it well-defined
#' even for directed acyclic graphs.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of alpha centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_eigenvector}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_alpha(adj)
centrality_alpha <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "alpha", mode = mode, ...)
  col <- paste0("alpha_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Bonacich Power Centrality
#'
#' Measures influence based on connections to other influential nodes.
#' The power parameter controls whether connections to well-connected
#' nodes increase or decrease centrality.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of power centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_eigenvector}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_power(adj)
centrality_power <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "power", mode = mode, ...)
  col <- paste0("power_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Subgraph Centrality
#'
#' Participation in closed loops (walks), weighting shorter loops more heavily.
#' Based on the diagonal of the matrix exponential of the adjacency matrix.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of subgraph centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_subgraph(adj)
centrality_subgraph <- function(x, ...) {
  df <- centrality(x, measures = "subgraph", ...)
  stats::setNames(df$subgraph, df$node)
}

#' Laplacian Centrality
#'
#' Energy drop from the graph Laplacian when a node is removed
#' (Qi et al. 2012). Measures a node's importance to the overall
#' network energy.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of Laplacian centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_laplacian(adj)
centrality_laplacian <- function(x, ...) {
  df <- centrality(x, measures = "laplacian", ...)
  stats::setNames(df$laplacian, df$node)
}

#' Load Centrality
#'
#' Fraction of all shortest paths passing through a node, similar to
#' betweenness but weighting paths by 1/count (Goh et al. 2001).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of load centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} for the standard variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_load(adj)
centrality_load <- function(x, ...) {
  df <- centrality(x, measures = "load", ...)
  stats::setNames(df$load, df$node)
}

#' Current Flow Closeness Centrality
#'
#' Information centrality based on electrical current flow through the network.
#' Uses the pseudoinverse of the Laplacian matrix. Requires a connected graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of current flow closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_closeness}} for the shortest-path variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_current_flow_closeness(adj)
centrality_current_flow_closeness <- function(x, ...) {
  df <- centrality(x, measures = "current_flow_closeness", ...)
  stats::setNames(df$current_flow_closeness, df$node)
}

#' Current Flow Betweenness Centrality
#'
#' Betweenness based on electrical current flow rather than shortest paths.
#' Uses the Laplacian pseudoinverse. Requires a connected graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of current flow betweenness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} for the shortest-path variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_current_flow_betweenness(adj)
centrality_current_flow_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "current_flow_betweenness", ...)
  stats::setNames(df$current_flow_betweenness, df$node)
}

#' VoteRank Centrality
#'
#' Identifies influential spreaders via an iterative voting mechanism.
#' Returns normalized rank (1 = most influential). Based on
#' Zhang et al. (2016).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of VoteRank values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_voterank(adj)
centrality_voterank <- function(x, ...) {
  df <- centrality(x, measures = "voterank", ...)
  stats::setNames(df$voterank, df$node)
}

#' Percolation Centrality
#'
#' Importance for spreading processes using node states. Each node has
#' a state (0-1) representing how activated it is. When all states are
#' equal, equivalent to betweenness.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param states Named numeric vector of node states (0-1). Default \code{NULL}
#'   (all nodes get state 1).
#' @param ... Additional arguments passed to \code{\link{centrality}} (e.g.,
#'   \code{weighted}, \code{directed}).
#'
#' @return Named numeric vector of percolation centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} which this generalizes.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_percolation(adj)
#' centrality_percolation(adj, states = c(A = 0.8, B = 0.2, C = 0.5))
centrality_percolation <- function(x, states = NULL, ...) {
  df <- centrality(x, measures = "percolation", states = states, ...)
  stats::setNames(df$percolation, df$node)
}

# =============================================================================
# Extended centrality convenience wrappers
# =============================================================================

#' Radiality Centrality
#'
#' Centrality based on sum of (diameter + 1 - distance) normalized by n-1.
#' Nodes closer to others (on average) have higher radiality.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of radiality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_closeness}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_radiality(adj)
centrality_radiality <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "radiality", mode = mode, ...)
  col <- paste0("radiality_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Lin Centrality
#'
#' Reachable nodes squared divided by sum of distances. Well-defined for
#' disconnected graphs.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Lin centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_closeness}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_lin(adj)
centrality_lin <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "lin", mode = mode, ...)
  col <- paste0("lin_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Decay Centrality
#'
#' Sum of delta^d over all nodes, where d is the shortest path distance.
#' Nodes near many others get higher scores. The \code{decay_parameter}
#' controls the distance penalty.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param decay_parameter Numeric between 0 and 1. Default 0.5.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of decay centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_decay(adj, decay_parameter = 0.5)
centrality_decay <- function(x, mode = "all", decay_parameter = 0.5, ...) {
  df <- centrality(x, measures = "decay", mode = mode,
                   decay_parameter = decay_parameter, ...)
  col <- paste0("decay_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Residual Closeness Centrality
#'
#' Sum of 1/2^d for all nodes, including self. Robust to disconnected graphs.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of residual closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_dangalchev}} (alias).
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_residual_closeness(adj)
centrality_residual_closeness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "residual_closeness", mode = mode, ...)
  col <- paste0("residual_closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Dangalchev Closeness Centrality
#'
#' Alias for residual closeness centrality: sum of 1/2^d.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Dangalchev closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_residual_closeness}} (equivalent).
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_dangalchev(adj)
centrality_dangalchev <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "dangalchev", mode = mode, ...)
  col <- paste0("dangalchev_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Generalized Closeness Centrality
#'
#' Sum of alpha^d over all nodes. Generalization of decay centrality
#' matching tidygraph's implementation.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param decay_parameter Numeric between 0 and 1 (the alpha parameter).
#'   Default 0.5.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of generalized closeness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_decay}} (equivalent formulation).
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_generalized_closeness(adj)
centrality_generalized_closeness <- function(x, mode = "all",
                                             decay_parameter = 0.5, ...) {
  df <- centrality(x, measures = "generalized_closeness", mode = mode,
                   decay_parameter = decay_parameter, ...)
  col <- paste0("generalized_closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Harary Centrality
#'
#' Sum of 1/d^2 over all reachable node pairs. Robust to disconnected graphs.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Harary centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_harary(adj)
centrality_harary <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "harary", mode = mode, ...)
  col <- paste0("harary_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Average Distance Centrality
#'
#' Sum of shortest path distances divided by (n + 1). Lower values indicate
#' more central nodes.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of average distance values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_average_distance(adj)
centrality_average_distance <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "average_distance", mode = mode, ...)
  col <- paste0("average_distance_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Barycenter Centrality
#'
#' Inverse of the total distance to all reachable nodes.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of barycenter centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_barycenter(adj)
centrality_barycenter <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "barycenter", mode = mode, ...)
  col <- paste0("barycenter_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Wiener Index Centrality
#'
#' Total sum of shortest path distances from a node to all others.
#' Higher values indicate less central (more peripheral) nodes.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Wiener index values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_wiener(adj)
centrality_wiener <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "wiener", mode = mode, ...)
  col <- paste0("wiener_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Closeness Vitality
#'
#' Drop in the Wiener index when a node is removed. Higher values indicate
#' more critical nodes for overall connectivity.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of closeness vitality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_closeness_vitality(adj)
centrality_closeness_vitality <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "closeness_vitality", mode = mode, ...)
  col <- paste0("closeness_vitality_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Lobby Index (H-Index of Neighborhood)
#'
#' Largest k such that the node's closed neighborhood contains at least k
#' nodes with degree >= k. Network analogue of the h-index.
#'
#' @inheritParams centrality_degree
#'
#' @return Named integer vector of lobby index values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_lobby(adj)
centrality_lobby <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "lobby", mode = mode, ...)
  col <- paste0("lobby_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Entropy Centrality
#'
#' Graph-theoretic entropy based on shortest path distribution in the residual
#' graph after removing the node.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of entropy centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_entropy(adj)
centrality_entropy <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "entropy", mode = mode, ...)
  col <- paste0("entropy_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Semi-Local Centrality
#'
#' Triple-nested neighborhood computation measuring 4-hop local influence.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of semi-local centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_semilocal(adj)
centrality_semilocal <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "semilocal", mode = mode, ...)
  col <- paste0("semilocal_", mode)
  stats::setNames(df[[col]], df$node)
}

#' ClusterRank Centrality
#'
#' Product of clustering coefficient and sum of (neighbor degree + 1).
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of ClusterRank values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_clusterrank(adj)
centrality_clusterrank <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "clusterrank", mode = mode, ...)
  col <- paste0("clusterrank_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Bottleneck Centrality
#'
#' Number of shortest path trees where the node appears in more than n/4 paths.
#'
#' @inheritParams centrality_degree
#'
#' @return Named integer vector of bottleneck centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_bottleneck(adj)
centrality_bottleneck <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "bottleneck", mode = mode, ...)
  col <- paste0("bottleneck_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Centroid Value
#'
#' Minimum difference between own and competitor's closer-node count.
#' Measures how much a node is at the center of the graph.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of centroid values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_centroid(adj)
centrality_centroid <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "centroid", mode = mode, ...)
  col <- paste0("centroid_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Maximum Neighborhood Component (MNC)
#'
#' Size of the largest connected component in the node's neighborhood subgraph.
#'
#' @inheritParams centrality_degree
#'
#' @return Named integer vector of MNC values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_dmnc}} for the density variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_mnc(adj)
centrality_mnc <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "mnc", mode = mode, ...)
  col <- paste0("mnc_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Density of Maximum Neighborhood Component (DMNC)
#'
#' Edge count divided by max component size^1.5 in the neighborhood subgraph.
#'
#' @inheritParams centrality_degree
#' @param dmnc_epsilon Numeric. Epsilon exponent for DMNC. Default 1.7 as
#'   recommended by Lin et al. (2008). centiserve uses 1.67 (four-community
#'   assumption). Must be between 1 and 2.
#'
#' @return Named numeric vector of DMNC values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_mnc}} for the size-only variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_dmnc(adj)
centrality_dmnc <- function(x, mode = "all", dmnc_epsilon = 1.7, ...) {
  df <- centrality(x, measures = "dmnc", mode = mode,
                   dmnc_epsilon = dmnc_epsilon, ...)
  col <- paste0("dmnc_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Local Average Connectivity (LAC)
#'
#' Average degree of neighbors within the neighborhood subgraph. Measures
#' how interconnected a node's neighbors are. Proposed by Li et al. (2011)
#' for identifying essential proteins in PPI networks.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of LAC values.
#'
#' @references
#' Li, M., Wang, J., Chen, X., Wang, H., & Pan, Y. (2011). A local average
#' connectivity-based method for identifying essential proteins from the network
#' level. \emph{Computational Biology and Chemistry}, 35(3), 143-150.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_dmnc}} for another neighborhood density measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_lac(adj)
centrality_lac <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "lac", mode = mode, ...)
  col <- paste0("lac_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Communicability Centrality
#'
#' Total communicability: row sums of the matrix exponential of the adjacency
#' matrix. Measures a node's ability to broadcast information through all paths.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of communicability values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_subgraph}} for the diagonal-only variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_communicability(adj)
centrality_communicability <- function(x, ...) {
  df <- centrality(x, measures = "communicability", ...)
  stats::setNames(df$communicability, df$node)
}

#' Communicability Betweenness Centrality
#'
#' Fraction of total communicability that passes through each node.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of communicability betweenness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_communicability_betweenness(adj)
centrality_communicability_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "communicability_betweenness", ...)
  stats::setNames(df$communicability_betweenness, df$node)
}

#' Random Walk Centrality
#'
#' Inverse sum of random walk distances. Requires a connected graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of random walk centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_random_walk(adj)
centrality_random_walk <- function(x, ...) {
  df <- centrality(x, measures = "random_walk", ...)
  stats::setNames(df$random_walk, df$node)
}

#' Stress Centrality
#'
#' Number of shortest paths passing through each node. Unlike betweenness,
#' does not normalize by the total number of shortest paths.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of stress centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} for the normalized variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_stress(adj)
centrality_stress <- function(x, ...) {
  df <- centrality(x, measures = "stress", ...)
  stats::setNames(df$stress, df$node)
}

#' Flow Betweenness Centrality
#'
#' Max-flow based betweenness centrality.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of flow betweenness values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_betweenness}} for shortest-path variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_flow_betweenness(adj)
centrality_flow_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "flow_betweenness", ...)
  stats::setNames(df$flow_betweenness, df$node)
}

#' Topological Coefficient
#'
#' Fraction of shared second-order neighbors, measuring topological overlap
#' between a node and its neighbors.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of topological coefficient values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_topological_coefficient(adj)
centrality_topological_coefficient <- function(x, ...) {
  df <- centrality(x, measures = "topological_coefficient", ...)
  stats::setNames(df$topological_coefficient, df$node)
}

#' Bridging Centrality
#'
#' Product of betweenness and bridging coefficient. Identifies nodes that
#' bridge communities.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of bridging centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_local_bridging}} for the local variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_bridging(adj)
centrality_bridging <- function(x, ...) {
  df <- centrality(x, measures = "bridging", ...)
  stats::setNames(df$bridging, df$node)
}

#' Local Bridging Centrality
#'
#' (1/degree) times bridging coefficient. Local measure of inter-community
#' connectivity.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of local bridging values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_bridging}} for the betweenness-weighted variant.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_local_bridging(adj)
centrality_local_bridging <- function(x, ...) {
  df <- centrality(x, measures = "local_bridging", ...)
  stats::setNames(df$local_bridging, df$node)
}

#' Effective Size (Burt's)
#'
#' Network effective size: degree minus redundancy. Measures non-redundant
#' contacts in ego network.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of effective size values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_constraint}} for a related structural holes measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_effective_size(adj)
centrality_effective_size <- function(x, ...) {
  df <- centrality(x, measures = "effective_size", ...)
  stats::setNames(df$effective_size, df$node)
}

#' Diversity Centrality
#'
#' Shannon entropy of the edge weight distribution per node. Measures
#' how evenly a node distributes its connections.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of diversity centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' mat <- matrix(c(0, .5, .3, .5, 0, .8, .3, .8, 0), 3, 3)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C")
#' centrality_diversity(mat)
centrality_diversity <- function(x, ...) {
  df <- centrality(x, measures = "diversity", ...)
  stats::setNames(df$diversity, df$node)
}

#' Cross-Clique Connectivity
#'
#' Count of all cliques (not just maximal) containing each node. Measures
#' embeddedness in dense substructures.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named integer vector of cross-clique counts.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_cross_clique(adj)
centrality_cross_clique <- function(x, ...) {
  df <- centrality(x, measures = "cross_clique", ...)
  stats::setNames(df$cross_clique, df$node)
}

#' Markov Centrality
#'
#' Inverse of column means of the mean first passage time matrix.
#' Requires a connected graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of Markov centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_markov(adj)
centrality_markov <- function(x, ...) {
  df <- centrality(x, measures = "markov", ...)
  stats::setNames(df$markov, df$node)
}

#' Integration Centrality
#'
#' Distance-based influence: sum of 1 - (d-1)/max(d) over all nodes.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of integration centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_integration(adj)
centrality_integration <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "integration", mode = mode, ...)
  col <- paste0("integration_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Expected Centrality
#'
#' Sum of neighbor degrees. Simple but effective influence proxy.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of expected centrality values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_expected(adj)
centrality_expected <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "expected", mode = mode, ...)
  col <- paste0("expected_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Gil-Schmidt Power Index
#'
#' Sum of 1/d(v,w) normalized by (n-1). Variant of closeness using harmonic
#' mean of distances.
#'
#' @inheritParams centrality_degree
#'
#' @return Named numeric vector of Gil-Schmidt power index values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_harmonic}} for a related measure.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_gilschmidt(adj)
centrality_gilschmidt <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "gilschmidt", mode = mode, ...)
  col <- paste0("gilschmidt_", mode)
  stats::setNames(df[[col]], df$node)
}

#' SALSA Authority Centrality
#'
#' Stochastic Approach for Link-Structure Analysis. Returns authority scores.
#' Requires a directed graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#'   Must be directed.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of SALSA authority scores.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_authority}} for HITS authority.
#'
#' @export
#' @examples
#' \dontrun{
#' adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_salsa(adj)
#' }
centrality_salsa <- function(x, ...) {
  df <- centrality(x, measures = "salsa", ...)
  stats::setNames(df$salsa, df$node)
}

#' LeaderRank Centrality
#'
#' PageRank variant with a ground node connected to all nodes.
#' Requires a directed graph.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#'   Must be directed.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of LeaderRank values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_pagerank}} for standard PageRank.
#'
#' @export
#' @examples
#' \dontrun{
#' adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_leaderrank(adj)
#' }
centrality_leaderrank <- function(x, ...) {
  df <- centrality(x, measures = "leaderrank", ...)
  stats::setNames(df$leaderrank, df$node)
}

#' Participation Coefficient
#'
#' Measures diversity of inter-community connections. Nodes connecting to
#' many communities have high participation. Requires community membership.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param membership Integer vector of community assignments (one per node).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of participation coefficient values (0-1).
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_within_module_z}} for within-community connectivity.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' centrality_participation(adj, membership = c(1, 1, 1, 2, 2))
centrality_participation <- function(x, membership = NULL, mode = "all", ...) {
  df <- centrality(x, measures = "participation", mode = mode,
                   membership = membership, ...)
  col <- paste0("participation_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Within-Module Degree Z-Score
#'
#' Z-score of intra-community connectivity. High values indicate hubs
#' within their own community. Requires community membership.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param membership Integer vector of community assignments (one per node).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of within-module z-score values.
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_participation}} for between-community diversity.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' centrality_within_module_z(adj, membership = c(1, 1, 1, 2, 2))
centrality_within_module_z <- function(x, membership = NULL, mode = "all", ...) {
  df <- centrality(x, measures = "within_module_z", mode = mode,
                   membership = membership, ...)
  col <- paste0("within_module_z_", mode)
  stats::setNames(df[[col]], df$node)
}

#' Gateway Coefficient
#'
#' Inter-community brokerage weighted by centrality. Combines participation
#' with degree information. Requires community membership.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param membership Integer vector of community assignments (one per node).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of gateway coefficient values (0-1).
#'
#' @seealso \code{\link{centrality}} for computing multiple measures at once,
#'   \code{\link{centrality_participation}} for the simpler participation
#'   coefficient.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' centrality_gateway(adj, membership = c(1, 1, 1, 2, 2))
centrality_gateway <- function(x, membership = NULL, mode = "all", ...) {
  df <- centrality(x, measures = "gateway", mode = mode,
                   membership = membership, ...)
  col <- paste0("gateway_", mode)
  stats::setNames(df[[col]], df$node)
}


# ---------------------------------------------------------------------------
# Batch 3 wrappers: classical measures validated against centiserve / sna /
# igraph / NetworkX reference implementations.
# ---------------------------------------------------------------------------

#' Katz Centrality
#'
#' Katz (1953) status index: \eqn{C = (I - \alpha A^T)^{-1} \mathbf{1}}.
#' Each node's score sums attenuated walks of every length back to it, with
#' attenuation \eqn{\alpha} applied per step. Rankings are identical to
#' Bonacich's alpha centrality with a uniform exogenous vector.
#'
#' Equivalence is verified bit-exact against \code{centiserve::katzcent}
#' (cograph mirrors centiserve's exact LAPACK call sequence) and at machine
#' epsilon against \code{igraph::alpha_centrality(exo = 1)} and
#' \code{networkx.katz_centrality_numpy}.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param katz_alpha Attenuation factor. Must satisfy
#'   \eqn{\alpha < 1 / \rho(A)} where \eqn{\rho(A)} is the spectral radius.
#'   Default 0.1 matches centiserve and NetworkX conventions.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of Katz centrality values.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_eigenvector}},
#'   \code{\link{centrality_pagerank}}.
#' @references
#' Katz, L. (1953). A new status index derived from sociometric analysis.
#' \emph{Psychometrika}, 18(1), 39-43.
#'
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_katz(adj)
centrality_katz <- function(x, katz_alpha = 0.1, ...) {
  df <- centrality(x, measures = "katz", katz_alpha = katz_alpha, ...)
  stats::setNames(df$katz, df$node)
}


#' Hubbell Centrality
#'
#' Hubbell (1965) input-output centrality:
#' \eqn{C = (I - w W)^{-1} \mathbf{1}}, where \eqn{W} is the (weighted)
#' adjacency matrix and \eqn{w} is a weight factor that must satisfy
#' \eqn{w \cdot \rho(W) < 1} for the system to be solvable.
#'
#' Bit-exact match against \code{centiserve::hubbell} when edge weights are
#' passed explicitly (cograph mirrors centiserve's full-inverse LAPACK call
#' path).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param hubbell_weight Attenuation factor \eqn{w}. Default 0.5. If
#'   \eqn{w \cdot \rho(W) \ge 1}, the function returns \code{NA} with a warning.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of Hubbell centrality values (or \code{NA} if
#'   the system is not solvable).
#'
#' @section Note on centiserve equivalence:
#' \code{centiserve::hubbell(g, weights = NULL)} silently resets all edge
#' weights to 1, ignoring the graph's weight attribute. To reproduce cograph's
#' values with centiserve on a weighted graph, pass
#' \code{weights = igraph::E(g)$weight} explicitly.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_katz}}.
#' @references
#' Hubbell, C. H. (1965). An input-output approach to clique identification.
#' \emph{Sociometry}, 28(4), 377-399.
#'
#' @export
#' @examples
#' # Small weighted path graph; spectral radius permits weightfactor = 0.5
#' adj <- matrix(0, 4, 4)
#' adj[1,2] <- adj[2,1] <- adj[2,3] <- adj[3,2] <- adj[3,4] <- adj[4,3] <- 0.3
#' rownames(adj) <- colnames(adj) <- LETTERS[1:4]
#' centrality_hubbell(adj, hubbell_weight = 0.5)
centrality_hubbell <- function(x, hubbell_weight = 0.5, ...) {
  df <- centrality(x, measures = "hubbell", hubbell_weight = hubbell_weight, ...)
  stats::setNames(df$hubbell, df$node)
}


#' Information Centrality (Stephenson-Zelen)
#'
#' Information centrality (Stephenson & Zelen 1989) measures a node's
#' importance in terms of the "information" contained in all paths (not only
#' shortest) passing through it. Defined via the inverse of a Laplacian-like
#' matrix, yielding per-node
#' \eqn{IC_i = 1 / (C_{ii} + (\mathrm{tr}(C) - 2 R_i) / n)} where
#' \eqn{C = A^{-1}} and \eqn{R_i} is the row sum of \eqn{C}.
#'
#' Bit-exact match against \code{sna::infocent} on connected undirected
#' graphs (cograph mirrors sna's exact construction and call sequence).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of information centrality values.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_current_flow_closeness}}.
#' @references
#' Stephenson, K., & Zelen, M. (1989). Rethinking centrality: Methods and
#' examples. \emph{Social Networks}, 11(1), 1-37.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,1, 0,1,1,0), 4, 4)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:4]
#' centrality_information(adj)
centrality_information <- function(x, ...) {
  df <- centrality(x, measures = "information", ...)
  stats::setNames(df$information, df$node)
}


#' Pairwise Disconnectivity (Potapov et al. 2008)
#'
#' For a directed network, \code{pairwisedis(v)} is the fraction of ordered
#' reachable pairs \eqn{(s, t)} that become unreachable when node \eqn{v} is
#' removed:
#' \deqn{PD(v) = (|P(G)| - |P(G - v)|) / |P(G)|}
#' where \eqn{|P(G)|} is the number of ordered pairs \eqn{(s, t), s \ne t}
#' with a directed path from \eqn{s} to \eqn{t}.
#'
#' Bit-exact match against \code{centiserve::pairwisedis} on directed
#' graphs. Requires the input to be directed; returns \code{NA} with a
#' warning on undirected inputs.
#'
#' @param x Directed network input (matrix, igraph, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of pairwise disconnectivity values in \eqn{[0, 1]}.
#'
#' @seealso \code{\link{centrality}}, \code{\link{robustness}}.
#' @references
#' Potapov, A. P., Voss, N., Sasse, N., & Wingender, E. (2008). Topology of
#' mammalian transcription networks. \emph{Genome Informatics}, 18, 193-204.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_pairwisedis(adj)
centrality_pairwisedis <- function(x, ...) {
  df <- centrality(x, measures = "pairwisedis", ...)
  stats::setNames(df$pairwisedis, df$node)
}


#' Local Reaching Centrality (Mones, Vicsek & Vicsek 2012)
#'
#' Local reaching centrality measures how much of the network is reachable
#' from a node.
#'
#' \itemize{
#'   \item Directed unweighted: \eqn{LRC(v) = |\{u : u \ne v, v \to u\}| / (N - 1)}.
#'   \item Undirected unweighted: average of \eqn{1/d(v, u)} over all
#'     \eqn{u \ne v}, divided by \eqn{N - 1}. Numerically equal to
#'     \code{igraph::harmonic_centrality(normalized = TRUE)}.
#'   \item Weighted: NetworkX convention, where edge weights are interpreted
#'     as strengths and path length is \eqn{\sum_e (\mathrm{total\_weight} / w_e)}.
#'     Per-path score is the mean of original edge weights along the shortest
#'     path.
#' }
#'
#' Bit-exact match against \code{networkx.local_reaching_centrality} across
#' all three branches. Bit-exact match against
#' \code{igraph::harmonic_centrality(normalized = TRUE)} for the undirected
#' unweighted branch. See \code{\link{reaching_global}} for the graph-level
#' hierarchy measure derived from per-node LRC.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of local reaching centrality values.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_harmonic}},
#'   \code{\link{reaching_global}}.
#' @references
#' Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for complex
#' networks. \emph{PLoS ONE}, 7(3), e33799.
#'
#' @export
#' @examples
#' # Directed path A -> B -> C
#' adj <- matrix(c(0,1,0, 0,0,1, 0,0,0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_reaching_local(adj, mode = "out")
centrality_reaching_local <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "reaching_local", mode = mode, ...)
  col <- paste0("reaching_local_", mode)
  stats::setNames(df[[col]], df$node)
}


#' Global Reaching Centrality (Mones, Vicsek & Vicsek 2012)
#'
#' A graph-level hierarchy measure computed from per-node local reaching
#' centralities:
#' \deqn{GRC(G) = \frac{1}{N - 1} \sum_v \left( \max_u LRC(u) - LRC(v) \right)}
#'
#' Values close to 0 indicate a flat network (all nodes reach equal
#' proportions of the graph); values close to 1 indicate strong hierarchical
#' structure. Matches \code{networkx.global_reaching_centrality} exactly.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param mode For directed networks: \code{"all"} (default), \code{"in"}, or
#'   \code{"out"}.
#' @param ... Additional arguments passed to \code{\link{centrality_reaching_local}}.
#'
#' @return A single numeric value in \eqn{[0, 1]}.
#'
#' @seealso \code{\link{centrality_reaching_local}}, \code{\link{summarize_network}}.
#' @references
#' Mones, E., Vicsek, L., & Vicsek, T. (2012). Hierarchy measure for complex
#' networks. \emph{PLoS ONE}, 7(3), e33799.
#'
#' @export
#' @examples
#' # Star graph: highly hierarchical (directed out from center)
#' adj <- matrix(0, 5, 5)
#' adj[1, 2:5] <- 1
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' reaching_global(adj, mode = "out")
reaching_global <- function(x, mode = "all", ...) {
  lrc <- centrality_reaching_local(x, mode = mode, ...)
  n <- length(lrc)
  if (n <= 1) return(0)
  max_lrc <- max(lrc, na.rm = TRUE)
  sum(max_lrc - lrc, na.rm = TRUE) / (n - 1)
}


# ---------------------------------------------------------------------------
# Batch 4 wrappers: directed prestige family (Wasserman-Faust / sna lineage).
# ---------------------------------------------------------------------------

#' Domain Prestige
#'
#' Directed-graph prestige measure: for each node \eqn{v}, the number of
#' other nodes that can reach \eqn{v} via a directed path.
#' \deqn{\mathrm{domain}(v) = |\{u \ne v : u \to^* v\}|}
#'
#' Bit-exact match against \code{sna::prestige(cmode = "domain")}.
#' Directed-only; returns \code{NA} with a warning on undirected input.
#'
#' @param x Directed network input (matrix, igraph, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of domain prestige values in
#'   \eqn{\{0, 1, \ldots, N - 1\}}.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_reaching_local}}
#'   for the dual "out-reachability" measure, \code{\link{centrality_pairwisedis}}
#'   for a related reachability-based directed measure.
#' @references
#' Wasserman, S., & Faust, K. (1994). \emph{Social Network Analysis: Methods
#' and Applications}. Cambridge University Press.
#'
#' @export
#' @examples
#' # Directed 3-cycle: every node reaches every other node
#' adj <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_prestige_domain(adj)
centrality_prestige_domain <- function(x, ...) {
  df <- centrality(x, measures = "prestige_domain", ...)
  stats::setNames(df$prestige_domain, df$node)
}


#' Domain Proximity Prestige
#'
#' Distance-weighted variant of domain prestige. For each directed node
#' \eqn{v}:
#' \deqn{PD(v) = R_v^2 / (D_v \cdot (n - 1))}
#' where \eqn{R_v} is the number of other nodes that reach \eqn{v}, and
#' \eqn{D_v} is the sum of geodesic distances from those reachers to
#' \eqn{v}. A node that is reachable quickly from many others scores high;
#' unreachable nodes score 0.
#'
#' Bit-exact match against \code{sna::prestige(cmode = "domain.proximity")}
#' on strongly connected directed graphs. Directed-only; returns \code{NA}
#' with a warning on undirected input.
#'
#' @section Divergence from sna on disconnected graphs:
#' sna's formula computes \code{(counts > 0) * gdist} element-wise and then
#' sums to get the denominator. For any pair where \code{gdist = Inf}
#' (unreachable), R evaluates \code{FALSE * Inf = NaN}, so the entire
#' denominator becomes \code{NaN} and sna zeros every node via
#' \code{p[is.nan(p)] <- 0}. cograph masks with \code{is.finite()} before
#' summing, producing mathematically correct values on any directed graph,
#' including those with disconnected components.
#'
#' @param x Directed network input (matrix, igraph, cograph_network, tna object).
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named numeric vector of domain proximity prestige values in
#'   \eqn{[0, 1]}.
#'
#' @seealso \code{\link{centrality}}, \code{\link{centrality_prestige_domain}}
#'   for the unweighted count, \code{\link{centrality_reaching_local}}
#'   for the dual out-reachability measure.
#' @references
#' Wasserman, S., & Faust, K. (1994). \emph{Social Network Analysis: Methods
#' and Applications}. Cambridge University Press.
#'
#' @export
#' @examples
#' # Directed 3-cycle: each node is reached by both others at distance 1 and 2
#' adj <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality_prestige_domain_proximity(adj)
centrality_prestige_domain_proximity <- function(x, ...) {
  df <- centrality(x, measures = "prestige_domain_proximity", ...)
  stats::setNames(df$prestige_domain_proximity, df$node)
}


# ---------------------------------------------------------------------------
# Batch 5 wrappers: Gould-Fernandez brokerage (5 roles).
# ---------------------------------------------------------------------------

#' Gould-Fernandez Brokerage — Coordinator Role
#'
#' Coordinator brokerage (w_I): count of open directed 2-paths
#' \eqn{A \to V \to A} passing through node \eqn{V}, where all three nodes
#' belong to \eqn{V}'s group. The broker mediates contact between two
#' in-group members.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "w_I"]}. Counts
#' OPEN 2-paths only — those where no direct edge from \code{a} to \code{c}
#' exists. Directed-only; returns \code{NA} with a warning on undirected input.
#'
#' @param x Directed network input (matrix, igraph, cograph_network, tna object).
#' @param membership Integer or character vector of group assignments, length
#'   equal to the number of nodes. Required.
#' @param ... Additional arguments passed to \code{\link{centrality}}.
#'
#' @return Named integer vector of coordinator role counts.
#'
#' @seealso \code{\link{centrality}},
#'   \code{\link{centrality_brokerage_itinerant}},
#'   \code{\link{centrality_brokerage_representative}},
#'   \code{\link{centrality_brokerage_gatekeeper}},
#'   \code{\link{centrality_brokerage_liaison}}.
#' @references
#' Gould, R. V., & Fernandez, R. M. (1989). Structures of mediation: A
#' formal approach to brokerage in transaction networks.
#' \emph{Sociological Methodology}, 19, 89-126.
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_coordinator(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_coordinator <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_coordinator",
                   membership = membership, ...)
  stats::setNames(df$brokerage_coordinator, df$node)
}

#' Gould-Fernandez Brokerage — Itinerant (Consultant) Role
#'
#' Itinerant brokerage (w_O): count of open directed 2-paths
#' \eqn{A \to V \to A} where the two endpoints are in the same group but
#' the broker \eqn{V} is in a different group. The broker mediates within
#' another group as an outsider.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "w_O"]}.
#' Directed-only.
#'
#' @inheritParams centrality_brokerage_coordinator
#' @return Named integer vector of itinerant role counts.
#' @seealso \code{\link{centrality_brokerage_coordinator}}.
#' @references Gould & Fernandez (1989).
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_itinerant(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_itinerant <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_itinerant",
                   membership = membership, ...)
  stats::setNames(df$brokerage_itinerant, df$node)
}

#' Gould-Fernandez Brokerage — Representative Role
#'
#' Representative brokerage (b_IO): count of open directed 2-paths
#' \eqn{A \to V \to B} where \eqn{A} and \eqn{V} are in the same group
#' and \eqn{B} is in a different group. The broker represents their group
#' outward.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "b_IO"]}.
#' Directed-only.
#'
#' @inheritParams centrality_brokerage_coordinator
#' @return Named integer vector of representative role counts.
#' @seealso \code{\link{centrality_brokerage_coordinator}}.
#' @references Gould & Fernandez (1989).
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_representative(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_representative <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_representative",
                   membership = membership, ...)
  stats::setNames(df$brokerage_representative, df$node)
}

#' Gould-Fernandez Brokerage — Gatekeeper Role
#'
#' Gatekeeper brokerage (b_OI): count of open directed 2-paths
#' \eqn{A \to V \to B} where \eqn{V} and \eqn{B} are in the same group
#' and \eqn{A} is in a different group. The broker acts as a gate letting
#' in-group members receive contact from outside.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "b_OI"]}.
#' Directed-only.
#'
#' @inheritParams centrality_brokerage_coordinator
#' @return Named integer vector of gatekeeper role counts.
#' @seealso \code{\link{centrality_brokerage_coordinator}}.
#' @references Gould & Fernandez (1989).
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_gatekeeper(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_gatekeeper <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_gatekeeper",
                   membership = membership, ...)
  stats::setNames(df$brokerage_gatekeeper, df$node)
}

#' Gould-Fernandez Brokerage — Liaison Role
#'
#' Liaison brokerage (b_O): count of open directed 2-paths
#' \eqn{A \to V \to B} where all three nodes belong to different groups.
#' The broker mediates between two groups to neither of which they belong.
#'
#' Bit-exact match against \code{sna::brokerage$raw.nli[, "b_O"]}.
#' Directed-only.
#'
#' @inheritParams centrality_brokerage_coordinator
#' @return Named integer vector of liaison role counts.
#' @seealso \code{\link{centrality_brokerage_coordinator}}.
#' @references Gould & Fernandez (1989).
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' centrality_brokerage_liaison(adj, membership = c(1, 1, 2, 2))
centrality_brokerage_liaison <- function(x, membership = NULL, ...) {
  df <- centrality(x, measures = "brokerage_liaison",
                   membership = membership, ...)
  stats::setNames(df$brokerage_liaison, df$node)
}


#' Calculate Edge Centrality Measures
#'
#' Computes centrality measures for edges in a network and returns a tidy
#' data frame. Unlike node centrality, these measures describe edge importance.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object)
#' @param measures Which measures to calculate. Default "all" calculates all
#'   available edge measures. Options: "betweenness", "weight", "overlap",
#'   "simmelian", "reciprocity".
#' @param weighted Logical. Use edge weights if available. Default TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param cutoff Maximum path length for betweenness. Default -1 (no limit).
#' @param invert_weights Logical or NULL. Invert weights for path-based measures?
#'   Default NULL (auto-detect: TRUE for tna objects, FALSE otherwise).
#' @param alpha Numeric. Exponent for weight inversion. Default 1.
#' @param digits Integer or NULL. Round numeric columns. Default NULL.
#' @param sort_by Character or NULL. Column to sort by (descending). Default NULL.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A data frame with columns \code{from}, \code{to}, and one column
#'   per requested measure.
#'
#' @details
#' Edge measures available:
#' \describe{
#'   \item{betweenness}{Number of shortest paths passing through the edge.}
#'   \item{weight}{Original edge weight.}
#'   \item{overlap}{Jaccard neighborhood overlap of edge endpoints.}
#'   \item{simmelian}{Number of triangles the edge participates in.}
#'   \item{reciprocity}{Whether the reverse edge exists (directed only).
#'     Adds columns: \code{reciprocated}, \code{reverse_weight},
#'     \code{weight_ratio}.}
#' }
#'
#' @export
#' @examples
#' # Create test network
#' mat <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,0, 0,1,0,0), 4, 4)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
#'
#' # All edge measures
#' edge_centrality(mat)
#'
#' # Just betweenness
#' edge_centrality(mat, measures = "betweenness")
#'
#' # Sort by betweenness to find bridge edges
#' edge_centrality(mat, sort_by = "betweenness")
edge_centrality <- function(x, measures = "all",
                            weighted = TRUE, directed = NULL,
                            cutoff = -1, invert_weights = NULL, alpha = 1,
                            digits = NULL, sort_by = NULL, ...) {

  # Auto-detect invert_weights for tna objects
 is_tna_input <- inherits(x, c("tna", "group_tna", "ctna", "ftna", "atna",
                                 "group_ctna", "group_ftna", "group_atna"))
  if (is.null(invert_weights)) {
    invert_weights <- is_tna_input
  }

  # Convert to igraph
  g <- to_igraph(x, directed = directed, ...)
  directed <- igraph::is_directed(g)

  # Get edge list
  edges <- igraph::as_data_frame(g, what = "edges")

  # Build result data frame
  result <- data.frame(
    from = edges$from,
    to = edges$to,
    stringsAsFactors = FALSE
  )

  # Available measures
  all_measures <- c("betweenness", "weight", "overlap", "simmelian",
                    "reciprocity")

  # Resolve measures
 if (identical(measures, "all")) {
    # reciprocity only for directed
    measures <- if (directed) all_measures else
      setdiff(all_measures, "reciprocity")
  } else {
    invalid <- setdiff(measures, all_measures)
    if (length(invalid) > 0) {
      stop("Unknown edge measures: ", paste(invalid, collapse = ", "),
           "\nAvailable: ", paste(all_measures, collapse = ", "), call. = FALSE)
    }
  }

  # Get weights
  weights <- if (weighted && !is.null(igraph::E(g)$weight)) {
    igraph::E(g)$weight
  } else {
    NULL
  }

  # Add weight column if requested
  if ("weight" %in% measures) {
    result$weight <- if (!is.null(weights)) weights else rep(1, nrow(result))
  }

  # Calculate edge betweenness
  if ("betweenness" %in% measures) {
    # Handle weight inversion for path-based measure
    bet_weights <- weights
    if (!is.null(weights) && invert_weights) {
      bet_weights <- 1 / (weights ^ alpha)
      bet_weights[!is.finite(bet_weights)] <- .Machine$double.xmax
      reason <- if (is_tna_input) "tna object detected" else "invert_weights=TRUE"
      message("Note: Weights inverted (1/w^", alpha, ") for edge betweenness (",
              reason, "). Higher weights = shorter paths.")
    }

    result$betweenness <- igraph::edge_betweenness(
      g, weights = bet_weights, directed = directed, cutoff = cutoff
    )
  }

  # Overlap and simmelian share neighbor computation — do it once
  needs_neighbors <- any(c("overlap", "simmelian") %in% measures)
  if (needs_neighbors) {
    adj_list <- igraph::as_adj_list(g, mode = "all")
    el_idx <- igraph::as_edgelist(g, names = FALSE)
    ne <- nrow(el_idx)

    # Compute shared + union neighbor counts per edge (single pass)
    neighbor_stats <- vapply(seq_len(ne), function(ei) {
      u <- el_idx[ei, 1]; v <- el_idx[ei, 2]
      n_u <- setdiff(as.integer(adj_list[[u]]), c(u, v))
      n_v <- setdiff(as.integer(adj_list[[v]]), c(u, v))
      sh <- length(intersect(n_u, n_v))
      un <- length(union(n_u, n_v))
      c(sh, un)
    }, numeric(2))

    if ("overlap" %in% measures) {
      result$overlap <- ifelse(neighbor_stats[2, ] == 0L, 0,
                               neighbor_stats[1, ] / neighbor_stats[2, ])
      result$shared_neighbors <- as.integer(neighbor_stats[1, ])
    }
    if ("simmelian" %in% measures) {
      result$triangles <- as.integer(neighbor_stats[1, ])
    }
  }

  # Edge reciprocity (directed only)
  if ("reciprocity" %in% measures && !directed) {
    warning("Reciprocity skipped: only meaningful for directed networks.",
            call. = FALSE)
  }
  if ("reciprocity" %in% measures && directed) {
    edge_keys <- paste(edges$from, edges$to, sep = "\t")
    w_vec <- if (!is.null(weights)) weights else rep(1, nrow(result))
    wt_map <- stats::setNames(w_vec, edge_keys)
    rev_keys <- paste(edges$to, edges$from, sep = "\t")
    rev_wts <- unname(wt_map[rev_keys])
    result$reciprocated <- !is.na(rev_wts)
    result$reverse_weight <- ifelse(result$reciprocated, rev_wts, NA_real_)
    result$weight_ratio <- ifelse(result$reciprocated, rev_wts / w_vec, NA_real_)
  }

  # Round if requested
  if (!is.null(digits)) {
    numeric_cols <- sapply(result, is.numeric)
    result[numeric_cols] <- lapply(result[numeric_cols], round, digits = digits)
  }

  # Sort if requested
  if (!is.null(sort_by)) {
    if (!sort_by %in% names(result)) {
      stop("sort_by column '", sort_by, "' not found in results", call. = FALSE)
    }
    result <- result[order(result[[sort_by]], decreasing = TRUE), ]
    rownames(result) <- NULL
  }

  result
}

#' @rdname edge_centrality
#' @return Named numeric vector of edge betweenness values (named by
#'   \code{"from->to"}).
#' @export
#' @examples
#' mat <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,0, 0,1,0,0), 4, 4)
#' rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
#' edge_betweenness(mat)
edge_betweenness <- function(x, ...) {
  df <- edge_centrality(x, measures = "betweenness", ...)
  stats::setNames(df$betweenness, paste(df$from, df$to, sep = "->"))
}
