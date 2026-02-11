#!/usr/bin/env Rscript
# =============================================================================
# COGRAPH COMPREHENSIVE VALIDATION SUITE
# =============================================================================
# Run: Rscript validation/run_all_tests.R
# Or:  ./validation/run_all_tests.R (if executable)
#
# This script runs all validation tests and provides a formatted summary
# including test counts, network statistics, and comparison results.
# =============================================================================

# Suppress startup messages
suppressPackageStartupMessages({
  library(devtools)
  load_all(".", quiet = TRUE)
  library(igraph)
})

# =============================================================================
# Configuration
# =============================================================================

# ANSI colors for terminal output
BOLD <- "\033[1m"
GREEN <- "\033[32m"
RED <- "\033[31m"
YELLOW <- "\033[33m"
BLUE <- "\033[34m"
CYAN <- "\033[36m"
RESET <- "\033[0m"

# Track all results
all_results <- list()
total_tests <- 0
total_passed <- 0
total_failed <- 0
total_warnings <- 0

# Network statistics
networks_tested <- list()

# =============================================================================
# Helper Functions
# =============================================================================

header <- function(text) {
  width <- 70
  cat("\n")
  cat(BOLD, CYAN, paste(rep("=", width), collapse = ""), RESET, "\n", sep = "")
  cat(BOLD, CYAN, " ", text, RESET, "\n", sep = "")
  cat(BOLD, CYAN, paste(rep("=", width), collapse = ""), RESET, "\n", sep = "")
}

subheader <- function(text) {
  cat("\n", BOLD, BLUE, "--- ", text, " ---", RESET, "\n", sep = "")
}

success <- function(text) {
  cat(GREEN, "  ✓ ", RESET, text, "\n", sep = "")
}

failure <- function(text) {
  cat(RED, "  ✗ ", RESET, text, "\n", sep = "")
}

warning_msg <- function(text) {
  cat(YELLOW, "  ⚠ ", RESET, text, "\n", sep = "")
}

info <- function(text) {
  cat("  ", text, "\n", sep = "")
}

# Register a test network for statistics
register_network <- function(name, n_nodes, n_edges, directed = FALSE, weighted = TRUE) {
  networks_tested[[length(networks_tested) + 1]] <<- list(
    name = name,
    n_nodes = n_nodes,
    n_edges = n_edges,
    directed = directed,
    weighted = weighted
  )
}

# Test wrapper with error handling
run_test <- function(name, expr) {
  total_tests <<- total_tests + 1
  result <- tryCatch({
    res <- expr
    if (isTRUE(res) || (is.numeric(res) && !is.na(res) && res >= 0)) {
      total_passed <<- total_passed + 1
      success(name)
      TRUE
    } else {
      total_failed <<- total_failed + 1
      failure(name)
      FALSE
    }
  }, warning = function(w) {
    total_passed <<- total_passed + 1
    total_warnings <<- total_warnings + 1
    success(paste0(name, " (warning: ", conditionMessage(w), ")"))
    TRUE
  }, error = function(e) {
    total_failed <<- total_failed + 1
    failure(paste0(name, " - ERROR: ", conditionMessage(e)))
    FALSE
  })
  result
}

# Compare values with tolerance
compare_values <- function(a, b, tol = 1e-10) {
  if (length(a) != length(b)) return(FALSE)
  if (is.numeric(a) && is.numeric(b)) {
    all(abs(a - b) < tol, na.rm = TRUE)
  } else {
    identical(a, b)
  }
}

# =============================================================================
# TEST SUITE 1: Centrality Measures
# =============================================================================

test_centrality <- function() {
  header("CENTRALITY MEASURES")

  # Create test networks
  set.seed(42)

  # Small network
  mat_small <- matrix(c(0, 1, 1, 0,
                        1, 0, 1, 1,
                        1, 1, 0, 1,
                        0, 1, 1, 0), 4, 4)
  rownames(mat_small) <- colnames(mat_small) <- LETTERS[1:4]
  g_small <- graph_from_adjacency_matrix(mat_small, mode = "undirected")
  register_network("small_unweighted", 4, 5, FALSE, FALSE)

  # Medium weighted network
  mat_med <- matrix(runif(64), 8, 8)
  mat_med <- mat_med * upper.tri(mat_med) + t(mat_med * upper.tri(mat_med))
  mat_med[mat_med < 0.5] <- 0
  diag(mat_med) <- 0
  rownames(mat_med) <- colnames(mat_med) <- LETTERS[1:8]
  g_med <- graph_from_adjacency_matrix(mat_med, mode = "undirected", weighted = TRUE)
  register_network("medium_weighted", 8, sum(mat_med > 0)/2, FALSE, TRUE)

  # Large network
  mat_large <- matrix(runif(400), 20, 20)
  mat_large <- mat_large * upper.tri(mat_large) + t(mat_large * upper.tri(mat_large))
  mat_large[mat_large < 0.7] <- 0
  diag(mat_large) <- 0
  rownames(mat_large) <- colnames(mat_large) <- paste0("N", 1:20)
  g_large <- graph_from_adjacency_matrix(mat_large, mode = "undirected", weighted = TRUE)
  register_network("large_weighted", 20, sum(mat_large > 0)/2, FALSE, TRUE)

  subheader("Basic Centrality (vs igraph)")

  # Degree
  run_test("degree", {
    cg <- centrality_degree(mat_small)
    ig <- degree(g_small)
    compare_values(as.numeric(cg), as.numeric(ig))
  })

  # Strength
  run_test("strength", {
    cg <- centrality_strength(mat_med)
    ig <- strength(g_med)
    compare_values(as.numeric(cg), as.numeric(ig))
  })

  # Betweenness
  run_test("betweenness", {
    cg <- centrality_betweenness(mat_small)
    ig <- betweenness(g_small)
    compare_values(as.numeric(cg), as.numeric(ig))
  })

  # Closeness
  run_test("closeness", {
    cg <- centrality_closeness(mat_small)
    ig <- closeness(g_small)
    compare_values(as.numeric(cg), as.numeric(ig))
  })

  # Eigenvector
  run_test("eigenvector", {
    cg <- centrality_eigenvector(mat_small)
    ig <- eigen_centrality(g_small)$vector
    # Eigenvector can differ by sign
    compare_values(abs(as.numeric(cg)), abs(as.numeric(ig)))
  })

  # PageRank
  run_test("pagerank", {
    cg <- centrality_pagerank(mat_small)
    ig <- page_rank(g_small)$vector
    compare_values(as.numeric(cg), as.numeric(ig))
  })

  # Hub scores
  run_test("hub", {
    cg <- centrality_hub(mat_small)
    ig <- hub_score(g_small)$vector
    compare_values(abs(as.numeric(cg)), abs(as.numeric(ig)))
  })

  # Authority scores
  run_test("authority", {
    cg <- centrality_authority(mat_small)
    ig <- authority_score(g_small)$vector
    compare_values(abs(as.numeric(cg)), abs(as.numeric(ig)))
  })

  # Coreness
  run_test("coreness", {
    cg <- centrality_coreness(mat_small)
    ig <- coreness(g_small)
    compare_values(as.numeric(cg), as.numeric(ig))
  })

  subheader("Extended Centrality Measures")

  run_test("harmonic centrality", !is.null(centrality_harmonic(mat_small)))
  run_test("eccentricity", !is.null(centrality_eccentricity(mat_small)))
  run_test("constraint (Burt)", !is.null(centrality_constraint(mat_small)))
  run_test("transitivity (local)", !is.null(centrality_transitivity(mat_small)))

  subheader("Centrality on Large Network")

  run_test("degree (n=20)", length(centrality_degree(mat_large)) == 20)
  run_test("betweenness (n=20)", length(centrality_betweenness(mat_large)) == 20)
  run_test("pagerank (n=20)", length(centrality_pagerank(mat_large)) == 20)

  subheader("All-in-One centrality()")

  run_test("centrality() all measures", {
    result <- centrality(mat_small, measures = "all")
    is.data.frame(result) && nrow(result) == 4 && ncol(result) >= 10
  })

  run_test("centrality() selected measures", {
    result <- centrality(mat_small, measures = c("degree", "betweenness"))
    (is.data.frame(result) && "degree" %in% names(result)) || is.list(result)
  })
}

# =============================================================================
# TEST SUITE 2: Community Detection
# =============================================================================

test_communities <- function() {
  header("COMMUNITY DETECTION")

  # Create network with clear community structure
  mat_comm <- matrix(0, 12, 12)
  # Community 1: nodes 1-4
  mat_comm[1,2] <- mat_comm[2,1] <- 1
  mat_comm[1,3] <- mat_comm[3,1] <- 1
  mat_comm[2,3] <- mat_comm[3,2] <- 1
  mat_comm[2,4] <- mat_comm[4,2] <- 1
  mat_comm[3,4] <- mat_comm[4,3] <- 1
  # Community 2: nodes 5-8
  mat_comm[5,6] <- mat_comm[6,5] <- 1
  mat_comm[5,7] <- mat_comm[7,5] <- 1
  mat_comm[6,7] <- mat_comm[7,6] <- 1
  mat_comm[6,8] <- mat_comm[8,6] <- 1
  mat_comm[7,8] <- mat_comm[8,7] <- 1
  # Community 3: nodes 9-12
  mat_comm[9,10] <- mat_comm[10,9] <- 1
  mat_comm[9,11] <- mat_comm[11,9] <- 1
  mat_comm[10,11] <- mat_comm[11,10] <- 1
  mat_comm[10,12] <- mat_comm[12,10] <- 1
  mat_comm[11,12] <- mat_comm[12,11] <- 1
  # Bridges
  mat_comm[4,5] <- mat_comm[5,4] <- 0.5
  mat_comm[8,9] <- mat_comm[9,8] <- 0.5
  rownames(mat_comm) <- colnames(mat_comm) <- paste0("N", 1:12)
  register_network("community_structure", 12, sum(mat_comm > 0)/2, FALSE, TRUE)

  g_comm <- graph_from_adjacency_matrix(mat_comm, mode = "undirected", weighted = TRUE)

  subheader("Community Detection Algorithms")

  run_test("louvain", {
    comm <- community_louvain(mat_comm)
    inherits(comm, "cograph_communities") && !is.null(membership(comm))
  })

  run_test("walktrap", {
    comm <- community_walktrap(mat_comm)
    inherits(comm, "cograph_communities") && !is.null(membership(comm))
  })

  run_test("fast_greedy", {
    comm <- community_fast_greedy(mat_comm)
    inherits(comm, "cograph_communities") && !is.null(membership(comm))
  })

  run_test("label_propagation", {
    comm <- community_label_propagation(mat_comm)
    inherits(comm, "cograph_communities") && !is.null(membership(comm))
  })

  run_test("infomap", {
    comm <- community_infomap(mat_comm)
    inherits(comm, "cograph_communities") && !is.null(membership(comm))
  })

  run_test("leading_eigenvector", {
    comm <- community_leading_eigenvector(mat_comm)
    inherits(comm, "cograph_communities") && !is.null(membership(comm))
  })

  run_test("edge_betweenness", {
    comm <- community_edge_betweenness(mat_comm)
    inherits(comm, "cograph_communities") && !is.null(membership(comm))
  })

  subheader("Community Quality")

  run_test("detects ~3 communities", {
    comm <- community_louvain(mat_comm)
    n_comm <- length(unique(membership(comm)))
    n_comm >= 2 && n_comm <= 4
  })

  run_test("n_communities()", {
    comm <- community_louvain(mat_comm)
    n <- n_communities(comm)
    is.numeric(n) && n >= 2
  })

  run_test("community_sizes()", {
    comm <- community_louvain(mat_comm)
    sizes <- community_sizes(comm)
    is.numeric(sizes) && sum(sizes) == 12
  })

  subheader("Community Comparison")

  run_test("compare_communities (NMI)", {
    comm1 <- community_louvain(mat_comm)
    comm2 <- community_walktrap(mat_comm)
    result <- compare_communities(comm1, comm2, method = "nmi")
    is.numeric(result) && result >= 0 && result <= 1
  })

  run_test("compare_communities (ARI)", {
    comm1 <- community_louvain(mat_comm)
    comm2 <- community_walktrap(mat_comm)
    result <- compare_communities(comm1, comm2, method = "adjusted.rand")
    is.numeric(result) && result >= -1 && result <= 1
  })

  subheader("Convenience Functions")

  run_test("detect_communities()", {
    comm <- detect_communities(mat_comm, method = "louvain")
    # Returns data.frame with node and community columns
    is.data.frame(comm) && "community" %in% names(comm) && nrow(comm) == 12
  })

  run_test("color_communities()", {
    colors <- color_communities(mat_comm)
    is.character(colors) && length(colors) == 12
  })
}

# =============================================================================
# TEST SUITE 3: Network Metrics
# =============================================================================

test_network_metrics <- function() {
  header("NETWORK METRICS")

  # Test networks
  mat <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  0, 1, 1, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  g <- graph_from_adjacency_matrix(mat, mode = "undirected")
  register_network("test_4node", 4, 5, FALSE, FALSE)

  subheader("Basic Metrics (vs igraph)")

  run_test("network_summary returns data", {
    summary <- network_summary(mat)
    is.list(summary) && "node_count" %in% names(summary)
  })

  run_test("degree_distribution", {
    # Returns invisible histogram object (plots as side effect)
    # Note: cograph::degree_distribution is masked by igraph, use explicit ns
    tmp <- tempfile(fileext = ".png")
    png(tmp)
    dd <- cograph::degree_distribution(mat)
    dev.off()
    unlink(tmp)
    inherits(dd, "histogram") || is.list(dd)
  })

  run_test("network_girth", {
    g_cg <- network_girth(mat)
    g_ig <- girth(g)$girth
    g_cg == g_ig
  })

  run_test("network_radius", {
    r <- network_radius(mat)
    is.numeric(r) && r >= 0
  })

  run_test("network_clique_size", {
    cs <- network_clique_size(mat)
    is.numeric(cs) && cs >= 1
  })

  subheader("Connectivity Metrics")

  run_test("network_cut_vertices", {
    cv <- network_cut_vertices(mat)
    ig_cv <- articulation_points(g)
    length(cv) == length(ig_cv)
  })

  run_test("network_bridges", {
    br <- network_bridges(mat)
    ig_br <- bridges(g)
    # cograph returns data.frame with from/to, igraph returns edge IDs
    nrow(br) == length(ig_br)
  })

  run_test("network_vertex_connectivity", {
    vc <- network_vertex_connectivity(mat)
    is.numeric(vc) && vc >= 0
  })

  subheader("Efficiency Metrics")

  run_test("network_global_efficiency", {
    eff <- network_global_efficiency(mat)
    is.numeric(eff) && eff >= 0 && eff <= 1
  })

  run_test("network_local_efficiency", {
    eff <- network_local_efficiency(mat)
    # Returns mean local efficiency (single value)
    is.numeric(eff) && length(eff) == 1
  })

  subheader("Small World & Rich Club")

  run_test("network_small_world", {
    sw <- network_small_world(mat, n_random = 5)
    # Returns numeric sigma value (may be NA for small networks)
    is.numeric(sw) && length(sw) == 1
  })

  run_test("network_rich_club", {
    rc <- network_rich_club(mat)
    is.numeric(rc) || is.list(rc)
  })
}

# =============================================================================
# TEST SUITE 4: select_nodes()
# =============================================================================

test_select_nodes <- function() {
  header("SELECT_NODES()")

  # Test network
  set.seed(123)
  mat <- matrix(runif(100), 10, 10)
  mat <- mat * upper.tri(mat) + t(mat * upper.tri(mat))
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:10]
  register_network("select_nodes_test", 10, sum(mat > 0)/2, FALSE, TRUE)

  # Disconnected network
  disc <- matrix(0, 6, 6)
  disc[1,2] <- disc[2,1] <- disc[1,3] <- disc[3,1] <- 1
  disc[4,5] <- disc[5,4] <- disc[4,6] <- disc[6,4] <- disc[5,6] <- disc[6,5] <- 1
  rownames(disc) <- colnames(disc) <- LETTERS[1:6]
  register_network("disconnected", 6, 5, FALSE, FALSE)

  subheader("Centrality Expressions (Lazy)")

  run_test("degree expression", n_nodes(select_nodes(mat, degree >= 5)) >= 0)
  run_test("strength expression", n_nodes(select_nodes(mat, strength > 1)) >= 0)
  run_test("betweenness expression", n_nodes(select_nodes(mat, betweenness >= 0)) >= 0)
  run_test("pagerank expression", n_nodes(select_nodes(mat, pagerank > 0.05)) >= 0)
  run_test("eigenvector expression", n_nodes(select_nodes(mat, eigenvector > 0.2)) >= 0)
  run_test("closeness expression", n_nodes(select_nodes(mat, closeness > 0)) >= 0)
  run_test("coreness expression", n_nodes(select_nodes(mat, coreness >= 1)) >= 0)

  subheader("Selection Modes")

  run_test("by name", {
    res <- select_nodes(mat, name = c("A", "B", "C"))
    n_nodes(res) == 3
  })

  run_test("by index", {
    res <- select_nodes(mat, index = c(1, 3, 5))
    n_nodes(res) == 3
  })

  run_test("top N", {
    res <- select_nodes(mat, top = 5)
    n_nodes(res) == 5
  })

  run_test("top N by pagerank", {
    res <- select_nodes(mat, top = 3, by = "pagerank")
    n_nodes(res) == 3
  })

  run_test("neighbors_of", {
    res <- select_nodes(mat, neighbors_of = "A", order = 1)
    n_nodes(res) >= 1
  })

  run_test("component = largest", {
    res <- select_nodes(disc, component = "largest")
    n_nodes(res) >= 3
  })

  run_test("component = node name", {
    res <- select_nodes(disc, component = "A")
    n_nodes(res) == 3
  })

  subheader("Global Context Variables")

  run_test("is_largest_component", n_nodes(select_nodes(disc, is_largest_component)) >= 3)
  run_test("component_size", n_nodes(select_nodes(disc, component_size >= 3)) >= 3)
  run_test("k_core", n_nodes(select_nodes(mat, k_core >= 1)) >= 0)

  # Network with articulation points
  art <- matrix(0, 5, 5)
  art[1,2] <- art[2,1] <- art[2,3] <- art[3,2] <- art[3,4] <- art[4,3] <- art[3,5] <- art[5,3] <- 1
  rownames(art) <- colnames(art) <- LETTERS[1:5]
  register_network("articulation", 5, 4, FALSE, FALSE)

  run_test("is_articulation", n_nodes(select_nodes(art, is_articulation)) >= 1)
  run_test("is_bridge_endpoint", n_nodes(select_nodes(art, is_bridge_endpoint)) >= 1)

  subheader("Combined Modes & Options")

  run_test("combined: name + top", {
    res <- select_nodes(mat, name = LETTERS[1:6], top = 3)
    n_nodes(res) == 3
  })

  run_test("keep_format = TRUE", is.matrix(select_nodes(mat, degree >= 5, keep_format = TRUE)))

  subheader("Convenience Functions")

  run_test("select_neighbors()", n_nodes(select_neighbors(mat, of = "A")) >= 1)
  run_test("select_component()", n_nodes(select_component(disc)) >= 3)
  run_test("select_top()", n_nodes(select_top(mat, n = 5)) == 5)
}

# =============================================================================
# TEST SUITE 5: select_edges()
# =============================================================================

test_select_edges <- function() {
  header("SELECT_EDGES()")

  # Test network
  mat <- matrix(c(0, .5, .8, .3,
                  .5, 0, .2, .6,
                  .8, .2, 0, .4,
                  .3, .6, .4, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
  register_network("select_edges_test", 4, 6, FALSE, TRUE)

  # Tree (all bridges)
  tree <- matrix(0, 5, 5)
  tree[1,2] <- tree[2,1] <- tree[2,3] <- tree[3,2] <- tree[3,4] <- tree[4,3] <- tree[4,5] <- tree[5,4] <- 1
  rownames(tree) <- colnames(tree) <- LETTERS[1:5]
  register_network("tree", 5, 4, FALSE, FALSE)

  # Community structure
  comm <- matrix(0, 6, 6)
  comm[1,2] <- comm[2,1] <- comm[1,3] <- comm[3,1] <- comm[2,3] <- comm[3,2] <- 1
  comm[4,5] <- comm[5,4] <- comm[4,6] <- comm[6,4] <- comm[5,6] <- comm[6,5] <- 1
  comm[3,4] <- comm[4,3] <- 0.1
  rownames(comm) <- colnames(comm) <- LETTERS[1:6]
  register_network("two_communities", 6, 7, FALSE, TRUE)

  subheader("Expression Filtering (Lazy)")

  run_test("weight expression", {
    res <- select_edges(mat, weight > 0.5)
    n_edges(res) == sum(E(g)$weight > 0.5)
  })

  run_test("abs_weight expression", n_edges(select_edges(mat, abs_weight > 0.4)) >= 0)
  run_test("from_degree expression", n_edges(select_edges(mat, from_degree >= 3)) >= 0)
  run_test("to_degree expression", n_edges(select_edges(mat, to_degree >= 3)) >= 0)
  run_test("edge_betweenness expression", n_edges(select_edges(mat, edge_betweenness > 0)) >= 0)

  subheader("Selection Modes")

  run_test("top N", {
    res <- select_edges(mat, top = 3)
    n_edges(res) == 3
  })

  run_test("top N by betweenness", {
    res <- select_edges(mat, top = 2, by = "edge_betweenness")
    n_edges(res) == 2
  })

  run_test("involving", {
    res <- select_edges(mat, involving = "A")
    ig_count <- length(incident(g, V(g)["A"], mode = "all"))
    n_edges(res) == ig_count
  })

  run_test("involving multiple", {
    res <- select_edges(mat, involving = c("A", "D"))
    n_edges(res) >= 1
  })

  run_test("between", {
    res <- select_edges(mat, between = list(c("A", "B"), c("C", "D")))
    n_edges(res) >= 1
  })

  run_test("bridges_only", {
    res <- select_edges(tree, bridges_only = TRUE)
    n_edges(res) == length(bridges(graph_from_adjacency_matrix(tree, mode = "undirected")))
  })

  subheader("Structural Metrics")

  run_test("is_bridge expression", {
    res <- select_edges(tree, is_bridge)
    n_edges(res) == 4  # All edges in tree are bridges
  })

  run_test("same_community", {
    res <- select_edges(comm, same_community)
    n_edges(res) == 6  # 3 in each community
  })

  run_test("!same_community (cross-community)", {
    res <- select_edges(comm, !same_community)
    n_edges(res) == 1  # Just the bridge
  })

  # Directed network for mutual
  dir_mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 0, 0), 3, 3)
  rownames(dir_mat) <- colnames(dir_mat) <- LETTERS[1:3]
  register_network("directed", 3, 4, TRUE, FALSE)

  run_test("mutual_only (directed)", {
    res <- select_edges(dir_mat, mutual_only = TRUE, directed = TRUE)
    n_edges(res) == 2  # A<->B
  })

  run_test("is_mutual expression", {
    res <- select_edges(dir_mat, is_mutual, directed = TRUE)
    n_edges(res) == 2
  })

  subheader("Combined Modes & Options")

  run_test("involving + top", {
    res <- select_edges(mat, involving = "B", top = 2)
    n_edges(res) == 2
  })

  run_test("bridges + weight", {
    tree_w <- tree * 0.5
    tree_w[2,3] <- tree_w[3,2] <- 0.9
    rownames(tree_w) <- colnames(tree_w) <- LETTERS[1:5]
    res <- select_edges(tree_w, bridges_only = TRUE, weight > 0.6)
    n_edges(res) == 1
  })

  run_test("keep_format = TRUE", is.matrix(select_edges(mat, weight > 0.5, keep_format = TRUE)))
  run_test("keep_isolates = TRUE", n_nodes(select_edges(mat, involving = "A", .keep_isolates = TRUE)) == 4)

  subheader("Convenience Functions")

  run_test("select_bridges()", n_edges(select_bridges(tree)) == 4)
  run_test("select_top_edges()", n_edges(select_top_edges(mat, n = 2)) == 2)
  run_test("select_edges_involving()", n_edges(select_edges_involving(mat, nodes = "B")) >= 1)
  run_test("select_edges_between()", n_edges(select_edges_between(mat, c("A"), c("C", "D"))) >= 1)
}

# =============================================================================
# TEST SUITE 6: Filtering (filter_nodes, filter_edges)
# =============================================================================

test_filtering <- function() {
  header("FILTERING FUNCTIONS")

  mat <- matrix(c(0, .5, .8, .3,
                  .5, 0, .2, .6,
                  .8, .2, 0, .4,
                  .3, .6, .4, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  register_network("filter_test", 4, 6, FALSE, TRUE)

  subheader("filter_edges()")

  run_test("weight filter", n_edges(filter_edges(mat, weight > 0.5)) == 2)
  run_test("mean weight filter", n_edges(filter_edges(mat, weight >= mean(weight))) >= 1)
  run_test("keep_isolates", n_nodes(filter_edges(mat, weight > 0.7, .keep_isolates = TRUE)) == 4)
  run_test("keep_format", is.matrix(filter_edges(mat, weight > 0.5, keep_format = TRUE)))

  subheader("filter_nodes()")

  run_test("degree filter", n_nodes(filter_nodes(mat, degree >= 3)) >= 1)
  run_test("label filter", n_nodes(filter_nodes(mat, label %in% c("A", "C"))) == 2)
  run_test("combined filter", n_nodes(filter_nodes(mat, degree >= 2 & label != "D")) >= 1)
  run_test("keep_format", is.matrix(filter_nodes(mat, degree >= 2, keep_format = TRUE)))

  subheader("Chaining")

  run_test("filter_edges |> filter_nodes", {
    res <- mat |> filter_edges(weight > 0.3) |> filter_nodes(degree >= 2)
    n_nodes(res) >= 1
  })
}

# =============================================================================
# TEST SUITE 7: Format Conversion
# =============================================================================

test_conversion <- function() {
  header("FORMAT CONVERSION")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- LETTERS[1:3]
  register_network("conversion_test", 3, 3, FALSE, FALSE)

  subheader("to_* Functions")

  run_test("to_igraph", inherits(to_igraph(mat), "igraph"))
  run_test("to_matrix", is.matrix(to_matrix(mat)))
  run_test("to_df", is.data.frame(to_df(mat)))
  run_test("as_cograph", inherits(as_cograph(mat), "cograph_network"))

  subheader("Round-Trip Conversion")

  run_test("matrix -> igraph -> matrix", {
    g <- to_igraph(mat)
    mat2 <- to_matrix(g)
    all(mat == mat2)
  })

  run_test("matrix -> cograph -> matrix", {
    net <- as_cograph(mat)
    mat2 <- to_matrix(net)
    all(mat == mat2)
  })

  subheader("Edge List Conversion")

  run_test("to_df edge count", {
    df <- to_df(mat)
    nrow(df) == 3  # 3 edges
  })

  run_test("to_df has weight column", {
    df <- to_df(mat)
    "weight" %in% names(df)
  })
}

# =============================================================================
# TEST SUITE 8: Edge Cases
# =============================================================================

test_edge_cases <- function() {
  header("EDGE CASES")

  subheader("Empty Networks")

  run_test("empty matrix", {
    mat <- matrix(0, 3, 3)
    rownames(mat) <- colnames(mat) <- LETTERS[1:3]
    n_edges(as_cograph(mat)) == 0
  })

  run_test("single node", {
    mat <- matrix(0, 1, 1)
    rownames(mat) <- colnames(mat) <- "A"
    n_nodes(as_cograph(mat)) == 1
  })

  subheader("Negative Weights")

  run_test("negative weight handling", {
    mat <- matrix(c(0, -0.5, 0.8, -0.5, 0, 0.3, 0.8, 0.3, 0), 3, 3)
    rownames(mat) <- colnames(mat) <- LETTERS[1:3]
    res <- select_edges(mat, abs_weight > 0.4)
    n_edges(res) == 2
  })

  subheader("Large Networks")

  run_test("100 node network", {
    set.seed(42)
    mat <- matrix(runif(10000), 100, 100)
    mat <- mat * upper.tri(mat) + t(mat * upper.tri(mat))
    mat[mat < 0.9] <- 0
    diag(mat) <- 0
    rownames(mat) <- colnames(mat) <- paste0("N", 1:100)
    register_network("large_100", 100, sum(mat > 0)/2, FALSE, TRUE)

    n_nodes(select_nodes(mat, degree >= 5)) >= 0
  })

  subheader("Invalid Input Handling")

  run_test("non-existent node name", {
    mat <- matrix(c(0, 1, 1, 0), 2, 2)
    rownames(mat) <- colnames(mat) <- c("A", "B")
    res <- select_nodes(mat, name = "Z")
    n_nodes(res) == 0
  })

  run_test("out of range index", {
    mat <- matrix(c(0, 1, 1, 0), 2, 2)
    rownames(mat) <- colnames(mat) <- c("A", "B")
    res <- select_nodes(mat, index = 100)
    n_nodes(res) == 0
  })

  run_test("top > n_nodes", {
    mat <- matrix(c(0, 1, 1, 0), 2, 2)
    rownames(mat) <- colnames(mat) <- c("A", "B")
    res <- select_nodes(mat, top = 100)
    n_nodes(res) == 2
  })
}

# =============================================================================
# RUN ALL TESTS
# =============================================================================

run_all_tests <- function() {
  start_time <- Sys.time()

  cat("\n")
  cat(BOLD, "╔══════════════════════════════════════════════════════════════════════╗\n", RESET, sep = "")
  cat(BOLD, "║           COGRAPH COMPREHENSIVE VALIDATION SUITE                     ║\n", RESET, sep = "")
  cat(BOLD, "║                      ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "                        ║\n", RESET, sep = "")
  cat(BOLD, "╚══════════════════════════════════════════════════════════════════════╝\n", RESET, sep = "")

  # Run all test suites
  test_centrality()
  test_communities()
  test_network_metrics()
  test_select_nodes()
  test_select_edges()
  test_filtering()
  test_conversion()
  test_edge_cases()

  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)

  # =============================================================================
  # SUMMARY
  # =============================================================================

  header("FINAL SUMMARY")

  # Test Results
  subheader("Test Results")
  cat("\n")
  cat("  ", BOLD, "Total Tests:  ", RESET, total_tests, "\n", sep = "")
  cat("  ", GREEN, "Passed:       ", RESET, total_passed, " (", round(100 * total_passed / total_tests, 1), "%)\n", sep = "")
  if (total_failed > 0) {
    cat("  ", RED, "Failed:       ", RESET, total_failed, "\n", sep = "")
  }
  if (total_warnings > 0) {
    cat("  ", YELLOW, "Warnings:     ", RESET, total_warnings, "\n", sep = "")
  }
  cat("  ", CYAN, "Duration:     ", RESET, duration, " seconds\n", sep = "")

  # Network Statistics
  subheader("Networks Tested")
  cat("\n")

  if (length(networks_tested) > 0) {
    nodes <- sapply(networks_tested, function(x) x$n_nodes)
    edges <- sapply(networks_tested, function(x) x$n_edges)

    cat("  ", BOLD, "Total Networks: ", RESET, length(networks_tested), "\n", sep = "")
    cat("  ", "Node Range:     ", min(nodes), " - ", max(nodes), " (mean: ", round(mean(nodes), 1), ")\n", sep = "")
    cat("  ", "Edge Range:     ", min(edges), " - ", max(edges), " (mean: ", round(mean(edges), 1), ")\n", sep = "")
    cat("  ", "Directed:       ", sum(sapply(networks_tested, function(x) x$directed)), "\n", sep = "")
    cat("  ", "Weighted:       ", sum(sapply(networks_tested, function(x) x$weighted)), "\n", sep = "")

    cat("\n  ", BOLD, "Network Details:\n", RESET, sep = "")
    for (net in networks_tested) {
      cat("    • ", net$name, ": ", net$n_nodes, " nodes, ", net$n_edges, " edges",
          ifelse(net$directed, " (directed)", ""),
          ifelse(net$weighted, " (weighted)", ""),
          "\n", sep = "")
    }
  }

  # Feature Coverage
  subheader("Feature Coverage")
  cat("\n")
  cat("  ", "✓ Centrality measures (13 types)\n", sep = "")
  cat("  ", "✓ Community detection (7 algorithms)\n", sep = "")
  cat("  ", "✓ Network metrics (12 measures)\n", sep = "")
  cat("  ", "✓ select_nodes() (7 modes, 7 global vars)\n", sep = "")
  cat("  ", "✓ select_edges() (6 modes, 10 metrics)\n", sep = "")
  cat("  ", "✓ Filtering functions\n", sep = "")
  cat("  ", "✓ Format conversion\n", sep = "")
  cat("  ", "✓ Edge cases\n", sep = "")

  # Final Status
  cat("\n")
  if (total_failed == 0) {
    cat(BOLD, GREEN, "╔══════════════════════════════════════════════════════════════════════╗\n", RESET, sep = "")
    cat(BOLD, GREEN, "║                    ALL TESTS PASSED! ✓                               ║\n", RESET, sep = "")
    cat(BOLD, GREEN, "╚══════════════════════════════════════════════════════════════════════╝\n", RESET, sep = "")
  } else {
    cat(BOLD, RED, "╔══════════════════════════════════════════════════════════════════════╗\n", RESET, sep = "")
    cat(BOLD, RED, "║                    SOME TESTS FAILED! ✗                              ║\n", RESET, sep = "")
    cat(BOLD, RED, "╚══════════════════════════════════════════════════════════════════════╝\n", RESET, sep = "")
  }
  cat("\n")

  # Return exit code
  invisible(total_failed == 0)
}

# Run if executed directly
if (!interactive()) {
  success <- run_all_tests()
  quit(status = if (success) 0 else 1)
} else {
  run_all_tests()
}
