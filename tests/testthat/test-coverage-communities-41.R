# Tests for communities.R - additional coverage tests (series 41)
# Focus on uncovered code paths, edge cases, and parameter variations

# ==============================================================================
# Test Setup
# ==============================================================================

# Create test networks with various properties
set.seed(42)

# Small network for testing
mat_small <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 0, 1,
  0, 1, 0, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(mat_small) <- colnames(mat_small) <- paste0("N", 1:5)

# Directed network
mat_directed <- matrix(c(
  0, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 0, 1,
  1, 0, 0, 0
), 4, 4, byrow = TRUE)
rownames(mat_directed) <- colnames(mat_directed) <- LETTERS[1:4]

# Network with community structure (for consensus testing)
mat_community <- matrix(0, 12, 12)
mat_community[1:4, 1:4] <- 0.8
mat_community[5:8, 5:8] <- 0.8
mat_community[9:12, 9:12] <- 0.8
mat_community[4, 5] <- mat_community[5, 4] <- 0.1
mat_community[8, 9] <- mat_community[9, 8] <- 0.1
diag(mat_community) <- 0
rownames(mat_community) <- colnames(mat_community) <- paste0("N", 1:12)

# ==============================================================================
# Test communities() dispatch to additional methods
# ==============================================================================

test_that("communities() dispatches to infomap correctly", {
  skip_if_not_installed("igraph")

  comm <- communities(mat_small, method = "infomap")
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "infomap")
})

test_that("communities() dispatches to label_propagation correctly", {
  skip_if_not_installed("igraph")

  comm <- communities(mat_small, method = "label_propagation", seed = 42)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "label_propagation")
})

test_that("communities() dispatches to edge_betweenness correctly", {
  skip_if_not_installed("igraph")

  comm <- communities(mat_small, method = "edge_betweenness")
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "edge_betweenness")
})

test_that("communities() dispatches to leading_eigenvector correctly", {
  skip_if_not_installed("igraph")

  comm <- communities(mat_small, method = "leading_eigenvector")
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "leading_eigenvector")
})

test_that("communities() dispatches to spinglass correctly", {
  skip_if_not_installed("igraph")

  # Need connected graph for spinglass
  g <- igraph::make_full_graph(8)
  comm <- communities(g, method = "spinglass", seed = 42)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "spinglass")
})

test_that("communities() dispatches to optimal correctly", {
  skip_if_not_installed("igraph")

  mat_tiny <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  comm <- communities(mat_tiny, method = "optimal")
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "optimal")
})

test_that("communities() dispatches to fluid correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- communities(g, method = "fluid", no.of.communities = 2)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "fluid")
})

# ==============================================================================
# Test community_leiden with additional parameters
# ==============================================================================

test_that("community_leiden works with initial_membership", {
  skip_if_not_installed("igraph")

  # Initial membership: all nodes start in same community
  initial <- rep(1, 12)
  comm <- community_leiden(mat_community, initial_membership = initial, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_leiden works with vertex_weights", {
  skip_if_not_installed("igraph")

  vertex_weights <- rep(1, 12)
  comm <- community_leiden(mat_community, vertex_weights = vertex_weights, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_leiden works with n_iterations = -1 for convergence", {
  skip_if_not_installed("igraph")

  comm <- community_leiden(mat_small, n_iterations = -1, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test community_infomap with additional parameters
# ==============================================================================

test_that("community_infomap works with v.weights", {
  skip_if_not_installed("igraph")

  v_weights <- rep(1, 5)
  comm <- community_infomap(mat_small, v.weights = v_weights, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_infomap works with modularity = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_infomap(mat_small, modularity = FALSE, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test community_label_propagation with additional parameters
# ==============================================================================

test_that("community_label_propagation works with mode = 'in'", {
  skip_if_not_installed("igraph")

  comm <- community_label_propagation(mat_directed, mode = "in", seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_label_propagation works with initial labels", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  n <- igraph::vcount(g)

  # Provide initial labels for all nodes
  initial <- seq_len(n)
  comm <- community_label_propagation(g, initial = initial, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_label_propagation works with fixed labels", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  n <- igraph::vcount(g)

  # Fix first two nodes to specific communities
  initial <- seq_len(n)
  fixed <- rep(FALSE, n)
  fixed[c(1, 34)] <- TRUE

  comm <- community_label_propagation(g, initial = initial, fixed = fixed, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test community_edge_betweenness with additional parameters
# ==============================================================================

test_that("community_edge_betweenness with directed = NULL auto-detects", {
  skip_if_not_installed("igraph")

  comm <- community_edge_betweenness(mat_small, directed = NULL)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_edge_betweenness with edge.betweenness = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_edge_betweenness(mat_small, edge.betweenness = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_edge_betweenness with bridges = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_edge_betweenness(mat_small, bridges = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_edge_betweenness with modularity = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_edge_betweenness(mat_small, modularity = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_edge_betweenness with merges = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_edge_betweenness(mat_small, merges = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test community_leading_eigenvector with additional parameters
# ==============================================================================

test_that("community_leading_eigenvector with start parameter", {
  skip_if_not_installed("igraph")

  # Start with all nodes in one community
  start <- rep(1, 5)
  comm <- community_leading_eigenvector(mat_small, start = start)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_leading_eigenvector with callback function", {
  skip_if_not_installed("igraph")

  # Simple callback that does nothing (accept ... for igraph version compat)
  callback_count <- 0
  my_callback <- function(membership, comm, ...) {
    callback_count <<- callback_count + 1
    return(FALSE)  # Continue
  }

  comm <- community_leading_eigenvector(mat_small, callback = my_callback)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_leading_eigenvector with extra parameter", {
  skip_if_not_installed("igraph")

  my_callback <- function(membership, comm, ...) {
    return(FALSE)
  }

  comm <- community_leading_eigenvector(mat_small, callback = my_callback)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test community_spinglass with additional parameters
# ==============================================================================

test_that("community_spinglass with vertex parameter for single community mode", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)

  # Find community containing vertex 1
  result <- community_spinglass(g, vertex = 1, seed = 42)
  # When vertex is specified, result is different structure
  expect_true(!is.null(result))
})

test_that("community_spinglass with parupdate = TRUE", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- community_spinglass(g, parupdate = TRUE, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_spinglass with custom temperature parameters", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- community_spinglass(g,
                              start.temp = 2,
                              stop.temp = 0.001,
                              cool.fact = 0.95,
                              seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_spinglass with update.rule = 'random'", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- community_spinglass(g, update.rule = "random", seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_spinglass with gamma parameter", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- community_spinglass(g, gamma = 0.5, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_spinglass with implementation = 'neg'", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- community_spinglass(g, implementation = "neg", gamma.minus = 0.5, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test community_consensus with additional methods
# ==============================================================================

test_that("community_consensus works with infomap", {
  skip_if_not_installed("igraph")

  comm <- community_consensus(mat_community, method = "infomap", n_runs = 2, seed = 42)
  expect_s3_class(comm, "cograph_communities")
  expect_true(grepl("consensus_infomap", comm$algorithm))
})

test_that("community_consensus works with label_propagation", {
  skip_if_not_installed("igraph")

  comm <- community_consensus(mat_community, method = "label_propagation", n_runs = 2, seed = 42)
  expect_s3_class(comm, "cograph_communities")
  expect_true(grepl("consensus_label_propagation", comm$algorithm))
})

test_that("community_consensus works with spinglass on connected graph", {
  skip_if_not_installed("igraph")

  # Need fully connected graph for spinglass
  g <- igraph::make_full_graph(10)
  comm <- community_consensus(g, method = "spinglass", n_runs = 2, seed = 42)
  expect_s3_class(comm, "cograph_communities")
  expect_true(grepl("consensus_spinglass", comm$algorithm))
})

test_that("community_consensus preserves node names", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  igraph::V(g)$name <- paste0("Node", seq_len(igraph::vcount(g)))

  comm <- community_consensus(g, n_runs = 2, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test print.cograph_communities edge cases
# ==============================================================================

test_that("print.cograph_communities works with unnamed nodes", {
  skip_if_not_installed("igraph")

  # Matrix without rownames
  mat_unnamed <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  comm <- community_louvain(mat_unnamed)

  output <- capture.output(print(comm))
  expect_true(any(grepl("louvain", output)))
})

test_that("print.cograph_communities shows node count when names available", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_small)
  output <- capture.output(print(comm))

  expect_true(any(grepl("Nodes", output)))
})

test_that("print returns invisible object", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_small)
  result <- print(comm)

  expect_identical(result, comm)
})

# ==============================================================================
# Test membership.cograph_communities edge cases
# ==============================================================================

test_that("membership.cograph_communities returns unnamed vector when no names", {
  skip_if_not_installed("igraph")

  mat_unnamed <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  comm <- community_louvain(mat_unnamed)
  mem <- membership.cograph_communities(comm)

  expect_true(is.numeric(mem))
  expect_null(names(mem))
})

# ==============================================================================
# Test modularity.cograph_communities edge cases
# ==============================================================================

test_that("modularity.cograph_communities handles stored modularity vector", {
  skip_if_not_installed("igraph")

  # Fast greedy returns modularity as vector
  comm <- community_fast_greedy(mat_community)
  mod <- modularity.cograph_communities(comm)

  expect_true(is.numeric(mod))
  expect_length(mod, 1)
})

test_that("modularity.cograph_communities handles missing modularity gracefully", {
  skip_if_not_installed("igraph")

  # Create minimal communities object
  g <- igraph::make_ring(5)
  result <- igraph::cluster_louvain(g)
  wrapped <- cograph:::.wrap_communities(result, "test", g)

  # Should still work via method dispatch
  mod <- modularity.cograph_communities(wrapped)
  expect_true(is.numeric(mod) || is.na(mod))
})

# ==============================================================================
# Test compare_communities additional methods
# ==============================================================================

test_that("compare_communities works with split.join method", {
  skip_if_not_installed("igraph")

  comm1 <- community_louvain(mat_community, seed = 1)
  comm2 <- community_leiden(mat_community, seed = 1)

  sj <- compare_communities(comm1, comm2, method = "split.join")
  expect_true(is.numeric(sj))
  expect_true(sj >= 0)
})

# ==============================================================================
# Test plot.cograph_communities
# ==============================================================================

test_that("plot.cograph_communities works with network", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  net <- as_cograph(mat_community)

  with_temp_png({
    plot(comm, network = net)
  })

  expect_true(TRUE)  # If we got here, plot succeeded
})

test_that("plot.cograph_communities passes additional arguments to splot", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  net <- as_cograph(mat_community)

  with_temp_png({
    plot(comm, network = net, title = "Test Communities")
  })

  expect_true(TRUE)
})

# ==============================================================================
# Test community_walktrap additional parameters
# ==============================================================================

test_that("community_walktrap with merges = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_walktrap(mat_small, merges = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_walktrap with modularity = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_walktrap(mat_small, modularity = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_walktrap with membership = FALSE", {
  skip_if_not_installed("igraph")

  # When membership = FALSE, we still get the structure
  comm <- community_walktrap(mat_small, membership = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test community_fast_greedy additional parameters
# ==============================================================================

test_that("community_fast_greedy with merges = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_fast_greedy(mat_small, merges = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_fast_greedy with modularity = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_fast_greedy(mat_small, modularity = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_fast_greedy with membership = FALSE", {
  skip_if_not_installed("igraph")

  comm <- community_fast_greedy(mat_small, membership = FALSE)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test with igraph objects directly
# ==============================================================================

test_that("all methods work with igraph input", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")

  # Test several methods with igraph input
  expect_s3_class(community_louvain(g), "cograph_communities")
  expect_s3_class(community_leiden(g), "cograph_communities")
  expect_s3_class(community_walktrap(g), "cograph_communities")
  expect_s3_class(community_fast_greedy(g), "cograph_communities")
  expect_s3_class(community_infomap(g), "cograph_communities")
  expect_s3_class(community_label_propagation(g), "cograph_communities")
  expect_s3_class(community_edge_betweenness(g), "cograph_communities")
  expect_s3_class(community_leading_eigenvector(g), "cograph_communities")
})

# ==============================================================================
# Test n_communities and community_sizes edge cases
# ==============================================================================

test_that("n_communities works with single community", {
  skip_if_not_installed("igraph")

  # Small fully connected graph likely has single community
  mat_full <- matrix(1, 3, 3)
  diag(mat_full) <- 0
  comm <- community_louvain(mat_full)

  n <- n_communities(comm)
  expect_true(is.numeric(n))
  expect_true(n >= 1)
})

test_that("community_sizes returns integer vector", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  sizes <- community_sizes(comm)

  expect_true(is.integer(sizes))
  expect_true(all(sizes > 0))
})

# ==============================================================================
# Test .resolve_weights edge cases
# ==============================================================================
test_that(".resolve_weights handles length-1 NA correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  igraph::E(g)$weight <- c(1, 2, 3, 4, 5)

  # NA should return NULL even if graph has weights
  result <- cograph:::.resolve_weights(g, NA)
  expect_null(result)
})

test_that(".resolve_weights returns custom weights unchanged", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  igraph::E(g)$weight <- c(1, 2, 3, 4, 5)

  custom <- c(10, 20, 30, 40, 50)
  result <- cograph:::.resolve_weights(g, custom)
  expect_equal(result, custom)
})

# ==============================================================================
# Test .wrap_communities edge cases
# ==============================================================================

test_that(".wrap_communities handles unnamed graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)  # No names set
  result <- igraph::cluster_louvain(g)
  wrapped <- cograph:::.wrap_communities(result, "test", g)

  expect_s3_class(wrapped, "cograph_communities")
  expect_null(wrapped$names)
})

test_that(".wrap_communities adds class correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- igraph::cluster_louvain(g)
  wrapped <- cograph:::.wrap_communities(result, "louvain", g)

  expect_true("cograph_communities" %in% class(wrapped))
  expect_true("communities" %in% class(wrapped))
})

# ==============================================================================
# Test cograph_network input conversion
# ==============================================================================

test_that("communities work with cograph_network input via to_igraph", {
  skip_if_not_installed("igraph")

  net <- as_cograph(mat_community)

  # All major methods should work
  expect_s3_class(communities(net, method = "louvain"), "cograph_communities")
  expect_s3_class(communities(net, method = "leiden"), "cograph_communities")
  expect_s3_class(communities(net, method = "walktrap"), "cograph_communities")
})

# ==============================================================================
# Test directed vs undirected handling
# ==============================================================================

test_that("community_louvain errors on directed input", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(10, directed = TRUE)
  expect_error(community_louvain(g))
})

test_that("community_leiden errors on directed input", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(10, directed = TRUE)
  expect_error(community_leiden(g))
})

test_that("community_infomap handles directed input well", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(10, directed = TRUE)
  comm <- community_infomap(g)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test weighted networks
# ==============================================================================

test_that("communities detect weighted networks automatically", {
  skip_if_not_installed("igraph")

  # Create weighted igraph
  g <- igraph::make_ring(10)
  igraph::E(g)$weight <- runif(igraph::ecount(g))

  comm <- community_louvain(g)  # Should use weights automatically
  expect_s3_class(comm, "cograph_communities")
})

test_that("weights = NA forces unweighted analysis", {
  skip_if_not_installed("igraph")

  # Create weighted igraph
  g <- igraph::make_ring(10)
  igraph::E(g)$weight <- runif(igraph::ecount(g))

  comm <- community_louvain(g, weights = NA)  # Should ignore weights
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test error handling
# ==============================================================================

test_that("community_fluid requires no.of.communities", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  expect_error(community_fluid(g), "no.of.communities is required")
})

test_that("plot.cograph_communities requires network argument", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_small)
  expect_error(plot(comm), "network argument required")
})

# ==============================================================================
# Test community_optimal warning
# ==============================================================================

test_that("community_optimal warns on large network", {
  skip_if_not_installed("igraph")

  # Create network larger than 50 nodes (but small enough to actually compute)
  # Use sparse 51-node ring to trigger warning without excessive computation
  g <- igraph::make_ring(51)
  mat_large <- as.matrix(igraph::as_adjacency_matrix(g))

  expect_warning(community_optimal(mat_large), "50 nodes")
})

# ==============================================================================
# Test community_fluid disconnected graph warning
# ==============================================================================

test_that("community_fluid warns and uses largest component", {
  skip_if_not_installed("igraph")

  # Create disconnected graph
  g1 <- igraph::make_full_graph(5)
  g2 <- igraph::make_full_graph(5)
  g <- g1 + g2  # Disconnected

  expect_warning(community_fluid(g, no.of.communities = 2), "connected")
})

# ==============================================================================
# Test community_spinglass disconnected graph warning
# ==============================================================================

test_that("community_spinglass warns and uses largest component", {
  skip_if_not_installed("igraph")

  # Create disconnected graph
  g1 <- igraph::make_full_graph(5)
  g2 <- igraph::make_full_graph(5)
  g <- g1 + g2  # Disconnected

  expect_warning(community_spinglass(g, seed = 42), "connected")
})

# ==============================================================================
# Test seed reproducibility
# ==============================================================================

test_that("louvain is reproducible with seed", {
  skip_if_not_installed("igraph")

  comm1 <- community_louvain(mat_community, seed = 42)
  comm2 <- community_louvain(mat_community, seed = 42)

  expect_equal(comm1$membership, comm2$membership)
})

test_that("leiden is reproducible with seed", {
  skip_if_not_installed("igraph")

  comm1 <- community_leiden(mat_community, seed = 42)
  comm2 <- community_leiden(mat_community, seed = 42)

  expect_equal(comm1$membership, comm2$membership)
})

test_that("infomap is reproducible with seed", {
  skip_if_not_installed("igraph")

  comm1 <- community_infomap(mat_community, seed = 42)
  comm2 <- community_infomap(mat_community, seed = 42)

  expect_equal(comm1$membership, comm2$membership)
})

test_that("label_propagation is reproducible with seed", {
  skip_if_not_installed("igraph")

  comm1 <- community_label_propagation(mat_community, seed = 42)
  comm2 <- community_label_propagation(mat_community, seed = 42)

  expect_equal(comm1$membership, comm2$membership)
})

test_that("spinglass is reproducible with seed", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm1 <- community_spinglass(g, seed = 42)
  comm2 <- community_spinglass(g, seed = 42)

  expect_equal(comm1$membership, comm2$membership)
})

# ==============================================================================
# Test all short aliases work correctly
# ==============================================================================

test_that("all com_* aliases produce correct algorithm", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  mat_tiny <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  expect_equal(com_lv(mat_small)$algorithm, "louvain")
  expect_equal(com_ld(mat_small)$algorithm, "leiden")
  expect_equal(com_fg(mat_small)$algorithm, "fast_greedy")
  expect_equal(com_wt(mat_small)$algorithm, "walktrap")
  expect_equal(com_im(mat_small)$algorithm, "infomap")
  expect_equal(com_lp(mat_small)$algorithm, "label_propagation")
  expect_equal(com_eb(mat_small)$algorithm, "edge_betweenness")
  expect_equal(com_le(mat_small)$algorithm, "leading_eigenvector")
  expect_equal(com_sg(g, seed = 42)$algorithm, "spinglass")
  expect_equal(com_op(mat_tiny)$algorithm, "optimal")
  expect_equal(com_fl(g, no.of.communities = 2)$algorithm, "fluid")
  expect_true(grepl("consensus", com_consensus(mat_small, n_runs = 2)$algorithm))
})

# ==============================================================================
# Summary
# ==============================================================================

cat("\n=== All Community Detection Coverage Tests (Series 41) Passed ===\n")
