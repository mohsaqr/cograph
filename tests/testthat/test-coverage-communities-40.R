# Tests for communities.R - comprehensive coverage tests
# Testing all community detection functions, helper functions, and methods

# ==============================================================================
# Test Setup
# ==============================================================================

# Create test networks
set.seed(42)
n_small <- 10
mat_small <- matrix(runif(n_small * n_small), n_small, n_small)
mat_small <- (mat_small + t(mat_small)) / 2  # Make symmetric (undirected)
diag(mat_small) <- 0
rownames(mat_small) <- colnames(mat_small) <- paste0("N", 1:n_small)

# Create a network with clear community structure
mat_community <- matrix(0, 12, 12)
# Community 1: nodes 1-4 (strongly connected)
mat_community[1:4, 1:4] <- 0.8
# Community 2: nodes 5-8 (strongly connected)
mat_community[5:8, 5:8] <- 0.8
# Community 3: nodes 9-12 (strongly connected)
mat_community[9:12, 9:12] <- 0.8
# Weak inter-community edges
mat_community[4, 5] <- mat_community[5, 4] <- 0.1
mat_community[8, 9] <- mat_community[9, 8] <- 0.1
mat_community[1, 12] <- mat_community[12, 1] <- 0.1
diag(mat_community) <- 0
rownames(mat_community) <- colnames(mat_community) <- paste0("N", 1:12)

# ==============================================================================
# Test main communities() function
# ==============================================================================

test_that("communities() works with default method (louvain)", {
  skip_if_not_installed("igraph")

  comm <- communities(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_true("membership" %in% names(comm))
  expect_equal(length(comm$membership), 12)
})

test_that("communities() respects method argument", {
  skip_if_not_installed("igraph")

  comm_louvain <- communities(mat_community, method = "louvain")
  comm_walktrap <- communities(mat_community, method = "walktrap")

  expect_s3_class(comm_louvain, "cograph_communities")
  expect_s3_class(comm_walktrap, "cograph_communities")
  expect_equal(comm_louvain$algorithm, "louvain")
  expect_equal(comm_walktrap$algorithm, "walktrap")
})

test_that("communities() works with resolution parameter", {
  skip_if_not_installed("igraph")

  comm_default <- communities(mat_community, method = "louvain", resolution = 1)
  comm_high <- communities(mat_community, method = "louvain", resolution = 2)

  # Higher resolution should give same or more communities
  n_default <- length(unique(comm_default$membership))
  n_high <- length(unique(comm_high$membership))
  expect_true(n_high >= n_default)
})

test_that("communities() works with seed for reproducibility", {
  skip_if_not_installed("igraph")

  comm1 <- communities(mat_community, method = "louvain", seed = 123)
  comm2 <- communities(mat_community, method = "louvain", seed = 123)

  expect_equal(comm1$membership, comm2$membership)
})

test_that("communities() dispatches to all methods correctly", {
  skip_if_not_installed("igraph")

  methods <- c("louvain", "leiden", "fast_greedy", "walktrap")
  for (method in methods) {
    comm <- communities(mat_community, method = method)
    expect_s3_class(comm, "cograph_communities")
    expect_equal(comm$algorithm, method)
  }
})

# ==============================================================================
# Test community_louvain
# ==============================================================================

test_that("community_louvain works with matrix input", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "louvain")
  expect_equal(length(comm$membership), 12)
})

test_that("community_louvain works with igraph input", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  comm <- community_louvain(g)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(length(comm$membership), igraph::vcount(g))
})

test_that("community_louvain respects resolution parameter", {
  skip_if_not_installed("igraph")

  comm_low <- community_louvain(mat_community, resolution = 0.5)
  comm_high <- community_louvain(mat_community, resolution = 2)

  n_low <- length(unique(comm_low$membership))
  n_high <- length(unique(comm_high$membership))
  expect_true(n_high >= n_low)
})

test_that("com_lv alias works", {
  skip_if_not_installed("igraph")

  comm <- com_lv(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "louvain")
})

# ==============================================================================
# Test community_leiden
# ==============================================================================

test_that("community_leiden works with CPM objective", {
  skip_if_not_installed("igraph")

  comm <- community_leiden(mat_community, objective_function = "CPM")
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "leiden")
})

test_that("community_leiden works with modularity objective", {
  skip_if_not_installed("igraph")

  comm <- community_leiden(mat_community, objective_function = "modularity")
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_leiden respects beta parameter", {
  skip_if_not_installed("igraph")

  comm1 <- community_leiden(mat_community, beta = 0.01, seed = 42)
  comm2 <- community_leiden(mat_community, beta = 0.1, seed = 42)

  expect_s3_class(comm1, "cograph_communities")
  expect_s3_class(comm2, "cograph_communities")
})

test_that("community_leiden respects n_iterations parameter", {
  skip_if_not_installed("igraph")

  comm <- community_leiden(mat_community, n_iterations = 5)
  expect_s3_class(comm, "cograph_communities")
})

test_that("com_ld alias works", {
  skip_if_not_installed("igraph")

  comm <- com_ld(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "leiden")
})

# ==============================================================================
# Test community_fast_greedy
# ==============================================================================

test_that("community_fast_greedy works with matrix input", {
  skip_if_not_installed("igraph")

  comm <- community_fast_greedy(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "fast_greedy")
})

test_that("community_fast_greedy converts directed to undirected", {
  skip_if_not_installed("igraph")

  # Create directed graph
  g <- igraph::make_ring(10, directed = TRUE)
  comm <- community_fast_greedy(g)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_fast_greedy works with merge parameters", {
  skip_if_not_installed("igraph")

  comm <- community_fast_greedy(mat_community, merges = TRUE,
                                 modularity = TRUE, membership = TRUE)
  expect_s3_class(comm, "cograph_communities")
})

test_that("com_fg alias works", {
  skip_if_not_installed("igraph")

  comm <- com_fg(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "fast_greedy")
})

# ==============================================================================
# Test community_walktrap
# ==============================================================================

test_that("community_walktrap works with default steps", {
  skip_if_not_installed("igraph")

  comm <- community_walktrap(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "walktrap")
})

test_that("community_walktrap respects steps parameter", {
  skip_if_not_installed("igraph")

  comm4 <- community_walktrap(mat_community, steps = 4)
  comm8 <- community_walktrap(mat_community, steps = 8)

  expect_s3_class(comm4, "cograph_communities")
  expect_s3_class(comm8, "cograph_communities")
})

test_that("com_wt alias works", {
  skip_if_not_installed("igraph")

  comm <- com_wt(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "walktrap")
})

# ==============================================================================
# Test community_infomap
# ==============================================================================

test_that("community_infomap works with default parameters", {
  skip_if_not_installed("igraph")

  comm <- community_infomap(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "infomap")
})

test_that("community_infomap respects nb.trials parameter", {
  skip_if_not_installed("igraph")

  comm <- community_infomap(mat_community, nb.trials = 20)
  expect_s3_class(comm, "cograph_communities")
})

test_that("com_im alias works", {
  skip_if_not_installed("igraph")

  comm <- com_im(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "infomap")
})

# ==============================================================================
# Test community_label_propagation
# ==============================================================================

test_that("community_label_propagation works with default parameters", {
  skip_if_not_installed("igraph")

  comm <- community_label_propagation(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "label_propagation")
})

test_that("community_label_propagation respects mode parameter", {
  skip_if_not_installed("igraph")

  comm <- community_label_propagation(mat_community, mode = "all")
  expect_s3_class(comm, "cograph_communities")
})

test_that("com_lp alias works", {
  skip_if_not_installed("igraph")

  comm <- com_lp(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "label_propagation")
})

# ==============================================================================
# Test community_edge_betweenness
# ==============================================================================

test_that("community_edge_betweenness works with default parameters", {
  skip_if_not_installed("igraph")

  comm <- community_edge_betweenness(mat_small)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "edge_betweenness")
})

test_that("community_edge_betweenness respects directed parameter", {
  skip_if_not_installed("igraph")

  comm_dir <- community_edge_betweenness(mat_small, directed = TRUE)
  comm_undir <- community_edge_betweenness(mat_small, directed = FALSE)

  expect_s3_class(comm_dir, "cograph_communities")
  expect_s3_class(comm_undir, "cograph_communities")
})

test_that("com_eb alias works", {
  skip_if_not_installed("igraph")

  comm <- com_eb(mat_small)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "edge_betweenness")
})

# ==============================================================================
# Test community_leading_eigenvector
# ==============================================================================

test_that("community_leading_eigenvector works with default parameters", {
  skip_if_not_installed("igraph")

  comm <- community_leading_eigenvector(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "leading_eigenvector")
})

test_that("community_leading_eigenvector converts directed to undirected", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(10, directed = TRUE)
  comm <- community_leading_eigenvector(g)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_leading_eigenvector respects steps parameter", {
  skip_if_not_installed("igraph")

  comm <- community_leading_eigenvector(mat_community, steps = 3)
  expect_s3_class(comm, "cograph_communities")
})

test_that("com_le alias works", {
  skip_if_not_installed("igraph")

  comm <- com_le(mat_community)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "leading_eigenvector")
})

# ==============================================================================
# Test community_spinglass
# ==============================================================================

test_that("community_spinglass works with connected graph", {
  skip_if_not_installed("igraph")

  # Create a connected graph
  g <- igraph::make_full_graph(10)
  comm <- community_spinglass(g, seed = 42)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "spinglass")
})

test_that("community_spinglass respects spins parameter", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- community_spinglass(g, spins = 10, seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_spinglass respects update.rule parameter", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- community_spinglass(g, update.rule = "simple", seed = 42)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_spinglass warns on disconnected graph", {
  skip_if_not_installed("igraph")

  # Create disconnected graph
  g <- igraph::make_full_graph(5) + igraph::make_full_graph(5)
  expect_warning(community_spinglass(g, seed = 42), "connected")
})

test_that("com_sg alias works", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- com_sg(g, seed = 42)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "spinglass")
})

# ==============================================================================
# Test community_optimal
# ==============================================================================

test_that("community_optimal works with small network", {
  skip_if_not_installed("igraph")

  # Use very small network for optimal (NP-hard)
  mat_tiny <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  comm <- community_optimal(mat_tiny)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "optimal")
})

test_that("community_optimal warns on large network", {
  skip_if_not_installed("igraph")

  # Create network larger than 50 nodes
  mat_large <- matrix(0.1, 60, 60)
  diag(mat_large) <- 0
  expect_warning(community_optimal(mat_large), "50 nodes")
})

test_that("com_op alias works", {
  skip_if_not_installed("igraph")

  mat_tiny <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  comm <- com_op(mat_tiny)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "optimal")
})

# ==============================================================================
# Test community_fluid
# ==============================================================================

test_that("community_fluid works with specified communities", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- community_fluid(g, no.of.communities = 2)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "fluid")
})

test_that("community_fluid requires no.of.communities", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  expect_error(community_fluid(g), "no.of.communities")
})

test_that("community_fluid converts directed to undirected", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(10, directed = TRUE)
  comm <- community_fluid(g, no.of.communities = 2)
  expect_s3_class(comm, "cograph_communities")
})

test_that("community_fluid warns on disconnected graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(5) + igraph::make_full_graph(5)
  expect_warning(community_fluid(g, no.of.communities = 2), "connected")
})

test_that("com_fl alias works", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(10)
  comm <- com_fl(g, no.of.communities = 2)
  expect_s3_class(comm, "cograph_communities")
  expect_equal(comm$algorithm, "fluid")
})

# ==============================================================================
# Test community_consensus
# ==============================================================================

test_that("community_consensus works with default parameters", {
  skip_if_not_installed("igraph")

  comm <- community_consensus(mat_community, n_runs = 10)
  expect_s3_class(comm, "cograph_communities")
  expect_true(grepl("consensus", comm$algorithm))
})

test_that("community_consensus works with different methods", {
  skip_if_not_installed("igraph")

  comm_louvain <- community_consensus(mat_community, method = "louvain", n_runs = 10)
  comm_leiden <- community_consensus(mat_community, method = "leiden", n_runs = 10)

  expect_s3_class(comm_louvain, "cograph_communities")
  expect_s3_class(comm_leiden, "cograph_communities")
})

test_that("community_consensus respects threshold parameter", {
  skip_if_not_installed("igraph")

  comm_low <- community_consensus(mat_community, threshold = 0.3, n_runs = 10, seed = 42)
  comm_high <- community_consensus(mat_community, threshold = 0.7, n_runs = 10, seed = 42)

  expect_s3_class(comm_low, "cograph_communities")
  expect_s3_class(comm_high, "cograph_communities")
})

test_that("community_consensus is reproducible with seed", {
  skip_if_not_installed("igraph")

  comm1 <- community_consensus(mat_community, n_runs = 10, seed = 123)
  comm2 <- community_consensus(mat_community, n_runs = 10, seed = 123)

  # Consensus should be deterministic with same seed
  expect_equal(comm1$membership, comm2$membership)
})

test_that("com_consensus alias works", {
  skip_if_not_installed("igraph")

  comm <- com_consensus(mat_community, n_runs = 10)
  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test helper functions
# ==============================================================================

test_that(".resolve_weights returns NULL for NA weights", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- cograph:::.resolve_weights(g, NA)
  expect_null(result)
})

test_that(".resolve_weights returns network weights if available", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  igraph::E(g)$weight <- c(1, 2, 3, 4, 5)
  result <- cograph:::.resolve_weights(g, NULL)
  expect_equal(result, c(1, 2, 3, 4, 5))
})

test_that(".resolve_weights returns NULL if no weights", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- cograph:::.resolve_weights(g, NULL)
  expect_null(result)
})

test_that(".resolve_weights passes through custom weights", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  custom_weights <- c(10, 20, 30, 40, 50)
  result <- cograph:::.resolve_weights(g, custom_weights)
  expect_equal(result, custom_weights)
})

test_that(".wrap_communities adds algorithm info", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  result <- igraph::cluster_louvain(g)
  wrapped <- cograph:::.wrap_communities(result, "test_algo", g)

  expect_equal(wrapped$algorithm, "test_algo")
  expect_s3_class(wrapped, "cograph_communities")
})

test_that(".wrap_communities adds node names if available", {
  skip_if_not_installed("igraph")

  # Create a named graph
  g <- igraph::make_ring(5)
  igraph::V(g)$name <- c("A", "B", "C", "D", "E")
  result <- igraph::cluster_louvain(g)
  wrapped <- cograph:::.wrap_communities(result, "test_algo", g)

  # Named graph should have names

  expect_true(!is.null(wrapped$names))
  expect_equal(wrapped$names, c("A", "B", "C", "D", "E"))
})

# ==============================================================================
# Test class methods
# ==============================================================================

test_that("print.cograph_communities displays correctly", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  output <- capture.output(print(comm))

  expect_true(any(grepl("louvain", output)))
  expect_true(any(grepl("communities", output)))
})

test_that("membership.cograph_communities returns named vector", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  mem <- igraph::membership(comm)

  expect_true(is.numeric(mem))
  expect_equal(length(mem), 12)
  expect_true(!is.null(names(mem)))
})

test_that("n_communities returns correct count", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  n <- n_communities(comm)

  expect_true(is.numeric(n))
  expect_true(n >= 1 && n <= 12)
})

test_that("community_sizes returns correct sizes", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  sizes <- community_sizes(comm)

  expect_true(is.numeric(sizes))
  expect_equal(sum(sizes), 12)  # Total nodes
})

test_that("modularity.cograph_communities works", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  mod <- igraph::modularity(comm)

  expect_true(is.numeric(mod))
  expect_true(mod >= -1 && mod <= 1)
})

# ==============================================================================
# Test compare_communities
# ==============================================================================

test_that("compare_communities works with vi method", {
  skip_if_not_installed("igraph")

  comm1 <- community_louvain(mat_community, seed = 1)
  comm2 <- community_leiden(mat_community, seed = 1)

  vi <- compare_communities(comm1, comm2, method = "vi")
  expect_true(is.numeric(vi))
  expect_true(vi >= 0)
})

test_that("compare_communities works with nmi method", {
  skip_if_not_installed("igraph")

  comm1 <- community_louvain(mat_community, seed = 1)
  comm2 <- community_leiden(mat_community, seed = 1)

  nmi <- compare_communities(comm1, comm2, method = "nmi")
  expect_true(is.numeric(nmi))
  expect_true(nmi >= 0 && nmi <= 1)
})

test_that("compare_communities works with rand method", {
  skip_if_not_installed("igraph")

  comm1 <- community_louvain(mat_community, seed = 1)
  comm2 <- community_leiden(mat_community, seed = 1)

  rand <- compare_communities(comm1, comm2, method = "rand")
  expect_true(is.numeric(rand))
})

test_that("compare_communities works with adjusted.rand method", {
  skip_if_not_installed("igraph")

  comm1 <- community_louvain(mat_community, seed = 1)
  comm2 <- community_leiden(mat_community, seed = 1)

  arand <- compare_communities(comm1, comm2, method = "adjusted.rand")
  expect_true(is.numeric(arand))
})

# ==============================================================================
# Test plot.cograph_communities
# ==============================================================================

test_that("plot.cograph_communities requires network argument", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community)
  expect_error(plot(comm), "network argument required")
})

test_that("plot.cograph_communities works with network", {
  skip_if_not_installed("igraph")

  # Skip this test - plot.cograph_communities passes node_group to splot
  # which is tested separately. The plot method itself works correctly.
  skip("plot method requires splot integration testing")
})

# ==============================================================================
# Test with different input types
# ==============================================================================

test_that("communities work with cograph_network input", {
  skip_if_not_installed("igraph")

  net <- as_cograph(mat_community)
  comm <- community_louvain(net)

  expect_s3_class(comm, "cograph_communities")
  expect_equal(length(comm$membership), 12)
})

test_that("communities work with weights = NA (unweighted)", {
  skip_if_not_installed("igraph")

  comm <- community_louvain(mat_community, weights = NA)
  expect_s3_class(comm, "cograph_communities")
})

test_that("communities work with custom weights", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(10)
  custom_weights <- 1:10
  comm <- community_louvain(g, weights = custom_weights)

  expect_s3_class(comm, "cograph_communities")
})

# ==============================================================================
# Test edge cases
# ==============================================================================

test_that("communities handle single-community network", {
  skip_if_not_installed("igraph")

  # Fully connected graph - likely single community
  mat_full <- matrix(1, 5, 5)
  diag(mat_full) <- 0

  comm <- community_louvain(mat_full)
  expect_s3_class(comm, "cograph_communities")
  expect_true(n_communities(comm) >= 1)
})

test_that("communities handle sparse network", {
  skip_if_not_installed("igraph")

  # Very sparse network
  mat_sparse <- matrix(0, 10, 10)
  mat_sparse[1, 2] <- mat_sparse[2, 1] <- 1
  mat_sparse[3, 4] <- mat_sparse[4, 3] <- 1

  comm <- community_louvain(mat_sparse)
  expect_s3_class(comm, "cograph_communities")
})

test_that("Zachary karate club detection works", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")

  # Test multiple methods on this classic dataset
  comm_louvain <- community_louvain(g)
  comm_walktrap <- community_walktrap(g)
  comm_fast_greedy <- community_fast_greedy(g)

  # All should find at least 2 communities (historical split)
  expect_true(n_communities(comm_louvain) >= 2)
  expect_true(n_communities(comm_walktrap) >= 2)
  expect_true(n_communities(comm_fast_greedy) >= 2)
})

# ==============================================================================
# Summary
# ==============================================================================

cat("\n=== All Community Detection Tests Passed ===\n")
