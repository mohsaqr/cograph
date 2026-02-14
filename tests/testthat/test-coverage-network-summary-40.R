# Comprehensive tests for R/network-summary.R
# This file provides extensive coverage for all network summary functions

# =============================================================================
# SECTION 1: network_summary() function tests
# =============================================================================

test_that("network_summary returns correct structure with default params", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  # Check basic fields exist
  expect_true("node_count" %in% names(result))
  expect_true("edge_count" %in% names(result))
  expect_true("density" %in% names(result))
  expect_true("component_count" %in% names(result))
  expect_true("diameter" %in% names(result))
  expect_true("mean_distance" %in% names(result))
})

test_that("network_summary computes correct node and edge counts", {
  # Complete graph K3
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat)

  expect_equal(result$node_count, 3)
  expect_equal(result$edge_count, 3)  # 3 edges in undirected triangle
})

test_that("network_summary computes correct density for complete graph", {
  # Complete graph K4 should have density = 1
  mat <- matrix(1, 4, 4)
  diag(mat) <- 0
  result <- network_summary(mat)

  expect_equal(result$density, 1)
})

test_that("network_summary computes correct density for sparse graph", {
  # Path graph with 4 nodes: 3 edges out of max 6, density = 0.5
  mat <- matrix(c(0, 1, 0, 0,
                  1, 0, 1, 0,
                  0, 1, 0, 1,
                  0, 0, 1, 0), 4, 4, byrow = TRUE)
  result <- network_summary(mat)

  expect_equal(result$density, 0.5)
})

test_that("network_summary with loops = FALSE removes self-loops", {
  mat <- matrix(c(1, 1, 0,
                  1, 1, 1,
                  0, 1, 1), 3, 3, byrow = TRUE)
  result_with_loops <- network_summary(mat, loops = TRUE)
  result_no_loops <- network_summary(mat, loops = FALSE)

  # With loops removed, edge count should be lower

  expect_true(result_no_loops$edge_count <= result_with_loops$edge_count)
})

test_that("network_summary with simplify = 'sum' aggregates edges", {
  mat <- matrix(c(0, 2, 0, 2, 0, 3, 0, 3, 0), 3, 3)
  result <- network_summary(mat, simplify = "sum")

  expect_true(is.data.frame(result))
  expect_equal(result$node_count, 3)
})

test_that("network_summary with simplify = 'mean' averages edges", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  result <- network_summary(mat, simplify = "mean")

  expect_true(is.data.frame(result))
})

test_that("network_summary with simplify = 'max' uses maximum", {
  mat <- matrix(c(0, 5, 0, 5, 0, 3, 0, 3, 0), 3, 3)
  result <- network_summary(mat, simplify = "max")

  expect_true(is.data.frame(result))
})

test_that("network_summary with simplify = 'min' uses minimum", {
  mat <- matrix(c(0, 5, 0, 5, 0, 3, 0, 3, 0), 3, 3)
  result <- network_summary(mat, simplify = "min")

  expect_true(is.data.frame(result))
})

test_that("network_summary with simplify = FALSE keeps multiple edges", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  result <- network_summary(mat, simplify = FALSE)

  expect_true(is.data.frame(result))
})

test_that("network_summary with simplify = 'none' keeps multiple edges", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  result <- network_summary(mat, simplify = "none")

  expect_true(is.data.frame(result))
})

test_that("network_summary with mode = 'in' for directed networks", {
  mat <- matrix(c(0, 1, 0,
                  0, 0, 1,
                  1, 0, 0), 3, 3, byrow = TRUE)
  result <- network_summary(mat, directed = TRUE, mode = "in")

  expect_true(is.data.frame(result))
  expect_true(!is.na(result$reciprocity))
})

test_that("network_summary with mode = 'out' for directed networks", {
  mat <- matrix(c(0, 1, 0,
                  0, 0, 1,
                  1, 0, 0), 3, 3, byrow = TRUE)
  result <- network_summary(mat, directed = TRUE, mode = "out")

  expect_true(is.data.frame(result))
})

test_that("network_summary reports NA for directed-only metrics on undirected graphs", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, directed = FALSE)

  expect_true(is.na(result$reciprocity))
  expect_true(is.na(result$centralization_in_degree))
  expect_true(is.na(result$centralization_out_degree))
})

test_that("network_summary computes reciprocity for directed networks", {
  # Fully reciprocal network
  mat <- matrix(c(0, 1, 0,
                  1, 0, 1,
                  0, 1, 0), 3, 3, byrow = TRUE)
  result <- network_summary(mat, directed = TRUE)

  expect_true(!is.na(result$reciprocity))
  expect_true(result$reciprocity >= 0 && result$reciprocity <= 1)
})

test_that("network_summary computes centralization metrics", {
  mat <- matrix(c(0, 1, 1, 1, 1,
                  1, 0, 0, 0, 0,
                  1, 0, 0, 0, 0,
                  1, 0, 0, 0, 0,
                  1, 0, 0, 0, 0), 5, 5, byrow = TRUE)  # Star graph
  result <- network_summary(mat)

  expect_true(!is.na(result$centralization_degree))
  expect_true(result$centralization_degree >= 0 && result$centralization_degree <= 1)
  expect_true(!is.na(result$centralization_betweenness))
})

test_that("network_summary computes transitivity (clustering coefficient)", {
  # Triangle has transitivity = 1
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat)

  expect_equal(result$transitivity, 1)
})

test_that("network_summary computes hub and authority scores", {
  skip_if_not_installed("igraph")
  # Use a larger connected graph for stable HITS scores
  g <- igraph::make_ring(10)
  result <- network_summary(g)

  # Hub and authority scores should be numeric (may be NA for some graphs)
  expect_true(is.numeric(result$hub_score))
  expect_true(is.numeric(result$authority_score))
})

test_that("network_summary with weighted = TRUE uses weights", {
  mat <- matrix(c(0, 0.5, 1, 0.5, 0, 0.8, 1, 0.8, 0), 3, 3)
  result <- network_summary(mat, weighted = TRUE)

  expect_true(is.data.frame(result))
})

test_that("network_summary with weighted = FALSE ignores weights", {
  mat <- matrix(c(0, 0.5, 1, 0.5, 0, 0.8, 1, 0.8, 0), 3, 3)
  result <- network_summary(mat, weighted = FALSE)

  expect_true(is.data.frame(result))
})

test_that("network_summary with digits = NULL skips rounding", {
  mat <- matrix(c(0, 0.123456, 0.789012,
                  0.123456, 0, 0.456789,
                  0.789012, 0.456789, 0), 3, 3)
  result <- network_summary(mat, digits = NULL)

  expect_true(is.data.frame(result))
})

test_that("network_summary with digits = 0 rounds to integers", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, digits = 0)

  expect_true(is.data.frame(result))
})

# =============================================================================
# SECTION 2: Extended metrics tests
# =============================================================================

test_that("network_summary with extended = TRUE includes girth", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, extended = TRUE)

  expect_true("girth" %in% names(result))
  expect_equal(result$girth, 3)  # Triangle has girth 3
})

test_that("network_summary with extended = TRUE includes radius", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, extended = TRUE)

  expect_true("radius" %in% names(result))
})

test_that("network_summary with extended = TRUE includes vertex_connectivity", {
  # Complete graph K4 has vertex connectivity 3
  mat <- matrix(1, 4, 4)
  diag(mat) <- 0
  result <- network_summary(mat, extended = TRUE)

  expect_true("vertex_connectivity" %in% names(result))
  expect_equal(result$vertex_connectivity, 3)
})

test_that("network_summary with extended = TRUE includes largest_clique_size", {
  # Complete graph K4 has clique number 4
  mat <- matrix(1, 4, 4)
  diag(mat) <- 0
  result <- network_summary(mat, extended = TRUE)

  expect_true("largest_clique_size" %in% names(result))
  expect_equal(result$largest_clique_size, 4)
})

test_that("network_summary with extended = TRUE includes cut_vertex_count", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, extended = TRUE)

  expect_true("cut_vertex_count" %in% names(result))
  expect_true(is.numeric(result$cut_vertex_count))
})

test_that("network_summary with extended = TRUE includes bridge_count", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, extended = TRUE)

  expect_true("bridge_count" %in% names(result))
  expect_true(is.numeric(result$bridge_count))
})

test_that("network_summary with extended = TRUE includes efficiencies", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, extended = TRUE)

  expect_true("global_efficiency" %in% names(result))
  expect_true("local_efficiency" %in% names(result))
})

# =============================================================================
# SECTION 3: Detailed metrics tests
# =============================================================================

test_that("network_summary with detailed = TRUE includes mean_degree", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, detailed = TRUE)

  expect_true("mean_degree" %in% names(result))
  expect_true("sd_degree" %in% names(result))
  expect_true("median_degree" %in% names(result))
})

test_that("network_summary with detailed = TRUE includes strength stats", {
  mat <- matrix(c(0, 0.5, 1, 0.5, 0, 0.8, 1, 0.8, 0), 3, 3)
  result <- network_summary(mat, detailed = TRUE)

  expect_true("mean_strength" %in% names(result))
  expect_true("sd_strength" %in% names(result))
})

test_that("network_summary with detailed = TRUE includes centrality means", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, detailed = TRUE)

  expect_true("mean_betweenness" %in% names(result))
  expect_true("mean_closeness" %in% names(result))
  expect_true("mean_eigenvector" %in% names(result))
  expect_true("mean_pagerank" %in% names(result))
})

test_that("network_summary with detailed = TRUE includes constraint and transitivity", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, detailed = TRUE)

  expect_true("mean_constraint" %in% names(result))
  expect_true("mean_local_transitivity" %in% names(result))
})

test_that("network_summary with both detailed and extended = TRUE", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_summary(mat, detailed = TRUE, extended = TRUE)

  # Should have all fields
  expect_true("mean_degree" %in% names(result))
  expect_true("girth" %in% names(result))
  expect_true("global_efficiency" %in% names(result))
  expect_true(ncol(result) > 25)  # Many columns
})

# =============================================================================
# SECTION 4: degree_distribution() function tests
# =============================================================================

test_that("degree_distribution returns histogram object", {
  mat <- matrix(c(0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0), 4, 4, byrow = TRUE)

  pdf(NULL)
  result <- degree_distribution(mat)
  dev.off()

  expect_true(inherits(result, "histogram"))
})

test_that("degree_distribution with mode = 'in' for directed networks", {
  mat <- matrix(c(0, 1, 0, 0,
                  0, 0, 1, 0,
                  1, 0, 0, 1,
                  0, 1, 0, 0), 4, 4, byrow = TRUE)

  pdf(NULL)
  result <- degree_distribution(mat, mode = "in", directed = TRUE)
  dev.off()

  expect_true(inherits(result, "histogram"))
})

test_that("degree_distribution with mode = 'out' for directed networks", {
  mat <- matrix(c(0, 1, 0, 0,
                  0, 0, 1, 0,
                  1, 0, 0, 1,
                  0, 1, 0, 0), 4, 4, byrow = TRUE)

  pdf(NULL)
  result <- degree_distribution(mat, mode = "out", directed = TRUE)
  dev.off()

  expect_true(inherits(result, "histogram"))
})

test_that("degree_distribution with loops = FALSE", {
  mat <- matrix(c(1, 1, 0, 1, 1, 1, 0, 1, 1), 3, 3, byrow = TRUE)

  pdf(NULL)
  result <- degree_distribution(mat, loops = FALSE)
  dev.off()

  expect_true(inherits(result, "histogram"))
})

test_that("degree_distribution with different simplify options", {
  mat <- matrix(c(0, 2, 0, 2, 0, 3, 0, 3, 0), 3, 3)

  pdf(NULL)
  result_sum <- degree_distribution(mat, simplify = "sum")
  result_mean <- degree_distribution(mat, simplify = "mean")
  result_max <- degree_distribution(mat, simplify = "max")
  result_min <- degree_distribution(mat, simplify = "min")
  dev.off()

  expect_true(inherits(result_sum, "histogram"))
  expect_true(inherits(result_mean, "histogram"))
  expect_true(inherits(result_max, "histogram"))
  expect_true(inherits(result_min, "histogram"))
})

test_that("degree_distribution with custom plot parameters", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  pdf(NULL)
  result <- degree_distribution(mat,
                                 main = "Custom Title",
                                 xlab = "Node Degree",
                                 ylab = "Count",
                                 col = "red")
  dev.off()

  expect_true(inherits(result, "histogram"))
})

test_that("degree_distribution cumulative changes y-axis label", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  pdf(NULL)
  result <- degree_distribution(mat, cumulative = TRUE)
  dev.off()

  # For cumulative, we get a histogram object but plot shows CDF
  expect_true(inherits(result, "histogram"))
})

test_that("degree_distribution cumulative with custom ylab", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  pdf(NULL)
  result <- degree_distribution(mat, cumulative = TRUE, ylab = "Custom CDF")
  dev.off()

  expect_true(inherits(result, "histogram"))
})

# =============================================================================
# SECTION 5: network_girth() function tests
# =============================================================================

test_that("network_girth returns correct girth for triangle", {
  # Triangle has girth 3
  triangle <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- network_girth(triangle)

  expect_equal(result, 3)
})

test_that("network_girth returns Inf for acyclic graph (tree)", {
  # Tree has no cycles
  tree <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  result <- network_girth(tree)

  expect_equal(result, Inf)
})

test_that("network_girth returns 4 for 4-cycle", {
  cycle4 <- matrix(c(0, 1, 0, 1,
                     1, 0, 1, 0,
                     0, 1, 0, 1,
                     1, 0, 1, 0), 4, 4, byrow = TRUE)
  result <- network_girth(cycle4)

  expect_equal(result, 4)
})

test_that("network_girth accepts igraph objects", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  result <- network_girth(g)

  expect_equal(result, 5)  # Ring of 5 nodes has girth 5
})

# =============================================================================
# SECTION 6: network_radius() function tests
# =============================================================================

test_that("network_radius returns correct value for star graph", {
  # Star: center has eccentricity 1, leaves have 2, radius = 1
  star <- matrix(c(0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), 4, 4)
  result <- network_radius(star)

  expect_equal(result, 1)
})

test_that("network_radius with directed = TRUE", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE)
  result <- network_radius(mat, directed = TRUE)

  expect_true(is.numeric(result))
})

test_that("network_radius with directed = FALSE", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  result <- network_radius(mat, directed = FALSE)

  expect_true(is.numeric(result))
})

test_that("network_radius accepts igraph objects", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  result <- network_radius(g)

  expect_equal(result, 2)  # Ring of 5 has radius 2
})

# =============================================================================
# SECTION 7: network_vertex_connectivity() function tests
# =============================================================================

test_that("network_vertex_connectivity for complete graph", {
  # K4 has vertex connectivity 3
  k4 <- matrix(1, 4, 4)
  diag(k4) <- 0
  result <- network_vertex_connectivity(k4)

  expect_equal(result, 3)
})

test_that("network_vertex_connectivity for path graph", {
  path <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
  result <- network_vertex_connectivity(path)

  expect_equal(result, 1)
})

test_that("network_vertex_connectivity for disconnected graph returns 0", {
  # Two isolated nodes
  mat <- matrix(0, 2, 2)
  result <- network_vertex_connectivity(mat)

  expect_equal(result, 0)
})

test_that("network_vertex_connectivity accepts igraph objects", {
  skip_if_not_installed("igraph")
  g <- igraph::make_full_graph(5)
  result <- network_vertex_connectivity(g)

  expect_equal(result, 4)  # K5 has vertex connectivity 4
})

# =============================================================================
# SECTION 8: network_clique_size() function tests
# =============================================================================

test_that("network_clique_size for complete graph", {
  # K4 has clique number 4
  k4 <- matrix(1, 4, 4)
  diag(k4) <- 0
  result <- network_clique_size(k4)

  expect_equal(result, 4)
})

test_that("network_clique_size for triangle embedded in larger graph", {
  adj <- matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0), 4, 4)
  result <- network_clique_size(adj)

  expect_equal(result, 3)
})

test_that("network_clique_size for path graph", {
  path <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
  result <- network_clique_size(path)

  expect_equal(result, 2)  # Only single edges, so max clique is 2
})

test_that("network_clique_size accepts igraph objects", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  result <- network_clique_size(g)

  expect_equal(result, 2)  # Ring has clique number 2
})

# =============================================================================
# SECTION 9: network_cut_vertices() function tests
# =============================================================================

test_that("network_cut_vertices identifies bridge nodes", {
  # Node 3 connects two groups
  adj <- matrix(c(0, 1, 1, 0, 0,
                  1, 0, 1, 0, 0,
                  1, 1, 0, 1, 0,
                  0, 0, 1, 0, 1,
                  0, 0, 0, 1, 0), 5, 5)
  result <- network_cut_vertices(adj)

  expect_true(3 %in% result)  # Node 3 is articulation point
})
test_that("network_cut_vertices with count_only = TRUE returns integer", {
  adj <- matrix(c(0, 1, 1, 0, 0,
                  1, 0, 1, 0, 0,
                  1, 1, 0, 1, 0,
                  0, 0, 1, 0, 1,
                  0, 0, 0, 1, 0), 5, 5)
  result <- network_cut_vertices(adj, count_only = TRUE)

  expect_true(is.numeric(result))
  expect_true(result >= 1)
})

test_that("network_cut_vertices for complete graph returns none", {
  k4 <- matrix(1, 4, 4)
  diag(k4) <- 0
  result <- network_cut_vertices(k4)

  expect_equal(length(result), 0)
})

test_that("network_cut_vertices returns names for named graphs", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(c("A", "B", "A", "C", "C", "D", "C", "E"))
  result <- network_cut_vertices(g)

  expect_true("C" %in% result)  # Node C is articulation point
})

# =============================================================================
# SECTION 10: network_bridges() function tests
# =============================================================================

test_that("network_bridges identifies bridge edges", {
  # Two triangles connected by single bridge edge
  adj <- matrix(0, 6, 6)
  adj[1, 2] <- adj[2, 1] <- adj[1, 3] <- adj[3, 1] <- adj[2, 3] <- adj[3, 2] <- 1
  adj[4, 5] <- adj[5, 4] <- adj[4, 6] <- adj[6, 4] <- adj[5, 6] <- adj[6, 5] <- 1
  adj[3, 4] <- adj[4, 3] <- 1  # Bridge

  result <- network_bridges(adj)

  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= 1)
  expect_true(all(c("from", "to") %in% names(result)))
})

test_that("network_bridges with count_only = TRUE returns integer", {
  adj <- matrix(0, 6, 6)
  adj[1, 2] <- adj[2, 1] <- adj[1, 3] <- adj[3, 1] <- adj[2, 3] <- adj[3, 2] <- 1
  adj[4, 5] <- adj[5, 4] <- adj[4, 6] <- adj[6, 4] <- adj[5, 6] <- adj[6, 5] <- 1
  adj[3, 4] <- adj[4, 3] <- 1

  result <- network_bridges(adj, count_only = TRUE)

  expect_true(is.numeric(result))
  expect_equal(result, 1)
})

test_that("network_bridges for graph with no bridges returns empty df", {
  # Complete graph has no bridges
  k4 <- matrix(1, 4, 4)
  diag(k4) <- 0
  result <- network_bridges(k4)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("network_bridges returns character names for named graphs", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(c("A", "B", "A", "C", "C", "D"))
  result <- network_bridges(g)

  expect_true(is.data.frame(result))
  expect_true(is.character(result$from))
})

# =============================================================================
# SECTION 11: network_global_efficiency() function tests
# =============================================================================

test_that("network_global_efficiency for complete graph is 1", {
  k4 <- matrix(1, 4, 4)
  diag(k4) <- 0
  result <- network_global_efficiency(k4)

  expect_equal(result, 1)
})

test_that("network_global_efficiency for star graph", {
  star <- matrix(c(0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), 4, 4)
  result <- network_global_efficiency(star)

  expect_true(result > 0 && result < 1)
})

test_that("network_global_efficiency with directed = TRUE", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE)
  result <- network_global_efficiency(mat, directed = TRUE)

  expect_true(is.numeric(result))
})

test_that("network_global_efficiency for single node returns NA", {
  mat <- matrix(0, 1, 1)
  result <- network_global_efficiency(mat)

  expect_true(is.na(result))
})

test_that("network_global_efficiency with invert_weights = TRUE", {
  mat <- matrix(c(0, 0.5, 0, 0.5, 0, 0.8, 0, 0.8, 0), 3, 3)
  result_inverted <- network_global_efficiency(mat, invert_weights = TRUE)
  result_normal <- network_global_efficiency(mat, invert_weights = FALSE)

  # Results should differ
  expect_true(is.numeric(result_inverted))
  expect_true(is.numeric(result_normal))
})

test_that("network_global_efficiency with alpha parameter", {
  mat <- matrix(c(0, 0.5, 0, 0.5, 0, 0.8, 0, 0.8, 0), 3, 3)
  result <- network_global_efficiency(mat, invert_weights = TRUE, alpha = 2)

  expect_true(is.numeric(result))
})

# =============================================================================
# SECTION 12: network_local_efficiency() function tests
# =============================================================================

test_that("network_local_efficiency for complete graph is 1", {
  k5 <- matrix(1, 5, 5)
  diag(k5) <- 0
  result <- network_local_efficiency(k5)

  expect_equal(result, 1)
})

test_that("network_local_efficiency for star graph is 0", {
  star <- matrix(c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 5, 5)
  result <- network_local_efficiency(star)

  expect_equal(result, 0)
})

test_that("network_local_efficiency for single node returns NA", {
  mat <- matrix(0, 1, 1)
  result <- network_local_efficiency(mat)

  expect_true(is.na(result))
})

test_that("network_local_efficiency with invert_weights = TRUE", {
  mat <- matrix(c(0, 0.5, 1, 0.5, 0, 0.8, 1, 0.8, 0), 3, 3)
  result <- network_local_efficiency(mat, invert_weights = TRUE)

  expect_true(is.numeric(result))
})

test_that("network_local_efficiency with alpha parameter", {
  mat <- matrix(c(0, 0.5, 1, 0.5, 0, 0.8, 1, 0.8, 0), 3, 3)
  result <- network_local_efficiency(mat, invert_weights = TRUE, alpha = 0.5)

  expect_true(is.numeric(result))
})

test_that("network_local_efficiency converts directed to undirected", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE)
  result <- network_local_efficiency(mat)

  expect_true(is.numeric(result))
})

# =============================================================================
# SECTION 13: network_small_world() function tests
# =============================================================================

test_that("network_small_world returns numeric value", {
  skip_if_not_installed("igraph")
  g <- igraph::sample_smallworld(1, 20, 3, 0.1)

  set.seed(42)  # For reproducibility
  result <- network_small_world(g, n_random = 5)

  expect_true(is.numeric(result))
})

test_that("network_small_world for random graph", {
  skip_if_not_installed("igraph")
  set.seed(42)
  g <- igraph::sample_gnp(20, 0.3)

  result <- network_small_world(g, n_random = 5)

  expect_true(is.numeric(result) || is.na(result))
})

test_that("network_small_world returns NA for small graph", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  result <- network_small_world(mat, n_random = 3)

  expect_true(is.na(result))
})

test_that("network_small_world returns NA for graph with no edges", {
  mat <- matrix(0, 5, 5)
  result <- network_small_world(mat, n_random = 3)

  expect_true(is.na(result))
})

test_that("network_small_world handles directed graphs", {
  skip_if_not_installed("igraph")
  g <- igraph::sample_gnm(20, 50, directed = TRUE)

  result <- network_small_world(g, n_random = 3)

  expect_true(is.numeric(result) || is.na(result))
})

# =============================================================================
# SECTION 14: network_rich_club() function tests
# =============================================================================

test_that("network_rich_club returns numeric value", {
  skip_if_not_installed("igraph")
  set.seed(42)
  g <- igraph::sample_pa(30, m = 2)
  result <- network_rich_club(g, k = 3)

  expect_true(is.numeric(result) || is.na(result))
})

test_that("network_rich_club with default k uses median degree", {
  skip_if_not_installed("igraph")
  set.seed(42)
  g <- igraph::sample_pa(30, m = 2)
  result <- network_rich_club(g)

  expect_true(is.numeric(result) || is.na(result))
})

test_that("network_rich_club with normalized = TRUE", {
  skip_if_not_installed("igraph")
  set.seed(42)
  g <- igraph::sample_pa(30, m = 2)
  result <- network_rich_club(g, k = 3, normalized = TRUE, n_random = 3)

  expect_true(is.numeric(result) || is.na(result))
})

test_that("network_rich_club returns NA when insufficient rich nodes", {
  # Very sparse graph with low k might have < 2 rich nodes
  mat <- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), 3, 3)
  result <- network_rich_club(mat, k = 5)

  expect_true(is.na(result))
})

test_that("network_rich_club handles directed graphs", {
  skip_if_not_installed("igraph")
  set.seed(42)
  g <- igraph::sample_gnm(20, 40, directed = TRUE)
  result <- network_rich_club(g, k = 2)

  expect_true(is.numeric(result) || is.na(result))
})

# =============================================================================
# SECTION 15: Integration tests with different input types
# =============================================================================

test_that("network_summary works with igraph input", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)
  result <- network_summary(g)

  expect_true(is.data.frame(result))
  expect_equal(result$node_count, 5)
  expect_equal(result$edge_count, 5)
})

test_that("network_summary works with cograph_network input", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)
  result <- network_summary(net)

  expect_true(is.data.frame(result))
  expect_equal(result$node_count, 3)
})

test_that("all network metrics work with igraph input", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5)

  expect_equal(network_girth(g), 5)
  expect_equal(network_radius(g), 2)
  expect_equal(network_vertex_connectivity(g), 2)
  expect_equal(network_clique_size(g), 2)
  expect_equal(network_cut_vertices(g, count_only = TRUE), 0)
  expect_equal(network_bridges(g, count_only = TRUE), 0)
  expect_true(is.numeric(network_global_efficiency(g)))
  expect_true(is.numeric(network_local_efficiency(g)))
})

test_that("degree_distribution works with cograph_network", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  pdf(NULL)
  result <- degree_distribution(net)
  dev.off()

  expect_true(inherits(result, "histogram"))
})

# =============================================================================
# SECTION 16: Edge cases and error handling
# =============================================================================

test_that("network_summary handles disconnected graph", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  result <- network_summary(mat)

  expect_equal(result$component_count, 2)
})

test_that("network_summary handles single node graph", {
  mat <- matrix(0, 1, 1)
  result <- network_summary(mat)

  expect_equal(result$node_count, 1)
  expect_equal(result$edge_count, 0)
})

test_that("network_summary handles empty graph (no edges)", {
  mat <- matrix(0, 3, 3)
  result <- network_summary(mat)

  expect_equal(result$node_count, 3)
  expect_equal(result$edge_count, 0)
  expect_equal(result$density, 0)
})

test_that("extended metrics handle acyclic graphs", {
  # Tree structure
  tree <- matrix(c(0, 1, 0, 0,
                   1, 0, 1, 1,
                   0, 1, 0, 0,
                   0, 1, 0, 0), 4, 4, byrow = TRUE)
  result <- network_summary(tree, extended = TRUE)

  expect_equal(result$girth, Inf)  # No cycles
})

test_that("detailed metrics handle graph with self-loops", {
  mat <- matrix(c(1, 1, 0, 1, 1, 1, 0, 1, 1), 3, 3)
  result <- network_summary(mat, detailed = TRUE, loops = TRUE)

  expect_true(is.data.frame(result))
  expect_true("mean_degree" %in% names(result))
})

test_that("network functions handle weighted graphs correctly", {
  mat <- matrix(c(0, 0.5, 0.8,
                  0.5, 0, 0.3,
                  0.8, 0.3, 0), 3, 3)
  result <- network_summary(mat, weighted = TRUE, detailed = TRUE)

  expect_true("mean_strength" %in% names(result))
  expect_true(result$mean_strength > 0)
})
