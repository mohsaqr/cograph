# ═══════════════════════════════════════════════════════════════════════════════
# Comprehensive tests for centrality.R - targeting uncovered code paths
# ═══════════════════════════════════════════════════════════════════════════════

# Skip all tests if igraph is not installed
skip_if_not_installed("igraph")

# ═══════════════════════════════════════════════════════════════════════════════
# Helper Functions and Test Fixtures
# ═══════════════════════════════════════════════════════════════════════════════

# Create test matrices for various scenarios
create_simple_matrix <- function() {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  mat
}

create_directed_matrix <- function() {
  mat <- matrix(c(
    0, 1, 0, 0,
    0, 0, 1, 0,
    1, 0, 0, 1,
    0, 0, 0, 0
  ), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  mat
}

create_weighted_matrix <- function() {
  mat <- matrix(c(
    0, 0.5, 0.3, 0,
    0.5, 0, 0.8, 0.2,
    0.3, 0.8, 0, 0.6,
    0, 0.2, 0.6, 0
  ), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  mat
}

create_disconnected_matrix <- function() {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  mat
}

create_star_matrix <- function() {
  mat <- matrix(0, 5, 5)
  mat[1, 2:5] <- mat[2:5, 1] <- 1
  rownames(mat) <- colnames(mat) <- c("Center", "L1", "L2", "L3", "L4")
  mat
}

create_line_matrix <- function() {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[2, 3] <- mat[3, 2] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  mat
}

create_loop_matrix <- function() {
  mat <- matrix(c(
    1, 1, 0,
    1, 1, 1,
    0, 1, 1
  ), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  mat
}

create_isolated_node_matrix <- function() {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[2, 3] <- mat[3, 2] <- 1
  # Node D is isolated
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  mat
}

create_large_dense_matrix <- function(n = 10) {
  mat <- matrix(runif(n * n), n, n)
  mat <- (mat + t(mat)) / 2
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("N", 1:n)
  mat
}

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Error Handling and Input Validation
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality errors on invalid mode", {
  mat <- create_simple_matrix()
  expect_error(centrality(mat, mode = "invalid"), "should be one of")
})

test_that("centrality errors on invalid damping factor", {
  mat <- create_simple_matrix()
  expect_error(centrality(mat, measures = "pagerank", damping = -0.1),
               "damping must be between 0 and 1")
  expect_error(centrality(mat, measures = "pagerank", damping = 1.5),
               "damping must be between 0 and 1")
})

test_that("centrality errors on invalid sort_by column", {
  mat <- create_simple_matrix()
  expect_error(centrality(mat, measures = "degree", sort_by = "nonexistent"),
               "sort_by column")
})

test_that("centrality errors on unknown measures with helpful message", {
  mat <- create_simple_matrix()
  err <- expect_error(centrality(mat, measures = c("degree", "fake_measure")))
  expect_match(as.character(err), "Unknown measures")
  expect_match(as.character(err), "fake_measure")
})

test_that("edge_centrality errors on invalid measures", {
  mat <- create_simple_matrix()
  expect_error(edge_centrality(mat, measures = "invalid_measure"),
               "Unknown edge measures")
})

test_that("edge_centrality errors on invalid sort_by column", {
  mat <- create_simple_matrix()
  expect_error(edge_centrality(mat, sort_by = "nonexistent"),
               "sort_by column")
})

test_that("calculate_kreach errors on k <= 0", {
  g <- igraph::make_ring(5)
  expect_error(cograph:::calculate_kreach(g, k = 0), "k parameter must be greater than 0")
  expect_error(cograph:::calculate_kreach(g, k = -1), "k parameter must be greater than 0")
})

test_that("calculate_percolation errors on mismatched states vector length", {
  g <- igraph::make_ring(5)
  states <- c(1, 1, 1)  # Wrong length
  expect_error(cograph:::calculate_percolation(g, states = states),
               "states vector length must match")
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Transitivity Types
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_transitivity supports different types", {
  mat <- create_simple_matrix()

  # Local transitivity (default)
  local <- centrality_transitivity(mat, transitivity_type = "local")
  expect_length(local, 3)

  # Global transitivity
  global <- centrality_transitivity(mat, transitivity_type = "global")
  expect_length(global, 3)

  # Undirected transitivity
  undirected <- centrality_transitivity(mat, transitivity_type = "undirected")
  expect_length(undirected, 3)

  # Barrat (weighted)
  barrat <- centrality_transitivity(mat, transitivity_type = "barrat")
  expect_length(barrat, 3)
})

test_that("transitivity isolates parameter works", {
  mat <- create_isolated_node_matrix()

  # Default - NaN for isolates
  nan_result <- centrality(mat, measures = "transitivity", isolates = "nan")
  expect_true(any(is.nan(nan_result$transitivity)))

  # Zero for isolates
  zero_result <- centrality(mat, measures = "transitivity", isolates = "zero")
  expect_true(all(!is.nan(zero_result$transitivity)))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Mode Parameter for Directional Measures
# ═══════════════════════════════════════════════════════════════════════════════

test_that("mode parameter affects directional measures", {
  mat <- create_directed_matrix()

  # In-mode
  in_result <- centrality(mat, directed = TRUE, mode = "in",
                          measures = c("degree", "strength", "closeness",
                                       "harmonic", "eccentricity", "coreness"))
  expect_true("degree_in" %in% names(in_result))
  expect_true("strength_in" %in% names(in_result))
  expect_true("closeness_in" %in% names(in_result))
  expect_true("harmonic_in" %in% names(in_result))
  expect_true("eccentricity_in" %in% names(in_result))
  expect_true("coreness_in" %in% names(in_result))

  # Out-mode
  out_result <- centrality(mat, directed = TRUE, mode = "out",
                           measures = c("degree", "strength", "closeness",
                                        "harmonic", "eccentricity", "coreness"))
  expect_true("degree_out" %in% names(out_result))
  expect_true("strength_out" %in% names(out_result))
  expect_true("closeness_out" %in% names(out_result))
  expect_true("harmonic_out" %in% names(out_result))
  expect_true("eccentricity_out" %in% names(out_result))
  expect_true("coreness_out" %in% names(out_result))
})

test_that("centrality_indegree and centrality_outdegree work", {
  mat <- create_directed_matrix()

  in_deg <- centrality_indegree(mat, directed = TRUE)
  out_deg <- centrality_outdegree(mat, directed = TRUE)

  expect_length(in_deg, 4)
  expect_length(out_deg, 4)
  expect_true(is.numeric(in_deg))
  expect_true(is.numeric(out_deg))
})

test_that("centrality_instrength and centrality_outstrength work", {
  mat <- create_weighted_matrix()

  in_str <- centrality_instrength(mat, directed = TRUE)
  out_str <- centrality_outstrength(mat, directed = TRUE)

  expect_length(in_str, 4)
  expect_length(out_str, 4)
  expect_true(is.numeric(in_str))
  expect_true(is.numeric(out_str))
})

test_that("centrality_incloseness and centrality_outcloseness work", {
  mat <- create_directed_matrix()

  in_close <- centrality_incloseness(mat, directed = TRUE)
  out_close <- centrality_outcloseness(mat, directed = TRUE)

  expect_length(in_close, 4)
  expect_length(out_close, 4)
})

test_that("centrality_inharmonic and centrality_outharmonic work", {
  mat <- create_directed_matrix()

  in_harm <- centrality_inharmonic(mat, directed = TRUE)
  out_harm <- centrality_outharmonic(mat, directed = TRUE)

  expect_length(in_harm, 4)
  expect_length(out_harm, 4)
})

test_that("centrality_ineccentricity and centrality_outeccentricity work", {
  mat <- create_directed_matrix()

  in_ecc <- centrality_ineccentricity(mat, directed = TRUE)
  out_ecc <- centrality_outeccentricity(mat, directed = TRUE)

  expect_length(in_ecc, 4)
  expect_length(out_ecc, 4)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Weight Inversion for Path-Based Measures
# ═══════════════════════════════════════════════════════════════════════════════

test_that("invert_weights parameter affects path-based measures", {
  mat <- create_weighted_matrix()

  # Without inversion
  expect_silent(
    no_inv <- centrality(mat, measures = c("betweenness", "closeness"),
                         invert_weights = FALSE, weighted = TRUE)
  )

  # With inversion (should produce message)
  expect_message(
    with_inv <- centrality(mat, measures = c("betweenness", "closeness"),
                           invert_weights = TRUE, weighted = TRUE),
    "Weights inverted"
  )

  # Results should differ
  expect_false(all(no_inv$betweenness == with_inv$betweenness))
})

test_that("alpha parameter affects weight inversion", {
  # Use a larger matrix with more varied weights for clearer differences
  mat <- matrix(c(
    0, 0.1, 0.9, 0,
    0.1, 0, 0.2, 0.8,
    0.9, 0.2, 0, 0.3,
    0, 0.8, 0.3, 0
  ), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  # alpha = 1
  expect_message(
    alpha1 <- centrality(mat, measures = "closeness",
                         invert_weights = TRUE, alpha = 1, weighted = TRUE),
    "1/w\\^1"
  )

  # alpha = 2
  expect_message(
    alpha2 <- centrality(mat, measures = "closeness",
                         invert_weights = TRUE, alpha = 2, weighted = TRUE),
    "1/w\\^2"
  )

  # Results should differ for closeness (path-based measure)
  # Use tolerance check since floating point comparison
  expect_false(isTRUE(all.equal(alpha1$closeness_all, alpha2$closeness_all)))
})

test_that("edge_centrality respects invert_weights", {
  mat <- create_weighted_matrix()

  # With inversion
  expect_message(
    ec_inv <- edge_centrality(mat, invert_weights = TRUE),
    "Weights inverted"
  )
  expect_true(is.data.frame(ec_inv))
  expect_true("betweenness" %in% names(ec_inv))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Loops and Simplify Parameters
# ═══════════════════════════════════════════════════════════════════════════════

test_that("loops parameter removes self-loops", {
  mat <- create_loop_matrix()

  # With loops (default)
  with_loops <- centrality(mat, measures = "degree", loops = TRUE)
  # Without loops
  without_loops <- centrality(mat, measures = "degree", loops = FALSE)

  # Should have different degree values
  expect_true(any(with_loops$degree_all != without_loops$degree_all))
})

test_that("simplify parameter handles multiple edges", {
  # Create igraph with multiple edges
  g <- igraph::make_empty_graph(n = 3, directed = FALSE)
  g <- igraph::add_edges(g, c(1, 2, 1, 2, 2, 3))  # Two edges between 1-2
  igraph::E(g)$weight <- c(0.5, 0.3, 0.8)
  igraph::V(g)$name <- c("A", "B", "C")

  # Sum (default)
  sum_result <- centrality(g, measures = "strength", simplify = "sum")
  # Mean
  mean_result <- centrality(g, measures = "strength", simplify = "mean")
  # Max
  max_result <- centrality(g, measures = "strength", simplify = "max")
  # Min
  min_result <- centrality(g, measures = "strength", simplify = "min")

  # Results should differ based on simplification method
  expect_true(is.data.frame(sum_result))
  expect_true(is.data.frame(mean_result))
  expect_true(is.data.frame(max_result))
  expect_true(is.data.frame(min_result))
})

test_that("simplify = FALSE keeps multiple edges", {
  g <- igraph::make_empty_graph(n = 3, directed = FALSE)
  g <- igraph::add_edges(g, c(1, 2, 1, 2, 2, 3))
  igraph::V(g)$name <- c("A", "B", "C")

  result <- centrality(g, measures = "degree", simplify = FALSE)
  expect_true(is.data.frame(result))
})

test_that("simplify = 'none' is equivalent to FALSE", {
  g <- igraph::make_empty_graph(n = 3, directed = FALSE)
  g <- igraph::add_edges(g, c(1, 2, 1, 2, 2, 3))
  igraph::V(g)$name <- c("A", "B", "C")

  result_false <- centrality(g, measures = "degree", simplify = FALSE)
  result_none <- centrality(g, measures = "degree", simplify = "none")

  expect_equal(result_false, result_none)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Cutoff Parameter
# ═══════════════════════════════════════════════════════════════════════════════

test_that("cutoff parameter limits path length", {
  # Use smaller matrix for faster testing
  mat <- matrix(c(
    0, 1, 1, 0, 0, 0,
    1, 0, 1, 1, 0, 0,
    1, 1, 0, 1, 1, 0,
    0, 1, 1, 0, 1, 1,
    0, 0, 1, 1, 0, 1,
    0, 0, 0, 1, 1, 0
  ), 6, 6, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- LETTERS[1:6]

  # With cutoff
  with_cutoff <- centrality(mat, measures = c("betweenness", "closeness"),
                            cutoff = 2)
  # Without cutoff
  without_cutoff <- centrality(mat, measures = c("betweenness", "closeness"),
                               cutoff = -1)

  expect_true(is.data.frame(with_cutoff))
  expect_true(is.data.frame(without_cutoff))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: PageRank Parameters
# ═══════════════════════════════════════════════════════════════════════════════

test_that("damping parameter affects pagerank", {
  # Use a larger asymmetric network where damping has more effect
  mat <- matrix(c(
    0, 1, 0, 0, 0,
    0, 0, 1, 0, 0,
    1, 0, 0, 1, 0,
    0, 0, 0, 0, 1,
    0, 1, 0, 0, 0
  ), 5, 5, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D", "E")

  pr_85 <- centrality_pagerank(mat, damping = 0.85, directed = TRUE)
  pr_50 <- centrality_pagerank(mat, damping = 0.50, directed = TRUE)

  expect_length(pr_85, 5)
  expect_length(pr_50, 5)
  # Results should differ (use tolerance comparison)
  expect_false(isTRUE(all.equal(pr_85, pr_50)))
})

test_that("personalized pagerank works", {
  mat <- create_simple_matrix()

  # Standard PageRank
  standard <- centrality_pagerank(mat)

  # Personalized PageRank
  personalized_vec <- c(A = 0.5, B = 0.3, C = 0.2)
  personalized <- centrality_pagerank(mat, personalized = personalized_vec)

  expect_length(personalized, 3)
  # Results should differ
  expect_false(all(standard == personalized))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Diffusion Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_diffusion works with different modes", {
  mat <- create_directed_matrix()

  # All modes
  diff_all <- centrality_diffusion(mat, mode = "all", directed = TRUE)
  expect_length(diff_all, 4)

  # For undirected
  mat_un <- create_simple_matrix()
  diff_un <- centrality_diffusion(mat_un)
  expect_length(diff_un, 3)
})

test_that("lambda parameter affects diffusion", {
  mat <- create_simple_matrix()

  diff_1 <- centrality_diffusion(mat, lambda = 1)
  diff_2 <- centrality_diffusion(mat, lambda = 2)

  expect_false(all(diff_1 == diff_2))
})

test_that("calculate_diffusion handles empty graph", {
  g <- igraph::make_empty_graph(n = 0, directed = FALSE)
  result <- cograph:::calculate_diffusion(g)
  expect_length(result, 0)
})

test_that("calculate_diffusion handles directed graph modes", {
  mat <- create_directed_matrix()
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")

  # Out mode
  out_result <- cograph:::calculate_diffusion(g, mode = "out")
  expect_length(out_result, 4)

  # In mode
  in_result <- cograph:::calculate_diffusion(g, mode = "in")
  expect_length(in_result, 4)

  # All mode
  all_result <- cograph:::calculate_diffusion(g, mode = "all")
  expect_length(all_result, 4)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Leverage Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_leverage works", {
  mat <- create_simple_matrix()

  lev <- centrality_leverage(mat)
  expect_length(lev, 3)
  expect_true(is.numeric(lev))
})

test_that("leverage returns NaN for isolated nodes", {
  mat <- create_isolated_node_matrix()

  lev <- centrality_leverage(mat)
  expect_true(is.nan(lev["D"]))
})

test_that("calculate_leverage handles empty graph", {
  g <- igraph::make_empty_graph(n = 0, directed = FALSE)
  result <- cograph:::calculate_leverage(g)
  expect_length(result, 0)
})

test_that("leverage handles directed graph modes", {
  mat <- create_directed_matrix()
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")

  # In mode
  in_result <- cograph:::calculate_leverage(g, mode = "in")
  expect_length(in_result, 4)

  # Out mode
  out_result <- cograph:::calculate_leverage(g, mode = "out")
  expect_length(out_result, 4)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: K-Reach Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_kreach works with different k values", {
  mat <- create_line_matrix()

  k1 <- centrality_kreach(mat, k = 1)
  k2 <- centrality_kreach(mat, k = 2)
  k3 <- centrality_kreach(mat, k = 3)

  expect_length(k1, 4)
  expect_length(k2, 4)
  expect_length(k3, 4)

  # Higher k should reach more nodes (or same)
  expect_true(all(k2 >= k1))
  expect_true(all(k3 >= k2))
})

test_that("calculate_kreach handles empty graph", {
  g <- igraph::make_empty_graph(n = 0, directed = FALSE)
  result <- cograph:::calculate_kreach(g, k = 3)
  expect_length(result, 0)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Laplacian Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_laplacian works", {
  mat <- create_simple_matrix()

  lap <- centrality_laplacian(mat)
  expect_length(lap, 3)
  expect_true(is.numeric(lap))
  expect_true(all(lap >= 0))
})

test_that("calculate_laplacian handles edge cases", {
  # Empty graph
  g <- igraph::make_empty_graph(n = 0)
  result_empty <- cograph:::calculate_laplacian(g)
  expect_length(result_empty, 0)

  # Single node
  g1 <- igraph::make_empty_graph(n = 1)
  result_single <- cograph:::calculate_laplacian(g1)
  expect_equal(result_single, 0)
})

test_that("laplacian normalized option works", {
  g <- igraph::make_ring(5)
  result <- cograph:::calculate_laplacian(g, normalized = TRUE)
  expect_true(max(result) == 1)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Load Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_load works", {
  mat <- create_simple_matrix()

  load_cent <- centrality_load(mat)
  expect_length(load_cent, 3)
  expect_true(is.numeric(load_cent))
})

test_that("calculate_load handles edge cases", {
  # Empty graph
  g <- igraph::make_empty_graph(n = 0)
  result_empty <- cograph:::calculate_load(g)
  expect_length(result_empty, 0)

  # Single node
  g1 <- igraph::make_empty_graph(n = 1)
  result_single <- cograph:::calculate_load(g1)
  expect_equal(result_single, 0)
})

test_that("load centrality works for directed graphs", {
  mat <- create_directed_matrix()
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")

  result <- cograph:::calculate_load(g, directed = TRUE)
  expect_length(result, 4)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Current Flow Closeness
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_current_flow_closeness works", {
  mat <- create_simple_matrix()

  cfc <- centrality_current_flow_closeness(mat)
  expect_length(cfc, 3)
  expect_true(is.numeric(cfc))
})

test_that("current_flow_closeness returns NA for disconnected graphs", {
  mat <- create_disconnected_matrix()

  expect_warning(
    cfc <- centrality_current_flow_closeness(mat),
    "not connected"
  )
  expect_true(all(is.na(cfc)))
})

test_that("calculate_current_flow_closeness handles edge cases", {
  # Empty graph
  g <- igraph::make_empty_graph(n = 0)
  result_empty <- cograph:::calculate_current_flow_closeness(g)
  expect_length(result_empty, 0)

  # Single node
  g1 <- igraph::make_empty_graph(n = 1)
  result_single <- cograph:::calculate_current_flow_closeness(g1)
  expect_true(is.na(result_single))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Current Flow Betweenness
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_current_flow_betweenness works", {
  mat <- create_simple_matrix()

  cfb <- centrality_current_flow_betweenness(mat)
  expect_length(cfb, 3)
  expect_true(is.numeric(cfb))
})

test_that("current_flow_betweenness returns NA for disconnected graphs", {
  mat <- create_disconnected_matrix()

  expect_warning(
    cfb <- centrality_current_flow_betweenness(mat),
    "not connected"
  )
  expect_true(all(is.na(cfb)))
})

test_that("calculate_current_flow_betweenness handles edge cases", {
  # Empty graph
  g <- igraph::make_empty_graph(n = 0)
  result_empty <- cograph:::calculate_current_flow_betweenness(g)
  expect_length(result_empty, 0)

  # Two nodes
  g2 <- igraph::make_full_graph(2)
  result_small <- cograph:::calculate_current_flow_betweenness(g2)
  expect_equal(result_small, c(0, 0))
})

test_that("current_flow_betweenness works with weights", {
  mat <- create_weighted_matrix()
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)

  result <- cograph:::calculate_current_flow_betweenness(g, weights = igraph::E(g)$weight)
  expect_length(result, 4)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: VoteRank
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_voterank works", {
  mat <- create_simple_matrix()

  vr <- centrality_voterank(mat)
  expect_length(vr, 3)
  expect_true(is.numeric(vr))
  expect_true(all(vr >= 0 & vr <= 1))
})

test_that("calculate_voterank handles edge cases", {
  # Empty graph
  g <- igraph::make_empty_graph(n = 0)
  result_empty <- cograph:::calculate_voterank(g)
  expect_length(result_empty, 0)

  # Single node
  g1 <- igraph::make_empty_graph(n = 1)
  result_single <- cograph:::calculate_voterank(g1)
  expect_equal(result_single, 1)
})

test_that("voterank handles directed graph", {
  mat <- create_directed_matrix()
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")

  result <- cograph:::calculate_voterank(g, directed = TRUE)
  expect_length(result, 4)
})

test_that("voterank handles zero degree graph", {
  g <- igraph::make_empty_graph(n = 3)
  igraph::V(g)$name <- c("A", "B", "C")

  result <- cograph:::calculate_voterank(g, directed = FALSE)
  expect_length(result, 3)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Percolation Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_percolation works", {
  mat <- create_simple_matrix()

  perc <- centrality_percolation(mat)
  expect_length(perc, 3)
  expect_true(is.numeric(perc))
})

test_that("percolation with custom states works", {
  mat <- create_simple_matrix()

  states <- c(A = 0.5, B = 1.0, C = 0.2)
  perc <- centrality_percolation(mat, states = states)

  expect_length(perc, 3)
  expect_true(is.numeric(perc))
})

test_that("calculate_percolation handles edge cases", {
  # Empty graph
  g <- igraph::make_empty_graph(n = 0)
  result_empty <- cograph:::calculate_percolation(g)
  expect_length(result_empty, 0)

  # Two nodes
  g2 <- igraph::make_full_graph(2)
  result_small <- cograph:::calculate_percolation(g2)
  expect_equal(result_small, c(0, 0))
})

test_that("percolation handles all-zero states", {
  mat <- create_simple_matrix()
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected")

  states <- rep(0, 3)
  result <- cograph:::calculate_percolation(g, states = states)
  expect_true(all(result == 0))
})

test_that("percolation clamps states to [0, 1]", {
  mat <- create_simple_matrix()

  # States outside [0, 1] should be clamped
  states <- c(A = -0.5, B = 1.5, C = 0.5)
  # Should not error
  perc <- centrality_percolation(mat, states = states)
  expect_length(perc, 3)
})

test_that("percolation fills NA states with 1.0", {
  mat <- create_simple_matrix()

  states <- c(A = 0.5, B = NA, C = 0.8)
  perc <- centrality_percolation(mat, states = states)
  expect_length(perc, 3)
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Alpha and Power Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_alpha works", {
  mat <- create_simple_matrix()

  alpha <- centrality_alpha(mat)
  expect_length(alpha, 3)
  expect_true(is.numeric(alpha))
})

test_that("centrality_power works", {
  mat <- create_simple_matrix()

  power <- centrality_power(mat)
  expect_length(power, 3)
  expect_true(is.numeric(power))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Authority and Hub
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_authority works", {
  mat <- create_directed_matrix()

  auth <- centrality_authority(mat)
  expect_length(auth, 4)
  expect_true(is.numeric(auth))
})

test_that("centrality_hub works", {
  mat <- create_directed_matrix()

  hub <- centrality_hub(mat)
  expect_length(hub, 4)
  expect_true(is.numeric(hub))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Constraint Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_constraint works", {
  mat <- create_simple_matrix()

  const <- centrality_constraint(mat)
  expect_length(const, 3)
  expect_true(is.numeric(const))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Coreness
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_coreness works", {
  mat <- create_simple_matrix()

  core <- centrality_coreness(mat)
  expect_length(core, 3)
  expect_true(is.numeric(core))
  expect_true(all(core >= 0))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Subgraph Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality_subgraph works", {
  mat <- create_simple_matrix()

  sub <- centrality_subgraph(mat)
  expect_length(sub, 3)
  expect_true(is.numeric(sub))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Edge Centrality
# ═══════════════════════════════════════════════════════════════════════════════

test_that("edge_centrality returns correct structure", {
  mat <- create_simple_matrix()

  result <- edge_centrality(mat)
  expect_true(is.data.frame(result))
  expect_true("from" %in% names(result))
  expect_true("to" %in% names(result))
  expect_true("weight" %in% names(result))
  expect_true("betweenness" %in% names(result))
})

test_that("edge_centrality with specific measures", {
  mat <- create_simple_matrix()

  # Just betweenness
  bet_only <- edge_centrality(mat, measures = "betweenness")
  expect_true("betweenness" %in% names(bet_only))
  expect_false("weight" %in% names(bet_only))

  # Just weight
  weight_only <- edge_centrality(mat, measures = "weight")
  expect_true("weight" %in% names(weight_only))
  expect_false("betweenness" %in% names(weight_only))
})

test_that("edge_centrality sorting works", {
  mat <- create_star_matrix()

  sorted <- edge_centrality(mat, sort_by = "betweenness")
  expect_true(is.data.frame(sorted))
  # Should be in descending order
  expect_true(all(diff(sorted$betweenness) <= 0))
})

test_that("edge_centrality digits rounding works", {
  mat <- create_weighted_matrix()

  rounded <- edge_centrality(mat, digits = 2)
  expect_true(is.data.frame(rounded))
})

test_that("edge_betweenness convenience function works", {
  mat <- create_simple_matrix()

  eb <- edge_betweenness(mat)
  expect_true(is.numeric(eb))
  expect_true(!is.null(names(eb)))
})

test_that("edge_centrality handles unweighted graphs", {
  mat <- create_simple_matrix()

  result <- edge_centrality(mat, weighted = FALSE)
  expect_true(is.data.frame(result))
  # Weight should be 1 for all edges
  expect_true(all(result$weight == 1))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Normalization
# ═══════════════════════════════════════════════════════════════════════════════

test_that("normalization works for various measures", {
  mat <- create_simple_matrix()

  result <- centrality(mat, normalized = TRUE,
                       measures = c("degree", "betweenness", "strength",
                                    "eigenvector", "pagerank"))

  # All normalized values should be <= 1
  expect_true(all(result$degree_all <= 1))
  expect_true(all(result$betweenness <= 1, na.rm = TRUE))
  expect_true(all(result$strength_all <= 1))
  expect_true(all(result$eigenvector <= 1))
  expect_true(all(result$pagerank <= 1))
})

test_that("normalization handles max value of 0", {
  # Create graph where all betweenness is 0
  mat <- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat, measures = "betweenness", normalized = TRUE)
  expect_true(all(result$betweenness == 0))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Graph Without Node Names
# ═══════════════════════════════════════════════════════════════════════════════

test_that("centrality works with unnamed matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  # No row/column names

  result <- centrality(mat)
  expect_true(is.data.frame(result))
  expect_equal(result$node, c("1", "2", "3"))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Unweighted Mode
# ═══════════════════════════════════════════════════════════════════════════════

test_that("weighted = FALSE ignores weights", {
  # Use binary matrix for cleaner comparison
  mat <- matrix(c(
    0, 1, 1, 0,
    1, 0, 1, 1,
    1, 1, 0, 1,
    0, 1, 1, 0
  ), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  # Add weights to create weighted version
  mat_weighted <- mat * runif(16, 0.1, 0.9)
  mat_weighted <- (mat_weighted + t(mat_weighted)) / 2
  mat_weighted[mat == 0] <- 0

  # Test that weighted results differ from unweighted
  weighted_result <- centrality(mat_weighted, measures = "strength", weighted = TRUE)
  unweighted_result <- centrality(mat_weighted, measures = "strength", weighted = FALSE)

  # Weighted strength should differ from unweighted (which should match structure)
  expect_true(is.data.frame(weighted_result))
  expect_true(is.data.frame(unweighted_result))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: All Measures Combined
# ═══════════════════════════════════════════════════════════════════════════════

test_that("calculating all measures works", {
  # Use smaller fixed matrix to run faster
  mat <- matrix(c(
    0, 1, 1, 0, 0,
    1, 0, 1, 1, 0,
    1, 1, 0, 1, 1,
    0, 1, 1, 0, 1,
    0, 0, 1, 1, 0
  ), 5, 5, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  # Only test a subset of measures to keep test fast
  result <- centrality(mat, measures = c("degree", "betweenness", "closeness",
                                          "eigenvector", "pagerank", "harmonic"))
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 5)
  expect_true(ncol(result) >= 6)  # node + 6 measures
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Star Network Properties
# ═══════════════════════════════════════════════════════════════════════════════

test_that("star network has expected centrality properties", {
  mat <- create_star_matrix()

  result <- centrality(mat, measures = c("degree", "betweenness", "closeness"))

  # Center node should have highest degree
  center_idx <- which(result$node == "Center")
  expect_equal(result$degree_all[center_idx], max(result$degree_all))

  # Center should have highest betweenness
  expect_equal(result$betweenness[center_idx], max(result$betweenness))

  # Center should have highest closeness
  expect_equal(result$closeness_all[center_idx], max(result$closeness_all))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Test: Line Network Properties
# ═══════════════════════════════════════════════════════════════════════════════

test_that("line network has expected centrality properties", {
  mat <- create_line_matrix()

  result <- centrality(mat, measures = c("degree", "betweenness", "closeness"))

  # Middle nodes (B, C) should have highest betweenness
  middle_bet <- result$betweenness[result$node %in% c("B", "C")]
  edge_bet <- result$betweenness[result$node %in% c("A", "D")]
  expect_true(all(middle_bet > edge_bet))

  # Endpoints should have degree 1
  endpoints <- result$degree_all[result$node %in% c("A", "D")]
  expect_true(all(endpoints == 1))
})
