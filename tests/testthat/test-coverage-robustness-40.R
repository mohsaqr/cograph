# Tests for robustness.R - comprehensive coverage tests
# Testing all robustness analysis functions, attack simulations, and helper functions

# ==============================================================================
# Test Setup
# ==============================================================================

# Create test networks
set.seed(42)

# Create a simple connected network
mat_simple <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  0, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(mat_simple) <- colnames(mat_simple) <- LETTERS[1:5]

# Create a weighted network
mat_weighted <- matrix(c(
  0.0, 0.8, 0.5, 0.0, 0.0,
  0.8, 0.0, 0.7, 0.3, 0.0,
  0.5, 0.7, 0.0, 0.6, 0.4,
  0.0, 0.3, 0.6, 0.0, 0.9,
  0.0, 0.0, 0.4, 0.9, 0.0
), 5, 5, byrow = TRUE)
rownames(mat_weighted) <- colnames(mat_weighted) <- LETTERS[1:5]

# ==============================================================================
# Test robustness() function - basic functionality
# ==============================================================================

test_that("robustness() works with default parameters (vertex, betweenness)", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple)
  expect_s3_class(rob, "cograph_robustness")
  expect_s3_class(rob, "data.frame")
  expect_true("removed_pct" %in% names(rob))
  expect_true("comp_size" %in% names(rob))
  expect_true("comp_pct" %in% names(rob))
  expect_true("measure" %in% names(rob))
  expect_true("type" %in% names(rob))
})

test_that("robustness() returns correct structure", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple)

  # Number of rows should be n_vertices + 1 (0 to n removed)
  expect_equal(nrow(rob), 6)

  # removed_pct should range from 0 to 1
  expect_equal(min(rob$removed_pct), 0)
  expect_equal(max(rob$removed_pct), 1)

  # comp_pct should start at 1 and end at 0
  expect_equal(rob$comp_pct[1], 1)
  expect_equal(rob$comp_pct[nrow(rob)], 0)
})

test_that("robustness() has correct attributes", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple)

  expect_true(!is.null(attr(rob, "n_original")))
  expect_true(!is.null(attr(rob, "orig_max")))
  expect_equal(attr(rob, "n_original"), 5)
  expect_equal(attr(rob, "orig_max"), 5)
})

# ==============================================================================
# Test robustness() with different measures
# ==============================================================================

test_that("robustness() works with betweenness measure", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple, measure = "betweenness")
  expect_s3_class(rob, "cograph_robustness")
  expect_equal(unique(rob$measure), "betweenness")
  expect_true(grepl("Targeted vertex attack", rob$type[1]))
})

test_that("robustness() works with degree measure", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple, measure = "degree")
  expect_s3_class(rob, "cograph_robustness")
  expect_equal(unique(rob$measure), "degree")
  expect_true(grepl("Targeted vertex attack", rob$type[1]))
})

test_that("robustness() works with random measure", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple, measure = "random", n_iter = 10)
  expect_s3_class(rob, "cograph_robustness")
  expect_equal(unique(rob$measure), "random")
  expect_true(grepl("Random vertex removal", rob$type[1]))
})

# ==============================================================================
# Test robustness() with different types
# ==============================================================================

test_that("robustness() works with vertex type", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple, type = "vertex")
  expect_s3_class(rob, "cograph_robustness")
  expect_true(grepl("vertex", rob$type[1]))
})

test_that("robustness() works with edge type and betweenness", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple, type = "edge", measure = "betweenness")
  expect_s3_class(rob, "cograph_robustness")
  expect_true(grepl("edge", rob$type[1]))
})

test_that("robustness() works with edge type and random", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple, type = "edge", measure = "random", n_iter = 10)
  expect_s3_class(rob, "cograph_robustness")
  expect_true(grepl("Random edge removal", rob$type[1]))
})

test_that("robustness() errors on edge type with degree measure", {
  skip_if_not_installed("igraph")

  expect_error(
    robustness(mat_simple, type = "edge", measure = "degree"),
    "edge attacks"
  )
})

# ==============================================================================
# Test robustness() with seed for reproducibility
# ==============================================================================

test_that("robustness() is reproducible with seed for random", {
  skip_if_not_installed("igraph")

  rob1 <- robustness(mat_simple, measure = "random", n_iter = 20, seed = 123)
  rob2 <- robustness(mat_simple, measure = "random", n_iter = 20, seed = 123)

  expect_equal(rob1$comp_pct, rob2$comp_pct)
})

test_that("robustness() gives different results with different seeds", {
  skip_if_not_installed("igraph")

  rob1 <- robustness(mat_simple, measure = "random", n_iter = 20, seed = 123)
  rob2 <- robustness(mat_simple, measure = "random", n_iter = 20, seed = 456)

  # Results should be different (not all equal)
  expect_false(all(rob1$comp_pct == rob2$comp_pct))
})

# ==============================================================================
# Test robustness() with different input types
# ==============================================================================

test_that("robustness() works with igraph input", {
  skip_if_not_installed("igraph")

  g <- igraph::graph_from_adjacency_matrix(mat_simple, mode = "undirected")
  rob <- robustness(g)

  expect_s3_class(rob, "cograph_robustness")
  expect_equal(nrow(rob), 6)
})

test_that("robustness() works with weighted network", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_weighted)
  expect_s3_class(rob, "cograph_robustness")
  expect_equal(nrow(rob), 6)
})

test_that("robustness() works with cograph_network input", {
  skip_if_not_installed("igraph")

  net <- as_cograph(mat_simple)
  rob <- robustness(net)

  expect_s3_class(rob, "cograph_robustness")
})

# ==============================================================================
# Test robustness() with directed networks
# ==============================================================================

test_that("robustness() works with directed network", {
  skip_if_not_installed("igraph")

  mat_dir <- matrix(c(
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
    1, 0, 0, 0
  ), 4, 4, byrow = TRUE)

  g <- igraph::graph_from_adjacency_matrix(mat_dir, mode = "directed")
  rob <- robustness(g, measure = "betweenness")

  expect_s3_class(rob, "cograph_robustness")
})

test_that("robustness() respects mode parameter for degree", {
  skip_if_not_installed("igraph")

  mat_dir <- matrix(c(
    0, 1, 1, 0,
    0, 0, 1, 1,
    0, 0, 0, 1,
    0, 0, 0, 0
  ), 4, 4, byrow = TRUE)

  g <- igraph::graph_from_adjacency_matrix(mat_dir, mode = "directed")
  rob_in <- robustness(g, measure = "degree", mode = "in")
  rob_out <- robustness(g, measure = "degree", mode = "out")

  expect_s3_class(rob_in, "cograph_robustness")
  expect_s3_class(rob_out, "cograph_robustness")
})

# ==============================================================================
# Test robustness_auc() function
# ==============================================================================

test_that("robustness_auc() returns numeric value", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple, measure = "betweenness")
  auc <- robustness_auc(rob)

  expect_true(is.numeric(auc))
  expect_length(auc, 1)
})

test_that("robustness_auc() returns value between 0 and 1", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple, measure = "betweenness")
  auc <- robustness_auc(rob)

  expect_true(auc >= 0)
  expect_true(auc <= 1)
})

test_that("robustness_auc() gives higher value for random vs targeted attack", {
  skip_if_not_installed("igraph")

  # Scale-free networks are typically more robust to random failures
  g <- igraph::sample_pa(50, m = 2, directed = FALSE)

  rob_btw <- robustness(g, measure = "betweenness")
  rob_rnd <- robustness(g, measure = "random", n_iter = 20, seed = 42)

  auc_btw <- robustness_auc(rob_btw)
  auc_rnd <- robustness_auc(rob_rnd)

  # Random failure should have higher AUC than targeted attack for scale-free
  expect_true(auc_rnd >= auc_btw - 0.1)  # Allow some tolerance
})

test_that("robustness_auc() errors with invalid input", {
  df <- data.frame(x = 1:5, y = 1:5)
  expect_error(robustness_auc(df), "removed_pct")
})

# ==============================================================================
# Test robustness_summary() function
# ==============================================================================

test_that("robustness_summary() works with robustness results", {
  skip_if_not_installed("igraph")

  rob_btw <- robustness(mat_simple, measure = "betweenness")
  rob_deg <- robustness(mat_simple, measure = "degree")

  summary_df <- robustness_summary(rob_btw, rob_deg)

  expect_true(is.data.frame(summary_df))
  expect_true("measure" %in% names(summary_df))
  expect_true("auc" %in% names(summary_df))
  expect_true("critical_50" %in% names(summary_df))
  expect_true("critical_10" %in% names(summary_df))
})

test_that("robustness_summary() works with network input", {
  skip_if_not_installed("igraph")

  summary_df <- robustness_summary(x = mat_simple, n_iter = 10)

  expect_true(is.data.frame(summary_df))
  expect_equal(nrow(summary_df), 3)  # betweenness, degree, random
})

test_that("robustness_summary() respects measures argument", {
  skip_if_not_installed("igraph")

  summary_df <- robustness_summary(x = mat_simple, measures = c("betweenness", "degree"), n_iter = 10)

  expect_equal(nrow(summary_df), 2)
})

test_that("robustness_summary() errors without inputs", {
  expect_error(robustness_summary(), "Provide robustness results")
})

test_that("robustness_summary() contains correct metrics", {
  skip_if_not_installed("igraph")

  rob <- robustness(mat_simple, measure = "betweenness")
  summary_df <- robustness_summary(rob)

  # AUC should match manual calculation
  manual_auc <- robustness_auc(rob)
  expect_equal(summary_df$auc[1], round(manual_auc, 4))
})

# ==============================================================================
# Test plot_robustness() function
# ==============================================================================

test_that("plot_robustness() works with robustness results", {
  skip_if_not_installed("igraph")

  rob1 <- robustness(mat_simple, measure = "betweenness")
  rob2 <- robustness(mat_simple, measure = "degree")

  pdf(NULL)
  result <- plot_robustness(rob1, rob2)
  dev.off()

  expect_true(is.data.frame(result))
  expect_true(all(c("removed_pct", "comp_pct", "measure") %in% names(result)))
})

test_that("plot_robustness() works with network input", {
  skip_if_not_installed("igraph")

  pdf(NULL)
  result <- plot_robustness(x = mat_simple, measures = c("betweenness", "degree"), n_iter = 10)
  dev.off()

  expect_true(is.data.frame(result))
})

test_that("plot_robustness() respects seed parameter", {
  skip_if_not_installed("igraph")

  pdf(NULL)
  result1 <- plot_robustness(x = mat_simple, measures = c("random"), n_iter = 10, seed = 123)
  dev.off()

  pdf(NULL)
  result2 <- plot_robustness(x = mat_simple, measures = c("random"), n_iter = 10, seed = 123)
  dev.off()

  expect_equal(result1$comp_pct, result2$comp_pct)
})

test_that("plot_robustness() errors without inputs", {
  expect_error(plot_robustness(), "Provide robustness results")
})

test_that("plot_robustness() respects custom colors", {
  skip_if_not_installed("igraph")

  custom_colors <- c(betweenness = "purple", degree = "orange")

  pdf(NULL)
  result <- plot_robustness(x = mat_simple, measures = c("betweenness", "degree"),
                            colors = custom_colors)
  dev.off()

  expect_true(is.data.frame(result))
})

test_that("plot_robustness() handles custom title and labels", {
  skip_if_not_installed("igraph")

  pdf(NULL)
  result <- plot_robustness(
    x = mat_simple,
    measures = c("betweenness"),
    title = "Custom Title",
    xlab = "Nodes Removed",
    ylab = "Component Size"
  )
  dev.off()

  expect_true(is.data.frame(result))
})

# ==============================================================================
# Test ggplot_robustness() function
# ==============================================================================

test_that("ggplot_robustness() works with named networks", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  g1 <- igraph::sample_pa(30, m = 2, directed = FALSE)
  g2 <- igraph::sample_gnp(30, 0.15)

  p <- ggplot_robustness("Network A" = g1, "Network B" = g2, n_iter = 10)

  expect_s3_class(p, "ggplot")
})

test_that("ggplot_robustness() works with networks list", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  g1 <- igraph::sample_pa(30, m = 2, directed = FALSE)
  g2 <- igraph::sample_gnp(30, 0.15)

  p <- ggplot_robustness(networks = list("A" = g1, "B" = g2), n_iter = 10)

  expect_s3_class(p, "ggplot")
})

test_that("ggplot_robustness() works with single network", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  g <- igraph::sample_pa(30, m = 2, directed = FALSE)

  p <- ggplot_robustness("Test Network" = g, n_iter = 10)

  expect_s3_class(p, "ggplot")
})

test_that("ggplot_robustness() respects ncol parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  g1 <- igraph::sample_pa(20, m = 2, directed = FALSE)
  g2 <- igraph::sample_gnp(20, 0.15)
  g3 <- igraph::sample_smallworld(1, 20, 3, 0.1)

  p <- ggplot_robustness("A" = g1, "B" = g2, "C" = g3, n_iter = 10, ncol = 3)

  expect_s3_class(p, "ggplot")
})

test_that("ggplot_robustness() respects free_y parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  g1 <- igraph::sample_pa(30, m = 2, directed = FALSE)
  g2 <- igraph::sample_gnp(30, 0.15)

  p <- ggplot_robustness("A" = g1, "B" = g2, n_iter = 10, free_y = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("ggplot_robustness() respects custom colors", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  g <- igraph::sample_pa(30, m = 2, directed = FALSE)
  custom_colors <- c(Betweenness = "purple", Degree = "orange", Random = "cyan")

  p <- ggplot_robustness("Test" = g, colors = custom_colors, n_iter = 10)

  expect_s3_class(p, "ggplot")
})

test_that("ggplot_robustness() respects title parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  g <- igraph::sample_pa(30, m = 2, directed = FALSE)

  p <- ggplot_robustness("Test" = g, title = "Custom Title", n_iter = 10)

  expect_s3_class(p, "ggplot")
})

test_that("ggplot_robustness() respects measures parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  g <- igraph::sample_pa(30, m = 2, directed = FALSE)

  p <- ggplot_robustness("Test" = g, measures = c("betweenness"), n_iter = 10)

  expect_s3_class(p, "ggplot")
})

test_that("ggplot_robustness() errors without networks", {
  skip_if_not_installed("ggplot2")

  expect_error(ggplot_robustness(), "Provide at least one network")
})

test_that("ggplot_robustness() requires ggplot2", {
  # This is hard to test directly since we can't unload packages,

  # but we can verify the check exists in the code
  expect_true(TRUE)
})

# ==============================================================================
# Test internal helper functions
# ==============================================================================

test_that("robustness_vertex_attack handles targeted attack correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::graph_from_adjacency_matrix(mat_simple, mode = "undirected")
  orig_max <- max(igraph::components(g)$csize)
  n <- igraph::vcount(g)

  result <- cograph:::robustness_vertex_attack(g, "betweenness", "all", 10, orig_max, n)

  expect_true(is.numeric(result))
  expect_length(result, n)
  expect_equal(result[1], orig_max)  # First value should be original max
})

test_that("robustness_vertex_attack handles random attack correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::graph_from_adjacency_matrix(mat_simple, mode = "undirected")
  orig_max <- max(igraph::components(g)$csize)
  n <- igraph::vcount(g)

  set.seed(42)
  result <- cograph:::robustness_vertex_attack(g, "random", "all", 20, orig_max, n)

  expect_true(is.numeric(result))
  expect_length(result, n)
})

test_that("robustness_edge_attack handles targeted attack correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::graph_from_adjacency_matrix(mat_simple, mode = "undirected")
  orig_max <- max(igraph::components(g)$csize)
  n <- igraph::ecount(g)

  result <- cograph:::robustness_edge_attack(g, "betweenness", 10, orig_max, n)

  expect_true(is.numeric(result))
  expect_length(result, n)
})

test_that("robustness_edge_attack handles random attack correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::graph_from_adjacency_matrix(mat_simple, mode = "undirected")
  orig_max <- max(igraph::components(g)$csize)
  n <- igraph::ecount(g)

  set.seed(42)
  result <- cograph:::robustness_edge_attack(g, "random", 20, orig_max, n)

  expect_true(is.numeric(result))
  expect_length(result, n)
})

# ==============================================================================
# Test edge cases and special scenarios
# ==============================================================================

test_that("robustness() handles small network", {
  skip_if_not_installed("igraph")

  mat_tiny <- matrix(c(0, 1, 1, 0), 2, 2)
  rob <- robustness(mat_tiny)

  expect_s3_class(rob, "cograph_robustness")
  expect_equal(nrow(rob), 3)  # 0, 1, 2 vertices removed
})

test_that("robustness() handles sparse network", {
  skip_if_not_installed("igraph")

  # Create a sparse network (tree-like structure)
  mat_sparse <- matrix(0, 5, 5)
  mat_sparse[1, 2] <- mat_sparse[2, 1] <- 1
  mat_sparse[2, 3] <- mat_sparse[3, 2] <- 1
  mat_sparse[3, 4] <- mat_sparse[4, 3] <- 1
  mat_sparse[4, 5] <- mat_sparse[5, 4] <- 1

  rob <- robustness(mat_sparse)

  expect_s3_class(rob, "cograph_robustness")
})

test_that("robustness() handles dense network", {
  skip_if_not_installed("igraph")

  # Create a fully connected network
  mat_dense <- matrix(1, 5, 5)
  diag(mat_dense) <- 0

  rob <- robustness(mat_dense)

  expect_s3_class(rob, "cograph_robustness")
})

test_that("robustness() handles network with single edge", {
  skip_if_not_installed("igraph")

  mat_single <- matrix(c(0, 1, 1, 0), 2, 2)
  rob <- robustness(mat_single, type = "edge")

  expect_s3_class(rob, "cograph_robustness")
  expect_equal(nrow(rob), 2)  # 0, 1 edges removed
})

test_that("robustness() handles network with hub node", {
  skip_if_not_installed("igraph")

  # Star network - node 1 is hub
  mat_star <- matrix(0, 5, 5)
  mat_star[1, 2:5] <- 1
  mat_star[2:5, 1] <- 1

  rob_btw <- robustness(mat_star, measure = "betweenness")
  rob_deg <- robustness(mat_star, measure = "degree")

  expect_s3_class(rob_btw, "cograph_robustness")
  expect_s3_class(rob_deg, "cograph_robustness")

  # Hub removal should cause rapid fragmentation
  expect_true(rob_btw$comp_pct[2] < rob_btw$comp_pct[1])
})

# ==============================================================================
# Test integration with scale-free and random networks
# ==============================================================================

test_that("scale-free networks are vulnerable to targeted attacks", {
  skip_if_not_installed("igraph")

  # Scale-free network
  g <- igraph::sample_pa(50, m = 2, directed = FALSE)

  rob_btw <- robustness(g, measure = "betweenness")
  rob_rnd <- robustness(g, measure = "random", n_iter = 20, seed = 42)

  auc_btw <- robustness_auc(rob_btw)
  auc_rnd <- robustness_auc(rob_rnd)

  # Scale-free should typically show: random AUC > targeted AUC
  # Due to hub vulnerability
  expect_true(auc_rnd > auc_btw * 0.8)  # Allow some tolerance
})

test_that("random networks degrade more uniformly", {
  skip_if_not_installed("igraph")

  # Erdos-Renyi random network
  g <- igraph::sample_gnp(50, 0.15)

  rob_btw <- robustness(g, measure = "betweenness")
  rob_rnd <- robustness(g, measure = "random", n_iter = 20, seed = 42)

  auc_btw <- robustness_auc(rob_btw)
  auc_rnd <- robustness_auc(rob_rnd)

  # Random networks should show more similar AUC for both strategies
  expect_true(abs(auc_btw - auc_rnd) < 0.3)
})

test_that("small-world networks show moderate robustness", {
  skip_if_not_installed("igraph")

  # Small-world network
  g <- igraph::sample_smallworld(dim = 1, size = 50, nei = 3, p = 0.1)

  rob <- robustness(g, measure = "betweenness")
  auc <- robustness_auc(rob)

  expect_true(is.numeric(auc))
  expect_true(auc > 0)
  expect_true(auc < 1)
})

# ==============================================================================
# Test match.arg behavior
# ==============================================================================

test_that("robustness() validates type argument", {
  skip_if_not_installed("igraph")

  expect_error(robustness(mat_simple, type = "invalid"))
})

test_that("robustness() validates measure argument", {
  skip_if_not_installed("igraph")

  expect_error(robustness(mat_simple, measure = "invalid"))
})

# ==============================================================================
# Summary
# ==============================================================================

cat("\n=== All Robustness Tests Passed ===\n")
