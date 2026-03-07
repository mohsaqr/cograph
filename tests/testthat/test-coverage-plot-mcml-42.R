# Additional coverage tests for plot-mcml.R
# Targets: nodes as data.frame, cograph_network input, label positions,
#          summary_labels positions 1/2/4, within-cluster arrows, pie chart self-loop,
#          within_w fallback from raw weights, label_abbrev, max_w == 0 edge case

# ============================================
# Test Setup
# ============================================

create_mcml_weights <- function(n = 6, seed = 42) {
  set.seed(seed)
  mat <- matrix(runif(n * n, 0, 0.5), n, n)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]
  mat
}

create_mcml_clusters <- function(n = 6) {
  list(
    Cluster1 = LETTERS[1:2],
    Cluster2 = LETTERS[3:4],
    Cluster3 = LETTERS[5:6]
  )
}

# ============================================
# Nodes as data.frame input
# ============================================

test_that("plot_mcml accepts nodes as data.frame with labels column", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  nodes_df <- data.frame(
    name = LETTERS[1:6],
    labels = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta"),
    stringsAsFactors = FALSE
  )

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, nodes = nodes_df)
  ))
})

test_that("plot_mcml accepts nodes as data.frame with label column", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  nodes_df <- data.frame(
    name = LETTERS[1:6],
    label = paste0("Node_", LETTERS[1:6]),
    stringsAsFactors = FALSE
  )

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, nodes = nodes_df)
  ))
})

# ============================================
# cograph_network input
# ============================================

test_that("plot_mcml works with cograph_network input", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  net <- as_cograph(weights)

  expect_no_error(with_temp_png(
    plot_mcml(net, clusters)
  ))
})

test_that("plot_mcml with cograph_network stores nodes_df", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  net <- as_cograph(weights)

  result <- with_temp_png(
    plot_mcml(net, clusters)
  )

  expect_s3_class(result, "cluster_summary")
})

# ============================================
# Summary label positions
# ============================================

test_that("plot_mcml summary_label_position = 1 (below)", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, summary_label_position = 1)
  ))
})

test_that("plot_mcml summary_label_position = 2 (left)", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, summary_label_position = 2)
  ))
})

test_that("plot_mcml summary_label_position = 4 (right)", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, summary_label_position = 4)
  ))
})

# ============================================
# between_arrows feature
# ============================================

test_that("plot_mcml draws arrows on between edges", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, between_arrows = TRUE)
  ))
})

# ============================================
# Within-cluster fallback from raw weights
# ============================================

test_that("plot_mcml uses raw weights for within when cs$within is NULL", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  # Create cluster_summary with compute_within=FALSE
  cs <- cluster_summary(weights, clusters, compute_within = FALSE)
  expect_null(cs$within)

  expect_no_error(with_temp_png(
    plot_mcml(cs, weights = weights)
  ))
})

# ============================================
# max_w == 0 edge case (all within-cluster weights zero)
# ============================================

test_that("plot_mcml handles zero within-cluster weights", {
  # Create matrix where within-cluster is all zero
  n <- 6
  mat <- matrix(0, n, n)
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]
  # Only between-cluster edges
  mat[1, 3] <- 0.5
  mat[3, 5] <- 0.3
  mat[5, 1] <- 0.2

  clusters <- create_mcml_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

# ============================================
# Summary pie chart self-loop proportions
# ============================================

test_that("plot_mcml draws pie charts with self-loop proportions", {
  # Matrix with significant self-loops at cluster level
  set.seed(42)
  n <- 6
  mat <- matrix(runif(n * n, 0.1, 0.8), n, n)
  diag(mat) <- runif(n, 0.5, 0.9)
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]

  clusters <- create_mcml_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters, mode = "tna")
  ))
})

test_that("plot_mcml pie with zero self-loop", {
  # Create cs where diagonal is 0
  n <- 6
  mat <- matrix(0.1, n, n)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]

  clusters <- create_mcml_clusters()
  cs <- cluster_summary(mat, clusters, type = "tna")
  # Zero diagonal → self_prop = 0

  expect_no_error(with_temp_png(
    plot_mcml(cs)
  ))
})

# ============================================
# label_abbrev parameter
# ============================================

test_that("plot_mcml with label_abbrev", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, label_abbrev = 3)
  ))
})

# ============================================
# Line 524: nodes_df exists but no labels/label columns
# ============================================

test_that("plot_mcml falls back to lab when nodes_df has no label columns", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  # nodes data.frame with only metadata, no labels/label columns
  nodes_df <- data.frame(
    name = LETTERS[1:6],
    color = rep("red", 6),
    size = rep(3, 6),
    stringsAsFactors = FALSE
  )

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, nodes = nodes_df)
  ))
})

# ============================================
# Line 612: max_w == 0 or NA within-cluster weights
# ============================================

test_that("plot_mcml handles NA/zero within-cluster weights (max_w fallback)", {
  n <- 6
  mat <- matrix(0, n, n)
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]
  # Only between-cluster edges, NO within-cluster
  mat[1, 4] <- 0.5
  mat[4, 1] <- 0.3

  clusters <- create_mcml_clusters()
  cs <- cluster_summary(mat, clusters, type = "tna")
  # Within matrices exist but are all zeros

  expect_no_error(with_temp_png(
    plot_mcml(cs)
  ))
})

# ============================================
# Lines 899-901: within_w fallback from raw weights matrix
# ============================================

test_that("plot_mcml extracts within from raw weights when cs$within is missing", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  cs <- cluster_summary(weights, clusters, compute_within = FALSE)

  # Pass original weights so the fallback path extracts submatrices
  expect_no_error(with_temp_png(
    plot_mcml(cs, weights = weights)
  ))
})

# ============================================
# Lines 987-993: Pie slice rendering (self_prop > 0.001)
# ============================================

test_that("plot_mcml renders pie slices with significant self-loops", {
  set.seed(42)
  n <- 6
  # Create matrix with strong within-cluster edges to ensure
  # the between-cluster diagonal (self-loops) is > 0.001
  mat <- matrix(0, n, n)
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]
  # Strong within-cluster edges (A<->B, C<->D, E<->F)
  mat[1, 2] <- 0.8; mat[2, 1] <- 0.7
  mat[3, 4] <- 0.9; mat[4, 3] <- 0.6
  mat[5, 6] <- 0.5; mat[6, 5] <- 0.4
  # Weak between-cluster
  mat[1, 3] <- 0.1; mat[3, 5] <- 0.1

  clusters <- create_mcml_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters, mode = "tna")
  ))
})

# ============================================
# Line 1111: mcml() with nodes parameter (non-cograph input)
# ============================================

test_that("mcml stores nodes_df when called with nodes parameter", {
  weights <- create_mcml_weights()
  clusters <- create_mcml_clusters()

  nodes_df <- data.frame(
    name = LETTERS[1:6],
    labels = paste0("Node_", LETTERS[1:6]),
    stringsAsFactors = FALSE
  )

  result <- mcml(weights, clusters, nodes = nodes_df)

  expect_s3_class(result, "cluster_summary")
  expect_true(!is.null(result$nodes_df))
  expect_equal(result$nodes_df$labels, paste0("Node_", LETTERS[1:6]))
})
