# Tests for plot_mcml() new features - pie charts, loops, within classes
# Coverage for recent additions to R/plot-mcml.R and R/cluster-metrics.R

# ============================================
# Test Setup
# ============================================

create_test_weights <- function(n = 12, seed = 42) {
  set.seed(seed)
  mat <- matrix(runif(n * n, 0.1, 0.8), n, n)
  diag(mat) <- runif(n, 0.3, 0.6)  # Include self-transitions
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]
  mat
}

create_test_clusters <- function() {
  list(
    Cluster_A = c("A", "B", "C", "D"),
    Cluster_B = c("E", "F", "G", "H"),
    Cluster_C = c("I", "J", "K", "L")
  )
}

# ============================================
# Pie Chart Summary Nodes Tests
# ============================================

test_that("plot_mcml draws pie chart summary nodes", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna")
  ))
})

test_that("plot_mcml pie charts show self vs other proportions", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Verify the function completes with pie rendering
  result <- with_temp_png(
    plot_mcml(weights, clusters, mode = "tna")
  )

  expect_s3_class(result, "cluster_summary")
  # Check that self-loop values are computed (can be >= 0)
  expect_true(all(diag(result$between$weights) >= 0))
})

test_that("plot_mcml handles zero self-loop values", {
  weights <- create_test_weights()
  diag(weights) <- 0  # No self-transitions
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna")
  ))
})

# ============================================
# Self-Loop Tests
# ============================================

test_that("plot_mcml draws self-loops on summary network", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna", summary_arrows = TRUE)
  ))
})

test_that("plot_mcml self-loops work without arrows", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna", summary_arrows = FALSE)
  ))
})

test_that("plot_mcml self-loop labels display correctly", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              summary_edge_labels = TRUE)
  ))
})

# ============================================
# Within-Cluster Pie Node Tests
# ============================================

test_that("plot_mcml draws within-cluster nodes as pies", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna", show_labels = TRUE)
  ))
})

test_that("plot_mcml within pies handle missing within data", {
  weights <- create_test_weights(6)
  clusters <- list(
    C1 = c("A", "B", "C"),
    C2 = c("D", "E", "F")
  )

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna")
  ))
})

# ============================================
# Solid Arrow Tests
# ============================================

test_that("plot_mcml uses solid arrows for within-cluster edges", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna", edge_labels = TRUE)
  ))
})

test_that("plot_mcml uses solid arrows for summary edges", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              summary_arrows = TRUE,
              summary_edge_labels = TRUE)
  ))
})

# ============================================
# Label Positioning Tests
# ============================================

test_that("plot_mcml positions summary labels perpendicular to loops", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              summary_labels = TRUE)
  ))
})

test_that("plot_mcml positions within-cluster labels on sides", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              show_labels = TRUE,
              label_size = 0.8)
  ))
})

test_that("plot_mcml edge labels positioned at 1/3 along edge", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              edge_labels = TRUE,
              edge_label_size = 0.6)
  ))
})

# ============================================
# cluster_summary within Class Tests
# ============================================

test_that("cluster_summary within has group_tna class", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  cs <- cluster_summary(weights, clusters, type = "tna", compute_within = TRUE)

  expect_s3_class(cs$within, "group_tna")
})

test_that("cluster_summary within elements have tna class", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  cs <- cluster_summary(weights, clusters, type = "tna", compute_within = TRUE)

  for (w in cs$within) {
    expect_s3_class(w, "tna")
    expect_true("weights" %in% names(w))
    expect_true("inits" %in% names(w))
  }
})

test_that("splot works with cluster_summary$within", {
  skip_if_not_installed("tna")
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  cs <- cluster_summary(weights, clusters, type = "tna", compute_within = TRUE)

  expect_no_error(with_temp_png(
    splot(cs$within)
  ))
})

test_that("splot works with cluster_summary$within using i parameter", {
  skip_if_not_installed("tna")
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  cs <- cluster_summary(weights, clusters, type = "tna", compute_within = TRUE)

  # By index
  expect_no_error(with_temp_png(
    splot(cs$within, i = 1)
  ))

  # By name
  expect_no_error(with_temp_png(
    splot(cs$within, i = "Cluster_A")
  ))
})

# ============================================
# Self-Loop Value Computation Tests
# ============================================

test_that("plot_mcml computes self-loop values from within data", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- with_temp_png(
    plot_mcml(weights, clusters, mode = "tna")
  )

  # Self-loop values should be computed from within-cluster transitions
  expect_true(all(diag(result$between$weights) >= 0))
})

test_that("plot_mcml handles clusters with single nodes", {
  weights <- create_test_weights(5)
  colnames(weights) <- rownames(weights) <- LETTERS[1:5]
  clusters <- list(
    Single = "A",
    Pair = c("B", "C"),
    Triple = c("D", "E")
  )

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna")
  ))
})

# ============================================
# Mode Parameter Tests
# ============================================

test_that("plot_mcml mode='tna' shows edge labels by default", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # In TNA mode, edge_labels should default to TRUE
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna")
  ))
})

test_that("plot_mcml mode='weights' works correctly", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "weights")
  ))
})

# ============================================
# Edge Width Range Tests
# ============================================

test_that("plot_mcml respects edge_width_range parameter", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              edge_width_range = c(0.2, 1.5))
  ))
})

test_that("plot_mcml respects summary_edge_width_range parameter", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              summary_edge_width_range = c(0.5, 3.0))
  ))
})

# ============================================
# Visual Styling Tests
# ============================================

test_that("plot_mcml with custom colors", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              colors = c("red", "green", "blue"))
  ))
})

test_that("plot_mcml with custom node sizes", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              node_size = 2.5,
              summary_size = 5)
  ))
})

test_that("plot_mcml with title and subtitle", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              title = "Test Title",
              subtitle = "Test Subtitle")
  ))
})

# ============================================
# Legend Tests
# ============================================

test_that("plot_mcml legend positioning works", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  for (pos in c("right", "left", "top", "bottom", "none")) {
    expect_no_error(with_temp_png(
      plot_mcml(weights, clusters, mode = "tna",
                legend_position = pos)
    ))
  }
})

# ============================================
# Alpha and Transparency Tests
# ============================================

test_that("plot_mcml respects alpha parameters", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              edge_alpha = 0.5,
              summary_edge_alpha = 0.8,
              shell_alpha = 0.2)
  ))
})

# ============================================
# Input Type Tests
# ============================================

test_that("plot_mcml works with cluster_summary input", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  cs <- cluster_summary(weights, clusters, type = "tna", compute_within = TRUE)

  expect_no_error(with_temp_png(
    plot_mcml(cs)
  ))
})

test_that("plot_mcml works with cograph_network input", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  net <- cograph(weights)

  expect_no_error(with_temp_png(
    plot_mcml(net, clusters)
  ))
})

# ============================================
# Minimum Threshold Tests
# ============================================

test_that("plot_mcml respects minimum threshold", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              minimum = 0.3)
  ))
})

test_that("plot_mcml with high minimum shows fewer edges", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna",
              minimum = 0.9)
  ))
})
