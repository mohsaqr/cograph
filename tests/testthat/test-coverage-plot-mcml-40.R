# Tests for plot_mcml() Multi-Cluster Multi-Layer Network visualization
# Comprehensive coverage for R/plot-mcml.R

# ============================================
# Test Setup
# ============================================

# Create test matrices and cluster lists for reuse
create_test_weights <- function(n = 6, seed = 42) {
  set.seed(seed)
  mat <- matrix(runif(n * n, 0, 0.5), n, n)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]
  mat
}

create_test_clusters <- function(n = 6) {
  list(
    Cluster1 = LETTERS[1:2],
    Cluster2 = LETTERS[3:4],
    Cluster3 = LETTERS[5:6]
  )
}

# ============================================
# Basic Functionality Tests
# ============================================

test_that("plot_mcml works with basic matrix and clusters", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters)
  ))
})

test_that("plot_mcml returns cluster_summary invisibly", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- with_temp_png(plot_mcml(weights, clusters))

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_clusters, 3)
  expect_equal(result$meta$n_nodes, 6)
})

test_that("mcml returns cluster_summary", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_clusters, 3)
  expect_equal(result$meta$n_nodes, 6)
  expect_equal(names(result$clusters), c("Cluster1", "Cluster2", "Cluster3"))
  expect_true(is.matrix(result$between$weights))
  expect_equal(nrow(result$between$weights), 3)
  expect_equal(ncol(result$between$weights), 3)
  expect_true(is.numeric(result$between$inits))
  expect_equal(length(result$between$inits), 3)
})

test_that("plot_mcml accepts cluster_summary object", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # First extract data
  data <- cluster_summary(weights, clusters)

  # Then plot from pre-extracted data
  expect_no_error(with_temp_png(
    result <- plot_mcml(data)
  ))

  # Should return the same object
  expect_s3_class(result, "cluster_summary")
})

test_that("plot_mcml handles unlabeled matrices", {
  set.seed(42)
  mat <- matrix(runif(16, 0, 0.5), 4, 4)
  diag(mat) <- 0
  # No row/column names

  clusters <- list(
    C1 = c(1, 2),
    C2 = c(3, 4)
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml works with numeric indices in clusters", {
  set.seed(42)
  mat <- matrix(runif(16, 0, 0.5), 4, 4)
  diag(mat) <- 0

  clusters <- list(
    A = c(1, 2),
    B = c(3, 4)
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

# ============================================
# TNA Object Integration Tests
# ============================================

test_that("plot_mcml works with tna objects", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")
  model <- tna(engagement)

  # engagement has 3 labels: Active, Average, Disengaged
  n_labels <- length(model$labels)
  if (n_labels >= 2) {
    clusters <- list(
      A = model$labels[1],
      B = model$labels[2:n_labels]
    )

    expect_no_error(with_temp_png(
      plot_mcml(model, clusters)
    ))
  }
})

test_that("plot_mcml extracts weights and labels from tna", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")
  model <- tna(engagement)

  n_labels <- length(model$labels)
  if (n_labels >= 2) {
    half <- ceiling(n_labels / 2)
    # Ensure we don't go out of bounds
    second_half_start <- min(half + 1, n_labels)

    clusters <- list(
      A = model$labels[1:half],
      B = model$labels[second_half_start:n_labels]
    )

    # Should work without error, extracting weights from tna
    expect_no_error(with_temp_png(
      plot_mcml(model, clusters)
    ))
  }
})

# ============================================
# Parameter Tests - layer_spacing
# ============================================

test_that("plot_mcml respects custom layer_spacing", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, layer_spacing = 6)
  ))

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, layer_spacing = 2)
  ))
})

test_that("plot_mcml auto-calculates layer_spacing when NULL", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Default is NULL - should auto-calculate

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, layer_spacing = NULL)
  ))
})

# ============================================
# Parameter Tests - spacing
# ============================================

test_that("plot_mcml respects cluster spacing parameter", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, spacing = 5)
  ))

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, spacing = 1)
  ))
})

# ============================================
# Parameter Tests - shape_size
# ============================================

test_that("plot_mcml respects shape_size parameter", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, shape_size = 2)
  ))

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, shape_size = 0.5)
  ))
})

# ============================================
# Parameter Tests - summary_size
# ============================================

test_that("plot_mcml respects summary_size parameter", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, summary_size = 6)
  ))

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, summary_size = 2)
  ))
})

# ============================================
# Parameter Tests - skew_angle
# ============================================

test_that("plot_mcml respects skew_angle parameter", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Different perspective angles
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, skew_angle = 30)
  ))

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, skew_angle = 80)
  ))

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, skew_angle = 0)
  ))
})

# ============================================
# Parameter Tests - aggregation
# ============================================

test_that("plot_mcml uses sum aggregation by default", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Default aggregation is "sum"
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, aggregation = "sum")
  ))
})

test_that("plot_mcml respects mean aggregation", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, aggregation = "mean")
  ))
})

test_that("plot_mcml respects max aggregation", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, aggregation = "max")
  ))
})

test_that("plot_mcml errors on invalid aggregation", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_error(
    with_temp_png(plot_mcml(weights, clusters, aggregation = "invalid")),
    "arg"
  )
})

# ============================================
# Parameter Tests - minimum
# ============================================

test_that("plot_mcml respects minimum edge threshold", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # High minimum filters out weak edges
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, minimum = 0.3)
  ))

  # No threshold
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, minimum = 0)
  ))
})

test_that("plot_mcml handles high minimum threshold", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Very high threshold - may filter all edges
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, minimum = 0.9)
  ))
})

# ============================================
# Parameter Tests - colors
# ============================================

test_that("plot_mcml uses auto colors when colors = NULL", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, colors = NULL)
  ))
})

test_that("plot_mcml respects custom colors", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  custom_colors <- c("red", "blue", "green")

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, colors = custom_colors)
  ))
})

test_that("plot_mcml recycles colors when fewer than clusters", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()  # 3 clusters

  # Only provide 2 colors - should recycle
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, colors = c("red", "blue"))
  ))
})

test_that("plot_mcml works with hex colors", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  hex_colors <- c("#E69F00", "#56B4E9", "#009E73")

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, colors = hex_colors)
  ))
})

# ============================================
# Parameter Tests - legend
# ============================================

test_that("plot_mcml shows legend by default", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, legend = TRUE)
  ))
})

test_that("plot_mcml hides legend when legend = FALSE", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, legend = FALSE)
  ))
})

# ============================================
# Cluster Configuration Tests
# ============================================

test_that("plot_mcml handles two clusters", {
  set.seed(42)
  mat <- matrix(runif(16, 0, 0.5), 4, 4)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles many clusters", {
  set.seed(42)
  n <- 12
  mat <- matrix(runif(n * n, 0, 0.5), n, n)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]

  clusters <- list(
    C1 = LETTERS[1:2],
    C2 = LETTERS[3:4],
    C3 = LETTERS[5:6],
    C4 = LETTERS[7:8],
    C5 = LETTERS[9:10],
    C6 = LETTERS[11:12]
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles single-node clusters", {
  set.seed(42)
  mat <- matrix(runif(9, 0, 0.5), 3, 3)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:3]

  clusters <- list(
    C1 = "A",
    C2 = "B",
    C3 = "C"
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles mixed cluster sizes", {
  set.seed(42)
  n <- 6
  mat <- matrix(runif(n * n, 0, 0.5), n, n)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:n]

  clusters <- list(
    Large = LETTERS[1:4],  # 4 nodes
    Small = LETTERS[5:6]   # 2 nodes
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles unnamed clusters", {
  weights <- create_test_weights()

  # Clusters without names
  clusters <- list(
    LETTERS[1:2],
    LETTERS[3:4],
    LETTERS[5:6]
  )

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters)
  ))
})

# ============================================
# Edge Case Tests
# ============================================

test_that("plot_mcml handles zero weight matrix", {
  mat <- matrix(0, 4, 4)
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles sparse matrix", {
  set.seed(42)
  mat <- matrix(0, 6, 6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  # Only one edge
  mat[1, 2] <- 0.5

  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles matrix with NAs", {
  set.seed(42)
  mat <- matrix(runif(16, 0, 0.5), 4, 4)
  diag(mat) <- 0
  mat[1, 2] <- NA
  mat[2, 3] <- NA
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles all weights equal", {
  mat <- matrix(0.5, 4, 4)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles diagonal weights", {
  set.seed(42)
  mat <- matrix(runif(16, 0, 0.5), 4, 4)
  # Non-zero diagonal (self-loops)
  diag(mat) <- 0.3
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

# ============================================
# Visual Parameter Combination Tests
# ============================================

test_that("plot_mcml works with all visual parameters", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(
      weights, clusters,
      layer_spacing = 5,
      spacing = 4,
      shape_size = 1.5,
      summary_size = 5,
      skew_angle = 45,
      aggregation = "mean",
      minimum = 0.1,
      colors = c("#FF5733", "#33FF57", "#3357FF"),
      legend = TRUE
    )
  ))
})

test_that("plot_mcml works with minimal visual parameters", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(
      weights, clusters,
      shape_size = 0.5,
      summary_size = 2,
      legend = FALSE
    )
  ))
})

# ============================================
# Edge Weight Scaling Tests
# ============================================

test_that("plot_mcml scales edge widths by weight", {
  set.seed(42)
  mat <- matrix(0, 4, 4)
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  # Create edges with varying weights
  mat[1, 2] <- 0.1  # weak
  mat[3, 4] <- 0.9  # strong

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles very large weights", {
  set.seed(42)
  mat <- matrix(runif(16, 0, 10), 4, 4)  # Large weights
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles very small weights", {
  set.seed(42)
  mat <- matrix(runif(16, 0, 0.01), 4, 4)  # Small weights
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

# ============================================
# Cluster Inter-connections Tests
# ============================================

test_that("plot_mcml handles no inter-cluster connections", {
  mat <- matrix(0, 6, 6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  # Only within-cluster edges
  mat[1, 2] <- 0.5
  mat[2, 1] <- 0.5
  mat[3, 4] <- 0.5
  mat[4, 3] <- 0.5
  mat[5, 6] <- 0.5
  mat[6, 5] <- 0.5

  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

test_that("plot_mcml handles only inter-cluster connections", {
  mat <- matrix(0, 6, 6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  # Only between-cluster edges (no within-cluster)
  mat[1, 3] <- 0.5  # Cluster1 -> Cluster2
  mat[3, 5] <- 0.5  # Cluster2 -> Cluster3
  mat[5, 1] <- 0.5  # Cluster3 -> Cluster1

  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters)
  ))
})

# ============================================
# Perspective Tests
# ============================================

test_that("plot_mcml handles 90-degree skew (flat view)", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, skew_angle = 90)
  ))
})

test_that("plot_mcml handles small skew angles", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, skew_angle = 10)
  ))
})

# ============================================
# Large Network Tests
# ============================================

test_that("plot_mcml handles larger networks", {
  skip_on_cran()

  set.seed(42)
  n <- 20
  mat <- matrix(runif(n * n, 0, 0.5), n, n)
  diag(mat) <- 0
  labels <- paste0("N", 1:n)
  colnames(mat) <- rownames(mat) <- labels

  clusters <- list(
    C1 = labels[1:5],
    C2 = labels[6:10],
    C3 = labels[11:15],
    C4 = labels[16:20]
  )

  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters),
    width = 400, height = 400
  ))
})

# ============================================
# Device Compatibility Tests
# ============================================

test_that("plot_mcml works with PDF device", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_pdf(
    plot_mcml(weights, clusters)
  ))
})

test_that("plot_mcml works in nested plotting context", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png({
    old_par <- par(mfrow = c(1, 1))
    on.exit(par(old_par), add = TRUE)
    plot_mcml(weights, clusters)
  }))
})

# ============================================
# Aggregation Logic Tests
# ============================================

test_that("plot_mcml sum aggregation accumulates weights", {
  mat <- matrix(0, 4, 4)
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  # Multiple edges from cluster 1 to cluster 2
  mat[1, 3] <- 0.3
  mat[1, 4] <- 0.2
  mat[2, 3] <- 0.4
  mat[2, 4] <- 0.1

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  # Should work - sum would be 1.0 for A->B
  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters, aggregation = "sum")
  ))
})

test_that("plot_mcml mean aggregation averages weights", {
  mat <- matrix(0, 4, 4)
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  mat[1, 3] <- 0.8
  mat[1, 4] <- 0.2

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  # Mean of 0.8 and 0.2 = 0.5
  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters, aggregation = "mean")
  ))
})

test_that("plot_mcml max aggregation takes maximum weight", {
  mat <- matrix(0, 4, 4)
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  mat[1, 3] <- 0.9
  mat[1, 4] <- 0.1

  clusters <- list(
    A = c("A", "B"),
    B = c("C", "D")
  )

  # Max should be 0.9
  expect_no_error(with_temp_png(
    plot_mcml(mat, clusters, aggregation = "max")
  ))
})

# ============================================
# Summary Layer Tests
# ============================================

test_that("plot_mcml draws summary nodes for each cluster", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Should draw top layer with summary nodes
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters)
  ))
})

test_that("plot_mcml draws inter-layer connections", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Dashed lines from detail nodes to summary nodes
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters)
  ))
})

# ============================================
# Shell Rendering Tests
# ============================================

test_that("plot_mcml draws cluster shells", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Elliptical shells around each cluster
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters)
  ))
})

# ============================================
# Integration Tests
# ============================================

test_that("plot_mcml integrates all components", {
  set.seed(123)
  n <- 8
  mat <- matrix(runif(n * n, 0, 0.6), n, n)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- paste0("Node", 1:n)

  clusters <- list(
    Alpha = paste0("Node", 1:3),
    Beta = paste0("Node", 4:5),
    Gamma = paste0("Node", 6:8)
  )

  expect_no_error(with_temp_png(
    plot_mcml(
      mat, clusters,
      layer_spacing = 4,
      spacing = 3,
      shape_size = 1.2,
      summary_size = 4,
      skew_angle = 50,
      aggregation = "mean",
      minimum = 0.1,
      colors = c("coral", "steelblue", "seagreen"),
      legend = TRUE
    )
  ))
})

test_that("plot_mcml works in sequence", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png({
    plot_mcml(weights, clusters, aggregation = "sum")
    plot_mcml(weights, clusters, aggregation = "mean")
    plot_mcml(weights, clusters, aggregation = "max")
  }))
})

# ============================================
# mcml() Data Extraction Tests - NEW STRUCTURE
# ============================================

test_that("mcml returns cluster_summary class", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  expect_s3_class(result, "cluster_summary")
})

test_that("mcml between$weights has correct dimensions", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  expect_equal(nrow(result$between$weights), 3)
  expect_equal(ncol(result$between$weights), 3)
  expect_equal(rownames(result$between$weights), c("Cluster1", "Cluster2", "Cluster3"))
  expect_equal(colnames(result$between$weights), c("Cluster1", "Cluster2", "Cluster3"))
})

test_that("mcml between$weights diagonal is zero", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  # Diagonal should be zero (no self-loops at cluster level)
  expect_equal(sum(diag(result$between$weights) != 0), 0)
})

test_that("mcml between$weights rows sum to 1 (type = tna)", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  row_sums <- rowSums(result$between$weights)
  expect_true(all(abs(row_sums - 1) < 1e-10 | row_sums == 0))
})

test_that("mcml between$inits sums to 1", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  expect_equal(sum(result$between$inits), 1, tolerance = 1e-10)
  expect_equal(length(result$between$inits), 3)
  expect_equal(names(result$between$inits), c("Cluster1", "Cluster2", "Cluster3"))
})

test_that("mcml with mean aggregation stores method", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters, aggregation = "mean")

  expect_equal(result$meta$method, "mean")
})

test_that("mcml with max aggregation stores method", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters, aggregation = "max")

  expect_equal(result$meta$method, "max")
})

test_that("mcml handles unlabeled matrix", {
  set.seed(42)
  mat <- matrix(runif(16, 0, 0.5), 4, 4)
  diag(mat) <- 0

  clusters <- list(
    C1 = c(1, 2),
    C2 = c(3, 4)
  )

  result <- mcml(mat, clusters)

  expect_equal(result$meta$n_nodes, 4)
  expect_equal(result$meta$n_clusters, 2)
})

test_that("mcml returns itself when given cluster_summary", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  data <- cluster_summary(weights, clusters)
  result <- mcml(data)

  expect_identical(data, result)
})

test_that("print.cluster_summary outputs summary", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  data <- mcml(weights, clusters)

  output <- capture.output(print(data))

  expect_true(any(grepl("Cluster Summary", output)))
  expect_true(any(grepl("Clusters:", output)))
})

test_that("mcml works with cograph_network input", {
  weights <- create_test_weights()

  net <- as_cograph(weights)
  net$nodes$cluster <- c("A", "A", "B", "B", "C", "C")

  result <- mcml(net)

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_clusters, 3)
})

test_that("mcml as_tna returns cluster_tna class object", {
  skip_if_not_installed("tna")
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters, as_tna = TRUE)

  expect_s3_class(result, "cluster_tna")
  expect_s3_class(result$between, "tna")
  expect_true("weights" %in% names(result$between))
  expect_true("inits" %in% names(result$between))
  expect_true("labels" %in% names(result$between))
})

test_that("mcml as_tna between weights match cluster_summary", {
  skip_if_not_installed("tna")
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  cs <- mcml(weights, clusters)
  tna_obj <- mcml(weights, clusters, as_tna = TRUE)

  expect_equal(nrow(cs$between$weights), nrow(tna_obj$between$weights))
  expect_equal(length(cs$between$inits), length(tna_obj$between$inits))
})

# ============================================
# mcml() $within Field Tests - NEW STRUCTURE
# ============================================

test_that("mcml includes within field by default", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  expect_true("within" %in% names(result))
  expect_true(is.list(result$within))
  expect_equal(names(result$within), c("Cluster1", "Cluster2", "Cluster3"))
})

test_that("mcml within field contains per-cluster data", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  # Each cluster should have weights and inits
  for (cl_name in names(result$within)) {
    cl_data <- result$within[[cl_name]]
    expect_true("weights" %in% names(cl_data))
    expect_true("inits" %in% names(cl_data))
  }
})

test_that("mcml within$weights is row-normalized", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  # Check each cluster's weights rows sum to 1 (or 0 if all zeros)
  for (cl_name in names(result$within)) {
    cl_w <- result$within[[cl_name]]$weights
    row_sums <- rowSums(cl_w)
    expect_true(all(abs(row_sums - 1) < 1e-10 | row_sums == 0))
  }
})

test_that("mcml within = FALSE skips within computation", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters, within = FALSE)

  expect_null(result$within)
  # between should still exist
  expect_true("between" %in% names(result))
})

test_that("mcml handles single-node clusters in within", {
  set.seed(42)
  mat <- matrix(runif(9, 0, 0.5), 3, 3)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:3]

  clusters <- list(
    C1 = "A",
    C2 = "B",
    C3 = "C"
  )

  result <- mcml(mat, clusters)

  # Single-node clusters should have 1x1 zero matrices
  expect_equal(dim(result$within$C1$weights), c(1, 1))
  expect_equal(result$within$C1$weights[1, 1], 0)
  expect_equal(result$within$C1$inits, c(A = 1))
})

# ============================================
# plot_mcml() mode Parameter Tests
# ============================================

test_that("plot_mcml uses weights mode by default", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Default mode = "weights"
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters)
  ))
})

test_that("plot_mcml respects mode = 'weights'", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "weights")
  ))
})

test_that("plot_mcml respects mode = 'tna'", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna")
  ))
})

test_that("plot_mcml mode = 'tna' with edge labels", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Edge labels should show TNA probabilities when mode = "tna"
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna", summary_edge_labels = TRUE)
  ))
})

test_that("plot_mcml mode = 'tna' with within edge labels", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Within-cluster edge labels should also show TNA values
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, mode = "tna", edge_labels = TRUE)
  ))
})

test_that("plot_mcml errors on invalid mode", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  expect_error(
    with_temp_png(plot_mcml(weights, clusters, mode = "invalid")),
    "arg"
  )
})

test_that("plot_mcml mode parameter works with cluster_summary input", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # First extract data
  data <- cluster_summary(weights, clusters)

  # Then plot with mode = "tna"
  expect_no_error(with_temp_png(
    plot_mcml(data, mode = "tna", summary_edge_labels = TRUE)
  ))
})

# ============================================
# Backward Compatibility Tests
# ============================================

test_that("mcml backward compat: main fields exist", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  result <- mcml(weights, clusters)

  # Key fields should exist
  expect_true("clusters" %in% names(result))
  expect_true("between" %in% names(result))
  expect_true("within" %in% names(result))
  expect_true("meta" %in% names(result))
})

test_that("plot_mcml backward compat: default behavior unchanged", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # This should work exactly as before - no mode parameter needed
  expect_no_error(with_temp_png(
    plot_mcml(weights, clusters, edge_labels = TRUE, summary_edge_labels = TRUE)
  ))
})

test_that("plot_mcml backward compat: cluster_summary input", {
  weights <- create_test_weights()
  clusters <- create_test_clusters()

  # Old workflow: extract then plot
  data <- cluster_summary(weights, clusters)
  expect_no_error(with_temp_png(
    result <- plot_mcml(data)
  ))

  # Should return the cluster_summary object
  expect_s3_class(result, "cluster_summary")
})
