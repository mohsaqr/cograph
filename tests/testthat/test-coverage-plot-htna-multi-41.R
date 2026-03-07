# Additional coverage tests for plot-htna-multi.R (plot_mtna)
# Targets: cograph_network input, nodes as data.frame, column-name cluster_list,
#          auto-detect clusters, triangle geometry edge cases,
#          zero inter-cluster connections (bundling fallback),
#          zero max_within edge weight, label_abbrev,
#          display_labels from nodes_df

# ============================================
# Test Setup
# ============================================

make_mtna_mat <- function(n = 12, n_clusters = 3, seed = 42) {
  set.seed(seed)
  nodes <- paste0("N", seq_len(n))
  m <- matrix(runif(n * n, 0, 0.3), n, n)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes
  m
}

make_mtna_clusters <- function(n = 12, n_clusters = 3) {
  cluster_size <- n %/% n_clusters
  cls <- lapply(seq_len(n_clusters), function(i) {
    start <- (i - 1) * cluster_size + 1
    end <- if (i == n_clusters) n else i * cluster_size
    paste0("N", start:end)
  })
  names(cls) <- paste0("Cluster", seq_len(n_clusters))
  cls
}

# ============================================
# cograph_network input
# ============================================

test_that("plot_mtna works with cograph_network input", {
  m <- make_mtna_mat()
  net <- as_cograph(m)
  clusters <- make_mtna_clusters()

  result <- safe_plot(plot_mtna(net, clusters))
  expect_true(result$success, info = result$error)
})

# ============================================
# Nodes as data.frame
# ============================================

test_that("plot_mtna with nodes data.frame (labels column)", {
  m <- make_mtna_mat()
  clusters <- make_mtna_clusters()

  nodes_df <- data.frame(
    name = paste0("N", 1:12),
    labels = paste0("Node", 1:12),
    stringsAsFactors = FALSE
  )

  result <- safe_plot(plot_mtna(m, clusters, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna with nodes data.frame (label column)", {
  m <- make_mtna_mat()
  clusters <- make_mtna_clusters()

  nodes_df <- data.frame(
    name = paste0("N", 1:12),
    label = paste0("N", 1:12),
    stringsAsFactors = FALSE
  )

  result <- safe_plot(plot_mtna(m, clusters, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

# ============================================
# Column-name cluster_list
# ============================================

test_that("plot_mtna uses column name for cluster_list", {
  m <- make_mtna_mat()
  net <- as_cograph(m)
  net$nodes$cluster <- rep(c("A", "B", "C"), each = 4)

  result <- safe_plot(plot_mtna(net, "cluster"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna errors for missing column name", {
  m <- make_mtna_mat()
  net <- as_cograph(m)

  expect_error(
    plot_mtna(net, "nonexistent"),
    "not found"
  )
})

test_that("plot_mtna errors for column name without cograph_network", {
  m <- make_mtna_mat()

  expect_error(
    plot_mtna(m, "cluster"),
    "cograph_network"
  )
})

# ============================================
# Auto-detect clusters from nodes
# ============================================

test_that("plot_mtna auto-detects clusters from 'cluster' column", {
  m <- make_mtna_mat()
  net <- as_cograph(m)
  net$nodes$cluster <- rep(c("X", "Y", "Z"), each = 4)

  result <- safe_plot(
    expect_message(
      plot_mtna(net),
      "Using.*cluster"
    )
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna auto-detects clusters from 'group' column", {
  m <- make_mtna_mat()
  net <- as_cograph(m)
  net$nodes$group <- rep(c("G1", "G2", "G3"), each = 4)

  result <- safe_plot(
    expect_message(
      plot_mtna(net),
      "Using.*group"
    )
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Triangle geometry edge cases
# ============================================

test_that("plot_mtna with triangle shape handles various edge angles", {
  # Create a 3-cluster setup with triangle shapes
  # Position clusters so inter-cluster edges hit different angle ranges
  set.seed(123)
  n <- 9
  nodes <- paste0("N", 1:n)
  m <- matrix(runif(n * n, 0, 0.5), n, n)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  # Make strong inter-cluster connections to exercise all triangle edge geometry
  m[1, 4] <- 0.9  # N1(Cluster1) -> N4(Cluster2)
  m[4, 7] <- 0.9  # N4(Cluster2) -> N7(Cluster3)
  m[7, 1] <- 0.9  # N7(Cluster3) -> N1(Cluster1)
  m[2, 5] <- 0.8
  m[5, 8] <- 0.8
  m[8, 2] <- 0.8

  clusters <- list(
    Cluster1 = paste0("N", 1:3),
    Cluster2 = paste0("N", 4:6),
    Cluster3 = paste0("N", 7:9)
  )

  result <- safe_plot(
    plot_mtna(m, clusters, shapes = c("triangle", "triangle", "triangle"))
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Square shape edge point calculation
# ============================================

test_that("plot_mtna with square shape handles edge calculations", {
  set.seed(42)
  n <- 8
  nodes <- paste0("N", 1:n)
  m <- matrix(runif(n * n, 0, 0.5), n, n)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  clusters <- list(
    ClA = paste0("N", 1:4),
    ClB = paste0("N", 5:8)
  )

  result <- safe_plot(
    plot_mtna(m, clusters, shapes = c("square", "square"))
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Diamond shape
# ============================================

test_that("plot_mtna with diamond shape", {
  m <- make_mtna_mat(9, 3)
  clusters <- make_mtna_clusters(9, 3)

  result <- safe_plot(
    plot_mtna(m, clusters, shapes = c("diamond", "diamond", "diamond"))
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Zero inter-cluster edges (bundling fallback)
# ============================================

test_that("plot_mtna handles nodes with zero inter-cluster connections", {
  n <- 9
  m <- matrix(0, n, n)
  colnames(m) <- rownames(m) <- paste0("N", 1:n)

  # Only within-cluster edges, no between-cluster
  m[1, 2] <- 0.5; m[2, 3] <- 0.3
  m[4, 5] <- 0.4; m[5, 6] <- 0.6
  m[7, 8] <- 0.2; m[8, 9] <- 0.4

  clusters <- make_mtna_clusters(9, 3)

  result <- safe_plot(
    plot_mtna(m, clusters, bundle_edges = TRUE)
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# max_within == 0 edge case
# ============================================

test_that("plot_mtna handles zero within-cluster weights", {
  n <- 6
  m <- matrix(0, n, n)
  colnames(m) <- rownames(m) <- paste0("N", 1:n)
  # Only between-cluster edges
  m[1, 4] <- 0.5; m[4, 1] <- 0.3

  clusters <- list(
    ClA = paste0("N", 1:3),
    ClB = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mtna(m, clusters))
  expect_true(result$success, info = result$error)
})

# ============================================
# Curved edges with zero length
# ============================================

test_that("plot_mtna handles zero-length inter-cluster edges", {
  # Very small network where curvature offset computation matters
  set.seed(99)
  n <- 6
  m <- matrix(runif(n * n, 0.01, 0.1), n, n)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- paste0("N", 1:n)

  clusters <- list(
    C1 = paste0("N", 1:3),
    C2 = paste0("N", 4:6)
  )

  result <- safe_plot(
    plot_mtna(m, clusters, curvature = 0.3)
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# label_abbrev
# ============================================

test_that("plot_mtna with label_abbrev", {
  m <- make_mtna_mat()
  clusters <- make_mtna_clusters()

  result <- safe_plot(plot_mtna(m, clusters, label_abbrev = 2))
  expect_true(result$success, info = result$error)
})

# ============================================
# show_labels with display_labels
# ============================================

test_that("plot_mtna shows labels with label_size", {
  m <- make_mtna_mat()
  clusters <- make_mtna_clusters()

  result <- safe_plot(
    plot_mtna(m, clusters, show_labels = TRUE, label_size = 0.5)
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Legend with various shapes
# ============================================

test_that("plot_mtna legend with pentagon/hexagon shape names", {
  set.seed(42)
  n <- 18
  nodes <- paste0("N", 1:n)
  m <- matrix(runif(n * n, 0, 0.2), n, n)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  clusters <- list(
    C1 = paste0("N", 1:3),
    C2 = paste0("N", 4:6),
    C3 = paste0("N", 7:9),
    C4 = paste0("N", 10:12),
    C5 = paste0("N", 13:15),
    C6 = paste0("N", 16:18)
  )

  result <- safe_plot(
    plot_mtna(m, clusters, legend = TRUE)
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# community parameter
# ============================================

test_that("plot_mtna with community detection", {
  set.seed(99)
  n <- 12
  m <- matrix(0, n, n)
  colnames(m) <- rownames(m) <- paste0("N", 1:n)
  # Sparse but clustered edges

  m[1, 2] <- 0.5; m[2, 3] <- 0.4; m[3, 4] <- 0.3
  m[5, 6] <- 0.6; m[6, 7] <- 0.5; m[7, 8] <- 0.4
  m[9, 10] <- 0.5; m[10, 11] <- 0.3; m[11, 12] <- 0.4
  # Weak between-cluster
  m[4, 5] <- 0.1; m[8, 9] <- 0.1

  result <- safe_plot(
    plot_mtna(m, community = "fast_greedy")
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Line 163: nodes_df without label/labels columns
# ============================================

test_that("plot_mtna falls back to lab when nodes_df has no label columns", {
  m <- make_mtna_mat()
  clusters <- make_mtna_clusters()

  nodes_df <- data.frame(
    name = paste0("N", 1:12),
    color = rep("blue", 12),
    stringsAsFactors = FALSE
  )

  result <- safe_plot(plot_mtna(m, clusters, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

# ============================================
# Lines 470, 500, 509, 512, 518: Triangle geometry edge cases
# ============================================

test_that("plot_mtna triangle edges from various angles", {
  # Create 3 clusters with strong inter-cluster edges from multiple directions
  set.seed(42)
  n <- 9
  nodes <- paste0("N", 1:n)
  m <- matrix(0, n, n)
  colnames(m) <- rownames(m) <- nodes

  # Multiple strong inter-cluster edges hitting different triangle edge sectors
  # Cluster1(N1-3) -> Cluster2(N4-6) -> Cluster3(N7-9) -> Cluster1
  m[1, 4] <- 0.9; m[1, 5] <- 0.8; m[1, 6] <- 0.7
  m[2, 7] <- 0.9; m[2, 8] <- 0.6
  m[3, 4] <- 0.5; m[3, 9] <- 0.8
  m[4, 7] <- 0.9; m[5, 8] <- 0.7; m[6, 9] <- 0.6
  m[7, 1] <- 0.8; m[8, 2] <- 0.7; m[9, 3] <- 0.9
  # Within-cluster edges too
  m[1, 2] <- 0.3; m[4, 5] <- 0.3; m[7, 8] <- 0.3

  clusters <- list(
    C1 = paste0("N", 1:3),
    C2 = paste0("N", 4:6),
    C3 = paste0("N", 7:9)
  )

  result <- safe_plot(
    plot_mtna(m, clusters,
              shapes = c("triangle", "triangle", "triangle"),
              curvature = 0.2)
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Lines 576-577: Curved inter-cluster edges (xspline with offsets)
# ============================================

test_that("plot_mtna renders curved inter-cluster edges", {
  set.seed(42)
  m <- make_mtna_mat(9, 3)
  clusters <- make_mtna_clusters(9, 3)

  result <- safe_plot(
    plot_mtna(m, clusters, curvature = 0.5)
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Line 708: max_within == 0 fallback edge width
# ============================================

test_that("plot_mtna within-edge width fallback when max_within=0", {
  n <- 6
  m <- matrix(0, n, n)
  colnames(m) <- rownames(m) <- paste0("N", 1:n)
  # Only between-cluster, no within
  m[1, 4] <- 0.5; m[4, 1] <- 0.3
  m[2, 5] <- 0.4

  clusters <- list(
    C1 = paste0("N", 1:3),
    C2 = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mtna(m, clusters))
  expect_true(result$success, info = result$error)
})

# ============================================
# Line 790: label_abbrev in show_labels context
# ============================================

test_that("plot_mtna applies label_abbrev with show_labels", {
  m <- make_mtna_mat()
  clusters <- make_mtna_clusters()

  result <- safe_plot(
    plot_mtna(m, clusters, show_labels = TRUE, label_abbrev = 2)
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Line 861: unrecognized cluster_shapes in legend
# ============================================

test_that("plot_mtna legend handles unrecognized shapes", {
  m <- make_mtna_mat(9, 3)
  clusters <- make_mtna_clusters(9, 3)

  result <- safe_plot(
    plot_mtna(m, clusters,
              shapes = c("unknown1", "unknown2", "unknown3"),
              legend = TRUE)
  )
  expect_true(result$success, info = result$error)
})
