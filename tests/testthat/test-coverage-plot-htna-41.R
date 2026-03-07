# Additional coverage tests for plot-htna.R
# Targets: cograph_network input, column-name node_list, auto-detect groups,
#          nodes as data.frame, label_abbrev, compute_polygon_layout outward flip,
#          display_labels from nodes_df (labels/label columns),
#          community auto-detect

# ============================================
# Test Setup
# ============================================

make_htna_mat <- function(n = 8, seed = 42) {
  set.seed(seed)
  nodes <- paste0("N", seq_len(n))
  m <- matrix(runif(n * n, 0, 0.5), n, n)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes
  m
}

make_htna_groups <- function(n = 8) {
  half <- n %/% 2
  list(
    GroupA = paste0("N", seq_len(half)),
    GroupB = paste0("N", (half + 1):n)
  )
}

# ============================================
# cograph_network input
# ============================================

test_that("plot_htna works with cograph_network input", {
  m <- make_htna_mat()
  net <- as_cograph(m)
  groups <- make_htna_groups()

  result <- safe_plot(plot_htna(net, groups))
  expect_true(result$success, info = result$error)
})

# ============================================
# Column-name node_list
# ============================================

test_that("plot_htna uses column name for node_list", {
  m <- make_htna_mat()
  net <- as_cograph(m)
  net$nodes$cluster <- rep(c("A", "B"), each = 4)

  result <- safe_plot(plot_htna(net, "cluster"))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna errors for missing column name", {
  m <- make_htna_mat()
  net <- as_cograph(m)

  expect_error(
    plot_htna(net, "nonexistent"),
    "not found"
  )
})

test_that("plot_htna errors for column name without cograph_network", {
  m <- make_htna_mat()

  expect_error(
    plot_htna(m, "cluster"),
    "cograph_network"
  )
})

# ============================================
# Auto-detect groups from node columns
# ============================================

test_that("plot_htna auto-detects groups from 'group' column", {
  m <- make_htna_mat()
  net <- as_cograph(m)
  net$nodes$group <- rep(c("A", "B"), each = 4)

  result <- safe_plot(
    expect_message(
      plot_htna(net),
      "Using.*group"
    )
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_htna auto-detects groups from 'cluster' column", {
  m <- make_htna_mat()
  net <- as_cograph(m)
  net$nodes$cluster <- rep(c("X", "Y"), each = 4)

  result <- safe_plot(
    expect_message(
      plot_htna(net),
      "Using.*cluster"
    )
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# community parameter
# ============================================

test_that("plot_htna with community auto-detection", {
  set.seed(99)
  n <- 12
  m <- matrix(0, n, n)
  colnames(m) <- rownames(m) <- paste0("N", 1:n)
  # Sparse but clustered
  m[1, 2] <- 0.5; m[2, 3] <- 0.4; m[3, 4] <- 0.3
  m[5, 6] <- 0.6; m[6, 7] <- 0.5; m[7, 8] <- 0.4
  m[9, 10] <- 0.5; m[10, 11] <- 0.3; m[11, 12] <- 0.4
  m[4, 5] <- 0.1; m[8, 9] <- 0.1

  result <- safe_plot(plot_htna(m, community = "fast_greedy"))
  expect_true(result$success, info = result$error)
})

# ============================================
# Nodes as data.frame with labels
# ============================================

test_that("plot_htna with nodes data.frame (labels column)", {
  m <- make_htna_mat()
  groups <- make_htna_groups()

  nodes_df <- data.frame(
    name = paste0("N", 1:8),
    labels = paste0("Node", 1:8),
    stringsAsFactors = FALSE
  )

  result <- safe_plot(plot_htna(m, groups, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna with nodes data.frame (label column)", {
  m <- make_htna_mat()
  groups <- make_htna_groups()

  nodes_df <- data.frame(
    name = paste0("N", 1:8),
    label = paste0("N", 1:8),
    stringsAsFactors = FALSE
  )

  result <- safe_plot(plot_htna(m, groups, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

# ============================================
# label_abbrev
# ============================================

test_that("plot_htna with label_abbrev", {
  m <- make_htna_mat()
  groups <- make_htna_groups()

  result <- safe_plot(plot_htna(m, groups, label_abbrev = 2))
  expect_true(result$success, info = result$error)
})

# ============================================
# compute_connectivity_jitter_vertical side=second
# ============================================

test_that("compute_connectivity_jitter_vertical with side = 'second'", {
  compute_jitter_v <- cograph:::compute_connectivity_jitter_vertical

  m <- matrix(c(0, 0.5, 0, 0.3,
                0.4, 0, 0.2, 0,
                0, 0.1, 0, 0.6,
                0.5, 0, 0.3, 0), 4, 4, byrow = TRUE)
  g1_idx <- 1:2
  g2_idx <- 3:4

  jitter <- compute_jitter_v(m, g1_idx, g2_idx, amount = 0.5, side = "group2")

  expect_length(jitter, 4)
  expect_true(all(jitter[g1_idx] == 0))  # Group 1 unaffected
})

test_that("compute_connectivity_jitter_horizontal with side = 'second'", {
  compute_jitter_h <- cograph:::compute_connectivity_jitter_horizontal

  m <- matrix(c(0, 0.5, 0, 0.3,
                0.4, 0, 0.2, 0,
                0, 0.1, 0, 0.6,
                0.5, 0, 0.3, 0), 4, 4, byrow = TRUE)
  g1_idx <- 1:2
  g2_idx <- 3:4

  jitter <- compute_jitter_h(m, g1_idx, g2_idx, amount = 0.5, side = "group2")

  expect_length(jitter, 4)
  expect_true(all(jitter[g1_idx] == 0))
})

# ============================================
# compute_polygon_layout outward direction flip
# ============================================

test_that("compute_polygon_layout handles outward direction flip", {
  compute_poly <- cograph:::compute_polygon_layout

  # 3 groups, 3 nodes each
  node_list <- list(G1 = paste0("N", 1:3), G2 = paste0("N", 4:6), G3 = paste0("N", 7:9))
  lab <- paste0("N", 1:9)
  group_indices <- list(1:3, 4:6, 7:9)
  result <- compute_poly(node_list, lab, group_indices, n_sides = 3)

  expect_equal(length(result$x), 9)
  expect_equal(length(result$y), 9)
  expect_true(all(is.finite(result$x)))
  expect_true(all(is.finite(result$y)))
})

test_that("compute_polygon_layout with 5 groups (pentagon)", {
  compute_poly <- cograph:::compute_polygon_layout

  node_list <- lapply(1:5, function(i) paste0("N", ((i - 1) * 3 + 1):(i * 3)))
  names(node_list) <- paste0("G", 1:5)
  lab <- paste0("N", 1:15)
  group_indices <- lapply(1:5, function(i) ((i - 1) * 3 + 1):(i * 3))
  result <- compute_poly(node_list, lab, group_indices, n_sides = 5)

  expect_equal(length(result$x), 15)
  expect_equal(length(result$y), 15)
  expect_true(all(is.finite(result$x)))
  expect_true(all(is.finite(result$y)))
})

test_that("compute_polygon_layout with 6 groups (hexagon)", {
  compute_poly <- cograph:::compute_polygon_layout

  node_list <- lapply(1:6, function(i) paste0("N", ((i - 1) * 2 + 1):(i * 2)))
  names(node_list) <- paste0("G", 1:6)
  lab <- paste0("N", 1:12)
  group_indices <- lapply(1:6, function(i) ((i - 1) * 2 + 1):(i * 2))
  result <- compute_poly(node_list, lab, group_indices, n_sides = 6)

  expect_equal(length(result$x), 12)
  expect_true(all(is.finite(result$x)))
})

# ============================================
# plot_htna with polygon 5+ groups
# ============================================

test_that("plot_htna works with polygon layout 5 groups", {
  set.seed(42)
  n <- 15
  nodes <- paste0("N", 1:n)
  m <- matrix(runif(n * n, 0, 0.3), n, n)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  groups5 <- list(
    G1 = paste0("N", 1:3),
    G2 = paste0("N", 4:6),
    G3 = paste0("N", 7:9),
    G4 = paste0("N", 10:12),
    G5 = paste0("N", 13:15)
  )

  result <- safe_plot(plot_htna(m, groups5, layout = "polygon"))
  expect_true(result$success, info = result$error)
})

# ============================================
# Lines 329-330: jitter=TRUE (boolean) with vertical layout
# ============================================

test_that("plot_htna with jitter=TRUE boolean in vertical layout", {
  m <- make_htna_mat()
  groups <- make_htna_groups()

  result <- safe_plot(
    plot_htna(m, groups, jitter = TRUE, orientation = "vertical")
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Line 459: nodes_df without label/labels columns
# ============================================

test_that("plot_htna falls back to lab when nodes_df has no label columns", {
  m <- make_htna_mat()
  groups <- make_htna_groups()

  # nodes data.frame without labels/label columns
  nodes_df <- data.frame(
    name = paste0("N", 1:8),
    color = rep("blue", 8),
    stringsAsFactors = FALSE
  )

  result <- safe_plot(plot_htna(m, groups, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

# ============================================
# Line 521: unrecognized shape defaults to pch=21
# ============================================

test_that("plot_htna handles unrecognized group_shapes gracefully", {
  m <- make_htna_mat()
  groups <- make_htna_groups()

  # Use shapes that aren't in the predefined list
  result <- safe_plot(
    plot_htna(m, groups, group_shapes = c("unknown_shape", "another_shape"))
  )
  expect_true(result$success, info = result$error)
})
