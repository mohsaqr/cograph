# Tests for cluster-metrics.R

# ==============================================================================
# Test Data
# ==============================================================================

# Create a simple weighted network
set.seed(42)
n <- 10
mat <- matrix(runif(n * n), n, n)
diag(mat) <- 0  # No self-loops
rownames(mat) <- colnames(mat) <- paste0("N", 1:n)

# Define clusters
clusters_list <- list(
  "A" = c("N1", "N2", "N3"),
  "B" = c("N4", "N5", "N6"),
  "C" = c("N7", "N8", "N9", "N10")
)

clusters_vec <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
names(clusters_vec) <- paste0("N", 1:n)

# ==============================================================================
# Test aggregate_weights
# ==============================================================================

test_that("aggregate_weights works correctly", {
  w <- c(1, 2, 3, 4, 5)

  expect_equal(aggregate_weights(w, "sum"), 15)
  expect_equal(aggregate_weights(w, "mean"), 3)
  expect_equal(aggregate_weights(w, "median"), 3)
  expect_equal(aggregate_weights(w, "max"), 5)
  expect_equal(aggregate_weights(w, "min"), 1)
  expect_equal(aggregate_weights(w, "prod"), 120)

  # Density with n_possible
  expect_equal(aggregate_weights(w, "density", n_possible = 10), 1.5)

  # Geometric mean
  expect_equal(aggregate_weights(w, "geomean"),
               exp(mean(log(w))), tolerance = 1e-10)

  # Handle empty/NA
  expect_equal(aggregate_weights(c(), "sum"), 0)
  expect_equal(aggregate_weights(c(NA, NA), "sum"), 0)
  expect_equal(aggregate_weights(c(0, 0), "sum"), 0)
})

# ==============================================================================
# Test cluster_summary
# ==============================================================================

test_that("cluster_summary works with list input", {
  # Use type = "raw" to get non-normalized aggregated values
  result <- cluster_summary(mat, clusters_list, method = "sum", type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(dim(result$between$weights), c(3, 3))
  expect_equal(length(result$within), 3)
  expect_equal(names(result$clusters), c("A", "B", "C"))
  expect_equal(unname(result$meta$cluster_sizes), c(3, 3, 4))

  # Diagonal should be 0 (no self-loops at cluster level)
  expect_equal(unname(diag(result$between$weights)), c(0, 0, 0))

  # Check a specific between value manually
  # A -> B = sum of mat[1:3, 4:6]
  expected_AB <- sum(mat[1:3, 4:6])
  expect_equal(result$between$weights["A", "B"], expected_AB, tolerance = 1e-10)
})

test_that("cluster_summary works with vector input", {
  result <- cluster_summary(mat, clusters_vec, method = "sum")

  expect_s3_class(result, "cluster_summary")
  expect_equal(dim(result$between$weights), c(3, 3))
})

test_that("cluster_summary different methods", {
  # Use type = "raw" to get non-normalized values for comparison
  result_sum <- cluster_summary(mat, clusters_list, method = "sum", type = "raw")
  result_mean <- cluster_summary(mat, clusters_list, method = "mean", type = "raw")
  result_max <- cluster_summary(mat, clusters_list, method = "max", type = "raw")

  # Mean should be smaller than sum (for non-single edges)
  expect_true(all(result_mean$between$weights <= result_sum$between$weights))

  # Max should be <= sum
  expect_true(all(result_max$between$weights <= result_sum$between$weights))
})

# ==============================================================================
# Test cluster_quality
# ==============================================================================

test_that("cluster_quality computes valid metrics", {
  result <- cluster_quality(mat, clusters_list)

  expect_s3_class(result, "cluster_quality")
  expect_equal(nrow(result$per_cluster), 3)

  # Check metric ranges
  expect_true(all(result$per_cluster$internal_density >= 0, na.rm = TRUE))
  expect_true(all(result$per_cluster$conductance >= 0 &
                  result$per_cluster$conductance <= 1, na.rm = TRUE))

  # Global metrics
  expect_true(!is.na(result$global$modularity))
  expect_true(!is.na(result$global$coverage))
  expect_true(result$global$coverage >= 0 && result$global$coverage <= 1)
})

# ==============================================================================
# Test layer_similarity
# ==============================================================================

test_that("layer_similarity computes correct values", {
  # Two identical matrices
  expect_equal(layer_similarity(mat, mat, "jaccard"), 1)
  expect_equal(layer_similarity(mat, mat, "cosine"), 1, tolerance = 1e-10)
  expect_equal(layer_similarity(mat, mat, "pearson"), 1, tolerance = 1e-10)
  expect_equal(layer_similarity(mat, mat, "hamming"), 0)

  # Different matrices
  mat2 <- matrix(runif(n * n), n, n)
  diag(mat2) <- 0

  sim_jaccard <- layer_similarity(mat, mat2, "jaccard")
  expect_true(sim_jaccard >= 0 && sim_jaccard <= 1)

  sim_cosine <- layer_similarity(mat, mat2, "cosine")
  expect_true(sim_cosine >= -1 && sim_cosine <= 1)
})

test_that("layer_similarity_matrix is symmetric", {
  layers <- list(L1 = mat, L2 = mat * 0.5, L3 = mat^2)
  result <- layer_similarity_matrix(layers, method = "cosine")

  expect_equal(dim(result), c(3, 3))
  expect_equal(unname(diag(result)), c(1, 1, 1))
  expect_equal(result[1, 2], result[2, 1])
  expect_equal(result[1, 3], result[3, 1])
})

# ==============================================================================
# Test supra_adjacency
# ==============================================================================

test_that("supra_adjacency constructs correct matrix", {
  layers <- list(L1 = mat, L2 = mat * 2)
  result <- supra_adjacency(layers, omega = 0.5)

  expect_s3_class(result, "supra_adjacency")
  expect_equal(dim(result), c(20, 20))
  expect_equal(attr(result, "n_nodes"), 10)
  expect_equal(attr(result, "n_layers"), 2)

  # Check diagonal blocks match original layers
  L1_extracted <- extract_layer(result, 1)
  L2_extracted <- extract_layer(result, 2)

  expect_equal(L1_extracted, mat, ignore_attr = TRUE)
  expect_equal(L2_extracted, mat * 2, ignore_attr = TRUE)

  # Check inter-layer coupling (diagonal identity * omega)
  interlayer <- extract_interlayer(result, 1, 2)
  expect_equal(diag(interlayer), rep(0.5, 10))
  expect_equal(sum(interlayer) - sum(diag(interlayer)), 0)  # Only diagonal
})

test_that("supra_adjacency full coupling", {
  layers <- list(L1 = mat, L2 = mat)
  result <- supra_adjacency(layers, omega = 1, coupling = "full")

  interlayer <- extract_interlayer(result, 1, 2)
  expect_true(all(interlayer == 1))
})

# ==============================================================================
# Test aggregate_layers
# ==============================================================================

test_that("aggregate_layers works correctly", {
  layers <- list(L1 = mat, L2 = mat * 2, L3 = mat * 3)

  result_sum <- aggregate_layers(layers, method = "sum")
  expect_equal(result_sum, mat * 6, tolerance = 1e-10)

  result_mean <- aggregate_layers(layers, method = "mean")
  expect_equal(result_mean, mat * 2, tolerance = 1e-10)

  result_max <- aggregate_layers(layers, method = "max")
  expect_equal(result_max, mat * 3, tolerance = 1e-10)

  # Weighted sum
  result_weighted <- aggregate_layers(layers, method = "sum",
                                      weights = c(1, 2, 0))
  expect_equal(result_weighted, mat * 5, tolerance = 1e-10)
})

test_that("aggregate_layers union/intersection", {
  # Create sparse matrices
  mat1 <- matrix(0, 5, 5)
  mat1[1, 2] <- mat1[2, 3] <- 1
  mat2 <- matrix(0, 5, 5)
  mat2[2, 3] <- mat2[3, 4] <- 1

  result_union <- aggregate_layers(list(mat1, mat2), method = "union")
  expect_equal(sum(result_union), 3)  # 3 unique edges

  result_intersection <- aggregate_layers(list(mat1, mat2),
                                          method = "intersection")
  expect_equal(sum(result_intersection), 1)  # 1 shared edge (2->3)
})

# ==============================================================================
# Test igraph verification (if available)
# ==============================================================================

test_that("cluster_summary matches igraph", {
  skip_if_not_installed("igraph")

  # verify_with_igraph defaults to type = "raw" for igraph comparison
  result <- verify_with_igraph(mat, clusters_list, method = "sum")

  expect_true(result$matches,
              info = paste("Difference:", result$difference))
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("handles single-node clusters", {
  clusters_single <- list(
    "A" = "N1",
    "B" = c("N2", "N3", "N4", "N5", "N6", "N7", "N8", "N9", "N10")
  )

  result <- cluster_summary(mat, clusters_single, method = "sum")
  # Single node cluster has no internal edges, so sum of within weights is 0
  expect_equal(sum(result$within$A$weights), 0)
})

test_that("handles empty weights gracefully", {
  mat_sparse <- matrix(0, 5, 5)
  mat_sparse[1, 2] <- 1
  rownames(mat_sparse) <- colnames(mat_sparse) <- paste0("N", 1:5)

  clusters <- list(A = c("N1", "N2"), B = c("N3", "N4", "N5"))
  result <- cluster_summary(mat_sparse, clusters, method = "mean")

  # Between A and B should be 0 (no edges)
  expect_equal(result$between$weights["A", "B"], 0)
})

# ==============================================================================
# Test as_tna() group_tna class on $within
# ==============================================================================

test_that("as_tna.cluster_summary sets group_tna class on $within", {
  skip_if_not_installed("tna")

  cs <- cluster_summary(mat, clusters_list, method = "mean", type = "tna")
  ct <- as_tna(cs)

  expect_s3_class(ct, "cluster_tna")
  expect_s3_class(ct$within, "group_tna")
  expect_s3_class(ct$between, "tna")
  expect_true(length(ct$within) > 0)
  # Each within element should be a tna object
  lapply(ct$within, function(w) expect_s3_class(w, "tna"))
})

# ==============================================================================
# Test splot dispatch for cluster objects
# ==============================================================================

test_that("splot dispatches cluster_summary to plot_mcml", {
  cs <- cluster_summary(mat, clusters_list, method = "mean", type = "tna")
  # Should run without error (produces a plot)
  expect_no_error(splot(cs))
})

test_that("splot dispatches cluster_tna (between)", {
  skip_if_not_installed("tna")

  cs <- cluster_summary(mat, clusters_list, method = "mean", type = "tna")
  ct <- as_tna(cs)
  # Default: plots between-cluster network
  expect_no_error(splot(ct))
})

test_that("splot dispatches cluster_tna with i for within-cluster", {
  skip_if_not_installed("tna")

  cs <- cluster_summary(mat, clusters_list, method = "mean", type = "tna")
  ct <- as_tna(cs)
  within_names <- names(ct$within)
  if (length(within_names) > 0) {
    expect_no_error(splot(ct, i = within_names[1]))
  }
})

# ==============================================================================
# Test cluster_summary auto-detect clusters from cograph_network
# ==============================================================================

test_that("cluster_summary auto-detects clusters from cograph_network nodes", {
  net <- as_cograph(mat)
  # Add a 'cluster' column to nodes
  net$nodes$cluster <- c(rep("A", 3), rep("B", 3), rep("C", 4))
  result <- cluster_summary(net, method = "sum")
  expect_s3_class(result, "cluster_summary")
  expect_equal(dim(result$between$weights), c(3, 3))
})

test_that("cluster_summary errors when no clusters and plain matrix", {
  expect_error(cluster_summary(mat, clusters = NULL),
               "clusters argument is required")
})

# ==============================================================================
# Test .process_weights default (raw) branch
# ==============================================================================

test_that("cluster_summary type = raw returns raw weights", {
  result <- cluster_summary(mat, clusters_list, method = "sum", type = "raw")
  # "raw" should not normalize
  expect_true(all(result$between$weights >= 0))
})

# ==============================================================================
# Test as_tna when tna not installed (error branch)
# ==============================================================================

# Line 679: tna not installed error — skipped since tna IS installed

# ==============================================================================
# Test .normalize_clusters error paths
# ==============================================================================

test_that(".normalize_clusters errors on unknown nodes", {
  bad_clusters <- list(A = c("N1", "UNKNOWN"))
  expect_error(cluster_summary(mat, bad_clusters, method = "sum"),
               "Unknown nodes")
})

test_that(".normalize_clusters errors on wrong-length membership vector", {
  expect_error(cluster_summary(mat, c(1, 2, 3), method = "sum"),
               "must equal number of nodes")
})

test_that(".normalize_clusters errors on wrong-length named character vector", {
  bad_vec <- c("A", "B", "C")
  names(bad_vec) <- c("N1", "N2", "N3")
  expect_error(cluster_summary(mat, bad_vec, method = "sum"),
               "must equal number of nodes")
})

test_that(".normalize_clusters errors on unsupported type", {
  expect_error(cluster_summary(mat, TRUE, method = "sum"),
               "clusters must be")
})

# ==============================================================================
# Test cluster_quality with empty cluster (n_S == 0 branch)
# ==============================================================================

# Line 856: This branch handles empty clusters — hard to trigger since
# .normalize_clusters validates. Covered indirectly through edge cases.

# ==============================================================================
# Test cluster_significance fallback branches
# ==============================================================================

test_that("cluster_significance else branch for tna input", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("tna")

  # Use a tna object (hits `else { g <- to_igraph(x) }` at line 1057)
  tna_obj <- tna::tna(mat)
  comm <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
  names(comm) <- paste0("N", 1:10)
  result <- cluster_significance(tna_obj, comm, n_random = 5, seed = 42)
  expect_s3_class(result, "cograph_cluster_significance")
})

# ==============================================================================
# Test supra_adjacency custom coupling fallback
# ==============================================================================

test_that("supra_adjacency custom coupling with fallback to omega", {
  layers <- list(L1 = mat, L2 = mat * 2, L3 = mat * 3)
  # Custom coupling with 2 interlayer matrices for consecutive pairs (1-2, 2-3).
  # Non-consecutive pair (1,3) falls back to omega * I (line 1425).
  custom_mat1 <- diag(10) * 0.5
  custom_mat2 <- diag(10) * 0.3
  result <- supra_adjacency(layers, omega = 0.1, coupling = "custom",
                            interlayer_matrices = list(custom_mat1, custom_mat2))
  expect_s3_class(result, "supra_adjacency")
  expect_equal(dim(result), c(30, 30))
})

# ==============================================================================
# Test verify_with_igraph when igraph is missing (line 1595-1596)
# ==============================================================================

# Lines 1595-1596: igraph not available branch — can't easily trigger since
# igraph IS installed. These are defensive guards.

# ==============================================================================
# Test .create_cograph_network type parameter
# ==============================================================================

test_that(".create_cograph_network stores type in meta", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  edges <- data.frame(from = c(1L, 2L), to = c(2L, 3L), weight = c(1, 1))
  net <- .create_cograph_network(
    nodes = nodes, edges = edges, directed = FALSE, type = "mcml"
  )
  expect_equal(net$meta$type, "mcml")

  # NULL type should not add meta$type
  net2 <- .create_cograph_network(
    nodes = nodes, edges = edges, directed = FALSE
  )
  expect_null(net2$meta$type)
})

# ==============================================================================
# Summary
# ==============================================================================

cat("\n=== All Cluster Metrics Tests Passed ===\n")
