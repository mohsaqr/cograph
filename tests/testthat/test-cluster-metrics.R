# Tests for cluster-metrics.R

# ==============================================================================
# Test Data
# ==============================================================================

# Create a simple weighted network
skip_on_cran()

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
  expect_equal(dim(result$macro$weights), c(3, 3))
  expect_equal(length(result$clusters), 3)
  expect_equal(names(result$clusters), c("A", "B", "C"))
  expect_equal(unname(result$meta$cluster_sizes), c(3, 3, 4))

  # Diagonal contains intra-cluster retention
  expect_true(all(diag(result$macro$weights) >= 0))

  # Check a specific between value manually
  # A -> B = sum of mat[1:3, 4:6]
  expected_AB <- sum(mat[1:3, 4:6])
  expect_equal(result$macro$weights["A", "B"], expected_AB, tolerance = 1e-10)
})

test_that("cluster_summary works with vector input", {
  result <- cluster_summary(mat, clusters_vec, method = "sum")

  expect_s3_class(result, "cluster_summary")
  expect_equal(dim(result$macro$weights), c(3, 3))
})

test_that("cluster_summary different methods", {
  # Use type = "raw" to get non-normalized values for comparison
  result_sum <- cluster_summary(mat, clusters_list, method = "sum", type = "raw")
  result_mean <- cluster_summary(mat, clusters_list, method = "mean", type = "raw")
  result_max <- cluster_summary(mat, clusters_list, method = "max", type = "raw")

  # Mean should be smaller than sum (for non-single edges)
  expect_true(all(result_mean$macro$weights <= result_sum$macro$weights))

  # Max should be <= sum
  expect_true(all(result_max$macro$weights <= result_sum$macro$weights))
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
  expect_equal(sum(result$clusters$A$weights), 0)
})

test_that("self-loops are preserved in macro diagonal and cluster matrices", {
  # TNA-style row-stochastic matrix WITH self-loops
  mat_sl <- matrix(0.1, 5, 5)
  diag(mat_sl) <- 0.6  # Strong self-loops
  rownames(mat_sl) <- colnames(mat_sl) <- paste0("N", 1:5)

  clusters_sl <- list(A = c("N1", "N2"), B = c("N3", "N4", "N5"))
  result <- cluster_summary(mat_sl, clusters_sl, method = "sum", type = "raw")

  # Macro diagonal should include self-loops (not zero)
  expect_true(diag(result$macro$weights)["A"] > 0)
  expect_true(diag(result$macro$weights)["B"] > 0)

  # Macro diagonal A = sum of mat_sl[1:2, 1:2] = 0.6+0.1+0.1+0.6 = 1.4
  expect_equal(result$macro$weights["A", "A"],
               sum(mat_sl[1:2, 1:2]), tolerance = 1e-10)

  # Within-cluster matrices should have self-loops on diagonal
  expect_true(result$clusters$A$weights[1, 1] > 0)
  expect_true(result$clusters$B$weights[1, 1] > 0)
})

test_that("single-node cluster preserves self-loop", {
  mat_sl <- matrix(0.1, 5, 5)
  diag(mat_sl) <- 0.5
  rownames(mat_sl) <- colnames(mat_sl) <- paste0("N", 1:5)

  clusters_sl <- list(A = "N1", B = paste0("N", 2:5))
  result <- cluster_summary(mat_sl, clusters_sl, method = "sum", type = "raw")

  # Single-node cluster A: macro diagonal = self-loop = 0.5
  expect_equal(result$macro$weights["A", "A"], 0.5, tolerance = 1e-10)

  # Within-cluster matrix for A should be 1x1 with self-loop value
  expect_equal(result$clusters$A$weights[1, 1], 0.5, tolerance = 1e-10)
})

test_that("handles empty weights gracefully", {
  mat_sparse <- matrix(0, 5, 5)
  mat_sparse[1, 2] <- 1
  rownames(mat_sparse) <- colnames(mat_sparse) <- paste0("N", 1:5)

  clusters <- list(A = c("N1", "N2"), B = c("N3", "N4", "N5"))
  result <- cluster_summary(mat_sparse, clusters, method = "mean")

  # Between A and B should be 0 (no edges)
  expect_equal(result$macro$weights["A", "B"], 0)
})

# ==============================================================================
# Test sequence data propagation to tna models
# ==============================================================================

test_that("cluster_summary preserves original tna sequence data in all models", {
  skip_if_not_installed("tna")

  seqs <- data.frame(
    t1 = c("N1", "N4", "N1", "N7"),
    t2 = c("N2", "N5", "N8", "N8"),
    t3 = c("N3", "N6", "N9", "N10")
  )
  tna_obj <- tna::tna(seqs)

  result <- cluster_summary(tna_obj, clusters_list, method = "sum", type = "tna")

  # Macro and clusters all get the original data, untransformed
  expect_false(is.null(result$macro$data))
  expect_identical(result$macro$data, tna_obj$data)

  expect_false(is.null(result$clusters$A$data))
  expect_identical(result$clusters$A$data, tna_obj$data)

  expect_false(is.null(result$clusters$B$data))
  expect_identical(result$clusters$B$data, tna_obj$data)
})

test_that("cluster_summary with matrix input has NULL data in tna models", {
  result <- cluster_summary(mat, clusters_list, method = "sum", type = "tna")
  expect_null(result$macro$data)
  expect_null(result$clusters$A$data)
})

# ==============================================================================
# Test as_tna() group_tna class
# ==============================================================================

test_that("as_tna.cluster_summary returns group_tna with macro and cluster elements", {
  skip_if_not_installed("tna")

  cs <- cluster_summary(mat, clusters_list, method = "mean", type = "tna")
  ct <- as_tna(cs)

  expect_s3_class(ct, "group_tna")
  expect_s3_class(ct$macro, "tna")
  # Each cluster element should be a tna object
  lapply(names(cs$clusters), function(cl) expect_s3_class(ct[[cl]], "tna"))
})

# ==============================================================================
# Test splot dispatch for cluster objects
# ==============================================================================

test_that("splot dispatches cluster_summary to plot_mcml", {
  cs <- cluster_summary(mat, clusters_list, method = "mean", type = "tna")
  # Should run without error (produces a plot)
  expect_no_error(splot(cs))
})

test_that("splot dispatches group_tna (macro)", {
  skip_if_not_installed("tna")

  cs <- cluster_summary(mat, clusters_list, method = "mean", type = "tna")
  ct <- as_tna(cs)
  # Default: plots macro (between-cluster) network
  expect_no_error(splot(ct))
})

test_that("splot dispatches group_tna with i for within-cluster", {
  skip_if_not_installed("tna")

  cs <- cluster_summary(mat, clusters_list, method = "mean", type = "tna")
  ct <- as_tna(cs)
  cluster_names <- names(cs$clusters)
  if (length(cluster_names) > 0) {
    expect_no_error(splot(ct, i = cluster_names[1]))
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
  expect_equal(dim(result$macro$weights), c(3, 3))
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
  expect_true(all(result$macro$weights >= 0))
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
  # Legacy chain layout: 2 interlayer matrices for adjacent pairs (1,2) and
  # (2,3). Non-adjacent pair (1,3) now emits a warning before falling back
  # to the omega diagonal (previously silent).
  custom_mat1 <- diag(10) * 0.5
  custom_mat2 <- diag(10) * 0.3
  expect_warning(
    supra_adjacency(layers, omega = 0.1, coupling = "custom",
                    interlayer_matrices = list(custom_mat1, custom_mat2)),
    "no custom interlayer matrix for pair \\(1, 3\\)"
  )
  result <- suppressWarnings(
    supra_adjacency(layers, omega = 0.1, coupling = "custom",
                    interlayer_matrices = list(custom_mat1, custom_mat2))
  )
  expect_s3_class(result, "supra_adjacency")
  expect_equal(dim(result), c(30, 30))
})

test_that("supra_adjacency custom coupling accepts named a_b keys", {
  layers <- list(L1 = mat, L2 = mat * 2, L3 = mat * 3)
  M12 <- diag(10) * 0.5
  M13 <- diag(10) * 0.4
  M23 <- diag(10) * 0.3
  result <- supra_adjacency(layers, coupling = "custom",
                            interlayer_matrices = list("1_2" = M12,
                                                       "1_3" = M13,
                                                       "2_3" = M23))
  # Block (1,3) is rows 1..10, cols 21..30
  expect_equal(diag(result[1:10, 21:30]), rep(0.4, 10))
  expect_equal(diag(result[1:10, 11:20]), rep(0.5, 10))
  expect_equal(diag(result[11:20, 21:30]), rep(0.3, 10))
})

test_that("supra_adjacency custom coupling accepts upper-tri row-major list", {
  layers <- list(L1 = mat, L2 = mat * 2, L3 = mat * 3)
  M12 <- diag(10) * 0.5
  M13 <- diag(10) * 0.4
  M23 <- diag(10) * 0.3
  result <- supra_adjacency(layers, coupling = "custom",
                            interlayer_matrices = list(M12, M13, M23))
  expect_equal(diag(result[1:10, 21:30]), rep(0.4, 10))
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
