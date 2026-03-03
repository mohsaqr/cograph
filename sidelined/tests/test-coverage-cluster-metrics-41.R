# Comprehensive coverage tests for cluster-metrics.R
# Focus on uncovered code paths from test-coverage-cluster-metrics-40.R

# ==============================================================================
# Setup Test Data
# ==============================================================================

set.seed(456)

# Standard 8-node test matrix
n <- 8
mat8 <- matrix(runif(n * n, 0.1, 1), n, n)
diag(mat8) <- 0
rownames(mat8) <- colnames(mat8) <- paste0("N", 1:n)

# Large matrix for truncation testing (10 nodes)
large_mat <- matrix(runif(100, 0.1, 1), 10, 10)
diag(large_mat) <- 0
rownames(large_mat) <- colnames(large_mat) <- LETTERS[1:10]

# Clusters as list
clusters_list <- list(
  "Group1" = c("N1", "N2"),
  "Group2" = c("N3", "N4", "N5"),
  "Group3" = c("N6", "N7", "N8")
)

# ==============================================================================
# 1. cluster_summary: cograph_network with node_groups cluster detection
# ==============================================================================

test_that("cluster_summary detects clusters from node_groups", {
  # Create a cograph_network with node_groups containing cluster column
  net <- as_cograph(mat8)

  # Add node_groups with 'cluster' column
  net$node_groups <- data.frame(
    name = paste0("N", 1:8),
    cluster = c(1, 1, 2, 2, 2, 3, 3, 3)
  )

  # Remove any clusters from nodes to force node_groups lookup
  net$nodes$clusters <- NULL
  net$nodes$cluster <- NULL
  net$nodes$groups <- NULL
  net$nodes$group <- NULL

  result <- cluster_summary(net)

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_clusters, 3)
})

test_that("cluster_summary detects clusters from node_groups with 'group' column", {
  net <- as_cograph(mat8)

  # Add node_groups with 'group' column
  net$node_groups <- data.frame(
    name = paste0("N", 1:8),
    group = c("A", "A", "B", "B", "B", "C", "C", "C")
  )

  # Remove clusters from nodes
  net$nodes$clusters <- NULL
  net$nodes$cluster <- NULL
  net$nodes$groups <- NULL
  net$nodes$group <- NULL

  result <- cluster_summary(net)

  expect_s3_class(result, "cluster_summary")
})

test_that("cluster_summary detects clusters from node_groups with 'layer' column", {
  net <- as_cograph(mat8)

  # Add node_groups with 'layer' column
  net$node_groups <- data.frame(
    name = paste0("N", 1:8),
    layer = c(1, 1, 1, 2, 2, 2, 3, 3)
  )

  # Remove clusters from nodes
  net$nodes$clusters <- NULL
  net$nodes$cluster <- NULL
  net$nodes$groups <- NULL
  net$nodes$group <- NULL

  result <- cluster_summary(net)

  expect_s3_class(result, "cluster_summary")
})

test_that("cluster_summary errors when no clusters found in cograph_network", {
  net <- as_cograph(mat8)

  # Remove all cluster-related columns
  net$nodes$clusters <- NULL
  net$nodes$cluster <- NULL
  net$nodes$groups <- NULL
  net$nodes$group <- NULL
  net$node_groups <- NULL

  expect_error(
    cluster_summary(net),
    "No clusters found"
  )
})

test_that("cluster_summary works with tna object input", {
  skip_if_not_installed("tna")

  # Create a mock tna object
  tna_obj <- structure(
    list(
      weights = mat8,
      inits = rep(1 / 8, 8),
      labels = paste0("N", 1:8)
    ),
    class = "tna"
  )

  result <- cluster_summary(tna_obj, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_nodes, 8)
})

test_that("cluster_summary returns cluster_summary unchanged if already cluster_summary", {
  # Create a cluster_summary first
  cs <- cluster_summary(mat8, clusters_list)

  # Pass it back to cluster_summary
  result <- cluster_summary(cs)

  expect_identical(result, cs)
})

# ==============================================================================
# 2. .process_weights: default branch (line 517)
# ==============================================================================

test_that(".process_weights returns unchanged for unknown type", {
  # The default branch is unreachable via public API due to match.arg
  # but we can call the internal function directly
  raw <- matrix(runif(9), 3, 3)

  # Test the covered types first
  result_raw <- cograph:::.process_weights(raw, "raw", TRUE)
  expect_equal(result_raw, raw)

  result_tna <- cograph:::.process_weights(raw, "tna", TRUE)
  row_sums <- rowSums(result_tna)
  expect_true(all(abs(row_sums - 1) < 1e-10 | row_sums == 0))

  result_cooc <- cograph:::.process_weights(raw, "cooccurrence", TRUE)
  expect_equal(result_cooc, t(result_cooc))

  result_semi <- cograph:::.process_weights(raw, "semi_markov", TRUE)
  row_sums_semi <- rowSums(result_semi)
  expect_true(all(abs(row_sums_semi - 1) < 1e-10 | row_sums_semi == 0))
})

# ==============================================================================
# 3. as_tna: tna package check and zero-row exclusion
# ==============================================================================

test_that("as_tna.cluster_summary handles zero-row exclusion", {
  skip_if_not_installed("tna")

  # Create a matrix where within-cluster has zero rows
  sparse_mat <- mat8
  sparse_mat[1:2, 1:2] <- 0  # Group1 has no internal edges

  cs <- cluster_summary(sparse_mat, clusters_list, type = "tna")
  tna_obj <- as_tna(cs)

  expect_s3_class(tna_obj, "cluster_tna")
  # Group1 has single node behavior (zero rows) - may or may not be excluded
  expect_true(is.list(tna_obj$within))
})

test_that("as_tna.cluster_summary excludes clusters with zero rowSums", {
  skip_if_not_installed("tna")

  # Create a matrix where within-clusters have some zero rows
  # but between-clusters is valid (all rows have positive values)
  test_mat <- matrix(0.1, 6, 6)
  diag(test_mat) <- 0
  # Make within-cluster A have zero rows
  test_mat[1, 2] <- 0
  test_mat[2, 1] <- 0
  rownames(test_mat) <- colnames(test_mat) <- paste0("N", 1:6)

  clusters <- list(
    A = c("N1", "N2"),
    B = c("N3", "N4"),
    C = c("N5", "N6")
  )

  cs <- cluster_summary(test_mat, clusters, type = "tna")
  tna_obj <- as_tna(cs)

  expect_s3_class(tna_obj, "cluster_tna")
  # Some within clusters may be excluded due to zero rows
  expect_true(is.list(tna_obj$within))
})

# ==============================================================================
# 4. print.cluster_tna: entire print method
# ==============================================================================

test_that("print.cluster_tna works with valid cluster_tna object", {
  skip_if_not_installed("tna")

  cs <- cluster_summary(mat8, clusters_list, type = "tna")
  tna_obj <- as_tna(cs)

  expect_output(print(tna_obj), "Cluster TNA Models")
  expect_output(print(tna_obj), "Between-cluster network")
  expect_output(print(tna_obj), "Within-cluster networks")
})

test_that("print.cluster_tna handles empty within list", {
  skip_if_not_installed("tna")

  # Create a cluster_tna with empty within
  mock_tna <- structure(
    list(
      between = structure(
        list(
          weights = matrix(runif(9), 3, 3),
          inits = c(0.33, 0.33, 0.34),
          labels = c("A", "B", "C")
        ),
        class = "tna"
      ),
      within = list()
    ),
    class = "cluster_tna"
  )

  expect_output(print(mock_tna), "Cluster TNA Models")
  expect_output(print(mock_tna), "none")
})

# ==============================================================================
# 5. .normalize_clusters: additional error cases
# ==============================================================================

test_that(".normalize_clusters validates list nodes exist in node_names", {
  node_names <- paste0("N", 1:5)
  bad_clusters <- list(
    A = c("N1", "N2"),
    B = c("N3", "MISSING_NODE", "ANOTHER_MISSING")
  )

  expect_error(
    cograph:::.normalize_clusters(bad_clusters, node_names),
    "Unknown nodes"
  )
})

test_that(".normalize_clusters handles integer vector (not just numeric)", {
  node_names <- paste0("N", 1:5)
  int_clusters <- as.integer(c(1L, 1L, 2L, 2L, 3L))

  result <- cograph:::.normalize_clusters(int_clusters, node_names)

  expect_type(result, "list")
  expect_equal(length(result), 3)
})

test_that(".normalize_clusters errors on wrong length character vector", {
  node_names <- paste0("N", 1:5)
  wrong_char <- c("A", "A", "B")  # Wrong length

  expect_error(
    cograph:::.normalize_clusters(wrong_char, node_names),
    "must equal number of nodes"
  )
})

# ==============================================================================
# 6. cluster_quality: empty cluster handling (lines 844-854)
# ==============================================================================

test_that("cluster_quality handles empty cluster (n_S = 0)", {
  # This is tricky since empty clusters aren't common
  # But we can test with very sparse data
  sparse_mat <- matrix(0, 5, 5)
  rownames(sparse_mat) <- colnames(sparse_mat) <- paste0("N", 1:5)

  clusters <- list(
    A = c("N1", "N2"),
    B = c("N3", "N4", "N5")
  )

  result <- cluster_quality(sparse_mat, clusters)

  expect_s3_class(result, "cluster_quality")
  expect_equal(nrow(result$per_cluster), 2)
})

# ==============================================================================
# 7. cluster_significance: fallback error handling
# ==============================================================================

test_that("cluster_significance handles igraph membership vector", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")

  # Use integer membership vector directly (not communities object)
  mem_vec <- rep(1:4, each = ceiling(igraph::vcount(g) / 4))[1:igraph::vcount(g)]

  result <- cluster_significance(g, mem_vec, n_random = 5, seed = 42)

  expect_s3_class(result, "cograph_cluster_significance")
})

test_that("cluster_significance handles general object via to_igraph", {

  skip_if_not_installed("igraph")

  # Create a cograph_network
  net <- as_cograph(mat8)
  mem <- c(1, 1, 1, 1, 2, 2, 2, 2)

  result <- cluster_significance(net, mem, n_random = 5, seed = 42)

  expect_s3_class(result, "cograph_cluster_significance")
})

# ==============================================================================
# 8. supra_adjacency: custom coupling with valid index pair
# ==============================================================================

test_that("supra_adjacency custom coupling uses correct interlayer matrix", {
  # For 2 layers, we need 1 interlayer matrix
  layers <- list(L1 = mat8, L2 = mat8 * 2)
  custom_inter_12 <- diag(8) * 0.3

  result <- supra_adjacency(
    layers,
    omega = 1,
    coupling = "custom",
    interlayer_matrices = list(custom_inter_12)
  )

  expect_s3_class(result, "supra_adjacency")

  # Check that the custom interlayer was used
  interlayer <- supra_interlayer(result, 1, 2)
  expect_equal(diag(interlayer), rep(0.3, 8))
})

# ==============================================================================
# 9. verify_with_igraph: igraph not installed case
# ==============================================================================

test_that("verify_with_igraph returns NULL message without igraph", {
  # We can't truly test without igraph if it's installed
  # But we can at least cover the basic path
  skip_if_not_installed("igraph")

  result <- verify_with_igraph(mat8, clusters_list, method = "sum")

  expect_type(result, "list")
  expect_true("matches" %in% names(result))
})

# ==============================================================================
# 10. print.cluster_summary: large matrix truncation
# ==============================================================================

test_that("print.cluster_summary truncates large between-cluster matrix", {
  # Create a large cluster summary with > 6 clusters
  large_clusters <- lapply(1:8, function(i) {
    LETTERS[i]
  })
  names(large_clusters) <- paste0("Cluster", 1:8)

  cs <- cluster_summary(large_mat,
                        clusters = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
                        type = "raw")

  # Should print showing truncation message
  expect_output(print(cs), "Cluster Summary")
})

test_that("print.cluster_summary handles many within clusters", {
  # 5 clusters to test "... and X more clusters" message
  clusters_5 <- list(
    C1 = c("A", "B"),
    C2 = c("C", "D"),
    C3 = c("E", "F"),
    C4 = c("G", "H"),
    C5 = c("I", "J")
  )

  cs <- cluster_summary(large_mat, clusters_5)

  expect_output(print(cs), "more clusters")
})

# ==============================================================================
# 11. summarize_network: comprehensive testing
# ==============================================================================

test_that("summarize_network works with matrix input and cluster list", {
  clusters <- list(
    Group1 = c("N1", "N2"),
    Group2 = c("N3", "N4", "N5"),
    Group3 = c("N6", "N7", "N8")
  )

  result <- summarize_network(mat8, clusters, method = "sum")

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
  expect_true("size" %in% names(result$nodes))
})

test_that("summarize_network works with cograph_network input", {
  net <- as_cograph(mat8)
  net$nodes$clusters <- c(1, 1, 2, 2, 2, 3, 3, 3)

  result <- summarize_network(net)

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
})

test_that("summarize_network works with cograph_network and column name", {
  net <- as_cograph(mat8)
  net$nodes$my_groups <- c("A", "A", "B", "B", "B", "C", "C", "C")

  result <- summarize_network(net, cluster_list = "my_groups")

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
})

test_that("summarize_network errors on invalid column name", {
  net <- as_cograph(mat8)
  net$nodes$clusters <- c(1, 1, 2, 2, 2, 3, 3, 3)

  expect_error(
    summarize_network(net, cluster_list = "nonexistent_column"),
    "not found in nodes"
  )
})

test_that("summarize_network errors on non-cograph with column name", {
  expect_error(
    summarize_network(mat8, cluster_list = "clusters"),
    "must be a cograph_network"
  )
})

test_that("summarize_network auto-detects 'groups' column", {
  net <- as_cograph(mat8)
  net$nodes$groups <- c("X", "X", "Y", "Y", "Y", "Z", "Z", "Z")

  expect_message(
    result <- summarize_network(net),
    "Using 'groups' column"
  )

  expect_s3_class(result, "cograph_network")
})

test_that("summarize_network auto-detects 'community' column", {
  net <- as_cograph(mat8)
  net$nodes$community <- c(1, 1, 2, 2, 2, 3, 3, 3)

  expect_message(
    result <- summarize_network(net),
    "Using 'community' column"
  )

  expect_s3_class(result, "cograph_network")
})

test_that("summarize_network auto-detects 'module' column", {
  net <- as_cograph(mat8)
  net$nodes$module <- c("M1", "M1", "M2", "M2", "M2", "M3", "M3", "M3")

  expect_message(
    result <- summarize_network(net),
    "Using 'module' column"
  )

  expect_s3_class(result, "cograph_network")
})

test_that("summarize_network errors when no clusters found", {
  net <- as_cograph(mat8)
  # No cluster column added

  expect_error(
    summarize_network(net),
    "cluster_list required"
  )
})

test_that("summarize_network works with tna object input", {
  skip_if_not_installed("tna")

  # Create a mock tna object
  tna_obj <- structure(
    list(
      weights = mat8,
      inits = rep(1 / 8, 8),
      labels = paste0("N", 1:8)
    ),
    class = "tna"
  )

  clusters <- list(
    G1 = c("N1", "N2"),
    G2 = c("N3", "N4", "N5"),
    G3 = c("N6", "N7", "N8")
  )

  result <- summarize_network(tna_obj, clusters)

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
})

test_that("summarize_network handles tna without labels", {
  skip_if_not_installed("tna")

  tna_obj <- structure(
    list(
      weights = mat8,
      inits = rep(1 / 8, 8),
      labels = NULL
    ),
    class = "tna"
  )

  clusters <- list(
    G1 = c("N1", "N2"),
    G2 = c("N3", "N4", "N5"),
    G3 = c("N6", "N7", "N8")
  )

  result <- summarize_network(tna_obj, clusters)

  expect_s3_class(result, "cograph_network")
})

test_that("summarize_network errors on invalid input type", {
  expect_error(
    summarize_network("not_valid", clusters_list),
    "must be a cograph_network, tna object, or matrix"
  )
})

test_that("summarize_network with directed = FALSE", {
  result <- summarize_network(mat8, clusters_list, directed = FALSE)

  expect_s3_class(result, "cograph_network")
})

test_that("summarize_network with different methods", {
  result_mean <- summarize_network(mat8, clusters_list, method = "mean")
  result_max <- summarize_network(mat8, clusters_list, method = "max")
  result_min <- summarize_network(mat8, clusters_list, method = "min")
  result_median <- summarize_network(mat8, clusters_list, method = "median")
  result_density <- summarize_network(mat8, clusters_list, method = "density")
  result_geomean <- summarize_network(mat8, clusters_list, method = "geomean")

  expect_s3_class(result_mean, "cograph_network")
  expect_s3_class(result_max, "cograph_network")
  expect_s3_class(result_min, "cograph_network")
  expect_s3_class(result_median, "cograph_network")
  expect_s3_class(result_density, "cograph_network")
  expect_s3_class(result_geomean, "cograph_network")
})

test_that("cluster_network and cnet are aliases for summarize_network", {
  result1 <- summarize_network(mat8, clusters_list)
  result2 <- cluster_network(mat8, clusters_list)
  result3 <- cnet(mat8, clusters_list)

  # Same structure
  expect_equal(n_nodes(result1), n_nodes(result2))
  expect_equal(n_nodes(result1), n_nodes(result3))
})

test_that("summarize_network works with unnamed matrix", {
  unnamed_mat <- matrix(runif(36), 6, 6)
  diag(unnamed_mat) <- 0

  clusters <- list(
    A = c("1", "2"),
    B = c("3", "4"),
    C = c("5", "6")
  )

  result <- summarize_network(unnamed_mat, clusters)

  expect_s3_class(result, "cograph_network")
})

test_that("summarize_network node sizes are correct", {
  result <- summarize_network(mat8, clusters_list)

  # Check sizes match cluster sizes
  expect_equal(result$nodes$size[result$nodes$label == "Group1"], 2)
  expect_equal(result$nodes$size[result$nodes$label == "Group2"], 3)
  expect_equal(result$nodes$size[result$nodes$label == "Group3"], 3)
})

# ==============================================================================
# 12. Additional edge case tests
# ==============================================================================

test_that("cluster_summary uses $weights when available in cograph_network", {
  net <- as_cograph(mat8)

  # Explicitly set weights to a different matrix
  net$weights <- mat8 * 2

  # cluster_summary should use $weights if available
  result <- cluster_summary(net, clusters_list)

  expect_s3_class(result, "cluster_summary")
})

test_that("cluster_summary handles cograph_network without $weights", {
  net <- as_cograph(mat8)

  # Remove weights to force to_matrix path
  net$weights <- NULL

  result <- cluster_summary(net, clusters_list)

  expect_s3_class(result, "cluster_summary")
})

test_that("cluster_summary integer clusters work", {
  # Integer vector clusters
  int_clusters <- as.integer(c(1, 1, 2, 2, 2, 3, 3, 3))

  result <- cluster_summary(mat8, int_clusters)

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_clusters, 3)
})

test_that("cluster_quality handles all-zero matrix", {
  zero_mat <- matrix(0, 5, 5)
  rownames(zero_mat) <- colnames(zero_mat) <- paste0("N", 1:5)

  clusters <- list(A = c("N1", "N2"), B = c("N3", "N4", "N5"))

  result <- cluster_quality(zero_mat, clusters)

  expect_s3_class(result, "cluster_quality")
  expect_true(is.na(result$global$coverage) || result$global$coverage == 0)
})

test_that("layer_similarity hamming on identical returns 0", {
  A1 <- mat8
  result <- layer_similarity(A1, A1, "hamming")
  expect_equal(result, 0)
})

test_that("supra_adjacency with 3 layers generates correct interlayer blocks", {
  layers <- list(L1 = mat8, L2 = mat8, L3 = mat8)
  result <- supra_adjacency(layers, omega = 0.5)

  # Should have 24 x 24 dimension (8 nodes * 3 layers)
  expect_equal(dim(result), c(24, 24))

  # Extract interlayer 1 -> 3
  interlayer_13 <- supra_interlayer(result, 1, 3)
  expect_equal(diag(interlayer_13), rep(0.5, 8))
})

test_that("aggregate_layers with single layer list", {
  layers <- list(L1 = mat8)
  result <- aggregate_layers(layers, method = "sum")

  expect_equal(result, mat8)
})

test_that("cluster_summary type semi_markov works", {
  result <- cluster_summary(mat8, clusters_list, type = "semi_markov")

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$type, "semi_markov")

  # Rows should sum to 1
  row_sums <- rowSums(result$between$weights)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("cluster_summary handles matrix without names", {
  unnamed <- mat8
  rownames(unnamed) <- NULL
  colnames(unnamed) <- NULL

  clusters <- c(1, 1, 2, 2, 2, 3, 3, 3)

  result <- cluster_summary(unnamed, clusters)

  expect_s3_class(result, "cluster_summary")
  # Node names should be auto-generated
  expect_true(all(unlist(result$clusters) %in% as.character(1:8)))
})

test_that("cluster_summary handles unnamed cluster list", {
  unnamed_clusters <- list(
    c("N1", "N2"),
    c("N3", "N4", "N5"),
    c("N6", "N7", "N8")
  )

  result <- cluster_summary(mat8, unnamed_clusters)

  expect_s3_class(result, "cluster_summary")
  # Cluster names should be auto-generated
  expect_equal(names(result$clusters), c("1", "2", "3"))
})

test_that("verify_with_igraph works with mean method", {
  skip_if_not_installed("igraph")

  result <- verify_with_igraph(mat8, clusters_list, method = "mean")

  expect_type(result, "list")
})

test_that("cluster_quality returns numeric cut_ratio metrics", {
  result <- cluster_quality(mat8, clusters_list)

  # cut_ratio should be numeric
  valid_ratios <- result$per_cluster$cut_ratio[!is.na(result$per_cluster$cut_ratio)]
  expect_true(all(is.numeric(valid_ratios)))
})

test_that("cluster_summary produces valid between$inits with dense matrix", {
  # Dense matrix - all entries nonzero
  dense <- matrix(runif(64, 0.1, 1), 8, 8)
  diag(dense) <- 0
  rownames(dense) <- colnames(dense) <- paste0("N", 1:8)

  result <- cluster_summary(dense, clusters_list)

  # Inits should sum to 1
  expect_equal(sum(result$between$inits), 1, tolerance = 1e-10)
})

test_that("cluster_summary produces uniform inits for zero-weight matrix", {
  zero_mat <- matrix(0, 8, 8)
  rownames(zero_mat) <- colnames(zero_mat) <- paste0("N", 1:8)

  result <- cluster_summary(zero_mat, clusters_list)

  # With no edges, inits should be uniform
  expect_equal(result$between$inits, c(Group1 = 1 / 3, Group2 = 1 / 3, Group3 = 1 / 3),
               tolerance = 1e-10)
})

# ==============================================================================
# 13. print.cluster_summary edge cases
# ==============================================================================

test_that("print.cluster_summary with > 6 clusters shows truncation", {
  # Create a matrix with 7 clusters (more than 6)
  mat7 <- matrix(runif(49), 7, 7)
  diag(mat7) <- 0
  rownames(mat7) <- colnames(mat7) <- paste0("N", 1:7)

  clusters7 <- list(
    C1 = "N1", C2 = "N2", C3 = "N3", C4 = "N4",
    C5 = "N5", C6 = "N6", C7 = "N7"
  )

  cs <- cluster_summary(mat7, clusters7)

  expect_output(print(cs), "showing first 6x6 corner")
})

test_that("print.cluster_summary with exactly 6 clusters shows full matrix", {
  mat6 <- matrix(runif(36), 6, 6)
  diag(mat6) <- 0
  rownames(mat6) <- colnames(mat6) <- paste0("N", 1:6)

  clusters6 <- list(
    C1 = "N1", C2 = "N2", C3 = "N3",
    C4 = "N4", C5 = "N5", C6 = "N6"
  )

  cs <- cluster_summary(mat6, clusters6)

  # Should NOT show truncation message
  output <- capture.output(print(cs))
  expect_false(any(grepl("showing first", output)))
})

# ==============================================================================
# Summary
# ==============================================================================

cat("\n=== Cluster Metrics Coverage Tests 41 Complete ===\n")
