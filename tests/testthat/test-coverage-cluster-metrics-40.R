# Test file for comprehensive coverage of cluster-metrics.R
# Targets uncovered code paths including edge cases and error handling

# ==============================================================================
# Setup Test Data
# ==============================================================================

# Create test matrices with node names
set.seed(123)
n <- 8
mat <- matrix(runif(n * n), n, n)
diag(mat) <- 0
rownames(mat) <- colnames(mat) <- paste0("N", 1:n)

# Clusters as list
clusters_list <- list(
  "Group1" = c("N1", "N2"),
  "Group2" = c("N3", "N4", "N5"),
  "Group3" = c("N6", "N7", "N8")
)

# Clusters as numeric vector
clusters_vec <- c(1, 1, 2, 2, 2, 3, 3, 3)

# Clusters as factor
clusters_factor <- factor(c("A", "A", "B", "B", "B", "C", "C", "C"))

# Clusters as character vector
clusters_char <- c("A", "A", "B", "B", "B", "C", "C", "C")

# Sparse matrix for edge cases
sparse_mat <- matrix(0, 5, 5)
sparse_mat[1, 2] <- 1
sparse_mat[2, 3] <- 0.5
rownames(sparse_mat) <- colnames(sparse_mat) <- paste0("S", 1:5)

# ==============================================================================
# Tests for aggregate_weights
# ==============================================================================

test_that("aggregate_weights handles density method with n_possible = 0", {
  w <- c(1, 2, 3)
  # When n_possible is 0 or NULL, falls back to length(w)
  result <- aggregate_weights(w, "density", n_possible = 0)
  expect_equal(result, sum(w) / length(w))
})

test_that("aggregate_weights density with n_possible specified", {
  w <- c(1, 2, 3)
  result <- aggregate_weights(w, "density", n_possible = 6)
  expect_equal(result, sum(w) / 6)
})

test_that("aggregate_weights density with NULL n_possible", {
  w <- c(1, 2, 3)
  result <- aggregate_weights(w, "density", n_possible = NULL)
  expect_equal(result, sum(w) / length(w))
})

test_that("aggregate_weights geomean with zeros returns 0", {
  w <- c(0, 0, 0)
  result <- aggregate_weights(w, "geomean")
  expect_equal(result, 0)
})

test_that("aggregate_weights geomean with negative values ignores them", {
  w <- c(-1, -2, 3, 4)
  result <- aggregate_weights(w, "geomean")
  # Only positive values used: exp(mean(log(c(3, 4))))
  expected <- exp(mean(log(c(3, 4))))
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("aggregate_weights geomean with all negative returns 0", {
  w <- c(-1, -2, -3)
  result <- aggregate_weights(w, "geomean")
  expect_equal(result, 0)
})

test_that("aggregate_weights throws error for unknown method", {
  expect_error(
    aggregate_weights(c(1, 2, 3), "unknown_method"),
    "Unknown method"
  )
})

test_that("wagg is an alias for aggregate_weights", {
  w <- c(1, 2, 3, 4, 5)
  expect_identical(wagg(w, "sum"), aggregate_weights(w, "sum"))
  expect_identical(wagg(w, "mean"), aggregate_weights(w, "mean"))
})

test_that("aggregate_weights handles single value", {
  expect_equal(aggregate_weights(5, "sum"), 5)
  expect_equal(aggregate_weights(5, "mean"), 5)
  expect_equal(aggregate_weights(5, "median"), 5)
  expect_equal(aggregate_weights(5, "max"), 5)
  expect_equal(aggregate_weights(5, "min"), 5)
  expect_equal(aggregate_weights(5, "prod"), 5)
  expect_equal(aggregate_weights(5, "geomean"), 5)
})

# ==============================================================================
# Tests for .normalize_clusters
# ==============================================================================

test_that(".normalize_clusters handles factor input", {
  node_names <- paste0("N", 1:8)
  result <- cograph:::.normalize_clusters(clusters_factor, node_names)

  expect_type(result, "list")
  expect_equal(length(result), 3)
  expect_true(all(unlist(result) %in% node_names))
})

test_that(".normalize_clusters handles character input", {
  node_names <- paste0("N", 1:8)
  result <- cograph:::.normalize_clusters(clusters_char, node_names)

  expect_type(result, "list")
  expect_equal(length(result), 3)
})

test_that(".normalize_clusters errors on invalid list nodes", {
  node_names <- paste0("N", 1:5)
  bad_clusters <- list(A = c("N1", "N2", "INVALID"))

  expect_error(
    cograph:::.normalize_clusters(bad_clusters, node_names),
    "Unknown nodes"
  )
})

test_that(".normalize_clusters errors on wrong length membership", {
  node_names <- paste0("N", 1:5)
  wrong_length <- c(1, 2, 3)

  expect_error(
    cograph:::.normalize_clusters(wrong_length, node_names),
    "must equal number of nodes"
  )
})

test_that(".normalize_clusters errors on wrong length factor", {
  node_names <- paste0("N", 1:5)
  wrong_factor <- factor(c("A", "B", "C"))

  expect_error(
    cograph:::.normalize_clusters(wrong_factor, node_names),
    "must equal number of nodes"
  )
})

test_that(".normalize_clusters errors on invalid type", {
  node_names <- paste0("N", 1:5)
  # Use a matrix which is neither a list, nor a vector, nor factor
  invalid <- matrix(1:5, nrow = 1)

  expect_error(
    cograph:::.normalize_clusters(invalid, node_names),
    "must be a list"
  )
})

# ==============================================================================
# Tests for cluster_summary
# ==============================================================================

test_that("cluster_summary validates input is numeric matrix", {
  expect_error(
    cluster_summary("not a matrix", clusters_list),
    "must be a numeric matrix"
  )

  expect_error(
    cluster_summary(data.frame(a = 1:5), clusters_list),
    "must be a numeric matrix"
  )
})

test_that("cluster_summary validates square matrix", {
  rect_mat <- matrix(1:12, 3, 4)
  expect_error(
    cluster_summary(rect_mat, list(A = 1:2, B = 3)),
    "must be a square matrix"
  )
})

test_that("cluster_summary works with unnamed matrix", {
  unnamed_mat <- matrix(runif(25), 5, 5)
  diag(unnamed_mat) <- 0
  clusters <- c(1, 1, 2, 2, 3)

  result <- cluster_summary(unnamed_mat, clusters)
  expect_s3_class(result, "cluster_summary")
})

test_that("cluster_summary works with factor clusters", {
  result <- cluster_summary(mat, clusters_factor)
  expect_s3_class(result, "cluster_summary")
  expect_equal(dim(result$between), c(3, 3))
})

test_that("cluster_summary directed = FALSE", {
  result <- cluster_summary(mat, clusters_list, directed = FALSE)
  expect_s3_class(result, "cluster_summary")
  expect_false(result$directed)
})

test_that("cluster_summary method median", {
  result <- cluster_summary(mat, clusters_list, method = "median")
  expect_equal(result$method, "median")
})

test_that("cluster_summary method min", {
  result <- cluster_summary(mat, clusters_list, method = "min")
  expect_equal(result$method, "min")
})

test_that("cluster_summary method density", {
  result <- cluster_summary(mat, clusters_list, method = "density")
  expect_equal(result$method, "density")
})

test_that("cluster_summary method geomean", {
  result <- cluster_summary(mat, clusters_list, method = "geomean")
  expect_equal(result$method, "geomean")
})

test_that("cluster_summary with unnamed clusters list", {
  unnamed_clusters <- list(c("N1", "N2"), c("N3", "N4"), c("N5", "N6", "N7", "N8"))
  result <- cluster_summary(mat, unnamed_clusters)
  expect_s3_class(result, "cluster_summary")
})

test_that("csum is an alias for cluster_summary", {
  result1 <- cluster_summary(mat, clusters_list, method = "sum")
  result2 <- csum(mat, clusters_list, method = "sum")
  expect_equal(result1$between, result2$between)
})

# ==============================================================================
# Tests for cluster_quality
# ==============================================================================

test_that("cluster_quality validates numeric matrix", {
  expect_error(
    cluster_quality("not a matrix", clusters_list),
    "must be a numeric matrix"
  )
})

test_that("cluster_quality weighted = FALSE", {
  result <- cluster_quality(mat, clusters_list, weighted = FALSE)
  expect_s3_class(result, "cluster_quality")
})

test_that("cluster_quality directed = FALSE", {
  result <- cluster_quality(mat, clusters_list, directed = FALSE)
  expect_s3_class(result, "cluster_quality")
})

test_that("cluster_quality handles unnamed matrix", {
  unnamed_mat <- matrix(runif(25), 5, 5)
  diag(unnamed_mat) <- 0
  clusters <- c(1, 1, 2, 2, 3)

  result <- cluster_quality(unnamed_mat, clusters)
  expect_s3_class(result, "cluster_quality")
})

test_that("cluster_quality returns valid per_cluster metrics", {
  result <- cluster_quality(mat, clusters_list)

  expect_true("cluster_name" %in% names(result$per_cluster))
  expect_true("internal_edges" %in% names(result$per_cluster))
  expect_true("cut_edges" %in% names(result$per_cluster))
  expect_true("expansion" %in% names(result$per_cluster))
  expect_true("cut_ratio" %in% names(result$per_cluster))
})

test_that("cluster_quality handles empty cluster gracefully", {
  # Create a matrix where one cluster has no internal or external edges
  zero_mat <- matrix(0, 5, 5)
  zero_mat[1, 2] <- 1
  zero_mat[2, 1] <- 1
  rownames(zero_mat) <- colnames(zero_mat) <- paste0("N", 1:5)

  clusters <- list(A = c("N1", "N2"), B = c("N3", "N4", "N5"))
  result <- cluster_quality(zero_mat, clusters)

  expect_s3_class(result, "cluster_quality")
})

test_that("cqual is an alias for cluster_quality", {
  result1 <- cluster_quality(mat, clusters_list)
  result2 <- cqual(mat, clusters_list)

  expect_equal(result1$global$modularity, result2$global$modularity)
})

# ==============================================================================
# Tests for .compute_modularity
# ==============================================================================

test_that(".compute_modularity handles zero edge sum", {
  zero_mat <- matrix(0, 5, 5)
  membership <- c(1, 1, 2, 2, 2)

  result <- cograph:::.compute_modularity(zero_mat, membership, directed = TRUE)
  expect_true(is.na(result))
})

test_that(".compute_modularity undirected", {
  membership <- c(1, 1, 2, 2, 2, 3, 3, 3)
  result <- cograph:::.compute_modularity(mat, membership, directed = FALSE)
  expect_true(is.numeric(result))
})

# ==============================================================================
# Tests for cluster_significance
# ==============================================================================

test_that("cluster_significance works with matrix input", {
  skip_if_not_installed("igraph")

  # Create a simple matrix
  test_mat <- matrix(runif(64), 8, 8)
  diag(test_mat) <- 0

  # Simple membership
  mem <- c(1, 1, 1, 1, 2, 2, 2, 2)

  result <- cluster_significance(test_mat, mem, n_random = 10, seed = 42)

  expect_s3_class(result, "cograph_cluster_significance")
  expect_true(!is.na(result$observed_modularity))
  expect_equal(length(result$null_values), 10)
})

test_that("cluster_significance gnm method", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  comm <- igraph::cluster_louvain(g)

  result <- cluster_significance(g, comm, method = "gnm", n_random = 10, seed = 42)

  expect_equal(result$method, "gnm")
  expect_s3_class(result, "cograph_cluster_significance")
})

test_that("cluster_significance errors on invalid communities", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")

  expect_error(
    cluster_significance(g, "invalid_communities"),
    "communities must be"
  )
})

test_that("csig is an alias for cluster_significance", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  comm <- igraph::cluster_louvain(g)

  set.seed(42)
  result1 <- cluster_significance(g, comm, n_random = 5)
  set.seed(42)
  result2 <- csig(g, comm, n_random = 5)

  expect_equal(result1$observed_modularity, result2$observed_modularity)
})

test_that("cluster_significance with cograph_network input", {
  skip_if_not_installed("igraph")

  # Create a cograph_network
  net <- cograph(mat)
  comm <- c(1, 1, 1, 1, 2, 2, 2, 2)

  result <- cluster_significance(net, comm, n_random = 5, seed = 42)
  expect_s3_class(result, "cograph_cluster_significance")
})

# ==============================================================================
# Tests for print.cograph_cluster_significance
# ==============================================================================

test_that("print.cograph_cluster_significance various p-values", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  comm <- igraph::cluster_louvain(g)

  # Get a result
  result <- cluster_significance(g, comm, n_random = 10, seed = 42)

  # Test print output
  expect_output(print(result), "Cluster Significance Test")
  expect_output(print(result), "Null model")
})

test_that("print.cograph_cluster_significance with NA p-value", {
  # Create mock object with NA p-value
  mock_result <- list(
    observed_modularity = 0.5,
    null_mean = 0.3,
    null_sd = 0,
    z_score = NA_real_,
    p_value = NA_real_,
    null_values = rep(0.3, 10),
    method = "configuration",
    n_random = 10
  )
  class(mock_result) <- "cograph_cluster_significance"

  expect_output(print(mock_result), "Cluster Significance Test")
})

test_that("print.cograph_cluster_significance low p-value", {
  # Create mock object with very low p-value
  mock_result <- list(
    observed_modularity = 0.8,
    null_mean = 0.2,
    null_sd = 0.05,
    z_score = 12,
    p_value = 1e-5,
    null_values = rnorm(100, 0.2, 0.05),
    method = "configuration",
    n_random = 100
  )
  class(mock_result) <- "cograph_cluster_significance"

  expect_output(print(mock_result), "Highly significant")
})

test_that("print.cograph_cluster_significance medium p-value", {
  mock_result <- list(
    observed_modularity = 0.5,
    null_mean = 0.3,
    null_sd = 0.05,
    z_score = 2.5,
    p_value = 0.006,
    null_values = rnorm(100, 0.3, 0.05),
    method = "configuration",
    n_random = 100
  )
  class(mock_result) <- "cograph_cluster_significance"

  expect_output(print(mock_result), "Very significant")
})

test_that("print.cograph_cluster_significance borderline p-value", {
  mock_result <- list(
    observed_modularity = 0.35,
    null_mean = 0.3,
    null_sd = 0.03,
    z_score = 1.67,
    p_value = 0.048,
    null_values = rnorm(100, 0.3, 0.03),
    method = "configuration",
    n_random = 100
  )
  class(mock_result) <- "cograph_cluster_significance"

  expect_output(print(mock_result), "Significant community structure")
})

test_that("print.cograph_cluster_significance non-significant", {
  mock_result <- list(
    observed_modularity = 0.31,
    null_mean = 0.3,
    null_sd = 0.05,
    z_score = 0.2,
    p_value = 0.42,
    null_values = rnorm(100, 0.3, 0.05),
    method = "gnm",
    n_random = 100
  )
  class(mock_result) <- "cograph_cluster_significance"

  expect_output(print(mock_result), "No significant")
})

# ==============================================================================
# Tests for plot.cograph_cluster_significance
# ==============================================================================

test_that("plot.cograph_cluster_significance works", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  comm <- igraph::cluster_louvain(g)
  result <- cluster_significance(g, comm, n_random = 10, seed = 42)

  # Capture plot in temp file
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  plot_result <- plot(result)
  grDevices::dev.off()

  expect_true(file.exists(tmp))
  # The plot function returns x invisibly, but we capture it via assignment
  # Just verify it returns the original object

  expect_s3_class(plot_result, "cograph_cluster_significance")
})

# ==============================================================================
# Tests for layer_similarity
# ==============================================================================

test_that("layer_similarity errors on dimension mismatch", {
  A1 <- matrix(1, 4, 4)
  A2 <- matrix(1, 5, 5)

  expect_error(
    layer_similarity(A1, A2, "jaccard"),
    "identical dimensions"
  )
})

test_that("layer_similarity jaccard with no edges", {
  A1 <- matrix(0, 4, 4)
  A2 <- matrix(0, 4, 4)

  result <- layer_similarity(A1, A2, "jaccard")
  expect_true(is.na(result))
})

test_that("layer_similarity overlap with no edges", {
  A1 <- matrix(0, 4, 4)
  A2 <- matrix(0, 4, 4)

  result <- layer_similarity(A1, A2, "overlap")
  expect_true(is.na(result))
})

test_that("layer_similarity cosine with zero norm", {
  A1 <- matrix(0, 4, 4)
  A2 <- matrix(1, 4, 4)

  result <- layer_similarity(A1, A2, "cosine")
  expect_true(is.na(result))
})

test_that("layer_similarity pearson works", {
  A1 <- matrix(runif(16), 4, 4)
  A2 <- matrix(runif(16), 4, 4)

  result <- layer_similarity(A1, A2, "pearson")
  expect_true(result >= -1 && result <= 1)
})

test_that("layer_similarity overlap works", {
  A1 <- matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,1, 0,1,1,0), 4, 4)
  A2 <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)

  result <- layer_similarity(A1, A2, "overlap")
  expect_true(result >= 0 && result <= 1)
})

test_that("lsim is an alias for layer_similarity", {
  A1 <- mat
  A2 <- mat * 0.5

  expect_equal(
    lsim(A1, A2, "jaccard"),
    layer_similarity(A1, A2, "jaccard")
  )
})

# ==============================================================================
# Tests for layer_similarity_matrix
# ==============================================================================

test_that("layer_similarity_matrix errors with < 2 layers", {
  expect_error(
    layer_similarity_matrix(list(mat)),
    "at least 2 layers"
  )
})

test_that("layer_similarity_matrix with unnamed layers", {
  layers <- list(mat, mat * 0.5, mat * 0.25)
  result <- layer_similarity_matrix(layers, method = "jaccard")

  expect_equal(dim(result), c(3, 3))
  expect_equal(rownames(result), c("Layer1", "Layer2", "Layer3"))
})

test_that("layer_similarity_matrix pearson method", {
  layers <- list(L1 = mat, L2 = mat * 2)
  result <- layer_similarity_matrix(layers, method = "pearson")

  expect_equal(dim(result), c(2, 2))
  expect_equal(result[1, 1], 1)
})

test_that("layer_similarity_matrix overlap method", {
  layers <- list(L1 = mat, L2 = mat)
  result <- layer_similarity_matrix(layers, method = "overlap")

  expect_equal(result[1, 2], 1)
})

test_that("lsim_matrix is an alias", {
  layers <- list(L1 = mat, L2 = mat * 0.5)

  expect_equal(
    lsim_matrix(layers, "cosine"),
    layer_similarity_matrix(layers, "cosine")
  )
})

# ==============================================================================
# Tests for layer_degree_correlation
# ==============================================================================

test_that("layer_degree_correlation in mode", {
  layers <- list(L1 = mat, L2 = mat * 2, L3 = mat^2)
  result <- layer_degree_correlation(layers, mode = "in")

  expect_equal(dim(result), c(3, 3))
})

test_that("layer_degree_correlation out mode", {
  layers <- list(L1 = mat, L2 = mat * 2)
  result <- layer_degree_correlation(layers, mode = "out")

  expect_equal(dim(result), c(2, 2))
})

test_that("layer_degree_correlation with unnamed layers", {
  layers <- list(mat, mat * 2)
  result <- layer_degree_correlation(layers, mode = "total")

  expect_equal(colnames(result), c("Layer1", "Layer2"))
})

test_that("ldegcor is an alias", {
  layers <- list(L1 = mat, L2 = mat * 2)

  expect_equal(
    ldegcor(layers, "total"),
    layer_degree_correlation(layers, "total")
  )
})

# ==============================================================================
# Tests for supra_adjacency
# ==============================================================================

test_that("supra_adjacency errors on empty layers", {
  expect_error(
    supra_adjacency(list()),
    "at least 1 layer"
  )
})

test_that("supra_adjacency errors on mismatched dimensions", {
  layers <- list(
    L1 = matrix(1, 3, 3),
    L2 = matrix(1, 4, 4)
  )
  expect_error(
    supra_adjacency(layers),
    "identical dimensions"
  )
})

test_that("supra_adjacency with single layer", {
  layers <- list(L1 = mat)
  result <- supra_adjacency(layers, omega = 1)

  expect_equal(dim(result), c(8, 8))
  expect_equal(attr(result, "n_layers"), 1)
})

test_that("supra_adjacency with omega matrix", {
  layers <- list(L1 = mat, L2 = mat * 2)
  omega_mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  result <- supra_adjacency(layers, omega = omega_mat, coupling = "diagonal")
  expect_s3_class(result, "supra_adjacency")
})

test_that("supra_adjacency errors on wrong omega matrix dimensions", {
  layers <- list(L1 = mat, L2 = mat * 2)
  wrong_omega <- matrix(1, 3, 3)

  expect_error(
    supra_adjacency(layers, omega = wrong_omega),
    "omega matrix must be L x L"
  )
})

test_that("supra_adjacency full coupling", {
  layers <- list(L1 = mat, L2 = mat)
  result <- supra_adjacency(layers, omega = 0.5, coupling = "full")

  # Extract interlayer and check all entries are 0.5
  interlayer <- supra_interlayer(result, 1, 2)
  expect_true(all(interlayer == 0.5))
})

test_that("supra_adjacency custom coupling without matrices", {
  layers <- list(L1 = mat, L2 = mat)

  expect_error(
    supra_adjacency(layers, coupling = "custom"),
    "interlayer_matrices required"
  )
})

test_that("supra_adjacency custom coupling with matrices", {
  layers <- list(L1 = mat, L2 = mat)
  custom_inter <- diag(nrow(mat)) * 0.3

  result <- supra_adjacency(
    layers,
    omega = 1,
    coupling = "custom",
    interlayer_matrices = list(custom_inter)
  )

  expect_s3_class(result, "supra_adjacency")
})

test_that("supra_adjacency with unnamed layers", {
  unnamed_mat <- matrix(runif(16), 4, 4)
  layers <- list(unnamed_mat, unnamed_mat * 2)

  result <- supra_adjacency(layers, omega = 0.5)
  expect_true(grepl("L1", rownames(result)[1]))
})

test_that("supra is an alias", {
  layers <- list(L1 = mat, L2 = mat * 2)

  result1 <- supra_adjacency(layers, omega = 0.5)
  result2 <- supra(layers, omega = 0.5)

  expect_equal(dim(result1), dim(result2))
})

# ==============================================================================
# Tests for supra_layer
# ==============================================================================

test_that("supra_layer errors on invalid layer index", {
  layers <- list(L1 = mat, L2 = mat * 2)
  s <- supra_adjacency(layers, omega = 0.5)

  expect_error(supra_layer(s, 0), "must be between")
  expect_error(supra_layer(s, 3), "must be between")
})

test_that("extract_layer is an alias for supra_layer", {
  layers <- list(L1 = mat, L2 = mat * 2)
  s <- supra_adjacency(layers, omega = 0.5)

  expect_equal(
    extract_layer(s, 1),
    supra_layer(s, 1)
  )
})

# ==============================================================================
# Tests for supra_interlayer
# ==============================================================================

test_that("supra_interlayer errors on invalid layer indices", {
  layers <- list(L1 = mat, L2 = mat * 2)
  s <- supra_adjacency(layers, omega = 0.5)

  expect_error(supra_interlayer(s, 0, 1), "must be between")
  expect_error(supra_interlayer(s, 1, 3), "must be between")
})

test_that("extract_interlayer is an alias", {
  layers <- list(L1 = mat, L2 = mat * 2)
  s <- supra_adjacency(layers, omega = 0.5)

  expect_equal(
    extract_interlayer(s, 1, 2),
    supra_interlayer(s, 1, 2)
  )
})

# ==============================================================================
# Tests for aggregate_layers
# ==============================================================================

test_that("aggregate_layers errors on empty list", {
  expect_error(
    aggregate_layers(list()),
    "at least 1 layer"
  )
})

test_that("aggregate_layers returns single layer unchanged", {
  result <- aggregate_layers(list(mat), method = "sum")
  expect_equal(result, mat)
})

test_that("aggregate_layers weighted sum errors on wrong weights length", {
  layers <- list(L1 = mat, L2 = mat * 2)

  expect_error(
    aggregate_layers(layers, method = "sum", weights = c(1, 2, 3)),
    "weights must have length"
  )
})

test_that("aggregate_layers min method", {
  layers <- list(L1 = mat, L2 = mat * 2, L3 = mat * 3)
  result <- aggregate_layers(layers, method = "min")

  expect_equal(result, mat)
})

test_that("aggregate_layers preserves dimnames", {
  layers <- list(L1 = mat, L2 = mat * 2)
  result <- aggregate_layers(layers, method = "sum")

  expect_equal(rownames(result), rownames(mat))
  expect_equal(colnames(result), colnames(mat))
})

test_that("lagg is an alias", {
  layers <- list(L1 = mat, L2 = mat * 2)

  expect_equal(
    lagg(layers, "mean"),
    aggregate_layers(layers, "mean")
  )
})

# ==============================================================================
# Tests for verify_with_igraph
# ==============================================================================

test_that("verify_with_igraph works with igraph installed", {
  skip_if_not_installed("igraph")

  result <- verify_with_igraph(mat, clusters_list, method = "sum")

  expect_type(result, "list")
  expect_true("matches" %in% names(result))
  expect_true("our_result" %in% names(result))
  expect_true("igraph_result" %in% names(result))
})

test_that("verify_with_igraph with unnamed matrix", {
  skip_if_not_installed("igraph")

  unnamed_mat <- matrix(runif(25), 5, 5)
  diag(unnamed_mat) <- 0
  clusters <- c(1, 1, 2, 2, 3)

  result <- verify_with_igraph(unnamed_mat, clusters, method = "sum")
  expect_type(result, "list")
})

test_that("verify_igraph is an alias", {
  skip_if_not_installed("igraph")

  result1 <- verify_with_igraph(mat, clusters_list, method = "sum")
  result2 <- verify_igraph(mat, clusters_list, method = "sum")

  expect_equal(result1$matches, result2$matches)
})

# ==============================================================================
# Tests for print.cluster_summary
# ==============================================================================

test_that("print.cluster_summary works", {
  result <- cluster_summary(mat, clusters_list)

  expect_output(print(result), "Cluster Summary")
  expect_output(print(result), "Clusters:")
  expect_output(print(result), "Between-cluster matrix")
  expect_output(print(result), "Within-cluster values")
})

# ==============================================================================
# Tests for print.cluster_quality
# ==============================================================================

test_that("print.cluster_quality works", {
  result <- cluster_quality(mat, clusters_list)

  expect_output(print(result), "Cluster Quality Metrics")
  expect_output(print(result), "Global metrics")
  expect_output(print(result), "Modularity")
  expect_output(print(result), "Per-cluster metrics")
})

# ==============================================================================
# Edge Cases and Complex Scenarios
# ==============================================================================

test_that("cluster_summary with all zeros in one cluster pair", {
  # Create matrix where one cluster pair has no edges
  special_mat <- mat
  special_mat[1:2, 6:8] <- 0
  special_mat[6:8, 1:2] <- 0

  result <- cluster_summary(special_mat, clusters_list, method = "sum")
  expect_equal(result$between["Group1", "Group3"], 0)
})

test_that("cluster_quality with single-node cluster", {
  single_clusters <- list(
    A = "N1",
    B = c("N2", "N3", "N4", "N5", "N6", "N7", "N8")
  )

  result <- cluster_quality(mat, single_clusters)
  expect_s3_class(result, "cluster_quality")
  # Single node cluster should have specific behavior for metrics
})

test_that("layer_similarity with identical sparse matrices", {
  result <- layer_similarity(sparse_mat, sparse_mat, "jaccard")
  expect_equal(result, 1)
})

test_that("supra_adjacency with three layers", {
  layers <- list(L1 = mat, L2 = mat * 2, L3 = mat * 0.5)
  result <- supra_adjacency(layers, omega = 0.3)

  expect_equal(dim(result), c(24, 24))
  expect_equal(attr(result, "n_layers"), 3)
})

test_that("aggregate_layers intersection with fully connected", {
  full1 <- matrix(1, 4, 4)
  diag(full1) <- 0
  full2 <- matrix(1, 4, 4)
  diag(full2) <- 0

  result <- aggregate_layers(list(full1, full2), method = "intersection")
  expected <- full1
  expect_equal(result, expected)
})

test_that("aggregate_layers union with sparse matrices", {
  m1 <- matrix(0, 4, 4)
  m1[1, 2] <- 1
  m2 <- matrix(0, 4, 4)
  m2[3, 4] <- 1

  result <- aggregate_layers(list(m1, m2), method = "union")
  expect_equal(sum(result), 2)
})

# ==============================================================================
# Additional Modularity Tests
# ==============================================================================

test_that(".compute_modularity directed graph", {
  membership <- c(1, 1, 2, 2, 2, 3, 3, 3)
  result_directed <- cograph:::.compute_modularity(mat, membership, directed = TRUE)
  result_undirected <- cograph:::.compute_modularity(mat, membership, directed = FALSE)

  # Both should be numeric
  expect_true(is.numeric(result_directed))
  expect_true(is.numeric(result_undirected))
})

# ==============================================================================
# Cluster Significance with Different Inputs
# ==============================================================================

test_that("cluster_significance handles configuration model failures gracefully",
{
  skip_if_not_installed("igraph")

  # Create a simple network
  g <- igraph::make_graph("Zachary")
  comm <- igraph::cluster_louvain(g)

  # This should handle any internal fallbacks
  result <- cluster_significance(g, comm, method = "configuration",
                                 n_random = 5, seed = 42)
  expect_s3_class(result, "cograph_cluster_significance")
})

test_that("cluster_significance computes z-score correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph("Zachary")
  comm <- igraph::cluster_louvain(g)

  result <- cluster_significance(g, comm, n_random = 20, seed = 42)

  # Verify z-score calculation
  expected_z <- (result$observed_modularity - result$null_mean) / result$null_sd
  expect_equal(result$z_score, expected_z, tolerance = 1e-10)
})

# ==============================================================================
# Summary
# ==============================================================================

cat("\n=== Cluster Metrics Coverage Tests Complete ===\n")
