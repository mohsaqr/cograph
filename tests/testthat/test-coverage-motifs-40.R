# Test coverage for R/motifs.R
# This file contains comprehensive tests for motif analysis functions

# =============================================================================
# TEST SETUP AND HELPER FUNCTIONS
# =============================================================================

# Create test matrices for motif analysis
create_directed_matrix <- function(n = 5, seed = 42) {
  set.seed(seed)
  mat <- matrix(sample(0:1, n * n, replace = TRUE, prob = c(0.6, 0.4)), n, n)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:n]
  mat
}

create_undirected_matrix <- function(n = 5, seed = 42) {

  set.seed(seed)
  mat <- matrix(0, n, n)
  upper_idx <- which(upper.tri(mat))
  selected <- sample(upper_idx, length(upper_idx) %/% 2)
  mat[selected] <- 1
  mat <- mat + t(mat)
  rownames(mat) <- colnames(mat) <- LETTERS[1:n]
  mat
}

create_weighted_matrix <- function(n = 5, seed = 42) {
  set.seed(seed)
  mat <- matrix(sample(0:10, n * n, replace = TRUE), n, n)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:n]
  mat
}

# =============================================================================
# MOTIF_CENSUS FUNCTION TESTS
# =============================================================================

test_that("motif_census works with directed matrix input", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)
  result <- motif_census(mat, n_random = 10, seed = 42)

  expect_s3_class(result, "cograph_motifs")
  expect_true("counts" %in% names(result))
  expect_true("z_scores" %in% names(result))
  expect_true("p_values" %in% names(result))
  expect_equal(result$size, 3)
  expect_true(result$directed)
})

test_that("motif_census works with undirected matrix", {
  skip_if_not_installed("igraph")

  mat <- create_undirected_matrix(6, seed = 123)
  result <- motif_census(mat, n_random = 10, seed = 42)

  expect_s3_class(result, "cograph_motifs")
  expect_false(result$directed)
  expect_true(all(c("empty", "wedge", "triangle") %in% names(result$counts)))
})

test_that("motif_census works with igraph input", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")

  result <- motif_census(g, n_random = 10, seed = 42)
  expect_s3_class(result, "cograph_motifs")
  expect_true(result$directed)
})

test_that("motif_census works with cograph_network input", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)
  net <- as_cograph(mat)

  result <- motif_census(net, n_random = 10, seed = 42)
  expect_s3_class(result, "cograph_motifs")
})

test_that("motif_census handles size parameter", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)

  result3 <- motif_census(mat, size = 3, n_random = 10, seed = 42)
  expect_equal(result3$size, 3)

  result4 <- motif_census(mat, size = 4, n_random = 10, seed = 42)
  expect_equal(result4$size, 4)
})

test_that("motif_census errors on invalid size", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)

  expect_error(motif_census(mat, size = 5, n_random = 10))
})

test_that("motif_census handles configuration method", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)

  result <- motif_census(mat, method = "configuration", n_random = 10, seed = 42)
  expect_equal(result$method, "configuration")
})

test_that("motif_census handles gnm method", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)

  result <- motif_census(mat, method = "gnm", n_random = 10, seed = 42)
  expect_equal(result$method, "gnm")
})

test_that("motif_census respects seed parameter", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)

  result1 <- motif_census(mat, n_random = 10, seed = 42)
  result2 <- motif_census(mat, n_random = 10, seed = 42)

  expect_equal(result1$z_scores, result2$z_scores)
})

test_that("motif_census errors on invalid input", {
  skip_if_not_installed("igraph")

  expect_error(motif_census("not a matrix"))
  expect_error(motif_census(list(a = 1, b = 2)))
})

# =============================================================================
# PRINT.COGRAPH_MOTIFS TESTS
# =============================================================================

test_that("print.cograph_motifs works", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)
  result <- motif_census(mat, n_random = 10, seed = 42)

  output <- capture.output(print(result))
  expect_true(any(grepl("Network Motif Analysis", output)))
  expect_true(any(grepl("Null model", output)))
})

test_that("print.cograph_motifs handles no significant motifs", {
  skip_if_not_installed("igraph")

  # Create a simple matrix likely to have no significant motifs
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- 1
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]

  result <- motif_census(mat, n_random = 10, seed = 42)
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# =============================================================================
# PLOT.COGRAPH_MOTIFS TESTS
# =============================================================================

test_that("plot.cograph_motifs bar type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_directed_matrix(6, seed = 123)
  result <- motif_census(mat, n_random = 10, seed = 42)

  # Capture output to avoid plot display
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  p <- plot(result, type = "bar", show_nonsig = TRUE)
  grDevices::dev.off()

  expect_true(inherits(p, "gg") || is.null(p))
})

test_that("plot.cograph_motifs heatmap type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_directed_matrix(6, seed = 123)
  result <- motif_census(mat, n_random = 10, seed = 42)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  p <- plot(result, type = "heatmap", show_nonsig = TRUE)
  grDevices::dev.off()

  expect_true(inherits(p, "gg") || is.null(p))
})

test_that("plot.cograph_motifs network type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_directed_matrix(6, seed = 123)
  result <- motif_census(mat, n_random = 10, seed = 42)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  p <- plot(result, type = "network", show_nonsig = TRUE)
  grDevices::dev.off()

  # network type returns NULL invisibly for directed triads
  expect_true(is.null(p) || inherits(p, "gg"))
})

test_that("plot.cograph_motifs handles top_n parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_directed_matrix(8, seed = 123)
  result <- motif_census(mat, n_random = 10, seed = 42)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  p <- plot(result, type = "bar", show_nonsig = TRUE, top_n = 5)
  grDevices::dev.off()

  expect_true(inherits(p, "gg") || is.null(p))
})

test_that("plot.cograph_motifs custom colors work", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_directed_matrix(6, seed = 123)
  result <- motif_census(mat, n_random = 10, seed = 42)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  p <- plot(result, type = "bar", show_nonsig = TRUE,
            colors = c("#FF0000", "#FFFFFF", "#0000FF"))
  grDevices::dev.off()

  expect_true(inherits(p, "gg") || is.null(p))
})

test_that("plot.cograph_motifs handles empty data gracefully", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  # Use a directed sparse matrix to avoid degree sequence issues
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- 1  # Single edge
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]

  result <- motif_census(mat, n_random = 10, seed = 42, directed = TRUE)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  # Should handle gracefully (may return NULL or message)
  p <- plot(result, type = "bar", show_nonsig = FALSE)
  grDevices::dev.off()

  expect_true(is.null(p) || inherits(p, "gg"))
})

# =============================================================================
# TRIAD_CENSUS FUNCTION TESTS
# =============================================================================

test_that("triad_census works with matrix input", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)
  result <- triad_census(mat)

  expect_true(is.numeric(result))
  expect_equal(length(result), 16)  # 16 triad types
  expect_true(all(c("003", "012", "300") %in% names(result)))
})

test_that("triad_census works with igraph input", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")

  result <- triad_census(g)
  expect_equal(length(result), 16)
})

test_that("triad_census works with cograph_network input", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 123)
  net <- as_cograph(mat)

  result <- triad_census(net)
  expect_equal(length(result), 16)
})

test_that("triad_census errors on undirected network", {
  skip_if_not_installed("igraph")

  mat <- create_undirected_matrix(6, seed = 123)
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected")

  expect_error(triad_census(g), "directed")
})

test_that("triad_census errors on invalid input", {
  skip_if_not_installed("igraph")

  expect_error(triad_census("invalid"))
})

test_that("triad_census returns all 16 MAN types", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(8, seed = 456)
  result <- triad_census(mat)

  expected_names <- c("003", "012", "102", "021D", "021U", "021C",
                      "111D", "111U", "030T", "030C", "201",
                      "120D", "120U", "120C", "210", "300")
  expect_equal(names(result), expected_names)
})

# =============================================================================
# EXTRACT_TRIADS FUNCTION TESTS
# =============================================================================

test_that("extract_triads works with basic matrix", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_triads(mat, min_total = 0)

  expect_true(is.data.frame(result))
  expect_true(all(c("A", "B", "C", "type") %in% names(result)))
})

test_that("extract_triads handles type filter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(6, seed = 123)
  result <- extract_triads(mat, type = c("030T", "030C"), min_total = 0)

  if (nrow(result) > 0) {
    expect_true(all(result$type %in% c("030T", "030C")))
  }
})

test_that("extract_triads handles involving filter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_triads(mat, involving = "A", min_total = 0)

  if (nrow(result) > 0) {
    has_A <- apply(result[, c("A", "B", "C")], 1, function(x) "A" %in% x)
    expect_true(all(has_A))
  }
})

test_that("extract_triads handles threshold parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)

  result_low <- extract_triads(mat, threshold = 0, min_total = 0)
  result_high <- extract_triads(mat, threshold = 5, min_total = 0)

  # Higher threshold should give same or fewer triads
  expect_true(nrow(result_high) <= nrow(result_low))
})

test_that("extract_triads handles min_total parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)

  result_low <- extract_triads(mat, min_total = 0)
  result_high <- extract_triads(mat, min_total = 20)

  expect_true(nrow(result_high) <= nrow(result_low))
})

test_that("extract_triads returns empty data frame for small networks", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")

  result <- extract_triads(mat)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("extract_triads includes weight columns", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_triads(mat, min_total = 0)

  weight_cols <- c("weight_AB", "weight_BA", "weight_AC",
                   "weight_CA", "weight_BC", "weight_CB", "total_weight")
  expect_true(all(weight_cols %in% names(result)))
})

# =============================================================================
# HELPER FUNCTION TESTS
# =============================================================================

test_that(".get_motif_names returns correct names", {
  # Access internal function
  get_motif_names <- cograph:::.get_motif_names

  # Directed size 3
  names_3_dir <- get_motif_names(3, TRUE)
  expect_equal(length(names_3_dir), 16)
  expect_true("003" %in% names_3_dir)
  expect_true("300" %in% names_3_dir)

  # Undirected size 3
  names_3_undir <- get_motif_names(3, FALSE)
  expect_true("empty" %in% names_3_undir)

  # Size 4
  names_4 <- get_motif_names(4, TRUE)
  expect_true(all(grepl("^M", names_4)))
})

test_that(".classify_triads_vectorized works", {
  # Access internal function
  classify_triads <- cograph:::.classify_triads_vectorized

  # Test with single triad (empty)
  result <- classify_triads(0L, 0L, 0L, 0L, 0L, 0L)
  expect_equal(result, "003")

  # Test with mutual edge
  result <- classify_triads(1L, 1L, 0L, 0L, 0L, 0L)
  expect_equal(result, "102")
})

test_that(".get_triad_lookup returns 64-element lookup", {
  # Access internal function
  get_lookup <- cograph:::.get_triad_lookup

  lookup <- get_lookup()
  expect_equal(length(lookup), 64)
  expect_true(all(lookup %in% c("003", "012", "102", "021D", "021U", "021C",
                                "111D", "111U", "030T", "030C", "201",
                                "120D", "120U", "120C", "210", "300")))
})

test_that(".build_triad_lookup works correctly", {
  # Access internal function
  build_lookup <- cograph:::.build_triad_lookup

  lookup <- build_lookup()
  expect_equal(length(lookup), 64)

  # Code 0 should be empty triad (003)
  expect_equal(lookup[1], "003")  # R is 1-indexed
})

test_that(".generate_random_graph works", {
  skip_if_not_installed("igraph")

  generate_random <- cograph:::.generate_random_graph

  mat <- create_directed_matrix(6, seed = 123)
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")

  g_rand_config <- generate_random(g, "configuration")
  expect_true(igraph::is_igraph(g_rand_config))

  g_rand_gnm <- generate_random(g, "gnm")
  expect_true(igraph::is_igraph(g_rand_gnm))
})

# =============================================================================
# EXTRACT_MOTIFS FUNCTION TESTS
# =============================================================================

test_that("extract_motifs works with matrix input", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs handles pattern parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(6, seed = 123)

  result_triangle <- extract_motifs(mat, pattern = "triangle", min_transitions = 0)
  result_network <- extract_motifs(mat, pattern = "network", min_transitions = 0)
  result_all <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  # All should return valid objects or NULL
  expect_true(is.null(result_triangle) || inherits(result_triangle, "cograph_motif_analysis"))
  expect_true(is.null(result_network) || inherits(result_network, "cograph_motif_analysis"))
  expect_true(is.null(result_all) || inherits(result_all, "cograph_motif_analysis"))
})

test_that("extract_motifs handles edge_method parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)

  result_any <- extract_motifs(mat, edge_method = "any", pattern = "all", min_transitions = 0)
  result_percent <- extract_motifs(mat, edge_method = "percent", edge_threshold = 0.1,
                                    pattern = "all", min_transitions = 0)

  expect_true(is.null(result_any) || inherits(result_any, "cograph_motif_analysis"))
  expect_true(is.null(result_percent) || inherits(result_percent, "cograph_motif_analysis"))
})

test_that("extract_motifs handles include_types parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(6, seed = 123)
  result <- extract_motifs(mat, include_types = c("030T"), min_transitions = 0)

  if (!is.null(result) && nrow(result$results) > 0) {
    expect_true(all(result$results$type == "030T"))
  }
})

test_that("extract_motifs handles exclude_types parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(6, seed = 123)
  result <- extract_motifs(mat, pattern = "all", exclude_types = c("003"),
                           min_transitions = 0)

  if (!is.null(result) && nrow(result$results) > 0) {
    expect_false("003" %in% result$results$type)
  }
})

test_that("extract_motifs handles top parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(6, seed = 123)
  result <- extract_motifs(mat, pattern = "all", top = 5, min_transitions = 0)

  if (!is.null(result)) {
    expect_true(nrow(result$results) <= 5)
  }
})

test_that("extract_motifs handles by_type parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(6, seed = 123)
  result <- extract_motifs(mat, pattern = "all", by_type = TRUE, min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs handles significance testing", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", significance = TRUE,
                           n_perm = 5, min_transitions = 0, seed = 42)

  if (!is.null(result)) {
    expect_true("z" %in% names(result$results) || nrow(result$results) == 0)
  }
})

test_that("extract_motifs works with cograph_network", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)
  net <- as_cograph(mat)
  result <- extract_motifs(net, pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs works with igraph", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE)
  result <- extract_motifs(g, pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs works with data frame and id", {
  skip_if_not_installed("igraph")

  # Create a simple edge data frame
  df <- data.frame(
    id = rep(1:3, each = 5),
    from = c("A", "B", "C", "A", "B", "A", "C", "B", "C", "A",
             "B", "C", "A", "B", "C"),
    to = c("B", "C", "A", "C", "A", "B", "A", "C", "B", "C",
           "A", "B", "C", "A", "C")
  )

  result <- extract_motifs(data = df, id = "id", pattern = "all", min_transitions = 0)
  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs errors on invalid input", {
  skip_if_not_installed("igraph")

  expect_error(extract_motifs("invalid"))
  expect_error(extract_motifs(list(a = 1)))
})

test_that("extract_motifs handles seed parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)

  result1 <- extract_motifs(mat, pattern = "all", min_transitions = 0,
                            significance = TRUE, n_perm = 5, seed = 42)
  result2 <- extract_motifs(mat, pattern = "all", min_transitions = 0,
                            significance = TRUE, n_perm = 5, seed = 42)

  # Results should be identical with same seed
  if (!is.null(result1) && !is.null(result2)) {
    expect_equal(result1$results$triad, result2$results$triad)
  }
})

# =============================================================================
# PRINT.COGRAPH_MOTIF_ANALYSIS TESTS
# =============================================================================

test_that("print.cograph_motif_analysis works", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result)) {
    output <- capture.output(print(result))
    expect_true(any(grepl("Motif Analysis", output)))
  }
})

test_that("print.cograph_motif_analysis handles n parameter", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(6, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result)) {
    output <- capture.output(print(result, n = 5))
    expect_true(length(output) > 0)
  }
})

# =============================================================================
# PLOT.COGRAPH_MOTIF_ANALYSIS TESTS
# =============================================================================

test_that("plot.cograph_motif_analysis triads type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result) && nrow(result$results) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 400, height = 400)
    plot(result, type = "triads", n = 4)
    grDevices::dev.off()

    expect_true(file.exists(tmp))
  }
})

test_that("plot.cograph_motif_analysis types type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_matrix(6, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result)) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 400, height = 400)
    p <- plot(result, type = "types")
    grDevices::dev.off()

    expect_true(inherits(p, "gg") || is.null(p))
  }
})

test_that("plot.cograph_motif_analysis significance type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", significance = TRUE,
                           n_perm = 5, min_transitions = 0, seed = 42)

  if (!is.null(result) && nrow(result$results) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 400, height = 400)
    p <- plot(result, type = "significance")
    grDevices::dev.off()

    expect_true(inherits(p, "gg") || is.null(p))
  }
})

test_that("plot.cograph_motif_analysis patterns type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_matrix(6, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result)) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 400, height = 400)
    plot(result, type = "patterns")
    grDevices::dev.off()

    expect_true(file.exists(tmp))
  }
})

test_that("plot.cograph_motif_analysis custom parameters work", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result) && nrow(result$results) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 400, height = 400)
    plot(result, type = "triads", n = 4, node_size = 3, label_size = 6,
         title_size = 8, stats_size = 4, ncol = 2, legend = FALSE,
         color = "#000080", spacing = 1.2)
    grDevices::dev.off()

    expect_true(file.exists(tmp))
  }
})

# =============================================================================
# EXTRACT_MOTIFS_TEMPORAL FUNCTION TESTS
# =============================================================================

test_that("extract_motifs_temporal works with wide format data", {
  skip_if_not_installed("igraph")

  # Create wide format data
  set.seed(123)
  wide_data <- data.frame(
    T1 = sample(c("A", "B", "C"), 20, replace = TRUE),
    T2 = sample(c("A", "B", "C"), 20, replace = TRUE),
    T3 = sample(c("A", "B", "C"), 20, replace = TRUE),
    T4 = sample(c("A", "B", "C"), 20, replace = TRUE),
    T5 = sample(c("A", "B", "C"), 20, replace = TRUE)
  )

  result <- extract_motifs_temporal(wide_data, window_size = 3, step = 1,
                                     pattern = "all", min_transitions = 0)

  expect_true(inherits(result, "cograph_temporal_motifs") || is.null(result))
})

test_that("extract_motifs_temporal works with edge list format", {
  skip_if_not_installed("igraph")

  edge_df <- data.frame(
    from = c("A", "B", "C", "A", "B", "C"),
    to = c("B", "C", "A", "C", "A", "B"),
    time = c(1, 1, 2, 2, 3, 3)
  )

  result <- extract_motifs_temporal(edge_df, from = "from", to = "to",
                                     time = "time", window_size = 2,
                                     pattern = "all", min_transitions = 0)

  expect_true(inherits(result, "cograph_temporal_motifs") || is.null(result))
})

test_that("extract_motifs_temporal works with long format", {
  skip_if_not_installed("igraph")

  long_df <- data.frame(
    person = rep(1:5, each = 4),
    time = rep(1:4, 5),
    state = sample(c("A", "B", "C"), 20, replace = TRUE)
  )

  result <- extract_motifs_temporal(long_df, id = "person", time = "time",
                                     state = "state", format = "long",
                                     window_size = 2, pattern = "all",
                                     min_transitions = 0)

  expect_true(inherits(result, "cograph_temporal_motifs") || is.null(result))
})

test_that("extract_motifs_temporal handles window_size and step", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 100, replace = TRUE),
    nrow = 20, ncol = 5
  ))
  names(wide_data) <- paste0("T", 1:5)

  result_slide <- extract_motifs_temporal(wide_data, window_size = 2, step = 1,
                                           pattern = "all", min_transitions = 0)
  result_tumble <- extract_motifs_temporal(wide_data, window_size = 2, step = 2,
                                            pattern = "all", min_transitions = 0)

  # Both should work
  expect_true(inherits(result_slide, "cograph_temporal_motifs") || is.null(result_slide))
  expect_true(inherits(result_tumble, "cograph_temporal_motifs") || is.null(result_tumble))
})

test_that("extract_motifs_temporal handles id columns", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(
    person = 1:20,
    group = rep(c("X", "Y"), 10),
    T1 = sample(c("A", "B", "C"), 20, replace = TRUE),
    T2 = sample(c("A", "B", "C"), 20, replace = TRUE),
    T3 = sample(c("A", "B", "C"), 20, replace = TRUE)
  )

  result <- extract_motifs_temporal(wide_data, id = c("person", "group"),
                                     window_size = 2, pattern = "all",
                                     min_transitions = 0)

  expect_true(inherits(result, "cograph_temporal_motifs") || is.null(result))
})

test_that("extract_motifs_temporal handles seed parameter", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 50, replace = TRUE),
    nrow = 10, ncol = 5
  ))
  names(wide_data) <- paste0("T", 1:5)

  result1 <- extract_motifs_temporal(wide_data, window_size = 3, seed = 42,
                                      pattern = "all", min_transitions = 0)
  result2 <- extract_motifs_temporal(wide_data, window_size = 3, seed = 42,
                                      pattern = "all", min_transitions = 0)

  # Should be reproducible
  expect_true(is.null(result1) || is.null(result2) ||
              identical(result1$summary, result2$summary))
})

# =============================================================================
# PRINT.COGRAPH_TEMPORAL_MOTIFS TESTS
# =============================================================================

test_that("print.cograph_temporal_motifs works", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 50, replace = TRUE),
    nrow = 10, ncol = 5
  ))
  names(wide_data) <- paste0("T", 1:5)

  result <- extract_motifs_temporal(wide_data, window_size = 2,
                                     pattern = "all", min_transitions = 0)

  if (!is.null(result)) {
    output <- capture.output(print(result))
    expect_true(any(grepl("Temporal Motif Analysis", output)))
  }
})

# =============================================================================
# PLOT.COGRAPH_TEMPORAL_MOTIFS TESTS
# =============================================================================

test_that("plot.cograph_temporal_motifs trends type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 50, replace = TRUE),
    nrow = 10, ncol = 5
  ))
  names(wide_data) <- paste0("T", 1:5)

  result <- extract_motifs_temporal(wide_data, window_size = 2,
                                     pattern = "all", min_transitions = 0)

  if (!is.null(result) && nrow(result$summary) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp)
    p <- plot(result, type = "trends")
    grDevices::dev.off()

    expect_true(inherits(p, "gg") || is.null(p))
  }
})

test_that("plot.cograph_temporal_motifs heatmap type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 50, replace = TRUE),
    nrow = 10, ncol = 5
  ))
  names(wide_data) <- paste0("T", 1:5)

  result <- extract_motifs_temporal(wide_data, window_size = 2,
                                     pattern = "all", min_transitions = 0)

  if (!is.null(result) && nrow(result$summary) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp)
    p <- plot(result, type = "heatmap")
    grDevices::dev.off()

    expect_true(inherits(p, "gg") || is.null(p))
  }
})

test_that("plot.cograph_temporal_motifs handles top_n parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 50, replace = TRUE),
    nrow = 10, ncol = 5
  ))
  names(wide_data) <- paste0("T", 1:5)

  result <- extract_motifs_temporal(wide_data, window_size = 2,
                                     pattern = "all", min_transitions = 0)

  if (!is.null(result) && nrow(result$summary) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp)
    p <- plot(result, type = "trends", top_n = 3)
    grDevices::dev.off()

    expect_true(inherits(p, "gg") || is.null(p))
  }
})

# =============================================================================
# TRIAD_PERSISTENCE FUNCTION TESTS
# =============================================================================

test_that("triad_persistence works with temporal motifs", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    expect_true(inherits(result, "cograph_triad_persistence") || is.null(result))
  }
})

test_that("triad_persistence handles by parameter", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result_triad <- triad_persistence(temporal, by = "triad")
    result_type <- triad_persistence(temporal, by = "type")

    expect_true(is.null(result_triad) || result_triad$params$by == "triad")
    expect_true(is.null(result_type) || result_type$params$by == "type")
  }
})

test_that("triad_persistence handles edge_weight parameter", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal, edge_weight = TRUE)
    expect_true(is.null(result) || result$params$edge_weight == TRUE)
  }
})

test_that("triad_persistence handles min_windows parameter", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 80, replace = TRUE),
    nrow = 20, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result_low <- triad_persistence(temporal, min_windows = 1)
    result_high <- triad_persistence(temporal, min_windows = 2)

    if (!is.null(result_low) && !is.null(result_high)) {
      expect_true(nrow(result_high$triads) <= nrow(result_low$triads))
    }
  }
})

test_that("triad_persistence handles min_persistence parameter", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 80, replace = TRUE),
    nrow = 20, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal, min_persistence = 0.6)
    expect_true(is.null(result) || result$params$min_persistence == 0.6)
  }
})

test_that("triad_persistence returns correct structure", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    if (!is.null(result)) {
      expect_true("triads" %in% names(result))
      expect_true("counts_matrix" %in% names(result))
      expect_true("presence_matrix" %in% names(result))
      expect_true("summary" %in% names(result))
      expect_true("params" %in% names(result))
    }
  }
})

test_that("triad_persistence errors on invalid input", {
  skip_if_not_installed("igraph")

  expect_error(triad_persistence("invalid"))
  expect_error(triad_persistence(list(a = 1)))
})

# =============================================================================
# PRINT.COGRAPH_TRIAD_PERSISTENCE TESTS
# =============================================================================

test_that("print.cograph_triad_persistence works", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    if (!is.null(result)) {
      output <- capture.output(print(result))
      expect_true(any(grepl("Triad Persistence Analysis", output)))
    }
  }
})

test_that("print.cograph_triad_persistence handles n parameter", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    if (!is.null(result)) {
      output <- capture.output(print(result, n = 5))
      expect_true(length(output) > 0)
    }
  }
})

# =============================================================================
# PLOT.COGRAPH_TRIAD_PERSISTENCE TESTS
# =============================================================================

test_that("plot.cograph_triad_persistence heatmap type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    if (!is.null(result) && nrow(result$triads) > 0) {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)

      grDevices::png(tmp)
      p <- plot(result, type = "heatmap", top_n = 5)
      grDevices::dev.off()

      expect_true(inherits(p, "gg") || is.null(p))
    }
  }
})

test_that("plot.cograph_triad_persistence timeline type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    if (!is.null(result) && nrow(result$triads) > 0) {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)

      grDevices::png(tmp)
      p <- plot(result, type = "timeline", top_n = 5)
      grDevices::dev.off()

      expect_true(inherits(p, "gg") || is.null(p))
    }
  }
})

test_that("plot.cograph_triad_persistence status type works", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    if (!is.null(result) && nrow(result$triads) > 0) {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)

      grDevices::png(tmp)
      p <- plot(result, type = "status")
      grDevices::dev.off()

      expect_true(inherits(p, "gg") || is.null(p))
    }
  }
})

test_that("plot.cograph_triad_persistence handles fill parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    if (!is.null(result) && nrow(result$triads) > 0) {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)

      grDevices::png(tmp)
      p <- plot(result, type = "heatmap", fill = "binary", top_n = 5)
      grDevices::dev.off()

      expect_true(inherits(p, "gg") || is.null(p))
    }
  }
})

test_that("plot.cograph_triad_persistence handles normalize parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal, by = "type")

    if (!is.null(result) && nrow(result$triads) > 0) {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)

      grDevices::png(tmp)
      p <- plot(result, type = "heatmap", normalize = TRUE)
      grDevices::dev.off()

      expect_true(inherits(p, "gg") || is.null(p))
    }
  }
})

test_that("plot.cograph_triad_persistence handles show_counts parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    if (!is.null(result) && nrow(result$triads) > 0) {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)

      grDevices::png(tmp)
      p <- plot(result, type = "heatmap", show_counts = FALSE, top_n = 5)
      grDevices::dev.off()

      expect_true(inherits(p, "gg") || is.null(p))
    }
  }
})

# =============================================================================
# INTERNAL HELPER TESTS
# =============================================================================

test_that(".draw_closed_arrow works", {
  draw_arrow <- cograph:::.draw_closed_arrow

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  plot(0:1, 0:1, type = "n")
  draw_arrow(0, 0, 1, 1, col = "#800020", lwd = 2)
  draw_arrow(0, 1, 1, 0, col = "#800020", lwd = 2, both = TRUE)
  grDevices::dev.off()

  expect_true(file.exists(tmp))
})

test_that(".grid_arrow works", {
  skip_if_not_installed("grid")

  grid_arrow <- cograph:::.grid_arrow

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  grid::grid.newpage()
  grid_arrow(0.8, 0.8, 0.2, 0.2, col = "#800020")
  grDevices::dev.off()

  expect_true(file.exists(tmp))
})

test_that(".edge_list_to_windows works", {
  edge_to_win <- cograph:::.edge_list_to_windows

  edges <- data.frame(
    from = c("A", "B", "C", "A"),
    to = c("B", "C", "A", "C"),
    time = c(1, 1, 2, 2)
  )

  result <- edge_to_win(edges, from = "from", to = "to", time = "time",
                        window_size = 1, step = 1)

  expect_true(is.list(result))
  expect_true("windows" %in% names(result))
  expect_true("start_times" %in% names(result))
  expect_true("end_times" %in% names(result))
})

test_that(".long_to_wide works", {
  long_to_wide <- cograph:::.long_to_wide

  long_df <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = c(1, 2, 3, 1, 2, 3),
    state = c("A", "B", "C", "B", "C", "A")
  )

  result <- long_to_wide(long_df, id = "id", time = "time", state = "state")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)  # 2 unique IDs
})

test_that(".count_triads_matrix_vectorized works", {
  count_triads <- cograph:::.count_triads_matrix_vectorized

  mat <- create_weighted_matrix(5, seed = 123)

  result <- count_triads(mat, edge_method = "any", edge_threshold = 0)

  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result)) {
    expect_true(all(c("i", "j", "k", "type") %in% names(result)))
  }
})

test_that(".count_triads_matrix_vectorized handles percent method", {
  count_triads <- cograph:::.count_triads_matrix_vectorized

  mat <- create_weighted_matrix(5, seed = 123)

  result <- count_triads(mat, edge_method = "percent", edge_threshold = 0.1)

  expect_true(is.null(result) || is.data.frame(result))
})

test_that(".count_triads_matrix_vectorized handles expected method", {
  count_triads <- cograph:::.count_triads_matrix_vectorized

  mat <- create_weighted_matrix(5, seed = 123)

  # Create expected matrix
  total <- sum(mat)
  row_sums <- rowSums(mat)
  col_sums <- colSums(mat)
  expected <- outer(row_sums, col_sums) / total
  expected[expected == 0] <- 0.001

  result <- count_triads(mat, edge_method = "expected", edge_threshold = 1.0,
                         expected_mat = expected)

  expect_true(is.null(result) || is.data.frame(result))
})

test_that(".count_triads_matrix_vectorized handles include filter", {
  count_triads <- cograph:::.count_triads_matrix_vectorized

  mat <- create_weighted_matrix(5, seed = 123)

  result <- count_triads(mat, edge_method = "any", edge_threshold = 0,
                         include = c("030T", "030C"))

  if (!is.null(result)) {
    expect_true(all(result$type %in% c("030T", "030C")))
  }
})

test_that(".count_triads_matrix_vectorized handles exclude filter", {
  count_triads <- cograph:::.count_triads_matrix_vectorized

  mat <- create_weighted_matrix(5, seed = 123)

  result <- count_triads(mat, edge_method = "any", edge_threshold = 0,
                         exclude = c("003"))

  if (!is.null(result)) {
    expect_false("003" %in% result$type)
  }
})

# =============================================================================
# EDGE CASES AND BOUNDARY TESTS
# =============================================================================

test_that("motif_census handles minimum size network", {
  skip_if_not_installed("igraph")

  # 3 nodes minimum for triads
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- motif_census(mat, n_random = 5, seed = 42)
  expect_s3_class(result, "cograph_motifs")
})

test_that("extract_triads handles network with no triads", {
  skip_if_not_installed("igraph")

  # Linear network - no triangles
  mat <- matrix(c(0, 1, 0, 0,
                   0, 0, 1, 0,
                   0, 0, 0, 1,
                   0, 0, 0, 0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- extract_triads(mat, min_total = 0)
  expect_true(is.data.frame(result))
})

test_that("extract_motifs handles sparse network", {
  skip_if_not_installed("igraph")

  mat <- matrix(0, 10, 10)
  mat[1, 2] <- 1
  mat[2, 3] <- 1
  rownames(mat) <- colnames(mat) <- LETTERS[1:10]

  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)
  # Should return NULL or valid object with few/no results
  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs handles dense network", {
  skip_if_not_installed("igraph")

  mat <- matrix(sample(5:15, 25, replace = TRUE), 5, 5)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)
  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("motif functions handle network with self-loops removed", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(5, seed = 123)
  diag(mat) <- 0  # Ensure no self-loops

  result <- motif_census(mat, n_random = 5, seed = 42)
  expect_s3_class(result, "cograph_motifs")
})

test_that("extract_motifs_temporal handles single window", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(
    T1 = sample(c("A", "B", "C"), 10, replace = TRUE),
    T2 = sample(c("A", "B", "C"), 10, replace = TRUE)
  )

  result <- extract_motifs_temporal(wide_data, window_size = 2, step = 1,
                                     pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_temporal_motifs"))
})

# =============================================================================
# REGRESSION TESTS
# =============================================================================

test_that("triad_census maintains consistent naming", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(6, seed = 42)
  result <- triad_census(mat)

  # MAN notation should be consistent
  expect_true(all(names(result) %in% c("003", "012", "102", "021D", "021U", "021C",
                                       "111D", "111U", "030T", "030C", "201",
                                       "120D", "120U", "120C", "210", "300")))
})

test_that("motif_census z-scores are computed correctly", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(8, seed = 123)
  result <- motif_census(mat, n_random = 20, seed = 42)

  # Z-scores should be finite for non-zero SD
  non_zero_sd <- result$null_sd > 0
  expect_true(all(is.finite(result$z_scores[non_zero_sd])))
})

test_that("motif_census p-values are in valid range", {
  skip_if_not_installed("igraph")

  mat <- create_directed_matrix(8, seed = 123)
  result <- motif_census(mat, n_random = 20, seed = 42)

  # P-values should be between 0 and 1
  expect_true(all(result$p_values >= 0 & result$p_values <= 1))
})

test_that("extract_triads weight columns sum correctly", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_matrix(5, seed = 123)
  result <- extract_triads(mat, min_total = 0)

  if (nrow(result) > 0) {
    # Total weight should equal sum of individual weights
    computed_total <- result$weight_AB + result$weight_BA +
                      result$weight_AC + result$weight_CA +
                      result$weight_BC + result$weight_CB
    expect_equal(result$total_weight, computed_total)
  }
})
