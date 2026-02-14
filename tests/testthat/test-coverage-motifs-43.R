# Test coverage for R/motifs.R - Additional coverage tests (43)
# Targets uncovered code paths and edge cases

# =============================================================================
# TEST SETUP AND HELPER FUNCTIONS
# =============================================================================

# Helper: Create test matrices
create_test_matrix <- function(n = 5, seed = 123, density = 0.4) {
  set.seed(seed)
  mat <- matrix(sample(0:1, n * n, replace = TRUE, prob = c(1 - density, density)), n, n)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:n]
  mat
}

create_weighted_test_matrix <- function(n = 5, seed = 123, max_weight = 10) {
  set.seed(seed)
  mat <- matrix(sample(0:max_weight, n * n, replace = TRUE), n, n)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:n]
  mat
}

# =============================================================================
# .draw_closed_arrow EDGE CASE TESTS
# =============================================================================

test_that(".draw_closed_arrow handles zero-length vector", {
  draw_arrow <- cograph:::.draw_closed_arrow

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  plot(0:1, 0:1, type = "n")
  # Zero length should return early without error
  draw_arrow(0.5, 0.5, 0.5, 0.5, col = "#800020", lwd = 2)
  grDevices::dev.off()

  expect_true(file.exists(tmp))
})

test_that(".draw_closed_arrow handles both parameter TRUE", {
  draw_arrow <- cograph:::.draw_closed_arrow

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  plot(0:1, 0:1, type = "n")
  draw_arrow(0.2, 0.2, 0.8, 0.8, col = "#000080", lwd = 3, both = TRUE,
             head_length = 0.15, head_width = 0.1)
  grDevices::dev.off()

  expect_true(file.exists(tmp))
})

test_that(".draw_closed_arrow handles custom head dimensions", {
  draw_arrow <- cograph:::.draw_closed_arrow

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  plot(0:1, 0:1, type = "n")
  draw_arrow(0.1, 0.9, 0.9, 0.1, col = "#FF0000", lwd = 1,
             head_length = 0.2, head_width = 0.15)
  grDevices::dev.off()

  expect_true(file.exists(tmp))
})

# =============================================================================
# .grid_arrow EDGE CASE TESTS
# =============================================================================

test_that(".grid_arrow handles zero-length path", {
  skip_if_not_installed("grid")

  grid_arrow <- cograph:::.grid_arrow

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  grid::grid.newpage()
  # Zero length should return early
  grid_arrow(0.5, 0.5, 0.5, 0.5, col = "#800020")
  grDevices::dev.off()

  expect_true(file.exists(tmp))
})

test_that(".grid_arrow handles various angles", {
  skip_if_not_installed("grid")

  grid_arrow <- cograph:::.grid_arrow

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  grid::grid.newpage()
  # Horizontal
  grid_arrow(0.8, 0.5, 0.2, 0.5, col = "#800020")
  # Vertical
  grid_arrow(0.5, 0.8, 0.5, 0.2, col = "#000080")
  # Diagonal
  grid_arrow(0.9, 0.1, 0.1, 0.9, col = "#008000")
  grDevices::dev.off()

  expect_true(file.exists(tmp))
})

# =============================================================================
# .get_motif_names ADDITIONAL TESTS
# =============================================================================

test_that(".get_motif_names handles all size/directed combinations", {
  get_motif_names <- cograph:::.get_motif_names

  # Size 3 directed
  names_3d <- get_motif_names(3, TRUE)
  expect_equal(length(names_3d), 16)
  expect_true("030T" %in% names_3d)

  # Size 3 undirected
  names_3u <- get_motif_names(3, FALSE)
  expect_true("empty" %in% names_3u)
  expect_true("edge" %in% names_3u)
  expect_true("triangle" %in% names_3u)

  # Size 4 directed (199 motifs)
  names_4d <- get_motif_names(4, TRUE)
  expect_equal(length(names_4d), 199)
  expect_true(all(grepl("^M", names_4d)))

  # Size 4 undirected (11 motifs)
  names_4u <- get_motif_names(4, FALSE)
  expect_equal(length(names_4u), 11)

  # Fallback for unknown size
  names_other <- get_motif_names(5, TRUE)
  expect_true(all(grepl("^motif_", names_other)))
})

# =============================================================================
# .classify_triads_vectorized ADDITIONAL TESTS
# =============================================================================

test_that(".classify_triads_vectorized handles all MAN types", {
  classify_triads <- cograph:::.classify_triads_vectorized

  # Test specific MAN types
  # 003 - Empty
  expect_equal(classify_triads(0, 0, 0, 0, 0, 0), "003")

  # 012 - Single edge
  expect_equal(classify_triads(1, 0, 0, 0, 0, 0), "012")

  # 102 - Mutual edge
  expect_equal(classify_triads(1, 1, 0, 0, 0, 0), "102")

  # 300 - Full clique
  expect_equal(classify_triads(1, 1, 1, 1, 1, 1), "300")
})

test_that(".classify_triads_vectorized handles vectorized input", {
  classify_triads <- cograph:::.classify_triads_vectorized

  # Multiple triads at once
  e_ij <- c(0, 1, 1)
  e_ji <- c(0, 1, 1)
  e_ik <- c(0, 0, 1)
  e_ki <- c(0, 0, 1)
  e_jk <- c(0, 0, 1)
  e_kj <- c(0, 0, 1)

  result <- classify_triads(e_ij, e_ji, e_ik, e_ki, e_jk, e_kj)

  expect_equal(length(result), 3)
  expect_equal(result[1], "003")  # Empty
  expect_equal(result[2], "102")  # Mutual pair
  expect_equal(result[3], "300")  # Full clique
})

# =============================================================================
# motif_census EDGE CASE TESTS
# =============================================================================

test_that("motif_census handles undirected configuration method", {
  skip_if_not_installed("igraph")

  # Create symmetric matrix
  mat <- matrix(c(0, 1, 1, 0,
                   1, 0, 1, 0,
                   1, 1, 0, 1,
                   0, 0, 1, 0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- motif_census(mat, n_random = 5, method = "configuration", seed = 42)
  expect_s3_class(result, "cograph_motifs")
  expect_false(result$directed)
})

test_that("motif_census handles gnm method with undirected network", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  mat <- mat + t(mat)
  mat[mat > 1] <- 1
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- motif_census(mat, n_random = 5, method = "gnm", seed = 42)
  expect_s3_class(result, "cograph_motifs")
  expect_equal(result$method, "gnm")
})

test_that("motif_census handles tetradic motifs (size = 4)", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(6, seed = 456, density = 0.5)

  result <- motif_census(mat, size = 4, n_random = 5, seed = 42)
  expect_s3_class(result, "cograph_motifs")
  expect_equal(result$size, 4)
})

test_that("motif_census auto-detects directed from asymmetric matrix", {
  skip_if_not_installed("igraph")

  # Asymmetric matrix should be detected as directed
  mat <- matrix(c(0, 1, 0,
                   0, 0, 1,
                   1, 0, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- motif_census(mat, n_random = 5, seed = 42)
  expect_true(result$directed)
})

# =============================================================================
# print.cograph_motifs EDGE CASE TESTS
# =============================================================================

test_that("print.cograph_motifs displays over and under-represented motifs", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(8, seed = 789, density = 0.5)
  result <- motif_census(mat, n_random = 30, seed = 42)

  output <- capture.output(print(result))

  # Should have Over/Under counts
  expect_true(any(grepl("Over-represented|Under-represented", output)))
})

# =============================================================================
# plot.cograph_motifs EDGE CASE TESTS
# =============================================================================

test_that("plot.cograph_motifs handles undirected network for network type", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  # Undirected should fall back to bar plot
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- motif_census(mat, n_random = 5, seed = 42)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  # Network type with undirected should message and use bar
  expect_message(p <- plot(result, type = "network", show_nonsig = TRUE))
  grDevices::dev.off()

  expect_true(is.null(p) || inherits(p, "gg"))
})

test_that("plot.cograph_motifs handles size != 3 for network type", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_test_matrix(6, seed = 123, density = 0.5)
  result <- motif_census(mat, size = 4, n_random = 5, seed = 42)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  # Size 4 with network type should message and use bar
  expect_message(p <- plot(result, type = "network", show_nonsig = TRUE))
  grDevices::dev.off()

  expect_true(is.null(p) || inherits(p, "gg"))
})

test_that("plot.cograph_motifs handles motifs not in standard triads", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  # Create mock result with non-standard motif names
  mock_result <- list(
    counts = c(custom1 = 5, custom2 = 3),
    null_mean = c(custom1 = 2.5, custom2 = 1.5),
    null_sd = c(custom1 = 1.0, custom2 = 0.5),
    z_scores = c(custom1 = 2.5, custom2 = 3.0),
    p_values = c(custom1 = 0.01, custom2 = 0.002),
    significant = c(custom1 = TRUE, custom2 = TRUE),
    size = 3,
    directed = TRUE,
    n_random = 10,
    method = "configuration"
  )
  class(mock_result) <- "cograph_motifs"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  # Should message about no standard triads and fall back to bar
  expect_message(p <- plot(mock_result, type = "network"))
  grDevices::dev.off()

  expect_true(is.null(p) || inherits(p, "gg"))
})

# =============================================================================
# extract_triads ADDITIONAL TESTS
# =============================================================================

test_that("extract_triads handles multiple involving nodes", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_test_matrix(6, seed = 123)
  result <- extract_triads(mat, involving = c("A", "B"), min_total = 0)

  if (nrow(result) > 0) {
    has_AB <- apply(result[, c("A", "B", "C")], 1, function(x)
      any(c("A", "B") %in% x))
    expect_true(all(has_AB))
  }
})

test_that("extract_triads handles multiple type filters", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_test_matrix(6, seed = 123)
  result <- extract_triads(mat, type = c("021D", "021U", "021C"), min_total = 0)

  expect_true(is.data.frame(result))
  if (nrow(result) > 0) {
    expect_true(all(result$type %in% c("021D", "021U", "021C")))
  }
})

# =============================================================================
# .count_triads_matrix_vectorized EDGE CASE TESTS
# =============================================================================

test_that(".count_triads_matrix_vectorized handles small matrix", {
  count_triads <- cograph:::.count_triads_matrix_vectorized

  # Too small (< 3 nodes)
  mat <- matrix(c(0, 1, 0, 0), 2, 2)
  result <- count_triads(mat, edge_method = "any", edge_threshold = 0)

  expect_null(result)
})

test_that(".count_triads_matrix_vectorized handles all-zero matrix", {
  count_triads <- cograph:::.count_triads_matrix_vectorized

  mat <- matrix(0, 5, 5)
  result <- count_triads(mat, edge_method = "any", edge_threshold = 0)

  expect_null(result)
})

test_that(".count_triads_matrix_vectorized expected method without expected_mat errors", {
  count_triads <- cograph:::.count_triads_matrix_vectorized

  mat <- create_weighted_test_matrix(5, seed = 123)

  # Should error without expected_mat
  expect_error(count_triads(mat, edge_method = "expected", edge_threshold = 1.5))
})

test_that(".count_triads_matrix_vectorized handles include and exclude together", {
  count_triads <- cograph:::.count_triads_matrix_vectorized

  mat <- create_weighted_test_matrix(5, seed = 123)

  result <- count_triads(mat, edge_method = "any", edge_threshold = 0,
                         include = c("030T", "030C", "300"),
                         exclude = c("300"))

  # Either NULL or data frame
  expect_true(is.null(result) || is.data.frame(result))
  if (!is.null(result) && nrow(result) > 0) {
    expect_true(all(result$type %in% c("030T", "030C")))
    expect_false("300" %in% result$type)
  }
})

# =============================================================================
# extract_motifs ADDITIONAL EDGE CASES
# =============================================================================

test_that("extract_motifs handles pattern 'closed'", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_test_matrix(6, seed = 123)
  result <- extract_motifs(mat, pattern = "closed", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))

  if (!is.null(result) && nrow(result$results) > 0) {
    # closed excludes 003, 012, 021C, 120C
    expect_false(any(result$results$type %in% c("003", "012", "021C", "120C")))
  }
})

test_that("extract_motifs handles 'expected' edge_method", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_test_matrix(5, seed = 123, max_weight = 20)
  result <- extract_motifs(mat, edge_method = "expected", edge_threshold = 1.0,
                           pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs handles multiple id columns in data frame", {
  skip_if_not_installed("igraph")

  df <- data.frame(
    group = rep(c("X", "Y"), each = 10),
    person = rep(1:5, 4),
    from = sample(c("A", "B", "C"), 20, replace = TRUE),
    to = sample(c("A", "B", "C"), 20, replace = TRUE)
  )

  result <- extract_motifs(data = df, id = c("group", "person"),
                           pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs handles igraph without vertex names", {
  skip_if_not_installed("igraph")

  # Create igraph without names from the start
  n <- 5
  edges <- c(1, 2, 2, 3, 3, 4, 4, 5, 1, 3, 2, 4)
  g <- igraph::make_graph(edges, directed = TRUE, n = n)

  result <- extract_motifs(g, pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs handles igraph with edge weights", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_test_matrix(5, seed = 123)
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE)

  result <- extract_motifs(g, pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("extract_motifs errors on missing data columns", {
  skip_if_not_installed("igraph")

  df <- data.frame(
    id = 1:5,
    from = c("A", "B", "C", "A", "B")
    # missing 'to' column
  )

  expect_error(extract_motifs(data = df, id = "id"))
})

test_that("extract_motifs errors on missing id column", {
  skip_if_not_installed("igraph")

  df <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A")
  )

  expect_error(extract_motifs(data = df, id = "nonexistent"))
})

test_that("extract_motifs handles level parameter with individual data", {
  skip_if_not_installed("igraph")

  df <- data.frame(
    id = rep(1:3, each = 5),
    from = sample(c("A", "B", "C"), 15, replace = TRUE),
    to = sample(c("A", "B", "C"), 15, replace = TRUE)
  )

  # Force individual level
  result_ind <- extract_motifs(data = df, id = "id", level = "individual",
                               pattern = "all", min_transitions = 0)

  expect_true(is.null(result_ind) || result_ind$params$level == "individual")
})

test_that("extract_motifs warns when individual level requested without individual data", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_test_matrix(5, seed = 123)

  # Request individual level but provide matrix (no individual data)
  expect_warning(
    extract_motifs(mat, level = "individual", pattern = "all", min_transitions = 0),
    "Individual level requested but no individual data"
  )
})

# =============================================================================
# print.cograph_motif_analysis EDGE CASE TESTS
# =============================================================================

test_that("print.cograph_motif_analysis shows threshold for non-any methods", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_test_matrix(6, seed = 123)
  result <- extract_motifs(mat, edge_method = "percent", edge_threshold = 0.15,
                           pattern = "all", min_transitions = 0)

  if (!is.null(result)) {
    output <- capture.output(print(result))
    expect_true(any(grepl("threshold", output)))
  }
})

test_that("print.cograph_motif_analysis handles significance results", {
  skip_if_not_installed("igraph")

  mat <- create_weighted_test_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0,
                           significance = TRUE, n_perm = 5, seed = 42)

  if (!is.null(result) && nrow(result$results) > 0) {
    output <- capture.output(print(result))
    expect_true(length(output) > 0)
  }
})

# =============================================================================
# plot.cograph_motif_analysis EDGE CASE TESTS
# =============================================================================

test_that("plot.cograph_motif_analysis handles triads with missing type pattern", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_test_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result) && nrow(result$results) > 0) {
    # Modify result to have unknown type
    result$results$type[1] <- "UNKNOWN"

    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 400, height = 400)
    plot(result, type = "triads", n = 2)
    grDevices::dev.off()

    expect_true(file.exists(tmp))
  }
})

test_that("plot.cograph_motif_analysis handles empty results", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  # Create mock empty result
  mock_result <- list(
    results = data.frame(triad = character(0), type = character(0),
                         observed = numeric(0), stringsAsFactors = FALSE),
    type_summary = table(character(0)),
    params = list(
      level = "aggregate",
      edge_method = "any",
      pattern = "triangle",
      significance = FALSE
    )
  )
  class(mock_result) <- "cograph_motif_analysis"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  expect_message(plot(mock_result, type = "triads"))
  grDevices::dev.off()
})

test_that("plot.cograph_motif_analysis significance type without significance data", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_test_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0,
                           significance = FALSE)

  if (!is.null(result) && nrow(result$results) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp)
    # Should fall back since no significance data
    p <- plot(result, type = "significance")
    grDevices::dev.off()

    # Either returns gg or falls back to triads
    expect_true(is.null(p) || inherits(p, "gg"))
  }
})

test_that("plot.cograph_motif_analysis handles legend parameter", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_test_matrix(5, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result) && nrow(result$results) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 400, height = 400)
    plot(result, type = "triads", n = 4, legend = TRUE)
    grDevices::dev.off()

    expect_true(file.exists(tmp))
  }
})

test_that("plot.cograph_motif_analysis patterns type with empty results", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mock_result <- list(
    results = data.frame(triad = character(0), type = character(0),
                         observed = numeric(0), stringsAsFactors = FALSE),
    type_summary = table(character(0)),
    params = list(
      level = "aggregate",
      edge_method = "any",
      pattern = "triangle",
      significance = FALSE
    )
  )
  class(mock_result) <- "cograph_motif_analysis"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  expect_message(plot(mock_result, type = "patterns"))
  grDevices::dev.off()
})

# =============================================================================
# .plot_motif_patterns TESTS
# =============================================================================

test_that(".plot_motif_patterns displays motifs with MAN types", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_test_matrix(6, seed = 123)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result) && length(result$type_summary) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 600, height = 400)
    plot(result, type = "patterns", n = 6)
    grDevices::dev.off()

    expect_true(file.exists(tmp))
  }
})

# =============================================================================
# extract_motifs_temporal EDGE CASE TESTS
# =============================================================================

test_that("extract_motifs_temporal handles missing columns in edge list", {
  skip_if_not_installed("igraph")

  df <- data.frame(
    from = c("A", "B"),
    to = c("B", "C")
    # missing time column
  )

  expect_error(extract_motifs_temporal(df, from = "from", to = "to", time = "time"))
})

test_that("extract_motifs_temporal handles missing columns in long format", {
  skip_if_not_installed("igraph")

  df <- data.frame(
    person = 1:5,
    time = 1:5
    # missing state column
  )

  expect_error(extract_motifs_temporal(df, id = "person", format = "long"))
})

test_that("extract_motifs_temporal handles invalid input type", {
  skip_if_not_installed("igraph")

  expect_error(extract_motifs_temporal("invalid string"))
  expect_error(extract_motifs_temporal(123))
})

test_that("extract_motifs_temporal handles id columns in wide format", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(
    person_id = 1:10,
    T1 = sample(c("A", "B", "C"), 10, replace = TRUE),
    T2 = sample(c("A", "B", "C"), 10, replace = TRUE),
    T3 = sample(c("A", "B", "C"), 10, replace = TRUE)
  )

  result <- extract_motifs_temporal(wide_data, id = "person_id",
                                     window_size = 2, pattern = "all",
                                     min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_temporal_motifs"))
})

test_that("extract_motifs_temporal handles missing id column error", {
  skip_if_not_installed("igraph")

  set.seed(123)
  wide_data <- data.frame(
    T1 = sample(c("A", "B", "C"), 10, replace = TRUE),
    T2 = sample(c("A", "B", "C"), 10, replace = TRUE)
  )

  expect_error(extract_motifs_temporal(wide_data, id = "nonexistent"))
})

test_that("extract_motifs_temporal handles tna_windows result input", {
  skip_if_not_installed("igraph")

  # Create mock tna_windows-like result
  mock_windows <- list(
    windows = list(
      list(weights = create_weighted_test_matrix(4, seed = 1),
           labels = c("A", "B", "C", "D")),
      list(weights = create_weighted_test_matrix(4, seed = 2),
           labels = c("A", "B", "C", "D"))
    ),
    start_times = c(1, 2),
    end_times = c(2, 3),
    na_proportions = c(0, 0)
  )
  class(mock_windows) <- "tna_windows"

  result <- extract_motifs_temporal(mock_windows, pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_temporal_motifs"))
})

test_that("extract_motifs_temporal handles edge list with weights", {
  skip_if_not_installed("igraph")

  edge_df <- data.frame(
    from = c("A", "B", "C", "A"),
    to = c("B", "C", "A", "C"),
    time = c(1, 1, 2, 2),
    weight = c(2, 3, 1, 4)
  )

  result <- extract_motifs_temporal(edge_df, from = "from", to = "to",
                                     time = "time", window_size = 2,
                                     pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_temporal_motifs"))
})

test_that("extract_motifs_temporal handles insufficient time points", {
  skip_if_not_installed("igraph")

  edge_df <- data.frame(
    from = c("A"),
    to = c("B"),
    time = c(1)
  )

  result <- extract_motifs_temporal(edge_df, from = "from", to = "to",
                                     time = "time", window_size = 5,
                                     pattern = "all", min_transitions = 0)

  expect_null(result)
})

# =============================================================================
# .long_to_wide TESTS
# =============================================================================

test_that(".long_to_wide handles multiple id columns", {
  long_to_wide <- cograph:::.long_to_wide

  long_df <- data.frame(
    group = rep(c("X", "Y"), each = 6),
    person = rep(1:2, each = 3, times = 2),
    time = rep(1:3, 4),
    state = c("A", "B", "C", "B", "C", "A", "C", "A", "B", "A", "B", "C")
  )

  result <- long_to_wide(long_df, id = c("group", "person"), time = "time", state = "state")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)  # 4 unique group-person combinations
})

test_that(".long_to_wide handles missing values", {
  long_to_wide <- cograph:::.long_to_wide

  # Some combinations don't have all time points
  long_df <- data.frame(
    id = c(1, 1, 2, 2, 2),
    time = c(1, 2, 1, 2, 3),
    state = c("A", "B", "C", "A", "B")
  )

  result <- long_to_wide(long_df, id = "id", time = "time", state = "state")

  expect_true(is.data.frame(result))
  # Person 1 doesn't have time 3, should be NA
  expect_true(is.na(result[1, "T3"]))
})

# =============================================================================
# .edge_list_to_windows TESTS
# =============================================================================

test_that(".edge_list_to_windows handles step > 1 (tumbling windows)", {
  edge_to_win <- cograph:::.edge_list_to_windows

  edges <- data.frame(
    from = c("A", "B", "C", "A", "B", "C"),
    to = c("B", "C", "A", "C", "A", "B"),
    time = c(1, 1, 2, 2, 3, 3)
  )

  result <- edge_to_win(edges, from = "from", to = "to", time = "time",
                        window_size = 2, step = 2)

  expect_true(is.list(result))
  expect_true(length(result$windows) > 0 || length(result$start_times) == 0)
})

# =============================================================================
# plot.cograph_temporal_motifs EDGE CASE TESTS
# =============================================================================

test_that("plot.cograph_temporal_motifs handles empty type columns", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  # Create mock with no type columns
  mock_result <- list(
    windows = list(),
    summary = data.frame(window = integer(0), start = integer(0),
                         end = integer(0), type = character(0),
                         count = integer(0)),
    type_trends = data.frame(window = 1:3, start = 1:3, end = 2:4),
    params = list(n_windows = 3, window_size = 2, pattern = "all")
  )
  class(mock_result) <- "cograph_temporal_motifs"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  expect_message(plot(mock_result, type = "trends"))
  grDevices::dev.off()
})

test_that("plot.cograph_temporal_motifs heatmap handles empty summary", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mock_result <- list(
    windows = list(),
    summary = data.frame(window = integer(0), start = integer(0),
                         end = integer(0), type = character(0),
                         count = integer(0)),
    type_trends = data.frame(window = integer(0)),
    params = list(n_windows = 0, window_size = 2, pattern = "all")
  )
  class(mock_result) <- "cograph_temporal_motifs"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  expect_message(plot(mock_result, type = "heatmap"))
  grDevices::dev.off()
})

# =============================================================================
# triad_persistence EDGE CASE TESTS
# =============================================================================

test_that("triad_persistence handles empty temporal result", {
  skip_if_not_installed("igraph")

  mock_temporal <- list(
    windows = list(NULL, NULL),
    summary = data.frame(),
    params = list(n_windows = 2)
  )
  class(mock_temporal) <- "cograph_temporal_motifs"

  expect_warning(result <- triad_persistence(mock_temporal))
  expect_null(result)
})

test_that("triad_persistence classifies status correctly", {
  skip_if_not_installed("igraph")

  set.seed(456)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 100, replace = TRUE),
    nrow = 25, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal, min_persistence = 0.5)

    if (!is.null(result) && nrow(result$triads) > 0) {
      # Check all statuses are valid
      valid_statuses <- c("persistent", "transient", "emerging", "fading", "sporadic")
      expect_true(all(result$triads$status %in% valid_statuses))
    }
  }
})

test_that("triad_persistence window_totals computed correctly", {
  skip_if_not_installed("igraph")

  set.seed(789)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 80, replace = TRUE),
    nrow = 20, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal, by = "type")

    if (!is.null(result)) {
      expect_true("window_totals" %in% names(result))
      expect_equal(length(result$window_totals), result$params$n_windows)
    }
  }
})

# =============================================================================
# plot.cograph_triad_persistence EDGE CASE TESTS
# =============================================================================

test_that("plot.cograph_triad_persistence handles large matrices", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(111)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C", "D", "E"), 300, replace = TRUE),
    nrow = 60, ncol = 5
  ))
  names(wide_data) <- paste0("T", 1:5)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal)

    if (!is.null(result) && nrow(result$triads) > 0) {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)

      grDevices::png(tmp, width = 800, height = 600)
      # Should adapt text size for dense matrix
      p <- plot(result, type = "heatmap", show_counts = TRUE)
      grDevices::dev.off()

      expect_true(inherits(p, "gg") || is.null(p))
    }
  }
})

test_that("plot.cograph_triad_persistence handles top_n = NULL", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(222)
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
      # top_n = NULL should show all triads
      p <- plot(result, type = "heatmap", top_n = NULL)
      grDevices::dev.off()

      expect_true(inherits(p, "gg") || is.null(p))
    }
  }
})

test_that("plot.cograph_triad_persistence timeline handles empty triads", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mock_result <- list(
    triads = data.frame(
      label = character(0),
      type = character(0),
      n_windows = integer(0),
      first_window = integer(0),
      last_window = integer(0),
      windows = character(0),
      persistence = numeric(0),
      total_count = integer(0),
      status = character(0),
      triad = character(0)
    ),
    counts_matrix = matrix(0, nrow = 0, ncol = 3),
    presence_matrix = matrix(0, nrow = 0, ncol = 3),
    summary = list(n_items = 0, n_persistent = 0, n_transient = 0,
                   n_emerging = 0, n_fading = 0, n_sporadic = 0),
    params = list(by = "triad", n_windows = 3)
  )
  class(mock_result) <- "cograph_triad_persistence"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  expect_message(plot(mock_result, type = "timeline"))
  grDevices::dev.off()
})

test_that("plot.cograph_triad_persistence status handles no counts", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mock_result <- list(
    triads = data.frame(
      label = character(0),
      type = character(0),
      n_windows = integer(0),
      first_window = integer(0),
      last_window = integer(0),
      windows = character(0),
      persistence = numeric(0),
      total_count = integer(0),
      status = character(0),
      triad = character(0)
    ),
    counts_matrix = matrix(0, nrow = 0, ncol = 3),
    presence_matrix = matrix(0, nrow = 0, ncol = 3),
    summary = list(n_items = 0, n_persistent = 0, n_transient = 0,
                   n_emerging = 0, n_fading = 0, n_sporadic = 0),
    params = list(by = "triad", n_windows = 3)
  )
  class(mock_result) <- "cograph_triad_persistence"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp)
  expect_message(plot(mock_result, type = "status"))
  grDevices::dev.off()
})

test_that("plot.cograph_triad_persistence heatmap handles long triad names", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  # Create mock result with long triad names
  long_names <- paste0("VeryLongStateName", 1:30, "_ExtraInfo_MoreData")

  mock_result <- list(
    triads = data.frame(
      label = long_names,
      type = rep("030T", 30),
      n_windows = rep(2, 30),
      first_window = rep(1, 30),
      last_window = rep(2, 30),
      windows = rep("1,2", 30),
      persistence = rep(0.67, 30),
      total_count = rep(5, 30),
      status = rep("persistent", 30),
      triad = long_names
    ),
    counts_matrix = matrix(sample(1:5, 60, replace = TRUE), nrow = 30, ncol = 2,
                           dimnames = list(long_names, c("W1", "W2"))),
    presence_matrix = matrix(1, nrow = 30, ncol = 2,
                             dimnames = list(long_names, c("W1", "W2"))),
    window_totals = c(W1 = 100, W2 = 100),
    summary = list(n_items = 30, n_persistent = 30, n_transient = 0,
                   n_emerging = 0, n_fading = 0, n_sporadic = 0),
    params = list(by = "triad", n_windows = 2)
  )
  class(mock_result) <- "cograph_triad_persistence"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp, width = 800, height = 1200)
  # Should truncate long names
  p <- plot(mock_result, type = "heatmap")
  grDevices::dev.off()

  expect_true(inherits(p, "gg") || is.null(p))
})

test_that("plot.cograph_triad_persistence heatmap binary fill type", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  set.seed(333)
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
      p <- plot(result, type = "heatmap", fill = "binary")
      grDevices::dev.off()

      expect_true(inherits(p, "gg") || is.null(p))
    }
  }
})

test_that("plot.cograph_triad_persistence handles coord_ratio adjustment", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  # Create mock result with many windows, few triads (wide aspect ratio)
  mock_result <- list(
    triads = data.frame(
      label = c("A - B - C", "B - C - D"),
      type = c("030T", "030C"),
      n_windows = c(5, 5),
      first_window = c(1, 1),
      last_window = c(5, 5),
      windows = c("1,2,3,4,5", "1,2,3,4,5"),
      persistence = c(1.0, 1.0),
      total_count = c(10, 8),
      status = c("persistent", "persistent"),
      triad = c("A - B - C", "B - C - D")
    ),
    counts_matrix = matrix(1:10, nrow = 2, ncol = 5,
                           dimnames = list(c("A - B - C", "B - C - D"),
                                           paste0("W", 1:5))),
    presence_matrix = matrix(1, nrow = 2, ncol = 5,
                             dimnames = list(c("A - B - C", "B - C - D"),
                                             paste0("W", 1:5))),
    window_totals = rep(10, 5),
    summary = list(n_items = 2, n_persistent = 2, n_transient = 0,
                   n_emerging = 0, n_fading = 0, n_sporadic = 0),
    params = list(by = "triad", n_windows = 5)
  )
  class(mock_result) <- "cograph_triad_persistence"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp, width = 600, height = 200)
  p <- plot(mock_result, type = "heatmap")
  grDevices::dev.off()

  expect_true(inherits(p, "gg") || is.null(p))
})

# =============================================================================
# print.cograph_triad_persistence EDGE CASE TESTS
# =============================================================================

test_that("print.cograph_triad_persistence shows by=type mode", {
  skip_if_not_installed("igraph")

  set.seed(444)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    result <- triad_persistence(temporal, by = "type")

    if (!is.null(result)) {
      output <- capture.output(print(result))
      expect_true(any(grepl("MAN types", output)))
    }
  }
})

test_that("print.cograph_triad_persistence handles empty results", {
  skip_if_not_installed("igraph")

  mock_result <- list(
    triads = data.frame(
      label = character(0),
      type = character(0),
      n_windows = integer(0),
      first_window = integer(0),
      last_window = integer(0),
      windows = character(0),
      persistence = numeric(0),
      total_count = integer(0),
      status = character(0),
      triad = character(0)
    ),
    counts_matrix = matrix(0, nrow = 0, ncol = 2),
    presence_matrix = matrix(0, nrow = 0, ncol = 2),
    summary = list(n_items = 0, n_persistent = 0, n_transient = 0,
                   n_emerging = 0, n_fading = 0, n_sporadic = 0),
    params = list(by = "triad", n_windows = 2)
  )
  class(mock_result) <- "cograph_triad_persistence"

  output <- capture.output(print(mock_result))
  expect_true(any(grepl("No triads found", output)))
})

# =============================================================================
# .plot_triad_networks EDGE CASE TESTS
# =============================================================================

test_that(".plot_triad_networks handles triads with short node names", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  # Create result with single-letter node names
  mat <- create_weighted_test_matrix(4, seed = 555)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(result) && nrow(result$results) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 400, height = 400)
    plot(result, type = "triads", n = 4, ncol = 2)
    grDevices::dev.off()

    expect_true(file.exists(tmp))
  }
})

test_that(".plot_triad_networks handles significance data", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")

  mat <- create_weighted_test_matrix(5, seed = 666)
  result <- extract_motifs(mat, pattern = "all", min_transitions = 0,
                           significance = TRUE, n_perm = 5, seed = 42)

  if (!is.null(result) && nrow(result$results) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    grDevices::png(tmp, width = 400, height = 400)
    # Should display p-values and z-scores
    plot(result, type = "triads", n = 4)
    grDevices::dev.off()

    expect_true(file.exists(tmp))
  }
})

# =============================================================================
# ADDITIONAL EDGE CASE AND BOUNDARY TESTS
# =============================================================================

test_that("motif functions handle network with exactly 3 nodes", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1,
                   1, 0, 1,
                   0, 1, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- motif_census(mat, n_random = 5, seed = 42)
  expect_s3_class(result, "cograph_motifs")

  triads <- extract_triads(mat, min_total = 0)
  expect_true(is.data.frame(triads))
})

test_that("extract_motifs handles data with weight column", {
  skip_if_not_installed("igraph")

  df <- data.frame(
    id = rep(1:3, each = 4),
    from = sample(c("A", "B", "C"), 12, replace = TRUE),
    to = sample(c("A", "B", "C"), 12, replace = TRUE),
    weight = sample(1:5, 12, replace = TRUE)
  )

  result <- extract_motifs(data = df, id = "id", pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_motif_analysis"))
})

test_that("triad_persistence handles edge_weight with weights_matrix fallback", {
  skip_if_not_installed("igraph")

  set.seed(777)
  wide_data <- data.frame(matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 15, ncol = 4
  ))
  names(wide_data) <- paste0("T", 1:4)

  temporal <- extract_motifs_temporal(wide_data, window_size = 2,
                                       pattern = "all", min_transitions = 0)

  if (!is.null(temporal)) {
    # Remove counts_matrix to force weights_matrix fallback
    for (i in seq_along(temporal$windows)) {
      if (!is.null(temporal$windows[[i]])) {
        temporal$windows[[i]]$counts_matrix <- NULL
      }
    }

    result <- triad_persistence(temporal, edge_weight = TRUE)

    if (!is.null(result)) {
      expect_true(result$params$edge_weight)
    }
  }
})

test_that("extract_motifs_temporal auto-detects edge_list format", {
  skip_if_not_installed("igraph")

  edge_df <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    time = c(1, 2, 3)
  )

  result <- extract_motifs_temporal(edge_df, from = "from", to = "to",
                                     time = "time", format = "auto",
                                     pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_temporal_motifs"))
})

test_that("extract_motifs_temporal auto-detects long format", {
  skip_if_not_installed("igraph")

  long_df <- data.frame(
    person = rep(1:3, each = 3),
    time = rep(1:3, 3),
    state = sample(c("A", "B", "C"), 9, replace = TRUE)
  )

  result <- extract_motifs_temporal(long_df, id = "person", time = "time",
                                     state = "state", format = "auto",
                                     pattern = "all", min_transitions = 0)

  expect_true(is.null(result) || inherits(result, "cograph_temporal_motifs"))
})

test_that("extract_motifs_temporal handles default window_size by format", {
  skip_if_not_installed("igraph")

  # Edge list format should default window_size to 1
  edge_df <- data.frame(
    from = c("A", "B", "C", "A"),
    to = c("B", "C", "A", "C"),
    time = c(1, 2, 3, 4)
  )

  result <- extract_motifs_temporal(edge_df, from = "from", to = "to",
                                     time = "time", pattern = "all",
                                     min_transitions = 0)

  if (!is.null(result)) {
    expect_equal(result$params$window_size, 1)
  }
})
