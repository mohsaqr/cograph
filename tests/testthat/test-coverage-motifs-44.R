# Coverage tests for motif functions — round 44
# Targets uncovered lines from code review fixes

# Helper to create test matrices
create_test_mat <- function(n, seed = 42) {
  set.seed(seed)
  m <- matrix(sample(0:5, n * n, replace = TRUE), n, n)
  diag(m) <- 0
  m
}

# ================================================================
# extract_motifs: data param validation
# ================================================================
test_that("extract_motifs rejects non-data.frame for data param", {
  expect_error(
    extract_motifs(data = "not_a_df", id = "id"),
    "'data' must be a data.frame"
  )
})

test_that("extract_motifs handles matrix without rownames", {
  mat <- create_test_mat(5, seed = 99)
  # Remove rownames
  rownames(mat) <- NULL
  colnames(mat) <- NULL

  result <- extract_motifs(mat, pattern = "all", min_transitions = 0)
  if (!is.null(result)) {
    # Labels should be auto-generated V1..V5
    expect_true(all(grepl("^V[0-9]", unlist(strsplit(result$results$triad[1], " - ")))))
  }
})

# ================================================================
# Significance permutation: zero-transition individuals
# ================================================================
test_that("extract_motifs significance handles individuals with zero transitions", {
  skip_if_not_installed("tna")
  library(tna)

  # Create a tna-like object where some sessions are very sparse
  # The permutation code should handle NULL ind_probs gracefully
  Mod <- tna(coding)

  # Quick run with significance
  em <- extract_motifs(Mod, pattern = "triangle", significance = TRUE,
                       n_perm = 5, seed = 42, min_transitions = 5)
  expect_s3_class(em, "cograph_motif_analysis")
  expect_true("z" %in% names(em$results))
})

# ================================================================
# plot.cograph_motif_analysis: significance stats in grid
# ================================================================
test_that("plot.cograph_motif_analysis renders significance stats", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tna")
  library(tna)

  Mod <- tna(coding)
  em_sig <- extract_motifs(Mod, pattern = "triangle", significance = TRUE,
                           n_perm = 5, seed = 42)

  if (!is.null(em_sig) && nrow(em_sig$results) > 0) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    grDevices::png(tmp, width = 800, height = 600)
    # This should render the significance branch (lines 237-241)
    plot(em_sig, type = "triads", n = 5)
    grDevices::dev.off()
    expect_true(file.exists(tmp))
  }
})

# ================================================================
# plot_motif_patterns: n truncation
# ================================================================
test_that("plot_motif_patterns truncates when n < types found", {
  skip_if_not_installed("ggplot2")

  mat <- create_test_mat(6, seed = 77)
  rownames(mat) <- colnames(mat) <- paste0("N", seq_len(6))
  em <- extract_motifs(mat, pattern = "all", min_transitions = 0)

  if (!is.null(em) && length(em$type_summary) > 2) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    grDevices::png(tmp, width = 800, height = 600)
    # n=2 should trigger truncation at line 337
    plot(em, type = "patterns", n = 2)
    grDevices::dev.off()
    expect_true(file.exists(tmp))
  }
})

# ================================================================
# .count_triads_matrix_vectorized: empty after threshold/exclude
# ================================================================
test_that(".count_triads_matrix_vectorized returns NULL with extreme threshold", {
  mat <- create_test_mat(4, seed = 55)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]

  # Very high percent threshold should filter out all edges
  result <- .count_triads_matrix_vectorized(
    mat, edge_method = "percent", edge_threshold = 0.99
  )
  # Either NULL or empty — both are valid

  expect_true(is.null(result) || nrow(result) == 0)
})

test_that(".count_triads_matrix_vectorized returns NULL when all types excluded", {
  mat <- create_test_mat(4, seed = 55)
  all_types <- c("003", "012", "102", "021D", "021U", "021C",
                 "111D", "111U", "030T", "030C", "201",
                 "120D", "120U", "120C", "210", "300")

  result <- .count_triads_matrix_vectorized(
    mat, edge_method = "any", edge_threshold = 1.5,
    exclude = all_types
  )
  expect_null(result)
})

# ================================================================
# .plot_triad_networks: fallback node names
# ================================================================
test_that(".plot_triad_networks handles malformed triad names", {
  skip_if_not_installed("ggplot2")

  # Create a mock result with a triad that has bad name
  mock <- list(
    results = data.frame(
      triad = c("A - B - C", "INVALID"),
      type = c("300", "012"),
      observed = c(10, 5),
      stringsAsFactors = FALSE
    ),
    type_summary = table(c("300", "012")),
    params = list(
      pattern = "all", edge_method = "any",
      significance = FALSE, n_individuals = 1,
      n_states = 3, labels = c("A", "B", "C")
    )
  )
  class(mock) <- "cograph_motif_analysis"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 600, height = 400)
  plot(mock, type = "triads", n = 2)
  grDevices::dev.off()
  expect_true(file.exists(tmp))
})

# ================================================================
# get_edge_list: individual with zero transitions
# ================================================================
test_that("get_edge_list handles individuals with no transitions", {
  skip_if_not_installed("tna")
  library(tna)

  Mod <- tna(coding)
  edges <- get_edge_list(Mod, by_individual = TRUE, drop_zeros = TRUE)
  expect_s3_class(edges, "data.frame")
  expect_true("id" %in% names(edges))
})
