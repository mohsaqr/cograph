# Tests for centrality functions

# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

skip_on_cran()

test_that("centrality works with adjacency matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true("node" %in% names(result))
})

test_that("centrality works with specific measures", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat, measures = c("degree", "betweenness"))
  expect_true(is.data.frame(result))
  expect_true("degree_all" %in% names(result))
  expect_true("betweenness" %in% names(result))
})

test_that("centrality accepts packaged edge-list data directly", {
  data(student_interactions, package = "cograph")

  result <- centrality(student_interactions)

  expect_true(is.data.frame(result))
  expect_true(all(c("node", "degree_all", "strength_all", "closeness_all",
                    "betweenness", "eigenvector", "pagerank") %in% names(result)))
  expect_equal(nrow(result), length(unique(c(student_interactions$from, student_interactions$to))))
})

test_that("centrality_degree works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality_degree(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_strength works", {
  mat <- matrix(c(0, 0.5, 0.8, 0.5, 0, 0.3, 0.8, 0.3, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality_strength(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_betweenness works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_betweenness(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_closeness works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_closeness(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_eigenvector works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_eigenvector(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_pagerank works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_pagerank(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
  expect_true(all(result >= 0))
})

test_that("centrality with normalization works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality(mat, normalized = TRUE)
  expect_true(is.data.frame(result))
  # All normalized values should be <= 1
  numeric_cols <- sapply(result, is.numeric)
  for (col in names(result)[numeric_cols]) {
    expect_true(all(result[[col]] <= 1, na.rm = TRUE))
  }
})

test_that("centrality with directed network works", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Use specific measures to avoid alpha centrality singularity on small matrices
  result <- centrality(mat, directed = TRUE, mode = "in",
                       measures = c("degree", "betweenness", "closeness"))
  expect_true(is.data.frame(result))
  expect_true("degree_in" %in% names(result))
})

test_that("centrality with cograph_network works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  result <- centrality(net)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that("centrality with igraph object works", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- centrality(g)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
})

test_that("centrality with sorting works", {
  mat <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  0, 1, 1, 0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- centrality(mat, sort_by = "degree_all")
  expect_true(is.data.frame(result))
  # Check descending order
  expect_true(result$degree_all[1] >= result$degree_all[nrow(result)])
})

test_that("centrality with digits rounding works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality(mat, digits = 2)
  expect_true(is.data.frame(result))
})

test_that("centrality errors on invalid measures", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  expect_error(centrality(mat, measures = "invalid_measure"))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Mathematical Equivalence Tests
# ═══════════════════════════════════════════════════════════════════════════════

# Test network used for validation
.test_mat <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  0, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(.test_mat) <- colnames(.test_mat) <- LETTERS[1:5]
.test_g <- igraph::graph_from_adjacency_matrix(.test_mat, mode = "undirected")

# --- igraph-backed measures ---


# --- centiserve package comparison ---


# --- sna package comparison ---


test_that("tna_network = FALSE on a tna object gives cograph's default diffusion", {
  skip_if_not_installed("tna")
  W <- matrix(c(0.4, 0.3, 0.3,
                0.2, 0.5, 0.3,
                0.1, 0.2, 0.7), 3, 3, byrow = TRUE)
  rownames(W) <- colnames(W) <- c("A", "B", "C")
  t1 <- tna::tna(W)

  # The comparisons with tna::centralities() are in
  # tests/equivalence/test-centrality-a1-tna.R.
  # Default kandhway_kuri on a raw matrix is the binary-degree formula.
  cog_kk <- cograph::centrality(W, measures = "diffusion")$diffusion_all

  # tna_network = FALSE on tna input opts out and returns cograph defaults.
  cog_off <- cograph::centrality(t1, measures = "diffusion",
                                  tna_network = FALSE)$diffusion_all
  expect_equal(cog_off, cog_kk, tolerance = 1e-12)
})

test_that("tna_network respects user-explicit overrides", {
  skip_if_not_installed("tna")
  W <- matrix(c(0.4, 0.3, 0.3,
                0.2, 0.5, 0.3,
                0.1, 0.2, 0.7), 3, 3, byrow = TRUE)
  rownames(W) <- colnames(W) <- c("A", "B", "C")
  t1 <- tna::tna(W)

  # User says tna_network = TRUE but explicitly passes loops = TRUE.
  # loops should stay TRUE; everything else takes tna defaults.
  out <- cograph::centrality(t1, measures = "diffusion",
                              tna_network = TRUE, loops = TRUE)
  # With loops kept on a row-stochastic matrix, the power-series collapses
  # to a constant n for every node, which has no min-max scaling: NaN, as
  # in tna >= 1.5.8. With the tna default loops = FALSE the scores vary.
  expect_true(all(is.nan(out$diffusion_all)))
  dropped <- cograph::centrality(t1, measures = "diffusion", tna_network = TRUE)
  expect_equal(range(dropped$diffusion_all), c(0, 1))

  # User explicitly sets transitivity_type = "local" — onnela should NOT
  # silently take over even with tna_network = TRUE. Positive assertion:
  # the explicit-override result equals the baseline obtained by manually
  # mirroring the umbrella's other defaults (loops = FALSE,
  # invert_weights = TRUE) under tna_network = FALSE. If the override
  # works, the umbrella adds nothing on top.
  out_explicit_local <- cograph::centrality(t1, measures = "transitivity",
                              tna_network = TRUE, transitivity_type = "local")
  out_baseline_local <- cograph::centrality(t1, measures = "transitivity",
                              tna_network = FALSE, transitivity_type = "local",
                              loops = FALSE, invert_weights = TRUE)
  expect_equal(out_explicit_local$transitivity,
               out_baseline_local$transitivity, tolerance = 1e-12)

  # And it must NOT equal the onnela value the umbrella would have picked.
  out_default_onnela <- cograph::centrality(t1, measures = "transitivity",
                              tna_network = TRUE)
  expect_false(isTRUE(all.equal(out_explicit_local$transitivity,
                                out_default_onnela$transitivity,
                                tolerance = 1e-3)))
})


# --- NetworkX package comparison (via reticulate) ---


# The pseudo-inverse check of current_flow_closeness is in
# tests/equivalence/test-centrality-flow.R.

# --- Property-based tests (fallback when NetworkX unavailable) ---

test_that("percolation equals betweenness with uniform states", {
  # When all states are 1, percolation should equal normalized betweenness
  perc <- centrality_percolation(.test_mat)
  betw <- centrality_betweenness(.test_mat)

  # Percolation should be 0 where betweenness is 0
  expect_true(all(perc[betw == 0] == 0))

  # Should equal normalized betweenness (correlation = 1)
  expect_equal(cor(perc, betw), 1, tolerance = 1e-6)
})

test_that("voterank returns valid scores", {
  vr <- centrality_voterank(.test_mat)

  # Node C has highest degree (4), should rank highest
  expect_equal(unname(vr["C"]), max(vr))

  # All values should be in [0, 1]
  expect_true(all(vr >= 0 & vr <= 1))
})

# ============================================
# Tiered centrality via `type` argument
# ============================================

test_that("centrality default tier is 'basic' with 6 canonical measures", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat)
  cols <- setdiff(names(result), "node")
  # basic resolves to 6 measure names; each measure expands to 1+ columns
  # (e.g. degree -> degree_all), so we check that each measure has a column.
  expect_true(any(grepl("^degree", cols)))
  expect_true(any(grepl("^strength", cols)))
  expect_true(any(grepl("^closeness", cols)))
  expect_true("betweenness" %in% cols)
  expect_true("eigenvector" %in% cols)
  expect_true("pagerank" %in% cols)
  # And confirms the trimming: no extended-only measure like katz/coreness
  expect_false("katz" %in% cols)
  expect_false(any(grepl("^coreness", cols)))
})

test_that("centrality type='extended' is a strict superset of basic", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  basic_cols <- setdiff(names(centrality(mat, type = "basic")), "node")
  ext_cols <- setdiff(names(centrality(mat, type = "extended")), "node")

  expect_true(all(basic_cols %in% ext_cols))
  expect_gt(length(ext_cols), length(basic_cols))
  expect_true(any(grepl("^coreness", ext_cols)))
  expect_true("katz" %in% ext_cols)
})

test_that("centrality type='all' returns every measure", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  all_cols <- setdiff(names(centrality(mat, type = "all")), "node")
  ext_cols <- setdiff(names(centrality(mat, type = "extended")), "node")
  # "all" must include everything in "extended" plus more
  expect_true(all(ext_cols %in% all_cols))
  expect_gt(length(all_cols), length(ext_cols))
})

test_that("centrality explicit measures= overrides type", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Asking for only pagerank should trump the "extended" tier
  result <- centrality(mat, type = "extended", measures = "pagerank")
  cols <- setdiff(names(result), "node")
  expect_equal(cols, "pagerank")
})

test_that("centrality type argument rejects invalid values", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  expect_error(centrality(mat, type = "bogus"), "should be one of")
})
