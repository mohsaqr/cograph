# Equivalence tests moved from tests/testthat/test-robustness.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
skip_on_cran()
set.seed(42)
mat_simple <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  0, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(mat_simple) <- colnames(mat_simple) <- LETTERS[1:5]
mat_weighted <- matrix(c(
  0.0, 0.8, 0.5, 0.0, 0.0,
  0.8, 0.0, 0.7, 0.3, 0.0,
  0.5, 0.7, 0.0, 0.6, 0.4,
  0.0, 0.3, 0.6, 0.0, 0.9,
  0.0, 0.0, 0.4, 0.9, 0.0
), 5, 5, byrow = TRUE)
rownames(mat_weighted) <- colnames(mat_weighted) <- LETTERS[1:5]
cat("\n=== All Robustness Tests Passed ===\n")

# ---- equivalence tests ----

test_that("static strategy matches brainGraph output", {
  skip_if_not_installed("brainGraph")

  set.seed(99)
  g <- igraph::sample_gnm(25, 50, directed = FALSE)
  igraph::V(g)$name <- paste0("V", seq_len(igraph::vcount(g)))

  cg <- robustness(g, measure = "betweenness", strategy = "static")
  bg <- brainGraph::robustness(g, type = "vertex", measure = "btwn.cent")

  expect_equal(cg$comp_pct, bg$comp.pct)
})

test_that("static strategy with degree matches brainGraph", {
  skip_if_not_installed("brainGraph")

  set.seed(99)
  g <- igraph::sample_gnm(25, 50, directed = FALSE)
  igraph::V(g)$name <- paste0("V", seq_len(igraph::vcount(g)))

  cg <- robustness(g, measure = "degree", strategy = "static")
  bg <- brainGraph::robustness(g, type = "vertex", measure = "degree")

  expect_equal(cg$comp_pct, bg$comp.pct)
})
