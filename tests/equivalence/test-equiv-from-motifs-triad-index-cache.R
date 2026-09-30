# Equivalence tests moved from tests/testthat/test-motifs-triad-index-cache.R:
# motifs() on the full tna::group_regulation against values pinned from the
# pre-refactor implementation. Developer-only; not part of R CMD check.

# ---- setup from the source file ----
.tic_matrix <- function(s, density, seed, max_weight = 7L) {
  set.seed(seed)
  m <- matrix(stats::rbinom(s * s, 1L, density) *
                sample.int(max_weight, s * s, replace = TRUE), s, s)
  diag(m) <- 0
  m
}
.tic_expected <- function(mat) {
  total <- sum(mat)
  if (total == 0) return(NULL)
  e <- outer(rowSums(mat), colSums(mat)) / total
  e[e == 0] <- 0.001
  e
}
.cores_limited <- function(n) {
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  nzchar(chk) && chk != "false" && n > 2L
}

# ---- equivalence tests ----

test_that("seeded census results match the pre-refactor values exactly", {
  skip_if_not_installed("tna")
  # Computed from the implementation at a27b0958, before any of this work.
  # Comparing a run to itself cannot catch a change that moves every number;
  # these literals can.
  model <- tna::tna(tna::group_regulation)
  got <- motifs(model, n_perm = 9L, seed = 4)$results

  expect_identical(got$type,
                   c("120C", "030T", "120U", "210", "120D", "030C", "300"))
  expect_identical(got$count, c(1481L, 620L, 190L, 581L, 178L, 1044L, 79L))
  expect_equal(got$expected,
               c(1010.9, 412.4, 138.1, 471.8, 128.2, 1081.6, 82),
               tolerance = 1e-8)
  expect_equal(got$p, c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.8), tolerance = 1e-8)
})

test_that("seeded instance results match the pre-refactor values exactly", {
  skip_if_not_installed("tna")
  model <- tna::tna(tna::group_regulation)
  got <- extract_motifs(model, n_perm = 6L, seed = 8,
                        significance = TRUE)$results
  expect_identical(nrow(got), 302L)
  expect_identical(got$observed[1], 1L)
  expect_equal(got$expected[1], 0, tolerance = 1e-8)
})
