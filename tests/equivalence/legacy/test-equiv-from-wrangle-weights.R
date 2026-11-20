# Equivalence tests moved from tests/testthat/test-wrangle-weights.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
ww_und <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- m[2, 1] <- 0.5
  m[1, 3] <- m[3, 1] <- 0.8
  m[2, 4] <- m[4, 2] <- 0.6
  m[3, 4] <- m[4, 3] <- 0.4
  m
}
ww_dir <- function() {
  m <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  m[1, 2] <- 0.5
  m[2, 1] <- 0.2
  m[2, 3] <- 0.7
  m
}
ww_signed <- function() {
  m <- ww_und()
  m[1, 2] <- m[2, 1] <- -0.9
  m
}

# ---- equivalence tests ----

test_that("symmetrize agrees with sna::symmetrize on the weak and strong rules", {
  skip_if_not_installed("sna")
  d <- (ww_dir() != 0) * 1

  weak <- sna::symmetrize(d, rule = "weak")
  strong <- sna::symmetrize(d, rule = "strong")

  # "strong" is the reciprocated-only rule, which is `mutual`. `min` is a
  # weight combination that keeps an unreciprocated edge at its own weight —
  # a different operation that happens to coincide on binary reciprocal data.
  expect_equal(unname(to_matrix(symmetrize(d, "max"))), unname(weak))
  expect_equal(unname(to_matrix(symmetrize(d, "mutual"))), unname(strong))
})
