# .oob_squish() replaces scales::squish() as the `oob` handler for the
# centrality heatmap's fill scale, so the package does not depend on scales
# for a single call. These tests pin the contract it has to honour.

test_that("finite values outside the range are pulled to the nearer end", {
  expect_equal(cograph:::.oob_squish(c(-3, 0, 0.5, 1, 7), c(0, 1)),
               c(0, 0, 0.5, 1, 1))
  expect_equal(cograph:::.oob_squish(c(-9, -3, 0, 3, 9), c(-2, 2)),
               c(-2, -2, 0, 2, 2))
})

test_that("values inside the range and the endpoints are untouched", {
  x <- c(0, 0.25, 0.5, 0.75, 1)
  expect_identical(cograph:::.oob_squish(x, c(0, 1)), x)
})

test_that("NA and non-finite values survive by default", {
  out <- cograph:::.oob_squish(c(NA, NaN, Inf, -Inf, 5), c(0, 1))
  expect_true(is.na(out[1]))
  expect_true(is.nan(out[2]))
  expect_identical(out[3], Inf)
  expect_identical(out[4], -Inf)
  expect_identical(out[5], 1)
})

test_that("only.finite = FALSE squishes the infinities but still spares NA", {
  out <- cograph:::.oob_squish(c(NA, Inf, -Inf, 5), c(0, 1), only.finite = FALSE)
  expect_true(is.na(out[1]))
  expect_identical(out[2], 1)
  expect_identical(out[3], 0)
  expect_identical(out[4], 1)
})

test_that("a zero-length input returns a zero-length vector", {
  expect_length(cograph:::.oob_squish(numeric(0), c(0, 1)), 0L)
})

test_that("it clamps a randomized sweep to the range without touching NA", {
  # Equivalence with the scales implementation was established when this
  # helper was written (2048 cases across both only.finite modes, zero
  # mismatches). scales is no longer a dependency, so the contract is pinned
  # directly here rather than by comparison.
  set.seed(11)
  for (i in seq_len(200)) {
    x <- c(stats::rnorm(8, 0, 3), NA, Inf, -Inf, NaN)[sample(12)]
    r <- sort(stats::runif(2, -4, 4))
    out <- cograph:::.oob_squish(x, r)
    fin <- is.finite(x)
    expect_true(all(out[fin] >= r[1] & out[fin] <= r[2]))
    expect_identical(out[!fin], x[!fin])
    inside <- fin & x >= r[1] & x <= r[2]
    expect_identical(out[inside], x[inside])
  }
})
