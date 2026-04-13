# Regression: integer-valued tna weight matrices (ftna, ctna, raw counts)
# must render edge labels without a ".00" decimal tail.

test_that("from_tna picks weight_digits = 0 for integer weights", {
  fit_int <- structure(
    list(weights = matrix(c(0, 12, 5, 0), 2, 2,
                          dimnames = list(c("a", "b"), c("a", "b"))),
         labels = c("a", "b"), inits = c(0.5, 0.5), directed = TRUE),
    class = c("tna", "list")
  )
  params <- from_tna(fit_int, plot = FALSE)
  expect_identical(params$weight_digits, 0L)
  expect_identical(params$edge_label_digits, 0L)
})

test_that("from_tna picks weight_digits = 2 for fractional weights", {
  fit_frac <- structure(
    list(weights = matrix(c(0, 0.32, 0.18, 0), 2, 2,
                          dimnames = list(c("a", "b"), c("a", "b"))),
         labels = c("a", "b"), inits = c(0.5, 0.5), directed = TRUE),
    class = c("tna", "list")
  )
  params <- from_tna(fit_frac, plot = FALSE)
  expect_identical(params$weight_digits, 2L)
})

test_that("explicit weight_digits still wins", {
  fit_int <- structure(
    list(weights = matrix(c(0, 12, 5, 0), 2, 2,
                          dimnames = list(c("a", "b"), c("a", "b"))),
         labels = c("a", "b"), inits = c(0.5, 0.5), directed = TRUE),
    class = c("tna", "list")
  )
  params <- from_tna(fit_int, plot = FALSE, weight_digits = 3)
  expect_identical(params$weight_digits, 3)
})
