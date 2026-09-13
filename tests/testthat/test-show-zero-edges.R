# Zero is how a weight matrix stores "no edge", so rounding an edge's weight
# to zero deletes it. `show_zero_edges = TRUE` is the opt-out. It was declared
# and documented on from_tna()/from_qgraph() for several releases without ever
# being read; these tests pin the behaviour it now has.

test_that(".keep_rounded_zero_edges rescues only the weights that would vanish", {
  m <- matrix(c(0, 0.004, -0.003, 0,
                0.5,  0,    0,     0.2,
                0,    0.9,  0,    -0.7,
                0,    0,    0.004, 0), 4, 4, byrow = TRUE)
  out <- cograph:::.keep_rounded_zero_edges(m, 2)
  at_risk <- m != 0 & round(m, 2) == 0
  expect_equal(sum(at_risk), 3L)
  # rescued entries survive rounding and keep their sign
  expect_true(all(round(out[at_risk], 2) != 0))
  expect_identical(sign(out[at_risk]), sign(m[at_risk]))
  expect_equal(abs(out[at_risk]), rep(0.01, 3))
  # everything else is untouched, to the bit
  expect_identical(out[!at_risk], m[!at_risk])
})

test_that("a matrix with nothing at risk comes back unchanged", {
  m <- matrix(c(0, 0.5, 0.9, 0), 2, 2)
  expect_identical(cograph:::.keep_rounded_zero_edges(m, 2), m)
  expect_identical(cograph:::.keep_rounded_zero_edges(m, NULL), m)
})

test_that("NA weights are left alone rather than erroring", {
  # matrix() fills column-wise: [2, 1] is NA and [1, 2] is 0.004.
  m <- matrix(c(0, NA, 0.004, 0), 2, 2)
  out <- cograph:::.keep_rounded_zero_edges(m, 2)
  expect_true(is.na(out[2, 1]))
  expect_equal(out[1, 2], 0.01)
})

test_that("digits = 0 rescues to the smallest representable integer", {
  # column-wise again: [2, 1] is 0.4 and [1, 2] is 3.
  m <- matrix(c(0, 0.4, 3, 0), 2, 2)
  out <- cograph:::.keep_rounded_zero_edges(m, 0)
  expect_equal(out[2, 1], 1)
  expect_equal(out[1, 2], 3)
})

test_that("from_tna(show_zero_edges = TRUE) keeps edges that rounding would drop", {
  skip_if_not_installed("tna")
  mod <- tna::tna(tna::group_regulation)
  w <- mod$weights
  at_risk <- sum(w != 0 & round(w, 2) == 0)
  skip_if(at_risk == 0, "fixture has no weight that rounds to zero")

  off <- from_tna(mod, plot = FALSE, show_zero_edges = FALSE)
  on  <- from_tna(mod, plot = FALSE, show_zero_edges = TRUE)

  expect_false(identical(off, on))
  expect_equal(sum(round(on$x, 2) != 0) - sum(round(off$x, 2) != 0), at_risk)
  keep <- round(w, 2) != 0
  expect_identical(on$x[keep], off$x[keep])
})
