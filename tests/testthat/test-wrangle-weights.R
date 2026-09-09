# =============================================================================
# Weight wrangling verbs: threshold_edges, binarize, symmetrize,
# normalize_weights, invert_weights
#
# Each verb gets a formula test against a hand-computed value, an invariant
# test, and an error-path test matched by condition class.
# =============================================================================

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

# --- threshold_edges ---------------------------------------------------------

test_that("threshold_edges(minimum) keeps exactly the edges at or above it", {
  kept <- as.data.frame(threshold_edges(ww_und(), minimum = 0.5))

  expect_equal(nrow(kept), 3L)
  expect_true(all(kept$weight >= 0.5))
})

test_that("threshold_edges(top) keeps the strongest n", {
  kept <- as.data.frame(suppressWarnings(threshold_edges(ww_und(), top = 2)))

  expect_equal(sort(kept$weight), c(0.6, 0.8))
})

test_that("threshold_edges(density) hits the requested density", {
  # 4 nodes undirected: 6 possible edges, density 0.5 asks for 3.
  kept <- threshold_edges(ww_und(), density = 0.5)

  expect_equal(n_edges(kept), 3L)
})

test_that("threshold_edges(proportion) keeps that share of the edges", {
  kept <- suppressWarnings(threshold_edges(ww_und(), proportion = 0.5))

  expect_equal(n_edges(kept), 2L)
})

test_that("threshold_edges combines criteria with AND", {
  both <- suppressWarnings(threshold_edges(ww_und(), minimum = 0.55, top = 1))

  expect_equal(n_edges(both), 1L)
  expect_equal(as.data.frame(both)$weight, 0.8)
})

test_that("threshold_edges keeps strong negative edges when absolute = TRUE", {
  signed <- ww_signed()

  with_abs <- as.data.frame(suppressWarnings(threshold_edges(signed, minimum = 0.7)))
  without <- as.data.frame(suppressWarnings(
    threshold_edges(signed, minimum = 0.7, absolute = FALSE)
  ))

  expect_true(-0.9 %in% with_abs$weight)
  expect_false(-0.9 %in% without$weight)
})

test_that("threshold_edges is monotone in the threshold", {
  und <- ww_und()
  counts <- vapply(c(0, 0.35, 0.55, 0.75, 1),
                   function(t) n_edges(suppressWarnings(threshold_edges(und, minimum = t))),
                   integer(1))

  expect_false(is.unsorted(rev(counts)))
})

test_that("threshold_edges keeps every node by default", {
  kept <- suppressWarnings(threshold_edges(ww_und(), top = 1))

  expect_equal(n_nodes(kept), 4L)
})

test_that("threshold_edges rejects an out-of-range proportion", {
  expect_error(threshold_edges(ww_und(), proportion = 2),
               class = "cograph_bad_selection")
  expect_error(threshold_edges(ww_und(), minimum = "a"),
               class = "cograph_bad_selection")
})

# --- binarize ----------------------------------------------------------------

test_that("binarize sets every surviving weight to one", {
  b <- as.data.frame(binarize(ww_und()))

  expect_equal(unique(b$weight), 1)
  expect_equal(nrow(b), 4L)
})

test_that("binarize drops edges at or below the threshold", {
  b <- as.data.frame(suppressWarnings(binarize(ww_und(), threshold = 0.45)))

  expect_equal(nrow(b), 3L)
})

test_that("binarize(signed = TRUE) keeps the sign of the association", {
  b <- as.data.frame(binarize(ww_signed(), signed = TRUE))

  expect_equal(sort(unique(b$weight)), c(-1, 1))
})

test_that("binarize is idempotent", {
  once <- binarize(ww_und())
  twice <- binarize(once)

  expect_equal(to_matrix(once), to_matrix(twice))
})

test_that("binarize rejects a non-numeric threshold", {
  expect_error(binarize(ww_und(), threshold = "high"),
               class = "cograph_bad_selection")
})

# --- symmetrize --------------------------------------------------------------

test_that("symmetrize computes the documented combination", {
  d <- ww_dir()

  expect_equal(to_matrix(symmetrize(d, "max"))["A", "B"], 0.5)
  expect_equal(to_matrix(symmetrize(d, "min"))["A", "B"], 0.2)
  expect_equal(to_matrix(symmetrize(d, "mean"))["A", "B"], 0.35)
  expect_equal(to_matrix(symmetrize(d, "sum"))["A", "B"], 0.7)
})

test_that("symmetrize(upper/lower) reads the named triangle", {
  d <- ww_dir()

  expect_equal(to_matrix(symmetrize(d, "upper"))["A", "B"], 0.5)
  expect_equal(to_matrix(symmetrize(d, "lower"))["A", "B"], 0.2)
})

test_that("symmetrize always produces a symmetric undirected network", {
  results <- lapply(c("max", "min", "mean", "sum", "mutual", "upper", "lower"),
                    function(m) symmetrize(ww_dir(), method = m))

  expect_true(all(vapply(results, function(r) isSymmetric(to_matrix(r)), logical(1))))
  expect_true(all(vapply(results, function(r) !is_directed(r), logical(1))))
})

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

test_that("symmetrize('min') keeps an unreciprocated edge, unlike 'mutual'", {
  d <- ww_dir()

  expect_equal(to_matrix(symmetrize(d, "min"))["B", "C"], 0.7)
  expect_equal(to_matrix(symmetrize(d, "mutual"))["B", "C"], 0)
})

test_that("symmetrize does not double self-loops under the sum rule", {
  d <- ww_dir()
  d[1, 1] <- 0.3

  expect_equal(to_matrix(symmetrize(d, "sum"))["A", "A"], 0.3)
})

test_that("symmetrize rejects an unknown method", {
  expect_error(symmetrize(ww_dir(), method = "bogus"))
})

# --- normalize_weights -------------------------------------------------------

test_that("normalize_weights('row') makes every non-empty row sum to one", {
  counts <- matrix(c(0, 3, 1,
                     2, 0, 4,
                     5, 1, 0), 3, 3, byrow = TRUE,
                   dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  m <- to_matrix(normalize_weights(counts, method = "row"))

  expect_equal(unname(rowSums(m)), rep(1, 3))
  expect_equal(m["A", "B"], 0.75)
})

test_that("normalize_weights('column') makes every non-empty column sum to one", {
  counts <- matrix(c(0, 3, 1,
                     2, 0, 4,
                     5, 1, 0), 3, 3, byrow = TRUE,
                   dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  m <- to_matrix(normalize_weights(counts, method = "column"))

  expect_equal(unname(colSums(m)), rep(1, 3))
})

test_that("normalize_weights('max') puts the largest weight at one", {
  m <- to_matrix(normalize_weights(ww_und(), method = "max"))

  expect_equal(max(m), 1)
  expect_equal(m["A", "B"], 0.5 / 0.8)
})

test_that("normalize_weights('sum') makes the weights total one", {
  m <- to_matrix(normalize_weights(ww_und(), method = "sum"))

  expect_equal(sum(m), 1)
})

test_that("normalize_weights('minmax') maps the extremes to zero-ish and one", {
  m <- to_matrix(normalize_weights(ww_und(), method = "minmax"))
  nz <- m[m != 0]

  expect_equal(max(nz), 1)
  expect_true(min(nz) < 1e-8)
})

test_that("normalize_weights preserves the edge set", {
  before <- ww_und() != 0
  after <- to_matrix(normalize_weights(ww_und(), method = "max")) != 0

  expect_equal(unname(before), unname(after))
})

test_that("normalize_weights warns rather than producing NaN on an empty row", {
  m <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  m[1, 2] <- 1

  expect_warning(result <- normalize_weights(m, method = "row"),
                 class = "cograph_zero_norm")
  expect_false(anyNA(to_matrix(result)))
})

# --- invert_weights ----------------------------------------------------------

test_that("invert_weights('reciprocal') is one over the weight", {
  inv <- as.data.frame(invert_weights(ww_und()))
  original <- as.data.frame(as_cograph(ww_und()))

  expect_equal(inv$weight, 1 / original$weight)
})

test_that("invert_weights('reflect') reverses the weight order and keeps every edge", {
  original <- as.data.frame(as_cograph(ww_und()))
  inv <- as.data.frame(invert_weights(ww_und(), method = "reflect"))

  expect_equal(nrow(inv), nrow(original))
  expect_equal(order(inv$weight), order(original$weight, decreasing = TRUE))
})

test_that("invert_weights('max_minus') drops the strongest edge and says so", {
  expect_warning(inv <- invert_weights(ww_und(), method = "max_minus"),
                 class = "cograph_edges_dropped")

  expect_equal(n_edges(inv), 3L)
})

test_that("invert_weights('reciprocal') is its own inverse", {
  twice <- invert_weights(invert_weights(ww_und()))

  expect_equal(to_matrix(twice), ww_und())
})

test_that("weight verbs return a network for every supported input class", {
  und <- ww_und()
  classes <- list(
    matrix = und,
    cograph = as_cograph(und),
    edgelist = as.data.frame(as_cograph(und))
  )

  results <- lapply(classes, function(x) threshold_edges(x, minimum = 0.4))
  expect_true(all(vapply(results, inherits, logical(1), "cograph_network")))
})

test_that("keep_format returns a matrix for matrix input", {
  out <- threshold_edges(ww_und(), minimum = 0.5, keep_format = TRUE)

  expect_true(is.matrix(out))
  expect_true(isSymmetric(out))
})
