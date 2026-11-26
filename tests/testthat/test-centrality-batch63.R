# Batch 63 --- TOPSIS with relative entropy (Liu, Jiang, Wang and Yu 2015,
# Knowledge-Based Systems 84:56-66), eqs. (2)-(4), (6), (7), (10), (12),
# (13) and section 2.2.5. The paper prints no per-node value. Comparisons
# with NetworkX-built indicators are in
# tests/equivalence/test-centrality-zoo-b-topsis-re.R.

tre_from_edges <- function(n, el) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  a[el[, 2:1, drop = FALSE]] <- 1
  a
}

tre_kite <- function() {
  tre_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

# Equations (6), (7), (10), (12), (13) on a matrix of four indicators.
tre_score <- function(indicators) {
  weights <- c(0.0625, 0.1875, 0.1875, 0.5625)
  r <- vapply(seq_len(4L), \(j) {
    weights[j] * indicators[, j] / sqrt(sum(indicators[, j]^2))
  }, numeric(nrow(indicators)))
  distance <- function(i, ideal) {
    terms <- vapply(seq_len(4L), \(j) {
      first <- if (ideal[j] > 0) ideal[j] * log(ideal[j] / r[i, j]) else 0
      first + (1 - ideal[j]) * log((1 - ideal[j]) / (1 - r[i, j]))
    }, numeric(1))
    sqrt(max(sum(terms), 0))
  }
  vapply(seq_len(nrow(r)), \(i) {
    far <- distance(i, apply(r, 2L, max))
    near <- distance(i, apply(r, 2L, min))
    if (is.infinite(far)) 0 else near / (far + near)
  }, numeric(1))
}

test_that("a node best on all four indicators scores 1; zero indicators 0", {
  # Star: the hub leads on degree, closeness, betweenness and is removed
  # last; the leaves have zero betweenness.
  star <- tre_from_edges(5L, cbind(1L, 2:5))
  expect_equal(unname(centrality_topsis_re(star)), c(1, 0, 0, 0, 0))
})

test_that("topsis_re equals eqs. (6)-(13) on hand-built indicators", {
  # Path 1-2-3-4-5. Degree / 4; closeness 4 / (10, 7, 6, 7, 10);
  # betweenness 0, 3, 4, 3, 0; improved k-shell 1, 2, 3, 2, 1.
  path <- tre_from_edges(5L, cbind(1:4, 2:5))
  indicators <- cbind(c(1, 2, 2, 2, 1) / 4, 4 / c(10, 7, 6, 7, 10),
                      c(0, 3, 4, 3, 0), c(1, 2, 3, 2, 1))
  expect_equal(unname(centrality_topsis_re(path)), tre_score(indicators))
  expect_equal(unname(centrality_topsis_re(path))[c(1, 3, 5)], c(0, 1, 0))
})

test_that("topsis_re lies in [0, 1] and puts the kite's hub region on top", {
  score <- centrality_topsis_re(tre_kite())
  expect_true(all(score >= 0 & score <= 1))
  # The pendant and the two degree-three nodes without betweenness: 0.
  expect_equal(unname(score[c(3, 5, 10)]), c(0, 0, 0))
  expect_equal(unname(score[1]), unname(score[2]))
  expect_equal(unname(score[6]), unname(score[7]))
})

test_that("topsis_re is equivariant and ignores direction and weights", {
  set.seed(63)
  kite <- tre_kite()
  score <- centrality_topsis_re(kite)
  perm <- sample.int(10L)
  expect_equal(unname(centrality_topsis_re(kite[perm, perm])),
               unname(score)[perm])
  arcs <- kite
  arcs[lower.tri(arcs)] <- 0
  expect_equal(centrality_topsis_re(arcs), score)
  expect_equal(centrality_topsis_re(kite * 3), score)
})

test_that("topsis_re is NA off its domain, with a classed warning", {
  ring <- tre_from_edges(6L, cbind(1:6, c(2:6, 1L)))
  expect_warning(out <- centrality_topsis_re(ring),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  two <- tre_from_edges(4L, rbind(c(1, 2), c(3, 4)))
  expect_warning(out <- centrality_topsis_re(two),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  lone <- matrix(0, 1L, 1L, dimnames = list("a", "a"))
  expect_warning(out <- centrality_topsis_re(lone),
                 class = "cograph_undefined_measure")
  expect_true(is.na(out))
  row <- list_centralities()
  expect_identical(sum(row$measure == "topsis_re"), 1L)
})
