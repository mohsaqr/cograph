# Batch 56 --- three measures:
#   dst   dynamical spanning tree centrality, eq. (50) of Lu et al. (2016),
#         Physics Reports 650:1-63 (attributed to Chen et al. 2003, unread)
#   kdec  Zhang, Wang, Sheng, Dai, Hu and Chen (2019), Information 10(10):311
#   counting_betweenness  Bloechl, Theis, Vega-Redondo and Fisher (2011),
#         Physical Review E 83:046127, eqs. (1), (6)-(8)
# Published values are compared in
# tests/equivalence/test-centrality-zoo-b-batch56.R.

b56_from_edges <- function(n, el, directed = FALSE) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  if (!directed) a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b56_kite <- function() {
  b56_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

b56_ring <- function(n) b56_from_edges(n, cbind(seq_len(n), c(2:n, 1L)))

b56_complete <- function(n) {
  a <- 1 - diag(n)
  dimnames(a) <- list(seq_len(n), seq_len(n))
  a
}

b56_quiet <- function(expr) {
  withCallingHandlers(expr, cograph_undefined_measure = \(w) {
    invokeRestart("muffleWarning")
  })
}

# --- dst -------------------------------------------------------------------

test_that("dst follows Cayley's formula and the cycle count", {
  # K_n has n^(n-2) spanning trees and K_(n-1) has (n-1)^(n-3).
  lapply(c(4L, 6L), \(n) {
    expect_equal(unname(centrality_dst(b56_complete(n))),
                 rep(1 - (n - 1)^(n - 3) / n^(n - 2), n))
  })
  # A cycle has n spanning trees; without a node it is a path with one.
  expect_equal(unname(centrality_dst(b56_ring(7L))), rep(1 - 1 / 7, 7))
})

test_that("dst: cut nodes score 1, pendants 0", {
  star <- b56_from_edges(5L, cbind(1L, 2:5))
  expect_equal(unname(centrality_dst(star)), c(1, 0, 0, 0, 0))
  # The kite has 2368 spanning trees; nodes 8 and 9 are cut nodes.
  kite <- centrality_dst(b56_kite())
  expect_equal(unname(kite[8:10]), c(1, 1, 0))
  expect_equal(exp(cograph:::.cg_log_spanning_trees(b56_kite())), 2368)
  expect_true(all(kite >= 0 & kite <= 1))
  pair <- b56_from_edges(2L, cbind(1L, 2L))
  expect_equal(unname(centrality_dst(pair)), c(0, 0))
})

test_that("dst is NA with a classed warning off its domain", {
  two <- b56_from_edges(4L, rbind(c(1, 2), c(3, 4)))
  expect_warning(out <- centrality_dst(two),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  lone <- matrix(0, 1L, 1L, dimnames = list("a", "a"))
  expect_warning(out <- centrality_dst(lone),
                 class = "cograph_undefined_measure")
  expect_true(is.na(out))
})

# --- kdec ------------------------------------------------------------------

test_that("kdec on a star, by hand", {
  # Four leaves, n = 5. W(hub) = 1 * 4, W(leaf) = 1. From the hub every
  # leaf is at eff = 1 + log10(1) = 1: KDEC(hub) = 4 * 4 / 5. From a leaf
  # the hub is at eff = 1 + log10(4), and the three other leaves lie one
  # more step (eff 1) beyond it.
  e <- 1 + log10(4)
  leaf <- (4 / e^2 + 3 / (e + 1)^2) / 5
  star <- b56_from_edges(5L, cbind(1L, 2:5))
  expect_equal(unname(centrality_kdec(star)), c(16 / 5, rep(leaf, 4)))
})

test_that("kdec counts a second neighbour once per path and skips neighbours", {
  # 4-cycle 1-2-3-4: node 3 is reached from 1 through 2 and through 4.
  ring <- b56_ring(4L)
  e <- 1 + log10(2)
  w <- 2 * 2
  expect_equal(unname(centrality_kdec(ring)),
               rep(w * (2 * w / e^2 + 2 * w / (2 * e)^2) / 4, 4))
  # In a triangle every second step lands on a neighbour or on the start.
  triangle <- b56_complete(3L)
  expect_equal(unname(centrality_kdec(triangle)),
               rep(4 * (2 * 4 / e^2) / 3, 3))
})

test_that("kdec: isolates score 0 and lower the others through n", {
  ring <- b56_ring(4L)
  padded <- rbind(cbind(ring, 0), 0)
  dimnames(padded) <- list(1:5, 1:5)
  out <- centrality_kdec(padded)
  expect_identical(unname(out[5]), 0)
  expect_equal(unname(out[1:4]), unname(centrality_kdec(ring)) * 4 / 5)
})

# --- counting betweenness --------------------------------------------------

# Equations (6)-(8) summed edge by edge.
cbet_literal <- function(a) {
  n <- nrow(a)
  m <- a / rowSums(a)
  per_target <- vapply(seq_len(n), \(target) {
    keep <- setdiff(seq_len(n), target)
    f <- solve(diag(n - 1L) - m[keep, keep, drop = FALSE])
    per_source <- vapply(seq_along(keep), \(s) {
      edge <- m[keep, keep, drop = FALSE] * f[s, ]
      visits <- (rowSums(edge) + colSums(edge)) / 2
      visits[s] <- visits[s] + 1
      out <- numeric(n)
      out[keep] <- visits
      out[target] <- 1
      out
    }, numeric(n))
    rowSums(per_source)
  }, numeric(n))
  rowSums(per_target) / (n * (n - 1))
}

test_that("counting betweenness equals the edge-by-edge sum of eqs. (6)-(8)", {
  set.seed(56)
  kite <- b56_kite()
  expect_equal(unname(centrality_counting_betweenness(kite)),
               cbet_literal(kite), tolerance = 1e-10)
  n <- 7L
  weighted <- b56_from_edges(n, cbind(seq_len(n), c(2:n, 1L)),
                             directed = TRUE)
  weighted <- weighted + (matrix(stats::runif(n * n) < 0.3, n, n)) *
    matrix(round(stats::runif(n * n, 0.5, 3), 2), n, n)
  diag(weighted) <- c(0.7, 0, 0, 1.3, 0, 0, 0)
  expect_equal(unname(centrality_counting_betweenness(weighted)),
               cbet_literal(weighted), tolerance = 1e-10)
})

test_that("counting betweenness on a pair is one visit each", {
  # s -> t in one step: the start counts 1 and the target counts 1.
  pair <- b56_from_edges(2L, cbind(1L, 2L))
  expect_equal(unname(centrality_counting_betweenness(pair)), c(1, 1))
})

test_that("a self-loop raises a node's counting betweenness", {
  # Bloechl et al., Fig. 2: 1-3, 2-3, 3-4 and a loop of weight a44 on node
  # 4. Node 4 overtakes node 3 once the loop is heavy enough.
  with_loop <- function(weight) {
    a <- b56_from_edges(4L, rbind(c(1, 3), c(2, 3), c(3, 4)))
    a[4, 4] <- weight
    centrality_counting_betweenness(a)
  }
  light <- with_loop(0.5)
  heavy <- with_loop(4)
  expect_gt(light[3], light[4])
  expect_gt(heavy[4], heavy[3])
  expect_gt(heavy[4], light[4])
})

test_that("nodes without in- or out-ties score 0; a weak rest gives NA", {
  # A directed triangle with a source feeding it.
  a <- b56_from_edges(4L, rbind(c(1, 2), c(2, 3), c(3, 1), c(4, 1)),
                      directed = TRUE)
  out <- centrality_counting_betweenness(a)
  expect_identical(unname(out[4]), 0)
  triangle <- b56_from_edges(3L, rbind(c(1, 2), c(2, 3), c(3, 1)),
                             directed = TRUE)
  expect_equal(unname(out[1:3]),
               unname(centrality_counting_betweenness(triangle)))
  two <- b56_from_edges(4L, rbind(c(1, 2), c(3, 4)))
  expect_warning(res <- centrality_counting_betweenness(two),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(res)))
  empty <- matrix(0, 3L, 3L, dimnames = list(1:3, 1:3))
  expect_warning(res <- centrality_counting_betweenness(empty),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(res)))
})

# --- shared ----------------------------------------------------------------

test_that("the three measures are permutation equivariant and registered", {
  set.seed(561)
  kite <- b56_kite()
  perm <- sample.int(10L)
  base <- centrality(kite, measures = c("dst", "kdec",
                                        "counting_betweenness"))
  moved <- centrality(kite[perm, perm], measures = c("dst", "kdec",
                                                     "counting_betweenness"))
  lapply(c("dst", "kdec", "counting_betweenness"), \(measure) {
    expect_equal(moved[[measure]], base[[measure]][perm], tolerance = 1e-10)
  })
  listed <- list_centralities()
  rows <- listed[listed$measure %in% c("dst", "kdec",
                                       "counting_betweenness"), ]
  expect_identical(nrow(rows), 3L)
  expect_identical(rows$costly[rows$measure == "counting_betweenness"], TRUE)
  expect_identical(rows$uses_weights[rows$measure == "counting_betweenness"],
                   TRUE)
  expect_equal(centrality_dst(kite * 3), centrality_dst(kite))
  expect_equal(centrality_kdec(kite * 3), centrality_kdec(kite))
})
