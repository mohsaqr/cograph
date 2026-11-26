# Batch 53 --- total and mediative effects centrality (Friedkin 1991,
# American Journal of Sociology 96(6):1478-1504): TEC, equation (14), page
# 1487, with the uniform total effects of page 1484, and MEC, equations
# (22)-(23), page 1490. The printed values of Table 1 are compared in
# tests/equivalence/test-centrality-zoo-b-friedkin.R.

friedkin_from_edges <- function(n, el, directed = FALSE) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  if (!directed) a[el[, 2:1, drop = FALSE]] <- 1
  a
}

friedkin_star <- function(leaves) {
  friedkin_from_edges(leaves + 1L, cbind(1L, seq_len(leaves) + 1L))
}

friedkin_cycle <- function(n, directed = FALSE) {
  friedkin_from_edges(n, cbind(seq_len(n), c(seq_len(n - 1L) + 1L, 1L)),
                      directed = directed)
}

friedkin_kite <- function() {
  friedkin_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

# Equations (22)-(23) through hitting probabilities instead of fundamental
# matrices: h_i = P(reach j before k from i) solves h = W h off {j, k}.
mec_by_hitting <- function(a) {
  n <- nrow(a)
  diag(a) <- 1
  w <- a / rowSums(a)
  share <- function(j, k) {
    free <- setdiff(seq_len(n), c(j, k))
    h <- solve(diag(length(free)) - w[free, free, drop = FALSE], w[free, j])
    sum(h) / (n - 2)
  }
  vapply(seq_len(n), \(j) {
    sum(vapply(setdiff(seq_len(n), j), \(k) share(j, k), numeric(1))) / (n - 1)
  }, numeric(1))
}

quietly_undefined <- function(expr) {
  withCallingHandlers(expr, cograph_undefined_measure = \(w) {
    invokeRestart("muffleWarning")
  })
}

test_that("mec on a star: the hub transmits everything, a leaf 3/8", {
  # Hand computation for four leaves. Every sequence between leaves passes
  # the hub, so the hub scores 1. For a leaf j: with the hub absorbing no
  # other leaf reaches j (0); with another leaf absorbing, each of the three
  # remaining starts reaches j first with probability 1/2. Mean over the four
  # absorbers: (0 + 3 * 1/2) / 4 = 3/8.
  expect_equal(unname(centrality_mec(friedkin_star(4L))),
               c(1, rep(3 / 8, 4)))
})

test_that("mec on a directed cycle is one half at every node", {
  # 1 -> 2 -> 3 -> 4 -> 1. For j = 2: absorber 4 gives 1/2 (start 1 passes
  # 2, start 3 does not), absorber 1 gives 0, absorber 3 gives 1.
  expect_equal(unname(centrality_mec(friedkin_cycle(4L, directed = TRUE))),
               rep(0.5, 4))
})

test_that("mec matches the hitting-probability form on general graphs", {
  set.seed(53)
  graphs <- c(list(friedkin_kite(), friedkin_cycle(7L)), lapply(1:3, \(i) {
    n <- 9L
    ring <- friedkin_cycle(n, directed = TRUE)
    extra <- matrix(stats::runif(n * n) < 0.2, n, n) * 1
    a <- (ring + extra > 0) * 1
    diag(a) <- 0
    a
  }))
  lapply(graphs, \(a) {
    expect_equal(unname(centrality_mec(a)), mec_by_hitting(a),
                 tolerance = 1e-12)
  })
})

test_that("tec is the degree plus one, normalised, on undirected graphs", {
  kite <- friedkin_kite()
  expect_equal(unname(centrality_tec(kite)),
               unname((rowSums(kite) + 1) / sum(rowSums(kite) + 1)))
})

test_that("tec is a left eigenvector of the influence matrix and sums to one", {
  set.seed(531)
  n <- 8L
  a <- (friedkin_cycle(n, directed = TRUE) +
          (matrix(stats::runif(n * n) < 0.25, n, n)) > 0) * 1
  diag(a) <- 0
  dimnames(a) <- list(seq_len(n), seq_len(n))
  tec <- unname(centrality_tec(a))
  w <- a
  diag(w) <- 1
  w <- w / rowSums(w)
  expect_equal(as.numeric(tec %*% w), tec, tolerance = 1e-12)
  expect_equal(sum(tec), 1)
})

test_that("mec lies in the unit interval and both are permutation equivariant", {
  set.seed(532)
  kite <- friedkin_kite()
  mec <- centrality_mec(kite)
  expect_true(all(mec >= 0 & mec <= 1))
  perm <- sample.int(nrow(kite))
  expect_equal(unname(centrality_mec(kite[perm, perm])), unname(mec)[perm])
  expect_equal(unname(centrality_tec(kite[perm, perm])),
               unname(centrality_tec(kite))[perm])
})

test_that("a cut vertex outranks the nodes it separates", {
  # Path 1-2-3-4-5: the middle node mediates most, the ends least.
  path <- friedkin_from_edges(5L, cbind(1:4, 2:5))
  mec <- unname(centrality_mec(path))
  expect_gt(mec[3], mec[2])
  expect_gt(mec[2], mec[1])
  expect_equal(mec, rev(mec))
})

test_that("weights, loops and mode are ignored", {
  kite <- friedkin_kite()
  looped <- kite
  diag(looped) <- 1
  expect_equal(centrality_mec(kite * 2.5), centrality_mec(kite))
  expect_equal(centrality_mec(looped), centrality_mec(kite))
  expect_equal(centrality_tec(kite * 2.5), centrality_tec(kite))
  directed <- friedkin_cycle(5L, directed = TRUE)
  expect_equal(centrality(directed, measures = "mec", mode = "in")$mec,
               centrality(directed, measures = "mec", mode = "out")$mec)
})

test_that("a reducible chain gives NA with a classed warning", {
  two_parts <- friedkin_from_edges(4L, rbind(c(1, 2), c(3, 4)))
  expect_warning(out <- centrality_mec(two_parts),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  expect_warning(out <- centrality_tec(two_parts),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  # A directed path is weakly but not strongly connected.
  one_way <- friedkin_from_edges(3L, cbind(1:2, 2:3), directed = TRUE)
  expect_warning(out <- centrality_mec(one_way),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
})

test_that("small networks: tec needs two nodes, mec three", {
  pair <- friedkin_from_edges(2L, cbind(1L, 2L))
  expect_equal(unname(centrality_tec(pair)), c(0.5, 0.5))
  expect_warning(out <- centrality_mec(pair),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  lone <- matrix(0, 1L, 1L, dimnames = list("a", "a"))
  expect_warning(out <- centrality_tec(lone),
                 class = "cograph_undefined_measure")
  expect_true(is.na(out))
  expect_warning(out <- centrality_mec(lone),
                 class = "cograph_undefined_measure")
  expect_true(is.na(out))
})

test_that("tec and mec are registered as costly, mode-free and unweighted", {
  listed <- list_centralities()
  rows <- listed[listed$measure %in% c("tec", "mec"), ]
  expect_identical(nrow(rows), 2L)
  expect_true(all(rows$costly))
  expect_false(any(rows$mode_aware))
  expect_false(any(rows$uses_weights))
  out <- quietly_undefined(centrality(friedkin_kite(),
                                      measures = c("tec", "mec")))
  expect_identical(names(out), c("node", "tec", "mec"))
})
