# Batch 54 --- X-nonbacktracking centrality (Torres, Chan, Tong and
# Eliassi-Rad 2021, SIAM Journal on Mathematics of Data Science
# 3(2):656-675), Proposition 3.7 / equation (3.11), pages 660-661.
# Comparisons with an edge-space evaluation of the same proposition are in
# tests/equivalence/test-centrality-zoo-b-x-nonbacktracking.R.

xnb_from_edges <- function(n, el) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  a[el[, 2:1, drop = FALSE]] <- 1
  a
}

xnb_complete <- function(n) {
  a <- 1 - diag(n)
  dimnames(a) <- list(seq_len(n), seq_len(n))
  a
}

xnb_kite <- function() {
  xnb_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

xnb_quiet <- function(expr) {
  withCallingHandlers(expr, cograph_undefined_measure = \(w) {
    invokeRestart("muffleWarning")
  })
}

test_that("a complete graph on n nodes scores (n - 2)^2", {
  # Without a node the rest is K_(n-1): the eigenvector is constant, x, on
  # its (n-1)(n-2) directed edges, every node collects (n-2)x, and
  # v'Pv = (n-1)(n-2)x^2 = 1. The closed form gives
  # ((n-1)^2 - (n-1)) (n-2)^2 x^2 = (n-2)^2.
  lapply(c(5L, 6L, 8L), \(n) {
    expect_equal(unname(centrality_x_nonbacktracking(xnb_complete(n))),
                 rep((n - 2)^2, n), tolerance = 1e-10)
  })
})

test_that("a node of degree one scores zero where the score is defined", {
  # K5 with a pendant on node 1. The paper states X = 0 for such a node.
  a <- xnb_from_edges(6L, rbind(t(utils::combn(5L, 2L)), c(1, 6)))
  scores <- centrality_x_nonbacktracking(a)
  expect_equal(unname(scores[6]), 0)
  # The pendant carries no nonbacktracking weight, so the other nodes score
  # as in K5.
  expect_equal(unname(scores[2:5]), rep(9, 4), tolerance = 1e-10)
})

test_that("the kite reproduces the values retained from the source audit", {
  # candidate_review47/xnb_worked.csv, two implementations agreeing there to
  # 5.8e-16; printed to nine decimals.
  retained <- c(6.307744781, 6.307744781, 3.593575014, 13.378203981,
                3.593575014, 8.320892671, 8.320892671, 1.202548658, 0, 0)
  expect_equal(unname(centrality_x_nonbacktracking(xnb_kite())), retained,
               tolerance = 1e-8)
})

test_that("the score is permutation equivariant and ignores weights", {
  set.seed(54)
  kite <- xnb_kite()
  scores <- centrality_x_nonbacktracking(kite)
  perm <- sample.int(nrow(kite))
  expect_equal(unname(centrality_x_nonbacktracking(kite[perm, perm])),
               unname(scores)[perm], tolerance = 1e-10)
  expect_equal(centrality_x_nonbacktracking(kite * 3), scores)
  arcs <- kite
  arcs[lower.tri(arcs)] <- 0
  expect_equal(centrality_x_nonbacktracking(arcs), scores)
})

test_that("a forest or single cycles left behind give NA with a warning", {
  # Removing any node of a 6-cycle leaves a path.
  ring <- xnb_from_edges(6L, cbind(1:6, c(2:6, 1L)))
  expect_warning(out <- centrality_x_nonbacktracking(ring),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  expect_identical(cograph:::.cg_x_nonbacktracking(ring)$status,
                   rep("forest", 6L))
  # Two triangles joined by a bridge: removing a node leaves one triangle,
  # a single cycle, at the top.
  bridge <- xnb_from_edges(6L, rbind(c(1, 2), c(2, 3), c(1, 3), c(3, 4),
                                     c(4, 5), c(5, 6), c(4, 6)))
  expect_identical(cograph:::.cg_x_nonbacktracking(bridge)$status,
                   rep("cycles", 6L))
})

test_that("two parts sharing the largest eigenvalue give NA at the cut node", {
  # Node 9 joins two K4. Without it the two K4 tie; without any other node
  # one K4 is left whole and is the unique largest part.
  k4 <- t(utils::combn(4L, 2L))
  joined <- xnb_from_edges(9L, rbind(k4, k4 + 4L, c(1, 9), c(5, 9)))
  terms <- cograph:::.cg_x_nonbacktracking(joined)
  expect_identical(terms$status, c(rep("ok", 8L), "tied"))
  expect_true(is.na(terms$value[9]))
  expect_false(anyNA(terms$value[1:8]))
  expect_warning(centrality_x_nonbacktracking(joined),
                 class = "cograph_undefined_measure")
})

test_that("zero is a value and NA is not zero", {
  # Kite nodes 9 and 10 score exactly zero; a path scores NA everywhere.
  expect_equal(unname(centrality_x_nonbacktracking(xnb_kite())[9:10]),
               c(0, 0))
  path <- xnb_from_edges(5L, cbind(1:4, 2:5))
  expect_true(all(is.na(xnb_quiet(centrality_x_nonbacktracking(path)))))
})

test_that("degenerate inputs do not error", {
  lone <- matrix(0, 1L, 1L, dimnames = list("a", "a"))
  expect_true(is.na(xnb_quiet(centrality_x_nonbacktracking(lone))))
  empty <- matrix(0, 3L, 3L, dimnames = list(letters[1:3], letters[1:3]))
  expect_true(all(is.na(xnb_quiet(centrality_x_nonbacktracking(empty)))))
})

test_that("x_nonbacktracking is registered as costly and mode-free", {
  listed <- list_centralities()
  row <- listed[listed$measure == "x_nonbacktracking", ]
  expect_identical(nrow(row), 1L)
  expect_true(row$costly)
  expect_false(row$mode_aware)
  expect_false(row$uses_weights)
  out <- centrality(xnb_complete(5L), measures = "x_nonbacktracking")
  expect_identical(names(out), c("node", "x_nonbacktracking"))
})
