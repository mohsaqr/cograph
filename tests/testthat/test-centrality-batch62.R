# Batch 62 --- hybrid centrality (Christensen, Kenett, Aste, Silvia and
# Kwapil 2018, Behavior Research Methods 50(6):2531-2550; accepted
# manuscript, page 26). Comparisons with igraph and NetworkToolbox are in
# tests/equivalence/test-centrality-zoo-b-hybrid.R.

hybrid_from_edges <- function(n, el, w = 1) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- w
  a[el[, 2:1, drop = FALSE]] <- w
  a
}

test_that("hybrid centrality on a star: the hub is first on everything", {
  # Four leaves, unweighted: the hub has rank 5 on all eight centralities,
  # the leaves tie and share the largest rank of their group, 4.
  star <- hybrid_from_edges(5L, cbind(1L, 2:5))
  expect_equal(unname(centrality_hybrid(star)),
               c((8 * 5 - 8) / 32, rep((8 * 4 - 8) / 32, 4)))
})

test_that("hybrid centrality on a path, by hand", {
  # Path 1-2-3, unweighted. Betweenness 0, 1, 0; closeness 1/3, 1/2, 1/3;
  # degree 1, 2, 1; eigenvector smallest at the ends. Middle: rank 3 eight
  # times -> 1. Ends: tied at rank 2 eight times -> (16 - 8) / 16.
  path <- hybrid_from_edges(3L, cbind(1:2, 2:3))
  expect_equal(unname(centrality_hybrid(path)), c(0.5, 1, 0.5))
})

test_that("all nodes of a regular vertex-transitive network tie at one", {
  ring <- hybrid_from_edges(6L, cbind(1:6, c(2:6, 1L)))
  expect_equal(unname(centrality_hybrid(ring)), rep(1, 6))
})

test_that("weights enter four of the eight rankings", {
  # Triangle with one strong tie (1-2). Unweighted, the three nodes tie.
  # Weighted: nodes 1 and 2 lead on strength and eigenvector centrality;
  # node 3 is closest (its two ties are the short ones... lengths 1/w).
  el <- rbind(c(1, 2), c(1, 3), c(2, 3))
  flat <- hybrid_from_edges(3L, el)
  expect_equal(unname(centrality_hybrid(flat)), rep(1, 3))
  uneven <- hybrid_from_edges(3L, el, w = c(0.9, 0.3, 0.3))
  score <- centrality_hybrid(uneven)
  expect_equal(unname(score[1]), unname(score[2]))
  expect_false(isTRUE(all.equal(unname(score[1]), unname(score[3]))))
  expect_equal(centrality_hybrid(uneven, weighted = FALSE),
               centrality_hybrid(flat))
})

test_that("hybrid centrality is between 0 and 1 and equivariant", {
  set.seed(62)
  n <- 9L
  a <- hybrid_from_edges(n, cbind(seq_len(n), c(2:n, 1L)))
  extra <- matrix(stats::runif(n * n) < 0.3, n, n)
  a[extra | t(extra)] <- 1
  diag(a) <- 0
  w <- matrix(round(stats::runif(n * n, 0.05, 1), 3), n, n)
  a <- a * pmax(w, t(w))
  score <- centrality_hybrid(a)
  expect_true(all(score >= 0 & score <= 1))
  perm <- sample.int(n)
  expect_equal(unname(centrality_hybrid(a[perm, perm])), unname(score)[perm])
  # A common factor on the weights changes no rank.
  expect_equal(centrality_hybrid(a * 7), score)
})

test_that("hybrid centrality is NA off its domain, with a classed warning", {
  two <- hybrid_from_edges(4L, rbind(c(1, 2), c(3, 4)))
  expect_warning(out <- centrality_hybrid(two),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  arcs <- matrix(0, 3L, 3L, dimnames = list(1:3, 1:3))
  arcs[rbind(c(1, 2), c(2, 3), c(3, 1))] <- 1
  expect_warning(out <- centrality_hybrid(arcs),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  lone <- matrix(0, 1L, 1L, dimnames = list("a", "a"))
  expect_warning(out <- centrality_hybrid(lone),
                 class = "cograph_undefined_measure")
  expect_true(is.na(out))
  row <- list_centralities()
  row <- row[row$measure == "hybrid", ]
  expect_identical(nrow(row), 1L)
  expect_true(row$uses_weights)
})
