# Regression tests for the 2026-10 weight-handling bug fixes
# (bugs 1, 2, 8, 11, 12, 14, 15 of the 2026-10 help-page revision list).

# A directed weighted network with unequal weights and a binary copy of it.
.bugfix_weighted_net <- function() {
  w <- matrix(c(
    0,   2.5, 0,   1.2, 0,   0,
    0.4, 0,   3.1, 0,   0,   1.7,
    0,   0,   0,   2.2, 0.6, 0,
    1.9, 0,   0,   0,   2.8, 0,
    0,   1.1, 0,   0,   0,   0.9,
    2.4, 0,   0.7, 0,   1.5, 0
  ), 6, byrow = TRUE, dimnames = list(LETTERS[1:6], LETTERS[1:6]))
  list(w = w, b = (w != 0) * 1)
}

test_that("bug 1: weighted = FALSE gives the binary-adjacency result", {
  net <- .bugfix_weighted_net()
  measures <- c("strength", "closeness", "eccentricity", "harmonic",
                "alpha", "reaching_local", "expected_influence_1",
                "expected_influence_2", "betweenness", "eigenvector",
                "pagerank", "authority", "hub", "constraint",
                "current_flow_closeness", "current_flow_betweenness",
                "bridging", "diversity", "katz", "kreach", "hubbell",
                "diffusion")
  unweighted <- centrality(net$w, measures = measures, weighted = FALSE,
                           hubbell_weight = 0.1)
  binary <- centrality(net$b, measures = measures, hubbell_weight = 0.1)
  expect_equal(unweighted, binary)
  # The weights really change these measures, so the test can fail.
  weighted <- centrality(net$w, measures = measures, hubbell_weight = 0.1)
  differs <- vapply(setdiff(names(weighted), "node"),
                    \(col) !isTRUE(all.equal(weighted[[col]], binary[[col]])),
                    logical(1))
  expect_true(all(differs[c("strength_all", "closeness_all",
                            "betweenness", "pagerank", "katz")]))
})

test_that("bug 1: weighted = FALSE matches igraph's unweighted measures", {
  skip_if_not_installed("igraph")
  net <- .bugfix_weighted_net()
  g <- igraph::graph_from_adjacency_matrix(net$w, mode = "directed",
                                           weighted = TRUE)
  res <- centrality(net$w, measures = c("betweenness", "closeness",
                                        "strength"),
                    mode = "out", weighted = FALSE)
  expect_equal(res$betweenness,
               as.numeric(igraph::betweenness(g, weights = NA)))
  expect_equal(res$closeness_out,
               as.numeric(igraph::closeness(g, mode = "out", weights = NA)))
  expect_equal(res$strength_out,
               as.numeric(igraph::degree(g, mode = "out")))
})

test_that("bug 2: current-flow betweenness with weighted = FALSE is binary", {
  # Four-cycle: each node carries 1/4 of the current for the two adjacent
  # pairs routed round the long way and 1/2 for the opposite pair, total 1,
  # times the normalization 2 / ((n - 1)(n - 2)) = 1/3.
  w <- matrix(0, 4, 4)
  w[cbind(1:4, c(2:4, 1))] <- c(1, 2, 3, 4)
  w <- w + t(w)
  res <- centrality(w, measures = "current_flow_betweenness",
                    weighted = FALSE)
  expect_equal(res$current_flow_betweenness, rep(1 / 3, 4))
  # On the weighted cycle the scores are no longer equal.
  weighted <- centrality(w, measures = "current_flow_betweenness")
  expect_gt(diff(range(weighted$current_flow_betweenness)), 0.01)

  cf <- centrality(regulation_net, measures = "current_flow_betweenness",
                   weighted = FALSE)
  expect_true(all(cf$current_flow_betweenness <= 1))
})

test_that("bug 8: normalized harmonic divides by n - 1 only", {
  # Path 1-2-3-4: ends 1 + 1/2 + 1/3 = 11/6, middle 1 + 1 + 1/2 = 5/2.
  p <- matrix(0, 4, 4)
  p[cbind(1:3, 2:4)] <- 1
  p <- p + t(p)
  res <- centrality(p, measures = "harmonic", normalized = TRUE)
  expect_equal(res$harmonic_all, c(11 / 6, 5 / 2, 5 / 2, 11 / 6) / 3)
  expect_equal(centrality_harmonic(p, normalized = TRUE),
               c(`1` = 11 / 18, `2` = 5 / 6, `3` = 5 / 6, `4` = 11 / 18))
})

test_that("bug 11: a named personalized vector is matched to node names", {
  net <- .bugfix_weighted_net()
  p <- c(A = 5, B = 1, C = 0, D = 2, E = 0, F = 1)
  by_name <- centrality_pagerank(net$w, personalized = rev(p))
  by_position <- centrality_pagerank(net$w, personalized = unname(p))
  expect_equal(by_name, by_position)
  expect_false(isTRUE(all.equal(by_name,
    centrality_pagerank(net$w, personalized = unname(rev(p))))))
  expect_error(
    centrality_pagerank(net$w, personalized = c(p[-1], Z = 1)),
    class = "cograph_bad_input")
})

test_that("bug 11: personalized PageRank matches igraph", {
  skip_if_not_installed("igraph")
  net <- .bugfix_weighted_net()
  p <- c(A = 5, B = 1, C = 0, D = 2, E = 0, F = 1)
  g <- igraph::graph_from_adjacency_matrix(net$w, mode = "directed",
                                           weighted = TRUE)
  ref <- igraph::page_rank(g, personalized = unname(p))$vector
  expect_equal(unname(centrality_pagerank(net$w, personalized = rev(p))),
               as.numeric(ref), tolerance = 1e-8)
})

test_that("bug 12: percolation honours invert_weights", {
  net <- .bugfix_weighted_net()
  inverted <- net$w
  inverted[net$w > 0] <- 1 / net$w[net$w > 0]
  res <- suppressMessages(
    centrality(net$w, measures = "percolation", invert_weights = TRUE))
  ref <- centrality(inverted, measures = "percolation")
  expect_equal(res$percolation, ref$percolation)
  raw <- centrality(net$w, measures = "percolation")
  expect_false(isTRUE(all.equal(res$percolation, raw$percolation)))
})

test_that("bug 14: katz on a single node is 1", {
  # (I - alpha * 0)^-1 %*% 1 = 1.
  expect_equal(centrality(matrix(0, 1, 1), measures = "katz")$katz, 1)
  expect_equal(unname(centrality_katz(matrix(0, 1, 1))), 1)
})

test_that("bug 15: delta betweenness counts intermediaries, not weights", {
  # Path 1-2-3-4, delta = 1: pairs (1,3) and (2,4) have one intermediary
  # (weight 1), pair (1,4) two (weight 1/2), so nodes 2 and 3 score 1.5.
  p <- matrix(0, 4, 4)
  p[cbind(1:3, 2:4)] <- 1
  p <- p + t(p)
  expected <- c(0, 1.5, 1.5, 0)
  expect_equal(centrality(p, measures = "delta_betweenness")$delta_betweenness,
               expected)
  # All weights below one: the same geodesics, so the same scores.
  expect_equal(
    centrality(p * 0.1, measures = "delta_betweenness")$delta_betweenness,
    expected)
  # Weighted geodesic with a different edge count: the A-C edge (distance 5)
  # loses to A-B-C (distance 2), so B is the single intermediary.
  tri <- matrix(c(0, 1, 5,
                  1, 0, 1,
                  5, 1, 0), 3, byrow = TRUE)
  expect_equal(
    centrality(tri, measures = "delta_betweenness")$delta_betweenness,
    c(0, 1, 0))
})
