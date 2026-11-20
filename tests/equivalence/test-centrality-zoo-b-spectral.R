# Two spectral measures of the centrality batches 36 and 37, through their wrappers centrality_controlrank() and
# centrality_spectralrank() (each a call of centrality(measures = "<measure>")).
#
# ControlRank (Zhou, Yu and Lu 2019, Theorem 3): the least eigenvalue of the symmetric part of the Laplacian
# L = D - A (D the out-strengths) with the node's row and column deleted. It is compared with the eleven values
# printed for the paper's Figure 1, with that definition written out with base R's eigen(), and with closed forms
# on stars, cliques and rings.
#
# SpectralRank (Xu, Wang, Zhang and Lu 2019): the leading eigenvector of the graph augmented by a ground node,
# scaled so that the largest score is one when the ground's prior allows. It is compared with the closed forms
# that the characteristic polynomials of small and regular graphs give.

independent <- "independent: closed forms and hand derivations from the definition"

# ControlRank by its definition (Zhou, Yu and Lu 2019, Theorem 3).
controlrank_by_definition <- function(graph) {
  laplacian <- diag(rowSums(graph)) - graph
  symmetric <- (laplacian + t(laplacian)) / 2
  vapply(seq_len(nrow(graph)), \(i) {
    min(eigen(symmetric[-i, -i, drop = FALSE], symmetric = TRUE, only.values = TRUE)$values)
  }, numeric(1))
}

bistar <- zoob_graph(11L, c(1, 2, 1, 7, 2, 3, 2, 4, 2, 5, 2, 6, 7, 8, 7, 9, 7, 10, 7, 11))

test_that("ControlRank equals the values printed for Zhou, Yu and Lu's Figure 1", {
  # The figure prints two decimals: half a unit is 0.005 (absolute).
  expect_reference(centrality_controlrank(bistar), c(.17, .09, .05, .05, .05, .05, .09, .05, .05, .05, .05),
                   reference = "published: Zhou, Yu and Lu 2019, Fig. 1",
                   covers = c("centrality()", "centrality_controlrank()"),
                   quantity = "controlrank, bi-star of Figure 1", tolerance = 0.005)
  # The hub's grounded Laplacian has least eigenvalue 3 - sqrt(8) exactly (its characteristic polynomial).
  expect_reference(centrality_controlrank(bistar)[["1"]], 3 - sqrt(8), reference = independent,
                   covers = c("centrality()", "centrality_controlrank()"),
                   quantity = "controlrank, bi-star of Figure 1, node 1 closed form", tolerance = 1e-12)
})

test_that("ControlRank equals its definition computed with eigen()", {
  weighted_directed <- matrix(c(0, 2, 1, 1, 0, 3, 4, 1, 0), 3, 3, byrow = TRUE,
                              dimnames = list(c("1", "2", "3"), c("1", "2", "3")))
  chain <- zoob_graph(3L, c(1, 2, 2, 3), directed = TRUE)
  cases <- list("bi-star" = bistar, karate = zoob_karate(), kite = zoob_kite(),
                "weighted directed triangle" = weighted_directed, "directed chain 1->2->3" = chain,
                "directed chain 3->2->1" = t(chain))
  # Two eigen-solvers on the same symmetric matrices: a few units of rounding, scaled by the score.
  lapply(names(cases), \(name) expect_reference(centrality_controlrank(cases[[name]]),
                                                controlrank_by_definition(cases[[name]]), reference = independent,
                                                covers = c("centrality()", "centrality_controlrank()"),
                                                quantity = sprintf("controlrank, %s, eigen()", name),
                                                tolerance = 1e-10, measure = "scaled"))
  # The directed chain by hand: (1 - sqrt(2)) / 2, 0 and 1 / 2.
  expect_reference(centrality_controlrank(chain), c((1 - sqrt(2)) / 2, 0, 0.5), reference = independent,
                   covers = c("centrality()", "centrality_controlrank()"),
                   quantity = "controlrank, directed chain 1->2->3, by hand", tolerance = 1e-12)
  # The weighted directed triangle: each grounded 2 x 2 block has eigenvalues (t +- sqrt(t^2 - 4 det)) / 2.
  expect_reference(centrality_controlrank(weighted_directed, directed = TRUE),
                   c((9 - sqrt(17)) / 2, (8 - sqrt(29)) / 2, (7 - sqrt(10)) / 2), reference = independent,
                   covers = c("centrality()", "centrality_controlrank()"),
                   quantity = "controlrank, weighted directed triangle, by hand", tolerance = 1e-12)
})

test_that("ControlRank equals its closed forms on stars, cliques and rings", {
  # Star (hub 1): deleting the hub leaves the leaves' unit degrees (score 1); deleting a leaf leaves a star
  # whose least eigenvalue is 2 / (n + sqrt(n^2 - 4)). A clique minus a node has least eigenvalue 1; a ring
  # minus a node is a path Laplacian with two grounded ends, least eigenvalue 2 - 2 cos(pi / n).
  lapply(c(3L, 5L, 8L), \(n) {
    expect_reference(centrality_controlrank(zoob_star(n - 1L)), c(1, rep(2 / (n + sqrt(n^2 - 4)), n - 1L)),
                     reference = independent, covers = c("centrality()", "centrality_controlrank()"),
                     quantity = sprintf("controlrank, star on %d nodes", n), tolerance = 1e-12)
    expect_reference(centrality_controlrank(zoob_complete(n)), rep(1, n), reference = independent,
                     covers = c("centrality()", "centrality_controlrank()"),
                     quantity = sprintf("controlrank, clique on %d nodes", n), tolerance = 1e-12)
    expect_reference(centrality_controlrank(zoob_ring(n)), rep(2 - 2 * cos(pi / n), n), reference = independent,
                     covers = c("centrality()", "centrality_controlrank()"),
                     quantity = sprintf("controlrank, ring on %d nodes", n), tolerance = 1e-12)
  })
})

test_that("SpectralRank equals the closed forms of edgeless graphs and a single edge", {
  # Without edges only the ground links the nodes, and every node scores the root s of n s^2 - p s - 1 = 0,
  # (p + sqrt(p^2 + 4n)) / (2n) for a common prior p, capped at one once p reaches n - 1 (prior 0: 1 / sqrt(n)).
  cases <- expand.grid(n = c(1L, 3L, 16L), prior = c(0, 1, 3, 1e300))
  lapply(seq_len(nrow(cases)), \(k) {
    n <- cases$n[[k]]
    p <- cases$prior[[k]]
    expected <- if (p >= n - 1) 1 else (p + sqrt(p^2 + 4 * n)) / (2 * n)
    expect_reference(centrality_spectralrank(matrix(0, n, n), sr_prior = p), rep(expected, n),
                     reference = independent, covers = c("centrality()", "centrality_spectralrank()"),
                     quantity = sprintf("spectralrank, %d isolates, prior %g", n, p), tolerance = 1e-10)
  })
  expect_reference(centrality_spectralrank(zoob_path(2L)), c(1, 1), reference = independent,
                   covers = c("centrality()", "centrality_spectralrank()"), quantity = "spectralrank, one edge",
                   tolerance = 1e-10)
  expect_reference(centrality_spectralrank(zoob_path(2L), sr_prior = c(3, 0)), c(1, 1 / (1 + sqrt(3))),
                   reference = independent, covers = c("centrality()", "centrality_spectralrank()"),
                   quantity = "spectralrank, one edge, priors 3 and 0", tolerance = 1e-10)
  # One arc 1 -> 2: the augmented characteristic polynomial is lambda^3 - 2 lambda - 1, root the golden ratio;
  # scores are outgoing, so the source scores 1 and the target 1 / phi.
  arc <- zoob_graph(2L, c(1, 2), directed = TRUE)
  golden <- (1 + sqrt(5)) / 2
  expect_reference(centrality_spectralrank(arc), c(1, 1 / golden), reference = independent,
                   covers = c("centrality()", "centrality_spectralrank()"), quantity = "spectralrank, arc 1->2",
                   tolerance = 1e-10)
  expect_reference(centrality_spectralrank(t(arc)), c(1 / golden, 1), reference = independent,
                   covers = c("centrality()", "centrality_spectralrank()"), quantity = "spectralrank, arc 2->1",
                   tolerance = 1e-10)
})

test_that("SpectralRank equals the closed forms of rings and cliques", {
  # On a d-regular graph the ground class gives lambda^2 - d lambda - n = 0, and the score is min(1, lambda / n).
  lapply(c(3L, 5L, 8L), \(n) {
    expect_reference(centrality_spectralrank(zoob_ring(n)), rep(min(1, (2 + sqrt(4 + 4 * n)) / 2 / n), n),
                     reference = independent, covers = c("centrality()", "centrality_spectralrank()"),
                     quantity = sprintf("spectralrank, ring on %d nodes", n), tolerance = 1e-10)
    expect_reference(centrality_spectralrank(zoob_complete(n)), rep(1, n), reference = independent,
                     covers = c("centrality()", "centrality_spectralrank()"),
                     quantity = sprintf("spectralrank, clique on %d nodes", n), tolerance = 1e-10)
  })
})
