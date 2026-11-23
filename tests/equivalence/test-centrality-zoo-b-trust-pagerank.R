# Trust-PageRank, centrality_trust_pagerank() (Sheng, Zhu, Wang, Wang and Hou 2020, Algorithms 13(11):280,
# doi:10.3390/a13110280), against the rankings the paper prints (Table 5, p. 10: the top ten of Zachary's karate
# club and of Krackhardt's kite at alpha = k = 0.85) and against computations written out from equations (2)-(7),
# pp. 5-7: uniform scores where symmetry forces them, the degree-only trust matrix (k = 1) solved directly, and the
# teleport share (1 - alpha) / n of a node that receives no trust. No other package implements this measure.
#
# cograph iterates equation (7) until the largest relative change is below tpr_tol = 1e-14 (at most 1000 steps);
# with alpha = 0.85 the distance to the fixed point is then below 1e-14 * 0.85 / 0.15 relative, far inside the
# 1e-10 used here.

independent <- "independent: closed forms and hand derivations from the definition"
table_5 <- "published: Sheng, Zhu, Wang, Wang and Hou 2020, Table 5, p. 10"
iterated <- 1e-10
# A rank position is an integer: half a unit.
rank_half_unit <- 0.5

# Krackhardt's kite with the labels of the paper's Figure 5(a).
tpr_paper_kite <- function() {
  zoob_graph(10L, c(6, 10, 6, 5, 6, 7, 10, 5, 10, 7, 10, 9, 5, 7, 5, 4, 5, 3, 7, 9, 7, 4, 7, 8, 9, 4, 9, 8, 4, 8,
                    4, 3, 3, 2, 2, 1))
}

test_that("trust_pagerank reproduces Table 5's top ten of the karate club", {
  # Rank position of each node the table lists, 1 for the highest score.
  printed <- c(`1` = 1, `34` = 2, `33` = 3, `3` = 4, `2` = 5, `32` = 6, `4` = 7, `14` = 8, `9` = 9, `24` = 10)
  score <- centrality_trust_pagerank(zoob_karate())
  expect_reference(rank(-score)[names(printed)], printed, reference = table_5,
                   covers = c("centrality()", "centrality_trust_pagerank()"),
                   quantity = "trust_pagerank, rank position, karate club", tolerance = rank_half_unit)
})

test_that("trust_pagerank reproduces Table 5's top ten of the kite", {
  # The kite's automorphisms swap 4 with 5, 9 with 10 and 6 with 8, so each pair has equal scores and the table's
  # order within a pair is a tie-break. Each node of a tied pair is given the better of the pair's two printed
  # positions; cograph's ranks are taken with ties.method = "min" on scores rounded to 12 decimals.
  printed <- c(`7` = 1, `4` = 2, `5` = 2, `9` = 4, `10` = 4, `3` = 6, `6` = 7, `8` = 7, `2` = 9, `1` = 10)
  score <- centrality_trust_pagerank(tpr_paper_kite())
  expect_reference(rank(-round(score, 12), ties.method = "min")[names(printed)], printed, reference = table_5,
                   covers = c("centrality()", "centrality_trust_pagerank()"),
                   quantity = "trust_pagerank, rank position, kite", tolerance = rank_half_unit,
                   note = "chained: the table's positions within each tied pair are re-assigned in the test (the better of the two)")
})

test_that("trust_pagerank is uniform where symmetry forces it", {
  # K_n: every degree ratio and every similarity ratio is 1 / (n - 1), so T is uniform and the fixed point of
  # equation (7) is 1 / n. The octahedron K(2, 2, 2) is vertex-transitive and not complete.
  lapply(c(3L, 5L, 10L), \(n) {
    expect_reference(centrality_trust_pagerank(zoob_complete(n)), rep(1 / n, n), reference = independent,
                     covers = c("centrality()", "centrality_trust_pagerank()"),
                     quantity = sprintf("trust_pagerank, complete graph n = %d", n), tolerance = iterated)
  })
  octahedron <- zoob_graph(6L, c(1, 3, 1, 4, 1, 5, 1, 6, 2, 3, 2, 4, 2, 5, 2, 6, 3, 5, 3, 6, 4, 5, 4, 6))
  expect_reference(centrality_trust_pagerank(octahedron), rep(1 / 6, 6), reference = independent,
                   covers = c("centrality()", "centrality_trust_pagerank()"), quantity = "trust_pagerank, octahedron",
                   tolerance = iterated)
})

test_that("trust_pagerank with k = 1 equals the direct solve of equation (7)", {
  # At k = 1 the trust value is T(i, j) = d_i / sum_{l in N(j)} d_l (equation (5)), and equation (7)
  # PR = (1 - alpha) / n + alpha T PR is the linear system (I - alpha T) PR = (1 - alpha) / n.
  paw <- zoob_graph(4L, c(1, 2, 1, 3, 2, 3, 3, 4))
  degree <- rowSums(paw)
  trust <- paw * degree / matrix(as.numeric(paw %*% degree), 4, 4, byrow = TRUE)
  expect_reference(centrality_trust_pagerank(paw, tpr_k = 1), solve(diag(4) - 0.85 * trust, rep(0.15 / 4, 4)),
                   reference = independent, covers = c("centrality()", "centrality_trust_pagerank()"),
                   quantity = "trust_pagerank, k = 1, triangle with a pendant", tolerance = iterated)
})

test_that("trust_pagerank gives a node that receives no trust the teleport share", {
  # Equation (7) with an empty sum: (1 - alpha) / n, alpha = 0.85.
  with_isolate <- zoob_union(zoob_complete(3), matrix(0, 1, 1))
  expect_reference(centrality_trust_pagerank(with_isolate)["4"], c(`4` = 0.15 / 4), reference = independent,
                   covers = c("centrality()", "centrality_trust_pagerank()"),
                   quantity = "trust_pagerank, isolate beside a triangle", tolerance = iterated)
  expect_reference(centrality_trust_pagerank(matrix(0, 6, 6)), rep(0.15 / 6, 6), reference = independent,
                   covers = c("centrality()", "centrality_trust_pagerank()"),
                   quantity = "trust_pagerank, six isolates", tolerance = iterated)
  expect_reference(centrality_trust_pagerank(matrix(0, 1, 1)), 0.15, reference = independent,
                   covers = c("centrality()", "centrality_trust_pagerank()"), quantity = "trust_pagerank, one node",
                   tolerance = iterated)
})
