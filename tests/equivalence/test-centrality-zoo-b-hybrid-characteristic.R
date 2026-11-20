# centrality_hcc() and centrality_ehcc(): hybrid characteristic centrality and its extended form (Liu and Zheng
# 2023, Scientific Reports 13, 3197). Compared with the values the paper prints for its figure 1 graph, and with
# the definition written out below in base R.
#
# Definition (Liu and Zheng 2023, eqs. 3-5): the extended degree is kex(u) = delta k(u) + (1 - delta) sum of the
# neighbours' degrees. The E-shell decomposition removes, round by round, every remaining node of minimum
# extended degree, recomputed on the residual graph; pos(u) is the round in which u leaves. Then
# HCC(u) = kex(u) / max kex + pos(u) / max pos, with kex on the whole graph (0/0 written 0 on an edgeless graph),
# and EHCC(u) = HCC(u) + the sum of the neighbours' HCC.

hcc_definition <- function(graph, delta = 0.5) {
  extended <- \(sub) {
    degree <- rowSums(sub)
    delta * degree + (1 - delta) * as.numeric(sub %*% degree)
  }
  peel <- function(alive, position, round) {
    if (!any(alive)) return(position)
    residual <- extended(graph[alive, alive, drop = FALSE])
    leaving <- which(alive)[abs(residual - min(residual)) < 1e-9]
    peel(replace(alive, leaving, FALSE), replace(position, leaving, round), round + 1)
  }
  kex <- extended(graph)
  position <- peel(rep(TRUE, nrow(graph)), numeric(nrow(graph)), 1)
  hcc <- (if (max(kex) > 0) kex / max(kex) else kex) + position / max(position)
  list(hcc = stats::setNames(hcc, rownames(graph)),
       ehcc = stats::setNames(hcc + as.numeric(graph %*% hcc), rownames(graph)))
}

# Liu and Zheng 2023, figure 1: ten nodes a to j, fourteen edges.
liu_figure1 <- function() {
  pairs <- c("c", "d", "a", "b", "a", "e", "d", "f", "d", "e", "d", "g", "e", "f", "f", "g", "f", "i", "b", "e",
             "e", "g", "g", "i", "g", "h", "i", "j")
  zoob_graph(10L, match(pairs, letters), labels = letters[1:10])
}

independent <- "independent: closed forms and hand derivations from the definition"
exact <- 1e-10

test_that("HCC and EHCC equal Liu and Zheng (2023) Table 3", {
  graph <- liu_figure1()
  # Table 3 prints two decimals: half a unit in the last place is 0.005, absolute.
  expect_reference(centrality_hcc(graph),
                   stats::setNames(c(1.08, 1.08, 0.56, 1.86, 2, 1.95, 2, 0.77, 1.42, 0.35), letters[1:10]),
                   reference = "published: Liu and Zheng 2023, Table 3", covers = c("centrality()", "centrality_hcc()"),
                   quantity = "hcc, figure 1", tolerance = 0.005)
  ehcc <- centrality_ehcc(graph)
  printed <- stats::setNames(c(4.15, 4.15, 2.42, 8.38, 9.97, 9.24, 10.01, 2.77, 5.73, 1.77), letters[1:10])
  expect_reference(ehcc[setdiff(letters[1:10], "g")], printed[setdiff(letters[1:10], "g")],
                   reference = "published: Liu and Zheng 2023, Table 3", covers = c("centrality()", "centrality_ehcc()"),
                   quantity = "ehcc, figure 1, nodes other than g", tolerance = 0.005)
  expect_known_difference(ehcc["g"], printed["g"], reference = "published: Liu and Zheng 2023, Table 3",
                          covers = c("centrality()", "centrality_ehcc()"), quantity = "ehcc, figure 1, node g",
                          tolerance = 0.005,
                          reason = paste("The exact EHCC(g) is 661/66 = 10.01515, which rounds to 10.02; the table",
                                         "prints the truncation 10.01. Six other printed cells need rounding, not",
                                         "truncation, so no one convention gives all twenty: a misprint in the table."))
})

test_that("HCC equals the worked line Liu and Zheng (2023) print on p. 4", {
  # The paper writes HCC(a) = 4.5/11 + 4/6: an exact expression, so the tolerance is that of exact arithmetic.
  expect_reference(centrality_hcc(liu_figure1())["a"], c(a = 4.5 / 11 + 4 / 6),
                   reference = "published: Liu and Zheng 2023, p. 4", covers = c("centrality()", "centrality_hcc()"),
                   quantity = "hcc, figure 1, node a", tolerance = exact)
})

test_that("HCC and EHCC equal the definition written out", {
  graphs <- list(`figure 1 of Liu 2023` = liu_figure1(), kite = zoob_kite(), karate = zoob_karate(),
                 `path of 5 with an isolate` = zoob_union(zoob_path(5L), zoob_graph(1L, matrix(numeric(), 0, 2))),
                 `star of 5 leaves` = zoob_star(5L))
  Map(\(graph, name) {
    reference <- hcc_definition(graph)
    expect_reference(centrality_hcc(graph), reference$hcc, reference = independent,
                     covers = c("centrality()", "centrality_hcc()"), quantity = paste0("hcc, definition, ", name),
                     tolerance = exact)
    expect_reference(centrality_ehcc(graph), reference$ehcc, reference = independent,
                     covers = c("centrality()", "centrality_ehcc()"), quantity = paste0("ehcc, definition, ", name),
                     tolerance = exact)
  }, graphs, names(graphs))
  delta <- 0.25
  star <- zoob_star(4L)
  expect_reference(centrality_hcc(star, hcc_delta = delta), hcc_definition(star, delta)$hcc, reference = independent,
                   covers = c("centrality()", "centrality_hcc()"), quantity = "hcc, definition, star of 4 leaves, delta 0.25",
                   tolerance = exact)
})

test_that("HCC and EHCC equal their closed forms", {
  # A regular graph peels in one round: HCC = 1 + 1 = 2 and EHCC = 2 (1 + degree).
  expect_reference(centrality_hcc(zoob_ring(6L)), rep(2, 6), reference = independent,
                   covers = c("centrality()", "centrality_hcc()"), quantity = "hcc, ring of 6", tolerance = exact)
  expect_reference(centrality_ehcc(zoob_complete(5L)), rep(10, 5), reference = independent,
                   covers = c("centrality()", "centrality_ehcc()"), quantity = "ehcc, complete graph of 5", tolerance = exact)
  # A q-star with delta > 0 sheds its leaves in round one and the centre in round two; kex(leaf) = delta +
  # (1 - delta) q against kex(centre) = q, so a leaf scores (delta + (1 - delta) q) / q + 1/2.
  leaf <- (0.5 + 0.5 * 5) / 5 + 1 / 2
  expect_reference(centrality_hcc(zoob_star(5L)), c(2, rep(leaf, 5)), reference = independent,
                   covers = c("centrality()", "centrality_hcc()"), quantity = "hcc, star of 5 leaves", tolerance = exact)
  expect_reference(centrality_ehcc(zoob_star(5L)), c(2 + 5 * leaf, rep(leaf + 2, 5)), reference = independent,
                   covers = c("centrality()", "centrality_ehcc()"), quantity = "ehcc, star of 5 leaves", tolerance = exact)
  # The path of five: kex 1.5, 2.5, 3, 2.5, 1.5 and pos 1, 2, 3, 2, 1.
  expect_reference(centrality_hcc(zoob_path(5L)), c(1.5 / 3 + 1 / 3, 2.5 / 3 + 2 / 3, 2, 2.5 / 3 + 2 / 3, 1.5 / 3 + 1 / 3),
                   reference = independent, covers = c("centrality()", "centrality_hcc()"), quantity = "hcc, path of 5",
                   tolerance = exact)
})
