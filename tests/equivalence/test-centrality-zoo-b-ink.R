# Improved neighbors' k-core, centrality_ink() (Lin, Guo, Dong, Tang and Liu 2014, Physics Letters A
# 378(45):3279-3284), equation (1), p. 3280: S(i) = sum over the neighbours j of i of k_s(j)^alpha, k_s the core
# number, the node's own core number not counted, no normalisation. Compared with the sum written out over
# NetworkX's and igraph's core numbers and with closed forms. The paper's Fig. 1 prints S for 4 of its 18 nodes and
# labels no others, so its values cannot be recomputed from a graph; they enter as the constraint they place on the
# four nodes of the 3-core (each scores 9 from the other three, and the smallest printed value is 10).

independent <- "independent: closed forms and hand derivations from the definition"
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
# Not connected and sparser: an isolate and cores of order 0 to 3.
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()
# Whole-number core values and short sums; fractional powers agree to rounding.
exact <- 1e-10
# The exponents the paper names: 0 and 1 (p. 3280) and its four network optima (section 3.4).
exponents <- c(0, 0.7, 1, 2.4, 5)

test_that("ink equals the neighbour sum of NetworkX's core numbers raised to alpha", {
  graphs <- list(undirected = undirected, sparse = sparse, karate = karate)
  Map(\(graph, name) {
    lapply(exponents, \(alpha) {
      calls <- c(ink = sprintf("(lambda core: {v: sum(core[j] ** %s for j in G[v]) for v in G})(nx.core_number(G))",
                               format(alpha)))
      expect_reference(centrality_ink(graph, ink_alpha = alpha), ref_networkx(graph, calls)$ink,
                       reference = "networkx", covers = c("centrality()", "centrality_ink()"),
                       quantity = sprintf("ink, alpha %s, %s", format(alpha), name), tolerance = exact,
                       measure = "scaled")
    })
  }, graphs, names(graphs))
})

test_that("ink equals the adjacency matrix times igraph's coreness raised to alpha", {
  skip_without_reference("igraph")
  graphs <- list(undirected = undirected, sparse = sparse, karate = karate)
  Map(\(graph, name) {
    lapply(exponents, \(alpha) {
      core <- igraph::coreness(ref_igraph(graph))
      expect_reference(centrality_ink(graph, ink_alpha = alpha), ref_named(graph %*% core^alpha, graph),
                       reference = "igraph", covers = c("centrality()", "centrality_ink()"),
                       quantity = sprintf("ink, alpha %s, %s, igraph coreness", format(alpha), name),
                       tolerance = exact, measure = "scaled")
    })
  }, graphs, names(graphs))
})

test_that("ink follows the closed forms of complete graphs, stars, rings and paths", {
  lapply(c(0, 1, 2.4), \(alpha) {
    # K_n: every core number is n - 1, so S = (n - 1) (n - 1)^alpha.
    lapply(c(2L, 5L, 9L), \(n) {
      expect_reference(centrality_ink(zoob_complete(n), ink_alpha = alpha), rep((n - 1) * (n - 1)^alpha, n),
                       reference = independent, covers = c("centrality()", "centrality_ink()"),
                       quantity = sprintf("ink, alpha %s, complete graph n = %d", format(alpha), n), tolerance = exact)
    })
    # Trees: every core number is 1, so S is the degree whatever alpha.
    expect_reference(centrality_ink(zoob_star(6L), ink_alpha = alpha), c(6, rep(1, 6)), reference = independent,
                     covers = c("centrality()", "centrality_ink()"),
                     quantity = sprintf("ink, alpha %s, star of 6", format(alpha)), tolerance = exact)
    expect_reference(centrality_ink(zoob_path(5L), ink_alpha = alpha), c(1, 2, 2, 2, 1), reference = independent,
                     covers = c("centrality()", "centrality_ink()"),
                     quantity = sprintf("ink, alpha %s, path of 5", format(alpha)), tolerance = exact)
    # Ring: every core number is 2 and every node has two neighbours.
    expect_reference(centrality_ink(zoob_ring(7L), ink_alpha = alpha), rep(2 * 2^alpha, 7), reference = independent,
                     covers = c("centrality()", "centrality_ink()"),
                     quantity = sprintf("ink, alpha %s, ring n = 7", format(alpha)), tolerance = exact)
  })
})

test_that("ink leaves the node's own core number out, as the values printed for Fig. 1 require", {
  # K4 on nodes 1 to 4 with one pendant on node 2: nodes 1 to 4 are the whole 3-core, as in the paper's Fig. 1,
  # whose smallest printed score, S(2) = 10, is 9 from the three other core nodes plus one neighbour of core 1.
  # Counting node 2's own core number would give 13.
  core_and_pendant <- zoob_graph(5L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 2, 5))
  expect_reference(centrality_ink(core_and_pendant), c(`1` = 9, `2` = 10, `3` = 9, `4` = 9, `5` = 3),
                   reference = independent, covers = c("centrality()", "centrality_ink()"),
                   quantity = "ink, alpha 1, K4 with a pendant", tolerance = exact)
})

test_that("ink at alpha 0 is the degree", {
  skip_without_reference("igraph")
  expect_reference(centrality_ink(karate, ink_alpha = 0), ref_named(igraph::degree(ref_igraph(karate)), karate),
                   reference = "igraph", covers = c("centrality()", "centrality_ink()"),
                   quantity = "ink, alpha 0, karate, igraph degree", tolerance = exact)
})
