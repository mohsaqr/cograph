# centrality() and its wrappers for the indices netrankr (Schoch 2017) builds from an indirect relation and an
# aggregation: total communicability (walks weighted by exp, summed), communicability betweenness (Estrada et al.
# 2009; netrankr's "depend_exp"), current-flow closeness (resistance distances; Brandes and Fleischer 2005) and
# random-walk closeness (hitting times; netrankr's "dist_rwalk"). netrankr works on undirected, unweighted igraph
# graphs; its relations are computed with igraph's distances and Laplacian or with base R's eigen().

undirected <- ref_connected_graph(12, density = 0.3, seed = 21L)
karate <- ref_karate()

test_that("total communicability equals netrankr's exp-weighted walks, summed", {
  skip_without_reference(c("netrankr", "igraph"))
  walks <- netrankr::indirect_relations(ref_igraph(karate), type = "walks", FUN = netrankr::walks_exp)
  # exp(A) by an eigendecomposition on both sides; entries run to about 1e5 on the karate club, so relative 1e-10.
  expect_reference(centrality_communicability(karate), ref_named(netrankr::aggregate_positions(walks, type = "sum"), karate),
                   reference = "netrankr", covers = c("centrality()", "centrality_communicability()"),
                   quantity = "communicability, karate", tolerance = 1e-10, measure = "relative")
})

test_that("communicability betweenness equals netrankr's exp dependencies, summed", {
  skip_without_reference(c("netrankr", "igraph"))
  dependencies <- netrankr::indirect_relations(ref_igraph(undirected), type = "depend_exp")
  # n deletions, each an eigendecomposition of an 11 x 11 matrix, against cograph's own: relative 1e-9.
  expect_reference(centrality_communicability_betweenness(undirected),
                   ref_named(netrankr::aggregate_positions(dependencies, type = "sum"), undirected),
                   reference = "netrankr", covers = c("centrality()", "centrality_communicability_betweenness()"),
                   quantity = "communicability betweenness, undirected", tolerance = 1e-9, measure = "relative")
})

test_that("current-flow closeness equals n - 1 over netrankr's resistance distances, summed", {
  skip_without_reference(c("netrankr", "igraph"))
  resistance <- netrankr::indirect_relations(ref_igraph(undirected), type = "dist_resist")
  # One inversion of L + J / n on each side: relative 1e-9.
  expect_reference(centrality_current_flow_closeness(undirected),
                   ref_named((nrow(undirected) - 1) * netrankr::aggregate_positions(resistance, type = "invsum"), undirected),
                   reference = "netrankr", covers = c("centrality()", "centrality_current_flow_closeness()"),
                   quantity = "current-flow closeness, undirected", tolerance = 1e-9, measure = "relative")
})

test_that("random-walk centrality equals netrankr's hitting times weighted by the stationary distribution", {
  skip_without_reference(c("netrankr", "igraph"))
  # netrankr's relation holds h(i -> j) in row j. Noh and Rieger (2004), Eq. 10-11 with Eq. 4 (pi_i = K_i / sum K):
  # 1 / C_j = tau_j / pi_j = sum_i pi_i h(i -> j), the passage time into j from the stationary start, so the score is
  # 1 / (hitting %*% pi), the degrees read from the graph.
  hitting <- netrankr::indirect_relations(ref_igraph(undirected), type = "dist_rwalk")
  stationary <- rowSums(undirected != 0) / sum(undirected != 0)
  # Passage times from an inverted Laplacian on one side and a fundamental matrix of the walk on the other: 1e-9.
  expect_reference(centrality_random_walk(undirected), ref_named(1 / as.vector(hitting %*% stationary), undirected),
                   reference = "netrankr", covers = c("centrality()", "centrality_random_walk()"),
                   quantity = "random walk, stationary-weighted passage times into the node, undirected",
                   tolerance = 1e-9, measure = "relative")
  expect_known_difference(centrality_random_walk(undirected),
                          ref_named(netrankr::aggregate_positions(hitting, type = "invsum"), undirected),
                          reference = "netrankr", covers = c("centrality()", "centrality_random_walk()"),
                          quantity = "random walk, passage times into the node (netrankr invsum)", tolerance = 1e-9,
                          measure = "relative",
                          reason = paste("netrankr's (and tidygraph's) random-walk closeness inverts the unweighted sum",
                                         "of the mean first passage times into the node, sum_i h(i -> j); cograph",
                                         "follows Noh and Rieger (2004) Eq. 10-11, 1 / sum_i pi_i h(i -> j), each",
                                         "source weighted by its stationary probability. The two are not",
                                         "proportional on a graph whose degrees differ."))
})
