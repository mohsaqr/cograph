# centrality() and its wrappers for Burt's (1992) structural-hole measures against influenceR (Jacobs and Khanna):
# effective network size (Borgatti 1997's form for binary ties) and constraint. The bridging measures are checked
# against their definitions, because influenceR's bridging() is a different measure under the same name: Valente and
# Fujimoto's (2010) loss of cohesion when a node's ties are removed, where cograph's bridging centrality is Hwang et
# al.'s (2006) betweenness times the bridging coefficient.

undirected <- ref_connected_graph(12, density = 0.3, seed = 21L)
karate <- ref_karate()

exact <- 1e-10

test_that("effective size equals influenceR's ens()", {
  skip_without_reference(c("influenceR", "igraph"))
  expect_reference(centrality_effective_size(undirected), ref_named(influenceR::ens(ref_igraph(undirected)), undirected),
                   reference = "influenceR", covers = c("centrality()", "centrality_effective_size()"),
                   quantity = "effective size, undirected", tolerance = exact)
  expect_reference(centrality_effective_size(karate), ref_named(influenceR::ens(ref_igraph(karate)), karate),
                   reference = "influenceR", covers = c("centrality()", "centrality_effective_size()"),
                   quantity = "effective size, karate", tolerance = exact)
})

test_that("redundancy equals degree minus influenceR's effective size", {
  skip_without_reference(c("influenceR", "igraph"))
  # Burt's redundancy is the part of a node's degree that its effective size discounts.
  expect_reference(centrality_redundancy(karate), ref_named(rowSums(karate) - influenceR::ens(ref_igraph(karate)), karate),
                   reference = "influenceR", covers = c("centrality()", "centrality_redundancy()"),
                   quantity = "redundancy, karate", tolerance = exact)
})

# influenceR::constraint() is not compared: its sparse kernel (process_sparse.c) weighs the indirect share
# p_iq p_qj with 1 / deg_j where Burt's p_qj is 1 / deg_q, so it is not Burt's constraint. Seen in the first run
# (node N9 of the 12-node graph: influenceR 0.343876, Burt 0.361080) and reproduced by transcribing that kernel.
test_that("constraint equals Burt's definition", {
  expect_reference(centrality_constraint(undirected), ref_constraint(undirected),
                   reference = "independent: Burt's constraint from its definition, base R",
                   covers = c("centrality()", "centrality_constraint()"), quantity = "constraint, undirected",
                   tolerance = exact)
  expect_reference(centrality_constraint(karate), ref_constraint(karate),
                   reference = "independent: Burt's constraint from its definition, base R",
                   covers = c("centrality()", "centrality_constraint()"), quantity = "constraint, karate",
                   tolerance = exact)
})

test_that("the bridging measures equal their definitions (Hwang et al. 2006)", {
  skip_without_reference("sna")
  coefficient <- ref_bridging_coefficient(undirected)
  expect_reference(centrality_bridging_coefficient(undirected), coefficient,
                   reference = "independent: bridging coefficient, Hwang et al. 2006, base R",
                   covers = c("centrality()", "centrality_bridging_coefficient()"),
                   quantity = "bridging coefficient, undirected", tolerance = exact)
  expect_reference(centrality_local_bridging(undirected), coefficient / rowSums(undirected),
                   reference = "independent: bridging coefficient, Hwang et al. 2006, base R",
                   covers = c("centrality()", "centrality_local_bridging()"),
                   quantity = "local bridging, undirected", tolerance = exact)
  # Bridging centrality: betweenness (here from sna) times the bridging coefficient.
  expect_reference(centrality_bridging(undirected),
                   ref_named(sna::betweenness(undirected, gmode = "graph"), undirected) * coefficient,
                   reference = "independent: bridging coefficient, Hwang et al. 2006, base R",
                   covers = c("centrality()", "centrality_bridging()"),
                   quantity = "bridging centrality, undirected", tolerance = exact, measure = "scaled",
                   note = "betweenness from sna::betweenness(gmode = 'graph')")
})
