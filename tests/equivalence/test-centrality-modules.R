# centrality() and its wrappers for the measures that read a partition of the nodes into modules (Guimera and Amaral
# 2005: participation coefficient and within-module degree z-score; Vargas and Wahl 2014: gateway coefficient),
# against brainGraph (Watson), and diversity (Eagle et al. 2010), the normalised entropy of a node's tie weights,
# against igraph. cograph computes all four natively.

undirected <- ref_connected_graph(12, density = 0.3, seed = 21L)
modules <- rep(c(1L, 2L, 3L), length.out = 12L)
karate <- ref_karate()
# Two modules of unequal size on Zachary's karate club, split at node 17.
karate_modules <- rep(c(1L, 2L), times = c(17L, 17L))
weighted <- ref_connected_graph(12, density = 0.3, seed = 25L, weighted = TRUE, digits = 6L)

# Sums of squared shares and one division: rounding of the last bits only.
exact <- 1e-10

test_that("participation coefficient equals brainGraph's part_coeff()", {
  skip_without_reference(c("brainGraph", "igraph"))
  expect_reference(centrality_participation(undirected, membership = modules),
                   ref_named(brainGraph::part_coeff(ref_igraph(undirected), modules), undirected), reference = "brainGraph",
                   covers = c("centrality()", "centrality_participation()"), quantity = "participation, three modules",
                   tolerance = exact)
  expect_reference(centrality_participation(karate, membership = karate_modules),
                   ref_named(brainGraph::part_coeff(ref_igraph(karate), karate_modules), karate), reference = "brainGraph",
                   covers = c("centrality()", "centrality_participation()"), quantity = "participation, karate, two modules",
                   tolerance = exact)
})

test_that("within-module degree z-score equals brainGraph's within_module_deg_z_score()", {
  skip_without_reference(c("brainGraph", "igraph"))
  expect_reference(centrality_within_module_z(undirected, membership = modules),
                   ref_named(brainGraph::within_module_deg_z_score(ref_igraph(undirected), modules), undirected),
                   reference = "brainGraph", covers = c("centrality()", "centrality_within_module_z()"),
                   quantity = "within-module z, three modules", tolerance = exact)
  expect_reference(centrality_within_module_z(karate, membership = karate_modules),
                   ref_named(brainGraph::within_module_deg_z_score(ref_igraph(karate), karate_modules), karate),
                   reference = "brainGraph", covers = c("centrality()", "centrality_within_module_z()"),
                   quantity = "within-module z, karate, two modules", tolerance = exact)
})

test_that("gateway coefficient equals brainGraph's gateway_coeff(centr = 'degree')", {
  skip_without_reference(c("brainGraph", "igraph"))
  # cograph weighs a node's links into a module by degree; brainGraph's default weighs them by betweenness, so the
  # degree variant is the one that is the same measure.
  expect_reference(centrality_gateway(undirected, membership = modules),
                   ref_named(brainGraph::gateway_coeff(ref_igraph(undirected), modules, centr = "degree"), undirected),
                   reference = "brainGraph", covers = c("centrality()", "centrality_gateway()"),
                   quantity = "gateway (degree), three modules", tolerance = exact)
  expect_reference(centrality_gateway(karate, membership = karate_modules),
                   ref_named(brainGraph::gateway_coeff(ref_igraph(karate), karate_modules, centr = "degree"), karate),
                   reference = "brainGraph", covers = c("centrality()", "centrality_gateway()"),
                   quantity = "gateway (degree), karate, two modules", tolerance = exact)
})

test_that("diversity equals igraph's diversity()", {
  skip_without_reference("igraph")
  expect_reference(centrality_diversity(weighted), igraph::diversity(ref_igraph(weighted)), reference = "igraph",
                   covers = c("centrality()", "centrality_diversity()"), quantity = "diversity, undirected weighted",
                   tolerance = exact)
})
