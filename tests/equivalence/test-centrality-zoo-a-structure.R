# centrality() and its wrappers for the Centrality Zoo measures that combine degree, clustering, cores and
# distances: the clustering degree algorithm (CDA), improved closeness, exogenous centrality and the global
# structure models (GSM, hybrid GSM, improved GSM). Each is compared with its definition written in base R
# (helper-zoo-a.R), and improved closeness at alpha = 0 with igraph's closeness() (cograph computes it
# natively).

defined <- "independent: the measure's definition written in base R (helper-zoo-a.R)"
exact <- 1e-10

star5 <- za_star(5)
karate <- ref_karate()
connected <- ref_connected_graph(8, density = 0.3, seed = 6L)
weighted <- ref_connected_graph(9, density = 0.35, seed = 5L, weighted = TRUE)
triangle <- matrix(c(0, 1, 3, 1, 0, 2, 3, 2, 0), 3, 3, dimnames = list(c("C", "A", "B"), c("C", "A", "B")))

test_that("CDA equals the clustering degree plus its weighted neighbours (Wang et al. 2018, eqs. 2-6)", {
  expect_reference(centrality_cda(triangle), za_cda(triangle), reference = defined, covers = c("centrality()", "centrality_cda()"),
                   quantity = "cda, alpha 0.5, weighted triangle", tolerance = exact)
  expect_reference(centrality_cda(weighted), za_cda(weighted), reference = defined, covers = c("centrality()", "centrality_cda()"),
                   quantity = "cda, alpha 0.5, weighted random graph", tolerance = exact, measure = "scaled")
  expect_reference(centrality_cda(weighted, cda_alpha = 0.25), za_cda(weighted, 0.25), reference = defined,
                   covers = c("centrality()", "centrality_cda()"), quantity = "cda, alpha 0.25, weighted random graph", tolerance = exact,
                   measure = "scaled")
  expect_reference(centrality_cda(karate), za_cda(karate), reference = defined, covers = c("centrality()", "centrality_cda()"),
                   quantity = "cda, karate", tolerance = exact, measure = "scaled")
  # By hand on the triangle (strengths 4, 3, 5; every clustering 1; w_max 3): CD = (3, 2.5, 3.5) / (1 + e^-1),
  # and C gets 3 + 2.5 / 3 + 3.5 = 22/3 of it.
  expect_reference(centrality_cda(triangle), c(C = 22 / 3, A = 35 / 6, B = 49 / 6) * stats::plogis(1),
                   reference = defined, covers = c("centrality()", "centrality_cda()"), quantity = "cda, alpha 0.5, weighted triangle, by hand",
                   tolerance = exact)
})

test_that("improved closeness equals (n - 1) / sum d / sigma^alpha (Luan et al. 2021)", {
  expect_reference(centrality_improved_closeness(karate), za_improved_closeness(karate), reference = defined,
                   covers = c("centrality()", "centrality_improved_closeness()"), quantity = "improved_closeness, alpha 0.2, karate", tolerance = exact)
  expect_reference(centrality_improved_closeness(karate, icc_alpha = 1), za_improved_closeness(karate, 1),
                   reference = defined, covers = c("centrality()", "centrality_improved_closeness()"), quantity = "improved_closeness, alpha 1, karate",
                   tolerance = exact)
  # The complete bipartite graph K(3, 3) at alpha 1: three neighbours at 1, two nodes at 2 by three paths each.
  bipartite <- za_graph(c(1, 4, 1, 5, 1, 6, 2, 4, 2, 5, 2, 6, 3, 4, 3, 5, 3, 6), 6)
  expect_reference(centrality_improved_closeness(bipartite, icc_alpha = 1), za_improved_closeness(bipartite, 1),
                   reference = defined, covers = c("centrality()", "centrality_improved_closeness()"), quantity = "improved_closeness, alpha 1, K(3, 3)",
                   tolerance = exact)
  expect_reference(centrality_improved_closeness(bipartite, icc_alpha = 1), rep(5 / (3 + 2 * 2 / 3), 6),
                   reference = defined, covers = c("centrality()", "centrality_improved_closeness()"), quantity = "improved_closeness, alpha 1, K(3, 3), by hand",
                   tolerance = exact)
})

test_that("improved closeness at alpha = 0 equals igraph's normalised closeness", {
  skip_without_reference("igraph")
  expect_reference(centrality_improved_closeness(karate, icc_alpha = 0),
                   igraph::closeness(ref_igraph(karate), normalized = TRUE), reference = "igraph",
                   covers = c("centrality()", "centrality_improved_closeness()"),
                   quantity = "improved_closeness, alpha 0, karate", tolerance = exact)
})

test_that("exogenous centrality is the drop in the others' base centrality on deletion (Everett & Borgatti 2010)", {
  chain <- za_graph(c(1, 2, 2, 3), 3, directed = TRUE)
  expect_reference(centrality_exogenous(star5), za_exogenous(star5), reference = defined, covers = c("centrality()", "centrality_exogenous()"),
                   quantity = "exogenous, reverse closeness, star of 5", tolerance = exact)
  expect_reference(centrality_exogenous(connected), za_exogenous(connected), reference = defined, covers = c("centrality()", "centrality_exogenous()"),
                   quantity = "exogenous, reverse closeness, connected random graph", tolerance = exact)
  expect_reference(centrality_exogenous(connected, exogenous_base = "betweenness"), za_exogenous(connected, "betweenness"),
                   reference = defined, covers = c("centrality()", "centrality_exogenous()"), quantity = "exogenous, betweenness, connected random graph",
                   tolerance = exact)
  expect_reference(centrality_exogenous(connected, exogenous_base = "degree"), za_exogenous(connected, "degree"),
                   reference = defined, covers = c("centrality()", "centrality_exogenous()"), quantity = "exogenous, degree, connected random graph",
                   tolerance = exact)
  expect_reference(centrality_exogenous(chain, mode = "out"), za_exogenous(chain, "reverse_closeness", "out"),
                   reference = defined, covers = c("centrality()", "centrality_exogenous()"), quantity = "exogenous, reverse closeness, mode out, directed chain",
                   tolerance = exact)
  expect_reference(centrality_exogenous(chain, mode = "in", exogenous_base = "betweenness"),
                   za_exogenous(chain, "betweenness", "in"), reference = defined, covers = c("centrality()", "centrality_exogenous()"),
                   quantity = "exogenous, betweenness, mode in, directed chain", tolerance = exact)
  expect_reference(centrality_exogenous(chain, mode = "out", exogenous_base = "degree"),
                   za_exogenous(chain, "degree", "out"), reference = defined, covers = c("centrality()", "centrality_exogenous()"),
                   quantity = "exogenous, degree, mode out, directed chain", tolerance = exact)
  # By hand on the star (N = 5): deleting the hub leaves four isolates, each losing 4 + 3 * 3 = 13 of reverse
  # closeness (52 in all); deleting a leaf costs the hub 4 and each other leaf 3 (13).
  expect_reference(centrality_exogenous(star5), c(A = 52, B = 13, C = 13, D = 13, E = 13), reference = defined,
                   covers = c("centrality()", "centrality_exogenous()"), quantity = "exogenous, reverse closeness, star of 5, by hand", tolerance = exact)
})

test_that("the global structure models equal their definitions", {
  # Ullah et al. (2021): exp(k_s(i) / N) sum_j k_s(j) / d_ij.
  expect_reference(centrality_global_structure(star5), za_global_structure(star5), reference = defined, covers = c("centrality()", "centrality_global_structure()"),
                   quantity = "global_structure, star of 5", tolerance = exact)
  expect_reference(centrality_global_structure(karate), za_global_structure(karate), reference = defined, covers = c("centrality()", "centrality_global_structure()"),
                   quantity = "global_structure, karate", tolerance = exact, measure = "scaled")
  edge_isolate <- za_graph(c(1, 2), 3)
  expect_reference(centrality_global_structure(edge_isolate), za_global_structure(edge_isolate), reference = defined,
                   covers = c("centrality()", "centrality_global_structure()"), quantity = "global_structure, one edge and an isolate", tolerance = exact)
  # Mukhtar et al. (2023): s_i = exp(k_s(i) k_i / N), a = ceiling(log2 mean s), s_i sum_j s_j / d_ij^a.
  expect_reference(centrality_hybrid_global_structure(star5), za_hybrid_global_structure(star5), reference = defined,
                   covers = c("centrality()", "centrality_hybrid_global_structure()"), quantity = "hybrid_global_structure, star of 5", tolerance = exact)
  expect_reference(centrality_hybrid_global_structure(connected), za_hybrid_global_structure(connected),
                   reference = defined, covers = c("centrality()", "centrality_hybrid_global_structure()"), quantity = "hybrid_global_structure, connected random graph",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_hybrid_global_structure(edge_isolate), za_hybrid_global_structure(edge_isolate),
                   reference = defined, covers = c("centrality()", "centrality_hybrid_global_structure()"), quantity = "hybrid_global_structure, one edge and an isolate",
                   tolerance = exact)
  # Zhu & Wang (2022): exp(k_i / N) sum_j k_j / d_ij^a, a = ceiling(log2 mean degree); a sparse graph of
  # mean degree 0.4 has a = -1, so distant nodes weigh more.
  sparse <- za_union(za_path(3), matrix(0, 7, 7))
  expect_reference(centrality_improved_global_structure(star5), za_improved_global_structure(star5),
                   reference = defined, covers = c("centrality()", "centrality_improved_global_structure()"), quantity = "improved_global_structure, star of 5",
                   tolerance = exact)
  expect_reference(centrality_improved_global_structure(karate), za_improved_global_structure(karate),
                   reference = defined, covers = c("centrality()", "centrality_improved_global_structure()"), quantity = "improved_global_structure, karate",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_improved_global_structure(sparse), za_improved_global_structure(sparse),
                   reference = defined, covers = c("centrality()", "centrality_improved_global_structure()"), quantity = "improved_global_structure, mean degree 0.4",
                   tolerance = exact)
  # By hand on the star (cores 1, N = 5): the hub sees four cores at distance 1, a leaf one at 1 and three at 2.
  expect_reference(centrality_global_structure(star5), exp(1 / 5) * c(4, 2.5, 2.5, 2.5, 2.5), reference = defined,
                   covers = c("centrality()", "centrality_global_structure()"), quantity = "global_structure, star of 5, by hand", tolerance = exact)
})
