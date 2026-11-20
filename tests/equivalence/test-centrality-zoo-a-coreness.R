# centrality() and its wrappers for the Centrality Zoo measures built on cores, trusses, cliques and
# neighbourhood counts: s-shell, weighted k-shell, s-core, renewed coreness, extended coreness, truss number,
# mixed-degree decomposition, the social-capital counts (godfather, support, bridging coefficient), volume,
# maximal clique centrality, LocalRank (semilocal) and Malatya centrality. The core-based measures that reduce
# to the k-core are compared with igraph's coreness(); every measure with its definition written in base R
# (helper-zoo-a.R: cores by peeling, cliques and decompositions by enumerating node subsets).

defined <- "independent: the measure's definition written in base R (helper-zoo-a.R)"
exact <- 1e-10

star5 <- za_star(5)
path5 <- za_path(5)
bridge6 <- za_bridge6()
karate <- ref_karate()
connected <- ref_connected_graph(10, density = 0.3, seed = 21L)
sparse <- ref_graph(12, density = 0.25, seed = 5L)
scattered <- ref_graph(14, density = 0.2, seed = 30L)
small <- ref_graph(9, density = 0.4, seed = 3L)

test_that("s-shell with a = 0, weighted k-shell with unit weights and the s-core reduce to igraph's coreness()", {
  skip_without_reference("igraph")
  # Liu et al. (2015): with a = 0 every link weighs 1 and the s-shells are the k-shells, numbered from 1 in
  # increasing order (the dense rank of the core number). Wei et al. (2015) and Eidsaa & Almaas (2013): with
  # unit weights the weighted k-shell and the s-core are the k-core numbers.
  dense_rank <- \(graph) stats::setNames(as.integer(factor(igraph::coreness(ref_igraph(graph)))), rownames(graph))
  expect_reference(centrality_s_shell(sparse, s_shell_a = 0), dense_rank(sparse), reference = "igraph",
                   covers = c("centrality()", "centrality_s_shell()"), quantity = "s_shell, a = 0, sparse random graph",
                   tolerance = 0)
  expect_reference(centrality_s_shell(scattered, s_shell_a = 0), dense_rank(scattered), reference = "igraph",
                   covers = c("centrality()", "centrality_s_shell()"),
                   quantity = "s_shell, a = 0, graph with isolates", tolerance = 0)
  expect_reference(centrality_weighted_kshell(sparse), igraph::coreness(ref_igraph(sparse)), reference = "igraph",
                   covers = c("centrality()", "centrality_weighted_kshell()"),
                   quantity = "weighted_kshell, unit weights, sparse random graph", tolerance = 0)
  expect_reference(centrality_weighted_kshell(scattered), igraph::coreness(ref_igraph(scattered)), reference = "igraph",
                   covers = c("centrality()", "centrality_weighted_kshell()"),
                   quantity = "weighted_kshell, unit weights, graph with isolates", tolerance = 0)
  expect_reference(centrality_s_core(karate), igraph::coreness(ref_igraph(karate)), reference = "igraph",
                   covers = c("centrality()", "centrality_s_core()"), quantity = "s_core, unit weights, karate",
                   tolerance = 0)
})

test_that("the s-core and renewed coreness equal their definitions", {
  # A triangle of weight-5 edges with a pendant D of weight 1: D (strength 1) leaves first, then each
  # triangle node keeps strength 10.
  pendant <- za_graph(c(1, 2, 1, 3, 2, 3, 3, 4), 4, weights = c(5, 5, 5, 1), names = LETTERS[1:4])
  expect_reference(centrality_s_core(pendant), za_s_core(pendant), reference = defined, covers = c("centrality()", "centrality_s_core()"),
                   quantity = "s_core, weighted triangle with a pendant", tolerance = exact)
  expect_reference(centrality_s_core(pendant), c(A = 10, B = 10, C = 10, D = 1), reference = defined, covers = c("centrality()", "centrality_s_core()"),
                   quantity = "s_core, weighted triangle with a pendant, by hand", tolerance = exact)
  weighted <- ref_connected_graph(9, density = 0.35, seed = 22L, weighted = TRUE)
  expect_reference(centrality_s_core(weighted), za_s_core(weighted), reference = defined, covers = c("centrality()", "centrality_s_core()"),
                   quantity = "s_core, weighted random graph", tolerance = exact)
  edge_and_isolate <- za_graph(c(1, 2), 3, names = LETTERS[1:3])
  expect_reference(centrality_s_core(edge_and_isolate), za_s_core(edge_and_isolate), reference = defined,
                   covers = c("centrality()", "centrality_s_core()"), quantity = "s_core, one edge and an isolate", tolerance = exact)
  # Liu et al. (2015): on two triangles only the bridge C-D leads somewhere new (importance 2); the core of
  # what is left is 1 at C and D, 0 elsewhere.
  expect_reference(centrality_renewed_coreness(bridge6), za_renewed_coreness(bridge6), reference = defined,
                   covers = c("centrality()", "centrality_renewed_coreness()"), quantity = "renewed_coreness, two triangles", tolerance = exact)
  expect_reference(centrality_renewed_coreness(za_complete(4)), za_renewed_coreness(za_complete(4)),
                   reference = defined, covers = c("centrality()", "centrality_renewed_coreness()"), quantity = "renewed_coreness, complete graph of 4",
                   tolerance = exact)
  expect_reference(centrality_renewed_coreness(karate), za_renewed_coreness(karate), reference = defined,
                   covers = c("centrality()", "centrality_renewed_coreness()"), quantity = "renewed_coreness, karate", tolerance = exact)
})

test_that("truss numbers and the social-capital counts equal their definitions", {
  diamond <- za_graph(c(1, 2, 1, 3, 2, 3, 1, 4, 2, 4), 4)
  # A cycle has no triangle (truss 2), K4 is a 4-truss, two triangles sharing an edge are a 3-truss only.
  expect_reference(centrality_truss(za_ring(4)), za_truss(za_ring(4)), reference = defined, covers = c("centrality()", "centrality_truss()"),
                   quantity = "truss, cycle of 4", tolerance = 0)
  expect_reference(centrality_truss(za_complete(4)), za_truss(za_complete(4)), reference = defined, covers = c("centrality()", "centrality_truss()"),
                   quantity = "truss, complete graph of 4", tolerance = 0)
  expect_reference(centrality_truss(diamond), za_truss(diamond), reference = defined, covers = c("centrality()", "centrality_truss()"),
                   quantity = "truss, two triangles sharing an edge", tolerance = 0)
  expect_reference(centrality_truss(karate), za_truss(karate), reference = defined, covers = c("centrality()", "centrality_truss()"),
                   quantity = "truss, karate", tolerance = 0)
  expect_reference(centrality_godfather(star5), za_godfather(star5), reference = defined,
                   covers = c("centrality()", "centrality_godfather()"), quantity = "godfather, star of 5", tolerance = 0)
  expect_reference(centrality_godfather(karate), za_godfather(karate), reference = defined,
                   covers = c("centrality()", "centrality_godfather()"), quantity = "godfather, karate", tolerance = 0)
  expect_reference(centrality_support(za_complete(4)), za_support(za_complete(4)), reference = defined,
                   covers = c("centrality()", "centrality_support()"), quantity = "support, complete graph of 4",
                   tolerance = 0)
  expect_reference(centrality_support(karate), za_support(karate), reference = defined,
                   covers = c("centrality()", "centrality_support()"), quantity = "support, karate", tolerance = 0)
  expect_reference(centrality_bridging_coefficient(star5), za_bridging_coefficient(star5), reference = defined,
                   covers = c("centrality()", "centrality_bridging_coefficient()"),
                   quantity = "bridging_coefficient, star of 5", tolerance = exact)
  expect_reference(centrality_bridging_coefficient(karate), za_bridging_coefficient(karate), reference = defined,
                   covers = c("centrality()", "centrality_bridging_coefficient()"),
                   quantity = "bridging_coefficient, karate", tolerance = exact)
  # By hand on the star: the hub has 6 unlinked leaf pairs; bridging (1/4) / (4 * 1) at the hub, 1 / (1/4) at
  # a leaf.
  expect_reference(centrality_godfather(star5), c(A = 6, B = 0, C = 0, D = 0, E = 0), reference = defined,
                   covers = c("centrality()", "centrality_godfather()"), quantity = "godfather, star of 5, by hand",
                   tolerance = 0)
  expect_reference(centrality_bridging_coefficient(star5), c(A = 1 / 16, B = 4, C = 4, D = 4, E = 4),
                   reference = defined, covers = c("centrality()", "centrality_bridging_coefficient()"),
                   quantity = "bridging_coefficient, star of 5, by hand", tolerance = exact)
})

test_that("mixed-degree decomposition equals the best threshold over node subsets", {
  expect_reference(centrality_mdd(star5), za_mdd(star5), reference = defined, covers = c("centrality()", "centrality_mdd()"),
                   quantity = "mdd, lambda 0.7, star of 5", tolerance = exact)
  expect_reference(centrality_mdd(star5, mdd_lambda = 0), za_mdd(star5, 0), reference = defined, covers = c("centrality()", "centrality_mdd()"),
                   quantity = "mdd, lambda 0, star of 5", tolerance = exact)
  expect_reference(centrality_mdd(star5, mdd_lambda = 1), za_mdd(star5, 1), reference = defined, covers = c("centrality()", "centrality_mdd()"),
                   quantity = "mdd, lambda 1, star of 5", tolerance = exact)
  expect_reference(centrality_mdd(path5, mdd_lambda = 0.5), za_mdd(path5, 0.5), reference = defined, covers = c("centrality()", "centrality_mdd()"),
                   quantity = "mdd, lambda 0.5, path of 5", tolerance = exact)
  expect_reference(centrality_mdd(bridge6), za_mdd(bridge6), reference = defined, covers = c("centrality()", "centrality_mdd()"),
                   quantity = "mdd, lambda 0.7, two triangles", tolerance = exact)
  expect_reference(centrality_mdd(small), za_mdd(small), reference = defined, covers = c("centrality()", "centrality_mdd()"),
                   quantity = "mdd, lambda 0.7, random graph of 9", tolerance = exact)
  # By hand on the star: the hub leaves after its four leaves, at 0 + 0.7 * 4.
  expect_reference(centrality_mdd(star5), c(A = 2.8, B = 1, C = 1, D = 1, E = 1), reference = defined, covers = c("centrality()", "centrality_mdd()"),
                   quantity = "mdd, lambda 0.7, star of 5, by hand", tolerance = exact)
})

test_that("volume and maximal clique centrality equal their definitions", {
  expect_reference(centrality_volume(path5, volume_radius = 0), za_volume(path5, 0), reference = defined,
                   covers = c("centrality()", "centrality_volume()"), quantity = "volume, radius 0, path of 5", tolerance = 0)
  expect_reference(centrality_volume(path5, volume_radius = 1), za_volume(path5, 1), reference = defined,
                   covers = c("centrality()", "centrality_volume()"), quantity = "volume, radius 1, path of 5", tolerance = 0)
  expect_reference(centrality_volume(path5), za_volume(path5), reference = defined, covers = c("centrality()", "centrality_volume()"),
                   quantity = "volume, radius 2, path of 5", tolerance = 0)
  expect_reference(centrality_volume(karate), za_volume(karate), reference = defined, covers = c("centrality()", "centrality_volume()"),
                   quantity = "volume, radius 2, karate", tolerance = 0)
  pieces <- za_union(za_path(5), za_complete(3), matrix(0, 1, 1))
  expect_reference(centrality_volume(pieces, volume_radius = Inf), za_volume(pieces, Inf), reference = defined,
                   covers = c("centrality()", "centrality_volume()"), quantity = "volume, infinite radius, path, triangle and isolate", tolerance = 0)
  # K4 on 1:4, a triangle 1-4-5, the pendant edge 5-6 and the isolate 7.
  cliques <- za_graph(c(as.vector(utils::combn(1:4, 2)), 1, 5, 4, 5, 5, 6), 7)
  expect_reference(centrality_mcc(cliques), za_mcc(cliques), reference = defined, covers = c("centrality()", "centrality_mcc()"),
                   quantity = "mcc, K4, triangle, pendant and isolate", tolerance = 0)
  expect_reference(centrality_mcc(cliques), c(v1 = 8, v2 = 6, v3 = 6, v4 = 8, v5 = 3, v6 = 1, v7 = 0),
                   reference = defined, covers = c("centrality()", "centrality_mcc()"), quantity = "mcc, K4, triangle, pendant and isolate, by hand",
                   tolerance = 0)
  expect_reference(centrality_mcc(za_complete(5)), za_mcc(za_complete(5)), reference = defined, covers = c("centrality()", "centrality_mcc()"),
                   quantity = "mcc, complete graph of 5", tolerance = 0)
  expect_reference(centrality_mcc(za_star(8)), za_mcc(za_star(8)), reference = defined, covers = c("centrality()", "centrality_mcc()"),
                   quantity = "mcc, star of 8", tolerance = 0)
  expect_reference(centrality_mcc(small), za_mcc(small), reference = defined, covers = c("centrality()", "centrality_mcc()"),
                   quantity = "mcc, random graph of 9", tolerance = 0)
})

test_that("extended coreness, LocalRank and Malatya centrality equal their definitions", {
  # Triangle 1-2-3, leaf 4 on node 1, isolate 5.
  triangle_leaf <- za_graph(c(1, 2, 2, 3, 3, 1, 1, 4), 5)
  expect_reference(centrality_extended_coreness(triangle_leaf), za_extended_coreness(triangle_leaf),
                   reference = defined, covers = c("centrality()", "centrality_extended_coreness()"), quantity = "extended_coreness, triangle with a leaf",
                   tolerance = 0)
  expect_reference(centrality_extended_coreness(za_complete(5)), za_extended_coreness(za_complete(5)),
                   reference = defined, covers = c("centrality()", "centrality_extended_coreness()"), quantity = "extended_coreness, complete graph of 5",
                   tolerance = 0)
  expect_reference(centrality_extended_coreness(karate), za_extended_coreness(karate), reference = defined,
                   covers = c("centrality()", "centrality_extended_coreness()"), quantity = "extended_coreness, karate", tolerance = 0)
  # Chen et al. (2012): LocalRank sums, over the neighbours u of the neighbours w, the number of nodes within
  # two steps of u.
  d <- za_hops(karate)
  within_two <- rowSums(d > 0 & d <= 2)
  expect_reference(centrality_semilocal(karate), as.numeric(karate %*% karate %*% within_two), reference = defined,
                   covers = c("centrality()", "centrality_semilocal()"), quantity = "semilocal, karate", tolerance = 0)
  expect_reference(centrality_malatya(star5), za_malatya(star5), reference = defined, covers = c("centrality()", "centrality_malatya()"),
                   quantity = "malatya, star of 5", tolerance = exact)
  expect_reference(centrality_malatya(karate), za_malatya(karate), reference = defined, covers = c("centrality()", "centrality_malatya()"),
                   quantity = "malatya, karate", tolerance = exact)
  expect_reference(centrality_malatya(connected), za_malatya(connected), reference = defined, covers = c("centrality()", "centrality_malatya()"),
                   quantity = "malatya, connected random graph", tolerance = exact)
})
