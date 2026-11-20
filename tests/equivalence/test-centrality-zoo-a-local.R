# centrality() and its wrappers for the local and path-census measures of the Centrality Zoo: heatmap
# centrality, two-way random walk betweenness, flow coefficient, neighbourhood connectivity, local efficiency,
# local entropy, weighted H-index, redundancy, fragmentation, the k-path and geodesic k-path censuses, and node
# contraction. Heatmap and two-way random walk betweenness are compared with the worked examples printed in
# their papers; flow coefficient and neighbourhood connectivity with igraph (cograph computes both natively);
# the k-path census with sna; every measure with its definition written in base R (helper-zoo-a.R).

defined <- "independent: the measure's definition written in base R (helper-zoo-a.R)"
exact <- 1e-10

star5 <- za_star(5)
path5 <- za_path(5)
bridge6 <- za_bridge6()
karate <- ref_karate()
connected <- ref_connected_graph(10, density = 0.3, seed = 21L)
scattered <- ref_graph(14, density = 0.2, seed = 30L)

# Duron (2020), Fig. 2: 15 nodes, 19 edges (read off the figure; degree, closeness, betweenness and eigenvector
# columns of Table 1 all check).
duron <- za_graph(c(1, 3, 2, 3, 3, 4, 3, 5, 3, 6, 3, 7, 4, 5, 5, 6, 6, 7, 6, 8, 6, 14, 8, 9, 8, 10, 9, 10, 9, 12,
                    10, 11, 10, 12, 10, 13, 12, 15), 15)

test_that("heatmap centrality reproduces Duron (2020), Table 1", {
  # Printed to three decimals (-6.667, 0.667, 1.667; the others exact): half a unit in the last place, 5e-4.
  printed <- c(13, 13, -6.667, 10.5, 0.667, -7.2, 6.5, -3, 0, -6.8, 13, 1.667, 13, 13, 13)
  expect_reference(centrality_heatmap(duron), printed, reference = "published: Duron 2020, Table 1", covers = c("centrality()", "centrality_heatmap()"),
                   quantity = "heatmap, Duron network", tolerance = 5e-4)
  # The rank column (1 = smallest heatmap score, the most important node); the six nodes tied at 13 are ranked
  # in node order, as the table does.
  ranks <- c(10, 11, 3, 9, 6, 1, 8, 4, 5, 2, 12, 7, 13, 14, 15)
  expect_reference(rank(centrality_heatmap(duron), ties.method = "first"), ranks,
                   reference = "published: Duron 2020, Table 1", covers = c("centrality()", "centrality_heatmap()"),
                   quantity = "heatmap, Duron network, rank", tolerance = 0)
})

test_that("two-way random walk betweenness reproduces Curado et al. (2022), Fig. 1", {
  # The toy network of Fig. 1(a) (its strengths 5, 6, 13, 18, 16 are printed); the counts are whole numbers.
  toy <- za_graph(c(1, 2, 1, 4, 1, 5, 2, 3, 2, 5, 4, 3, 4, 5, 5, 3), 5, weights = c(1, 3, 1, 2, 3, 7, 8, 4))
  expect_reference(centrality_two_way_rw(toy), c(2, 2, 3, 3, 2), reference = "published: Curado et al. 2022, Fig. 1",
                   covers = c("centrality()", "centrality_two_way_rw()"), quantity = "two_way_rw, toy network",
                   tolerance = 0)
})

test_that("flow coefficient equals one minus igraph's local transitivity", {
  skip_without_reference("igraph")
  # Honey et al. (2007): the share of a node's neighbour pairs that are not linked, on an undirected graph
  # 1 - local clustering. Every node of a ring-based connected graph has two neighbours or more.
  expect_reference(centrality_flow_coefficient(connected),
                   1 - igraph::transitivity(ref_igraph(connected), type = "local"), reference = "igraph",
                   covers = c("centrality()", "centrality_flow_coefficient()"),
                   quantity = "flow_coefficient, connected random graph", tolerance = exact)
})

test_that("neighbourhood connectivity equals igraph's knn()", {
  skip_without_reference("igraph")
  # The mean degree of the neighbours (Maslov & Sneppen 2002); a non-zero entry is a tie, weights are not read
  # (igraph with weights = NA).
  weighted <- ref_connected_graph(12, density = 0.3, seed = 3L, weighted = TRUE)
  expect_reference(centrality_neighborhood_connectivity(connected), igraph::knn(ref_igraph(connected))$knn,
                   reference = "igraph", covers = c("centrality()", "centrality_neighborhood_connectivity()"),
                   quantity = "neighborhood_connectivity, connected random graph", tolerance = exact)
  expect_reference(centrality_neighborhood_connectivity(weighted), igraph::knn(ref_igraph(weighted), weights = NA)$knn,
                   reference = "igraph", covers = c("centrality()", "centrality_neighborhood_connectivity()"),
                   quantity = "neighborhood_connectivity, weights not read", tolerance = exact)
})

test_that("neighbourhood connectivity, local entropy, weighted H-index and redundancy equal their definitions", {
  # By hand on the star: the hub's neighbours have degree 1, each leaf's neighbour degree 4; an isolate scores 0.
  star_isolate <- za_union(star5, matrix(0, 1, 1))
  expect_reference(centrality_neighborhood_connectivity(star_isolate), c(1, 4, 4, 4, 4, 0), reference = defined,
                   covers = c("centrality()", "centrality_neighborhood_connectivity()"),
                   quantity = "neighborhood_connectivity, star and isolate, by hand", tolerance = exact)
  expect_reference(centrality_local_entropy(star5), za_local_entropy(star5), reference = defined, covers = c("centrality()", "centrality_local_entropy()"),
                   quantity = "local_entropy, star of 5", tolerance = exact)
  expect_reference(centrality_local_entropy(karate), za_local_entropy(karate), reference = defined, covers = c("centrality()", "centrality_local_entropy()"),
                   quantity = "local_entropy, karate", tolerance = exact)
  # C of the two triangles: link weights 3 * 2 (twice, from A), 3 * 2 (twice, from B), 3 * 3 (three times, from
  # D): six values are at least 6 and only three at least 7, so h = 6.
  expect_reference(centrality_weighted_h_index(bridge6), za_weighted_h_index(bridge6), reference = defined,
                   covers = c("centrality()", "centrality_weighted_h_index()"), quantity = "weighted_h_index, two triangles", tolerance = 0)
  expect_reference(centrality_weighted_h_index(star5), za_weighted_h_index(star5), reference = defined,
                   covers = c("centrality()", "centrality_weighted_h_index()"), quantity = "weighted_h_index, star of 5", tolerance = 0)
  expect_reference(centrality_weighted_h_index(karate), za_weighted_h_index(karate), reference = defined,
                   covers = c("centrality()", "centrality_weighted_h_index()"), quantity = "weighted_h_index, karate", tolerance = 0)
  expect_reference(centrality_redundancy(bridge6), za_redundancy(bridge6), reference = defined, covers = c("centrality()", "centrality_redundancy()"),
                   quantity = "redundancy, two triangles", tolerance = exact)
  expect_reference(centrality_redundancy(karate), za_redundancy(karate), reference = defined, covers = c("centrality()", "centrality_redundancy()"),
                   quantity = "redundancy, karate", tolerance = exact)
})

test_that("local efficiency equals the efficiency of the neighbour subgraph (Latora & Marchiori 2001)", {
  expect_reference(centrality_local_efficiency(bridge6), za_local_efficiency(bridge6), reference = defined,
                   covers = c("centrality()", "centrality_local_efficiency()"), quantity = "local_efficiency, two triangles", tolerance = exact)
  expect_reference(centrality_local_efficiency(star5), za_local_efficiency(star5), reference = defined,
                   covers = c("centrality()", "centrality_local_efficiency()"), quantity = "local_efficiency, star of 5", tolerance = exact)
  expect_reference(centrality_local_efficiency(karate), za_local_efficiency(karate), reference = defined,
                   covers = c("centrality()", "centrality_local_efficiency()"), quantity = "local_efficiency, karate", tolerance = exact)
  # By hand: A's neighbours B, C are linked (efficiency 1); C's neighbours A, B, D hold one link, two of the
  # six ordered pairs at distance 1.
  expect_reference(centrality_local_efficiency(bridge6), c(A = 1, B = 1, C = 2 / 6, D = 2 / 6, E = 1, F = 1),
                   reference = defined, covers = c("centrality()", "centrality_local_efficiency()"), quantity = "local_efficiency, two triangles, by hand",
                   tolerance = exact)
})

test_that("local efficiency differs from igraph's local_efficiency() by convention", {
  skip_without_reference("igraph")
  cycle <- za_ring(4)
  expect_known_difference(centrality_local_efficiency(cycle), igraph::local_efficiency(ref_igraph(cycle), weights = NA),
                          reference = "igraph", covers = c("centrality()", "centrality_local_efficiency()"),
                          quantity = "local_efficiency, cycle of 4", tolerance = exact,
                          reason = paste("cograph measures distances inside the subgraph induced by the neighbours",
                                         "(Latora & Marchiori 2001); igraph measures them in the graph without the",
                                         "node, where the two neighbours on a cycle of 4 are two steps apart through",
                                         "the fourth node (0.5 instead of 0)."))
})

test_that("fragmentation equals Borgatti's distance-weighted fragmentation after the deletion", {
  expect_reference(centrality_fragmentation(bridge6), za_fragmentation(bridge6), reference = defined, covers = c("centrality()", "centrality_fragmentation()"),
                   quantity = "fragmentation, two triangles", tolerance = exact)
  expect_reference(centrality_fragmentation(connected), za_fragmentation(connected), reference = defined,
                   covers = c("centrality()", "centrality_fragmentation()"), quantity = "fragmentation, connected random graph", tolerance = exact)
  expect_reference(centrality_fragmentation(scattered), za_fragmentation(scattered), reference = defined,
                   covers = c("centrality()", "centrality_fragmentation()"), quantity = "fragmentation, graph with isolates", tolerance = exact)
  # By hand: deleting C leaves {A, B} and {D, E, F}; the inverse distances sum to 2 + 6 over 5 * 4 pairs.
  expect_reference(centrality_fragmentation(bridge6)[["C"]], 1 - 8 / 20, reference = defined, covers = c("centrality()", "centrality_fragmentation()"),
                   quantity = "fragmentation, node C of two triangles, by hand", tolerance = exact)
})

test_that("the k-path and geodesic k-path censuses equal their enumerations", {
  expect_reference(centrality_kpath(za_path(3), kpath_len = 2), za_kpath(za_path(3), 2), reference = defined,
                   covers = c("centrality()", "centrality_kpath()"), quantity = "kpath, length 2, path of 3", tolerance = 0)
  expect_reference(centrality_kpath(bridge6), za_kpath(bridge6), reference = defined, covers = c("centrality()", "centrality_kpath()"),
                   quantity = "kpath, length 3, two triangles", tolerance = 0)
  expect_reference(centrality_kpath(connected), za_kpath(connected), reference = defined, covers = c("centrality()", "centrality_kpath()"),
                   quantity = "kpath, length 3, connected random graph", tolerance = 0)
  arcs <- ref_connected_graph(8, density = 0.3, seed = 9L, directed = TRUE)
  expect_reference(centrality(arcs, measures = "kpath", mode = "out"), ref_nodes(kpath_out = za_kpath(arcs)),
                   reference = defined, covers = "centrality()", quantity = "kpath, length 3, mode out, directed",
                   tolerance = 0, key = "node")
  expect_reference(centrality_geodesic_kpath(path5, kpath_k = 3), za_geodesic_kpath(path5, 3), reference = defined,
                   covers = c("centrality()", "centrality_geodesic_kpath()"), quantity = "geodesic_kpath, k = 3, path of 5", tolerance = 0)
  expect_reference(centrality_geodesic_kpath(star5, kpath_k = 1), za_geodesic_kpath(star5, 1), reference = defined,
                   covers = c("centrality()", "centrality_geodesic_kpath()"), quantity = "geodesic_kpath, k = 1, star of 5", tolerance = 0)
  expect_reference(centrality_geodesic_kpath(za_ring(4), kpath_k = 2), za_geodesic_kpath(za_ring(4), 2),
                   reference = defined, covers = c("centrality()", "centrality_geodesic_kpath()"), quantity = "geodesic_kpath, k = 2, cycle of 4", tolerance = 0)
  expect_reference(centrality_geodesic_kpath(connected), za_geodesic_kpath(connected), reference = defined,
                   covers = c("centrality()", "centrality_geodesic_kpath()"), quantity = "geodesic_kpath, k = 3, connected random graph", tolerance = 0)
})

test_that("the k-path census equals sna's kpath.census()", {
  skip_without_reference("sna")
  # sna tabulates by vertex the paths of each length that pass through it, endpoints included; its first column
  # is the aggregate.
  by_vertex <- \(graph, mode) colSums(sna::kpath.census(graph, maxlen = 3, mode = mode, tabulate.by.vertex = TRUE)$path.count)[-1L]
  arcs <- ref_connected_graph(8, density = 0.3, seed = 9L, directed = TRUE)
  expect_reference(centrality_kpath(connected), ref_named(by_vertex(connected, "graph"), connected),
                   reference = "sna", covers = c("centrality()", "centrality_kpath()"),
                   quantity = "kpath, length 3, connected random graph", tolerance = 0)
  expect_reference(centrality(arcs, measures = "kpath", mode = "out"),
                   ref_nodes(kpath_out = ref_named(by_vertex(arcs, "digraph"), arcs)), reference = "sna",
                   covers = "centrality()", quantity = "kpath, length 3, mode out, directed", tolerance = 0, key = "node")
})

test_that("node contraction and its improved form equal their definitions (Wang et al. 2011)", {
  expect_reference(centrality_node_contraction(path5), za_node_contraction(path5), reference = defined,
                   covers = c("centrality()", "centrality_node_contraction()"), quantity = "node_contraction, path of 5", tolerance = exact)
  expect_reference(centrality_node_contraction(connected), za_node_contraction(connected), reference = defined,
                   covers = c("centrality()", "centrality_node_contraction()"), quantity = "node_contraction, connected random graph", tolerance = exact)
  expect_reference(centrality_node_contraction(karate), za_node_contraction(karate), reference = defined,
                   covers = c("centrality()", "centrality_node_contraction()"), quantity = "node_contraction, karate", tolerance = exact)
  expect_reference(centrality_node_contraction_improved(path5), za_node_contraction_improved(path5),
                   reference = defined, covers = c("centrality()", "centrality_node_contraction_improved()"), quantity = "node_contraction_improved, path of 5",
                   tolerance = exact)
  expect_reference(centrality_node_contraction_improved(connected), za_node_contraction_improved(connected),
                   reference = defined, covers = c("centrality()", "centrality_node_contraction_improved()"), quantity = "node_contraction_improved, connected random graph",
                   tolerance = exact)
  # The closed form on a path of n nodes: 2 / (n + 1) at an end, 2 (2n - 1) / (n (n + 1)) inside; the line graph
  # of the path of 5 is the path of 4, with link scores 2/5, 0.7, 0.7, 2/5.
  nodes <- c(2 / 6, 0.6, 0.6, 0.6, 2 / 6)
  links <- c(0.4, 0.7, 0.7, 0.4)
  expect_reference(centrality_node_contraction(path5), nodes, reference = defined, covers = c("centrality()", "centrality_node_contraction()"),
                   quantity = "node_contraction, path of 5, closed form", tolerance = exact)
  expect_reference(centrality_node_contraction_improved(path5),
                   5 / 6 * nodes + 1 / 6 * c(links[[1]], links[[1]] + links[[2]], links[[2]] + links[[3]],
                                             links[[3]] + links[[4]], links[[4]]),
                   reference = defined, covers = c("centrality()", "centrality_node_contraction_improved()"), quantity = "node_contraction_improved, path of 5, closed form",
                   tolerance = exact)
})
