# centrality() and its wrappers for the core decompositions and the h-index family: the k-core number
# (coreness), onion layers, k-truss, the local h-index (whose iteration converges to the core number), s-core,
# weighted k-shell, renewed coreness, extended neighbourhood coreness, the strength h-index and the weighted
# h-index. The core number, onion layers and truss are compared with NetworkX and igraph; the weighted and
# modified peelings with their published definitions, written in base R in helper-centrality-a2.R (a peeling
# that removes every node at or below the current threshold until none is left, then raises the threshold).
# cograph computes all of these natively, not through igraph.

undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
undirected_weighted <- ref_connected_graph(12, density = 0.3, seed = 42L, weighted = TRUE, digits = 6L)
directed <- ref_connected_graph(12, density = 0.25, seed = 43L, directed = TRUE)
# Not connected and sparser: an isolate, cores of order 0 to 3, links below the renewed-coreness threshold.
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()
# Karate with six-decimal weights: strengths, s-cores and weighted shells of many different values.
karate_weighted <- karate * ref_graph(34, density = 1, seed = 45L, weighted = TRUE, digits = 6L)

# Integer peelings and short sums: agreement to the last bits is expected.
exact <- 1e-10

test_that("coreness equals NetworkX's core_number() and igraph's coreness()", {
  skip_without_reference("igraph")
  calls <- c(core = "nx.core_number(G)")
  expect_reference(centrality_coreness(undirected), ref_networkx(undirected, calls)$core, reference = "networkx",
                   covers = c("centrality()", "centrality_coreness()"), quantity = "coreness, undirected", tolerance = exact)
  expect_reference(centrality_coreness(karate), ref_networkx(karate, calls)$core, reference = "networkx",
                   covers = c("centrality()", "centrality_coreness()"), quantity = "coreness, karate", tolerance = exact)
  # NetworkX peels a DiGraph by in-degree plus out-degree, cograph's mode = "all".
  expect_reference(centrality_coreness(directed), ref_networkx(directed, calls)$core, reference = "networkx",
                   covers = c("centrality()", "centrality_coreness()"), quantity = "coreness, mode all, directed",
                   tolerance = exact)
  graph <- ref_igraph(directed)
  expect_reference(centrality(directed, measures = "coreness", mode = "out"),
                   ref_nodes(coreness_out = igraph::coreness(graph, mode = "out")), reference = "igraph",
                   covers = "centrality()", quantity = "coreness, mode out, directed", tolerance = exact, key = "node")
  expect_reference(centrality_coreness(directed, mode = "in"), ref_named(igraph::coreness(graph, mode = "in"), directed),
                   reference = "igraph", covers = c("centrality()", "centrality_coreness()"),
                   quantity = "coreness, mode in, directed", tolerance = exact)
  expect_reference(centrality_coreness(sparse), ref_named(igraph::coreness(ref_igraph(sparse)), sparse),
                   reference = "igraph", covers = c("centrality()", "centrality_coreness()"),
                   quantity = "coreness, sparse undirected", tolerance = exact)
})

test_that("onion layers equal NetworkX's onion_layers()", {
  calls <- c(onion = "nx.onion_layers(G)")
  expect_reference(centrality(undirected, measures = "onion"),
                   ref_nodes(onion_all = ref_networkx(undirected, calls)$onion), reference = "networkx",
                   covers = "centrality()", quantity = "onion, undirected", tolerance = exact, key = "node")
  expect_reference(centrality(sparse, measures = "onion"),
                   ref_nodes(onion_all = ref_networkx(sparse, calls)$onion), reference = "networkx",
                   covers = "centrality()", quantity = "onion, sparse undirected", tolerance = exact, key = "node")
  expect_reference(centrality(karate, measures = "onion"),
                   ref_nodes(onion_all = ref_networkx(karate, calls)$onion), reference = "networkx",
                   covers = "centrality()", quantity = "onion, karate", tolerance = exact, key = "node")
})

test_that("the truss number equals the largest k whose NetworkX k_truss() holds the node", {
  # Malliaros et al. (2016): a node's truss number is the largest truss number of an incident edge. NetworkX's
  # k_truss(G, k) keeps the edges in at least k - 2 triangles and drops the nodes left without an edge.
  calls <- c(truss = paste("(lambda trusses: {v: max([k for k, nodes in trusses.items() if v in nodes] or [0])",
                           "for v in G})({k: set(nx.k_truss(G, k)) for k in range(2, len(G) + 1)})"))
  expect_reference(centrality_truss(undirected), ref_networkx(undirected, calls)$truss, reference = "networkx",
                   covers = c("centrality()", "centrality_truss()"), quantity = "truss, undirected", tolerance = exact)
  expect_reference(centrality_truss(karate), ref_networkx(karate, calls)$truss, reference = "networkx",
                   covers = c("centrality()", "centrality_truss()"), quantity = "truss, karate", tolerance = exact)
})

test_that("the local h-index converges to the core number (NetworkX) and equals its iteration", {
  # Lu et al. (2016): iterating the h-index operator from the degrees converges to the core number.
  calls <- c(core = "nx.core_number(G)")
  expect_reference(centrality(karate, measures = "local_hindex"),
                   ref_nodes(local_hindex_all = ref_networkx(karate, calls)$core), reference = "networkx",
                   covers = "centrality()", quantity = "local_hindex, karate (limit: the core number)",
                   tolerance = exact, key = "node")
  expect_reference(centrality(sparse, measures = "local_hindex"),
                   ref_nodes(local_hindex_all = ref_networkx(sparse, calls)$core), reference = "networkx",
                   covers = "centrality()", quantity = "local_hindex, sparse undirected (limit: the core number)",
                   tolerance = exact, key = "node")
  adjacency <- ref_a2_binary(undirected)
  h_step <- \(h) ref_a2_per_node(undirected, \(i) ref_a2_hindex(h[which(adjacency[i, ] != 0)]))
  iterate <- \(h) {
    nxt <- h_step(h)
    if (identical(nxt, h)) h else iterate(nxt)
  }
  expect_reference(centrality(undirected, measures = "local_hindex"),
                   ref_nodes(local_hindex_all = iterate(ref_a2_per_node(undirected, \(i) sum(adjacency[i, ])))),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Lu et al. (2016) h-index operator iterated from the degrees, base R",
                   covers = "centrality()", quantity = "local_hindex, undirected", tolerance = exact, key = "node")
})

test_that("the s-core index equals Eidsaa and Almaas's peeling by strength, and the core number without weights", {
  expect_reference(centrality_s_core(undirected_weighted), ref_a2_s_core(undirected_weighted),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Eidsaa and Almaas (2013) s-core peeling, base R",
                   covers = c("centrality()", "centrality_s_core()"), quantity = "s_core, undirected weighted",
                   tolerance = exact)
  expect_reference(centrality_s_core(karate_weighted), ref_a2_s_core(karate_weighted),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Eidsaa and Almaas (2013) s-core peeling, base R",
                   covers = c("centrality()", "centrality_s_core()"), quantity = "s_core, karate weighted",
                   tolerance = exact)
  # With unit weights the strength is the degree and the s-core index is the core number.
  expect_reference(centrality_s_core(sparse), ref_networkx(sparse, c(core = "nx.core_number(G)"))$core,
                   reference = "networkx", covers = c("centrality()", "centrality_s_core()"),
                   quantity = "s_core, sparse undirected, unit weights", tolerance = exact)
})

test_that("the weighted k-shell equals Garas et al.'s peeling and the core number with unit weights", {
  expect_reference(centrality_weighted_kshell(karate_weighted), ref_a2_weighted_kshell(karate_weighted),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Garas, Schweitzer and Havlin (2012) weighted k-shell, base R",
                   covers = c("centrality()", "centrality_weighted_kshell()"),
                   quantity = "weighted_kshell, alpha 1, beta 1, karate weighted", tolerance = exact)
  expect_reference(centrality_weighted_kshell(undirected_weighted, wks_alpha = 2, wks_beta = 1),
                   ref_a2_weighted_kshell(undirected_weighted, alpha = 2, beta = 1),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Garas, Schweitzer and Havlin (2012) weighted k-shell, base R",
                   covers = c("centrality()", "centrality_weighted_kshell()"),
                   quantity = "weighted_kshell, alpha 2, beta 1, undirected weighted", tolerance = exact)
  expect_reference(centrality_weighted_kshell(karate), ref_networkx(karate, c(core = "nx.core_number(G)"))$core,
                   reference = "networkx", covers = c("centrality()", "centrality_weighted_kshell()"),
                   quantity = "weighted_kshell, karate, unit weights", tolerance = exact)
})

test_that("renewed and extended neighbourhood coreness equal their definitions and NetworkX", {
  expect_reference(centrality_renewed_coreness(sparse), ref_a2_renewed_coreness(sparse, threshold = 2),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Liu, Tang, Zhou and Do (2015) renewed coreness, base R",
                   covers = c("centrality()", "centrality_renewed_coreness()"),
                   quantity = "renewed_coreness, threshold 2, sparse undirected", tolerance = exact)
  expect_reference(centrality_renewed_coreness(karate), ref_a2_renewed_coreness(karate, threshold = 2),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Liu, Tang, Zhou and Do (2015) renewed coreness, base R",
                   covers = c("centrality()", "centrality_renewed_coreness()"),
                   quantity = "renewed_coreness, threshold 2, karate", tolerance = exact)
  expect_reference(centrality_renewed_coreness(karate, renewed_threshold = 3),
                   ref_a2_renewed_coreness(karate, threshold = 3),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Liu, Tang, Zhou and Do (2015) renewed coreness, base R",
                   covers = c("centrality()", "centrality_renewed_coreness()"),
                   quantity = "renewed_coreness, threshold 3, karate", tolerance = exact)
  # Bae and Kim (2014): sum over the neighbours j of the core numbers of the neighbours of j.
  calls <- c(extended = "(lambda core: {v: sum(core[l] for j in G[v] for l in G[j]) for v in G})(nx.core_number(G))")
  expect_reference(centrality_extended_coreness(undirected), ref_networkx(undirected, calls)$extended,
                   reference = "networkx", covers = c("centrality()", "centrality_extended_coreness()"),
                   quantity = "extended_coreness, undirected", tolerance = exact)
  expect_reference(centrality_extended_coreness(karate), ref_networkx(karate, calls)$extended,
                   reference = "networkx", covers = c("centrality()", "centrality_extended_coreness()"),
                   quantity = "extended_coreness, karate", tolerance = exact)
  expect_reference(centrality_extended_coreness(sparse), ref_a2_extended_coreness(sparse),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Bae and Kim (2014) extended neighbourhood coreness, base R",
                   covers = c("centrality()", "centrality_extended_coreness()"),
                   quantity = "extended_coreness, sparse undirected", tolerance = exact)
})

test_that("the strength h-index and the weighted h-index equal their definitions", {
  # Weights of 1 to 5 so that strengths reach past 1 and the h-index is not trivially 0 or 1.
  heavy <- ceiling(5 * undirected_weighted)
  expect_reference(centrality(heavy, measures = "hindex_strength"),
                   ref_nodes(hindex_strength_all = ref_a2_hindex_strength(heavy)),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "h-index of the neighbours' strengths (Korn et al. 2009), base R",
                   covers = "centrality()", quantity = "hindex_strength, undirected, weights 1 to 5",
                   tolerance = exact, key = "node")
  expect_reference(centrality(karate, measures = "hindex_strength"),
                   ref_nodes(hindex_strength_all = ref_a2_hindex_strength(karate)),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "h-index of the neighbours' strengths (Korn et al. 2009), base R",
                   covers = "centrality()", quantity = "hindex_strength, karate, unit weights",
                   tolerance = exact, key = "node")
  expect_reference(centrality_weighted_h_index(undirected), ref_a2_weighted_h_index(undirected),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Gao et al. (2019) weighted h-index, base R",
                   covers = c("centrality()", "centrality_weighted_h_index()"),
                   quantity = "weighted_h_index, undirected", tolerance = exact)
  expect_reference(centrality_weighted_h_index(karate), ref_a2_weighted_h_index(karate),
                   reference = "independent: the core and h-index definitions written in base R (each named in its note)", note = "Gao et al. (2019) weighted h-index, base R",
                   covers = c("centrality()", "centrality_weighted_h_index()"),
                   quantity = "weighted_h_index, karate", tolerance = exact)
})
