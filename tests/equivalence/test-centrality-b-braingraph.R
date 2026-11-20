# The node and network measures cograph shares with brainGraph (Watson) beyond the module roles: local and nodal
# efficiency (Latora and Marchiori 2001), leverage (Joyce et al. 2010), the s-core, vulnerability() (the loss of
# global efficiency when a node is removed) and the unweighted rich-club coefficient of rich_club() (Zhou and
# Mondragon 2004). brainGraph computes on igraph objects built from the same matrices.

undirected <- ref_connected_graph(12, density = 0.3, seed = 21L)
karate <- ref_karate()
weighted <- ref_connected_graph(12, density = 0.3, seed = 25L, weighted = TRUE, digits = 6L)
sparse <- ref_graph(14, density = 0.2, seed = 26L)

exact <- 1e-10

test_that("local efficiency equals brainGraph's efficiency(type = 'local')", {
  skip_without_reference(c("brainGraph", "igraph"))
  expect_reference(centrality_local_efficiency(karate),
                   ref_named(brainGraph::efficiency(ref_igraph(karate), type = "local", use.parallel = FALSE), karate),
                   reference = "brainGraph", covers = c("centrality()", "centrality_local_efficiency()"),
                   quantity = "local efficiency, karate", tolerance = exact)
  expect_reference(centrality_local_efficiency(undirected),
                   ref_named(brainGraph::efficiency(ref_igraph(undirected), type = "local", use.parallel = FALSE), undirected),
                   reference = "brainGraph", covers = c("centrality()", "centrality_local_efficiency()"),
                   quantity = "local efficiency, undirected", tolerance = exact)
})

test_that("normalised harmonic closeness equals brainGraph's nodal efficiency", {
  skip_without_reference(c("brainGraph", "igraph"))
  # Nodal efficiency is the mean inverse distance to the n - 1 others: harmonic closeness over n - 1.
  expect_reference(centrality_harmonic(karate, normalized = TRUE),
                   ref_named(brainGraph::efficiency(ref_igraph(karate), type = "nodal", use.parallel = FALSE), karate),
                   reference = "brainGraph", covers = c("centrality()", "centrality_harmonic()"),
                   quantity = "harmonic, normalised (nodal efficiency), karate", tolerance = exact)
})

test_that("leverage equals brainGraph's centr_lev()", {
  skip_without_reference(c("brainGraph", "igraph"))
  expect_reference(centrality_leverage(karate), ref_named(brainGraph::centr_lev(ref_igraph(karate)), karate),
                   reference = "brainGraph", covers = c("centrality()", "centrality_leverage()"),
                   quantity = "leverage, karate", tolerance = exact)
})

test_that("the s-core of an unweighted graph equals brainGraph's s_core()", {
  skip_without_reference(c("brainGraph", "igraph"))
  # Without weights both return the k-core number.
  expect_reference(centrality_s_core(karate), ref_named(brainGraph::s_core(ref_igraph(karate)), karate),
                   reference = "brainGraph", covers = c("centrality()", "centrality_s_core()"),
                   quantity = "s-core, karate unweighted", tolerance = exact)
})

test_that("the s-core of a weighted graph is a strength, brainGraph's the rank of the peeling step", {
  skip_without_reference(c("brainGraph", "igraph"))
  expect_known_difference(centrality_s_core(weighted), ref_named(brainGraph::s_core(ref_igraph(weighted)), weighted),
                          reference = "brainGraph", covers = c("centrality()", "centrality_s_core()"),
                          quantity = "s-core, undirected weighted", tolerance = exact,
                          reason = paste("cograph reports the largest strength threshold whose s-core holds the node",
                                         "(Eidsaa and Almaas 2013); brainGraph::s_core() numbers the peeling steps",
                                         "1, 2, 3, ... and reports the step at which the node was removed. The two",
                                         "order the nodes alike and agree on unweighted graphs."))
})

test_that("vulnerability() equals brainGraph's vulnerability()", {
  skip_without_reference(c("brainGraph", "igraph"))
  # brainGraph::vulnerability() reads the full graph's global efficiency from the graph attribute E.global (it
  # computes it only when that attribute is already there), so the attribute is set first with brainGraph's own
  # efficiency(type = "global") on the unweighted graph.
  with_efficiency <- \(graph) {
    as_igraph <- ref_igraph(graph)
    igraph::set_graph_attr(as_igraph, "E.global",
                           brainGraph::efficiency(as_igraph, type = "global", weights = NA, use.parallel = FALSE))
  }
  # Seen in the first run and reproduced from the definition: brainGraph takes the efficiency of the graph without
  # node i over its own n - 1 nodes, cograph over the original n.
  reason <- paste("cograph divides the inverse distances of the graph without node i by the original n (n - 1)",
                  "pairs, so a removal can only lower efficiency; brainGraph takes the global efficiency of the",
                  "reduced graph over its own (n - 1)(n - 2) pairs, which can rise and give a negative vulnerability.")
  expect_known_difference(vulnerability(undirected),
                          data.frame(node = rownames(undirected),
                                     vulnerability = brainGraph::vulnerability(with_efficiency(undirected), use.parallel = FALSE)),
                          reference = "brainGraph", covers = "vulnerability()", quantity = "vulnerability, undirected",
                          tolerance = exact, key = "node", reason = reason)
  expect_known_difference(vulnerability(karate),
                          data.frame(node = rownames(karate),
                                     vulnerability = brainGraph::vulnerability(with_efficiency(karate), use.parallel = FALSE)),
                          reference = "brainGraph", covers = "vulnerability()", quantity = "vulnerability, karate",
                          tolerance = exact, key = "node", reason = reason)
})

test_that("vulnerability() equals its definition over the original n (Latora and Marchiori 2005)", {
  # 1 - E(G - i) / E(G), both efficiencies sums of 1 / d over the n (n - 1) ordered pairs of the original graph.
  inverse_sum <- \(graph) {
    distances <- ref_hop_distances(graph)
    diag(distances) <- Inf
    sum(1 / distances)
  }
  by_definition <- \(graph) {
    whole <- inverse_sum(graph)
    data.frame(node = rownames(graph),
               vulnerability = vapply(seq_len(nrow(graph)), \(i) 1 - inverse_sum(graph[-i, -i, drop = FALSE]) / whole,
                                      numeric(1)))
  }
  expect_reference(vulnerability(undirected), by_definition(undirected),
                   reference = "independent: loss of global efficiency over the original n, base R",
                   covers = "vulnerability()", quantity = "vulnerability, undirected", tolerance = exact, key = "node")
  expect_reference(vulnerability(karate), by_definition(karate),
                   reference = "independent: loss of global efficiency over the original n, base R",
                   covers = "vulnerability()", quantity = "vulnerability, karate", tolerance = exact, key = "node")
})

test_that("the unweighted rich-club coefficient of rich_club() equals brainGraph's rich_club_coeff()", {
  skip_without_reference(c("brainGraph", "igraph"))
  # At each degree threshold rich_club() reports, phi(k) is the density among the nodes of degree above k.
  club <- rich_club(sparse, weighted = FALSE, normalized = FALSE)
  graph <- ref_igraph(sparse)
  expect_reference(club,
                   data.frame(threshold = club$threshold,
                              phi = vapply(club$threshold, \(k) brainGraph::rich_club_coeff(graph, k = k)$phi, numeric(1))),
                   reference = "brainGraph", covers = "rich_club()", quantity = "rich-club phi, unweighted",
                   tolerance = exact, key = "threshold")
  club_karate <- rich_club(karate, weighted = FALSE, normalized = FALSE)
  expect_reference(club_karate,
                   data.frame(threshold = club_karate$threshold,
                              phi = vapply(club_karate$threshold,
                                           \(k) brainGraph::rich_club_coeff(ref_igraph(karate), k = k)$phi, numeric(1))),
                   reference = "brainGraph", covers = "rich_club()", quantity = "rich-club phi, unweighted, karate",
                   tolerance = exact, key = "threshold")
})
