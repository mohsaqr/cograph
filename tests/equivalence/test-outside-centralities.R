# Centralities that were checked only against a formula written for the check, now set beside outside software
# as well: networktools (two-step expected influence), centiserve (average distance, expected percolated
# component), NetworkX (sums of shortest-path lengths, Wiener index, neighbour degrees, harmonic centrality),
# tidygraph (integration) and sna (length-scaled betweenness). The formula checks stay where they were; these are
# the second way.
#
# Inputs: random graphs of helper-reference.R (six-decimal weights, so two weighted paths seldom tie) and networks
# drawn by simulab (helper-simulab.R) of other shapes: scale-free, small-world, block, and sparse ones with
# isolates and more than one component.

undirected <- ref_connected_graph(12, density = 0.3, seed = 4L, weighted = TRUE, digits = 6L)
directed <- ref_graph(12, density = 0.3, seed = 7L, directed = TRUE, weighted = TRUE, digits = 6L)
binary <- (undirected != 0) * 1
karate <- ref_karate()
scale_free <- sim_graph(20, seed = 11L, model = "barabasi_albert", directed = FALSE)
small_world <- sim_graph(20, seed = 12L, model = "small_world")
sparse <- sim_graph(18, seed = 13L, model = "bernoulli", probability = 0.08, directed = FALSE, weight = "uniform")
sparse_directed <- sim_graph(16, seed = 14L, model = "bernoulli", probability = 0.12, directed = TRUE, weight = "uniform")
block_directed <- sim_graph(18, seed = 15L, model = "block", directed = TRUE, within_probability = 0.5,
                            between_probability = 0.05)
# signed weights, the networks expected influence was made for
signed <- undirected * ref_with_seed(5L, {
  flips <- matrix(sample(c(-1, 1), length(undirected), replace = TRUE, prob = c(0.3, 0.7)), nrow(undirected))
  flips[lower.tri(flips)] <- t(flips)[lower.tri(flips)]
  flips
})

# Sums over at most n^2 path lengths or products: rounding only.
exact <- 1e-10

test_that("two-step expected influence equals networktools' expectedInf()", {
  skip_without_reference("networktools")
  step2 <- \(graph, directed = FALSE) networktools::expectedInf(graph, step = "both", directed = directed)$step2
  graphs <- list(weighted = undirected, signed = signed, sparse_with_isolates = sparse, scale_free = scale_free)
  lapply(names(graphs), \(name) {
    expect_reference(centrality_expected_influence_2(graphs[[name]]), step2(graphs[[name]]), reference = "networktools",
                     covers = c("centrality_expected_influence_2()", "centrality()"),
                     quantity = sprintf("two-step expected influence, undirected, %s", name), tolerance = exact)
  })
  expect_reference(centrality_expected_influence_2(directed, mode = "out"), step2(directed, TRUE),
                   reference = "networktools", covers = c("centrality_expected_influence_2()", "centrality()"),
                   quantity = "two-step expected influence, directed, out", tolerance = exact)
  # networktools follows outgoing edges; incoming edges are the outgoing edges of the transpose
  expect_reference(centrality_expected_influence_2(directed, mode = "in"), step2(t(directed), TRUE),
                   reference = "networktools", covers = c("centrality_expected_influence_2()", "centrality()"),
                   quantity = "two-step expected influence, directed, in (networktools on the transpose)",
                   tolerance = exact)
})

test_that("Wiener centrality equals the sums of NetworkX's shortest-path lengths and half of them its Wiener index", {
  sums <- \(graph, reverse = FALSE) ref_networkx(graph, c(total = sprintf(
    "{v: sum(nx.single_source_dijkstra_path_length(%s, v, weight='weight').values()) for v in G}",
    if (reverse) "G.reverse()" else "G")))$total[rownames(graph)]
  graphs <- list(weighted = undirected, scale_free = scale_free, sparse_two_components = sparse)
  lapply(names(graphs), \(name) {
    expect_reference(centrality_wiener(graphs[[name]]), sums(graphs[[name]]), reference = "networkx",
                     covers = c("centrality_wiener()", "centrality()"),
                     quantity = sprintf("Wiener centrality, sum of shortest-path lengths, %s", name), tolerance = exact)
  })
  expect_reference(centrality_wiener(directed, mode = "out"), sums(directed), reference = "networkx",
                   covers = c("centrality_wiener()", "centrality()"),
                   quantity = "Wiener centrality, directed, out", tolerance = exact)
  expect_reference(centrality_wiener(sparse_directed, mode = "in"), sums(sparse_directed, reverse = TRUE),
                   reference = "networkx", covers = c("centrality_wiener()", "centrality()"),
                   quantity = "Wiener centrality, directed, in", tolerance = exact)
  index <- ref_networkx(undirected, c(index = "nx.wiener_index(G, weight='weight')"))$index
  expect_reference(sum(centrality_wiener(undirected)) / 2, index, reference = "networkx",
                   covers = "centrality_wiener()", quantity = "half the sum of the scores, Wiener index",
                   tolerance = exact)
})

test_that("Wiener centrality equals centiserve's averagedis() times n + 1 on a connected graph", {
  skip_without_reference("centiserve")
  # cograph's average distance divides the sum by n + 1, as centiserve does (both verified elsewhere)
  graphs <- list(weighted = undirected, karate = karate, small_world = small_world)
  lapply(names(graphs), \(name) {
    graph <- ref_igraph(graphs[[name]])
    weights <- if (igraph::is_weighted(graph)) igraph::E(graph)$weight else NULL
    expect_reference(centrality_wiener(graphs[[name]]),
                     ref_named(centiserve::averagedis(graph, weights = weights) * (nrow(graphs[[name]]) + 1),
                               graphs[[name]]),
                     reference = "centiserve", covers = c("centrality_wiener()", "centrality()"),
                     quantity = sprintf("Wiener centrality, averagedis x (n + 1), %s", name), tolerance = exact)
  })
})

test_that("expected centrality equals NetworkX's average neighbour degree times the degree", {
  undirected_call <- c(e = "{v: nx.average_neighbor_degree(G)[v] * G.degree(v) for v in G}")
  graphs <- list(random = binary, karate = karate, scale_free = scale_free, sparse_with_isolates = (sparse != 0) * 1)
  lapply(names(graphs), \(name) {
    expect_reference(centrality_expected(graphs[[name]]), ref_networkx(graphs[[name]], undirected_call)$e[rownames(graphs[[name]])],
                     reference = "networkx", covers = c("centrality_expected()", "centrality()"),
                     quantity = sprintf("expected centrality, %s", name), tolerance = exact)
  })
  arcs <- (block_directed != 0) * 1
  out <- ref_networkx(arcs, c(
    out = "{v: nx.average_neighbor_degree(G, source='out', target='out')[v] * G.out_degree(v) for v in G}",
    into = "{v: nx.average_neighbor_degree(G, source='in', target='in')[v] * G.in_degree(v) for v in G}"))
  expect_reference(centrality_expected(arcs, mode = "out"), out$out[rownames(arcs)], reference = "networkx",
                   covers = c("centrality_expected()", "centrality()"), quantity = "expected centrality, directed, out",
                   tolerance = exact)
  expect_reference(centrality_expected(arcs, mode = "in"), out$into[rownames(arcs)], reference = "networkx",
                   covers = c("centrality_expected()", "centrality()"), quantity = "expected centrality, directed, in",
                   tolerance = exact)
})

test_that("integration equals tidygraph's centrality_integration() rescaled to Valente and Foreman's form", {
  skip_without_reference("tidygraph")
  # tidygraph (netrankr) sums 1 - (d - 1) / D over every node, the node itself included; Valente and Foreman (1998),
  # which cograph follows since 2.7.20, sum D + 1 - d over the other nodes and divide by n - 1. On a connected
  # graph the two are tied exactly: V&F = (tidygraph * D - (D + 1)) / (n - 1).
  graphs <- list(random = binary, karate = karate, small_world = small_world, scale_free = scale_free)
  lapply(names(graphs), \(name) {
    graph <- ref_igraph(graphs[[name]])
    tidy <- ref_named(tidygraph::with_graph(tidygraph::as_tbl_graph(graph), tidygraph::centrality_integration()),
                      graphs[[name]])
    diameter <- igraph::diameter(graph, weights = NA)
    expect_known_difference(centrality_integration(graphs[[name]]), tidy, reference = "tidygraph",
                            covers = c("centrality_integration()", "centrality()"),
                            quantity = sprintf("integration, %s", name), tolerance = exact,
                            reason = paste("tidygraph's integration sums 1 - (d - 1) / D over every node, the node",
                                           "itself included; Valente and Foreman (1998) sum D + 1 - d over the other",
                                           "nodes and divide by n - 1."),
                            agrees_with = "(tidygraph * D - (D + 1)) / (n - 1)")
    expect_reference(centrality_integration(graphs[[name]]), (tidy * diameter - (diameter + 1)) / (nrow(graphs[[name]]) - 1),
                     reference = "tidygraph", covers = c("centrality_integration()", "centrality()"),
                     quantity = sprintf("integration, %s, tidygraph rescaled to Valente and Foreman", name),
                     tolerance = exact)
  })
})

test_that("length-scaled betweenness equals sna's betweenness(cmode = 'lengthscaled')", {
  skip_without_reference("sna")
  scaled <- \(graph, gmode, ignore_eval = TRUE) ref_named(sna::betweenness(graph, gmode = gmode, cmode = "lengthscaled",
                                                                           ignore.eval = ignore_eval), graph)
  graphs <- list(random = binary, karate = karate, scale_free = scale_free, sparse_two_components = (sparse != 0) * 1)
  lapply(names(graphs), \(name) {
    expect_reference(centrality_length_scaled_betweenness(graphs[[name]], weighted = FALSE), scaled(graphs[[name]], "graph"),
                     reference = "sna", covers = c("centrality_length_scaled_betweenness()", "centrality()"),
                     quantity = sprintf("length-scaled betweenness, undirected, %s", name), tolerance = exact)
  })
  arcs <- (block_directed != 0) * 1
  expect_reference(centrality_length_scaled_betweenness(arcs, weighted = FALSE), scaled(arcs, "digraph"),
                   reference = "sna", covers = c("centrality_length_scaled_betweenness()", "centrality()"),
                   quantity = "length-scaled betweenness, directed block model", tolerance = exact)
  expect_reference(centrality_length_scaled_betweenness(undirected), scaled(undirected, "graph", ignore_eval = FALSE),
                   reference = "sna", covers = c("centrality_length_scaled_betweenness()", "centrality()"),
                   quantity = "length-scaled betweenness, weights as lengths", tolerance = exact)
})

test_that("delta closeness with delta = 1 equals NetworkX's harmonic centrality over n - 1", {
  # NetworkX sums the reciprocal distances into a node; cograph's out-mode the distances from it (G reversed)
  harmonic <- \(graph, reverse = FALSE) {
    call <- sprintf("nx.harmonic_centrality(%s, distance='weight')", if (reverse) "G.reverse()" else "G")
    ref_networkx(graph, c(h = call))$h[rownames(graph)] / (nrow(graph) - 1)
  }
  graphs <- list(weighted = undirected, sparse_two_components = sparse, scale_free = scale_free)
  lapply(names(graphs), \(name) {
    expect_reference(centrality_delta_closeness(graphs[[name]], closeness_delta = 1), harmonic(graphs[[name]]),
                     reference = "networkx", covers = c("centrality_delta_closeness()", "centrality()"),
                     quantity = sprintf("delta closeness, delta 1, %s", name), tolerance = exact)
  })
  expect_reference(centrality_delta_closeness(directed, mode = "out"), harmonic(directed, reverse = TRUE),
                   reference = "networkx", covers = c("centrality_delta_closeness()", "centrality()"),
                   quantity = "delta closeness, delta 1, directed, out", tolerance = exact)
  expect_reference(centrality_delta_closeness(directed, mode = "in"), harmonic(directed), reference = "networkx",
                   covers = c("centrality_delta_closeness()", "centrality()"),
                   quantity = "delta closeness, delta 1, directed, in", tolerance = exact)
})

test_that("expected percolated component equals centiserve's epc() by seeded Monte Carlo", {
  skip_without_reference("centiserve")
  # centiserve::epc() removes each edge with probability `threshold` in each of 1000 realisations and returns the
  # sum over the realisations of |component of v| / n; cograph returns the mean, so the reference is epc() / 1000.
  # The reference is the mean of 20 calls (20000 realisations); cograph runs 20000. Tolerance: 4 standard errors
  # of the difference, sqrt(2) x the largest standard error of the reference's mean over its own 20 calls, a
  # rule fixed before the run (cograph's estimate has the same number of realisations).
  graphs <- list(random = binary, small_world = small_world)
  lapply(names(graphs), \(name) {
    graph <- ref_igraph(graphs[[name]])
    calls <- ref_with_seed(2026L, vapply(1:20, \(i) centiserve::epc(graph, threshold = 0.5) / 1000,
                                         numeric(nrow(graphs[[name]]))))
    tolerance <- 4 * sqrt(2) * max(apply(calls, 1L, stats::sd)) / sqrt(20)
    lapply(1:3, \(seed) {
      expect_reference(centrality_epc(graphs[[name]], epc_threshold = 0.5, epc_runs = 20000, epc_seed = seed),
                       ref_named(rowMeans(calls), graphs[[name]]), reference = "centiserve",
                       covers = c("centrality_epc()", 'centrality(measures = "epc")', "centrality()"),
                       quantity = sprintf("expected percolated component, %s, seed %d", name, seed),
                       tolerance = tolerance)
    })
  })
})
