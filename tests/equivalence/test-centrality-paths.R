# centrality() and its wrappers for the measures built on degrees and shortest paths: degree, strength,
# betweenness, closeness, harmonic closeness, eccentricity, stress and load. Each is compared with NetworkX
# (a separate implementation in Python), igraph (cograph computes these natively, not through igraph) and sna
# where sna has the measure. A wrapper `centrality_<measure>()` calls `centrality(measures = "<measure>")`, so a
# row covers both.
#
# Graphs: connected random graphs from helper-reference.R (a ring through every node keeps every distance
# finite). Weighted graphs carry six-decimal weights, so two weighted paths are almost never of equal length and
# the shortest-path counts do not hinge on how a program rounds a sum. Weights are read as distances by every
# path measure here, as cograph, igraph and NetworkX (`weight=` / `distance=`) all do by default.

undirected <- ref_connected_graph(14, density = 0.25, seed = 11L)
undirected_weighted <- ref_connected_graph(14, density = 0.25, seed = 12L, weighted = TRUE, digits = 6L)
directed_weighted <- ref_connected_graph(14, density = 0.2, seed = 13L, directed = TRUE, weighted = TRUE, digits = 6L)
directed <- ref_connected_graph(14, density = 0.2, seed = 14L, directed = TRUE)

# Exact arithmetic on integers or a few additions of doubles: agreement to the last bits is expected.
exact <- 1e-10

test_that("degree and strength equal NetworkX's degree views", {
  nx <- ref_networkx(directed_weighted, c(
    degree = "dict(G.degree())", indegree = "dict(G.in_degree())", outdegree = "dict(G.out_degree())",
    strength = "dict(G.degree(weight='weight'))", instrength = "dict(G.in_degree(weight='weight'))",
    outstrength = "dict(G.out_degree(weight='weight'))"))
  expect_reference(centrality_degree(directed_weighted), nx$degree, reference = "networkx",
                   covers = c("centrality()", "centrality_degree()"), quantity = "degree, mode all, directed",
                   tolerance = exact)
  expect_reference(centrality_indegree(directed_weighted), nx$indegree, reference = "networkx",
                   covers = c("centrality()", "centrality_indegree()"), quantity = "in-degree, directed", tolerance = exact)
  expect_reference(centrality_outdegree(directed_weighted), nx$outdegree, reference = "networkx",
                   covers = c("centrality()", "centrality_outdegree()"), quantity = "out-degree, directed", tolerance = exact)
  expect_reference(centrality_strength(directed_weighted), nx$strength, reference = "networkx",
                   covers = c("centrality()", "centrality_strength()"), quantity = "strength, mode all, directed",
                   tolerance = exact)
  expect_reference(centrality_instrength(directed_weighted), nx$instrength, reference = "networkx",
                   covers = c("centrality()", "centrality_instrength()"), quantity = "in-strength, directed", tolerance = exact)
  expect_reference(centrality_outstrength(directed_weighted), nx$outstrength, reference = "networkx",
                   covers = c("centrality()", "centrality_outstrength()"), quantity = "out-strength, directed",
                   tolerance = exact)
})

test_that("degree and strength equal igraph's degree() and strength()", {
  skip_without_reference("igraph")
  graph <- ref_igraph(directed_weighted)
  expect_reference(centrality(directed_weighted, measures = c("degree", "strength"), mode = "in"),
                   ref_nodes(degree_in = igraph::degree(graph, mode = "in"), strength_in = igraph::strength(graph, mode = "in")),
                   reference = "igraph", covers = "centrality()", quantity = "degree and strength, mode in, directed",
                   tolerance = exact, key = "node")
  expect_reference(centrality(undirected_weighted, measures = c("degree", "strength")),
                   ref_nodes(degree_all = igraph::degree(ref_igraph(undirected_weighted)),
                             strength_all = igraph::strength(ref_igraph(undirected_weighted))),
                   reference = "igraph", covers = "centrality()", quantity = "degree and strength, undirected",
                   tolerance = exact, key = "node")
})

test_that("betweenness equals NetworkX's betweenness_centrality(normalized = False)", {
  on_undirected <- ref_networkx(undirected_weighted, c(
    betweenness = "nx.betweenness_centrality(G, normalized=False, weight='weight')"))
  on_directed <- ref_networkx(directed_weighted, c(
    betweenness = "nx.betweenness_centrality(G, normalized=False, weight='weight')"))
  on_unweighted <- ref_networkx(undirected, c(betweenness = "nx.betweenness_centrality(G, normalized=False)"))
  # Sums of path counts: a relative 1e-10 allows for the order of the additions.
  expect_reference(centrality_betweenness(undirected_weighted), on_undirected$betweenness, reference = "networkx",
                   covers = c("centrality()", "centrality_betweenness()"), quantity = "betweenness, undirected weighted",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_betweenness(directed_weighted), on_directed$betweenness, reference = "networkx",
                   covers = c("centrality()", "centrality_betweenness()"), quantity = "betweenness, directed weighted",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_betweenness(undirected), on_unweighted$betweenness, reference = "networkx",
                   covers = c("centrality()", "centrality_betweenness()"), quantity = "betweenness, undirected unweighted",
                   tolerance = exact, measure = "scaled")
})

test_that("betweenness equals igraph's and sna's betweenness()", {
  skip_without_reference(c("igraph", "sna"))
  expect_reference(centrality_betweenness(directed_weighted), igraph::betweenness(ref_igraph(directed_weighted)),
                   reference = "igraph", covers = c("centrality()", "centrality_betweenness()"),
                   quantity = "betweenness, directed weighted", tolerance = exact, measure = "scaled")
  # sna reads the matrix as unweighted (ignore.eval = TRUE).
  expect_reference(centrality_betweenness(undirected),
                   ref_named(sna::betweenness(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_betweenness()"),
                   quantity = "betweenness, undirected unweighted", tolerance = exact, measure = "scaled")
  expect_reference(centrality_betweenness(directed),
                   ref_named(sna::betweenness(directed, gmode = "digraph"), directed),
                   reference = "sna", covers = c("centrality()", "centrality_betweenness()"),
                   quantity = "betweenness, directed unweighted", tolerance = exact, measure = "scaled")
})

test_that("normalised closeness equals NetworkX's closeness_centrality(), in and out", {
  # NetworkX's closeness of u is (n - 1) / sum of the distances from the others TO u: cograph's in-closeness,
  # normalised. Out-closeness is NetworkX's closeness on the reversed graph.
  nx <- ref_networkx(directed_weighted, c(
    incloseness = "nx.closeness_centrality(G, distance='weight')",
    outcloseness = "nx.closeness_centrality(G.reverse(), distance='weight')"))
  on_undirected <- ref_networkx(undirected_weighted, c(closeness = "nx.closeness_centrality(G, distance='weight')"))
  expect_reference(centrality_incloseness(directed_weighted, normalized = TRUE), nx$incloseness, reference = "networkx",
                   covers = c("centrality()", "centrality_incloseness()"), quantity = "in-closeness, normalised, directed",
                   tolerance = exact)
  expect_reference(centrality_outcloseness(directed_weighted, normalized = TRUE), nx$outcloseness, reference = "networkx",
                   covers = c("centrality()", "centrality_outcloseness()"), quantity = "out-closeness, normalised, directed",
                   tolerance = exact)
  expect_reference(centrality_closeness(undirected_weighted, normalized = TRUE), on_undirected$closeness,
                   reference = "networkx", covers = c("centrality()", "centrality_closeness()"),
                   quantity = "closeness, normalised, undirected", tolerance = exact)
})

test_that("closeness equals igraph's closeness() and sna's closeness()", {
  skip_without_reference(c("igraph", "sna"))
  graph <- ref_igraph(directed_weighted)
  expect_reference(centrality(directed_weighted, measures = "closeness", mode = "out"),
                   ref_nodes(closeness_out = igraph::closeness(graph, mode = "out")),
                   reference = "igraph", covers = "centrality()", quantity = "closeness, mode out, directed weighted",
                   tolerance = exact, key = "node")
  expect_reference(centrality_closeness(undirected_weighted), igraph::closeness(ref_igraph(undirected_weighted)),
                   reference = "igraph", covers = c("centrality()", "centrality_closeness()"),
                   quantity = "closeness, undirected weighted", tolerance = exact)
  # sna's closeness is (n - 1) / sum of distances, on the unweighted graph.
  expect_reference(centrality_closeness(undirected, normalized = TRUE),
                   ref_named(sna::closeness(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_closeness()"),
                   quantity = "closeness, normalised, undirected unweighted", tolerance = exact)
})

test_that("harmonic closeness equals NetworkX's harmonic_centrality(), in and out", {
  # NetworkX sums 1 / d(v, u) over the others v: distances TO u, cograph's in-harmonic.
  nx <- ref_networkx(directed_weighted, c(
    inharmonic = "nx.harmonic_centrality(G, distance='weight')",
    outharmonic = "nx.harmonic_centrality(G.reverse(), distance='weight')"))
  on_undirected <- ref_networkx(undirected_weighted, c(harmonic = "nx.harmonic_centrality(G, distance='weight')"))
  expect_reference(centrality_inharmonic(directed_weighted), nx$inharmonic, reference = "networkx",
                   covers = c("centrality()", "centrality_inharmonic()"), quantity = "in-harmonic, directed", tolerance = exact)
  expect_reference(centrality_outharmonic(directed_weighted), nx$outharmonic, reference = "networkx",
                   covers = c("centrality()", "centrality_outharmonic()"), quantity = "out-harmonic, directed",
                   tolerance = exact)
  expect_reference(centrality_harmonic(undirected_weighted), on_undirected$harmonic, reference = "networkx",
                   covers = c("centrality()", "centrality_harmonic()"), quantity = "harmonic, undirected", tolerance = exact)
})

test_that("harmonic closeness equals igraph's harmonic_centrality()", {
  skip_without_reference("igraph")
  expect_reference(centrality_outharmonic(directed_weighted),
                   igraph::harmonic_centrality(ref_igraph(directed_weighted), mode = "out"),
                   reference = "igraph", covers = c("centrality()", "centrality_outharmonic()"),
                   quantity = "out-harmonic, directed", tolerance = exact)
})

test_that("eccentricity equals NetworkX's eccentricity(), out and in", {
  # NetworkX's eccentricity of v is the largest distance FROM v: cograph's out-eccentricity.
  nx <- ref_networkx(directed_weighted, c(
    outeccentricity = "nx.eccentricity(G, weight='weight')",
    ineccentricity = "nx.eccentricity(G.reverse(), weight='weight')"))
  on_undirected <- ref_networkx(undirected_weighted, c(eccentricity = "nx.eccentricity(G, weight='weight')"))
  expect_reference(centrality_outeccentricity(directed_weighted), nx$outeccentricity, reference = "networkx",
                   covers = c("centrality()", "centrality_outeccentricity()"), quantity = "out-eccentricity, directed",
                   tolerance = exact)
  expect_reference(centrality_ineccentricity(directed_weighted), nx$ineccentricity, reference = "networkx",
                   covers = c("centrality()", "centrality_ineccentricity()"), quantity = "in-eccentricity, directed",
                   tolerance = exact)
  expect_reference(centrality_eccentricity(undirected_weighted), on_undirected$eccentricity, reference = "networkx",
                   covers = c("centrality()", "centrality_eccentricity()"), quantity = "eccentricity, undirected",
                   tolerance = exact)
})

test_that("eccentricity equals igraph's eccentricity()", {
  skip_without_reference("igraph")
  expect_reference(centrality_eccentricity(undirected_weighted),
                   igraph::eccentricity(ref_igraph(undirected_weighted), weights = igraph::E(ref_igraph(undirected_weighted))$weight),
                   reference = "igraph", covers = c("centrality()", "centrality_eccentricity()"),
                   quantity = "eccentricity, undirected weighted", tolerance = exact)
})

test_that("stress equals sna's stresscent() on unweighted graphs", {
  skip_without_reference("sna")
  expect_reference(centrality_stress(undirected), ref_named(sna::stresscent(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_stress()"), quantity = "stress, undirected",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_stress(directed), ref_named(sna::stresscent(directed, gmode = "digraph"), directed),
                   reference = "sna", covers = c("centrality()", "centrality_stress()"), quantity = "stress, directed",
                   tolerance = exact, measure = "scaled")
})

test_that("load equals sna's loadcent() and NetworkX's load_centrality()", {
  skip_without_reference("sna")
  expect_reference(centrality_load(undirected), ref_named(sna::loadcent(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_load()"), quantity = "load, undirected",
                   tolerance = exact, measure = "scaled")
  nx <- ref_networkx(undirected, c(load = "nx.load_centrality(G, normalized=False)"))
  # Seen in the first run: every node differs by the same 27 = 2 (n - 1) + 1 on this 14-node graph.
  expect_known_difference(centrality_load(undirected), nx$load, reference = "networkx",
                          covers = c("centrality()", "centrality_load()"), quantity = "load, undirected",
                          tolerance = exact, measure = "scaled",
                          reason = paste("cograph and sna count, for each node, the flow it sends and receives itself",
                                         "(Goh et al. 2001 as implemented by sna::loadcent); NetworkX's load_centrality()",
                                         "leaves the end points out, so every node is lower by the same constant."))
})

test_that("load of the single node of a one-node graph equals sna's loadcent()", {
  skip_without_reference("sna")
  # Owner, 2026-11-22: follow sna. Load counts the end points (the convention above), so the one node scores 1.
  one_node <- matrix(0, 1, 1, dimnames = list("N1", "N1"))
  expect_reference(centrality_load(one_node), ref_named(sna::loadcent(one_node, gmode = "graph"), one_node),
                   reference = "sna", covers = c("centrality()", "centrality_load()"), quantity = "load, one-node graph",
                   tolerance = exact)
})
