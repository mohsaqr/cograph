# centrality() and its wrappers for the local measures, those read off a node's neighbourhood or its ball of a
# few hops: local clustering, neighbour degree, local efficiency, local reaching, k-reach, diversity,
# percolation, ego betweenness, closeness vitality, k-path counts, collective influence and gravity, and the
# distance sums (Harary, Wiener, Dangalchev, generalised closeness, integration, radiality, average distance,
# centroid, Gil-Schmidt). Each is compared with NetworkX (a separate implementation in Python), with igraph and
# sna where they have the measure (cograph computes all of these natively, not through igraph), and with the
# published definition written in base R in helper-centrality-a2.R. A wrapper `centrality_<measure>()` calls
# `centrality(measures = "<measure>")`, so a row covers both.
#
# Graphs: connected random graphs (a ring through every node keeps every distance finite and every degree at
# least two, so no clustering coefficient is 0 / 0). Weighted graphs carry six-decimal weights, read as
# distances by every path measure, as cograph, igraph and NetworkX do by default.

undirected <- ref_connected_graph(12, density = 0.3, seed = 31L)
undirected_weighted <- ref_connected_graph(12, density = 0.3, seed = 32L, weighted = TRUE, digits = 6L)
directed <- ref_connected_graph(12, density = 0.25, seed = 33L, directed = TRUE)
directed_weighted <- ref_connected_graph(12, density = 0.25, seed = 34L, directed = TRUE, weighted = TRUE, digits = 6L)
# Not strongly connected: some nodes reach only part of the network.
directed_sparse <- ref_graph(12, density = 0.15, seed = 35L, directed = TRUE)
karate <- ref_karate()

# Counts, or a few additions and divisions of doubles: agreement to the last bits is expected.
exact <- 1e-10

test_that("local transitivity equals igraph's transitivity(type = 'local') and NetworkX's clustering()", {
  skip_without_reference("igraph")
  nx_undirected <- ref_networkx(undirected, c(clustering = "nx.clustering(G)"))
  nx_karate <- ref_networkx(karate, c(clustering = "nx.clustering(G)"))
  # NetworkX's clustering() on a DiGraph counts directed triangles (Fagiolo 2007); cograph and igraph read a
  # directed network as undirected for this measure, so NetworkX gets the undirected view.
  nx_directed <- ref_networkx(directed, c(clustering = "nx.clustering(G.to_undirected())"))
  expect_reference(centrality_transitivity(undirected), nx_undirected$clustering, reference = "networkx",
                   covers = c("centrality()", "centrality_transitivity()"), quantity = "transitivity, local, undirected",
                   tolerance = exact)
  # Karate node 12 has one neighbour: its coefficient is 0 / 0, NaN in cograph by default (isolates = "nan")
  # and 0 in NetworkX; isolates = "zero" is the same convention as NetworkX's.
  expect_reference(centrality_transitivity(karate, isolates = "zero"), nx_karate$clustering, reference = "networkx",
                   covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, local, isolates zero, karate", tolerance = exact)
  expect_reference(centrality_transitivity(directed), nx_directed$clustering, reference = "networkx",
                   covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, local, directed read as undirected", tolerance = exact)
  expect_reference(centrality_transitivity(undirected),
                   ref_named(igraph::transitivity(ref_igraph(undirected), type = "local"), undirected),
                   reference = "igraph", covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, local, undirected", tolerance = exact)
  expect_reference(centrality_transitivity(directed),
                   ref_named(igraph::transitivity(ref_igraph(directed), type = "local"), directed),
                   reference = "igraph", covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, local, directed read as undirected", tolerance = exact)
})

test_that("Barrat's weighted transitivity equals igraph's transitivity(type = 'barrat')", {
  skip_without_reference("igraph")
  expect_reference(centrality_transitivity(undirected_weighted, transitivity_type = "barrat"),
                   ref_named(igraph::transitivity(ref_igraph(undirected_weighted), type = "barrat"), undirected_weighted),
                   reference = "igraph", covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, barrat, undirected weighted", tolerance = exact)
  expect_reference(centrality_transitivity(karate, transitivity_type = "barrat"),
                   ref_named(igraph::transitivity(ref_igraph(karate), type = "barrat"), karate),
                   reference = "igraph", covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, barrat, karate (unit weights)", tolerance = exact)
})

test_that("transitivity_type = 'onnela' is the Zhang-Horvath coefficient of W + t(W)", {
  # Zhang and Horvath (2005): C_i = sum_jk w_ij w_jk w_ki / ((sum_j w_ij)^2 - sum_j w_ij^2), weights in [0, 1].
  # cograph (as tna's Clustering) applies it to M = W + t(W), the symmetrised matrix of a directed network.
  zhang_horvath <- \(w) stats::setNames(diag(w %*% w %*% w) / (rowSums(w)^2 - rowSums(w^2)), rownames(w))
  expect_reference(centrality_transitivity(directed_weighted, transitivity_type = "onnela"),
                   zhang_horvath(directed_weighted + t(directed_weighted)),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Zhang-Horvath (2005) coefficient of W + t(W), base R",
                   covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, onnela, directed weighted", tolerance = exact)
  # On an undirected network W is already symmetric, so W + t(W) = 2 W, and the coefficient (cubic over
  # quadratic in the weights) doubles: a triangle of unit weights scores 2.
  # Owner, 2026-11-22: a decision, not a defect. tna's Clustering is the same formula and doubles the same way.
  expect_known_difference(centrality_transitivity(undirected_weighted, transitivity_type = "onnela"),
                          zhang_horvath(undirected_weighted),
                          reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Zhang-Horvath (2005) coefficient of W + t(W), base R",
                          covers = c("centrality()", "centrality_transitivity()"),
                          quantity = "transitivity, onnela, undirected weighted", tolerance = exact,
                          reason = paste("transitivity_type = 'onnela' is documented as the form tna uses: Zhang-Horvath",
                                         "(2005) on W + t(W), which is 2 W for a symmetric W, so the coefficient is twice",
                                         "Zhang-Horvath of W; a unit triangle scores 2, as in tna 1.5.7's Clustering."),
                          agrees_with = "tna")
  nx <- ref_networkx(undirected_weighted, c(clustering = "nx.clustering(G, weight='weight')"))
  expect_known_difference(centrality_transitivity(undirected_weighted, transitivity_type = "onnela"), nx$clustering,
                          reference = "networkx", covers = c("centrality()", "centrality_transitivity()"),
                          quantity = "transitivity, onnela, undirected weighted", tolerance = exact,
                          reason = paste("NetworkX's weighted clustering is Onnela et al. (2005): the geometric mean of",
                                         "the triangle's weights scaled by the largest weight. cograph's 'onnela' type",
                                         "is the Zhang-Horvath formula (tna's Clustering), a different coefficient."))
})

test_that("neighbourhood connectivity equals igraph's knn() and NetworkX's average_neighbor_degree()", {
  skip_without_reference("igraph")
  nx_undirected <- ref_networkx(undirected, c(knn = "nx.average_neighbor_degree(G)"))
  nx_weighted <- ref_networkx(undirected_weighted, c(knn = "nx.average_neighbor_degree(G, weight='weight')",
                                                     plain = "nx.average_neighbor_degree(G)"))
  nx_directed <- ref_networkx(directed, c(knn = "nx.average_neighbor_degree(G, source='out', target='out')"))
  expect_reference(centrality_neighborhood_connectivity(undirected), nx_undirected$knn, reference = "networkx",
                   covers = c("centrality()", "centrality_neighborhood_connectivity()"),
                   quantity = "neighborhood_connectivity, undirected", tolerance = exact)
  # cograph's neighbourhood connectivity ignores edge weights (Maslov and Sneppen 2002, as documented); NetworkX
  # and igraph weight it as Barrat et al. (2004), sum_j w_ij k_j / s_i, when asked to.
  expect_reference(centrality_neighborhood_connectivity(undirected_weighted), nx_weighted$plain, reference = "networkx",
                   covers = c("centrality()", "centrality_neighborhood_connectivity()"),
                   quantity = "neighborhood_connectivity, weighted network, weights ignored", tolerance = exact)
  expect_known_difference(centrality_neighborhood_connectivity(undirected_weighted), nx_weighted$knn,
                          reference = "networkx", covers = c("centrality()", "centrality_neighborhood_connectivity()"),
                          quantity = "neighborhood_connectivity, undirected weighted, Barrat weighting", tolerance = exact,
                          reason = paste("cograph ignores edge weights in neighbourhood connectivity (the mean degree of",
                                         "the neighbours); NetworkX with weight= computes Barrat et al.'s weighted",
                                         "average, sum_j w_ij k_j / s_i."),
                          agrees_with = "NetworkX average_neighbor_degree() without weight")
  expect_reference(centrality_neighborhood_connectivity(directed, mode = "out"), nx_directed$knn, reference = "networkx",
                   covers = c("centrality()", "centrality_neighborhood_connectivity()"),
                   quantity = "neighborhood_connectivity, mode out, directed", tolerance = exact)
  expect_reference(centrality_neighborhood_connectivity(undirected_weighted),
                   ref_named(igraph::knn(ref_igraph(undirected_weighted), weights = NA)$knn, undirected_weighted),
                   reference = "igraph", covers = c("centrality()", "centrality_neighborhood_connectivity()"),
                   quantity = "neighborhood_connectivity, weighted network, weights ignored", tolerance = exact)
  expect_reference(centrality_neighborhood_connectivity(directed, mode = "out"),
                   ref_named(igraph::knn(ref_igraph(directed), mode = "out", neighbor.degree.mode = "out")$knn, directed),
                   reference = "igraph", covers = c("centrality()", "centrality_neighborhood_connectivity()"),
                   quantity = "neighborhood_connectivity, mode out, directed", tolerance = exact)
})

test_that("local efficiency equals NetworkX's global efficiency of each neighbourhood and its definition", {
  # nx.local_efficiency() averages exactly these per-node values.
  nx <- ref_networkx(undirected, c(efficiency = "{v: nx.global_efficiency(G.subgraph(G[v])) for v in G}"))
  nx_karate <- ref_networkx(karate, c(efficiency = "{v: nx.global_efficiency(G.subgraph(G[v])) for v in G}"))
  expect_reference(centrality_local_efficiency(undirected), nx$efficiency, reference = "networkx",
                   covers = c("centrality()", "centrality_local_efficiency()"),
                   quantity = "local_efficiency, undirected", tolerance = exact)
  expect_reference(centrality_local_efficiency(karate), nx_karate$efficiency, reference = "networkx",
                   covers = c("centrality()", "centrality_local_efficiency()"),
                   quantity = "local_efficiency, karate", tolerance = exact)
  # NetworkX's efficiency has no weights; Latora and Marchiori's definition with weights read as lengths.
  expect_reference(centrality_local_efficiency(undirected_weighted), ref_a2_local_efficiency(undirected_weighted),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Latora and Marchiori (2001) local efficiency, base R",
                   covers = c("centrality()", "centrality_local_efficiency()"),
                   quantity = "local_efficiency, undirected weighted", tolerance = exact)
})

test_that("local and global reaching centrality equal NetworkX's", {
  calls <- c(local = "{v: nx.local_reaching_centrality(G, v) for v in G}",
             global = "nx.global_reaching_centrality(G)")
  on_undirected <- ref_networkx(undirected, calls)
  on_directed <- ref_networkx(directed_sparse, calls)
  on_weighted <- ref_networkx(undirected_weighted, c(
    raw = "{v: nx.local_reaching_centrality(G, v, weight='weight', normalized=False) for v in G}",
    normalized = "{v: nx.local_reaching_centrality(G, v, weight='weight') for v in G}"))
  expect_reference(centrality_reaching_local(undirected), on_undirected$local, reference = "networkx",
                   covers = c("centrality()", "centrality_reaching_local()"), quantity = "reaching_local, undirected",
                   tolerance = exact)
  expect_reference(centrality_reaching_local(directed_sparse, mode = "out"), on_directed$local, reference = "networkx",
                   covers = c("centrality()", "centrality_reaching_local()"), quantity = "reaching_local, mode out, directed",
                   tolerance = exact)
  # Weighted: shortest paths under the length total weight / w, then the mean weight along each path.
  expect_reference(centrality_reaching_local(undirected_weighted), on_weighted$raw, reference = "networkx",
                   covers = c("centrality()", "centrality_reaching_local()"),
                   quantity = "reaching_local, undirected weighted", tolerance = exact)
  expect_known_difference(centrality_reaching_local(undirected_weighted), on_weighted$normalized, reference = "networkx",
                          covers = c("centrality()", "centrality_reaching_local()"),
                          quantity = "reaching_local, undirected weighted, NetworkX normalized", tolerance = exact,
                          reason = paste("NetworkX's default normalized = True divides the weighted score by the mean",
                                         "edge weight; cograph reports the unnormalised score of Mones et al. (2012)."),
                          agrees_with = "NetworkX local_reaching_centrality(normalized = False)")
  expect_reference(reaching_global(undirected), on_undirected$global, reference = "networkx", covers = "reaching_global()",
                   quantity = "global reaching, undirected", tolerance = exact)
  expect_reference(reaching_global(directed_sparse, mode = "out"), on_directed$global, reference = "networkx",
                   covers = "reaching_global()", quantity = "global reaching, mode out, directed", tolerance = exact)
})

test_that("k-reach equals igraph's ego_size() and NetworkX's bounded shortest paths", {
  skip_without_reference("igraph")
  expect_reference(centrality_kreach(undirected),
                   ref_named(igraph::ego_size(ref_igraph(undirected), order = 3, mindist = 1), undirected),
                   reference = "igraph", covers = c("centrality()", "centrality_kreach()"),
                   quantity = "kreach, k = 3, undirected", tolerance = exact)
  expect_reference(centrality_kreach(directed, mode = "out", k = 2),
                   ref_named(igraph::ego_size(ref_igraph(directed), order = 2, mode = "out", mindist = 1), directed),
                   reference = "igraph", covers = c("centrality()", "centrality_kreach()"),
                   quantity = "kreach, k = 2, mode out, directed", tolerance = exact)
  nx <- ref_networkx(undirected_weighted, c(
    kreach = "{v: len(nx.single_source_dijkstra_path_length(G, v, cutoff=1.0, weight='weight')) - 1 for v in G}"))
  # On a weighted network the bound is on the weighted distance.
  expect_reference(centrality_kreach(undirected_weighted, k = 1), nx$kreach, reference = "networkx",
                   covers = c("centrality()", "centrality_kreach()"), quantity = "kreach, k = 1, undirected weighted",
                   tolerance = exact)
})

test_that("diversity equals igraph's diversity() and the normalised entropy of a node's edge weights", {
  skip_without_reference("igraph")
  expect_reference(centrality_diversity(undirected_weighted),
                   ref_named(igraph::diversity(ref_igraph(undirected_weighted)), undirected_weighted),
                   reference = "igraph", covers = c("centrality()", "centrality_diversity()"),
                   quantity = "diversity, undirected weighted", tolerance = exact)
  # Eagle, Macy and Claxton (2010): -sum p_ij log p_ij / log k_i, p_ij = w_ij / s_i.
  entropy <- ref_a2_per_node(undirected_weighted, \(i) {
    p <- undirected_weighted[i, undirected_weighted[i, ] != 0] / sum(undirected_weighted[i, ])
    -sum(p * log(p)) / log(length(p))
  })
  expect_reference(centrality_diversity(undirected_weighted), entropy,
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Eagle, Macy and Claxton (2010) diversity, base R",
                   covers = c("centrality()", "centrality_diversity()"), quantity = "diversity, undirected weighted",
                   tolerance = exact)
})

test_that("percolation centrality equals NetworkX's percolation_centrality()", {
  states <- stats::setNames(seq(0.1, 1, length.out = 12), rownames(undirected))
  on_undirected <- ref_networkx(undirected, c(
    percolation = sprintf("nx.percolation_centrality(G, states=%s)", ref_a2_py_dict(states))))
  on_directed <- ref_networkx(directed, c(
    percolation = sprintf("nx.percolation_centrality(G, states=%s)", ref_a2_py_dict(states))))
  on_equal <- ref_networkx(undirected, c(
    percolation = sprintf("nx.percolation_centrality(G, states=%s)", ref_a2_py_dict(states * 0 + 1))))
  expect_reference(centrality_percolation(undirected, states = states), on_undirected$percolation,
                   reference = "networkx", covers = c("centrality()", "centrality_percolation()"),
                   quantity = "percolation, graded states, undirected", tolerance = exact)
  expect_reference(centrality_percolation(directed, states = states), on_directed$percolation,
                   reference = "networkx", covers = c("centrality()", "centrality_percolation()"),
                   quantity = "percolation, graded states, directed", tolerance = exact)
  expect_reference(centrality_percolation(undirected), on_equal$percolation,
                   reference = "networkx", covers = c("centrality()", "centrality_percolation()"),
                   quantity = "percolation, default states, undirected", tolerance = exact)
})

test_that("ego betweenness equals NetworkX's betweenness of each node in its own ego network", {
  calls <- c(ego = "{v: nx.betweenness_centrality(nx.ego_graph(G, v, undirected=True), normalized=False)[v] for v in G}")
  on_undirected <- ref_networkx(undirected, calls)
  on_karate <- ref_networkx(karate, calls)
  on_directed <- ref_networkx(directed, calls)
  expect_reference(centrality_ego_betweenness(undirected), on_undirected$ego, reference = "networkx",
                   covers = c("centrality()", "centrality_ego_betweenness()"), quantity = "ego_betweenness, undirected",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_ego_betweenness(karate), on_karate$ego, reference = "networkx",
                   covers = c("centrality()", "centrality_ego_betweenness()"), quantity = "ego_betweenness, karate",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_ego_betweenness(directed), on_directed$ego, reference = "networkx",
                   covers = c("centrality()", "centrality_ego_betweenness()"), quantity = "ego_betweenness, directed",
                   tolerance = exact, measure = "scaled")
})

test_that("closeness vitality equals NetworkX's closeness_vitality() over ordered pairs", {
  # Brandes and Erlebach (2005): W(G) - W(G - v), W the sum of the distances over ordered pairs. NetworkX's
  # wiener_index() counts an unordered pair once on an undirected graph, so it is given the graph with both
  # arcs of every edge.
  ordered <- c(vitality = "nx.closeness_vitality(G.to_directed(), weight='weight')")
  on_weighted <- ref_networkx(undirected_weighted, ordered)
  on_karate <- ref_networkx(karate, ordered)
  expect_reference(centrality_closeness_vitality(undirected_weighted), on_weighted$vitality, reference = "networkx",
                   covers = c("centrality()", "centrality_closeness_vitality()"),
                   quantity = "closeness_vitality, undirected weighted", tolerance = exact, measure = "scaled")
  # Removing karate node 1 cuts off node 12, its only neighbour: NetworkX's Wiener index of the rest is then
  # infinite and its vitality -Inf, while cograph sums the finite distances only.
  expect_known_difference(centrality_closeness_vitality(karate), on_karate$vitality, reference = "networkx",
                          covers = c("centrality()", "centrality_closeness_vitality()"),
                          quantity = "closeness_vitality, karate", tolerance = exact, measure = "scaled",
                          reason = paste("Where removing the node disconnects the network (karate node 1), NetworkX",
                                         "returns -Inf; cograph sums the finite distances only. The other 33 nodes agree."))
  unordered <- ref_networkx(karate, c(vitality = "nx.closeness_vitality(G)"))
  expect_known_difference(centrality_closeness_vitality(karate), unordered$vitality, reference = "networkx",
                          covers = c("centrality()", "centrality_closeness_vitality()"),
                          quantity = "closeness_vitality, karate, unordered pairs", tolerance = exact, measure = "scaled",
                          reason = paste("On an undirected graph NetworkX's Wiener index counts each unordered pair once,",
                                         "so its closeness vitality is half cograph's, which sums over ordered pairs as",
                                         "Brandes and Erlebach (2005) define it."),
                          agrees_with = "NetworkX closeness_vitality(G.to_directed())")
})

test_that("k-path and geodesic k-path counts equal sna's kpath.census() and NetworkX's shortest-path counts", {
  skip_without_reference("sna")
  # sna::kpath.census(): the simple paths of length 1 to 3 each node lies on (Sade 1989), endpoints included.
  kpaths <- \(graph, mode) {
    census <- sna::kpath.census(graph, maxlen = 3, mode = mode, tabulate.by.vertex = TRUE,
                                path.comembership = "none", dyadic.tabulation = "none")
    ref_named(colSums(census$path.count[, -1L, drop = FALSE]), graph)
  }
  expect_reference(centrality_kpath(undirected), kpaths(undirected, "graph"), reference = "sna",
                   covers = c("centrality()", "centrality_kpath()"), quantity = "kpath, length 3, undirected",
                   tolerance = exact)
  expect_reference(centrality_kpath(directed, mode = "out"), kpaths(directed, "digraph"), reference = "sna",
                   covers = c("centrality()", "centrality_kpath()"), quantity = "kpath, length 3, mode out, directed",
                   tolerance = exact)
  # Borgatti and Everett (2006): the shortest paths of length at most 3 that start at the node, with multiplicity.
  calls <- c(geodesic = paste("{v: sum(len(list(nx.all_shortest_paths(G, v, t)))",
                              "for t, d in nx.single_source_shortest_path_length(G, v, cutoff=3).items() if d > 0)",
                              "for v in G}"))
  on_undirected <- ref_networkx(undirected, calls)
  on_directed <- ref_networkx(directed, calls)
  expect_reference(centrality_geodesic_kpath(undirected), on_undirected$geodesic, reference = "networkx",
                   covers = c("centrality()", "centrality_geodesic_kpath()"), quantity = "geodesic_kpath, k = 3, undirected",
                   tolerance = exact)
  expect_reference(centrality_geodesic_kpath(directed, mode = "out"), on_directed$geodesic, reference = "networkx",
                   covers = c("centrality()", "centrality_geodesic_kpath()"),
                   quantity = "geodesic_kpath, k = 3, mode out, directed", tolerance = exact)
})

test_that("collective influence and gravity equal their definitions", {
  expect_reference(centrality(undirected, measures = "collective_influence"),
                   ref_nodes(collective_influence_all = ref_a2_collective_influence(undirected, radius = 2)),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Morone and Makse (2015) collective influence, radius 2, base R",
                   covers = "centrality()", quantity = "collective_influence, undirected", tolerance = exact, key = "node")
  expect_reference(centrality(karate, measures = "collective_influence"),
                   ref_nodes(collective_influence_all = ref_a2_collective_influence(karate, radius = 2)),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Morone and Makse (2015) collective influence, radius 2, base R",
                   covers = "centrality()", quantity = "collective_influence, karate", tolerance = exact, key = "node")
  expect_reference(centrality_gravity(undirected), ref_a2_gravity(undirected, radius = 3),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Ma et al. (2016) gravity, k-shell masses, radius 3, base R",
                   covers = c("centrality()", "centrality_gravity()"), quantity = "gravity, undirected", tolerance = exact)
  expect_reference(centrality_gravity(karate), ref_a2_gravity(karate, radius = 3),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Ma et al. (2016) gravity, k-shell masses, radius 3, base R",
                   covers = c("centrality()", "centrality_gravity()"), quantity = "gravity, karate", tolerance = exact)
  # cograph's form before 2.4.8, kept for reproducibility: no mass on the focal node, degree times k-shell on
  # the partner, no radius.
  legacy_mass <- rowSums(ref_a2_binary(undirected)) * ref_a2_kcore(undirected)
  expect_reference(centrality_gravity(undirected, gravity_mass = "legacy", gravity_radius = NULL),
                   ref_a2_gravity(undirected, radius = Inf, mass_i = rep(1, 12), mass_j = legacy_mass),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "cograph's pre-2.4.8 gravity formula, base R",
                   covers = c("centrality()", "centrality_gravity()"), quantity = "gravity, legacy mass, undirected",
                   tolerance = exact)
})

test_that("the distance sums equal their definitions on weighted shortest distances", {
  distances <- ref_a2_distances(undirected_weighted)
  n <- nrow(distances)
  off <- row(distances) != col(distances)
  definition <- "independent: the local and distance measures written from their definitions in base R (each named in its note)"
  expect_reference(centrality_harary(undirected_weighted), rowSums(ifelse(off, 1 / distances^2, 0)),
                   reference = definition, note = "sum of 1 / d^2 over the other nodes (cograph's Harary centrality), on Floyd-Warshall distances",
                   covers = c("centrality()", "centrality_harary()"), quantity = "harary, undirected weighted",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_wiener(undirected_weighted), rowSums(distances),
                   reference = definition, note = "Wiener (1947) sum of distances, on Floyd-Warshall distances",
                   covers = c("centrality()", "centrality_wiener()"), quantity = "wiener, undirected weighted",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_generalized_closeness(undirected_weighted, decay_parameter = 0.3), rowSums(0.3^distances),
                   reference = definition, note = "sum of alpha^d over every node, the node itself included, on Floyd-Warshall distances",
                   covers = c("centrality()", "centrality_generalized_closeness()"),
                   quantity = "generalized_closeness, alpha 0.3, undirected weighted", tolerance = exact)
  expect_reference(centrality_centroid(undirected_weighted), ref_a2_centroid(distances),
                   reference = definition, note = "Koschuetzki et al. (2005) centroid value, on Floyd-Warshall distances",
                   covers = c("centrality()", "centrality_centroid()"), quantity = "centroid, undirected weighted",
                   tolerance = exact)
  # Dangalchev (2006) sums 2^-d over the OTHER nodes (cograph <= 2.7.19, as centiserve, added the node itself).
  dangalchev <- rowSums(ifelse(off, 2^-distances, 0))
  expect_reference(centrality_dangalchev(undirected_weighted), dangalchev,
                   reference = definition, note = "Dangalchev (2006) residual closeness, on Floyd-Warshall distances",
                   covers = c("centrality()", "centrality_dangalchev()"), quantity = "dangalchev, undirected weighted",
                   tolerance = exact)
  expect_reference(centrality_residual_closeness(undirected_weighted), dangalchev,
                   reference = definition, note = "Dangalchev (2006) residual closeness, on Floyd-Warshall distances",
                   covers = c("centrality()", "centrality_residual_closeness()"),
                   quantity = "residual_closeness, undirected weighted", tolerance = exact)
  # Average distance: the mean over the other nodes, sum / (n - 1) (cograph <= 2.7.19 divided by n + 1).
  expect_reference(centrality_average_distance(undirected_weighted), rowSums(distances) / (n - 1),
                   reference = definition, note = "mean distance to the other nodes, on Floyd-Warshall distances",
                   covers = c("centrality()", "centrality_average_distance()"),
                   quantity = "average_distance, undirected weighted", tolerance = exact)
  # Radiality (Valente and Foreman 1998): sum over the other nodes of (D + 1 - d) / (n - 1), D the diameter
  # (cograph <= 2.7.19 also summed the node itself, adding (D + 1) / (n - 1)).
  diameter <- max(distances)
  radiality <- rowSums(ifelse(off, diameter + 1 - distances, 0)) / (n - 1)
  expect_reference(centrality_radiality(undirected_weighted), radiality,
                   reference = definition, note = "Valente and Foreman (1998) radiality, on Floyd-Warshall distances",
                   covers = c("centrality()", "centrality_radiality()"), quantity = "radiality, undirected weighted",
                   tolerance = exact)
})

test_that("integration and the Gil-Schmidt index equal their definitions and sna's gilschmidt()", {
  skip_without_reference("sna")
  hops <- ref_a2_hops(undirected)
  n <- nrow(hops)
  diameter <- max(hops)
  # Valente and Foreman (1998): the reverse distances D + 1 - d(j, i) from the other nodes, over n - 1 (cograph
  # <= 2.7.19 followed tidygraph, 1 - (d - 1) / D summed over every node, the node itself included).
  expect_reference(centrality_integration(undirected),
                   rowSums(ifelse(row(hops) != col(hops), diameter + 1 - hops, 0)) / (n - 1),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Valente and Foreman (1998) integration, base R",
                   covers = c("centrality()", "centrality_integration()"), quantity = "integration, undirected",
                   tolerance = exact)
  # On a directed graph integration reads the incoming paths, d(j, i); an unreached pair contributes 0.
  sparse_directed <- ref_graph(12, density = 0.15, seed = 24L, directed = TRUE)
  in_hops <- ref_hop_distances(sparse_directed)
  in_diameter <- max(in_hops[is.finite(in_hops)])
  expect_reference(centrality_integration(sparse_directed, mode = "out"),
                   colSums(ifelse(row(in_hops) != col(in_hops) & is.finite(in_hops), in_diameter + 1 - in_hops, 0)) /
                     (nrow(in_hops) - 1),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Valente and Foreman (1998) integration over the paths into the node, base R",
                   covers = c("centrality()", "centrality_integration()"),
                   quantity = "integration, mode out, directed, not strongly connected", tolerance = exact)
  expect_reference(centrality_gilschmidt(undirected), ref_named(sna::gilschmidt(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_gilschmidt()"),
                   quantity = "gilschmidt, undirected", tolerance = exact)
  expect_reference(centrality_gilschmidt(directed, mode = "out"), ref_named(sna::gilschmidt(directed), directed),
                   reference = "sna", covers = c("centrality()", "centrality_gilschmidt()"),
                   quantity = "gilschmidt, mode out, directed", tolerance = exact)
  # Gil and Schmidt (1996): the mean of 1 / d over the nodes reached, |R(v)| (cograph <= 2.7.19 divided by n - 1,
  # which differs wherever a node does not reach every other).
  expect_reference(centrality_gilschmidt(sparse_directed, mode = "out"),
                   ref_named(sna::gilschmidt(sparse_directed, gmode = "digraph"), sparse_directed),
                   reference = "sna", covers = c("centrality()", "centrality_gilschmidt()"),
                   quantity = "gilschmidt, mode out, directed, not strongly connected", tolerance = exact)
  expect_reference(centrality_gilschmidt(undirected), rowSums(ifelse(hops > 0, 1 / hops, 0)) / (n - 1),
                   reference = "independent: the local and distance measures written from their definitions in base R (each named in its note)", note = "Gil and Schmidt (1996) power index (every node reached), base R",
                   covers = c("centrality()", "centrality_gilschmidt()"), quantity = "gilschmidt, undirected",
                   tolerance = exact)
})
