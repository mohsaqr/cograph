# network_summary(): every column it reports, compared with NetworkX (a separate implementation in Python), sna
# and the definitions. network_summary() computes almost every column with igraph (R/network-summary.R calls
# igraph::edge_density(), diameter(), mean_distance(), min_cut(), centr_*(), reciprocity(), assortativity_degree(),
# girth(), radius(), ...), so igraph is `internal` here and the counted evidence is NetworkX, sna and the
# definitions. Transitivity is cograph's own kernel.
#
# Graphs: connected random graphs from helper-reference.R (a ring through every node keeps every distance finite
# and a directed graph strongly connected), with six-decimal weights so that two weighted paths are almost never of
# equal length; and the bundled regulation_net (10 nodes, directed, weighted, three reciprocated pairs), where the
# girth and reciprocity findings were made. Weights are read as distances, as igraph and NetworkX (`weight=`) do.
# digits = NULL: the default rounds every column to three decimals.

undirected_weighted <- ref_connected_graph(12, density = 0.3, seed = 41L, weighted = TRUE, digits = 6L)
directed_weighted <- ref_connected_graph(12, density = 0.25, seed = 42L, directed = TRUE, weighted = TRUE, digits = 6L)
undirected <- d1_binary(undirected_weighted)
directed <- d1_binary(directed_weighted)

# Counts, or a few additions and divisions of doubles: agreement to the last bits is expected.
exact <- 1e-10
# Eigenvector and PageRank values come out of an eigen-solver (ARPACK, numpy's LAPACK) or a power iteration in each
# program; 1e-8 allows for their stopping rules.
spectral <- 1e-8

columns_of <- function(summary, columns) unlist(summary[columns])

test_that("network_summary() of an undirected weighted network equals NetworkX", {
  summary <- network_summary(undirected_weighted, digits = NULL, extended = TRUE, detailed = TRUE)
  nx <- ref_networkx(undirected_weighted, c(
    node_count = "G.number_of_nodes()", edge_count = "G.number_of_edges()", density = "nx.density(G)",
    component_count = "nx.number_connected_components(G)",
    diameter = "nx.diameter(G, weight='weight')",
    mean_distance = "nx.average_shortest_path_length(G, weight='weight')",
    min_cut = "nx.edge_connectivity(G)",
    transitivity = "nx.transitivity(G)",
    assortativity_degree = "nx.degree_assortativity_coefficient(G)",
    girth = "nx.girth(G)",
    radius = "nx.radius(G, weight='weight')",
    vertex_connectivity = "nx.node_connectivity(G)",
    largest_clique_size = "max(len(c) for c in nx.find_cliques(G))",
    cut_vertex_count = "len(list(nx.articulation_points(G)))",
    bridge_count = "len(list(nx.bridges(G)))",
    global_efficiency = d1_py_global_efficiency,
    local_efficiency = d1_py_local_efficiency,
    mean_degree = "float(np.mean([d for _, d in G.degree()]))",
    sd_degree = "float(np.std([d for _, d in G.degree()], ddof=1))",
    median_degree = "float(np.median([d for _, d in G.degree()]))",
    mean_strength = "float(np.mean([d for _, d in G.degree(weight='weight')]))",
    sd_strength = "float(np.std([d for _, d in G.degree(weight='weight')], ddof=1))",
    mean_betweenness = "float(np.mean(list(nx.betweenness_centrality(G, normalized=False, weight='weight').values())))",
    # igraph's closeness() is 1 / (sum of distances), not normalised
    mean_closeness = "float(np.mean([1 / sum(nx.single_source_dijkstra_path_length(G, v, weight='weight').values()) for v in G]))",
    mean_local_transitivity = "float(np.mean(list(nx.clustering(G).values())))",
    mean_constraint = "float(np.mean(list(nx.constraint(G, weight='weight').values())))"))
  counted <- c("node_count", "edge_count", "density", "component_count", "diameter", "mean_distance", "min_cut",
               "transitivity", "assortativity_degree", "girth", "radius", "vertex_connectivity",
               "largest_clique_size", "cut_vertex_count", "bridge_count", "global_efficiency", "local_efficiency",
               "mean_degree", "sd_degree", "median_degree", "mean_strength", "sd_strength", "mean_betweenness",
               "mean_closeness", "mean_local_transitivity", "mean_constraint")
  expect_reference(columns_of(summary, counted), unlist(nx[counted]), reference = "networkx",
                   covers = "network_summary()", quantity = "network summary, undirected weighted",
                   tolerance = exact, measure = "scaled")
  # Eigenvector centrality scaled to a maximum of 1, as igraph's eigen_centrality() does.
  spectral_nx <- ref_networkx(undirected_weighted, c(
    mean_eigenvector = "(lambda c: float(np.mean([x / max(c.values()) for x in c.values()])))(nx.eigenvector_centrality_numpy(G, weight='weight'))",
    mean_pagerank = "float(np.mean(list(nx.pagerank(G, weight='weight', tol=1e-14).values())))"))
  expect_reference(columns_of(summary, c("mean_eigenvector", "mean_pagerank")),
                   unlist(spectral_nx[c("mean_eigenvector", "mean_pagerank")]), reference = "networkx",
                   covers = "network_summary()", quantity = "network summary, spectral means, undirected weighted",
                   tolerance = spectral)
})

test_that("network_summary() of a directed weighted network equals NetworkX", {
  summary <- network_summary(directed_weighted, digits = NULL, extended = TRUE, detailed = TRUE)
  # Distances follow the arcs (out-paths). Girth, cliques, cut vertices, bridges and transitivity are read on the
  # undirected skeleton, as igraph reads a directed graph for them; degree is in + out.
  nx <- ref_networkx(directed_weighted, c(
    node_count = "G.number_of_nodes()", edge_count = "G.number_of_edges()", density = "nx.density(G)",
    component_count = "nx.number_weakly_connected_components(G)",
    diameter = "nx.diameter(G, weight='weight')",
    mean_distance = "nx.average_shortest_path_length(G, weight='weight')",
    min_cut = "nx.edge_connectivity(G)",
    transitivity = "nx.transitivity(G.to_undirected())",
    assortativity_degree = "nx.degree_assortativity_coefficient(G)",
    girth = "nx.girth(G.to_undirected())",
    radius = "nx.radius(G, weight='weight')",
    vertex_connectivity = "nx.node_connectivity(G)",
    largest_clique_size = "max(len(c) for c in nx.find_cliques(G.to_undirected()))",
    cut_vertex_count = "len(list(nx.articulation_points(G.to_undirected())))",
    bridge_count = "len(list(nx.bridges(G.to_undirected())))",
    global_efficiency = d1_py_global_efficiency,
    local_efficiency = d1_py_local_efficiency,
    mean_degree = "float(np.mean([d for _, d in G.degree()]))",
    sd_degree = "float(np.std([d for _, d in G.degree()], ddof=1))",
    median_degree = "float(np.median([d for _, d in G.degree()]))",
    mean_strength = "float(np.mean([d for _, d in G.degree(weight='weight')]))",
    sd_strength = "float(np.std([d for _, d in G.degree(weight='weight')], ddof=1))",
    mean_betweenness = "float(np.mean(list(nx.betweenness_centrality(G, normalized=False, weight='weight').values())))",
    mean_local_transitivity = "float(np.mean(list(nx.clustering(G.to_undirected()).values())))",
    mean_constraint = "float(np.mean(list(nx.constraint(G, weight='weight').values())))"))
  counted <- setdiff(names(nx), "networkx")
  expect_reference(columns_of(summary, counted), unlist(nx[counted]), reference = "networkx",
                   covers = "network_summary()", quantity = "network summary, directed weighted",
                   tolerance = exact, measure = "scaled")
})

test_that("network_summary() centralizations equal sna and Freeman's definition", {
  skip_without_reference("sna")
  on_undirected <- network_summary(undirected, digits = NULL)
  on_directed <- network_summary(directed, digits = NULL)
  # sna reads the matrix as unweighted, as igraph's centr_betw() and centr_clo() do. Closeness centralization is
  # taken over the undirected skeleton (mode = "all").
  skeleton <- ((directed + t(directed)) != 0) * 1
  expect_reference(c(betweenness = on_undirected$centralization_betweenness,
                     closeness = on_undirected$centralization_closeness),
                   c(betweenness = sna::centralization(undirected, sna::betweenness, mode = "graph"),
                     closeness = sna::centralization(undirected, sna::closeness, mode = "graph")),
                   reference = "sna", covers = "network_summary()", quantity = "centralization, undirected",
                   tolerance = exact)
  expect_reference(c(betweenness = on_directed$centralization_betweenness,
                     closeness = on_directed$centralization_closeness),
                   c(betweenness = sna::centralization(directed, sna::betweenness, mode = "digraph"),
                     closeness = sna::centralization(skeleton, sna::closeness, mode = "graph")),
                   reference = "sna", covers = "network_summary()", quantity = "centralization, directed",
                   tolerance = exact)
})

test_that("network_summary() eigenvector centralization: directed = TRUE, against igraph's default", {
  skip_without_reference("igraph")
  regulation <- network_summary(regulation_net, digits = NULL)
  graph <- ref_igraph(regulation_net)
  expect_reference(regulation$centralization_eigen, d1_eigen_centralization(regulation_net, directed = TRUE),
                   reference = "independent: in-eigenvector of the 0/1 adjacency, scaled to max 1, over n - 1",
                   covers = "network_summary()", quantity = "eigenvector centralization, regulation_net",
                   tolerance = spectral)
  expect_reference(regulation$centralization_eigen, igraph::centr_eigen(graph, directed = TRUE)$centralization,
                   reference = "igraph, called by cograph", covers = "network_summary()",
                   quantity = "eigenvector centralization, regulation_net, directed = TRUE", tolerance = spectral)
  expect_known_difference(regulation$centralization_eigen, igraph::centr_eigen(graph)$centralization,
                          reference = "igraph, called by cograph", covers = "network_summary()",
                          quantity = "eigenvector centralization, regulation_net, igraph default", tolerance = spectral,
                          reason = paste("network_summary() calls centr_eigen(directed = TRUE) on a directed network;",
                                         "igraph's default directed = FALSE reads the arcs as undirected edges."),
                          agrees_with = "igraph::centr_eigen(directed = TRUE)")
})

test_that("network_summary() degree centralization uses igraph's maximum for a graph with self-loops", {
  skip_without_reference("igraph")
  on_undirected <- network_summary(undirected, digits = NULL)
  on_directed <- network_summary(directed, digits = NULL)
  n <- nrow(undirected)
  degree_u <- rowSums(undirected)
  degree_all <- rowSums(directed) + colSums(directed)
  # Freeman (1979): the maximum is a star without self-loops: (n - 1)(n - 2) undirected, 2 (n - 1)(n - 2) for
  # in + out degree, (n - 1)^2 for in- or out-degree.
  freeman <- c(undirected = d1_freeman(degree_u, (n - 1) * (n - 2)),
               all = d1_freeman(degree_all, 2 * (n - 1) * (n - 2)),
               `in` = d1_freeman(colSums(directed), (n - 1)^2), out = d1_freeman(rowSums(directed), (n - 1)^2))
  reported <- c(undirected = on_undirected$centralization_degree, all = on_directed$centralization_degree,
                `in` = on_directed$centralization_in_degree, out = on_directed$centralization_out_degree)
  expect_known_difference(reported, freeman,
                          reference = "independent: Freeman degree centralization, maximum of a star without loops",
                          covers = "network_summary()", quantity = "degree centralization", tolerance = exact,
                          reason = paste("network_summary() calls igraph::centr_degree() with its default loops = TRUE,",
                                         "whose maximum counts self-loops: n (n - 1) undirected and for in/out,",
                                         "2 (n - 1)^2 for in + out."),
                          agrees_with = "igraph::centr_degree(loops = TRUE)")
  expect_reference(reported,
                   c(undirected = igraph::centr_degree(ref_igraph(undirected))$centralization,
                     all = igraph::centr_degree(ref_igraph(directed), mode = "all")$centralization,
                     `in` = igraph::centr_degree(ref_igraph(directed), mode = "in")$centralization,
                     out = igraph::centr_degree(ref_igraph(directed), mode = "out")$centralization),
                   reference = "igraph, called by cograph", covers = "network_summary()",
                   quantity = "degree centralization, loops = TRUE", tolerance = exact)
})

test_that("network_summary() reciprocity is the dyadic ratio of sna, not NetworkX's edgewise reciprocity", {
  skip_without_reference("sna")
  summary <- network_summary(regulation_net, digits = NULL)
  expect_reference(summary$reciprocity, unname(sna::grecip(regulation_net, measure = "dyadic.nonnull")),
                   reference = "sna", covers = "network_summary()",
                   quantity = "reciprocity, mutual pairs over connected pairs, regulation_net", tolerance = exact)
  nx <- ref_networkx(regulation_net, c(reciprocity = "nx.overall_reciprocity(G)"))
  expect_known_difference(summary$reciprocity, nx$reciprocity, reference = "networkx",
                          covers = "network_summary()", quantity = "reciprocity, regulation_net", tolerance = exact,
                          reason = paste("network_summary() reports igraph::reciprocity(mode = 'ratio'), mutual pairs",
                                         "over connected pairs (3 / 27); NetworkX and dyad_census() report the share of",
                                         "arcs that are reciprocated, 2M / (2M + A) (6 / 30)."))
})

test_that("network_summary() girth and radius on regulation_net equal NetworkX", {
  skip_without_reference("igraph")
  summary <- network_summary(regulation_net, digits = NULL, extended = TRUE)
  nx <- ref_networkx(regulation_net, c(girth = "nx.girth(G.to_undirected())", radius = "nx.radius(G, weight='weight')"))
  # The girth is the shortest cycle of the simple undirected graph (igraph::girth() ignores directions); a
  # reciprocated pair is one edge there, not a cycle of length 2.
  expect_reference(summary$girth, nx$girth, reference = "networkx", covers = "network_summary()",
                   quantity = "girth, regulation_net", tolerance = exact,
                   defect = paste("cograph network_summary(extended = TRUE) reports girth 2 on regulation_net:",
                                  "igraph::girth() of the simplified directed graph counts a reciprocated pair as a",
                                  "2-cycle; NetworkX gives 3, as cograph's own network_girth() does; FINDINGS.md"))
  # Out-eccentricity on the weighted arcs: NetworkX's eccentricity on a DiGraph is the largest distance FROM a node.
  expect_reference(summary$radius, nx$radius, reference = "networkx", covers = "network_summary()",
                   quantity = "radius, regulation_net", tolerance = exact)
  graph <- ref_igraph(regulation_net)
  expect_reference(summary$radius, igraph::radius(graph, mode = "out"), reference = "igraph, called by cograph", covers = "network_summary()", quantity = "radius, regulation_net, mode out", tolerance = exact)
  expect_known_difference(summary$radius, igraph::radius(graph), reference = "igraph, called by cograph", covers = "network_summary()", quantity = "radius, regulation_net, igraph default",
                          tolerance = exact,
                          reason = paste("network_summary() and network_radius() take out-paths on a directed network",
                                         "(radius(mode = 'out')); igraph's default mode = 'all' ignores directions."),
                          agrees_with = "igraph::radius(mode = 'out')")
})
