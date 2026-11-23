# The structural network_*() measures (girth, radius, vertex connectivity, cut vertices, bridges, clique size,
# global and local efficiency), estrada_index(), trophic_incoherence(), dispersion(), group_centrality() and
# reaching_global(). The network_*() functions call igraph for the computation (igraph::girth(), radius(),
# vertex_connectivity(), articulation_points(), bridges(), clique_num(), distances(); local efficiency on
# induced subgraphs since 2.7.20), so igraph is `internal` for them; NetworkX, brainGraph and the definitions are the
# counted references. estrada_index(), trophic_incoherence(), dispersion(), group_centrality() and
# reaching_global() are cograph's own code.
#
# Graphs: connected random graphs from helper-reference.R (six-decimal weights where weights are read, as
# distances), a sparse graph with cut vertices and bridges, an acyclic directed graph (trophic levels need basal
# nodes), and regulation_net.

undirected <- ref_connected_graph(12, density = 0.3, seed = 61L)
undirected_weighted <- ref_connected_graph(12, density = 0.3, seed = 62L, weighted = TRUE, digits = 6L)
directed_weighted <- ref_connected_graph(12, density = 0.25, seed = 63L, directed = TRUE, weighted = TRUE, digits = 6L)
directed <- ref_connected_graph(12, density = 0.2, seed = 64L, directed = TRUE)
sparse <- ref_graph(14, density = 0.17, seed = 65L)
acyclic <- local({
  graph <- ref_graph(10, density = 0.35, seed = 66L, directed = TRUE)
  graph[lower.tri(graph)] <- 0
  graph
})
reach_graph <- ref_graph(12, density = 0.12, seed = 67L, directed = TRUE)

exact <- 1e-10

test_that("network_girth() and network_radius() equal NetworkX", {
  on_undirected <- ref_networkx(undirected_weighted, c(girth = "nx.girth(G)", radius = "nx.radius(G, weight='weight')"))
  on_directed <- ref_networkx(directed_weighted, c(girth = "nx.girth(G.to_undirected())",
                                                   radius = "nx.radius(G, weight='weight')"))
  on_regulation <- ref_networkx(regulation_net, c(girth = "nx.girth(G.to_undirected())",
                                                  radius = "nx.radius(G, weight='weight')"))
  # The girth is the shortest cycle of the simple undirected graph: igraph::girth() ignores directions.
  expect_reference(c(undirected = network_girth(undirected_weighted), directed = network_girth(directed_weighted),
                     regulation_net = network_girth(regulation_net)),
                   c(undirected = on_undirected$girth, directed = on_directed$girth, regulation_net = on_regulation$girth),
                   reference = "networkx", covers = "network_girth()", quantity = "girth", tolerance = exact)
  # Weighted eccentricity, out-paths on a directed graph (NetworkX's eccentricity is the distance FROM a node).
  expect_reference(c(undirected = network_radius(undirected_weighted), directed = network_radius(directed_weighted),
                     regulation_net = network_radius(regulation_net)),
                   c(undirected = on_undirected$radius, directed = on_directed$radius,
                     regulation_net = on_regulation$radius),
                   reference = "networkx", covers = "network_radius()", quantity = "radius, weighted", tolerance = exact)
})

test_that("network_global_efficiency() and network_local_efficiency() equal NetworkX", {
  # Unweighted undirected: NetworkX's own global_efficiency(), and the documented local efficiency.
  on_unweighted <- ref_networkx(undirected, c(global = "nx.global_efficiency(G)", local = d1_py_local_efficiency,
                                              induced = "nx.local_efficiency(G)"))
  expect_reference(c(global = network_global_efficiency(undirected), local = network_local_efficiency(undirected)),
                   unlist(on_unweighted[c("global", "local")]), reference = "networkx",
                   covers = c("network_global_efficiency()", "network_local_efficiency()"),
                   quantity = "efficiency, undirected unweighted", tolerance = exact)
  # Latora & Marchiori (2001): the distances between a node's neighbours inside the subgraph they induce, as
  # NetworkX's own local_efficiency() (cograph 2.7.20; before, igraph's form in the graph without the node).
  expect_reference(network_local_efficiency(undirected), on_unweighted$induced, reference = "networkx",
                   covers = "network_local_efficiency()", quantity = "local efficiency, induced subgraphs",
                   tolerance = exact)
  # Weighted, and directed: the Latora-Marchiori definitions over NetworkX's Dijkstra distances.
  on_weighted <- ref_networkx(undirected_weighted, c(global = d1_py_global_efficiency, local = d1_py_local_efficiency))
  on_directed <- ref_networkx(directed_weighted, c(global = d1_py_global_efficiency, local = d1_py_local_efficiency))
  expect_reference(c(global = network_global_efficiency(undirected_weighted),
                     local = network_local_efficiency(undirected_weighted)),
                   unlist(on_weighted[c("global", "local")]), reference = "networkx",
                   covers = c("network_global_efficiency()", "network_local_efficiency()"),
                   quantity = "efficiency, undirected weighted", tolerance = exact)
  expect_reference(c(global = network_global_efficiency(directed_weighted),
                     local = network_local_efficiency(directed_weighted)),
                   unlist(on_directed[c("global", "local")]), reference = "networkx",
                   covers = c("network_global_efficiency()", "network_local_efficiency()"),
                   quantity = "efficiency, directed weighted", tolerance = exact)
})

test_that("network_global_efficiency() equals brainGraph's efficiency()", {
  skip_without_reference(c("brainGraph", "igraph"))
  expect_reference(network_global_efficiency(undirected),
                   brainGraph::efficiency(ref_igraph(undirected), type = "global"),
                   reference = "brainGraph", covers = "network_global_efficiency()",
                   quantity = "global efficiency, undirected unweighted", tolerance = exact)
})

test_that("efficiencies with weights = NA equal igraph's unweighted efficiencies (moved from bug 20)", {
  skip_without_reference("igraph")
  # A tna-shaped object: the efficiency functions read the class only, to invert weights by default.
  tna_like <- structure(list(weights = regulation_net, labels = rownames(regulation_net),
                             inits = rep(1 / nrow(regulation_net), nrow(regulation_net))), class = c("tna", "list"))
  graph <- ref_igraph(regulation_net)
  expect_reference(network_global_efficiency(tna_like, weights = NA), igraph::global_efficiency(graph, weights = NA),
                   reference = "igraph, called by cograph", covers = "network_global_efficiency()",
                   quantity = "global efficiency of tna input, weights = NA", tolerance = exact)
  on_regulation <- ref_networkx(d1_binary(regulation_net), c(global = d1_py_global_efficiency,
                                                             local = d1_py_local_efficiency))
  expect_reference(c(global = network_global_efficiency(tna_like, weights = NA),
                     local = network_local_efficiency(tna_like, weights = NA)),
                   unlist(on_regulation[c("global", "local")]), reference = "networkx",
                   covers = c("network_global_efficiency()", "network_local_efficiency()"),
                   quantity = "efficiency of tna input, weights = NA, NetworkX", tolerance = exact)
})

test_that("network_vertex_connectivity() equals NetworkX's node_connectivity()", {
  on_undirected <- ref_networkx(undirected, c(k = "nx.node_connectivity(G)"))
  on_directed <- ref_networkx(directed, c(k = "nx.node_connectivity(G)"))
  on_sparse <- ref_networkx(sparse, c(k = "nx.node_connectivity(G)"))
  expect_reference(c(undirected = network_vertex_connectivity(undirected),
                     directed = network_vertex_connectivity(directed), sparse = network_vertex_connectivity(sparse)),
                   c(undirected = on_undirected$k, directed = on_directed$k, sparse = on_sparse$k),
                   reference = "networkx", covers = "network_vertex_connectivity()", quantity = "vertex connectivity",
                   tolerance = exact)
})

test_that("network_cut_vertices() and network_bridges() equal NetworkX", {
  nx <- ref_networkx(sparse, c(cut = "sorted(nx.articulation_points(G))",
                               bridges = "[f'{u}|{v}' for u, v in nx.bridges(G)]"))
  nodes <- rownames(sparse)
  expect_reference(data.frame(node = nodes, cut_vertex = nodes %in% network_cut_vertices(sparse)),
                   data.frame(node = nodes, cut_vertex = nodes %in% unlist(nx$cut)),
                   reference = "networkx", covers = "network_cut_vertices()", quantity = "cut vertex, sparse graph",
                   tolerance = 0, key = "node")
  expect_reference(c(cut_vertices = network_cut_vertices(sparse, count_only = TRUE),
                     bridges = network_bridges(sparse, count_only = TRUE)),
                   c(cut_vertices = length(unlist(nx$cut)), bridges = length(unlist(nx$bridges))),
                   reference = "networkx", covers = c("network_cut_vertices()", "network_bridges()"),
                   quantity = "number of cut vertices and bridges, sparse graph", tolerance = 0)
  # Each bridge as an unordered pair, listed from the node that comes first in the matrix.
  found <- network_bridges(sparse)
  bridge_names <- unlist(nx$bridges)
  expect_reference(d1_edge_table(stats::setNames(rep(1, nrow(found)), paste(found$from, found$to, sep = "|")),
                                 sparse, directed = FALSE, column = "bridge"),
                   d1_edge_table(stats::setNames(rep(1, length(bridge_names)), bridge_names), sparse,
                                 directed = FALSE, column = "bridge"),
                   reference = "networkx", covers = "network_bridges()", quantity = "bridge, sparse graph",
                   tolerance = 0, key = c("from", "to"))
})

test_that("network_clique_size() equals NetworkX's largest maximal clique", {
  nx <- ref_networkx(undirected, c(k = "max(len(c) for c in nx.find_cliques(G))"))
  on_directed <- ref_networkx(directed, c(k = "max(len(c) for c in nx.find_cliques(G.to_undirected()))"))
  # student_interactions is a directed multigraph with self-loops (an edge list); its adjacency is built in base R.
  people <- sort(unique(c(student_interactions$from, student_interactions$to)))
  interactions <- unclass(table(factor(student_interactions$from, people), factor(student_interactions$to, people)))
  interactions <- matrix(as.numeric(interactions), length(people), dimnames = list(people, people))
  on_students <- ref_networkx(interactions, c(k = "max(len(c) for c in nx.find_cliques(G.to_undirected()))"),
                              directed = TRUE)
  expect_reference(c(undirected = network_clique_size(undirected), directed = network_clique_size(directed),
                     student_interactions = network_clique_size(student_interactions)),
                   c(undirected = nx$k, directed = on_directed$k, student_interactions = on_students$k),
                   reference = "networkx", covers = "network_clique_size()", quantity = "clique number",
                   tolerance = 0)
})

test_that("estrada_index() equals NetworkX and trace(exp(A))", {
  nx <- ref_networkx(undirected, c(estrada = "nx.estrada_index(G)"))
  expect_reference(estrada_index(undirected), nx$estrada, reference = "networkx", covers = "estrada_index()",
                   quantity = "Estrada index, undirected", tolerance = exact, measure = "relative")
  # Directed: the trace of the matrix exponential, by its Taylor series (moved from bug 19). On the directed
  # 3-cycle A^3 = I, so trace(A^k) is 3 when k is a multiple of 3 and 0 otherwise.
  cycle <- matrix(0, 3, 3)
  cycle[cbind(1:3, c(2, 3, 1))] <- 1
  taylor_trace <- \(a, terms) sum(diag(Reduce(`+`, Reduce(\(term, k) term %*% a / k, seq_len(terms),
                                                          accumulate = TRUE, init = diag(nrow(a))))))
  expect_reference(c(cycle = estrada_index(cycle), directed = estrada_index(directed)),
                   c(cycle = 3 * sum(1 / factorial(seq(0, 30, by = 3))), directed = taylor_trace(directed, 80L)),
                   reference = "independent: trace of exp(A), Taylor series (Estrada 2000)", covers = "estrada_index()",
                   quantity = "Estrada index, directed", tolerance = 1e-12, measure = "relative")
  # Undirected: the sum of exp of the eigenvalues of the symmetric adjacency (moved from bug 19).
  triangle <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3)
  expect_reference(estrada_index(triangle),
                   sum(exp(eigen(triangle, symmetric = TRUE, only.values = TRUE)$values)),
                   reference = "independent: sum of exp(eigenvalues) of the adjacency (Estrada 2000)",
                   covers = "estrada_index()", quantity = "Estrada index, triangle", tolerance = exact,
                   measure = "relative")
})

test_that("trophic_incoherence() equals NetworkX's trophic_incoherence_parameter()", {
  nx <- ref_networkx(acyclic, c(q = "nx.trophic_incoherence_parameter(G, cannibalism=False)"))
  expect_reference(trophic_incoherence(acyclic), nx$q, reference = "networkx", covers = "trophic_incoherence()",
                   quantity = "trophic incoherence, acyclic directed", tolerance = exact)
})

test_that("dispersion() equals NetworkX's dispersion()", {
  nx <- ref_networkx(undirected, c(
    normalized = "{f'{u}|{v}': d for u, dd in nx.dispersion(G).items() for v, d in dd.items()}",
    raw = "{f'{u}|{v}': d for u, dd in nx.dispersion(G, normalized=False).items() for v, d in dd.items()}"))
  table_of <- \(result, column) data.frame(from = rownames(undirected)[result$from],
                                           to = rownames(undirected)[result$to], value = result[[column]])
  expected <- d1_edge_table(nx$normalized, undirected, directed = TRUE, column = "value")
  expect_reference(table_of(dispersion(undirected), "dispersion"), expected, reference = "networkx",
                   covers = "dispersion()", quantity = "dispersion, normalised", tolerance = exact,
                   key = c("from", "to"))
  expect_reference(table_of(dispersion(undirected, normalized = FALSE), "dispersion"),
                   d1_edge_table(nx$raw, undirected, directed = TRUE, column = "value"), reference = "networkx",
                   covers = "dispersion()", quantity = "dispersion, raw", tolerance = exact, key = c("from", "to"))
})

test_that("group_centrality() equals NetworkX's group centralities", {
  group <- c("N1", "N5", "N9")
  py_group <- "['N1', 'N5', 'N9']"
  nx <- ref_networkx(undirected, c(
    closeness = sprintf("nx.group_closeness_centrality(G, %s)", py_group),
    degree = sprintf("nx.group_degree_centrality(G, %s)", py_group)))
  on_directed <- ref_networkx(directed, c(
    closeness = sprintf("nx.group_closeness_centrality(G, %s)", py_group),
    out_degree = sprintf("nx.group_out_degree_centrality(G, %s)", py_group),
    in_degree = sprintf("nx.group_in_degree_centrality(G, %s)", py_group)))
  # Group betweenness is not compared with NetworkX: nx.group_betweenness_centrality() 3.7 depends on Python's
  # string-hash order. On the ci machine 40 calls on one graph returned 0.048611 37 times (cograph's value) and
  # 0.0625, 0.076389, 0.090278 once each (probe of 2026-11-20). Group betweenness is checked against its
  # definition in test-centrality-zoo-a-betweenness.R.
  measures <- c("closeness", "degree")
  expect_reference(vapply(measures, \(m) group_centrality(undirected, group, measure = m), numeric(1)),
                   unlist(nx[measures]), reference = "networkx", covers = "group_centrality()",
                   quantity = "group centrality, undirected", tolerance = exact)
  expect_reference(c(closeness = group_centrality(directed, group, measure = "closeness"),
                     out_degree = group_centrality(directed, group, measure = "degree", mode = "out"),
                     in_degree = group_centrality(directed, group, measure = "degree", mode = "in")),
                   unlist(on_directed[c("closeness", "out_degree", "in_degree")]),
                   reference = "networkx", covers = "group_centrality()", quantity = "group centrality, directed",
                   tolerance = exact)
})

test_that("reaching_global() equals NetworkX's global_reaching_centrality()", {
  # Unweighted: the local reaching centrality of v is the share of the other nodes v reaches by out-paths.
  nx <- ref_networkx(reach_graph, c(grc = "nx.global_reaching_centrality(G)"))
  expect_reference(reaching_global(reach_graph, mode = "out"), nx$grc, reference = "networkx",
                   covers = "reaching_global()", quantity = "global reaching centrality, directed, out-paths",
                   tolerance = exact)
  on_sparse <- ref_networkx(sparse, c(grc = "nx.global_reaching_centrality(G)"))
  expect_reference(reaching_global(sparse), on_sparse$grc, reference = "networkx", covers = "reaching_global()",
                   quantity = "global reaching centrality, undirected", tolerance = exact)
})
