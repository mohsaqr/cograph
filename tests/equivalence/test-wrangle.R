# Structural wrangling verbs: reverse_edges(), to_directed(), complement_network(), contract_nodes(),
# project_bipartite(), simplify() and its methods, to_matrix(), split_components(), select_k_core(),
# spanning_tree(), select_component(), select_top(), select_top_edges(), select_bridges(), and the node
# predicates and measures used through select_nodes() and mutate_nodes(). cograph computes these in base R; the
# references are NetworkX (a separate implementation in Python), igraph where cograph does not call it for the
# verb, and the definition where neither names the operation.

directed <- ref_graph(8, density = 0.35, seed = 91L, directed = TRUE, weighted = TRUE, digits = 6L)
undirected <- ref_connected_graph(10, density = 0.25, seed = 92L, weighted = TRUE, digits = 6L)
binary <- (ref_graph(8, density = 0.4, seed = 93L) != 0) * 1
# Two components (7 and 4 nodes): triangles joined by bridges, with pendant nodes N7 and N11. Cut vertices
# N3, N4, N6, N10; bridges N3-N4, N6-N7, N10-N11; 2-core everything but the pendants. Weights all differ, so
# the minimum spanning forest is unique.
two_parts <- ref_edgelist_matrix(
  from = c("N1", "N2", "N1", "N3", "N4", "N5", "N6", "N6", "N8", "N9", "N10", "N10"),
  to = c("N2", "N3", "N3", "N4", "N5", "N6", "N4", "N7", "N9", "N10", "N8", "N11"),
  weight = c(0.31, 0.52, 0.47, 0.66, 0.28, 0.73, 0.19, 0.84, 0.45, 0.62, 0.37, 0.58),
  nodes = paste0("N", 1:11), directed = FALSE)
nodes_of <- \(graph) rownames(graph)
as_matrix_of <- \(values, graph) ref_square(values, nodes_of(graph))

# Integers and a few sums of weights: rounding only.
exact <- 1e-12

test_that("reverse_edges(), to_directed() and complement_network() equal NetworkX's reverse(), to_directed() and complement()", {
  flat <- \(expression) sprintf("nx.to_numpy_array(%s, nodelist=%s, weight='weight').flatten().tolist()", expression,
                                py_strings(nodes_of(directed)))
  nx_directed <- ref_networkx(directed, c(reverse = flat("G.reverse()"), complement = flat("nx.complement(G)")))
  nx_binary <- ref_networkx(binary, c(complement = sprintf(
    "nx.to_numpy_array(nx.complement(G), nodelist=%s).flatten().tolist()", py_strings(nodes_of(binary)))))
  nx_undirected <- ref_networkx(undirected, c(directed = sprintf(
    "nx.to_numpy_array(G.to_directed(), nodelist=%s, weight='weight').flatten().tolist()",
    py_strings(nodes_of(undirected)))))
  expect_reference(to_matrix(reverse_edges(directed)), as_matrix_of(nx_directed$reverse, directed),
                   reference = "networkx", covers = "reverse_edges()", quantity = "reversed arcs, weighted",
                   tolerance = 0)
  expect_reference(to_matrix(complement_network(directed)), as_matrix_of(nx_directed$complement, directed),
                   reference = "networkx", covers = "complement_network()", quantity = "complement, directed",
                   tolerance = 0)
  expect_reference(to_matrix(complement_network(binary)), as_matrix_of(nx_binary$complement, binary),
                   reference = "networkx", covers = "complement_network()", quantity = "complement, undirected",
                   tolerance = 0)
  expect_reference(to_matrix(to_directed(undirected, mode = "mutual")), as_matrix_of(nx_undirected$directed, undirected),
                   reference = "networkx", covers = "to_directed()", quantity = "both arcs of every edge", tolerance = 0)
})

test_that("complement_network() and contract_nodes() equal igraph's complementer() and contract()", {
  skip_without_reference("igraph")
  complement <- igraph::as_adjacency_matrix(igraph::complementer(ref_igraph(binary), loops = FALSE), sparse = FALSE)
  expect_reference(to_matrix(complement_network(binary)), complement, reference = "igraph",
                   covers = "complement_network()", quantity = "complement, undirected", tolerance = 0)
  groups <- list(a = c("N1", "N2", "N3"), b = c("N4", "N5"), c = c("N6", "N7", "N8"))
  mapping <- rep(seq_along(groups), lengths(groups))[match(rownames(directed), unlist(groups))]
  lapply(c("sum", "mean", "max", "min"), \(method) {
    merged <- igraph::simplify(igraph::contract(ref_igraph(directed, directed = TRUE), mapping, vertex.attr.comb = "ignore"),
                               remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = method))
    contracted <- igraph::as_adjacency_matrix(merged, attr = "weight", sparse = FALSE)
    dimnames(contracted) <- list(names(groups), names(groups))
    expect_reference(to_matrix(contract_nodes(directed, groups = groups, weight = method)), contracted,
                     reference = "igraph", covers = "contract_nodes()",
                     quantity = sprintf("contracted arcs, %s of merged weights", method), tolerance = exact)
  })
})

test_that("project_bipartite() equals NetworkX's bipartite projections and igraph's multiplicity", {
  incidence <- matrix(c(1, 0, 2, 1, 0, 1, 1, 1, 0, 0, 3, 1, 0, 2, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1), 6, 4,
                      dimnames = list(paste0("P", 1:6), paste0("E", 1:4)))
  people <- rownames(incidence)
  both <- c(people, colnames(incidence))
  bipartite <- matrix(0, 10, 10, dimnames = list(both, both))
  bipartite[people, colnames(incidence)] <- incidence
  bipartite[colnames(incidence), people] <- t(incidence)
  flat <- \(projection) sprintf("nx.to_numpy_array(%s, nodelist=%s, weight='weight').flatten().tolist()", projection,
                                py_strings(people))
  nx <- ref_networkx(bipartite, c(
    shared = flat(sprintf("nx.bipartite.weighted_projected_graph(G, %s)", py_strings(people))),
    newman = flat(sprintf("nx.bipartite.collaboration_weighted_projected_graph(G, %s)", py_strings(people))),
    jaccard = flat(sprintf("nx.bipartite.overlap_weighted_projected_graph(G, %s)", py_strings(people)))))
  # number of shared neighbours; Newman (2001) sum of 1 / (k - 1); Jaccard |N(u) & N(v)| / |N(u) | N(v)|
  lapply(c(binary = "shared", newman = "newman", jaccard = "jaccard"), \(name) {
    method <- c(shared = "binary", newman = "newman", jaccard = "jaccard")[[name]]
    expect_reference(project_bipartite(incidence, method = method), ref_square(nx[[name]], people),
                     reference = "networkx", covers = "project_bipartite()",
                     quantity = sprintf("projection on the rows, %s", method), tolerance = exact)
  })
  skip_without_reference("igraph")
  graph <- igraph::graph_from_biadjacency_matrix(incidence, weighted = NULL)
  projected <- igraph::bipartite_projection(graph, multiplicity = TRUE)$proj1
  expect_reference(project_bipartite(incidence, method = "binary"),
                   igraph::as_adjacency_matrix(projected, attr = "weight", sparse = FALSE),
                   reference = "igraph", covers = "project_bipartite()", quantity = "projection on the rows, binary",
                   tolerance = 0)
  expect_reference(project_bipartite(incidence, mode = "columns", method = "binary"),
                   igraph::as_adjacency_matrix(igraph::bipartite_projection(graph, multiplicity = TRUE)$proj2,
                                               attr = "weight", sparse = FALSE),
                   reference = "igraph", covers = "project_bipartite()", quantity = "projection on the columns, binary",
                   tolerance = 0)
  # weighted sum A A' and cosine A A' / (|a_i| |a_j|) have no named counterpart: their definitions
  products <- tcrossprod(incidence)
  diag(products) <- 0
  norms <- sqrt(rowSums(incidence^2))
  expect_reference(project_bipartite(incidence, method = "sum"), products,
                   reference = "independent: incidence times its transpose, base R", covers = "project_bipartite()",
                   quantity = "projection on the rows, weighted sum", tolerance = exact)
  expect_reference(project_bipartite(incidence, method = "cosine"), products / outer(norms, norms),
                   reference = "independent: incidence times its transpose, base R", covers = "project_bipartite()",
                   quantity = "projection on the rows, cosine", tolerance = exact)
})

test_that("simplify() merges parallel edges by the mean and drops loops, as igraph's simplify() does", {
  skip_without_reference("igraph")
  cells <- which(directed != 0, arr.ind = TRUE)
  edge_list <- data.frame(from = rownames(directed)[cells[, 1L]], to = colnames(directed)[cells[, 2L]],
                          weight = directed[cells])
  # two parallel arcs and two loops added
  with_duplicates <- rbind(edge_list, data.frame(from = c(edge_list$from[1:2], "N3", "N5"),
                                                 to = c(edge_list$to[1:2], "N3", "N5"), weight = c(0.9, 0.05, 0.3, 0.6)))
  merged <- igraph::simplify(igraph::graph_from_data_frame(with_duplicates, directed = TRUE, vertices = rownames(directed)),
                             edge.attr.comb = list(weight = "mean"))
  expected <- igraph::as_adjacency_matrix(merged, attr = "weight", sparse = FALSE)
  expect_reference(to_matrix(simplify(as_cograph(with_duplicates))), expected, reference = "igraph",
                   covers = c("simplify()", "simplify.cograph_network()"),
                   quantity = "parallel arcs averaged, loops dropped, cograph_network", tolerance = exact)
  expect_reference(to_matrix(simplify(with_duplicates)), expected, reference = "igraph",
                   covers = c("simplify()", "simplify.default()"),
                   quantity = "parallel arcs averaged, loops dropped, edge-list data frame", tolerance = exact)
})

test_that("simplify() of a matrix, an igraph graph and a tna model equals the definition", {
  reference <- "independent: loops dropped, parallel weights averaged, base R"
  looped <- directed
  diag(looped) <- c(0.5, 0, 0.2, 0, 0, 0.1, 0, 0)
  expect_reference(simplify(looped), directed, reference = reference, covers = c("simplify()", "simplify.matrix()"),
                   quantity = "matrix, loops dropped", tolerance = 0)
  skip_without_reference("igraph")
  # an igraph multigraph: one arc doubled with weights 0.2 and 0.6, so the merged arc weighs their mean 0.4
  multigraph <- igraph::graph_from_data_frame(data.frame(from = c("a", "a", "b", "c", "c"), to = c("b", "b", "c", "c", "a"),
                                                        weight = c(0.2, 0.6, 0.7, 0.9, 0.3)), directed = TRUE)
  expected <- matrix(c(0, 0, 0.3, 0.4, 0, 0, 0, 0.7, 0), 3, 3, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  expect_reference(to_matrix(simplify(multigraph)), expected, reference = reference,
                   covers = c("simplify()", "simplify.igraph()"), quantity = "igraph multigraph", tolerance = exact)
  skip_without_reference("tna")
  model <- tna::tna(tna::group_regulation)
  without_loops <- model$weights
  diag(without_loops) <- 0
  expect_reference(simplify(model)$weights, without_loops, reference = reference,
                   covers = c("simplify()", "simplify.tna()"), quantity = "tna model, loops dropped", tolerance = 0)
})

test_that("to_matrix() of a tna model holds the transition probabilities counted from its sequences", {
  skip_without_reference("tna")
  counted <- ref_transitions(tna::group_regulation)
  expect_reference(to_matrix(tna::tna(tna::group_regulation)), counted$weights,
                   reference = "independent: transitions counted from the sequences, base R", covers = "to_matrix()",
                   quantity = "transition probabilities of group_regulation", tolerance = exact)
})

test_that("components, cores and minimum spanning trees equal NetworkX's", {
  nx <- ref_networkx(two_parts, c(
    components = "nx.number_connected_components(G)",
    largest = "{n: 1 for n in max(nx.connected_components(G), key=len)}",
    core = "{n: 1 for n in nx.k_core(G, 2)}",
    tree = "nx.minimum_spanning_tree(G, weight='weight').size(weight='weight')",
    max_tree = "nx.maximum_spanning_tree(G, weight='weight').size(weight='weight')"))
  expect_reference(length(split_components(two_parts)), nx$components, reference = "networkx",
                   covers = "split_components()", quantity = "number of components", tolerance = 0)
  expect_reference(ref_node_flags(nodes_of(two_parts), get_labels(select_component(two_parts, which = "largest"))),
                   ref_node_flags(nodes_of(two_parts), names(nx$largest)), reference = "networkx",
                   covers = "select_component()", quantity = "nodes of the largest component", tolerance = 0)
  expect_reference(ref_node_flags(nodes_of(two_parts), get_labels(select_k_core(two_parts, k = 2))),
                   ref_node_flags(nodes_of(two_parts), names(nx$core)), reference = "networkx",
                   covers = "select_k_core()", quantity = "nodes of the 2-core", tolerance = 0)
  expect_reference(sum(as.data.frame(spanning_tree(two_parts))$weight), nx$tree, reference = "networkx",
                   covers = "spanning_tree()", quantity = "total weight of the minimum spanning forest",
                   tolerance = exact)
  expect_reference(sum(as.data.frame(spanning_tree(two_parts, maximum = TRUE))$weight), nx$max_tree,
                   reference = "networkx", covers = "spanning_tree()",
                   quantity = "total weight of the maximum spanning forest", tolerance = exact)
})

test_that("cut vertices, local transitivity and triangles equal NetworkX's", {
  nx <- ref_networkx(two_parts, c(cut = "{n: 1 for n in nx.articulation_points(G)}",
                                  clustering = "nx.clustering(G)", triangles = "nx.triangles(G)"))
  expect_reference(ref_node_flags(nodes_of(two_parts), get_labels(select_nodes(two_parts, is_cut))),
                   ref_node_flags(nodes_of(two_parts), names(nx$cut)), reference = "networkx",
                   covers = "select_nodes()", quantity = "cut vertices", tolerance = 0)
  measured <- as.data.frame(mutate_nodes(two_parts, transitivity = local_transitivity, triangles = local_triangles),
                            what = "nodes")
  expected <- data.frame(label = names(nx$clustering), transitivity = nx$clustering,
                         triangles = nx$triangles[names(nx$clustering)])
  pendant <- c("N7", "N11")
  expect_reference(subset(measured, !label %in% pendant), subset(expected, !label %in% pendant),
                   reference = "networkx", covers = "mutate_nodes()",
                   quantity = "local transitivity and triangles, nodes of degree 2 or more", tolerance = exact,
                   key = "label")
  expect_known_difference(subset(measured, label %in% pendant, select = c(label, transitivity)),
                          subset(expected, label %in% pendant, select = c(label, transitivity)),
                          reference = "networkx", covers = "mutate_nodes()",
                          quantity = "local transitivity, nodes of degree 1", tolerance = exact, key = "label",
                          reason = paste("A node with fewer than two neighbours has no pair of neighbours: cograph",
                                         "(as igraph) leaves its transitivity undefined (NaN); NetworkX's",
                                         "clustering() returns 0."))
})

test_that("select_top() keeps the nodes NetworkX ranks highest by strength and betweenness", {
  nx <- ref_networkx(undirected, c(strength = "dict(G.degree(weight='weight'))",
                                   betweenness = "nx.betweenness_centrality(G, normalized=False, weight='weight')"))
  top_three <- \(scores) names(sort(scores, decreasing = TRUE))[1:3]
  expect_reference(ref_node_flags(nodes_of(undirected), get_labels(select_top(undirected, n = 3, by = "strength"))),
                   ref_node_flags(nodes_of(undirected), top_three(nx$strength)), reference = "networkx",
                   covers = "select_top()", quantity = "three strongest nodes", tolerance = 0)
  expect_reference(ref_node_flags(nodes_of(undirected), get_labels(select_top(undirected, n = 3, by = "betweenness"))),
                   ref_node_flags(nodes_of(undirected), top_three(nx$betweenness)), reference = "networkx",
                   covers = "select_top()", quantity = "three nodes of highest betweenness", tolerance = 0)
})

test_that("select_top_edges() keeps the heaviest edges and NetworkX's top edges by edge betweenness", {
  nx <- ref_networkx(undirected, c(betweenness = paste0(
    "[[u, v, b] for (u, v), b in nx.edge_betweenness_centrality(G, normalized=False, weight='weight').items()]")))
  scores <- vapply(nx$betweenness, \(edge) as.numeric(edge[[3L]]), numeric(1))
  ends <- vapply(nx$betweenness, \(edge) c(edge[[1L]], edge[[2L]]), character(2))
  top <- order(scores, decreasing = TRUE)[1:4]
  kept <- as.data.frame(select_top_edges(undirected, n = 4, by = "edge_betweenness"))
  expect_reference(ref_edge_flags(undirected, kept$from, kept$to), ref_edge_flags(undirected, ends[1L, top], ends[2L, top]),
                   reference = "networkx", covers = "select_top_edges()",
                   quantity = "four edges of highest edge betweenness", tolerance = 0, key = "edge")
  # Six-decimal weights without the ring of weight-1 edges: no two weights tie at the cut. (On the connected
  # graph above six edges weigh 1; select_top_edges() then keeps an arbitrary four of them, where
  # threshold_edges(top =) keeps all tied edges.)
  untied <- ref_graph(10, density = 0.35, seed = 96L, weighted = TRUE, digits = 6L)
  heaviest <- which(untied >= sort(untied[upper.tri(untied) & untied != 0], decreasing = TRUE)[[4L]] &
                      upper.tri(untied), arr.ind = TRUE)
  by_weight <- as.data.frame(select_top_edges(untied, n = 4))
  expect_reference(ref_edge_flags(untied, by_weight$from, by_weight$to),
                   ref_edge_flags(untied, rownames(untied)[heaviest[, 1L]], colnames(untied)[heaviest[, 2L]]),
                   reference = "independent: the four largest weights, base R", covers = "select_top_edges()",
                   quantity = "four heaviest edges", tolerance = 0, key = "edge")
})

test_that("select_bridges() keeps the edges NetworkX and igraph find to be bridges", {
  nx <- ref_networkx(two_parts, c(bridges = "[[u, v] for u, v in nx.bridges(G)]"))
  ends <- vapply(nx$bridges, \(edge) c(edge[[1L]], edge[[2L]]), character(2))
  kept <- as.data.frame(select_bridges(two_parts))
  expect_reference(ref_edge_flags(two_parts, kept$from, kept$to), ref_edge_flags(two_parts, ends[1L, ], ends[2L, ]),
                   reference = "networkx", covers = "select_bridges()", quantity = "bridges", tolerance = 0, key = "edge")
  skip_without_reference("igraph")
  graph <- ref_igraph(two_parts)
  bridge_ends <- igraph::ends(graph, igraph::bridges(graph))
  expect_reference(ref_edge_flags(two_parts, kept$from, kept$to),
                   ref_edge_flags(two_parts, bridge_ends[, 1L], bridge_ends[, 2L]), reference = "igraph",
                   covers = "select_bridges()", quantity = "bridges", tolerance = 0, key = "edge")
})

test_that("select_bridges() on a network whose edge table is ordered unlike igraph's edge list", {
  # cograph stores the edges of a matrix column by column; igraph lists them row by row. On this graph the two
  # orders differ, and the bridge flags, computed in igraph's order, are applied to cograph's edge table.
  shuffled <- ref_graph(8, density = 0.35, seed = 4L, weighted = TRUE)
  nx <- ref_networkx(shuffled, c(bridges = "[[u, v] for u, v in nx.bridges(G)]"))
  ends <- vapply(nx$bridges, \(edge) c(edge[[1L]], edge[[2L]]), character(2))
  kept <- as.data.frame(select_bridges(shuffled))
  expect_reference(ref_edge_flags(shuffled, kept$from, kept$to), ref_edge_flags(shuffled, ends[1L, ], ends[2L, ]),
                   reference = "networkx", covers = "select_bridges()",
                   quantity = "bridges, edge table not in igraph's order", tolerance = 0, key = "edge",
                   defect = paste("cograph select_bridges() flags edges by igraph's edge order but selects from cograph's edge",
                                  "table (ref_graph(8, 0.35, seed = 4): keeps N2-N6 and N2-N8, misses N4-N6 and N1-N8);",
                                  "NetworkX and igraph::bridges() agree with each other; FINDINGS.md"))
  skip_without_reference("igraph")
  graph <- ref_igraph(shuffled)
  bridge_ends <- igraph::ends(graph, igraph::bridges(graph))
  expect_reference(ref_edge_flags(shuffled, kept$from, kept$to),
                   ref_edge_flags(shuffled, bridge_ends[, 1L], bridge_ends[, 2L]), reference = "igraph",
                   covers = "select_bridges()", quantity = "bridges, edge table not in igraph's order", tolerance = 0, key = "edge",
                   defect = paste("cograph select_bridges() flags edges by igraph's edge order but selects from cograph's edge",
                                  "table (ref_graph(8, 0.35, seed = 4): keeps N2-N6 and N2-N8, misses N4-N6 and N1-N8);",
                                  "NetworkX and igraph::bridges() agree with each other; FINDINGS.md"))
})
