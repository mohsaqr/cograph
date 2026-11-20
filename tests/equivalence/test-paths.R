# shortest_paths(), k_shortest_paths() (Yen 1971) and spanning_tree() (minimum and maximum spanning trees, Kruskal
# 1956). shortest_paths() and k_shortest_paths() call igraph::distances() and igraph::shortest_paths()
# (R/paths.R), so igraph is `internal` for them; spanning_tree() is cograph's own code (R/wrangle-structure.R).
# NetworkX (all_pairs_dijkstra_path_length(), shortest_simple_paths(), minimum_spanning_tree()) is the counted
# reference.
#
# Graphs: connected random graphs from helper-reference.R with six-decimal weights read as distances, so that two
# paths, or two candidate tree edges, almost never weigh the same.

directed_weighted <- ref_connected_graph(10, density = 0.3, seed = 121L, directed = TRUE, weighted = TRUE, digits = 6L)
undirected_weighted <- ref_connected_graph(10, density = 0.35, seed = 122L, weighted = TRUE, digits = 6L)
# The ring of ref_connected_graph() has weight 1 on every added edge, so spanning trees are taken on a graph without
# it: dense enough to be connected, every weight distinct (a unique minimum and maximum tree).
tree_graph <- ref_graph(10, density = 0.5, seed = 123L, weighted = TRUE, digits = 6L)

exact <- 1e-10

# NetworkX values keyed "u|v" as a node-by-node matrix (0 where no value is given).
as_matrix_of <- \(values, graph, symmetric = FALSE) {
  ends <- do.call(rbind, strsplit(names(values), "|", fixed = TRUE))
  out <- matrix(0, nrow(graph), ncol(graph), dimnames = dimnames(graph))
  out[ends] <- as.numeric(values)
  if (symmetric) out[ends[, 2:1, drop = FALSE]] <- as.numeric(values)
  out
}

test_that("shortest_paths() equals NetworkX's Dijkstra distances", {
  py <- c(d = "{f'{u}|{v}': x for u, s in nx.all_pairs_dijkstra_path_length(G, weight='weight') for v, x in s.items()}")
  on_directed <- ref_networkx(directed_weighted, py)
  on_undirected <- ref_networkx(undirected_weighted, py)
  expect_reference(shortest_paths(directed_weighted), as_matrix_of(on_directed$d, directed_weighted),
                   reference = "networkx", covers = "shortest_paths()", quantity = "distance, directed weighted",
                   tolerance = exact)
  expect_reference(shortest_paths(undirected_weighted), as_matrix_of(on_undirected$d, undirected_weighted),
                   reference = "networkx", covers = "shortest_paths()", quantity = "distance, undirected weighted",
                   tolerance = exact)
  expect_reference(shortest_paths(directed_weighted, from = "N1"), as_matrix_of(on_directed$d, directed_weighted)["N1", ],
                   reference = "networkx", covers = "shortest_paths()", quantity = "distance from N1, directed weighted",
                   tolerance = exact)
})

test_that("k_shortest_paths() equals NetworkX's shortest_simple_paths()", {
  py <- \(from, to) c(
    distance = sprintf("[nx.path_weight(G, p, 'weight') for _, p in zip(range(4), nx.shortest_simple_paths(G, '%s', '%s', weight='weight'))]", from, to),
    path = sprintf("['>'.join(p) for _, p in zip(range(4), nx.shortest_simple_paths(G, '%s', '%s', weight='weight'))]", from, to))
  on_directed <- ref_networkx(directed_weighted, py("N1", "N6"))
  found <- k_shortest_paths(directed_weighted, from = "N1", to = "N6", k = 4)
  expect_reference(found$distances, on_directed$distance, reference = "networkx", covers = "k_shortest_paths()",
                   quantity = "length of the k shortest paths N1 to N6, directed", tolerance = exact)
  expect_reference(vapply(found$paths, paste, character(1), collapse = ">"), unlist(on_directed$path),
                   reference = "networkx", covers = "k_shortest_paths()",
                   quantity = "the k shortest paths N1 to N6, directed", tolerance = 0)
  on_undirected <- ref_networkx(undirected_weighted, py("N2", "N9"))
  found_u <- k_shortest_paths(undirected_weighted, from = "N2", to = "N9", k = 4)
  expect_reference(found_u$distances, on_undirected$distance, reference = "networkx", covers = "k_shortest_paths()",
                   quantity = "length of the k shortest paths N2 to N9, undirected", tolerance = exact)
})

test_that("spanning_tree() equals NetworkX's minimum and maximum spanning trees", {
  py <- c(minimum = "{f'{u}|{v}': d['weight'] for u, v, d in nx.minimum_spanning_tree(G, weight='weight').edges(data=True)}",
          maximum = "{f'{u}|{v}': d['weight'] for u, v, d in nx.maximum_spanning_tree(G, weight='weight').edges(data=True)}")
  nx <- ref_networkx(tree_graph, py)
  expect_reference(spanning_tree(tree_graph, keep_format = TRUE),
                   as_matrix_of(nx$minimum, tree_graph, symmetric = TRUE), reference = "networkx",
                   covers = "spanning_tree()", quantity = "minimum spanning tree, weight matrix", tolerance = exact)
  expect_reference(spanning_tree(tree_graph, maximum = TRUE, keep_format = TRUE),
                   as_matrix_of(nx$maximum, tree_graph, symmetric = TRUE), reference = "networkx",
                   covers = "spanning_tree()", quantity = "maximum spanning tree, weight matrix", tolerance = exact)
})
