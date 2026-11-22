# centrality() and its wrappers for the measures of sna (Butts 2008) that are not built on shortest-path counts
# alone: Gil-Schmidt power, flow betweenness, information centrality, domain prestige and domain proximity prestige,
# Bonacich power, geodesic k-path and the k-path census. (Degree, betweenness, closeness, stress and load are in
# test-centrality-paths.R.) Flow betweenness is also compared with netrankr's network-flow dependencies.
#
# sna reads every matrix here as unweighted: the graphs are 0/1.

undirected <- ref_connected_graph(12, density = 0.3, seed = 21L)
directed <- ref_connected_graph(12, density = 0.25, seed = 22L, directed = TRUE)
# Not strongly connected (113 of 144 ordered pairs reachable): domain prestige is not n - 1 everywhere on it.
sparse_directed <- ref_graph(12, density = 0.15, seed = 24L, directed = TRUE)
# A tree: one path joins each pair, so a maximum flow between two nodes is unique.
tree <- ref_tree(10, seed = 3L)

exact <- 1e-10

test_that("Gil-Schmidt power equals sna's gilschmidt()", {
  skip_without_reference("sna")
  expect_reference(centrality_gilschmidt(undirected), ref_named(sna::gilschmidt(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_gilschmidt()"),
                   quantity = "gilschmidt, undirected", tolerance = exact)
  # sna follows the out-ties of a digraph.
  expect_reference(centrality_gilschmidt(directed, mode = "out"),
                   ref_named(sna::gilschmidt(directed, gmode = "digraph"), directed), reference = "sna",
                   covers = c("centrality()", "centrality_gilschmidt()"), quantity = "gilschmidt, mode out, directed",
                   tolerance = exact)
})

test_that("information centrality equals sna's infocent()", {
  skip_without_reference("sna")
  # One matrix inversion on each side, built the same way (Stephenson and Zelen 1989): relative 1e-10.
  expect_reference(centrality_information(undirected), ref_named(sna::infocent(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_information()"),
                   quantity = "information, undirected", tolerance = 1e-10, measure = "relative")
  path_plus_triangle <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
  path_plus_triangle[cbind(c(1, 1, 2, 4, 3), c(2, 3, 3, 5, 4))] <- 1
  path_plus_triangle <- path_plus_triangle + t(path_plus_triangle)
  expect_reference(centrality_information(path_plus_triangle),
                   ref_named(sna::infocent(path_plus_triangle, gmode = "graph"), path_plus_triangle), reference = "sna",
                   covers = c("centrality()", "centrality_information()"),
                   quantity = "information, triangle with a two-edge tail", tolerance = 1e-10, measure = "relative")
})

test_that("domain prestige equals sna's prestige(cmode = 'domain')", {
  skip_without_reference("sna")
  expect_reference(centrality_prestige_domain(sparse_directed),
                   ref_named(sna::prestige(sparse_directed, cmode = "domain"), sparse_directed), reference = "sna",
                   covers = c("centrality()", "centrality_prestige_domain()"),
                   quantity = "domain prestige, directed, not strongly connected", tolerance = exact)
})

test_that("domain proximity prestige equals sna on a strongly connected graph", {
  skip_without_reference("sna")
  # On a graph with an unreachable pair, sna multiplies (counts > 0) by an infinite distance, gets NaN for the sum
  # and returns 0 for every node; that reference is not used there (next test).
  expect_reference(centrality_prestige_domain_proximity(directed),
                   ref_named(sna::prestige(directed, cmode = "domain.proximity"), directed), reference = "sna",
                   covers = c("centrality()", "centrality_prestige_domain_proximity()"),
                   quantity = "domain proximity prestige, directed, strongly connected", tolerance = exact)
})

test_that("domain proximity prestige equals its definition on a graph with unreachable pairs", {
  expect_reference(centrality_prestige_domain_proximity(sparse_directed), ref_domain_proximity(sparse_directed),
                   reference = "independent: domain proximity prestige, Lin 1976 / Wasserman and Faust 1994, base R",
                   covers = c("centrality()", "centrality_prestige_domain_proximity()"),
                   quantity = "domain proximity prestige, directed, not strongly connected", tolerance = exact)
})

test_that("Bonacich power equals sna's bonpow(exponent = 1)", {
  skip_without_reference("sna")
  # (I - A)^-1 A 1, scaled to a sum of squares n, on both sides: one linear solve. I - A is well conditioned on
  # these graphs (condition numbers 12 and 8), so 1e-9 scaled.
  expect_reference(centrality_power(undirected), ref_named(sna::bonpow(undirected, gmode = "graph", exponent = 1), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_power()"), quantity = "power, undirected",
                   tolerance = 1e-9, measure = "scaled")
  # sna sums along the rows of a digraph: the out-ties.
  expect_reference(centrality_power(directed, mode = "out"),
                   ref_named(sna::bonpow(directed, gmode = "digraph", exponent = 1), directed), reference = "sna",
                   covers = c("centrality()", "centrality_power()"), quantity = "power, mode out, directed",
                   tolerance = 1e-9, measure = "scaled")
})

test_that("flow betweenness equals sna's flowbet() where maximum flows are unique", {
  skip_without_reference("sna")
  expect_reference(centrality_flow_betweenness(tree), ref_named(sna::flowbet(tree, gmode = "graph"), tree),
                   reference = "sna", covers = c("centrality()", "centrality_flow_betweenness()"),
                   quantity = "flow betweenness, tree", tolerance = exact, measure = "scaled")
})

test_that("flow betweenness equals sna's flowbet() where maximum flows are not unique", {
  skip_without_reference("sna")
  # sna (Freeman, Borgatti and White 1991) sums the drop in maximum flow when the node is removed, as cograph does
  # since 2.7.19. cograph 2.7.18 and earlier read the flow through a node from the one maximum flow
  # igraph::max_flow() returns, which is not unique; that agreed only where every maximum flow is unique (a tree).
  expect_reference(centrality_flow_betweenness(undirected), ref_named(sna::flowbet(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_flow_betweenness()"),
                   quantity = "flow betweenness, undirected", tolerance = exact, measure = "scaled")
})

test_that("flow betweenness on a tree equals netrankr's network-flow dependencies", {
  skip_without_reference(c("netrankr", "igraph"))
  dependencies <- netrankr::indirect_relations(ref_igraph(tree), type = "depend_netflow", netflowmode = "raw")
  # netrankr's raw dependencies are directed pairs; a pair (s, t) counts twice, so half the row sum is sna's value.
  expect_reference(centrality_flow_betweenness(tree), ref_named(netrankr::aggregate_positions(dependencies, type = "sum") / 2, tree),
                   reference = "netrankr", covers = c("centrality()", "centrality_flow_betweenness()"),
                   quantity = "flow betweenness, tree", tolerance = exact, measure = "scaled")
})

test_that("geodesic k-path equals the geodesic counts of sna's geodist()", {
  skip_without_reference("sna")
  # Borgatti's geodesic k-path: the number of geodesics of length at most k from the node.
  geodesics <- sna::geodist(undirected)
  within <- (geodesics$gdist <= 3) * geodesics$counts
  diag(within) <- 0
  expect_reference(centrality_geodesic_kpath(undirected, kpath_k = 3), ref_named(rowSums(within), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_geodesic_kpath()"),
                   quantity = "geodesic k-path k 3, undirected", tolerance = exact)
})

test_that("k-path equals sna's kpath.census() by vertex", {
  skip_without_reference("sna")
  # Sade's k-path: simple paths of length 1 to k that the node lies on, end points included. kpath.census() tabulates
  # them by length (rows) and vertex (columns, after the aggregate column).
  census <- sna::kpath.census(undirected, maxlen = 3, mode = "graph", tabulate.by.vertex = TRUE)$path.count
  expect_reference(centrality_kpath(undirected, kpath_len = 3), ref_named(colSums(census)[-1L], undirected),
                   reference = "sna", covers = c("centrality()", "centrality_kpath()"), quantity = "kpath 3, undirected",
                   tolerance = exact, measure = "scaled")
})
