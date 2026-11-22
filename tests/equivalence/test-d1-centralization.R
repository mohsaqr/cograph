# centralization() (Freeman 1979 centralization of degree, betweenness, closeness and eigenvector centrality),
# compared with igraph (centr_degree(), centr_betw(), centr_clo(), centr_eigen()), sna (centralization()) and the
# definition. cograph computes the scores and the maxima itself (R/centrality-extended.R), so igraph is a counted
# reference. Betweenness, closeness and eigenvector centralization read weights; the graphs here are unweighted, as
# igraph's centr_*() and sna read them.
#
# Graphs: connected random graphs from helper-reference.R, and a five-node star with reciprocated arcs (the graph
# whose in + out degree centralization is the largest possible: 1).

undirected <- d1_binary(ref_connected_graph(12, density = 0.3, seed = 151L))
directed <- d1_binary(ref_connected_graph(12, density = 0.25, seed = 152L, directed = TRUE))
mutual_star <- local({
  star <- matrix(0, 6, 6, dimnames = list(paste0("S", 1:6), paste0("S", 1:6)))
  star[1, -1] <- 1
  star[-1, 1] <- 1
  star
})

exact <- 1e-10
# Eigenvector scores come out of an eigen-solver in each program (ARPACK in igraph, LAPACK in R's eigen()).
spectral <- 1e-8

test_that("centralization() equals igraph's centr_*() on an undirected network", {
  skip_without_reference("igraph")
  graph <- ref_igraph(undirected)
  expect_reference(c(degree = centralization(undirected, "degree"), betweenness = centralization(undirected, "betweenness"),
                     closeness = centralization(undirected, "closeness")),
                   c(degree = igraph::centr_degree(graph, loops = FALSE)$centralization,
                     betweenness = igraph::centr_betw(graph)$centralization,
                     closeness = igraph::centr_clo(graph)$centralization),
                   reference = "igraph", covers = "centralization()", quantity = "centralization, undirected",
                   tolerance = exact)
})

test_that("centralization() equals igraph's centr_*() on a directed network", {
  skip_without_reference("igraph")
  graph <- ref_igraph(directed)
  expect_reference(c(in_degree = centralization(directed, "degree", mode = "in"),
                     out_degree = centralization(directed, "degree", mode = "out"),
                     betweenness = centralization(directed, "betweenness"),
                     closeness_all = centralization(directed, "closeness"),
                     eigenvector = centralization(directed, "eigenvector")),
                   c(in_degree = igraph::centr_degree(graph, mode = "in", loops = FALSE)$centralization,
                     out_degree = igraph::centr_degree(graph, mode = "out", loops = FALSE)$centralization,
                     betweenness = igraph::centr_betw(graph)$centralization,
                     closeness_all = igraph::centr_clo(graph, mode = "all")$centralization,
                     eigenvector = igraph::centr_eigen(graph, directed = TRUE)$centralization),
                   reference = "igraph", covers = "centralization()", quantity = "centralization, directed",
                   tolerance = spectral)
})

test_that("centralization() equals sna's centralization()", {
  skip_without_reference("sna")
  expect_reference(c(degree = centralization(undirected, "degree"), betweenness = centralization(undirected, "betweenness"),
                     closeness = centralization(undirected, "closeness")),
                   c(degree = sna::centralization(undirected, sna::degree, mode = "graph"),
                     betweenness = sna::centralization(undirected, sna::betweenness, mode = "graph"),
                     closeness = sna::centralization(undirected, sna::closeness, mode = "graph")),
                   reference = "sna", covers = "centralization()", quantity = "centralization, undirected",
                   tolerance = exact)
  expect_reference(centralization(directed, "betweenness"),
                   sna::centralization(directed, sna::betweenness, mode = "digraph"),
                   reference = "sna", covers = "centralization()", quantity = "betweenness centralization, directed",
                   tolerance = exact)
})

test_that("centralization(measure = 'degree') of in + out degree is at most 1 on a directed network", {
  skip_without_reference("igraph")
  # Freeman's maximum of in + out degree without self-loops is 2 (n - 1)(n - 2), reached by a star with reciprocated
  # arcs; cograph 2.7.18 and earlier divided by (n - 1)^2 and reported 1.6 for that star (corrected in 2.7.19).
  n <- nrow(mutual_star)
  degree_all <- rowSums(mutual_star) + colSums(mutual_star)
  expect_reference(c(mutual_star = centralization(mutual_star, "degree", directed = TRUE),
                     random = centralization(directed, "degree")),
                   c(mutual_star = d1_freeman(degree_all, 2 * (n - 1) * (n - 2)),
                     random = d1_freeman(rowSums(directed) + colSums(directed), 2 * (nrow(directed) - 1) * (nrow(directed) - 2))),
                   reference = "independent: Freeman degree centralization, in + out degree, maximum 2 (n - 1)(n - 2)",
                   covers = "centralization()", quantity = "degree centralization, mode all, directed",
                   tolerance = exact)
  expect_reference(centralization(mutual_star, "degree", directed = TRUE),
                   igraph::centr_degree(ref_igraph(mutual_star, directed = TRUE), mode = "all", loops = FALSE)$centralization,
                   reference = "igraph", covers = "centralization()",
                   quantity = "degree centralization, mode all, reciprocated star", tolerance = exact)
})

test_that("centralization(measure = 'eigenvector') divides by n - 1 on an undirected network", {
  skip_without_reference("igraph")
  # The documented divisor is n - 1 for every network; igraph's maximum for an undirected network is n - 2 (a single
  # edge), so the two differ by the factor (n - 2) / (n - 1).
  n <- nrow(undirected)
  expect_reference(centralization(undirected, "eigenvector"),
                   d1_eigen_centralization(undirected, directed = FALSE) * (n - 2) / (n - 1),
                   reference = "independent: eigenvector scores scaled to max 1, summed gap over n - 1",
                   covers = "centralization()", quantity = "eigenvector centralization, undirected", tolerance = spectral)
  expect_known_difference(centralization(undirected, "eigenvector"), igraph::centr_eigen(ref_igraph(undirected))$centralization,
                          reference = "igraph", covers = "centralization()",
                          quantity = "eigenvector centralization, undirected", tolerance = spectral,
                          reason = paste("centralization() divides the summed gap by n - 1 on every network, as its help",
                                         "page states; igraph's centr_eigen() divides by n - 2 on an undirected one."))
})
