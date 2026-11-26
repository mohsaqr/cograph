# Hybrid centrality, centrality_hybrid() (Christensen, Kenett, Aste, Silvia and Kwapil 2018, Behavior Research Methods
# 50(6):2531-2550; read in the accepted manuscript, p. 26):
#   HC = (BC^w + BC^u + LC^w + LC^u + k^u + EC^w + EC^u + NS^w - 8) / (8 (N - 1)),
# each term the rank (ties sharing the largest rank of their group, the most central node ranked highest) of the node
# on betweenness, closeness or eigenvector centrality of the weighted (w) or unweighted (u) network, on degree and on
# strength. The paper prints no per-node value. The eight centralities are taken from igraph here and ranked the
# same way; the first author's own function, NetworkToolbox::hybrid(BC = "standard"), is compared as well.

exact <- 1e-10
# NetworkToolbox::hybrid() returns three decimals.
toolbox_3dp <- 5e-4

hybrid_weighted <- function(nodes, seed) {
  graph <- ref_connected_graph(nodes, density = 0.4, seed = seed, weighted = TRUE, digits = 3L)
  graph
}

# The eight ranks from igraph's centralities; a path's length on the weighted network is the sum of 1 / weight.
hybrid_from_igraph <- function(graph) {
  n <- nrow(graph)
  weighted <- igraph::graph_from_adjacency_matrix(graph, mode = "undirected", weighted = TRUE)
  plain <- igraph::graph_from_adjacency_matrix((graph > 0) * 1, mode = "undirected")
  lengths <- 1 / igraph::E(weighted)$weight
  position <- \(x) rank(signif(x, 12L), ties.method = "max")
  terms <- list(igraph::betweenness(weighted, weights = lengths), igraph::betweenness(plain),
                igraph::closeness(weighted, weights = lengths), igraph::closeness(plain),
                igraph::degree(plain), igraph::strength(weighted),
                igraph::eigen_centrality(weighted)$vector, igraph::eigen_centrality(plain, weights = NA)$vector)
  ref_named((Reduce(`+`, lapply(terms, position)) - 8) / (8 * (n - 1)), graph)
}

test_that("hybrid centrality equals the eight ranks taken from igraph's centralities", {
  skip_without_reference("igraph")
  graphs <- list(`weighted 9 nodes` = hybrid_weighted(9, 62L), `weighted 14 nodes` = hybrid_weighted(14, 63L),
                 `karate, unit weights` = ref_karate(), kite = zoob_kite())
  Map(\(graph, name) {
    expect_reference(centrality_hybrid(graph), hybrid_from_igraph(graph), reference = "igraph",
                     covers = c("centrality()", "centrality_hybrid()"), quantity = sprintf("hybrid, %s", name),
                     tolerance = exact)
  }, graphs, names(graphs))
})

test_that("hybrid centrality against NetworkToolbox::hybrid(BC = 'standard') on the kite", {
  skip_without_reference("NetworkToolbox")
  # On the unweighted kite the two differ at three nodes. NetworkToolbox computes the "weighted" betweenness of the
  # 0/1 matrix with a routine that miscounts when several shortest paths tie: it gives the two mirror-image nodes 6
  # and 7 different scores (0.903 and 0.889), which no function of the graph can do. cograph gives both 0.917.
  graph <- zoob_kite()
  expect_known_difference(round(centrality_hybrid(graph), 3), NetworkToolbox::hybrid(graph, BC = "standard"),
                          reference = "NetworkToolbox", covers = c("centrality()", "centrality_hybrid()"),
                          quantity = "hybrid, kite, unit weights", tolerance = toolbox_3dp,
                          reason = paste(
                            "NetworkToolbox's weighted betweenness miscounts tied shortest paths (it scores the",
                            "mirror-image nodes 6 and 7 differently), and it rounds each centrality to three",
                            "decimals before ranking, which creates ties the exact values lack."))
})
