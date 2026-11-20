# Weighted closeness and betweenness with the tuning exponent alpha of Opsahl, Agneessens and Skvoretz (2010), and the
# local weighted rich-club ratio of rich_club_local() (Opsahl et al. 2008), against tnet (Opsahl). cograph's
# `invert_weights = TRUE, alpha = a` reads a tie of weight w as a distance 1 / w^a, as tnet does.
#
# tnet takes an edge list (i, j, w) with every undirected tie listed both ways; nodes are numbered in the order of the
# matrix rows.

weighted <- ref_connected_graph(12, density = 0.3, seed = 25L, weighted = TRUE, digits = 6L)
directed_weighted <- ref_connected_graph(12, density = 0.25, seed = 28L, directed = TRUE, weighted = TRUE, digits = 6L)

# Shortest weighted paths on six-decimal weights, then one sum and one division: rounding of the last bits.
exact <- 1e-10

test_that("betweenness with alpha equals tnet's betweenness_w()", {
  skip_without_reference("tnet")
  on_undirected <- tnet::betweenness_w(ref_tnet_edges(weighted), directed = FALSE, alpha = 0.5)
  on_directed <- tnet::betweenness_w(ref_tnet_edges(directed_weighted), directed = TRUE, alpha = 1.5)
  expect_reference(centrality_betweenness(weighted, invert_weights = TRUE, alpha = 0.5),
                   ref_named(on_undirected[, "betweenness"], weighted), reference = "tnet",
                   covers = c("centrality()", "centrality_betweenness()"),
                   quantity = "betweenness, alpha 0.5, undirected weighted", tolerance = exact, measure = "scaled")
  expect_reference(centrality_betweenness(directed_weighted, invert_weights = TRUE, alpha = 1.5),
                   ref_named(on_directed[, "betweenness"], directed_weighted), reference = "tnet",
                   covers = c("centrality()", "centrality_betweenness()"),
                   quantity = "betweenness, alpha 1.5, directed weighted", tolerance = exact, measure = "scaled")
})

test_that("closeness with alpha equals tnet's closeness_w() once tnet's mean-weight scale is undone", {
  skip_without_reference("tnet")
  edges <- ref_tnet_edges(weighted)
  edges_directed <- ref_tnet_edges(directed_weighted)
  on_undirected <- tnet::closeness_w(edges, directed = FALSE, alpha = 0.5)
  on_directed <- tnet::closeness_w(edges_directed, directed = TRUE, alpha = 1.5)
  # tnet measures a tie as mean(w^alpha) / w^alpha, so every distance is cograph's times the mean of w^alpha over the
  # edge list, and every closeness is cograph's divided by it.
  scale_undirected <- mean(edges[, "w"]^0.5)
  scale_directed <- mean(edges_directed[, "w"]^1.5)
  expect_reference(centrality_closeness(weighted, invert_weights = TRUE, alpha = 0.5),
                   ref_named(on_undirected[, "closeness"] * scale_undirected, weighted), reference = "tnet",
                   covers = c("centrality()", "centrality_closeness()"),
                   quantity = "closeness, alpha 0.5, undirected weighted, times mean w^alpha", tolerance = exact,
                   measure = "relative")
  expect_reference(centrality_outcloseness(directed_weighted, invert_weights = TRUE, alpha = 1.5),
                   ref_named(on_directed[, "closeness"] * scale_directed, directed_weighted), reference = "tnet",
                   covers = c("centrality()", "centrality_outcloseness()"),
                   quantity = "out-closeness, alpha 1.5, directed weighted, times mean w^alpha", tolerance = exact,
                   measure = "relative")
  expect_known_difference(centrality_closeness(weighted, invert_weights = TRUE, alpha = 0.5),
                          ref_named(on_undirected[, "closeness"], weighted), reference = "tnet",
                          covers = c("centrality()", "centrality_closeness()"),
                          quantity = "closeness, alpha 0.5, undirected weighted, as tnet reports it", tolerance = exact,
                          measure = "relative",
                          reason = paste("tnet::distance_w() divides every tie's distance by the mean of w^alpha over",
                                         "the ties (Opsahl et al. 2010), so its closeness is cograph's divided by",
                                         "that mean; cograph uses 1 / w^alpha as it stands."))
})

test_that("rich_club_local() equals tnet's weighted_richclub_local_w()", {
  skip_without_reference("tnet")
  prominent <- as.integer(rowSums(weighted != 0) > stats::median(rowSums(weighted != 0)))
  ratio <- tnet::weighted_richclub_local_w(ref_tnet_edges(weighted), prominent)
  expect_reference(rich_club_local(weighted, prominence = prominent),
                   data.frame(node = rownames(weighted)[ratio[, "node"]], score = ratio[, "ratio"]),
                   reference = "tnet", covers = "rich_club_local()", quantity = "local rich-club ratio, degree prominence",
                   tolerance = exact, key = "node")
})
