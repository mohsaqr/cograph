# centrality() and its wrappers for four brokerage measures:
# - Coleman-Theil hierarchy (Burt 1991, STRUCTURE 4.2 reference manual, pp. 181-183; Burt 1992, eqs. 2.4 and 2.9):
#   the Coleman-Theil concentration of a node's dyadic constraints. NetworkX computes each dyadic constraint
#   (nx.local_constraint, on mutual weights, as Burt defines it); the concentration is written in the expression,
#   with the author's conventions of page 183: 0 for an isolate, 1 for a single contact.
# - proximal betweenness (Brandes 2008, Social Networks 30, section 2.1 and Algorithm 3): the share of shortest
#   s-t paths on which a node is the last intermediary (source) or the first (target), over ordered pairs.
#   NetworkX enumerates the shortest paths (nx.all_shortest_paths).
# - localized bridging and extended localized bridging (Macker 2016, MILCOM, eqs. 3-5): ego betweenness of the
#   radius-one (radius-two) induced ego network times the bridging coefficient of equation 3,
#   (1 / d_i) / sum_{j in N(i)} 1 / d_j, on degrees in the whole graph. NetworkX computes the ego betweenness
#   (nx.ego_graph, nx.betweenness_centrality(normalized=False), unordered pairs); Macker's Table I prints it for
#   his Figure 1.
# cograph computes all four natively (no igraph call in R/centrality-batch28.R, -30.R, -33.R).

exact <- 1e-10
karate <- zoob_karate()
weighted <- ref_connected_graph(12, density = 0.3, seed = 21L, weighted = TRUE)
directed_weighted <- ref_connected_graph(12, density = 0.25, seed = 22L, directed = TRUE, weighted = TRUE)
directed <- ref_connected_graph(12, density = 0.25, seed = 23L, directed = TRUE)

# Burt's hierarchy: r_j = c_ij / mean(c_i.), H_i = sum_j r_j log r_j / (N log N), N contacts.
coleman_theil_nx <- paste0(
  "{v: (lambda c: 0.0 if len(c) == 0 else 1.0 if len(c) == 1 else ",
  "sum((x / (sum(c) / len(c))) * np.log(x / (sum(c) / len(c))) for x in c) / (len(c) * np.log(len(c))))",
  "([nx.local_constraint(G, v, u, weight='weight') for u in set(nx.all_neighbors(G, v))]) for v in G}")

test_that("coleman_theil equals the concentration of NetworkX's local_constraint()", {
  # Two weighted contacts (A-B 1, A-C 2), the same as arcs (1 -> 2, 3 -> 1), and larger graphs.
  pair <- zoob_graph(3, c(1, 2, 1, 3), labels = c("A", "B", "C"))
  pair["A", "C"] <- pair["C", "A"] <- 2
  arcs <- zoob_graph(3, c(1, 2, 3, 1), directed = TRUE)
  arcs[3, 1] <- 2
  graphs <- list(`two weighted contacts` = pair, `two weighted arcs` = arcs, `star of 4` = zoob_star(4),
                 `ring of 5` = zoob_ring(5), `complete graph on 5` = zoob_complete(5), `karate club` = karate, `weighted graph` = weighted, `directed weighted graph` = directed_weighted)
  lapply(names(graphs), \(name) {
    nx <- ref_networkx(graphs[[name]], c(hierarchy = coleman_theil_nx))
    expect_reference(centrality_coleman_theil(graphs[[name]]), nx$hierarchy, reference = "networkx",
                     covers = c("centrality()", "centrality_coleman_theil()"),
                     quantity = paste0("coleman_theil, ", name), tolerance = exact)
  })
})

# The share of the shortest s-t paths (ordered pairs, s != t, t reachable) on which v is the penultimate node
# (proximal source) or the second node (proximal target). Paths of one edge have no intermediary.
proximal_nx <- function(position) {
  role <- switch(position, source = "p[-2] == v", target = "p[1] == v", union = "(p[1] == v or p[-2] == v)")
  sprintf(paste0("{v: sum(sum(1 for p in P if len(p) > 2 and %s) / len(P) for s in G for t in G ",
                 "if s != t and nx.has_path(G, s, t) for P in [list(nx.all_shortest_paths(G, s, t))]) for v in G}"),
          role)
}

test_that("proximal_betweenness equals the shares of NetworkX's all_shortest_paths()", {
  ties <- zoob_graph(6, c(1, 2, 1, 3, 2, 4, 3, 4, 3, 5, 4, 6, 5, 6), directed = TRUE)
  graphs <- list(`directed path of 4` = zoob_graph(4, c(1, 2, 2, 3, 3, 4), directed = TRUE),
                 `path of 4` = zoob_path(4), `digraph with tied paths` = ties, `star of 4` = zoob_star(4),
                 `karate club` = karate, `directed graph` = directed)
  lapply(names(graphs), \(name) {
    nx <- ref_networkx(graphs[[name]], c(source = proximal_nx("source"), target = proximal_nx("target"),
                                         union = proximal_nx("union")))
    expect_reference(centrality_proximal_betweenness(graphs[[name]], "source"), nx$source, reference = "networkx",
                     covers = c("centrality()", "centrality_proximal_betweenness()"), quantity = paste0("proximal_betweenness, source, ", name), tolerance = exact)
    expect_reference(centrality_proximal_betweenness(graphs[[name]], "target"), nx$target, reference = "networkx",
                     covers = c("centrality()", "centrality_proximal_betweenness()"), quantity = paste0("proximal_betweenness, target, ", name), tolerance = exact)
    expect_reference(centrality_proximal_betweenness(graphs[[name]], "sum"), nx$source + nx$target,
                     reference = "networkx", covers = c("centrality()", "centrality_proximal_betweenness()"),
                     quantity = paste0("proximal_betweenness, sum, ", name), tolerance = exact)
    expect_reference(centrality_proximal_betweenness(graphs[[name]], "union"), nx$union, reference = "networkx",
                     covers = c("centrality()", "centrality_proximal_betweenness()"), quantity = paste0("proximal_betweenness, union, ", name), tolerance = exact)
  })
})

# Macker (2016), equation 3: the bridging coefficient from degrees in the whole (undirected, simple) graph.
bridging_coefficient <- function(graph) {
  skeleton <- (graph + t(graph) > 0) * 1
  diag(skeleton) <- 0
  degree <- rowSums(skeleton)
  inverse <- ifelse(degree > 0, 1 / pmax(degree, 1), 0)
  around <- as.numeric(skeleton %*% inverse)
  stats::setNames(ifelse(around > 0, inverse / pmax(around, .Machine$double.xmin), 0), rownames(graph))
}

# Macker (2016), Figure 1: nodes A to K.
macker <- zoob_graph(11, c(1, 2, 1, 3, 1, 4, 1, 5, 2, 10, 4, 10, 10, 11, 5, 6, 6, 7, 6, 8, 7, 9, 8, 9),
                     labels = LETTERS[1:11])

test_that("localized bridging reproduces Macker's Table I", {
  # Table I prints the one-hop and two-hop ego betweenness of every node of Figure 1 to at most one decimal
  # (6, 11.5, 0.5, ...); the score is that ego betweenness times the equation 3 coefficient. Half a unit of the
  # printed decimal is 0.05; every node with a nonzero printed value has a coefficient below one (C and K, the
  # two with coefficients 4 and 3, print 0), so the product is within 0.05 of the exact score.
  one_hop <- c(6, 1, 0, 1, 1, 3, 1, 1, 1, 3, 0)
  two_hop <- c(11.5, 3, 0, 3, 12, 6.5, 1, 1, 0.5, 3.5, 0)
  coefficient <- bridging_coefficient(macker)
  expect_reference(centrality_localized_bridging(macker), one_hop * coefficient,
                   reference = "published: Macker 2016, Fig. 1 and Table I, chained with the equation 3 coefficient computed in the test",
                   covers = c("centrality()", "centrality_localized_bridging()"),
                   quantity = "localized_bridging, Macker Figure 1", tolerance = 0.05)
  expect_reference(centrality_extended_local_bridging(macker), two_hop * coefficient,
                   reference = "published: Macker 2016, Fig. 1 and Table I, chained with the equation 3 coefficient computed in the test",
                   covers = c("centrality()", "centrality_extended_local_bridging()"),
                   quantity = "extended_local_bridging, Macker Figure 1", tolerance = 0.05)
})

test_that("localized bridging equals NetworkX's ego betweenness times the bridging coefficient", {
  ego <- "{v: nx.betweenness_centrality(nx.ego_graph(G, v, radius=%d), normalized=False)[v] for v in G}"
  graphs <- list(`Macker Figure 1` = macker, `path of 5` = zoob_path(5), `star of 4` = zoob_star(4),
                 `complete graph on 4` = zoob_complete(4), `karate club` = karate, `weighted graph` = weighted)
  lapply(names(graphs), \(name) {
    nx <- ref_networkx(graphs[[name]], c(one = sprintf(ego, 1L), two = sprintf(ego, 2L)))
    coefficient <- bridging_coefficient(graphs[[name]])
    expect_reference(centrality_localized_bridging(graphs[[name]]), nx$one * coefficient[names(nx$one)],
                     reference = "networkx", covers = c("centrality()", "centrality_localized_bridging()"),
                     quantity = paste0("localized_bridging, ", name), tolerance = exact)
    expect_reference(centrality_extended_local_bridging(graphs[[name]]), nx$two * coefficient[names(nx$two)],
                     reference = "networkx", covers = c("centrality()", "centrality_extended_local_bridging()"),
                     quantity = paste0("extended_local_bridging, ", name), tolerance = exact)
  })
})
