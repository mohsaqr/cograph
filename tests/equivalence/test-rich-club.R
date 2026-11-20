# rich_club() (the rich-club curve, Zhou & Mondragon 2004; weighted, Opsahl et al. 2008), network_rich_club()
# (one threshold) and rich_club_local() (Opsahl's local rich-club ratio), compared with NetworkX
# (rich_club_coefficient(normalized=False)), tnet (weighted_richclub_local_w()) and the definitions. cograph
# counts the club's ties itself (igraph::induced_subgraph() and ecount() in R/rich-club.R and
# R/network-summary.R serve as containers), so these are counted references. The normalised curve divides by graphs
# drawn inside the function; only the observed coefficient phi is compared.
#
# Graphs: random undirected graphs from helper-reference.R; six-decimal weights for the weighted club.

undirected <- ref_graph(16, density = 0.3, seed = 91L)
undirected_weighted <- ref_graph(16, density = 0.3, seed = 92L, weighted = TRUE, digits = 6L)

exact <- 1e-10

# The thresholds at which rich_club() reports phi: each degree below the largest, when at least two nodes lie above.
thresholds_of <- \(graph) {
  degree <- rowSums(d1_binary(graph))
  levels <- sort(unique(degree))
  levels[levels < max(degree) & vapply(levels, \(r) sum(degree > r) >= 2, logical(1))]
}

test_that("rich_club(weighted = FALSE) equals NetworkX's rich_club_coefficient(normalized = False)", {
  nx <- ref_networkx(undirected, c(phi = "nx.rich_club_coefficient(G, normalized=False)"))
  levels <- thresholds_of(undirected)
  expect_reference(rich_club(undirected, weighted = FALSE, normalized = FALSE),
                   data.frame(threshold = levels, phi = unname(nx$phi[as.character(levels)])),
                   reference = "networkx", covers = "rich_club()", quantity = "rich-club coefficient, unweighted",
                   tolerance = exact, key = "threshold")
  expect_reference(vapply(c(`3` = 3, `4` = 4, `5` = 5), \(k) network_rich_club(undirected, k = k), numeric(1)),
                   nx$phi[c("3", "4", "5")], reference = "networkx", covers = "network_rich_club()",
                   quantity = "rich-club coefficient at k", tolerance = exact)
})

test_that("rich_club(weighted = TRUE) equals Opsahl's weighted rich-club coefficient", {
  levels <- thresholds_of(undirected_weighted)
  expect_reference(rich_club(undirected_weighted, weighted = TRUE, normalized = FALSE),
                   d1_weighted_rich_club(undirected_weighted, levels),
                   reference = "independent: weighted rich-club coefficient (Opsahl et al. 2008, eq. 2)",
                   covers = "rich_club()", quantity = "rich-club coefficient, weighted", tolerance = exact,
                   key = "threshold")
})

test_that("rich_club_local() equals tnet's weighted_richclub_local_w()", {
  skip_without_reference("tnet")
  # Prominent nodes: degree above the median (rich_club_local()'s default), given to both as a 0/1 vector.
  prominent <- as.integer(rowSums(d1_binary(undirected_weighted)) > stats::median(rowSums(d1_binary(undirected_weighted))))
  ties <- which(undirected_weighted != 0, arr.ind = TRUE)
  edge_list <- cbind(i = ties[, 1L], j = ties[, 2L], w = undirected_weighted[ties])
  from_tnet <- tnet::weighted_richclub_local_w(edge_list, prominent)
  expect_reference(rich_club_local(undirected_weighted, prominence = prominent),
                   data.frame(node = rownames(undirected_weighted)[from_tnet[, "node"]], score = from_tnet[, "ratio"]),
                   reference = "tnet", covers = "rich_club_local()", quantity = "local rich-club ratio, weighted",
                   tolerance = exact, key = "node")
})

test_that("rich_club_local() equals the local rich-club ratio from its definition", {
  # Opsahl's local ratio: the mean weight of a node's ties to prominent nodes over the mean weight of all its ties;
  # 1 for a node without ties or without prominent neighbours.
  degree <- rowSums(d1_binary(undirected_weighted))
  prominent <- degree > stats::median(degree)
  ratio <- vapply(seq_len(nrow(undirected_weighted)), \(i) {
    ties <- undirected_weighted[i, ]
    to_rich <- ties[ties > 0 & prominent]
    if (!any(ties > 0) || !length(to_rich)) 1 else mean(to_rich) / mean(ties[ties > 0])
  }, numeric(1))
  expect_reference(rich_club_local(undirected_weighted), data.frame(node = rownames(undirected_weighted), score = ratio),
                   reference = "independent: local rich-club ratio (Opsahl et al. 2008)", covers = "rich_club_local()",
                   quantity = "local rich-club ratio, default prominence", tolerance = exact, key = "node")
})
