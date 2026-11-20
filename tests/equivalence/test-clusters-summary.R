# Cluster summaries: csum(), summarize_clusters(), summarize_network() and its alias cnet(), mcml(), and the
# conversions as_tna() and as_mcml() of a summary. A summary aggregates a node-level weight matrix into a
# cluster-by-cluster ("macro") matrix and keeps each cluster's own block; type "tna" turns each into transition
# probabilities (rows sum to 1) with initial probabilities from the column sums.
#
# References: igraph's contract() + simplify(edge.attr.comb = "sum"), which merges the nodes of each cluster
# and sums the weights of the merged edges (cograph's summaries do not call igraph), and the aggregation written
# from its definition in helper-d2.R (ref_cluster_table(): the non-zero cells of each block, aggregated).

graph <- ref_graph(9, density = 0.5, seed = 51L, directed = TRUE, weighted = TRUE, digits = 6L)
# every node a self-loop, so every within-cluster block can become a tna model
diag(graph) <- c(0.4, 0.3, 0.2, 0.5, 0.6, 0.7, 0.2, 0.3, 0.1)
clusters <- list(A = paste0("N", 1:3), B = paste0("N", 4:6), C = paste0("N", 7:9))
reference <- "independent: block aggregation of the weight matrix, base R"
# Row proportions; a row without weight stays zero, as a cluster without outgoing edges has no probabilities.
row_share <- \(m) m / ifelse(rowSums(m) > 0, rowSums(m), 1)

# A few sums and one division per number: rounding only.
exact <- 1e-12

test_that("the summed macro matrix equals igraph's contract() with summed edge weights", {
  skip_without_reference("igraph")
  mapping <- rep(seq_along(clusters), lengths(clusters))[match(rownames(graph), unlist(clusters))]
  merged <- igraph::simplify(igraph::contract(ref_igraph(graph, directed = TRUE), mapping, vertex.attr.comb = "ignore"),
                             remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb = list(weight = "sum"))
  contracted <- igraph::as_adjacency_matrix(merged, attr = "weight", sparse = FALSE)
  dimnames(contracted) <- list(names(clusters), names(clusters))
  expect_reference(csum(graph, clusters, method = "sum", type = "raw")$macro$weights, contracted,
                   reference = "igraph", covers = "csum()", quantity = "macro weights, sum, raw", tolerance = exact)
  expect_reference(to_matrix(summarize_network(graph, clusters)), contracted, reference = "igraph",
                   covers = "summarize_network()", quantity = "summary network weights, sum", tolerance = exact)
  expect_reference(to_matrix(cnet(graph, clusters)), contracted, reference = "igraph",
                   covers = c("cnet()", "summarize_network()"), quantity = "cnet() weights, sum", tolerance = exact)
})

test_that("every aggregation method gives the block aggregate of the non-zero weights", {
  methods <- list(sum = sum, mean = mean, median = stats::median, max = max, min = min, density = "density",
                  geomean = \(w) exp(mean(log(w))))
  lapply(names(methods), \(method) {
    expect_reference(csum(graph, clusters, method = method, type = "raw")$macro$weights,
                     ref_cluster_table(graph, clusters, methods[[method]]), reference = reference, covers = "csum()",
                     quantity = sprintf("macro weights, %s, raw", method), tolerance = exact)
  })
  expect_reference(summarize_clusters(graph, clusters, method = "mean", type = "raw")$macro$weights,
                   ref_cluster_table(graph, clusters, mean), reference = reference, covers = "summarize_clusters()",
                   quantity = "summarize_clusters() macro weights, mean, raw", tolerance = exact)
  expect_reference(to_matrix(summarize_network(graph, clusters, method = "max")),
                   ref_cluster_table(graph, clusters, max), reference = reference, covers = "summarize_network()",
                   quantity = "summary network weights, max", tolerance = exact)
})

test_that("an undirected summary aggregates the symmetrised weights", {
  expect_reference(csum(graph, clusters, method = "sum", type = "raw", directed = FALSE)$macro$weights,
                   ref_cluster_table((graph + t(graph)) / 2, clusters, sum), reference = reference, covers = "csum()",
                   quantity = "macro weights, sum, raw, undirected", tolerance = exact)
})

test_that("type tna gives row proportions of the summed blocks and initial probabilities from column sums", {
  summed <- ref_cluster_table(graph, clusters, sum)
  expected_inits <- colSums(summed) / sum(summed)
  summary <- csum(graph, clusters, method = "sum", type = "tna")
  expect_reference(summary$macro$weights, row_share(summed), reference = reference, covers = "csum()",
                   quantity = "macro transition probabilities", tolerance = exact)
  expect_reference(summary$macro$inits, expected_inits, reference = reference, covers = "csum()",
                   quantity = "macro initial probabilities", tolerance = exact)
  lapply(names(clusters), \(name) {
    expect_reference(summary$clusters[[name]]$weights, row_share(graph[clusters[[name]], clusters[[name]]]),
                     reference = reference, covers = "csum()",
                     quantity = sprintf("within-cluster transition probabilities, cluster %s", name), tolerance = exact)
  })
  mapped <- summarize_clusters(graph, clusters, method = "sum", type = "tna")
  expect_reference(mapped$macro$weights, row_share(summed), reference = reference, covers = "summarize_clusters()",
                   quantity = "summarize_clusters() macro transition probabilities", tolerance = exact)
  expect_reference(mcml(graph, clusters)$macro$weights, row_share(summed), reference = reference, covers = "mcml()",
                   quantity = "mcml() macro transition probabilities", tolerance = exact)
  expect_reference(mcml(graph, clusters, aggregation = "max")$macro$weights,
                   row_share(ref_cluster_table(graph, clusters, max)), reference = reference, covers = "mcml()",
                   quantity = "mcml() macro transition probabilities, max", tolerance = exact)
})

test_that("raw within-cluster blocks keep the node-level weights, self-loops included", {
  summary <- csum(graph, clusters, method = "sum", type = "raw")
  lapply(names(clusters), \(name) {
    expect_reference(summary$clusters[[name]]$weights, graph[clusters[[name]], clusters[[name]]],
                     reference = reference, covers = "csum()",
                     quantity = sprintf("within-cluster weights, raw, cluster %s", name), tolerance = 0)
  })
})

test_that("as_mcml() and as_tna() of a summary carry its probabilities", {
  summed <- ref_cluster_table(graph, clusters, sum)
  summary <- csum(graph, clusters, method = "sum", type = "tna")
  expect_reference(as_mcml(summary)$macro$weights, row_share(summed), reference = reference,
                   covers = c("as_mcml()", "as_mcml.cluster_summary()"),
                   quantity = "as_mcml() macro transition probabilities", tolerance = exact)
  skip_without_reference("tna")
  expect_reference(as_tna(summary)$macro$weights, row_share(summed), reference = reference,
                   covers = c("as_tna()", "as_tna.cluster_summary()"),
                   quantity = "as_tna() macro transition probabilities", tolerance = exact)
  expect_reference(as_tna(summarize_clusters(graph, clusters, method = "sum", type = "tna"))$macro$weights,
                   row_share(summed), reference = reference, covers = c("as_tna()", "as_tna.mcml()"),
                   quantity = "as_tna() of an mcml, macro transition probabilities", tolerance = exact)
})

test_that("as_mcml() of a grouped tna model carries each group's transition probabilities of its sequences", {
  # tna builds the input (a group_tna); the reference counts the transitions of each group's sequences in base
  # R: every pair of consecutive states of a row, the counts divided by their row sums, and the share of each
  # first state.
  skip_without_reference("tna")
  states <- c("A", "B", "C")
  sequences <- ref_with_seed(3L, as.data.frame(matrix(sample(states, 60 * 6, replace = TRUE), 60, 6)))
  groups <- rep(c("g1", "g2"), each = 30)
  result <- as_mcml(tna::group_model(sequences, group = groups), clusters = groups)
  counted <- \(rows) {
    pairs <- do.call(rbind, lapply(seq_len(ncol(rows) - 1L), \(t) data.frame(from = rows[[t]], to = rows[[t + 1L]])))
    counts <- unclass(table(factor(pairs$from, states), factor(pairs$to, states)))
    counts / rowSums(counts)
  }
  lapply(c("g1", "g2"), \(group) {
    rows <- sequences[groups == group, , drop = FALSE]
    expected <- counted(rows)
    dimnames(expected) <- list(states, states)
    expect_reference(result$clusters[[group]]$weights, expected,
                     reference = "independent: transitions counted from the group's sequences, base R",
                     covers = c("as_mcml.group_tna()", "as_mcml()"),
                     quantity = sprintf("transition probabilities, group %s", group), tolerance = exact)
    expect_reference(result$clusters[[group]]$inits, c(table(factor(rows[[1L]], states))) / nrow(rows),
                     reference = "independent: transitions counted from the group's sequences, base R",
                     covers = c("as_mcml.group_tna()", "as_mcml()"),
                     quantity = sprintf("initial probabilities, group %s", group), tolerance = exact)
  })
  expect_reference(result$macro$data, groups, reference = "independent: the assignment passed as `clusters`",
                   covers = "as_mcml.group_tna()", quantity = "row assignment kept as the macro data", tolerance = 0)
})

test_that("as_mcml() of the group_tna made from a summary keeps the summary's probabilities", {
  skip_without_reference("tna")
  summed <- ref_cluster_table(graph, clusters, sum)
  rebuilt <- as_mcml(as_tna(csum(graph, clusters, method = "sum", type = "tna")))
  lapply(names(clusters), \(name) {
    expect_reference(rebuilt$clusters[[name]]$weights, row_share(graph[clusters[[name]], clusters[[name]]]),
                     reference = reference, covers = c("as_mcml.group_tna()", "as_mcml()"),
                     quantity = sprintf("within-cluster transition probabilities after as_tna(), cluster %s", name),
                     tolerance = exact)
  })
  expect_reference(rebuilt$macro$weights, row_share(summed), reference = reference,
                   covers = c("as_mcml.group_tna()", "as_mcml()"),
                   quantity = "macro transition probabilities after as_tna()", tolerance = exact,
                   defect = paste("as_mcml() of a node-level group_tna without data rebuilds the macro matrix from the",
                                  "within-cluster blocks only and ignores the group's macro model: the macro transitions",
                                  "become the identity; FINDINGS.md"))
})
