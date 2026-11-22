# Cluster summaries and conversions set beside tna (the gold standard reference), in addition to the block
# aggregation of test-clusters-summary.R. The input is sequences drawn by simulab from a known transition matrix
# (helper-simulab.R); tna::ftna() counts their transitions.
#
# The summed macro matrix of a count matrix is the matrix of transitions between clusters: relabel every state by
# its cluster and count again. Row proportions of it are therefore tna::tna() of the relabelled sequences. The
# within-group models of as_mcml() of a group_tna are tna::tna() of each group's sequences.

sequences <- lapply(c(61L, 62L), \(seed) sim_sequences(300, seed = seed, n_states = 6, chain_length = 15))
relabel <- \(rows, clusters) {
  label <- stats::setNames(rep(names(clusters), lengths(clusters)), unlist(clusters))
  as.data.frame(lapply(rows, \(column) unname(label[column])))
}
# three clusters of two states, in the alphabetical order tna gives the states
clusters_of <- \(counts) {
  states <- rownames(counts)
  list(A = states[1:2], B = states[3:4], C = states[5:6])
}

# Sums of counts and one division per number: rounding only.
exact <- 1e-12

test_that("the macro transition probabilities of a count matrix equal tna's model of the relabelled sequences", {
  skip_without_reference("tna")
  lapply(seq_along(sequences), \(draw) {
    rows <- sequences[[draw]]
    counts <- tna::ftna(rows)$weights
    clusters <- clusters_of(counts)
    expected <- tna::tna(relabel(rows, clusters))$weights[names(clusters), names(clusters)]
    expect_reference(mcml(counts, clusters)$macro$weights, expected, reference = "tna", covers = "mcml()",
                     quantity = sprintf("mcml() macro transition probabilities, draw %d", draw), tolerance = exact)
    summary <- summarize_clusters(counts, clusters, method = "sum", type = "tna")
    expect_reference(summary$macro$weights, expected, reference = "tna", covers = "summarize_clusters()",
                     quantity = sprintf("summarize_clusters() macro transition probabilities, draw %d", draw),
                     tolerance = exact)
    expect_reference(csum(counts, clusters, method = "sum", type = "tna")$macro$weights, expected, reference = "tna",
                     covers = "csum()", quantity = sprintf("csum() macro transition probabilities, draw %d", draw),
                     tolerance = exact)
    expect_reference(as_mcml(csum(counts, clusters, method = "sum", type = "tna"))$macro$weights, expected,
                     reference = "tna", covers = c("as_mcml()", "as_mcml.cluster_summary()"),
                     quantity = sprintf("as_mcml() of a summary, macro transition probabilities, draw %d", draw),
                     tolerance = exact)
    expect_reference(as_tna(csum(counts, clusters, method = "sum", type = "tna"))$macro$weights, expected,
                     reference = "tna", covers = c("as_tna()", "as_tna.cluster_summary()"),
                     quantity = sprintf("as_tna() of a summary, macro transition probabilities, draw %d", draw),
                     tolerance = exact)
    expect_reference(as_tna(mcml(counts, clusters))$macro$weights, expected, reference = "tna",
                     covers = c("as_tna()", "as_tna.mcml()"),
                     quantity = sprintf("as_tna() of an mcml, macro transition probabilities, draw %d", draw),
                     tolerance = exact)
  })
})

test_that("as_mcml() of a group_tna holds tna's model of each group's sequences", {
  skip_without_reference("tna")
  lapply(seq_along(sequences), \(draw) {
    rows <- sequences[[draw]]
    groups <- rep(c("g1", "g2", "g3"), each = 100)
    result <- as_mcml(tna::group_model(rows, group = groups), clusters = groups)
    lapply(c("g1", "g2", "g3"), \(group) {
      model <- tna::tna(rows[groups == group, , drop = FALSE])
      expect_reference(result$clusters[[group]]$weights, model$weights, reference = "tna",
                       covers = c("as_mcml.group_tna()", "as_mcml()"),
                       quantity = sprintf("transition probabilities, group %s, draw %d", group, draw), tolerance = exact)
      expect_reference(result$clusters[[group]]$inits, model$inits, reference = "tna",
                       covers = c("as_mcml.group_tna()", "as_mcml()"),
                       quantity = sprintf("initial probabilities, group %s, draw %d", group, draw), tolerance = exact)
    })
  })
})
