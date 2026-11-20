# The verbs that change edge weights: symmetrize(), to_undirected(), normalize_weights(), invert_weights(),
# threshold_edges(), binarize() and aggregate_weights() with its alias wagg(). cograph computes all of these
# itself in base R (no igraph call). References: sna's symmetrize() for its four rules, igraph's
# as_undirected(mode = "collapse") for the ways of merging two arcs into one edge, and each operation written
# from its definition (the formula is given beside each comparison).
#
# Graphs: a directed weighted random graph with some reciprocated and some one-way arcs, so that every rule of
# merging is exercised, and its 0/1 version for the rules that sna defines on binary relations.

directed <- ref_graph(8, density = 0.4, seed = 61L, directed = TRUE, weighted = TRUE, digits = 6L)
binary <- (directed != 0) * 1
undirected <- ref_graph(8, density = 0.45, seed = 62L, weighted = TRUE, digits = 6L)

# One arithmetic operation per number: rounding only.
exact <- 1e-12

test_that("symmetrize() equals sna's symmetrize() for the weak, strong, upper and lower rules", {
  skip_without_reference("sna")
  # sna's "weak" (a tie if either arc exists) is cograph's "max" on a 0/1 relation; "strong" (both arcs) is
  # "mutual".
  rules <- c(max = "weak", mutual = "strong", upper = "upper", lower = "lower")
  lapply(names(rules), \(method) {
    reference_matrix <- sna::symmetrize(binary, rule = rules[[method]])
    dimnames(reference_matrix) <- dimnames(binary)
    expect_reference(to_matrix(symmetrize(binary, method = method)), reference_matrix, reference = "sna",
                     covers = "symmetrize()", quantity = sprintf("symmetrize %s, sna rule %s, binary", method, rules[[method]]),
                     tolerance = 0)
  })
})

test_that("to_undirected() equals igraph's as_undirected(mode = 'collapse') for sum, max, min and mean", {
  skip_without_reference("igraph")
  graph <- ref_igraph(directed, directed = TRUE, weighted = TRUE)
  lapply(c("sum", "max", "min", "mean"), \(method) {
    collapsed <- igraph::as_undirected(graph, mode = "collapse", edge.attr.comb = list(weight = method))
    expect_reference(to_matrix(to_undirected(directed, method = method)),
                     igraph::as_adjacency_matrix(collapsed, attr = "weight", sparse = FALSE),
                     reference = "igraph", covers = "to_undirected()", quantity = sprintf("to_undirected %s", method),
                     tolerance = exact)
  })
})

test_that("symmetrize() of weights equals the combination of the two arcs, from the definition", {
  # For each pair: the arcs present are combined (max, min, sum, mean of the present arcs); mutual keeps the
  # smaller weight of a reciprocated pair and drops a one-way arc.
  forward <- directed
  backward <- t(directed)
  present <- (forward != 0) + (backward != 0)
  expected <- list(max = pmax(forward, backward), sum = forward + backward,
                   mean = (forward + backward) / pmax(present, 1),
                   min = ifelse(present == 2, pmin(forward, backward), forward + backward),
                   mutual = ifelse(present == 2, pmin(forward, backward), 0))
  lapply(names(expected), \(method) {
    reference_matrix <- expected[[method]]
    dimnames(reference_matrix) <- dimnames(directed)
    expect_reference(to_matrix(symmetrize(directed, method = method)), reference_matrix,
                     reference = "independent: combination of the two arcs of each pair, base R",
                     covers = "symmetrize()", quantity = sprintf("symmetrize %s, weighted", method), tolerance = exact)
  })
})

test_that("normalize_weights() divides by the row, column, maximum or total weight, or rescales to [0, 1]", {
  reference <- "independent: weight normalisation, base R"
  expect_reference(to_matrix(normalize_weights(directed[rowSums(directed) > 0, rowSums(directed) > 0], method = "row")),
                   prop.table(directed[rowSums(directed) > 0, rowSums(directed) > 0], 1L), reference = reference,
                   covers = "normalize_weights()", quantity = "row proportions", tolerance = exact)
  expect_reference(to_matrix(normalize_weights(directed[colSums(directed) > 0, colSums(directed) > 0], method = "column")),
                   prop.table(directed[colSums(directed) > 0, colSums(directed) > 0], 2L), reference = reference,
                   covers = "normalize_weights()", quantity = "column proportions", tolerance = exact)
  expect_reference(to_matrix(normalize_weights(directed, method = "max")), directed / max(directed),
                   reference = reference, covers = "normalize_weights()", quantity = "divided by the largest weight",
                   tolerance = exact)
  expect_reference(to_matrix(normalize_weights(directed, method = "sum")), directed / sum(directed),
                   reference = reference, covers = "normalize_weights()", quantity = "divided by the total weight",
                   tolerance = exact)
  # (w - min) / (max - min) over the edges; the smallest edge would become 0, which is "no edge", so it is kept
  # at the smallest double above zero (2.2e-16): within the tolerance of 0 for that one edge.
  edge_weights <- directed[directed != 0]
  rescaled <- directed
  rescaled[directed != 0] <- (edge_weights - min(edge_weights)) / (max(edge_weights) - min(edge_weights))
  expect_reference(to_matrix(normalize_weights(directed, method = "minmax")), rescaled, reference = reference,
                   covers = "normalize_weights()", quantity = "min-max rescaled edge weights", tolerance = exact)
})

test_that("invert_weights() takes the reciprocal, the distance from the maximum, or the reflection", {
  reference <- "independent: weight inversion, base R"
  edge_weights <- directed[directed != 0]
  inverted <- \(values) {
    out <- directed
    out[directed != 0] <- values
    out
  }
  expect_reference(to_matrix(invert_weights(directed, method = "reciprocal")), inverted(1 / edge_weights),
                   reference = reference, covers = "invert_weights()", quantity = "reciprocal", tolerance = exact)
  # max - w is zero for the heaviest edge, which is then no edge
  expect_reference(to_matrix(invert_weights(directed, method = "max_minus")), inverted(max(edge_weights) - edge_weights),
                   reference = reference, covers = "invert_weights()", quantity = "maximum minus weight", tolerance = exact)
  expect_reference(to_matrix(invert_weights(directed, method = "reflect")),
                   inverted(max(edge_weights) + min(edge_weights) - edge_weights), reference = reference,
                   covers = "invert_weights()", quantity = "reflection max + min - w", tolerance = exact)
})

test_that("threshold_edges() and binarize() keep the edges their rules select", {
  reference <- "independent: edge selection by weight, base R"
  keep_if <- \(rule) directed * rule
  # the k strongest edges, ties at the cut kept
  strongest <- \(k) directed >= sort(directed[directed != 0], decreasing = TRUE)[[k]]
  edge_count <- sum(directed != 0)
  possible <- nrow(directed) * (nrow(directed) - 1)
  expect_reference(to_matrix(threshold_edges(directed, minimum = 0.5)), keep_if(directed >= 0.5), reference = reference,
                   covers = "threshold_edges()", quantity = "minimum 0.5", tolerance = 0)
  expect_reference(to_matrix(threshold_edges(directed, maximum = 0.5)), keep_if(directed <= 0.5), reference = reference,
                   covers = "threshold_edges()", quantity = "maximum 0.5", tolerance = 0)
  expect_reference(to_matrix(threshold_edges(directed, top = 7)), keep_if(strongest(7)), reference = reference,
                   covers = "threshold_edges()", quantity = "top 7", tolerance = 0)
  expect_reference(to_matrix(threshold_edges(directed, proportion = 0.4)), keep_if(strongest(ceiling(0.4 * edge_count))),
                   reference = reference, covers = "threshold_edges()", quantity = "proportion 0.4", tolerance = 0)
  expect_reference(to_matrix(threshold_edges(directed, density = 0.1)), keep_if(strongest(ceiling(0.1 * possible))),
                   reference = reference, covers = "threshold_edges()", quantity = "density 0.1", tolerance = 0)
  expect_reference(to_matrix(binarize(directed, threshold = 0.4)), (directed > 0.4) * 1, reference = reference,
                   covers = "binarize()", quantity = "binarize above 0.4", tolerance = 0)
  signed <- directed * ifelse(upper.tri(directed), -1, 1)
  expect_reference(to_matrix(binarize(signed, threshold = 0.4, signed = TRUE)), sign(signed) * (abs(signed) > 0.4),
                   reference = reference, covers = "binarize()", quantity = "binarize signed, absolute above 0.4",
                   tolerance = 0)
})

test_that("aggregate_weights() and wagg() equal their summaries of the non-zero weights", {
  weights <- c(0.4, 0, 1.7, 2.5, NA, 0.9, 3.1)
  present <- c(0.4, 1.7, 2.5, 0.9, 3.1)
  expected <- c(sum = sum(present), mean = mean(present), median = stats::median(present), max = max(present),
                min = min(present), prod = prod(present), geomean = exp(mean(log(present))),
                density = sum(present) / 12)
  computed <- vapply(names(expected), \(method) {
    aggregate_weights(weights, method = method, n_possible = if (method == "density") 12 else NULL)
  }, numeric(1))
  expect_reference(computed, expected, reference = "independent: summaries of the non-zero weights, base R",
                   covers = "aggregate_weights()", quantity = "aggregate of 5 non-zero weights", tolerance = exact)
  expect_reference(wagg(weights, method = "geomean"), exp(mean(log(present))),
                   reference = "independent: summaries of the non-zero weights, base R",
                   covers = c("wagg()", "aggregate_weights()"), quantity = "wagg() geometric mean", tolerance = exact)
})
