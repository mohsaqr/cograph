# Comparisons moved from tests/testthat/test-input-consistency.R and tests/testthat/test-cluster-metrics.R,
# where they compared cograph with tna, with igraph or with a value computed from the definition. The unit tests
# that compare cograph with itself stayed there.

exact <- 1e-12

test_that("to_matrix() and threshold_edges() of a sequence tna read the model's transition probabilities", {
  # from test-input-consistency.R: "to_matrix and to_df return the model weights of a sequence tna" and
  # "threshold_edges on a sequence tna keeps real weights"
  skip_without_reference("tna")
  model <- tna::tna(tna::group_regulation)
  weights <- model$weights
  expect_reference(to_matrix(model), unclass(weights), reference = "tna", covers = "to_matrix()",
                   quantity = "weights of a sequence tna, as stored by tna::tna()", tolerance = 0)
  kept <- weights * (weights >= 0.1)
  expect_reference(to_matrix(threshold_edges(model, minimum = 0.1)), unclass(kept),
                   reference = "independent: weights of at least 0.1 kept, base R", covers = "threshold_edges()",
                   quantity = "sequence tna, minimum 0.1", tolerance = 0)
  expect_reference(n_edges(threshold_edges(model, minimum = 0.1)), sum(weights >= 0.1),
                   reference = "independent: weights of at least 0.1 kept, base R", covers = "threshold_edges()",
                   quantity = "sequence tna, minimum 0.1, number of edges kept", tolerance = 0)
})

test_that("network_summary() transitivity of an undirected simple graph equals igraph's transitivity()", {
  # from test-input-consistency.R: "network_summary transitivity matches hand counts"
  skip_without_reference("igraph")
  ring_chord <- igraph::make_ring(6) + igraph::edge(1, 3)
  expect_reference(network_summary(ring_chord, digits = NULL)$transitivity,
                   igraph::transitivity(ring_chord, type = "global"), reference = "igraph",
                   covers = "network_summary()", quantity = "global transitivity, ring of 6 with a chord",
                   tolerance = exact)
})

# The network of tests/testthat/test-cluster-metrics.R: uniform weights on 10 nodes, three clusters.
metrics_net <- local({
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()),
          add = TRUE, after = FALSE)
  set.seed(42)
  graph <- matrix(stats::runif(100), 10, 10)
  diag(graph) <- 0
  dimnames(graph) <- list(paste0("N", 1:10), paste0("N", 1:10))
  graph
})
metrics_clusters <- list(A = c("N1", "N2", "N3"), B = c("N4", "N5", "N6"), C = c("N7", "N8", "N9", "N10"))

test_that("cluster summaries and layer aggregates equal their definitions on the unit-test network", {
  # from test-cluster-metrics.R: "aggregate_weights works correctly", "cluster_summary works with list input",
  # "self-loops are preserved in macro diagonal and cluster matrices" and "aggregate_layers works correctly"
  reference <- "independent: block sums and cell-wise sums, base R"
  values <- c(1, 2, 3, 4, 5)
  expect_reference(aggregate_weights(values, "geomean"), exp(mean(log(values))), reference = reference,
                   covers = "aggregate_weights()", quantity = "geometric mean of 1 to 5", tolerance = 1e-10)
  expect_reference(csum(metrics_net, metrics_clusters, method = "sum", type = "raw")$macro$weights,
                   ref_cluster_table(metrics_net, metrics_clusters, sum), reference = reference, covers = "csum()",
                   quantity = "macro weights, sum, raw, unit-test network", tolerance = 1e-10)
  looped <- matrix(0.1, 5, 5, dimnames = list(paste0("N", 1:5), paste0("N", 1:5)))
  diag(looped) <- 0.6
  looped_clusters <- list(A = c("N1", "N2"), B = c("N3", "N4", "N5"))
  expect_reference(csum(looped, looped_clusters, method = "sum", type = "raw")$macro$weights,
                   ref_cluster_table(looped, looped_clusters, sum), reference = reference, covers = "csum()",
                   quantity = "macro weights with self-loops, sum, raw", tolerance = 1e-10)
  layers <- list(L1 = metrics_net, L2 = metrics_net * 2, L3 = metrics_net * 3)
  expect_reference(aggregate_layers(layers, method = "sum"), metrics_net * 6, reference = reference,
                   covers = "aggregate_layers()", quantity = "sum of three scaled layers", tolerance = 1e-10)
  expect_reference(aggregate_layers(layers, method = "mean"), metrics_net * 2, reference = reference,
                   covers = "aggregate_layers()", quantity = "mean of three scaled layers", tolerance = 1e-10)
  expect_reference(aggregate_layers(layers, method = "max"), metrics_net * 3, reference = reference,
                   covers = "aggregate_layers()", quantity = "maximum of three scaled layers", tolerance = 1e-10)
  expect_reference(aggregate_layers(layers, method = "sum", weights = c(1, 2, 0)), metrics_net * 5,
                   reference = reference, covers = "aggregate_layers()",
                   quantity = "weighted sum of three scaled layers", tolerance = 1e-10)
})
