# Regression tests for the 2026-11-18 review of 2.7.14-2.7.16.

test_that("an undirected tna model keeps its self-loops in to_matrix() and to_df()", {
  skip_if_not_installed("tna")
  seqs <- data.frame(t1 = c("A", "B", "A", "C"), t2 = c("A", "A", "B", "C"),
                     t3 = c("B", "A", "A", "C"))
  model <- tna::ctna(seqs)
  expect_true(any(diag(model$weights) != 0))
  expect_equal(unname(to_matrix(model)), unname(model$weights))
  loops <- subset(to_df(model), from == to)
  expect_setequal(loops$from, rownames(model$weights)[diag(model$weights) != 0])
})

test_that("network_summary(weighted = FALSE) counts a zero-weight edge as a tie", {
  skip_if_not_installed("igraph")
  g <- igraph::graph_from_data_frame(
    data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"), weight = c(1, 1, 0))
  )
  ns <- network_summary(g, weighted = FALSE)
  expect_equal(ns$edge_count, 3)
  expect_equal(ns$transitivity, 1)
  # Compared with igraph::transitivity() in tests/equivalence/test-d1-regressions.R.
})

test_that("splot(isolates = FALSE) hides isolates of an edge-list data frame", {
  el <- data.frame(from = c("A", "C"), to = c("B", "C"), weight = c(1, 0))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  shown <- splot(el, isolates = FALSE)
  expect_setequal(get_labels(shown), c("A", "B"))
  all_nodes <- splot(el)
  expect_setequal(get_labels(all_nodes), c("A", "B", "C"))
})

test_that("triad_census() of an edge list does not depend on the order of duplicate rows", {
  skip_if_not_installed("igraph")
  el <- data.frame(from = c("A", "A", "B", "C"), to = c("B", "B", "C", "A"),
                   weight = c(1, -1, 1, 1))
  forward <- triad_census(el)
  reversed <- triad_census(el[c(2, 1, 3, 4), ])
  expect_equal(forward, reversed)
  # A pair with any positive row is a tie, as for igraph input edge by edge.
  expect_equal(forward$count[forward$type == "300"], 1)
  # An edge list and its matrix give the same census.
  expect_equal(triad_census(to_df(regulation_net)), triad_census(regulation_net))
})

test_that("centrality(type = 'all') on signed weights gives NA columns instead of stopping", {
  signed <- matrix(c(0, .5, -.3, 0,  .5, 0, .4, -.2,  -.3, .4, 0, .6,  0, -.2, .6, 0),
                   4, dimnames = list(letters[1:4], letters[1:4]))
  res <- suppressWarnings(centrality(signed, type = "all"))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4L)
  expect_true(all(is.na(res$closeness_all)))
  expect_true(all(is.na(res$weighted_kshell)))
  expect_false(all(is.na(res$degree_all)))
  expect_warning(centrality(signed, type = "all"), class = "cograph_undefined_measure")
  # Named explicitly, a path measure still raises its error.
  expect_error(centrality(signed, measures = "closeness"), class = "cograph_negative_weights")
  expect_error(centrality(signed, measures = "weighted_kshell"), class = "cograph_negative_weights")
})
