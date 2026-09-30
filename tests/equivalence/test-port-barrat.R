# Barrat weighted transitivity: native kernel against igraph on a weighted zoo.

test_that(".cg_barrat_transitivity matches igraph::transitivity(type = 'barrat')", {
  skip_if_not_installed("igraph")
  grid <- expand.grid(n = c(3L, 5L, 8L, 12L, 20L), density = c(0.2, 0.5, 0.9), seed = 1:4,
                      KEEP.OUT.ATTRS = FALSE)
  checked <- 0L
  for (i in seq_len(nrow(grid))) {
    # one graph per design row
    set.seed(grid$seed[i]); n <- grid$n[i]
    m <- matrix(0, n, n); idx <- which(upper.tri(m))
    take <- idx[stats::runif(length(idx)) < grid$density[i]]
    m[take] <- stats::runif(length(take), 0.1, 5); m <- m + t(m)
    g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", weighted = TRUE)
    ref <- igraph::transitivity(g, type = "barrat")
    got <- .cg_barrat_transitivity(m, directed = FALSE)
    expect_identical(is.nan(ref), is.nan(got))
    expect_equal(got[!is.nan(ref)], ref[!is.nan(ref)], tolerance = 1e-12)
    checked <- checked + 1L
  }
  expect_identical(checked, nrow(grid))
})

test_that("centrality(transitivity_type = 'barrat') is native and refuses directed input", {
  m <- test_network("karate")
  old <- options(cograph.forbid_igraph = TRUE); on.exit(options(old), add = TRUE)
  out <- centrality(m, measures = "transitivity", transitivity_type = "barrat")
  expect_identical(nrow(out), 34L)
  expect_true(all(is.finite(out$transitivity) | is.nan(out$transitivity)))
  d <- test_network("macaque")
  expect_error(centrality(d, measures = "transitivity", transitivity_type = "barrat"),
               class = "cograph_directed_unsupported")
})
