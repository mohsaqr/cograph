# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("lnc normalization divides by the maximum", {
  g <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  score <- unname(centrality_lnc(g))
  expect_equal(unname(centrality_lnc(g, normalized = TRUE)),
               score / max(score))
})

test_that("lnc terms reproduce the printed v5 intermediates", {
  # Dai et al. (2019), Figure 1 and Table 1, journal page 131720.
  edges <- c(2, 1, 1, 3, 1, 4, 1, 5, 5, 6, 5, 7, 5, 8, 6, 8, 7, 8, 8, 9,
             9, 10, 9, 11, 10, 11)
  g <- igraph::make_graph(edges, directed = FALSE)
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  terms <- .cg_lnc_terms(a)
  # The three printed intermediates for v5, journal pages 131722-131723.
  expect_equal(terms$cluster_degree[5], 12)
  expect_equal(terms$own_con[5], 1.6875)
  expect_equal(terms$nei_con[5], 19.2)
  # Table 1 and the ranking are compared in tests/equivalence
  # (zoo-b-local-contribution).
})

test_that("lnc rescales when a disconnected component is added", {
  ring <- igraph::make_ring(5)
  alone <- unname(centrality_lnc(ring))
  expect_equal(alone, rep(4, 5))
  # n is the whole graph's order, so an isolate divides every score by 5/4.
  with_isolate <- igraph::disjoint_union(
    ring, igraph::make_empty_graph(1, directed = FALSE)
  )
  expect_equal(unname(centrality_lnc(with_isolate)), c(rep(16 / 5, 5), 0))
  expect_false(isTRUE(all.equal(unname(centrality_lnc(with_isolate))[1:5],
                                alone)))
  # A second ring rescales the first by (n - 1) / (n' - 1) = 4 / 9, and the
  # ranking, being constant here, is untouched.
  pair <- igraph::disjoint_union(ring, igraph::make_ring(5))
  expect_equal(unname(centrality_lnc(pair)), rep(16 / 9, 10))
  expect_equal(unname(centrality_lnc(pair))[1:5], alone * 4 / 9)
})

test_that("lnc scores isolates, singletons and empty graphs explicitly", {
  skip_on_cran()
  # A degree-zero node has no contribution probability; cograph extends the
  # source with zero rather than dividing by a zero degree.
  for (n in 0:4) {
    g <- igraph::make_empty_graph(n, directed = FALSE)
    expect_equal(unname(centrality_lnc(g)), numeric(n))
  }
  # The singleton is the case where n - 1 is also zero; nothing is NaN.
  singleton <- centrality_lnc(igraph::make_empty_graph(1, directed = FALSE))
  expect_equal(unname(singleton), 0)
  expect_false(is.nan(singleton))
  expect_length(centrality_lnc(igraph::make_empty_graph(0)), 0L)
  # An isolate beside an edge: the edge's nodes still score, the isolate
  # does not, and no value is NaN.
  g <- igraph::make_graph(c(1, 2), n = 3, directed = FALSE)
  expect_equal(unname(centrality_lnc(g)), c(0.5, 0.5, 0))
  expect_true(all(is.finite(centrality_lnc(g))))
})

test_that("lnc terms obey the cluster-degree identity", {
  # Every node's degree is counted once per neighbour, so the cluster
  # degrees sum to the sum of squared degrees. This holds on any graph.
  for (seed in 1:5) {
    set.seed(seed)
    g <- igraph::sample_gnp(12, 0.3)
    a <- as.matrix(igraph::as_adjacency_matrix(g))
    terms <- .cg_lnc_terms(a)
    expect_equal(sum(terms$cluster_degree), sum(terms$degree^2))
    # A score is zero exactly at the isolates.
    expect_equal(terms$lnc == 0, terms$degree == 0)
    expect_equal(terms$lnc, terms$own_con * terms$nei_con)
  }
})

test_that("lnc input projections and public contracts are explicit", {
  g <- igraph::make_graph(c(1, 2, 2, 1, 2, 3, 2, 3, 3, 3), directed = TRUE)
  igraph::E(g)$weight <- c(2, 8, 3, 9, 7)
  igraph::V(g)$name <- c("b", "c", "a")
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(3, 1, 2)
  score <- centrality_lnc(g)
  # The skeleton is the three-node path b - c - a: degrees 1, 2, 1, cluster
  # degrees 2, 2, 2 and n - 1 = 2, so the ends score 1 and the middle 4.
  expect_named(score, c("b", "c", "a"))
  expect_equal(unname(score), c(1, 4, 1))
  expect_equal(centrality(g, measures = "lnc")$lnc, unname(score))
  expect_equal(centrality_lnc(g, simplify = FALSE, mode = "in", loops = TRUE,
                              invert_weights = TRUE, cutoff = 1), score)
  expect_equal(centrality_lnc(g, weighted = FALSE), score)
  expect_equal(centrality_lnc(a[perm, perm]), score[perm])
})

test_that("lnc is permutation invariant on random graphs", {
  set.seed(4419)
  for (trial in seq_len(6)) {
    g <- igraph::sample_gnp(10, 0.35)
    a <- as.matrix(igraph::as_adjacency_matrix(g))
    perm <- sample.int(10)
    expect_equal(unname(centrality_lnc(a[perm, perm])),
                 unname(centrality_lnc(a))[perm])
  }
})

test_that("lnc is registered as a parameter-free topology-only measure", {
  meta <- list_centralities()
  expect_true("lnc" %in% meta$measure)
  expect_false(meta$uses_weights[meta$measure == "lnc"])
  expect_false(meta$mode_aware[meta$measure == "lnc"])
  expect_false(meta$costly[meta$measure == "lnc"])
  expect_false(meta$needs_membership[meta$measure == "lnc"])
  expect_identical(meta$orientation[meta$measure == "lnc"], "higher")
  expect_true("lnc" %in% .cg_no_mode_measures())
  expect_false("lnc" %in% .cg_mode_measures())
  # The source advertises LNC as parameter-free, so the wrapper carries no
  # tuning argument of its own beyond what centrality() already accepts.
  expect_identical(names(formals(centrality_lnc)), c("x", "..."))
})
