# Regression tests for the network-level bugs found during the 2026-10
# help-page revision (bugs 19-27 and 29).

# A minimal tna-shaped object: the efficiency functions only test the class.
.bug_tna <- function(w) {
  structure(list(weights = w, labels = rownames(w),
                 inits = rep(1 / nrow(w), nrow(w))),
            class = c("tna", "list"))
}

test_that("bug 19: estrada_index() equals trace(exp(A)) on a directed cycle", {
  # Directed 3-cycle: complex eigenvalues 1, exp(+-2 pi i / 3). A^3 = I, so
  # trace(A^k) is 3 when k is a multiple of 3 and 0 otherwise.
  a <- matrix(0, 3, 3)
  a[cbind(1:3, c(2, 3, 1))] <- 1
  k <- seq(0, 30, by = 3)
  expected <- 3 * sum(1 / factorial(k))
  expect_equal(estrada_index(a), expected, tolerance = 1e-12)
  # The old formula summed exp(Re(lambda)) and gave e + 2 exp(-1/2).
  expect_false(isTRUE(all.equal(estrada_index(a), exp(1) + 2 * exp(-0.5))))
})

test_that("bug 19: estrada_index() on an undirected graph is unchanged", {
  a <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3)
  ev <- eigen(a, symmetric = TRUE, only.values = TRUE)$values
  expect_equal(estrada_index(a), sum(exp(ev)))
})

test_that("bug 20: efficiency functions accept weights = NA on tna input", {
  skip_if_not_installed("igraph")
  x <- .bug_tna(regulation_net)
  g <- to_igraph(x)
  expect_equal(network_global_efficiency(x, weights = NA),
               igraph::global_efficiency(g, weights = NA))
  expect_equal(network_local_efficiency(x, weights = NA),
               igraph::average_local_efficiency(g, weights = NA))
  # weights = NA is unweighted, so tna and matrix input agree.
  expect_equal(network_global_efficiency(x, weights = NA),
               network_global_efficiency(regulation_net, weights = NA))
})

test_that("bug 21: simplify() rebuilds $weights after merging duplicates", {
  m <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  m[1, 2] <- 1
  m[2, 3] <- 2
  m[3, 3] <- 4
  net <- cograph(m, directed = TRUE)
  net$edges <- rbind(net$edges, data.frame(from = 1L, to = 2L, weight = 3))

  s <- cograph::simplify(net, edge_attr_comb = "sum")
  expected <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  expected[1, 2] <- 4
  expected[2, 3] <- 2
  expect_equal(s$weights, expected)
  expect_equal(nrow(s$edges), 2L)

  s_mean <- cograph::simplify(net, edge_attr_comb = "mean")
  expect_equal(s_mean$weights["A", "B"], 2)
})

test_that("bug 22: cluster_significance() null graphs carry the observed weights", {
  skip_if_not_installed("igraph")
  # A complete graph: every G(n, m) draw with m = n (n - 1) / 2 is the same
  # graph, so unweighted nulls all have one modularity (sd 0). Weighted nulls
  # differ only in how the observed weights are assigned to the edges.
  w <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
  w[upper.tri(w)] <- c(9, 1, 8, 2, 7, 3, 6, 4, 5, 10)
  w <- w + t(w)
  mem <- c(1, 1, 2, 2, 2)

  res <- cluster_significance(w, mem, n_random = 25, method = "gnm",
                              null = "fixed", seed = 7)
  expect_gt(res$null_sd, 0)
  expect_false(is.na(res$z_score))

  # Independent computation of the first null value.
  g_obs <- igraph::graph_from_adjacency_matrix(w, weighted = TRUE,
                                               mode = "directed")
  obs_w <- igraph::E(g_obs)$weight
  set.seed(7)
  g_null <- igraph::sample_gnm(5, length(obs_w), directed = TRUE)
  igraph::E(g_null)$weight <- obs_w[sample.int(length(obs_w))]
  expect_equal(res$null_values[1], igraph::modularity(g_null, mem))

  # Equal weights reduce to the unweighted null: modularity is scale free.
  flat <- (w > 0) * 2
  res_flat <- cluster_significance(flat, mem, n_random = 5, method = "gnm",
                                   null = "fixed", seed = 7)
  g_flat <- igraph::graph_from_adjacency_matrix((w > 0) * 1, mode = "directed")
  expect_equal(res_flat$null_values,
               rep(igraph::modularity(g_flat, mem), 5))
})

test_that("bug 23: csum(directed = FALSE) symmetrizes before aggregation", {
  m <- matrix(c(0, 4, 0, 0,
                0, 0, 2, 0,
                0, 0, 0, 6,
                1, 0, 0, 0), 4, byrow = TRUE,
              dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  cl <- list(X = c("A", "B"), Y = c("C", "D"))
  res <- csum(m, cl, type = "raw", directed = FALSE)
  # Symmetrized: A-B 2, B-C 1, C-D 3, D-A 0.5 in both directions.
  expected <- matrix(c(4, 1.5, 1.5, 6), 2, dimnames = list(c("X", "Y"),
                                                           c("X", "Y")))
  expect_equal(res$macro$weights, expected)
  expect_false(res$meta$directed)
  # The directed default is unchanged.
  dir <- csum(m, cl, type = "raw")
  expect_equal(unname(dir$macro$weights), matrix(c(4, 1, 2, 6), 2))
  # The matrix route of summarize_clusters() follows csum().
  sc <- summarize_clusters(m, cl, type = "raw", directed = FALSE)
  expect_equal(sc$macro$weights, expected)
})

test_that("bug 23: summarize_clusters() reads V1, V2, ... as sequence data", {
  seqs <- data.frame(V1 = c("A", "A", "C"), V2 = c("B", "C", "D"),
                     V3 = c("C", "D", "A"))
  cl <- list(X = c("A", "B"), Y = c("C", "D"))
  # Transitions: A-B, B-C, A-C, C-D, C-D, D-A.
  res <- summarize_clusters(seqs, cl, type = "raw")
  expect_equal(unname(res$macro$weights), matrix(c(1, 1, 2, 2), 2))
  expect_equal(nrow(res$edges), 6L)

  und <- summarize_clusters(seqs, cl, type = "raw", directed = FALSE)
  expect_equal(unname(und$macro$weights), matrix(c(1, 1.5, 1.5, 2), 2))
  # The edges table still reports the observed transitions.
  expect_equal(nrow(und$edges), 6L)

  # An explicit edge list with V1, V2 and a weight column is still an edge list.
  el <- data.frame(V1 = c("A", "C"), V2 = c("C", "D"), weight = c(2, 5))
  res_el <- summarize_clusters(el, cl, type = "raw")
  expect_equal(unname(res_el$macro$weights), matrix(c(0, 0, 2, 5), 2))
})

test_that("bug 24: aggregate_layers() binarizes a single layer", {
  m <- matrix(c(0, 2, 0.5, 0), 2, dimnames = list(c("a", "b"), c("a", "b")))
  binary <- matrix(c(0, 1, 1, 0), 2, dimnames = dimnames(m))
  expect_equal(aggregate_layers(list(m), method = "union"), binary)
  expect_equal(aggregate_layers(list(m), method = "intersection"), binary)
  expect_equal(aggregate_layers(list(m), method = "sum", weights = 3), m * 3)
  expect_equal(aggregate_layers(list(m), method = "sum"), m)
  # Several layers: union / intersection as before.
  m2 <- (m > 1) * 1
  expect_equal(unname(aggregate_layers(list(m, m2), method = "intersection")),
               matrix(c(0, 1, 0, 0), 2))
  expect_equal(unname(aggregate_layers(list(m, m2), method = "union")),
               matrix(c(0, 1, 1, 0), 2))
})

test_that("bug 24: cluster_quality() density leaves self-loops out", {
  x <- matrix(c(5, 1, 0,
                1, 0, 1,
                0, 1, 0), 3, dimnames = list(letters[1:3], letters[1:3]))
  q <- cluster_quality(x, list(A = c("a", "b"), B = "c"), directed = FALSE)
  # Cluster A: one pair (a, b) with weight 1, one possible pair.
  expect_equal(q$per_cluster$internal_density[1], 1)
  # internal_edges still counts the loop (5 / 2 + 1).
  expect_equal(q$per_cluster$internal_edges[1], 3.5)

  qd <- cluster_quality(x, list(A = c("a", "b"), B = "c"), directed = TRUE)
  # Directed: a->b and b->a, weight 1 each, over 2 possible ordered pairs.
  expect_equal(qd$per_cluster$internal_density[1], 1)
})

test_that("bug 25: rich_club() warns when null draws fail", {
  skip_if_not_installed("igraph")
  real_degseq <- igraph::sample_degseq
  calls <- 0L
  local_mocked_bindings(
    sample_degseq = function(...) {
      calls <<- calls + 1L
      if (calls %% 2L == 0L) stop("no graph")
      real_degseq(...)
    },
    .package = "igraph"
  )
  expect_warning(
    res <- rich_club(regulation_net, n_random = 6, seed = 1),
    regexp = "3 of 6 null graphs",
    class = "cograph_null_draw_failed"
  )
  expect_true("phi_norm" %in% names(res))
})

test_that("bug 25: rich_club() gives no warning when every draw succeeds", {
  skip_if_not_installed("igraph")
  expect_no_warning(rich_club(regulation_net, n_random = 5, seed = 1))
})

test_that("bug 26: nodes() signals its deprecation", {
  net <- cograph(regulation_net)
  expect_warning(out <- nodes(net), class = "deprecatedWarning")
  expect_identical(out, get_nodes(net))
})

test_that("bug 27: split_components() on a 0 x 0 matrix warns and returns list()", {
  empty <- matrix(numeric(0), 0, 0)
  expect_warning(res <- split_components(empty), "no nodes")
  expect_identical(res, list())
  expect_identical(n_nodes(as_cograph(empty)), 0L)
})

test_that("bug 29: is_bipartite() handles a named non-bipartite graph", {
  tri <- matrix(1, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  diag(tri) <- 0
  expect_false(is_bipartite(tri))
  expect_identical(is_bipartite(tri), is_bipartite(unname(tri)))
  expect_identical(is_bipartite(tri), cograph:::.is_bipartite_bfs(tri))

  sq <- matrix(c(0, 1, 0, 1,
                 1, 0, 1, 0,
                 0, 1, 0, 1,
                 1, 0, 1, 0), 4, dimnames = list(letters[1:4], letters[1:4]))
  expect_true(is_bipartite(sq))
  expect_false(is_bipartite(regulation_net))
})
