# Regression tests for the 2026-10 algorithm fixes (bugs 3-7, 9, 13 of the
# help-page revision list). Each test checks the formula against a hand
# computation or an independent implementation.

bin_adj <- function(x) {
  a <- (x != 0) * 1
  diag(a) <- 0
  a
}

# Matrix exponential by its Taylor series, an independent reference for
# small matrices with modest norm.
taylor_expm <- function(a, terms = 60L) {
  n <- nrow(a)
  # A^k / k!, each term built from the previous one.
  series <- Reduce(function(term, k) term %*% a / k, seq_len(terms),
                   accumulate = TRUE, init = diag(1, n, n))
  Reduce(`+`, series)
}

test_that("bug 3: communicability on directed input is rowSums(expm(A))", {
  a <- bin_adj(regulation_net)
  expect_false(isSymmetric(unname(a)))
  got <- centrality_communicability(regulation_net)
  expect_equal(unname(got), rowSums(taylor_expm(a)), tolerance = 1e-10, ignore_attr = TRUE)
  # The old t(V) shortcut gave 9.11 for Explore; the true value is 11.24.
  expect_equal(unname(got[["Explore"]]), 11.23741, tolerance = 1e-6)
})

test_that("bug 3: the matrix exponential is exact on a nilpotent matrix", {
  # A defective matrix: its eigenvector matrix is singular.
  nil <- matrix(0, 3L, 3L)
  nil[1L, 2L] <- 1
  nil[2L, 3L] <- 1
  expect_equal(cograph:::.cg_expm(nil), diag(3L) + nil + nil %*% nil / 2,
               tolerance = 1e-14, ignore_attr = TRUE)
  expect_equal(centrality_communicability(nil, directed = TRUE),
               c(`1` = 2.5, `2` = 2, `3` = 1), ignore_attr = TRUE)
})

test_that("bug 3: the matrix exponential matches a Taylor series on dense input", {
  saved <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  }
  on.exit(if (!is.null(saved)) assign(".Random.seed", saved, envir = globalenv()),
          add = TRUE)
  set.seed(2026)
  m <- matrix(stats::rnorm(36L, sd = 0.8), 6L, 6L)
  ref <- taylor_expm(m, terms = 80L)
  expect_equal(cograph:::.cg_expm(m), ref, tolerance = 1e-11, ignore_attr = TRUE)
  # Symmetric fast path agrees with the series as well.
  s <- m + t(m)
  expect_equal(cograph:::.cg_expm(s), taylor_expm(s, terms = 120L),
               tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("bug 3: undirected communicability is unchanged", {
  k3 <- matrix(1, 3L, 3L)
  diag(k3) <- 0
  # exp(K3) row sum = e^2 for every node.
  expect_equal(unname(centrality_communicability(k3)), rep(exp(2), 3L),
               tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("bug 4: communicability_betweenness works on directed regulation_net", {
  a <- bin_adj(regulation_net)
  n <- nrow(a)
  g <- taylor_expm(a)
  ref <- vapply(seq_len(n), function(r) {
    ar <- a
    ar[r, ] <- 0
    ar[, r] <- 0
    ratio <- (g - taylor_expm(ar)) / g
    diag(ratio) <- 0
    ratio[r, ] <- 0
    ratio[, r] <- 0
    sum(ratio)
  }, numeric(1L)) / ((n - 1) * (n - 2))
  got <- centrality_communicability_betweenness(regulation_net)
  expect_equal(unname(got), ref, tolerance = 1e-10, ignore_attr = TRUE)
  expect_true(all(got >= 0 & got <= 1))
})

test_that("bug 4: centrality(type = 'all') runs on regulation_net", {
  res <- suppressWarnings(centrality(regulation_net, type = "all"))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 10L)
  expect_true(all(is.finite(res$communicability_betweenness)))
})

test_that("bug 5: gateway matches brainGraph on undirected input", {
  skip_if_not_installed("brainGraph")
  skip_if_not_installed("igraph")
  s <- ((regulation_net + t(regulation_net)) != 0) * 1
  diag(s) <- 0
  memb <- rep(1:2, each = 5L)
  gu <- igraph::graph_from_adjacency_matrix(s, mode = "undirected")
  ref <- brainGraph::gateway_coeff(gu, memb, centr = "degree")
  got <- centrality_gateway(s, membership = memb, directed = FALSE)
  expect_equal(unname(got), ref, tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("bug 5: directed gateway with mode = 'all' uses in + out ties", {
  skip_if_not_installed("brainGraph")
  skip_if_not_installed("igraph")
  b <- bin_adj(regulation_net)
  memb <- rep(1:2, each = 5L)
  g <- igraph::graph_from_adjacency_matrix(b, mode = "directed")
  # brainGraph reads the symmetric matrix A it is given.
  ref <- brainGraph::gateway_coeff(g, memb, centr = "degree", A = b + t(b))
  got <- centrality_gateway(regulation_net, membership = memb, mode = "all")
  expect_equal(unname(got), ref, tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("bug 5: directed gateway lies in [0, 1] and honours mode", {
  memb <- rep(1:2, each = 5L)
  g_out <- centrality_gateway(regulation_net, membership = memb, mode = "out")
  g_in <- centrality_gateway(regulation_net, membership = memb, mode = "in")
  g_all <- centrality_gateway(regulation_net, membership = memb, mode = "all")
  lapply(list(g_out, g_in, g_all), \(v) expect_true(all(v >= 0 & v <= 1)))
  expect_false(isTRUE(all.equal(g_out, g_in)))
  # In-ties of a network are the out-ties of its transpose.
  expect_equal(unname(g_in),
               unname(centrality_gateway(t(regulation_net), membership = memb,
                                         mode = "out")),
               tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("bug 5: gateway accepts character and factor membership", {
  memb <- rep(1:2, each = 5L)
  ref <- centrality_gateway(regulation_net, membership = memb)
  expect_equal(centrality_gateway(regulation_net,
                                  membership = rep(c("a", "b"), each = 5L)),
               ref)
  expect_equal(centrality_gateway(regulation_net,
                                  membership = factor(rep(c("x", "y"), each = 5L))),
               ref)
  # Gapped integer labels name the same partition.
  expect_equal(centrality_gateway(regulation_net,
                                  membership = rep(c(2L, 7L), each = 5L)),
               ref)
})

test_that("bug 6: alpha honours mode and matches igraph", {
  skip_if_not_installed("igraph")
  g <- igraph::graph_from_adjacency_matrix(regulation_net, mode = "directed",
                                           weighted = TRUE, diag = FALSE)
  expect_equal(unname(centrality_alpha(regulation_net, mode = "in")),
               igraph::alpha_centrality(g, alpha = 1), tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(unname(centrality_alpha(regulation_net, mode = "out")),
               igraph::alpha_centrality(igraph::reverse_edges(g), alpha = 1),
               tolerance = 1e-10, ignore_attr = TRUE)
  gu <- igraph::as_undirected(g, mode = "collapse", edge.attr.comb = "sum")
  expect_equal(unname(centrality_alpha(regulation_net, mode = "all")),
               igraph::alpha_centrality(gu, alpha = 1), tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("bug 6: alpha formula by hand, (I - A^T)^-1 1 for mode = 'in'", {
  a <- regulation_net
  diag(a) <- 0
  n <- nrow(a)
  expect_equal(unname(centrality_alpha(regulation_net, mode = "in")),
               as.numeric(solve(diag(n) - t(a), rep(1, n))), tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(unname(centrality_alpha(regulation_net, mode = "out")),
               as.numeric(solve(diag(n) - a, rep(1, n))), tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("bug 6: power honours mode and matches igraph", {
  skip_if_not_installed("igraph")
  b <- bin_adj(regulation_net)
  gb <- igraph::graph_from_adjacency_matrix(b, mode = "directed")
  expect_equal(unname(centrality_power(regulation_net, mode = "out")),
               igraph::power_centrality(gb, exponent = 1), tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(unname(centrality_power(regulation_net, mode = "in")),
               igraph::power_centrality(igraph::reverse_edges(gb), exponent = 1),
               tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(unname(centrality_power(regulation_net, mode = "all")),
               igraph::power_centrality(igraph::as_undirected(gb), exponent = 1),
               tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("bug 6: mode has no effect on undirected alpha and power", {
  s <- regulation_net + t(regulation_net)
  modes <- c("all", "in", "out")
  al <- lapply(modes, \(md) centrality_alpha(s, mode = md, directed = FALSE))
  expect_equal(al[[1L]], al[[2L]])
  expect_equal(al[[1L]], al[[3L]])
  skeleton <- (s != 0) * 1
  pw <- lapply(modes, \(md) centrality_power(skeleton, mode = md,
                                             directed = FALSE))
  expect_equal(pw[[1L]], pw[[2L]])
  expect_equal(pw[[1L]], pw[[3L]])
})

test_that("bug 7: undirected expected influence counts each edge once", {
  s <- regulation_net + t(regulation_net)
  ei1 <- centrality(s, measures = "expected_influence_1", mode = "all",
                    directed = FALSE)$expected_influence_1_all
  expect_equal(ei1, unname(rowSums(s) - diag(s)), tolerance = 1e-12, ignore_attr = TRUE)
  ei2 <- centrality(s, measures = "expected_influence_2", mode = "all",
                    directed = FALSE)$expected_influence_2_all
  w <- s
  diag(w) <- 0
  e1 <- rowSums(w)
  expect_equal(ei2, unname(e1 + as.numeric(w %*% e1)), tolerance = 1e-12, ignore_attr = TRUE)
  # Signed network: the three modes agree on undirected input.
  signed <- matrix(c(0, 0.4, -0.3, 0.4, 0, 0.2, -0.3, 0.2, 0), 3L, 3L)
  modes <- c("all", "in", "out")
  vals <- lapply(modes, \(md) centrality_expected_influence_1(
    signed, mode = md, directed = FALSE))
  expect_equal(unname(vals[[1L]]), c(0.1, 0.6, -0.1), tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(vals[[1L]], vals[[2L]])
  expect_equal(vals[[1L]], vals[[3L]])
})

test_that("bug 7: directed expected influence with mode = 'all' still sums both", {
  ei <- centrality_expected_influence_1(regulation_net, mode = "all")
  expect_equal(unname(ei),
               unname(rowSums(regulation_net) + colSums(regulation_net) -
                        diag(regulation_net)),
               tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("bug 9: directed local transitivity equals igraph", {
  skip_if_not_installed("igraph")
  g <- igraph::graph_from_adjacency_matrix(regulation_net, mode = "directed",
                                           weighted = TRUE, diag = FALSE)
  ref <- igraph::transitivity(g, type = "local")
  expect_equal(unname(centrality_transitivity(regulation_net)), ref,
               tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(unname(centrality_transitivity(
    regulation_net, transitivity_type = "localundirected")), ref,
    tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("bug 9: a reciprocated dyad is one neighbor, by hand", {
  # 1 <-> 2 -> 3: node 1 has one distinct neighbor (NaN), node 2 has two
  # neighbors that are not linked (0), node 3 one neighbor (NaN).
  m <- matrix(0, 3L, 3L)
  m[1L, 2L] <- 1
  m[2L, 1L] <- 1
  m[2L, 3L] <- 1
  expect_equal(unname(centrality_transitivity(m, directed = TRUE)),
               c(NaN, 0, NaN))
})

test_that("bug 9: onnela is the Zhang-Horvath form tna reports", {
  w <- regulation_net + t(regulation_net)
  diag(w) <- 0
  num <- diag(w %*% w %*% w)
  den <- rowSums(w)^2 - rowSums(w^2)
  got <- centrality_transitivity(regulation_net, transitivity_type = "onnela")
  expect_equal(unname(got), unname(num / den), tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("bug 13: directed dmnc indexes the deduplicated neighbor set", {
  skip_if_not_installed("igraph")
  b <- bin_adj(regulation_net)
  n <- nrow(b)
  eps <- 1.7
  # Independent reference: igraph strong components on the induced
  # neighbor subgraph, edges counted among the largest components' nodes.
  ref <- vapply(seq_len(n), function(v) {
    nbs <- sort(unique(c(which(b[v, ] != 0), which(b[, v] != 0))))
    if (length(nbs) == 0L) return(0)
    sub <- b[nbs, nbs, drop = FALSE]
    comp <- igraph::components(
      igraph::graph_from_adjacency_matrix(sub, mode = "directed"),
      mode = "strong")
    big <- which(comp$csize == max(comp$csize))
    keep <- nbs[comp$membership %in% big]
    ec <- sum(b[keep, keep] != 0)
    if (ec == 0) 0 else ec / max(comp$csize)^eps
  }, numeric(1L))
  got <- centrality_dmnc(regulation_net, mode = "all")
  expect_equal(unname(got), ref, tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("bug 13: dmnc on a hand-built directed graph", {
  # 1 <-> 2, 1 <-> 3, 2 -> 3, 3 -> 2. Neighbors of 1 are {2, 3}, listed as
  # 2 2 3 3 under mode "all"; the induced subgraph 2 <-> 3 is one strong
  # component of size 2 with 2 directed edges: 2 / 2^1.7.
  m <- matrix(0, 3L, 3L)
  m[1L, 2L] <- m[2L, 1L] <- m[1L, 3L] <- m[3L, 1L] <- 1
  m[2L, 3L] <- m[3L, 2L] <- 1
  got <- centrality_dmnc(m, mode = "all", directed = TRUE)
  expect_equal(unname(got[1L]), 2 / 2^1.7, tolerance = 1e-12, ignore_attr = TRUE)
  # The deduplicated kernel agrees with the wired calculator.
  expect_equal(cograph:::.cg_dmnc(m, TRUE, "all", 1.7), unname(got),
               tolerance = 1e-12, ignore_attr = TRUE)
})
