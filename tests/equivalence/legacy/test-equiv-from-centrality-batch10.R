# Equivalence tests moved from tests/testthat/test-centrality-batch10.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
adj6 <- matrix(0, 6, 6)
adj6[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj6 <- adj6 + t(adj6)
rownames(adj6) <- colnames(adj6) <- LETTERS[1:6]
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1
star5 <- star5 + t(star5)
rownames(star5) <- colnames(star5) <- LETTERS[1:5]

# ---- equivalence tests ----

test_that("local efficiency matches brainGraph on a real network", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("brainGraph")
  skip_on_cran()
  g <- igraph::make_graph("Zachary")
  expect_equal(
    centrality(g, measures = "local_efficiency")$local_efficiency_all,
    unname(brainGraph::efficiency(g, type = "local", use.parallel = FALSE))
  )
})

test_that("fragmentation matches keyplayer::fragment", {
  skip_if_not_installed("keyplayer")
  skip_if_not_installed("sna")
  skip_on_cran()
  expect_equal(
    unname(centrality_fragmentation(adj6)),
    as.numeric(keyplayer::fragment(adj6, binary = TRUE, large = FALSE))
  )
})

test_that("the k-path census matches sna::kpath.census", {
  skip_if_not_installed("sna")
  skip_on_cran()
  ref <- sna::kpath.census(adj6, maxlen = 3, mode = "graph",
                           tabulate.by.vertex = TRUE)$path.count
  expect_equal(unname(centrality_kpath(adj6, kpath_len = 3)),
               unname(colSums(ref)[-1]))
})

test_that("EPC is centiserve's number divided by the run count", {
  skip_if_not_installed("centiserve")
  skip_if_not_installed("igraph")
  skip_on_cran()
  # Different random draws, so the two agree in scale rather than exactly.
  # centiserve is fixed at 1000 runs, whose standard error on a component
  # share is about 0.011; over 34 nodes the largest gap runs to a few of
  # those, so the bound is set at 0.06 rather than at the per-node error.
  g <- igraph::make_graph("Zachary")
  set.seed(4)
  ref <- as.numeric(centiserve::epc(g)) / 1000
  mine <- centrality(g, measures = "epc", epc_runs = 20000, epc_seed = 4)$epc
  expect_lt(max(abs(mine - ref)), 0.06)
  expect_gt(stats::cor(mine, ref, method = "kendall"), 0.9)
})

test_that("centiserve::closeness.latora is cograph's harmonic centrality", {
  skip_if_not_installed("centiserve")
  skip_if_not_installed("igraph")
  skip_on_cran()
  g <- igraph::make_graph("Zachary")
  expect_equal(centrality(g, measures = "harmonic")$harmonic_all,
               unname(centiserve::closeness.latora(g)))
})

test_that("brainGraph nodal efficiency is harmonic centrality over n - 1", {
  skip_if_not_installed("brainGraph")
  skip_if_not_installed("igraph")
  skip_on_cran()
  g <- igraph::make_graph("Zachary")
  expect_equal(
    centrality(g, measures = "harmonic")$harmonic_all / (igraph::vcount(g) - 1),
    unname(brainGraph::efficiency(g, type = "nodal", use.parallel = FALSE))
  )
})

test_that("centiserve::communibet is cograph's communicability betweenness", {
  skip_if_not_installed("Matrix")
  skip_if_not_installed("igraph")
  skip_on_cran()
  # centiserve::communibet transcribed with Matrix::expm, which uses scaling
  # and squaring rather than the eigendecomposition cograph uses, so the two
  # routes to exp(A) are independent.
  g <- igraph::make_graph("Zachary")
  n <- igraph::vcount(g)
  adj <- as.matrix(igraph::as_adjacency_matrix(g, names = FALSE))
  ex <- function(m) as.matrix(Matrix::expm(Matrix::Matrix(m)))
  exp_adj <- ex(adj)
  ref <- vapply(seq_len(n), function(v) {
    reduced <- adj
    reduced[v, ] <- 0
    reduced[, v] <- 0
    b <- (exp_adj - ex(reduced)) / exp_adj
    b[v, ] <- 0
    b[, v] <- 0
    diag(b) <- 0
    sum(b)
  }, numeric(1)) / ((n - 1)^2 - (n - 1))
  expect_equal(
    centrality(g, measures = "communicability_betweenness")$communicability_betweenness,
    ref, tolerance = 1e-8)
})
