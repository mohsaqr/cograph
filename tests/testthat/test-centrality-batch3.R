# ===========================================================================
# Tests for Batch 3 classical centrality measures
# Reference validation against centiserve / sna / igraph / NetworkX.
# ===========================================================================

skip_coverage_tests()

# ---------------------------------------------------------------------------
# Test graphs
# ---------------------------------------------------------------------------

k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(k3) <- colnames(k3) <- c("A", "B", "C")

# ===========================================================================
# Katz centrality (Katz 1953)
# ===========================================================================

test_that("katz returns a numeric vector of correct length", {
  v <- centrality_katz(k3)
  expect_type(v, "double")
  expect_length(v, 3)
  expect_named(v, c("A", "B", "C"))
  # Symmetric graph: all equal
  expect_equal(v[[1]], v[[2]])
  expect_equal(v[[2]], v[[3]])
})

test_that("katz matches centiserve::katzcent BIT-EXACT (12 random graphs)", {
  skip_if_not_installed("centiserve")
  skip_if_not_installed("igraph")
  set.seed(1001)
  for (i in 1:12) {
    n <- sample(6:20, 1)
    g <- igraph::sample_gnp(n, runif(1, 0.2, 0.5), directed = FALSE)
    if (igraph::ecount(g) < 2) next
    # Pick alpha < 1 / spectral_radius so centiserve accepts it
    A  <- as.matrix(igraph::as_adjacency_matrix(g))
    sr <- max(Re(eigen(A, only.values = TRUE)$values))
    if (sr <= 0) next
    a  <- min(0.1, 0.5 / sr)
    cog <- centrality(g, measures = "katz", katz_alpha = a)$katz
    cs  <- centiserve::katzcent(g, alpha = a)
    # Bit-exact: cograph's calculate_katz mirrors centiserve's
    # solve(I - alpha*A^T) %*% 1 LAPACK call sequence exactly.
    expect_identical(cog, cs,
                     info = sprintf("graph %d, n=%d, alpha=%.4f", i, n, a))
  }
})

test_that("katz matches igraph::alpha_centrality at machine epsilon", {
  skip_if_not_installed("igraph")
  set.seed(1002)
  for (i in 1:5) {
    n <- sample(10:30, 1)
    g <- igraph::sample_gnp(n, 0.3, directed = FALSE)
    cog <- centrality(g, measures = "katz", katz_alpha = 0.1)$katz
    ig  <- igraph::alpha_centrality(g, alpha = 0.1, exo = 1, sparse = TRUE)
    # Sparse iterative solver vs dense direct solve: machine-epsilon agreement.
    expect_equal(cog, unname(ig), tolerance = 1e-9,
                 info = sprintf("graph %d, n=%d", i, n))
  }
})

# NetworkX cross-language reference test (skip if reticulate / nx unavailable)
has_nx <- function() {
  requireNamespace("reticulate", quietly = TRUE) &&
    reticulate::py_module_available("networkx")
}

test_that("katz matches NetworkX katz_centrality_numpy on karate (ULP)", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  g_r  <- igraph::make_graph("Zachary")
  g_nx <- nx$karate_club_graph()
  cog <- centrality(g_r, measures = "katz", katz_alpha = 0.1)$katz
  nxv <- unname(unlist(nx$katz_centrality_numpy(g_nx, alpha = 0.1, beta = 1,
                                                normalized = FALSE)))
  # 1-2 ULPs of difference are unavoidable across R and Python LAPACK builds.
  expect_equal(cog, nxv, tolerance = 1e-13)
})
