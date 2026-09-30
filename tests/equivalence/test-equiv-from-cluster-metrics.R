# Equivalence tests moved from tests/testthat/test-cluster-metrics.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
skip_on_cran()
set.seed(42)
n <- 10
mat <- matrix(runif(n * n), n, n)
diag(mat) <- 0  # No self-loops
rownames(mat) <- colnames(mat) <- paste0("N", 1:n)
clusters_list <- list(
  "A" = c("N1", "N2", "N3"),
  "B" = c("N4", "N5", "N6"),
  "C" = c("N7", "N8", "N9", "N10")
)
clusters_vec <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
names(clusters_vec) <- paste0("N", 1:n)
cat("\n=== All Cluster Metrics Tests Passed ===\n")

# ---- equivalence tests ----

test_that("cluster_summary matches igraph", {
  skip_if_not_installed("igraph")

  # verify_with_igraph defaults to type = "raw" for igraph comparison
  result <- verify_with_igraph(mat, clusters_list, method = "sum")

  expect_true(result$matches,
              info = paste("Difference:", result$difference))
})
