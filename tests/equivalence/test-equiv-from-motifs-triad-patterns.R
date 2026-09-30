# Equivalence tests moved from tests/testthat/test-motifs-triad-patterns.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
.census_names <- c("003", "012", "102", "021D", "021U", "021C",
                   "111D", "111U", "030T", "030C", "201",
                   "120D", "120U", "120C", "210", "300")
.classify_triad <- function(m) {
  g <- igraph::graph_from_adjacency_matrix(m, mode = "directed")
  census <- igraph::triad_census(g)
  .census_names[which(census == 1L)]
}

# ---- equivalence tests ----

test_that("the 64-entry classifier lookup matches igraph exhaustively", {
  edge_positions <- matrix(c(1L, 2L, 2L, 1L, 1L, 3L,
                             3L, 1L, 2L, 3L, 3L, 2L),
                           ncol = 2, byrow = TRUE)
  lookup <- cograph:::.get_triad_lookup()

  reference <- vapply(0:63, function(code) {
    m <- matrix(0L, 3, 3)
    bits <- as.integer(intToBits(code))[1:6]
    m[edge_positions] <- bits
    .classify_triad(m)
  }, character(1))

  expect_identical(unname(lookup), unname(reference))
})
