# Equivalence tests moved from tests/testthat/test-wrangle-vocabulary.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
wv_star <- function() {
  m <- matrix(c(0, 1, 1, 1,
                1, 0, 1, 0,
                1, 1, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
  dimnames(m) <- list(LETTERS[1:4], LETTERS[1:4])
  m
}
wv_dir <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- 1   # A -> B
  m[2, 3] <- 1   # B -> C
  m[3, 2] <- 1   # C -> B, so B <-> C is mutual
  m
}
wv_iso <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- m[2, 1] <- 1
  m[2, 3] <- m[3, 2] <- 1
  m
}

# ---- equivalence tests ----

test_that("is_cut agrees with igraph::articulation_points", {
  skip_if_not_installed("igraph")
  m <- wv_star()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")
  expected <- sort(names(igraph::articulation_points(g)))

  expect_equal(sort(get_labels(select_nodes(m, is_cut))), expected)
})

test_that("local_transitivity and local_triangles match igraph", {
  skip_if_not_installed("igraph")
  m <- wv_star()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")

  nodes <- as.data.frame(
    mutate_nodes(m, lt = local_transitivity, tri = local_triangles),
    what = "nodes"
  )

  expect_equal(nodes$lt, unname(igraph::transitivity(g, type = "local")))
  expect_equal(nodes$tri, unname(as.numeric(igraph::count_triangles(g))))
})
