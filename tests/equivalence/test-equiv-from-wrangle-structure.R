# Equivalence tests moved from tests/testthat/test-wrangle-structure.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
ws_dir <- function() {
  m <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  m[1, 2] <- 0.5
  m[2, 1] <- 0.2
  m[2, 3] <- 0.7
  m
}
ws_und <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- m[2, 1] <- 0.5
  m[1, 3] <- m[3, 1] <- 0.8
  m[2, 4] <- m[4, 2] <- 0.6
  m[3, 4] <- m[4, 3] <- 0.4
  m
}
ws_split <- function() {
  m <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
  m[1, 2] <- m[2, 1] <- 1
  m[2, 3] <- m[3, 2] <- 1
  m[4, 5] <- m[5, 4] <- 1
  m
}
ws_core <- function() {
  m <- matrix(c(0, 1, 1, 1,
                1, 0, 1, 0,
                1, 1, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
  dimnames(m) <- list(LETTERS[1:4], LETTERS[1:4])
  m
}

# ---- equivalence tests ----

test_that("to_undirected agrees with igraph::as_undirected on the collapse rules", {
  skip_if_not_installed("igraph")
  d <- ws_dir()
  g <- igraph::graph_from_adjacency_matrix(d, mode = "directed", weighted = TRUE)

  ig_sum <- igraph::as_undirected(g, mode = "collapse",
                                  edge.attr.comb = list(weight = "sum"))
  ig_mat <- as.matrix(igraph::as_adjacency_matrix(ig_sum, attr = "weight"))

  expect_equal(unname(to_matrix(to_undirected(d, "sum"))), unname(ig_mat))
})

test_that("split_components agrees with igraph on the component count", {
  skip_if_not_installed("igraph")
  m <- ws_split()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", weighted = TRUE)

  expect_equal(length(split_components(m)), igraph::components(g)$no)
})

test_that("select_k_core agrees with igraph coreness", {
  skip_if_not_installed("igraph")
  m <- ws_core()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")
  expected <- names(igraph::coreness(g))[igraph::coreness(g) >= 2]

  expect_equal(get_labels(select_k_core(m, k = 2)), expected)
})

test_that("spanning_tree agrees with igraph::mst on total weight", {
  skip_if_not_installed("igraph")
  m <- ws_und()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", weighted = TRUE)
  ig_total <- sum(igraph::E(igraph::mst(g))$weight)

  expect_equal(sum(as.data.frame(spanning_tree(m))$weight), ig_total)
})

test_that("complement_network agrees with igraph::complementer", {
  skip_if_not_installed("igraph")
  m <- (ws_und() != 0) * 1
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")
  ig <- as.matrix(igraph::as_adjacency_matrix(igraph::complementer(g)))

  expect_equal(unname(to_matrix(complement_network(m))), unname(ig))
})
