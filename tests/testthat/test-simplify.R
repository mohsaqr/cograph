# Tests for simplify()

test_that("simplify.matrix removes self-loops", {
  mat <- matrix(c(0.5, 0.3, 0, 0.3, 0.2, 0.4, 0, 0.4, 0.1), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- simplify(mat)
  expect_equal(unname(diag(result)), c(0, 0, 0))
  expect_equal(result["A", "B"], 0.3)
  expect_equal(result["B", "C"], 0.4)
})

test_that("simplify.matrix with remove_loops = FALSE keeps diagonal", {
  mat <- matrix(c(0.5, 0.3, 0.3, 0.2), 2, 2)
  result <- simplify(mat, remove_loops = FALSE)
  expect_equal(unname(diag(result)), c(0.5, 0.2))
})

test_that("simplify.cograph_network removes self-loop edges", {
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 1, 2), weight = c(0.5, 0.3, 0.4))
  net <- cograph(edges, layout = NULL)
  result <- simplify(net)
  result_edges <- get_edges(result)
  expect_true(all(result_edges$from != result_edges$to))
})

test_that("simplify.cograph_network merges duplicate edges with mean", {
  edges <- data.frame(
    from = c(1, 1, 2, 1), to = c(2, 3, 3, 2),
    weight = c(0.5, 0.3, 0.4, 0.7)
  )
  net <- cograph(edges, layout = NULL)
  result <- simplify(net, edge_attr_comb = "mean")
  result_edges <- get_edges(result)
  expect_equal(nrow(result_edges), 3)
  e12 <- result_edges[result_edges$from == 1 & result_edges$to == 2, "weight"]
  expect_equal(e12, 0.6)
})

test_that("simplify.cograph_network merges with sum", {
  edges <- data.frame(from = c(1, 1), to = c(2, 2), weight = c(0.3, 0.7))
  net <- cograph(edges, layout = NULL)
  result <- simplify(net, edge_attr_comb = "sum")
  expect_equal(get_edges(result)$weight, 1.0)
})

test_that("simplify.cograph_network merges with max", {
  edges <- data.frame(from = c(1, 1), to = c(2, 2), weight = c(0.3, 0.7))
  net <- cograph(edges, layout = NULL)
  result <- simplify(net, edge_attr_comb = "max")
  expect_equal(get_edges(result)$weight, 0.7)
})

test_that("simplify.cograph_network merges with min", {
  edges <- data.frame(from = c(1, 1), to = c(2, 2), weight = c(0.3, 0.7))
  net <- cograph(edges, layout = NULL)
  result <- simplify(net, edge_attr_comb = "min")
  expect_equal(get_edges(result)$weight, 0.3)
})

test_that("simplify.cograph_network merges with first", {
  edges <- data.frame(from = c(1, 1), to = c(2, 2), weight = c(0.3, 0.7))
  net <- cograph(edges, layout = NULL)
  result <- simplify(net, edge_attr_comb = "first")
  expect_equal(get_edges(result)$weight, 0.3)
})

test_that("simplify.cograph_network with no duplicates is no-op", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.3))
  net <- cograph(edges, layout = NULL)
  result <- simplify(net)
  expect_equal(nrow(get_edges(result)), 2)
})

test_that("simplify.cograph_network cleans weight matrix diagonal", {
  mat <- matrix(c(0.5, 0.3, 0.3, 0.2), 2, 2,
    dimnames = list(c("A", "B"), c("A", "B")))
  net <- cograph(mat, layout = NULL)
  result <- simplify(net)
  expect_equal(unname(diag(result$weights)), c(0, 0))
})

test_that("simplify.cograph_network with remove_multiple = FALSE keeps duplicates", {
  edges <- data.frame(from = c(1, 1), to = c(2, 2), weight = c(0.3, 0.7))
  net <- cograph(edges, layout = NULL)
  result <- simplify(net, remove_multiple = FALSE)
  expect_equal(nrow(get_edges(result)), 2)
})

test_that("simplify works with igraph objects", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3), directed = FALSE)
  result <- simplify(g)
  expect_equal(igraph::ecount(result), 2)
})

test_that("simplify.tna removes self-loops from weights", {
  skip_if_not_installed("tna")
  model <- tna::tna(tna::engagement)
  has_diag <- any(diag(model$weights) != 0)
  result <- simplify(model)
  expect_true(all(diag(result$weights) == 0))
})

test_that("simplify errors on unsupported types", {
  expect_error(simplify("not a network"), "Cannot simplify")
})

test_that("simplify with custom function", {
  edges <- data.frame(from = c(1, 1), to = c(2, 2), weight = c(0.3, 0.7))
  net <- cograph(edges, layout = NULL)
  result <- simplify(net, edge_attr_comb = function(x) prod(x))
  expect_equal(get_edges(result)$weight, 0.21, tolerance = 1e-10)
})
