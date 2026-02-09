test_that("parse_matrix works with symmetric matrix", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  result <- parse_matrix(adj)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 3)  # Upper triangle only
expect_false(result$directed)
})

test_that("parse_matrix works with asymmetric matrix", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3)
  result <- parse_matrix(adj)

  expect_equal(nrow(result$nodes), 3)
  expect_true(result$directed)
})

test_that("parse_matrix handles weighted matrix", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3)
  result <- parse_matrix(adj)

  expect_true(all(result$edges$weight != 1))
})

test_that("parse_matrix errors on non-square matrix", {
  adj <- matrix(1:6, nrow = 2)
  expect_error(parse_matrix(adj), "square")
})

test_that("parse_matrix preserves node labels from dimnames", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(adj) <- c("A", "B", "C")
  result <- parse_matrix(adj)

  expect_equal(result$nodes$label, c("A", "B", "C"))
})

test_that("parse_edgelist works with basic edge list", {
  df <- data.frame(from = c(1, 1, 2), to = c(2, 3, 3))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 3)
})

test_that("parse_edgelist handles character node names", {
  df <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$nodes), 3)
  expect_true(all(c("A", "B", "C") %in% result$nodes$label))
})

test_that("parse_edgelist handles weighted edges", {
  df <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 1.5))
  result <- parse_edgelist(df)

  expect_equal(result$weights, c(0.5, 1.5))
})

test_that("parse_edgelist errors on empty data frame", {
  df <- data.frame(from = integer(0), to = integer(0))
  expect_error(parse_edgelist(df), "empty")
})
