test_that("dyad_census returns tidy three-row data.frame", {
  skip_if_no_igraph()
  adj <- matrix(c(0, 1, 1, 0,
                  1, 0, 0, 1,
                  0, 0, 0, 1,
                  0, 0, 0, 0), 4, 4, byrow = TRUE)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  dc <- dyad_census(adj)

  expect_s3_class(dc, "cograph_dyad_census")
  expect_s3_class(dc, "data.frame")
  expect_identical(dc$type, c("mutual", "asymmetric", "null"))
  expect_identical(names(dc), c("type", "count", "proportion"))
  # 1 mutual (A<->B), 3 asymmetric (A->C, B->D, C->D), 2 null
  expect_identical(dc$count, c(1L, 3L, 2L))
})

test_that("dyad_census counts sum to choose(n, 2)", {
  skip_if_no_igraph()
  m <- create_test_matrix(n = 7, density = 0.4, symmetric = FALSE)
  dc <- dyad_census(m, directed = TRUE)
  expect_equal(sum(dc$count), choose(7, 2))
  expect_equal(sum(dc$proportion), 1)
})

test_that("dyad_census reciprocity attribute is 2M / (2M + A)", {
  skip_if_no_igraph()
  adj <- matrix(c(0, 1, 1, 0,
                  1, 0, 0, 1,
                  0, 0, 0, 1,
                  0, 0, 0, 0), 4, 4, byrow = TRUE)
  dc <- dyad_census(adj)
  expect_equal(attr(dc, "reciprocity"), 2 * 1 / (2 * 1 + 3))
})

test_that("dyad_census on undirected has zero asymmetric dyads", {
  skip_if_no_igraph()
  und <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 0,
                  1, 1, 0, 1,
                  0, 0, 1, 0), 4, 4, byrow = TRUE)
  dc <- dyad_census(und, directed = FALSE)
  expect_equal(dc$count[dc$type == "asymmetric"], 0L)
  expect_false(attr(dc, "directed"))
  # 4 undirected edges -> 4 mutual, choose(4,2)-4 = 2 null
  expect_equal(dc$count[dc$type == "mutual"], 4L)
})

test_that("dyad_census print method returns invisibly", {
  skip_if_no_igraph()
  adj <- matrix(c(0, 1, 0, 0,
                  1, 0, 0, 0,
                  0, 0, 0, 0,
                  0, 0, 0, 0), 4, 4, byrow = TRUE)
  dc <- dyad_census(adj)
  expect_output(print(dc), "Dyad Census")
  expect_invisible(print(dc))
})
