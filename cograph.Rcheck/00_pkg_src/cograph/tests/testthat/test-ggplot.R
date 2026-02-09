test_that("sn_ggplot() returns ggplot object", {
  skip_if_not_installed("ggplot2")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  p <- sn_ggplot(net)

  expect_s3_class(p, "ggplot")
})

test_that("sn_ggplot() includes title", {
  skip_if_not_installed("ggplot2")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  p <- sn_ggplot(net, title = "Test Network")

  expect_true("title" %in% names(p$labels))
})

test_that("sn_ggplot() works with custom aesthetics", {
  skip_if_not_installed("ggplot2")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |>
    sn_nodes(fill = "red", size = 0.1) |>
    sn_edges(color = "blue")

  p <- sn_ggplot(net)
  expect_s3_class(p, "ggplot")
})

test_that("sn_ggplot() handles directed networks", {
  skip_if_not_installed("ggplot2")

  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- cograph(adj, directed = TRUE)
  p <- sn_ggplot(net)

  expect_s3_class(p, "ggplot")
})

test_that("sn_ggplot() can be further customized", {
  skip_if_not_installed("ggplot2")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  p <- sn_ggplot(net) +
    ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20))

  expect_s3_class(p, "ggplot")
})
