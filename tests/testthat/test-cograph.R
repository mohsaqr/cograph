test_that("cograph() creates network from matrix", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 3)
  expect_equal(n_edges(net), 3)
})

test_that("cograph() creates network from edge list", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  net <- cograph(edges)

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 3)
})

test_that("cograph() without layout argument has NA coordinates", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Layout coordinates exist but are NA when not explicitly requested
  nodes <- get_nodes(net)
  expect_true("x" %in% names(nodes))
  expect_true("y" %in% names(nodes))
  # No layout is computed unless explicitly requested
  expect_true(all(is.na(nodes$x)))
})

test_that("cograph() applies layout when requested", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj, layout = "circle")

  # Layout coordinates should be in nodes when layout is specified
  nodes <- get_nodes(net)
  expect_true("x" %in% names(nodes))
  expect_true("y" %in% names(nodes))
  expect_false(all(is.na(nodes$x)))
})

test_that("sn_layout() changes layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net1 <- cograph(adj, layout = "spring", seed = 42)
  net2 <- net1 |> sn_layout("circle")

  nodes1 <- get_nodes(net1)
  nodes2 <- get_nodes(net2)

  # Layouts should be different
  expect_false(all(nodes1$x == nodes2$x))
})

test_that("sn_nodes() returns network (styling handled by splot)", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |>
    sn_nodes(size = 0.1, fill = "red")

  # sn_nodes returns the network unchanged (styling is via splot params)
  expect_s3_class(net, "cograph_network")
})

test_that("sn_edges() returns network (styling handled by splot)", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |>
    sn_edges(width = 2, color = "blue")

  # sn_edges returns the network unchanged (styling is via splot params)
  expect_s3_class(net, "cograph_network")
})

test_that("sn_theme() returns network (styling handled by splot)", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |> sn_theme("dark")

  # sn_theme returns the network unchanged (theming is via splot params)
  expect_s3_class(net, "cograph_network")
})

test_that("pipe chain works correctly", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- adj |>
    cograph(layout = "circle") |>
    sn_nodes(size = 0.08, fill = "steelblue") |>
    sn_edges(width = 1.5) |>
    sn_theme("minimal")

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 3)
})

test_that("print method works", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_output(print(net), "Cograph network")
})

test_that("summary method works", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_output(summary(net), "Cograph Network Summary")
  result <- summary(net)
  expect_equal(result$n_nodes, 3)
})

test_that("as_cograph() and cograph() produce same structure", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net1 <- cograph(adj)
  net2 <- as_cograph(adj)

  # Same core values
  expect_equal(n_nodes(net1), n_nodes(net2))
  expect_equal(n_edges(net1), n_edges(net2))
  expect_equal(is_directed(net1), is_directed(net2))

  # Both have core data
  expect_true(!is.null(net1$nodes))
  expect_true(!is.null(net2$nodes))
  expect_true(!is.null(net1$edges))
  expect_true(!is.null(net2$edges))
})

test_that("weights matrix preserved", {
  adj <- matrix(c(0, 0.5, 0.3, 0.2, 0, 0.4, 0.1, 0.6, 0), nrow = 3)
  net1 <- cograph(adj)
  net2 <- as_cograph(adj)

  expect_true(!is.null(net1$weights) && is.matrix(net1$weights))
  expect_true(!is.null(net2$weights) && is.matrix(net2$weights))
  expect_equal(net1$weights, adj)
  expect_equal(net2$weights, adj)
})
