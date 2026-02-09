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

test_that("cograph() applies default layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  layout <- net$layout
  expect_false(is.null(layout))
  expect_equal(nrow(layout), 3)
})

test_that("sn_layout() changes layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net1 <- cograph(adj, layout = "spring", seed = 42)
  net2 <- net1 |> sn_layout("circle")

  coords1 <- net1$layout
  coords2 <- net2$layout

  # Layouts should be different
  expect_false(all(coords1$x == coords2$x))
})

test_that("sn_nodes() modifies node aesthetics", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |>
    sn_nodes(size = 0.1, fill = "red")

  aes <- net$node_aes
  expect_true(all(aes$size == 0.1))
  expect_true(all(aes$fill == "red"))
})

test_that("sn_edges() modifies edge aesthetics", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |>
    sn_edges(width = 2, color = "blue")

  aes <- net$edge_aes
  expect_true(all(aes$width == 2))
  expect_true(all(aes$color == "blue"))
})

test_that("sn_theme() applies theme", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |> sn_theme("dark")

  theme <- net$theme
  expect_equal(theme$name, "dark")
  expect_equal(theme$get("background"), "#1a1a2e")
})

test_that("pipe chain works correctly", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- adj |>
    cograph(layout = "circle") |>
    sn_nodes(size = 0.08, fill = "steelblue") |>
    sn_edges(width = 1.5) |>
    sn_theme("minimal")

  expect_s3_class(net, "cograph_network")
  expect_true(all(net$node_aes$fill == "steelblue"))
  expect_equal(net$theme$name, "minimal")
})

test_that("print method works", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_output(print(net), "Cograph network")
  expect_output(print(net), "3 nodes")
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

  # Same names
  expect_equal(sort(names(net1)), sort(names(net2)))

  # Same core values
  expect_equal(n_nodes(net1), n_nodes(net2))
  expect_equal(n_edges(net1), n_edges(net2))
  expect_equal(is_directed(net1), is_directed(net2))

  # Both have node_aes and edge_aes
  expect_false(is.null(net1$node_aes))
  expect_false(is.null(net2$node_aes))
  expect_false(is.null(net1$edge_aes))
  expect_false(is.null(net2$edge_aes))
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
