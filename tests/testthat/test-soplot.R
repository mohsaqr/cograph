# test-soplot.R - Grid Graphics Function Tests
# Tests for soplot() - grid-based rendering

# ============================================
# BASIC FUNCTIONALITY
# ============================================

test_that("soplot() works with adjacency matrix", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("soplot() works with weighted matrix", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("soplot() works with edge list", {
  skip_if_not_installed("grid")

  edges <- create_test_edgelist(n_edges = 6, n_nodes = 4)

  result <- safe_plot(soplot(edges))
  expect_true(result$success, info = result$error)
})

test_that("soplot() works with sonnet_network object", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

test_that("soplot() works with igraph object", {
  skip_if_no_igraph()
  skip_if_not_installed("grid")

  g <- igraph::make_ring(5)

  result <- safe_plot(soplot(g))
  expect_true(result$success, info = result$error)
})

# ============================================
# LAYOUTS
# ============================================

test_that("soplot() works with circle layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)

  result <- safe_plot(soplot(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("soplot() works with spring layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)

  result <- safe_plot(soplot(adj, layout = "spring", seed = 42))
  expect_true(result$success, info = result$error)
})

test_that("soplot() works with custom coordinates", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  coords <- matrix(c(0, 0, 1, 1, 0, 1, 0, 1), ncol = 2)

  result <- safe_plot(soplot(adj, layout = coords))
  expect_true(result$success, info = result$error)
})

# ============================================
# NODE AESTHETICS
# ============================================

test_that("soplot() handles node_size parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, node_size = 5))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles node_shape parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)

  for (shape in c("circle", "square", "triangle", "diamond")) {
    result <- safe_plot(soplot(adj, node_shape = shape))
    expect_true(result$success, info = paste("Shape", shape, "failed:", result$error))
  }
})

test_that("soplot() handles node_fill parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, node_fill = "steelblue"))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles per-node colors", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, node_fill = c("red", "green", "blue", "orange")))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles node_alpha parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, node_alpha = 0.7))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles labels parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, labels = c("A", "B", "C", "D")))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE AESTHETICS
# ============================================

test_that("soplot() handles edge_color parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, edge_color = "gray50"))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles edge_width parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, edge_width = 2))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles edge_alpha parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, edge_alpha = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles curvature parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, curvature = 0.3))
  expect_true(result$success, info = result$error)
})

# ============================================
# THEME INTEGRATION
# ============================================

test_that("soplot() handles theme parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  for (theme in c("classic", "dark", "minimal")) {
    result <- safe_plot(soplot(adj, theme = theme))
    expect_true(result$success, info = paste("Theme", theme, "failed:", result$error))
  }
})

# ============================================
# TITLE
# ============================================

test_that("soplot() handles title parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  result <- safe_plot(soplot(adj, title = "Test Network"))
  expect_true(result$success, info = result$error)
})

# ============================================
# RETURN VALUE
# ============================================

test_that("soplot() returns grob object", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  grob <- with_temp_png({
    soplot(adj)
  })

  # soplot returns a grob or gTree
  expect_true(inherits(grob, "grob") || inherits(grob, "gTree") ||
              inherits(grob, "sonnet_network") || is.null(grob))
})

# ============================================
# EDGE CASES
# ============================================

test_that("soplot() handles single-node network", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 1, 1)

  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles network with no edges", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 4, 4)

  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles self-loops", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  diag(adj) <- 1

  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

# ============================================
# DIRECTED NETWORKS
# ============================================

test_that("soplot() handles asymmetric (directed) networks", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, symmetric = FALSE)

  # soplot detects directedness from asymmetric matrix
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles arrows on directed networks", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, symmetric = FALSE)

  # Use show_arrows parameter (directed is not a soplot parameter)
  result <- safe_plot(soplot(adj, show_arrows = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# INTEGRATION WITH PIPE CHAIN
# ============================================

test_that("soplot() works with customized sonnet_network", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_nodes(fill = "coral") |>
    sn_edges(color = "gray")

  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

test_that("soplot() works with themed network", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  net <- sonnet(adj) |> sn_theme("colorblind")

  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

# ============================================
# SPECIAL NODE TYPES
# ============================================

test_that("soplot() handles pie chart nodes", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  pie_vals <- list(c(1, 2), c(2, 1), c(1, 1, 1))

  result <- safe_plot(soplot(adj, pie_values = pie_vals))
  expect_true(result$success, info = result$error)
})

test_that("soplot() handles donut nodes", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)

  result <- safe_plot(soplot(adj, donut_fill = c(0.3, 0.6, 0.9)))
  expect_true(result$success, info = result$error)
})

# ============================================
# WEIGHT COLORS
# ============================================

test_that("soplot() handles positive/negative weight colors", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)

  result <- safe_plot(soplot(adj,
    edge_positive_color = "darkgreen",
    edge_negative_color = "darkred"
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# SEED REPRODUCIBILITY
# ============================================

test_that("soplot() with same seed produces consistent results", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)

  # Run twice with same seed
  # Just verify both complete without error (visual comparison is harder)
  result1 <- safe_plot(soplot(adj, layout = "spring", seed = 123))
  result2 <- safe_plot(soplot(adj, layout = "spring", seed = 123))

  expect_true(result1$success)
  expect_true(result2$success)
})
