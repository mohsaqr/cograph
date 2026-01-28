# test-integration.R - Integration Tests
# Multi-step workflow and end-to-end tests

# ============================================
# FULL PIPE CHAIN WORKFLOWS
# ============================================

test_that("complete pipe chain works: sonnet |> sn_nodes |> sn_edges |> sn_theme", {
  adj <- create_test_matrix(5)

  net <- adj |>
    sonnet(layout = "circle") |>
    sn_nodes(size = 5, fill = "steelblue") |>
    sn_edges(width = 2, color = "gray50") |>
    sn_theme("minimal")

  expect_sonnet_network(net)

  # Verify customizations applied
  node_aes <- net$network$get_node_aes()
  expect_true(all(node_aes$size == 5))
  expect_true(all(node_aes$fill == "steelblue"))

  edge_aes <- net$network$get_edge_aes()
  expect_true(all(edge_aes$width == 2))

  theme <- net$network$get_theme()
  expect_equal(theme$name, "minimal")
})

test_that("pipe chain renders correctly with splot()", {
  adj <- create_test_matrix(5)

  net <- adj |>
    sonnet(layout = "spring", seed = 42) |>
    sn_nodes(fill = palette_colorblind(5), shape = "circle") |>
    sn_edges(alpha = 0.7) |>
    sn_theme("classic")

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("pipe chain with sn_layout change works", {
  adj <- create_test_matrix(5)

  net <- adj |>
    sonnet(layout = "spring", seed = 42) |>
    sn_layout("circle") |>
    sn_nodes(fill = "coral") |>
    sn_theme("dark")

  expect_sonnet_network(net)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("pipe chain with sn_palette works", {
  adj <- create_test_matrix(5)

  net <- adj |>
    sonnet(layout = "circle") |>
    sn_palette("viridis", target = "nodes") |>
    sn_edges(width = 1.5)

  expect_sonnet_network(net)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

# ============================================
# CONVERTER -> CUSTOMIZE -> SAVE WORKFLOW
# ============================================

test_that("qgraph conversion workflow works", {
  skip_if_no_qgraph()

  adj <- matrix(c(0, 0.5, -0.3, 0.5, 0, 0.4, -0.3, 0.4, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Convert to Sonnet parameters
  params <- from_qgraph(q, plot = FALSE)

  expect_true(is.list(params))
  expect_true("x" %in% names(params))

  # Create network and customize
  net <- do.call(sonnet, list(input = params$x)) |>
    sn_nodes(fill = "steelblue") |>
    sn_theme("minimal")

  expect_sonnet_network(net)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("tna conversion workflow works", {
  skip_if_no_tna()

  # Create a transition matrix for tna
  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  # Convert and customize
  params <- from_tna(tna_obj, plot = FALSE)

  net <- do.call(sonnet, list(input = params$x, directed = TRUE)) |>
    sn_nodes(fill = palette_colorblind(3)) |>
    sn_theme("colorblind")

  expect_sonnet_network(net)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

# ============================================
# THEME + PALETTE COMBINATIONS
# ============================================

test_that("dark theme with viridis palette works", {
  adj <- create_test_matrix(5)

  net <- sonnet(adj) |>
    sn_palette("viridis") |>
    sn_theme("dark")

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("colorblind theme with colorblind palette works", {
  adj <- create_test_matrix(5)

  net <- sonnet(adj) |>
    sn_nodes(fill = palette_colorblind(5)) |>
    sn_theme("colorblind")

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("minimal theme with pastel palette works", {
  adj <- create_test_matrix(5)

  net <- sonnet(adj) |>
    sn_nodes(fill = palette_pastel(5)) |>
    sn_theme("minimal")

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

# ============================================
# LAYOUT + SHAPE COMBINATIONS
# ============================================

test_that("circle layout with various shapes works", {
  adj <- create_test_matrix(6)
  shapes <- c("circle", "square", "triangle", "diamond", "pentagon", "hexagon")

  net <- sonnet(adj, layout = "circle") |>
    sn_nodes(shape = shapes)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("spring layout with pie nodes works", {
  adj <- create_test_matrix(4)
  pie_vals <- list(c(1, 2), c(2, 1), c(1, 1, 1), c(3, 1))

  net <- sonnet(adj, layout = "spring", seed = 42) |>
    sn_nodes(pie_values = pie_vals)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("groups layout with colored groups works", {
  adj <- create_test_matrix(9)
  groups <- rep(1:3, each = 3)
  colors <- rep(palette_colorblind(3), each = 3)

  net <- sonnet(adj, layout = "groups", groups = groups) |>
    sn_nodes(fill = colors)

  result <- safe_plot(splot(net, groups = groups, legend = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# MULTIPLE AESTHETIC CHANGES
# ============================================

test_that("multiple sn_nodes calls accumulate", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_nodes(fill = "red") |>
    sn_nodes(size = 6) |>
    sn_nodes(shape = "square")

  aes <- net$network$get_node_aes()

  # All customizations should be preserved
  expect_true(all(aes$fill == "red"))
  expect_true(all(aes$size == 6))
  expect_true(all(aes$shape == "square"))
})

test_that("multiple sn_edges calls accumulate", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_edges(color = "gray50") |>
    sn_edges(width = 2) |>
    sn_edges(alpha = 0.7)

  aes <- net$network$get_edge_aes()

  expect_true(all(aes$color == "gray50"))
  expect_true(all(aes$width == 2))
  expect_true(all(aes$alpha == 0.7))
})

# ============================================
# REPRODUCIBILITY WITH SEEDS
# ============================================

test_that("full workflow is reproducible with seed", {
  adj <- create_test_matrix(5)

  # First run
  net1 <- adj |>
    sonnet(layout = "spring", seed = 42) |>
    sn_nodes(fill = "steelblue")

  # Second run with same seed
  net2 <- adj |>
    sonnet(layout = "spring", seed = 42) |>
    sn_nodes(fill = "steelblue")

  layout1 <- net1$network$get_layout()
  layout2 <- net2$network$get_layout()

  expect_equal(layout1$x, layout2$x)
  expect_equal(layout1$y, layout2$y)
})

test_that("sn_layout preserves seed reproducibility", {
  adj <- create_test_matrix(5)

  net1 <- sonnet(adj) |> sn_layout("spring", seed = 123)
  net2 <- sonnet(adj) |> sn_layout("spring", seed = 123)

  layout1 <- net1$network$get_layout()
  layout2 <- net2$network$get_layout()

  expect_equal(layout1$x, layout2$x)
  expect_equal(layout1$y, layout2$y)
})

# ============================================
# SAVE WORKFLOW
# ============================================

test_that("customize and save to PDF workflow", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  net <- sonnet(adj, layout = "circle") |>
    sn_nodes(fill = palette_viridis(4)) |>
    sn_edges(color = "gray") |>
    sn_theme("minimal")

  expect_message(sn_save(net, tmp, width = 6, height = 6), "Saved")
  expect_file_created(tmp)
})

test_that("customize and save to PNG workflow", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  net <- sonnet(adj) |>
    sn_nodes(shape = c("circle", "square", "triangle", "diamond")) |>
    sn_theme("dark")

  expect_message(sn_save(net, tmp, dpi = 150), "Saved")
  expect_file_created(tmp)
})

# ============================================
# COMPLEX NETWORK SCENARIOS
# ============================================

test_that("directed weighted network with all customizations", {
  adj <- create_test_matrix(5, weighted = TRUE, symmetric = FALSE)

  net <- sonnet(adj, directed = TRUE) |>
    sn_nodes(
      size = c(4, 5, 6, 5, 4),
      fill = palette_colorblind(5),
      border_width = 2
    ) |>
    sn_edges(
      positive_color = "darkgreen",
      negative_color = "darkred",
      alpha = 0.8
    ) |>
    sn_theme("minimal")

  result <- safe_plot(splot(net, show_arrows = TRUE, legend = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("network with donut nodes and edge labels", {
  adj <- create_test_matrix(4, weighted = TRUE)

  net <- sonnet(adj) |>
    sn_nodes(
      donut_fill = c(0.3, 0.5, 0.7, 0.9),
      donut_color = "steelblue",
      donut_show_value = TRUE
    ) |>
    sn_edges(labels = TRUE)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("network with pie nodes and curved edges", {
  adj <- create_test_matrix(4)

  pie_vals <- list(c(1, 2, 3), c(2, 2), c(1, 1, 1, 1), c(3, 1))
  pie_cols <- list(
    palette_pastel(3),
    palette_pastel(2),
    palette_pastel(4),
    palette_pastel(2)
  )

  net <- sonnet(adj) |>
    sn_nodes(pie_values = pie_vals, pie_colors = pie_cols) |>
    sn_edges(curvature = 0.3)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

# ============================================
# IGRAPH INTEGRATION WORKFLOW
# ============================================

test_that("igraph to sonnet workflow with customization", {
  skip_if_no_igraph()

  g <- igraph::make_ring(6)

  net <- sonnet(g) |>
    sn_layout("kk", seed = 42) |>
    sn_nodes(fill = palette_viridis(6)) |>
    sn_theme("colorblind")

  expect_sonnet_network(net)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("igraph with vertex attributes workflow", {
  skip_if_no_igraph()

  g <- igraph::make_star(5)
  igraph::V(g)$name <- c("Center", "A", "B", "C", "D")

  net <- sonnet(g, layout = "star") |>
    sn_nodes(
      fill = c("red", rep("blue", 4)),
      size = c(8, rep(5, 4))
    )

  result <- safe_plot(splot(net, labels = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# ERROR RECOVERY
# ============================================

test_that("workflow continues after recoverable error", {
  adj <- create_test_matrix(4)

  # Start workflow
  net <- sonnet(adj)

  # Invalid operation - should error
  err <- tryCatch(
    sn_nodes(net, alpha = 2.0),  # Invalid alpha
    error = function(e) "error"
  )
  expect_equal(err, "error")

  # Continue with valid operations
  net2 <- net |>
    sn_nodes(alpha = 0.8) |>
    sn_edges(color = "gray")

  expect_sonnet_network(net2)
})

# ============================================
# GGPLOT INTEGRATION
# ============================================

test_that("sn_ggplot workflow with customizations", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_nodes(fill = palette_colorblind(4)) |>
    sn_theme("minimal")

  p <- sn_ggplot(net, title = "Test Network")

  expect_s3_class(p, "ggplot")
})

test_that("sn_save_ggplot workflow", {
  skip_if_not_installed("ggplot2")

  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  net <- sonnet(adj) |>
    sn_nodes(fill = "steelblue") |>
    sn_theme("classic")

  expect_message(sn_save_ggplot(net, tmp, dpi = 100), "Saved")
  expect_file_created(tmp)
})

# ============================================
# PERFORMANCE CONSIDERATIONS
# ============================================

test_that("moderate-size network workflow completes", {
  skip_on_cran()

  adj <- create_test_matrix(30, density = 0.15)

  net <- adj |>
    sonnet(layout = "spring", seed = 42) |>
    sn_nodes(fill = palette_viridis(30)) |>
    sn_edges(alpha = 0.6) |>
    sn_theme("minimal")

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})
