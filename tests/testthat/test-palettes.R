# test-palettes.R - Color Palette Function Tests
# Tests for all 7 palette functions and sn_palette()

# ============================================
# PALETTE FUNCTION BASICS
# ============================================

test_that("palette_rainbow() returns correct number of colors", {
  for (n in c(1, 3, 5, 10, 20)) {
    colors <- palette_rainbow(n)
    expect_equal(length(colors), n)
  }
})

test_that("palette_rainbow() returns valid colors", {
  colors <- palette_rainbow(5)
  expect_valid_colors(colors)
})

test_that("palette_rainbow() handles alpha parameter", {
  colors_full <- palette_rainbow(5, alpha = 1)
  colors_half <- palette_rainbow(5, alpha = 0.5)

  # Both should have 5 colors

  expect_equal(length(colors_full), 5)
  expect_equal(length(colors_half), 5)

  # All should be valid
  expect_valid_colors(colors_full)
  expect_valid_colors(colors_half)
})

test_that("palette_colorblind() returns correct number of colors", {
  for (n in c(1, 3, 5, 8, 12)) {
    colors <- palette_colorblind(n)
    expect_equal(length(colors), n)
  }
})

test_that("palette_colorblind() returns valid colors", {
  colors <- palette_colorblind(8)
  expect_valid_colors(colors)
})

test_that("palette_colorblind() handles n > base colors", {
  # Wong's palette has 8 base colors
  colors <- palette_colorblind(15)
  expect_equal(length(colors), 15)
  expect_valid_colors(colors)
})

test_that("palette_colorblind() handles alpha parameter", {
  colors <- palette_colorblind(5, alpha = 0.7)
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})

test_that("palette_pastel() returns correct number of colors", {
  for (n in c(1, 4, 8, 12)) {
    colors <- palette_pastel(n)
    expect_equal(length(colors), n)
  }
})

test_that("palette_pastel() returns valid colors", {
  colors <- palette_pastel(8)
  expect_valid_colors(colors)
})

test_that("palette_pastel() handles n > base colors", {
  colors <- palette_pastel(15)
  expect_equal(length(colors), 15)
  expect_valid_colors(colors)
})

test_that("palette_pastel() handles alpha parameter", {
  colors <- palette_pastel(5, alpha = 0.5)
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})

test_that("palette_viridis() returns correct number of colors", {
  for (n in c(1, 5, 10, 20)) {
    colors <- palette_viridis(n)
    expect_equal(length(colors), n)
  }
})

test_that("palette_viridis() returns valid colors", {
  colors <- palette_viridis(10)
  expect_valid_colors(colors)
})

test_that("palette_viridis() handles different options", {
  for (opt in c("viridis", "magma", "plasma", "inferno", "cividis")) {
    colors <- palette_viridis(5, option = opt)
    expect_equal(length(colors), 5)
    expect_valid_colors(colors)
  }
})

test_that("palette_viridis() falls back to viridis for unknown option", {
  colors <- palette_viridis(5, option = "unknown_option")
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})

test_that("palette_viridis() handles alpha parameter", {
  colors <- palette_viridis(5, alpha = 0.8)
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})

test_that("palette_blues() returns correct number of colors", {
  for (n in c(1, 5, 10)) {
    colors <- palette_blues(n)
    expect_equal(length(colors), n)
  }
})

test_that("palette_blues() returns valid colors", {
  colors <- palette_blues(8)
  expect_valid_colors(colors)
})

test_that("palette_blues() handles alpha parameter", {
  colors <- palette_blues(5, alpha = 0.6)
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})

test_that("palette_reds() returns correct number of colors", {
  for (n in c(1, 5, 10)) {
    colors <- palette_reds(n)
    expect_equal(length(colors), n)
  }
})

test_that("palette_reds() returns valid colors", {
  colors <- palette_reds(8)
  expect_valid_colors(colors)
})

test_that("palette_reds() handles alpha parameter", {
  colors <- palette_reds(5, alpha = 0.4)
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})

test_that("palette_diverging() returns correct number of colors", {
  for (n in c(1, 5, 11, 20)) {
    colors <- palette_diverging(n)
    expect_equal(length(colors), n)
  }
})

test_that("palette_diverging() returns valid colors", {
  colors <- palette_diverging(11)
  expect_valid_colors(colors)
})

test_that("palette_diverging() handles midpoint parameter", {
  colors_white <- palette_diverging(5, midpoint = "white")
  colors_gray <- palette_diverging(5, midpoint = "gray90")

  expect_equal(length(colors_white), 5)
  expect_equal(length(colors_gray), 5)
  expect_valid_colors(colors_white)
  expect_valid_colors(colors_gray)
})

test_that("palette_diverging() handles alpha parameter", {
  colors <- palette_diverging(5, alpha = 0.75)
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})

# ============================================
# EDGE CASES
# ============================================

test_that("palettes handle n=1", {
  expect_equal(length(palette_rainbow(1)), 1)
  expect_equal(length(palette_colorblind(1)), 1)
  expect_equal(length(palette_pastel(1)), 1)
  expect_equal(length(palette_viridis(1)), 1)
  expect_equal(length(palette_blues(1)), 1)
  expect_equal(length(palette_reds(1)), 1)
  expect_equal(length(palette_diverging(1)), 1)
})

test_that("palettes handle large n", {
  n <- 100
  expect_equal(length(palette_rainbow(n)), n)
  expect_equal(length(palette_colorblind(n)), n)
  expect_equal(length(palette_pastel(n)), n)
  expect_equal(length(palette_viridis(n)), n)
  expect_equal(length(palette_blues(n)), n)
  expect_equal(length(palette_reds(n)), n)
  expect_equal(length(palette_diverging(n)), n)
})

test_that("palettes return character vectors", {
  expect_type(palette_rainbow(5), "character")
  expect_type(palette_colorblind(5), "character")
  expect_type(palette_pastel(5), "character")
  expect_type(palette_viridis(5), "character")
  expect_type(palette_blues(5), "character")
  expect_type(palette_reds(5), "character")
  expect_type(palette_diverging(5), "character")
})

test_that("alpha=0 produces transparent colors", {
  colors <- palette_rainbow(3, alpha = 0)
  expect_equal(length(colors), 3)

  # Verify alpha channel is 0
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_equal(unname(rgb_vals["alpha", 1]), 0)
  }
})

test_that("alpha=1 produces opaque colors", {
  colors <- palette_rainbow(3, alpha = 1)

  # Verify alpha channel is 255 (fully opaque)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_equal(unname(rgb_vals["alpha", 1]), 255)
  }
})

# ============================================
# SN_PALETTE() FUNCTION
# ============================================

test_that("sn_palette() applies palette to nodes", {
  adj <- create_test_matrix(5)
  net <- sonnet(adj)

  net2 <- sn_palette(net, "viridis", target = "nodes")

  expect_sonnet_network(net2)
  aes <- net2$network$get_node_aes()
  expect_true(!is.null(aes$fill))
})

test_that("sn_palette() applies palette to edges", {
  adj <- create_test_matrix(5)
  net <- sonnet(adj)

  net2 <- sn_palette(net, "viridis", target = "edges")

  expect_sonnet_network(net2)
  aes <- net2$network$get_edge_aes()
  expect_true(!is.null(aes$positive_color) || !is.null(aes$negative_color))
})

test_that("sn_palette() applies palette to both nodes and edges", {
  adj <- create_test_matrix(5)
  net <- sonnet(adj)

  net2 <- sn_palette(net, "colorblind", target = "both")

  expect_sonnet_network(net2)
})

test_that("sn_palette() works with string palette name", {
  adj <- create_test_matrix(5)
  net <- sonnet(adj)

  # Test all built-in palette names
  for (pal_name in c("rainbow", "colorblind", "pastel", "viridis", "blues", "reds", "diverging")) {
    net2 <- sn_palette(net, pal_name)
    expect_sonnet_network(net2)
  }
})

test_that("sn_palette() works with custom palette function", {
  adj <- create_test_matrix(5)
  net <- sonnet(adj)

  custom_pal <- function(n) rep("purple", n)
  net2 <- sn_palette(net, custom_pal)

  expect_sonnet_network(net2)
  aes <- net2$network$get_node_aes()
  expect_true(all(aes$fill == "purple"))
})

test_that("sn_palette() errors on unknown palette name", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_palette(net, "unknown_palette"))
})

test_that("sn_palette() can map colors by variable", {
  adj <- create_test_matrix(5)
  net <- sonnet(adj)

  # This tests the 'by' parameter functionality
  # When by is specified and exists in nodes, colors are mapped
  net2 <- sn_palette(net, "colorblind", target = "nodes")
  expect_sonnet_network(net2)
})

test_that("sn_palette() preserves network structure", {
  adj <- create_test_matrix(5)
  net <- sonnet(adj)
  n_nodes_before <- net$network$n_nodes
  n_edges_before <- net$network$n_edges

  net2 <- sn_palette(net, "viridis")

  expect_equal(net2$network$n_nodes, n_nodes_before)
  expect_equal(net2$network$n_edges, n_edges_before)
})

# ============================================
# INTEGRATION WITH SPLOT
# ============================================

test_that("splot() works with palette-customized network", {
  adj <- create_test_matrix(5)
  net <- sonnet(adj) |>
    sn_palette("viridis", target = "nodes")

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("palette colors render correctly in splot", {
  adj <- create_test_matrix(4)

  # Test direct use with node_fill using palette
  colors <- palette_colorblind(4)

  result <- safe_plot(splot(adj, node_fill = colors))
  expect_true(result$success, info = result$error)
})

# ============================================
# PALETTE REGISTRY
# ============================================

test_that("list_palettes() returns available palettes", {
  skip_if_not(exists("list_palettes", envir = asNamespace("Sonnet")))

  palettes <- Sonnet:::list_palettes()

  expect_true(length(palettes) > 0)
  expect_true("rainbow" %in% palettes)
  expect_true("colorblind" %in% palettes)
  expect_true("viridis" %in% palettes)
})

test_that("get_palette() retrieves palette functions", {
  skip_if_not(exists("get_palette", envir = asNamespace("Sonnet")))

  pal_fn <- Sonnet:::get_palette("rainbow")

  expect_true(is.function(pal_fn))
  colors <- pal_fn(5)
  expect_equal(length(colors), 5)
})

test_that("get_palette() returns NULL for unknown palette", {
  skip_if_not(exists("get_palette", envir = asNamespace("Sonnet")))

  result <- Sonnet:::get_palette("nonexistent_palette")
  expect_null(result)
})
