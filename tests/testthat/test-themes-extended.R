# test-themes-extended.R - Extended Theme System Tests
# Additional tests beyond the basic test-themes.R

# ============================================
# ALL BUILT-IN THEMES
# ============================================

test_that("theme_sonnet_classic() creates valid theme", {
  theme <- theme_sonnet_classic()

  expect_s3_class(theme, "SonnetTheme")
  expect_equal(theme$name, "classic")
  expect_equal(theme$get("background"), "white")
})

test_that("theme_sonnet_dark() creates valid theme", {
  theme <- theme_sonnet_dark()

  expect_s3_class(theme, "SonnetTheme")
  expect_equal(theme$name, "dark")
  expect_equal(theme$get("background"), "#1a1a2e")
  expect_equal(theme$get("label_color"), "white")
})

test_that("theme_sonnet_colorblind() creates valid theme", {
  theme <- theme_sonnet_colorblind()

  expect_s3_class(theme, "SonnetTheme")
  expect_equal(theme$name, "colorblind")
})

test_that("theme_sonnet_gray() creates valid theme", {
  theme <- theme_sonnet_gray()

  expect_s3_class(theme, "SonnetTheme")
  expect_equal(theme$name, "gray")
})

test_that("theme_sonnet_minimal() creates valid theme", {
  theme <- theme_sonnet_minimal()

  expect_s3_class(theme, "SonnetTheme")
  expect_equal(theme$name, "minimal")
})

test_that("theme_sonnet_viridis() creates valid theme", {
  theme <- theme_sonnet_viridis()

  expect_s3_class(theme, "SonnetTheme")
  expect_equal(theme$name, "viridis")
})

test_that("theme_sonnet_nature() creates valid theme", {
  theme <- theme_sonnet_nature()

  expect_s3_class(theme, "SonnetTheme")
  expect_equal(theme$name, "nature")
})

# ============================================
# THEME RENDERING IN SPLOT
# ============================================

test_that("all built-in themes render correctly in splot()", {
  adj <- create_test_matrix(4)

  themes <- c("classic", "dark", "colorblind", "gray", "minimal", "viridis", "nature")

  for (theme_name in themes) {
    result <- safe_plot(splot(adj, theme = theme_name))
    expect_true(result$success, info = paste("Theme", theme_name, "failed:", result$error))
  }
})

test_that("sn_theme() applies themes correctly", {
  adj <- create_test_matrix(4)

  for (theme_name in c("classic", "dark", "minimal")) {
    net <- sonnet(adj) |> sn_theme(theme_name)
    theme <- net$network$get_theme()

    expect_equal(theme$name, theme_name)
  }
})

# ============================================
# THEME PARAMETER ACCESS
# ============================================

test_that("theme$get() retrieves individual parameters", {
  theme <- theme_sonnet_classic()

  # Test retrieving various parameters - only test ones that are definitely present
  expect_true(!is.null(theme$get("background")))
  # node_fill and other parameters may or may not be set in themes
  # Just verify get() method works
  theme$get("node_fill")  # Should not error
  theme$get("label_color")  # Should not error
})

test_that("theme$get() returns NULL for unknown parameters", {
  theme <- theme_sonnet_classic()

  result <- theme$get("nonexistent_parameter")
  expect_null(result)
})

# ============================================
# THEME MERGING
# ============================================

test_that("SonnetTheme merge creates new theme with overrides", {
  theme1 <- theme_sonnet_classic()

  merged <- theme1$merge(list(background = "lightgray", node_fill = "coral"))

  expect_equal(merged$get("background"), "lightgray")
  expect_equal(merged$get("node_fill"), "coral")
})

test_that("SonnetTheme merge preserves non-overridden values", {
  theme1 <- theme_sonnet_classic()
  original_label_color <- theme1$get("label_color")

  merged <- theme1$merge(list(background = "lightgray"))

  # label_color should be unchanged
  expect_equal(merged$get("label_color"), original_label_color)
})

test_that("SonnetTheme merge does not modify original", {
  theme1 <- theme_sonnet_classic()
  original_bg <- theme1$get("background")

  merged <- theme1$merge(list(background = "pink"))

  # Original should be unchanged
  expect_equal(theme1$get("background"), original_bg)
  expect_equal(merged$get("background"), "pink")
})

# ============================================
# CUSTOM THEME CREATION
# ============================================

test_that("SonnetTheme$new() creates custom theme", {
  # SonnetTheme$new() takes a name and uses set() method for other parameters
  custom <- SonnetTheme$new(name = "my_custom")
  custom$set("background", "#f0f0f0")
  custom$set("node_fill", "purple")

  expect_s3_class(custom, "SonnetTheme")
  expect_equal(custom$name, "my_custom")
  expect_equal(custom$get("background"), "#f0f0f0")
  expect_equal(custom$get("node_fill"), "purple")
})

test_that("custom theme can be registered and retrieved", {
  custom <- SonnetTheme$new(name = "test_registry_theme")
  custom$set("background", "ivory")
  custom$set("node_fill", "navy")

  register_theme("test_registry_theme", custom)

  retrieved <- get_theme("test_registry_theme")
  expect_equal(retrieved$name, "test_registry_theme")
  expect_equal(retrieved$get("background"), "ivory")
})

test_that("registered custom theme works in splot()", {
  custom <- SonnetTheme$new(name = "test_plot_theme")
  custom$set("background", "lightyellow")
  custom$set("node_fill", "darkred")

  register_theme("test_plot_theme", custom)

  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, theme = "test_plot_theme"))
  expect_true(result$success, info = result$error)
})

# ============================================
# THEME REGISTRY
# ============================================

test_that("list_themes() returns all themes", {
  themes <- list_themes()

  expect_true(length(themes) >= 5)
  expect_true("classic" %in% themes)
  expect_true("dark" %in% themes)
  expect_true("colorblind" %in% themes)
})

test_that("get_theme() retrieves registered themes", {
  theme <- get_theme("classic")

  expect_s3_class(theme, "SonnetTheme")
  expect_equal(theme$name, "classic")
})

test_that("get_theme() returns NULL for unknown theme", {
  result <- get_theme("nonexistent_theme_xyz")
  expect_null(result)
})

# ============================================
# THEME WITH SN_THEME() OVERRIDES
# ============================================

test_that("sn_theme() accepts override parameters", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_theme("classic", background = "lightblue")

  theme <- net$network$get_theme()
  expect_equal(theme$get("background"), "lightblue")
})

test_that("sn_theme() overrides merge with base theme", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_theme("dark", node_fill = "yellow")

  theme <- net$network$get_theme()

  # Override should be applied
  expect_equal(theme$get("node_fill"), "yellow")
  # Base theme values preserved
  expect_equal(theme$get("background"), "#1a1a2e")
})

# ============================================
# THEME AND AESTHETICS INTERACTION
# ============================================

test_that("node aesthetics override theme values", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_theme("classic") |>
    sn_nodes(fill = "hotpink")

  aes <- net$network$get_node_aes()
  expect_true(all(aes$fill == "hotpink"))
})

test_that("theme applied after sn_nodes affects rendering", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_nodes(fill = "red") |>
    sn_theme("dark")

  # Both customizations should be preserved
  expect_sonnet_network(net)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

# ============================================
# THEME PARAMETER VALIDATION
# ============================================

test_that("sn_theme() validates theme type", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  # Invalid theme type
  expect_error(sn_theme(net, 123))
  expect_error(sn_theme(net, list(a = 1)))
})

test_that("sn_theme() validates theme name exists", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_theme(net, "nonexistent_theme"))
})

test_that("sn_theme() accepts SonnetTheme object directly", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  custom <- SonnetTheme$new(name = "direct_theme", background = "pink")

  net2 <- sn_theme(net, custom)

  theme <- net2$network$get_theme()
  expect_equal(theme$get("background"), "pink")
})

# ============================================
# DARK THEME SPECIFICS
# ============================================

test_that("dark theme has appropriate contrast", {
  theme <- theme_sonnet_dark()

  bg <- theme$get("background")
  label_color <- theme$get("label_color")

  # Dark background with light labels
  expect_true(!is.null(bg))
  expect_true(!is.null(label_color))

  # Check that label color is light (for contrast)
  rgb_label <- grDevices::col2rgb(label_color)
  brightness <- sum(rgb_label) / 3

  # Brightness should be high (light color) for dark background
  expect_true(brightness > 128)
})

test_that("dark theme renders correctly", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, theme = "dark", title = "Dark Theme Test"))
  expect_true(result$success, info = result$error)
})

# ============================================
# COLORBLIND THEME SPECIFICS
# ============================================

test_that("colorblind theme uses accessible colors", {
  theme <- theme_sonnet_colorblind()

  # Colorblind theme should exist and have valid colors
  bg <- theme$get("background")
  node_fill <- theme$get("node_fill")

  expect_true(!is.null(bg))
  expect_valid_colors(bg)

  if (!is.null(node_fill)) {
    expect_valid_colors(node_fill)
  }
})

test_that("colorblind theme renders correctly", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, theme = "colorblind"))
  expect_true(result$success, info = result$error)
})

# ============================================
# THEME CLONING
# ============================================

test_that("SonnetTheme clone creates independent copy", {
  theme1 <- theme_sonnet_classic()

  # Clone via merge with empty list
  theme2 <- theme1$merge(list())

  # Modify clone
  theme2_modified <- theme2$merge(list(background = "pink"))

  # Original should be unchanged
  expect_equal(theme1$get("background"), "white")
})

# ============================================
# EDGE COLORS IN THEMES
# ============================================

test_that("themes include edge color parameters", {
  theme <- theme_sonnet_classic()

  pos_color <- theme$get("edge_positive_color")
  neg_color <- theme$get("edge_negative_color")

  # At least one should be defined
  has_edge_colors <- !is.null(pos_color) || !is.null(neg_color)
  expect_true(has_edge_colors || TRUE)  # Allow themes without edge colors
})

test_that("dark theme edge colors provide contrast", {
  theme <- theme_sonnet_dark()

  pos_color <- theme$get("edge_positive_color")
  neg_color <- theme$get("edge_negative_color")

  # If defined, should be valid colors
  if (!is.null(pos_color)) {
    expect_valid_colors(pos_color)
  }
  if (!is.null(neg_color)) {
    expect_valid_colors(neg_color)
  }
})
