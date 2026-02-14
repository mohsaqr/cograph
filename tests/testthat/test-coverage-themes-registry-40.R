# =============================================================================
# Test Coverage for themes-registry.R
# =============================================================================
# Comprehensive tests for theme registration, retrieval, built-in themes,
# theme application, and CographTheme class functionality

# =============================================================================
# Test: register_builtin_themes function
# =============================================================================

test_that("register_builtin_themes registers classic theme", {
  expect_true("classic" %in% list_themes())
  theme <- get_theme("classic")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "classic")
})

test_that("register_builtin_themes registers colorblind theme", {
  expect_true("colorblind" %in% list_themes())
  theme <- get_theme("colorblind")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "colorblind")
})

test_that("register_builtin_themes registers gray theme", {
  expect_true("gray" %in% list_themes())
  theme <- get_theme("gray")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "gray")
})

test_that("register_builtin_themes registers grey alias", {
  expect_true("grey" %in% list_themes())
  theme_gray <- get_theme("gray")
  theme_grey <- get_theme("grey")

  # Both should return the same theme (same values)
  expect_equal(theme_gray$get("background"), theme_grey$get("background"))
  expect_equal(theme_gray$get("node_fill"), theme_grey$get("node_fill"))
})

test_that("register_builtin_themes registers dark theme", {
  expect_true("dark" %in% list_themes())
  theme <- get_theme("dark")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "dark")
})

test_that("register_builtin_themes registers minimal theme", {
  expect_true("minimal" %in% list_themes())
  theme <- get_theme("minimal")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "minimal")
})

test_that("register_builtin_themes registers viridis theme", {
  expect_true("viridis" %in% list_themes())
  theme <- get_theme("viridis")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "viridis")
})

test_that("register_builtin_themes registers nature theme", {
  expect_true("nature" %in% list_themes())
  theme <- get_theme("nature")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "nature")
})

# =============================================================================
# Test: Theme registry functions (register_theme, get_theme, list_themes)
# =============================================================================

test_that("list_themes returns character vector", {
  themes <- list_themes()
  expect_type(themes, "character")
  expect_true(length(themes) >= 7)  # At least 7 built-in themes
})

test_that("list_themes contains all expected built-in themes", {
  themes <- list_themes()
  expected <- c("classic", "colorblind", "gray", "grey", "dark", "minimal", "viridis", "nature")

  for (name in expected) {
    expect_true(name %in% themes, info = paste("Missing theme:", name))
  }
})

test_that("get_theme returns CographTheme for valid theme", {
  theme <- get_theme("classic")
  expect_s3_class(theme, "CographTheme")
})

test_that("get_theme returns NULL for unknown theme", {
  result <- get_theme("nonexistent_theme_xyz123")
  expect_null(result)
})

test_that("register_theme can register new theme", {
  custom <- CographTheme$new(name = "test_custom_1")
  register_theme("test_custom_1", custom)

  expect_true("test_custom_1" %in% list_themes())
  retrieved <- get_theme("test_custom_1")
  expect_equal(retrieved$name, "test_custom_1")
})

test_that("register_theme can register theme with list", {
  custom_list <- list(
    background = "ivory",
    node_fill = "coral"
  )
  register_theme("test_custom_list", custom_list)

  expect_true("test_custom_list" %in% list_themes())
  retrieved <- get_theme("test_custom_list")
  expect_equal(retrieved$background, "ivory")
})

test_that("register_theme can overwrite existing theme", {
  # Register initial theme
  custom1 <- CographTheme$new(name = "test_overwrite", background = "red")
  register_theme("test_overwrite", custom1)

  # Overwrite
  custom2 <- CographTheme$new(name = "test_overwrite", background = "blue")
  register_theme("test_overwrite", custom2)

  retrieved <- get_theme("test_overwrite")
  expect_equal(retrieved$get("background"), "blue")
})

# =============================================================================
# Test: theme_cograph_* functions
# =============================================================================

test_that("theme_cograph_classic returns valid CographTheme", {
  theme <- theme_cograph_classic()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "classic")
  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("node_fill"), "#4A90D9")
  expect_equal(theme$get("node_border"), "#2C5AA0")
})

test_that("theme_cograph_colorblind returns valid CographTheme", {
  theme <- theme_cograph_colorblind()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "colorblind")
  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("node_fill"), "#0072B2")
})

test_that("theme_cograph_gray returns valid CographTheme", {
  theme <- theme_cograph_gray()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "gray")
  expect_equal(theme$get("node_fill"), "gray70")
  expect_equal(theme$get("node_border"), "gray30")
})

test_that("theme_cograph_dark returns valid CographTheme", {
  theme <- theme_cograph_dark()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "dark")
  expect_equal(theme$get("background"), "#1a1a2e")
  expect_equal(theme$get("label_color"), "white")
})

test_that("theme_cograph_minimal returns valid CographTheme", {
  theme <- theme_cograph_minimal()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "minimal")
  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("node_fill"), "white")
})

test_that("theme_cograph_viridis returns valid CographTheme", {
  theme <- theme_cograph_viridis()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "viridis")
  expect_equal(theme$get("node_fill"), "#21918c")
})

test_that("theme_cograph_nature returns valid CographTheme", {
  theme <- theme_cograph_nature()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "nature")
  expect_equal(theme$get("background"), "#fefae0")
  expect_equal(theme$get("node_fill"), "#606c38")
})

# =============================================================================
# Test: CographTheme class functionality
# =============================================================================

test_that("CographTheme$new creates theme with defaults", {
  theme <- CographTheme$new()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "custom")
  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("node_fill"), "#4A90D9")
})

test_that("CographTheme$new accepts custom parameters", {
  theme <- CographTheme$new(
    name = "my_theme",
    background = "black",
    node_fill = "yellow"
  )

  expect_equal(theme$name, "my_theme")
  expect_equal(theme$get("background"), "black")
  expect_equal(theme$get("node_fill"), "yellow")
})

test_that("CographTheme$get retrieves parameter values", {
  theme <- theme_cograph_classic()

  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("label_size"), 10)
  expect_equal(theme$get("title_size"), 14)
})

test_that("CographTheme$get returns NULL for unknown parameter", {
  theme <- theme_cograph_classic()

  result <- theme$get("unknown_param_xyz")
  expect_null(result)
})

test_that("CographTheme$set updates parameter value", {
  theme <- CographTheme$new()

  theme$set("background", "pink")
  expect_equal(theme$get("background"), "pink")

  theme$set("node_fill", "purple")
  expect_equal(theme$get("node_fill"), "purple")
})

test_that("CographTheme$set returns self for chaining", {
  theme <- CographTheme$new()

  result <- theme$set("background", "gray")

  # Should return self

  expect_s3_class(result, "CographTheme")
  expect_identical(result, theme)
})

test_that("CographTheme$get_all returns all parameters", {
  theme <- theme_cograph_classic()

  params <- theme$get_all()

  expect_type(params, "list")
  expect_true("background" %in% names(params))
  expect_true("node_fill" %in% names(params))
  expect_true("edge_color" %in% names(params))
  expect_true("label_color" %in% names(params))
})

test_that("CographTheme$merge merges with list", {
  theme <- theme_cograph_classic()

  merged <- theme$merge(list(background = "lightgray", node_fill = "orange"))

  expect_equal(merged$get("background"), "lightgray")
  expect_equal(merged$get("node_fill"), "orange")
  # Unchanged values preserved
  expect_equal(merged$get("edge_color"), theme$get("edge_color"))
})

test_that("CographTheme$merge merges with another CographTheme", {
  theme1 <- theme_cograph_classic()
  theme2 <- CographTheme$new(background = "navy", label_color = "white")

  merged <- theme1$merge(theme2)

  expect_equal(merged$get("background"), "navy")
  expect_equal(merged$get("label_color"), "white")
})

test_that("CographTheme$merge does not modify original", {
  theme <- theme_cograph_classic()
  original_bg <- theme$get("background")

  merged <- theme$merge(list(background = "pink"))

  expect_equal(theme$get("background"), original_bg)
  expect_equal(merged$get("background"), "pink")
})

test_that("CographTheme$merge returns new CographTheme", {
  theme <- theme_cograph_classic()

  merged <- theme$merge(list())

  expect_s3_class(merged, "CographTheme")
  expect_equal(merged$name, "merged")
})

test_that("CographTheme$clone_theme creates independent copy", {
  theme <- theme_cograph_classic()

  cloned <- theme$clone_theme()

  expect_s3_class(cloned, "CographTheme")
  expect_equal(cloned$get("background"), theme$get("background"))

  # Modifying clone should not affect original
  cloned$set("background", "red")
  expect_equal(theme$get("background"), "white")
  expect_equal(cloned$get("background"), "red")
})

test_that("CographTheme$print outputs summary", {
  theme <- theme_cograph_classic()

  output <- capture.output(theme$print())

  expect_true(any(grepl("CographTheme", output)))
  expect_true(any(grepl("classic", output)))
})

test_that("CographTheme name active binding returns correct name", {
  theme <- CographTheme$new(name = "test_binding")

  expect_equal(theme$name, "test_binding")
})

# =============================================================================
# Test: is_cograph_theme helper function
# =============================================================================

test_that("is_cograph_theme returns TRUE for CographTheme", {
  theme <- theme_cograph_classic()

  expect_true(cograph:::is_cograph_theme(theme))
})
test_that("is_cograph_theme returns FALSE for list", {
  theme_list <- list(background = "white")

  expect_false(cograph:::is_cograph_theme(theme_list))
})

test_that("is_cograph_theme returns FALSE for other objects", {
  expect_false(cograph:::is_cograph_theme(NULL))
  expect_false(cograph:::is_cograph_theme("classic"))
  expect_false(cograph:::is_cograph_theme(42))
})

# =============================================================================
# Test: Theme parameters - edge colors
# =============================================================================

test_that("themes have edge_positive_color defined", {
  themes <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")

  for (name in themes) {
    theme <- get_theme(name)
    pos_color <- theme$get("edge_positive_color")
    expect_true(!is.null(pos_color), info = paste("Theme:", name))
    expect_valid_colors(pos_color)
  }
})

test_that("themes have edge_negative_color defined", {
  themes <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")

  for (name in themes) {
    theme <- get_theme(name)
    neg_color <- theme$get("edge_negative_color")
    expect_true(!is.null(neg_color), info = paste("Theme:", name))
    expect_valid_colors(neg_color)
  }
})

test_that("dark theme has contrasting label color", {
  theme <- theme_cograph_dark()

  bg <- theme$get("background")
  label_color <- theme$get("label_color")

  # Background should be dark
  bg_rgb <- grDevices::col2rgb(bg)
  bg_brightness <- sum(bg_rgb) / 3
  expect_true(bg_brightness < 128, info = "Background should be dark")

  # Label should be light
  label_rgb <- grDevices::col2rgb(label_color)
  label_brightness <- sum(label_rgb) / 3
  expect_true(label_brightness > 128, info = "Label should be light for contrast")
})

# =============================================================================
# Test: Theme application in splot
# =============================================================================

test_that("splot accepts theme name string", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, theme = "classic"))
  expect_true(result$success, info = result$error)
})

test_that("splot accepts theme by name only", {
  adj <- create_test_matrix(4)

  # splot uses get_theme() internally which expects a string name
  result <- safe_plot(splot(adj, theme = "dark"))
  expect_true(result$success, info = result$error)
})

test_that("all built-in themes render correctly in splot", {
  adj <- create_test_matrix(4)

  themes <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")

  for (theme_name in themes) {
    result <- safe_plot(splot(adj, theme = theme_name))
    expect_true(result$success, info = paste("Theme", theme_name, "failed:", result$error))
  }
})

test_that("grey alias works in splot", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, theme = "grey"))
  expect_true(result$success, info = result$error)
})

# =============================================================================
# Test: Theme application via sn_theme
# =============================================================================

test_that("sn_theme applies theme to network", {
  adj <- create_test_matrix(4)

  net <- cograph(adj) |> sn_theme("dark")

  expect_s3_class(net$theme, "CographTheme")
  expect_equal(net$theme$name, "dark")
})

test_that("sn_theme accepts CographTheme object", {
  adj <- create_test_matrix(4)
  custom <- CographTheme$new(name = "custom_direct", background = "lightblue")

  net <- cograph(adj) |> sn_theme(custom)

  expect_equal(net$theme$get("background"), "lightblue")
})

test_that("sn_theme accepts override parameters", {
  adj <- create_test_matrix(4)

  net <- cograph(adj) |> sn_theme("classic", background = "pink")

  expect_equal(net$theme$get("background"), "pink")
  # Other classic values preserved
  expect_equal(net$theme$get("node_fill"), "#4A90D9")
})

test_that("sn_theme errors on unknown theme name", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(sn_theme(net, "unknown_theme_xyz"))
})

test_that("sn_theme errors on invalid theme type", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(sn_theme(net, 123))
  expect_error(sn_theme(net, list(a = 1)))
})

# =============================================================================
# Test: Theme colors are valid
# =============================================================================

test_that("all theme colors are valid R colors", {
  themes <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")

  for (name in themes) {
    theme <- get_theme(name)
    params <- theme$get_all()

    color_params <- c("background", "node_fill", "node_border", "edge_color",
                      "edge_positive_color", "edge_negative_color",
                      "label_color", "title_color", "legend_background")

    for (param in color_params) {
      if (!is.null(params[[param]])) {
        result <- tryCatch(
          grDevices::col2rgb(params[[param]]),
          error = function(e) NULL
        )
        expect_false(is.null(result),
                     info = paste("Invalid color in theme", name, "param", param, ":", params[[param]]))
      }
    }
  }
})

# =============================================================================
# Test: Theme numeric parameters
# =============================================================================

test_that("themes have valid border width", {
  themes <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")

  for (name in themes) {
    theme <- get_theme(name)
    width <- theme$get("node_border_width")
    expect_true(is.numeric(width), info = paste("Theme:", name))
    expect_true(width > 0, info = paste("Theme:", name))
  }
})

test_that("themes have valid edge width", {
  themes <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")

  for (name in themes) {
    theme <- get_theme(name)
    width <- theme$get("edge_width")
    expect_true(is.numeric(width), info = paste("Theme:", name))
    expect_true(width > 0, info = paste("Theme:", name))
  }
})

test_that("themes have valid label size", {
  themes <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")

  for (name in themes) {
    theme <- get_theme(name)
    size <- theme$get("label_size")
    expect_true(is.numeric(size), info = paste("Theme:", name))
    expect_true(size > 0, info = paste("Theme:", name))
  }
})

test_that("themes have valid title size", {
  themes <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")

  for (name in themes) {
    theme <- get_theme(name)
    size <- theme$get("title_size")
    expect_true(is.numeric(size), info = paste("Theme:", name))
    expect_true(size > 0, info = paste("Theme:", name))
  }
})

# =============================================================================
# Test: Theme chaining
# =============================================================================

test_that("CographTheme set allows chaining", {
  theme <- CographTheme$new()

  theme$set("background", "black")$set("node_fill", "white")$set("edge_color", "gray")

  expect_equal(theme$get("background"), "black")
  expect_equal(theme$get("node_fill"), "white")
  expect_equal(theme$get("edge_color"), "gray")
})

test_that("multiple merge operations create proper hierarchy", {
  base <- theme_cograph_classic()
  mid <- base$merge(list(background = "lightgray"))
  final <- mid$merge(list(node_fill = "red"))

  expect_equal(final$get("background"), "lightgray")
  expect_equal(final$get("node_fill"), "red")
  # Original should be unchanged
  expect_equal(base$get("background"), "white")
  expect_equal(base$get("node_fill"), "#4A90D9")
})

# =============================================================================
# Test: Edge cases
# =============================================================================

test_that("empty theme merge returns copy of original", {
  theme <- theme_cograph_classic()
  merged <- theme$merge(list())

  expect_equal(merged$get("background"), theme$get("background"))
  expect_equal(merged$get("node_fill"), theme$get("node_fill"))
})

test_that("theme with all parameters specified works", {
  theme <- CographTheme$new(
    name = "full_params",
    background = "white",
    node_fill = "blue",
    node_border = "navy",
    node_border_width = 2,
    edge_color = "gray",
    edge_positive_color = "green",
    edge_negative_color = "red",
    edge_width = 1.5,
    label_color = "black",
    label_size = 12,
    title_color = "darkgray",
    title_size = 16,
    legend_background = "white"
  )

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$get("node_border_width"), 2)
  expect_equal(theme$get("edge_width"), 1.5)
})

test_that("register_theme handles special characters in name", {
  custom <- CographTheme$new(name = "special_chars_test")
  register_theme("test-with-dashes", custom)

  expect_true("test-with-dashes" %in% list_themes())
})

test_that("get_theme with special characters returns registered theme", {
  custom <- CographTheme$new(name = "special_chars_test2")
  register_theme("test_with_underscores", custom)

  result <- get_theme("test_with_underscores")
  expect_s3_class(result, "CographTheme")
})

# =============================================================================
# Test: Theme consistency between creation and registry
# =============================================================================

test_that("theme_cograph_classic matches get_theme classic", {
  direct <- theme_cograph_classic()
  registry <- get_theme("classic")

  expect_equal(direct$get("background"), registry$get("background"))
  expect_equal(direct$get("node_fill"), registry$get("node_fill"))
  expect_equal(direct$get("edge_color"), registry$get("edge_color"))
})

test_that("theme_cograph_dark matches get_theme dark", {
  direct <- theme_cograph_dark()
  registry <- get_theme("dark")

  expect_equal(direct$get("background"), registry$get("background"))
  expect_equal(direct$get("label_color"), registry$get("label_color"))
})
