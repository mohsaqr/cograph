# =============================================================================
# Test Coverage for themes-registry.R (Test Suite 41)
# =============================================================================
# Comprehensive tests for register_builtin_themes function, theme registration,
# retrieval, built-in themes, theme application, and CographTheme integration

# =============================================================================
# Test 1-8: register_builtin_themes function - All built-in themes
# =============================================================================

test_that("register_builtin_themes registers classic theme correctly", {
  # Test that classic theme is registered with correct properties
  expect_true("classic" %in% list_themes())
  theme <- get_theme("classic")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "classic")
  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("node_fill"), "#4A90D9")
  expect_equal(theme$get("node_border"), "#2C5AA0")
  expect_equal(theme$get("node_border_width"), 1.5)
})

test_that("register_builtin_themes registers colorblind theme correctly", {
  expect_true("colorblind" %in% list_themes())
  theme <- get_theme("colorblind")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "colorblind")
  expect_equal(theme$get("node_fill"), "#0072B2")
  expect_equal(theme$get("node_border"), "#004C7F")
  expect_equal(theme$get("edge_positive_color"), "#0000FF")
  expect_equal(theme$get("edge_negative_color"), "#FF0000")
})

test_that("register_builtin_themes registers gray theme correctly", {
  expect_true("gray" %in% list_themes())
  theme <- get_theme("gray")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "gray")
  expect_equal(theme$get("node_fill"), "gray70")
  expect_equal(theme$get("node_border"), "gray30")
  expect_equal(theme$get("edge_positive_color"), "gray20")
  expect_equal(theme$get("edge_negative_color"), "gray60")
})

test_that("register_builtin_themes registers grey alias correctly", {
  expect_true("grey" %in% list_themes())
  theme_gray <- get_theme("gray")
  theme_grey <- get_theme("grey")

  # Both should return equivalent themes
  expect_equal(theme_gray$get("background"), theme_grey$get("background"))
  expect_equal(theme_gray$get("node_fill"), theme_grey$get("node_fill"))
  expect_equal(theme_gray$get("node_border"), theme_grey$get("node_border"))
  expect_equal(theme_gray$get("edge_color"), theme_grey$get("edge_color"))
})

test_that("register_builtin_themes registers dark theme correctly", {
  expect_true("dark" %in% list_themes())
  theme <- get_theme("dark")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "dark")
  expect_equal(theme$get("background"), "#1a1a2e")
  expect_equal(theme$get("node_fill"), "#e94560")
  expect_equal(theme$get("node_border"), "#ff6b6b")
  expect_equal(theme$get("label_color"), "white")
  expect_equal(theme$get("title_color"), "white")
})

test_that("register_builtin_themes registers minimal theme correctly", {
  expect_true("minimal" %in% list_themes())
  theme <- get_theme("minimal")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "minimal")
  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("node_fill"), "white")
  expect_equal(theme$get("node_border"), "gray40")
  expect_equal(theme$get("node_border_width"), 0.75)
  expect_equal(theme$get("edge_width"), 0.5)
})

test_that("register_builtin_themes registers viridis theme correctly", {
  expect_true("viridis" %in% list_themes())
  theme <- get_theme("viridis")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "viridis")
  expect_equal(theme$get("node_fill"), "#21918c")
  expect_equal(theme$get("node_border"), "#31688e")
  expect_equal(theme$get("edge_positive_color"), "#5ec962")
  expect_equal(theme$get("edge_negative_color"), "#b5367a")
})

test_that("register_builtin_themes registers nature theme correctly", {
  expect_true("nature" %in% list_themes())
  theme <- get_theme("nature")
  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "nature")
  expect_equal(theme$get("background"), "#fefae0")
  expect_equal(theme$get("node_fill"), "#606c38")
  expect_equal(theme$get("node_border"), "#283618")
  expect_equal(theme$get("edge_color"), "#bc6c25")
  expect_equal(theme$get("legend_background"), "#fefae0")
})

# =============================================================================
# Test 9-14: list_themes function
# =============================================================================

test_that("list_themes returns character vector", {
  themes <- list_themes()
  expect_type(themes, "character")
  expect_true(length(themes) >= 8)
})

test_that("list_themes contains all expected built-in theme names", {
  themes <- list_themes()
  expected <- c("classic", "colorblind", "gray", "grey", "dark", "minimal", "viridis", "nature")

  for (name in expected) {
    expect_true(name %in% themes, info = paste("Missing theme:", name))
  }
})

test_that("list_themes returns unique names", {
  themes <- list_themes()
  expect_equal(length(themes), length(unique(themes)))
})

test_that("list_themes returns names without NAs", {
  themes <- list_themes()
  expect_false(any(is.na(themes)))
})

test_that("list_themes returns names without empty strings", {
  themes <- list_themes()
  expect_false(any(themes == ""))
})

test_that("list_themes is idempotent (returns same result on multiple calls)", {
  themes1 <- list_themes()
  themes2 <- list_themes()
  expect_equal(sort(themes1), sort(themes2))
})

# =============================================================================
# Test 15-20: get_theme function
# =============================================================================

test_that("get_theme returns CographTheme for valid theme name", {
  theme <- get_theme("classic")
  expect_s3_class(theme, "CographTheme")
})

test_that("get_theme returns NULL for nonexistent theme", {
  result <- get_theme("nonexistent_theme_xyz123")
  expect_null(result)
})

test_that("get_theme returns NULL for empty string",
{
  result <- get_theme("")
  expect_null(result)
})

test_that("get_theme handles case sensitivity correctly", {
  # Should return NULL for wrong case
  result <- get_theme("Classic")
  expect_null(result)

  result2 <- get_theme("DARK")
  expect_null(result2)
})

test_that("get_theme returns different objects for different themes", {
  classic <- get_theme("classic")
  dark <- get_theme("dark")

  expect_false(identical(classic$get("background"), dark$get("background")))
})

test_that("get_theme returns consistent theme properties across calls", {
  theme1 <- get_theme("viridis")
  theme2 <- get_theme("viridis")

  expect_equal(theme1$get("node_fill"), theme2$get("node_fill"))
  expect_equal(theme1$get("background"), theme2$get("background"))
})

# =============================================================================
# Test 21-27: register_theme function
# =============================================================================

test_that("register_theme can register new CographTheme", {
  custom <- CographTheme$new(name = "test_reg_41_a", background = "cyan")
  result <- register_theme("test_reg_41_a", custom)

  expect_null(result)  # Should return invisible NULL
  expect_true("test_reg_41_a" %in% list_themes())

  retrieved <- get_theme("test_reg_41_a")
  expect_equal(retrieved$name, "test_reg_41_a")
  expect_equal(retrieved$get("background"), "cyan")
})

test_that("register_theme can register theme as list", {
  custom_list <- list(
    background = "magenta",
    node_fill = "yellow"
  )
  register_theme("test_reg_41_b", custom_list)

  expect_true("test_reg_41_b" %in% list_themes())
  retrieved <- get_theme("test_reg_41_b")
  expect_equal(retrieved$background, "magenta")
  expect_equal(retrieved$node_fill, "yellow")
})

test_that("register_theme can overwrite existing theme", {
  custom1 <- CographTheme$new(name = "test_overwrite_41", background = "red")
  register_theme("test_overwrite_41", custom1)

  custom2 <- CographTheme$new(name = "test_overwrite_41", background = "green")
  register_theme("test_overwrite_41", custom2)

  retrieved <- get_theme("test_overwrite_41")
  expect_equal(retrieved$get("background"), "green")
})

test_that("register_theme handles theme names with special characters", {
  custom <- CographTheme$new(name = "special_test")
  register_theme("test-dashes-41", custom)
  register_theme("test_underscore_41", custom)
  register_theme("test.dots.41", custom)

  expect_true("test-dashes-41" %in% list_themes())
  expect_true("test_underscore_41" %in% list_themes())
  expect_true("test.dots.41" %in% list_themes())
})

test_that("register_theme handles numeric theme name", {
  custom <- CographTheme$new(name = "numeric_test")
  register_theme("theme123", custom)

  expect_true("theme123" %in% list_themes())
})

test_that("register_theme returns invisible NULL", {
  custom <- CographTheme$new(name = "test_invisible_41")
  result <- register_theme("test_invisible_41", custom)

  expect_null(result)
})

test_that("register_theme does not modify the original theme object", {
  custom <- CographTheme$new(name = "test_preserve_41", background = "purple")
  original_bg <- custom$get("background")

  register_theme("test_preserve_41", custom)

  expect_equal(custom$get("background"), original_bg)
})

# =============================================================================
# Test 28-35: theme_cograph_* functions
# =============================================================================

test_that("theme_cograph_classic creates valid theme with all properties", {
  theme <- theme_cograph_classic()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "classic")
  expect_equal(theme$get("edge_color"), "gray50")
  expect_equal(theme$get("edge_positive_color"), "#2E7D32")
  expect_equal(theme$get("edge_negative_color"), "#C62828")
  expect_equal(theme$get("label_color"), "black")
  expect_equal(theme$get("label_size"), 10)
  expect_equal(theme$get("title_size"), 14)
})

test_that("theme_cograph_colorblind creates accessible theme", {
  theme <- theme_cograph_colorblind()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("label_color"), "black")
  expect_equal(theme$get("legend_background"), "white")
})

test_that("theme_cograph_gray creates grayscale theme", {
  theme <- theme_cograph_gray()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$get("edge_color"), "gray50")
  expect_equal(theme$get("label_size"), 10)
  expect_equal(theme$get("title_size"), 14)
})

test_that("theme_cograph_dark creates dark theme with light text", {
  theme <- theme_cograph_dark()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$get("edge_color"), "gray60")
  expect_equal(theme$get("edge_positive_color"), "#4ecca3")
  expect_equal(theme$get("edge_negative_color"), "#fc5185")
  expect_equal(theme$get("legend_background"), "#1a1a2e")
})

test_that("theme_cograph_minimal creates thin-line theme", {
  theme <- theme_cograph_minimal()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$get("edge_color"), "gray70")
  expect_equal(theme$get("edge_positive_color"), "gray40")
  expect_equal(theme$get("edge_negative_color"), "gray40")
  expect_equal(theme$get("label_color"), "gray30")
  expect_equal(theme$get("label_size"), 9)
  expect_equal(theme$get("title_color"), "gray20")
  expect_equal(theme$get("title_size"), 12)
})

test_that("theme_cograph_viridis creates viridis-inspired theme", {
  theme <- theme_cograph_viridis()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$get("edge_color"), "gray50")
  expect_equal(theme$get("label_color"), "black")
  expect_equal(theme$get("title_color"), "black")
})

test_that("theme_cograph_nature creates earth-tone theme", {
  theme <- theme_cograph_nature()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$get("edge_positive_color"), "#606c38")
  expect_equal(theme$get("edge_negative_color"), "#9b2226")
  expect_equal(theme$get("label_color"), "#283618")
  expect_equal(theme$get("title_color"), "#283618")
})

test_that("all theme_cograph_* functions return independent objects", {
  themes <- list(
    theme_cograph_classic(),
    theme_cograph_colorblind(),
    theme_cograph_gray(),
    theme_cograph_dark(),
    theme_cograph_minimal(),
    theme_cograph_viridis(),
    theme_cograph_nature()
  )

  # Modify first theme, verify others unchanged
  original_bg <- themes[[2]]$get("background")
  themes[[1]]$set("background", "modified_test")

  expect_equal(themes[[2]]$get("background"), original_bg)
})

# =============================================================================
# Test 36-42: Theme color validation
# =============================================================================

test_that("classic theme has all valid R colors", {
  theme <- theme_cograph_classic()
  params <- theme$get_all()

  color_params <- c("background", "node_fill", "node_border", "edge_color",
                    "edge_positive_color", "edge_negative_color",
                    "label_color", "title_color", "legend_background")

  for (param in color_params) {
    result <- tryCatch(grDevices::col2rgb(params[[param]]), error = function(e) NULL)
    expect_false(is.null(result), info = paste("Invalid color:", param, "=", params[[param]]))
  }
})

test_that("dark theme has proper contrast", {
  theme <- theme_cograph_dark()

  bg <- theme$get("background")
  label <- theme$get("label_color")

  bg_rgb <- grDevices::col2rgb(bg)
  label_rgb <- grDevices::col2rgb(label)

  bg_luminance <- sum(bg_rgb) / 3
  label_luminance <- sum(label_rgb) / 3

  # Dark background with light text
  expect_true(bg_luminance < 100, info = "Background should be dark")
  expect_true(label_luminance > 200, info = "Label should be light")
})

test_that("colorblind theme uses blue-red distinction", {
  theme <- theme_cograph_colorblind()

  pos_color <- theme$get("edge_positive_color")
  neg_color <- theme$get("edge_negative_color")

  # Blue for positive
  pos_rgb <- grDevices::col2rgb(pos_color)
  expect_true(pos_rgb["blue", 1] > pos_rgb["red", 1])

  # Red for negative
  neg_rgb <- grDevices::col2rgb(neg_color)
  expect_true(neg_rgb["red", 1] > neg_rgb["blue", 1])
})

test_that("minimal theme has muted colors", {
  theme <- theme_cograph_minimal()

  # Node fill should be white (for minimal look)
  expect_equal(theme$get("node_fill"), "white")

  # Edge colors should be gray
  edge_color <- theme$get("edge_color")
  expect_true(grepl("gray", edge_color))
})

test_that("nature theme uses earth tones", {
  theme <- theme_cograph_nature()

  # Check that background is cream/beige colored
  bg <- theme$get("background")
  bg_rgb <- grDevices::col2rgb(bg)

  # Cream colors have high red/green, lower blue
  expect_true(bg_rgb["red", 1] > 200)
  expect_true(bg_rgb["green", 1] > 200)
})

test_that("viridis theme uses teal-green spectrum", {
  theme <- theme_cograph_viridis()

  node_fill <- theme$get("node_fill")
  node_rgb <- grDevices::col2rgb(node_fill)

  # Teal has similar green and blue, low red
  expect_true(node_rgb["red", 1] < 100)
  expect_true(node_rgb["green", 1] > 100)
  expect_true(node_rgb["blue", 1] > 100)
})

test_that("all themes have edge_width defined and positive", {
  theme_names <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")

  for (name in theme_names) {
    theme <- get_theme(name)
    width <- theme$get("edge_width")
    expect_true(is.numeric(width), info = paste(name, "edge_width not numeric"))
    expect_true(width > 0, info = paste(name, "edge_width not positive"))
  }
})

# =============================================================================
# Test 43-48: Theme application in plotting
# =============================================================================

test_that("splot accepts all built-in theme names", {
  adj <- create_test_matrix(4)

  themes <- c("classic", "colorblind", "gray", "grey", "dark", "minimal", "viridis", "nature")

  for (theme_name in themes) {
    result <- safe_plot(splot(adj, theme = theme_name))
    expect_true(result$success, info = paste("Theme", theme_name, "failed:", result$error))
  }
})

test_that("sn_theme applies theme to cograph_network", {
  adj <- create_test_matrix(4)
  net <- cograph(adj) |> sn_theme("dark")

  expect_s3_class(net$theme, "CographTheme")
  expect_equal(net$theme$name, "dark")
  expect_equal(net$theme$get("background"), "#1a1a2e")
})

test_that("sn_theme accepts CographTheme object directly", {
  adj <- create_test_matrix(4)
  custom <- CographTheme$new(name = "direct_41", background = "orange")

  net <- cograph(adj) |> sn_theme(custom)

  expect_equal(net$theme$get("background"), "orange")
})

test_that("sn_theme accepts override parameters", {
  adj <- create_test_matrix(4)

  net <- cograph(adj) |> sn_theme("classic", background = "lavender", node_fill = "coral")

  expect_equal(net$theme$get("background"), "lavender")
  expect_equal(net$theme$get("node_fill"), "coral")
  # Original classic properties should be preserved
  expect_equal(net$theme$get("node_border"), "#2C5AA0")
})

test_that("sn_theme errors on unknown theme name", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(sn_theme(net, "unknown_xyz_41"))
})

test_that("sn_theme errors on invalid theme type", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(sn_theme(net, 12345))
  expect_error(sn_theme(net, list(not_a_theme = TRUE)))
})

# =============================================================================
# Test 49-52: CographTheme class methods (tested through themes-registry)
# =============================================================================

test_that("CographTheme merge works with registry themes", {
  base <- get_theme("classic")
  merged <- base$merge(list(background = "salmon", edge_width = 2.5))

  expect_equal(merged$get("background"), "salmon")
  expect_equal(merged$get("edge_width"), 2.5)
  # Original unmodified
  expect_equal(base$get("background"), "white")
})

test_that("CographTheme clone_theme creates independent copy from registry", {
  original <- get_theme("viridis")
  cloned <- original$clone_theme()

  cloned$set("node_fill", "pink")

  expect_equal(original$get("node_fill"), "#21918c")
  expect_equal(cloned$get("node_fill"), "pink")
})

test_that("CographTheme get_all returns complete parameter list from registry theme", {
  theme <- get_theme("nature")
  params <- theme$get_all()

  expect_type(params, "list")
  expect_true(length(params) >= 12)

  required <- c("background", "node_fill", "node_border", "node_border_width",
                "edge_color", "edge_positive_color", "edge_negative_color",
                "edge_width", "label_color", "label_size", "title_color", "title_size")

  for (param in required) {
    expect_true(param %in% names(params), info = paste("Missing param:", param))
  }
})

test_that("CographTheme print method works for registry themes", {
  theme <- get_theme("dark")

  output <- capture.output(theme$print())

  expect_true(any(grepl("CographTheme", output)))
  expect_true(any(grepl("dark", output)))
  expect_true(any(grepl("Background", output)))
})

# =============================================================================
# Test 53-55: Registry state after registration
# =============================================================================

test_that("registering new theme increases theme count", {
  before_count <- length(list_themes())

  unique_name <- paste0("test_count_", format(Sys.time(), "%H%M%S"))
  register_theme(unique_name, CographTheme$new(name = unique_name))

  after_count <- length(list_themes())
  expect_equal(after_count, before_count + 1)
})

test_that("registry persists themes across function calls", {
  unique_name <- paste0("test_persist_", format(Sys.time(), "%H%M%S"))
  register_theme(unique_name, CographTheme$new(name = unique_name, background = "teal"))

  # Multiple retrieval calls
  theme1 <- get_theme(unique_name)
  theme2 <- get_theme(unique_name)
  theme3 <- get_theme(unique_name)

  expect_s3_class(theme1, "CographTheme")
  expect_s3_class(theme2, "CographTheme")
  expect_s3_class(theme3, "CographTheme")
  expect_equal(theme1$get("background"), "teal")
})

test_that("built-in themes remain available after custom registrations", {
  # Register several custom themes
  for (i in 1:5) {
    register_theme(paste0("custom_flood_", i),
                   CographTheme$new(name = paste0("flood_", i)))
  }

  # Verify built-ins still work
  builtin_names <- c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")
  for (name in builtin_names) {
    theme <- get_theme(name)
    expect_s3_class(theme, "CographTheme")
    expect_true(!is.null(theme), info = paste("Built-in", name, "should exist"))
  }
})
