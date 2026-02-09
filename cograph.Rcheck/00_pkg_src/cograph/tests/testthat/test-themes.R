test_that("built-in themes exist", {
  themes <- list_themes()

  expect_true("classic" %in% themes)
  expect_true("colorblind" %in% themes)
  expect_true("gray" %in% themes)
  expect_true("dark" %in% themes)
  expect_true("minimal" %in% themes)
})

test_that("theme_cograph_classic() returns CographTheme", {
  theme <- theme_cograph_classic()

  expect_s3_class(theme, "CographTheme")
  expect_equal(theme$name, "classic")
  expect_equal(theme$get("background"), "white")
})

test_that("theme_cograph_dark() has dark background", {
  theme <- theme_cograph_dark()

  expect_equal(theme$get("background"), "#1a1a2e")
  expect_equal(theme$get("label_color"), "white")
})

test_that("CographTheme merge works", {
  theme1 <- theme_cograph_classic()
  merged <- theme1$merge(list(background = "gray90", node_fill = "orange"))

  expect_equal(merged$get("background"), "gray90")
  expect_equal(merged$get("node_fill"), "orange")
  # Original should be unchanged
  expect_equal(theme1$get("background"), "white")
})

test_that("custom theme can be registered", {
  custom <- CographTheme$new(
    name = "test_custom",
    background = "black",
    node_fill = "white"
  )
  register_theme("test_custom", custom)

  retrieved <- get_theme("test_custom")
  expect_equal(retrieved$name, "test_custom")
  expect_equal(retrieved$get("background"), "black")
})
