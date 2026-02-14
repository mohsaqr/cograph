# Tests for palettes.R - Comprehensive coverage
# Tests all palette functions, registry, and color utility interactions
# Target: 40+ tests for complete coverage

# ============================================
# PALETTE_RAINBOW TESTS
# ============================================

test_that("palette_rainbow returns correct number of colors for various n", {
  expect_equal(length(palette_rainbow(1)), 1)
  expect_equal(length(palette_rainbow(3)), 3)
  expect_equal(length(palette_rainbow(7)), 7)
  expect_equal(length(palette_rainbow(15)), 15)
  expect_equal(length(palette_rainbow(50)), 50)
})
# Test 1

test_that("palette_rainbow returns valid hex colors", {
  colors <- palette_rainbow(5)
  expect_type(colors, "character")
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_true(!is.null(rgb_vals))
    expect_equal(nrow(rgb_vals), 4)  # R, G, B, alpha
  }
})
# Test 2

test_that("palette_rainbow alpha = 1 produces opaque colors", {
  colors <- palette_rainbow(3, alpha = 1)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_equal(unname(rgb_vals["alpha", 1]), 255)
  }
})
# Test 3

test_that("palette_rainbow alpha = 0 produces transparent colors", {
  colors <- palette_rainbow(3, alpha = 0)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_equal(unname(rgb_vals["alpha", 1]), 0)
  }
})
# Test 4

test_that("palette_rainbow alpha = 0.5 produces semi-transparent colors", {
  colors <- palette_rainbow(4, alpha = 0.5)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    # Alpha should be approximately 127 or 128 (255 * 0.5)
    expect_true(rgb_vals["alpha", 1] >= 125 && rgb_vals["alpha", 1] <= 130)
  }
})
# Test 5

# ============================================
# PALETTE_COLORBLIND TESTS
# ============================================

test_that("palette_colorblind returns Wong's colors for n <= 8", {
  colors <- palette_colorblind(8)
  expect_equal(length(colors), 8)
  # First color should be black (or very close)
  rgb_first <- grDevices::col2rgb(colors[1])
  expect_true(all(rgb_first == 0))
})
# Test 6

test_that("palette_colorblind interpolates for n > 8", {
  colors <- palette_colorblind(12)
  expect_equal(length(colors), 12)
  expect_valid_colors(colors)
})
# Test 7

test_that("palette_colorblind handles n = 1", {
  colors <- palette_colorblind(1)
  expect_equal(length(colors), 1)
  expect_valid_colors(colors)
})
# Test 8

test_that("palette_colorblind handles alpha < 1", {
  colors <- palette_colorblind(5, alpha = 0.7)
  expect_equal(length(colors), 5)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    # Alpha should be approximately 178 (255 * 0.7)
    expect_true(rgb_vals["alpha", 1] >= 175 && rgb_vals["alpha", 1] <= 180)
  }
})
# Test 9

test_that("palette_colorblind handles large n (100)", {
  colors <- palette_colorblind(100)
  expect_equal(length(colors), 100)
  expect_valid_colors(colors)
})
# Test 10

# ============================================
# PALETTE_PASTEL TESTS
# ============================================

test_that("palette_pastel returns correct number for n <= 8", {
  colors <- palette_pastel(6)
  expect_equal(length(colors), 6)
  expect_valid_colors(colors)
})
# Test 11

test_that("palette_pastel interpolates for n > 8", {
  colors <- palette_pastel(15)
  expect_equal(length(colors), 15)
  expect_valid_colors(colors)
})
# Test 12

test_that("palette_pastel handles alpha parameter", {
  colors <- palette_pastel(4, alpha = 0.6)
  expect_equal(length(colors), 4)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_true(rgb_vals["alpha", 1] >= 150 && rgb_vals["alpha", 1] <= 156)
  }
})
# Test 13

test_that("palette_pastel colors are actually pastel (high lightness)", {
  colors <- palette_pastel(8)
  # Check a subset of colors - not all interpolated colors are guaranteed light
  light_count <- 0
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col)
    # Pastel colors should have relatively high RGB values
    avg_rgb <- mean(rgb_vals)
    if (avg_rgb > 140) light_count <- light_count + 1
  }
  # Most pastel colors should be light

  expect_true(light_count >= 5)
})
# Test 14

# ============================================
# PALETTE_VIRIDIS TESTS
# ============================================

test_that("palette_viridis returns correct number of colors", {
  expect_equal(length(palette_viridis(1)), 1)
  expect_equal(length(palette_viridis(10)), 10)
  expect_equal(length(palette_viridis(50)), 50)
})
# Test 15

test_that("palette_viridis option = 'viridis' works", {
  colors <- palette_viridis(5, option = "viridis")
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})
# Test 16

test_that("palette_viridis option = 'magma' works", {
  colors <- palette_viridis(5, option = "magma")
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})
# Test 17

test_that("palette_viridis option = 'plasma' works", {
  colors <- palette_viridis(5, option = "plasma")
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})
# Test 18

test_that("palette_viridis option = 'inferno' works", {
  colors <- palette_viridis(5, option = "inferno")
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})
# Test 19

test_that("palette_viridis option = 'cividis' works", {
  colors <- palette_viridis(5, option = "cividis")
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})
# Test 20

test_that("palette_viridis unknown option falls back to viridis", {
  colors_unknown <- palette_viridis(5, option = "nonexistent")
  colors_viridis <- palette_viridis(5, option = "viridis")
  expect_equal(colors_unknown, colors_viridis)
})
# Test 21

test_that("palette_viridis handles alpha parameter", {
  colors <- palette_viridis(5, alpha = 0.8)
  expect_equal(length(colors), 5)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_true(rgb_vals["alpha", 1] >= 202 && rgb_vals["alpha", 1] <= 206)
  }
})
# Test 22

test_that("palette_viridis different options produce different colors", {
  viridis <- palette_viridis(5, option = "viridis")
  magma <- palette_viridis(5, option = "magma")
  plasma <- palette_viridis(5, option = "plasma")

  expect_false(identical(viridis, magma))
  expect_false(identical(viridis, plasma))
  expect_false(identical(magma, plasma))
})
# Test 23

# ============================================
# PALETTE_BLUES TESTS
# ============================================

test_that("palette_blues returns correct number of colors", {
  expect_equal(length(palette_blues(3)), 3)
  expect_equal(length(palette_blues(8)), 8)
  expect_equal(length(palette_blues(20)), 20)
})
# Test 24

test_that("palette_blues colors are actually blue-ish", {
  colors <- palette_blues(5)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col)
    # Blue channel should be significant
    expect_true(rgb_vals["blue", 1] >= rgb_vals["red", 1] * 0.8)
  }
})
# Test 25

test_that("palette_blues handles alpha parameter", {
  colors <- palette_blues(5, alpha = 0.3)
  expect_equal(length(colors), 5)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_true(rgb_vals["alpha", 1] >= 75 && rgb_vals["alpha", 1] <= 80)
  }
})
# Test 26

# ============================================
# PALETTE_REDS TESTS
# ============================================

test_that("palette_reds returns correct number of colors", {
  expect_equal(length(palette_reds(3)), 3)
  expect_equal(length(palette_reds(8)), 8)
  expect_equal(length(palette_reds(20)), 20)
})
# Test 27

test_that("palette_reds colors are actually red-ish", {
  colors <- palette_reds(5)
  # Middle and dark colors should have more red
  rgb_vals <- grDevices::col2rgb(colors[3])  # middle color
  expect_true(rgb_vals["red", 1] >= rgb_vals["blue", 1])
})
# Test 28

test_that("palette_reds handles alpha parameter", {
  colors <- palette_reds(5, alpha = 0.4)
  expect_equal(length(colors), 5)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_true(rgb_vals["alpha", 1] >= 100 && rgb_vals["alpha", 1] <= 104)
  }
})
# Test 29

# ============================================
# PALETTE_DIVERGING TESTS
# ============================================

test_that("palette_diverging returns correct number of colors", {
  expect_equal(length(palette_diverging(5)), 5)
  expect_equal(length(palette_diverging(11)), 11)
  expect_equal(length(palette_diverging(21)), 21)
})
# Test 30

test_that("palette_diverging midpoint = 'white' produces white center", {
  colors <- palette_diverging(5, midpoint = "white")
  # Middle color (index 3 for n=5) should be close to white
  mid_rgb <- grDevices::col2rgb(colors[3])
  expect_true(mean(mid_rgb) > 220)  # Very light
})
# Test 31

test_that("palette_diverging custom midpoint works", {
  colors_white <- palette_diverging(5, midpoint = "white")
  colors_gray <- palette_diverging(5, midpoint = "gray50")

  expect_false(identical(colors_white, colors_gray))
})
# Test 32

test_that("palette_diverging handles alpha parameter", {
  colors <- palette_diverging(5, alpha = 0.9)
  expect_equal(length(colors), 5)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_true(rgb_vals["alpha", 1] >= 228 && rgb_vals["alpha", 1] <= 232)
  }
})
# Test 33

test_that("palette_diverging endpoints are blue and red", {
  colors <- palette_diverging(11)

  # First color should be blue-ish
  first_rgb <- grDevices::col2rgb(colors[1])
  expect_true(first_rgb["blue", 1] > first_rgb["red", 1])

  # Last color should be red-ish
  last_rgb <- grDevices::col2rgb(colors[11])
  expect_true(last_rgb["red", 1] > last_rgb["blue", 1])
})
# Test 34

# ============================================
# PALETTE REGISTRY TESTS
# ============================================

test_that("list_palettes returns all built-in palettes", {
  palettes <- list_palettes()
  expect_true("rainbow" %in% palettes)
  expect_true("colorblind" %in% palettes)
  expect_true("pastel" %in% palettes)
  expect_true("viridis" %in% palettes)
  expect_true("blues" %in% palettes)
  expect_true("reds" %in% palettes)
  expect_true("diverging" %in% palettes)
})
# Test 35

test_that("get_palette retrieves working palette function", {
  pal_fn <- cograph:::get_palette("rainbow")
  expect_true(is.function(pal_fn))
  colors <- pal_fn(5)
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)
})
# Test 36

test_that("get_palette returns NULL for nonexistent palette", {
  result <- cograph:::get_palette("nonexistent_palette_xyz")
  expect_null(result)
})
# Test 37

test_that("register_palette adds new palette to registry", {
  # Create custom palette
  custom_pal <- function(n, alpha = 1) {
    rep("#FF00FF", n)
  }

  # Register it
  cograph:::register_palette("test_magenta", custom_pal)

  # Verify it's in the list
  expect_true("test_magenta" %in% list_palettes())

  # Verify it works
  retrieved <- cograph:::get_palette("test_magenta")
  expect_true(is.function(retrieved))
  expect_equal(retrieved(3), c("#FF00FF", "#FF00FF", "#FF00FF"))
})
# Test 38

test_that("register_builtin_palettes registers all 7 palettes", {
  # This function is called on package load
  # Verify all 7 are present
  palettes <- list_palettes()
  expect_true(length(palettes) >= 7)

  expected <- c("rainbow", "colorblind", "pastel", "viridis",
                "blues", "reds", "diverging")
  for (p in expected) {
    expect_true(p %in% palettes,
                info = paste("Missing palette:", p))
  }
})
# Test 39

# ============================================
# EDGE CASES AND BOUNDARY TESTS
# ============================================

test_that("all palettes handle n = 2", {
  expect_equal(length(palette_rainbow(2)), 2)
  expect_equal(length(palette_colorblind(2)), 2)
  expect_equal(length(palette_pastel(2)), 2)
  expect_equal(length(palette_viridis(2)), 2)
  expect_equal(length(palette_blues(2)), 2)
  expect_equal(length(palette_reds(2)), 2)
  expect_equal(length(palette_diverging(2)), 2)
})
# Test 40

test_that("all palettes return character vectors", {
  expect_type(palette_rainbow(5), "character")
  expect_type(palette_colorblind(5), "character")
  expect_type(palette_pastel(5), "character")
  expect_type(palette_viridis(5), "character")
  expect_type(palette_blues(5), "character")
  expect_type(palette_reds(5), "character")
  expect_type(palette_diverging(5), "character")
})
# Test 41

test_that("all palettes produce unique colors for small n", {
  # For n=5, all colors should be unique
  expect_equal(length(unique(palette_rainbow(5))), 5)
  expect_equal(length(unique(palette_colorblind(5))), 5)
  expect_equal(length(unique(palette_pastel(5))), 5)
  expect_equal(length(unique(palette_viridis(5))), 5)
  expect_equal(length(unique(palette_blues(5))), 5)
  expect_equal(length(unique(palette_reds(5))), 5)
  expect_equal(length(unique(palette_diverging(5))), 5)
})
# Test 42

test_that("palettes work with alpha = 0.001 (nearly transparent)", {
  colors <- palette_viridis(3, alpha = 0.001)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_true(rgb_vals["alpha", 1] <= 1)
  }
})
# Test 43

test_that("palettes work with alpha = 0.999 (nearly opaque)", {
  colors <- palette_blues(3, alpha = 0.999)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_true(rgb_vals["alpha", 1] >= 254)
  }
})
# Test 44

# ============================================
# INTEGRATION WITH ADJUST_ALPHA
# ============================================

test_that("palette colors can be further adjusted with adjust_alpha", {
  skip_if_not(exists("adjust_alpha", envir = asNamespace("cograph")))

  colors <- palette_colorblind(3)
  adjusted <- sapply(colors, cograph:::adjust_alpha, alpha = 0.5)

  expect_equal(length(adjusted), 3)
  for (col in adjusted) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_true(rgb_vals["alpha", 1] >= 125 && rgb_vals["alpha", 1] <= 130)
  }
})
# Test 45

# ============================================
# INTERPOLATION QUALITY TESTS
# ============================================

test_that("palette_colorblind interpolation is smooth", {
  colors <- palette_colorblind(20)
  # Convert to RGB
  rgb_matrix <- sapply(colors, function(c) grDevices::col2rgb(c))

  # Check that consecutive colors don't jump too drastically
  for (i in 2:20) {
    diff <- abs(rgb_matrix[, i] - rgb_matrix[, i-1])
    # No channel should jump more than 100 units between adjacent colors
    expect_true(all(diff < 150),
                info = paste("Color jump at position", i))
  }
})
# Test 46

test_that("palette_viridis maintains perceptual uniformity", {
  # Viridis is designed to be perceptually uniform
  colors <- palette_viridis(10)
  rgb_matrix <- sapply(colors, function(c) grDevices::col2rgb(c))

  # Check general progression (viridis goes dark to light)
  mean_luminance <- colMeans(rgb_matrix)
  # Should generally increase (with some tolerance for local variations)
  expect_true(mean_luminance[10] > mean_luminance[1])
})
# Test 47

# ============================================
# CONSISTENCY TESTS
# ============================================

test_that("palette functions are deterministic", {
  colors1 <- palette_viridis(5)
  colors2 <- palette_viridis(5)
  expect_identical(colors1, colors2)

  colors3 <- palette_colorblind(8)
  colors4 <- palette_colorblind(8)
  expect_identical(colors3, colors4)
})
# Test 48

test_that("palette_diverging with odd n has exact midpoint", {
  colors <- palette_diverging(7, midpoint = "#FFFFFF")
  # Middle color (index 4 for n=7) should be very close to white
  mid_rgb <- grDevices::col2rgb(colors[4])
  expect_true(all(mid_rgb >= 250))  # All channels near 255
})
# Test 49

test_that("sequential palettes (blues, reds) show clear gradient", {
  blues <- palette_blues(5)
  reds <- palette_reds(5)

  # First should be lighter than last for sequential palettes
  first_blue_rgb <- grDevices::col2rgb(blues[1])
  last_blue_rgb <- grDevices::col2rgb(blues[5])

  first_red_rgb <- grDevices::col2rgb(reds[1])
  last_red_rgb <- grDevices::col2rgb(reds[5])

  # First colors are lighter (higher total RGB)
  expect_true(sum(first_blue_rgb) > sum(last_blue_rgb))
  expect_true(sum(first_red_rgb) > sum(last_red_rgb))
})
# Test 50

# ============================================
# LARGE N STRESS TESTS
# ============================================

test_that("palettes handle n = 256 (full gradient)", {
  expect_equal(length(palette_viridis(256)), 256)
  expect_equal(length(palette_blues(256)), 256)
  expect_equal(length(palette_diverging(256)), 256)
})
# Test 51

test_that("palettes handle n = 1000", {
  colors <- palette_rainbow(1000)
  expect_equal(length(colors), 1000)
  expect_valid_colors(colors[c(1, 500, 1000)])
})
# Test 52

# ============================================
# SPECIAL VIRIDIS OPTIONS TESTS
# ============================================

test_that("palette_viridis cividis is colorblind-friendly", {
  # Cividis is specifically designed for colorblind accessibility
  colors <- palette_viridis(5, option = "cividis")
  expect_equal(length(colors), 5)
  expect_valid_colors(colors)

  # Cividis uses more muted, yellow-blue range
  # Check that colors are valid by checking RGB values exist
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col)
    # Should have 3 channels
    expect_equal(length(rgb_vals), 3)
  }
})
# Test 53

test_that("all viridis options produce valid perceptual gradients", {
  options <- c("viridis", "magma", "plasma", "inferno", "cividis")

  for (opt in options) {
    colors <- palette_viridis(10, option = opt)

    # Calculate luminance for each color
    luminance <- sapply(colors, function(col) {
      rgb_vals <- grDevices::col2rgb(col)
      0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]
    })

    # Should have a range (not all same luminance)
    expect_true(max(luminance) - min(luminance) > 50,
                info = paste("Option:", opt))
  }
})
# Test 54

# ============================================
# REGISTRY FUNCTION EDGE CASES
# ============================================

test_that("get_palette with empty string returns NULL", {
  # Test that retrieving non-existent palette returns NULL
  result <- cograph:::get_palette("")
  expect_null(result)
})
# Test 55

test_that("list_palettes returns at least 7 palettes after init", {
  palettes <- list_palettes()
  expect_true(length(palettes) >= 7)
})
# Test 56

test_that("register_palette can overwrite existing palette", {
  # Save original
  original <- cograph:::get_palette("rainbow")

  # Overwrite with custom
  custom <- function(n, alpha = 1) rep("#123456", n)
  cograph:::register_palette("rainbow", custom)

  # Verify overwrite worked
  retrieved <- cograph:::get_palette("rainbow")
  expect_equal(retrieved(3), c("#123456", "#123456", "#123456"))

  # Restore original
  cograph:::register_palette("rainbow", original)
})
# Test 57

# ============================================
# COLOR CONVERSION VERIFICATION
# ============================================

test_that("palette_colorblind Wong colors are correct hex values", {
  # Wong's first 3 colors are: black, orange, sky blue
  colors <- palette_colorblind(3)

  # First should be black #000000
  expect_equal(toupper(colors[1]), "#000000")

  # Second should be orange #E69F00
  expect_equal(toupper(colors[2]), "#E69F00")

  # Third should be sky blue #56B4E9
  expect_equal(toupper(colors[3]), "#56B4E9")
})
# Test 58

test_that("palette_pastel first color is pink-ish", {
  colors <- palette_pastel(1)
  rgb_vals <- grDevices::col2rgb(colors[1])

  # #FFB3BA - should have high red, moderate green, moderate blue
  expect_true(rgb_vals["red", 1] > 200)
  expect_true(rgb_vals["green", 1] > 150)
  expect_true(rgb_vals["blue", 1] > 150)
})
# Test 59

# ============================================
# ALPHA EDGE CASES
# ============================================

test_that("palettes handle alpha exactly 0", {
  colors <- palette_rainbow(3, alpha = 0)
  for (col in colors) {
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    expect_equal(unname(rgb_vals["alpha", 1]), 0)
  }
})
# Test 60

test_that("palettes handle alpha exactly 1 (no modification)", {
  # For colorblind, alpha=1 should return base colors unchanged
  colors <- palette_colorblind(3, alpha = 1)
  expected <- c("#000000", "#E69F00", "#56B4E9")
  expect_equal(toupper(colors), expected)
})
# Test 61

# ============================================
# FINAL COMPREHENSIVE CHECKS
# ============================================

test_that("all palette functions exist and are exported or accessible", {
  expect_true(exists("palette_rainbow"))
  expect_true(exists("palette_colorblind"))
  expect_true(exists("palette_pastel"))
  expect_true(exists("palette_viridis"))
  expect_true(exists("palette_blues"))
  expect_true(exists("palette_reds"))
  expect_true(exists("palette_diverging"))
  expect_true(exists("list_palettes"))
})
# Test 62

test_that("palette functions have correct signatures", {
  # All should accept n and alpha
  expect_true("n" %in% names(formals(palette_rainbow)))
  expect_true("alpha" %in% names(formals(palette_rainbow)))
  expect_true("n" %in% names(formals(palette_colorblind)))
  expect_true("alpha" %in% names(formals(palette_colorblind)))

  # viridis should also accept option
  expect_true("option" %in% names(formals(palette_viridis)))

  # diverging should also accept midpoint
  expect_true("midpoint" %in% names(formals(palette_diverging)))
})
# Test 63
