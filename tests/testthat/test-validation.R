# test-validation.R - Input Validation and Error Handling Tests
# Tests for proper error messages and validation

# ============================================
# MATRIX VALIDATION
# ============================================

test_that("sonnet() errors on non-square matrix", {
  non_square <- matrix(1:6, nrow = 2, ncol = 3)

  expect_error(sonnet(non_square), "square")
})

test_that("sonnet() errors on non-numeric matrix", {
  char_mat <- matrix(letters[1:9], nrow = 3)

  expect_error(sonnet(char_mat))
})

test_that("parse_matrix errors on non-square matrix", {
  non_square <- matrix(1:6, nrow = 2, ncol = 3)

  expect_error(parse_matrix(non_square), "square")
})

# ============================================
# EDGE LIST VALIDATION
# ============================================

test_that("parse_edgelist errors on empty data frame", {
  empty_df <- data.frame(from = integer(0), to = integer(0))

  expect_error(parse_edgelist(empty_df), "empty")
})

test_that("sonnet() handles edge list with missing from/to columns gracefully", {
  bad_df <- data.frame(source = c(1, 2), target = c(2, 3))

  # The package may accept alternative column names or auto-detect
  # Just verify it doesn't crash silently with an unintended result
  result <- tryCatch(
    sonnet(bad_df),
    error = function(e) "error"
  )

  # Either errors or produces a valid network (not silent corruption)
  if (!identical(result, "error")) {
    expect_sonnet_network(result)
  }
})

# ============================================
# PARAMETER VALIDATION
# ============================================

test_that("sn_nodes() validates alpha range", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Values outside 0-1 should error
  expect_error(sn_nodes(net, alpha = 1.5))
  expect_error(sn_nodes(net, alpha = -0.5))
})

test_that("sn_nodes() validates label_position options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_nodes(net, label_position = "invalid_position"))
})

test_that("sn_nodes() validates donut_shape options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  valid_shapes <- c("circle", "square", "hexagon", "triangle", "diamond", "pentagon")

  # Valid shapes should work
  for (shape in valid_shapes) {
    expect_silent(sn_nodes(net, donut_shape = shape))
  }

  # Invalid shape should error
  expect_error(sn_nodes(net, donut_shape = "invalid_shape"))
})

test_that("sn_nodes() validates fontface options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Valid fontfaces
  for (face in c("plain", "bold", "italic", "bold.italic")) {
    expect_silent(sn_nodes(net, label_fontface = face))
  }

  # Invalid fontface
  expect_error(sn_nodes(net, label_fontface = "extra_bold"))
})

test_that("sn_nodes() validates donut_value_fontface options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_nodes(net, donut_value_fontface = "invalid"))
})

test_that("sn_nodes() validates donut_value_format is function", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_nodes(net, donut_value_format = "not_a_function"))
})

test_that("sn_edges() validates alpha range", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_edges(net, alpha = 2.0))
  expect_error(sn_edges(net, alpha = -0.1))
})

test_that("sn_edges() validates edge_scale_mode options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Valid modes should work
  for (mode in c("linear", "log", "sqrt", "rank")) {
    expect_silent(sn_edges(net, edge_scale_mode = mode))
  }

  # Invalid mode should error
  expect_error(sn_edges(net, edge_scale_mode = "invalid_mode"))
})

test_that("sn_edges() validates style options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Valid styles should work
  for (style in c("solid", "dashed", "dotted", "longdash", "twodash")) {
    expect_silent(sn_edges(net, style = style))
  }

  # Invalid style should error
  expect_error(sn_edges(net, style = "wavy"))
})

test_that("sn_edges() validates curves parameter", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Valid values
  expect_silent(sn_edges(net, curves = FALSE))
  expect_silent(sn_edges(net, curves = "mutual"))
  expect_silent(sn_edges(net, curves = "force"))

  # Invalid value
  expect_error(sn_edges(net, curves = "all"))
  expect_error(sn_edges(net, curves = TRUE))  # TRUE is deprecated, use "mutual"
})

test_that("sn_edges() validates label_fontface options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_edges(net, label_fontface = "invalid"))
})

test_that("sn_edges() validates label_border options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Valid borders
  for (border in c("rect", "rounded", "circle")) {
    expect_silent(sn_edges(net, label_border = border))
  }

  # Invalid border
  expect_error(sn_edges(net, label_border = "hexagon"))
})

test_that("sn_edges() validates label_ci_format options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Valid formats
  expect_silent(sn_edges(net, label_ci_format = "bracket"))
  expect_silent(sn_edges(net, label_ci_format = "dash"))

  # Invalid format
  expect_error(sn_edges(net, label_ci_format = "parenthesis"))
})

test_that("sn_edges() validates label_style options", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Valid styles
  for (style in c("none", "estimate", "full", "range", "stars")) {
    expect_silent(sn_edges(net, label_style = style))
  }

  # Invalid style
  expect_error(sn_edges(net, label_style = "fancy"))
})

test_that("sn_edges() validates ci_alpha range", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_edges(net, ci_alpha = 1.5))
  expect_error(sn_edges(net, ci_alpha = -0.1))
})

test_that("sn_edges() validates label_shadow_alpha range", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_edges(net, label_shadow_alpha = 1.5))
  expect_error(sn_edges(net, label_shadow_alpha = -0.1))
})

# ============================================
# THEME VALIDATION
# ============================================

test_that("sn_theme() errors on unknown theme name", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_theme(net, "nonexistent_theme"))
})

test_that("sn_theme() errors on invalid theme type", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_theme(net, 123))  # Not a string or SonnetTheme
})

# ============================================
# PALETTE VALIDATION
# ============================================

test_that("sn_palette() errors on unknown palette name", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_palette(net, "nonexistent_palette"))
})

test_that("sn_palette() errors on invalid palette type", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_palette(net, 123))  # Not a string or function
})

# ============================================
# LAYOUT VALIDATION
# ============================================

test_that("sn_layout() errors on invalid layout type", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_layout(net, 123))  # Not a valid layout specification
})

test_that("sn_layout() handles unknown string layout", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Should error on unknown layout name
  expect_error(sn_layout(net, "nonexistent_layout"))
})

# ============================================
# INPUT TYPE VALIDATION
# ============================================

test_that("ensure_sonnet_network errors on unsupported types", {
  ensure_sonnet_network <- Sonnet:::ensure_sonnet_network

  expect_error(ensure_sonnet_network("string"))
  expect_error(ensure_sonnet_network(123))
  expect_error(ensure_sonnet_network(list(a = 1)))
})

test_that("sn_save() errors on filename without extension", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_save(net, "no_extension"))
})

test_that("sn_save() errors on unsupported format", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)
  tmp <- tempfile(fileext = ".xyz")

  expect_error(sn_save(net, tmp), "Unsupported")
})

# ============================================
# CONVERTER VALIDATION
# ============================================

test_that("from_qgraph() errors on non-qgraph object", {
  expect_error(from_qgraph(list(a = 1)), "qgraph")
  expect_error(from_qgraph(matrix(1:4, 2, 2)))  # Error message may vary
})

test_that("from_tna() errors on non-tna object", {
  expect_error(from_tna(list(a = 1)), "tna")
  expect_error(from_tna(matrix(1:4, 2, 2)), "tna")
})

# ============================================
# REGISTRY VALIDATION
# ============================================

test_that("get_theme() returns NULL for unknown theme", {
  result <- get_theme("nonexistent_theme_xyz")
  expect_null(result)
})

test_that("get_layout() returns NULL or errors for unknown layout", {
  # Depending on implementation, either returns NULL or errors
  result <- tryCatch(
    get_layout("nonexistent_layout_xyz"),
    error = function(e) "error"
  )

  expect_true(is.null(result) || result == "error")
})

test_that("get_shape() returns NULL for unknown shape", {
  result <- get_shape("nonexistent_shape_xyz")
  expect_null(result)
})

# ============================================
# VECTOR LENGTH VALIDATION
# ============================================

test_that("sn_nodes() recycles shorter vectors correctly", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  # Single value should be recycled to all nodes
  net2 <- sn_nodes(net, fill = "red")
  aes <- net2$network$get_node_aes()
  expect_equal(length(aes$fill), 4)
  expect_true(all(aes$fill == "red"))
})

test_that("sn_nodes() handles exact-length vectors", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  colors <- c("red", "green", "blue", "orange")
  net2 <- sn_nodes(net, fill = colors)
  aes <- net2$network$get_node_aes()
  expect_equal(aes$fill, colors)
})

# ============================================
# ERROR MESSAGE QUALITY
# ============================================

test_that("error messages are informative", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Check that error messages mention the problematic parameter
  err <- tryCatch(
    sn_nodes(net, alpha = 2.0),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("alpha|0|1", err, ignore.case = TRUE))

  err <- tryCatch(
    sn_nodes(net, label_position = "nowhere"),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("position|center|above|below", err, ignore.case = TRUE))
})

# ============================================
# BOUNDARY CONDITIONS
# ============================================

test_that("alpha at boundaries works", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  # Exactly 0 and 1 should be valid
  expect_silent(sn_nodes(net, alpha = 0))
  expect_silent(sn_nodes(net, alpha = 1))
  expect_silent(sn_edges(net, alpha = 0))
  expect_silent(sn_edges(net, alpha = 1))
})

test_that("empty network does not crash on aesthetic functions", {
  adj <- matrix(0, 3, 3)  # No edges
  net <- sonnet(adj)

  # These should all work without crashing

  expect_silent(sn_nodes(net, fill = "red"))
  expect_silent(sn_edges(net, color = "blue"))  # No edges to color
  expect_silent(sn_theme(net, "dark"))
})
