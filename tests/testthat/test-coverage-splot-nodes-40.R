# test-coverage-splot-nodes-40.R - Comprehensive tests for R/splot-nodes.R
# Tests for node rendering functions including shapes, pie/donut nodes, labels

# Make internal functions available for testing
draw_node_base <- cograph:::draw_node_base
draw_pie_node_base <- cograph:::draw_pie_node_base
draw_donut_node_base <- cograph:::draw_donut_node_base
draw_polygon_donut_node_base <- cograph:::draw_polygon_donut_node_base
draw_donut_pie_node_base <- cograph:::draw_donut_pie_node_base
draw_polygon_donut_pie_node_base <- cograph:::draw_polygon_donut_pie_node_base
draw_double_donut_pie_node_base <- cograph:::draw_double_donut_pie_node_base
draw_node_label_base <- cograph:::draw_node_label_base
draw_neural_node_base <- cograph:::draw_neural_node_base
draw_chip_node_base <- cograph:::draw_chip_node_base
draw_robot_node_base <- cograph:::draw_robot_node_base
draw_network_node_base <- cograph:::draw_network_node_base
draw_database_node_base <- cograph:::draw_database_node_base
render_nodes_base <- cograph:::render_nodes_base
recycle_to_length <- cograph:::recycle_to_length
get_shape_vertices <- cograph:::get_shape_vertices
get_donut_base_vertices <- cograph:::get_donut_base_vertices
inset_polygon_vertices <- cograph:::inset_polygon_vertices

# ============================================
# BASIC NODE DRAWING - draw_node_base()
# ============================================

test_that("draw_node_base draws circle shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "circle", col = "blue", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
  expect_true(file.exists(tmp))
})

test_that("draw_node_base draws square shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "square", col = "red", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws ellipse shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, size2 = 0.3, shape = "ellipse", col = "green", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws rectangle shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.6, size2 = 0.4, shape = "rectangle", col = "orange", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws triangle shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "triangle", col = "purple", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws diamond shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "diamond", col = "cyan", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws pentagon shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "pentagon", col = "yellow", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws hexagon shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "hexagon", col = "pink", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws star shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "star", col = "gold", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws heart shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "heart", col = "red", border.col = "darkred")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws cross shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "cross", col = "white", border.col = "red")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws gear shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "gear", col = "gray50", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws cloud shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "cloud", col = "lightblue", border.col = "gray")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base draws brain shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "brain", col = "pink", border.col = "maroon")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base handles size2 = NULL for circle", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, size2 = NULL, shape = "circle", col = "blue", border.col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base handles custom border width", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_base(0, 0, size = 0.5, shape = "circle", col = "blue", border.col = "black", border.width = 3)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# SPECIAL NODE SHAPES - neural, chip, robot, network, database
# ============================================

test_that("draw_neural_node_base draws neural network node", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_neural_node_base(0, 0, size = 0.8, col = "lightblue", border.col = "darkblue", border.width = 1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_neural_node_base handles n_connections parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_neural_node_base(0, 0, size = 0.8, col = "lightblue", border.col = "darkblue", border.width = 1, n_connections = 8)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_chip_node_base draws chip node", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_chip_node_base(0, 0, size = 0.8, col = "darkgray", border.col = "black", border.width = 1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_chip_node_base handles pins_per_side parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_chip_node_base(0, 0, size = 0.8, col = "darkgray", border.col = "black", border.width = 1, pins_per_side = 5)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_robot_node_base draws robot node", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_robot_node_base(0, 0, size = 0.8, col = "#C0C0C0", border.col = "black", border.width = 1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_network_node_base draws network pattern node", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_network_node_base(0, 0, size = 0.8, col = "lightgreen", border.col = "darkgreen", border.width = 1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_database_node_base draws database cylinder node", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_database_node_base(0, 0, size = 0.5, col = "lightblue", border.col = "blue", border.width = 1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_base dispatches to special shapes via shape parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  for (shape in c("neural", "chip", "robot", "network", "database")) {
    png(tmp, width = 200, height = 200)
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    result <- tryCatch({
      draw_node_base(0, 0, size = 0.6, shape = shape, col = "lightgray", border.col = "black")
      TRUE
    }, error = function(e) FALSE)
    dev.off()

    expect_true(result, info = paste("Failed for shape:", shape))
  }
})

# ============================================
# PIE CHART NODES - draw_pie_node_base()
# ============================================

test_that("draw_pie_node_base draws simple pie chart", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_pie_node_base(0, 0, size = 0.8, values = c(1, 2, 3), colors = c("red", "green", "blue"))
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_pie_node_base uses default rainbow colors when colors is NULL", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_pie_node_base(0, 0, size = 0.8, values = c(1, 2, 3, 4), colors = NULL)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_pie_node_base uses default_color for single segment", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_pie_node_base(0, 0, size = 0.8, values = c(5), colors = NULL, default_color = "orange")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_pie_node_base handles NULL or empty values", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_pie_node_base(0, 0, size = 0.8, values = NULL)
    draw_pie_node_base(0, 0, size = 0.8, values = c())
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_pie_node_base handles zero values in proportions", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_pie_node_base(0, 0, size = 0.8, values = c(1, 0, 2, 0), colors = c("red", "green", "blue", "yellow"))
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_pie_node_base handles pie_border.width parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_pie_node_base(0, 0, size = 0.8, values = c(1, 2, 3), colors = c("red", "green", "blue"), pie_border.width = 2)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_pie_node_base skips slice dividers when border width is small", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_pie_node_base(0, 0, size = 0.8, values = c(1, 2, 3), colors = c("red", "green", "blue"), pie_border.width = 0.05)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# DONUT NODES - draw_donut_node_base()
# ============================================

test_that("draw_donut_node_base draws progress donut with single value", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_node_base(0, 0, size = 0.8, values = 0.7, colors = "steelblue")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_node_base draws segmented donut with multiple values", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_node_base(0, 0, size = 0.8, values = c(1, 2, 3), colors = c("red", "green", "blue"))
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_node_base handles inner_ratio parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  for (ratio in c(0.3, 0.5, 0.7)) {
    png(tmp, width = 200, height = 200)
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    result <- tryCatch({
      draw_donut_node_base(0, 0, size = 0.8, values = 0.6, inner_ratio = ratio)
      TRUE
    }, error = function(e) FALSE)
    dev.off()

    expect_true(result, info = paste("Failed for inner_ratio:", ratio))
  }
})

test_that("draw_donut_node_base handles bg_color parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_node_base(0, 0, size = 0.8, values = 0.5, bg_color = "lightyellow")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_node_base handles center_color parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_node_base(0, 0, size = 0.8, values = 0.5, center_color = "lightblue")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_node_base shows value in center when show_value = TRUE", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_node_base(0, 0, size = 0.8, values = 0.75, show_value = TRUE, value_cex = 1.2, value_col = "navy")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_node_base hides value when show_value = FALSE", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_node_base(0, 0, size = 0.8, values = 0.5, show_value = FALSE)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_node_base handles value formatting parameters", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_node_base(0, 0, size = 0.8, values = 0.75, show_value = TRUE,
                         value_digits = 0, value_prefix = "", value_suffix = "%")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_node_base handles fontface parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  for (face in c("plain", "bold", "italic", "bold.italic")) {
    png(tmp, width = 200, height = 200)
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    result <- tryCatch({
      draw_donut_node_base(0, 0, size = 0.8, values = 0.5, show_value = TRUE, value_fontface = face)
      TRUE
    }, error = function(e) FALSE)
    dev.off()

    expect_true(result, info = paste("Failed for fontface:", face))
  }
})

test_that("draw_donut_node_base handles outer_border.col for double border", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_node_base(0, 0, size = 0.8, values = 0.6, outer_border.col = "red")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_node_base handles border.lty parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_node_base(0, 0, size = 0.8, values = 0.5, border.lty = 2)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_node_base clamps values to 0-1 range", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    # Value > 1 should be clamped
    draw_donut_node_base(0, 0, size = 0.8, values = 1.5)
    # Value < 0 should be clamped
    draw_donut_node_base(0.5, 0.5, size = 0.4, values = -0.2)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# POLYGON DONUT NODES - draw_polygon_donut_node_base()
# ============================================

test_that("draw_polygon_donut_node_base draws square donut", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_node_base(0, 0, size = 0.8, values = 0.6, donut_shape = "square")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_polygon_donut_node_base draws hexagon donut", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_node_base(0, 0, size = 0.8, values = 0.7, donut_shape = "hexagon")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_polygon_donut_node_base draws triangle donut", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_node_base(0, 0, size = 0.8, values = 0.5, donut_shape = "triangle")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_polygon_donut_node_base draws pentagon donut", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_node_base(0, 0, size = 0.8, values = 0.4, donut_shape = "pentagon")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_polygon_donut_node_base draws diamond donut", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_node_base(0, 0, size = 0.8, values = 0.8, donut_shape = "diamond")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_polygon_donut_node_base handles multi-segment values", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_node_base(0, 0, size = 0.8, values = c(1, 2, 3),
                                  colors = c("red", "green", "blue"), donut_shape = "hexagon")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_polygon_donut_node_base handles NULL values (defaults to 1)", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_node_base(0, 0, size = 0.8, values = NULL, donut_shape = "square")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_polygon_donut_node_base handles outer_border.col parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_node_base(0, 0, size = 0.8, values = 0.5, donut_shape = "hexagon", outer_border.col = "red")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# DONUT + PIE COMBINATION - draw_donut_pie_node_base()
# ============================================

test_that("draw_donut_pie_node_base draws donut with inner pie", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_pie_node_base(0, 0, size = 0.8, donut_value = 0.7, donut_color = "steelblue",
                             pie_values = c(1, 2, 3), pie_colors = c("red", "green", "blue"))
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_pie_node_base works with donut only (no pie)", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_pie_node_base(0, 0, size = 0.8, donut_value = 0.5, donut_color = "purple",
                             pie_values = NULL)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_donut_pie_node_base handles pie_border.width and donut_border.width", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_donut_pie_node_base(0, 0, size = 0.8, donut_value = 0.6, donut_color = "orange",
                             pie_values = c(1, 1, 1), pie_colors = c("red", "green", "blue"),
                             pie_border.width = 0.5, donut_border.width = 2)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# POLYGON DONUT + PIE - draw_polygon_donut_pie_node_base()
# ============================================

test_that("draw_polygon_donut_pie_node_base draws hexagon donut with pie", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_pie_node_base(0, 0, size = 0.8, donut_value = 0.7, donut_color = "steelblue",
                                      donut_shape = "hexagon", pie_values = c(2, 3),
                                      pie_colors = c("red", "blue"))
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_polygon_donut_pie_node_base handles different shapes", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  for (shape in c("square", "triangle", "pentagon", "diamond")) {
    png(tmp, width = 200, height = 200)
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    result <- tryCatch({
      draw_polygon_donut_pie_node_base(0, 0, size = 0.8, donut_value = 0.5, donut_color = "green",
                                        donut_shape = shape, pie_values = c(1, 1),
                                        pie_colors = c("orange", "purple"))
      TRUE
    }, error = function(e) FALSE)
    dev.off()

    expect_true(result, info = paste("Failed for shape:", shape))
  }
})

test_that("draw_polygon_donut_pie_node_base works without pie (no pie values)", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_polygon_donut_pie_node_base(0, 0, size = 0.8, donut_value = 0.8, donut_color = "navy",
                                      donut_shape = "hexagon", pie_values = NULL)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# DOUBLE DONUT + PIE - draw_double_donut_pie_node_base()
# ============================================

test_that("draw_double_donut_pie_node_base draws two concentric donuts with pie", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_double_donut_pie_node_base(0, 0, size = 0.9,
                                    donut_values = 0.7, donut_colors = "steelblue",
                                    donut2_values = 0.5, donut2_colors = "orange",
                                    pie_values = c(1, 2), pie_colors = c("red", "green"))
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base handles segmented outer donut", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_double_donut_pie_node_base(0, 0, size = 0.9,
                                    donut_values = c(1, 2, 3), donut_colors = c("red", "green", "blue"),
                                    donut2_values = 0.6, donut2_colors = "purple")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base handles NULL outer donut", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_double_donut_pie_node_base(0, 0, size = 0.9,
                                    donut_values = NULL,
                                    donut2_values = 0.5, donut2_colors = "orange",
                                    pie_values = c(1, 1, 1))
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base handles NULL inner donut", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_double_donut_pie_node_base(0, 0, size = 0.9,
                                    donut_values = 0.8, donut_colors = "steelblue",
                                    donut2_values = NULL,
                                    pie_values = c(2, 3))
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base handles NULL pie", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_double_donut_pie_node_base(0, 0, size = 0.9,
                                    donut_values = 0.7, donut_colors = "steelblue",
                                    donut2_values = 0.5, donut2_colors = "orange",
                                    pie_values = NULL)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base handles custom ratios", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_double_donut_pie_node_base(0, 0, size = 0.9,
                                    donut_values = 0.6, donut_colors = "blue",
                                    donut2_values = 0.8, donut2_colors = "green",
                                    pie_values = c(1, 1),
                                    outer_inner_ratio = 0.8, inner_inner_ratio = 0.5)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# NODE LABELS - draw_node_label_base()
# ============================================

test_that("draw_node_label_base draws text label", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_label_base(0, 0, label = "Test", cex = 1.2, col = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_label_base handles NULL and empty labels", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_label_base(0, 0, label = NULL)
    draw_node_label_base(0, 0.5, label = NA)
    draw_node_label_base(0, 1, label = "")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_label_base handles font parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_label_base(0, 0.5, label = "Bold", font = 2)
    draw_node_label_base(0, -0.5, label = "Italic", font = 3)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_label_base handles family parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_label_base(0, 0.5, label = "Sans", family = "sans")
    draw_node_label_base(0, -0.5, label = "Serif", family = "serif")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_label_base handles hjust and vjust", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_label_base(0, 0, label = "Center", hjust = 0.5, vjust = 0.5)
    draw_node_label_base(-1, 0, label = "Left", hjust = 0, vjust = 0.5)
    draw_node_label_base(1, 0, label = "Right", hjust = 1, vjust = 0.5)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_label_base handles srt rotation", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_label_base(0, 0, label = "Rotated", srt = 45)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_node_label_base handles pos and offset", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
  result <- tryCatch({
    draw_node_label_base(0, 0, label = "Above", pos = 3, offset = 0.3)
    draw_node_label_base(0, -0.5, label = "Below", pos = 1, offset = 0.3)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# RENDER ALL NODES - render_nodes_base()
# ============================================

test_that("render_nodes_base renders multiple nodes", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 300, height = 300)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)

  layout <- matrix(c(0, 1, -1, 0, 1, 0), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = c(0.3, 0.3, 0.3),
                      shape = "circle", color = c("red", "green", "blue"),
                      border.color = "black", border.width = 1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("render_nodes_base renders nodes with different shapes", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 300, height = 300)
  plot(0, 0, xlim = c(-3, 3), ylim = c(-3, 3), type = "n", asp = 1)

  layout <- matrix(c(-1, 1, 1, 1, -1, -1, 1, -1), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = c(0.4, 0.4, 0.4, 0.4),
                      shape = c("circle", "square", "triangle", "diamond"),
                      color = c("red", "green", "blue", "orange"),
                      border.color = "black", border.width = 1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("render_nodes_base renders nodes with labels", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 300, height = 300)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)

  layout <- matrix(c(-0.5, 0.5, 0.5, 0.5, 0, -0.5), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = c(0.3, 0.3, 0.3),
                      shape = "circle", color = "lightblue", border.color = "navy",
                      labels = c("A", "B", "C"), label.cex = 1, label.color = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("render_nodes_base renders pie chart nodes", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 300, height = 300)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)

  layout <- matrix(c(-0.5, 0, 0.5, 0), ncol = 2, byrow = TRUE)
  pie_vals <- list(c(1, 2), c(2, 3, 1))
  pie_cols <- list(c("red", "blue"), c("green", "yellow", "purple"))

  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = c(0.5, 0.5),
                      shape = "circle", color = "white", border.color = "black",
                      pie = pie_vals, pieColor = pie_cols)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("render_nodes_base renders donut nodes", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 300, height = 300)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)

  layout <- matrix(c(-0.5, 0, 0.5, 0), ncol = 2, byrow = TRUE)
  donut_vals <- list(0.3, 0.7)
  donut_cols <- list("steelblue", "orange")

  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = c(0.5, 0.5),
                      shape = "circle", color = "white", border.color = "black",
                      donut = donut_vals, donutColor = donut_cols)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("render_nodes_base renders donut + pie combination", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 300, height = 300)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)

  layout <- matrix(c(0, 0), ncol = 2, byrow = TRUE)
  donut_vals <- list(0.6)
  donut_cols <- list("steelblue")
  pie_vals <- list(c(1, 2, 1))
  pie_cols <- list(c("red", "green", "blue"))

  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = 0.8,
                      shape = "circle", color = "white", border.color = "black",
                      donut = donut_vals, donutColor = donut_cols,
                      pie = pie_vals, pieColor = pie_cols)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("render_nodes_base handles empty layout (0 nodes)", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", asp = 1)

  layout <- matrix(nrow = 0, ncol = 2)

  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = numeric(0), shape = character(0))
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("render_nodes_base renders nodes in order by size (largest first)", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 300, height = 300)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)

  # Overlapping nodes at same position - larger should be drawn first (under)
  layout <- matrix(c(0, 0, 0, 0, 0, 0), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = c(0.2, 0.5, 0.3),
                      shape = "circle", color = c("red", "blue", "green"),
                      border.color = "black", border.width = 1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("render_nodes_base handles recycled parameters", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 300, height = 300)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)

  layout <- matrix(c(-1, 0, 0, 0, 1, 0), ncol = 2, byrow = TRUE)

  # Single values should be recycled to 3 nodes
  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = 0.3,  # recycled
                      shape = "circle",  # recycled
                      color = "lightblue",  # recycled
                      border.color = "navy",  # recycled
                      border.width = 2,  # recycled
                      labels = c("A", "B", "C"),
                      label.cex = 0.8,  # recycled
                      label.color = "black")  # recycled
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("render_nodes_base handles vsize2 parameter for ellipses", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 300, height = 300)
  plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)

  layout <- matrix(c(-0.5, 0, 0.5, 0), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    render_nodes_base(layout = layout, vsize = c(0.4, 0.4),
                      vsize2 = c(0.2, 0.3),  # Different heights
                      shape = "ellipse", color = c("pink", "lavender"),
                      border.color = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# HELPER FUNCTIONS
# ============================================

test_that("get_shape_vertices returns vertices for all standard shapes", {
  shapes <- c("circle", "square", "rectangle", "triangle", "diamond",
              "pentagon", "hexagon", "star", "heart", "ellipse", "cross",
              "gear", "cloud", "brain")

  for (shape in shapes) {
    verts <- get_shape_vertices(shape, 0, 0, 1)
    expect_true(is.list(verts), info = paste("Failed for shape:", shape))
    expect_true("x" %in% names(verts), info = paste("Missing x for shape:", shape))
    expect_true("y" %in% names(verts), info = paste("Missing y for shape:", shape))
    expect_true(length(verts$x) > 0, info = paste("Empty x for shape:", shape))
    expect_equal(length(verts$x), length(verts$y), info = paste("x/y length mismatch for shape:", shape))
  }
})

test_that("get_donut_base_vertices returns vertices for donut shapes", {
  shapes <- c("circle", "square", "rectangle", "triangle", "diamond", "pentagon", "hexagon")

  for (shape in shapes) {
    verts <- get_donut_base_vertices(shape, 0, 0, 1)
    expect_true(is.list(verts), info = paste("Failed for shape:", shape))
    expect_true("x" %in% names(verts))
    expect_true("y" %in% names(verts))
    expect_true(length(verts$x) > 0)
  }
})

test_that("inset_polygon_vertices creates smaller polygon", {
  outer <- list(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
  inner <- inset_polygon_vertices(outer, 0.5)

  expect_true(is.list(inner))
  expect_equal(length(inner$x), length(outer$x))

  # Inner should be closer to center (0, 0)
  outer_dist <- sqrt(outer$x^2 + outer$y^2)
  inner_dist <- sqrt(inner$x^2 + inner$y^2)
  expect_true(all(inner_dist < outer_dist))
})

test_that("recycle_to_length recycles values correctly", {
  # Single value to many
  result <- recycle_to_length("red", 5)
  expect_equal(result, rep("red", 5))

  # Vector shorter than target
  result <- recycle_to_length(c("a", "b"), 5)
  expect_equal(length(result), 5)
  expect_equal(result, c("a", "b", "a", "b", "a"))

  # Vector same length
  result <- recycle_to_length(c(1, 2, 3), 3)
  expect_equal(result, c(1, 2, 3))

  # Vector longer than target (truncates)
  result <- recycle_to_length(c(1, 2, 3, 4, 5), 3)
  expect_equal(length(result), 3)
})

# ============================================
# INTEGRATION WITH SPLOT
# ============================================

test_that("splot renders all standard node shapes correctly", {
  adj <- create_test_matrix(3)
  shapes <- c("circle", "square", "triangle", "diamond", "pentagon",
              "hexagon", "star", "heart", "ellipse", "cross")

  for (shape in shapes) {
    result <- safe_plot(splot(adj, node_shape = shape))
    expect_true(result$success, info = paste("splot failed for shape:", shape, "-", result$error))
  }
})

test_that("splot renders special node shapes correctly", {
  adj <- create_test_matrix(3)
  special_shapes <- c("gear", "cloud", "brain")

  for (shape in special_shapes) {
    result <- safe_plot(splot(adj, node_shape = shape))
    expect_true(result$success, info = paste("splot failed for shape:", shape, "-", result$error))
  }
})

test_that("splot renders tech node shapes correctly", {
  adj <- create_test_matrix(3)
  tech_shapes <- c("neural", "chip", "robot", "network", "database")

  for (shape in tech_shapes) {
    result <- safe_plot(splot(adj, node_shape = shape))
    expect_true(result$success, info = paste("splot failed for shape:", shape, "-", result$error))
  }
})

test_that("splot renders mixed shapes per node", {
  adj <- create_test_matrix(4)
  shapes <- c("circle", "square", "triangle", "diamond")

  result <- safe_plot(splot(adj, node_shape = shapes))
  expect_true(result$success, info = result$error)
})
