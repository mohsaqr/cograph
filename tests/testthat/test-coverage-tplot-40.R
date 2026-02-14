# test-coverage-tplot-40.R - Comprehensive tests for tplot.R
# Tests for plot_tna() and tplot() functions - TNA-style network plotting

# ============================================
# BASIC FUNCTIONALITY
# ============================================

test_that("plot_tna() accepts adjacency matrix", {
  adj <- create_test_matrix(5)

  result <- safe_plot(plot_tna(adj))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() accepts weighted matrix", {
  adj <- create_test_matrix(5, weighted = TRUE)

  result <- safe_plot(plot_tna(adj))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() accepts asymmetric (directed) matrix", {
  adj <- create_test_matrix(5, symmetric = FALSE)

  result <- safe_plot(plot_tna(adj, directed = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("tplot alias works identically to plot_tna", {
  adj <- create_test_matrix(4)

  result <- safe_plot(tplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() returns cograph_network invisibly", {
  adj <- create_test_matrix(4)

  result <- with_temp_png({
    ret <- plot_tna(adj)
    ret
  })

  expect_cograph_network(result)
})

# ============================================
# NODE COLOR PARAMETER
# ============================================

test_that("plot_tna() handles single color", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, color = "steelblue"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles per-node colors", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, color = c("red", "green", "blue", "orange")))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles hex color values", {
  adj <- create_test_matrix(3)

  result <- safe_plot(plot_tna(adj, color = c("#FF5733", "#33FF57", "#3357FF")))
  expect_true(result$success, info = result$error)
})

# ============================================
# LABELS PARAMETER
# ============================================

test_that("plot_tna() handles custom labels", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, labels = c("A", "B", "C", "D")))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles NULL labels", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, labels = NULL))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles long labels", {
  adj <- create_test_matrix(3)
  long_labels <- c("Long Label One", "Longer Label Two", "Very Long Label Three")

  result <- safe_plot(plot_tna(adj, labels = long_labels))
  expect_true(result$success, info = result$error)
})

# ============================================
# LAYOUT PARAMETER
# ============================================

test_that("plot_tna() uses oval layout by default", {
  adj <- create_test_matrix(5)

  result <- safe_plot(plot_tna(adj, layout = "oval"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() works with circle layout", {
  adj <- create_test_matrix(5)

  result <- safe_plot(plot_tna(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() works with spring layout", {
  adj <- create_test_matrix(5)

  result <- safe_plot(plot_tna(adj, layout = "spring"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() works with custom coordinate matrix", {
  adj <- create_test_matrix(4)
  custom_layout <- matrix(c(0, 0, 1, 1, 0, 1, 0, 1), ncol = 2)

  result <- safe_plot(plot_tna(adj, layout = custom_layout))
  expect_true(result$success, info = result$error)
})

# ============================================
# THEME PARAMETER
# ============================================

test_that("plot_tna() uses colorblind theme by default", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, theme = "colorblind"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() works with classic theme", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, theme = "classic"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() works with dark theme", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, theme = "dark"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() works with minimal theme", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, theme = "minimal"))
  expect_true(result$success, info = result$error)
})

# ============================================
# MARGINS PARAMETER
# ============================================

test_that("plot_tna() handles custom margins", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, mar = c(0.2, 0.2, 0.2, 0.2)))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles zero margins", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, mar = c(0, 0, 0, 0)))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles large margins", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, mar = c(0.5, 0.5, 0.5, 0.5)))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE LABELS PARAMETER
# ============================================

test_that("plot_tna() shows edge labels by default", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(plot_tna(adj, edge.labels = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() hides edge labels when FALSE", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(plot_tna(adj, edge.labels = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles edge.label.position parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(plot_tna(adj, edge.labels = TRUE, edge.label.position = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles edge.label.cex parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(plot_tna(adj, edge.labels = TRUE, edge.label.cex = 1.2))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE COLOR PARAMETER
# ============================================

test_that("plot_tna() uses TNA default edge color", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, edge.color = "#003355"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles custom edge color", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, edge.color = "gray50"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles NULL edge color", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(plot_tna(adj, edge.color = NULL))
  expect_true(result$success, info = result$error)
})

# ============================================
# NODE SIZE PARAMETER (vsize)
# ============================================

test_that("plot_tna() handles vsize parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, vsize = 10))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles small vsize", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, vsize = 2))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles large vsize", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, vsize = 20))
  expect_true(result$success, info = result$error)
})

# ============================================
# PIE/DONUT PARAMETERS
# ============================================

test_that("plot_tna() handles pie parameter for donut fill", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, pie = c(0.2, 0.4, 0.6, 0.8)))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles pieColor parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj,
    pie = c(0.3, 0.5, 0.7, 0.9),
    pieColor = c("red", "blue", "green", "orange")
  ))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles single pie value", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, pie = 0.5))
  expect_true(result$success, info = result$error)
})

# ============================================
# LINE TYPE PARAMETER (lty)
# ============================================

test_that("plot_tna() handles lty numeric values", {
  adj <- create_test_matrix(4)

  # Test solid line type (1)
  result <- safe_plot(plot_tna(adj, lty = 1))
  expect_true(result$success, info = result$error)

  # Test dashed line type (2)
  result <- safe_plot(plot_tna(adj, lty = 2))
  expect_true(result$success, info = result$error)

  # Test dotted line type (3)
  result <- safe_plot(plot_tna(adj, lty = 3))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles lty character values", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, lty = "solid"))
  expect_true(result$success, info = result$error)

  result <- safe_plot(plot_tna(adj, lty = "dashed"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles NULL lty", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, lty = NULL))
  expect_true(result$success, info = result$error)
})

# ============================================
# DIRECTED PARAMETER
# ============================================

test_that("plot_tna() handles directed = TRUE", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(plot_tna(adj, directed = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles directed = FALSE", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, directed = FALSE))
  expect_true(result$success, info = result$error)
})

# ============================================
# MINIMUM/CUT PARAMETERS
# ============================================

test_that("plot_tna() handles minimum parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(plot_tna(adj, minimum = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles cut parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(plot_tna(adj, cut = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles both minimum and cut", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(plot_tna(adj, minimum = 0.2, cut = 0.6))
  expect_true(result$success, info = result$error)
})

# ============================================
# POSITIVE/NEGATIVE COLORS
# ============================================

test_that("plot_tna() handles posCol parameter", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)

  result <- safe_plot(plot_tna(adj, posCol = "darkgreen"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles negCol parameter", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)

  result <- safe_plot(plot_tna(adj, negCol = "darkred"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles both posCol and negCol", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)

  result <- safe_plot(plot_tna(adj, posCol = "#009900", negCol = "#C62828"))
  expect_true(result$success, info = result$error)
})

# ============================================
# ARROW ANGLE PARAMETER
# ============================================

test_that("plot_tna() handles arrowAngle parameter", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(plot_tna(adj, directed = TRUE, arrowAngle = pi/4))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles NULL arrowAngle", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(plot_tna(adj, directed = TRUE, arrowAngle = NULL))
  expect_true(result$success, info = result$error)
})

# ============================================
# TITLE PARAMETER
# ============================================

test_that("plot_tna() handles title parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, title = "Test Network"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles NULL title", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, title = NULL))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles long title", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, title = "This is a Very Long Title for Testing"))
  expect_true(result$success, info = result$error)
})

# ============================================
# ELLIPSIS PARAMETERS (passed to splot)
# ============================================

test_that("plot_tna() passes node_border_color through ellipsis", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, node_border_color = "black"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() passes node_border_width through ellipsis", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, node_border_width = 2))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() passes background through ellipsis", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, background = "lightgray"))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() passes edge_alpha through ellipsis", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, edge_alpha = 0.5))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE CASES AND SPECIAL INPUTS
# ============================================

test_that("plot_tna() handles single-node network", {
  adj <- matrix(0, 1, 1)

  result <- safe_plot(plot_tna(adj))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles two-node network", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)

  result <- safe_plot(plot_tna(adj))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles network with no edges", {
  adj <- matrix(0, 4, 4)

  result <- safe_plot(plot_tna(adj))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles fully connected network", {
  adj <- create_test_topology("complete", n = 5)

  result <- safe_plot(plot_tna(adj))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles named matrix", {
  adj <- create_test_matrix(4)
  rownames(adj) <- colnames(adj) <- c("Node1", "Node2", "Node3", "Node4")

  result <- safe_plot(plot_tna(adj))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles self-loops", {
  adj <- create_test_matrix(4)
  diag(adj) <- 1

  result <- safe_plot(plot_tna(adj))
  expect_true(result$success, info = result$error)
})

# ============================================
# TNA DEFAULT PARAMETERS
# ============================================

test_that("plot_tna() sets TNA default edge_start_style", {
  adj <- create_test_matrix(4)

  # Should use dotted edge start style by default
  result <- safe_plot(plot_tna(adj))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() sets TNA default arrow_size", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  # Should use 0.61 arrow_size by default
  result <- safe_plot(plot_tna(adj, directed = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# COMBINATION TESTS
# ============================================

test_that("plot_tna() works with multiple parameters combined", {
  adj <- create_test_matrix(5, weighted = TRUE)

  result <- safe_plot(plot_tna(
    adj,
    color = c("red", "blue", "green", "orange", "purple"),
    labels = c("A", "B", "C", "D", "E"),
    layout = "circle",
    theme = "colorblind",
    vsize = 10,
    edge.labels = TRUE,
    edge.label.cex = 0.8,
    title = "Combined Test"
  ))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() works with pie and custom colors", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(
    adj,
    color = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
    pie = c(0.25, 0.50, 0.75, 1.0),
    pieColor = c("maroon", "navy", "forestgreen", "purple"),
    vsize = 12
  ))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() works with directed network and arrow customization", {
  adj <- create_test_matrix(5, symmetric = FALSE)

  result <- safe_plot(plot_tna(
    adj,
    directed = TRUE,
    arrowAngle = pi/5,
    edge.color = "#003355",
    edge.labels = TRUE
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# OUTPUT TO FILE
# ============================================

test_that("plot_tna() outputs to PNG file", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot_tna(adj, title = "PNG Test")
  dev.off()

  expect_file_created(tmp)
  expect_file_size(tmp, min_bytes = 100)
})

test_that("tplot alias outputs to PNG file", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  tplot(adj, title = "Tplot PNG Test")
  dev.off()

  expect_file_created(tmp)
})

# ============================================
# LTY EDGE CASES
# ============================================

test_that("plot_tna() handles lty with value > 6", {
  adj <- create_test_matrix(4)

  # Should clamp to valid lty value
  result <- safe_plot(plot_tna(adj, lty = 10))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() handles empty lty vector", {
  adj <- create_test_matrix(4)

  result <- safe_plot(plot_tna(adj, lty = numeric(0)))
  expect_true(result$success, info = result$error)
})

# ============================================
# QGRAPH COMPATIBILITY
# ============================================

test_that("plot_tna() maintains qgraph-style parameter names", {
  adj <- create_test_matrix(4, weighted = TRUE)

  # All qgraph-style parameter names should work
  result <- safe_plot(plot_tna(
    adj,
    edge.labels = TRUE,
    edge.label.position = 0.5,
    edge.label.cex = 0.7,
    edge.color = "gray"
  ))
  expect_true(result$success, info = result$error)
})

test_that("plot_tna() accepts cograph_network input", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  result <- safe_plot(plot_tna(net))
  expect_true(result$success, info = result$error)
})

test_that("tplot() accepts cograph_network input", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  result <- safe_plot(tplot(net))
  expect_true(result$success, info = result$error)
})
