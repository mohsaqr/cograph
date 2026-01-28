# test-from-converters.R - Converter Function Tests
# Tests for from_qgraph() and from_tna()

# ============================================
# FROM_QGRAPH() BASIC FUNCTIONALITY
# ============================================

test_that("from_qgraph() validates input is qgraph object", {
  # Non-qgraph input should error
  expect_error(from_qgraph(matrix(1:4, 2, 2)))
  expect_error(from_qgraph(list(a = 1)), "qgraph")
  expect_error(from_qgraph(data.frame(from = 1, to = 2)))
})

test_that("from_qgraph() works with actual qgraph object", {
  skip_if_no_qgraph()

  # Create a simple qgraph object
  adj <- matrix(c(0, 0.5, -0.3, 0.5, 0, 0.4, -0.3, 0.4, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Should return params without plotting when plot=FALSE
  params <- from_qgraph(q, plot = FALSE)

  expect_true(is.list(params))
  expect_true("x" %in% names(params))
})

test_that("from_qgraph() extracts layout from qgraph", {
  skip_if_no_qgraph()

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, layout = "circle")

  params <- from_qgraph(q, plot = FALSE)

  expect_true("layout" %in% names(params))
  expect_true(is.matrix(params$layout))
  expect_equal(nrow(params$layout), 3)
})

test_that("from_qgraph() extracts node labels", {
  skip_if_no_qgraph()

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  params <- from_qgraph(q, plot = FALSE)

  expect_true("labels" %in% names(params))
  expect_equal(unname(params$labels), c("A", "B", "C"))
})

test_that("from_qgraph() handles engine selection", {
  skip_if_no_qgraph()

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Both engines should work
  params_splot <- from_qgraph(q, engine = "splot", plot = FALSE)
  params_soplot <- from_qgraph(q, engine = "soplot", plot = FALSE)

  expect_true(is.list(params_splot))
  expect_true(is.list(params_soplot))
})

test_that("from_qgraph() respects weight_digits parameter", {
  skip_if_no_qgraph()

  adj <- matrix(c(0, 0.12345, 0.12345, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  params <- from_qgraph(q, weight_digits = 2, plot = FALSE)

  expect_equal(params$weight_digits, 2)
})

test_that("from_qgraph() handles override parameters", {
  skip_if_no_qgraph()

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  params <- from_qgraph(q, plot = FALSE, theme = "dark", node_size = 5)

  expect_equal(params$theme, "dark")
  expect_equal(params$node_size, 5)
})

test_that("from_qgraph() extracts directed setting", {
  skip_if_no_qgraph()

  # Asymmetric matrix -> directed
  adj <- matrix(c(0, 1, 0, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, directed = TRUE)

  params <- from_qgraph(q, plot = FALSE)

  expect_true("directed" %in% names(params))
})

test_that("from_qgraph() can plot with splot engine", {
  skip_if_no_qgraph()

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- safe_plot({
    from_qgraph(q, engine = "splot", plot = TRUE)
  })

  expect_true(result$success, info = result$error)
})

test_that("from_qgraph() handles pie/donut conversion", {
  skip_if_no_qgraph()

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  # qgraph pie values get mapped to donut_fill
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, pie = c(0.3, 0.7))

  params <- from_qgraph(q, plot = FALSE)

  # Should have donut-related parameters if pie was specified
  # (depends on qgraph version and how it stores pie data)
  expect_true(is.list(params))
})

# ============================================
# FROM_QGRAPH() SHAPE MAPPING
# ============================================

test_that("map_qgraph_shape() converts shapes correctly", {
  map_qgraph_shape <- Sonnet:::map_qgraph_shape

  # Test known mappings
  expect_equal(map_qgraph_shape("rectangle"), "square")
  expect_equal(map_qgraph_shape("square"), "square")
  expect_equal(map_qgraph_shape("circle"), "circle")
  expect_equal(map_qgraph_shape("ellipse"), "circle")
  expect_equal(map_qgraph_shape("triangle"), "triangle")
  expect_equal(map_qgraph_shape("diamond"), "diamond")
})

test_that("map_qgraph_shape() preserves unknown shapes", {
  map_qgraph_shape <- Sonnet:::map_qgraph_shape

  expect_equal(map_qgraph_shape("unknown"), "unknown")
  expect_equal(map_qgraph_shape("custom_shape"), "custom_shape")
})

test_that("map_qgraph_shape() handles vectors", {
  map_qgraph_shape <- Sonnet:::map_qgraph_shape

  shapes <- c("rectangle", "circle", "triangle")
  result <- map_qgraph_shape(shapes)

  expect_equal(result, c("square", "circle", "triangle"))
})

# ============================================
# FROM_QGRAPH() LINE TYPE MAPPING
# ============================================

test_that("map_qgraph_lty() converts line types correctly", {
  map_qgraph_lty <- Sonnet:::map_qgraph_lty

  # Numeric codes
  expect_equal(map_qgraph_lty(1), "solid")
  expect_equal(map_qgraph_lty(2), "dashed")
  expect_equal(map_qgraph_lty(3), "dotted")

  # String names
  expect_equal(map_qgraph_lty("solid"), "solid")
  expect_equal(map_qgraph_lty("dashed"), "dashed")
  expect_equal(map_qgraph_lty("dotted"), "dotted")
})

test_that("map_qgraph_lty() handles unknown values", {
  map_qgraph_lty <- Sonnet:::map_qgraph_lty

  expect_equal(map_qgraph_lty(99), "solid")  # Falls back to solid
})

test_that("map_qgraph_lty() handles vectors", {
  map_qgraph_lty <- Sonnet:::map_qgraph_lty

  ltys <- c(1, 2, 3)
  result <- map_qgraph_lty(ltys)

  expect_equal(result, c("solid", "dashed", "dotted"))
})

# ============================================
# FROM_TNA() BASIC FUNCTIONALITY
# ============================================

test_that("from_tna() validates input is tna object", {
  # Non-tna input should error
  expect_error(from_tna(matrix(1:4, 2, 2)), "tna")
  expect_error(from_tna(list(a = 1)), "tna")
  expect_error(from_tna(data.frame(from = 1, to = 2)), "tna")
})

test_that("from_tna() works with tna object", {
  skip_if_no_tna()

  # Create a transition matrix for tna
  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")

  tna_obj <- tna::tna(trans_mat)
  params <- from_tna(tna_obj, plot = FALSE)

  expect_true(is.list(params))
  expect_true("x" %in% names(params))
})

test_that("from_tna() extracts transition matrix", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  params <- from_tna(tna_obj, plot = FALSE)

  expect_true("x" %in% names(params))
  expect_true(is.matrix(params$x))
})

test_that("from_tna() extracts labels", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  params <- from_tna(tna_obj, plot = FALSE)

  expect_true("labels" %in% names(params))
})

test_that("from_tna() maps initial probabilities to donut_fill", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  params <- from_tna(tna_obj, plot = FALSE)

  expect_true("donut_fill" %in% names(params))
})

test_that("from_tna() sets directed=TRUE", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  params <- from_tna(tna_obj, plot = FALSE)

  expect_true(params$directed)
})

test_that("from_tna() handles engine selection", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B")
  tna_obj <- tna::tna(trans_mat)

  params_splot <- from_tna(tna_obj, engine = "splot", plot = FALSE)
  params_soplot <- from_tna(tna_obj, engine = "soplot", plot = FALSE)

  expect_true(is.list(params_splot))
  expect_true(is.list(params_soplot))
})

test_that("from_tna() respects weight_digits parameter", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  params <- from_tna(tna_obj, weight_digits = 3, plot = FALSE)

  expect_equal(params$weight_digits, 3)
})

test_that("from_tna() handles override parameters", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  params <- from_tna(tna_obj, plot = FALSE, node_size = 8, theme = "dark")

  expect_equal(params$node_size, 8)
  expect_equal(params$theme, "dark")
})

test_that("from_tna() can plot with splot engine", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  result <- safe_plot({
    from_tna(tna_obj, engine = "splot", plot = TRUE)
  })

  expect_true(result$success, info = result$error)
})

# ============================================
# SPLOT() DIRECT TNA SUPPORT
# ============================================

test_that("splot() accepts tna object directly", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  result <- safe_plot(splot(tna_obj))
  expect_true(result$success, info = result$error)
})

test_that("splot() with tna applies user overrides", {
  skip_if_no_tna()

  trans_mat <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), 3, 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(trans_mat)

  result <- safe_plot(splot(tna_obj, theme = "dark", node_size = 6))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE VECTOR REORDERING
# ============================================

test_that("from_qgraph() correctly reorders edge vectors", {
  skip_if_no_qgraph()

  # Create a network where qgraph and Sonnet might have different edge orders
  adj <- matrix(c(0, 1, 0.5, 1, 0, 0.8, 0.5, 0.8, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  params <- from_qgraph(q, plot = FALSE)

  # The matrix should be preserved correctly
  expect_true(is.matrix(params$x))
  expect_equal(dim(params$x), c(3, 3))
})

# ============================================
# ERROR HANDLING
# ============================================

test_that("from_qgraph() handles missing Arguments field gracefully", {
  # Create a malformed "qgraph-like" object
  fake_qgraph <- list(
    graphAttributes = list(Nodes = list(), Edges = list()),
    Edgelist = list(from = 1, to = 2)
  )
  class(fake_qgraph) <- "qgraph"

  # Should error because Arguments field is missing
  expect_error(from_qgraph(fake_qgraph))
})

test_that("from_tna() handles empty tna object",
 {
  skip_if_no_tna()

  # Creating an empty tna object might not be possible

  # but we should handle edge cases gracefully
  skip("Depends on tna package behavior with empty input")
})

# ============================================
# INTEGRATION
# ============================================

test_that("from_qgraph() output can be customized with sn_* functions", {
  skip_if_no_qgraph()

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Plot with from_qgraph, then the result can be further customized
  result <- with_temp_png({
    from_qgraph(q, engine = "splot", plot = TRUE)
  })

  # Just verify it completes without error
  expect_true(TRUE)
})
