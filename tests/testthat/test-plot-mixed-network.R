# Tests for plot_mixed_network() function
# Combines symmetric (undirected) and asymmetric (directed) matrices

# ============================================
# Basic Functionality Tests
# ============================================

test_that("plot_mixed_network works with basic matrices", {
  sym <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  sym[1, 2] <- sym[2, 1] <- 0.5
  sym[3, 4] <- sym[4, 3] <- 0.6

  asym <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  asym[1, 3] <- 0.7
  asym[3, 1] <- 0.3
  asym[2, 4] <- 0.8

  expect_no_error(with_temp_png(plot_mixed_network(sym, asym)))
})

test_that("plot_mixed_network returns invisibly", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.5

  result <- with_temp_png(plot_mixed_network(sym, asym))

  expect_type(result, "list")
  expect_true("edges" %in% names(result))
  expect_true("sym_matrix" %in% names(result))
  expect_true("asym_matrix" %in% names(result))
})

test_that("plot_mixed_network works with title", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.5

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, title = "Test Network")
  ))
})

test_that("plot_mixed_network works with numeric node names", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[2, 3] <- 0.5

  # No dimnames - should auto-generate 1, 2, 3

expect_no_error(with_temp_png(plot_mixed_network(sym, asym)))
})

# ============================================
# Input Validation Tests
# ============================================

test_that("plot_mixed_network validates matrix inputs", {
  # Not a matrix
  expect_error(
    plot_mixed_network("not a matrix", matrix(0, 2, 2)),
    "must be matrices"
  )

  expect_error(
    plot_mixed_network(matrix(0, 2, 2), "not a matrix"),
    "must be matrices"
  )

  expect_error(
    plot_mixed_network(data.frame(a = 1), matrix(0, 2, 2)),
    "must be matrices"
  )
})

test_that("plot_mixed_network requires matching dimensions", {
  sym <- matrix(0, 2, 2)
  asym <- matrix(0, 3, 3)

  expect_error(
    plot_mixed_network(sym, asym),
    "same dimensions"
  )
})

test_that("plot_mixed_network errors on empty matrices", {
  sym <- matrix(0, 3, 3)
  asym <- matrix(0, 3, 3)

  expect_error(
    with_temp_png(plot_mixed_network(sym, asym)),
    "No edges found"
  )
})

# ============================================
# Color Parameter Tests
# ============================================

test_that("plot_mixed_network respects sym_color parameter", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.5

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, sym_color = "blue")
  ))

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, sym_color = "#FF5500")
  ))
})

test_that("plot_mixed_network respects asym_color parameter", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.7
  asym[3, 1] <- 0.3  # Reciprocal

  # Single color
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, asym_color = "red")
  ))

  # Two colors for reciprocal pairs
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, asym_color = c("green", "purple"))
  ))
})

# ============================================
# Layout Parameter Tests
# ============================================

test_that("plot_mixed_network works with different layouts", {
  sym <- matrix(0, 4, 4)
  sym[1, 2] <- sym[2, 1] <- 0.5
  sym[3, 4] <- sym[4, 3] <- 0.6

  asym <- matrix(0, 4, 4)
  asym[1, 3] <- 0.5

  # Circle layout
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, layout = "circle")
  ))

  # Spring layout (requires igraph)
  skip_if_no_igraph()
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, layout = "spring")
  ))
})

test_that("plot_mixed_network works with custom layout matrix", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.5

  custom_layout <- data.frame(
    x = c(0, 1, 0.5),
    y = c(0, 0, 1)
  )

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, layout = custom_layout)
  ))
})

# ============================================
# Curvature Parameter Tests
# ============================================

test_that("plot_mixed_network respects curvature parameter", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.7
  asym[3, 1] <- 0.3

  # Low curvature
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, curvature = 0.1)
  ))

  # High curvature
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, curvature = 0.5)
  ))

  # Zero curvature (straight directed edges)
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, curvature = 0)
  ))
})

# ============================================
# Edge Width and Node Size Tests
# ============================================

test_that("plot_mixed_network respects edge_width parameter", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.5

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, edge_width = 1)
  ))

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, edge_width = 5)
  ))
})

test_that("plot_mixed_network respects node_size parameter", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.5

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, node_size = 5)
  ))

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, node_size = 15)
  ))
})

# ============================================
# Edge Labels Tests
# ============================================

test_that("plot_mixed_network respects edge_labels parameter", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.5

  # With labels
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, edge_labels = TRUE)
  ))

  # Without labels
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, edge_labels = FALSE)
  ))
})

# ============================================
# Edge Case Tests
# ============================================

test_that("plot_mixed_network handles only symmetric edges", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5
  sym[2, 3] <- sym[3, 2] <- 0.6

  asym <- matrix(0, 3, 3)  # Empty asymmetric

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym)
  ))
})

test_that("plot_mixed_network handles only asymmetric edges", {
  sym <- matrix(0, 3, 3)  # Empty symmetric

  asym <- matrix(0, 3, 3)
  asym[1, 2] <- 0.5
  asym[2, 3] <- 0.6

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym)
  ))
})

test_that("plot_mixed_network handles reciprocal asymmetric edges", {
  sym <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))

  asym <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  # Reciprocal pair A <-> C
  asym[1, 3] <- 0.7
  asym[3, 1] <- 0.3
  # Single direction B -> D
  asym[2, 4] <- 0.8

  result <- with_temp_png(plot_mixed_network(sym, asym))

  # Check edges were captured correctly
  expect_true(nrow(result$edges) >= 3)  # At least 3 directed edges
})

test_that("plot_mixed_network handles large network", {
  skip_on_cran()

  n <- 20
  sym <- matrix(0, n, n)
  # Add some symmetric edges
  for (i in 1:(n - 1)) {
    if (runif(1) > 0.7) {
      sym[i, i + 1] <- sym[i + 1, i] <- runif(1)
    }
  }

  asym <- matrix(0, n, n)
  # Add some asymmetric edges
  set.seed(42)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j && runif(1) > 0.9) {
        asym[i, j] <- runif(1)
      }
    }
  }

  # Ensure at least one edge exists
  if (sum(sym) == 0 && sum(asym) == 0) {
    sym[1, 2] <- sym[2, 1] <- 0.5
  }

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym),
    width = 400, height = 400
  ))
})

test_that("plot_mixed_network with 2 nodes", {
  sym <- matrix(0, 2, 2)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 2, 2)

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym)
  ))
})

# ============================================
# Passthrough Argument Tests
# ============================================

test_that("plot_mixed_network passes additional arguments to splot", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.5

  # Test theme passthrough
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, theme = "minimal")
  ))

  # Test node_fill passthrough (note: node_color is not a valid parameter)
  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym, node_fill = "red")
  ))
})

# ============================================
# Weight Value Tests
# ============================================

test_that("plot_mixed_network handles negative weights", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- -0.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- -0.3

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym)
  ))
})

test_that("plot_mixed_network handles mixed positive/negative weights", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5
  sym[2, 3] <- sym[3, 2] <- -0.3

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.7
  asym[3, 1] <- -0.4

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym)
  ))
})

test_that("plot_mixed_network handles very small weights", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.001

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.0001

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym)
  ))
})

test_that("plot_mixed_network handles weights > 1", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 2.5

  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 10

  expect_no_error(with_temp_png(
    plot_mixed_network(sym, asym)
  ))
})
