# Tests for R/render-nodes.R - Additional coverage tests (Series 41)
# Targets uncovered code paths in render_nodes_grid and render_node_labels_grid
# Improves coverage from 92% by testing edge cases and alternative branches

# ============================================
# Helper Functions for Mock Objects (Extended)
# ============================================

#' Create a mock CographNetwork R6 object for testing
#' @param n Number of nodes
#' @param include_layout Include layout coordinates?
#' @param custom_aes Custom node aesthetics (list)
#' @param custom_theme Custom theme object or NULL
#' @param use_labels_column Use 'labels' instead of 'label' column
create_mock_network_ext <- function(n = 3, include_layout = TRUE,
                                     custom_aes = list(), custom_theme = NULL,
                                     use_labels_column = FALSE) {
  # Create nodes data frame
  nodes <- data.frame(
    id = seq_len(n),
    stringsAsFactors = FALSE
  )

  # Choose column name for labels

if (use_labels_column && n > 0) {
    nodes$labels <- LETTERS[seq_len(n)]
  } else if (n > 0) {
    nodes$label <- LETTERS[seq_len(n)]
  }

  if (include_layout && n > 0) {
    # Circle layout
    if (n == 1) {
      nodes$x <- 0.5
      nodes$y <- 0.5
    } else {
      angles <- seq(0, 2 * pi * (1 - 1/n), length.out = n)
      nodes$x <- 0.5 + 0.3 * cos(angles)
      nodes$y <- 0.5 + 0.3 * sin(angles)
    }
  }

  # Create edges data frame
  edges <- data.frame(
    from = integer(0),
    to = integer(0),
    weight = numeric(0)
  )
  if (n >= 2) {
    edges <- data.frame(
      from = seq_len(n - 1),
      to = seq(2, n),
      weight = rep(1, n - 1)
    )
  }

  # Build default aesthetics
  default_aes <- list(
    size = 0.05,
    shape = "circle",
    fill = "#4A90D9",
    border_color = "#2C5AA0",
    border_width = 1,
    alpha = 1,
    label_size = 10,
    label_color = "black",
    label_position = "center"
  )

  # Merge custom aesthetics
  node_aes <- utils::modifyList(default_aes, custom_aes)

  # Default theme
  if (is.null(custom_theme)) {
    custom_theme <- CographTheme$new()
  }

  # Create mock R6 object
  mock_network <- list(
    get_nodes = function() nodes,
    get_node_aes = function() node_aes,
    get_theme = function() custom_theme
  )

  class(mock_network) <- "CographNetwork"
  mock_network
}

# Make internal functions available
render_nodes_grid <- cograph:::render_nodes_grid
render_node_labels_grid <- cograph:::render_node_labels_grid
expand_param <- cograph:::expand_param

# ============================================
# Donut Shape Override with Short donut_shape Vector
# Tests line 99: when donut_shape length < i, falls back to [1]
# ============================================

test_that("render_nodes_grid handles donut_shape with single value for multiple nodes", {
  # donut_shape has length 1, but we have 3 nodes
  # This tests the fallback: if length(aes$donut_shape) >= i else aes$donut_shape[1]
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "circle",  # Not a donut shape, so donut_values triggers override
    donut_values = list(0.5, 0.75, 1.0),
    donut_shape = "square"  # Single value, not length 3
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_true(length(result) >= 3)  # gList may contain multiple grobs per node
})

test_that("render_nodes_grid handles donut_shape with length 2 for 4 nodes", {
  # donut_shape has length 2, but we have 4 nodes
  # Nodes 3 and 4 will fall back to donut_shape[1]
  net <- create_mock_network_ext(n = 4, custom_aes = list(
    shape = "circle",
    donut_values = list(0.5, 0.6, 0.7, 0.8),
    donut_shape = c("hexagon", "triangle")  # Length 2 < 4 nodes
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_true(length(result) >= 4)  # gList may contain multiple grobs per node
})

# ============================================
# Donut Colors as Vector (not list) when has_donut_value
# Tests lines 119-120: !is.list(aes$donut_colors) branch
# ============================================

test_that("render_nodes_grid handles donut_colors as vector with donut_values override", {
  # donut_colors is a character vector, not a list
  # This triggers line 120: extra_args$colors <- aes$donut_colors[1]
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "square",  # Non-donut shape
    donut_values = list(0.5, 0.75, 1.0),  # Triggers donut override
    donut_colors = c("red", "green", "blue")  # Vector, not list
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles single-value donut_colors with override", {
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "triangle",
    donut_values = list(0.4, 0.6, 0.8),
    donut_colors = "purple"  # Single color
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Numeric pie_values (not list or matrix)
# Tests lines 143-145: else if (is.numeric(aes$pie_values))
# ============================================

test_that("render_nodes_grid handles numeric vector pie_values for donut", {
  # When pie_values is a plain numeric vector, each node gets pie_values[i]
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut",
    pie_values = c(0.3, 0.6, 0.9)  # Numeric vector
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles numeric pie_values for pie shape", {
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "pie",
    pie_values = c(1, 2, 3)  # Single value per node
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Donut Shape Without donut_values (Default to 1.0)
# Tests lines 161-163: default value for explicit donut shapes
# ============================================

test_that("render_nodes_grid defaults to 1.0 for donut without donut_values", {
  # Explicit donut shape but no donut_values - should default to 1.0
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut"
    # No donut_values provided
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles polygon_donut without donut_values", {
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "polygon_donut",
    donut_shape = "hexagon"
    # No donut_values - defaults to 1.0
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut with partial donut_values", {
  # Only first node has a donut value, others should default to 1.0
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5)  # Only one value for 3 nodes
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Donut Colors - List with Short Length vs Vector (for explicit donut shape)
# Tests lines 167-171 in the donut/polygon_donut block
# ============================================

test_that("render_nodes_grid handles donut_colors list shorter than n for explicit donut", {
  # donut_colors list has length 2 but we have 3 nodes
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_colors = list("red", "green")  # List length 2 < 3
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_colors vector for explicit donut", {
  # Non-list donut_colors for explicit donut shape
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_colors = c("red", "green", "blue")  # Vector, not list
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Vectorized donut_shape Fallback for Explicit Donut
# Tests lines 176-182: length(aes$donut_shape) >= i else [1]
# ============================================

test_that("render_nodes_grid handles short donut_shape for polygon_donut", {
  # donut_shape length 2 for 4 nodes
  net <- create_mock_network_ext(n = 4, custom_aes = list(
    shape = "polygon_donut",
    donut_values = list(0.5, 0.6, 0.7, 0.8),
    donut_shape = c("square", "triangle")  # Length 2 < 4
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_true(length(result) >= 4)  # gList may contain multiple grobs per node
})

test_that("render_nodes_grid handles single donut_shape for polygon_donut", {
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "polygon_donut",
    donut_values = list(0.5, 0.6, 0.7),
    donut_shape = "pentagon"  # Single value
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# donut_pie with Matrix pie_values
# Tests lines 217-219
# ============================================

test_that("render_nodes_grid handles donut_pie with matrix pie_values", {
  # pie_values as matrix (n rows, k cols for k pie segments)
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut_pie",
    donut_values = c(0.5, 0.75, 1.0),
    pie_values = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_pie with scalar donut_values", {
  # Use scalar donut_value per node (not list)
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut_pie",
    donut_values = c(0.5, 0.75, 1.0),  # Vector of scalars
    pie_values = list(c(1, 2), c(2, 3), c(1, 1))  # Required for donut_pie
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# double_donut_pie with Non-list Values
# Tests lines 244-246, 251-253, 259-262, 265-268
# ============================================

test_that("render_nodes_grid handles double_donut_pie with vector donut_values", {
  # donut_values as numeric vector, not list
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = c(0.5, 0.75, 1.0),  # Vector
    donut2_values = list(0.3, 0.5, 0.8),
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles double_donut_pie with vector donut_colors", {
  # donut_colors as vector, not list
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.75, 1.0),
    donut_colors = c("red", "green", "blue"),  # Vector
    donut2_values = list(0.3, 0.5, 0.8),
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles double_donut_pie with vector donut2_values", {
  # donut2_values as vector, not list
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.75, 1.0),
    donut2_values = c(0.3, 0.5, 0.8),  # Vector
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles double_donut_pie with vector donut2_colors", {
  # donut2_colors as vector, not list
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.75, 1.0),
    donut2_values = list(0.3, 0.5, 0.8),
    donut2_colors = c("orange", "purple", "cyan"),  # Vector
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles double_donut_pie with all vector values", {
  # All values as vectors, not lists
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = c(0.5, 0.75, 1.0),
    donut_colors = c("red", "green", "blue"),
    donut2_values = c(0.3, 0.5, 0.8),
    donut2_colors = c("orange", "purple", "cyan"),
    pie_values = list(c(1, 2), c(3, 1), c(1, 1)),  # List required for variable segments
    pie_colors = c("pink", "lightgreen")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# double_donut_pie with Matrix pie_values
# Tests lines 271-276
# ============================================

test_that("render_nodes_grid handles double_donut_pie with matrix pie_values", {
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.75, 1.0),
    donut2_values = list(0.3, 0.5, 0.8),
    pie_values = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),  # Matrix
    pie_colors = c("pink", "lightgreen")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Label Column: 'labels' vs 'label'
# Tests lines 333-334: nodes$labels takes priority
# ============================================

test_that("render_node_labels_grid uses labels column when available", {
  # Create network with 'labels' column (plural)
  net <- create_mock_network_ext(n = 3, use_labels_column = TRUE)

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 3)
})

test_that("render_node_labels_grid falls back to indices when no label columns", {
  # Create network without any label columns
  net <- create_mock_network_ext(n = 3)
  nodes <- net$get_nodes()
  nodes$label <- NULL
  nodes$labels <- NULL
  net$get_nodes <- function() nodes

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 3)
})

# ============================================
# Unknown Fontface Fallback
# Tests line 401: default case in switch
# ============================================

test_that("render_node_labels_grid defaults to plain for unknown fontface", {
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    label_fontface = "unknown_fontface_xyz"
  ))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 3)
})

test_that("render_node_labels_grid handles mixed fontfaces including unknown", {
  net <- create_mock_network_ext(n = 4, custom_aes = list(
    label_fontface = c("bold", "italic", "crazy", "bold.italic")
  ))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 4)
})

# ============================================
# Additional Edge Cases for Coverage
# ============================================

test_that("render_nodes_grid handles donut with vectorized value formatting params", {
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_show_value = TRUE,
    donut_value_color = c("red", "green", "blue"),
    donut_value_fontface = c("bold", "italic", "plain"),
    donut_value_fontfamily = c("sans", "serif", "mono")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles default node size calculation", {
  # Test default node size using COGRAPH_SCALE constants
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    size = NULL  # Use default
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles NULL aes values with theme fallback", {
  custom_theme <- CographTheme$new(
    node_fill = "coral",
    node_border = "navy",
    node_border_width = 2
  )
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    fill = NULL,
    border_color = NULL,
    border_width = NULL
  ), custom_theme = custom_theme)

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles all NULL font properties", {
  custom_theme <- CographTheme$new(
    label_size = 12,
    label_color = "darkblue"
  )
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    label_size = NULL,
    label_color = NULL,
    label_position = NULL,
    label_fontface = NULL,
    label_fontfamily = NULL,
    label_hjust = NULL,
    label_vjust = NULL,
    label_angle = NULL
  ), custom_theme = custom_theme)

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Complex Combined Scenarios
# ============================================

test_that("render_nodes_grid handles mixed shapes with various parameters", {
  # Test a simpler mix of shapes that don't have conflicting parameter requirements
  net <- create_mock_network_ext(n = 4, custom_aes = list(
    shape = c("circle", "square", "triangle", "hexagon"),
    size = c(0.03, 0.05, 0.04, 0.06),
    fill = c("red", "green", "blue", "orange"),
    alpha = c(0.8, 0.9, 1.0, 0.7)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_true(length(result) >= 4)
})

test_that("render_node_labels_grid handles all label positions in one network", {
  net <- create_mock_network_ext(n = 5, custom_aes = list(
    label_position = c("center", "above", "below", "left", "right"),
    label_size = c(8, 10, 12, 10, 8),
    label_fontface = c("plain", "bold", "italic", "bold.italic", "plain")
  ))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 5)
})

# ============================================
# expand_param Edge Cases
# ============================================

test_that("expand_param rejects NULL input", {
  # NULL has length 0, which is not 1 or n, so it should error
  expect_error(
    expand_param(NULL, 3, "test"),
    "test must be length 1 or 3"
  )
})

test_that("expand_param handles zero-length target", {
  result <- expand_param("circle", 0, "shape")
  expect_equal(length(result), 0)
})

test_that("expand_param handles NA values", {
  result <- expand_param(NA, 3, "test")
  expect_equal(result, rep(NA, 3))
})

test_that("expand_param preserves numeric type", {
  result <- expand_param(5.5, 3, "size")
  expect_true(is.numeric(result))
  expect_equal(result, rep(5.5, 3))
})

test_that("expand_param preserves logical type", {
  result <- expand_param(TRUE, 3, "flag")
  expect_true(is.logical(result))
  expect_equal(result, rep(TRUE, 3))
})

# ============================================
# Donut Override Circle Path (line 108-109)
# ============================================

test_that("render_nodes_grid uses regular donut for circle donut_shape", {
  # When donut_shape is "circle" (or NULL default), use regular donut shape
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "square",  # Non-donut triggers override
    donut_values = list(0.5, 0.75, 1.0),
    donut_shape = "circle"  # Explicitly circle
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_values with NULL donut_shape", {
  # When donut_shape is NULL, defaults to "circle"
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "triangle",
    donut_values = list(0.5, 0.75, 1.0)
    # donut_shape is NULL - should default to circle
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Additional donut_pie Parameter Tests
# ============================================

test_that("render_nodes_grid handles donut_pie without optional parameters", {
  # Minimal donut_pie - no colors, ratios, etc.
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut_pie"
    # No donut_values, pie_values, or other parameters
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_pie with short donut_values vector", {
  # donut_values shorter than n
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut_pie",
    donut_values = c(0.5, 0.7),  # Only 2 values for 3 nodes
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# double_donut_pie Partial Parameter Tests
# ============================================

test_that("render_nodes_grid handles double_donut_pie without donut2_values", {
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.75, 1.0),
    # No donut2_values
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles double_donut_pie with full list values", {
  # Lists with full length
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.75, 0.9),
    donut_colors = list("red", "green", "blue"),
    donut2_values = list(0.3, 0.4, 0.5),
    donut2_colors = list("orange", "purple", "cyan"),
    pie_values = list(c(1, 2), c(2, 1), c(1, 1)),
    pie_colors = c("pink", "lightgreen")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Theme Border Width Default
# ============================================

test_that("render_nodes_grid uses theme node_border_width", {
  custom_theme <- CographTheme$new(node_border_width = 3)
  net <- create_mock_network_ext(n = 3,
                                  custom_aes = list(border_width = NULL),
                                  custom_theme = custom_theme)

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Large Network Stress Tests
# ============================================

test_that("render_nodes_grid handles larger networks efficiently", {
  net <- create_mock_network_ext(n = 50)

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 50)
})

test_that("render_node_labels_grid handles larger networks", {
  net <- create_mock_network_ext(n = 50)

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 50)
})

# ============================================
# Donut Shape Index Boundary Tests
# ============================================

test_that("render_nodes_grid handles donut_colors list with exact length match", {
  # List has exactly n elements
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_colors = list("red", "green", "blue")  # Exact length 3
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_shape with exact length match", {
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "polygon_donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_shape = c("square", "hexagon", "triangle")  # Exact length 3
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Special Single-Node Cases
# ============================================

test_that("render_nodes_grid handles single node with all donut params", {
  net <- create_mock_network_ext(n = 1, custom_aes = list(
    shape = "donut",
    donut_values = list(0.75),
    donut_colors = list("coral"),
    donut_inner_ratio = 0.4,
    donut_bg_color = "gray95",
    donut_show_value = TRUE,
    donut_value_size = 12,
    donut_value_color = "darkblue",
    donut_value_digits = 2,
    donut_value_prefix = "$",
    donut_value_suffix = "k",
    donut_border_width = 2
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_true(length(result) >= 1)
})

test_that("render_nodes_grid handles single node double_donut_pie", {
  net <- create_mock_network_ext(n = 1, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = list(0.8),
    donut_colors = list("red"),
    donut2_values = list(0.5),
    donut2_colors = list("blue"),
    pie_values = list(c(1, 2, 1)),
    pie_colors = c("pink", "lightgreen", "lightblue")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_true(length(result) >= 1)
})

# ============================================
# Has donut_value with shapes that don't trigger override
# Line 96: shapes that are already donut-family don't get overridden
# ============================================

test_that("render_nodes_grid does not override pie shape with donut_values", {
  # pie shape should NOT be overridden by donut_values
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "pie",
    donut_values = list(0.5, 0.75, 1.0),  # This should be ignored for pie
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid does not override donut_pie shape with donut_values", {
  # donut_pie shape should NOT be overridden
  net <- create_mock_network_ext(n = 3, custom_aes = list(
    shape = "donut_pie",
    donut_values = c(0.5, 0.75, 1.0),
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})
