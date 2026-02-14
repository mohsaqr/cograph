# Tests for R/render-nodes.R
# Coverage for render_nodes_grid and render_node_labels_grid functions
# Tests for node rendering, shape handling, and styling

# ============================================
# Helper Functions for Mock Objects
# ============================================

#' Create a mock CographNetwork R6 object for testing
#' @param n Number of nodes
#' @param include_layout Include layout coordinates?
#' @param custom_aes Custom node aesthetics (list)
#' @param custom_theme Custom theme object or NULL
create_mock_network <- function(n = 3, include_layout = TRUE,
                                 custom_aes = list(), custom_theme = NULL) {
  # Create nodes data frame
  nodes <- data.frame(
    id = seq_len(n),
    label = LETTERS[seq_len(n)],
    stringsAsFactors = FALSE
  )

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

#' Create a mock network with donut values
create_mock_donut_network <- function(n = 3, donut_values = NULL, donut_colors = NULL) {
  aes <- list()
  if (!is.null(donut_values)) {
    aes$donut_values <- donut_values
  }
  if (!is.null(donut_colors)) {
    aes$donut_colors <- donut_colors
  }
  create_mock_network(n, custom_aes = aes)
}

#' Create a mock network with pie values
create_mock_pie_network <- function(n = 3, pie_values = NULL, pie_colors = NULL) {
  aes <- list(shape = "pie")
  if (!is.null(pie_values)) {
    aes$pie_values <- pie_values
  }
  if (!is.null(pie_colors)) {
    aes$pie_colors <- pie_colors
  }
  create_mock_network(n, custom_aes = aes)
}

# Make internal functions available
render_nodes_grid <- cograph:::render_nodes_grid
render_node_labels_grid <- cograph:::render_node_labels_grid
expand_param <- cograph:::expand_param

# ============================================
# Basic render_nodes_grid Tests
# ============================================

test_that("render_nodes_grid returns gList for basic network", {
  net <- create_mock_network(n = 3)

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid returns empty gList for zero nodes", {
  net <- create_mock_network(n = 0)

  result <- render_nodes_grid(net)

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 0)
})

test_that("render_nodes_grid handles single node network", {
  net <- create_mock_network(n = 1)

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 1)
})

test_that("render_nodes_grid handles 5 nodes", {
  net <- create_mock_network(n = 5)

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 5)
})

test_that("render_nodes_grid handles 10 nodes", {
  net <- create_mock_network(n = 10)

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 10)
})

# ============================================
# Shape Tests
# ============================================

test_that("render_nodes_grid works with circle shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "circle"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with square shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "square"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with triangle shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "triangle"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with diamond shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "diamond"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with hexagon shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "hexagon"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with ellipse shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "ellipse"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with star shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "star"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with heart shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "heart"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid falls back to circle for unknown shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "nonexistent_shape_xyz"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with mixed shapes (vectorized)", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = c("circle", "square", "triangle")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 3)
})

# ============================================
# Pie Shape Tests
# ============================================

test_that("render_nodes_grid works with pie shape and list values", {
  net <- create_mock_pie_network(
    n = 3,
    pie_values = list(c(1, 2), c(3, 1), c(1, 1, 1))
  )

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with pie shape and matrix values", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "pie",
    pie_values = matrix(c(1, 2, 3, 2, 1, 2), nrow = 3, ncol = 2)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with pie shape and custom colors", {
  net <- create_mock_pie_network(
    n = 3,
    pie_values = list(c(1, 2), c(3, 1), c(1, 1, 1)),
    pie_colors = c("red", "blue", "green")
  )

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with pie_border_width parameter", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "pie",
    pie_values = list(c(1, 2), c(3, 1), c(1, 1)),
    pie_border_width = 2
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Donut Shape Tests
# ============================================

test_that("render_nodes_grid works with donut shape", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid converts regular shape to donut when donut_values provided", {
  net <- create_mock_donut_network(
    n = 3,
    donut_values = list(0.3, 0.6, 0.9)
  )

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_inner_ratio parameter", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_inner_ratio = 0.6
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_bg_color parameter", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_bg_color = "lightblue"
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut with show_value", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_show_value = TRUE
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_value_prefix and suffix", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_show_value = TRUE,
    donut_value_prefix = "$",
    donut_value_suffix = "%"
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_border_width parameter", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_border_width = 3
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Polygon Donut Tests
# ============================================

test_that("render_nodes_grid handles polygon_donut shape", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "polygon_donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_shape = "hexagon"
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid uses polygon_donut for non-circle donut_shape", {
  net <- create_mock_network(n = 3, custom_aes = list(
    donut_values = list(0.5, 0.75, 1.0),
    donut_shape = "square"
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Donut-Pie Combined Tests
# ============================================

test_that("render_nodes_grid handles donut_pie shape", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut_pie",
    donut_values = c(0.5, 0.75, 1.0),
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles double_donut_pie shape", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.75, 1.0),
    donut2_values = list(0.3, 0.5, 0.8),
    pie_values = list(c(1, 2), c(3, 1), c(1, 1))
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Styling Tests
# ============================================

test_that("render_nodes_grid handles per-node sizes", {
  net <- create_mock_network(n = 3, custom_aes = list(
    size = c(0.03, 0.05, 0.07)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles per-node fills", {
  net <- create_mock_network(n = 3, custom_aes = list(
    fill = c("red", "green", "blue")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles per-node border colors", {
  net <- create_mock_network(n = 3, custom_aes = list(
    border_color = c("darkred", "darkgreen", "darkblue")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles per-node border widths", {
  net <- create_mock_network(n = 3, custom_aes = list(
    border_width = c(1, 2, 3)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles per-node alpha", {
  net <- create_mock_network(n = 3, custom_aes = list(
    alpha = c(0.3, 0.6, 1.0)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# render_node_labels_grid Tests
# ============================================

test_that("render_node_labels_grid returns gList for basic network", {
  net <- create_mock_network(n = 3)

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid returns empty gList for zero nodes", {
  net <- create_mock_network(n = 0)

  result <- render_node_labels_grid(net)

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 0)
})

test_that("render_node_labels_grid returns empty when show_labels is FALSE", {
  net <- create_mock_network(n = 3, custom_aes = list(show_labels = FALSE))

  result <- render_node_labels_grid(net)

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 0)
})

test_that("render_node_labels_grid handles label_position = above", {
  net <- create_mock_network(n = 3, custom_aes = list(label_position = "above"))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles label_position = below", {
  net <- create_mock_network(n = 3, custom_aes = list(label_position = "below"))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles label_position = left", {
  net <- create_mock_network(n = 3, custom_aes = list(label_position = "left"))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles label_position = right", {
  net <- create_mock_network(n = 3, custom_aes = list(label_position = "right"))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles per-node label positions", {
  net <- create_mock_network(n = 3, custom_aes = list(
    label_position = c("above", "center", "below")
  ))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles label_fontface = bold", {
  net <- create_mock_network(n = 3, custom_aes = list(label_fontface = "bold"))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles label_fontface = italic", {
  net <- create_mock_network(n = 3, custom_aes = list(label_fontface = "italic"))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles label_fontface = bold.italic", {
  net <- create_mock_network(n = 3, custom_aes = list(label_fontface = "bold.italic"))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles label_angle", {
  net <- create_mock_network(n = 3, custom_aes = list(label_angle = 45))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles label_hjust and label_vjust", {
  net <- create_mock_network(n = 3, custom_aes = list(
    label_hjust = 0,
    label_vjust = 1
  ))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles per-node label colors", {
  net <- create_mock_network(n = 3, custom_aes = list(
    label_color = c("red", "green", "blue")
  ))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles per-node label sizes", {
  net <- create_mock_network(n = 3, custom_aes = list(
    label_size = c(8, 10, 12)
  ))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid handles label_fontfamily", {
  net <- create_mock_network(n = 3, custom_aes = list(label_fontfamily = "serif"))

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid creates labels from node ids when no label column", {
  net <- create_mock_network(n = 3)
  # Remove label column
  nodes <- net$get_nodes()
  nodes$label <- NULL
  net$get_nodes <- function() nodes

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# expand_param Tests
# ============================================

test_that("expand_param expands single value to length n", {
  result <- expand_param("circle", 5, "shape")
  expect_equal(result, rep("circle", 5))
})
test_that("expand_param returns vector unchanged when length matches n", {
  result <- expand_param(c(1, 2, 3), 3, "size")
  expect_equal(result, c(1, 2, 3))
})

test_that("expand_param errors when length is not 1 or n", {
  expect_error(
    expand_param(c(1, 2), 5, "size"),
    "size must be length 1 or 5"
  )
})

# ============================================
# AI-Themed Shape Tests
# ============================================

test_that("render_nodes_grid works with neural shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "neural"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with chip shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "chip"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with robot shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "robot"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with brain shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "brain"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with network shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "network"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with database shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "database"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with cloud shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "cloud"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with gear shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "gear"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with cross shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "cross"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with pentagon shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "pentagon"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with none shape (invisible)", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "none"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid works with rectangle shape", {
  net <- create_mock_network(n = 3, custom_aes = list(shape = "rectangle"))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Donut Colors Tests (List vs Vector)
# ============================================

test_that("render_nodes_grid handles donut_colors as list", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_colors = list("red", "green", "blue")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_colors as vector", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_colors = c("red", "green", "blue")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Vectorized Donut Parameters Tests
# ============================================

test_that("render_nodes_grid handles vectorized donut_inner_ratio", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_inner_ratio = c(0.3, 0.5, 0.7)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles vectorized donut_bg_color", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_bg_color = c("gray80", "gray85", "gray90")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles vectorized donut_show_value", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_show_value = c(TRUE, FALSE, TRUE)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles vectorized donut_value_size", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_show_value = TRUE,
    donut_value_size = c(6, 8, 10)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_value_digits", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.333, 0.666, 0.999),
    donut_show_value = TRUE,
    donut_value_digits = 1
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_value_format function", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_show_value = TRUE,
    donut_value_format = function(x) paste0(round(x * 100), "%")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles vectorized donut_shape", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "polygon_donut",
    donut_values = list(0.5, 0.75, 1.0),
    donut_shape = c("square", "hexagon", "triangle")
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Double Donut-Pie Complex Tests
# ============================================

test_that("render_nodes_grid handles double_donut_pie with all parameters", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.75, 1.0),
    donut_colors = list("red", "green", "blue"),
    donut2_values = list(0.3, 0.5, 0.8),
    donut2_colors = list("orange", "purple", "cyan"),
    pie_values = list(c(1, 2), c(3, 1), c(1, 1)),
    pie_colors = c("pink", "lightgreen"),
    donut_inner_ratio = 0.7,
    donut2_inner_ratio = 0.4,
    donut_bg_color = "gray95"
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles donut_pie with custom border widths", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut_pie",
    donut_values = c(0.5, 0.75, 1.0),
    pie_values = list(c(1, 2), c(3, 1), c(1, 1)),
    pie_border_width = 1,
    donut_border_width = 2
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Theme Integration Tests
# ============================================

test_that("render_nodes_grid uses theme defaults for fill", {
  custom_theme <- CographTheme$new(node_fill = "coral")
  net <- create_mock_network(n = 3, custom_aes = list(fill = NULL), custom_theme = custom_theme)

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid uses theme defaults for border", {
  custom_theme <- CographTheme$new(node_border = "navy")
  net <- create_mock_network(n = 3, custom_aes = list(border_color = NULL), custom_theme = custom_theme)

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid uses theme defaults for label color", {
  custom_theme <- CographTheme$new(label_color = "darkred")
  net <- create_mock_network(n = 3, custom_aes = list(label_color = NULL), custom_theme = custom_theme)

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_node_labels_grid uses theme defaults for label size", {
  custom_theme <- CographTheme$new(label_size = 14)
  net <- create_mock_network(n = 3, custom_aes = list(label_size = NULL), custom_theme = custom_theme)

  result <- with_temp_png({
    render_node_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Edge Case Tests
# ============================================

test_that("render_nodes_grid handles NA in donut_values", {
  net <- create_mock_network(n = 3, custom_aes = list(
    donut_values = list(0.5, NA, 1.0)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles NULL in donut_values list", {
  net <- create_mock_network(n = 3, custom_aes = list(
    donut_values = list(0.5, NULL, 1.0)
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles empty donut_values list", {
  net <- create_mock_network(n = 3, custom_aes = list(
    shape = "donut",
    donut_values = list()
  ))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles zero alpha (invisible nodes)", {
  net <- create_mock_network(n = 3, custom_aes = list(alpha = 0))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles very small node sizes", {
  net <- create_mock_network(n = 3, custom_aes = list(size = 0.001))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_nodes_grid handles very large node sizes", {
  net <- create_mock_network(n = 3, custom_aes = list(size = 0.5))

  result <- with_temp_png({
    render_nodes_grid(net)
  })

  expect_true(inherits(result, "gList"))
})
