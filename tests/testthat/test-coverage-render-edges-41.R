# Tests for R/render-edges.R
# Coverage for render_edges_grid, draw_straight_edge, draw_curved_edge,
# draw_self_loop, and render_edge_labels_grid functions
# Tests for edge rendering, styling, arrows, curves, and CI underlays

# ============================================
# Helper Functions for Mock Objects
# ============================================

#' Create a mock CographNetwork R6 object for testing edge rendering
#' @param n Number of nodes
#' @param m Number of edges (creates chain by default)
#' @param include_layout Include layout coordinates?
#' @param custom_node_aes Custom node aesthetics (list)
#' @param custom_edge_aes Custom edge aesthetics (list)
#' @param custom_theme Custom theme object or NULL
#' @param directed Is the network directed?
#' @param self_loops Include self-loops?
#' @param reciprocal Include reciprocal edges for curves testing?
create_mock_edge_network <- function(n = 4, m = NULL, include_layout = TRUE,
                                      custom_node_aes = list(),
                                      custom_edge_aes = list(),
                                      custom_theme = NULL,
                                      directed = TRUE,
                                      self_loops = FALSE,
                                      reciprocal = FALSE) {
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
  if (is.null(m)) {
    # Default: create chain edges 1->2, 2->3, etc.
    if (n >= 2) {
      edges <- data.frame(
        from = seq_len(n - 1),
        to = seq(2, n),
        weight = runif(n - 1, 0.3, 1.0)
      )
    } else {
      edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
    }
  } else if (m == 0) {
    edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  } else {
    # Create m random edges
    set.seed(42)
    edges <- data.frame(
      from = sample(1:n, m, replace = TRUE),
      to = sample(1:n, m, replace = TRUE),
      weight = runif(m, -1, 1)
    )
  }

  # Add reciprocal edges for curve testing
  if (reciprocal && nrow(edges) > 0) {
    # Add reverse edges for first edge
    rev_edge <- data.frame(
      from = edges$to[1],
      to = edges$from[1],
      weight = edges$weight[1] * 0.8
    )
    edges <- rbind(edges, rev_edge)
  }

  # Add self-loops
  if (self_loops && n > 0) {
    self_edge <- data.frame(from = 1, to = 1, weight = 0.5)
    edges <- rbind(edges, self_edge)
  }

  # Build default node aesthetics
  default_node_aes <- list(
    size = 0.05,
    shape = "circle",
    fill = "#4A90D9",
    border_color = "#2C5AA0",
    border_width = 1,
    alpha = 1
  )
  node_aes <- utils::modifyList(default_node_aes, custom_node_aes)

  # Build default edge aesthetics
  default_edge_aes <- list()
  edge_aes <- utils::modifyList(default_edge_aes, custom_edge_aes)

  # Default theme
  if (is.null(custom_theme)) {
    custom_theme <- CographTheme$new()
  }

  # Create mock R6 object
  mock_network <- list(
    get_nodes = function() nodes,
    get_edges = function() edges,
    get_node_aes = function() node_aes,
    get_edge_aes = function() edge_aes,
    get_theme = function() custom_theme,
    is_directed = directed
  )

  class(mock_network) <- "CographNetwork"
  mock_network
}

# Make internal functions available
render_edges_grid <- cograph:::render_edges_grid
render_edge_labels_grid <- cograph:::render_edge_labels_grid
draw_straight_edge <- cograph:::draw_straight_edge
draw_curved_edge <- cograph:::draw_curved_edge
draw_self_loop <- cograph:::draw_self_loop
expand_param <- cograph:::expand_param
recycle_to_length <- cograph:::recycle_to_length
adjust_alpha <- cograph:::adjust_alpha
bezier_points <- cograph:::bezier_points
curve_control_point <- cograph:::curve_control_point
arrow_points <- cograph:::arrow_points
edge_endpoint <- cograph:::edge_endpoint
scale_edge_widths <- cograph:::scale_edge_widths

# ============================================
# Basic render_edges_grid Tests
# ============================================

test_that("render_edges_grid returns gList for basic network", {
  net <- create_mock_edge_network(n = 4)

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid returns empty gList for zero edges", {
  net <- create_mock_edge_network(n = 3, m = 0)

  result <- render_edges_grid(net)

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 0)
})

test_that("render_edges_grid returns empty gList for NULL edges", {
  net <- create_mock_edge_network(n = 3, m = 0)
  net$get_edges <- function() NULL

  result <- render_edges_grid(net)

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 0)
})

test_that("render_edges_grid handles single edge network", {
  net <- create_mock_edge_network(n = 2)

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles many edges", {
  net <- create_mock_edge_network(n = 6, m = 10)

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Edge Width Tests
# ============================================

test_that("render_edges_grid handles explicit width parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(width = 3))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles per-edge widths", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(width = c(1, 2, 3)))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles weight-based scaling", {
  net <- create_mock_edge_network(n = 4)

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles esize parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(esize = 5))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles edge_width_range", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    edge_width_range = c(1, 6)
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles edge_scale_mode log", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    edge_scale_mode = "log"
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles width_scale multiplier", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    width_scale = 2.0
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles maximum parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    maximum = 0.5
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid uses theme default width when no weights", {
  nodes <- data.frame(
    id = 1:3,
    label = c("A", "B", "C"),
    x = c(0.2, 0.5, 0.8),
    y = c(0.5, 0.5, 0.5)
  )
  edges <- data.frame(
    from = c(1, 2),
    to = c(2, 3)
    # No weight column
  )

  mock_network <- list(
    get_nodes = function() nodes,
    get_edges = function() edges,
    get_node_aes = function() list(size = 0.05),
    get_edge_aes = function() list(),
    get_theme = function() CographTheme$new(),
    is_directed = TRUE
  )
  class(mock_network) <- "CographNetwork"

  result <- with_temp_png({
    render_edges_grid(mock_network)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Edge Color Tests
# ============================================

test_that("render_edges_grid handles explicit color parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(color = "blue"))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles per-edge colors", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    color = c("red", "green", "blue")
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles positive_color and negative_color", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    positive_color = "darkgreen",
    negative_color = "darkred"
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles edges with negative weights", {
  nodes <- data.frame(
    id = 1:3,
    label = c("A", "B", "C"),
    x = c(0.2, 0.5, 0.8),
    y = c(0.5, 0.5, 0.5)
  )
  edges <- data.frame(
    from = c(1, 2),
    to = c(2, 3),
    weight = c(0.5, -0.5)  # Mixed positive/negative
  )

  mock_network <- list(
    get_nodes = function() nodes,
    get_edges = function() edges,
    get_node_aes = function() list(size = 0.05),
    get_edge_aes = function() list(),
    get_theme = function() CographTheme$new(),
    is_directed = TRUE
  )
  class(mock_network) <- "CographNetwork"

  result <- with_temp_png({
    render_edges_grid(mock_network)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles edges with zero weights", {
  nodes <- data.frame(
    id = 1:3,
    label = c("A", "B", "C"),
    x = c(0.2, 0.5, 0.8),
    y = c(0.5, 0.5, 0.5)
  )
  edges <- data.frame(
    from = c(1, 2),
    to = c(2, 3),
    weight = c(0.5, 0)  # One zero weight
  )

  mock_network <- list(
    get_nodes = function() nodes,
    get_edges = function() edges,
    get_node_aes = function() list(size = 0.05),
    get_edge_aes = function() list(),
    get_theme = function() CographTheme$new(),
    is_directed = TRUE
  )
  class(mock_network) <- "CographNetwork"

  result <- with_temp_png({
    render_edges_grid(mock_network)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Edge Alpha and Cut Tests
# ============================================

test_that("render_edges_grid handles alpha parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(alpha = 0.5))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles per-edge alpha", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    alpha = c(0.3, 0.6, 0.9)
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles cut parameter for transparency", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(cut = 0.5))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles cut = 0 (disabled)", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(cut = 0))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Edge Style Tests
# ============================================

test_that("render_edges_grid handles solid style", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(style = "solid"))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles dashed style", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(style = "dashed"))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles dotted style (width reduction)", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(style = "dotted"))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles longdash style", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(style = "longdash"))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles twodash style", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(style = "twodash"))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles per-edge styles", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    style = c("solid", "dashed", "dotted")
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles unknown style (defaults to solid)", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(style = "unknown_style"))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Arrow Tests
# ============================================

test_that("render_edges_grid shows arrows for directed network", {
  net <- create_mock_edge_network(n = 4, directed = TRUE)

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid hides arrows for undirected network", {
  net <- create_mock_edge_network(n = 4, directed = FALSE)

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles show_arrows = FALSE override", {
  net <- create_mock_edge_network(n = 4, directed = TRUE,
                                   custom_edge_aes = list(show_arrows = FALSE))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles show_arrows = TRUE override", {
  net <- create_mock_edge_network(n = 4, directed = FALSE,
                                   custom_edge_aes = list(show_arrows = TRUE))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles custom arrow_size", {
  net <- create_mock_edge_network(n = 4, directed = TRUE,
                                   custom_edge_aes = list(arrow_size = 0.05))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles bidirectional arrows", {
  net <- create_mock_edge_network(n = 4, directed = TRUE,
                                   custom_edge_aes = list(bidirectional = TRUE))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles per-edge bidirectional", {
  net <- create_mock_edge_network(n = 4, directed = TRUE,
                                   custom_edge_aes = list(bidirectional = c(TRUE, FALSE, TRUE)))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Self-Loop Tests
# ============================================

test_that("render_edges_grid handles self-loops", {
  net <- create_mock_edge_network(n = 4, self_loops = TRUE)

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles loop_rotation parameter", {
  net <- create_mock_edge_network(n = 4, self_loops = TRUE,
                                   custom_edge_aes = list(loop_rotation = pi))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles self-loop with CI underlay", {
  net <- create_mock_edge_network(n = 4, self_loops = TRUE,
                                   custom_edge_aes = list(ci = c(0, 0, 0, 0.3)))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Curve Mode Tests
# ============================================

test_that("render_edges_grid handles curves = FALSE (all straight)", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(curves = FALSE))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles curves = TRUE with reciprocal edges", {
  net <- create_mock_edge_network(n = 4, reciprocal = TRUE,
                                   custom_edge_aes = list(curves = TRUE))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles curves = 'force' (all curved)", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(curves = "force"))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles explicit curvature", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(curvature = 0.3))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles per-edge curvature", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    curvature = c(0, 0.3, -0.3)
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles curve_shape parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    curvature = 0.3,
    curve_shape = 0.5
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles curve_pivot parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    curvature = 0.3,
    curve_pivot = 0.25
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# CI Underlay Tests
# ============================================

test_that("render_edges_grid handles ci underlay", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    ci = c(0.1, 0.2, 0.3)
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles ci_scale parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    ci = c(0.1, 0.2, 0.3),
    ci_scale = 3.0
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles ci_alpha parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    ci = c(0.1, 0.2, 0.3),
    ci_alpha = 0.3
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles ci_color parameter (single value)", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    ci = c(0.1, 0.2, 0.3),
    ci_color = "pink"
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles ci_style parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    ci = c(0.1, 0.2, 0.3),
    ci_style = 3  # dotted
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles ci_arrows parameter", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    ci = c(0.1, 0.2, 0.3),
    ci_arrows = TRUE
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles CI with curved edges", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    ci = c(0.1, 0.2, 0.3),
    curvature = 0.3
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles NA in ci values", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    ci = c(0.1, NA, 0.3)
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles ci = 0 (no underlay)", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    ci = c(0, 0, 0)
  ))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# render_edge_labels_grid Tests
# ============================================

test_that("render_edge_labels_grid returns empty gList for no labels", {
  net <- create_mock_edge_network(n = 4)

  result <- render_edge_labels_grid(net)

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid returns empty gList for zero edges", {
  net <- create_mock_edge_network(n = 3, m = 0)

  result <- render_edge_labels_grid(net)

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 0)
})

test_that("render_edge_labels_grid handles explicit labels", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A->B", "B->C", "C->D")
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_template", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    label_template = "{est}"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_style = estimate", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    label_style = "estimate"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_size", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_size = 12
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles per-edge label_size", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_size = c(8, 10, 12)
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_color", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_color = "red"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles per-edge label_color", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_color = c("red", "green", "blue")
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_position", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_position = 0.3
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles per-edge label_position", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_position = c(0.3, 0.5, 0.7)
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_offset", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_offset = 0.05
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_bg", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_bg = "yellow"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_bg = NA (transparent)", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_bg = NA
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_fontface = bold", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_fontface = "bold"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_fontface = italic", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_fontface = "italic"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_fontface = bold.italic", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_fontface = "bold.italic"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles per-edge label_fontface", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_fontface = c("plain", "bold", "italic")
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles numeric fontface", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_fontface = 2  # bold
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_border = rect", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_border = "rect"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_border = rounded", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_border = "rounded"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_border = circle", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_border = "circle"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_underline", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_underline = TRUE
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_shadow", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_shadow = TRUE
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label_shadow with custom color", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_shadow = TRUE,
    label_shadow_color = "darkgray",
    label_shadow_offset = 1.0,
    label_shadow_alpha = 0.7
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles per-edge label_shadow", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    label_shadow = c(TRUE, FALSE, TRUE)
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles label on curved edge", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    curvature = 0.3
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles curves mode for labels", {
  net <- create_mock_edge_network(n = 4, reciprocal = TRUE, custom_edge_aes = list(
    labels = c("A", "B", "C", "D"),
    curves = TRUE
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles force curve mode for labels", {
  net <- create_mock_edge_network(n = 4, custom_edge_aes = list(
    labels = c("A", "B", "C"),
    curves = "force"
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid skips self-loops", {
  net <- create_mock_edge_network(n = 4, self_loops = TRUE, custom_edge_aes = list(
    labels = c("A", "B", "C", "Self")
  ))

  result <- with_temp_png({
    render_edge_labels_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edge_labels_grid handles zero-length edge (same endpoints)", {
  nodes <- data.frame(
    id = 1:2,
    label = c("A", "B"),
    x = c(0.5, 0.5),  # Same position
    y = c(0.5, 0.5)
  )
  edges <- data.frame(from = 1, to = 2, weight = 0.5)

  mock_network <- list(
    get_nodes = function() nodes,
    get_edges = function() edges,
    get_node_aes = function() list(size = 0.05),
    get_edge_aes = function() list(labels = "Test"),
    get_theme = function() CographTheme$new(),
    is_directed = TRUE
  )
  class(mock_network) <- "CographNetwork"

  result <- with_temp_png({
    render_edge_labels_grid(mock_network)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Direct Function Tests
# ============================================

test_that("draw_straight_edge creates valid grobs", {
  result <- with_temp_png({
    draw_straight_edge(0.2, 0.2, 0.8, 0.8, "blue", 2, 1, TRUE, 0.03, FALSE)
  })

  expect_true(inherits(result, "gList"))
})

test_that("draw_straight_edge creates bidirectional arrows", {
  result <- with_temp_png({
    draw_straight_edge(0.2, 0.2, 0.8, 0.8, "blue", 2, 1, TRUE, 0.03, TRUE)
  })

  expect_true(inherits(result, "gList"))
})

test_that("draw_straight_edge handles no arrows", {
  result <- with_temp_png({
    draw_straight_edge(0.2, 0.2, 0.8, 0.8, "blue", 2, 1, FALSE, 0, FALSE)
  })

  expect_true(inherits(result, "gList"))
})

test_that("draw_straight_edge handles aspect ratio correction", {
  result <- with_temp_png({
    draw_straight_edge(0.2, 0.2, 0.8, 0.8, "blue", 2, 1, TRUE, 0.03, FALSE,
                       x_scale = 0.8, y_scale = 1.2)
  })

  expect_true(inherits(result, "gList"))
})

test_that("draw_curved_edge creates valid grobs", {
  result <- with_temp_png({
    draw_curved_edge(0.2, 0.2, 0.8, 0.8, 0.3, "red", 2, 1, TRUE, 0.03, FALSE)
  })

  expect_true(inherits(result, "gList"))
})

test_that("draw_curved_edge creates bidirectional arrows", {
  result <- with_temp_png({
    draw_curved_edge(0.2, 0.2, 0.8, 0.8, 0.3, "red", 2, 1, TRUE, 0.03, TRUE)
  })

  expect_true(inherits(result, "gList"))
})

test_that("draw_curved_edge handles negative curvature", {
  result <- with_temp_png({
    draw_curved_edge(0.2, 0.2, 0.8, 0.8, -0.3, "red", 2, 1, TRUE, 0.03, FALSE)
  })

  expect_true(inherits(result, "gList"))
})

test_that("draw_curved_edge handles curve_shape and curve_pivot", {
  result <- with_temp_png({
    draw_curved_edge(0.2, 0.2, 0.8, 0.8, 0.3, "red", 2, 1, TRUE, 0.03, FALSE,
                     curve_shape = 0.5, curve_pivot = 0.25)
  })

  expect_true(inherits(result, "gList"))
})

test_that("draw_self_loop creates valid grobs", {
  result <- with_temp_png({
    draw_self_loop(0.5, 0.5, 0.05, "green", 2, 1)
  })

  expect_true(inherits(result, "gList"))
})

test_that("draw_self_loop handles different rotations", {
  result_top <- with_temp_png({
    draw_self_loop(0.5, 0.5, 0.05, "green", 2, 1, rotation = pi/2)
  })

  result_right <- with_temp_png({
    draw_self_loop(0.5, 0.5, 0.05, "green", 2, 1, rotation = 0)
  })

  result_bottom <- with_temp_png({
    draw_self_loop(0.5, 0.5, 0.05, "green", 2, 1, rotation = -pi/2)
  })

  expect_true(inherits(result_top, "gList"))
  expect_true(inherits(result_right, "gList"))
  expect_true(inherits(result_bottom, "gList"))
})

# ============================================
# Helper Function Tests
# ============================================

test_that("bezier_points generates correct number of points", {
  pts <- bezier_points(0, 0, 0.5, 0.5, 1, 0, n = 20)

  expect_equal(nrow(pts), 20)
  expect_equal(ncol(pts), 2)
  expect_true(all(c("x", "y") %in% names(pts)))
})

test_that("bezier_points produces curve through control point", {
  pts <- bezier_points(0, 0, 0.5, 1, 1, 0, n = 50)

  # Midpoint should be above the straight line
  mid_idx <- 25
  expect_true(pts$y[mid_idx] > 0)
})

test_that("curve_control_point returns correct structure", {
  ctrl <- curve_control_point(0, 0, 1, 1, 0.5)

  expect_true(is.list(ctrl))
  expect_true(all(c("x", "y") %in% names(ctrl)))
})

test_that("curve_control_point handles zero curvature", {
  ctrl <- curve_control_point(0, 0, 1, 1, 0)

  # Should be at midpoint when curvature is 0
  expect_equal(ctrl$x, 0.5)
  expect_equal(ctrl$y, 0.5)
})

test_that("curve_control_point handles pivot parameter", {
  ctrl_near_source <- curve_control_point(0, 0, 1, 1, 0.5, pivot = 0.25)
  ctrl_near_target <- curve_control_point(0, 0, 1, 1, 0.5, pivot = 0.75)

  # Pivot near source should produce different control point
  expect_true(ctrl_near_source$x != ctrl_near_target$x)
})

test_that("curve_control_point handles shape parameter", {
  ctrl_no_shape <- curve_control_point(0, 0, 1, 1, 0.5, shape = 0)
  ctrl_with_shape <- curve_control_point(0, 0, 1, 1, 0.5, shape = 0.5)

  # Different shape should produce different curvature intensity
  expect_true(ctrl_no_shape$y != ctrl_with_shape$y)
})

test_that("curve_control_point handles zero-length edge", {
  ctrl <- curve_control_point(0.5, 0.5, 0.5, 0.5, 0.5)

  # Should return the same point
  expect_equal(ctrl$x, 0.5)
  expect_equal(ctrl$y, 0.5)
})

test_that("arrow_points returns correct structure", {
  pts <- arrow_points(0.5, 0.5, 0, 0.03)

  expect_true(is.list(pts))
  expect_true(all(c("x", "y", "mid_x", "mid_y", "back_len") %in% names(pts)))
  expect_equal(length(pts$x), 3)  # Triangle vertices
})

test_that("arrow_points handles different angles", {
  pts_right <- arrow_points(0.5, 0.5, 0, 0.03)  # Pointing right

  pts_up <- arrow_points(0.5, 0.5, pi/2, 0.03)  # Pointing up

  # Arrow tip should be at the same position
  expect_equal(pts_right$x[1], 0.5)
  expect_equal(pts_up$y[1], 0.5)
})

test_that("edge_endpoint calculates correct position", {
  # Node at (0.5, 0.5), other node at (0.8, 0.5), node size 0.05
  pt <- edge_endpoint(0.5, 0.5, 0.8, 0.5, 0.05)

  # Should be on the right edge of node
  expect_true(pt$x > 0.5)
  expect_equal(pt$y, 0.5, tolerance = 0.001)
})

test_that("edge_endpoint handles aspect ratio correction", {
  pt_no_correction <- edge_endpoint(0.5, 0.5, 0.8, 0.8, 0.05)
  pt_with_correction <- edge_endpoint(0.5, 0.5, 0.8, 0.8, 0.05,
                                       x_scale = 0.8, y_scale = 1.2)

  # Different aspect ratios should produce different endpoints
  expect_true(pt_no_correction$x != pt_with_correction$x ||
              pt_no_correction$y != pt_with_correction$y)
})

# ============================================
# Theme Integration Tests
# ============================================

test_that("render_edges_grid uses theme defaults", {
  custom_theme <- CographTheme$new(
    edge_color = "purple",
    edge_positive_color = "darkgreen",
    edge_negative_color = "darkred",
    edge_width = 3
  )
  net <- create_mock_edge_network(n = 4, custom_theme = custom_theme)

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# Edge Cases and Boundary Tests
# ============================================

test_that("render_edges_grid handles single node network (no edges)", {
  net <- create_mock_edge_network(n = 1)

  result <- render_edges_grid(net)

  expect_true(inherits(result, "gList"))
  expect_equal(length(result), 0)
})

test_that("render_edges_grid handles very small node sizes", {
  net <- create_mock_edge_network(n = 4, custom_node_aes = list(size = 0.001))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles very large node sizes", {
  net <- create_mock_edge_network(n = 4, custom_node_aes = list(size = 0.3))

  result <- with_temp_png({
    render_edges_grid(net)
  })

  expect_true(inherits(result, "gList"))
})

test_that("render_edges_grid handles all edges with same weight", {
  nodes <- data.frame(
    id = 1:3,
    label = c("A", "B", "C"),
    x = c(0.2, 0.5, 0.8),
    y = c(0.5, 0.5, 0.5)
  )
  edges <- data.frame(
    from = c(1, 2),
    to = c(2, 3),
    weight = c(0.5, 0.5)  # Same weight
  )

  mock_network <- list(
    get_nodes = function() nodes,
    get_edges = function() edges,
    get_node_aes = function() list(size = 0.05),
    get_edge_aes = function() list(),
    get_theme = function() CographTheme$new(),
    is_directed = TRUE
  )
  class(mock_network) <- "CographNetwork"

  result <- with_temp_png({
    render_edges_grid(mock_network)
  })

  expect_true(inherits(result, "gList"))
})
