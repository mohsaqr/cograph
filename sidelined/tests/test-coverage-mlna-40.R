# test-coverage-mlna-40.R - Comprehensive tests for mlna.R
# Multilevel network visualization function tests

# ============================================
# BASIC INPUT TESTS
# ============================================

test_that("plot_mlna works with basic matrix input", {
  set.seed(42)
  nodes <- paste0("N", 1:9)
  m <- matrix(runif(81, 0, 0.3), 9, 9)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Layer1 = paste0("N", 1:3),
    Layer2 = paste0("N", 4:6),
    Layer3 = paste0("N", 7:9)
  )

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with two layers", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with four layers", {
  set.seed(42)
  nodes <- paste0("N", 1:12)
  m <- matrix(runif(144, 0, 0.3), 12, 12)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    L1 = paste0("N", 1:3),
    L2 = paste0("N", 4:6),
    L3 = paste0("N", 7:9),
    L4 = paste0("N", 10:12)
  )

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna returns NULL invisibly", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- with_temp_png({
    ret <- plot_mlna(m, layers)
    ret
  })

  expect_null(result)
})

test_that("mlna alias works the same as plot_mlna", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- safe_plot(mlna(m, layers))
  expect_true(result$success, info = result$error)
})

# ============================================
# INPUT VALIDATION TESTS
# ============================================

test_that("plot_mlna errors when layer_list is NULL and community is NULL", {
  m <- matrix(runif(16, 0, 0.3), 4, 4)

  expect_error(
    plot_mlna(m, layer_list = NULL, community = NULL),
    "layer_list or community"
  )
})

test_that("plot_mlna errors with less than 2 layers", {
  nodes <- paste0("N", 1:4)
  m <- matrix(runif(16, 0, 0.3), 4, 4)
  colnames(m) <- rownames(m) <- nodes

  # Single layer
  layers <- list(Only = nodes)

  expect_error(
    plot_mlna(m, layers),
    "2\\+"
  )
})

test_that("plot_mlna errors with invalid layer_list type", {
  m <- matrix(runif(16, 0, 0.3), 4, 4)

  # Not a list
  layers <- c("N1", "N2", "N3", "N4")

  expect_error(
    plot_mlna(m, layers),
    "list"
  )
})

test_that("plot_mlna errors with overlapping layers", {
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  colnames(m) <- rownames(m) <- nodes

  # N3 appears in both layers
  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 3:6)
  )

  expect_error(
    plot_mlna(m, layers),
    "overlap"
  )
})

test_that("plot_mlna errors with nodes not in model", {
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  colnames(m) <- rownames(m) <- nodes

  # "X1" does not exist
  layers <- list(
    Top = c("N1", "N2", "X1"),
    Bottom = paste0("N", 4:6)
  )

  expect_error(
    plot_mlna(m, layers),
    "not found"
  )
})

test_that("plot_mlna errors with non-matrix non-tna input", {
  layers <- list(
    Top = c("A", "B"),
    Bottom = c("C", "D")
  )

  expect_error(
    plot_mlna("not a matrix", layers),
    "matrix"
  )
})

# ============================================
# LAYOUT PARAMETER TESTS
# ============================================

test_that("plot_mlna works with horizontal layout (default)", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, layout = "horizontal"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with circle layout", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with spring layout", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, layout = "spring"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SPACING AND DIMENSION TESTS
# ============================================

test_that("plot_mlna works with custom layer_spacing", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, layer_spacing = 6))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with custom layer_width", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, layer_width = 10))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with custom layer_depth", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, layer_depth = 6))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with custom skew_angle", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, skew_angle = 45))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with custom node_spacing", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, node_spacing = 0.5))
  expect_true(result$success, info = result$error)
})

# ============================================
# COLOR AND SHAPE TESTS
# ============================================

test_that("plot_mlna works with custom colors", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, colors = c("red", "blue")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with custom shapes", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, shapes = c("square", "triangle")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with custom edge_colors", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, edge_colors = c("#FF0000", "#0000FF")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna handles shape diamond", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, shapes = c("diamond", "circle")))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE DISPLAY TESTS
# ============================================

test_that("plot_mlna works with within_edges = FALSE", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, within_edges = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with between_edges = FALSE", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, between_edges = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with both edge types disabled", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, within_edges = FALSE, between_edges = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with between_style solid", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, between_style = 1))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with between_style dotted", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, between_style = 3))
  expect_true(result$success, info = result$error)
})

# ============================================
# VISUAL OPTIONS TESTS
# ============================================

test_that("plot_mlna works with show_border = FALSE", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, show_border = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with legend = FALSE", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, legend = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with different legend_position options", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  for (pos in c("topright", "topleft", "bottomright", "bottomleft")) {
    result <- safe_plot(plot_mlna(m, layers, legend_position = pos))
    expect_true(result$success, info = paste("legend_position", pos, "failed:", result$error))
  }
})

test_that("plot_mlna works with custom curvature", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, curvature = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with custom node_size", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, node_size = 5))
  expect_true(result$success, info = result$error)
})

# ============================================
# MINIMUM THRESHOLD TESTS
# ============================================

test_that("plot_mlna works with minimum threshold", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, minimum = 0.1))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with high minimum hiding most edges", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, minimum = 0.25))
  expect_true(result$success, info = result$error)
})

# ============================================
# SCALE PARAMETER TESTS
# ============================================

test_that("plot_mlna works with scale parameter", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, scale = 2))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with large scale for high-res output", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, scale = 4))
  expect_true(result$success, info = result$error)
})

# ============================================
# COMMUNITY DETECTION TESTS
# ============================================

test_that("plot_mlna works with community parameter (louvain)", {
  skip_if_not_installed("igraph")

  set.seed(42)
  n <- 12
  m <- matrix(runif(n*n, 0, 0.3), n, n)
  diag(m) <- 0
  # Make symmetric for undirected (louvain requires undirected)
  m <- (m + t(m)) / 2
  # Create some structure for community detection
  m[1:4, 1:4] <- m[1:4, 1:4] + 0.3
  m[5:8, 5:8] <- m[5:8, 5:8] + 0.3
  m[9:12, 9:12] <- m[9:12, 9:12] + 0.3
  m <- pmin(m, 1)
  colnames(m) <- rownames(m) <- paste0("N", 1:n)

  result <- safe_plot(plot_mlna(m, community = "louvain"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with community parameter (walktrap)", {
  skip_if_not_installed("igraph")

  set.seed(42)
  n <- 12
  m <- matrix(runif(n*n, 0, 0.3), n, n)
  diag(m) <- 0
  # Make symmetric for better community detection
  m <- (m + t(m)) / 2
  m[1:4, 1:4] <- m[1:4, 1:4] + 0.3
  m[5:8, 5:8] <- m[5:8, 5:8] + 0.3
  m[9:12, 9:12] <- m[9:12, 9:12] + 0.3
  m <- pmin(m, 1)
  colnames(m) <- rownames(m) <- paste0("N", 1:n)

  result <- safe_plot(plot_mlna(m, community = "walktrap"))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE CASE TESTS
# ============================================

test_that("plot_mlna works with single node per layer", {
  set.seed(42)
  nodes <- paste0("N", 1:3)
  m <- matrix(runif(9, 0, 0.3), 3, 3)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Layer1 = "N1",
    Layer2 = "N2",
    Layer3 = "N3"
  )

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with unequal layer sizes", {
  set.seed(42)
  nodes <- paste0("N", 1:8)
  m <- matrix(runif(64, 0, 0.3), 8, 8)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Large = paste0("N", 1:5),
    Small = paste0("N", 6:8)
  )

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with zero weight edges", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(0, 6, 6)
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with NA weight values", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  m[1, 2] <- NA
  m[3, 4] <- NA
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with matrix without named rows/columns", {
  set.seed(42)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  # No rownames/colnames set, will use indices

  layers <- list(
    Top = c("1", "2", "3"),
    Bottom = c("4", "5", "6")
  )

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna handles unnamed layers (NULL names)", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  # Unnamed list
  layers <- list(
    paste0("N", 1:3),
    paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

# ============================================
# SPRING LAYOUT SPECIFIC TESTS
# ============================================

test_that("plot_mlna spring layout works with strongly connected layer", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.1), 6, 6)
  diag(m) <- 0
  # Make top layer strongly connected
  m[1:3, 1:3] <- 0.8
  diag(m[1:3, 1:3]) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, layout = "spring"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna spring layout works with disconnected nodes in layer", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(0, 6, 6)
  # Only one edge in top layer
  m[1, 2] <- 0.5
  m[2, 1] <- 0.5
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, layout = "spring"))
  expect_true(result$success, info = result$error)
})

# ============================================
# COMBINED PARAMETER TESTS
# ============================================

test_that("plot_mlna works with multiple customizations", {
  set.seed(42)
  nodes <- paste0("N", 1:9)
  m <- matrix(runif(81, 0, 0.3), 9, 9)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Top = paste0("N", 1:3),
    Middle = paste0("N", 4:6),
    Bottom = paste0("N", 7:9)
  )

  result <- safe_plot(plot_mlna(
    m, layers,
    layout = "circle",
    layer_spacing = 3,
    layer_width = 6,
    layer_depth = 3,
    skew_angle = 30,
    node_spacing = 0.8,
    colors = c("#E57373", "#81C784", "#64B5F6"),
    shapes = c("circle", "square", "triangle"),
    within_edges = TRUE,
    between_edges = TRUE,
    between_style = 2,
    show_border = TRUE,
    legend = TRUE,
    legend_position = "topleft",
    curvature = 0.2,
    node_size = 2,
    minimum = 0.05
  ))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with minimal configuration", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(
    m, layers,
    show_border = FALSE,
    legend = FALSE,
    within_edges = FALSE,
    between_edges = FALSE
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# SKEW ANGLE BOUNDARY TESTS
# ============================================

test_that("plot_mlna works with zero skew_angle", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, skew_angle = 0))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with large skew_angle", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, skew_angle = 60))
  expect_true(result$success, info = result$error)
})

# ============================================
# CURVATURE BOUNDARY TESTS
# ============================================

test_that("plot_mlna works with zero curvature", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, curvature = 0))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with high curvature", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, curvature = 0.5))
  expect_true(result$success, info = result$error)
})

# ============================================
# MAX WEIGHT HANDLING TESTS
# ============================================

test_that("plot_mlna handles uniform weight matrix", {
  nodes <- paste0("N", 1:6)
  m <- matrix(0.5, 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna handles all-zero max weight gracefully", {
  nodes <- paste0("N", 1:6)
  m <- matrix(0, 6, 6)
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  # Should not error - max_w will be set to 1 to avoid division by zero
  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna handles very small weights", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.01), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna handles very large weights", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 100), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})
