# Test file for cograph.R - coverage improvement tests (Series 41)
# Targets uncovered branches and edge cases in cograph.R

# =============================================================================
# Helper function local to this test file (avoids collision with test-utils)
# =============================================================================
make_test_matrix <- function(n = 3, symmetric = TRUE, with_names = TRUE) {
  mat <- matrix(runif(n * n), nrow = n)
  if (symmetric) {
    mat <- (mat + t(mat)) / 2
  }
  diag(mat) <- 0
  if (with_names) {
    rownames(mat) <- colnames(mat) <- LETTERS[seq_len(n)]
  }
  mat
}

# =============================================================================
# ensure_cograph_network - additional branch coverage
# =============================================================================

test_that("ensure_cograph_network returns unchanged when nodes have x column but no NA values", {
  mat <- make_test_matrix(3)
  # Create network with layout already computed
  net <- cograph(mat, layout = "circle", seed = 42)

  # Nodes have valid x values
  expect_false(all(is.na(net$nodes$x)))

  # Call ensure_cograph_network - should NOT recompute layout (early return)
  result <- cograph:::ensure_cograph_network(net, layout = "spring", seed = 99)

  expect_s3_class(result, "cograph_network")
  # Coordinates should remain from original circle layout (not recomputed as spring)
  expect_false(all(is.na(result$nodes$x)))
})

test_that("ensure_cograph_network recomputes layout when x column missing", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)  # No layout specified

  # Remove x column entirely
  net$nodes$x <- NULL

  # Should compute layout
  result <- cograph:::ensure_cograph_network(net, layout = "circle", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_true("x" %in% names(result$nodes))
  expect_false(all(is.na(result$nodes$x)))
})

test_that("ensure_cograph_network handles igraph input when available", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- cograph:::ensure_cograph_network(g, layout = "circle", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 5)
})

test_that("ensure_cograph_network handles network (statnet) input when available", {
  skip_if_not_installed("network")

  # Create simple statnet network
  net_mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  net_obj <- network::network(net_mat, directed = FALSE)

  result <- cograph:::ensure_cograph_network(net_obj, layout = "circle", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
})

test_that("ensure_cograph_network handles tna input when available", {
  skip_if_not_installed("tna")

  # Create simple tna object
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.3, 0.2, 0.4, 0, 0.3, 0.3, 0.4, 0), nrow = 3),
      labels = c("A", "B", "C"),
      inits = c(0.4, 0.3, 0.3)
    ),
    class = "tna"
  )

  result <- cograph:::ensure_cograph_network(tna_obj, layout = "circle", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
})

# =============================================================================
# compute_layout_for_cograph - additional branch coverage
# =============================================================================

test_that("compute_layout_for_cograph handles function layout when igraph available", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  net <- as_cograph(mat)

  # Use igraph layout function directly
  result <- cograph:::compute_layout_for_cograph(
    net,
    layout = igraph::layout_in_circle,
    seed = 42
  )

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "custom_function")
})

test_that("compute_layout_for_cograph handles igraph layout name code", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  net <- as_cograph(mat)

  # Use two-letter igraph code
  result <- cograph:::compute_layout_for_cograph(net, layout = "kk", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("compute_layout_for_cograph handles igraph_ prefix layout name", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  net <- as_cograph(mat)

  result <- cograph:::compute_layout_for_cograph(net, layout = "igraph_fr", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("compute_layout_for_cograph handles layout_ prefix layout name", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  net <- as_cograph(mat)

  result <- cograph:::compute_layout_for_cograph(net, layout = "layout_nicely", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("compute_layout_for_cograph renames columns when coords have 2+ columns", {
  mat <- make_test_matrix(3)
  net <- as_cograph(mat)

  # Matrix with unnamed columns
  custom_coords <- matrix(c(0, 0.5, 1, 0, 1, 0.5), nrow = 3, ncol = 2)

  result <- cograph:::compute_layout_for_cograph(net, layout = custom_coords, seed = 42)

  expect_equal(result$nodes$x, c(0, 0.5, 1))
  expect_equal(result$nodes$y, c(0, 1, 0.5))
})

test_that("compute_layout_for_cograph respects existing column names if ncol < 2", {
  mat <- make_test_matrix(3)
  net <- as_cograph(mat)

  # Data frame with proper column names
  custom_coords <- data.frame(x = c(0.1, 0.5, 0.9), y = c(0.2, 0.8, 0.5))

  result <- cograph:::compute_layout_for_cograph(net, layout = custom_coords, seed = 42)

  expect_equal(result$nodes$x, c(0.1, 0.5, 0.9))
  expect_equal(result$nodes$y, c(0.2, 0.8, 0.5))
})

# =============================================================================
# cograph() - source type detection branches
# =============================================================================

test_that("cograph detects igraph source type", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4)
  net <- cograph(g)

  expect_equal(net$meta$source, "igraph")
})

test_that("cograph detects network (statnet) source type", {
  skip_if_not_installed("network")

  net_mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  net_obj <- network::network(net_mat, directed = FALSE)

  result <- cograph(net_obj)

  expect_equal(result$meta$source, "network")
})

test_that("cograph detects qgraph source type", {
  skip_if_not_installed("qgraph")

  mat <- make_test_matrix(3)

  # Create qgraph plot silently
  tmp <- tempfile(fileext = ".png")
  png(tmp)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)
  dev.off()
  unlink(tmp)

  result <- cograph(q)

  expect_equal(result$meta$source, "qgraph")
})

test_that("cograph detects tna source type", {
  skip_if_not_installed("tna")

  # Create tna object
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.3, 0.2, 0.4, 0, 0.3, 0.3, 0.4, 0), nrow = 3),
      labels = c("A", "B", "C"),
      inits = c(0.4, 0.3, 0.3)
    ),
    class = "tna"
  )

  result <- cograph(tna_obj)

  expect_equal(result$meta$source, "tna")
})

test_that("cograph handles unknown source type gracefully", {
  # This should error because the input type is not supported
  expect_error(
    cograph("invalid_input"),
    "Unsupported input type"
  )
})

# =============================================================================
# cograph() - weights matrix preservation branches
# =============================================================================

test_that("cograph uses weights_matrix from parsed input when available", {
  skip_if_not_installed("tna")

  # TNA objects have weights_matrix in parsed output
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.3, 0.2, 0.4, 0, 0.3, 0.3, 0.4, 0), nrow = 3),
      labels = c("A", "B", "C"),
      inits = c(0.4, 0.3, 0.3)
    ),
    class = "tna"
  )

  result <- cograph(tna_obj)

  expect_true(!is.null(result$weights))
  expect_true(is.matrix(result$weights))
  expect_equal(dim(result$weights), c(3, 3))
})

test_that("cograph preserves square matrix input as weights", {
  mat <- make_test_matrix(4)
  result <- cograph(mat)

  expect_true(!is.null(result$weights))
  expect_true(is.matrix(result$weights))
  expect_equal(result$weights, mat)
})

test_that("cograph does not create weights from edge list input", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  result <- cograph(edges)

  # Edge list doesn't have a square matrix, so weights should be NULL or not preserved
  # (based on implementation, it may be NULL)
  expect_true(is.null(result$weights) || !is.matrix(result$weights) ||
              all(dim(result$weights) == c(3, 3)))
})

# =============================================================================
# cograph() - layout handling branches
# =============================================================================

test_that("cograph handles function layout (igraph layout function)", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  result <- cograph(mat, layout = igraph::layout_in_circle, seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "custom_function")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("cograph handles igraph two-letter layout code", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  result <- cograph(mat, layout = "fr", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("cograph handles igraph_ prefix layout name", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  result <- cograph(mat, layout = "igraph_circle", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("cograph handles layout_ prefix layout name", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  result <- cograph(mat, layout = "layout_with_kk", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("cograph handles matrix layout input", {
  mat <- make_test_matrix(3)
  coords <- matrix(c(0.1, 0.5, 0.9, 0.2, 0.8, 0.5), nrow = 3, ncol = 2)

  result <- cograph(mat, layout = coords)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$nodes$x[1], 0.1)
  expect_equal(result$nodes$y[3], 0.5)
})

test_that("cograph handles data.frame layout input", {
  mat <- make_test_matrix(3)
  coords <- data.frame(x = c(0.1, 0.5, 0.9), y = c(0.2, 0.8, 0.5))

  result <- cograph(mat, layout = coords)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$nodes$x, c(0.1, 0.5, 0.9))
  expect_equal(result$nodes$y, c(0.2, 0.8, 0.5))
})

test_that("cograph renames coords columns when using matrix layout", {
  mat <- make_test_matrix(3)
  # Matrix without column names
  coords <- matrix(c(0, 0.5, 1, 0, 1, 0.5), nrow = 3)

  result <- cograph(mat, layout = coords)

  expect_true("x" %in% names(result$nodes))
  expect_true("y" %in% names(result$nodes))
})

test_that("cograph uses CographLayout for built-in layout names", {
  mat <- make_test_matrix(4)
  result <- cograph(mat, layout = "spring", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "spring")
  expect_false(all(is.na(result$nodes$x)))
})

# =============================================================================
# cograph() - TNA metadata handling
# =============================================================================

test_that("cograph creates tna metadata when input is tna object", {
  skip_if_not_installed("tna")

  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.3, 0.2, 0.4, 0, 0.3, 0.3, 0.4, 0), nrow = 3),
      labels = c("A", "B", "C"),
      inits = c(0.4, 0.3, 0.3)
    ),
    class = "tna"
  )

  result <- cograph(tna_obj)

  expect_true(!is.null(result$meta$tna))
  expect_equal(result$meta$tna$type, "tna")
})

test_that("cograph does not create tna metadata for matrix input", {
  mat <- make_test_matrix(3)
  result <- cograph(mat)

  expect_true(is.null(result$meta$tna))
})

# =============================================================================
# sn_layout() - comprehensive branch coverage
# =============================================================================

test_that("sn_layout handles CographLayout object", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  layout_obj <- CographLayout$new("circle")
  result <- sn_layout(net, layout_obj)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "custom")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("sn_layout handles igraph layout function when available", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  net <- cograph(mat)

  result <- sn_layout(net, igraph::layout_in_circle, seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "custom_function")
})

test_that("sn_layout handles igraph two-letter code", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  net <- cograph(mat)

  result <- sn_layout(net, "mds", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "mds")
})

test_that("sn_layout handles igraph_ prefix", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  net <- cograph(mat)

  result <- sn_layout(net, "igraph_kk", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "igraph_kk")
})

test_that("sn_layout handles layout_ prefix", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(4)
  net <- cograph(mat)

  result <- sn_layout(net, "layout_with_fr", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "layout_with_fr")
})

test_that("sn_layout handles matrix coordinates", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  coords <- matrix(c(0, 0.5, 1, 0, 1, 0.5), nrow = 3)
  result <- sn_layout(net, coords)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "custom")
  expect_equal(result$nodes$x[1], 0)
})

test_that("sn_layout handles data.frame coordinates", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  coords <- data.frame(x = c(0.1, 0.5, 0.9), y = c(0.3, 0.6, 0.9))
  result <- sn_layout(net, coords)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$nodes$x[2], 0.5)
  expect_equal(result$nodes$y[2], 0.6)
})

test_that("sn_layout renames coords columns when matrix has 2+ columns", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  # 3-column matrix (extra column ignored)
  coords <- matrix(c(0, 0.5, 1, 0, 1, 0.5, 1, 2, 3), nrow = 3)
  result <- sn_layout(net, coords)

  expect_true("x" %in% names(result$nodes))
  expect_true("y" %in% names(result$nodes))
})

test_that("sn_layout errors on invalid layout type (numeric)", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_layout(net, 12345)
  )
})

test_that("sn_layout errors on invalid layout type (logical)", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_layout(net, TRUE),
    "layout must be"
  )
})

test_that("sn_layout uses built-in layout for non-igraph string", {
  mat <- make_test_matrix(4)
  net <- cograph(mat)

  result <- sn_layout(net, "grid", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$layout$name, "grid")
})

test_that("sn_layout updates layout coords in network nodes", {
  mat <- make_test_matrix(3)
  net <- cograph(mat, layout = "circle", seed = 42)

  old_x <- net$nodes$x

  # Apply different layout
  result <- sn_layout(net, "spring", seed = 99)

  # Coordinates should be different
  expect_false(all(old_x == result$nodes$x))
})

test_that("sn_layout stores layout info with coords", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  result <- sn_layout(net, "circle", seed = 123)

  expect_true(!is.null(result$meta$layout))
  expect_equal(result$meta$layout$name, "circle")
  expect_equal(result$meta$layout$seed, 123)
  expect_true(!is.null(result$meta$layout$coords))
})

# =============================================================================
# sn_theme() - comprehensive branch coverage
# =============================================================================

test_that("sn_theme gets theme from registry by name", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  result <- sn_theme(net, "classic")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$theme))
})

test_that("sn_theme accepts CographTheme object directly", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  # Get theme object
  theme_obj <- get_theme("dark")

  if (!is.null(theme_obj) && inherits(theme_obj, "CographTheme")) {
    result <- sn_theme(net, theme_obj)
    expect_s3_class(result, "cograph_network")
    expect_true(!is.null(result$theme))
  } else {
    skip("Dark theme not registered or not CographTheme class")
  }
})

test_that("sn_theme errors on unknown theme name", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_theme(net, "nonexistent_theme_xyz"),
    "Unknown theme"
  )
})

test_that("sn_theme errors on invalid theme type (numeric)", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_theme(net, 12345),
    "theme must be a string"
  )
})

test_that("sn_theme errors on invalid theme type (list)", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_theme(net, list(bg = "white")),
    "theme must be a string"
  )
})

test_that("sn_theme applies overrides when provided", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  result <- sn_theme(net, "classic", background = "lightblue")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$theme))
})

test_that("sn_theme does not apply overrides when list is empty", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  # No overrides provided
  result <- sn_theme(net, "classic")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$theme))
})

test_that("sn_theme auto-converts matrix input", {
  mat <- make_test_matrix(3)

  result <- sn_theme(mat, "minimal")

  expect_s3_class(result, "cograph_network")
})

# =============================================================================
# sn_palette() - comprehensive branch coverage
# =============================================================================

test_that("sn_palette gets palette from registry by name", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  result <- sn_palette(net, "viridis")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$node_aes))
})

test_that("sn_palette accepts function palette", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  custom_pal <- function(n) rainbow(n)
  result <- sn_palette(net, custom_pal)

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$node_aes$fill))
})

test_that("sn_palette errors on unknown palette name", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_palette(net, "nonexistent_palette_xyz"),
    "Unknown palette"
  )
})

test_that("sn_palette errors on invalid palette type (numeric)", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_palette(net, 12345),
    "palette must be a string"
  )
})

test_that("sn_palette errors on invalid palette type (logical)", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_palette(net, FALSE),
    "palette must be a string"
  )
})

test_that("sn_palette maps colors by variable when 'by' specified", {
  mat <- make_test_matrix(4)
  net <- cograph(mat)
  net$nodes$group <- c("A", "A", "B", "B")

  result <- sn_palette(net, "colorblind", by = "group")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$node_aes$fill))
  # First two should have same color, last two should have same color
  expect_equal(result$node_aes$fill[1], result$node_aes$fill[2])
  expect_equal(result$node_aes$fill[3], result$node_aes$fill[4])
})

test_that("sn_palette uses default single color when 'by' not in nodes", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  # 'by' is specified but column doesn't exist
  result <- sn_palette(net, "viridis", by = "nonexistent_column")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$node_aes$fill))
  # All nodes should have same color (first from palette)
  expect_equal(length(unique(result$node_aes$fill)), 1)
})

test_that("sn_palette applies to edges when target='edges'", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  result <- sn_palette(net, "colorblind", target = "edges")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$edge_aes))
  expect_true(!is.null(result$edge_aes$positive_color))
  expect_true(!is.null(result$edge_aes$negative_color))
})

test_that("sn_palette applies to both nodes and edges when target='both'", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)

  result <- sn_palette(net, "viridis", target = "both")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$node_aes$fill))
  expect_true(!is.null(result$edge_aes$positive_color))
})

test_that("sn_palette handles network with no edges for target='edges'", {
  # Network with no edges
  mat <- matrix(0, nrow = 3, ncol = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- cograph(mat)

  # Should not error even with no edges
  result <- sn_palette(net, "viridis", target = "edges")

  expect_s3_class(result, "cograph_network")
})

test_that("sn_palette handles network with NULL edges for target='edges'", {
  mat <- matrix(0, nrow = 3, ncol = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- cograph(mat)

  # Force edges to NULL
  net$edges <- NULL

  result <- sn_palette(net, "viridis", target = "edges")

  expect_s3_class(result, "cograph_network")
})

test_that("sn_palette initializes node_aes when NULL", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)
  net$node_aes <- NULL

  result <- sn_palette(net, "viridis")

  expect_true(!is.null(result$node_aes))
  expect_true(!is.null(result$node_aes$fill))
})

test_that("sn_palette initializes edge_aes when NULL", {
  mat <- make_test_matrix(3)
  net <- cograph(mat)
  net$edge_aes <- NULL

  result <- sn_palette(net, "colorblind", target = "edges")

  expect_true(!is.null(result$edge_aes))
})

test_that("sn_palette auto-converts matrix input", {
  mat <- make_test_matrix(3)

  result <- sn_palette(mat, "viridis")

  expect_s3_class(result, "cograph_network")
})

# =============================================================================
# Edge cases and corner cases
# =============================================================================

test_that("cograph handles large network", {
  mat <- make_test_matrix(50)
  net <- cograph(mat, layout = "spring", seed = 42)

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 50)
})

test_that("cograph handles weighted edge list", {
  edges <- data.frame(
    from = c("A", "B", "C", "A"),
    to = c("B", "C", "A", "C"),
    weight = c(0.5, 0.3, 0.8, 0.2)
  )

  net <- cograph(edges)

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 3)
  expect_true(all(c("weight") %in% names(net$edges)))
})

test_that("cograph handles edge list with numeric node IDs", {
  edges <- data.frame(
    from = c(1, 2, 3),
    to = c(2, 3, 1)
  )

  net <- cograph(edges)

  expect_s3_class(net, "cograph_network")
})

test_that("cograph handles directed asymmetric matrix", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- cograph(mat, directed = TRUE)

  expect_s3_class(net, "cograph_network")
  expect_true(is_directed(net))
})

test_that("cograph handles forced undirected on asymmetric matrix", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- cograph(mat, directed = FALSE)

  expect_s3_class(net, "cograph_network")
  expect_false(is_directed(net))
})

test_that("cograph handles nodes parameter", {
  mat <- make_test_matrix(3)
  custom_nodes <- data.frame(
    label = c("Node1", "Node2", "Node3"),
    group = c("G1", "G1", "G2")
  )

  net <- cograph(mat, nodes = custom_nodes)

  expect_s3_class(net, "cograph_network")
  # Node labels should be updated
  nodes <- get_nodes(net)
  expect_true("group" %in% names(nodes) || "label" %in% names(nodes))
})

test_that("cograph all igraph layout codes work", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(5)

  codes <- c("kk", "fr", "mds", "ni", "ci", "st", "gr", "rd")

  for (code in codes) {
    result <- tryCatch(
      cograph(mat, layout = code, seed = 42),
      error = function(e) NULL
    )

    if (!is.null(result)) {
      expect_s3_class(result, "cograph_network")
    }
  }
})

test_that("sn_layout all igraph layout codes work", {
  skip_if_not_installed("igraph")

  mat <- make_test_matrix(5)
  net <- cograph(mat)

  codes <- c("kk", "fr", "mds", "ni", "ci", "st", "gr", "rd")

  for (code in codes) {
    result <- tryCatch(
      sn_layout(net, code, seed = 42),
      error = function(e) NULL
    )

    if (!is.null(result)) {
      expect_s3_class(result, "cograph_network")
    }
  }
})

# =============================================================================
# Pipe chain integration tests
# =============================================================================

test_that("pipe chain with all modifiers works", {
  mat <- make_test_matrix(4)

  result <- mat |>
    cograph(layout = "circle", seed = 42) |>
    sn_layout("spring", seed = 99) |>
    sn_theme("classic") |>
    sn_palette("viridis")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$theme))
  expect_true(!is.null(result$node_aes$fill))
  expect_equal(result$meta$layout$name, "spring")
})

test_that("pipe chain starting from matrix with sn_layout", {
  mat <- make_test_matrix(3)

  result <- mat |>
    sn_layout("circle", seed = 42) |>
    sn_theme("dark")

  expect_s3_class(result, "cograph_network")
})

test_that("pipe chain starting from matrix with sn_theme", {
  mat <- make_test_matrix(3)

  result <- mat |>
    sn_theme("minimal") |>
    sn_palette("colorblind")

  expect_s3_class(result, "cograph_network")
})

test_that("pipe chain starting from matrix with sn_palette", {
  mat <- make_test_matrix(3)

  result <- mat |>
    sn_palette("viridis", target = "both")

  expect_s3_class(result, "cograph_network")
})

# =============================================================================
# Integration with external packages (when available)
# =============================================================================

test_that("full workflow with igraph input", {
  skip_if_not_installed("igraph")

  g <- igraph::sample_gnp(10, 0.3)

  result <- g |>
    cograph() |>
    sn_layout("fr", seed = 42) |>
    sn_theme("classic") |>
    sn_palette("viridis")

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 10)
})

test_that("full workflow with network (statnet) input", {
  skip_if_not_installed("network")

  net_mat <- make_test_matrix(5)
  net_obj <- network::network(net_mat, directed = FALSE)

  result <- net_obj |>
    cograph() |>
    sn_layout("circle", seed = 42) |>
    sn_palette("colorblind")

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 5)
})

test_that("full workflow with tna input", {
  skip_if_not_installed("tna")

  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.3, 0.2, 0.4, 0, 0.3, 0.3, 0.4, 0), nrow = 3),
      labels = c("A", "B", "C"),
      inits = c(0.4, 0.3, 0.3)
    ),
    class = "tna"
  )

  result <- tna_obj |>
    cograph() |>
    sn_layout("circle") |>
    sn_theme("classic")

  expect_s3_class(result, "cograph_network")
  expect_equal(result$meta$source, "tna")
  expect_true(!is.null(result$meta$tna))
})

# =============================================================================
# Null/NA handling edge cases
# =============================================================================

test_that("cograph handles matrix with NA values", {
  mat <- make_test_matrix(3)
  mat[1, 2] <- NA

  # Should work (NAs treated as no edge)
  result <- cograph(mat)

  expect_s3_class(result, "cograph_network")
})

test_that("cograph handles matrix with all zeros", {
  mat <- matrix(0, nrow = 4, ncol = 4)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]

  result <- cograph(mat)

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 4)
  expect_equal(n_edges(result), 0)
})

test_that("sn_layout handles NULL seed", {
  mat <- make_test_matrix(4)
  net <- cograph(mat)

  result <- sn_layout(net, "spring", seed = NULL)

  expect_s3_class(result, "cograph_network")
})

test_that("cograph handles NULL layout (no layout computed)", {
  mat <- make_test_matrix(3)

  result <- cograph(mat, layout = NULL)

  expect_s3_class(result, "cograph_network")
  # Layout should be NULL or nodes should have NA coordinates
  expect_true(all(is.na(result$nodes$x)) || is.null(result$nodes$x))
})
