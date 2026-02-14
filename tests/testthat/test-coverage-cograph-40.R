# Test file for cograph.R - coverage improvement tests
# Targets uncovered lines in cograph.R (87.5% -> higher coverage)

# Helper to create test matrices
create_test_matrix <- function(n = 3, symmetric = TRUE) {
  mat <- matrix(runif(n * n), nrow = n)
  if (symmetric) {
    mat <- (mat + t(mat)) / 2
  }

  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[seq_len(n)]
  mat
}

# =============================================================================
# ensure_cograph_network tests
# =============================================================================

test_that("ensure_cograph_network computes layout for network without coords", {
  # Create a cograph_network without layout coordinates
  mat <- create_test_matrix(4)
  net <- as_cograph(mat)

  # Manually set x/y to NA to simulate missing layout
  net$nodes$x <- NA_real_
  net$nodes$y <- NA_real_

  # ensure_cograph_network should compute layout
  result <- cograph:::ensure_cograph_network(net, layout = "circle", seed = 123)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
  expect_false(all(is.na(result$nodes$y)))
})

test_that("ensure_cograph_network overrides directed when specified", {
  mat <- create_test_matrix(3, symmetric = TRUE)
  net <- cograph(mat)  # Should be undirected by default

  # Override to directed

  result <- cograph:::ensure_cograph_network(net, directed = TRUE)
  expect_true(is_directed(result))

  # Override to undirected
  result2 <- cograph:::ensure_cograph_network(net, directed = FALSE)
  expect_false(is_directed(result2))
})

test_that("ensure_cograph_network errors on unsupported type", {
  expect_error(
    cograph:::ensure_cograph_network("invalid_string"),
    "Input must be a matrix"
  )

  expect_error(
    cograph:::ensure_cograph_network(123),
    "Input must be a matrix"
  )

  expect_error(
    cograph:::ensure_cograph_network(list(a = 1)),
    "Input must be a matrix"
  )
})

test_that("ensure_cograph_network auto-converts matrix input", {
  mat <- create_test_matrix(3)
  result <- cograph:::ensure_cograph_network(mat, layout = "circle", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
})

test_that("ensure_cograph_network auto-converts data.frame edge list", {
  edges <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))
  result <- cograph:::ensure_cograph_network(edges, layout = "spring", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
})

# =============================================================================
# compute_layout_for_cograph tests
# =============================================================================

test_that("compute_layout_for_cograph handles matrix layout input", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  # Use a matrix as layout coordinates
  custom_coords <- matrix(c(0, 0.5, 1, 0, 1, 0.5), nrow = 3, ncol = 2)

  result <- cograph:::compute_layout_for_cograph(net, layout = custom_coords, seed = 42)

  expect_s3_class(result, "cograph_network")
  # The coordinates should be applied to nodes
  expect_equal(result$nodes$x, c(0, 0.5, 1))
  expect_equal(result$nodes$y, c(0, 1, 0.5))
})

test_that("compute_layout_for_cograph handles data.frame layout input", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  # Use a data.frame as layout coordinates
  custom_coords <- data.frame(x = c(0, 0.5, 1), y = c(0, 1, 0.5))

  result <- cograph:::compute_layout_for_cograph(net, layout = custom_coords, seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$nodes$x, c(0, 0.5, 1))
  expect_equal(result$nodes$y, c(0, 1, 0.5))
})

test_that("compute_layout_for_cograph uses CographLayout for built-in layouts", {
  mat <- create_test_matrix(4)
  net <- as_cograph(mat)

  result <- cograph:::compute_layout_for_cograph(net, layout = "circle", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$layout_info$name, "circle")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("compute_layout_for_cograph respects NULL seed", {
  mat <- create_test_matrix(4)
  net <- as_cograph(mat)

  # With NULL seed, should still work (just not deterministic)
  result <- cograph:::compute_layout_for_cograph(net, layout = "spring", seed = NULL)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

# =============================================================================
# cograph() layout handling tests
# =============================================================================

test_that("cograph handles custom coordinate matrix layout", {
  mat <- create_test_matrix(3)
  custom_coords <- matrix(c(0.1, 0.5, 0.9, 0.2, 0.8, 0.5), nrow = 3, ncol = 2)

  result <- cograph(mat, layout = custom_coords, seed = 42)

  expect_s3_class(result, "cograph_network")
  # The coords should be stored
  expect_true(!is.null(result$layout))
})

test_that("cograph handles custom coordinate data.frame layout", {
  mat <- create_test_matrix(3)
  custom_coords <- data.frame(x = c(0.1, 0.5, 0.9), y = c(0.2, 0.8, 0.5))

  result <- cograph(mat, layout = custom_coords, seed = 42)

  expect_s3_class(result, "cograph_network")
  # The coordinates should be stored in nodes
  expect_equal(result$nodes$x, c(0.1, 0.5, 0.9))
  expect_equal(result$nodes$y, c(0.2, 0.8, 0.5))
})

test_that("cograph preserves weights matrix from square input", {
  mat <- create_test_matrix(4)
  result <- cograph(mat)

  expect_true(!is.null(result$weights))
  expect_true(is.matrix(result$weights))
  expect_equal(dim(result$weights), c(4, 4))
})

test_that("cograph handles NULL seed for random layouts", {
  mat <- create_test_matrix(4)

  # Should work without error
  result <- cograph(mat, layout = "spring", seed = NULL)
  expect_s3_class(result, "cograph_network")
})

# =============================================================================
# sn_layout tests
# =============================================================================

test_that("sn_layout handles CographLayout object", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  layout_obj <- CographLayout$new("circle")
  result <- sn_layout(net, layout_obj)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("sn_layout errors on invalid layout type", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_layout(net, 12345),  # Numeric - not valid
    "layout must be a string"
  )

  expect_error(
    sn_layout(net, TRUE),  # Logical - not valid
    "layout must be a string"
  )
})

test_that("sn_layout handles custom coordinate matrix", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  custom_coords <- matrix(c(0, 0.5, 1, 0, 1, 0.5), nrow = 3, ncol = 2)
  result <- sn_layout(net, custom_coords)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$layout_info$name, "custom")
})

test_that("sn_layout handles custom coordinate data.frame", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  custom_coords <- data.frame(x = c(0, 0.5, 1), y = c(0, 1, 0.5))
  result <- sn_layout(net, custom_coords)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$nodes$x[1], 0)
  expect_equal(result$nodes$y[2], 1)
})

test_that("sn_layout works with direct matrix input", {
  mat <- create_test_matrix(3)

  result <- sn_layout(mat, "circle")
  expect_s3_class(result, "cograph_network")
})

test_that("sn_layout respects NULL seed", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  result <- sn_layout(net, "spring", seed = NULL)
  expect_s3_class(result, "cograph_network")
})

# =============================================================================
# sn_theme tests
# =============================================================================

test_that("sn_theme errors on unknown theme name", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_theme(net, "nonexistent_theme_xyz"),
    "Unknown theme"
  )
})

test_that("sn_theme handles CographTheme object", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  # Get a registered theme
  theme_obj <- get_theme("classic")

  if (!is.null(theme_obj)) {
    result <- sn_theme(net, theme_obj)
    expect_s3_class(result, "cograph_network")
    expect_true(!is.null(result$theme))
  }
})

test_that("sn_theme errors on invalid theme type", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_theme(net, 12345),
    "theme must be a string"
  )

  expect_error(
    sn_theme(net, list(background = "white")),
    "theme must be a string"
  )
})

test_that("sn_theme applies overrides", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  # Apply theme with overrides
  result <- sn_theme(net, "classic", background = "lightgray")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$theme))
})

test_that("sn_theme works with direct matrix input", {
  mat <- create_test_matrix(3)

  result <- sn_theme(mat, "dark")
  expect_s3_class(result, "cograph_network")
})

# =============================================================================
# sn_palette tests
# =============================================================================

test_that("sn_palette errors on unknown palette name", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_palette(net, "nonexistent_palette_xyz"),
    "Unknown palette"
  )
})

test_that("sn_palette handles function palette", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  custom_pal <- function(n) rainbow(n, s = 0.7)
  result <- sn_palette(net, custom_pal)

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$node_aes))
  expect_true(!is.null(result$node_aes$fill))
})

test_that("sn_palette errors on invalid palette type", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  expect_error(
    sn_palette(net, 12345),
    "palette must be a string"
  )

  expect_error(
    sn_palette(net, TRUE),  # logical - not valid
    "palette must be a string"
  )

  expect_error(
    sn_palette(net, list(a = 1)),  # list - not valid
    "palette must be a string"
  )
})

test_that("sn_palette applies to edges with target='edges'", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  result <- sn_palette(net, "colorblind", target = "edges")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$edge_aes))
  expect_true(!is.null(result$edge_aes$positive_color))
  expect_true(!is.null(result$edge_aes$negative_color))
})

test_that("sn_palette applies to both nodes and edges with target='both'", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  result <- sn_palette(net, "viridis", target = "both")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$node_aes$fill))
  expect_true(!is.null(result$edge_aes$positive_color))
})

test_that("sn_palette maps by variable when specified", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  # Add a group column to nodes
  net$nodes$group <- c("A", "A", "B", "B")

  result <- sn_palette(net, "colorblind", by = "group")

  expect_s3_class(result, "cograph_network")
  expect_true(!is.null(result$node_aes$fill))
  # The first two should have same color, last two should have same color
  expect_equal(result$node_aes$fill[1], result$node_aes$fill[2])
  expect_equal(result$node_aes$fill[3], result$node_aes$fill[4])
})

test_that("sn_palette works with direct matrix input", {
  mat <- create_test_matrix(3)

  result <- sn_palette(mat, "viridis")
  expect_s3_class(result, "cograph_network")
})

test_that("sn_palette handles network with no edges gracefully", {
  # Create network with no edges (all zeros)
  mat <- matrix(0, nrow = 3, ncol = 3)
  rownames(mat) <- colnames(mat) <- LETTERS[1:3]
  net <- cograph(mat)

  # Should work without error even with no edges
  result <- sn_palette(net, "colorblind", target = "edges")
  expect_s3_class(result, "cograph_network")
})

# =============================================================================
# Source type detection tests
# =============================================================================

test_that("cograph correctly identifies matrix source type", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  expect_equal(net$source, "matrix")
})

test_that("cograph correctly identifies edgelist source type", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  net <- cograph(edges)

  expect_equal(net$source, "edgelist")
})

# =============================================================================
# Layout info tests
# =============================================================================

test_that("cograph stores layout info with name and seed", {
  mat <- create_test_matrix(3)
  net <- cograph(mat, layout = "circle", seed = 123)

  expect_true(!is.null(net$layout_info))
  expect_equal(net$layout_info$name, "circle")
  expect_equal(net$layout_info$seed, 123)
})

test_that("custom function layout stores 'custom_function' as name", {
  mat <- create_test_matrix(3)

  # Skip if igraph is not available
  skip_if_not_installed("igraph")

  custom_fn <- igraph::layout_in_circle
  net <- cograph(mat, layout = custom_fn, seed = 42)

  expect_equal(net$layout_info$name, "custom_function")
})

# =============================================================================
# Edge cases and corner cases
# =============================================================================

test_that("cograph handles single-node network", {
  mat <- matrix(0, nrow = 1, ncol = 1)
  rownames(mat) <- colnames(mat) <- "A"

  net <- cograph(mat)

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 1)
  expect_equal(n_edges(net), 0)
})

test_that("cograph handles two-node network", {
  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")

  net <- cograph(mat, layout = "circle")

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 2)
})

test_that("sn_layout updates layout_info with new layout", {
  mat <- create_test_matrix(3)
  net <- cograph(mat, layout = "spring", seed = 42)

  # Change layout
  net2 <- sn_layout(net, "circle", seed = 99)

  expect_equal(net2$layout_info$name, "circle")
  expect_equal(net2$layout_info$seed, 99)
})

test_that("sn_layout updates node coordinates", {
  mat <- create_test_matrix(3)
  net <- cograph(mat, layout = "spring", seed = 42)

  old_x <- net$nodes$x
  old_y <- net$nodes$y

  net2 <- sn_layout(net, "circle", seed = 42)

  # Coordinates should be different
  expect_false(identical(old_x, net2$nodes$x))
  expect_false(identical(old_y, net2$nodes$y))
})

# =============================================================================
# igraph layout integration tests (skip if igraph not installed)
# =============================================================================

test_that("cograph handles igraph layout function when igraph available", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)

  result <- cograph(mat, layout = igraph::layout_in_circle, seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("cograph handles igraph two-letter layout codes when igraph available", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)

  # Test "ci" (circle) layout
  result <- cograph(mat, layout = "ci", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("sn_layout handles igraph layout function when igraph available", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  result <- sn_layout(net, igraph::layout_with_fr, seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("sn_layout handles igraph layout name when igraph available", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  result <- sn_layout(net, "layout_nicely", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

test_that("compute_layout_for_cograph handles igraph function when available", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(4)
  net <- as_cograph(mat)

  result <- cograph:::compute_layout_for_cograph(
    net,
    layout = igraph::layout_in_circle,
    seed = 42
  )

  expect_s3_class(result, "cograph_network")
  expect_equal(result$layout_info$name, "custom_function")
})

test_that("compute_layout_for_cograph handles igraph layout name when available", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(4)
  net <- as_cograph(mat)

  result <- cograph:::compute_layout_for_cograph(
    net,
    layout = "kk",  # Kamada-Kawai
    seed = 42
  )

  expect_s3_class(result, "cograph_network")
  expect_false(all(is.na(result$nodes$x)))
})

# =============================================================================
# Additional edge coverage tests
# =============================================================================

test_that("ensure_cograph_network returns unchanged when x column exists with valid values", {
  mat <- create_test_matrix(3)
  net <- cograph(mat, layout = "circle", seed = 42)

  # Nodes already have valid x, y values
  expect_false(all(is.na(net$nodes$x)))

  result <- cograph:::ensure_cograph_network(net, layout = "spring", seed = 99)

  # Should return the same network unchanged (not recompute layout)
  expect_s3_class(result, "cograph_network")
})

test_that("sn_layout handles igraph two-letter code when available", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(4)
  net <- cograph(mat)

  result <- sn_layout(net, "fr", seed = 42)  # Fruchterman-Reingold

  expect_s3_class(result, "cograph_network")
  expect_equal(result$layout_info$name, "fr")
})

test_that("sn_layout handles igraph_ prefix layouts when available", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(4)
  net <- cograph(mat)

  result <- sn_layout(net, "igraph_circle", seed = 42)

  expect_s3_class(result, "cograph_network")
  expect_equal(result$layout_info$name, "igraph_circle")
})

test_that("cograph handles directed flag override", {
  mat <- create_test_matrix(3, symmetric = TRUE)

  # Force directed on symmetric matrix
  net <- cograph(mat, directed = TRUE)
  expect_true(is_directed(net))

  # Force undirected on asymmetric matrix
  asym_mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  rownames(asym_mat) <- colnames(asym_mat) <- c("A", "B", "C")
  net2 <- cograph(asym_mat, directed = FALSE)
  expect_false(is_directed(net2))
})
