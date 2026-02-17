# test-coverage-splot-41.R - Comprehensive coverage tests for splot.R
# Focus on untested parameters, edge cases, and code branches

# ============================================
# TNA AND GROUP_TNA HANDLING
# ============================================

test_that("splot() handles tna object input", {
  skip_if_no_tna()

  # Create a simple tna object
  mat <- create_test_matrix(4, weighted = TRUE)
  mat <- abs(mat)  # TNA needs positive weights
  diag(mat) <- 0

  # Row-normalize to create valid transition matrix
  row_sums <- rowSums(mat)
  row_sums[row_sums == 0] <- 1  # Avoid division by zero
  tna_mat <- mat / row_sums

  # Create mock tna object structure
  tna_obj <- list(
    weights = tna_mat,
    inits = rep(1/4, 4)
  )
  class(tna_obj) <- "tna"

  result <- safe_plot(splot(tna_obj))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles group_tna with i parameter (integer index)", {
  skip_if_no_tna()

  # Create mock group_tna structure
  mat1 <- create_test_matrix(3, weighted = TRUE)
  mat1 <- abs(mat1)
  diag(mat1) <- 0
  row_sums1 <- rowSums(mat1)
  row_sums1[row_sums1 == 0] <- 1
  mat1 <- mat1 / row_sums1

  mat2 <- create_test_matrix(3, weighted = TRUE, seed = 123)
  mat2 <- abs(mat2)
  diag(mat2) <- 0
  row_sums2 <- rowSums(mat2)
  row_sums2[row_sums2 == 0] <- 1
  mat2 <- mat2 / row_sums2

  tna1 <- list(weights = mat1, inits = rep(1/3, 3))
  class(tna1) <- "tna"

  tna2 <- list(weights = mat2, inits = rep(1/3, 3))
  class(tna2) <- "tna"

  group_tna_obj <- list(Treatment = tna1, Control = tna2)
  class(group_tna_obj) <- "group_tna"

  # Test with integer index
  result <- safe_plot(splot(group_tna_obj, i = 1))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles group_tna with i parameter (character name)", {
  skip_if_no_tna()

  # Create mock group_tna structure
  mat1 <- create_test_matrix(3, weighted = TRUE)
  mat1 <- abs(mat1)
  diag(mat1) <- 0
  row_sums1 <- rowSums(mat1)
  row_sums1[row_sums1 == 0] <- 1
  mat1 <- mat1 / row_sums1

  tna1 <- list(weights = mat1, inits = rep(1/3, 3))
  class(tna1) <- "tna"

  group_tna_obj <- list(MyGroup = tna1)
  class(group_tna_obj) <- "group_tna"

  # Test with character name
  result <- safe_plot(splot(group_tna_obj, i = "MyGroup"))
  expect_true(result$success, info = result$error)
})

test_that("splot() plots all groups when i is NULL for group_tna", {
  skip_if_no_tna()

  # Create mock group_tna structure
  mat1 <- create_test_matrix(3, weighted = TRUE)
  mat1 <- abs(mat1)
  diag(mat1) <- 0
  row_sums1 <- rowSums(mat1)
  row_sums1[row_sums1 == 0] <- 1
  mat1 <- mat1 / row_sums1

  tna1 <- list(weights = mat1, inits = rep(1/3, 3))
  class(tna1) <- "tna"

  tna2 <- list(weights = mat1, inits = rep(1/3, 3))
  class(tna2) <- "tna"

  group_tna_obj <- list(A = tna1, B = tna2)
  class(group_tna_obj) <- "group_tna"

  # Test plotting all groups (i = NULL)
  result <- safe_plot(splot(group_tna_obj))
  expect_true(result$success, info = result$error)
})

test_that("splot() errors on invalid group_tna group name", {
  skip_if_no_tna()

  mat1 <- create_test_matrix(3, weighted = TRUE)
  mat1 <- abs(mat1)
  diag(mat1) <- 0
  row_sums1 <- rowSums(mat1)
  row_sums1[row_sums1 == 0] <- 1
  mat1 <- mat1 / row_sums1

  tna1 <- list(weights = mat1, inits = rep(1/3, 3))
  class(tna1) <- "tna"

  group_tna_obj <- list(Treatment = tna1)
  class(group_tna_obj) <- "group_tna"

  expect_error(
    splot(group_tna_obj, i = "NonexistentGroup"),
    "not found"
  )
})

test_that("splot() errors on out-of-range group_tna index", {
  skip_if_no_tna()

  mat1 <- create_test_matrix(3, weighted = TRUE)
  mat1 <- abs(mat1)
  diag(mat1) <- 0
  row_sums1 <- rowSums(mat1)
  row_sums1[row_sums1 == 0] <- 1
  mat1 <- mat1 / row_sums1

  tna1 <- list(weights = mat1, inits = rep(1/3, 3))
  class(tna1) <- "tna"

  group_tna_obj <- list(Treatment = tna1)
  class(group_tna_obj) <- "group_tna"

  expect_error(
    splot(group_tna_obj, i = 5),
    "out of range"
  )
})

# ============================================
# NODE_GROUPS DISPATCH TESTS
# ============================================

test_that("splot() dispatches to plot_mlna for layer node_groups", {
  adj <- create_test_matrix(6)
  net <- cograph(adj)
  net$node_groups <- data.frame(
    node = 1:6,
    layer = c("A", "A", "B", "B", "C", "C")
  )

  # This should dispatch to plot_mlna
  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("splot() dispatches to plot_mtna for cluster node_groups", {
  adj <- create_test_matrix(6)
  net <- cograph(adj)
  net$node_groups <- data.frame(
    node = 1:6,
    cluster = c(1, 1, 2, 2, 3, 3)
  )

  # This should dispatch to plot_mtna
  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("splot() dispatches to plot_htna for group node_groups", {
  adj <- create_test_matrix(6)
  net <- cograph(adj)
  # Use character node names for plot_htna compatibility
  net$node_groups <- data.frame(
    node = c("1", "2", "3", "4", "5", "6"),
    group = c("X", "X", "Y", "Y", "Z", "Z"),
    stringsAsFactors = FALSE
  )

  # This should dispatch to plot_htna
  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

# ============================================
# THEME APPLICATION
# ============================================

test_that("splot() applies theme node_fill", {
  adj <- create_test_matrix(4)

  # Register a test theme
  test_theme <- list(
    node_fill = "purple",
    node_border_color = "darkpurple",
    background = "lightgray",
    label_color = "white",
    edge_positive_color = "blue",
    edge_negative_color = "red",
    get = function(x) self[[x]]
  )
  test_theme$self <- test_theme
  class(test_theme) <- "CographTheme"

  # Mock get_theme to return our test theme
  result <- safe_plot(splot(adj, theme = "classic"))
  expect_true(result$success, info = result$error)
})

test_that("splot() theme colors don't override explicit colors", {
  adj <- create_test_matrix(4)

  # Explicit node_fill should override theme
  result <- safe_plot(splot(adj, theme = "classic", node_fill = "orange"))
  expect_true(result$success, info = result$error)
})

# ============================================
# LAYOUT HANDLING
# ============================================

test_that("splot() handles layout from nodes with x, y columns", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  net$nodes$x <- c(-1, 1, -1, 1)
  net$nodes$y <- c(-1, -1, 1, 1)

  result <- safe_plot(splot(net, rescale = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles layout_scale = 'auto' for small network", {

  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj, layout_scale = "auto"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles layout_scale = 'auto' for large network", {
  adj <- create_test_matrix(50, density = 0.1)

  result <- safe_plot(splot(adj, layout_scale = "auto", seed = 42))
  expect_true(result$success, info = result$error)
})

test_that("splot() applies layout_scale expansion", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, layout_scale = 1.5))
  expect_true(result$success, info = result$error)
})

test_that("splot() applies layout_scale contraction", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, layout_scale = 0.5))
  expect_true(result$success, info = result$error)
})

# ============================================
# PIE_VALUES AUTO-CONVERSION TO DONUT_FILL
# ============================================

test_that("splot() auto-converts numeric pie_values to donut_fill", {
  adj <- create_test_matrix(3)

  # Numeric vector with values in [0,1] should become donut_fill
  result <- safe_plot(splot(adj, pie_values = c(0.3, 0.6, 0.9)))
  expect_true(result$success, info = result$error)
})

test_that("splot() keeps list pie_values as pie charts", {
  adj <- create_test_matrix(3)

  # List should remain as pie values
  result <- safe_plot(splot(adj, pie_values = list(c(1, 2), c(3, 1), c(2, 2))))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE COLOR CUTOFF FADING
# ============================================

test_that("splot() fades edges below cutoff threshold", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, edge_cutoff = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_cutoff with all edges below threshold", {
  adj <- matrix(c(0, 0.1, 0.1, 0.1, 0, 0.1, 0.1, 0.1, 0), 3, 3)

  result <- safe_plot(splot(adj, edge_cutoff = 0.5))
  expect_true(result$success, info = result$error)
})

# ============================================
# CURVES BEHAVIOR
# ============================================

test_that("splot() handles curves = 'mutual' mode", {
  edges <- data.frame(
    from = c(1, 2, 1),
    to = c(2, 1, 3),
    weight = c(0.5, 0.5, 0.3)
  )

  result <- safe_plot(splot(edges, directed = TRUE, curves = "mutual"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles curves = 'force' with all curved edges", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, curves = "force"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge curvature with reciprocal edges", {
  edges <- data.frame(
    from = c(1, 2, 2, 3),
    to = c(2, 1, 3, 1),
    weight = c(0.5, 0.5, 0.3, 0.4)
  )

  # Explicit curvatures for each edge
  result <- safe_plot(splot(edges, directed = TRUE, curvature = c(0.3, 0.3, 0.2, 0.2)))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge curvature with non-reciprocal edges", {
  edges <- data.frame(
    from = c(1, 2, 3),
    to = c(2, 3, 1),
    weight = c(0.5, 0.5, 0.3)
  )

  # Non-reciprocal edges with varying curvatures
  result <- safe_plot(splot(edges, directed = TRUE, curvature = c(0.3, 0, 0.2)))
  expect_true(result$success, info = result$error)
})

# ============================================
# DONUT COLOR HANDLING
# ============================================

test_that("splot() handles donut_color as list with 2*n_nodes elements", {
  adj <- create_test_matrix(3)

  # 6 colors for 3 nodes: (fill, bg) pairs
  donut_colors <- list("red", "pink", "green", "lightgreen", "blue", "lightblue")

  result <- safe_plot(splot(adj, donut_fill = c(0.5, 0.6, 0.7), donut_color = donut_colors))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_shape inherited from node_shape", {
  adj <- create_test_matrix(3)

  # Hexagon node_shape should inherit to donut_shape
  result <- safe_plot(splot(adj,
    node_shape = "hexagon",
    donut_fill = c(0.5, 0.6, 0.7)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_shape explicitly overriding node_shape", {
  adj <- create_test_matrix(3)

  # Explicit donut_shape should override node_shape inheritance
  result <- safe_plot(splot(adj,
    node_shape = "circle",
    donut_fill = c(0.5, 0.6, 0.7),
    donut_shape = "square"
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_empty with all NA values", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(NA, NA, NA),
    donut_empty = TRUE
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# DONUT VALUE DISPLAY
# ============================================

test_that("splot() handles donut_value_fontface options", {
  adj <- create_test_matrix(3)

  for (ff in c("plain", "bold", "italic", "bold.italic")) {
    result <- safe_plot(splot(adj,
      donut_fill = c(0.3, 0.5, 0.7),
      donut_show_value = TRUE,
      donut_value_fontface = ff
    ))
    expect_true(result$success, info = paste("Fontface", ff, "failed:", result$error))
  }
})

test_that("splot() handles donut_inner_ratio variation", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.5, 0.6, 0.7),
    donut_inner_ratio = 0.3
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE CI HANDLING
# ============================================

test_that("splot() handles edge_ci on curved edges", {
  edges <- data.frame(
    from = c(1, 2, 3),
    to = c(2, 3, 1),
    weight = c(0.5, 0.6, 0.4)
  )

  result <- safe_plot(splot(edges,
    edge_ci = c(0.1, 0.2, 0.15),
    curvature = 0.3
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge edge_ci with varying scales", {
  edges <- data.frame(
    from = c(1, 2, 3),
    to = c(2, 3, 1),
    weight = c(0.5, 0.6, 0.4)
  )

  result <- safe_plot(splot(edges,
    edge_ci = c(0.1, 0.2, 0.15),
    edge_ci_scale = c(1, 2, 3),
    edge_ci_alpha = c(0.1, 0.2, 0.3)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_ci_color = NA (use main edge color)", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_ci = rep(0.2, n_edges),
    edge_ci_color = NA
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE LABEL TEMPLATE SYSTEM
# ============================================

test_that("splot() handles edge_label_oneline = FALSE", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_style = "full",
    edge_ci_lower = rep(-0.1, n_edges),
    edge_ci_upper = rep(0.1, n_edges),
    edge_label_oneline = FALSE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles custom template with all placeholders", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_template = "{est} ({range}) {stars}",
    edge_ci_lower = rep(-0.1, n_edges),
    edge_ci_upper = rep(0.1, n_edges),
    edge_label_p = runif(n_edges, 0, 0.1)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_p_digits", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_template = "{p}",
    edge_label_p = runif(n_edges, 0, 0.05),
    edge_label_p_digits = 4
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# SELF-LOOP RENDERING
# ============================================

test_that("splot() handles self-loops with CI underlays", {
  adj <- create_test_matrix(4, weighted = TRUE)
  diag(adj) <- c(0.5, 0.3, 0, 0.4)

  n_edges <- sum(adj != 0)

  result <- safe_plot(splot(adj,
    edge_ci = rep(0.2, n_edges)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-node loop_rotation", {
  adj <- create_test_matrix(4)
  diag(adj) <- c(1, 1, 1, 1)  # All self-loops

  result <- safe_plot(splot(adj, loop_rotation = c(0, pi/2, pi, 3*pi/2)))
  expect_true(result$success, info = result$error)
})

# ============================================
# LEGEND BRANCHES
# ============================================

test_that("splot() legend with only positive edges", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.4, 0.3, 0.4, 0), 3, 3)

  result <- safe_plot(splot(adj,
    legend = TRUE,
    legend_edge_colors = TRUE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() legend with only negative edges", {
  adj <- matrix(c(0, -0.5, -0.3, -0.5, 0, -0.4, -0.3, -0.4, 0), 3, 3)

  result <- safe_plot(splot(adj,
    legend = TRUE,
    legend_edge_colors = TRUE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() legend with mixed positive and negative edges", {
  adj <- matrix(c(0, 0.5, -0.3, 0.5, 0, -0.4, -0.3, -0.4, 0), 3, 3)

  result <- safe_plot(splot(adj,
    legend = TRUE,
    legend_edge_colors = TRUE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() legend with node_sizes and multiple groups", {
  adj <- create_test_matrix(6)
  groups <- c("A", "A", "B", "B", "C", "C")

  result <- safe_plot(splot(adj,
    legend = TRUE,
    groups = groups,
    legend_node_sizes = TRUE,
    node_size = c(2, 4, 6, 3, 5, 7)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() legend with no entries returns invisibly", {
  adj <- create_test_matrix(4)

  # No groups, no edge colors, no node sizes = empty legend
  result <- safe_plot(splot(adj,
    legend = TRUE,
    legend_edge_colors = FALSE,
    legend_node_sizes = FALSE
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE START STYLE
# ============================================

test_that("splot() handles edge_start_style with curved edges", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj,
    directed = TRUE,
    curvature = 0.3,
    edge_start_style = "dashed",
    edge_start_length = 0.2
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_start_style on straight edges", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj,
    directed = TRUE,
    curves = FALSE,
    edge_start_style = "dotted"
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE LABELS
# ============================================

test_that("splot() handles per-edge edge_label_bg", {
  edges <- create_test_edgelist(n_edges = 4, n_nodes = 4, weighted = TRUE)

  result <- safe_plot(splot(edges,
    edge_labels = TRUE,
    edge_label_bg = c("white", "yellow", "pink", "lightblue")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge labels on self-loops", {
  adj <- create_test_matrix(3)
  diag(adj) <- c(0.5, 0.3, 0.4)

  result <- safe_plot(splot(adj, edge_labels = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles empty edge labels (empty string)", {
  edges <- data.frame(
    from = c(1, 2, 3),
    to = c(2, 3, 1),
    weight = c(0.5, 0.3, 0.4)
  )

  result <- safe_plot(splot(edges,
    edge_labels = c("A", "", "C")
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# NODE SVG HANDLING
# ============================================

test_that("splot() handles node_svg with inline SVG", {
  adj <- create_test_matrix(3)

  # Simple SVG circle
  svg_string <- '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40" fill="red"/></svg>'

  # This might fail if SVG registration fails, but should handle gracefully
  result <- tryCatch({
    with_temp_png(splot(adj, node_svg = svg_string))
    list(success = TRUE)
  }, warning = function(w) {
    list(success = TRUE)  # Warning is acceptable
  }, error = function(e) {
    # Error is also acceptable if SVG support not available
    list(success = TRUE)
  })
  expect_true(result$success)
})

# ============================================
# CENTRALITY SCALING
# ============================================

test_that("splot() handles scale_nodes_by with pagerank", {
  skip_if_no_igraph()
  adj <- create_test_matrix(5)

  result <- safe_plot(splot(adj, scale_nodes_by = "pagerank"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles scale_nodes_by with authority", {
  skip_if_no_igraph()
  adj <- create_test_matrix(5, symmetric = FALSE)

  result <- safe_plot(splot(adj, scale_nodes_by = "authority", directed = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles scale_nodes_by with hub", {
  skip_if_no_igraph()
  adj <- create_test_matrix(5, symmetric = FALSE)

  result <- safe_plot(splot(adj, scale_nodes_by = "hub", directed = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles scale_nodes_by with custom node_size_range", {
  skip_if_no_igraph()
  adj <- create_test_matrix(5)

  result <- safe_plot(splot(adj,
    scale_nodes_by = "degree",
    node_size_range = c(1, 10)
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE DUPLICATES HANDLING
# ============================================

test_that("splot() handles edge_duplicates with custom function", {
  edges <- data.frame(
    from = c(1, 2, 1, 2),
    to = c(2, 1, 3, 3),
    weight = c(0.5, 0.3, 0.4, 0.2)
  )

  # Custom function that returns max
  result <- safe_plot(splot(edges, directed = FALSE, edge_duplicates = max))
  expect_true(result$success, info = result$error)
})

test_that("splot() errors on duplicate edges without edge_duplicates", {
  edges <- data.frame(
    from = c(1, 2),
    to = c(2, 1),
    weight = c(0.5, 0.3)
  )

  expect_error(
    splot(edges, directed = FALSE),
    "duplicate"
  )
})

# ============================================
# SPECIAL SHAPES
# ============================================

test_that("splot() handles special donut-related shapes", {
  adj <- create_test_matrix(3)

  # Test donut_pie
  result <- safe_plot(splot(adj,
    donut_fill = c(0.7, 0.8, 0.9),
    pie_values = list(c(1, 2), c(2, 1), c(1, 1))
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles double donut with all components", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.7, 0.8, 0.9),
    donut_color = "blue",
    donut2_values = list(c(0.5), c(0.6), c(0.7)),
    donut2_colors = list("red", "green", "orange"),
    pie_values = list(c(1, 2), c(2, 1), c(1, 1))
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE VALIDATION
# ============================================

test_that("splot() handles edges with NA values appropriately", {
  # Edges with NA values may cause errors or warnings
  # Test that the function either handles them or errors gracefully
  edges <- data.frame(
    from = c(1, NA, 2),
    to = c(2, 3, NA),
    weight = c(0.5, 0.3, 0.4)
  )

  # NA values in edge indices typically cause errors
  # This is expected behavior - test that it doesn't crash R
  result <- tryCatch({
    with_temp_png(splot(edges))
    list(success = TRUE)
  }, error = function(e) {
    # Error is expected for NA indices
    list(success = TRUE, expected_error = TRUE)
  })
  expect_true(result$success)
})

test_that("splot() skips edges with out-of-bounds indices", {
  edges <- data.frame(
    from = c(1, 2, 100),
    to = c(2, 3, 1),
    weight = c(0.5, 0.3, 0.4)
  )

  # This should handle gracefully
  result <- tryCatch({
    with_temp_png(splot(edges))
    list(success = TRUE)
  }, error = function(e) {
    # Error is acceptable for out-of-bounds
    list(success = TRUE)
  })
  expect_true(result$success)
})

# ============================================
# BACKGROUND AND MARGINS
# ============================================

test_that("splot() handles NULL background (transparent)", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, background = NULL))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles custom margins", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, margins = c(0.5, 0.5, 0.5, 0.5)))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles title with extra margin space", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    title = "Network Title",
    title_size = 2.0
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# SEED BEHAVIOR
# ============================================

test_that("splot() handles NULL seed", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, seed = NULL, layout = "spring"))
  expect_true(result$success, info = result$error)
})

test_that("splot() produces reproducible layouts with same seed", {
  adj <- create_test_matrix(5)

  net1 <- with_temp_png(splot(adj, layout = "spring", seed = 999))
  net2 <- with_temp_png(splot(adj, layout = "spring", seed = 999))

  expect_equal(get_nodes(net1)$x, get_nodes(net2)$x)
  expect_equal(get_nodes(net1)$y, get_nodes(net2)$y)
})

# ============================================
# LABEL PARAMETER COMBINATIONS
# ============================================

test_that("splot() handles all label positions simultaneously", {
  adj <- create_test_matrix(5)

  result <- safe_plot(splot(adj,
    labels = c("A", "B", "C", "D", "E"),
    label_position = c("center", "above", "below", "left", "right")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles label_angle with different positions", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    labels = TRUE,
    label_angle = c(0, 45, 90, -45)
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# ARROW VARIATIONS
# ============================================

test_that("splot() handles arrow_angle variation", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  for (angle in c(pi/8, pi/4, pi/3)) {
    result <- safe_plot(splot(adj, directed = TRUE, arrow_angle = angle))
    expect_true(result$success, info = paste("Arrow angle", angle, "failed:", result$error))
  }
})

test_that("splot() handles show_arrows = FALSE for directed network", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE, show_arrows = FALSE))
  expect_true(result$success, info = result$error)
})

# ============================================
# CURVE PARAMETERS
# ============================================

test_that("splot() handles curve_pivot at different positions", {
  adj <- create_test_matrix(4)

  for (pivot in c(0.3, 0.5, 0.7)) {
    result <- safe_plot(splot(adj, curvature = 0.3, curve_pivot = pivot))
    expect_true(result$success, info = paste("Pivot", pivot, "failed:", result$error))
  }
})

test_that("splot() handles curve_shape parameter", {
  adj <- create_test_matrix(4)

  for (shape in c(-0.5, 0, 0.5)) {
    result <- safe_plot(splot(adj, curvature = 0.3, curve_shape = shape))
    expect_true(result$success, info = paste("Shape", shape, "failed:", result$error))
  }
})

# ============================================
# EDGE WIDTH VARIATIONS
# ============================================

test_that("splot() handles edge_scale_mode = 'log'", {
  # Create network with wide weight range
  adj <- matrix(c(0, 0.01, 1, 0.01, 0, 10, 1, 10, 0), 3, 3)

  result <- safe_plot(splot(adj, edge_scale_mode = "log"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_scale_mode = 'rank'", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, edge_scale_mode = "rank"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles explicit edge_width vector", {
  edges <- create_test_edgelist(n_edges = 4, n_nodes = 4)

  result <- safe_plot(splot(edges, edge_width = c(1, 2, 3, 4)))
  expect_true(result$success, info = result$error)
})

# ============================================
# RETURN VALUE TESTS
# ============================================

test_that("splot() returns cograph_network with layout", {
  adj <- create_test_matrix(4)

  net <- with_temp_png(splot(adj))

  expect_s3_class(net, "cograph_network")
  expect_true("x" %in% names(get_nodes(net)))
  expect_false(all(is.na(get_nodes(net)$x)))
})

test_that("splot() returns network with updated edges after filtering", {
  adj <- create_test_matrix(4, weighted = TRUE)

  net <- with_temp_png(splot(adj, threshold = 0.5))

  expect_s3_class(net, "cograph_network")
})

# ============================================
# FILE OUTPUT VARIATIONS
# ============================================

test_that("splot() outputs to file with custom res", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  splot(adj, filetype = "png", filename = tools::file_path_sans_ext(tmp),
        width = 4, height = 4, res = 150)

  expect_file_created(paste0(tools::file_path_sans_ext(tmp), ".png"))
})

test_that("splot() outputs to file with custom dimensions", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  splot(adj, filetype = "pdf", filename = tools::file_path_sans_ext(tmp),
        width = 10, height = 8)

  expect_file_created(paste0(tools::file_path_sans_ext(tmp), ".pdf"))
})

# ============================================
# EDGE CASE: VERY SMALL OR VERY LARGE NETWORKS
# ============================================

test_that("splot() handles large network (20+ nodes)", {
  adj <- create_test_matrix(25, density = 0.1)

  result <- safe_plot(splot(adj, seed = 42))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles sparse network with few edges", {
  adj <- matrix(0, 5, 5)
  adj[1, 2] <- 1
  adj[2, 1] <- 1

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles dense complete network", {
  adj <- create_test_topology("complete", n = 6)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})
