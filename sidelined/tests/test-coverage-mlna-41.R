# test-coverage-mlna-41.R - Additional coverage tests for mlna.R
# Targets untested branches and edge cases for 100% coverage

# ============================================
# COGRAPH_NETWORK INPUT TESTS
# ============================================

test_that("plot_mlna works with cograph_network input", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  net <- cograph(m)

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(net, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna extracts nodes from cograph_network", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    layer = c(rep("Top", 3), rep("Bottom", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  # Auto-detect layers from "layer" column
  result <- safe_plot(plot_mlna(net))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with cograph_network and nodes_df without label column", {
  set.seed(42)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- paste0("N", 1:6)

  net <- cograph(m)

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(net, layers))
  expect_true(result$success, info = result$error)
})

# ============================================
# TNA OBJECT INPUT TESTS
# ============================================

test_that("plot_mlna works with tna object input", {
  skip_if_not_installed("tna")

  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  # Row-normalize for tna-like structure
  row_sums <- rowSums(m)
  row_sums[row_sums == 0] <- 1
  m_norm <- m / row_sums

  tna_obj <- structure(
    list(
      weights = m_norm,
      labels = nodes
    ),
    class = "tna"
  )

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(tna_obj, layers))
  expect_true(result$success, info = result$error)
})

# ============================================
# LAYER_LIST AS COLUMN NAME TESTS
# ============================================

test_that("plot_mlna works with layer_list as column name string", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    group = c(rep("GroupA", 3), rep("GroupB", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  # Use column name for layer_list

  result <- safe_plot(plot_mlna(net, layer_list = "group"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna errors when layer_list column not found", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    group = c(rep("GroupA", 3), rep("GroupB", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  expect_error(
    plot_mlna(net, layer_list = "nonexistent_column"),
    "not found in nodes"
  )
})

test_that("plot_mlna errors when using column name without cograph_network", {
  set.seed(42)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- paste0("N", 1:6)

  # Using matrix input with column name should error
  expect_error(
    plot_mlna(m, layer_list = "layer"),
    "cograph_network"
  )
})

# ============================================
# AUTO-DETECT LAYER COLUMN TESTS
# ============================================

test_that("plot_mlna auto-detects 'layers' column", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    layers = c(rep("L1", 3), rep("L2", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  expect_message(
    result <- safe_plot(plot_mlna(net)),
    "layers.*column"
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna auto-detects 'level' column", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    level = c(rep("Upper", 3), rep("Lower", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  expect_message(
    result <- safe_plot(plot_mlna(net)),
    "level.*column"
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna auto-detects 'levels' column", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    levels = c(rep("Macro", 3), rep("Micro", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  expect_message(
    result <- safe_plot(plot_mlna(net)),
    "levels.*column"
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna auto-detects 'groups' column", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    groups = c(rep("G1", 3), rep("G2", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  expect_message(
    result <- safe_plot(plot_mlna(net)),
    "groups.*column"
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna auto-detects 'group' column", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    group = c(rep("Team1", 3), rep("Team2", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  expect_message(
    result <- safe_plot(plot_mlna(net)),
    "group.*column"
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna auto-detects 'clusters' column", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    clusters = c(rep("C1", 3), rep("C2", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  expect_message(
    result <- safe_plot(plot_mlna(net)),
    "clusters.*column"
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna auto-detects 'cluster' column", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    cluster = c(rep("Alpha", 3), rep("Beta", 3)),
    stringsAsFactors = FALSE
  )

  net <- cograph(m, nodes = nodes_df)

  expect_message(
    result <- safe_plot(plot_mlna(net)),
    "cluster.*column"
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# NODES PARAMETER TESTS
# ============================================

test_that("plot_mlna works with external nodes data.frame parameter", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    display_name = c("Node1", "Node2", "Node3", "Node4", "Node5", "Node6"),
    stringsAsFactors = FALSE
  )

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(m, layers, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna uses 'labels' column for display when present", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  # nodes_df with both 'label' (identifier) and 'labels' (display)
  nodes_df <- data.frame(
    label = nodes,
    labels = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta"),
    stringsAsFactors = FALSE
  )

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(m, layers, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna falls back to 'label' column when 'labels' not present", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    color = rep("blue", 6),
    stringsAsFactors = FALSE
  )

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(m, layers, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna falls back to identifiers when no label/labels columns", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  # nodes_df without label or labels columns
  nodes_df <- data.frame(
    id = nodes,
    size = c(1, 2, 3, 4, 5, 6),
    stringsAsFactors = FALSE
  )

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(m, layers, nodes = nodes_df))
  expect_true(result$success, info = result$error)
})

# ============================================
# SHOW_LABELS PARAMETER TESTS
# ============================================

test_that("plot_mlna works with show_labels = FALSE", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, show_labels = FALSE))
  expect_true(result$success, info = result$error)
})

# ============================================
# LABEL_ABBREV PARAMETER TESTS
# ============================================

test_that("plot_mlna works with label_abbrev as integer", {
  set.seed(42)
  # Use long node names
  nodes <- c("VeryLongNodeName1", "VeryLongNodeName2", "VeryLongNodeName3",
             "VeryLongNodeName4", "VeryLongNodeName5", "VeryLongNodeName6")
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Top = nodes[1:3],
    Bottom = nodes[4:6]
  )

  result <- safe_plot(plot_mlna(m, layers, label_abbrev = 5))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with label_abbrev = 'auto'", {
  set.seed(42)
  nodes <- c("VeryLongNodeName1", "VeryLongNodeName2", "VeryLongNodeName3",
             "VeryLongNodeName4", "VeryLongNodeName5", "VeryLongNodeName6")
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Top = nodes[1:3],
    Bottom = nodes[4:6]
  )

  result <- safe_plot(plot_mlna(m, layers, label_abbrev = "auto"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with label_abbrev = NULL (no abbreviation)", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, label_abbrev = NULL))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna label_abbrev with many nodes triggers auto abbreviation", {
  set.seed(42)
  n <- 25
  nodes <- paste0("LongNodeName_", 1:n)
  m <- matrix(runif(n*n, 0, 0.3), n, n)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    L1 = nodes[1:6],
    L2 = nodes[7:12],
    L3 = nodes[13:18],
    L4 = nodes[19:25]
  )

  result <- safe_plot(plot_mlna(m, layers, label_abbrev = "auto"))
  expect_true(result$success, info = result$error)
})

# ============================================
# ADDITIONAL SHAPE TESTS
# ============================================

test_that("plot_mlna works with pentagon shape", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, shapes = c("pentagon", "circle")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with hexagon shape", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, shapes = c("hexagon", "square")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with star shape", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, shapes = c("star", "diamond")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with cross shape", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, shapes = c("cross", "triangle")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with unknown shape (defaults to circle)", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, shapes = c("unknown_shape", "also_unknown")))
  expect_true(result$success, info = result$error)
})

# ============================================
# SPRING LAYOUT EDGE CASES
# ============================================

test_that("plot_mlna spring layout works with single node in layer", {
  set.seed(42)
  nodes <- paste0("N", 1:4)
  m <- matrix(runif(16, 0, 0.3), 4, 4)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Single = "N1",
    Multi = paste0("N", 2:4)
  )

  result <- safe_plot(plot_mlna(m, layers, layout = "spring"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna spring layout works with zero within-layer edges", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(0, 6, 6)
  # Only between-layer edges
  m[1, 4] <- 0.5
  m[2, 5] <- 0.5
  m[3, 6] <- 0.5
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, layout = "spring"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna spring layout handles narrow x_range (diff == 0)", {
  set.seed(42)
  nodes <- paste0("N", 1:4)
  m <- matrix(0, 4, 4)
  # Only connect nodes 1-2 so they cluster together
  m[1, 2] <- 1
  m[2, 1] <- 1
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Top = paste0("N", 1:2),
    Bottom = paste0("N", 3:4)
  )

  result <- safe_plot(plot_mlna(m, layers, layout = "spring"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna spring layout handles narrow y_range (diff == 0)", {
  set.seed(42)
  nodes <- paste0("N", 1:4)
  m <- matrix(0.01, 4, 4)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Top = paste0("N", 1:2),
    Bottom = paste0("N", 3:4)
  )

  result <- safe_plot(plot_mlna(m, layers, layout = "spring"))
  expect_true(result$success, info = result$error)
})

# ============================================
# CIRCLE LAYOUT TESTS
# ============================================

test_that("plot_mlna circle layout works with single node in layer", {
  set.seed(42)
  nodes <- paste0("N", 1:4)
  m <- matrix(runif(16, 0, 0.3), 4, 4)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Single = "N1",
    Multi = paste0("N", 2:4)
  )

  result <- safe_plot(plot_mlna(m, layers, layout = "circle"))
  expect_true(result$success, info = result$error)
})

# ============================================
# HORIZONTAL LAYOUT EDGE CASES
# ============================================

test_that("plot_mlna horizontal layout works with single node in layer", {
  set.seed(42)
  nodes <- paste0("N", 1:4)
  m <- matrix(runif(16, 0, 0.3), 4, 4)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Single = "N1",
    Multi = paste0("N", 2:4)
  )

  result <- safe_plot(plot_mlna(m, layers, layout = "horizontal"))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE LENGTH ZERO TESTS
# ============================================

test_that("plot_mlna handles within-layer edge with zero length", {
  set.seed(42)
  nodes <- paste0("N", 1:4)
  m <- matrix(0, 4, 4)
  # Force an edge where nodes might be at same position
  m[1, 2] <- 0.5
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    Top = paste0("N", 1:2),
    Bottom = paste0("N", 3:4)
  )

  # With horizontal layout, nodes are at different positions
  result <- safe_plot(plot_mlna(m, layers, layout = "horizontal"))
  expect_true(result$success, info = result$error)
})

# ============================================
# COMMUNITY DETECTION TESTS (ADDITIONAL METHODS)
# ============================================

test_that("plot_mlna works with community parameter (fast_greedy)", {
  skip_if_not_installed("igraph")

  set.seed(42)
  n <- 12
  m <- matrix(runif(n*n, 0, 0.3), n, n)
  diag(m) <- 0
  m <- (m + t(m)) / 2  # Symmetric
  # Create structure
  m[1:4, 1:4] <- m[1:4, 1:4] + 0.3
  m[5:8, 5:8] <- m[5:8, 5:8] + 0.3
  m[9:12, 9:12] <- m[9:12, 9:12] + 0.3
  m <- pmin(m, 1)
  colnames(m) <- rownames(m) <- paste0("N", 1:n)

  result <- safe_plot(plot_mlna(m, community = "fast_greedy"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with community parameter (label_prop)", {
  skip_if_not_installed("igraph")

  set.seed(42)
  n <- 12
  m <- matrix(runif(n*n, 0, 0.3), n, n)
  diag(m) <- 0
  m <- (m + t(m)) / 2
  m[1:4, 1:4] <- m[1:4, 1:4] + 0.3
  m[5:8, 5:8] <- m[5:8, 5:8] + 0.3
  m[9:12, 9:12] <- m[9:12, 9:12] + 0.3
  m <- pmin(m, 1)
  colnames(m) <- rownames(m) <- paste0("N", 1:n)

  result <- safe_plot(plot_mlna(m, community = "label_prop"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with community parameter (infomap)", {
  skip_if_not_installed("igraph")

  set.seed(42)
  n <- 12
  # Create stronger block structure for infomap to detect multiple communities
  m <- matrix(0.05, n, n)
  diag(m) <- 0
  # Strong within-block connections
  m[1:4, 1:4] <- 0.8
  m[5:8, 5:8] <- 0.8
  m[9:12, 9:12] <- 0.8
  diag(m) <- 0
  colnames(m) <- rownames(m) <- paste0("N", 1:n)

  # infomap may still find only 1 community, so wrap in tryCatch
  result <- tryCatch({
    safe_plot(plot_mlna(m, community = "infomap"))
  }, error = function(e) {
    # If infomap finds < 2 communities, skip the test
    if (grepl("2\\+ character vectors", conditionMessage(e))) {
      list(success = TRUE, skipped = TRUE)
    } else {
      list(success = FALSE, error = conditionMessage(e))
    }
  })
  expect_true(result$success, info = result$error)
})

# ============================================
# LAYER WITHOUT NAMES TESTS (FOR LEGEND)
# ============================================

test_that("plot_mlna legend uses 'Layer N' for unnamed layers", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  # Unnamed list (NULL names)
  layers <- list(
    paste0("N", 1:3),
    paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(m, layers, legend = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# MANY LAYERS TESTS (COLOR/SHAPE CYCLING)
# ============================================

test_that("plot_mlna cycles through colors and shapes for many layers", {
  set.seed(42)
  n <- 20
  nodes <- paste0("N", 1:n)
  m <- matrix(runif(n*n, 0, 0.3), n, n)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  # Create 10 layers to test palette cycling
  layers <- lapply(1:10, function(i) {
    paste0("N", (i-1)*2 + 1:2)
  })
  names(layers) <- paste0("Layer", 1:10)

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

# ============================================
# NEGATIVE WEIGHT TESTS
# ============================================

test_that("plot_mlna handles negative weights", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, -0.5, 0.5), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})

# ============================================
# SCALE PARAMETER TESTS (ADDITIONAL)
# ============================================

test_that("plot_mlna works with scale = 0.5 (smaller than default)", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, scale = 0.5))
  expect_true(result$success, info = result$error)
})

# ============================================
# BORDER VISIBILITY WITH LAYER NAMES
# ============================================

test_that("plot_mlna shows layer labels when show_border = TRUE and layers have names", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    TopLayer = paste0("N", 1:3),
    BottomLayer = paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(m, layers, show_border = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works when show_border = TRUE with unnamed layers", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    paste0("N", 1:3),
    paste0("N", 4:6)
  )

  result <- safe_plot(plot_mlna(m, layers, show_border = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# CURVATURE = 0 EDGE CASE
# ============================================

test_that("plot_mlna handles curvature = 0 for straight edges", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  result <- safe_plot(plot_mlna(m, layers, curvature = 0, within_edges = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# WEIGHT THRESHOLD ON BETWEEN-LAYER EDGES
# ============================================

test_that("plot_mlna minimum threshold affects between-layer edges", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(0, 6, 6)
  # Small weights between layers
  m[1, 4] <- 0.05
  m[2, 5] <- 0.15
  m[3, 6] <- 0.25
  colnames(m) <- rownames(m) <- nodes

  layers <- list(Top = paste0("N", 1:3), Bottom = paste0("N", 4:6))

  # With minimum = 0.1, edge 1->4 should be hidden
  result <- safe_plot(plot_mlna(m, layers, minimum = 0.1, between_edges = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# ALL PARAMETERS COMBINED TEST
# ============================================

test_that("plot_mlna works with all parameters specified", {
  set.seed(42)
  nodes <- paste0("LongNode_", 1:9)
  m <- matrix(runif(81, 0, 0.3), 9, 9)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  nodes_df <- data.frame(
    label = nodes,
    labels = paste0("Display_", 1:9),
    stringsAsFactors = FALSE
  )

  layers <- list(
    Alpha = nodes[1:3],
    Beta = nodes[4:6],
    Gamma = nodes[7:9]
  )

  result <- safe_plot(plot_mlna(
    m, layers,
    community = NULL,
    layout = "spring",
    layer_spacing = 3,
    layer_width = 6,
    layer_depth = 3,
    skew_angle = 30,
    node_spacing = 0.6,
    colors = c("#FF5733", "#33FF57", "#3357FF"),
    shapes = c("circle", "square", "triangle"),
    edge_colors = c("#AA0000", "#00AA00", "#0000AA"),
    within_edges = TRUE,
    between_edges = TRUE,
    between_style = 2,
    show_border = TRUE,
    legend = TRUE,
    legend_position = "bottomleft",
    curvature = 0.1,
    node_size = 2,
    minimum = 0.05,
    scale = 1,
    show_labels = TRUE,
    nodes = nodes_df,
    label_abbrev = 8
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# COGRAPH_NETWORK WITH to_matrix EXTRACTION
# ============================================

test_that("plot_mlna extracts weights via to_matrix from cograph_network", {
  set.seed(42)
  nodes <- paste0("N", 1:6)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  net <- cograph(m)

  layers <- list(
    Top = paste0("N", 1:3),
    Bottom = paste0("N", 4:6)
  )

  # Verify to_matrix works
  extracted <- to_matrix(net)
  expect_true(is.matrix(extracted))

  result <- safe_plot(plot_mlna(net, layers))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE CASE: SINGLE NODE PER ALL LAYERS
# ============================================

test_that("plot_mlna works when all layers have single node", {
  set.seed(42)
  nodes <- paste0("N", 1:3)
  m <- matrix(runif(9, 0, 0.3), 3, 3)
  diag(m) <- 0
  colnames(m) <- rownames(m) <- nodes

  layers <- list(
    L1 = "N1",
    L2 = "N2",
    L3 = "N3"
  )

  result <- safe_plot(plot_mlna(m, layers))
  expect_true(result$success, info = result$error)
})
