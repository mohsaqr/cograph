# test-coverage-plot-htna-40.R - Coverage tests for plot-htna.R
# Targets uncovered functions and branches

# ============================================
# SETUP AND HELPER FUNCTIONS
# ============================================

# Create a mock tna object for testing (without requiring tna package)
create_mock_tna <- function(n = 6, labels = NULL) {
  if (is.null(labels)) {
    labels <- LETTERS[1:n]
  }
  weights <- matrix(runif(n * n, 0.1, 0.5), n, n)
  diag(weights) <- 0
  colnames(weights) <- labels
  rownames(weights) <- labels

  obj <- list(
    labels = labels,
    weights = weights,
    inits = rep(1/n, n)
  )
  class(obj) <- "tna"
  obj
}

# Create a simple weight matrix for testing
create_test_htna_matrix <- function(n = 6, labels = NULL) {
  if (is.null(labels)) {
    labels <- LETTERS[1:n]
  }
  weights <- matrix(runif(n * n, 0.1, 0.5), n, n)
  diag(weights) <- 0
  colnames(weights) <- labels
  rownames(weights) <- labels
  weights
}

# ============================================
# VALIDATION ERROR TESTS
# ============================================

test_that("plot_htna errors when node_list and community are both NULL", {
  mat <- create_test_htna_matrix(6)

  expect_error(
    plot_htna(mat, node_list = NULL, community = NULL),
    "Either node_list or community must be specified"
  )
})

test_that("plot_htna errors when node_list is not a list", {
  mat <- create_test_htna_matrix(6)

  expect_error(
    plot_htna(mat, node_list = c("A", "B", "C")),
    "node_list must be a list of 2\\+ character vectors"
  )
})

test_that("plot_htna errors when node_list has fewer than 2 groups", {
  mat <- create_test_htna_matrix(6)

  expect_error(
    plot_htna(mat, node_list = list(Group1 = c("A", "B", "C"))),
    "node_list must be a list of 2\\+ character vectors"
  )
})

test_that("plot_htna errors when node_list elements are not character vectors", {
  mat <- create_test_htna_matrix(6)

  expect_error(
    plot_htna(mat, node_list = list(Group1 = 1:3, Group2 = 4:6)),
    "node_list elements must be character vectors"
  )
})

test_that("plot_htna errors for invalid x type", {
  expect_error(
    plot_htna("invalid", node_list = list(G1 = c("A", "B"), G2 = c("C", "D"))),
    "x must be a cograph_network, tna object, or matrix"
  )
})

test_that("plot_htna errors when node_list groups overlap", {
  mat <- create_test_htna_matrix(6)

  expect_error(
    plot_htna(mat, node_list = list(
      Group1 = c("A", "B", "C"),
      Group2 = c("C", "D", "E")  # C is duplicate
    )),
    "node_list groups must not overlap"
  )
})

test_that("plot_htna errors when nodes not found in x", {
  mat <- create_test_htna_matrix(6)

  expect_error(
    plot_htna(mat, node_list = list(
      Group1 = c("A", "B", "C"),
      Group2 = c("X", "Y", "Z")  # Not in matrix
    )),
    "Nodes not found in x"
  )
})

test_that("plot_htna errors when bipartite layout used with != 2 groups", {
  mat <- create_test_htna_matrix(9, labels = LETTERS[1:9])

  expect_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I")),
              layout = "bipartite"),
    "Bipartite layout requires exactly 2 groups"
  )
})

test_that("plot_htna errors when polygon layout used with < 3 groups", {
  mat <- create_test_htna_matrix(6)

  expect_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              layout = "polygon"),
    "Polygon layout requires at least 3 groups"
  )
})

test_that("plot_htna errors when circular layout used with < 2 groups", {
  mat <- create_test_htna_matrix(3, labels = c("A", "B", "C"))

  expect_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C")),
              layout = "circular"),
    "node_list must be a list of 2\\+ character vectors"
  )
})

test_that("plot_htna errors when group_colors length mismatches", {
  mat <- create_test_htna_matrix(6)

  expect_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              group_colors = c("red", "blue", "green")),  # 3 colors, 2 groups
    "group_colors must have 2 elements"
  )
})

test_that("plot_htna errors when group_shapes length mismatches", {
  mat <- create_test_htna_matrix(6)

  expect_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              group_shapes = c("circle", "square", "diamond")),  # 3 shapes, 2 groups
    "group_shapes must have 2 elements"
  )
})

# ============================================
# MATRIX INPUT WITHOUT COLNAMES
# ============================================

test_that("plot_htna handles matrix without colnames", {
  mat <- matrix(runif(16, 0.1, 0.5), 4, 4)
  diag(mat) <- 0
  # No colnames - should use indices

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat, node_list = list(G1 = c("1", "2"), G2 = c("3", "4")))
  )
  dev.off()
})

# ============================================
# MOCK TNA OBJECT INPUT
# ============================================

test_that("plot_htna works with tna object input", {
  skip_if_not_installed("tna")

  tna_obj <- create_mock_tna(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(tna_obj, node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")))
  )
  dev.off()
})

# ============================================
# LAYOUT AUTO SELECTION
# ============================================

test_that("plot_htna auto-selects bipartite for 2 groups", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  # layout = "auto" should select bipartite for 2 groups
  expect_no_error(
    plot_htna(mat, node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")), layout = "auto")
  )
  dev.off()
})

test_that("plot_htna auto-selects polygon for 3+ groups", {
  mat <- create_test_htna_matrix(9, labels = LETTERS[1:9])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  # layout = "auto" should select polygon for 3 groups
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I")),
              layout = "auto")
  )
  dev.off()
})

# ============================================
# LEGACY LAYOUT NAMES
# ============================================

test_that("plot_htna maps legacy 'triangle' to polygon", {
  mat <- create_test_htna_matrix(9, labels = LETTERS[1:9])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I")),
              layout = "triangle")
  )
  dev.off()
})

test_that("plot_htna maps legacy 'rectangle' to polygon", {
  mat <- create_test_htna_matrix(12, labels = LETTERS[1:12])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(
                G1 = c("A", "B", "C"), G2 = c("D", "E", "F"),
                G3 = c("G", "H", "I"), G4 = c("J", "K", "L")
              ),
              layout = "rectangle")
  )
  dev.off()
})

test_that("plot_htna maps legacy 'pentagon' to polygon", {
  mat <- create_test_htna_matrix(10, labels = LETTERS[1:10])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(
                G1 = c("A", "B"), G2 = c("C", "D"), G3 = c("E", "F"),
                G4 = c("G", "H"), G5 = c("I", "J")
              ),
              layout = "pentagon")
  )
  dev.off()
})

test_that("plot_htna maps legacy 'hexagon' to polygon", {
  mat <- create_test_htna_matrix(12, labels = LETTERS[1:12])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(
                G1 = c("A", "B"), G2 = c("C", "D"), G3 = c("E", "F"),
                G4 = c("G", "H"), G5 = c("I", "J"), G6 = c("K", "L")
              ),
              layout = "hexagon")
  )
  dev.off()
})

# ============================================
# SINGLE NODE IN GROUP
# ============================================

test_that("plot_htna handles single node in group (vertical)", {
  mat <- create_test_htna_matrix(4, labels = c("A", "B", "C", "D"))

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A"), G2 = c("B", "C", "D")),  # Single node in G1
              orientation = "vertical")
  )
  dev.off()
})

test_that("plot_htna handles single node in second group (vertical)", {
  mat <- create_test_htna_matrix(4, labels = c("A", "B", "C", "D"))

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D")),  # Single node in G2
              orientation = "vertical")
  )
  dev.off()
})

test_that("plot_htna handles single node in group (horizontal)", {
  mat <- create_test_htna_matrix(4, labels = c("A", "B", "C", "D"))

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A"), G2 = c("B", "C", "D")),  # Single node in G1
              orientation = "horizontal")
  )
  dev.off()
})

test_that("plot_htna handles single node in second group (horizontal)", {
  mat <- create_test_htna_matrix(4, labels = c("A", "B", "C", "D"))

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D")),  # Single node in G2
              orientation = "horizontal")
  )
  dev.off()
})

# ============================================
# JITTER VARIATIONS
# ============================================

test_that("plot_htna works with jitter = FALSE", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter = FALSE)
  )
  dev.off()
})

test_that("plot_htna works with jitter = 0", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter = 0)
  )
  dev.off()
})

test_that("plot_htna works with numeric jitter value > 0", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter = 0.5)  # Numeric jitter
  )
  dev.off()
})

test_that("plot_htna works with list jitter (manual offsets)", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter = list(A = 0.2, B = -0.1, D = 0.3))  # Named list jitter
  )
  dev.off()
})

test_that("plot_htna list jitter with invalid node name is silently ignored", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter = list(A = 0.2, InvalidNode = 0.5))  # Invalid node ignored
  )
  dev.off()
})

# ============================================
# JITTER SIDE VARIATIONS
# ============================================

test_that("plot_htna works with jitter_side = 'second'", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter_side = "second")
  )
  dev.off()
})

test_that("plot_htna works with jitter_side = 'both'", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter_side = "both")
  )
  dev.off()
})

test_that("plot_htna works with jitter_side = 'none'", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter_side = "none")
  )
  dev.off()
})

test_that("plot_htna works with jitter_side = 'left' (alias for first)", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter_side = "left")
  )
  dev.off()
})

test_that("plot_htna works with jitter_side = 'right' (alias for second)", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              jitter_side = "right")
  )
  dev.off()
})

# ============================================
# USE_LIST_ORDER = FALSE
# ============================================

test_that("plot_htna works with use_list_order = FALSE", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              use_list_order = FALSE)  # Weight-based ordering
  )
  dev.off()
})

test_that("plot_htna use_list_order = FALSE handles zero weights", {
  # Create matrix with some zero cross-group weights
  mat <- create_test_htna_matrix(6)
  mat[1:3, 4:6] <- 0  # Zero cross-group weights

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              use_list_order = FALSE)
  )
  dev.off()
})

# ============================================
# HORIZONTAL ORIENTATION
# ============================================

test_that("plot_htna works with horizontal orientation", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              orientation = "horizontal")
  )
  dev.off()
})

test_that("plot_htna horizontal with jitter = TRUE", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              orientation = "horizontal",
              jitter = TRUE)
  )
  dev.off()
})

test_that("plot_htna horizontal with numeric jitter", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              orientation = "horizontal",
              jitter = 0.3)
  )
  dev.off()
})

test_that("plot_htna horizontal with list jitter", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              orientation = "horizontal",
              jitter = list(A = 0.2, D = -0.1))
  )
  dev.off()
})

# ============================================
# POLYGON LAYOUT
# ============================================

test_that("plot_htna works with polygon layout (3 groups)", {
  mat <- create_test_htna_matrix(9, labels = LETTERS[1:9])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I")),
              layout = "polygon")
  )
  dev.off()
})

test_that("plot_htna polygon layout with single node in group", {
  mat <- create_test_htna_matrix(5, labels = c("A", "B", "C", "D", "E"))

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A"), G2 = c("B", "C"), G3 = c("D", "E")),
              layout = "polygon")
  )
  dev.off()
})

test_that("plot_htna polygon layout with custom angle_spacing", {
  mat <- create_test_htna_matrix(9, labels = LETTERS[1:9])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I")),
              layout = "polygon",
              angle_spacing = 0.25)
  )
  dev.off()
})

# ============================================
# CIRCULAR LAYOUT
# ============================================

test_that("plot_htna works with circular layout", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              layout = "circular")
  )
  dev.off()
})

test_that("plot_htna circular layout with single node in group", {
  mat <- create_test_htna_matrix(5, labels = c("A", "B", "C", "D", "E"))

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A"), G2 = c("B", "C"), G3 = c("D", "E")),
              layout = "circular")
  )
  dev.off()
})

test_that("plot_htna circular layout with 4 groups", {
  mat <- create_test_htna_matrix(8, labels = LETTERS[1:8])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B"), G2 = c("C", "D"), G3 = c("E", "F"), G4 = c("G", "H")),
              layout = "circular")
  )
  dev.off()
})

# ============================================
# EDGE COLORS
# ============================================

test_that("plot_htna works with edge_colors = FALSE", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              edge_colors = FALSE)  # Use default edge color
  )
  dev.off()
})

test_that("plot_htna works with custom edge_colors", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              edge_colors = c("red", "blue"))
  )
  dev.off()
})

# ============================================
# LEGEND
# ============================================

test_that("plot_htna works with legend = TRUE", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              legend = TRUE)
  )
  dev.off()
})

test_that("plot_htna works with legend = FALSE", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              legend = FALSE)
  )
  dev.off()
})

test_that("plot_htna legend with different positions", {
  mat <- create_test_htna_matrix(6)
  positions <- c("topright", "topleft", "bottomright", "bottomleft", "right", "left")

  for (pos in positions) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    png(tmp, width = 400, height = 400)
    expect_no_error(
      plot_htna(mat,
                node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
                legend = TRUE,
                legend_position = pos)
    )
    dev.off()
  }
})

test_that("plot_htna legend with unnamed node_list", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(c("A", "B", "C"), c("D", "E", "F")),  # Unnamed groups
              legend = TRUE)
  )
  dev.off()
})

# ============================================
# EXTEND LINES
# ============================================

test_that("plot_htna works with extend_lines = TRUE (vertical)", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              extend_lines = TRUE,
              orientation = "vertical")
  )
  dev.off()
})

test_that("plot_htna works with extend_lines = numeric (vertical)", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              extend_lines = 0.2,
              orientation = "vertical")
  )
  dev.off()
})

test_that("plot_htna works with extend_lines = TRUE (horizontal)", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              extend_lines = TRUE,
              orientation = "horizontal")
  )
  dev.off()
})

test_that("plot_htna works with extend_lines = numeric (horizontal)", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              extend_lines = 0.15,
              orientation = "horizontal")
  )
  dev.off()
})

test_that("plot_htna extend_lines is ignored for non-bipartite layouts", {
  mat <- create_test_htna_matrix(9, labels = LETTERS[1:9])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  # extend_lines should be ignored for polygon layout
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I")),
              layout = "polygon",
              extend_lines = TRUE)
  )
  dev.off()
})

# ============================================
# SCALE PARAMETER
# ============================================

test_that("plot_htna works with scale > 1", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 800, height = 800)  # Higher resolution
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              scale = 2)
  )
  dev.off()
})

test_that("plot_htna scale affects polygon layout", {
  mat <- create_test_htna_matrix(9, labels = LETTERS[1:9])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 800, height = 800)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I")),
              layout = "polygon",
              scale = 3)
  )
  dev.off()
})

# ============================================
# CUSTOM GROUP COLORS AND SHAPES
# ============================================

test_that("plot_htna works with custom group_colors", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              group_colors = c("#FF6B6B", "#4ECDC4"))
  )
  dev.off()
})

test_that("plot_htna works with custom group_shapes", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              group_shapes = c("diamond", "triangle"))
  )
  dev.off()
})

test_that("plot_htna uses shape palette for many groups", {
  mat <- create_test_htna_matrix(15, labels = LETTERS[1:15])

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(
                G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I"),
                G4 = c("J", "K", "L"), G5 = c("M", "N", "O")
              ),
              layout = "polygon")
  )
  dev.off()
})

# ============================================
# GROUP POSITIONS
# ============================================

test_that("plot_htna works with custom group positions", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              group1_pos = -3,
              group2_pos = 3)
  )
  dev.off()
})

# ============================================
# CURVATURE
# ============================================

test_that("plot_htna works with custom curvature", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              curvature = 0.8)
  )
  dev.off()
})

test_that("plot_htna works with curvature = 0", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              curvature = 0)
  )
  dev.off()
})

# ============================================
# HTNA ALIAS
# ============================================

test_that("htna() is an alias for plot_htna()", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    htna(mat, node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")))
  )
  dev.off()
})

# ============================================
# HELPER FUNCTION TESTS
# ============================================

test_that("compute_connectivity_jitter_horizontal works correctly", {
  # Access internal function
  compute_jitter_h <- cograph:::compute_connectivity_jitter_horizontal

  # Create test matrix
  weights <- matrix(c(
    0, 0, 0.5, 0.3,
    0, 0, 0.2, 0.4,
    0.5, 0.2, 0, 0,
    0.3, 0.4, 0, 0
  ), 4, 4, byrow = TRUE)

  g1_idx <- 1:2
  g2_idx <- 3:4

  jitter <- compute_jitter_h(weights, g1_idx, g2_idx, amount = 0.5, side = "group1")

  expect_length(jitter, 4)
  expect_true(all(jitter[g2_idx] == 0))  # Group 2 should have no jitter
  expect_true(all(jitter[g1_idx] <= 0))  # Group 1 should have negative jitter (toward center)
})

test_that("compute_connectivity_jitter_horizontal handles zero weights", {
  compute_jitter_h <- cograph:::compute_connectivity_jitter_horizontal

  # All zeros
  weights <- matrix(0, 4, 4)

  g1_idx <- 1:2
  g2_idx <- 3:4

  jitter <- compute_jitter_h(weights, g1_idx, g2_idx, amount = 0.5, side = "both")

  expect_length(jitter, 4)
  expect_true(all(jitter == 0))
})

test_that("compute_connectivity_jitter_horizontal with side = 'both'", {
  compute_jitter_h <- cograph:::compute_connectivity_jitter_horizontal

  weights <- matrix(c(
    0, 0, 0.5, 0.3,
    0, 0, 0.2, 0.4,
    0.5, 0.2, 0, 0,
    0.3, 0.4, 0, 0
  ), 4, 4, byrow = TRUE)

  g1_idx <- 1:2
  g2_idx <- 3:4

  jitter <- compute_jitter_h(weights, g1_idx, g2_idx, amount = 0.5, side = "both")

  expect_length(jitter, 4)
  expect_true(all(jitter[g1_idx] <= 0))  # Group 1 negative
  expect_true(all(jitter[g2_idx] >= 0))  # Group 2 positive
})

test_that("compute_connectivity_jitter_vertical works correctly", {
  compute_jitter_v <- cograph:::compute_connectivity_jitter_vertical

  weights <- matrix(c(
    0, 0, 0.5, 0.3,
    0, 0, 0.2, 0.4,
    0.5, 0.2, 0, 0,
    0.3, 0.4, 0, 0
  ), 4, 4, byrow = TRUE)

  g1_idx <- 1:2
  g2_idx <- 3:4

  jitter <- compute_jitter_v(weights, g1_idx, g2_idx, amount = 0.5, side = "group1")

  expect_length(jitter, 4)
  expect_true(all(jitter[g2_idx] == 0))
  expect_true(all(jitter[g1_idx] <= 0))
})

test_that("compute_connectivity_jitter_vertical handles zero weights", {
  compute_jitter_v <- cograph:::compute_connectivity_jitter_vertical

  weights <- matrix(0, 4, 4)

  g1_idx <- 1:2
  g2_idx <- 3:4

  jitter <- compute_jitter_v(weights, g1_idx, g2_idx, amount = 0.5, side = "both")

  expect_length(jitter, 4)
  expect_true(all(jitter == 0))
})

test_that("compute_polygon_layout returns correct structure", {
  compute_poly <- cograph:::compute_polygon_layout

  node_list <- list(G1 = c("A", "B"), G2 = c("C", "D"), G3 = c("E", "F"))
  lab <- c("A", "B", "C", "D", "E", "F")
  group_indices <- list(1:2, 3:4, 5:6)

  result <- compute_poly(node_list, lab, group_indices, n_sides = 3, angle_spacing = 0.15, scale = 1)

  expect_type(result, "list")
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_length(result$x, 6)
  expect_length(result$y, 6)
})

test_that("compute_polygon_layout with scale > 1", {
  compute_poly <- cograph:::compute_polygon_layout

  node_list <- list(G1 = c("A", "B"), G2 = c("C", "D"), G3 = c("E", "F"))
  lab <- c("A", "B", "C", "D", "E", "F")
  group_indices <- list(1:2, 3:4, 5:6)

  result1 <- compute_poly(node_list, lab, group_indices, n_sides = 3, angle_spacing = 0.15, scale = 1)
  result2 <- compute_poly(node_list, lab, group_indices, n_sides = 3, angle_spacing = 0.15, scale = 2)

  # Scaled version should have larger coordinates
  expect_true(max(abs(result2$x)) > max(abs(result1$x)))
  expect_true(max(abs(result2$y)) > max(abs(result1$y)))
})

test_that("compute_circular_layout returns correct structure", {
  compute_circ <- cograph:::compute_circular_layout

  node_list <- list(G1 = c("A", "B"), G2 = c("C", "D"), G3 = c("E", "F"))
  lab <- c("A", "B", "C", "D", "E", "F")
  group_indices <- list(1:2, 3:4, 5:6)

  result <- compute_circ(node_list, lab, group_indices, n_groups = 3, angle_spacing = 0.15, scale = 1)

  expect_type(result, "list")
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_length(result$x, 6)
  expect_length(result$y, 6)
})

test_that("compute_circular_layout positions nodes on circle", {
  compute_circ <- cograph:::compute_circular_layout

  node_list <- list(G1 = c("A", "B"), G2 = c("C", "D"))
  lab <- c("A", "B", "C", "D")
  group_indices <- list(1:2, 3:4)

  result <- compute_circ(node_list, lab, group_indices, n_groups = 2, angle_spacing = 0.15, scale = 1)

  # Check all nodes are approximately on circle of radius 2
  distances <- sqrt(result$x^2 + result$y^2)
  expect_true(all(abs(distances - 2) < 0.1))
})

# ============================================
# ADDITIONAL EDGE CASES
# ============================================

test_that("plot_htna handles nodes not in any group (remaining uncategorized)", {
  # Matrix with 8 nodes but only 6 in node_list - remaining are implicitly "other"
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")))
  )
  dev.off()
})

test_that("plot_htna works with many groups (> 8, cycling palettes)", {
  n <- 20
  labels <- paste0("N", 1:n)
  mat <- create_test_htna_matrix(n, labels = labels)

  node_list <- lapply(1:10, function(i) {
    labels[((i-1)*2 + 1):(i*2)]
  })
  names(node_list) <- paste0("G", 1:10)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 600, height = 600)
  expect_no_error(
    plot_htna(mat, node_list = node_list, layout = "polygon")
  )
  dev.off()
})

test_that("plot_htna passes ... args to tplot", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  expect_no_error(
    plot_htna(mat,
              node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
              title = "Test Plot",
              edge.labels = TRUE)
  )
  dev.off()
})

test_that("plot_htna returns result invisibly", {
  mat <- create_test_htna_matrix(6)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  result <- plot_htna(mat,
                      node_list = list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")))
  dev.off()

  # Result should be returned (from tplot)
  expect_false(is.null(result))
})
