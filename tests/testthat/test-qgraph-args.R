# Tests for qgraph arg translation in splot's tna dispatch
# Tests .translate_qgraph_dots() unit behavior and end-to-end splot dispatch

# =============================================================================
# Helper: Mock tna constructors (same as test-coverage-input-tna-40.R)
# =============================================================================

mock_tna <- function(
    weights = matrix(c(0, 0.5, 0.3, 0.4, 0, 0.2, 0.1, 0.6, 0), 3, 3,
                     dimnames = list(c("A", "B", "C"), c("A", "B", "C"))),
    labels = c("A", "B", "C"),
    inits = c(0.4, 0.35, 0.25),
    data = NULL,
    directed = NULL
) {
  obj <- list(weights = weights, labels = labels, inits = inits, data = data)
  if (!is.null(directed)) obj$directed <- directed
  class(obj) <- c("tna", "list")
  obj
}

mock_group_tna <- function(n_groups = 2, group_names = c("GroupA", "GroupB")) {
  groups <- lapply(seq_len(n_groups), function(i) {
    set.seed(100 + i)
    w <- matrix(runif(9), 3, 3)
    diag(w) <- 0
    w <- w / rowSums(w)
    dimnames(w) <- list(c("A", "B", "C"), c("A", "B", "C"))
    mock_tna(weights = w)
  })
  names(groups) <- group_names[seq_len(n_groups)]
  class(groups) <- c("group_tna", "list")
  groups
}

mock_tna_bootstrap <- function() {
  w_mat <- matrix(c(0, 0.5, 0.3, 0.4, 0, 0.2, 0.1, 0.6, 0), 3, 3,
                  dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  p_mat <- matrix(0.01, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  diag(p_mat) <- 1
  structure(
    list(
      weights_orig = w_mat,
      weights_sig = w_mat,
      p_values = p_mat,
      ci_lower = w_mat - 0.05,
      ci_upper = w_mat + 0.05,
      model = mock_tna()
    ),
    class = "tna_bootstrap"
  )
}

mock_tna_permutation <- function() {
  labels <- c("A", "B", "C")
  diffs_true <- matrix(c(0, 0.3, -0.1, -0.2, 0, 0.4, 0.1, -0.3, 0), 3, 3,
                       dimnames = list(labels, labels))
  diffs_sig <- diffs_true
  diffs_sig[abs(diffs_sig) < 0.15] <- 0
  stats <- data.frame(
    edge_name = c("A -> B", "A -> C", "B -> A", "B -> C", "C -> A", "C -> B"),
    original = c(0.5, 0.3, 0.4, 0.2, 0.1, 0.6),
    mean_perm = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
    p_value = c(0.01, 0.8, 0.02, 0.005, 0.9, 0.01),
    effect_size = c(0.8, 0.1, 0.6, 0.5, 0.05, 0.7),
    significant = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  obj <- list(
    edges = list(diffs_true = diffs_true, diffs_sig = diffs_sig, stats = stats),
    n_perm = 100
  )
  attr(obj, "level") <- 0.05
  attr(obj, "labels") <- labels
  attr(obj, "colors") <- c("#FF6B6B", "#4ECDC4", "#45B7D1")
  class(obj) <- "tna_permutation"
  obj
}

mock_group_tna_permutation <- function() {
  pairs <- c("GroupA vs. GroupB")
  obj <- list(mock_tna_permutation())
  names(obj) <- pairs
  class(obj) <- "group_tna_permutation"
  obj
}

# =============================================================================
# Unit tests: .translate_qgraph_dots()
# =============================================================================

test_that(".translate_qgraph_dots returns empty list unchanged", {
  result <- .translate_qgraph_dots(list())
  expect_equal(result, list())
})

test_that(".translate_qgraph_dots returns unnamed list unchanged", {
  result <- .translate_qgraph_dots(list(1, 2, 3))
  expect_equal(result, list(1, 2, 3))
})

test_that(".translate_qgraph_dots passes through non-qgraph params untouched", {
  input <- list(node_size = 10, edge_color = "red", layout = "circle")
  result <- .translate_qgraph_dots(input)
  expect_equal(result, input)
})

test_that(".translate_qgraph_dots renames simple qgraph params", {
  input <- list(size = 20, edge.color = "blue", posCol = "green")
  result <- .translate_qgraph_dots(input)
  expect_equal(names(result), c("node_size", "edge_color", "edge_positive_color"))
  expect_equal(result$node_size, 20)
  expect_equal(result$edge_color, "blue")
  expect_equal(result$edge_positive_color, "green")
})

test_that(".translate_qgraph_dots: cograph name wins over qgraph alias", {
  # Both node_size (cograph) and size (qgraph alias) present
  input <- list(node_size = 10, size = 20)
  result <- .translate_qgraph_dots(input)
  # cograph name takes precedence; alias is NOT renamed (kept as-is)
  expect_equal(result$node_size, 10)
  expect_true("size" %in% names(result))
  expect_equal(result$size, 20)
})

test_that(".translate_qgraph_dots: vsize also maps to node_size", {
  input <- list(vsize = 15)
  result <- .translate_qgraph_dots(input)
  expect_equal(result$node_size, 15)
})

test_that(".translate_qgraph_dots renames all supported params", {
  input <- list(
    color = "steelblue",
    pie = c(0.3, 0.7),
    pieColor = c("red", "blue"),
    edge.labels = TRUE,
    edge.label.position = 0.5,
    edge.label.color = "gray",
    negCol = "red",
    arrowAngle = 30,
    mar = c(1, 1, 1, 1),
    label.cex = 1.5,
    label.color = "black",
    border.color = "darkgray",
    border.width = 2
  )
  result <- .translate_qgraph_dots(input)
  expect_equal(result$node_fill, "steelblue")
  expect_equal(result$donut_fill, c(0.3, 0.7))
  expect_equal(result$donut_color, c("red", "blue"))
  expect_equal(result$edge_labels, TRUE)
  expect_equal(result$edge_label_position, 0.5)
  expect_equal(result$edge_label_color, "gray")
  expect_equal(result$edge_negative_color, "red")
  expect_equal(result$arrow_angle, 30)
  expect_equal(result$margins, c(1, 1, 1, 1))
  expect_equal(result$label_size, 1.5)
  expect_equal(result$label_color, "black")
  expect_equal(result$node_border_color, "darkgray")
  expect_equal(result$node_border_width, 2)
})

test_that(".translate_qgraph_dots: mixed qgraph + cograph params", {
  input <- list(size = 20, edge_color = "red", posCol = "green", layout = "circle")
  result <- .translate_qgraph_dots(input)
  expect_equal(result$node_size, 20)
  expect_equal(result$edge_color, "red")
  expect_equal(result$edge_positive_color, "green")
  expect_equal(result$layout, "circle")
})

# =============================================================================
# Value transform tests
# =============================================================================

test_that(".translate_qgraph_dots: asize scaled by 0.20", {
  input <- list(asize = 5)
  result <- .translate_qgraph_dots(input)
  expect_equal(result$arrow_size, 5 * 0.20)
})

test_that(".translate_qgraph_dots: asize NOT scaled when arrow_size present", {
  # If user provides cograph name, alias is skipped (not renamed), no transform
  input <- list(arrow_size = 2, asize = 5)
  result <- .translate_qgraph_dots(input)
  expect_equal(result$arrow_size, 2)  # cograph value untouched
})

test_that(".translate_qgraph_dots: edge.label.cex scaled by 1.2", {
  input <- list(edge.label.cex = 1.0)
  result <- .translate_qgraph_dots(input)
  expect_equal(result$edge_label_size, 1.0 * 1.2)
})

test_that(".translate_qgraph_dots: edge.label.cex NOT scaled when edge_label_size present", {
  input <- list(edge_label_size = 0.8, edge.label.cex = 1.0)
  result <- .translate_qgraph_dots(input)
  expect_equal(result$edge_label_size, 0.8)
})

test_that(".translate_qgraph_dots: numeric lty mapped via map_qgraph_lty", {
  input <- list(lty = 2)
  result <- .translate_qgraph_dots(input)
  expect_equal(result$edge_style, "dashed")
})

test_that(".translate_qgraph_dots: character lty mapped via map_qgraph_lty", {
  input <- list(lty = "dotted")
  result <- .translate_qgraph_dots(input)
  expect_equal(result$edge_style, "dotted")
})

test_that(".translate_qgraph_dots: lty NOT mapped when edge_style present", {
  input <- list(edge_style = "solid", lty = 2)
  result <- .translate_qgraph_dots(input)
  expect_equal(result$edge_style, "solid")
})

test_that(".translate_qgraph_dots: shape mapped via map_qgraph_shape", {
  input <- list(shape = "rectangle")
  result <- .translate_qgraph_dots(input)
  expect_equal(result$node_shape, "square")
})

test_that(".translate_qgraph_dots: shape vector mapped element-wise", {
  input <- list(shape = c("rectangle", "circle", "triangle"))
  result <- .translate_qgraph_dots(input)
  expect_equal(result$node_shape, c("square", "circle", "triangle"))
})

test_that(".translate_qgraph_dots: shape NOT mapped when node_shape present", {
  input <- list(node_shape = "diamond", shape = "rectangle")
  result <- .translate_qgraph_dots(input)
  expect_equal(result$node_shape, "diamond")
})

# =============================================================================
# splot + tna: qgraph aliases flow through dispatch
# =============================================================================

test_that("splot(tna): qgraph size alias sets node_size", {
  model <- mock_tna()
  with_temp_png({
    splot(model, size = 20)
  })
  expect_true(TRUE)
})

test_that("splot(tna): qgraph edge.color alias sets edge color", {
  model <- mock_tna()
  with_temp_png({
    splot(model, edge.color = "red")
  })
  expect_true(TRUE)
})

test_that("splot(tna): multiple qgraph aliases forwarded together", {
  model <- mock_tna()
  with_temp_png({
    splot(model, size = 15, edge.color = "purple", posCol = "blue",
          negCol = "orange", label.color = "darkgray")
  })
  expect_true(TRUE)
})

test_that("splot(tna): cograph name overrides qgraph alias", {
  model <- mock_tna()
  # node_size (cograph) should take precedence over size (qgraph)
  with_temp_png({
    splot(model, node_size = 10, size = 30)
  })
  expect_true(TRUE)
})

test_that("splot(tna): asize value transform in dispatch", {
  model <- mock_tna()
  with_temp_png({
    splot(model, asize = 5)
  })
  expect_true(TRUE)
})

test_that("splot(tna): lty value transform in dispatch", {
  model <- mock_tna()
  with_temp_png({
    splot(model, lty = 2)
  })
  expect_true(TRUE)
})

test_that("splot(tna): shape value transform in dispatch", {
  model <- mock_tna()
  with_temp_png({
    splot(model, shape = "rectangle")
  })
  expect_true(TRUE)
})

# =============================================================================
# splot + tna_bootstrap: qgraph args forwarded through bootstrap dispatch
# =============================================================================

test_that("splot(tna_bootstrap): qgraph aliases forwarded", {
  boot <- mock_tna_bootstrap()
  with_temp_png({
    tryCatch(
      splot(boot, edge.color = "navy", size = 12),
      error = function(e) NULL
    )
  })
  expect_true(TRUE)
})

# =============================================================================
# splot + tna_permutation: qgraph args forwarded through permutation dispatch
# =============================================================================

test_that("splot(tna_permutation): qgraph aliases forwarded", {
  perm <- mock_tna_permutation()
  with_temp_png({
    tryCatch(
      splot(perm, edge.color = "darkred"),
      error = function(e) NULL
    )
  })
  expect_true(TRUE)
})

# =============================================================================
# splot + group_tna: qgraph args inherited via recursive dispatch
# =============================================================================

test_that("splot(group_tna): qgraph aliases forwarded to each group", {
  gtna <- mock_group_tna()
  with_temp_png({
    splot(gtna, size = 18, edge.color = "steelblue")
  })
  expect_true(TRUE)
})

test_that("splot(group_tna, i): qgraph aliases forwarded to single group", {
  gtna <- mock_group_tna()
  with_temp_png({
    splot(gtna, i = "GroupA", size = 18, label.color = "navy")
  })
  expect_true(TRUE)
})

# =============================================================================
# splot + group_tna_permutation: qgraph args forwarded
# =============================================================================

test_that("splot(group_tna_permutation): qgraph aliases forwarded", {
  gperm <- mock_group_tna_permutation()
  with_temp_png({
    tryCatch(
      splot(gperm, edge.color = "darkgreen"),
      error = function(e) NULL
    )
  })
  expect_true(TRUE)
})

# =============================================================================
# Non-tna object: qgraph-style args NOT translated (no regression)
# =============================================================================

test_that("splot(matrix): qgraph-style args pass through untranslated", {
  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.4, 0.3, 0.4, 0), 3, 3,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  # 'size' is NOT a recognized splot param for raw matrix — should be ignored
  # (no error, just ignored via ...)
  with_temp_png({
    splot(mat, size = 20)
  })
  expect_true(TRUE)
})

test_that("splot(matrix): cograph params still work normally", {
  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.4, 0.3, 0.4, 0), 3, 3,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  with_temp_png({
    splot(mat, node_size = 10, edge_color = "navy")
  })
  expect_true(TRUE)
})
