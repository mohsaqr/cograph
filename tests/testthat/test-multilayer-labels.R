# Tests for multi-layer plot label and abbreviation functionality

test_that("abbrev_label returns labels unchanged with NULL abbrev", {
  labels <- c("VeryLongStateName", "Short", "AnotherLongName")
  result <- abbrev_label(labels, NULL)
  expect_equal(result, labels)
})

test_that("abbrev_label truncates with integer abbrev", {
  labels <- c("VeryLongStateName", "Short", "AnotherLongName")
  result <- abbrev_label(labels, 5)

  expect_equal(nchar(result[1]), 5)
  expect_equal(result[2], "Short")  # Already short enough
  expect_equal(nchar(result[3]), 5)
  expect_true(grepl("\u2026$", result[1]))  # Ends with ellipsis
})

test_that("abbrev_label auto mode scales with label count", {
  # Few labels - no abbreviation
  labels_few <- LETTERS[1:4]
  result_few <- abbrev_label(labels_few, "auto")
  expect_equal(result_few, labels_few)

  # Many labels - abbreviation
  labels_many <- paste0("LongLabel", 1:25)
  result_many <- abbrev_label(labels_many, "auto")
  expect_true(all(nchar(result_many) <= 4))
})

test_that(".shape_to_pch returns correct pch values", {
  expect_equal(.shape_to_pch("circle"), 21L)
  expect_equal(.shape_to_pch("square"), 22L)
  expect_equal(.shape_to_pch("diamond"), 23L)
  expect_equal(.shape_to_pch("triangle"), 24L)
  expect_equal(.shape_to_pch("triangle_down"), 25L)
  expect_equal(.shape_to_pch("CIRCLE"), 21L)  # Case insensitive
  expect_equal(.shape_to_pch("unknown"), 21L)  # Default to circle
})

test_that(".shape_to_pch handles vector input", {
  shapes <- c("circle", "square", "diamond")
  result <- .shape_to_pch(shapes)
  expect_equal(result, c(21L, 22L, 23L))
})

test_that("plot_mcml accepts new label parameters", {
  skip_if_not_installed("png")

  mat <- matrix(runif(100), 10, 10)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("N", 1:10)

  clusters <- list(
    A = paste0("N", 1:3),
    B = paste0("N", 4:6),
    C = paste0("N", 7:10)
  )

  png(tempfile(fileext = ".png"), width = 400, height = 400)
  expect_no_error(
    plot_mcml(mat, clusters,
              show_labels = TRUE,
              label_size = 0.8,
              label_abbrev = 3,
              node_size = 2,
              node_shape = "square",
              cluster_shape = "diamond")
  )
  dev.off()
})

test_that("plot_mcml show_labels = FALSE hides labels", {
  skip_if_not_installed("png")

  mat <- matrix(runif(25), 5, 5)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("Node", 1:5)

  clusters <- list(A = c("Node1", "Node2"), B = c("Node3", "Node4", "Node5"))

  png(tempfile(fileext = ".png"), width = 400, height = 400)
  expect_no_error(plot_mcml(mat, clusters, show_labels = FALSE))
  dev.off()
})

test_that("plot_mtna accepts new label parameters", {
  skip_if_not_installed("png")

  mat <- matrix(runif(100), 10, 10)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("State", 1:10)

  clusters <- list(
    GroupA = paste0("State", 1:3),
    GroupB = paste0("State", 4:6),
    GroupC = paste0("State", 7:10)
  )

  png(tempfile(fileext = ".png"), width = 400, height = 400)
  expect_no_error(
    plot_mtna(mat, clusters,
              show_labels = TRUE,
              label_abbrev = "auto",
              label_size = 0.6)
  )
  dev.off()
})

test_that("plot_mlna accepts label_abbrev parameter", {
  skip_if_not_installed("png")

  mat <- matrix(runif(100), 10, 10)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("VeryLongNodeName", 1:10)

  layers <- list(
    Layer1 = paste0("VeryLongNodeName", 1:3),
    Layer2 = paste0("VeryLongNodeName", 4:7),
    Layer3 = paste0("VeryLongNodeName", 8:10)
  )

  png(tempfile(fileext = ".png"), width = 400, height = 400)
  expect_no_error(plot_mlna(mat, layers, label_abbrev = 8))
  dev.off()
})

test_that("plot_mlna show_labels = FALSE hides labels", {
  skip_if_not_installed("png")

  mat <- matrix(runif(36), 6, 6)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("N", 1:6)

  layers <- list(Top = c("N1", "N2"), Mid = c("N3", "N4"), Bot = c("N5", "N6"))

  png(tempfile(fileext = ".png"), width = 400, height = 400)
  expect_no_error(plot_mlna(mat, layers, show_labels = FALSE))
  dev.off()
})

test_that("plot_htna accepts label_abbrev parameter", {
  skip_if_not_installed("png")

  mat <- matrix(runif(64), 8, 8)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("LongStateName", 1:8)

  groups <- list(
    TypeA = paste0("LongStateName", 1:4),
    TypeB = paste0("LongStateName", 5:8)
  )

  png(tempfile(fileext = ".png"), width = 400, height = 400)
  expect_no_error(plot_htna(mat, groups, label_abbrev = 5))
  dev.off()
})

test_that("summarize_network creates cograph_network from matrix", {
  mat <- matrix(runif(100), 10, 10)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:10]

  clusters <- list(
    G1 = c("A", "B", "C"),
    G2 = c("D", "E", "F"),
    G3 = c("G", "H", "I", "J")
  )

  result <- summarize_network(mat, clusters)

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
  expect_equal(sort(result$nodes$label), c("G1", "G2", "G3"))
  expect_equal(result$nodes$size, c(3, 3, 4))
})

test_that("summarize_network auto-detects cluster column", {
  mat <- matrix(runif(100), 10, 10)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("N", 1:10)

  net <- cograph(mat)
  net$nodes$clusters <- rep(c("A", "B", "C"), c(3, 4, 3))

  expect_message(
    result <- summarize_network(net),
    "Using 'clusters' column"
  )

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 3)
})

test_that("summarize_network with column name string", {
  mat <- matrix(runif(100), 10, 10)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("N", 1:10)

  net <- cograph(mat)
  net$nodes$my_groups <- rep(c("X", "Y"), each = 5)

  result <- summarize_network(net, "my_groups")

  expect_s3_class(result, "cograph_network")
  expect_equal(n_nodes(result), 2)
  expect_equal(sort(result$nodes$label), c("X", "Y"))
})

test_that("summarize_network different aggregation methods", {
  mat <- matrix(c(
    0, 1, 2, 3,
    1, 0, 4, 5,
    2, 4, 0, 6,
    3, 5, 6, 0
  ), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  clusters <- list(G1 = c("A", "B"), G2 = c("C", "D"))

  result_sum <- summarize_network(mat, clusters, method = "sum")
  result_mean <- summarize_network(mat, clusters, method = "mean")
  result_max <- summarize_network(mat, clusters, method = "max")

  # Between G1 and G2: edges are (A,C)=2, (A,D)=3, (B,C)=4, (B,D)=5
  # Sum = 2+3+4+5 = 14
  between_mat_sum <- to_matrix(result_sum)
  expect_equal(between_mat_sum["G1", "G2"], 14)

  # Mean = 14/4 = 3.5
  between_mat_mean <- to_matrix(result_mean)
  expect_equal(between_mat_mean["G1", "G2"], 3.5)

  # Max = 5
  between_mat_max <- to_matrix(result_max)
  expect_equal(between_mat_max["G1", "G2"], 5)
})

test_that("summarize_network errors with missing cluster_list", {
  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  expect_error(
    summarize_network(mat, NULL),
    "cluster_list required"
  )
})

test_that("label_abbrev alias works", {
  labels <- c("VeryLongName", "Short")
  result1 <- abbrev_label(labels, 5)
  result2 <- label_abbrev(labels, 5)
  expect_equal(result1, result2)
})

test_that("cluster_network and cnet aliases work", {
  mat <- matrix(runif(16), 4, 4)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  clusters <- list(G1 = c("A", "B"), G2 = c("C", "D"))

  result1 <- summarize_network(mat, clusters)
  result2 <- cluster_network(mat, clusters)
  result3 <- cnet(mat, clusters)

  expect_equal(to_matrix(result1), to_matrix(result2))
  expect_equal(to_matrix(result1), to_matrix(result3))
})
