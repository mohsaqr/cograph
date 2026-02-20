test_that("plot_time_line works with basic matrix input", {
  skip_if_not_installed("igraph")
  mat <- matrix(runif(36), 6, 6)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  clusters <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(mat, clusters)
  dev.off()

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_clusters, 2)
})

test_that("plot_time_line works with cluster_summary input", {
  mat <- matrix(runif(36), 6, 6)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  clusters <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))
  cs <- cluster_summary(mat, clusters, method = "sum", type = "tna",
                        compute_within = TRUE)

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(cs)
  dev.off()

  expect_s3_class(result, "cluster_summary")
})

test_that("plot_time_line works with many clusters", {
  n <- 35
  mat <- matrix(runif(n * n, 0, 0.3), n, n)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- paste0("N", seq_len(n))
  clusters <- list(
    A = paste0("N", 1:7), B = paste0("N", 8:14),
    C = paste0("N", 15:21), D = paste0("N", 22:28),
    E = paste0("N", 29:35)
  )

  png(tempfile(fileext = ".png"), width = 1200, height = 400)
  result <- plot_time_line(mat, clusters, between_minimum = 0.15)
  dev.off()

  expect_equal(result$meta$n_clusters, 5)
})

test_that("plot_time_line mode='tna' enables edge labels", {
  mat <- matrix(runif(16), 4, 4)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]
  clusters <- list(G1 = c("A", "B"), G2 = c("C", "D"))

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(mat, clusters, mode = "tna")
  dev.off()

  expect_s3_class(result, "cluster_summary")
})

test_that("plot_time_line respects styling parameters", {
  mat <- matrix(runif(36), 6, 6)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  clusters <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(mat, clusters,
    colors = c("red", "blue"),
    shell_alpha = 0.3,
    edge_alpha = 0.5,
    arrows = FALSE,
    show_labels = FALSE,
    cluster_labels = FALSE,
    legend = FALSE,
    title = "Test Title",
    subtitle = "Test Sub"
  )
  dev.off()

  expect_s3_class(result, "cluster_summary")
})

test_that("plot_time_line works with single-node clusters", {
  mat <- matrix(runif(9), 3, 3)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:3]
  clusters <- list(G1 = "A", G2 = "B", G3 = "C")

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(mat, clusters)
  dev.off()

  expect_equal(result$meta$n_clusters, 3)
})

test_that("plot_time_line legend positions work", {
  mat <- matrix(runif(16), 4, 4)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]
  clusters <- list(G1 = c("A", "B"), G2 = c("C", "D"))

  for (pos in c("bottom", "top", "left", "right", "none")) {
    png(tempfile(fileext = ".png"))
    result <- plot_time_line(mat, clusters, legend_position = pos)
    dev.off()
    expect_s3_class(result, "cluster_summary")
  }
})

test_that("plot_time_line label_abbrev works", {
  mat <- matrix(runif(16), 4, 4)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- c("LongNameA", "LongNameB",
                                       "LongNameC", "LongNameD")
  clusters <- list(G1 = c("LongNameA", "LongNameB"),
                   G2 = c("LongNameC", "LongNameD"))

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(mat, clusters, label_abbrev = 4)
  dev.off()
  expect_s3_class(result, "cluster_summary")

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(mat, clusters, label_abbrev = "auto")
  dev.off()
  expect_s3_class(result, "cluster_summary")
})

# === Vertical orientation tests ===

test_that("plot_time_line vertical orientation works", {
  mat <- matrix(runif(36), 6, 6)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  clusters <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(mat, clusters, orientation = "vertical")
  dev.off()

  expect_s3_class(result, "cluster_summary")
})

test_that("plot_time_line vertical with many clusters", {
  n <- 28
  mat <- matrix(runif(n * n, 0, 0.3), n, n)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- paste0("N", seq_len(n))
  clusters <- list(
    A = paste0("N", 1:7), B = paste0("N", 8:14),
    C = paste0("N", 15:21), D = paste0("N", 22:28)
  )

  png(tempfile(fileext = ".png"), width = 400, height = 1200)
  result <- plot_time_line(mat, clusters, orientation = "vertical",
                           between_minimum = 0.15)
  dev.off()

  expect_equal(result$meta$n_clusters, 4)
})

# === Multi-layer tests ===

test_that("plot_time_line multi-layer horizontal works", {
  n <- 14
  nodes <- paste0("N", seq_len(n))
  make_mat <- function(seed) {
    set.seed(seed)
    m <- matrix(runif(n * n, 0, 0.3), n, n)
    diag(m) <- 0
    colnames(m) <- rownames(m) <- nodes
    m
  }

  layers <- list("T1" = make_mat(10), "T2" = make_mat(20), "T3" = make_mat(30))
  clusters <- list(A = paste0("N", 1:7), B = paste0("N", 8:14))

  png(tempfile(fileext = ".png"), width = 800, height = 1200)
  result <- plot_time_line(layers = layers, cluster_list = clusters,
                           between_minimum = 0.15)
  dev.off()

  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_s3_class(result[[1]], "cluster_summary")
})

test_that("plot_time_line multi-layer vertical works", {
  n <- 14
  nodes <- paste0("N", seq_len(n))
  make_mat <- function(seed) {
    set.seed(seed)
    m <- matrix(runif(n * n, 0, 0.3), n, n)
    diag(m) <- 0
    colnames(m) <- rownames(m) <- nodes
    m
  }

  layers <- list("T1" = make_mat(10), "T2" = make_mat(20))
  clusters <- list(A = paste0("N", 1:7), B = paste0("N", 8:14))

  png(tempfile(fileext = ".png"), width = 1200, height = 800)
  result <- plot_time_line(layers = layers, cluster_list = clusters,
                           orientation = "vertical", between_minimum = 0.15)
  dev.off()

  expect_true(is.list(result))
  expect_equal(length(result), 2)
})

test_that("plot_time_line multi-layer auto-names unnamed layers", {
  n <- 6
  nodes <- LETTERS[1:6]
  m1 <- matrix(runif(36), 6, 6); diag(m1) <- 0
  colnames(m1) <- rownames(m1) <- nodes
  m2 <- matrix(runif(36), 6, 6); diag(m2) <- 0
  colnames(m2) <- rownames(m2) <- nodes

  clusters <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(layers = list(m1, m2), cluster_list = clusters)
  dev.off()

  expect_true(is.list(result))
  expect_equal(length(result), 2)
})

test_that("plot_time_line multi-layer errors without cluster_list", {
  m1 <- matrix(runif(16), 4, 4)
  colnames(m1) <- rownames(m1) <- LETTERS[1:4]

  expect_error(
    plot_time_line(layers = list(m1)),
    "cluster_list is required"
  )
})

test_that("plot_time_line single layer via layers param works", {
  mat <- matrix(runif(36), 6, 6)
  diag(mat) <- 0
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  clusters <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(layers = list("Only" = mat), cluster_list = clusters)
  dev.off()

  expect_true(is.list(result))
  expect_equal(length(result), 1)
})

test_that("plot_time_line multi-layer with global_scale FALSE works", {
  n <- 6
  nodes <- LETTERS[1:6]
  m1 <- matrix(runif(36), 6, 6); diag(m1) <- 0
  colnames(m1) <- rownames(m1) <- nodes
  m2 <- matrix(runif(36) * 5, 6, 6); diag(m2) <- 0
  colnames(m2) <- rownames(m2) <- nodes

  clusters <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  png(tempfile(fileext = ".png"))
  result <- plot_time_line(layers = list("T1" = m1, "T2" = m2),
                           cluster_list = clusters, global_scale = FALSE)
  dev.off()

  expect_true(is.list(result))
})
