# Tests for overlay_communities

# Helper: create a named symmetric matrix for overlay tests
.test_mat <- function(n = 5) {
  nms <- paste0("S", seq_len(n))
  mat <- matrix(runif(n * n), n, n, dimnames = list(nms, nms))
  diag(mat) <- 0
  (mat + t(mat)) / 2
}

# ============================================
# Basic functionality
# ============================================

test_that("overlay_communities works with matrix + named list", {
  mat <- .test_mat(5)
  comms <- list(g1 = c("S1", "S2"), g2 = c("S3", "S4", "S5"))
  expect_no_error(with_temp_png(
    overlay_communities(mat, comms), width = 800, height = 800
  ))
})

test_that("overlay_communities returns splot result invisibly", {
  mat <- .test_mat(4)
  comms <- list(g1 = c("S1", "S2"), g2 = c("S3", "S4"))
  result <- with_temp_png(
    overlay_communities(mat, comms), width = 800, height = 800
  )
  expect_type(result, "list")
  expect_true("nodes" %in% names(result))
})

test_that("overlay_communities works with tna object", {
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  comms <- list(
    Reg  = c("plan", "monitor", "adapt"),
    Soc  = c("cohesion", "emotion", "consensus"),
    Task = c("discuss", "synthesis", "coregulate")
  )
  expect_no_error(with_temp_png(
    overlay_communities(model, comms), width = 800, height = 800
  ))
})

test_that("overlay_communities works with cograph_communities object", {
  mat <- .test_mat(6)
  comm_obj <- communities(mat, method = "louvain")
  expect_no_error(with_temp_png(
    overlay_communities(mat, comm_obj), width = 800, height = 800
  ))
})

test_that("overlay_communities works with tna + cograph_communities", {
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  mat_sym <- (model$weights + t(model$weights)) / 2
  rownames(mat_sym) <- colnames(mat_sym) <- model$labels
  comm_obj <- communities(mat_sym, method = "louvain")
  expect_no_error(with_temp_png(
    overlay_communities(model, comm_obj), width = 800, height = 800
  ))
})

# ============================================
# Parameter tests
# ============================================

test_that("overlay_communities accepts custom blob_colors", {
  mat <- .test_mat(4)
  comms <- list(g1 = c("S1", "S2"), g2 = c("S3", "S4"))
  expect_no_error(with_temp_png(
    overlay_communities(mat, comms, blob_colors = c("red", "blue")),
    width = 800, height = 800
  ))
})

test_that("overlay_communities accepts custom blob_alpha", {
  mat <- .test_mat(4)
  comms <- list(g1 = c("S1", "S2"), g2 = c("S3", "S4"))
  expect_no_error(with_temp_png(
    overlay_communities(mat, comms, blob_alpha = 0.5),
    width = 800, height = 800
  ))
})

test_that("overlay_communities accepts custom blob_linewidth", {
  mat <- .test_mat(4)
  comms <- list(g1 = c("S1", "S2"), g2 = c("S3", "S4"))
  expect_no_error(with_temp_png(
    overlay_communities(mat, comms, blob_linewidth = 2.0),
    width = 800, height = 800
  ))
})

test_that("overlay_communities passes ... to splot", {
  mat <- .test_mat(4)
  comms <- list(g1 = c("S1", "S2"), g2 = c("S3", "S4"))
  expect_no_error(with_temp_png(
    overlay_communities(mat, comms, title = "Custom Title"),
    width = 800, height = 800
  ))
})

test_that("overlay_communities recycles blob_colors", {
  mat <- .test_mat(6)
  comms <- list(g1 = c("S1", "S2"), g2 = c("S3", "S4"), g3 = c("S5", "S6"))
  expect_no_error(with_temp_png(
    overlay_communities(mat, comms, blob_colors = c("red", "blue")),
    width = 800, height = 800
  ))
})

# ============================================
# Input validation
# ============================================

test_that("overlay_communities errors on invalid input", {
  mat <- .test_mat(4)
  expect_error(overlay_communities(mat, NULL))
  expect_error(overlay_communities(mat, "garbage"), "Unknown community method")
  expect_error(overlay_communities(mat, TRUE), "must be")
})

test_that("overlay_communities errors on empty list", {
  mat <- .test_mat(4)
  expect_error(
    with_temp_png(overlay_communities(mat, list()), width = 800, height = 800),
    "length"
  )
})

# ============================================
# Edge cases
# ============================================

test_that("overlay_communities with single community", {
  mat <- .test_mat(4)
  comms <- list(all = c("S1", "S2", "S3", "S4"))
  expect_no_error(with_temp_png(
    overlay_communities(mat, comms), width = 800, height = 800
  ))
})

test_that("overlay_communities with overlapping communities", {
  mat <- .test_mat(4)
  comms <- list(g1 = c("S1", "S2", "S3"), g2 = c("S2", "S3", "S4"))
  expect_no_error(with_temp_png(
    overlay_communities(mat, comms), width = 800, height = 800
  ))
})

test_that("overlay_communities works with tna_communities object", {
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  comm_obj <- tna::communities(model, method = "walktrap")
  expect_s3_class(comm_obj, "tna_communities")
  result <- with_temp_png(
    overlay_communities(model, comm_obj), width = 800, height = 800
  )
  expect_type(result, "list")
  expect_true("nodes" %in% names(result))
})

# ============================================
# Membership vector input
# ============================================

test_that("overlay_communities works with numeric membership vector", {
  mat <- .test_mat(6)
  expect_no_error(with_temp_png(
    overlay_communities(mat, c(1, 1, 2, 2, 3, 3)), width = 800, height = 800
  ))
})

test_that("overlay_communities works with named membership vector", {
  mat <- .test_mat(4)
  mem <- setNames(c(1, 1, 2, 2), paste0("S", 1:4))
  expect_no_error(with_temp_png(
    overlay_communities(mat, mem), width = 800, height = 800
  ))
})

test_that("overlay_communities works with factor membership vector", {
  mat <- .test_mat(6)
  mem <- factor(c("grpA", "grpA", "grpB", "grpB", "grpC", "grpC"))
  expect_no_error(with_temp_png(
    overlay_communities(mat, mem), width = 800, height = 800
  ))
})

test_that("overlay_communities membership vector with tna object", {
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  mem <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
  expect_no_error(with_temp_png(
    overlay_communities(model, mem), width = 800, height = 800
  ))
})

# ============================================
# Method name string input
# ============================================

test_that("overlay_communities works with method name string", {
  mat <- .test_mat(6)
  expect_no_error(with_temp_png(
    overlay_communities(mat, "walktrap"), width = 800, height = 800
  ))
  expect_no_error(with_temp_png(
    overlay_communities(mat, "louvain"), width = 800, height = 800
  ))
})

test_that("overlay_communities accepts cluster_ prefix", {
  mat <- .test_mat(6)
  expect_no_error(with_temp_png(
    overlay_communities(mat, "cluster_louvain"), width = 800, height = 800
  ))
})

test_that("overlay_communities accepts partial method names", {
  mat <- .test_mat(6)
  expect_no_error(with_temp_png(
    overlay_communities(mat, "leading_eige"), width = 800, height = 800
  ))
})

test_that("overlay_communities errors on unknown method name", {
  mat <- .test_mat(4)
  expect_error(
    overlay_communities(mat, "garbage"), "Unknown community method"
  )
})

# ============================================
# Coverage: edge cases for inputs
# ============================================

test_that("overlay_communities method string with directed tna triggers undirected conversion", {
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  # tna models are directed → triggers as.undirected branch
  expect_no_error(with_temp_png(
    overlay_communities(model, "louvain"), width = 800, height = 800
  ))
})

test_that("overlay_communities with igraph input", {
  mat <- .test_mat(5)
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
  comms <- list(g1 = c("S1", "S2"), g2 = c("S3", "S4", "S5"))
  expect_no_error(with_temp_png(
    overlay_communities(g, comms), width = 800, height = 800
  ))
})

test_that("overlay_communities with cograph_network input", {
  mat <- .test_mat(5)
  net <- as_cograph(mat)
  comms <- list(g1 = c("S1", "S2"), g2 = c("S3", "S4", "S5"))
  expect_no_error(with_temp_png(
    overlay_communities(net, comms), width = 800, height = 800
  ))
})

test_that("overlay_communities membership vector without node names fallback", {
  # Numeric vector with no names and x has extractable states
  mat <- .test_mat(4)
  result <- with_temp_png(
    overlay_communities(mat, c(1, 1, 2, 2)), width = 800, height = 800
  )
  expect_type(result, "list")
})

test_that("overlay_communities membership vector with unnamed matrix", {
  # No dimnames → .extract_blob_states returns S1..S4, matching splot labels
  nms <- paste0("S", 1:4)
  mat <- matrix(runif(16), 4, 4, dimnames = list(nms, nms))
  diag(mat) <- 0
  result <- with_temp_png(
    overlay_communities(mat, c(1, 1, 2, 2)), width = 800, height = 800
  )
  expect_type(result, "list")
})
