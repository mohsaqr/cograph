# Tests for TNA integration in cograph
# Tests is_tna_network() function

skip_on_cran()

test_that("is_tna_network returns FALSE for non-TNA networks", {
  # Matrix input
  mat <- matrix(runif(25), 5, 5)
  net <- as_cograph(mat)
  expect_false(is_tna_network(net))

  # Edge list input
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.5, 0.3, 0.2))
  net2 <- as_cograph(edges)
  expect_false(is_tna_network(net2))
})

test_that("is_tna_network returns TRUE for TNA networks", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  expect_true(is_tna_network(net))
})

test_that("cograph_network $meta$tna field has correct structure", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Check $meta$tna field exists
  expect_true(!is.null(net$meta$tna))
  expect_true(is.list(net$meta$tna))

  # Check required fields (minimal structure - no model stored)
  expect_true("type" %in% names(net$meta$tna))
  expect_equal(net$meta$tna$type, "tna")

  # For single tna, group fields should be NULL
  expect_null(net$meta$tna$group_index)
  expect_null(net$meta$tna$group_name)
})

test_that("source field is 'tna' for TNA networks", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  expect_equal(net$meta$source, "tna")
})

test_that("TNA network can still be plotted", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Should not error
  expect_no_error({
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 400, height = 400)
    splot(net)
    dev.off()
    unlink(tmp)
  })
})

test_that("TNA weights matrix is preserved in cograph_network", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Check weights matrix is stored
  expect_true(!is.null(net$weights))
  expect_true(is.matrix(net$weights))
  expect_equal(dim(net$weights), dim(model$weights))
  expect_equal(net$weights, model$weights)
})

test_that("TNA inits are preserved in nodes", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Check inits are stored in nodes
  nodes <- get_nodes(net)
  expect_true("inits" %in% names(nodes))
  expect_equal(as.numeric(nodes$inits), as.numeric(model$inits))
})

test_that("TNA colors are extracted if available", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)

  # Check if colors exist in model
  has_colors <- !is.null(model$data) && !is.null(attr(model$data, "colors"))

  net <- as_cograph(model)
  nodes <- get_nodes(net)

  if (has_colors) {
    expect_true("color" %in% names(nodes))
  }
})

# ===========================================================================
# Plotting / dispatch / analytics surface (added 2026-07-04)
#
# The tests above cover tna -> cograph conversion. These drive every cograph
# PLOTTING and analytics entry point with real tna objects, because cograph is
# the plotting layer for tna and `tna::plot_compare()` delegates to
# `cograph::plot_compare()` by name — directed-tna-only regressions are
# invisible to the matrix-based tests and only surface here.
#
# Explicit `cograph::` / `tna::` prefixes throughout: earlier tests in this
# file call library(tna), which masks cograph's `communities`/`plot_compare`.
# Objects are built from a small wide subset (+ a group vector) for speed;
# never call tna::group_tna() on the full long dataset (multi-GB memory).
# ===========================================================================

.tna_gr <- function(n = 200) {
  e <- new.env()
  utils::data("group_regulation", package = "tna", envir = e)
  utils::head(get("group_regulation", envir = e), n)
}

test_that("splot() renders core tna models (tna / ftna / ctna)", {
  skip_if_not_installed("tna")
  gr <- .tna_gr()
  for (ctor in c("tna", "ftna", "ctna")) {
    obj <- getExportedValue("tna", ctor)(gr)
    res <- safe_plot(cograph::splot(obj))
    expect_true(res$success, info = paste(ctor, "->", res$error))
  }
})

test_that("splot() renders tna_bootstrap and tna_permutation", {
  skip_if_not_installed("tna")
  gr <- .tna_gr()
  b <- tna::bootstrap(tna::tna(gr), iter = 20)
  expect_true(safe_plot(cograph::splot(b))$success)
  perm <- tna::permutation_test(tna::tna(gr[1:100, ]),
                                tna::tna(gr[101:200, ]), iter = 20)
  expect_true(safe_plot(cograph::splot(perm))$success)
})

test_that("splot() renders tna_communities and tna_disparity", {
  skip_if_not_installed("tna")
  t <- tna::tna(.tna_gr())
  expect_true(safe_plot(cograph::splot(tna::communities(t)))$success)
  expect_true(safe_plot(cograph::splot(cograph::disparity_filter(t)))$success)
})

test_that("splot() renders group_tna and its bootstrap / permutation", {
  skip_if_not_installed("tna")
  gr <- .tna_gr()
  g <- tna::group_tna(gr, group = rep(c("G1", "G2"), length.out = nrow(gr)))
  expect_true(safe_plot(cograph::splot(g))$success)
  expect_true(safe_plot(cograph::splot(g, i = 1))$success)
  expect_true(safe_plot(cograph::splot(g, i = "G1"))$success)
  expect_true(safe_plot(cograph::splot(tna::bootstrap(g, iter = 10)))$success)
  expect_true(safe_plot(cograph::splot(tna::permutation_test(g, iter = 20)))$success)
})

test_that("plot_tna() / tplot() accept qgraph-style params on a tna", {
  skip_if_not_installed("tna")
  t <- tna::tna(.tna_gr())
  expect_true(safe_plot(cograph::plot_tna(t))$success)
  expect_true(safe_plot(cograph::tplot(t))$success)
  expect_true(safe_plot(
    cograph::plot_tna(t, vsize = 10, asize = 4, edge.color = "grey"))$success)
  expect_true(safe_plot(cograph::tplot(t, minimum = 0.05, cut = 0.1))$success)
})

test_that("tna converts through to_matrix / to_igraph / from_tna", {
  skip_if_not_installed("tna")
  t <- tna::tna(.tna_gr())
  cg <- cograph::as_cograph(t)
  m <- cograph::to_matrix(cg)
  expect_true(is.matrix(m))
  expect_equal(nrow(m), ncol(m))
  expect_true(igraph::is_igraph(cograph::to_igraph(cg)))
  expect_true(is.list(cograph::from_tna(t, engine = "splot", plot = FALSE)))
})

test_that("plot_difference / plot_compare / tna::plot_compare seam work on tnas", {
  skip_if_not_installed("tna")
  gr <- .tna_gr()
  a <- tna::tna(gr[1:100, ]); b <- tna::tna(gr[101:200, ])
  expect_true(safe_plot(cograph::plot_difference(a, b))$success)
  expect_true(safe_plot(cograph::plot_compare(a, b))$success)
  # the integration contract: tna's S3 method calls cograph by name
  expect_true(safe_plot(tna::plot_compare(a, b))$success)
  # pre-computed difference object
  cmp <- tna::compare(a, b)
  expect_true(safe_plot(cograph::plot_difference(cmp))$success)
})

test_that("detect_communities() works on a directed tna for every method", {
  skip_if_not_installed("tna")
  skip_if_not_installed("igraph")
  t <- tna::tna(.tna_gr())
  # louvain/leiden are undirected-only in igraph and used to ABORT on a tna
  # (which is always directed); they must now collapse and return communities.
  for (meth in c("louvain", "leiden", "walktrap", "fast_greedy",
                 "label_prop", "infomap")) {
    d <- suppressMessages(cograph::detect_communities(t, method = meth))
    expect_s3_class(d, "data.frame")
    expect_true(all(c("node", "community") %in% names(d)),
                info = paste("method", meth))
  }
})

test_that("centrality() computes on a tna", {
  skip_if_not_installed("tna")
  cen <- cograph::centrality(tna::tna(.tna_gr()))
  expect_true(is.data.frame(cen))
  expect_gt(nrow(cen), 0)
})
