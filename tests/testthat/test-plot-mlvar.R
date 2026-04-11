skip_coverage_tests()

# --- Helper: build a synthetic net_mlvar object ---
make_test_mlvar <- function(d = 5) {
  vars <- paste0("V", seq_len(d))

  make_netobj <- function(mat, dir, method) {
    nodes_df <- data.frame(id = seq_len(d), label = vars, name = vars,
                           stringsAsFactors = FALSE)
    obj <- list(
      weights    = mat,
      nodes      = nodes_df,
      edges      = data.frame(from = character(0), to = character(0)),
      directed   = dir,
      n_nodes    = d,
      n_edges    = 0L,
      method     = method,
      meta       = list(tna = list(method = method)),
      node_groups = NULL
    )
    class(obj) <- c("netobject", "cograph_network")
    obj
  }

  set.seed(99)
  temporal_mat <- matrix(rnorm(d * d, 0, 0.3), d, d, dimnames = list(vars, vars))

  contemp_mat <- matrix(rnorm(d * d, 0, 0.2), d, d)
  contemp_mat <- (contemp_mat + t(contemp_mat)) / 2
  diag(contemp_mat) <- 0
  dimnames(contemp_mat) <- list(vars, vars)

  between_mat <- matrix(rnorm(d * d, 0, 0.15), d, d)
  between_mat <- (between_mat + t(between_mat)) / 2
  diag(between_mat) <- 0
  dimnames(between_mat) <- list(vars, vars)

  fit <- list(
    temporal        = make_netobj(temporal_mat, TRUE,  "mlvar_temporal"),
    contemporaneous = make_netobj(contemp_mat,  FALSE, "mlvar_contemporaneous"),
    between         = make_netobj(between_mat,  FALSE, "mlvar_between")
  )
  class(fit) <- c("net_mlvar", "netobject_group")
  fit
}

# --- Type alias resolution ---
test_that(".resolve_mlvar_type resolves aliases", {
  resolve <- cograph:::.resolve_mlvar_type
  expect_equal(resolve("temporal"),        "temporal")
  expect_equal(resolve("t"),               "temporal")
  expect_equal(resolve("contemporaneous"), "contemporaneous")
  expect_equal(resolve("c"),               "contemporaneous")
  expect_equal(resolve("between"),         "between")
  expect_equal(resolve("b"),               "between")
  expect_equal(resolve("all"),             "all")
  expect_equal(resolve("a"),              "all")
  expect_equal(resolve("T"),               "temporal")
  expect_equal(resolve("C"),               "contemporaneous")
  expect_error(resolve("xyz"), "type must be one of")
})

# --- Plotting individual types ---
test_that("splot.net_mlvar plots temporal (default)", {
  fit <- make_test_mlvar()
  expect_no_error(splot(fit, filetype = "png",
                        filename = file.path(tempdir(), "mlvar_t")))
  expect_true(file.exists(paste0(tempdir(), "/mlvar_t.png")))
})

test_that("splot.net_mlvar plots contemporaneous via alias", {
  fit <- make_test_mlvar()
  expect_no_error(splot(fit, type = "c", filetype = "png",
                        filename = file.path(tempdir(), "mlvar_c")))
  expect_true(file.exists(paste0(tempdir(), "/mlvar_c.png")))
})

test_that("splot.net_mlvar plots between via alias", {
  fit <- make_test_mlvar()
  expect_no_error(splot(fit, type = "b", filetype = "png",
                        filename = file.path(tempdir(), "mlvar_b")))
  expect_true(file.exists(paste0(tempdir(), "/mlvar_b.png")))
})

# --- Panel mode ---
test_that("splot.net_mlvar type='all' renders 1x3 panel", {
  fit <- make_test_mlvar()
  expect_no_error(splot(fit, type = "all", filetype = "png",
                        filename = file.path(tempdir(), "mlvar_all"),
                        width = 21))
  expect_true(file.exists(paste0(tempdir(), "/mlvar_all.png")))
})

# --- User args override styling ---
test_that("user args override default styling", {
  fit <- make_test_mlvar()
  expect_no_error(splot(fit, type = "t",
                        layout = "circle", node_size = 15,
                        title = "Custom Title",
                        filetype = "png",
                        filename = file.path(tempdir(), "mlvar_override")))
  expect_true(file.exists(paste0(tempdir(), "/mlvar_override.png")))
})

# --- Missing network errors ---
test_that("splot.net_mlvar errors on missing network", {
  fit <- make_test_mlvar()
  fit$between <- NULL
  expect_error(splot(fit, type = "b"), "not found")
})

# --- Dispatch from splot() works ---
test_that("splot() dispatches net_mlvar before netobject_group", {
  fit <- make_test_mlvar()
  # If dispatch is wrong, this would hit plot_netobject_group and likely error
  # or produce a different plot. The key check is no error.
  expect_no_error(splot(fit, type = "t", filetype = "png",
                        filename = file.path(tempdir(), "mlvar_dispatch")))
})
