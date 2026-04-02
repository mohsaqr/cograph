# test-coverage-plot-nestimate-40.R
# 100% line-coverage tests for Nestimate plotting support
# (splot.net_bootstrap, splot.net_permutation, splot.boot_glasso,
#  plot_netobject_group, plot_netobject_ml and their S3 aliases)

# ============================================================
# Mock factories
# ============================================================

skip_coverage_tests()

create_mock_netobject <- function(n = 4, seed = 42, directed = TRUE) {
  set.seed(seed)
  w <- matrix(runif(n * n, 0, 0.5), n, n, dimnames = list(LETTERS[1:n], LETTERS[1:n]))
  diag(w) <- 0
  structure(
    list(
      weights  = w,
      directed = directed,
      nodes    = data.frame(label = LETTERS[1:n], stringsAsFactors = FALSE)
    ),
    class = c("netobject", "cograph_network")
  )
}

create_mock_net_bootstrap <- function(n = 4, seed = 42, directed = TRUE) {
  set.seed(seed)
  nms <- LETTERS[1:n]
  w   <- matrix(runif(n * n, 0, 0.5), n, n, dimnames = list(nms, nms))
  diag(w) <- 0
  pv  <- matrix(runif(n * n), n, n, dimnames = list(nms, nms))
  diag(pv) <- 1
  orig <- create_mock_netobject(n, seed, directed)
  structure(
    list(
      original  = orig,
      model     = orig,
      mean      = w,
      sd        = w * 0.1,
      p_values  = pv,
      significant = (pv < 0.05) * 1L,
      ci_lower  = w - 0.05,
      ci_upper  = w + 0.05,
      ci_level  = 0.05,
      inference = "stability",
      method    = "relative",
      iter      = 100L
    ),
    class = c("net_bootstrap", "list")
  )
}

create_mock_net_permutation <- function(n = 4, seed = 42, directed = TRUE) {
  set.seed(seed)
  nms <- LETTERS[1:n]
  d   <- matrix(runif(n * n, -0.4, 0.4), n, n, dimnames = list(nms, nms))
  diag(d) <- 0
  pv  <- matrix(runif(n * n, 0, 0.3), n, n, dimnames = list(nms, nms))
  diag(pv) <- 1
  net_x <- create_mock_netobject(n, seed, directed)
  structure(
    list(
      x          = net_x,
      y          = net_x,
      diff       = d,
      diff_sig   = d * (pv < 0.05),
      p_values   = pv,
      effect_size = d / 0.1,
      iter       = 500L,
      alpha      = 0.05,
      adjust     = "none",
      paired     = FALSE,
      method     = "relative"
    ),
    class = c("net_permutation", "list")
  )
}

create_mock_boot_glasso <- function(n = 5, seed = 42) {
  set.seed(seed)
  nms <- LETTERS[1:n]
  raw <- matrix(runif(n * n, -0.5, 0.5), n, n)
  raw <- (raw + t(raw)) / 2
  diag(raw) <- 0
  dimnames(raw) <- list(nms, nms)
  ep    <- which(upper.tri(raw), arr.ind = TRUE)
  inc   <- runif(nrow(ep), 0.5, 1)
  thresh <- raw
  thresh[ep[inc < 0.8, , drop = FALSE]] <- 0
  thresh <- thresh + t(thresh)
  diag(thresh) <- 0
  structure(
    list(
      original_pcor    = raw,
      thresholded_pcor = thresh,
      edge_ci          = data.frame(
        edge      = paste(nms[ep[, 1]], "--", nms[ep[, 2]]),
        weight    = raw[ep],
        ci_lower  = raw[ep] - 0.05,
        ci_upper  = raw[ep] + 0.05,
        inclusion = inc,
        stringsAsFactors = FALSE
      ),
      nodes             = nms,
      p                 = n,
      alpha             = 0.05,
      iter              = 100L,
      centrality_measures = character(0)
    ),
    class = c("boot_glasso", "list")
  )
}

create_mock_netobject_group <- function(k = 3, n = 4) {
  gs <- lapply(seq_len(k), function(i) create_mock_netobject(n, seed = i * 7))
  names(gs) <- paste0("Group_", LETTERS[1:k])
  class(gs) <- c("netobject_group", "list")
  gs
}

create_mock_netobject_ml <- function(n = 4) {
  structure(
    list(
      between = create_mock_netobject(n, seed = 1),
      within  = create_mock_netobject(n, seed = 2),
      method  = "pcor"
    ),
    class = c("netobject_ml", "list")
  )
}

# ============================================================
# Tests: splot.netobject
# ============================================================

test_that("netobject: directed network gets TNA-style defaults", {
  net <- create_mock_netobject(directed = TRUE)
  expect_no_error(with_temp_png(splot.netobject(net)))
})

test_that("netobject: undirected network gets spring layout, no arrows", {
  net <- create_mock_netobject(directed = FALSE)
  expect_no_error(with_temp_png(splot.netobject(net)))
})

test_that("netobject: user overrides respected (layout, node_fill)", {
  net <- create_mock_netobject(directed = TRUE)
  expect_no_error(with_temp_png(splot.netobject(net, layout = "circle", node_fill = "steelblue")))
})

test_that("netobject: dispatch via splot()", {
  net <- create_mock_netobject(directed = TRUE)
  expect_no_error(with_temp_png(splot(net)))
})

test_that("netobject: undirected dispatch via splot()", {
  net <- create_mock_netobject(directed = FALSE)
  expect_no_error(with_temp_png(splot(net)))
})

test_that("netobject: label fallback to rownames when nodes$label is NULL", {
  net <- create_mock_netobject(directed = TRUE)
  net$nodes$label <- NULL   # force fallback to rownames(x$weights)
  expect_no_error(with_temp_png(splot.netobject(net)))
})

# ============================================================
# Tests: splot.net_bootstrap
# ============================================================

test_that("net_bootstrap: default styled mode (directed)", {
  mock_nb <- create_mock_net_bootstrap(directed = TRUE)
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb)))
})

test_that("net_bootstrap: display = 'significant'", {
  mock_nb <- create_mock_net_bootstrap()
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb, display = "significant")))
})

test_that("net_bootstrap: display = 'full'", {
  mock_nb <- create_mock_net_bootstrap()
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb, display = "full")))
})

test_that("net_bootstrap: show_stars = TRUE", {
  mock_nb <- create_mock_net_bootstrap()
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb, show_stars = TRUE)))
})

test_that("net_bootstrap: show_ci = TRUE", {
  mock_nb <- create_mock_net_bootstrap()
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb, show_ci = TRUE)))
})

test_that("net_bootstrap: show_ci = TRUE, show_stars = FALSE (line 444 path)", {
  mock_nb <- create_mock_net_bootstrap()
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb, show_ci = TRUE, show_stars = FALSE)))
})

test_that("net_bootstrap: styled mode with guaranteed significant edges (lines 401-409)", {
  mock_nb <- create_mock_net_bootstrap(n = 4, seed = 42)
  # Force at least one edge to be significant (p < 0.05) with non-zero weight
  mock_nb$p_values[1, 2] <- 0.01
  mock_nb$original$weights[1, 2] <- 0.35
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb)))
})

test_that("net_bootstrap: inherit_style = FALSE", {
  mock_nb <- create_mock_net_bootstrap()
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb, inherit_style = FALSE)))
})

test_that("net_bootstrap: undirected network", {
  mock_nb <- create_mock_net_bootstrap(directed = FALSE)
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb)))
})

test_that("net_bootstrap: user node_fill override wins", {
  mock_nb <- create_mock_net_bootstrap()
  expect_no_error(with_temp_png(splot.net_bootstrap(mock_nb, node_fill = "red")))
})

test_that("net_bootstrap: dispatch via splot()", {
  mock_nb <- create_mock_net_bootstrap()
  expect_no_error(with_temp_png(splot(mock_nb)))
})

test_that("net_bootstrap: stops when weights missing", {
  bad <- structure(list(original = list(), model = NULL, ci_level = 0.05),
    class = c("net_bootstrap", "list"))
  expect_error(splot.net_bootstrap(bad), "Cannot find weight matrix")
})

# ============================================================
# Tests: splot.net_permutation
# ============================================================

test_that("net_permutation: default (significant only, directed)", {
  mock_np <- create_mock_net_permutation(directed = TRUE)
  expect_no_error(with_temp_png(splot.net_permutation(mock_np)))
})

test_that("net_permutation: show_nonsig = TRUE", {
  mock_np <- create_mock_net_permutation()
  expect_no_error(with_temp_png(splot.net_permutation(mock_np, show_nonsig = TRUE)))
})

test_that("net_permutation: show_effect = TRUE", {
  mock_np <- create_mock_net_permutation()
  expect_no_error(with_temp_png(splot.net_permutation(mock_np, show_effect = TRUE)))
})

test_that("net_permutation: show_stars = FALSE", {
  mock_np <- create_mock_net_permutation()
  expect_no_error(with_temp_png(splot.net_permutation(mock_np, show_stars = FALSE)))
})

test_that("net_permutation: undirected", {
  mock_np <- create_mock_net_permutation(directed = FALSE)
  expect_no_error(with_temp_png(splot.net_permutation(mock_np)))
})

test_that("net_permutation: all-zero diff_sig emits message", {
  mock_np <- create_mock_net_permutation()
  mock_np$diff_sig <- mock_np$diff_sig * 0
  expect_message(
    with_temp_png(splot.net_permutation(mock_np)),
    "No edges to display"
  )
})

test_that("net_permutation: dispatch via splot()", {
  mock_np <- create_mock_net_permutation()
  expect_no_error(with_temp_png(splot(mock_np)))
})

test_that("net_permutation: stops when diff missing", {
  bad <- structure(list(x = create_mock_netobject(), alpha = 0.05),
    class = c("net_permutation", "list"))
  expect_error(splot.net_permutation(bad), "Cannot find diff matrix")
})

# ============================================================
# Tests: splot.boot_glasso
# ============================================================

test_that("boot_glasso: default (thresholded + show_inclusion)", {
  mock_bg <- create_mock_boot_glasso()
  expect_no_error(with_temp_png(splot.boot_glasso(mock_bg)))
})

test_that("boot_glasso: use_thresholded = FALSE", {
  mock_bg <- create_mock_boot_glasso()
  expect_no_error(with_temp_png(splot.boot_glasso(mock_bg, use_thresholded = FALSE)))
})

test_that("boot_glasso: show_inclusion = FALSE", {
  mock_bg <- create_mock_boot_glasso()
  expect_no_error(with_temp_png(splot.boot_glasso(mock_bg, show_inclusion = FALSE)))
})

test_that("boot_glasso: inclusion_threshold = 0.9", {
  mock_bg <- create_mock_boot_glasso()
  expect_no_error(with_temp_png(splot.boot_glasso(mock_bg, inclusion_threshold = 0.9)))
})

test_that("boot_glasso: inclusion_threshold = 0 (all edges)", {
  mock_bg <- create_mock_boot_glasso()
  expect_no_error(with_temp_png(splot.boot_glasso(mock_bg, inclusion_threshold = 0)))
})

test_that("boot_glasso: edge names with no valid node match skipped gracefully", {
  mock_bg <- create_mock_boot_glasso()
  mock_bg$edge_ci$edge[1] <- "X -- Y"  # invalid nodes
  expect_no_error(with_temp_png(splot.boot_glasso(mock_bg)))
})

test_that("boot_glasso: dispatch via splot()", {
  mock_bg <- create_mock_boot_glasso()
  expect_no_error(with_temp_png(splot(mock_bg)))
})

test_that("boot_glasso: empty edge_ci handled gracefully", {
  mock_bg <- create_mock_boot_glasso()
  mock_bg$edge_ci <- mock_bg$edge_ci[0, ]  # empty data frame
  expect_no_error(with_temp_png(splot.boot_glasso(mock_bg)))
})

# ============================================================
# Tests: plot_netobject_group
# ============================================================

test_that("netobject_group: default 3 groups, auto grid, common_scale = TRUE", {
  mock_ng <- create_mock_netobject_group(k = 3)
  expect_no_error(with_temp_png(plot_netobject_group(mock_ng), width = 600, height = 300))
})

test_that("netobject_group: common_scale = FALSE", {
  mock_ng <- create_mock_netobject_group()
  expect_no_error(with_temp_png(plot_netobject_group(mock_ng, common_scale = FALSE)))
})

test_that("netobject_group: explicit nrow=1, ncol=3", {
  mock_ng <- create_mock_netobject_group(k = 3)
  expect_no_error(with_temp_png(plot_netobject_group(mock_ng, nrow = 1, ncol = 3),
    width = 600, height = 200))
})

test_that("netobject_group: title_prefix added", {
  mock_ng <- create_mock_netobject_group()
  expect_no_error(with_temp_png(plot_netobject_group(mock_ng, title_prefix = "Net: ")))
})

test_that("netobject_group: single group (no grid)", {
  mock_ng <- create_mock_netobject_group(k = 1)
  expect_no_error(with_temp_png(plot_netobject_group(mock_ng)))
})

test_that("netobject_group: single group dispatch via splot()", {
  mock_ng <- create_mock_netobject_group(k = 1)
  expect_no_error(with_temp_png(splot(mock_ng)))
})

test_that("netobject_group: zero groups emits message and returns NULL", {
  empty_ng <- structure(list(), class = c("netobject_group", "list"))
  expect_message(plot_netobject_group(empty_ng), "No groups to display")
  expect_null(suppressMessages(plot_netobject_group(empty_ng)))
})

test_that("netobject_group: unnamed groups get auto names", {
  mock_ng <- create_mock_netobject_group(k = 2)
  names(mock_ng) <- NULL
  expect_no_error(with_temp_png(plot_netobject_group(mock_ng)))
})

test_that("netobject_group: dispatch via splot()", {
  mock_ng <- create_mock_netobject_group()
  expect_no_error(with_temp_png(splot(mock_ng), width = 600, height = 300))
})

test_that("netobject_group: dispatch via plot()", {
  mock_ng <- create_mock_netobject_group()
  expect_no_error(with_temp_png(plot(mock_ng), width = 600, height = 300))
})

# ============================================================
# Tests: plot_netobject_ml
# ============================================================

test_that("netobject_ml: default oval layout, common_scale = TRUE", {
  mock_ml <- create_mock_netobject_ml()
  expect_no_error(with_temp_png(plot_netobject_ml(mock_ml), width = 600, height = 300))
})

test_that("netobject_ml: common_scale = FALSE", {
  mock_ml <- create_mock_netobject_ml()
  expect_no_error(with_temp_png(plot_netobject_ml(mock_ml, common_scale = FALSE)))
})

test_that("netobject_ml: custom layout = 'circle'", {
  mock_ml <- create_mock_netobject_ml()
  expect_no_error(with_temp_png(plot_netobject_ml(mock_ml, layout = "circle")))
})

test_that("netobject_ml: custom titles", {
  mock_ml <- create_mock_netobject_ml()
  expect_no_error(with_temp_png(plot_netobject_ml(mock_ml, titles = c("Level 1", "Level 2"))))
})

test_that("netobject_ml: missing $between stops with error", {
  bad <- structure(list(within = create_mock_netobject()), class = c("netobject_ml", "list"))
  expect_error(plot_netobject_ml(bad), "missing \\$between")
})

test_that("netobject_ml: missing $within stops with error", {
  bad <- structure(list(between = create_mock_netobject()), class = c("netobject_ml", "list"))
  expect_error(plot_netobject_ml(bad), "missing \\$within")
})

test_that("netobject_ml: titles length < 2 stops with error", {
  mock_ml <- create_mock_netobject_ml()
  expect_error(plot_netobject_ml(mock_ml, titles = "Only one"), "titles must have length >= 2")
})

test_that("netobject_ml: dispatch via splot()", {
  mock_ml <- create_mock_netobject_ml()
  expect_no_error(with_temp_png(splot(mock_ml), width = 600, height = 300))
})

test_that("netobject_ml: dispatch via plot()", {
  mock_ml <- create_mock_netobject_ml()
  expect_no_error(with_temp_png(plot(mock_ml), width = 600, height = 300))
})

# ============================================================
# Mock factory: wtna_mixed
# ============================================================

create_mock_wtna_mixed <- function(n = 4, with_initial = FALSE) {
  set.seed(99)
  nms <- LETTERS[1:n]
  # asymmetric transition weights
  trans_w <- matrix(runif(n * n, 0, 0.4), n, n, dimnames = list(nms, nms))
  diag(trans_w) <- 0
  # symmetric co-occurrence weights
  coocc_w <- matrix(0, n, n, dimnames = list(nms, nms))
  coocc_w[1, 2] <- coocc_w[2, 1] <- 0.5
  coocc_w[3, 4] <- coocc_w[4, 3] <- 0.4

  trans_net <- structure(
    list(weights = trans_w, directed = TRUE,
         nodes = data.frame(label = nms, stringsAsFactors = FALSE),
         initial = if (with_initial) setNames(rep(1/n, n), nms) else NULL),
    class = c("netobject", "cograph_network")
  )
  coocc_net <- structure(
    list(weights = coocc_w, directed = FALSE,
         nodes = data.frame(label = nms, stringsAsFactors = FALSE)),
    class = c("netobject", "cograph_network")
  )
  structure(
    list(transition = trans_net, cooccurrence = coocc_net, method = "wtna_both"),
    class = "wtna_mixed"
  )
}

# ============================================================
# Tests: splot.wtna_mixed
# ============================================================

test_that("wtna_mixed: overlay type calls plot_mixed_network", {
  x <- create_mock_wtna_mixed()
  expect_no_error(with_temp_png(splot.wtna_mixed(x, type = "overlay")))
})

test_that("wtna_mixed: overlay inherits initial from transition net", {
  x <- create_mock_wtna_mixed(with_initial = TRUE)
  expect_no_error(with_temp_png(splot.wtna_mixed(x, type = "overlay")))
})

test_that("wtna_mixed: group type renders two-panel netobject_group", {
  x <- create_mock_wtna_mixed()
  expect_no_error(with_temp_png(splot.wtna_mixed(x, type = "group"),
                                width = 600, height = 300))
})

test_that("wtna_mixed: default type is overlay", {
  x <- create_mock_wtna_mixed()
  expect_no_error(with_temp_png(splot.wtna_mixed(x)))
})

test_that("wtna_mixed: returns invisibly", {
  x <- create_mock_wtna_mixed()
  result <- with_temp_png(splot.wtna_mixed(x))
  expect_true(!is.null(result))
})

test_that("wtna_mixed: dispatch via splot()", {
  x <- create_mock_wtna_mixed()
  expect_no_error(with_temp_png(splot(x)))
})

# ============================================================
# Tests: plot_mixed_network — initial parameter
# ============================================================

test_that("plot_mixed_network: initial probabilities draw donuts", {
  sym <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  sym[1, 2] <- sym[2, 1] <- 0.5

  asym <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  asym[1, 3] <- 0.7
  asym[3, 1] <- 0.3

  init <- setNames(c(0.4, 0.3, 0.2, 0.1), LETTERS[1:4])
  expect_no_error(with_temp_png(plot_mixed_network(sym, asym, initial = init)))
})

test_that("plot_mixed_network: initial = non-numeric stops with error", {
  sym <- matrix(0, 3, 3)
  sym[1, 2] <- sym[2, 1] <- 0.5
  asym <- matrix(0, 3, 3)
  asym[1, 3] <- 0.5

  expect_error(
    plot_mixed_network(sym, asym, initial = c("a", "b", "c")),
    "initial must be"
  )
})
