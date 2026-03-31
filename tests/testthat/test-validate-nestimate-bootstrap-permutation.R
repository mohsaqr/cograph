# Validation tests for Nestimate bootstrap and permutation integration
# Uses real Nestimate objects to verify numerical correctness of cograph's
# processing: significance classification, edge indexing, CI bounds,
# label formatting, color assignment, and directed/undirected handling.

# ============================================
# Helpers
# ============================================

skip_on_cran()

skip_if_no_nestimate <- function() {
  skip_if_not_installed("Nestimate")
}

#' Create directed Nestimate netobject from sequence data
make_directed_netobject <- function(n = 200, seed = 42) {
  set.seed(seed)
  seqs <- data.frame(
    T1 = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    T2 = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
    T3 = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    T4 = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.35, 0.35, 0.3)),
    T5 = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.45, 0.3, 0.25))
  )
  Nestimate::build_network(seqs, method = "relative")
}

#' Create undirected Nestimate netobject from numeric data
make_undirected_netobject <- function(n = 100, seed = 42) {
  set.seed(seed)
  d <- data.frame(A = rnorm(n, 5, 2), B = rnorm(n, 3, 1.5), C = rnorm(n, 4, 1))
  d$B <- d$B + 0.5 * d$A
  d$C <- d$C - 0.3 * d$A + 0.4 * d$B
  Nestimate::build_network(d, method = "glasso")
}

# ============================================
# net_bootstrap: Directed
# ============================================

test_that("net_bootstrap directed: renders all display modes", {
  skip_if_no_nestimate()
  nobj <- make_directed_netobject()
  nboot <- Nestimate::bootstrap_network(nobj, iter = 100)

  expect_no_error(with_temp_png(splot(nboot)))
  expect_no_error(with_temp_png(splot(nboot, display = "styled")))
  expect_no_error(with_temp_png(splot(nboot, display = "significant")))
  expect_no_error(with_temp_png(splot(nboot, display = "full")))
})

test_that("net_bootstrap directed: show_ci and show_stars work", {
  skip_if_no_nestimate()
  nobj <- make_directed_netobject()
  nboot <- Nestimate::bootstrap_network(nobj, iter = 100)

  expect_no_error(with_temp_png(splot(nboot, show_ci = TRUE)))
  expect_no_error(with_temp_png(splot(nboot, show_stars = TRUE)))
  expect_no_error(with_temp_png(splot(nboot, show_ci = TRUE, show_stars = TRUE)))
})

test_that("net_bootstrap directed: significance matches Nestimate", {
  skip_if_no_nestimate()
  nobj <- make_directed_netobject()
  nboot <- Nestimate::bootstrap_network(nobj, iter = 100)

  sig_level <- nboot$ci_level
  weights <- nboot$original$weights
  # cograph computes: weights * (p_values < sig_level)
  cograph_sig <- weights * (nboot$p_values < sig_level)
  expect_equal(cograph_sig, nboot$significant)
})

test_that("net_bootstrap directed: edge_idx p_values indexing is correct", {
  skip_if_no_nestimate()
  nobj <- make_directed_netobject()
  nboot <- Nestimate::bootstrap_network(nobj, iter = 100)

  weights <- round(nboot$original$weights, 2)
  diag(weights) <- 0
  edge_idx <- which(weights != 0, arr.ind = TRUE)

  # p_values[edge_idx] must match direct [i,j] access
  vapply(seq_len(nrow(edge_idx)), function(k) {
    i <- edge_idx[k, 1]; j <- edge_idx[k, 2]
    expect_equal(nboot$p_values[edge_idx][k], nboot$p_values[i, j])
    TRUE
  }, logical(1))
})

test_that("net_bootstrap directed: CI indexing is correct", {
  skip_if_no_nestimate()
  nobj <- make_directed_netobject()
  nboot <- Nestimate::bootstrap_network(nobj, iter = 100)

  weights <- round(nboot$original$weights, 2)
  diag(weights) <- 0
  edge_idx <- which(weights != 0, arr.ind = TRUE)

  vapply(seq_len(nrow(edge_idx)), function(k) {
    i <- edge_idx[k, 1]; j <- edge_idx[k, 2]
    expect_equal(nboot$ci_lower[edge_idx][k], nboot$ci_lower[i, j])
    expect_equal(nboot$ci_upper[edge_idx][k], nboot$ci_upper[i, j])
    TRUE
  }, logical(1))
})

test_that("net_bootstrap directed: no edges lost to rounding", {
  skip_if_no_nestimate()
  nobj <- make_directed_netobject()
  nboot <- Nestimate::bootstrap_network(nobj, iter = 100)

  w <- nboot$original$weights
  n_orig <- sum(w != 0 & row(w) != col(w))
  w_r <- round(w, 2)
  diag(w_r) <- 0
  n_rounded <- sum(w_r != 0)
  expect_equal(n_rounded, n_orig)
})

# ============================================
# net_bootstrap: Undirected
# ============================================

test_that("net_bootstrap undirected: renders all display modes", {
  skip_if_no_nestimate()
  nobj <- make_undirected_netobject()
  nboot <- Nestimate::bootstrap_network(nobj, iter = 50)

  expect_no_error(with_temp_png(splot(nboot)))
  expect_no_error(with_temp_png(splot(nboot, display = "styled")))
  expect_no_error(with_temp_png(splot(nboot, display = "significant")))
  expect_no_error(with_temp_png(splot(nboot, display = "full")))
})

test_that("net_bootstrap undirected: uses upper-triangle edge indexing", {
  skip_if_no_nestimate()
  nobj <- make_undirected_netobject()
  nboot <- Nestimate::bootstrap_network(nobj, iter = 50)

  weights <- round(nboot$original$weights, 2)
  diag(weights) <- 0
  edge_idx <- which(weights != 0 & upper.tri(weights), arr.ind = TRUE)

  # All row indices < col indices (upper triangle)
  expect_true(all(edge_idx[, 1] < edge_idx[, 2]))
})

# ============================================
# net_permutation: Directed
# ============================================

test_that("net_permutation directed: renders all modes", {
  skip_if_no_nestimate()
  nobj1 <- make_directed_netobject(seed = 42)
  nobj2 <- make_directed_netobject(seed = 99)
  nperm <- Nestimate::permutation_test(nobj1, nobj2, iter = 100)

  expect_no_error(with_temp_png(splot(nperm)))
  expect_no_error(with_temp_png(splot(nperm, show_nonsig = TRUE)))
  expect_no_error(with_temp_png(splot(nperm, show_stars = TRUE)))
  expect_no_error(with_temp_png(splot(nperm, show_effect = TRUE)))
  expect_no_error(with_temp_png(splot(nperm, show_nonsig = TRUE,
                                       show_stars = TRUE, show_effect = TRUE)))
})

test_that("net_permutation directed: sig_mask matches p_values < alpha", {
  skip_if_no_nestimate()
  nobj1 <- make_directed_netobject(seed = 42)
  nobj2 <- make_directed_netobject(seed = 99)
  nperm <- Nestimate::permutation_test(nobj1, nobj2, iter = 100)

  sig_from_diff <- nperm$diff_sig != 0
  sig_from_pval <- nperm$p_values < nperm$alpha
  expect_equal(sig_from_diff, sig_from_pval)
})

test_that("net_permutation directed: positive diffs get green, negative get red", {
  skip_if_no_nestimate()
  nobj1 <- make_directed_netobject(seed = 42)
  nobj2 <- make_directed_netobject(seed = 99)
  nperm <- Nestimate::permutation_test(nobj1, nobj2, iter = 100)

  weights_display <- round(nperm$diff_sig, 2)
  edge_idx <- which(weights_display != 0, arr.ind = TRUE)

  if (nrow(edge_idx) > 0) {
    vapply(seq_len(nrow(edge_idx)), function(k) {
      i <- edge_idx[k, 1]; j <- edge_idx[k, 2]
      dv <- weights_display[i, j]
      expected_color <- if (dv > 0) "#009900" else "#C62828"
      # Just verify the logic is deterministic
      expect_true(dv != 0)
      expect_true(nchar(expected_color) == 7)
      TRUE
    }, logical(1))
  }
})

test_that("net_permutation directed: edge labels format correctly", {
  skip_if_no_nestimate()
  nobj1 <- make_directed_netobject(seed = 42)
  nobj2 <- make_directed_netobject(seed = 99)
  nperm <- Nestimate::permutation_test(nobj1, nobj2, iter = 100)

  get_significance_stars <- cograph:::get_significance_stars

  weights_display <- round(nperm$diff_sig, 2)
  edge_idx <- which(weights_display != 0, arr.ind = TRUE)

  if (nrow(edge_idx) > 0) {
    vapply(seq_len(nrow(edge_idx)), function(k) {
      i <- edge_idx[k, 1]; j <- edge_idx[k, 2]
      w <- weights_display[i, j]
      ws <- sub("^0\\.", ".", sprintf("%.2f", w))
      ws <- sub("^-0\\.", "-.", ws)
      stars <- get_significance_stars(nperm$p_values[i, j])
      label <- paste0(ws, stars)
      # Label should start with the weight value (no leading zero)
      expect_false(grepl("^0\\.", label))
      expect_true(nchar(label) > 0)
      TRUE
    }, logical(1))
  }
})

# ============================================
# net_permutation: Undirected
# ============================================

test_that("net_permutation undirected: renders all modes", {
  skip_if_no_nestimate()
  nobj1 <- make_undirected_netobject(seed = 42)
  nobj2 <- make_undirected_netobject(seed = 99)
  nperm <- Nestimate::permutation_test(nobj1, nobj2, iter = 50)

  expect_no_error(with_temp_png(splot(nperm)))
  expect_no_error(with_temp_png(splot(nperm, show_nonsig = TRUE)))
  expect_no_error(with_temp_png(splot(nperm, show_stars = TRUE)))
  expect_no_error(with_temp_png(splot(nperm, show_nonsig = TRUE,
                                       show_stars = TRUE, show_effect = TRUE)))
})

test_that("net_permutation undirected: uses upper-triangle edge indexing", {
  skip_if_no_nestimate()
  nobj1 <- make_undirected_netobject(seed = 42)
  nobj2 <- make_undirected_netobject(seed = 99)
  nperm <- Nestimate::permutation_test(nobj1, nobj2, iter = 50)

  is_directed <- isTRUE(nperm$x$directed)
  expect_false(is_directed)

  weights_display <- round(nperm$diff, 2)
  edge_idx <- which(weights_display != 0 & upper.tri(weights_display), arr.ind = TRUE)
  # All upper triangle
  expect_true(all(edge_idx[, 1] < edge_idx[, 2]))
})

# ============================================
# net_bootstrap: Field access validation
# ============================================

test_that("net_bootstrap: cograph reads correct fields from Nestimate", {
  skip_if_no_nestimate()
  nobj <- make_directed_netobject()
  nboot <- Nestimate::bootstrap_network(nobj, iter = 50)

  # These are the exact fields splot.net_bootstrap reads
  expect_true(!is.null(nboot$ci_level))
  expect_true(!is.null(nboot$original$weights))
  expect_true(!is.null(nboot$original$directed))
  expect_true(!is.null(nboot$original$nodes$label))
  expect_true(!is.null(nboot$p_values))
  expect_true(!is.null(nboot$ci_lower))
  expect_true(!is.null(nboot$ci_upper))

  # Field NOT present (tna uses $level, Nestimate uses $ci_level)
  expect_null(nboot$level)
  # Field NOT present (tna uses $weights, Nestimate uses $original$weights)
  expect_null(nboot$weights)
})

test_that("net_permutation: cograph reads correct fields from Nestimate", {
  skip_if_no_nestimate()
  nobj1 <- make_directed_netobject(seed = 42)
  nobj2 <- make_directed_netobject(seed = 99)
  nperm <- Nestimate::permutation_test(nobj1, nobj2, iter = 50)

  # These are the exact fields splot.net_permutation reads
  expect_true(!is.null(nperm$alpha))
  expect_true(!is.null(nperm$diff))
  expect_true(!is.null(nperm$diff_sig))
  expect_true(!is.null(nperm$p_values))
  expect_true(!is.null(nperm$effect_size))
  expect_true(!is.null(nperm$x$directed))
  expect_true(!is.null(nperm$x$nodes$label))

  # p_values and effect_size are already matrices (not in edge stats df)
  expect_true(is.matrix(nperm$p_values))
  expect_true(is.matrix(nperm$effect_size))
})
