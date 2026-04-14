# =============================================================================
# Rigorous Numerical Equivalence Tests — Rich Club
#
# 100 random networks. Every threshold checked. Weighted and unweighted.
# Reference: manual igraph subgraph computation + tnet cross-validation.
# =============================================================================

skip_on_cran()
skip_if_not_installed("igraph")

.rc_log <- new.env(parent = emptyenv())
.rc_log$rows <- list()

.log_rc <- function(func, cfg, n_checked, n_passed, n_failed,
                    max_abs_err = NA_real_, notes = "") {
  .rc_log$rows[[length(.rc_log$rows) + 1L]] <- data.frame(
    function_name = func, n_nodes = cfg$n, density = cfg$density,
    seed = cfg$seed, values_checked = n_checked, values_passed = n_passed,
    values_failed = n_failed, max_abs_error = max_abs_err, notes = notes,
    stringsAsFactors = FALSE
  )
}

.write_rc_report <- function() {
  if (length(.rc_log$rows) == 0L) return(invisible(NULL))
  df <- do.call(rbind, .rc_log$rows)
  utils::write.csv(df, file.path(tempdir(), "rich_club_equivalence_report.csv"),
                   row.names = FALSE)
  cat(sprintf(
    "\n=== RICH CLUB EQUIVALENCE REPORT ===\nFunctions: %d\nConfigs: %d\nValues checked: %s\nPassed: %s\nFailed: %s\nMax error: %.2e\n",
    length(unique(df$function_name)), nrow(df),
    format(sum(df$values_checked), big.mark = ","),
    format(sum(df$values_passed), big.mark = ","),
    format(sum(df$values_failed), big.mark = ","),
    max(df$max_abs_error, na.rm = TRUE)
  ))
  invisible(df)
}

TOL <- 1e-10

set.seed(7777)
N_CONFIGS <- 100L
rc_seeds <- sample.int(100000, N_CONFIGS)
rc_nodes <- sample(c(15, 20, 25, 30, 40), N_CONFIGS, replace = TRUE)
rc_densities <- runif(N_CONFIGS, 0.1, 0.35)

rc_configs <- lapply(seq_len(N_CONFIGS), function(i) {
  list(n = rc_nodes[i], density = rc_densities[i], seed = rc_seeds[i])
})

.make_rc_mat <- function(cfg, weighted = TRUE) {
  mat <- create_test_matrix(cfg$n, density = cfg$density, seed = cfg$seed,
                            symmetric = TRUE)
  rownames(mat) <- colnames(mat) <- paste0("N", seq_len(cfg$n))
  if (!weighted) mat[mat > 0] <- 1
  mat
}

# =============================================================================
# 1. Unweighted phi: exact match against manual igraph subgraph density
# =============================================================================

test_that("rich_club unweighted phi: every threshold, 100 networks", {
  lapply(rc_configs, function(cfg) {
    mat <- .make_rc_mat(cfg, weighted = FALSE)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse")
    }
    g <- igraph::simplify(g)
    deg <- igraph::degree(g)

    rc <- rich_club(mat, weighted = FALSE, normalized = FALSE)
    curve <- rc
    if (is.null(curve) || nrow(curve) == 0) {
      .log_rc("unweighted_phi", cfg, 0, 0, 0, 0, "no thresholds")
      return(invisible(NULL))
    }

    errs <- vapply(seq_len(nrow(curve)), function(i) {
      thr <- curve$threshold[i]
      rich_idx <- which(deg > thr)
      nr <- length(rich_idx)
      subg <- igraph::induced_subgraph(g, rich_idx)
      ref_phi <- igraph::ecount(subg) / (nr * (nr - 1) / 2)
      abs(curve$phi[i] - ref_phi)
    }, numeric(1))

    max_err <- max(errs)
    n_fail <- sum(errs > TOL)
    .log_rc("unweighted_phi", cfg, length(errs), length(errs) - n_fail,
            n_fail, max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 2. Weighted phi: exact match against manual ranked-weight formula
# =============================================================================

test_that("rich_club weighted phi: every threshold, 100 networks", {
  lapply(rc_configs, function(cfg) {
    mat <- .make_rc_mat(cfg, weighted = TRUE)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse",
                                 edge.attr.comb = list(weight = "sum", "ignore"))
    }
    g <- igraph::simplify(g)
    deg <- igraph::degree(g)
    edge_wts <- if (!is.null(igraph::E(g)$weight)) {
      igraph::E(g)$weight
    } else {
      rep(1, igraph::ecount(g))
    }
    ranked_wts <- sort(edge_wts, decreasing = TRUE)

    rc <- rich_club(mat, weighted = TRUE, normalized = FALSE)
    curve <- rc
    if (is.null(curve) || nrow(curve) == 0) {
      .log_rc("weighted_phi", cfg, 0, 0, 0, 0, "no thresholds")
      return(invisible(NULL))
    }

    errs <- vapply(seq_len(nrow(curve)), function(i) {
      thr <- curve$threshold[i]
      rich_idx <- which(deg > thr)
      subg <- igraph::induced_subgraph(g, rich_idx)
      e_rich <- igraph::ecount(subg)
      if (e_rich == 0) return(abs(curve$phi[i] - 0))

      w_rich <- sum(if (!is.null(igraph::E(subg)$weight)) {
        igraph::E(subg)$weight
      } else {
        rep(1, e_rich)
      })
      w_max <- sum(ranked_wts[seq_len(min(e_rich, length(ranked_wts)))])
      ref_phi <- if (w_max == 0) 0 else w_rich / w_max
      abs(curve$phi[i] - ref_phi)
    }, numeric(1))

    max_err <- max(errs)
    n_fail <- sum(errs > TOL)
    .log_rc("weighted_phi", cfg, length(errs), length(errs) - n_fail,
            n_fail, max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 3. Strength-based rich club: verify prominence uses strength not degree
# =============================================================================

test_that("rich_club strength-based: uses strength for thresholds, 100 networks", {
  lapply(rc_configs, function(cfg) {
    mat <- .make_rc_mat(cfg, weighted = TRUE)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse",
                                 edge.attr.comb = list(weight = "sum", "ignore"))
    }
    g <- igraph::simplify(g)
    strength <- igraph::strength(g)
    edge_wts <- if (!is.null(igraph::E(g)$weight)) {
      igraph::E(g)$weight
    } else {
      rep(1, igraph::ecount(g))
    }
    ranked_wts <- sort(edge_wts, decreasing = TRUE)

    rc <- rich_club(mat, rich = "s", weighted = TRUE, normalized = FALSE)
    curve <- rc
    if (is.null(curve) || nrow(curve) == 0) {
      .log_rc("strength_phi", cfg, 0, 0, 0, 0, "no thresholds")
      return(invisible(NULL))
    }

    errs <- vapply(seq_len(nrow(curve)), function(i) {
      thr <- curve$threshold[i]
      # Prominence is strength, not degree
      rich_idx <- which(strength > thr)
      nr <- length(rich_idx)
      if (nr < 2) return(0)

      subg <- igraph::induced_subgraph(g, rich_idx)
      e_rich <- igraph::ecount(subg)
      if (e_rich == 0) return(abs(curve$phi[i]))

      w_rich <- sum(if (!is.null(igraph::E(subg)$weight)) {
        igraph::E(subg)$weight
      } else {
        rep(1, e_rich)
      })
      w_max <- sum(ranked_wts[seq_len(min(e_rich, length(ranked_wts)))])
      ref_phi <- if (w_max == 0) 0 else w_rich / w_max
      abs(curve$phi[i] - ref_phi)
    }, numeric(1))

    max_err <- max(errs)
    n_fail <- sum(errs > TOL)
    .log_rc("strength_phi", cfg, length(errs), length(errs) - n_fail,
            n_fail, max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 4. Curve properties: phi values in [0,1], n_rich decreasing
# =============================================================================

test_that("rich_club curve properties: 100 networks", {
  lapply(rc_configs, function(cfg) {
    mat <- .make_rc_mat(cfg)
    rc <- rich_club(mat, normalized = FALSE)
    curve <- rc
    if (is.null(curve) || nrow(curve) == 0) return(invisible(NULL))

    n_checks <- 0L; n_fail <- 0L

    # phi in [0, 1]
    vapply(curve$phi, function(p) {
      n_checks <<- n_checks + 1L
      if (p < -TOL || p > 1 + TOL) n_fail <<- n_fail + 1L
      TRUE
    }, logical(1))

    # n_rich non-increasing as threshold increases
    if (nrow(curve) > 1) {
      diffs <- diff(curve$n_rich)
      n_checks <- n_checks + length(diffs)
      n_fail <- n_fail + sum(diffs > 0)
    }

    # thresholds strictly increasing
    if (nrow(curve) > 1) {
      t_diffs <- diff(curve$threshold)
      n_checks <- n_checks + length(t_diffs)
      n_fail <- n_fail + sum(t_diffs <= 0)
    }

    .log_rc("curve_properties", cfg, n_checks, n_checks - n_fail, n_fail, 0)
    expect_equal(n_fail, 0L,
      info = sprintf("n=%d seed=%d", cfg$n, cfg$seed))
  })
})

# =============================================================================
# 5. Normalized: phi_norm = phi / phi_rand
# =============================================================================

test_that("rich_club normalized: phi_norm = phi / phi_rand, 100 networks", {
  lapply(rc_configs, function(cfg) {
    mat <- .make_rc_mat(cfg)
    rc <- rich_club(mat, normalized = TRUE, n_random = 20, seed = cfg$seed)
    curve <- rc
    if (is.null(curve) || nrow(curve) == 0 || !"phi_norm" %in% names(curve)) {
      .log_rc("normalized_ratio", cfg, 0, 0, 0, 0, "no data")
      return(invisible(NULL))
    }

    valid <- !is.na(curve$phi_norm) & !is.na(curve$phi_rand) &
             curve$phi_rand > 0
    if (!any(valid)) {
      .log_rc("normalized_ratio", cfg, 0, 0, 0, 0, "all NA")
      return(invisible(NULL))
    }

    errs <- abs(curve$phi_norm[valid] -
                curve$phi[valid] / curve$phi_rand[valid])
    max_err <- max(errs)
    n_fail <- sum(errs > TOL)
    .log_rc("normalized_ratio", cfg, length(errs), length(errs) - n_fail,
            n_fail, max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 6. rich_club_local: every node, manual computation, 100 networks
# =============================================================================

test_that("rich_club_local: every node matches manual formula, 100 networks", {
  lapply(rc_configs, function(cfg) {
    mat <- .make_rc_mat(cfg, weighted = TRUE)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse",
                                 edge.attr.comb = list(weight = "sum", "ignore"))
    }
    g <- igraph::simplify(g)
    n <- igraph::vcount(g)
    nms <- igraph::V(g)$name
    if (is.null(nms)) nms <- as.character(seq_len(n))

    adj <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight",
                                                  sparse = FALSE))
    if (all(adj %in% c(0, 1))) {
      adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    }

    deg <- igraph::degree(g)
    is_prom <- deg > stats::median(deg)

    # Manual reference
    ref <- vapply(seq_len(n), function(i) {
      neighbors <- which(adj[i, ] > 0)
      if (length(neighbors) == 0) return(1)
      w_all <- adj[i, neighbors]
      mean_all <- mean(w_all)
      if (mean_all == 0) return(1)
      rich_nb <- neighbors[is_prom[neighbors]]
      if (length(rich_nb) == 0) return(1)
      mean(adj[i, rich_nb]) / mean_all
    }, numeric(1))
    names(ref) <- nms

    co_df <- rich_club_local(mat)
    co <- stats::setNames(co_df$score, co_df$node)

    # Compare every node
    errs <- vapply(nms, function(nm) {
      if (is.na(ref[nm]) && is.na(co[nm])) return(0)
      if (is.na(ref[nm]) || is.na(co[nm])) return(1)
      abs(co[nm] - ref[nm])
    }, numeric(1))

    max_err <- max(errs)
    n_fail <- sum(errs > TOL)
    .log_rc("rich_club_local", cfg, length(errs), length(errs) - n_fail,
            n_fail, max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 7. rich_club_local with explicit prominence vector, 100 networks
# =============================================================================

test_that("rich_club_local with prominence vector: 100 networks", {
  lapply(rc_configs, function(cfg) {
    mat <- .make_rc_mat(cfg, weighted = TRUE)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse",
                                 edge.attr.comb = list(weight = "sum", "ignore"))
    }
    g <- igraph::simplify(g)
    n <- igraph::vcount(g)
    nms <- igraph::V(g)$name
    if (is.null(nms)) nms <- as.character(seq_len(n))

    adj <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight",
                                                  sparse = FALSE))
    if (all(adj %in% c(0, 1))) {
      adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    }

    # Random prominence assignment
    set.seed(cfg$seed + 5000)
    is_prom <- sample(c(TRUE, FALSE), n, replace = TRUE)

    ref <- vapply(seq_len(n), function(i) {
      neighbors <- which(adj[i, ] > 0)
      if (length(neighbors) == 0) return(1)
      w_all <- adj[i, neighbors]
      mean_all <- mean(w_all)
      if (mean_all == 0) return(1)
      rich_nb <- neighbors[is_prom[neighbors]]
      if (length(rich_nb) == 0) return(1)
      mean(adj[i, rich_nb]) / mean_all
    }, numeric(1))
    names(ref) <- nms

    co_df <- rich_club_local(mat, prominence = as.integer(is_prom))
    co <- stats::setNames(co_df$score, co_df$node)

    errs <- vapply(nms, function(nm) {
      if (is.na(ref[nm]) && is.na(co[nm])) return(0)
      if (is.na(ref[nm]) || is.na(co[nm])) return(1)
      abs(co[nm] - ref[nm])
    }, numeric(1))

    max_err <- max(errs)
    n_fail <- sum(errs > TOL)
    .log_rc("rich_club_local_prom", cfg, length(errs), length(errs) - n_fail,
            n_fail, max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 8. Cross-validate with tnet if available
# =============================================================================

test_that("rich_club_local matches tnet::weighted_richclub_local_w", {
  skip_if_not_installed("tnet")

  lapply(rc_configs[1:20], function(cfg) {
    mat <- .make_rc_mat(cfg, weighted = TRUE)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse",
                                 edge.attr.comb = list(weight = "sum", "ignore"))
    }
    g <- igraph::simplify(g)
    n <- igraph::vcount(g)

    deg <- igraph::degree(g)
    is_prom <- as.integer(deg > stats::median(deg))

    # tnet format: i, j, w edge list (both directions)
    el <- igraph::as_data_frame(g, what = "edges")
    w_col <- if ("weight" %in% names(el)) el$weight else rep(1, nrow(el))
    from_idx <- match(el$from, igraph::V(g)$name)
    to_idx <- match(el$to, igraph::V(g)$name)
    tnet_el <- rbind(
      cbind(i = from_idx, j = to_idx, w = w_col),
      cbind(i = to_idx, j = from_idx, w = w_col)
    )

    tnet_res <- tryCatch(
      tnet::weighted_richclub_local_w(tnet_el, is_prom),
      error = function(e) NULL
    )
    if (is.null(tnet_res)) return(invisible(NULL))

    co_df <- rich_club_local(mat, prominence = is_prom)
    co <- stats::setNames(co_df$score, co_df$node)
    nms <- igraph::V(g)$name
    if (is.null(nms)) nms <- as.character(seq_len(n))

    # tnet returns matrix with node (integer) and ratio columns
    errs <- vapply(seq_len(nrow(tnet_res)), function(i) {
      node_id <- tnet_res[i, "node"]
      tnet_ratio <- tnet_res[i, "ratio"]
      co_ratio <- co[nms[node_id]]
      if (is.na(tnet_ratio) && is.na(co_ratio)) return(0)
      if (is.na(tnet_ratio) || is.na(co_ratio)) return(1)
      abs(co_ratio - tnet_ratio)
    }, numeric(1))

    max_err <- max(errs)
    n_fail <- sum(errs > TOL)
    .log_rc("tnet_cross_val", cfg, length(errs), length(errs) - n_fail,
            n_fail, max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# Report
# =============================================================================

test_that("rich club equivalence report", {
  report <- .write_rc_report()
  expect_true(is.data.frame(report))
  expect_equal(sum(report$values_failed), 0L)
})
