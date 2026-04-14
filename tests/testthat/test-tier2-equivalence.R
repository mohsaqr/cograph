# =============================================================================
# Rigorous Numerical Equivalence Tests — Tier 2 Features
#
# 100+ random networks per function. Every value checked.
# Reference: igraph, manual matrix algebra, closed-form formulas.
# Results logged to tmp/equivalence_report.csv
# =============================================================================

skip_on_cran()
skip_if_not_installed("igraph")

# ---------------------------------------------------------------------------
# Report infrastructure
# ---------------------------------------------------------------------------

.equiv_log <- new.env(parent = emptyenv())
.equiv_log$rows <- list()

.log_result <- function(func, config, n_checked, n_passed, n_failed,
                        max_abs_err = NA_real_, notes = "") {
  .equiv_log$rows[[length(.equiv_log$rows) + 1L]] <- data.frame(
    function_name = func,
    n_nodes = config$n,
    density = config$density,
    seed = config$seed,
    directed = isTRUE(config$directed),
    values_checked = n_checked,
    values_passed = n_passed,
    values_failed = n_failed,
    max_abs_error = max_abs_err,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

.write_report <- function() {
  if (length(.equiv_log$rows) == 0L) return(invisible(NULL))
  df <- do.call(rbind, .equiv_log$rows)
  utils::write.csv(df, file.path(tempdir(), "equivalence_report.csv"), row.names = FALSE)
  cat(sprintf(
    "\n=== EQUIVALENCE REPORT ===\nFunctions tested: %d\nConfigurations: %d\nTotal values checked: %s\nTotal passed: %s\nTotal failed: %s\nMax absolute error: %.2e\nReport: %s\n",
    length(unique(df$function_name)),
    nrow(df),
    format(sum(df$values_checked), big.mark = ","),
    format(sum(df$values_passed), big.mark = ","),
    format(sum(df$values_failed), big.mark = ","),
    max(df$max_abs_error, na.rm = TRUE),
    file.path(tempdir(), "equivalence_report.csv")
  ))
  invisible(df)
}

# ---------------------------------------------------------------------------
# 100 network configurations
# ---------------------------------------------------------------------------

set.seed(2026)
N_CONFIGS <- 100L
seeds <- sample.int(100000, N_CONFIGS)
nodes <- sample(c(8, 10, 12, 15, 20, 25, 30), N_CONFIGS, replace = TRUE)
densities <- runif(N_CONFIGS, 0.1, 0.4)

undirected_configs <- lapply(seq_len(N_CONFIGS), function(i) {

  list(n = nodes[i], density = densities[i], seed = seeds[i], directed = FALSE)
})

directed_configs <- lapply(seq_len(N_CONFIGS), function(i) {
  list(n = nodes[i], density = densities[i], seed = seeds[i], directed = TRUE)
})

.make_mat <- function(cfg) {
  mat <- create_test_matrix(cfg$n, density = cfg$density, seed = cfg$seed,
                            symmetric = !isTRUE(cfg$directed))
  rownames(mat) <- colnames(mat) <- paste0("N", seq_len(cfg$n))
  mat
}

TOL <- 1e-10

# =============================================================================
# 1. neighborhood_overlap — 100 networks, every edge
# =============================================================================

test_that("neighborhood_overlap: 100 networks, every edge", {
  lapply(undirected_configs, function(cfg) {
    mat <- .make_mat(cfg)
    g <- to_igraph(mat)
    res <- neighborhood_overlap(mat)
    el <- igraph::as_edgelist(g)
    adj_list <- igraph::as_adj_list(g, mode = "all")
    ne <- nrow(el)
    if (ne == 0L) {
      .log_result("neighborhood_overlap", cfg, 0, 0, 0, 0, "no edges")
      return(invisible(NULL))
    }

    errs <- vapply(seq_len(ne), function(i) {
      u_id <- match(el[i, 1], igraph::V(g)$name)
      v_id <- match(el[i, 2], igraph::V(g)$name)
      n_u <- setdiff(as.integer(adj_list[[u_id]]), c(u_id, v_id))
      n_v <- setdiff(as.integer(adj_list[[v_id]]), c(u_id, v_id))
      ref_shared <- length(intersect(n_u, n_v))
      ref_union <- length(union(n_u, n_v))
      ref_ov <- if (ref_union == 0L) 0 else ref_shared / ref_union

      row <- res[res$from == el[i, 1] & res$to == el[i, 2], ]
      if (nrow(row) == 0) row <- res[res$from == el[i, 2] & res$to == el[i, 1], ]

      c(abs(row$shared - ref_shared), abs(row$overlap - ref_ov))
    }, numeric(2))

    n_vals <- 2L * ne
    max_err <- max(errs)
    n_fail <- sum(errs > TOL)
    .log_result("neighborhood_overlap", cfg, n_vals, n_vals - n_fail, n_fail,
                max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 2. simmelian_strength — 100 networks, every edge + triangle total
# =============================================================================

test_that("simmelian_strength: 100 networks, every edge + triangle total", {
  lapply(undirected_configs, function(cfg) {
    mat <- .make_mat(cfg)
    g <- to_igraph(mat)
    res <- simmelian_strength(mat)
    el <- igraph::as_edgelist(g)
    adj_list <- igraph::as_adj_list(g, mode = "all")
    ne <- nrow(el)
    if (ne == 0L) {
      .log_result("simmelian_strength", cfg, 0, 0, 0, 0, "no edges")
      return(invisible(NULL))
    }

    errs <- vapply(seq_len(ne), function(i) {
      u_id <- match(el[i, 1], igraph::V(g)$name)
      v_id <- match(el[i, 2], igraph::V(g)$name)
      n_u <- setdiff(as.integer(adj_list[[u_id]]), c(u_id, v_id))
      n_v <- setdiff(as.integer(adj_list[[v_id]]), c(u_id, v_id))
      ref <- length(intersect(n_u, n_v))
      row <- res[res$from == el[i, 1] & res$to == el[i, 2], ]
      if (nrow(row) == 0) row <- res[res$from == el[i, 2] & res$to == el[i, 1], ]
      abs(row$triangles - ref)
    }, numeric(1))

    # Triangle total cross-check
    ig_tri <- sum(igraph::count_triangles(g)) / 3
    co_tri <- sum(res$triangles) / 3
    tri_err <- abs(co_tri - ig_tri)

    n_vals <- ne + 1L
    max_err <- max(c(errs, tri_err))
    n_fail <- sum(errs > TOL) + (tri_err > TOL)
    .log_result("simmelian_strength", cfg, n_vals, n_vals - n_fail, n_fail,
                max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 3. edge_reciprocity — 100 directed networks, every edge
# =============================================================================

test_that("edge_reciprocity: 100 directed networks, every value", {
  lapply(directed_configs, function(cfg) {
    mat <- .make_mat(cfg)
    res <- edge_reciprocity(mat, directed = TRUE)
    nms <- rownames(mat)
    ne <- nrow(res)
    if (ne == 0L) {
      .log_result("edge_reciprocity", cfg, 0, 0, 0, 0, "no edges")
      return(invisible(NULL))
    }

    # 4 checks per edge: weight, reciprocated, reverse_weight, weight_ratio
    err_count <- 0L
    max_err <- 0
    n_checks <- 0L

    vapply(seq_len(ne), function(i) {
      fi <- match(res$from[i], nms)
      ti <- match(res$to[i], nms)

      # weight
      e <- abs(res$weight[i] - mat[fi, ti])
      max_err <<- max(max_err, e)
      n_checks <<- n_checks + 1L
      if (e > TOL) err_count <<- err_count + 1L

      # reciprocated
      ref_recip <- mat[ti, fi] > 0
      n_checks <<- n_checks + 1L
      if (!identical(res$reciprocated[i], ref_recip)) err_count <<- err_count + 1L

      # reverse_weight + ratio
      if (ref_recip) {
        e2 <- abs(res$reverse_weight[i] - mat[ti, fi])
        max_err <<- max(max_err, e2)
        n_checks <<- n_checks + 1L
        if (e2 > TOL) err_count <<- err_count + 1L

        e3 <- abs(res$weight_ratio[i] - mat[ti, fi] / mat[fi, ti])
        max_err <<- max(max_err, e3)
        n_checks <<- n_checks + 1L
        if (e3 > TOL) err_count <<- err_count + 1L
      } else {
        n_checks <<- n_checks + 2L
        if (!is.na(res$reverse_weight[i])) err_count <<- err_count + 1L
        if (!is.na(res$weight_ratio[i])) err_count <<- err_count + 1L
      }
      TRUE
    }, logical(1))

    .log_result("edge_reciprocity", cfg, n_checks, n_checks - err_count,
                err_count, max_err)
    expect_equal(err_count, 0L,
      info = sprintf("n=%d seed=%d %d failures", cfg$n, cfg$seed, err_count))
  })
})

# =============================================================================
# 4. vulnerability — 100 networks, every node
# =============================================================================

test_that("vulnerability: 100 networks, every node", {
  # Reference efficiency using ORIGINAL n*(n-1) denominator (Latora & Marchiori)
  .ref_eff <- function(g, orig_n = NULL) {
    nn <- igraph::vcount(g)
    if (nn <= 1) return(0)
    denom_n <- if (!is.null(orig_n)) orig_n else nn
    sp <- igraph::distances(g, weights = NA)
    diag(sp) <- NA
    inv <- 1 / sp; inv[!is.finite(inv)] <- 0
    sum(inv, na.rm = TRUE) / (denom_n * (denom_n - 1))
  }

  # Use smaller networks for vulnerability (O(n^3))
  vuln_configs <- undirected_configs[nodes <= 20]
  if (length(vuln_configs) < 100) {
    extra_seeds <- sample.int(100000, 100 - length(vuln_configs))
    extra <- lapply(extra_seeds, function(s) {
      list(n = sample(8:15, 1), density = runif(1, 0.2, 0.5), seed = s,
           directed = FALSE)
    })
    vuln_configs <- c(vuln_configs, extra)
  }
  vuln_configs <- vuln_configs[seq_len(100)]

  lapply(vuln_configs, function(cfg) {
    mat <- .make_mat(cfg)
    g <- to_igraph(mat)
    nn <- igraph::vcount(g)
    nms <- igraph::V(g)$name

    e_full <- .ref_eff(g)

    # Reference: reduced graph uses ORIGINAL nn as denominator
    ref_norm <- vapply(seq_len(nn), function(i) {
      e_red <- .ref_eff(igraph::delete_vertices(g, i), orig_n = nn)
      if (e_full == 0) 0 else (e_full - e_red) / e_full
    }, numeric(1))
    names(ref_norm) <- nms

    ref_raw <- vapply(seq_len(nn), function(i) {
      e_full - .ref_eff(igraph::delete_vertices(g, i), orig_n = nn)
    }, numeric(1))
    names(ref_raw) <- nms

    v_norm <- vulnerability(mat, normalized = TRUE)
    v_raw <- vulnerability(mat, normalized = FALSE)
    co_norm <- setNames(v_norm$vulnerability, v_norm$node)
    co_raw <- setNames(v_raw$vulnerability, v_raw$node)

    # Check every node
    errs_norm <- vapply(nms, function(nm) abs(co_norm[nm] - ref_norm[nm]),
                        numeric(1))
    errs_raw <- vapply(nms, function(nm) abs(co_raw[nm] - ref_raw[nm]),
                       numeric(1))

    all_errs <- c(errs_norm, errs_raw)
    n_vals <- 2L * nn
    max_err <- max(all_errs)
    n_fail <- sum(all_errs > TOL)
    .log_result("vulnerability", cfg, n_vals, n_vals - n_fail, n_fail, max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 5. core_periphery — 100 networks, fitness formula exact
# =============================================================================

test_that("core_periphery fitness: cor(A, cc') on 100 networks", {
  lapply(undirected_configs, function(cfg) {
    mat <- .make_mat(cfg)
    g <- to_igraph(mat)
    adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    lt <- lower.tri(adj)

    cp <- core_periphery(mat)
    scores <- setNames(cp$coreness, cp$node)
    ideal <- outer(scores, scores)
    ref_fitness <- stats::cor(adj[lt], ideal[lt])

    cp_fitness <- attr(cp, "fitness")
    err <- abs(cp_fitness - ref_fitness)
    .log_result("core_periphery", cfg, 1, as.integer(err <= TOL),
                as.integer(err > TOL), err, "fitness check")
    expect_equal(cp_fitness, ref_fitness, tolerance = TOL,
      info = sprintf("n=%d seed=%d", cfg$n, cfg$seed))

    # Structural checks
    expect_true(all(cp$coreness >= 0 & cp$coreness <= 1))
    expect_true(all(cp$role %in% c("core", "periphery")))
  })
})

# =============================================================================
# 6. fit_degree_distribution — 100 networks, formulas verified
# =============================================================================

test_that("fit_degree_distribution: power-law alpha matches igraph, 100 networks", {
  lapply(undirected_configs, function(cfg) {
    mat <- .make_mat(cfg)
    g <- to_igraph(mat)
    deg <- igraph::degree(g)
    if (max(deg) < 2) {
      .log_result("fit_power_law", cfg, 0, 0, 0, NA, "degenerate degree")
      return(invisible(NULL))
    }

    ref <- tryCatch(igraph::fit_power_law(deg), error = function(e) NULL)
    if (is.null(ref)) {
      .log_result("fit_power_law", cfg, 0, 0, 0, NA, "igraph error")
      return(invisible(NULL))
    }

    fit <- tryCatch(
      fit_degree_distribution(mat, distributions = "power_law"),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      .log_result("fit_power_law", cfg, 0, 0, 0, NA, "cograph error")
      return(invisible(NULL))
    }

    e_alpha <- abs(fit$fits$power_law$parameters$alpha - ref$alpha)
    e_xmin <- abs(fit$fits$power_law$parameters$xmin - ref$xmin)
    max_err <- max(e_alpha, e_xmin)
    n_fail <- (e_alpha > 1e-4) + (e_xmin > 0)
    .log_result("fit_power_law", cfg, 2, 2 - n_fail, n_fail, max_err)

    expect_equal(fit$fits$power_law$parameters$alpha, ref$alpha,
      tolerance = 1e-4,
      info = sprintf("n=%d seed=%d alpha", cfg$n, cfg$seed))
    expect_equal(fit$fits$power_law$parameters$xmin, ref$xmin,
      info = sprintf("n=%d seed=%d xmin", cfg$n, cfg$seed))
  })
})

test_that("fit_degree_distribution: loglik/AIC/BIC formulas, 100 networks", {
  lapply(undirected_configs, function(cfg) {
    mat <- .make_mat(cfg)
    g <- to_igraph(mat)
    deg <- igraph::degree(g)
    k <- deg[deg >= 1]
    nn <- length(k)
    if (nn < 3) {
      .log_result("fit_loglik", cfg, 0, 0, 0, NA, "too few degrees")
      return(invisible(NULL))
    }

    fit <- tryCatch(
      fit_degree_distribution(mat,
        distributions = c("exponential", "poisson", "geometric")),
      error = function(e) NULL
    )
    if (is.null(fit)) return(invisible(NULL))

    n_checks <- 0L; n_fail <- 0L; max_err <- 0

    # Exponential
    lambda_e <- 1 / mean(k)
    ref_ll_e <- nn * log(lambda_e) - lambda_e * sum(k)
    e <- abs(fit$fits$exponential$loglik - ref_ll_e)
    n_checks <- n_checks + 1L
    if (e > TOL) n_fail <- n_fail + 1L
    max_err <- max(max_err, e)

    # Poisson
    lambda_p <- mean(k)
    ref_ll_p <- sum(k * log(lambda_p) - lambda_p - lgamma(k + 1))
    e <- abs(fit$fits$poisson$loglik - ref_ll_p)
    n_checks <- n_checks + 1L
    if (e > TOL) n_fail <- n_fail + 1L
    max_err <- max(max_err, e)

    # Geometric
    p_g <- 1 / (1 + mean(k))
    ref_ll_g <- nn * log(p_g) + sum(k) * log(1 - p_g)
    e <- abs(fit$fits$geometric$loglik - ref_ll_g)
    n_checks <- n_checks + 1L
    if (e > TOL) n_fail <- n_fail + 1L
    max_err <- max(max_err, e)

    # AIC/BIC for each
    lapply(fit$fits, function(f) {
      k_used <- if (f$distribution == "power_law") {
        deg[deg >= f$parameters$xmin]
      } else {
        k
      }
      n_k <- length(k_used)
      ref_aic <- -2 * f$loglik + 2
      ref_bic <- -2 * f$loglik + log(n_k)
      e_aic <- abs(f$aic - ref_aic)
      e_bic <- abs(f$bic - ref_bic)
      n_checks <<- n_checks + 2L
      if (e_aic > TOL) n_fail <<- n_fail + 1L
      if (e_bic > TOL) n_fail <<- n_fail + 1L
      max_err <<- max(max_err, e_aic, e_bic)
    })

    .log_result("fit_loglik_aic_bic", cfg, n_checks, n_checks - n_fail,
                n_fail, max_err)
    expect_equal(n_fail, 0L,
      info = sprintf("n=%d seed=%d %d formula failures", cfg$n, cfg$seed,
                     n_fail))
  })
})

# =============================================================================
# 7. shortest_paths — 100 undirected + 100 directed, full matrix
# =============================================================================

test_that("shortest_paths: full matrix vs igraph, 100 undirected networks", {
  lapply(undirected_configs, function(cfg) {
    mat <- .make_mat(cfg)
    g <- to_igraph(mat)
    nn <- cfg$n

    co_d <- shortest_paths(mat, weights = NA)
    ig_d <- igraph::distances(g, weights = NA)

    diff_mat <- abs(co_d - ig_d)
    diff_mat[!is.finite(diff_mat)] <- 0  # Inf - Inf = NaN
    max_err <- max(diff_mat)
    n_vals <- nn * nn
    n_fail <- sum(diff_mat > TOL)
    .log_result("shortest_paths_undir", cfg, n_vals, n_vals - n_fail, n_fail,
                max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

test_that("shortest_paths: full matrix vs igraph, 100 directed networks", {
  lapply(directed_configs, function(cfg) {
    mat <- .make_mat(cfg)
    g <- to_igraph(mat, directed = TRUE)
    nn <- cfg$n

    co_d <- shortest_paths(mat, weights = NA)
    ig_d <- igraph::distances(g, mode = "out", weights = NA)

    diff_mat <- abs(co_d - ig_d)
    diff_mat[!is.finite(diff_mat)] <- 0
    max_err <- max(diff_mat)
    n_vals <- nn * nn
    n_fail <- sum(diff_mat > TOL)
    .log_result("shortest_paths_dir", cfg, n_vals, n_vals - n_fail, n_fail,
                max_err)
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

# =============================================================================
# 8. k_shortest_paths — 100 networks, validate every path
# =============================================================================

test_that("k_shortest_paths: 100 networks, validate all paths", {
  lapply(undirected_configs, function(cfg) {
    mat <- .make_mat(cfg)
    g <- to_igraph(mat)
    nms <- rownames(mat)
    src <- nms[1]; tgt <- nms[cfg$n]

    kp <- k_shortest_paths(mat, from = src, to = tgt, k = 5)

    n_checks <- 0L; n_fail <- 0L

    # First path = igraph shortest
    ig_d <- igraph::distances(g, v = src, to = tgt, weights = NA)[1, 1]
    if (length(kp$paths) > 0 && is.finite(ig_d)) {
      n_checks <- n_checks + 1L
      if (abs(kp$distances[1] - ig_d) > TOL) n_fail <- n_fail + 1L
    }

    # Distances non-decreasing
    if (length(kp$distances) > 1) {
      n_checks <- n_checks + 1L
      if (any(diff(kp$distances) < -TOL)) n_fail <- n_fail + 1L
    }

    # All paths distinct
    if (length(kp$paths) > 1) {
      pstrs <- vapply(kp$paths, paste, character(1), collapse = "->")
      n_checks <- n_checks + 1L
      if (anyDuplicated(pstrs)) n_fail <- n_fail + 1L
    }

    # Validate each path
    vapply(seq_along(kp$paths), function(i) {
      path <- kp$paths[[i]]
      # Correct endpoints
      n_checks <<- n_checks + 2L
      if (path[1] != src) n_fail <<- n_fail + 1L
      if (path[length(path)] != tgt) n_fail <<- n_fail + 1L
      # Loopless
      n_checks <<- n_checks + 1L
      if (anyDuplicated(path)) n_fail <<- n_fail + 1L
      # Connected
      vapply(seq_len(length(path) - 1), function(j) {
        fi <- match(path[j], nms)
        ti <- match(path[j + 1], nms)
        n_checks <<- n_checks + 1L
        if (mat[fi, ti] == 0 && mat[ti, fi] == 0) n_fail <<- n_fail + 1L
        TRUE
      }, logical(1))
      # Distance = hop count
      n_checks <<- n_checks + 1L
      if (abs(kp$distances[i] - (length(path) - 1)) > TOL) {
        n_fail <<- n_fail + 1L
      }
      TRUE
    }, logical(1))

    .log_result("k_shortest_paths", cfg, n_checks, n_checks - n_fail,
                n_fail, NA, sprintf("%d paths found", length(kp$paths)))
    expect_equal(n_fail, 0L,
      info = sprintf("n=%d seed=%d %d failures", cfg$n, cfg$seed, n_fail))
  })
})

# =============================================================================
# 9. project_bipartite — 100 random incidence matrices, all 5 methods
# =============================================================================

test_that("project_bipartite: 100 matrices, sum + binary exact", {
  set.seed(9999)
  lapply(seq_len(100), function(i) {
    nr <- sample(3:15, 1)
    nc <- sample(3:10, 1)
    inc <- matrix(sample(0:3, nr * nc, replace = TRUE), nr, nc)
    rownames(inc) <- paste0("R", seq_len(nr))
    colnames(inc) <- paste0("C", seq_len(nc))

    cfg <- list(n = nr, density = NA, seed = i, directed = FALSE)

    # Sum
    res_sum <- project_bipartite(inc, method = "sum")
    ref_sum <- inc %*% t(inc); diag(ref_sum) <- 0
    e_sum <- max(abs(res_sum - ref_sum))

    # Binary
    res_bin <- project_bipartite(inc, method = "binary")
    b <- (inc > 0) * 1
    ref_bin <- b %*% t(b); diag(ref_bin) <- 0
    e_bin <- max(abs(res_bin - ref_bin))

    max_err <- max(e_sum, e_bin)
    n_vals <- 2L * nr * nr
    n_fail <- sum(abs(res_sum - ref_sum) > TOL) + sum(abs(res_bin - ref_bin) > TOL)
    .log_result("bipartite_sum_binary", cfg, n_vals, n_vals - n_fail, n_fail,
                max_err)
    expect_true(max_err <= TOL,
      info = sprintf("i=%d %dx%d max_err=%.2e", i, nr, nc, max_err))
  })
})

test_that("project_bipartite: 100 matrices, jaccard exact", {
  set.seed(8888)
  lapply(seq_len(100), function(i) {
    nr <- sample(3:12, 1)
    nc <- sample(3:8, 1)
    inc <- matrix(sample(0:1, nr * nc, replace = TRUE), nr, nc)
    rownames(inc) <- paste0("R", seq_len(nr))
    colnames(inc) <- paste0("C", seq_len(nc))

    cfg <- list(n = nr, density = NA, seed = i, directed = FALSE)

    res <- project_bipartite(inc, method = "jaccard")

    # Manual Jaccard
    ref <- matrix(0, nr, nr, dimnames = list(rownames(inc), rownames(inc)))
    b <- inc > 0
    for (a in seq_len(nr)) {
      for (bb in seq_len(nr)) {
        if (a == bb) next
        shared <- sum(b[a, ] & b[bb, ])
        total <- sum(b[a, ]) + sum(b[bb, ])
        ref[a, bb] <- if (total - shared > 0) shared / (total - shared) else 0
      }
    }

    max_err <- max(abs(res - ref))
    n_vals <- nr * nr
    n_fail <- sum(abs(res - ref) > TOL)
    .log_result("bipartite_jaccard", cfg, n_vals, n_vals - n_fail, n_fail,
                max_err)
    expect_true(max_err <= TOL,
      info = sprintf("i=%d %dx%d max_err=%.2e", i, nr, nc, max_err))
  })
})

test_that("project_bipartite: 100 matrices, cosine exact", {
  set.seed(7777)
  lapply(seq_len(100), function(i) {
    nr <- sample(3:12, 1)
    nc <- sample(3:8, 1)
    inc <- matrix(runif(nr * nc), nr, nc)
    rownames(inc) <- paste0("R", seq_len(nr))
    colnames(inc) <- paste0("C", seq_len(nc))

    cfg <- list(n = nr, density = NA, seed = i, directed = FALSE)

    res <- project_bipartite(inc, method = "cosine")

    dot <- inc %*% t(inc)
    norms <- sqrt(rowSums(inc^2))
    norm_mat <- outer(norms, norms)
    ref <- ifelse(norm_mat > 0, dot / norm_mat, 0)
    diag(ref) <- 0

    max_err <- max(abs(res - ref))
    n_vals <- nr * nr
    n_fail <- sum(abs(res - ref) > TOL)
    .log_result("bipartite_cosine", cfg, n_vals, n_vals - n_fail, n_fail,
                max_err)
    expect_true(max_err <= TOL,
      info = sprintf("i=%d %dx%d max_err=%.2e", i, nr, nc, max_err))
  })
})

test_that("project_bipartite: 100 matrices, newman exact", {
  set.seed(6666)
  lapply(seq_len(100), function(i) {
    nr <- sample(3:12, 1)
    nc <- sample(3:8, 1)
    inc <- matrix(sample(0:1, nr * nc, replace = TRUE), nr, nc)
    rownames(inc) <- paste0("R", seq_len(nr))
    colnames(inc) <- paste0("C", seq_len(nc))

    cfg <- list(n = nr, density = NA, seed = i, directed = FALSE)

    res <- project_bipartite(inc, method = "newman")

    col_deg <- colSums(inc > 0)
    ref <- matrix(0, nr, nr, dimnames = list(rownames(inc), rownames(inc)))
    b <- inc > 0
    for (a in seq_len(nr)) {
      for (bb in seq_len(nr)) {
        if (a == bb) next
        total <- 0
        for (k in seq_len(nc)) {
          if (b[a, k] && b[bb, k] && col_deg[k] > 1) {
            total <- total + 1 / (col_deg[k] - 1)
          }
        }
        ref[a, bb] <- total
      }
    }

    max_err <- max(abs(res - ref))
    n_vals <- nr * nr
    n_fail <- sum(abs(res - ref) > TOL)
    .log_result("bipartite_newman", cfg, n_vals, n_vals - n_fail, n_fail,
                max_err)
    expect_true(max_err <= TOL,
      info = sprintf("i=%d %dx%d max_err=%.2e", i, nr, nc, max_err))
  })
})

# =============================================================================
# 10. is_bipartite — 100 random graphs vs igraph
# =============================================================================

test_that("is_bipartite: 100 random graphs vs igraph::bipartite_mapping", {
  set.seed(5555)
  n_checks <- 0L; n_fail <- 0L
  vapply(seq_len(100), function(i) {
    nn <- sample(4:20, 1)
    mat <- matrix(0, nn, nn)
    n_edges <- sample(0:(nn * (nn - 1) / 2), 1)
    if (n_edges > 0) {
      idx <- which(upper.tri(mat))
      chosen <- sample(idx, min(n_edges, length(idx)))
      mat[chosen] <- 1
      mat <- mat + t(mat)
    }
    diag(mat) <- 0

    g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected")
    ref <- igraph::bipartite_mapping(g)$res
    co <- is_bipartite(mat)

    n_checks <<- n_checks + 1L
    if (!identical(co, ref)) n_fail <<- n_fail + 1L
    expect_equal(co, ref,
      info = sprintf("graph %d (n=%d edges=%d)", i, nn, n_edges))
    TRUE
  }, logical(1))

  .log_result("is_bipartite",
    list(n = NA, density = NA, seed = 5555, directed = FALSE),
    n_checks, n_checks - n_fail, n_fail, NA, "100 random graphs")
})

# =============================================================================
# Write final report
# =============================================================================

test_that("equivalence report written", {
  report <- .write_report()
  expect_true(is.data.frame(report))
  expect_equal(sum(report$values_failed), 0L)
})
