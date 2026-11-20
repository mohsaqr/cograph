# Equivalence tests moved from tests/testthat/test-centrality-extended.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
skip_if_not_installed("igraph")
skip_coverage_tests()
path4 <- matrix(c(
  0, 1, 0, 0,
  1, 0, 1, 0,
  0, 1, 0, 1,
  0, 0, 1, 0
), 4, 4)
rownames(path4) <- colnames(path4) <- c("A", "B", "C", "D")
k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(k3) <- colnames(k3) <- c("A", "B", "C")
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1
star5[2:5, 1] <- 1
rownames(star5) <- colnames(star5) <- c("A", "B", "C", "D", "E")
dir3 <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3)
rownames(dir3) <- colnames(dir3) <- c("A", "B", "C")
comm6 <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), 6, 6)
rownames(comm6) <- colnames(comm6) <- LETTERS[1:6]
comm_membership <- c(1, 1, 1, 2, 2, 2)
stress_from_all_paths <- function(g, weights = NULL, directed = TRUE) {
  n <- igraph::vcount(g)
  is_dir <- igraph::is_directed(g) && directed
  mode <- if (is_dir) "out" else "all"
  stress <- numeric(n)
  w_use <- if (is.null(weights)) NA else weights
  pairs <- if (is_dir) {
    expand.grid(s = seq_len(n), t = seq_len(n))
    } else {
      do.call(rbind, lapply(seq_len(n - 1), function(s) {
        data.frame(s = s, t = seq.int(s + 1L, n))
      }))
    }
  pairs <- pairs[pairs$s != pairs$t, , drop = FALSE]

  for (row in seq_len(nrow(pairs))) {
    s <- pairs$s[row]; t <- pairs$t[row]
    paths <- suppressWarnings(
      igraph::all_shortest_paths(g, from = s, to = t,
                                 mode = mode, weights = w_use)$vpaths
    )
    for (p in paths) {
      p_int <- as.integer(p)
      if (length(p_int) <= 2L) next
      interior <- p_int[-c(1L, length(p_int))]
      stress[interior] <- stress[interior] + 1
    }
  }
  stress
}
.nx_mat <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  0, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(.nx_mat) <- colnames(.nx_mat) <- LETTERS[1:5]

# ---- equivalence tests ----

test_that("extended measures match centiserve on random graphs", {
  skip_if_not_installed("centiserve")

  set.seed(42)
  n_tests <- 20
  failures <- 0L

  for (i in seq_len(n_tests)) {
    g <- igraph::sample_gnp(8, 0.4)
    while (!igraph::is_connected(g)) {
      g <- igraph::sample_gnp(8, 0.4)
    }

    # Radiality
    co_rad <- cograph:::calculate_radiality(g, mode = "all", weights = NULL)
    cs_rad <- centiserve::radiality(g)
    if (!isTRUE(all.equal(co_rad, cs_rad, tolerance = 1e-8))) {
      failures <- failures + 1L
    }

    # Lobby index (centiserve returns double, we return integer)
    co_lob <- cograph:::calculate_lobby(g, mode = "all")
    cs_lob <- centiserve::lobby(g)
    if (!isTRUE(all.equal(as.numeric(co_lob), as.numeric(cs_lob)))) {
      failures <- failures + 1L
    }

    # Barycenter
    co_bar <- cograph:::calculate_barycenter(g, mode = "all", weights = NULL)
    cs_bar <- centiserve::barycenter(g)
    if (!isTRUE(all.equal(co_bar, cs_bar, tolerance = 1e-8))) {
      failures <- failures + 1L
    }

    # Bottleneck
    co_bn <- cograph:::calculate_bottleneck(g, mode = "all")
    cs_bn <- centiserve::bottleneck(g)
    if (!isTRUE(all.equal(as.numeric(co_bn), as.numeric(cs_bn)))) {
      failures <- failures + 1L
    }

    # Centroid — SKIP: centiserve::centroid() has a known bug where the
    # self-exclusion check uses stale loop variable `u` instead of `w`,
    # causing incorrect results on some graphs. Our implementation is
    # verified by hand on known topologies above.

    # MNC
    co_mnc <- cograph:::calculate_mnc(g, mode = "all")
    cs_mnc <- centiserve::mnc(g)
    if (!isTRUE(all.equal(as.numeric(co_mnc), as.numeric(cs_mnc)))) {
      failures <- failures + 1L
    }

    # Average distance
    co_ad <- cograph:::calculate_average_distance(g, mode = "all",
                                                   weights = NULL)
    cs_ad <- centiserve::averagedis(g)
    if (!isTRUE(all.equal(co_ad, cs_ad, tolerance = 1e-8))) {
      failures <- failures + 1L
    }

    # Closeness vitality (centiserve errors on some graphs)
    cs_cv <- tryCatch(centiserve::closeness.vitality(g), error = function(e) NULL)
    if (!is.null(cs_cv)) {
      co_cv <- cograph:::calculate_closeness_vitality(g, mode = "all",
                                                       weights = NULL)
      if (!isTRUE(all.equal(co_cv, cs_cv, tolerance = 1e-8))) {
        failures <- failures + 1L
      }
    }

    # Cross-clique
    co_cc <- cograph:::calculate_cross_clique(g)
    cs_cc <- centiserve::crossclique(g)
    if (!isTRUE(all.equal(as.numeric(co_cc), as.numeric(cs_cc)))) {
      failures <- failures + 1L
    }
  }

  cat(sprintf("centiserve equivalence: %d tests, %d failures\n",
              n_tests * 8, failures))
  expect_equal(failures, 0L)
})

test_that("stress matches sna on random graphs", {
  skip_if_not_installed("sna")

  set.seed(123)
  failures <- 0L

  for (i in seq_len(20)) {
    g <- igraph::sample_gnp(8, 0.4)
    while (!igraph::is_connected(g)) {
      g <- igraph::sample_gnp(8, 0.4)
    }

    co_stress <- cograph:::calculate_stress(g, weights = NULL, directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    sna_stress <- sna::stresscent(mat, gmode = "graph")

    if (!isTRUE(all.equal(co_stress, sna_stress, tolerance = 1e-8))) {
      failures <- failures + 1L
    }
  }

  cat(sprintf("sna stress equivalence: 20 tests, %d failures\n", failures))
  expect_equal(failures, 0L)
})

test_that("gilschmidt matches sna on random graphs", {
  skip_if_not_installed("sna")

  set.seed(456)
  failures <- 0L

  for (i in seq_len(20)) {
    g <- igraph::sample_gnp(8, 0.4)
    while (!igraph::is_connected(g)) {
      g <- igraph::sample_gnp(8, 0.4)
    }

    co_gs <- cograph:::calculate_gilschmidt(g, mode = "all")
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    sna_gs <- sna::gilschmidt(mat, gmode = "graph")

    if (!isTRUE(all.equal(co_gs, sna_gs, tolerance = 1e-8))) {
      failures <- failures + 1L
    }
  }

  cat(sprintf("sna gilschmidt equivalence: 20 tests, %d failures\n", failures))
  expect_equal(failures, 0L)
})

test_that("effective_size matches influenceR on random graphs", {
  skip_if_not_installed("influenceR")

  set.seed(321)
  failures <- 0L

  for (i in seq_len(20)) {
    g <- igraph::sample_gnp(10, 0.35)
    while (!igraph::is_connected(g)) {
      g <- igraph::sample_gnp(10, 0.35)
    }

    co_es <- cograph:::calculate_effective_size(g)
    ir_es <- influenceR::ens(g)

    if (!isTRUE(all.equal(co_es, ir_es, tolerance = 1e-8))) {
      failures <- failures + 1L
    }
  }

  cat(sprintf("influenceR effective_size equivalence: 20 tests, %d failures\n",
              failures))
  expect_equal(failures, 0L)
})

test_that("current_flow_betweenness matches NetworkX", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  nx_cfb <- nx$current_flow_betweenness_centrality(G)
  nx_vec <- vapply(LETTERS[1:5], function(x) nx_cfb[[x]], numeric(1))

  expect_equal(
    unname(centrality_current_flow_betweenness(.nx_mat)),
    unname(nx_vec),
    tolerance = 1e-5
  )
})

test_that("percolation matches NetworkX", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  states <- reticulate::py_dict(LETTERS[1:5], rep(1.0, 5))
  nx_perc <- nx$percolation_centrality(G, states = states)
  nx_vec <- vapply(LETTERS[1:5], function(x) nx_perc[[x]], numeric(1))

  expect_equal(
    unname(centrality_percolation(.nx_mat)),
    unname(nx_vec),
    tolerance = 1e-6
  )
})

test_that("laplacian matches NetworkX", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  nx_lap <- nx$laplacian_centrality(G, normalized = FALSE)
  nx_vec <- vapply(LETTERS[1:5], function(x) nx_lap[[x]], numeric(1))

  expect_equal(
    unname(centrality_laplacian(.nx_mat)),
    unname(nx_vec),
    tolerance = 1e-6
  )
})

test_that("voterank matches NetworkX ordering", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  nx_vr <- unlist(nx$voterank(G))
  cg_vr <- centrality_voterank(.nx_mat)
  cg_order <- names(sort(cg_vr, decreasing = TRUE))

  # Top spreaders should match in order
  expect_equal(cg_order[seq_along(nx_vr)], nx_vr)
})

test_that("local efficiency matches igraph", {
  set.seed(42)
  g <- igraph::sample_gnp(20, 0.3)
  while (!igraph::is_connected(g)) g <- igraph::sample_gnp(20, 0.3)

  expect_equal(
    cograph::network_local_efficiency(g),
    igraph::average_local_efficiency(g),
    tolerance = 1e-10
  )
})

test_that("local efficiency matches igraph (weighted)", {
  set.seed(42)
  g <- igraph::sample_gnp(20, 0.3)
  while (!igraph::is_connected(g)) g <- igraph::sample_gnp(20, 0.3)
  igraph::E(g)$weight <- runif(igraph::ecount(g), 0.1, 1.0)

  expect_equal(
    cograph::network_local_efficiency(g, invert_weights = FALSE),
    igraph::average_local_efficiency(g),
    tolerance = 1e-10
  )
})
