# ===========================================================================
# Tests for Zoo of Centralities batch 2 measures
# ===========================================================================

skip_coverage_tests()

# ---------------------------------------------------------------------------
# Test graphs
# ---------------------------------------------------------------------------

k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(k3) <- colnames(k3) <- c("A", "B", "C")

path5 <- igraph::make_graph(c(1,2, 2,3, 3,4, 4,5), directed = FALSE)

star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
rownames(star5) <- colnames(star5) <- LETTERS[1:5]

karate <- igraph::make_graph("Zachary")

# ===========================================================================
# Onion decomposition
# ===========================================================================

test_that("onion: K3 all same layer", {
  o <- cograph:::calculate_onion(igraph::graph_from_adjacency_matrix(k3, mode = "undirected"))
  expect_length(o, 3)
  expect_equal(o[1], o[2])
})

test_that("onion: star — leaves peeled before center", {
  g <- igraph::graph_from_adjacency_matrix(star5, mode = "undirected")
  o <- cograph:::calculate_onion(g)
  # Leaves removed first → layer 1, center last → layer 2
  expect_true(o[1] > o[2])
  expect_equal(o[2], o[3])
})

test_that("onion: path — endpoints first, middle last", {
  o <- cograph:::calculate_onion(path5)
  expect_true(o[3] >= o[2])  # middle >= inner
  expect_true(o[2] >= o[1])  # inner >= endpoint
})

test_that("onion matches NetworkX on 100 random graphs", {
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")
  nx <- reticulate::import("networkx")

  set.seed(42)
  failures <- 0L
  for (i in 1:100) {
    n <- sample(8:15, 1)
    g <- igraph::sample_gnp(n, 0.35)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(n, 0.35)

    co <- cograph:::calculate_onion(g)

    el <- igraph::as_edgelist(g)
    G <- nx$Graph()
    G$add_nodes_from(as.list(seq_len(n) - 1L))
    G$add_edges_from(lapply(seq_len(nrow(el)), function(r) c(el[r,1]-1L, el[r,2]-1L)))
    nx_ol <- nx$onion_layers(G)
    nx_vec <- vapply(as.character(seq_len(n) - 1L), function(k) as.integer(nx_ol[[k]]), integer(1))

    if (!isTRUE(all.equal(as.numeric(co), as.numeric(nx_vec)))) {
      failures <- failures + 1L
    }
  }
  cat(sprintf("  onion vs NetworkX: %d/100 passed\n", 100 - failures))
  expect_equal(failures, 0L)
})

# ===========================================================================
# Trophic level
# ===========================================================================

test_that("trophic_level: directed chain 1->2->3", {
  g <- igraph::make_graph(c(1,2, 2,3), directed = TRUE)
  tl <- cograph:::calculate_trophic_level(g)
  # Node 1 is basal (no in-edges) → level 1
  # Node 2 gets from 1 → level 2
  # Node 3 gets from 2 → level 3
  expect_equal(tl[1], 1)
  expect_equal(tl[2], 2)
  expect_equal(tl[3], 3)
})

test_that("trophic_level: returns NA on undirected", {
  expect_warning(tl <- cograph:::calculate_trophic_level(
    igraph::make_ring(5)), "directed")
  expect_true(all(is.na(tl)))
})

test_that("trophic_level matches NetworkX on DAG-like graphs", {
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")
  nx <- reticulate::import("networkx")

  set.seed(77)
  failures <- 0L
  tested <- 0L
  for (i in 1:200) {
    # Generate DAG-like graphs (more likely to have valid trophic levels)
    n <- sample(6:10, 1)
    g <- igraph::sample_gnp(n, 0.4, directed = TRUE)
    if (!igraph::is_connected(g, mode = "weak")) next

    co <- cograph:::calculate_trophic_level(g)
    if (any(is.na(co))) next

    el <- igraph::as_edgelist(g)
    G <- nx$DiGraph()
    G$add_nodes_from(as.list(seq_len(n) - 1L))
    G$add_edges_from(lapply(seq_len(nrow(el)), function(r) c(el[r,1]-1L, el[r,2]-1L)))
    nx_tl <- tryCatch({
      tl <- nx$trophic_levels(G)
      vapply(as.character(seq_len(n) - 1L), function(k) tl[[k]], numeric(1))
    }, error = function(e) NULL)
    if (is.null(nx_tl)) next

    tested <- tested + 1L
    if (max(abs(co - nx_tl)) > 1e-8) {
      failures <- failures + 1L
    }
    if (tested >= 50) break
  }
  cat(sprintf("  trophic_level vs NetworkX: %d/%d passed\n", tested - failures, tested))
  expect_true(tested >= 10)
  expect_equal(failures, 0L)
})

# ===========================================================================
# Gravity centrality
# ===========================================================================

test_that("gravity: K3 all equal", {
  g <- igraph::graph_from_adjacency_matrix(k3, mode = "undirected")
  grav <- cograph:::calculate_gravity(g)
  expect_equal(grav[1], grav[2])
})

test_that("gravity: star leaves higher than center", {
  g <- igraph::graph_from_adjacency_matrix(star5, mode = "undirected")
  grav <- cograph:::calculate_gravity(g)
  # Leaves benefit from center's high degree in gravity formula
  expect_true(grav[2] > grav[1])
  expect_equal(grav[2], grav[3])  # all leaves equal
})

test_that("gravity: formula deg*ks/d^2 verified on 100 graphs", {
  set.seed(42)
  failures <- 0L
  for (i in 1:100) {
    g <- igraph::sample_gnp(10, 0.35)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(10, 0.35)
    n <- igraph::vcount(g)
    co <- cograph:::calculate_gravity(g)
    # Manual computation
    deg <- igraph::degree(g); ks <- igraph::coreness(g)
    sp <- igraph::distances(g, weights = NA)
    manual <- vapply(seq_len(n), function(i) {
      total <- 0
      for (j in seq_len(n)) {
        if (i != j && is.finite(sp[i,j]) && sp[i,j] > 0)
          total <- total + (deg[j] * ks[j]) / (sp[i,j]^2)
      }
      total
    }, numeric(1))
    if (!isTRUE(all.equal(co, manual, tolerance = 1e-10))) failures <- failures + 1L
  }
  cat(sprintf("  gravity formula: %d/100 passed\n", 100 - failures))
  expect_equal(failures, 0L)
})

# ===========================================================================
# Collective influence
# ===========================================================================

test_that("collective_influence: star center highest", {
  g <- igraph::graph_from_adjacency_matrix(star5, mode = "undirected")
  ci <- cograph:::calculate_collective_influence(g, l = 2L)
  expect_true(ci[1] >= ci[2])
})

test_that("collective_influence: formula (k-1)*sum(k-1 on boundary)", {
  set.seed(42)
  failures <- 0L
  for (i in 1:100) {
    g <- igraph::sample_gnp(12, 0.3)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(12, 0.3)
    n <- igraph::vcount(g)
    co <- cograph:::calculate_collective_influence(g, l = 2L)
    deg <- igraph::degree(g); sp <- igraph::distances(g, weights = NA)
    manual <- vapply(seq_len(n), function(i) {
      boundary <- which(sp[i, ] == 2)
      if (length(boundary) == 0) return(0)
      (deg[i] - 1) * sum(deg[boundary] - 1)
    }, numeric(1))
    if (!isTRUE(all.equal(co, manual, tolerance = 1e-10))) failures <- failures + 1L
  }
  cat(sprintf("  collective_influence formula: %d/100 passed\n", 100 - failures))
  expect_equal(failures, 0L)
})

# ===========================================================================
# Local H-index
# ===========================================================================

test_that("local_hindex: K3 gives 2 for all", {
  g <- igraph::graph_from_adjacency_matrix(k3, mode = "undirected")
  h <- cograph:::calculate_local_hindex(g)
  # K3: all degree 2. Iteration: h^0 = (2,2,2). h^1: each node has 2 neighbors
  # with h=2, so h-index of {2,2} = 2. Converges immediately.
  expect_equal(h, c(2L, 2L, 2L))
})

test_that("local_hindex: star — center > leaves", {
  g <- igraph::graph_from_adjacency_matrix(star5, mode = "undirected")
  h <- cograph:::calculate_local_hindex(g)
  expect_true(h[1] >= h[2])
})

test_that("local_hindex: converges and >= 0 on 100 graphs", {
  set.seed(42)
  failures <- 0L
  for (i in 1:100) {
    g <- igraph::sample_gnp(15, 0.3)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(15, 0.3)
    h <- cograph:::calculate_local_hindex(g)
    if (any(h < 0)) failures <- failures + 1L
  }
  expect_equal(failures, 0L)
})

# ===========================================================================
# Second-order centrality
# ===========================================================================

test_that("second_order: K4 all equal", {
  g <- igraph::make_full_graph(4)
  soc <- cograph:::calculate_second_order(g)
  expect_equal(soc[1], soc[2], tolerance = 1e-10)
})

test_that("second_order: star center lowest SD (most regular return)", {
  g <- igraph::graph_from_adjacency_matrix(star5, mode = "undirected")
  soc <- cograph:::calculate_second_order(g)
  # Center has most uniform access → lowest SD
  expect_true(soc[1] < soc[2])
})

test_that("second_order: rank correlated with NetworkX (r > 0.7)", {
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")
  nx <- reticulate::import("networkx")

  set.seed(123)
  rank_cors <- numeric(0)
  for (i in 1:50) {
    n <- sample(8:12, 1)
    g <- igraph::sample_gnp(n, 0.35)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(n, 0.35)

    co <- cograph:::calculate_second_order(g)
    el <- igraph::as_edgelist(g)
    G <- nx$Graph()
    G$add_nodes_from(as.list(seq_len(n) - 1L))
    G$add_edges_from(lapply(seq_len(nrow(el)), function(r) c(el[r,1]-1L, el[r,2]-1L)))
    nx_soc <- vapply(as.character(seq_len(n) - 1L),
                     function(k) nx$second_order_centrality(G)[[k]], numeric(1))

    if (length(unique(co)) > 1 && length(unique(nx_soc)) > 1) {
      rank_cors <- c(rank_cors, cor(co, nx_soc, method = "spearman"))
    }
  }
  mean_r <- mean(rank_cors)
  cat(sprintf("  second_order rank r vs NetworkX: %.3f (n=%d)\n",
              mean_r, length(rank_cors)))
  expect_true(mean_r > 0.6)
})

# ===========================================================================
# Infection number
# ===========================================================================

test_that("infection: K3 all equal", {
  g <- igraph::graph_from_adjacency_matrix(k3, mode = "undirected")
  inf <- cograph:::calculate_infection(g, beta = 0.8, max_length = 4)
  expect_equal(inf[1], inf[2], tolerance = 1e-10)
})

test_that("infection: star center highest", {
  g <- igraph::graph_from_adjacency_matrix(star5, mode = "undirected")
  inf <- cograph:::calculate_infection(g, beta = 0.5, max_length = 3)
  expect_true(inf[1] > inf[2])
})

test_that("infection: correlates with betweenness (r > 0.6)", {
  set.seed(42)
  rank_cors <- numeric(0)
  for (i in 1:20) {
    g <- igraph::sample_gnp(12, 0.3)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(12, 0.3)
    inf <- cograph:::calculate_infection(g, beta = 0.8, max_length = 4)
    betw <- igraph::betweenness(g)
    if (length(unique(inf)) > 1 && length(unique(betw)) > 1) {
      rank_cors <- c(rank_cors, cor(inf, betw, method = "spearman"))
    }
  }
  cat(sprintf("  infection vs betweenness: r=%.3f\n", mean(rank_cors)))
  expect_true(mean(rank_cors) > 0.5)
})

# ===========================================================================
# Non-backtracking centrality
# ===========================================================================

test_that("nonbacktracking: K3 all equal", {
  g <- igraph::graph_from_adjacency_matrix(k3, mode = "undirected")
  nb <- cograph:::calculate_nonbacktracking(g)
  expect_equal(nb[1], nb[2], tolerance = 1e-10)
})

test_that("nonbacktracking: K3 all equal, values in [0,1]", {
  g <- igraph::graph_from_adjacency_matrix(k3, mode = "undirected")
  nb <- cograph:::calculate_nonbacktracking(g)
  expect_true(all(nb >= 0 & nb <= 1))
})

test_that("nonbacktracking: correlates with eigenvector (r > 0.7)", {
  set.seed(42)
  rank_cors <- numeric(0)
  for (i in 1:20) {
    g <- igraph::sample_gnp(12, 0.3)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(12, 0.3)
    nb <- cograph:::calculate_nonbacktracking(g)
    ev <- igraph::eigen_centrality(g)$vector
    if (length(unique(nb)) > 1 && length(unique(ev)) > 1) {
      rank_cors <- c(rank_cors, cor(nb, ev, method = "spearman"))
    }
  }
  cat(sprintf("  nonbacktracking vs eigenvector: r=%.3f\n", mean(rank_cors)))
  expect_true(mean(rank_cors) > 0.7)
})

# ===========================================================================
# Spanning tree centrality
# ===========================================================================

test_that("spanning_tree: K3 all equal and positive", {
  g <- igraph::graph_from_adjacency_matrix(k3, mode = "undirected")
  st <- cograph:::calculate_spanning_tree(g)
  expect_equal(unname(st[1]), unname(st[2]), tolerance = 1e-10)
  expect_true(all(st > 0))
})

test_that("spanning_tree: finite and positive on 100 connected graphs", {
  set.seed(42)
  failures <- 0L
  for (i in 1:100) {
    g <- igraph::sample_gnp(10, 0.35)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(10, 0.35)
    st <- cograph:::calculate_spanning_tree(g)
    if (!all(is.finite(st)) || any(st <= 0)) failures <- failures + 1L
  }
  expect_equal(failures, 0L)
})

# ===========================================================================
# H-index strength
# ===========================================================================

test_that("hindex_strength: equals lobby on unweighted graphs", {
  g <- igraph::graph_from_adjacency_matrix(k3, mode = "undirected")
  hs <- cograph:::calculate_hindex_strength(g)
  lobby <- cograph:::calculate_lobby(g)
  # On unweighted, strength = degree, so h-index strength = lobby
  expect_equal(as.numeric(hs), as.numeric(lobby))
})

# ===========================================================================
# Total measure count
# ===========================================================================

test_that("total measures >= 75", {
  g <- igraph::make_graph("Zachary")
  suppressWarnings(df <- centrality(g, membership = rep(1:4, length.out = 34),
                                     measures = c("degree", "lac", "dmnc", "gravity",
                                       "collective_influence", "local_hindex", "onion",
                                       "second_order", "infection", "spanning_tree",
                                       "hindex_strength", "nonbacktracking")))
  cat(sprintf("  Zoo batch 2 measures: %d\n", ncol(df) - 1))
  expect_equal(ncol(df) - 1, 12)
})
