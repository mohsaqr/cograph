# ===========================================================================
# Port of Centrality Zoo batches 7-11 off igraph
# ===========================================================================
# Two new kernels (.cg_knn, .cg_loop_coreness) are pinned against igraph on
# a graph zoo that includes self-loops, the calculators of the five batches
# are run under options(cograph.forbid_igraph = TRUE) so any remaining
# bridge to igraph errors with class cograph_igraph_leak, and the three
# calculators whose igraph algorithms were replaced (entropy variation on
# betweenness, NCVoteRank, the gravity masses) are checked against the
# igraph-built reference they used to call.

.port_zoo_matrix <- function(n, density, directed, weighted, loops, seed) {
  set.seed(seed)
  m <- matrix(0, n, n)
  if (n > 1L && density > 0) {
    idx <- if (loops) seq_along(m) else which(row(m) != col(m))
    take <- idx[stats::runif(length(idx)) < density]
    m[take] <- if (weighted) stats::runif(length(take), 0.1, 1) else 1
    if (!directed) m[lower.tri(m)] <- t(m)[lower.tri(m)]
  }
  m
}

.port_zoo_grid <- function(weighted = c(FALSE, TRUE)) {
  expand.grid(
    n = c(1L, 2L, 3L, 5L, 8L, 12L, 20L),
    density = c(0, 0.1, 0.35, 0.7, 1),
    directed = c(FALSE, TRUE),
    weighted = weighted,
    loops = c(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# .cg_knn
# ---------------------------------------------------------------------------

test_that("graph zoo: .cg_knn matches igraph::knn, loops and weights included", {
  skip_if_not_installed("igraph")
  grid <- .port_zoo_grid()
  mismatches <- unlist(lapply(seq_len(nrow(grid)), function(i) {
    cfg <- grid[i, ]
    m <- .port_zoo_matrix(cfg$n, cfg$density, cfg$directed, cfg$weighted,
                          cfg$loops, seed = 2000L + i)
    g <- igraph::graph_from_adjacency_matrix(
      m, mode = if (cfg$directed) "directed" else "undirected",
      weighted = if (cfg$weighted) TRUE else NULL, diag = TRUE)
    b <- (m != 0) * 1
    modes <- if (cfg$directed) c("all", "out", "in") else "all"
    combos <- expand.grid(mode = modes, nm = modes, stringsAsFactors = FALSE)
    unlist(lapply(seq_len(nrow(combos)), function(k) {
      md <- combos$mode[k]
      nm <- combos$nm[k]
      ref <- as.numeric(igraph::knn(
        g, mode = md, neighbor.degree.mode = nm,
        weights = if (cfg$weighted) NULL else NA)$knn)
      got <- .cg_knn(b, if (cfg$weighted) m else b, cfg$directed, md, nm)
      fin <- is.finite(got) & is.finite(ref)
      same <- identical(is.nan(got), is.nan(ref)) &&
        (!any(fin) || isTRUE(all.equal(got[fin], ref[fin], tolerance = 1e-10)))
      if (same) NULL else sprintf("n=%d dens=%.2f dir=%s w=%s loops=%s mode=%s nm=%s",
                                  cfg$n, cfg$density, cfg$directed, cfg$weighted,
                                  cfg$loops, md, nm)
    }))
  }))
  expect_null(mismatches)
})

test_that(".cg_knn closed forms: star, loop and isolate", {
  # Star K_{1,4}: the hub's neighbours all have degree 1; each leaf's only
  # neighbour is the hub with degree 4.
  star <- matrix(0, 5, 5)
  star[1, -1] <- 1
  star[-1, 1] <- 1
  expect_equal(.cg_knn(star, star, directed = FALSE), c(1, 4, 4, 4, 4))
  # Path 1-2 plus a loop on 2 with weight 3: node 2 has degree 1 + 2 = 3, its
  # incidences are the edge to node 1 (degree 1) and the loop twice (degree
  # 3): (1 * 1 + 3 * 3 + 3 * 3) / (1 + 3 + 3) = 19 / 7.
  m <- matrix(0, 2, 2)
  m[1, 2] <- m[2, 1] <- 1
  m[2, 2] <- 3
  expect_equal(.cg_knn((m != 0) * 1, m, directed = FALSE), c(3, 19 / 7))
  # An isolate has no incidence: 0 / 0.
  iso <- matrix(0, 3, 3)
  iso[1, 2] <- iso[2, 1] <- 1
  expect_true(is.nan(.cg_knn(iso, iso, directed = FALSE)[3]))
  expect_identical(.cg_knn(matrix(0, 0, 0), matrix(0, 0, 0), FALSE), numeric(0))
})

test_that(".cg_knn is permutation equivariant", {
  m <- .port_zoo_matrix(9L, 0.4, directed = TRUE, weighted = TRUE, loops = TRUE,
                        seed = 77L)
  set.seed(78L)
  p <- sample(9L)
  b <- (m != 0) * 1
  full <- .cg_knn(b, m, directed = TRUE, "all", "out")
  permuted <- .cg_knn(b[p, p], m[p, p], directed = TRUE, "all", "out")
  expect_equal(permuted, full[p])
})

# ---------------------------------------------------------------------------
# .cg_loop_coreness
# ---------------------------------------------------------------------------

test_that("graph zoo: .cg_loop_coreness matches igraph::coreness with loops", {
  skip_if_not_installed("igraph")
  grid <- .port_zoo_grid(weighted = FALSE)
  mismatches <- unlist(lapply(seq_len(nrow(grid)), function(i) {
    cfg <- grid[i, ]
    m <- .port_zoo_matrix(cfg$n, cfg$density, cfg$directed, FALSE, cfg$loops,
                          seed = 3000L + i)
    g <- igraph::graph_from_adjacency_matrix(
      m, mode = if (cfg$directed) "directed" else "undirected", diag = TRUE)
    unlist(lapply(c("all", "out", "in"), function(md) {
      ref <- as.numeric(igraph::coreness(g, mode = md))
      got <- .cg_loop_coreness(m, cfg$n, cfg$directed, md)
      if (isTRUE(all.equal(got, ref))) NULL else
        sprintf("n=%d dens=%.2f dir=%s loops=%s mode=%s",
                cfg$n, cfg$density, cfg$directed, cfg$loops, md)
    }))
  }))
  expect_null(mismatches)
})

test_that(".cg_loop_coreness: a loop is a fixed offset on the k-core", {
  # A 5-cycle is a 2-core. With a loop on every node the loop-inclusive
  # degree is 4 everywhere and nothing is ever peeled below it, so the
  # index is 4 (the loops_everywhere network of the degenerate tier).
  cyc <- matrix(0, 5, 5)
  cyc[cbind(1:5, c(2:5, 1))] <- 1
  cyc <- cyc + t(cyc)
  expect_equal(.cg_loop_coreness(cyc, 5L, directed = FALSE), rep(2, 5))
  expect_equal(.cg_coreness(cyc, 5L, directed = FALSE), rep(2, 5))
  diag(cyc) <- 1
  expect_equal(.cg_loop_coreness(cyc, 5L, directed = FALSE), rep(4, 5))
  # Read as directed, the symmetric cycle gives every node out-degree 2 and
  # in-degree 2; the loop adds one under "out" and "in" and two under "all".
  expect_equal(.cg_loop_coreness(cyc, 5L, directed = TRUE, "out"), rep(3, 5))
  expect_equal(.cg_loop_coreness(cyc, 5L, directed = TRUE, "in"), rep(3, 5))
  expect_equal(.cg_loop_coreness(cyc, 5L, directed = TRUE, "all"), rep(6, 5))
  expect_identical(.cg_loop_coreness(matrix(0, 0, 0), 0L), numeric(0))
})

# ---------------------------------------------------------------------------
# Calculators never reach igraph
# ---------------------------------------------------------------------------

.port_calculator_calls <- function(cg) {
  memb <- rep(1:2, length.out = cg$n)
  wts <- cg$weights
  list(
    distance_entropy = quote(calculate_distance_entropy(cg, mode = "all")),
    local_dimension = quote(calculate_local_dimension(cg, mode = "out")),
    local_information_dimension =
      quote(calculate_local_information_dimension(cg, mode = "in")),
    modularity_vitality =
      quote(calculate_modularity_vitality(cg, weights = wts, membership = memb)),
    neighborhood_connectivity =
      quote(calculate_neighborhood_connectivity(cg, mode = "all")),
    shapley_game1 = quote(calculate_shapley(cg, game = 1L)),
    shapley_game2 = quote(calculate_shapley(cg, game = 2L, k = 2)),
    shapley_game3 = quote(calculate_shapley(cg, game = 3L, cutoff = 2)),
    access_information = quote(calculate_search_information(cg, what = "access")),
    hide_information = quote(calculate_search_information(cg, what = "hide")),
    rumor = quote(calculate_rumor(cg)),
    community_hub_bridge =
      quote(calculate_community_hub_bridge(cg, membership = memb, mode = "all")),
    entropy_variation_degree =
      quote(calculate_entropy_variation(cg, of = "degree", mode = "out")),
    entropy_variation_betweenness =
      quote(calculate_entropy_variation(cg, of = "betweenness")),
    s_shell = quote(calculate_s_shell(cg)),
    degree_discount = quote(calculate_degree_discount(cg)),
    single_discount = quote(calculate_degree_discount(cg, single = TRUE)),
    ncvoterank = quote(calculate_ncvoterank(cg)),
    community_based =
      quote(calculate_community_based(cg, membership = memb, mode = "all")),
    comm_centrality =
      quote(calculate_comm_centrality(cg, membership = memb, mode = "all")),
    community_mediator =
      quote(calculate_community_mediator(cg, membership = memb, mode = "all")),
    local_dimension_fixed = quote(calculate_local_dimension_fixed(cg, mode = "all")),
    fuzzy_local_dimension = quote(calculate_fuzzy_local_dimension(cg, mode = "all")),
    local_volume_dimension = quote(calculate_local_volume_dimension(cg, mode = "all")),
    wvoterank = quote(calculate_wvoterank(cg, weights = wts)),
    enrenew = quote(calculate_enrenew(cg)),
    voterank_plus = quote(calculate_voterank_plus(cg)),
    node_contraction = quote(calculate_node_contraction(cg)),
    node_contraction_improved = quote(calculate_node_contraction(cg, improved = TRUE)),
    two_way_rw = quote(calculate_two_way_rw(cg, weights = wts)),
    heatmap = quote(calculate_heatmap(cg, mode = "all")),
    flow_coefficient = quote(calculate_flow_coefficient(cg)),
    local_entropy = quote(calculate_local_entropy(cg, mode = "out")),
    weighted_h_index = quote(calculate_weighted_h_index(cg, mode = "in")),
    redundancy = quote(calculate_redundancy(cg)),
    weighted_kshell = quote(calculate_weighted_kshell(cg, weights = wts)),
    renewed_coreness = quote(calculate_renewed_coreness(cg)),
    geodesic_kpath = quote(calculate_geodesic_kpath(cg, mode = "all")),
    local_efficiency = quote(calculate_local_efficiency(cg, mode = "all", weights = wts)),
    s_core = quote(calculate_s_core(cg, weights = wts)),
    fragmentation = quote(calculate_fragmentation(cg, mode = "all", weights = wts)),
    kpath = quote(calculate_kpath(cg, mode = "out", k = 3)),
    epc = quote(calculate_epc(cg, runs = 10, seed = 1)),
    length_scaled_betweenness =
      quote(calculate_length_scaled_betweenness(cg, weights = wts)),
    delta_betweenness = quote(calculate_delta_betweenness(cg, weights = wts)),
    ego_betweenness = quote(calculate_ego_betweenness(cg)),
    delta_closeness = quote(calculate_delta_closeness(cg, mode = "all", weights = wts)),
    gravity_kshell = quote(calculate_gravity(cg, mode = "all", mass = "kshell")),
    gravity_degree = quote(calculate_gravity(cg, mode = "out", mass = "degree")),
    gravity_legacy =
      quote(calculate_gravity(cg, mode = "in", mass = "legacy", radius = "auto"))
  )
}

test_that("batch 7-11 calculators never reach igraph", {
  old <- options(cograph.forbid_igraph = TRUE)
  on.exit(options(old), add = TRUE)
  graphs <- list(
    undirected_weighted = .port_zoo_matrix(12L, 0.35, FALSE, TRUE, FALSE, 501L),
    directed_loops = .port_zoo_matrix(10L, 0.4, TRUE, TRUE, TRUE, 502L),
    empty = matrix(0, 0, 0),
    single = matrix(0, 1, 1)
  )
  for (name in names(graphs)) {
    cg <- .cg_graph(graphs[[name]])
    calls <- .port_calculator_calls(cg)
    env <- list2env(list(cg = cg, memb = rep(1:2, length.out = cg$n),
                         wts = cg$weights))
    status <- vapply(names(calls), function(nm) {
      r <- tryCatch(eval(calls[[nm]], envir = env), error = function(e) e)
      if (inherits(r, "cograph_igraph_leak")) return("leak")
      if (inherits(r, "error")) return(paste("error:", conditionMessage(r)))
      if (length(r) != cg$n) return(sprintf("length %d", length(r)))
      "ok"
    }, character(1))
    expect_identical(status[status != "ok"], setNames(character(0), character(0)),
                     info = name)
  }
})

test_that("ported calculators keep their contract errors and NA warnings", {
  cg <- .cg_graph(.port_zoo_matrix(6L, 0.5, FALSE, FALSE, FALSE, 601L))
  expect_error(calculate_modularity_vitality(cg, membership = 1:2),
               class = "cograph_bad_membership")
  expect_error(calculate_community_hub_bridge(cg, membership = c(1, NA, 1, 2, 2, 1)),
               class = "cograph_bad_membership")
  expect_error(calculate_comm_centrality(cg, membership = 1:3),
               class = "cograph_bad_membership")
  expect_warning(out <- calculate_modularity_vitality(cg, membership = NULL),
                 "requires membership")
  expect_true(all(is.na(out)) && length(out) == 6L)
  expect_warning(out <- calculate_community_mediator(cg, membership = NULL),
                 "requires membership")
  expect_true(all(is.na(out)) && length(out) == 6L)
})

# ---------------------------------------------------------------------------
# Replaced igraph algorithms against the igraph reference they replaced
# ---------------------------------------------------------------------------

test_that("entropy variation on betweenness reproduces the igraph deletion loop", {
  skip_if_not_installed("igraph")
  grid <- expand.grid(directed = c(FALSE, TRUE), loops = c(FALSE, TRUE),
                      stringsAsFactors = FALSE)
  for (i in seq_len(nrow(grid))) {
    m <- .port_zoo_matrix(9L, 0.4, grid$directed[i], TRUE, grid$loops[i], 700L + i)
    cg <- .cg_graph(m)
    # The old calculator built a weighted igraph object and then dropped the
    # weight attribute; building unweighted would read sub-1 weights as
    # zero multi-edge counts.
    g <- igraph::graph_from_adjacency_matrix(
      m, mode = if (cg$directed) "directed" else "undirected", weighted = TRUE,
      diag = TRUE)
    g <- igraph::delete_edge_attr(g, "weight")
    f <- igraph::betweenness(g)
    ref <- .cg_entropy_variation(f, function(k) {
      igraph::betweenness(igraph::delete_vertices(g, k))
    })
    expect_equal(calculate_entropy_variation(cg, of = "betweenness"), ref,
                 tolerance = 1e-10)
  }
})

test_that("NCVoteRank reproduces the simplified-undirected igraph reference", {
  skip_if_not_installed("igraph")
  for (directed in c(FALSE, TRUE)) {
    m <- .port_zoo_matrix(11L, 0.3, directed, TRUE, TRUE, 800L + directed)
    cg <- .cg_graph(m)
    g <- igraph::graph_from_adjacency_matrix(
      m, mode = if (directed) "directed" else "undirected", weighted = TRUE,
      diag = TRUE)
    h <- igraph::as_undirected(igraph::simplify(g, remove.loops = TRUE),
                               mode = "collapse")
    b <- igraph::as_adjacency_matrix(h, sparse = FALSE)
    dimnames(b) <- NULL
    ref <- .cg_ncvoterank(b, ks = as.numeric(igraph::coreness(h)), theta = 0.5)
    expect_equal(calculate_ncvoterank(cg, theta = 0.5), ref)
  }
})

test_that("gravity masses use igraph's loop-inclusive degree and coreness", {
  skip_if_not_installed("igraph")
  m <- .port_zoo_matrix(10L, 0.4, TRUE, TRUE, TRUE, 901L)
  cg <- .cg_graph(m)
  g <- igraph::graph_from_adjacency_matrix(m, mode = "directed", weighted = TRUE,
                                           diag = TRUE)
  for (md in c("all", "out", "in")) {
    deg <- as.numeric(igraph::degree(g, mode = md))
    ks <- as.numeric(igraph::coreness(g, mode = md))
    got <- .cg_gravity_mass(cg, "kshell", md)
    expect_equal(got$i, ks, info = md)
    expect_equal(.cg_gravity_mass(cg, "degree", md)$j, deg, info = md)
    expect_equal(.cg_gravity_mass(cg, "legacy", md)$j, deg * ks, info = md)
    expect_equal(.cg_gravity_mass(cg, "legacy", md)$i, rep(1, 10), info = md)
  }
})
