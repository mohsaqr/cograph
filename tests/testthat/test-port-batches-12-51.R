# Port of centrality batches 12-51 off igraph: the graph context replaces the
# igraph bridge, so these pin the pieces that were rewritten rather than
# merely deleted, plus a leak check over every measure the batches own.

.port_measures <- function() {
  c("truss", "mdd", "bridging_coefficient", "godfather", "support", "volume",
    "mcc", "diffusion_centrality", "dynamical_importance", "dynamics_sensitive",
    "malatya", "expected_force", "mcgm", "spectralrank", "controlrank",
    "map_equation", "ninl", "beta_measure", "localized_bridging",
    "extended_local_bridging", "modified_expected_force",
    "proximal_betweenness", "x_degree", "coleman_theil", "bridging_capital",
    "linerank", "random_walk_decay", "graph_regularization",
    "resistance_curvature", "extended_coreness", "cda", "improved_closeness",
    "exogenous", "global_structure", "hybrid_global_structure",
    "improved_global_structure", "dkgm", "neighbor_distance", "ira", "iira",
    "lnc", "ked", "hcc", "ehcc", "lhc", "iec", "dil", "trust_pagerank",
    "rsp_betweenness", "relative_entropy", "mixed_gravity",
    "extended_mixed_gravity", "extended_gravity", "weighted_leaderrank",
    "adaptive_leaderrank")
}

.port_kite <- function() {
  edges <- rbind(c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7),
                 c(3, 4), c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7),
                 c(6, 8), c(7, 8), c(8, 9), c(9, 10))
  m <- matrix(0, 10, 10)
  m[edges] <- 1
  m <- m + t(m)
  dimnames(m) <- list(LETTERS[1:10], LETTERS[1:10])
  m
}

test_that("candidate adjacency places canonical weights, mirrored when undirected", {
  m <- matrix(c(0, 2, 0,
                0, 5, 3,
                4, 0, 0), 3, 3, byrow = TRUE)
  cg <- .cg_graph(m)
  expect_true(cg$directed)
  expect_equal(.cg_candidate_adjacency(cg, NULL, "x"), (m != 0) * 1)
  expect_equal(.cg_candidate_adjacency(cg, cg$weights, "x"), m)
  # Doubling the canonical weights doubles every cell, loop included.
  expect_equal(.cg_candidate_adjacency(cg, 2 * cg$weights, "x"), 2 * m)
  u <- matrix(c(0, 1, 0,
                1, 7, 3,
                0, 3, 0), 3, 3, byrow = TRUE)
  cu <- .cg_graph(u)
  expect_false(cu$directed)
  expect_equal(.cg_candidate_adjacency(cu, cu$weights, "x"), u)
  expect_error(.cg_candidate_adjacency(cg, c(1, -1, 1, 1), "x"),
               "finite nonnegative")
  expect_error(.cg_candidate_adjacency(cg, c(1, Inf, 1, 1), "x"),
               "finite nonnegative")
})

test_that("cda ignores loops, weighted and unweighted", {
  with_loop <- matrix(c(0, 2, 1, 0,
                        2, 4, 1, 0,
                        1, 1, 0, 3,
                        0, 0, 3, 0), 4, 4, byrow = TRUE)
  no_loop <- with_loop
  diag(no_loop) <- 0
  expect_equal(centrality_cda(with_loop), centrality_cda(no_loop))
  expect_equal(centrality_cda(with_loop, weighted = FALSE),
               centrality_cda(no_loop, weighted = FALSE))
  cg <- .cg_graph(with_loop)
  loop_weights <- cg$weights
  loop_weights[cg$edges[, 1L] == cg$edges[, 2L]] <- NA
  # An unusable loop weight is dropped, not validated.
  expect_equal(calculate_cda(cg, loop_weights), calculate_cda(.cg_graph(no_loop), .cg_graph(no_loop)$weights))
})

test_that("relative entropy path indexes match hand values on a path graph", {
  # P4: A - B - C - D
  a <- matrix(0, 4, 4)
  a[cbind(1:3, 2:4)] <- 1
  a <- a + t(a)
  ctx <- .cg_re_context(a)
  expect_equal(ctx$b, a)
  # Betweenness 0, 2, 2, 0 on the path, doubled as the paper counts ordered pairs.
  expect_equal(.cg_re_index(ctx, "betweenness"), c(0, 4, 4, 0))
  # Removing an end leaves one component of three; removing an inner node two.
  expect_equal(.cg_re_index(ctx, "n_components"), c(1, 2, 2, 1))
  expect_equal(.cg_re_index(ctx, "largest_component"), c(3, 2, 2, 3))
  # A weighted, asymmetric input is read as a simple undirected graph.
  w <- a * 5
  w[1, 2] <- 0
  expect_equal(.cg_re_context(w)$b, a)
  expect_equal(.cg_re_index(.cg_re_context(w), "betweenness"), c(0, 4, 4, 0))
  # One node: deleting it leaves nothing.
  one <- .cg_re_context(matrix(0, 1, 1))
  expect_equal(.cg_re_index(one, "n_components"), 0)
  expect_equal(.cg_re_index(one, "largest_component"), 0)
})

test_that("relative entropy on the kite with every index has no igraph and is a distribution", {
  old <- options(cograph.forbid_igraph = TRUE)
  on.exit(options(old), add = TRUE)
  df <- centrality(.port_kite(), measures = "relative_entropy",
                   re_indexes = .cg_re_vocabulary())
  expect_equal(sum(df$relative_entropy), 1)
  # A leaf carries no shortest path, so its geometric mean is exactly zero.
  expect_true(all(is.finite(df$relative_entropy) & df$relative_entropy >= 0))
  expect_true(any(df$relative_entropy > 0))
})

test_that("named node vectors align by label, and need labels to exist", {
  m <- .port_kite()
  prior <- stats::setNames(seq_len(10), LETTERS[1:10])
  base <- centrality_spectralrank(m, sr_prior = prior)
  expect_equal(centrality_spectralrank(m, sr_prior = rev(prior)), base)
  expect_equal(centrality_spectralrank(m, sr_prior = unname(prior)), base)
  expect_error(centrality_spectralrank(unname(m), sr_prior = prior),
               "names must match")
  mass <- stats::setNames(seq_len(10), LETTERS[1:10])
  expect_equal(centrality_random_walk_decay(m, rwd_node_weights = rev(mass)),
               centrality_random_walk_decay(m, rwd_node_weights = mass))
  expect_equal(unname(centrality_random_walk_decay(unname(m), rwd_node_weights = unname(mass))),
               unname(centrality_random_walk_decay(m, rwd_node_weights = mass)))
  expect_error(centrality_random_walk_decay(m, rwd_node_weights = stats::setNames(mass, letters[1:10])),
               "names must match")
  member <- stats::setNames(rep(1:2, each = 5), LETTERS[1:10])
  expect_equal(centrality_map_equation(m, membership = rev(member)),
               centrality_map_equation(m, membership = member))
  values <- matrix(1, 10, 10, dimnames = list(LETTERS[1:10], LETTERS[1:10]))
  expect_equal(centrality_bridging_capital(m, bridging_values = values[10:1, 10:1]),
               centrality_bridging_capital(m, bridging_values = values))
})

test_that("linerank reads the canonical edge list", {
  # Directed 3-cycle plus a chord: every edge has exactly one out-neighbour
  # edge except the chord's source, which feeds two.
  m <- matrix(0, 3, 3)
  m[cbind(c(1, 2, 3, 1), c(2, 3, 1, 3))] <- 1
  old <- options(cograph.forbid_igraph = TRUE)
  on.exit(options(old), add = TRUE)
  score <- centrality_linerank(m)
  expect_length(score, 3)
  expect_true(all(is.finite(score)))
  # Every edge contributes its stationary mass to both endpoints.
  expect_equal(sum(score), 2)
})

test_that("no batch 12-51 measure reaches igraph", {
  old <- options(cograph.forbid_igraph = TRUE)
  on.exit(options(old), add = TRUE)
  m <- .port_kite()
  leaked <- vapply(.port_measures(), function(mm) {
    r <- tryCatch(suppressWarnings(centrality(m, measures = mm)),
                  error = function(e) e)
    inherits(r, "cograph_igraph_leak")
  }, logical(1))
  expect_false(any(leaked), info = paste(names(leaked)[leaked], collapse = ", "))
  failed <- vapply(.port_measures(), function(mm) {
    inherits(tryCatch(suppressWarnings(centrality(m, measures = mm)),
                      error = function(e) e), "error")
  }, logical(1))
  expect_false(any(failed), info = paste(names(failed)[failed], collapse = ", "))
})
