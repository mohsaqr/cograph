# Regression tests for the 2026-10 condition fixes (bugs 10, 16, 17, 18):
# undefined measures warn with `cograph_undefined_measure`, bad arguments
# raise `cograph_bad_parameter`, and a missing or malformed partition raises
# `cograph_bad_membership`. Conditions are tested by class.

# Every warning class raised while evaluating `expr`, with the value.
.collect_warning_classes <- function(expr) {
  classes <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    classes <<- c(classes, class(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, classes = unique(classes))
}

.labelled <- function(m) {
  dimnames(m) <- list(letters[seq_len(nrow(m))], letters[seq_len(nrow(m))])
  m
}

# --- bug 10: random_walk, markov, information -------------------------------

test_that("bug 10: markov on an undirected path matches hand-computed values", {
  path <- .labelled(matrix(c(0, 1, 0,
                             1, 0, 1,
                             0, 1, 0), 3, byrow = TRUE))
  # m_ba = 3 and m_ca = 4 (m_ba = 1 + m_ca / 2, m_ca = 1 + m_ba), so the mean
  # passage time into a is 7/3; into b it is 2/3.
  expect_no_warning(mk <- centrality_markov(path))
  expect_equal(unname(mk), c(3 / 7, 3 / 2, 3 / 7))
})

test_that("bug 10: markov is NA with a classed warning for unreachable nodes", {
  # a -> b, b <-> c: a is reached by no node, b and c by every node.
  m <- .labelled(matrix(0, 3, 3))
  m[1, 2] <- m[2, 3] <- m[3, 2] <- 1
  expect_warning(mk <- centrality_markov(m),
                 class = "cograph_undefined_measure")
  # Passage times into b: 1 from a, 1 from c; into c: 2 from a, 1 from b.
  expect_equal(unname(mk), c(NA, 3 / 2, 1))
})

test_that("bug 10: markov is NA when the walk stops at a node without out-ties", {
  # a -> b -> c: c has no out-tie. The old code returned 2 for c, but the
  # mean passage time into c is (2 + 1 + 0) / 3 = 1, so 2 was meaningless.
  m <- .labelled(matrix(0, 3, 3))
  m[1, 2] <- m[2, 3] <- 1
  expect_warning(mk <- centrality_markov(m),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(mk)))
})

test_that("bug 10: random_walk is NA on a directed network not strongly connected", {
  m <- .labelled(matrix(0, 3, 3))
  m[1, 2] <- m[2, 3] <- m[3, 2] <- 1
  expect_warning(rw <- centrality_random_walk(m),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(rw)))
})

test_that("bug 10: disconnected random_walk and markov warn by class", {
  disc <- .labelled(matrix(0, 4, 4))
  disc[1, 2] <- disc[2, 1] <- 1
  expect_warning(rw <- centrality_random_walk(disc),
                 class = "cograph_undefined_measure")
  expect_warning(mk <- centrality_markov(disc),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(rw)))
  expect_true(all(is.na(mk)))
})

test_that("bug 10: strongly connected input keeps its random-walk values", {
  # Directed 3-cycle: m_ij is 1 to the next node and 2 to the one after,
  # so every random-walk distance sum is (1 + 2) / 2 * 2 = 3.
  cyc <- .labelled(matrix(0, 3, 3))
  cyc[1, 2] <- cyc[2, 3] <- cyc[3, 1] <- 1
  expect_no_warning(rw <- centrality_random_walk(cyc))
  expect_equal(unname(rw), rep(1 / 3, 3))
  expect_no_warning(mk <- centrality_markov(cyc))
  expect_equal(unname(mk), rep(1, 3))
})

test_that("bug 10: information is NA with a classed warning when disconnected", {
  # K3 + K2: the old result was rounding noise of order 1e-16.
  m <- .labelled(matrix(0, 5, 5))
  m[1, 2] <- m[2, 1] <- m[1, 3] <- m[3, 1] <- m[2, 3] <- m[3, 2] <- 1
  m[4, 5] <- m[5, 4] <- 1
  expect_warning(ic <- centrality_information(m),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(ic)))
})

test_that("bug 10: connected information still equals sna::infocent", {
  skip_if_not_installed("sna")
  m <- .labelled(matrix(0, 5, 5))
  m[1, 2] <- m[2, 1] <- m[1, 3] <- m[3, 1] <- m[2, 3] <- m[3, 2] <- 1
  m[4, 5] <- m[5, 4] <- m[3, 4] <- m[4, 3] <- 1
  expect_no_warning(ic <- centrality_information(m))
  expect_equal(unname(ic), sna::infocent(m, gmode = "graph"))
  # An isolate leaves the component intact and scores 0.
  iso <- .labelled(rbind(cbind(unname(m), 0), 0))
  expect_no_warning(ic_iso <- centrality_information(iso))
  expect_equal(unname(ic_iso), c(unname(ic), 0))
})

# --- bug 16: NaN with cograph_undefined_measure ------------------------------

test_that("bug 16: dynamical_importance on an acyclic network warns by class", {
  dag <- .labelled(matrix(0, 3, 3))
  dag[1, 2] <- dag[2, 3] <- 1
  expect_warning(di <- centrality_dynamical_importance(dag),
                 class = "cograph_undefined_measure")
  expect_true(all(is.nan(di)))
})

test_that("bug 16: weighted_leaderrank with no in-degree warns by class", {
  empty <- .labelled(matrix(0, 3, 3))
  expect_warning(wl <- centrality_weighted_leaderrank(empty),
                 class = "cograph_undefined_measure")
  expect_true(all(is.nan(wl)))
  # wlr_alpha = 0 needs no in-degree and stays defined: (n + 1) / (2n).
  expect_no_warning(wl0 <- centrality_weighted_leaderrank(empty, wlr_alpha = 0))
  expect_equal(unname(wl0), rep(4 / 6, 3))
})

test_that("bug 16: adaptive_leaderrank with every H-index 0 warns by class", {
  m <- .labelled(matrix(0, 2, 2))
  m[1, 2] <- 1
  expect_warning(al <- centrality_adaptive_leaderrank(m, alr_h_mode = "out"),
                 class = "cograph_undefined_measure")
  expect_true(all(is.nan(al)))
})

# --- bug 17: decay_parameter and dmnc_epsilon --------------------------------

test_that("bug 17: decay_parameter outside (0, 1) raises cograph_bad_parameter", {
  m <- .labelled(matrix(c(0, 1, 1, 0), 2))
  bad <- list(0, 1, 1.5, -0.2, NA_real_, Inf, c(0.2, 0.3), "0.5")
  invisible(lapply(bad, \(value) {
    expect_error(centrality_decay(m, decay_parameter = value),
                 class = "cograph_bad_parameter")
    expect_error(centrality_generalized_closeness(m, decay_parameter = value),
                 class = "cograph_bad_parameter")
  }))
  # Hand value on one edge: delta^0 + delta^1.
  expect_equal(unname(centrality_decay(m, decay_parameter = 0.3)), c(1.3, 1.3))
  # Unused parameters are not checked.
  expect_no_error(centrality(m, measures = "degree", decay_parameter = 2))
})

test_that("bug 17: a bad dmnc_epsilon raises cograph_bad_parameter", {
  m <- .labelled(matrix(1, 3, 3) - diag(3))
  bad <- list(0, -1, NA_real_, Inf, c(1, 2), "1.7")
  invisible(lapply(bad, \(value) {
    expect_error(centrality_dmnc(m, dmnc_epsilon = value),
                 class = "cograph_bad_parameter")
  }))
  # Triangle: each neighborhood is one edge on two nodes, so E / N^eps.
  expect_equal(unname(centrality_dmnc(m, dmnc_epsilon = 1)), rep(1 / 2, 3))
  expect_no_error(centrality(m, measures = "degree", dmnc_epsilon = -1))
})

# --- bug 18: unclassed conditions --------------------------------------------

test_that("bug 18: community measures without membership warn by class", {
  m <- .labelled(matrix(1, 4, 4) - diag(4))
  measures <- c("participation", "within_module_z", "gateway",
                "community_based", "comm_centrality", "community_mediator",
                "modularity_vitality", "community_hub_bridge",
                "brokerage_coordinator")
  invisible(lapply(measures, \(measure) {
    res <- .collect_warning_classes(centrality(m, measures = measure,
                                               directed = TRUE))
    expect_true("cograph_bad_membership" %in% res$classes, info = measure)
    expect_true("cograph_undefined_measure" %in% res$classes, info = measure)
  }))
})

test_that("bug 18: membership of the wrong length raises cograph_bad_membership", {
  m <- .labelled(matrix(1, 4, 4) - diag(4))
  measures <- c("participation", "within_module_z", "gateway",
                "brokerage_coordinator")
  invisible(lapply(measures, \(measure) {
    expect_error(centrality(m, measures = measure, membership = c(1, 2),
                            directed = TRUE),
                 class = "cograph_bad_membership")
  }))
})

test_that("bug 18: directed-only measures warn by class on undirected input", {
  m <- .labelled(matrix(1, 4, 4) - diag(4))
  measures <- c("salsa", "leaderrank", "pairwisedis", "prestige_domain",
                "prestige_domain_proximity", "trophic_level")
  invisible(lapply(measures, \(measure) {
    expect_warning(centrality(m, measures = measure, directed = FALSE),
                   class = "cograph_undefined_measure")
  }))
  expect_warning(
    centrality(m, measures = "brokerage_coordinator", directed = FALSE,
               membership = c(1, 1, 2, 2)),
    class = "cograph_undefined_measure")
})

test_that("bug 18: measures that need a connected graph warn by class", {
  disc <- .labelled(matrix(0, 4, 4))
  disc[1, 2] <- disc[2, 1] <- disc[3, 4] <- disc[4, 3] <- 1
  measures <- c("current_flow_closeness", "current_flow_betweenness",
                "second_order", "spanning_tree")
  invisible(lapply(measures, \(measure) {
    expect_warning(centrality(disc, measures = measure),
                   class = "cograph_undefined_measure")
  }))
})

test_that("bug 18: hubbell, kreach and damping raise cograph_bad_parameter", {
  m <- .labelled(matrix(1, 3, 3) - diag(3))
  expect_error(centrality_hubbell(m, hubbell_weight = -1),
               class = "cograph_bad_parameter")
  expect_error(centrality_kreach(m, k = 0), class = "cograph_bad_parameter")
  expect_error(centrality(m, measures = "pagerank", damping = 2),
               class = "cograph_bad_parameter")
  expect_error(centrality(m, measures = "degree", sort_by = "nope"),
               class = "cograph_bad_parameter")
  expect_error(centrality(m, measures = "no_such_measure"),
               class = "cograph_unknown_measure")
  # Spectral radius of the triangle is 2, so weightfactor 0.5 is not solvable.
  expect_warning(centrality_hubbell(m, hubbell_weight = 0.5),
                 class = "cograph_undefined_measure")
})

test_that("bug 18: kernel-batch parameter errors are cograph_bad_parameter", {
  m <- .labelled(matrix(1, 4, 4) - diag(4))
  calls <- list(
    \() centrality(m, measures = "diffusion_centrality", diffusion_q = 2),
    \() centrality(m, measures = "weighted_leaderrank", wlr_alpha = NA_real_),
    \() centrality(m, measures = "random_walk_decay", rwd_decay = 1),
    \() centrality(m, measures = "mdd", mdd_lambda = 2),
    \() centrality(m, measures = "map_equation", map_flow = "nope",
                   membership = c(1, 1, 2, 2)),
    \() centrality(m, measures = "graph_regularization", grc_gamma = -1),
    \() centrality(m, measures = "neighbor_distance", nd_decay = NA_real_)
  )
  invisible(lapply(calls, \(f) {
    expect_error(f(), class = "cograph_bad_parameter")
  }))
})
