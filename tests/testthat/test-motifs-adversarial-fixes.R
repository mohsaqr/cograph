# Regression tests for the 2026-08-12 adversarial review of the motifs
# subsystem. Each test pins the fix for one verified finding; numbers refer
# to the review (docs/CHANGES.md, 2026-08-12 entry).

skip_on_cran()

.man_names <- c("003", "012", "102", "021D", "021U", "021C",
                "111D", "111U", "030T", "030C", "201",
                "120D", "120U", "120C", "210", "300")

# ---- #1: motif_census() must label MAN classes correctly -------------------

test_that("motif_census labels every single-triad graph with its own class", {
  pats <- cograph:::.get_triad_patterns_canonical()
  for (nm in names(pats)) {
    mc <- motif_census(pats[[nm]], directed = TRUE, n_random = 2, seed = 1)
    expect_identical(mc$motif, .man_names)
    expect_identical(mc$motif[mc$count == 1], nm)
  }
})

# ---- #2: pattern = "all" includes 003 --------------------------------------

test_that("motifs(pattern = 'all') reports 003 and sums to choose(n, 3)", {
  z4 <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  r <- motifs(z4, pattern = "all", significance = FALSE,
              min_transitions = 0, min_count = NULL)
  expect_identical(r$results$type, "003")
  expect_identical(r$results$count, as.integer(choose(4, 3)))

  set.seed(7)
  mm <- matrix(rbinom(36, 1, 0.2), 6, 6)
  diag(mm) <- 0
  dimnames(mm) <- list(LETTERS[1:6], LETTERS[1:6])
  r2 <- motifs(mm, pattern = "all", significance = FALSE,
               min_transitions = 0, min_count = NULL)
  expect_identical(sum(r2$results$count), as.integer(choose(6, 3)))

  tc <- igraph::triad_census(
    igraph::graph_from_adjacency_matrix(mm, mode = "directed"))
  names(tc) <- .man_names
  got <- stats::setNames(r2$results$count, r2$results$type)
  for (nm in names(tc)[tc > 0]) {
    expect_identical(as.integer(got[[nm]]), as.integer(tc[[nm]]))
  }
})

# ---- #3: undirected census has the one-edge class --------------------------

test_that("undirected census reports empty/edge/wedge/triangle correctly", {
  u <- matrix(0, 4, 4)
  u[1, 2] <- u[2, 1] <- 1
  r <- motif_census(u, directed = FALSE, method = "gnm", n_random = 3,
                    seed = 1)
  expect_identical(r$motif, c("empty", "edge", "wedge", "triangle"))
  expect_identical(r$count, c(2, 2, 0, 0))
})

test_that("undirected census ignores self-loops", {
  u <- matrix(0, 4, 4)
  u[1, 1] <- 1
  r <- motif_census(u, directed = FALSE, method = "gnm", n_random = 3,
                    seed = 1)
  expect_identical(r$count[r$motif == "empty"], 4)
  expect_identical(r$count[r$motif == "wedge"], 0)
})

# ---- #4: configuration null preserves degrees, works when disconnected -----

test_that("configuration null handles disconnected graphs with real variance", {
  k3 <- matrix(1, 3, 3); diag(k3) <- 0
  m6 <- matrix(0, 6, 6)
  m6[1:3, 1:3] <- k3
  m6[4:6, 4:6] <- k3
  r <- motif_census(m6, directed = FALSE, method = "configuration",
                    n_random = 30, seed = 1)
  # the old vl/stub-matching null either errored or returned sd = 0, z = 0
  expect_true(any(r$null_sd > 0))
  # an observation that differs from a degenerate null must never get z = 0
  expect_false(any(r$z_score == 0 & r$count != r$null_mean, na.rm = TRUE))
})

test_that("configuration null tolerates isolated vertices", {
  u <- matrix(0, 4, 4)
  u[1, 2] <- u[2, 1] <- 1
  expect_no_error(
    motif_census(u, directed = FALSE, method = "configuration",
                 n_random = 3, seed = 1)
  )
})

# ---- #5: instance null tests the row's own type, not "any edge" ------------

test_that("instance significance null converges to the type-specific rate", {
  el <- do.call(rbind, lapply(1:10, function(id) {
    data.frame(actor = paste0("s", id),
               from = c("A", "B", "C"), to = c("B", "C", "A"))
  }))
  sg <- subgraphs(el, pattern = "triangle", min_transitions = 3,
                  min_count = NULL, significance = TRUE,
                  n_perm = 2000, seed = 1)
  row <- sg$results[sg$results$type == "030C", ]
  # exact configuration permutations of A->B->C->A stubs yield a directed
  # 3-cycle (either orientation) with probability 2/6, so expected ~= 3.33;
  # the old "any edge" null converged to 10 * 5/6 = 8.33
  expect_lt(abs(row$expected - 10 * 2 / 6), 0.5)
  expect_true(row$sig)
})

# ---- #6: census and instance per-type totals agree --------------------------

test_that("census and instance modes report the same per-type totals", {
  cyc <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))
  tr <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"))
  el <- rbind(transform(cyc, actor = "c1"),
              transform(cyc, actor = "c2"),
              transform(tr, actor = "t1"))
  cen <- motifs(el, pattern = "triangle", significance = FALSE,
                min_transitions = 3, min_count = NULL)
  ins <- motifs(el, named_nodes = TRUE, pattern = "triangle",
                significance = FALSE, min_transitions = 3, min_count = NULL)
  cen_totals <- stats::setNames(cen$results$count, cen$results$type)
  ins_totals <- tapply(ins$results$observed, ins$results$type, sum)
  expect_identical(sort(names(cen_totals)), sort(names(ins_totals)))
  for (nm in names(cen_totals)) {
    expect_identical(as.integer(ins_totals[[nm]]),
                     as.integer(cen_totals[[nm]]))
  }
})

# ---- #7: extract_motifs(level = "aggregate") pools the network -------------

test_that("extract_motifs aggregate level analyzes the pooled network", {
  cyc <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))
  tr <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"))
  el <- rbind(transform(cyc, actor = "c1"),
              transform(cyc, actor = "c2"),
              transform(tr, actor = "t1"))
  ind <- extract_motifs(data = el, id = "actor", level = "individual",
                        pattern = "triangle", min_transitions = 0)
  agg <- extract_motifs(data = el, id = "actor", level = "aggregate",
                        pattern = "triangle", min_transitions = 0)
  expect_false(identical(ind$results, agg$results))
  # pooled graph: A<->B mutual (A->B from both + B->A never... A->B, B->C,
  # C->A from cycles; A->B, A->C, B->C from transitive) => A->B, B->C, C->A,
  # A->C: one mutual pair (C<->A? no) — assert via igraph instead:
  pooled <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  pooled["A", "B"] <- 3; pooled["B", "C"] <- 3
  pooled["C", "A"] <- 2; pooled["A", "C"] <- 1
  tc <- igraph::triad_census(
    igraph::graph_from_adjacency_matrix(pooled > 0, mode = "directed"))
  names(tc) <- .man_names
  expect_identical(agg$results$type, names(tc)[tc == 1])
})

# ---- #8: aggregate instance significance warns instead of silently skipping -

test_that("instance significance on aggregate input warns and reports FALSE", {
  m030c <- cograph:::.get_triad_patterns_canonical()[["030C"]]
  expect_warning(
    r <- subgraphs(m030c, pattern = "triangle", min_transitions = 0,
                   min_count = NULL, significance = TRUE, n_perm = 5,
                   seed = 1),
    "individual-level"
  )
  expect_false(r$params$significance)
  expect_false("z" %in% names(r$results))
})

# ---- #9: symmetric matrices get a directed census null ----------------------

test_that("census significance on a symmetric matrix joins MAN rows", {
  m3 <- matrix(1, 3, 3); diag(m3) <- 0
  r <- motifs(m3, pattern = "triangle", significance = TRUE,
              n_perm = 10, seed = 1)
  row <- r$results[r$results$type == "300", ]
  expect_false(is.na(row$expected))
  expect_false(is.na(row$p))
})

# ---- #10: conflicting directed= on igraph input errors ----------------------

test_that("motif_census rejects directed= conflicting with an igraph input", {
  g <- igraph::make_full_graph(3, directed = FALSE)
  expect_error(motif_census(g, directed = TRUE, n_random = 2),
               "as_directed")
})

# ---- #11: percent threshold semantics and fractional weights ---------------

test_that("percent threshold treats values above 1 as percentages", {
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- mat[2, 3] <- mat[3, 1] <- 10
  r <- cograph:::.count_triads_matrix_vectorized(
    mat, edge_method = "percent", edge_threshold = 1.5, exclude = "003")
  # each edge is 1/3 of the triad weight >= 1.5%; the old > total * 1.5
  # rule classified nothing
  expect_identical(r$type, "030C")
})

test_that("fractional weights keep their stubs in the permutation null", {
  el <- data.frame(actor = c("s1", "s1", "s1", "s2", "s2", "s2"),
                   from = rep(c("A", "B", "C"), 2),
                   to = rep(c("B", "C", "A"), 2),
                   weight = rep(0.6, 6))
  r <- motifs(el, pattern = "triangle", min_transitions = 0,
              significance = TRUE, n_perm = 50, seed = 1)
  # 0.6 rounds to 1 stub; the old as.integer() truncation gave an all-zero
  # null (expected = 0 for every row)
  expect_true(any(r$results$expected > 0))
})

# ---- #12: n_random validation and honest degenerate-null statistics --------

test_that("motif_census rejects n_random < 2", {
  m <- cograph:::.get_triad_patterns_canonical()[["030C"]]
  expect_error(motif_census(m, directed = TRUE, n_random = 1))
  expect_error(motif_census(m, directed = TRUE, n_random = 0))
})

test_that(".motif_null_stats: empirical p, NA z on degenerate disagreement", {
  ns <- cograph:::.motif_null_stats(
    observed = c(a = 5, b = 3),
    null_counts = matrix(c(3, 3, 3, 3, 3, 3), nrow = 3,
                         dimnames = list(NULL, c("a", "b")))
  )
  expect_true(is.na(ns$z[["a"]]))     # sd = 0, observed != mean
  expect_identical(ns$z[["b"]], 0)    # sd = 0, observed == mean
  expect_identical(ns$p[["a"]], 1 / 4)  # (1 + 0) / (3 + 1)
  expect_identical(ns$p[["b"]], 1)
})
