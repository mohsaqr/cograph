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
               "conflicts")
})

test_that("motif_census rejects directed= conflicting with cograph input", {
  undirected <- matrix(1, 3, 3); diag(undirected) <- 0
  directed <- cograph:::.get_triad_patterns_canonical()[["030C"]]
  expect_error(
    motif_census(as_cograph(undirected, directed = FALSE), directed = TRUE,
                 n_random = 2),
    "conflicts"
  )
  expect_error(
    motif_census(as_cograph(directed, directed = TRUE), directed = FALSE,
                 n_random = 2),
    "conflicts"
  )
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

# ---- #13: all permutation entry points validate their sample size ---------

test_that("motif permutation counts must be finite whole numbers >= 2", {
  m <- cograph:::.get_triad_patterns_canonical()[["030C"]]
  dimnames(m) <- list(LETTERS[1:3], LETTERS[1:3])

  expect_error(motif_census(m, n_random = 2.5), "whole number")
  expect_error(motifs(m, pattern = "all", n_perm = 1,
                      min_transitions = 0), "`n_perm`")
  expect_error(extract_motifs(m, pattern = "all", n_perm = 1,
                              min_transitions = 0, significance = TRUE),
               "`n_perm`")
})

# ---- #14: balanced stubs and low-activity instance nulls ------------------

test_that("fractional configuration stubs have balanced margins", {
  m <- matrix(c(0, .3, .3,
                0,  0, .6,
                0,  0,  0), 3, 3, byrow = TRUE)
  stubs <- cograph:::.motif_configuration_stubs(m)

  expect_identical(length(stubs$rows), length(stubs$cols))
  expect_identical(length(stubs$rows), stubs$total)
  expect_identical(sum(stubs$row_degrees), sum(stubs$col_degrees))

  tiny <- cograph:::.motif_configuration_stubs(
    matrix(c(0, .4, 0, 0), 2, 2, byrow = TRUE)
  )
  expect_identical(tiny$total, 1L)
  expect_identical(tiny$row_degrees, c(1L, 0L))
  expect_identical(tiny$col_degrees, c(0L, 1L))

  dense <- matrix(.1, 4, 4); diag(dense) <- 0
  dense_stubs <- cograph:::.motif_configuration_stubs(dense)
  expect_identical(dense_stubs$total, 12L)
  expect_identical(dense_stubs$row_degrees, rep(3L, 4))
  expect_identical(dense_stubs$col_degrees, rep(3L, 4))

  el <- data.frame(actor = "s1", from = c("A", "A", "B"),
                   to = c("B", "C", "C"), weight = c(.3, .3, .6))
  expect_no_warning(
    motifs(el, pattern = "all", significance = TRUE, n_perm = 5,
           min_transitions = 0, min_count = NULL, seed = 1)
  )
})

test_that("instance null includes one-transition and empty units", {
  el <- rbind(
    data.frame(actor = "edge", from = "A", to = "B"),
    data.frame(actor = "loop", from = "C", to = "C")
  )
  r <- motifs(el, named_nodes = TRUE, pattern = "all",
              significance = TRUE, n_perm = 10, min_transitions = 0,
              min_count = NULL, seed = 1)

  got <- stats::setNames(r$results$expected, r$results$type)
  expect_identical(got[c("003", "012")], c("003" = 1, "012" = 1))
  expect_true(all(r$results$p == 1))
})

# ---- #15: legacy extractor agrees with the maintained instance API --------

test_that("extract_motifs retains each triple's per-type observations", {
  cyc <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))
  tr <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"))
  el <- rbind(transform(cyc, actor = "cycle"),
              transform(tr, actor = "transitive"))

  legacy <- extract_motifs(data = el, id = "actor", pattern = "all",
                           min_transitions = 0, significance = FALSE)
  current <- subgraphs(el, actor = "actor", pattern = "all",
                       min_transitions = 0, min_count = NULL,
                       significance = FALSE)

  legacy_rows <- legacy$results[order(legacy$results$type),
                                c("triad", "observed", "type")]
  current_rows <- current$results[order(current$results$type),
                                  c("triad", "observed", "type")]
  rownames(legacy_rows) <- rownames(current_rows) <- NULL
  expect_identical(legacy_rows, current_rows)
})

test_that("extract_motifs null tests the row's own MAN type", {
  el <- do.call(rbind, lapply(seq_len(10), function(id) {
    data.frame(actor = paste0("s", id),
               from = c("A", "B", "C"), to = c("B", "C", "A"))
  }))
  r <- extract_motifs(data = el, id = "actor", pattern = "triangle",
                      min_transitions = 0, significance = TRUE,
                      n_perm = 1000, seed = 1)

  row <- r$results[r$results$type == "030C", ]
  expect_equal(nrow(row), 1L)
  expect_lt(abs(row$expected - 10 * 2 / 6), 0.5)
})

test_that("extract_motifs include_types overrides explicit exclusions", {
  m <- cograph:::.get_triad_patterns_canonical()[["030C"]]
  dimnames(m) <- list(LETTERS[1:3], LETTERS[1:3])
  r <- extract_motifs(m, include_types = "030C", exclude_types = "030C",
                      min_transitions = 0)
  expect_identical(r$results$type, "030C")
})

# ---- #16: motif_census consistently projects multigraphs to simple graphs --

test_that("motif_census multigraph input equals its simple projection", {
  skip_if_not_installed("igraph")

  multi <- igraph::make_empty_graph(4, directed = TRUE) |>
    igraph::add_edges(c(1, 2, 1, 2, 2, 3, 3, 1, 4, 4))
  simple <- igraph::simplify(multi, remove.multiple = TRUE,
                             remove.loops = TRUE)

  got <- motif_census(multi, n_random = 10, seed = 9)
  ref <- motif_census(simple, n_random = 10, seed = 9)
  expect_identical(got, ref)
})

# ---- #17: empirical p-values are the single significance decision rule ----

test_that("aggregate motifs sig column follows its empirical p-value", {
  set.seed(7)
  m <- matrix(stats::rbinom(64, 1, .25), 8, 8)
  diag(m) <- 0
  dimnames(m) <- list(paste0("V", 1:8), paste0("V", 1:8))

  r <- motifs(m, pattern = "all", significance = TRUE, n_perm = 20,
              min_transitions = 0, seed = 17)
  expect_identical(r$results$sig, r$results$p < .05)
  # This fixture contains a z > 1.96 row whose empirical p is not < .05.
  expect_true(any(abs(r$results$z) > 1.96 & r$results$p >= .05,
                  na.rm = TRUE))
})

test_that("low-level motif bar and print use empirical significance", {
  df <- data.frame(motif = "201", count = 4, expected = .9, z = 2.66,
                   p = .0952, significant = FALSE)
  p <- cograph:::.plot_motifs_bar(
    df, colors = c("blue", "grey", "red"), directed = TRUE, size = 3)
  expect_identical(p$data$direction, "neutral")

  low <- data.frame(motif = "201", count = 4, null_mean = .9,
                    null_sd = 1, z_score = 2.66, p_value = .0952,
                    significant = FALSE)
  class(low) <- c("cograph_motifs", "data.frame")
  out <- capture.output(print(low))
  expect_true(any(grepl("Over-represented: 0", out, fixed = TRUE)))
})

# ---- #18: display separators in labels never become internal identifiers ---

test_that("instance significance supports labels containing ' - '", {
  el <- data.frame(
    actor = "s1",
    from = c("A", "B - C", "D", "A - B", "A - B", "C"),
    to = c("B - C", "D", "A", "C", "D", "D")
  )

  expect_no_warning(
    current <- subgraphs(el, actor = "actor", pattern = "all",
                         significance = TRUE, n_perm = 5,
                         min_transitions = 0, min_count = NULL, seed = 1)
  )
  expect_no_warning(
    legacy <- extract_motifs(data = el, id = "actor", pattern = "all",
                             significance = TRUE, n_perm = 5,
                             min_transitions = 0, seed = 1)
  )

  for (result in list(current, legacy)) {
    expect_false(".triad_key" %in% names(result$results))
    expect_true(all(c("node1", "node2", "node3") %in%
                    names(result$results)))
    expect_false(anyNA(result$results$p))
    collision <- result$results$triad == "A - B - C - D"
    expect_setequal(result$results$type[collision], c("030C", "030T"))
  }
})

test_that("structured node columns disambiguate identical triad text", {
  el <- rbind(
    data.frame(actor = "s1", from = c("A", "B - C", "D"),
               to = c("B - C", "D", "A")),
    data.frame(actor = "s2", from = c("A - B", "C", "D"),
               to = c("C", "D", "A - B"))
  )
  current <- subgraphs(el, actor = "actor", pattern = "triangle",
                       significance = TRUE, n_perm = 10,
                       min_transitions = 0, min_count = NULL, seed = 1)
  legacy <- extract_motifs(data = el, id = "actor", pattern = "triangle",
                           significance = TRUE, n_perm = 10,
                           min_transitions = 0, seed = 1)

  for (result in list(current, legacy)) {
    expect_identical(length(unique(result$results$triad)), 1L)
    node_keys <- apply(result$results[c("node1", "node2", "node3")], 1,
                       paste, collapse = "\r")
    expect_identical(length(unique(node_keys)), 2L)
    p <- plot(result, type = "significance", n = 20)
    expect_identical(nrow(p$data), 2L)
    expect_identical(length(unique(as.character(p$data$label))), 2L)
  }
})

test_that("extract_motifs matches permutations by internal triple key", {
  el <- data.frame(
    actor = rep(c("s1", "s2"), each = 2),
    from = rep(c("A", "C"), 2),
    to = rep(c("B", "C"), 2),
    weight = rep(c(1, 0), 2)
  )
  r <- extract_motifs(data = el, id = "actor", pattern = "all",
                      significance = TRUE, n_perm = 5,
                      min_transitions = 0.8, seed = 1)

  row <- r$results[r$results$type == "012", ]
  expect_identical(row$observed, 2L)
  expect_identical(row$expected, 2)
  expect_identical(row$p, 1)
})

test_that("legacy significance plot retains per-type rows and empirical colors", {
  x <- list(
    results = data.frame(
      triad = c("A - B - C", "A - B - C"),
      observed = c(5L, 3L), type = c("030C", "030T"),
      expected = c(1, 4), z = c(2.5, -2.5), p = c(.01, .2),
      sig = c("*", "")
    ),
    type_summary = table(c("030C", "030T")),
    params = list(significance = TRUE, n_perm = 100,
                  pattern = "all", edge_method = "any")
  )
  class(x) <- "cograph_motif_analysis"

  p <- plot(x, type = "significance", n = 20)
  expect_identical(nrow(p$data), 2L)
  expect_setequal(as.character(p$data$label),
                  c("A - B - C [030C]", "A - B - C [030T]"))
  expect_setequal(p$data$direction, c("over", "ns"))
})

test_that("fractional unit eligibility is frozen before permutations", {
  el <- data.frame(
    actor = rep(c("low", "high"), each = 2),
    from = rep(c("A", "C"), 2), to = rep(c("B", "C"), 2),
    weight = c(.6, 0, 1, 0)
  )

  current <- motifs(el, actor = "actor", pattern = "all",
                    significance = TRUE, n_perm = 5,
                    min_transitions = .8, min_count = NULL, seed = 1)
  current_row <- current$results[current$results$type == "012", ]
  expect_identical(current_row$count, 1L)
  expect_identical(current_row$expected, 1)

  legacy <- extract_motifs(data = el, id = "actor", pattern = "all",
                           significance = TRUE, n_perm = 5,
                           min_transitions = .8, seed = 1)
  legacy_row <- legacy$results[legacy$results$type == "012", ]
  expect_identical(legacy_row$observed, 1L)
  expect_identical(legacy_row$expected, 1)
})

test_that("sub-half transitions remain present in the null", {
  el <- data.frame(actor = "s1", from = c("A", "C"),
                   to = c("B", "C"), weight = c(.4, 0))
  current <- motifs(el, actor = "actor", pattern = "all",
                    significance = TRUE, n_perm = 20,
                    min_transitions = 0, min_count = NULL, seed = 1)
  row <- current$results[current$results$type == "012", ]
  expect_identical(row$expected, 1)
  expect_identical(row$p, 1)

  legacy <- extract_motifs(data = el, id = "actor", pattern = "all",
                           significance = TRUE, n_perm = 20,
                           min_transitions = 0, seed = 1)
  legacy_row <- legacy$results[legacy$results$type == "012", ]
  expect_identical(legacy_row$expected, 1)
  expect_identical(legacy_row$p, 1)
})

test_that("dense fractional support is not thinned by integerization", {
  states <- LETTERS[1:4]
  edges <- which(row(matrix(0, 4, 4)) != col(matrix(0, 4, 4)),
                 arr.ind = TRUE)
  fractional <- data.frame(actor = "s1", from = states[edges[, 1]],
                           to = states[edges[, 2]], weight = .1)
  binary <- fractional; binary$weight <- 1

  frac_result <- motifs(fractional, actor = "actor", pattern = "all",
                        significance = TRUE, n_perm = 100,
                        min_transitions = 0, min_count = NULL, seed = 1)
  binary_result <- motifs(binary, actor = "actor", pattern = "all",
                          significance = TRUE, n_perm = 100,
                          min_transitions = 0, min_count = NULL, seed = 1)
  expect_identical(frac_result$results, binary_result$results)
})

test_that("self-loops never become motif edges in individual nulls", {
  el <- data.frame(actor = "s", from = c("A", "B"),
                   to = c("A", "C"))

  current <- motifs(el, actor = "actor", pattern = "all",
                    significance = TRUE, n_perm = 100,
                    min_transitions = 0, min_count = NULL, seed = 1)
  current_row <- current$results[current$results$type == "012", ]
  expect_identical(current_row$count, 1L)
  expect_identical(current_row$expected, 1)
  expect_identical(current_row$p, 1)

  legacy <- extract_motifs(data = el, id = "actor", pattern = "all",
                           significance = TRUE, n_perm = 100,
                           min_transitions = 0, seed = 1)
  legacy_row <- legacy$results[legacy$results$type == "012", ]
  expect_identical(legacy_row$observed, 1L)
  expect_identical(legacy_row$expected, 1)
  expect_identical(legacy_row$p, 1)
})

test_that("one-node inputs remain valid empty motif analyses", {
  m <- matrix(0, 1, 1, dimnames = list("A", "A"))
  expect_message(
    expect_null(motifs(m, pattern = "all", significance = FALSE,
                       min_transitions = 0)),
    "No motifs"
  )
  expect_warning(
    expect_null(extract_motifs(m, pattern = "all", min_transitions = 0)),
    "No triads"
  )
})

test_that("returned p-values retain decision precision", {
  null <- matrix(c(rep(1, 49), rep(0, 951)), ncol = 1)
  ns <- cograph:::.motif_null_stats(1, null)
  expect_true(ns$significant)
  expect_lt(ns$p, .05)
  expect_identical(round(ns$p, 4), 0.05)

  set.seed(7)
  m <- matrix(stats::rbinom(64, 1, .25), 8, 8)
  diag(m) <- 0
  dimnames(m) <- list(paste0("V", 1:8), paste0("V", 1:8))
  result <- motifs(m, pattern = "all", significance = TRUE,
                   n_perm = 1000, min_transitions = 0, seed = 17)
  expect_identical(result$results$sig, result$results$p < .05)
})

# ---- 2026-08-13 review follow-up fixes --------------------------------------
# Regression tests for the confirmed findings of the post-remediation code
# review: fractional-count plot crash, degenerate-null (z = NA) rows being
# sorted/cut/hidden as if they were the weakest findings, and stub validation
# aborting runs over units the null never touches.

test_that("triads plot renders fractional aggregate weighted counts", {
  mat <- matrix(c(0, .3, .2, 0, 0, .5, .4, 0, 0), 3, 3, byrow = TRUE,
                dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  r <- subgraphs(mat, significance = FALSE, min_count = NULL,
                 pattern = "all", min_transitions = 0)
  expect_true(any(r$results$observed != round(r$results$observed)))

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  expect_no_error(plot(r, type = "triads"))
  grDevices::dev.off()
  unlink(tmp)
})

test_that("degenerate-null rows outrank finite z when sorting results", {
  z <- c(1.5, NA, -2, NA)
  p <- c(.20, .01, .04, NA)
  rank_abs <- cograph:::.motif_z_rank(z, p)
  expect_identical(rank_abs, c(1.5, Inf, 2, -Inf))
  rank_signed <- cograph:::.motif_z_rank(z, p, effect = c(1, 5, -1, 0))
  expect_identical(rank_signed, c(1.5, Inf, -2, -Inf))
})

test_that("top-N keeps degenerate-null rows instead of cutting them", {
  el <- data.frame(actor = rep(c("p1", "p2"), c(3, 1)),
                   from = c("A", "B", "C", "A"),
                   to   = c("B", "C", "A", "B"),
                   stringsAsFactors = FALSE)

  # seed = 1 makes both permutations miss the observed 030C cycle, so its
  # null is all-zero: z = NA with the smallest possible p, while the 012 row
  # has a finite z = 0. top = 1 must keep the degenerate row, not cut it.
  legacy <- extract_motifs(data = el, id = "actor", pattern = "all",
                           significance = TRUE, n_perm = 2,
                           min_transitions = 0, seed = 1, top = 1)
  expect_identical(legacy$results$type, "030C")
  expect_true(is.na(legacy$results$z))

  sg <- subgraphs(el, actor = "actor", pattern = "all", significance = TRUE,
                  n_perm = 2, min_transitions = 0, min_count = NULL,
                  seed = 1, top = 1)
  expect_identical(sg$results$type, "030C")
  expect_true(is.na(sg$results$z))
})

test_that("significance plots message about omitted degenerate-null rows", {
  el <- data.frame(actor = rep(c("p1", "p2"), c(3, 1)),
                   from = c("A", "B", "C", "A"),
                   to   = c("B", "C", "A", "B"),
                   stringsAsFactors = FALSE)

  legacy <- extract_motifs(data = el, id = "actor", pattern = "all",
                           significance = TRUE, n_perm = 2,
                           min_transitions = 0, seed = 1)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  expect_message(print(plot(legacy, type = "significance")),
                 "degenerate null")
  grDevices::dev.off()

  sg <- subgraphs(el, actor = "actor", pattern = "all", significance = TRUE,
                  n_perm = 2, min_transitions = 0, min_count = NULL, seed = 1)
  grDevices::png(tmp)
  expect_message(print(plot(sg, type = "significance")), "degenerate null")
  grDevices::dev.off()
  unlink(tmp)
})

test_that("malformed cells in null-ineligible units do not abort the run", {
  el <- data.frame(actor = rep(c("good", "bad"), c(6, 1)),
                   from = c("A", "B", "C", "A", "B", "C", "A"),
                   to   = c("B", "C", "A", "C", "A", "B", "B"),
                   weight = c(rep(1, 6), -0.4))

  r_census <- motifs(el, actor = "actor", pattern = "all",
                     significance = TRUE, n_perm = 20,
                     min_transitions = 5, min_count = NULL, seed = 1)
  expect_s3_class(r_census, "cograph_motif_result")

  r_instance <- subgraphs(el, actor = "actor", pattern = "all",
                          significance = TRUE, n_perm = 20,
                          min_transitions = 5, min_count = NULL, seed = 1)
  expect_s3_class(r_instance, "cograph_motif_result")

  r_legacy <- extract_motifs(data = el, id = "actor", pattern = "all",
                             significance = TRUE, n_perm = 20,
                             min_transitions = 5, seed = 1)
  expect_s3_class(r_legacy, "cograph_motif_analysis")

  # A malformed cell in an ELIGIBLE unit must still error loudly.
  el_bad <- data.frame(actor = "good",
                       from = c("A", "B", "C", "A", "B", "C", "D"),
                       to   = c("B", "C", "A", "C", "A", "B", "A"),
                       weight = c(rep(1, 6), -0.4))
  expect_error(
    motifs(el_bad, actor = "actor", pattern = "all", significance = TRUE,
           n_perm = 20, min_transitions = 5, min_count = NULL, seed = 1),
    "finite and non-negative"
  )
})

test_that(".motif_strip_loops zeroes every unit diagonal and nothing else", {
  arr <- array(as.numeric(seq_len(2 * 3 * 3)), dim = c(2, 3, 3))
  out <- cograph:::.motif_strip_loops(arr)
  diags <- vapply(1:2, function(ind) {
    diag(cograph:::.motif_unit_matrix(out, ind))
  }, numeric(3))
  expect_identical(unname(diags), matrix(0, 3, 2))

  off <- upper.tri(matrix(0, 3, 3)) | lower.tri(matrix(0, 3, 3))
  for (ind in 1:2) {
    expect_identical(cograph:::.motif_unit_matrix(out, ind)[off],
                     cograph:::.motif_unit_matrix(arr, ind)[off])
  }
})
