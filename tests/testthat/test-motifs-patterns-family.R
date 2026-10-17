# Labelled patterns, full-family testing, p-value adjustment and unit counts
# for motifs()/subgraphs(). Expected values are worked out by hand in the
# comments next to each fixture.

cycle_units <- function(n_fwd, n_rev) {
  fwd <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))
  rev <- data.frame(from = c("A", "C", "B"), to = c("C", "B", "A"))
  units <- c(rep(list(fwd), n_fwd), rep(list(rev), n_rev))
  do.call(rbind, Map(function(u, id) transform(u, actor = sprintf("u%02d", id)),
                     units, seq_along(units)))
}

test_that("strict = FALSE reports the modal labelled pattern of each row", {
  # Three units run A -> B -> C -> A and one runs the reverse cycle. Both are
  # 030C, so the row pools them. Edge code bits (node1 = A, node2 = B,
  # node3 = C): A->B = 1, B->A = 2, A->C = 4, C->A = 8, B->C = 16, C->B = 32.
  # Forward cycle = 1 + 8 + 16 = 25; reverse = 2 + 4 + 32 = 38.
  sg <- subgraphs(cycle_units(3, 1), significance = FALSE, min_count = 1,
                  min_transitions = 1, strict = FALSE)
  row <- as.data.frame(sg)
  expect_identical(nrow(row), 1L)
  expect_identical(row$type, "030C")
  expect_identical(row$observed, 4L)
  expect_identical(row$pattern_code, 25L)
  expect_identical(row$pattern_n, 3L)
  expect_identical(row$pattern, "A -> B, C -> A, B -> C")
})

test_that("modal pattern ties go to the smallest edge code", {
  sg <- subgraphs(cycle_units(2, 2), significance = FALSE, min_count = 1,
                  min_transitions = 1, strict = FALSE)
  row <- as.data.frame(sg)
  expect_identical(row$pattern_code, 25L)
  expect_identical(row$pattern_n, 2L)
})

test_that("strict rows keep each labelled pattern apart", {
  # Three forward cycles (code 25) and one reverse cycle (code 38): same 030C
  # shape, different arrows, so strict matching gives two rows.
  sg <- subgraphs(cycle_units(3, 1), significance = FALSE, min_count = 1,
                  min_transitions = 1)
  res <- as.data.frame(sg)
  expect_identical(res$type, c("030C", "030C"))
  expect_identical(res$pattern_code, c(25L, 38L))
  expect_identical(res$observed, c(3L, 1L))
  expect_identical(res$pattern_n, res$observed)
  expect_identical(res$pattern, c("A -> B, C -> A, B -> C",
                                  "B -> A, A -> C, C -> B"))
  # Pooling the two rows recovers the shape-level count.
  pooled <- as.data.frame(subgraphs(cycle_units(3, 1), significance = FALSE,
                                    min_count = 1, min_transitions = 1,
                                    strict = FALSE))
  expect_identical(sum(res$observed), pooled$observed)
})

test_that("strict full family has one row per triple and labelled pattern", {
  el <- rbind(cycle_units(4, 1),
              data.frame(from = c("A", "B", "D"), to = c("B", "D", "A"),
                         actor = "d1"))
  res <- as.data.frame(subgraphs(el, n_perm = 200, seed = 3, min_count = NULL,
                                 min_transitions = 1, family = "all"))
  # The 7 closed types hold 2 + 6 + 6 + 3 + 3 + 6 + 1 = 27 labelled patterns;
  # 4 nodes give choose(4, 3) = 4 triples.
  expect_identical(nrow(res), 4L * 27L)
  expect_false(anyDuplicated(res[c("triad", "pattern_code")]) > 0)
  expect_true(all(!is.na(res$pattern)))
  lookup <- cograph:::.get_triad_lookup()
  expect_identical(res$type, unname(lookup[res$pattern_code + 1L]))
  expect_identical(sum(res$observed), 6L)
})

test_that("strict must be a single logical", {
  expect_error(subgraphs(cycle_units(2, 0), strict = "yes"), "strict")
})

test_that("pattern labels read mutual and one-way edges correctly", {
  # A <-> B, A -> C, nothing between B and C: 1 + 2 + 4 = 7.
  expect_identical(
    cograph:::.motif_pattern_label(7L, "A", "B", "C"),
    "A <-> B, A -> C"
  )
  expect_identical(cograph:::.motif_pattern_label(0L, "A", "B", "C"), "none")
  expect_identical(cograph:::.motif_pattern_label(NA_integer_, "A", "B", "C"),
                   NA_character_)
})

test_that("edge codes round-trip through the drawing matrix", {
  # Every code maps to the adjacency whose bits it encodes.
  rows <- c(1L, 2L, 1L, 3L, 2L, 3L)
  cols <- c(2L, 1L, 3L, 1L, 3L, 2L)
  decoded <- vapply(0:63, function(code) {
    mat <- cograph:::.motif_code_matrix(code)
    as.integer(sum(mat[cbind(rows, cols)] * 2^(0:5))) + 100L * sum(diag(mat))
  }, integer(1))
  expect_identical(decoded, 0:63)
})

test_that("the triads plot draws each row's own pattern", {
  skip_on_cran()
  sg <- subgraphs(cycle_units(3, 1), significance = FALSE, min_count = 1,
                  min_transitions = 1)
  drawn <- integer(0)
  local_mocked_bindings(.motif_code_matrix = function(code) {
    drawn <<- c(drawn, code)
    matrix(0L, 3, 3)
  })
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(sg, type = "triads")
  expect_identical(drawn, as.data.frame(sg)$pattern_code)
})

test_that("n_units counts the units analysed, not the units supplied", {
  # Two 3-transition units and one 1-transition unit; min_transitions = 2
  # drops the last.
  el <- rbind(cycle_units(2, 0),
              data.frame(from = "A", to = "B", actor = "short"))
  sg <- subgraphs(el, significance = FALSE, min_count = 1, min_transitions = 2)
  expect_identical(sg$n_units, 2L)
  expect_identical(sg$n_units_total, 3L)
  expect_output(print(sg), "2 of 3 units analysed (1 below min_transitions = 2)",
                fixed = TRUE)
})

test_that("family = 'all' tests every triple and type, observed or not", {
  el <- rbind(cycle_units(4, 1),
              data.frame(from = c("A", "B", "D"), to = c("B", "D", "A"),
                         actor = "d1"))
  full <- subgraphs(el, n_perm = 200, seed = 3, min_count = NULL,
                    min_transitions = 1, family = "all", strict = FALSE)
  res <- as.data.frame(full)
  # 4 nodes -> choose(4, 3) = 4 triples, crossed with the 7 closed types.
  expect_identical(nrow(res), 28L)
  expect_false(anyDuplicated(res[c("triad", "type")]) > 0)
  unobserved <- res[res$observed == 0L, ]
  expect_gt(nrow(unobserved), 0L)
  expect_true(all(is.na(unobserved$pattern)))
  expect_true(all(unobserved$pattern_n == 0L))
  # Observed counts agree with the default (observed-only) family.
  obs <- as.data.frame(subgraphs(el, significance = FALSE, min_count = 1,
                                 min_transitions = 1, strict = FALSE))
  merged <- merge(obs, res, by = c("triad", "type"))
  expect_identical(nrow(merged), nrow(obs))
  expect_identical(merged$observed.x, merged$observed.y)
})

test_that("min_count filters what family = 'all' shows, not what it tests", {
  el <- rbind(cycle_units(4, 1),
              data.frame(from = c("A", "B", "D"), to = c("B", "D", "A"),
                         actor = "d1"))
  full <- as.data.frame(subgraphs(el, n_perm = 200, seed = 3, min_count = NULL,
                                  min_transitions = 1, family = "all",
                                  p_adjust = "BH", strict = FALSE))
  shown <- as.data.frame(subgraphs(el, n_perm = 200, seed = 3, min_count = 1,
                                   min_transitions = 1, family = "all",
                                   p_adjust = "BH", strict = FALSE))
  expect_true(all(shown$observed >= 1L))
  expect_lt(nrow(shown), nrow(full))
  # Same null, same family: the shown rows carry the full-family adjustment.
  key <- function(d) paste(d$triad, d$type)
  expect_equal(shown$p_adj, full$p_adj[match(key(shown), key(full))])
  expect_equal(full$p_adj, stats::p.adjust(full$p, "BH"))
})

test_that("p_adjust fills p_adj and drives sig", {
  el <- cycle_units(6, 1)
  none <- as.data.frame(subgraphs(el, n_perm = 200, seed = 1, min_count = 1,
                                  min_transitions = 1))
  expect_identical(none$p_adj, none$p)
  expect_identical(none$sig, none$p < 0.05)
  holm <- as.data.frame(subgraphs(el, n_perm = 200, seed = 1, min_count = 1,
                                  min_transitions = 1, family = "all",
                                  p_adjust = "holm"))
  expect_identical(holm$sig, holm$p_adj < 0.05)
  expect_error(subgraphs(el, p_adjust = "bonf"), "p.adjust.methods")
})

test_that("expected and z are stored unrounded", {
  el <- cycle_units(6, 1)
  res <- as.data.frame(subgraphs(el, n_perm = 300, seed = 2, min_count = 1,
                                 min_transitions = 1))
  # 300 replicates of counts: a mean rounded to one decimal would be a
  # multiple of 0.1; the exact Monte Carlo mean is a multiple of 1/300.
  expect_equal(res$expected * 300, round(res$expected * 300))
  expect_false(isTRUE(all.equal(res$expected, round(res$expected, 1))))
})

test_that("family = 'all' controls the false-positive rate under the null", {
  skip_on_cran()
  # Data drawn from the null: every unit's targets are shuffled, so no triad
  # is special. Testing only observed rows conditions on the outcome; testing
  # the full family does not. Seeded, so the rates below are deterministic.
  labels <- LETTERS[1:6]
  null_data <- function(r) {
    set.seed(r)
    do.call(rbind, lapply(seq_len(25), function(u) {
      from <- sample(labels, 6, replace = TRUE)
      data.frame(from = from, to = sample(labels, 6, replace = TRUE),
                 actor = sprintf("u%02d", u))
    }))
  }
  rates <- vapply(seq_len(30), function(r) {
    el <- null_data(r)
    all_rows <- as.data.frame(subgraphs(el, n_perm = 200, seed = r,
                                        min_count = NULL, min_transitions = 1,
                                        family = "all"))
    obs_rows <- as.data.frame(subgraphs(el, n_perm = 200, seed = r,
                                        min_count = 1, min_transitions = 1))
    c(all_sig = sum(all_rows$sig), all_n = nrow(all_rows),
      obs_sig = sum(obs_rows$sig), obs_n = nrow(obs_rows))
  }, numeric(4))
  rate_all <- sum(rates["all_sig", ]) / sum(rates["all_n", ])
  rate_obs <- sum(rates["obs_sig", ]) / sum(rates["obs_n", ])
  expect_lte(rate_all, 0.05)
  expect_gt(rate_obs, rate_all)
})

test_that("triad captions show three-decimal p and the adjusted p when used", {
  caption <- cograph:::.motif_stats_caption
  # 0.003 used to print as "p=0.00" under two-decimal rounding.
  expect_identical(caption("n=10", 3.27, 0.003), "n=10 z=3.3 p=0.003")
  expect_identical(caption("n=21", 4.39, 0.00008), "n=21 z=4.4 p<.001")
  expect_identical(caption("n=21", 4.39, 0.031, adjusted = TRUE),
                   "n=21 z=4.4 p_adj=0.031")
  expect_identical(caption("n=1", NA, NA), "n=1 z=NA p=NA")
})

test_that("defaults keep 3-transition units and drop single-unit patterns", {
  # Two forward cycles and one reverse cycle, three transitions each. Every
  # unit passes min_transitions = 3; the reverse pattern occurs once and falls
  # below min_count = 2.
  sg <- subgraphs(cycle_units(2, 1), significance = FALSE)
  expect_identical(sg$n_units, 3L)
  res <- as.data.frame(sg)
  expect_identical(res$pattern_code, 25L)
  expect_identical(res$observed, 2L)
  expect_identical(sg$params$min_transitions, 3)
  expect_identical(sg$params$min_count, 2L)
})

test_that("extract_motifs() keeps units with three transitions by default", {
  el <- cycle_units(3, 0)
  res <- extract_motifs(data = el, id = "actor", level = "individual")
  expect_identical(as.integer(sum(res$results$observed)), 3L)
})

test_that("census mode keeps min_transitions = 5 by default", {
  # Three 3-transition cycles: instance mode analyses them, census mode
  # drops them under its unchanged threshold.
  el <- cycle_units(3, 0)
  expect_identical(subgraphs(el, significance = FALSE)$n_units, 3L)
  expect_null(suppressMessages(motifs(el, significance = FALSE)))
  expect_identical(motifs(el, significance = FALSE,
                          min_transitions = 3)$params$min_transitions, 3)
})
