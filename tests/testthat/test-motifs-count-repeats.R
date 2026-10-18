# motifs(count_repeats = TRUE): repeated codes (A -> A) count toward
# min_transitions and are shuffled in the permutation null; observed triads
# never contain them. Default FALSE keeps the loop-free behaviour of 2.4.6+.

# Edge list of consecutive transitions, one session per sequence
.repeat_edges <- function(seqs) {
  rows <- lapply(names(seqs), \(id) {
    s <- seqs[[id]]
    data.frame(from = s[-length(s)], to = s[-1], session = id)
  })
  do.call(rbind, rows)
}

# Two sessions: S1 = A A A B C has 4 transitions, 2 between different codes;
# S2 = A B C A is a 3-cycle with no repeats.
.two_sessions <- .repeat_edges(list(S1 = c("A", "A", "A", "B", "C"),
                                    S2 = c("A", "B", "C", "A")))

# Random coded sessions with frequent repeats, for the null tests
.repeat_heavy <- function(n_sessions = 40, len = 12, seed = 7) {
  saved <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  }
  on.exit(if (!is.null(saved)) assign(".Random.seed", saved, envir = globalenv()),
          add = TRUE)
  set.seed(seed)
  seqs <- lapply(seq_len(n_sessions), \(i) {
    runs <- sample(c("A", "B", "C", "D"), len, replace = TRUE)
    rep(runs, times = sample(1:3, len, replace = TRUE))
  })
  .repeat_edges(stats::setNames(seqs, paste0("S", seq_len(n_sessions))))
}

test_that("count_repeats = TRUE counts repeats toward min_transitions", {
  base <- motifs(.two_sessions, pattern = "all", significance = FALSE,
                 min_transitions = 3)
  with_rep <- motifs(.two_sessions, pattern = "all", significance = FALSE,
                     min_transitions = 3, count_repeats = TRUE)
  expect_identical(base$n_units, 1L)
  expect_identical(with_rep$n_units, 2L)
  expect_identical(with_rep$n_units_total, 2L)
  # S1 joins with its loop-free triad A -> B -> C (021C), not with any loop
  expect_identical(as.data.frame(base, what = "types")$type, "030C")
  expect_setequal(as.data.frame(with_rep, what = "types")$type,
                  c("021C", "030C"))
  expect_true(with_rep$params$count_repeats)
  expect_false(base$params$count_repeats)
})

test_that("observed counts do not depend on count_repeats when every unit qualifies", {
  el <- .repeat_heavy()
  base <- motifs(el, pattern = "all", significance = FALSE, min_transitions = 0)
  with_rep <- motifs(el, pattern = "all", significance = FALSE,
                     min_transitions = 0, count_repeats = TRUE)
  expect_identical(base$n_units, with_rep$n_units)
  expect_identical(as.data.frame(base, what = "types"),
                   as.data.frame(with_rep, what = "types"))
})

test_that("count_repeats = TRUE shuffles repeats into the null", {
  el <- .repeat_heavy()
  base <- motifs(el, n_perm = 200, seed = 1, min_transitions = 0)
  with_rep <- motifs(el, n_perm = 200, seed = 1, min_transitions = 0,
                     count_repeats = TRUE)
  b <- as.data.frame(base)
  r <- as.data.frame(with_rep)
  r <- r[match(b$type, r$type), ]
  expect_identical(r$count, b$count)
  # Repeat-heavy sessions: shuffling the repeats adds ties between different
  # codes, so the null expects more of the densest closed triad than the
  # loop-free null does.
  expect_gt(r$expected[b$type == "300"], b$expected[b$type == "300"])
})

test_that("count_repeats applies to instance mode as well", {
  el <- .repeat_heavy()
  base <- subgraphs(el, n_perm = 100, seed = 1, min_transitions = 0)
  with_rep <- subgraphs(el, n_perm = 100, seed = 1, min_transitions = 0,
                        count_repeats = TRUE)
  b <- as.data.frame(base)
  r <- as.data.frame(with_rep)
  expect_identical(nrow(b), nrow(r))
  expect_false(isTRUE(all.equal(b$expected, r$expected)))
})

test_that("the default equals count_repeats = FALSE", {
  el <- .repeat_heavy()
  expect_identical(motifs(el, n_perm = 100, seed = 1),
                   motifs(el, n_perm = 100, seed = 1, count_repeats = FALSE))
})

test_that("keep_loops keeps the diagonal in the stub pool", {
  mat <- matrix(c(2, 0, 1, 0), 2, 2)  # A -> A twice, A -> B once
  without <- cograph:::.motif_configuration_stubs(mat)
  with_loops <- cograph:::.motif_configuration_stubs(mat, keep_loops = TRUE)
  expect_identical(without$total, 1L)
  expect_identical(with_loops$total, 3L)
  expect_identical(with_loops$rows, c(1L, 1L, 1L))
  expect_identical(with_loops$cols, c(1L, 1L, 2L))
})

test_that("count_repeats must be TRUE or FALSE", {
  expect_error(motifs(.two_sessions, count_repeats = NA), "count_repeats")
  expect_error(motifs(.two_sessions, count_repeats = "yes"), "count_repeats")
})
