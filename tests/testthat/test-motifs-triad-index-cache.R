# Performance refactor of the triad counters: cached triple indices, a shared
# edge-indicator step, and a class-count counter that skips materialising the
# triple table. These tests pin the contract that the refactor must not change
# any result, only how fast it is produced.

.tic_matrix <- function(s, density, seed, max_weight = 7L) {
  set.seed(seed)
  m <- matrix(stats::rbinom(s * s, 1L, density) *
                sample.int(max_weight, s * s, replace = TRUE), s, s)
  diag(m) <- 0
  m
}

.tic_expected <- function(mat) {
  total <- sum(mat)
  if (total == 0) return(NULL)
  e <- outer(rowSums(mat), colSums(mat)) / total
  e[e == 0] <- 0.001
  e
}

# ---- .triad_indices() ------------------------------------------------------

test_that(".triad_indices() reproduces combn() and indexes the right cells", {
  s <- 7L
  idx <- cograph:::.triad_indices(s)
  combos <- utils::combn(s, 3)

  expect_identical(idx$i, as.integer(combos[1, ]))
  expect_identical(idx$j, as.integer(combos[2, ]))
  expect_identical(idx$k, as.integer(combos[3, ]))
  expect_identical(idx$n, ncol(combos))

  # The linear indices must read exactly what cbind() indexing reads.
  mat <- .tic_matrix(s, 0.5, seed = 11)
  i <- idx$i; j <- idx$j; k <- idx$k
  expect_identical(mat[idx$ij], mat[cbind(i, j)])
  expect_identical(mat[idx$ji], mat[cbind(j, i)])
  expect_identical(mat[idx$ik], mat[cbind(i, k)])
  expect_identical(mat[idx$ki], mat[cbind(k, i)])
  expect_identical(mat[idx$jk], mat[cbind(j, k)])
  expect_identical(mat[idx$kj], mat[cbind(k, j)])
})

test_that(".triad_indices() caches small s and refuses to pin large s", {
  small <- 9L
  key_small <- paste0(".triad_idx_", small)
  if (exists(key_small, envir = cograph:::.cograph_cache)) {
    rm(list = key_small, envir = cograph:::.cograph_cache)
  }
  first <- cograph:::.triad_indices(small)
  expect_true(exists(key_small, envir = cograph:::.cograph_cache))
  expect_identical(cograph:::.triad_indices(small), first)

  # A large s is a single-call census; caching it would pin a large index set
  # for the whole session, so it must be rebuilt each time.
  large <- 70L
  key_large <- paste0(".triad_idx_", large)
  if (exists(key_large, envir = cograph:::.cograph_cache)) {
    rm(list = key_large, envir = cograph:::.cograph_cache)
  }
  big <- cograph:::.triad_indices(large)
  expect_identical(big$n, ncol(utils::combn(large, 3)))
  expect_false(exists(key_large, envir = cograph:::.cograph_cache))
})

# ---- .triad_type_index() ---------------------------------------------------

test_that(".triad_type_index() maps every 6-bit code to its MAN class", {
  man <- cograph:::.triad_type_names()
  ti <- cograph:::.triad_type_index()

  expect_length(ti, 64L)
  expect_false(anyNA(ti))
  expect_identical(man[ti], cograph:::.get_triad_lookup())
  # Code 0 is the empty triad.
  expect_identical(man[ti[1L]], "003")
  # No non-empty code may classify as 003, which is what lets the class
  # counter zero an excluded 003 instead of filtering edgeless triples.
  expect_false(any(man[ti[-1L]] == "003"))
})

# ---- .count_triad_types() vs the triple-level counter ----------------------

test_that(".count_triad_types() equals tabulating the triple table", {
  configs <- list(
    list(method = "any", threshold = 0, exclude = character(0), include = NULL),
    list(method = "any", threshold = 0, exclude = "003", include = NULL),
    list(method = "any", threshold = 0, exclude = character(0),
         include = c("030T", "120C", "300", "021C")),
    list(method = "percent", threshold = 1.5, exclude = "003", include = NULL),
    list(method = "percent", threshold = 0.25, exclude = character(0),
         include = NULL),
    list(method = "expected", threshold = 1, exclude = "003", include = NULL)
  )
  man <- cograph:::.triad_type_names()
  checked <- 0L

  for (s in c(3L, 5L, 9L, 16L)) {
    for (density in c(0, 0.1, 0.45, 0.9)) {
      mat <- .tic_matrix(s, density, seed = s * 100L + density * 10L)
      expected_mat <- .tic_expected(mat)
      for (cfg in configs) {
        if (cfg$method == "expected" && is.null(expected_mat)) next
        counts <- cograph:::.count_triad_types(
          mat, cfg$method, cfg$threshold,
          expected_mat = if (cfg$method == "expected") expected_mat else NULL,
          exclude = cfg$exclude, include = cfg$include
        )
        triples <- cograph:::.count_triads_matrix_vectorized(
          mat, cfg$method, cfg$threshold,
          expected_mat = if (cfg$method == "expected") expected_mat else NULL,
          exclude = cfg$exclude, include = cfg$include
        )
        reference <- stats::setNames(integer(length(man)), man)
        if (!is.null(triples) && nrow(triples) > 0) {
          tab <- table(triples$type)
          reference[names(tab)] <- as.integer(tab)
        }
        expect_identical(counts, reference)
        checked <- checked + 1L
      }
    }
  }
  expect_gt(checked, 60L)
})

test_that(".count_triad_types() returns a zeroed census below three nodes", {
  man <- cograph:::.triad_type_names()
  counts <- cograph:::.count_triad_types(matrix(0, 2, 2), "any", 0)
  expect_identical(counts, stats::setNames(integer(length(man)), man))
})

# ---- contract preserved by the shared indicator step ----------------------

test_that("the expected_mat contract still fires only after the empty check", {
  # An edgeless unit with no expected matrix returned NULL before the edge
  # rule was ever consulted; splitting the weights out of the indicators must
  # keep that order, or empty units start erroring mid-permutation.
  empty <- matrix(0, 5, 5)
  expect_null(cograph:::.count_triads_matrix_vectorized(
    empty, "expected", 1, expected_mat = NULL, exclude = "003"
  ))

  # With 003 admitted there is nothing to short-circuit, so the contract holds.
  populated <- .tic_matrix(5L, 0.6, seed = 5)
  expect_error(
    cograph:::.count_triads_matrix_vectorized(
      populated, "expected", 1, expected_mat = NULL
    ),
    "expected_mat required"
  )
})

# ---- end-to-end reproducibility -------------------------------------------

test_that("seeded census significance is unchanged and reproducible", {
  skip_if_not_installed("tna")
  model <- tna::tna(tna::group_regulation)

  first <- motifs(model, n_perm = 9L, seed = 4)
  second <- motifs(model, n_perm = 9L, seed = 4)
  expect_identical(first$results, second$results)

  # A different seed must actually move the null, otherwise the permutation
  # loop is not consuming the RNG stream the way it used to.
  other <- motifs(model, n_perm = 9L, seed = 5)
  expect_false(identical(first$results$expected, other$results$expected))
})

# ---- cores = : parallel permutation null ----------------------------------

# R CMD check sets _R_CHECK_LIMIT_CORES_ (always under --as-cran), and
# parallel:::.check_ncores() then ERRORS on any request above 2 workers:
#   Error in .check_ncores(cores): 4 simultaneous processes spawned
# The tests below deliberately use 3 and 4 workers to prove that results do not
# depend on how replicates are chunked, so they cannot run under that limit.
# skip_on_cran() does not cover this: r-lib/actions sets NOT_CRAN=true, so the
# check runs with --as-cran while skip_on_cran() stays inactive -- which is why
# these failed on ubuntu and windows while passing locally and on macOS.
.cores_limited <- function(n) {
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  nzchar(chk) && chk != "false" && n > 2L
}


test_that(".motif_validate_cores() enforces its contract and caps at detected", {
  expect_identical(cograph:::.motif_validate_cores(1), 1L)
  expect_identical(cograph:::.motif_validate_cores(2L), 2L)

  # Assert the class, not the message text: messages change, classes are the
  # contract.
  for (bad in list(0, -3, 2.5, c(2, 4), NA_integer_, "2", NULL)) {
    expect_error(cograph:::.motif_validate_cores(bad),
                 class = "cograph_bad_cores")
  }

  available <- parallel::detectCores()
  skip_if(is.na(available))
  expect_warning(
    capped <- cograph:::.motif_validate_cores(available + 10L),
    class = "cograph_cores_capped"
  )
  expect_identical(capped, as.integer(available))
})

test_that(".motif_rng_streams() yields distinct reproducible streams", {
  a <- cograph:::.motif_rng_streams(5L, seed = 3)
  b <- cograph:::.motif_rng_streams(5L, seed = 3)
  expect_length(a, 5L)
  expect_identical(a, b)
  expect_false(identical(a[[1]], a[[2]]))
  expect_false(identical(a, cograph:::.motif_rng_streams(5L, seed = 4)))

  # The caller's RNG kind must survive the stream construction.
  before <- RNGkind()
  invisible(cograph:::.motif_rng_streams(3L, seed = 1))
  expect_identical(RNGkind(), before)
})

test_that(".motif_run_replicates() is stream-driven, not order-driven", {
  streams <- cograph:::.motif_rng_streams(6L, seed = 8)
  draw <- function(p) stats::runif(1)

  serial <- cograph:::.motif_run_replicates(6L, 1L, streams, draw)
  again <- cograph:::.motif_run_replicates(6L, 1L, streams, draw)
  expect_identical(serial, again)

  skip_on_os("windows")
  skip_if(is.na(parallel::detectCores()) || parallel::detectCores() < 2)
  # Each replicate draws from its own stream, so farming them out must not
  # change any value - this is what makes a result core-count independent.
  expect_identical(cograph:::.motif_run_replicates(6L, 2L, streams, draw),
                   serial)
})

test_that("cores > 1 is core-count independent and leaves cores = 1 alone", {
  skip_if_not_installed("tna")
  skip_on_os("windows")
  skip_on_cran()
  skip_if(.cores_limited(4L), "R CMD check limits parallel workers to 2")
  skip_if(is.na(parallel::detectCores()) || parallel::detectCores() < 4)

  model <- tna::tna(tna::group_regulation)
  run <- function(...) motifs(model, n_perm = 12L, seed = 11, ...)$results

  serial <- run(cores = 1)
  expect_identical(serial, run(cores = 1))

  p2 <- run(cores = 2)
  p4 <- run(cores = 4)
  expect_identical(p2, p4)
  expect_identical(p4, run(cores = 4))

  # Parallel uses independent per-replicate streams by design, so it must not
  # be expected to reproduce the serial draws.
  expect_false(identical(serial$expected, p4$expected))

  # The observed census is not permuted, so it cannot move.
  expect_identical(serial[order(serial$type), "count"],
                   p4[order(p4$type), "count"])
})

# ---- .motif_triad_pair_counts() : the instance-level null's counter --------

test_that(".motif_triad_pair_counts() counts units per (triple, class) pair", {
  set.seed(19)
  s <- 7L
  n_units <- 6L
  arr <- array(stats::rpois(n_units * s * s, 1.1), dim = c(n_units, s, s))
  for (u in seq_len(n_units)) arr[u, , ][cbind(seq_len(s), seq_len(s))] <- 0L
  idx <- cograph:::.triad_indices(s)
  units <- seq_len(n_units)

  configs <- list(
    list(exclude = character(0), include = NULL),
    list(exclude = "003", include = NULL),
    list(exclude = character(0), include = c("030T", "120C", "300"))
  )

  for (cfg in configs) {
    bins <- cograph:::.motif_triad_pair_counts(
      arr, units, idx, "any", 0, cfg$exclude, cfg$include
    )
    # Reference: tabulate the triple tables the old path would have built.
    reference <- integer(length(bins))
    man <- cograph:::.triad_type_names()
    for (u in units) {
      td <- cograph:::.count_triads_matrix_vectorized(
        cograph:::.motif_unit_matrix(arr, u), "any", 0,
        exclude = cfg$exclude, include = cfg$include
      )
      if (is.null(td) || nrow(td) == 0) next
      pos <- match(paste(td$i, td$j, td$k, sep = "\r"),
                   paste(idx$i, idx$j, idx$k, sep = "\r"))
      bin <- (match(td$type, man) - 1L) * idx$n + pos
      reference[bin] <- reference[bin] + 1L
    }
    expect_identical(bins, reference)
  }
})

test_that(".motif_triad_pair_counts() honours the aggregate weight filter", {
  set.seed(23)
  s <- 6L
  arr <- array(stats::rpois(s * s, 2), dim = c(1L, s, s))
  arr[1, , ][cbind(seq_len(s), seq_len(s))] <- 0L
  idx <- cograph:::.triad_indices(s)

  unfiltered <- cograph:::.motif_triad_pair_counts(
    arr, 1L, idx, "any", 0, "003", NULL
  )
  filtered <- cograph:::.motif_triad_pair_counts(
    arr, 1L, idx, "any", 0, "003", NULL, min_weight = 12
  )
  expect_true(sum(filtered) < sum(unfiltered))

  # Matches the weight filter the triple-level path applies.
  td <- cograph:::.count_triads_matrix_vectorized(
    cograph:::.motif_unit_matrix(arr, 1L), "any", 0, exclude = "003"
  )
  expect_identical(sum(filtered), sum(td$weight >= 12))
})

test_that(".motif_triad_pair_counts() returns an empty census for no units", {
  idx <- cograph:::.triad_indices(5L)
  arr <- array(0L, dim = c(1L, 5L, 5L))
  bins <- cograph:::.motif_triad_pair_counts(
    arr, integer(0), idx, "any", 0, character(0), NULL
  )
  expect_identical(bins, integer(idx$n * 16L))
  # Every class excluded is also an empty census, not an error.
  expect_identical(
    cograph:::.motif_triad_pair_counts(arr, 1L, idx, "any", 0,
                                       cograph:::.triad_type_names(), NULL),
    integer(idx$n * 16L)
  )
})

test_that(".motif_triad_pair_bins() addresses the counter's layout", {
  idx <- cograph:::.triad_indices(6L)
  man <- cograph:::.triad_type_names()
  keys <- paste(idx$i, idx$j, idx$k, sep = "\r")

  expect_identical(
    cograph:::.motif_triad_pair_bins(keys[c(1L, 4L)], c(man[1L], man[3L]), idx),
    c(0L * idx$n + 1L, 2L * idx$n + 4L)
  )
  # A triple that does not exist at this state count addresses nothing.
  expect_true(is.na(
    cograph:::.motif_triad_pair_bins("99\r99\r99", man[1L], idx)
  ))
})

test_that("extract_motifs() significance is reproducible for a seed", {
  skip_if_not_installed("tna")
  model <- tna::tna(tna::group_regulation)
  a <- extract_motifs(model, n_perm = 8L, seed = 2, significance = TRUE)
  b <- extract_motifs(model, n_perm = 8L, seed = 2, significance = TRUE)
  expect_identical(a, b)
  expect_false(identical(
    a$results$expected,
    extract_motifs(model, n_perm = 8L, seed = 3, significance = TRUE)$results$expected
  ))
})

# ---- parallel failure must not become corrupt statistics -------------------

test_that("a failed replicate raises instead of corrupting the null", {
  skip_on_os("windows")
  skip_on_cran()
  skip_if(is.na(parallel::detectCores()) || parallel::detectCores() < 2)

  streams <- cograph:::.motif_rng_streams(4L, seed = 1)
  boom <- function(p) if (p == 2L) stop("boom") else c(a = 1L, b = 2L)

  # mclapply() returns "try-error" entries rather than raising. Binding those
  # into a numeric matrix coerces it to character silently, so the check must
  # happen before the caller ever sees the results.
  expect_error(
    suppressWarnings(cograph:::.motif_run_replicates(4L, 2L, streams, boom)),
    class = "cograph_parallel_failure"
  )
  expect_error(
    suppressWarnings(cograph:::.motif_run_replicates(4L, 2L, streams, boom)),
    "boom"
  )
})

test_that(".motif_check_replicates() passes clean results through", {
  good <- list(c(a = 1L), c(a = 2L))
  expect_identical(cograph:::.motif_check_replicates(good, 2L, 1L), good)

  bare <- list(c(a = 1L), structure("failed", class = "try-error"))
  expect_error(cograph:::.motif_check_replicates(bare, 2L, 1L),
               class = "cograph_parallel_failure")
  # A try-error with no recorded condition must still raise, not subscript-fail.
  expect_error(cograph:::.motif_check_replicates(bare, 2L, 1L),
               "no condition recorded")
})

test_that("an undetectable core count is reported rather than passed silently", {
  testthat::local_mocked_bindings(
    detectCores = function(...) NA_integer_, .package = "parallel"
  )
  expect_silent(expect_identical(cograph:::.motif_validate_cores(1L), 1L))
  expect_warning(
    got <- cograph:::.motif_validate_cores(6L),
    class = "cograph_cores_undetected"
  )
  expect_identical(got, 6L)
})

# ---- the PSOCK backend, exercised on any platform --------------------------

test_that("the PSOCK backend receives the streams and matches serial", {
  skip_on_cran()
  skip_if(is.na(parallel::detectCores()) || parallel::detectCores() < 2)

  # PSOCK clusters work everywhere; Windows merely defaults to them. Forcing
  # the branch here is the only way this path is ever exercised in CI on a
  # non-Windows runner -- and it shipped broken twice: once because the worker
  # never received the captured `streams`, and once because a `fun =` argument
  # collided with parLapply()'s own parameter of that name.
  real_platform <- .Platform
  testthat::local_mocked_bindings(
    .Platform = c(real_platform[setdiff(names(real_platform), "OS.type")],
                  list(OS.type = "windows")),
    .package = "base"
  )
  expect_identical(.Platform$OS.type, "windows")

  # A PSOCK cluster needs to open sockets, which can fail for reasons that
  # have nothing to do with this package -- notably in a session that has
  # already forked (earlier tests here use mclapply). Probe first and skip on
  # an environmental failure, so this never reports a defect it did not find.
  probe <- tryCatch({
    cl <- parallel::makePSOCKcluster(2L)
    parallel::stopCluster(cl)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  skip_if_not(probe, "PSOCK cluster cannot be created in this session")

  streams <- cograph:::.motif_rng_streams(6L, seed = 4)
  draw <- function(p) c(x = stats::runif(1))

  serial <- cograph:::.motif_run_replicates(6L, 1L, streams, draw)
  psock <- cograph:::.motif_run_replicates(6L, 2L, streams, draw)

  expect_length(psock, 6L)
  expect_identical(psock, serial)
  # Results must not depend on how replicates were chunked across workers.
  # 3 workers exceeds the R CMD check core limit, so this one assertion is
  # conditional; the 2-worker comparison above still runs under check.
  if (!.cores_limited(3L)) {
    expect_identical(cograph:::.motif_run_replicates(6L, 3L, streams, draw),
                     serial)
  }
})

# ---- regression pins against the pre-refactor implementation ---------------

test_that("seeded census results match the pre-refactor values exactly", {
  skip_if_not_installed("tna")
  # Computed from the implementation at a27b0958, before any of this work.
  # Comparing a run to itself cannot catch a change that moves every number;
  # these literals can.
  model <- tna::tna(tna::group_regulation)
  got <- motifs(model, n_perm = 9L, seed = 4)$results

  expect_identical(got$type,
                   c("120C", "030T", "120U", "210", "120D", "030C", "300"))
  expect_identical(got$count, c(1481L, 620L, 190L, 581L, 178L, 1044L, 79L))
  expect_equal(got$expected,
               c(1010.9, 412.4, 138.1, 471.8, 128.2, 1081.6, 82),
               tolerance = 1e-8)
  expect_equal(got$p, c(0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.8), tolerance = 1e-8)
})

test_that("seeded instance results match the pre-refactor values exactly", {
  skip_if_not_installed("tna")
  model <- tna::tna(tna::group_regulation)
  got <- extract_motifs(model, n_perm = 6L, seed = 8,
                        significance = TRUE)$results
  expect_identical(nrow(got), 302L)
  expect_identical(got$observed[1], 1L)
  expect_equal(got$expected[1], 0, tolerance = 1e-8)
})

# ---- a worker that vanishes must not be recycled into the null -------------

test_that("a replicate that comes back malformed raises, never recycles", {
  n_values <- 4L
  good <- replicate(3L, seq_len(n_values), simplify = FALSE)

  expect_identical(cograph:::.motif_check_replicates(good, 3L, n_values), good)

  # A forked child killed by the OOM reaper yields NULL, not a try-error, and
  # mclapply only warns. cbind() then drops those columns and the caller's
  # `null_matrix[] <-` recycles the survivors to fill the gap.
  killed <- list(good[[1L]], NULL, good[[3L]])
  expect_error(cograph:::.motif_check_replicates(killed, 3L, n_values),
               class = "cograph_parallel_failure")
  expect_error(cograph:::.motif_check_replicates(killed, 3L, n_values),
               "killed")

  # A wrong-length replicate is recycled by cbind() without warning.
  short <- list(good[[1L]], seq_len(2L), good[[3L]])
  expect_error(cograph:::.motif_check_replicates(short, 3L, n_values),
               class = "cograph_parallel_failure")
  expect_error(cograph:::.motif_check_replicates(short, 3L, n_values),
               "expected 4 numeric")

  # NA would propagate into every downstream statistic.
  nas <- list(good[[1L]], c(1, NA, 3, 4), good[[3L]])
  expect_error(cograph:::.motif_check_replicates(nas, 3L, n_values),
               class = "cograph_parallel_failure")

  chr <- list(good[[1L]], letters[1:4], good[[3L]])
  expect_error(cograph:::.motif_check_replicates(chr, 3L, n_values),
               class = "cograph_parallel_failure")
})

test_that("the serial replicate path restores the caller's RNG", {
  # one() installs a replicate's L'Ecuyer stream into the global RNG. In a
  # worker that dies with the process; on the serial path it would leave the
  # whole session on a different generator and silently change every later
  # seeded result -- including other test files.
  before_kind <- RNGkind()
  streams <- cograph:::.motif_rng_streams(4L, seed = 7)

  # .motif_rng_streams() seeds deliberately, so the SEED legitimately moves
  # here; restoring it is motifs()' job. What must not survive is the
  # generator swap, which would change every later seeded result.
  set.seed(1234)
  before_seed <- .Random.seed
  invisible(cograph:::.motif_run_replicates(4L, 1L, streams,
                                            function(p) c(x = 1), 1L))

  expect_identical(RNGkind(), before_kind)
  expect_identical(.Random.seed, before_seed)
})

test_that("motifs() leaves the caller's RNG untouched at any cores", {
  skip_if_not_installed("tna")
  model <- tna::tna(tna::group_regulation)

  set.seed(321); before <- .Random.seed
  invisible(motifs(model, n_perm = 4L, seed = 5, cores = 1))
  expect_identical(.Random.seed, before)

  skip_on_cran()
  skip_if(is.na(parallel::detectCores()) || parallel::detectCores() < 2)
  set.seed(321); before2 <- .Random.seed
  invisible(motifs(model, n_perm = 4L, seed = 5, cores = 2))
  expect_identical(.Random.seed, before2)
  expect_identical(RNGkind()[1], "Mersenne-Twister")
})

# ---- branches the first round of tests left uncovered ---------------------

test_that(".motif_triad_pair_counts() honours every edge rule", {
  set.seed(31)
  s <- 6L
  arr <- array(stats::rpois(3L * s * s, 1.4), dim = c(3L, s, s))
  for (u in 1:3) arr[u, , ][cbind(seq_len(s), seq_len(s))] <- 0L
  idx <- cograph:::.triad_indices(s)
  man <- cograph:::.triad_type_names()

  for (method in c("any", "percent", "expected")) {
    threshold <- if (method == "any") 0 else 1
    bins <- cograph:::.motif_triad_pair_counts(
      arr, 1:3, idx, method, threshold, "003", NULL
    )
    reference <- integer(length(bins))
    for (u in 1:3) {
      mat <- cograph:::.motif_unit_matrix(arr, u)
      emat <- NULL
      if (method == "expected") {
        emat <- outer(rowSums(mat), colSums(mat)) / sum(mat)
        emat[emat == 0] <- 0.001
      }
      td <- cograph:::.count_triads_matrix_vectorized(
        mat, method, threshold, expected_mat = emat, exclude = "003"
      )
      if (is.null(td) || nrow(td) == 0) next
      pos <- match(paste(td$i, td$j, td$k, sep = "\r"),
                   paste(idx$i, idx$j, idx$k, sep = "\r"))
      bin <- (match(td$type, man) - 1L) * idx$n + pos
      reference[bin] <- reference[bin] + 1L
    }
    expect_identical(bins, reference, info = method)
  }

  # include and exclude together, which no earlier counter test exercised.
  both <- cograph:::.motif_triad_pair_counts(
    arr, 1:3, idx, "any", 0, exclude = "030T",
    include = c("030T", "021C", "111D")
  )
  kept <- man[unique((which(both > 0) - 1L) %/% idx$n) + 1L]
  expect_false("030T" %in% kept)
  expect_true(all(kept %in% c("021C", "111D")))
})

test_that(".motif_rng_streams() works without a seed and caches at the boundary", {
  # seed = NULL is reachable via motifs(seed = NULL, cores > 1).
  set.seed(3)
  a <- cograph:::.motif_rng_streams(3L, seed = NULL)
  expect_length(a, 3L)
  expect_false(identical(a[[1]], a[[2]]))
  expect_identical(RNGkind()[1], "Mersenne-Twister")

  # The cache boundary is s <= 64.
  for (s in c(64L, 65L)) {
    key <- paste0(".triad_idx_", s)
    if (exists(key, envir = cograph:::.cograph_cache)) {
      rm(list = key, envir = cograph:::.cograph_cache)
    }
    invisible(cograph:::.triad_indices(s))
  }
  expect_true(exists(".triad_idx_64", envir = cograph:::.cograph_cache))
  expect_false(exists(".triad_idx_65", envir = cograph:::.cograph_cache))
})
