# igraph is a Suggests dependency. Every entry point that reaches it must say
# so plainly rather than letting R raise the bare "there is no package called
# 'igraph'" from the `::` operator, which tells a user nothing actionable.

test_that(".need_igraph() passes when igraph is present", {
  skip_if_not_installed("igraph")
  expect_true(cograph:::.need_igraph("to_igraph()"))
  expect_silent(cograph:::.need_igraph("anything()"))
})

test_that(".need_igraph() raises a classed, actionable condition when absent", {
  # Capture the real function BEFORE mocking: mocking the "base" binding means
  # `base::requireNamespace` would resolve back to the mock and recurse.
  real_require <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "igraph")) FALSE else real_require(package, ...)
    },
    .package = "base"
  )
  expect_error(cograph:::.need_igraph("to_igraph()"),
               class = "cograph_missing_suggest")
  # The message must name the caller and say how to fix it.
  err <- tryCatch(cograph:::.need_igraph("to_igraph()"),
                  cograph_missing_suggest = function(e) conditionMessage(e))
  expect_match(err, "to_igraph()", fixed = TRUE)
  expect_match(err, "install.packages", fixed = TRUE)

  # The argument is the caller's name for the message, not a package to test:
  # the guard always checks igraph, whatever label it is given.
  expect_error(cograph:::.need_igraph("something_else()"),
               class = "cograph_missing_suggest")
  # The mock must leave other packages alone, or the test proves nothing.
  expect_true(requireNamespace("stats", quietly = TRUE))
})

test_that("igraph-dependent entry points guard instead of leaking the raw error", {
  skip_if_not_installed("igraph")
  mat <- matrix(c(0, 2, 1, 0,
                  0, 0, 3, 1,
                  1, 0, 0, 2,
                  1, 0, 0, 0), 4, 4, byrow = TRUE)
  dimnames(mat) <- list(LETTERS[1:4], LETTERS[1:4])

  # Capture the real function BEFORE mocking: mocking the "base" binding means
  # `base::requireNamespace` would resolve back to the mock and recurse.
  real_require <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "igraph")) FALSE else real_require(package, ...)
    },
    .package = "base"
  )

  # to_igraph() is the choke point reached from most of the package.
  expect_error(to_igraph(mat), class = "cograph_missing_suggest")
  # motif_census() builds the graph itself and needs its own guard.
  expect_error(motif_census(mat), class = "cograph_missing_suggest")

  # Whatever the class, no entry point may surface R's bare namespace error.
  raw <- function(expr) {
    msg <- tryCatch({ force(expr); "" }, error = conditionMessage)
    grepl("there is no package called", msg, fixed = TRUE)
  }
  expect_false(raw(to_igraph(mat)))
  expect_false(raw(motif_census(mat)))
  expect_false(raw(detect_communities(mat)))
  expect_false(raw(motifs(mat, n_perm = 10L, seed = 1)))
  expect_false(raw(robustness(mat)))
  expect_false(raw(vulnerability(mat)))
  expect_false(raw(rich_club(mat)))
  expect_false(raw(network_summary(mat)))
})

test_that("paths needing no igraph keep working without it", {
  mat <- matrix(c(0, 2, 1, 0,
                  0, 0, 3, 1,
                  1, 0, 0, 2,
                  1, 0, 0, 0), 4, 4, byrow = TRUE)
  dimnames(mat) <- list(LETTERS[1:4], LETTERS[1:4])

  # Capture the real function BEFORE mocking: mocking the "base" binding means
  # `base::requireNamespace` would resolve back to the mock and recurse.
  real_require <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "igraph")) FALSE else real_require(package, ...)
    },
    .package = "base"
  )
  # The descriptive motif engine is pure base R; guarding must not break it.
  expect_s3_class(motifs(mat, significance = FALSE), "cograph_motif_result")
  expect_s3_class(extract_triads(as_cograph(mat), min_total = 0), "data.frame")
})

test_that("igraph paths outside to_igraph() guard too", {
  skip_if_not_installed("igraph")
  real_require <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "igraph")) FALSE else real_require(package, ...)
    },
    .package = "base"
  )

  # membership()'s igraph call is the fallback for ANY input that is not a
  # cograph_communities, so it is reachable with ordinary arguments.
  expect_error(membership(list(a = 1)), class = "cograph_missing_suggest")
  expect_error(membership(matrix(1:4, 2)), class = "cograph_missing_suggest")

  # An object can carry the igraph class without the package installed, for
  # instance after readRDS().
  expect_error(is_directed(structure(list(), class = "igraph")),
               class = "cograph_missing_suggest")

  # These must not surface R's bare namespace error.
  raw <- function(expr) {
    msg <- tryCatch({ force(expr); "" }, error = conditionMessage)
    grepl("there is no package called", msg, fixed = TRUE)
  }
  expect_false(raw(membership(list(a = 1))))
  expect_false(raw(is_directed(structure(list(), class = "igraph"))))
  expect_false(raw(to_network(structure(list(), class = "igraph"))))
})

test_that("is_directed() still answers from cograph data without igraph", {
  real_require <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "igraph")) FALSE else real_require(package, ...)
    },
    .package = "base"
  )
  net <- as_cograph(matrix(c(0, 1, 0,
                             0, 0, 1,
                             1, 0, 0), 3, 3, byrow = TRUE))
  # The guard must not intercept a path that never needed igraph.
  expect_true(is_directed(net))
})
