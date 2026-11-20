# Equivalence tests moved from tests/testthat/test-assortativity.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
skip_on_cran()

# ---- equivalence tests ----

test_that("assortativity: matches igraph on 100 random undirected networks", {
  skip_if_no_igraph()
  set.seed(42)
  failures <- 0L

  for (i in seq_len(100)) {
    n <- sample(5:25, 1)
    p <- runif(1, 0.15, 0.5)
    g <- igraph::sample_gnp(n, p, directed = FALSE)
    if (igraph::ecount(g) == 0) next

    adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    ig_val <- igraph::assortativity_degree(g)
    co_val <- cograph::assortativity(adj)$coefficient

    # Both NaN/NA or both equal
    both_na <- (is.nan(ig_val) || is.na(ig_val)) && is.na(co_val)
    if (!both_na && !isTRUE(all.equal(ig_val, co_val, tolerance = 1e-6))) {
      failures <- failures + 1L
    }
  }

  expect_equal(failures, 0L,
               info = paste("Failed on", failures, "of 100 undirected networks"))
})

test_that("assortativity: matches igraph on 100 random directed networks", {
  skip_if_no_igraph()
  set.seed(123)
  failures <- 0L

  for (i in seq_len(100)) {
    n <- sample(5:25, 1)
    p <- runif(1, 0.1, 0.4)
    g <- igraph::sample_gnp(n, p, directed = TRUE)
    if (igraph::ecount(g) == 0) next

    adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    ig_val <- igraph::assortativity_degree(g, directed = TRUE)
    co_val <- cograph::assortativity(adj, directed = TRUE)$coefficient

    both_na <- (is.nan(ig_val) || is.na(ig_val)) && is.na(co_val)
    if (!both_na && !isTRUE(all.equal(ig_val, co_val, tolerance = 1e-6))) {
      failures <- failures + 1L
    }
  }

  expect_equal(failures, 0L,
               info = paste("Failed on", failures, "of 100 directed networks"))
})

test_that("assortativity_attribute: nominal matches igraph on 100 networks", {
  skip_if_no_igraph()
  set.seed(77)
  failures <- 0L

  for (i in seq_len(100)) {
    n <- sample(5:20, 1)
    p <- runif(1, 0.15, 0.5)
    g <- igraph::sample_gnp(n, p, directed = FALSE)
    if (igraph::ecount(g) == 0) next
    adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

    n_cats <- sample(2:4, 1)
    types_int <- sample(seq_len(n_cats), n, replace = TRUE)
    types_char <- LETTERS[types_int]

    ig_val <- igraph::assortativity_nominal(g, types_int)
    co_val <- cograph::assortativity_attribute(adj, types_char)$coefficient

    both_na <- (is.nan(ig_val) || is.na(ig_val)) && is.na(co_val)
    if (!both_na && !isTRUE(all.equal(ig_val, co_val, tolerance = 1e-6))) {
      failures <- failures + 1L
    }
  }

  expect_equal(failures, 0L,
               info = paste("Failed on", failures, "of 100 nominal tests"))
})

test_that("assortativity_attribute: scalar matches igraph on 100 networks", {
  skip_if_no_igraph()
  set.seed(99)
  failures <- 0L

  for (i in seq_len(100)) {
    n <- sample(5:20, 1)
    p <- runif(1, 0.15, 0.5)
    g <- igraph::sample_gnp(n, p, directed = FALSE)
    if (igraph::ecount(g) == 0) next
    adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

    vals <- rnorm(n)

    ig_val <- igraph::assortativity(g, vals)
    co_val <- cograph::assortativity_attribute(adj, vals)$coefficient

    both_na <- (is.nan(ig_val) || is.na(ig_val)) && is.na(co_val)
    if (!both_na && !isTRUE(all.equal(ig_val, co_val, tolerance = 1e-6))) {
      failures <- failures + 1L
    }
  }

  expect_equal(failures, 0L,
               info = paste("Failed on", failures, "of 100 scalar tests"))
})
