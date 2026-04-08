skip_on_cran()

# =============================================================================
# Assortativity & Homophily Tests
# =============================================================================

# ============================================
# SECTION 1: assortativity() — basic functionality
# ============================================

test_that("assortativity: star graph is maximally disassortative", {
  star <- matrix(0, 6, 6)
  star[1, 2:6] <- 1
  star[2:6, 1] <- 1
  rownames(star) <- colnames(star) <- c("hub", LETTERS[1:5])
  res <- assortativity(star)
  expect_s3_class(res, "cograph_assortativity")
  expect_equal(res$coefficient, -1, tolerance = 1e-6)
  expect_equal(res$type, "degree")
  expect_false(res$directed)
  expect_equal(res$n_nodes, 6L)
  expect_equal(res$n_edges, 5L)
})

test_that("assortativity: complete graph returns NA (constant degree)", {
  k5 <- matrix(1, 5, 5)
  diag(k5) <- 0
  res <- assortativity(k5)
  expect_true(is.na(res$coefficient))
})

test_that("assortativity: ring returns NA (constant degree)", {
  ring <- create_test_topology("ring", 8)
  res <- assortativity(ring)
  expect_true(is.na(res$coefficient))
})

test_that("assortativity: path graph has known value", {
  path <- create_test_topology("path", 6)
  res <- assortativity(path)
  # Path: endpoints degree 1, interior degree 2
  expect_equal(res$coefficient, -0.25, tolerance = 1e-6)
})

test_that("assortativity: empty graph returns NA", {
  empty <- matrix(0, 4, 4)
  res <- assortativity(empty)
  expect_true(is.na(res$coefficient))
})

test_that("assortativity: 2-node graph returns NA", {
  two <- matrix(c(0, 1, 1, 0), 2, 2)
  res <- assortativity(two)
  expect_true(is.na(res$coefficient))
})

test_that("assortativity: directed graph with type parameter", {
  adj <- matrix(c(
    0, 1, 1, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
    1, 0, 0, 0
  ), 4, 4, byrow = TRUE)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]

  res_oi <- assortativity(adj, directed = TRUE, type = "out-in")
  expect_s3_class(res_oi, "cograph_assortativity")
  expect_true(res_oi$directed)
  expect_equal(res_oi$type, "out-in")

  res_ii <- assortativity(adj, directed = TRUE, type = "in-in")
  expect_equal(res_ii$type, "in-in")

  res_oo <- assortativity(adj, directed = TRUE, type = "out-out")
  expect_equal(res_oo$type, "out-out")

  res_io <- assortativity(adj, directed = TRUE, type = "in-out")
  expect_equal(res_io$type, "in-out")
})

test_that("assortativity: invalid type errors", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(assortativity(adj, type = "bogus"), "Invalid type")
  expect_error(assortativity(adj, directed = TRUE, type = "degree"),
               "Invalid type")
})

test_that("assortativity: digits parameter rounds", {
  star <- matrix(0, 6, 6)
  star[1, 2:6] <- 1
  star[2:6, 1] <- 1
  res <- assortativity(star, digits = 2)
  expect_equal(res$coefficient, -1)
})

test_that("assortativity: print method works", {
  star <- matrix(0, 6, 6)
  star[1, 2:6] <- 1
  star[2:6, 1] <- 1
  res <- assortativity(star)
  output <- capture.output(print(res))
  expect_true(any(grepl("Assortativity", output)))
  expect_true(any(grepl("disassortative", output)))
})


# ============================================
# SECTION 2: assortativity_attribute() — nominal
# ============================================

test_that("assortativity_attribute: perfect assortative nominal", {
  # Two disconnected pairs, same type within each pair
  adj <- matrix(c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  types <- c(A = "x", B = "x", C = "y", D = "y")
  res <- assortativity_attribute(adj, types)
  expect_s3_class(res, "cograph_assortativity")
  expect_equal(res$coefficient, 1, tolerance = 1e-6)
  expect_equal(res$type, "nominal")
})

test_that("assortativity_attribute: perfect disassortative nominal", {
  # Edges only between different types
  adj <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  types <- c(A = "x", B = "x", C = "y", D = "y")
  res <- assortativity_attribute(adj, types)
  expect_equal(res$coefficient, -1, tolerance = 1e-6)
})

test_that("assortativity_attribute: single category returns NA", {
  k3 <- matrix(1, 3, 3)
  diag(k3) <- 0
  types <- c("a", "a", "a")
  res <- assortativity_attribute(k3, types)
  expect_true(is.na(res$coefficient))
})

test_that("assortativity_attribute: missing node names errors", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(adj) <- colnames(adj) <- c("A", "B")
  types <- c(C = "x", D = "y")
  expect_error(assortativity_attribute(adj, types), "missing for nodes")
})

test_that("assortativity_attribute: unnamed vector by position", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  # Unnamed vector, length must match
  res <- assortativity_attribute(adj, c("x", "y"))
  expect_s3_class(res, "cograph_assortativity")
})


# ============================================
# SECTION 3: assortativity_attribute() — scalar
# ============================================

test_that("assortativity_attribute: numeric values give scalar type", {
  adj <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  vals <- c(A = 10, B = 20, C = 15, D = 25)
  res <- assortativity_attribute(adj, vals)
  expect_equal(res$type, "scalar")
  expect_true(is.numeric(res$coefficient))
})


# ============================================
# SECTION 4: homophily() alias
# ============================================

test_that("homophily is an alias for assortativity_attribute", {
  expect_identical(homophily, assortativity_attribute)
})


# ============================================
# SECTION 5: igraph equivalence — degree
# ============================================

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


# ============================================
# SECTION 6: igraph equivalence — nominal
# ============================================

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


# ============================================
# SECTION 7: igraph equivalence — scalar
# ============================================

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


# ============================================
# SECTION 8: igraph input format
# ============================================

test_that("assortativity: accepts igraph object directly", {
  skip_if_no_igraph()
  g <- igraph::make_star(6, mode = "undirected")
  res <- assortativity(g)
  expect_equal(res$coefficient, -1, tolerance = 1e-6)
})

test_that("assortativity_attribute: accepts igraph with named values", {
  skip_if_no_igraph()
  g <- igraph::make_star(4, mode = "undirected")
  igraph::V(g)$name <- c("hub", "a", "b", "c")
  vals <- c(hub = "core", a = "leaf", b = "leaf", c = "leaf")
  res <- assortativity_attribute(g, vals)
  expect_s3_class(res, "cograph_assortativity")
})
