# =============================================================================
# The vocabulary available inside filter and select expressions:
# node predicates, edge predicates, and delegation of unknown measure names
# to centrality().
# =============================================================================

wv_star <- function() {
  m <- matrix(c(0, 1, 1, 1,
                1, 0, 1, 0,
                1, 1, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
  dimnames(m) <- list(LETTERS[1:4], LETTERS[1:4])
  m
}

wv_dir <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- 1   # A -> B
  m[2, 3] <- 1   # B -> C
  m[3, 2] <- 1   # C -> B, so B <-> C is mutual
  m
}

wv_iso <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- m[2, 1] <- 1
  m[2, 3] <- m[3, 2] <- 1
  m
}

# --- node predicates ---------------------------------------------------------

test_that("is_isolated selects the nodes with no edges", {
  kept <- select_nodes(wv_iso(), !is_isolated)

  expect_equal(get_labels(kept), c("A", "B", "C"))
  expect_equal(get_labels(select_nodes(wv_iso(), is_isolated)), "D")
})

test_that("is_source and is_sink read arc direction", {
  expect_equal(get_labels(select_nodes(wv_dir(), is_source)), "A")
  expect_equal(get_labels(suppressWarnings(select_nodes(wv_dir(), is_sink))), character(0))
})

test_that("is_source and is_sink are never TRUE on an undirected network", {
  expect_warning(select_nodes(wv_star(), is_source))
  expect_warning(select_nodes(wv_star(), is_sink))
})

test_that("is_leaf selects the degree-one nodes", {
  expect_equal(get_labels(select_nodes(wv_star(), is_leaf)), "D")
})

test_that("is_cut selects the articulation points", {
  expect_equal(get_labels(select_nodes(wv_star(), is_cut)), "A")
})

test_that("is_cut agrees with igraph::articulation_points", {
  skip_if_not_installed("igraph")
  m <- wv_star()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")
  expected <- sort(names(igraph::articulation_points(g)))

  expect_equal(sort(get_labels(select_nodes(m, is_cut))), expected)
})

test_that("local_transitivity and local_triangles match igraph", {
  skip_if_not_installed("igraph")
  m <- wv_star()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")

  nodes <- as.data.frame(
    mutate_nodes(m, lt = local_transitivity, tri = local_triangles),
    what = "nodes"
  )

  expect_equal(nodes$lt, unname(igraph::transitivity(g, type = "local")))
  expect_equal(nodes$tri, unname(as.numeric(igraph::count_triangles(g))))
})

test_that("node predicates combine with other criteria", {
  kept <- select_nodes(wv_star(), !is_leaf, degree >= 2)

  expect_equal(get_labels(kept), c("A", "B", "C"))
})

# --- edge predicates ---------------------------------------------------------

test_that("is_loop selects self-loops", {
  # A directed matrix keeps its diagonal as self-loops; parse_matrix() reads
  # an undirected matrix from the strict upper triangle, so an undirected
  # diagonal never becomes an edge in the first place.
  m <- wv_dir()
  m[1, 1] <- 1

  expect_equal(n_edges(suppressWarnings(select_edges(m, is_loop))), 1L)
  expect_equal(n_edges(select_edges(m, !is_loop)), 3L)
})

test_that("is_loop is FALSE everywhere on a loop-free network", {
  edges <- as.data.frame(mutate_edges(wv_star(), loop = is_loop))

  expect_equal(edges$loop, rep(FALSE, nrow(edges)))
})

test_that("is_reciprocal is an alias for is_mutual", {
  a <- as.data.frame(mutate_edges(wv_dir(), m = is_mutual, r = is_reciprocal))

  expect_equal(a$m, a$r)
  expect_equal(a$r, c(FALSE, TRUE, TRUE))
})

test_that("is_multiple is FALSE when no pair repeats", {
  edges <- as.data.frame(mutate_edges(wv_star(), dup = is_multiple))

  expect_equal(edges$dup, rep(FALSE, nrow(edges)))
})

test_that("is_multiple finds a repeated pair in an edge list", {
  el <- data.frame(
    from = c("A", "A", "B"),
    to = c("B", "B", "C"),
    weight = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  edges <- as.data.frame(mutate_edges(el, dup = is_multiple))

  expect_true(any(edges$dup))
})

test_that("weight_rank orders the edges from weakest to strongest", {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- m[2, 1] <- 0.5
  m[1, 3] <- m[3, 1] <- 0.8
  m[2, 4] <- m[4, 2] <- 0.1

  edges <- as.data.frame(mutate_edges(m, r = weight_rank))

  expect_equal(edges$r[order(edges$weight)], c(1, 2, 3))
})

test_that("from_community and to_community are available", {
  skip_if_not_installed("igraph")
  edges <- as.data.frame(
    mutate_edges(wv_star(), fc = from_community, tc = to_community)
  )

  expect_equal(length(edges$fc), nrow(edges))
  expect_true(is.numeric(edges$fc))
})

test_that("select_edges accepts the new metrics for top =", {
  m <- wv_star()

  expect_equal(n_edges(suppressWarnings(select_edges(m, top = 2, by = "from_degree"))), 2L)
  expect_equal(n_edges(suppressWarnings(select_edges(m, top = 2, by = "weight_rank"))), 2L)
})

# --- delegation to centrality() ----------------------------------------------

test_that("an unknown-but-real measure name is delegated to centrality()", {
  m <- wv_star()
  kept <- select_nodes(m, harmonic > 0)

  expect_equal(n_nodes(kept), 4L)
})

test_that("a delegated measure gives the same values centrality() does", {
  m <- wv_star()
  nodes <- as.data.frame(mutate_nodes(m, lev = leverage), what = "nodes")
  reference <- centrality(m, measures = "leverage")

  expect_equal(nodes$lev, as.numeric(reference[[setdiff(names(reference), "node")[1]]]))
})

test_that("select_top accepts any measure centrality() knows", {
  expect_equal(n_nodes(select_top(wv_star(), n = 2, by = "harmonic")), 2L)
})

test_that("a name that is neither vocabulary nor a measure is left to the caller", {
  m <- wv_star()
  my_flag <- c(TRUE, FALSE, TRUE, FALSE)
  kept <- select_nodes(m, my_flag)

  expect_equal(get_labels(kept), c("A", "C"))
})

test_that("an unknown measure in `by` is an error", {
  expect_error(select_top(wv_star(), n = 2, by = "not_a_measure"),
               class = "cograph_bad_selection")
})
