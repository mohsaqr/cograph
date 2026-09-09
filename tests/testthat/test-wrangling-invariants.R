# =============================================================================
# Invariant tests for the network wrangling verbs
#
# These pin the contract of the filter/select family: what must hold for any
# input, not what one worked example happens to return. Each block names the
# defect from docs/network-wrangling-plan.md that it was written against.
# =============================================================================

# --- fixtures ---------------------------------------------------------------

wrangle_und <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- m[2, 1] <- 0.5
  m[1, 3] <- m[3, 1] <- 0.8
  m[2, 4] <- m[4, 2] <- 0.6
  m[3, 4] <- m[4, 3] <- 0.4
  m
}

wrangle_dir <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- 0.5
  m[2, 1] <- 0.2
  m[1, 3] <- 0.8
  m[3, 4] <- 0.4
  m
}

# Node E has no edges and sits after every edge endpoint: the shape that
# breaks a vertex count derived from the edge list.
wrangle_iso <- function() {
  m <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
  m[1, 2] <- m[2, 1] <- 1
  m[2, 3] <- m[3, 2] <- 1
  m[3, 4] <- m[4, 3] <- 1
  m
}

wrangle_neg <- function() {
  m <- wrangle_und()
  m[1, 2] <- m[2, 1] <- -0.5
  m
}

# --- D1: trailing isolates ---------------------------------------------------

test_that("D1 every verb survives a network whose last node is isolated", {
  iso <- wrangle_iso()

  expect_no_error(filter_nodes(iso, label != "E"))
  expect_no_error(filter_edges(iso, weight > 0))
  expect_no_error(select_nodes(iso, component = "largest"))
  expect_no_error(select_edges(iso, top = 2))
  expect_no_error(to_df(iso))
  expect_no_error(to_igraph(as_cograph(iso)))
})

test_that("D1 to_igraph keeps every node, not just the connected ones", {
  skip_if_not_installed("igraph")
  g <- to_igraph(as_cograph(wrangle_iso()))

  expect_equal(igraph::vcount(g), 5L)
  expect_equal(igraph::V(g)$name, LETTERS[1:5])
})

test_that("D1 node count is preserved through a node filter with an isolate", {
  kept <- filter_nodes(wrangle_iso(), label != "E")

  expect_equal(n_nodes(kept), 4L)
  expect_equal(get_labels(kept), LETTERS[1:4])
})

# --- D2: undirected results stay symmetric -----------------------------------

test_that("D2 an undirected network stays undirected through every verb", {
  und <- wrangle_und()

  results <- list(
    filter_edges = to_matrix(filter_edges(und, weight > 0.35)),
    filter_nodes = to_matrix(filter_nodes(und, degree >= 2)),
    select_edges = to_matrix(select_edges(und, top = 2)),
    select_nodes = to_matrix(select_nodes(und, index = 1:3))
  )

  expect_true(all(vapply(results, isSymmetric, logical(1))))
})

test_that("D2 keep_format = TRUE round-trips an undirected matrix as undirected", {
  m <- to_matrix(filter_edges(wrangle_und(), weight > 0.35, keep_format = TRUE))

  expect_true(isSymmetric(m))
  expect_false(is_directed(as_cograph(m)))
})

test_that("D2 a directed network is not symmetrised by a filter", {
  m <- to_matrix(filter_edges(wrangle_dir(), weight > 0.1))

  expect_false(isSymmetric(m))
  expect_equal(m["A", "B"], 0.5)
  expect_equal(m["B", "A"], 0.2)
})

test_that("D2 total strength is conserved by an all-pass filter", {
  und <- wrangle_und()
  kept <- to_matrix(filter_edges(und, weight > 0))

  expect_equal(sum(kept), sum(und))
})

# --- D3: negative weights ----------------------------------------------------

test_that("D3 filter_nodes tolerates negative weights", {
  neg <- wrangle_neg()

  expect_no_error(suppressWarnings(filter_nodes(neg, degree >= 2)))
  expect_equal(n_nodes(suppressWarnings(filter_nodes(neg, degree >= 2))), 4L)
})

test_that("D3 path measures on negative weights warn with a class, not an error", {
  expect_warning(
    filter_nodes(wrangle_neg(), betweenness >= 0),
    class = "cograph_negative_weights"
  )
})

# --- D4: empty results with keep_format --------------------------------------

test_that("D4 an empty result keeps the input format instead of erroring", {
  und <- wrangle_und()

  m1 <- suppressWarnings(filter_edges(und, weight > 5, keep_format = TRUE))
  m2 <- suppressWarnings(filter_nodes(und, degree > 99, keep_format = TRUE))
  m3 <- suppressWarnings(select_edges(und, top = 0, keep_format = TRUE))

  expect_true(is.matrix(m1))
  expect_true(is.matrix(m2))
  expect_true(is.matrix(m3))
  expect_equal(sum(m1), 0)
})

# --- D5: set_edges / set_nodes keep $weights honest --------------------------

test_that("D5 set_edges makes to_matrix agree with the new edge table", {
  net <- as_cograph(wrangle_und())
  net <- set_edges(net, data.frame(from = 1L, to = 2L, weight = 9))
  m <- to_matrix(net)

  expect_equal(m[1, 2], 9)
  expect_equal(sum(m), 18) # mirrored once for the undirected pair
  expect_equal(nrow(m), 4L)
})

test_that("D5 set_edges keeps extra edge columns", {
  net <- as_cograph(wrangle_und())
  net <- set_edges(net, data.frame(from = 1L, to = 2L, weight = 1, kind = "x"))

  expect_true("kind" %in% names(get_edges(net)))
})

test_that("D5 set_nodes relabels the stored weight matrix", {
  net <- as_cograph(wrangle_und())
  nodes <- get_nodes(net)
  nodes$label <- paste0("N", 1:4)
  net <- set_nodes(net, nodes)

  expect_equal(rownames(to_matrix(net)), paste0("N", 1:4))
})

# --- D6: extra edge columns --------------------------------------------------

test_that("D6 extra edge-list columns survive as_cograph and the filters", {
  el <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    weight = c(1, 2, 3),
    session = c("s1", "s1", "s2"),
    stringsAsFactors = FALSE
  )
  net <- as_cograph(el)

  expect_true("session" %in% names(get_edges(net)))
  expect_equal(get_edges(net)$session, c("s1", "s1", "s2"))
  expect_equal(get_edges(filter_edges(net, session == "s1"))$session, c("s1", "s1"))
})

test_that("D6 a custom edge column is usable in a filter expression", {
  el <- data.frame(
    from = c("A", "B", "C"), to = c("B", "C", "A"),
    weight = c(1, 2, 3), session = c("s1", "s1", "s2"),
    stringsAsFactors = FALSE
  )

  expect_equal(n_edges(filter_edges(el, session == "s2")), 1L)
})

# --- D7: metadata survival ---------------------------------------------------

test_that("D7 node groups survive a node filter", {
  net <- as_cograph(wrangle_und())
  net <- set_groups(net, list(G1 = c("A", "B"), G2 = c("C", "D")))
  kept <- filter_nodes(net, label %in% c("A", "B", "C"))

  expect_false(is.null(get_groups(kept)))
  expect_equal(sort(get_groups(kept)$node), c("A", "B", "C"))
})

test_that("D7 sequence data survives an edge filter", {
  net <- as_cograph(wrangle_und())
  net$data <- data.frame(t1 = c("A", "B"), t2 = c("B", "C"))
  kept <- filter_edges(net, weight > 0.3)

  expect_false(is.null(kept$data))
  expect_equal(nrow(kept$data), 2L)
})

test_that("D7 layout and source metadata survive a filter", {
  net <- as_cograph(wrangle_und())
  net <- set_layout(net, data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1)))
  net$meta$layout <- "manual"
  kept <- filter_nodes(net, degree >= 1)

  expect_equal(kept$meta$layout, "manual")
  expect_equal(kept$meta$source, "matrix")
  expect_false(anyNA(get_nodes(kept)$x))
})

# --- D9 / D10: validation ----------------------------------------------------

test_that("D9 an unknown centrality name is an error, not a silent fallback", {
  expect_error(select_top(wrangle_und(), n = 2, by = "bogus"),
               class = "cograph_bad_selection")
  expect_error(select_top_edges(wrangle_und(), n = 2, by = "bogus"),
               class = "cograph_bad_selection")
})

test_that("D10 a malformed 'between' is an error, not a select-everything warning", {
  expect_error(select_edges(wrangle_und(), between = "nonsense"),
               class = "cograph_bad_selection")
  expect_error(select_edges(wrangle_und(), between = list(1)),
               class = "cograph_bad_selection")
})

test_that("D10 out-of-range and fractional indices are errors", {
  expect_error(select_nodes(wrangle_und(), index = 99),
               class = "cograph_bad_selection")
  expect_error(select_nodes(wrangle_und(), index = 2.7),
               class = "cograph_bad_selection")
  expect_error(select_neighbors(wrangle_und(), of = "Z"),
               class = "cograph_bad_selection")
})

# --- D12: isolates are not deleted by an edge filter -------------------------

test_that("D12 filtering edges never removes nodes", {
  und <- wrangle_und()

  expect_equal(n_nodes(suppressWarnings(filter_edges(und, weight > 0.7))), 4L)
  expect_equal(n_nodes(suppressWarnings(select_edges(und, top = 1))), 4L)
})

test_that("D12 a node isolated before the filter is still there after it", {
  kept <- filter_edges(wrangle_iso(), weight > 0)

  expect_true("E" %in% get_labels(kept))
})

test_that("D12 creating an isolate warns with a class", {
  expect_warning(filter_edges(wrangle_und(), weight > 0.7),
                 class = "cograph_isolates_created")
})

test_that("D12 remove_isolates() gives the old pruning behaviour", {
  pruned <- remove_isolates(suppressWarnings(filter_edges(wrangle_und(), weight > 0.7)))

  expect_equal(n_nodes(pruned), 2L)
  expect_equal(sort(get_labels(pruned)), c("A", "C"))
})

# --- structural invariants that must hold for any input ----------------------

test_that("an all-pass filter is the identity on the weight matrix", {
  inputs <- list(und = wrangle_und(), dir = wrangle_dir(), iso = wrangle_iso())

  identical_after <- vapply(inputs, function(m) {
    isTRUE(all.equal(to_matrix(filter_edges(m, weight > -Inf)), m))
  }, logical(1))

  expect_true(all(identical_after))
})

test_that("node selection is order-invariant", {
  und <- wrangle_und()
  a <- to_matrix(select_nodes(und, index = c(1, 3)))
  b <- to_matrix(select_nodes(und, index = c(3, 1)))

  expect_equal(a, b)
})

test_that("filters are idempotent", {
  und <- wrangle_und()
  once <- filter_edges(und, weight > 0.45)
  twice <- filter_edges(once, weight > 0.45)

  expect_equal(to_matrix(once), to_matrix(twice))
})

test_that("edge count matches the weight matrix for directed and undirected", {
  und <- as_cograph(wrangle_und())
  dir <- as_cograph(wrangle_dir())

  expect_equal(n_edges(und), sum(to_matrix(und) != 0) / 2)
  expect_equal(n_edges(dir), sum(to_matrix(dir) != 0))
})
