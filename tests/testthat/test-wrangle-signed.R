# =============================================================================
# Regression tests for the defects an adversarial review of 2.6.0 found.
#
# The theme of the worst group: zero is how this representation stores
# "no edge", so it must never be fed to pmax()/pmin() as a comparable weight.
# A social-network or psychometric network routinely carries negative edges,
# and comparing one against a structural zero silently deletes it.
# =============================================================================

sg_one_way <- function(w) {
  m <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  m[1, 2] <- w
  m
}

sg_reciprocal <- function(ab, ba) {
  m <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  m[1, 2] <- ab
  m[2, 1] <- ba
  m
}

sg_negative_triangle <- function() {
  m <- matrix(c(0, -1, -3,
                -1, 0, -2,
                -3, -2, 0), 3, 3, byrow = TRUE,
              dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  m
}

# --- one-way edges are not compared against a phantom reverse arc -----------

test_that("to_undirected keeps a one-way negative edge under every rule", {
  neg <- sg_one_way(-2)
  rules <- c("max", "min", "mean", "sum")

  kept <- vapply(rules, function(r) {
    to_matrix(to_undirected(neg, method = r, directed = TRUE))["A", "B"]
  }, numeric(1))

  expect_equal(unname(kept), rep(-2, 4))
})

test_that("to_undirected keeps a one-way positive edge under every rule", {
  pos <- sg_one_way(2)
  rules <- c("max", "min", "mean", "sum")

  kept <- vapply(rules, function(r) {
    to_matrix(to_undirected(pos, method = r, directed = TRUE))["A", "B"]
  }, numeric(1))

  expect_equal(unname(kept), rep(2, 4))
})

test_that("to_undirected still combines a genuinely reciprocated pair", {
  both <- sg_reciprocal(4, 6)

  expect_equal(to_matrix(to_undirected(both, "max", directed = TRUE))["A", "B"], 6)
  expect_equal(to_matrix(to_undirected(both, "min", directed = TRUE))["A", "B"], 4)
  expect_equal(to_matrix(to_undirected(both, "mean", directed = TRUE))["A", "B"], 5)
  expect_equal(to_matrix(to_undirected(both, "sum", directed = TRUE))["A", "B"], 10)
})

test_that("symmetrize keeps a one-way negative edge", {
  neg <- sg_one_way(-2)

  expect_equal(to_matrix(symmetrize(neg, "max", directed = TRUE))["A", "B"], -2)
  expect_equal(to_matrix(symmetrize(neg, "min", directed = TRUE))["A", "B"], -2)
})

test_that("a self-loop is one arc, not a reciprocated pair", {
  m <- sg_reciprocal(1, 1)
  m[1, 1] <- 0.3

  expect_equal(to_matrix(symmetrize(m, "sum", directed = TRUE))["A", "A"], 0.3)
  expect_equal(to_matrix(to_undirected(m, "sum", directed = TRUE))["A", "A"], 0.3)
})

test_that("combining to exactly zero warns rather than silently dropping", {
  cancelling <- sg_reciprocal(2, -2)

  expect_warning(to_undirected(cancelling, "sum", directed = TRUE),
                 class = "cograph_edges_dropped")
})

# --- spanning_tree on signed weights ---------------------------------------

test_that("spanning_tree keeps negative edges", {
  m <- sg_negative_triangle()
  tree <- spanning_tree(m)

  expect_equal(n_edges(tree), 2L)
  expect_equal(sort(as.data.frame(tree)$weight), c(-3, -2))
})

test_that("spanning_tree(maximum) on negative weights picks the largest", {
  tree <- spanning_tree(sg_negative_triangle(), maximum = TRUE)

  expect_equal(n_edges(tree), 2L)
  expect_equal(sort(as.data.frame(tree)$weight), c(-2, -1))
})

test_that("spanning_tree rejects non-finite weights", {
  m <- matrix(c(0, NA, NA, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))

  expect_error(spanning_tree(m), class = "cograph_bad_selection")
})

# --- bind_networks ----------------------------------------------------------

test_that("bind_networks keeps an edge only one network has, even negative", {
  a <- sg_one_way(-2)
  z <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))

  out <- bind_networks(a, z, method = "union", weight = "max", directed = TRUE)

  expect_equal(to_matrix(out)["A", "B"], -2)
})

test_that("bind_networks warns when two weights cancel to zero", {
  a <- sg_one_way(-1)
  b <- sg_one_way(1)

  expect_warning(bind_networks(a, b, method = "union", weight = "sum",
                               directed = TRUE),
                 class = "cograph_edges_dropped")
})

test_that("bind_networks(directed = FALSE) returns a symmetric network", {
  lower <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  lower[2, 1] <- 7
  zero <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))

  out <- bind_networks(as_cograph(lower, directed = TRUE),
                       as_cograph(zero, directed = FALSE),
                       weight = "first", directed = FALSE)

  expect_true(isSymmetric(to_matrix(out)))
  expect_false(is_directed(out))
  expect_equal(n_edges(out), 1L)
})

test_that("bind_networks carries the metadata and node attributes of x", {
  m <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  m["A", "B"] <- m["B", "A"] <- 2
  net <- as_cograph(m)
  net$data <- data.frame(raw = 1:2)
  net$meta$custom <- "keep"

  out <- bind_networks(net, m)

  expect_false(is.null(out$data))
  expect_equal(out$meta$custom, "keep")
})

# --- contract_nodes ---------------------------------------------------------

test_that("contract_nodes counts an undirected within-group edge once", {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m["A", "B"] <- m["B", "A"] <- 3
  m["A", "C"] <- m["C", "A"] <- 5

  out <- to_matrix(contract_nodes(m, groups = c("g1", "g1", "g2", "g2"),
                                  weight = "sum", loops = TRUE))

  expect_equal(out["g1", "g1"], 3)
  expect_equal(out["g1", "g2"], 5)
})

test_that("contract_nodes conserves the total edge weight", {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m["A", "B"] <- m["B", "A"] <- 3
  m["A", "C"] <- m["C", "A"] <- 5

  before <- sum(as.data.frame(as_cograph(m))$weight)
  after <- sum(as.data.frame(contract_nodes(m, groups = c("g1", "g1", "g2", "g2"),
                                            weight = "sum", loops = TRUE))$weight)

  expect_equal(after, before)
})

test_that("contract_nodes keeps both directions on a directed network", {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m["A", "C"] <- 2
  m["C", "A"] <- 7

  out <- to_matrix(contract_nodes(m, groups = c("g1", "g1", "g2", "g2"),
                                  weight = "sum", directed = TRUE))

  expect_equal(out["g1", "g2"], 2)
  expect_equal(out["g2", "g1"], 7)
})

# --- permutation and duplicate validation -----------------------------------

test_that("reorder_nodes rejects a non-permutation of the right length", {
  m <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))

  expect_error(reorder_nodes(m, c("A", "A", "B")), class = "cograph_bad_selection")
  expect_error(reorder_nodes(m, c(1, 1, 2)), class = "cograph_bad_selection")
})

test_that("reorder_nodes still accepts a genuine permutation", {
  m <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))

  expect_equal(get_labels(reorder_nodes(m, c("C", "A", "B"))), c("C", "A", "B"))
})

test_that("add_edges rejects the same undirected edge named twice", {
  z <- as_cograph(matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B"))))

  expect_error(add_edges(z, from = c("A", "B"), to = c("B", "A"), weight = c(1, 2)),
               class = "cograph_bad_selection")
})

test_that("set_edges rejects a duplicated undirected pair", {
  z <- as_cograph(matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B"))))

  expect_error(set_edges(z, data.frame(from = c(1, 2), to = c(2, 1), weight = c(4, 5))),
               class = "cograph_bad_selection")
})

# --- structural columns and zero weights ------------------------------------

test_that("mutate verbs refuse to overwrite structural columns", {
  m <- matrix(c(0, 2, 2, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))

  expect_error(mutate_nodes(m, label = "X"), class = "cograph_bad_selection")
  expect_error(mutate_nodes(m, id = 1), class = "cograph_bad_selection")
  expect_error(mutate_edges(m, from = 99), class = "cograph_bad_selection")
  expect_error(mutate_edges(m, to = 99), class = "cograph_bad_selection")
})

test_that("a weight mutated to zero drops the edge with a classed warning", {
  m <- matrix(c(0, 2, 2, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))

  expect_warning(out <- mutate_edges(m, weight = 0), class = "cograph_edges_dropped")
  expect_equal(n_edges(out), 0L)
  expect_equal(sum(to_matrix(out) != 0), 0L)
})

test_that("the edge table and the weight matrix always agree", {
  m <- matrix(c(0, 2, 2, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  nets <- list(
    plain = as_cograph(m),
    mutated = suppressWarnings(mutate_edges(m, weight = weight * 2)),
    normalized = normalize_weights(m, "max")
  )

  agree <- vapply(nets, function(net) {
    n_edges(net) == sum(to_matrix(net) != 0) / (if (is_directed(net)) 1 else 2)
  }, logical(1))

  expect_true(all(agree))
})

# --- classed warnings and validation ----------------------------------------

test_that("removing every edge still reports the isolates it created", {
  m <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))

  expect_warning(filter_edges(m, FALSE), class = "cograph_isolates_created")
  expect_warning(threshold_edges(m, minimum = 9), class = "cograph_isolates_created")
  expect_warning(binarize(m, threshold = 9), class = "cograph_isolates_created")
})

test_that("documented ranges and counts are enforced", {
  m <- matrix(c(0, 3, 2, 3, 0, 1, 2, 1, 0), 3, 3, byrow = TRUE,
              dimnames = list(LETTERS[1:3], LETTERS[1:3]))

  expect_error(threshold_edges(m, proportion = 0), class = "cograph_bad_selection")
  expect_error(threshold_edges(m, density = 0), class = "cograph_bad_selection")
  expect_error(threshold_edges(m, top = 0.5), class = "cograph_bad_selection")
  expect_error(select_k_core(m, k = 1.5), class = "cograph_bad_selection")
  expect_error(split_components(m, min_size = 1.5), class = "cograph_bad_selection")
  expect_error(complement_network(m, weight = 0), class = "cograph_bad_selection")
})

test_that("non-finite weights are a classed error, not an internal failure", {
  m <- matrix(c(0, NA, NA, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))

  expect_error(normalize_weights(m, "row"), class = "cograph_bad_selection")
  expect_error(normalize_weights(m, "max"), class = "cograph_bad_selection")
})

test_that("a zero-node network does not crash the structural verbs", {
  base <- as_cograph(matrix(0, 1, 1, dimnames = list("A", "A")))
  empty <- suppressWarnings(filter_nodes(base, FALSE))

  expect_equal(length(suppressWarnings(split_components(empty))), 0L)
  expect_equal(n_nodes(suppressWarnings(contract_nodes(empty, character()))), 0L)
})

# --- attributes and metadata survive ----------------------------------------

test_that("an empty result keeps the extra edge columns in its tidy table", {
  el <- data.frame(from = c("A", "B"), to = c("B", "C"), weight = c(1, 2),
                   session = c("s1", "s2"), stringsAsFactors = FALSE)
  z <- suppressWarnings(filter_edges(as_cograph(el, directed = TRUE), FALSE))

  expect_true("session" %in% names(as.data.frame(z)))
})

test_that("an empty result keeps the estimation data", {
  m <- matrix(c(0, 2, 2, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  net <- as_cograph(m)
  net$data <- data.frame(raw = 1:2)

  expect_false(is.null(suppressWarnings(filter_nodes(net, FALSE))$data))
})

test_that("verbs with a one-to-one edge mapping keep extra edge columns", {
  el <- data.frame(from = c("A", "B"), to = c("B", "C"), weight = c(2, 4),
                   session = c("s1", "s2"), stringsAsFactors = FALSE)
  net <- as_cograph(el, directed = TRUE)

  expect_true("session" %in% names(get_edges(reverse_edges(net))))
  expect_true("session" %in% names(get_edges(normalize_weights(net, "max"))))
  expect_true("session" %in% names(get_edges(invert_weights(net))))
})

test_that("node attributes reach igraph", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(3)
  igraph::V(g)$name <- LETTERS[1:3]
  igraph::V(g)$color <- c("red", "green", "blue")

  out <- mutate_nodes(g, score = degree, keep_format = TRUE)

  expect_true(all(c("name", "color", "score") %in% igraph::vertex_attr_names(out)))
})

# --- the undirected diagonal -------------------------------------------------

test_that("an undirected self-loop is an edge, not a matrix-only artefact", {
  m <- matrix(5, 1, 1, dimnames = list("A", "A"))
  net <- as_cograph(m)

  expect_equal(n_edges(net), 1L)
  expect_equal(to_matrix(net)[1, 1], 5)
})

test_that("the undirected diagonal survives a matrix-level verb unchanged", {
  m <- matrix(5, 1, 1, dimnames = list("A", "A"))
  out <- symmetrize(as_cograph(m))

  expect_equal(n_edges(out), 1L)
  expect_equal(to_matrix(out)[1, 1], 5)
})

test_that("an undirected loop is counted once by the edge table", {
  m <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  m["A", "B"] <- m["B", "A"] <- 1
  m["A", "A"] <- 4

  expect_equal(n_edges(as_cograph(m)), 2L)
  expect_equal(sum(as.data.frame(as_cograph(m))$weight), 5)
})
