# =============================================================================
# Editing verbs: add_nodes, remove_nodes, add_edges, remove_edges,
# mutate_nodes, mutate_edges, bind_networks
# =============================================================================

we_und <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- m[2, 1] <- 0.5
  m[1, 3] <- m[3, 1] <- 0.8
  m[2, 4] <- m[4, 2] <- 0.6
  m[3, 4] <- m[4, 3] <- 0.4
  m
}

we_star <- function() {
  m <- matrix(c(0, 1, 1, 1,
                1, 0, 1, 0,
                1, 1, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
  dimnames(m) <- list(LETTERS[1:4], LETTERS[1:4])
  m
}

# --- add_nodes / remove_nodes ------------------------------------------------

test_that("add_nodes appends isolated nodes and keeps the edges", {
  bigger <- add_nodes(we_und(), labels = c("E", "F"))

  expect_equal(get_labels(bigger), LETTERS[1:6])
  expect_equal(n_edges(bigger), n_edges(as_cograph(we_und())))
  expect_equal(to_matrix(bigger)[1:4, 1:4], we_und())
})

test_that("add_nodes stores extra attributes and pads the existing rows", {
  bigger <- add_nodes(we_und(), labels = "E", cohort = "2026")
  nodes <- as.data.frame(bigger, what = "nodes")

  expect_equal(nodes$cohort, c(NA, NA, NA, NA, "2026"))
})

test_that("add_nodes refuses duplicate or existing labels", {
  expect_error(add_nodes(we_und(), labels = "A"), class = "cograph_bad_selection")
  expect_error(add_nodes(we_und(), labels = c("E", "E")), class = "cograph_bad_selection")
  expect_error(add_nodes(we_und(), labels = character(0)), class = "cograph_bad_selection")
})

test_that("remove_nodes drops the node and every edge touching it", {
  smaller <- remove_nodes(we_und(), nodes = "A")

  expect_equal(get_labels(smaller), c("B", "C", "D"))
  expect_equal(n_edges(smaller), 2L)
})

test_that("add_nodes then remove_nodes is the identity", {
  round_trip <- remove_nodes(add_nodes(we_und(), labels = "E"), nodes = "E")

  expect_equal(to_matrix(round_trip), we_und())
})

test_that("remove_nodes rejects an unknown node", {
  expect_error(remove_nodes(we_und(), nodes = "Z"), class = "cograph_bad_selection")
})

# --- add_edges / remove_edges ------------------------------------------------

test_that("add_edges creates the edge with the given weight", {
  bigger <- add_edges(we_und(), from = "A", to = "D", weight = 0.9)

  expect_equal(to_matrix(bigger)["A", "D"], 0.9)
  expect_equal(to_matrix(bigger)["D", "A"], 0.9)
  expect_equal(n_edges(bigger), 5L)
})

test_that("add_edges replaces an existing edge and says so", {
  expect_warning(out <- add_edges(we_und(), from = "A", to = "B", weight = 9),
                 class = "cograph_edges_replaced")
  expect_equal(to_matrix(out)["A", "B"], 9)
  expect_equal(n_edges(out), 4L)
})

test_that("add_edges carries extra edge attributes", {
  out <- add_edges(we_und(), from = "A", to = "D", weight = 1, kind = "new")

  expect_true("kind" %in% names(as.data.frame(out)))
  expect_equal(sum(!is.na(as.data.frame(out)$kind)), 1L)
})

test_that("remove_edges deletes exactly the named pairs", {
  smaller <- remove_edges(we_und(), from = "A", to = "B")

  expect_equal(n_edges(smaller), 3L)
  expect_equal(to_matrix(smaller)["A", "B"], 0)
})

test_that("remove_edges keeps the nodes", {
  smaller <- suppressWarnings(remove_edges(we_star(), from = "A", to = "D"))

  expect_equal(n_nodes(smaller), 4L)
})

test_that("remove_edges warns about pairs that carry no edge", {
  expect_warning(remove_edges(we_und(), from = "A", to = "D"),
                 class = "cograph_no_such_edge")
})

test_that("add_edges then remove_edges is the identity", {
  round_trip <- suppressWarnings(
    remove_edges(add_edges(we_und(), from = "A", to = "D", weight = 0.9),
                 from = "A", to = "D")
  )

  expect_equal(to_matrix(round_trip), we_und())
})

test_that("edge editing verbs reject mismatched from/to", {
  expect_error(add_edges(we_und(), from = c("A", "B"), to = "C"),
               class = "cograph_bad_selection")
  expect_error(remove_edges(we_und(), from = c("A", "B"), to = "C"),
               class = "cograph_bad_selection")
})

# --- mutate_nodes / mutate_edges ---------------------------------------------

test_that("mutate_nodes stores a computed measure as a node column", {
  out <- mutate_nodes(we_star(), deg = degree)
  nodes <- as.data.frame(out, what = "nodes")

  expect_equal(nodes$deg, c(3, 2, 2, 1))
})

test_that("mutate_nodes sees earlier expressions in later ones", {
  nodes <- as.data.frame(mutate_nodes(we_star(), deg = degree, twice = deg * 2),
                         what = "nodes")

  expect_equal(nodes$twice, c(6, 4, 4, 2))
})

test_that("mutate_nodes reaches the full centrality vocabulary", {
  nodes <- as.data.frame(mutate_nodes(we_star(), h = harmonic), what = "nodes")

  expect_equal(length(nodes$h), 4L)
  expect_false(anyNA(nodes$h))
})

test_that("mutate_nodes computes node predicates", {
  m <- we_star()
  nodes <- as.data.frame(
    mutate_nodes(m, leaf = is_leaf, cut = is_cut, tri = local_triangles),
    what = "nodes"
  )

  expect_equal(nodes$leaf, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(nodes$cut, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(nodes$tri, c(1, 1, 1, 0))
})

test_that("mutate_edges stores a computed metric as an edge column", {
  edges <- as.data.frame(mutate_edges(we_und(), strong = weight > 0.5))

  expect_equal(edges$strong, c(FALSE, TRUE, TRUE, FALSE))
})

test_that("mutate_edges computes edge predicates", {
  edges <- as.data.frame(
    mutate_edges(we_star(), bridge = is_bridge, loop = is_loop,
                 rank = weight_rank)
  )

  expect_true(is.logical(edges$bridge))
  expect_equal(edges$loop, rep(FALSE, nrow(edges)))
  expect_equal(edges$rank, rep(1, nrow(edges)))
})

test_that("mutate verbs require named expressions", {
  expect_error(mutate_nodes(we_und(), degree), class = "cograph_bad_selection")
  expect_error(mutate_edges(we_und(), weight > 1), class = "cograph_bad_selection")
  expect_error(mutate_nodes(we_und()), class = "cograph_bad_selection")
})

test_that("mutate verbs reject a wrong-length result", {
  expect_error(mutate_nodes(we_und(), bad = c(1, 2)),
               class = "cograph_bad_selection")
})

test_that("mutate_nodes leaves the structure alone", {
  out <- mutate_nodes(we_und(), deg = degree)

  expect_equal(to_matrix(out), we_und())
})

# --- bind_networks -----------------------------------------------------------

bind_a <- function() {
  m <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  m["A", "B"] <- m["B", "A"] <- 1
  m["B", "C"] <- m["C", "B"] <- 2
  m
}

bind_b <- function() {
  m <- matrix(0, 3, 3, dimnames = list(c("B", "C", "D"), c("B", "C", "D")))
  m["B", "C"] <- m["C", "B"] <- 3
  m["C", "D"] <- m["D", "C"] <- 4
  m
}

test_that("bind_networks('union') spans both node sets", {
  out <- bind_networks(bind_a(), bind_b())

  expect_equal(get_labels(out), c("A", "B", "C", "D"))
  expect_equal(n_edges(out), 3L)
})

test_that("bind_networks sums the weight of a shared edge by default", {
  out <- to_matrix(bind_networks(bind_a(), bind_b()))

  expect_equal(out["B", "C"], 5)
})

test_that("bind_networks(weight) honours the combination rule", {
  expect_equal(to_matrix(bind_networks(bind_a(), bind_b(), weight = "max"))["B", "C"], 3)
  expect_equal(to_matrix(bind_networks(bind_a(), bind_b(), weight = "min"))["B", "C"], 2)
  expect_equal(to_matrix(bind_networks(bind_a(), bind_b(), weight = "mean"))["B", "C"], 2.5)
  expect_equal(to_matrix(bind_networks(bind_a(), bind_b(), weight = "first"))["B", "C"], 2)
})

test_that("bind_networks('intersection') keeps only the shared edges", {
  out <- bind_networks(bind_a(), bind_b(), method = "intersection")

  expect_equal(get_labels(out), c("B", "C"))
  expect_equal(n_edges(out), 1L)
})

test_that("bind_networks('difference') keeps what only x has", {
  out <- as.data.frame(bind_networks(bind_a(), bind_b(), method = "difference"))

  expect_equal(nrow(out), 1L)
  expect_equal(out$from, "A")
  expect_equal(out$to, "B")
})

test_that("bind_networks union is commutative in its edge set", {
  ab <- to_matrix(bind_networks(bind_a(), bind_b()))
  ba <- to_matrix(bind_networks(bind_b(), bind_a()))
  labels <- sort(rownames(ab))

  expect_equal(ab[labels, labels], ba[labels, labels])
})

test_that("bind_networks with a network of its own is idempotent under max", {
  once <- to_matrix(bind_networks(bind_a(), bind_a(), weight = "max"))

  expect_equal(unname(once), unname(bind_a()))
})

test_that("bind_networks errors when the node sets do not meet", {
  a <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  a["A", "B"] <- a["B", "A"] <- 1
  b <- matrix(0, 2, 2, dimnames = list(c("Y", "Z"), c("Y", "Z")))
  b["Y", "Z"] <- b["Z", "Y"] <- 1

  expect_error(bind_networks(a, b, method = "intersection"),
               class = "cograph_bad_selection")
})
