# =============================================================================
# Structural wrangling verbs: remove_isolates, to_undirected, to_directed,
# reverse_edges, split_components, select_k_core, spanning_tree,
# complement_network, contract_nodes, reorder_nodes, rename_nodes
#
# Where igraph offers the same operation, the result is checked against it.
# =============================================================================

ws_dir <- function() {
  m <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  m[1, 2] <- 0.5
  m[2, 1] <- 0.2
  m[2, 3] <- 0.7
  m
}

ws_und <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- m[2, 1] <- 0.5
  m[1, 3] <- m[3, 1] <- 0.8
  m[2, 4] <- m[4, 2] <- 0.6
  m[3, 4] <- m[4, 3] <- 0.4
  m
}

ws_split <- function() {
  m <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
  m[1, 2] <- m[2, 1] <- 1
  m[2, 3] <- m[3, 2] <- 1
  m[4, 5] <- m[5, 4] <- 1
  m
}

ws_core <- function() {
  m <- matrix(c(0, 1, 1, 1,
                1, 0, 1, 0,
                1, 1, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
  dimnames(m) <- list(LETTERS[1:4], LETTERS[1:4])
  m
}

# --- remove_isolates ---------------------------------------------------------

test_that("remove_isolates drops exactly the edgeless nodes", {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m[1, 2] <- m[2, 1] <- 1

  expect_equal(get_labels(remove_isolates(m)), c("A", "B"))
})

test_that("remove_isolates is a no-op when nothing is isolated", {
  expect_equal(to_matrix(remove_isolates(ws_und())), ws_und())
})

test_that("remove_isolates on an edgeless network gives an empty one", {
  m <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))

  expect_equal(n_nodes(remove_isolates(m)), 0L)
})

# --- to_undirected / to_directed / reverse_edges -----------------------------

test_that("to_undirected computes the documented combination", {
  d <- ws_dir()

  expect_equal(to_matrix(to_undirected(d, "sum"))["A", "B"], 0.7)
  expect_equal(to_matrix(to_undirected(d, "max"))["A", "B"], 0.5)
  expect_equal(to_matrix(to_undirected(d, "min"))["A", "B"], 0.2)
  expect_equal(to_matrix(to_undirected(d, "mean"))["A", "B"], 0.35)
})

test_that("to_undirected('mutual') keeps only reciprocated pairs", {
  m <- to_matrix(to_undirected(ws_dir(), "mutual"))

  expect_equal(m["A", "B"], 0.2)
  expect_equal(m["B", "C"], 0)
})

test_that("to_undirected agrees with igraph::as_undirected on the collapse rules", {
  skip_if_not_installed("igraph")
  d <- ws_dir()
  g <- igraph::graph_from_adjacency_matrix(d, mode = "directed", weighted = TRUE)

  ig_sum <- igraph::as_undirected(g, mode = "collapse",
                                  edge.attr.comb = list(weight = "sum"))
  ig_mat <- as.matrix(igraph::as_adjacency_matrix(ig_sum, attr = "weight"))

  expect_equal(unname(to_matrix(to_undirected(d, "sum"))), unname(ig_mat))
})

test_that("to_directed('mutual') mirrors every edge", {
  u <- ws_und()
  out <- to_matrix(to_directed(u))

  expect_true(is_directed(to_directed(u)))
  expect_equal(unname(out), unname(u))
})

test_that("to_directed('arbitrary') keeps one arc per edge", {
  out <- to_directed(ws_und(), mode = "arbitrary")

  expect_equal(n_edges(out), 4L)
  expect_true(all(to_matrix(out)[lower.tri(to_matrix(out))] == 0))
})

test_that("reverse_edges transposes the weight matrix", {
  d <- ws_dir()

  expect_equal(unname(to_matrix(reverse_edges(d))), unname(t(d)))
})

test_that("reverse_edges is its own inverse", {
  d <- ws_dir()

  expect_equal(to_matrix(reverse_edges(reverse_edges(d))), d)
})

test_that("reverse_edges on an undirected network warns and changes nothing", {
  expect_warning(out <- reverse_edges(ws_und()), class = "cograph_no_effect")
  expect_equal(to_matrix(out), ws_und())
})

# --- split_components --------------------------------------------------------

test_that("split_components returns one network per component, largest first", {
  parts <- split_components(ws_split())

  expect_equal(length(parts), 2L)
  expect_equal(get_labels(parts$component_1), c("A", "B", "C"))
  expect_equal(get_labels(parts$component_2), c("D", "E"))
})

test_that("split_components partitions the nodes exactly", {
  parts <- split_components(ws_split())
  all_labels <- unlist(lapply(parts, get_labels), use.names = FALSE)

  expect_equal(sort(all_labels), LETTERS[1:5])
  expect_equal(anyDuplicated(all_labels), 0L)
})

test_that("split_components(min_size) drops the small components", {
  parts <- split_components(ws_split(), min_size = 3)

  expect_equal(length(parts), 1L)
})

test_that("split_components agrees with igraph on the component count", {
  skip_if_not_installed("igraph")
  m <- ws_split()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", weighted = TRUE)

  expect_equal(length(split_components(m)), igraph::components(g)$no)
})

# --- select_k_core -----------------------------------------------------------

test_that("select_k_core keeps the nodes of coreness at least k", {
  expect_equal(get_labels(select_k_core(ws_core(), k = 2)), c("A", "B", "C"))
  expect_equal(get_labels(select_k_core(ws_core(), k = 1)), LETTERS[1:4])
})

test_that("select_k_core agrees with igraph coreness", {
  skip_if_not_installed("igraph")
  m <- ws_core()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")
  expected <- names(igraph::coreness(g))[igraph::coreness(g) >= 2]

  expect_equal(get_labels(select_k_core(m, k = 2)), expected)
})

test_that("select_k_core warns and returns empty when k is unreachable", {
  expect_warning(out <- select_k_core(ws_core(), k = 10))
  expect_equal(n_nodes(out), 0L)
})

test_that("the k-core is nested in the (k-1)-core", {
  core2 <- get_labels(select_k_core(ws_core(), k = 2))
  core1 <- get_labels(select_k_core(ws_core(), k = 1))

  expect_true(all(core2 %in% core1))
})

# --- spanning_tree -----------------------------------------------------------

test_that("spanning_tree returns n - 1 edges on a connected network", {
  tree <- spanning_tree(ws_und())

  expect_equal(n_edges(tree), n_nodes(tree) - 1L)
})

test_that("spanning_tree minimises total weight", {
  total <- sum(as.data.frame(spanning_tree(ws_und()))$weight)

  # The graph is the 4-cycle A-B-D-C-A; the minimum tree drops its heaviest
  # edge, A-C (.8), leaving .5 + .6 + .4.
  expect_equal(total, 1.5)
})

test_that("spanning_tree(maximum = TRUE) maximises total weight", {
  total <- sum(as.data.frame(spanning_tree(ws_und(), maximum = TRUE))$weight)

  # The maximum tree drops the cycle's lightest edge, C-D (.4).
  expect_equal(total, 1.9)
})

test_that("spanning_tree agrees with igraph::mst on total weight", {
  skip_if_not_installed("igraph")
  m <- ws_und()
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", weighted = TRUE)
  ig_total <- sum(igraph::E(igraph::mst(g))$weight)

  expect_equal(sum(as.data.frame(spanning_tree(m))$weight), ig_total)
})

test_that("spanning_tree on a disconnected network gives a forest", {
  forest <- spanning_tree(ws_split())

  expect_equal(n_edges(forest), 3L)
  expect_equal(n_nodes(forest), 5L)
})

test_that("spanning_tree keeps every node", {
  expect_equal(n_nodes(spanning_tree(ws_und())), 4L)
})

# --- complement_network ------------------------------------------------------

test_that("complement_network joins exactly the non-adjacent pairs", {
  m <- matrix(c(0, 1, 0,
                1, 0, 0,
                0, 0, 0), 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  comp <- to_matrix(complement_network(m))

  expect_equal(comp["A", "B"], 0)
  expect_equal(comp["A", "C"], 1)
  expect_equal(diag(comp), c(A = 0, B = 0, C = 0))
})

test_that("complement of the complement is the original edge set", {
  m <- (ws_und() != 0) * 1
  twice <- to_matrix(complement_network(complement_network(m)))

  expect_equal(unname(twice), unname(m))
})

test_that("complement_network agrees with igraph::complementer", {
  skip_if_not_installed("igraph")
  m <- (ws_und() != 0) * 1
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected")
  ig <- as.matrix(igraph::as_adjacency_matrix(igraph::complementer(g)))

  expect_equal(unname(to_matrix(complement_network(m))), unname(ig))
})

# --- contract_nodes ----------------------------------------------------------

test_that("contract_nodes aggregates the between-group edges", {
  m <- matrix(c(0, 1, 1, 0,
                1, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 1, 0), 4, 4, byrow = TRUE,
              dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  out <- to_matrix(contract_nodes(m, groups = c("L", "L", "R", "R")))

  # A-C and B-D cross the split, so the L-R weight is 2.
  expect_equal(out["L", "R"], 2)
  expect_equal(out["L", "L"], 0)
})

test_that("contract_nodes(loops = TRUE) keeps the within-group edges", {
  m <- matrix(c(0, 1, 1, 0,
                1, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 1, 0), 4, 4, byrow = TRUE,
              dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  out <- to_matrix(contract_nodes(m, groups = c("L", "L", "R", "R"), loops = TRUE))

  expect_equal(out["L", "L"], 2)
})

test_that("contract_nodes conserves total weight under the sum rule", {
  m <- ws_und()
  out <- to_matrix(contract_nodes(m, groups = c("L", "L", "R", "R"), loops = TRUE))

  expect_equal(sum(out), sum(m))
})

test_that("contract_nodes accepts a named list of groups", {
  out <- contract_nodes(ws_und(), groups = list(L = c("A", "B"), R = c("C", "D")))

  expect_equal(get_labels(out), c("L", "R"))
})

test_that("contract_nodes rejects an incomplete or wrong-length assignment", {
  expect_error(contract_nodes(ws_und(), groups = c("L", "L", "R")),
               class = "cograph_bad_selection")
  expect_error(contract_nodes(ws_und(), groups = list(L = c("A", "B"))),
               class = "cograph_bad_selection")
  expect_error(contract_nodes(ws_und(), groups = list(L = c("A", "Z"), R = c("B", "C", "D"))),
               class = "cograph_bad_selection")
})

# --- reorder_nodes / rename_nodes -------------------------------------------

test_that("reorder_nodes permutes the nodes and remaps the edges", {
  reordered <- reorder_nodes(ws_und(), order = c("D", "C", "B", "A"))

  expect_equal(get_labels(reordered), c("D", "C", "B", "A"))
  # The network itself is unchanged: A-C is still 0.8 whatever the row order.
  expect_equal(to_matrix(reordered)["A", "C"], 0.8)
})

test_that("reorder_nodes by degree sorts descending", {
  ordered <- reorder_nodes(ws_core(), order = "degree")
  cg <- cograph:::.cg_graph(as_cograph(to_matrix(ordered)))
  degrees <- cograph:::.cg_degree(cg$b, cg$directed, "all")

  expect_false(is.unsorted(rev(degrees)))
})

test_that("reorder_nodes rejects a partial ordering", {
  expect_error(reorder_nodes(ws_und(), order = c("A", "B")),
               class = "cograph_bad_selection")
  expect_error(reorder_nodes(ws_und(), order = c("A", "B", "C", "Z")),
               class = "cograph_bad_selection")
})

test_that("rename_nodes changes labels without touching the structure", {
  renamed <- rename_nodes(ws_und(), from = c(A = "Alpha", B = "Beta"))

  expect_equal(get_labels(renamed), c("Alpha", "Beta", "C", "D"))
  expect_equal(unname(to_matrix(renamed)), unname(ws_und()))
})

test_that("rename_nodes accepts from/to vectors", {
  renamed <- rename_nodes(ws_und(), from = "A", to = "Alpha")

  expect_equal(get_labels(renamed)[1], "Alpha")
})

test_that("rename_nodes refuses to create a duplicate label", {
  expect_error(rename_nodes(ws_und(), from = "A", to = "B"),
               class = "cograph_bad_selection")
  expect_error(rename_nodes(ws_und(), from = "Z", to = "Y"),
               class = "cograph_bad_selection")
  expect_error(rename_nodes(ws_und(), from = c("A", "B"), to = "One"),
               class = "cograph_bad_selection")
})

# --- format and metadata invariants -----------------------------------------

test_that("structure verbs keep node groups", {
  net <- set_groups(as_cograph(ws_und()), list(G1 = c("A", "B"), G2 = c("C", "D")))

  expect_false(is.null(get_groups(remove_isolates(net))))
  expect_false(is.null(get_groups(to_undirected(net))))
  expect_equal(sort(get_groups(rename_nodes(net, from = c(A = "Alpha")))$node),
               c("Alpha", "B", "C", "D"))
})

test_that("keep_format returns a matrix for matrix input", {
  out <- to_undirected(ws_dir(), keep_format = TRUE)

  expect_true(is.matrix(out))
  expect_true(isSymmetric(out))
})
