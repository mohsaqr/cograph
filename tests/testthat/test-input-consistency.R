# One network, many input types: every verb reads the same weights.

skip_on_cran()

regulation_inputs <- function() {
  skip_if_not_installed("tna")
  skip_if_not_installed("igraph")
  sequences <- tna::tna(tna::group_regulation)
  w <- sequences$weights
  edge_list <- subset(as.data.frame(as.table(w), responseName = "weight",
                                    stringsAsFactors = FALSE), weight > 0)
  names(edge_list) <- c("from", "to", "weight")
  list(matrix = w,
       igraph = igraph::graph_from_adjacency_matrix(w, mode = "directed",
                                                    weighted = TRUE),
       cograph = as_cograph(w),
       tna_matrix = tna::tna(w),
       tna_sequences = sequences,
       edge_list = edge_list)
}

# ---- a tna model built from sequences keeps its weights --------------------

test_that("to_matrix and to_df return the model weights of a sequence tna", {
  inp <- regulation_inputs()
  w <- inp$matrix
  edges <- to_df(inp$tna_sequences)
  expect_equal(nrow(edges), sum(w != 0))
  expect_equal(sum(edges$weight), sum(w))      # rows of a tna sum to 1
  expect_equal(sum(edges$weight), 9)
})

test_that("wrangling verbs give the same network for both kinds of tna", {
  inp <- regulation_inputs()
  verbs <- list(
    threshold = function(x) threshold_edges(x, minimum = 0.1),
    reverse = function(x) reverse_edges(x),
    isolates = function(x) remove_isolates(x),
    invert = function(x) invert_weights(x),
    binarize = function(x) binarize(x),
    symmetrize = function(x) symmetrize(x),
    undirected = function(x) to_undirected(x)
  )
  lapply(names(verbs), function(name) {
    from_sequences <- to_matrix(verbs[[name]](inp$tna_sequences))
    from_matrix <- to_matrix(verbs[[name]](inp$matrix))
    expect_equal(from_sequences, from_matrix, ignore_attr = TRUE, label = name)
  })
})

test_that("as_cograph still reads a sequence tna as a temporal multigraph", {
  inp <- regulation_inputs()
  net <- as_cograph(inp$tna_sequences)
  expect_true("session" %in% names(get_edges(net)))
  expect_gt(n_edges(net), sum(inp$matrix != 0))
})

# ---- accessors take every network input ------------------------------------

test_that("accessors agree across input types", {
  inp <- regulation_inputs()
  expect_identical(unique(vapply(inp, n_nodes, integer(1), USE.NAMES = FALSE)),
                   9L)
  expect_identical(unique(vapply(inp, n_edges, integer(1), USE.NAMES = FALSE)),
                   78L)
  expect_true(all(vapply(inp, is_directed, logical(1))))
  expect_true(all(vapply(inp, function(x) nrow(get_nodes(x)), integer(1)) == 9))
  expect_true(all(vapply(inp, function(x) nrow(get_edges(x)), integer(1)) == 78))
  expect_setequal(get_labels(inp$edge_list), rownames(inp$matrix))
  expect_identical(get_labels(inp$tna_sequences), rownames(inp$matrix))
})

test_that("accessors still refuse what is not a network", {
  expect_error(n_nodes("a"), "Cannot count nodes")
  expect_error(get_edges(list(1)), "Cannot extract edges")
  expect_error(is_directed(NULL), "Cannot determine directedness")
})

# ---- censuses, filters and plots take tna models and edge lists ------------

test_that("triad_census and motif_census agree across input types", {
  inp <- regulation_inputs()
  reference <- triad_census(inp$matrix)
  lapply(names(inp), function(name) {
    expect_equal(triad_census(inp[[name]]), reference, label = name)
  })
  census <- motif_census(inp$tna_sequences, n_random = 5, seed = 1)
  expect_equal(census$counts,
               motif_census(inp$matrix, n_random = 5, seed = 1)$counts)
})

test_that("an edge list is accepted by disparity_filter, simplify and plots", {
  inp <- regulation_inputs()
  backbone <- disparity_filter(inp$edge_list, level = 0.3)
  expect_s3_class(backbone, "tna_disparity")
  expect_equal(sum(backbone$significant),
               sum(disparity_filter(inp$matrix, level = 0.3)))
  expect_s3_class(simplify(inp$edge_list), "cograph_network")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot_heatmap(inp$edge_list))
  expect_no_error(plot_chord(inp$edge_list))
})

test_that("motifs reads a matrix-built tna model as an aggregate network", {
  inp <- regulation_inputs()
  from_tna <- motifs(inp$tna_matrix, seed = 1)
  from_matrix <- motifs(inp$matrix, seed = 1)
  expect_equal(as.data.frame(from_tna), as.data.frame(from_matrix))
})

test_that("community counters refuse a network", {
  inp <- regulation_inputs()
  expect_error(n_communities(inp$matrix), class = "cograph_bad_input")
  expect_error(community_sizes(inp$cograph), class = "cograph_bad_input")
  expect_error(n_communities(inp$igraph), class = "cograph_bad_input")
  found <- community_walktrap(inp$matrix)
  expect_equal(sum(community_sizes(found)), 9)
  expect_equal(n_communities(found), length(community_sizes(found)))
})

# ---- transitivity: one formula in network_summary() and centrality() -------

test_that("network_summary transitivity is the centrality() value", {
  inp <- regulation_inputs()
  w <- inp$matrix
  # every pair of states is tied, so every connected triple is closed
  expect_true(all((w + t(w))[upper.tri(w)] > 0))
  from_centrality <- unique(centrality(w, measures = "transitivity",
                                       transitivity_type = "global",
                                       tna_network = FALSE)$transitivity)
  expect_equal(from_centrality, 1)
  values <- vapply(inp, function(x) network_summary(x)$transitivity, numeric(1))
  expect_equal(unname(values), rep(1, length(inp)))
})

test_that("network_summary transitivity does not depend on node order", {
  inp <- regulation_inputs()
  w <- inp$matrix
  orders <- list(rev(seq_len(9)), c(3, 7, 1, 9, 5, 2, 8, 4, 6))
  lapply(orders, function(o) {
    expect_equal(network_summary(w[o, o])$transitivity, 1)
  })
  detailed <- network_summary(w, detailed = TRUE)
  shuffled <- network_summary(w[orders[[2]], orders[[2]]], detailed = TRUE)
  expect_equal(shuffled$mean_local_transitivity,
               detailed$mean_local_transitivity)
})

test_that("network_summary transitivity matches hand counts", {
  # skeleton: triangle a-b-c plus pendant c-d -> 1 triangle, 5 triples = 3/5,
  # drawn with reciprocated arcs and self-loops
  w <- matrix(0, 4, 4, dimnames = list(letters[1:4], letters[1:4]))
  w["a", "b"] <- w["b", "a"] <- w["b", "c"] <- w["c", "a"] <- w["c", "d"] <- 1
  diag(w) <- 1
  expect_equal(network_summary(w, digits = NULL)$transitivity, 3 / 5)
  expect_equal(network_summary(w[c(3, 1, 4, 2), c(3, 1, 4, 2)],
                               digits = NULL)$transitivity, 3 / 5)
  skip_if_not_installed("igraph")
  expect_equal(network_summary(igraph::make_full_graph(5))$transitivity, 1)
})
