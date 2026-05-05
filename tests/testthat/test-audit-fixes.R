# =============================================================================
# Tests for audit-identified bugs — ground truth equivalence
# =============================================================================

# ---------------------------------------------------------------------------
# Bug 1: detect_duplicate_edges / aggregate_duplicate_edges must respect
#         directed vs undirected semantics
# Ground truth: igraph::simplify() — the reference implementation
# ---------------------------------------------------------------------------

test_that("directed: A->B and B->A are distinct edges, not duplicates", {
  edges <- data.frame(
    from   = c(1, 2, 1),
    to     = c(2, 1, 3),
    weight = c(0.5, 0.8, 0.3)
  )


  # Undirected: 1-2 appears twice (A->B and B->A canonicalize to same key)
  res_undir <- detect_duplicate_edges(edges, directed = FALSE)
  expect_true(res_undir$has_duplicates)


  # Directed: 1->2 and 2->1 are distinct — no duplicates
  res_dir <- detect_duplicate_edges(edges, directed = TRUE)
  expect_false(res_dir$has_duplicates)
})


test_that("directed: true duplicates (same direction) are still caught", {
  edges <- data.frame(
    from   = c(1, 1, 2),
    to     = c(2, 2, 1),
    weight = c(0.5, 0.3, 0.8)
  )

  res <- detect_duplicate_edges(edges, directed = TRUE)
  expect_true(res$has_duplicates)
  expect_equal(length(res$info), 1)
  expect_equal(res$info[[1]]$count, 2)
})


test_that("aggregate_duplicate_edges preserves direction for directed graphs", {
  edges <- data.frame(
    from   = c(1, 2, 1),
    to     = c(2, 1, 2),
    weight = c(0.4, 0.8, 0.6)
  )

  # Directed aggregation: 1->2 appears twice (sum = 1.0), 2->1 once (0.8)
  agg_dir <- aggregate_duplicate_edges(edges, method = "sum", directed = TRUE)
  expect_equal(nrow(agg_dir), 2)
  row_12 <- agg_dir[agg_dir$from == 1 & agg_dir$to == 2, ]
  row_21 <- agg_dir[agg_dir$from == 2 & agg_dir$to == 1, ]
  expect_equal(row_12$weight, 1.0)
  expect_equal(row_21$weight, 0.8)

  # Undirected aggregation: all three merge to one canonical edge 1-2
  agg_undir <- aggregate_duplicate_edges(edges, method = "sum", directed = FALSE)
  expect_equal(nrow(agg_undir), 1)
  expect_equal(agg_undir$weight, 1.8)
})


test_that("aggregate_duplicate_edges directed matches igraph::simplify", {
  # Build a directed igraph with duplicate 1->2 edges
  g <- igraph::make_empty_graph(n = 4, directed = TRUE)
  g <- igraph::add_edges(g, c(1,2, 1,2, 2,1, 1,3, 3,4))
  igraph::E(g)$weight <- c(0.3, 0.7, 0.5, 0.4, 0.6)

  g_simple <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE,
                                edge.attr.comb = list(weight = "sum"))

  # Extract igraph result as edge list
  ig_edges <- igraph::as_data_frame(g_simple, what = "edges")

  # Build the same edge list for cograph
  co_edges <- data.frame(
    from   = c(1, 1, 2, 1, 3),
    to     = c(2, 2, 1, 3, 4),
    weight = c(0.3, 0.7, 0.5, 0.4, 0.6)
  )
  co_agg <- aggregate_duplicate_edges(co_edges, method = "sum", directed = TRUE)

  # Both should produce 4 edges after merging 1->2 duplicates

  expect_equal(nrow(co_agg), nrow(ig_edges))

  # Compare edge weights — sort both by from,to for stable comparison
  ig_sorted <- ig_edges[order(ig_edges$from, ig_edges$to), ]
  co_sorted <- co_agg[order(co_agg$from, co_agg$to), ]
  expect_equal(co_sorted$weight, ig_sorted$weight)
  expect_equal(co_sorted$from, ig_sorted$from)
  expect_equal(co_sorted$to, ig_sorted$to)
})


test_that("aggregate_duplicate_edges undirected matches igraph::simplify", {
  # Undirected igraph: 1--2 with two edges, 2--1 with another
  g <- igraph::make_empty_graph(n = 3, directed = FALSE)
  g <- igraph::add_edges(g, c(1,2, 1,2, 2,3))
  igraph::E(g)$weight <- c(0.3, 0.7, 0.5)

  g_simple <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE,
                                edge.attr.comb = list(weight = "mean"))
  ig_edges <- igraph::as_data_frame(g_simple, what = "edges")

  co_edges <- data.frame(
    from   = c(1, 1, 2),
    to     = c(2, 2, 3),
    weight = c(0.3, 0.7, 0.5)
  )
  co_agg <- aggregate_duplicate_edges(co_edges, method = "mean", directed = FALSE)

  expect_equal(nrow(co_agg), nrow(ig_edges))

  ig_sorted <- ig_edges[order(ig_edges$from, ig_edges$to), ]
  co_sorted <- co_agg[order(co_agg$from, co_agg$to), ]
  expect_equal(co_sorted$weight, ig_sorted$weight)
})


test_that("simplify.cograph_network respects directedness", {
  # Directed network: 1->2 and 2->1 should both survive
  dir_mat <- matrix(c(0, 0.5, 0, 0.8, 0, 0, 0, 0.3, 0), 3, 3, byrow = TRUE)
  rownames(dir_mat) <- colnames(dir_mat) <- c("A", "B", "C")
  net_dir <- as_cograph(dir_mat)
  expect_true(cograph::is_directed(net_dir))

  edges_before <- get_edges(net_dir)
  net_simplified <- simplify(net_dir)
  edges_after <- get_edges(net_simplified)

  # A->B (0.5) and B->A (0.8) are distinct — both must survive
  expect_true(any(edges_after$from == 1 & edges_after$to == 2))
  expect_true(any(edges_after$from == 2 & edges_after$to == 1))
  expect_equal(nrow(edges_before), nrow(edges_after))
})


test_that("simplify.cograph_network directed matches igraph::simplify", {
  # 5-node directed network with some reciprocal edges
  mat <- matrix(0, 5, 5)
  mat[1, 2] <- 0.3; mat[2, 1] <- 0.7
  mat[1, 3] <- 0.5; mat[3, 1] <- 0.2
  mat[2, 3] <- 0.4
  mat[4, 5] <- 0.9; mat[5, 4] <- 0.1
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  # igraph reference
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE)
  g_simple <- igraph::simplify(g, remove.loops = TRUE, remove.multiple = TRUE)
  ig_n <- igraph::ecount(g_simple)

  # cograph
  net <- as_cograph(mat)
  net_s <- simplify(net)
  co_n <- nrow(get_edges(net_s))

  expect_equal(co_n, ig_n)
})


# ---------------------------------------------------------------------------
# Bug 2: layout$name should be layout$get_type()
# Ground truth: CographLayout R6 class API
# ---------------------------------------------------------------------------

test_that("CographLayout has no public $name field", {
  layout <- CographLayout$new("spring")
  # $name should be NULL (no such field)
  expect_null(layout$name)
  # get_type() should work
  expect_equal(layout$get_type(), "spring")
})


test_that("compute_layout_for_cograph records correct layout name via CographLayout", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # String layout via cograph()
  net_str <- cograph(mat, layout = "circle")
  expect_equal(net_str$meta$layout$name, "circle")

  # CographLayout object via compute_layout_for_cograph
  net <- as_cograph(mat)
  layout_obj <- CographLayout$new("circle")
  net_r6 <- compute_layout_for_cograph(net, layout = layout_obj)
  # Before the fix this returned "custom" (layout$name was NULL)
  expect_equal(net_r6$meta$layout$name, "circle")
})


# ---------------------------------------------------------------------------
# Bug 3: is_directed() must work on R6 CographNetwork, S3 cograph_network,
#         and igraph objects
# Ground truth: direct property access on each object type
# ---------------------------------------------------------------------------

test_that("is_directed works on R6 CographNetwork (directed)", {
  mat <- matrix(c(0, 1, 0, 0), 2, 2)
  r6 <- CographNetwork$new(mat)
  # R6 active binding
  expect_true(r6$is_directed)
  # Global function
  expect_true(cograph::is_directed(r6))
})

test_that("is_directed works on R6 CographNetwork (undirected)", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  r6 <- CographNetwork$new(mat)
  expect_false(r6$is_directed)
  expect_false(cograph::is_directed(r6))
})

test_that("is_directed works on S3 cograph_network", {
  mat_dir <- matrix(c(0, 0.5, 0, 0.8, 0, 0, 0, 0.3, 0), 3, 3, byrow = TRUE)
  rownames(mat_dir) <- colnames(mat_dir) <- c("A", "B", "C")
  net_dir <- as_cograph(mat_dir)
  expect_true(cograph::is_directed(net_dir))

  mat_undir <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat_undir) <- colnames(mat_undir) <- c("A", "B", "C")
  net_undir <- as_cograph(mat_undir)
  expect_false(cograph::is_directed(net_undir))
})


test_that("is_directed agrees across R6, S3, and igraph for the same matrix", {
  # Directed
  mat_dir <- matrix(c(0, 1, 0, 0), 2, 2)
  rownames(mat_dir) <- colnames(mat_dir) <- c("A", "B")

  r6_dir  <- CographNetwork$new(mat_dir)
  s3_dir  <- as_cograph(mat_dir)
  ig_dir  <- igraph::graph_from_adjacency_matrix(mat_dir, mode = "directed",
                                                  weighted = TRUE)

  expect_identical(cograph::is_directed(r6_dir), cograph::is_directed(s3_dir))
  expect_identical(cograph::is_directed(s3_dir), igraph::is_directed(ig_dir))

  # Undirected
  mat_undir <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat_undir) <- colnames(mat_undir) <- c("A", "B")

  r6_undir  <- CographNetwork$new(mat_undir)
  s3_undir  <- as_cograph(mat_undir)
  ig_undir  <- igraph::graph_from_adjacency_matrix(mat_undir, mode = "undirected",
                                                    weighted = TRUE)

  expect_identical(cograph::is_directed(r6_undir), cograph::is_directed(s3_undir))
  expect_identical(cograph::is_directed(s3_undir), igraph::is_directed(ig_undir))
})


# ---------------------------------------------------------------------------
# Edge case: backward compatibility — defaults unchanged
# ---------------------------------------------------------------------------

test_that("detect_duplicate_edges default (directed=FALSE) preserves old behavior", {
  edges <- data.frame(from = c(1, 2), to = c(2, 1), weight = c(0.5, 0.8))
  # Default = undirected: 1-2 and 2-1 are duplicates
  res <- detect_duplicate_edges(edges)
  expect_true(res$has_duplicates)
})

test_that("aggregate_duplicate_edges default (directed=FALSE) preserves old behavior", {
  edges <- data.frame(from = c(1, 2), to = c(2, 1), weight = c(0.5, 0.8))
  agg <- aggregate_duplicate_edges(edges, method = "mean")
  expect_equal(nrow(agg), 1)
  expect_equal(agg$weight, 0.65)
})


# ---------------------------------------------------------------------------
# Bug 4: fast_greedy must recompute weights after directed -> undirected collapse
# ---------------------------------------------------------------------------

test_that("detect_communities fast_greedy handles collapsed directed weights", {
  mat <- matrix(c(
    0, 1, 2, 0,
    3, 0, 4, 0,
    0, 0, 0, 5,
    6, 0, 0, 0
  ), 4, 4, byrow = TRUE)

  result <- detect_communities(mat, method = "fast_greedy", directed = TRUE)
  expect_s3_class(result, "cograph_communities")
  expect_equal(nrow(result), 4)
})


# ---------------------------------------------------------------------------
# Bug 5: parse_edgelist auto-detect should not treat duplicate same-direction
# rows as directed
# ---------------------------------------------------------------------------

test_that("parse_edgelist duplicate same-direction rows remain undirected", {
  df <- data.frame(from = c("A", "A"), to = c("B", "B"))
  result <- parse_edgelist(df)
  expect_false(result$directed)
})

test_that("parse_edgelist reciprocal rows are directed", {
  df <- data.frame(from = c("A", "B"), to = c("B", "A"))
  result <- parse_edgelist(df)
  expect_true(result$directed)
})


# ---------------------------------------------------------------------------
# Bug 6: custom layout coordinates must have exactly one row per node
# ---------------------------------------------------------------------------

test_that("cograph rejects custom layout row-count mismatch", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(
    cograph(mat, layout = data.frame(x = 0, y = 0)),
    "one row per node"
  )
})

test_that("sn_layout rejects custom layout row-count mismatch", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- cograph(mat)
  expect_error(
    sn_layout(net, data.frame(x = 0, y = 0)),
    "one row per node"
  )
})

test_that("CographNetwork rejects custom layout row-count mismatch", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- CographNetwork$new(mat)
  expect_error(
    net$set_layout_coords(data.frame(x = 0, y = 0)),
    "one row per node"
  )
})


# ---------------------------------------------------------------------------
# Bug 7: filter predicates must be logical and length 1 or n
# ---------------------------------------------------------------------------

test_that("filter_nodes rejects wrong-length logical predicates", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(
    filter_nodes(as_cograph(mat), c(TRUE, FALSE, TRUE)),
    "length 1 or 2"
  )
})

test_that("filter_edges rejects wrong-length logical predicates", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(
    filter_edges(as_cograph(mat), c(TRUE, FALSE, TRUE)),
    "length 1 or 1"
  )
})

test_that("filter_nodes rejects non-logical predicates", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(
    filter_nodes(as_cograph(mat), c(1, 0)),
    "logical vectors"
  )
})


# ---------------------------------------------------------------------------
# Bug 8: signed psychometric expected influence should normalize by max abs
# ---------------------------------------------------------------------------

test_that("psych_network normalizes signed expected influence by max abs", {
  w <- matrix(
    c(
      0, -0.8, 0.2,
      -0.8, 0, 0.1,
      0.2, 0.1, 0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )

  raw <- centrality(
    w,
    measures = "expected_influence_1",
    normalized = FALSE
  )[["expected_influence_1_all"]]
  expected <- raw / max(abs(raw))

  explicit <- centrality(
    w,
    measures = "expected_influence_1",
    normalized = TRUE,
    psych_network = TRUE
  )[["expected_influence_1_all"]]
  auto <- centrality(
    w,
    measures = "expected_influence_1",
    normalized = TRUE,
    psych_network = NULL
  )[["expected_influence_1_all"]]

  expect_equal(explicit, expected)
  expect_equal(auto, expected)
  expect_lte(max(abs(explicit)), 1)
})

test_that("psych_network can be disabled to preserve generic normalization", {
  w <- matrix(
    c(
      0, -0.8, 0.2,
      -0.8, 0, 0.1,
      0.2, 0.1, 0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )

  raw <- centrality(
    w,
    measures = "expected_influence_1",
    normalized = FALSE
  )[["expected_influence_1_all"]]
  legacy <- centrality(
    w,
    measures = "expected_influence_1",
    normalized = TRUE,
    psych_network = FALSE
  )[["expected_influence_1_all"]]

  expect_equal(legacy, raw / max(raw))
  expect_gt(max(abs(legacy)), 1)
})
