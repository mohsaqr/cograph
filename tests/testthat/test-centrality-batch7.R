# ===========================================================================
# Tests for Batch 7 — Centrality Zoo comparison batch
#   distance_entropy, local_dimension, local_information_dimension,
#   neighborhood_connectivity, modularity_vitality
#
# Unit tests: invariants, conventions and contracts. The comparisons with
# hand-computed values, the worked example of Wen & Deng (2019) and igraph
# are declared in tests/equivalence/test-centrality-zoo-a-*.R.
# ===========================================================================

# ---------------------------------------------------------------------------
# Test graphs
# ---------------------------------------------------------------------------

star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1
star5 <- star5 + t(star5)
rownames(star5) <- colnames(star5) <- LETTERS[1:5]

# Two triangles (A,B,C) and (D,E,F) joined by the bridge C -- D
bridge6 <- matrix(0, 6, 6)
bridge6[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
bridge6 <- bridge6 + t(bridge6)
rownames(bridge6) <- colnames(bridge6) <- LETTERS[1:6]
bridge_membership <- c(1, 1, 1, 2, 2, 2)

# ===========================================================================
# Distance entropy
# ===========================================================================

test_that("distance_entropy: bounded in [0, 1] and NaN for an isolate", {
  skip_if_not_installed("igraph")
  set.seed(31)
  g <- igraph::sample_gnp(30, 0.12)
  v <- centrality(g, measures = "distance_entropy")$distance_entropy_all
  finite <- v[is.finite(v)]
  expect_true(all(finite >= 0 & finite <= 1 + 1e-12))
  iso <- igraph::add_vertices(igraph::make_graph("Zachary"), 1)
  w <- centrality(iso, measures = "distance_entropy")$distance_entropy_all
  expect_true(is.nan(w[35]))
  expect_false(anyNA(w[1:34]))
})

test_that("local_dimension: the hub scores below every leaf", {
  v <- centrality_local_dimension(star5)
  # Lower = more influential: the hub scores below every leaf
  expect_true(all(v[["A"]] < v[-1]))
})

test_that("local_dimension: isolate is NaN, others unaffected", {
  skip_if_not_installed("igraph")
  iso <- igraph::add_vertices(igraph::make_graph("Zachary"), 1)
  v <- centrality(iso, measures = "local_dimension")$local_dimension_all
  expect_true(is.nan(v[35]))
  expect_false(anyNA(v[1:34]))
})

# ===========================================================================
# Local information dimensionality
# ===========================================================================

test_that("local_information_dimension: star hub scores above the leaves", {
  v <- centrality_local_information_dimension(star5)
  expect_true(all(v[["A"]] > v[-1]))
})

# ===========================================================================
# Modularity and modularity vitality
# ===========================================================================


test_that("modularity_vitality: bridges are negative, hubs positive", {
  v <- centrality_modularity_vitality(bridge6, membership = bridge_membership)
  expect_named(v, LETTERS[1:6])
  expect_true(all(v[c("C", "D")] < 0))
  expect_true(all(v[c("A", "B", "E", "F")] > 0))
  # The graph is symmetric under the swap of the two triangles
  expect_equal(v[["A"]], v[["F"]])
  expect_equal(v[["C"]], v[["D"]])
})

test_that("modularity_vitality: missing membership warns and returns NA", {
  expect_warning(v <- centrality(bridge6, measures = "modularity_vitality"),
                 "membership")
  expect_true(all(is.na(v$modularity_vitality)))
})

test_that("modularity_vitality: wrong-length membership is a classed error", {
  expect_error(centrality(bridge6, measures = "modularity_vitality",
                          membership = 1:3),
               class = "cograph_bad_membership")
  expect_error(centrality(bridge6, measures = "modularity_vitality",
                          membership = c(1, 1, NA, 2, 2, 2)),
               class = "cograph_bad_membership")
})

test_that("modularity_vitality accepts factor and character labels", {
  ref <- centrality_modularity_vitality(bridge6, membership = bridge_membership)
  fac <- centrality_modularity_vitality(bridge6,
                                        membership = factor(bridge_membership))
  labels <- c("x", "x", "x", "y", "y", "y")
  chr <- centrality_modularity_vitality(bridge6, membership = labels)
  expect_equal(fac, ref)
  expect_equal(chr, ref)
})

# ===========================================================================
# Verb integration and invariants
# ===========================================================================

batch7 <- c("distance_entropy", "local_dimension",
            "local_information_dimension", "neighborhood_connectivity",
            "modularity_vitality")

test_that("centrality(): batch 7 columns carry the mode suffix where defined", {
  df <- centrality(bridge6, measures = batch7, membership = bridge_membership)
  expect_named(df, c("node", "distance_entropy_all", "local_dimension_all",
                     "local_information_dimension_all",
                     "neighborhood_connectivity_all", "modularity_vitality"))
  expect_equal(nrow(df), 6)
  expect_false(anyNA(df[-1]))
})

test_that("centrality(type = 'all') includes the batch 7 measures", {
  df <- suppressWarnings(centrality(bridge6, type = "all",
                                    membership = bridge_membership))
  expect_true(all(c("distance_entropy_all", "local_dimension_all",
                    "local_information_dimension_all",
                    "neighborhood_connectivity_all",
                    "modularity_vitality") %in% names(df)))
})

test_that("batch 7 measures are invariant to node relabelling", {
  skip_if_not_installed("igraph")
  set.seed(41)
  g <- igraph::sample_gnp(16, 0.3)
  igraph::V(g)$name <- paste0("n", seq_len(16))
  memb <- sample(1:3, 16, replace = TRUE)
  ref <- centrality(g, measures = batch7, membership = memb)
  perm <- sample(16)
  gp <- igraph::permute(g, perm)
  memb_p <- integer(16)
  memb_p[perm] <- memb
  got <- centrality(gp, measures = batch7, membership = memb_p)
  got <- got[match(ref$node, got$node), ]
  rownames(got) <- NULL
  expect_equal(got, ref)
})

test_that("kernels return zero-length output on an empty matrix", {
  empty <- matrix(numeric(0), 0, 0)
  expect_length(cograph:::.cg_distance_entropy(empty), 0)
  expect_length(cograph:::.cg_local_dimension(empty), 0)
  expect_length(cograph:::.cg_local_information_dimension(empty), 0)
  expect_length(cograph:::.cg_neighborhood_connectivity(empty), 0)
  expect_length(cograph:::.cg_modularity_vitality(empty, integer(0)), 0)
})
