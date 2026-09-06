# ===========================================================================
# Tests for Batch 7 — Centrality Zoo comparison batch
#   distance_entropy, local_dimension, local_information_dimension,
#   neighborhood_connectivity, modularity_vitality
#
# Calibration: hand-computed values, the worked example published in
# Wen & Deng (2019), and brute-force igraph references.
# ===========================================================================

# ---------------------------------------------------------------------------
# Test graphs
# ---------------------------------------------------------------------------

path4 <- matrix(c(0, 1, 0, 0,
                  1, 0, 1, 0,
                  0, 1, 0, 1,
                  0, 0, 1, 0), 4, 4)
rownames(path4) <- colnames(path4) <- LETTERS[1:4]

path5 <- matrix(0, 5, 5)
path5[cbind(1:4, 2:5)] <- 1
path5 <- path5 + t(path5)
rownames(path5) <- colnames(path5) <- LETTERS[1:5]

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

# Layered graph reproducing the ring sizes 4, 5, 4, 4 of the worked example
# in Wen & Deng (2019, Example 2.1): node 1 is the centre, each ring hangs
# off the first node of the previous ring.
layers <- c(4, 5, 4, 4)
n_layered <- 1 + sum(layers)
ring_ids <- split(seq_len(n_layered)[-1], rep(seq_along(layers), layers))
layered <- matrix(0, n_layered, n_layered)
layered[cbind(1, ring_ids[[1]])] <- 1
layered[cbind(ring_ids[[1]][1], ring_ids[[2]])] <- 1
layered[cbind(ring_ids[[2]][1], ring_ids[[3]])] <- 1
layered[cbind(ring_ids[[3]][1], ring_ids[[4]])] <- 1
layered <- layered + t(layered)
rownames(layered) <- colnames(layered) <- paste0("v", seq_len(n_layered))

# Directed chain A -> B -> C
chain3 <- matrix(c(0, 1, 0,
                   0, 0, 1,
                   0, 0, 0), 3, 3, byrow = TRUE)
rownames(chain3) <- colnames(chain3) <- LETTERS[1:3]

entropy_bits <- function(p) -sum(p * log(p)) / log(length(p))

# ===========================================================================
# Distance entropy
# ===========================================================================

test_that("distance_entropy: path4 matches hand-computed values", {
  v <- centrality_distance_entropy(path4)
  expect_named(v, LETTERS[1:4])
  # Endpoints see distances 1, 2, 3 once each: uniform -> 1
  expect_equal(v[["A"]], 1)
  expect_equal(v[["D"]], 1)
  # Inner nodes see distances 1, 1, 2 -> p = (2/3, 1/3) over a span of 2
  expect_equal(v[["B"]], entropy_bits(c(2 / 3, 1 / 3)))
  expect_equal(v[["C"]], v[["B"]])
})

test_that("distance_entropy: star centre is 0, leaves are p = (1/4, 3/4)", {
  v <- centrality_distance_entropy(star5)
  expect_equal(v[["A"]], 0)
  expect_equal(unname(v[-1]), rep(entropy_bits(c(1 / 4, 3 / 4)), 4))
})

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

test_that("distance_entropy: mode follows edge direction", {
  out <- centrality_distance_entropy(chain3, mode = "out")
  inn <- centrality_distance_entropy(chain3, mode = "in")
  expect_equal(out[["A"]], 1)        # reaches B (1) and C (2): uniform
  expect_true(is.nan(inn[["A"]]))    # nothing reaches A
  expect_equal(inn[["C"]], 1)
})

# ===========================================================================
# Local dimension
# ===========================================================================

test_that("local_dimension: reproduces the published worked example (0.9231)", {
  v <- centrality_local_dimension(layered)
  # Wen & Deng (2019), Example 2.1 / Fig. 2 report 0.9231 for rings 4,5,4,4.
  expect_equal(v[["v1"]], 0.9231, tolerance = 5e-5)
  # Same number from the formula: OLS slope of ln(1 + cumsum) on ln r
  ball <- 1 + cumsum(layers)
  r <- seq_along(ball)
  expect_equal(v[["v1"]], unname(stats::coef(stats::lm(log(ball) ~ log(r)))[2]))
})

test_that("local_dimension: star centre uses the discretised derivative", {
  v <- centrality_local_dimension(star5)
  # Centre reaches everything in one hop: single radius, r * n(1) / B(1) = 4/5
  expect_equal(v[["A"]], 4 / 5)
  # Leaves: B = (2, 5) at r = (1, 2) -> slope log(5/2) / log(2)
  expect_equal(unname(v[-1]), rep(log(5 / 2) / log(2), 4))
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

test_that("local_information_dimension: path5 matches hand-computed values", {
  v <- centrality_local_information_dimension(path5)
  n <- 5
  # Middle node C: d_max = 2 -> one box (l = 1), discretised derivative
  #   (1 + ln(3/5)) * n(1) / N with n(1) = 2
  expect_equal(v[["C"]], (1 + log(3 / n)) * 2 / n)
  # Endpoint A: d_max = 4 -> boxes l = 1, 2 with balls 2, 3
  p <- c(2, 3) / n
  info <- -p * log(p)
  slope <- (info[2] - info[1]) / (log(2) - log(1))
  expect_equal(v[["A"]], -slope)
  expect_equal(v[["E"]], v[["A"]])
  expect_equal(v[["B"]], v[["D"]])
})

test_that("local_information_dimension: star hub scores above the leaves", {
  v <- centrality_local_information_dimension(star5)
  expect_equal(v[["A"]], (1 + log(1)) * 4 / 5)
  expect_equal(unname(v[-1]), rep((1 + log(2 / 5)) * 1 / 5, 4))
  expect_true(all(v[["A"]] > v[-1]))
})

# ===========================================================================
# Modularity and modularity vitality
# ===========================================================================

test_that("modularity kernel matches igraph::modularity", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  set.seed(11)
  # Sequential sweep: each random graph is one independent expectation and
  # the seed stream must advance in order for the run to be reproducible.
  for (i in 1:12) {
    directed <- i %% 2 == 0
    weighted <- i %% 3 == 0
    g <- igraph::sample_gnp(sample(6:14, 1), 0.35, directed = directed)
    if (igraph::ecount(g) < 3) next
    w <- if (weighted) stats::runif(igraph::ecount(g), 0.5, 3) else NULL
    if (weighted) igraph::E(g)$weight <- w
    memb <- sample(1:3, igraph::vcount(g), replace = TRUE)
    m <- cograph:::.cg_path_matrix(g, w)
    expect_equal(cograph:::.cg_modularity(m, memb),
                 igraph::modularity(g, memb, weights = w),
                 info = sprintf("graph %d directed=%s weighted=%s",
                                i, directed, weighted))
  }
})

test_that("modularity_vitality matches brute-force node deletion", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  set.seed(23)
  # Sequential sweep, same reasoning as the modularity kernel test above.
  for (i in 1:12) {
    directed <- i %% 2 == 0
    weighted <- i %% 3 == 0
    g <- igraph::sample_gnp(sample(6:14, 1), 0.35, directed = directed)
    if (igraph::ecount(g) < 3) next
    w <- if (weighted) stats::runif(igraph::ecount(g), 0.5, 3) else NULL
    if (weighted) igraph::E(g)$weight <- w
    memb <- sample(1:3, igraph::vcount(g), replace = TRUE)
    q0 <- igraph::modularity(g, memb, weights = w)
    brute <- vapply(seq_len(igraph::vcount(g)), function(v) {
      g2 <- igraph::delete_vertices(g, v)
      if (igraph::ecount(g2) == 0) return(NaN)
      w2 <- if (weighted) igraph::E(g2)$weight else NULL
      q0 - igraph::modularity(g2, memb[-v], weights = w2)
    }, numeric(1))
    mine <- centrality(g, measures = "modularity_vitality",
                       membership = memb)$modularity_vitality
    expect_equal(mine, brute,
                 info = sprintf("graph %d directed=%s weighted=%s",
                                i, directed, weighted))
  }
})

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
# Neighborhood connectivity
# ===========================================================================

test_that("neighborhood_connectivity: star and isolate hand values", {
  v <- centrality_neighborhood_connectivity(star5)
  expect_equal(unname(v), c(1, 4, 4, 4, 4))
  iso <- rbind(cbind(star5, 0), 0)
  rownames(iso) <- colnames(iso) <- LETTERS[1:6]
  w <- centrality_neighborhood_connectivity(iso)
  expect_equal(w[["F"]], 0)
})

test_that("neighborhood_connectivity matches igraph::knn", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  v <- centrality(g, measures = "neighborhood_connectivity")
  expect_equal(v$neighborhood_connectivity_all,
               igraph::knn(g, weights = NA)$knn)

  set.seed(5)
  gd <- igraph::sample_gnp(15, 0.3, directed = TRUE)
  out <- centrality(gd, measures = "neighborhood_connectivity", mode = "out")
  ref <- igraph::knn(gd, mode = "out", neighbor.degree.mode = "out",
                     weights = NA)$knn
  ref[is.nan(ref)] <- 0
  expect_equal(out$neighborhood_connectivity_out, ref)
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
