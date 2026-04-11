# Tests for Tier 2 network science features
# edge-metrics, vulnerability, core-periphery, fit-distribution, paths, bipartite

skip_on_cran()

# =============================================================================
# SECTION 1: neighborhood_overlap()
# =============================================================================

test_that("neighborhood_overlap: triangle has overlap 1", {
  adj <- matrix(c(0,1,1, 1,0,1, 1,1,0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  res <- neighborhood_overlap(adj)
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 3L)
  expect_true(all(res$overlap == 1))
  expect_true(all(res$shared == 1L))
})

test_that("neighborhood_overlap: star has overlap 0 between spokes", {
  star <- matrix(c(0,1,1,1, 1,0,0,0, 1,0,0,0, 1,0,0,0), 4, 4)
  rownames(star) <- colnames(star) <- c("hub", "a", "b", "c")
  res <- neighborhood_overlap(star)
  # Edges from hub to spokes: spokes share no neighbors except hub (excluded)
  spoke_edges <- res[res$from != "hub" | res$to == "hub", ]
  expect_true(all(res$overlap == 0))
})

test_that("neighborhood_overlap: empty graph returns empty df", {
  adj <- matrix(0, 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  res <- neighborhood_overlap(adj)
  expect_equal(nrow(res), 0L)
})

test_that("neighborhood_overlap: weighted network includes weight column", {
  adj <- matrix(c(0, 2, 3, 2, 0, 1, 3, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  res <- neighborhood_overlap(adj)
  expect_true("weight" %in% names(res))
})

# =============================================================================
# SECTION 2: simmelian_strength()
# =============================================================================

test_that("simmelian_strength: K4 has 2 triangles per edge", {
  k4 <- matrix(1, 4, 4); diag(k4) <- 0
  rownames(k4) <- colnames(k4) <- LETTERS[1:4]
  res <- simmelian_strength(k4)
  expect_true(all(res$triangles == 2L))
})

test_that("simmelian_strength: path has 0 triangles", {
  path <- matrix(c(0,1,0, 1,0,1, 0,1,0), 3, 3)
  rownames(path) <- colnames(path) <- c("A", "B", "C")
  res <- simmelian_strength(path)
  expect_true(all(res$triangles == 0L))
})

test_that("simmelian_strength: empty graph returns empty df", {
  adj <- matrix(0, 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  res <- simmelian_strength(adj)
  expect_equal(nrow(res), 0L)
})

# =============================================================================
# SECTION 3: edge_reciprocity()
# =============================================================================

test_that("edge_reciprocity: detects mutual and non-mutual edges", {
  adj <- matrix(c(0, 0.8, 0, 0.3, 0, 0.5, 0.7, 0, 0), 3, 3, byrow = TRUE)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  res <- edge_reciprocity(adj, directed = TRUE)
  expect_true(is.data.frame(res))
  # A->B and B->A are reciprocal
  ab <- res[res$from == "A" & res$to == "B", ]
  expect_true(ab$reciprocated)
  expect_equal(ab$reverse_weight, 0.3)
  # B->C is not reciprocated
  bc <- res[res$from == "B" & res$to == "C", ]
  expect_false(bc$reciprocated)
  expect_true(is.na(bc$weight_ratio))
})

test_that("edge_reciprocity: errors on undirected network", {
  adj <- matrix(c(0,1,1, 1,0,1, 1,1,0), 3, 3)
  expect_error(edge_reciprocity(adj), "directed")
})

test_that("edge_reciprocity: empty graph returns empty df", {
  adj <- matrix(0, 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  res <- edge_reciprocity(adj, directed = TRUE)
  expect_equal(nrow(res), 0L)
})

# =============================================================================
# SECTION 4: vulnerability()
# =============================================================================

test_that("vulnerability: star hub has max vulnerability", {
  star <- matrix(c(0,1,1,1, 1,0,0,0, 1,0,0,0, 1,0,0,0), 4, 4)
  rownames(star) <- colnames(star) <- c("hub", "a", "b", "c")
  v <- vulnerability(star)
  expect_s3_class(v, "cograph_vulnerability")
  expect_equal(v$node[1], "hub")
  expect_equal(v$vulnerability[v$node == "hub"], 1.0)
})

test_that("vulnerability: K4 all nodes equal", {
  k4 <- matrix(1, 4, 4); diag(k4) <- 0
  rownames(k4) <- colnames(k4) <- LETTERS[1:4]
  v <- vulnerability(k4)
  expect_equal(length(unique(round(v$vulnerability, 10))), 1L)
})

test_that("vulnerability: normalized vs raw", {
  adj <- create_test_matrix(10, density = 0.3)
  v_norm <- vulnerability(adj, normalized = TRUE)
  v_raw <- vulnerability(adj, normalized = FALSE)
  expect_true(all(is.numeric(v_norm$vulnerability)))
  expect_true(all(is.numeric(v_raw$vulnerability)))
  expect_equal(nrow(v_norm), 10L)
})

test_that("vulnerability: single node returns NA", {
  adj <- matrix(0, 1, 1)
  rownames(adj) <- colnames(adj) <- "A"
  v <- vulnerability(adj)
  expect_true(is.na(v$vulnerability[v$node == "A"]))
})

test_that("vulnerability: plot method works", {
  adj <- create_test_matrix(10, density = 0.3)
  v <- vulnerability(adj)
  pdf(NULL)
  expect_no_error(plot(v))
  expect_no_error(plot(v, top = 5))
  dev.off()
})

test_that("core_periphery: plot method works", {
  adj <- create_test_matrix(10, density = 0.3)
  rownames(adj) <- colnames(adj) <- paste0("N", 1:10)
  cp <- core_periphery(adj)
  pdf(NULL)
  expect_no_error(plot(cp))
  dev.off()
})

# =============================================================================
# SECTION 5: core_periphery()
# =============================================================================

test_that("core_periphery: continuous returns correct structure", {
  adj <- matrix(c(
    0,1,1,1,0,
    1,0,1,1,0,
    1,1,0,1,1,
    1,1,1,0,1,
    0,0,1,1,0
  ), 5, 5)
  rownames(adj) <- colnames(adj) <- LETTERS[1:5]
  cp <- core_periphery(adj)
  expect_s3_class(cp, "cograph_core_periphery")
  expect_true(all(cp$coreness >= 0 & cp$coreness <= 1))
  expect_true(all(cp$role %in% c("core", "periphery")))
  expect_true(is.numeric(attr(cp, "fitness")))
  expect_true(attr(cp, "core_density") >= 0)
})

test_that("core_periphery: discrete refines assignment", {
  adj <- matrix(c(
    0,1,1,1,0,0,
    1,0,1,1,0,0,
    1,1,0,1,1,0,
    1,1,1,0,1,1,
    0,0,1,1,0,0,
    0,0,0,1,0,0
  ), 6, 6)
  rownames(adj) <- colnames(adj) <- LETTERS[1:6]
  cp <- core_periphery(adj, method = "discrete")
  expect_true(sum(cp$role == "core") > 0)
  expect_true(sum(cp$role == "periphery") > 0)
})

test_that("core_periphery: print method works", {
  adj <- create_test_matrix(8, density = 0.4)
  cp <- core_periphery(adj)
  expect_output(print(cp), "Core-Periphery")
})

# =============================================================================
# SECTION 6: fit_degree_distribution()
# =============================================================================

test_that("fit_degree_distribution: returns correct structure", {
  mat <- create_test_matrix(50, density = 0.15)
  fit <- fit_degree_distribution(mat,
    distributions = c("exponential", "poisson"))
  expect_s3_class(fit, "cograph_degree_fit")
  expect_true(is.data.frame(fit$comparison))
  expect_equal(nrow(fit$comparison), 2L)
  expect_true(fit$best %in% c("exponential", "poisson"))
  expect_true(all(c("aic", "bic", "ks_stat") %in% names(fit$comparison)))
})

test_that("fit_degree_distribution: all four distributions", {
  mat <- create_test_matrix(50, density = 0.15)
  fit <- fit_degree_distribution(mat)
  expect_equal(length(fit$fits), 4L)
  expect_equal(nrow(fit$comparison), 4L)
})

test_that("fit_degree_distribution: print method works", {
  mat <- create_test_matrix(30, density = 0.2)
  fit <- fit_degree_distribution(mat,
    distributions = c("exponential", "poisson"))
  expect_output(print(fit), "Degree Distribution Fit")
})

test_that("fit_degree_distribution: plot method works", {
  mat <- create_test_matrix(50, density = 0.15)
  fit <- fit_degree_distribution(mat,
    distributions = c("exponential", "poisson"))
  pdf(NULL)
  expect_no_error(plot(fit))
  dev.off()
})

test_that("fit_degree_distribution: errors on unknown distribution", {
  mat <- create_test_matrix(20, density = 0.3)
  expect_error(fit_degree_distribution(mat, distributions = "gamma"),
               "Unknown")
})

# =============================================================================
# SECTION 7: shortest_paths()
# =============================================================================

test_that("shortest_paths: all-pairs returns matrix", {
  adj <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  d <- shortest_paths(adj)
  expect_true(is.matrix(d))
  expect_equal(dim(d), c(4L, 4L))
  expect_equal(d["A", "D"], 3)
})

test_that("shortest_paths: single source returns vector", {
  adj <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  d <- shortest_paths(adj, from = "A")
  expect_true(is.numeric(d))
  expect_equal(length(d), 4L)
  expect_equal(unname(d["D"]), 3)
})

test_that("shortest_paths: point to point returns scalar", {
  adj <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  d <- shortest_paths(adj, from = "A", to = "D")
  expect_equal(d, 3)
})

test_that("shortest_paths: weights = NA forces unweighted", {
  adj <- matrix(c(0,5,0, 5,0,1, 0,1,0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  d <- shortest_paths(adj, from = "A", to = "C", weights = NA)
  expect_equal(d, 2)  # 2 hops regardless of weight
})

test_that("shortest_paths: error on invalid node", {
  adj <- matrix(c(0,1, 1,0), 2, 2)
  rownames(adj) <- colnames(adj) <- c("A", "B")
  expect_error(shortest_paths(adj, from = "Z"), "not found")
})

# =============================================================================
# SECTION 8: k_shortest_paths()
# =============================================================================

test_that("k_shortest_paths: finds correct paths", {
  adj <- matrix(c(
    0,1,1,0,0,
    0,0,1,1,0,
    0,0,0,1,1,
    0,0,0,0,1,
    0,0,0,0,0
  ), 5, 5, byrow = TRUE)
  rownames(adj) <- colnames(adj) <- LETTERS[1:5]
  kp <- k_shortest_paths(adj, from = "A", to = "E", k = 3)
  expect_s3_class(kp, "cograph_k_paths")
  expect_equal(length(kp$paths), 3L)
  expect_equal(kp$distances[1], 2)  # shortest: A->C->E
  expect_true(all(vapply(kp$paths, function(p) p[1] == "A" && p[length(p)] == "E",
                         logical(1))))
})

test_that("k_shortest_paths: returns fewer if not enough paths", {
  adj <- matrix(c(0,1, 0,0), 2, 2, byrow = TRUE)
  rownames(adj) <- colnames(adj) <- c("A", "B")
  kp <- k_shortest_paths(adj, from = "A", to = "B", k = 5)
  expect_equal(length(kp$paths), 1L)
})

test_that("k_shortest_paths: no path returns empty", {
  adj <- matrix(0, 3, 3)
  rownames(adj) <- colnames(adj) <- LETTERS[1:3]
  kp <- k_shortest_paths(adj, from = "A", to = "C", k = 2)
  expect_equal(length(kp$paths), 0L)
})

test_that("k_shortest_paths: print method works", {
  adj <- matrix(c(0,1,1, 0,0,1, 0,0,0), 3, 3, byrow = TRUE)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  kp <- k_shortest_paths(adj, from = "A", to = "C", k = 2)
  expect_output(print(kp), "K Shortest Paths")
})

# =============================================================================
# SECTION 9: project_bipartite()
# =============================================================================

test_that("project_bipartite: sum projection correct", {
  inc <- matrix(c(1,1,0, 1,0,1, 0,1,1, 1,1,1), 4, 3, byrow = TRUE)
  rownames(inc) <- paste0("S", 1:4)
  colnames(inc) <- paste0("C", 1:3)
  res <- project_bipartite(inc, mode = "rows", method = "sum")
  expect_equal(dim(res), c(4L, 4L))
  expect_true(all(diag(res) == 0))
  expect_equal(res["S1", "S4"], 2)
})

test_that("project_bipartite: column mode works", {
  inc <- matrix(c(1,1,0, 1,0,1, 0,1,1), 3, 3, byrow = TRUE)
  rownames(inc) <- paste0("S", 1:3)
  colnames(inc) <- paste0("C", 1:3)
  res <- project_bipartite(inc, mode = "columns", method = "binary")
  expect_equal(dim(res), c(3L, 3L))
  expect_equal(rownames(res), paste0("C", 1:3))
})

test_that("project_bipartite: jaccard values in [0,1]", {
  inc <- matrix(c(1,1,0, 1,0,1, 0,1,1, 1,1,1), 4, 3, byrow = TRUE)
  rownames(inc) <- paste0("S", 1:4)
  colnames(inc) <- paste0("C", 1:3)
  res <- project_bipartite(inc, method = "jaccard")
  expect_true(all(res >= 0 & res <= 1))
})

test_that("project_bipartite: cosine values in [0,1]", {
  inc <- matrix(c(1,1,0, 1,0,1, 0,1,1), 3, 3, byrow = TRUE)
  rownames(inc) <- paste0("S", 1:3)
  colnames(inc) <- paste0("C", 1:3)
  res <- project_bipartite(inc, method = "cosine")
  expect_true(all(res >= -1e-10 & res <= 1 + 1e-10))
})

test_that("project_bipartite: newman projection works", {
  inc <- matrix(c(1,1,0, 1,0,1, 0,1,1, 1,1,1), 4, 3, byrow = TRUE)
  rownames(inc) <- paste0("S", 1:4)
  colnames(inc) <- paste0("C", 1:3)
  res <- project_bipartite(inc, method = "newman")
  expect_equal(dim(res), c(4L, 4L))
  expect_true(all(diag(res) == 0))
})

test_that("project_bipartite: data.frame input works", {
  df <- data.frame(
    type1 = c("S1", "S1", "S2", "S3"),
    type2 = c("C1", "C2", "C1", "C2")
  )
  res <- project_bipartite(df, method = "binary")
  expect_true(is.matrix(res))
  expect_equal(res["S1", "S2"], 1)
})

# =============================================================================
# SECTION 10: is_bipartite()
# =============================================================================

test_that("is_bipartite: non-square is TRUE", {
  inc <- matrix(c(1,0,1, 1,1,0), 2, 3)
  expect_true(is_bipartite(inc))
})

test_that("is_bipartite: triangle is FALSE", {
  tri <- matrix(c(0,1,1, 1,0,1, 1,1,0), 3, 3)
  expect_false(is_bipartite(tri))
})

test_that("is_bipartite: bipartite square is TRUE", {
  bp <- matrix(c(0,0,1,1, 0,0,1,0, 1,1,0,0, 1,0,0,0), 4, 4, byrow = TRUE)
  expect_true(is_bipartite(bp))
})
