# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

ira_fig <- function(panel) {
  # Ren et al. 2014 figure 1, transcribed from a page render. Paper labels
  # 0-4 become igraph vertices 1-5.
  edges <- c(3, 2, 3, 1, 2, 1, 1, 4, 1, 5)
  if (panel >= 2) edges <- c(edges, 3, 4)
  if (panel >= 3) edges <- c(edges, 4, 5)
  if (panel >= 4) edges <- c(edges, 2, 5)
  igraph::make_graph(edges, directed = FALSE)
}

test_that("ira conserves the resource on the figure 1 panels", {
  # Resource is conserved: every panel's scores sum to its vertex count.
  expect_equal(vapply(1:4, function(panel) sum(centrality_ira(ira_fig(panel))),
                      numeric(1)), rep(5, 4))
})

test_that("iira builds the equation (5) matrix the source prints", {
  # Zhong et al. 2015 figure 2 is the same graph as Ren's figure 1(a). Its
  # printed I(50) is compared in tests/equivalence (zoo-b-resource-allocation).
  # Every printed entry of the equation (5) matrix, which the source
  # truncates rather than rounds: 0.2952 appears as 0.29, 0.0333 as 0.03.
  a <- as.matrix(igraph::as_adjacency_matrix(ira_fig(1)))
  theta <- c(2, 2, 2, 1, 1)
  psi <- 1 - (1 - 0.2)^rowSums(a)
  built <- .cg_resource_matrix(a, theta, psi * theta)
  expect_equal(built, matrix(c(0, 3 / 25, 3 / 25, 1 / 30, 1 / 30,
                               369 / 1250, 0, 9 / 50, 0, 0,
                               369 / 1250, 9 / 50, 0, 0, 0,
                               369 / 625, 0, 0, 0, 0,
                               369 / 625, 0, 0, 0, 0), 5, 5))
  # The source truncates rather than rounds: 0.2952 prints as 0.29 and
  # 0.0333 as 0.03. Binary rounding is cleared first, since 0.36 * 2 / 6
  # evaluates to 0.11999999999999997 in double arithmetic.
  expect_equal(trunc(round(built, 10) * 100) / 100,
               matrix(c(0, 0.12, 0.12, 0.03, 0.03,
                        0.29, 0, 0.18, 0, 0,
                        0.29, 0.18, 0, 0, 0,
                        0.59, 0, 0, 0, 0,
                        0.59, 0, 0, 0, 0), 5, 5))
  # Sub-stochastic by construction, so the scores decay to nothing.
  expect_true(all(colSums(built) < 1))
})

test_that("ira surfaces the period-two cycle a bipartite graph has", {
  # A star's two vertex classes have sizes 1 and q, so the coefficient of
  # the -1 eigenvector in I(0) = 1 is nonzero and the iteration alternates
  # for ever between (q, 1/q, ..., 1/q) and (1, 1, ..., 1).
  for (q in c(2, 3, 5, 8)) {
    star <- igraph::make_star(q + 1, mode = "undirected")
    expect_warning(centrality_ira(star), class = "cograph_no_converge")
    even <- suppressWarnings(centrality_ira(star, ira_max_iter = 1000))
    odd <- suppressWarnings(centrality_ira(star, ira_max_iter = 999))
    expect_equal(unname(even), rep(1, q + 1))
    expect_equal(unname(odd), c(q, rep(1 / q, q)))
    # The mass and the exponent leave a star's allocation matrix unchanged.
    expect_equal(suppressWarnings(centrality_ira(star, ira_mass = "degree",
                                                 ira_alpha = 2)), even)
  }
  # The three-path alternates between (1/2, 2, 1/2) and (1, 1, 1).
  path3 <- igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
  expect_equal(unname(suppressWarnings(centrality_ira(path3))), rep(1, 3))
  expect_equal(unname(suppressWarnings(centrality_ira(path3,
                                                      ira_max_iter = 999))),
               c(0.5, 2, 0.5))
  # The four-path's classes are equal, so it converges to a real limit.
  path4 <- igraph::make_graph(c(1, 2, 2, 3, 3, 4), directed = FALSE)
  expect_no_warning(centrality_ira(path4))
})

test_that("both measures handle isolates, empties and components", {
  for (n in 0:3) {
    empty <- igraph::make_empty_graph(n, directed = FALSE)
    expect_equal(unname(centrality_ira(empty)), numeric(n))
    expect_equal(unname(centrality_iira(empty)), numeric(n))
  }
  # An isolate is in nobody's neighbourhood: it receives nothing and passes
  # nothing on, so it scores zero and breaks the sum-to-n conservation.
  ring <- igraph::make_ring(5)
  with_isolate <- igraph::disjoint_union(
    ring, igraph::make_empty_graph(1, directed = FALSE)
  )
  expect_equal(unname(centrality_ira(with_isolate)), c(rep(1, 5), 0))
  expect_equal(sum(centrality_ira(with_isolate)), 5)
  # Components are independent, and each keeps its own vertex count.
  kite <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4), directed = FALSE)
  joined <- igraph::disjoint_union(ring, kite)
  expect_equal(unname(centrality_ira(joined)),
               c(unname(centrality_ira(ring)), unname(centrality_ira(kite))),
               tolerance = 1e-5)
  expect_equal(unname(centrality_iira(joined)),
               c(unname(centrality_iira(ring)), unname(centrality_iira(kite))))
  # A whole graph of isolates settles in two steps and never warns.
  expect_no_warning(centrality_ira(matrix(0, 3, 3)))
})

test_that("both measures normalize and take their source parameters", {
  kite <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4), directed = FALSE)
  raw <- unname(centrality_ira(kite))
  expect_equal(unname(centrality_ira(kite, normalized = TRUE)), raw / max(raw))
  small <- unname(centrality_iira(kite))
  expect_equal(unname(centrality_iira(kite, normalized = TRUE)),
               small / max(small))
  # Zero steps returns the initial unit resource the sources both state.
  expect_equal(unname(centrality_iira(kite, iira_steps = 0)), rep(1, 4))
  # beta = 1 makes every spreading factor one, so IIRA becomes IRA's matrix
  # at the same fixed step count.
  expect_equal(unname(centrality_iira(kite, iira_beta = 1, iira_steps = 60)),
               raw, tolerance = 1e-5)
  # A loose tolerance stops earlier and lands further from the limit.
  early <- unname(centrality_ira(kite, ira_tol = 0.5))
  expect_false(isTRUE(all.equal(early, raw)))
  expect_true(max(abs(early - raw)) < 1)
  # The degree mass and a nonlinear exponent change the answer.
  expect_false(isTRUE(all.equal(unname(centrality_ira(kite, ira_alpha = 2)),
                                raw)))
})

test_that("both measures project their input and preserve labels", {
  g <- igraph::make_graph(c(1, 2, 2, 1, 2, 3, 2, 3, 3, 3), directed = TRUE)
  igraph::E(g)$weight <- c(2, 8, 3, 9, 7)
  igraph::V(g)$name <- c("b", "c", "a")
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(3, 1, 2)
  # Either arc makes one edge, parallels count once and the loop is dropped,
  # so the skeleton is the two-edge path b-c-a, whose classes are 2 and 1.
  score <- suppressWarnings(centrality_ira(g))
  expect_named(score, c("b", "c", "a"))
  expect_equal(unname(score), rep(1, 3))
  bare <- suppressWarnings(centrality(g, measures = "ira")$ira)
  expect_equal(bare, unname(score))
  expect_equal(centrality(g, measures = "iira")$iira,
               unname(centrality_iira(g)))
  ignored <- suppressWarnings(centrality_ira(
    g, simplify = FALSE, mode = "in", loops = TRUE, invert_weights = TRUE,
    cutoff = 1
  ))
  expect_equal(ignored, score)
  expect_equal(suppressWarnings(centrality_ira(g, weighted = FALSE)), score)
  expect_equal(suppressWarnings(centrality_ira(a[perm, perm])), score[perm])
  expect_equal(centrality_iira(a[perm, perm]), centrality_iira(g)[perm])
  expect_equal(centrality_iira(g, simplify = FALSE, mode = "in", loops = TRUE,
                               invert_weights = TRUE, cutoff = 1),
               centrality_iira(g))
})

test_that("both measures validate their parameters and metadata", {
  skip_on_cran()
  g <- igraph::make_ring(6)
  for (alpha in list(NA_real_, NaN, Inf, -Inf, "1", TRUE, c(1, 2), numeric())) {
    expect_error(centrality_ira(g, ira_alpha = alpha), "ira_alpha")
  }
  for (tol in list(0, -1, NA_real_, NaN, Inf, "1", TRUE, c(1, 2), numeric())) {
    expect_error(centrality_ira(g, ira_tol = tol), "ira_tol")
  }
  for (bound in list(0, -1, 1.5, NA_real_, NaN, Inf, "5", TRUE, 1:2,
                     numeric())) {
    expect_error(centrality_ira(g, ira_max_iter = bound), "ira_max_iter")
  }
  for (beta in list(0, -0.1, 1.5, NA_real_, NaN, Inf, "1", TRUE, c(1, 2),
                    numeric())) {
    expect_error(centrality_iira(g, iira_beta = beta), "iira_beta")
  }
  for (steps in list(-1, 1.5, NA_real_, NaN, Inf, "5", TRUE, 1:2, numeric())) {
    expect_error(centrality_iira(g, iira_steps = steps), "iira_steps")
  }
  expect_error(centrality_ira(g, ira_mass = "betweenness"))
  expect_error(centrality_iira(g, ira_mass = "betweenness"))
  # Validation runs before the graph is read, so an empty graph still errors.
  expect_error(centrality_ira(igraph::make_empty_graph(0), ira_tol = 0),
               "ira_tol")
  meta <- list_centralities()
  for (measure in c("ira", "iira")) {
    expect_true(measure %in% meta$measure)
    expect_false(meta$uses_weights[meta$measure == measure])
    expect_false(meta$mode_aware[meta$measure == measure])
    expect_false(meta$costly[meta$measure == measure])
    expect_false(meta$needs_membership[meta$measure == measure])
    expect_identical(meta$orientation[meta$measure == measure], "higher")
    expect_true(measure %in% .cg_no_mode_measures())
  }
})

test_that("the allocation kernels build the matrices the sources print", {
  a <- as.matrix(igraph::as_adjacency_matrix(ira_fig(1)))
  built <- .cg_resource_matrix(a, c(2, 2, 2, 1, 1))
  # Equation (4) of Ren et al. 2014, all twenty-five printed entries.
  expect_equal(built, matrix(c(0, 1 / 3, 1 / 3, 1 / 6, 1 / 6,
                               1 / 2, 0, 1 / 2, 0, 0,
                               1 / 2, 1 / 2, 0, 0, 0,
                               1, 0, 0, 0, 0,
                               1, 0, 0, 0, 0), 5, 5))
  expect_equal(colSums(built), rep(1, 5))
  # A separate numerator only rescales rows, never the support.
  scaled <- .cg_resource_matrix(a, c(2, 2, 2, 1, 1), c(1, 1, 1, 0.5, 0.5))
  expect_equal(scaled, built / 2)
  # An isolate's column denominator is never evaluated, so no NaN appears.
  lonely <- matrix(0, 2, 2)
  expect_equal(.cg_resource_matrix(lonely, c(0, 0)), lonely)
  expect_equal(.cg_resource_steps(built, 0), rep(1, 5))
  expect_equal(.cg_resource_steps(built, 1), as.numeric(built %*% rep(1, 5)))
})
