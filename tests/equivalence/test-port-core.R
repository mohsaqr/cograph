# ===========================================================================
# Core centrality measures ported off igraph (R/centrality.R) and the
# edge-betweenness kernel (R/kernels-edges.R)
# ===========================================================================
# The golden files pin the values; these tests cover what the golden cannot:
# the edge kernel against igraph on a graph zoo, the spectral measures by
# their defining eigen equation, the argument paths the golden designs do
# not exercise (normalised closeness / harmonic, personalised PageRank), the
# classed failure of the linear-solve measures, and the igraph-free contract.

.port_zoo_matrix <- function(n, density, directed, weighted, seed) {
  set.seed(seed)
  m <- matrix(0, n, n)
  if (n > 1L && density > 0) {
    idx <- which(row(m) != col(m))
    take <- idx[stats::runif(length(idx)) < density]
    m[take] <- if (weighted) stats::runif(length(take), 0.1, 1) else 1
    if (!directed) m[lower.tri(m)] <- t(m)[lower.tri(m)]
  }
  m
}

.port_core_measures <- c(
  "degree", "strength", "closeness", "eccentricity", "coreness", "harmonic",
  "alpha", "power", "subgraph", "betweenness", "eigenvector", "pagerank",
  "constraint", "transitivity", "authority", "hub", "diffusion", "leverage",
  "kreach", "load", "laplacian", "current_flow_closeness",
  "current_flow_betweenness", "voterank", "percolation"
)

# A closed-form check needing no reference package: on an undirected path
# of n vertices the k-th edge separates k vertices from n - k, so it lies on
# exactly k * (n - k) shortest paths.
test_that("edge betweenness of a path graph is k * (n - k)", {
  n <- 7L
  m <- matrix(0, n, n)
  idx <- cbind(seq_len(n - 1L), seq_len(n - 1L) + 1L)
  m[idx] <- 1
  m[idx[, c(2, 1)]] <- 1
  cg <- .cg_graph(m)
  got <- .cg_edge_betweenness(.cg_path_matrix(cg, cg$weights), n, cg$directed,
                              edges = cg$edges)
  k <- seq_len(n - 1L)
  expect_equal(got, k * (n - k))
  # A self-loop is never on a shortest path.
  m_loop <- m
  m_loop[3L, 3L] <- 1
  cg_loop <- .cg_graph(m_loop)
  got_loop <- .cg_edge_betweenness(.cg_path_matrix(cg_loop, cg_loop$weights), n,
                                   cg_loop$directed, edges = cg_loop$edges)
  loop_row <- which(cg_loop$edges[, 1L] == 3L & cg_loop$edges[, 2L] == 3L)
  expect_equal(got_loop[loop_row], 0)
  expect_equal(got_loop[-loop_row], k * (n - k))
})

test_that("graph zoo: .cg_edge_betweenness matches igraph::edge_betweenness", {
  skip_if_not_installed("igraph")
  grid <- expand.grid(
    n = c(1L, 2L, 3L, 5L, 8L, 12L, 20L),
    density = c(0, 0.1, 0.35, 0.7, 1),
    directed = c(FALSE, TRUE),
    weighted = c(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  mismatches <- unlist(lapply(seq_len(nrow(grid)), function(i) {
    cfg <- grid[i, ]
    m <- .port_zoo_matrix(cfg$n, cfg$density, cfg$directed, cfg$weighted,
                          seed = 2000L + i)
    g <- igraph::graph_from_adjacency_matrix(
      m, mode = if (cfg$directed) "directed" else "undirected", weighted = TRUE)
    cg <- .cg_graph(m, directed = cfg$directed)
    w <- .cg_path_matrix(cg, cg$weights)
    vapply(c(-1, 2), function(cut) {
      ref <- igraph::edge_betweenness(g, cutoff = cut)
      got <- .cg_edge_betweenness(w, cfg$n, cg$directed, cutoff = cut,
                                  edges = cg$edges)
      if (length(ref) == length(got) &&
            isTRUE(all.equal(got, unname(ref), tolerance = 1e-10))) return("")
      sprintf("n=%d dens=%.2f %s %s cutoff=%g", cfg$n, cfg$density,
              if (cfg$directed) "directed" else "undirected",
              if (cfg$weighted) "weighted" else "unweighted", cut)
    }, character(1))
  }))
  expect_identical(mismatches[nzchar(mismatches)], character(0))
})

test_that("edge_centrality keeps igraph's edge order and endpoint typing", {
  skip_if_not_installed("igraph")
  m <- .port_zoo_matrix(9L, 0.4, directed = TRUE, weighted = TRUE, seed = 7L)
  rownames(m) <- colnames(m) <- paste0("v", seq_len(9L))
  g <- igraph::graph_from_adjacency_matrix(m, mode = "directed", weighted = TRUE)
  ref <- igraph::as_data_frame(g, what = "edges")
  got <- edge_centrality(m)
  expect_identical(got$from, ref$from)
  expect_identical(got$to, ref$to)
  expect_equal(got$weight, ref$weight)
  expect_equal(got$betweenness, unname(igraph::edge_betweenness(g)))
  # Unnamed input reports numeric vertex ids, as igraph did.
  m_un <- unname(m)
  g_un <- igraph::graph_from_adjacency_matrix(m_un, mode = "directed", weighted = TRUE)
  ref_un <- igraph::as_data_frame(g_un, what = "edges")
  got_un <- edge_centrality(m_un)
  expect_identical(got_un$from, ref_un$from)
  expect_identical(got_un$to, ref_un$to)
})

# The spectral measures are pinned by the golden only on the well-posed
# networks it happens to contain; the defining equation A v = lambda v holds
# on every well-posed graph and is checked directly here.
test_that("eigenvector, hub and authority satisfy their eigen equations", {
  residual <- function(a, v) {
    e <- eigen(a, symmetric = isSymmetric(unname(a)))
    lambda <- Re(e$values[.cg_dominant_index(e$values)])
    max(abs(a %*% v - lambda * v)) / max(1, abs(lambda))
  }
  graphs <- list(
    undirected_unweighted = .port_zoo_matrix(15L, 0.4, FALSE, FALSE, seed = 11L),
    undirected_weighted = .port_zoo_matrix(15L, 0.4, FALSE, TRUE, seed = 12L),
    directed_weighted = .port_zoo_matrix(12L, 0.5, TRUE, TRUE, seed = 13L)
  )
  lapply(names(graphs), function(nm) {
    w <- graphs[[nm]]
    n <- nrow(w)
    # Well-posedness: connected (strongly, when directed) and aperiodic.
    expect_equal(.cg_n_components((w != 0) * 1), 1L, info = nm)
    df <- centrality(w, measures = c("eigenvector", "hub", "authority"))
    expect_lt(residual(t(w), df$eigenvector), 1e-10, label = paste(nm, "eigenvector"))
    expect_lt(residual(tcrossprod(w), df$hub), 1e-10, label = paste(nm, "hub"))
    expect_lt(residual(crossprod(w), df$authority), 1e-10, label = paste(nm, "authority"))
    # igraph's scaling: the largest entry is exactly 1.
    expect_equal(max(df$eigenvector), 1, info = nm)
    expect_equal(max(df$hub), 1, info = nm)
    expect_equal(max(df$authority), 1, info = nm)
    invisible(NULL)
  })
})

test_that("normalised closeness / harmonic and personalised PageRank match igraph", {
  skip_if_not_installed("igraph")
  set.seed(31)
  m <- .port_zoo_matrix(14L, 0.3, directed = TRUE, weighted = TRUE, seed = 31L)
  g <- igraph::graph_from_adjacency_matrix(m, mode = "directed", weighted = TRUE)
  lapply(c("all", "out", "in"), function(md) {
    got <- centrality(m, measures = c("closeness", "harmonic"), mode = md,
                      normalized = TRUE)
    expect_equal(got[[paste0("closeness_", md)]],
                 unname(igraph::closeness(g, mode = md, normalized = TRUE)),
                 tolerance = 1e-12, info = md)
    # centrality() rescales every measure but closeness to a maximum of 1.
    ref_h <- igraph::harmonic_centrality(g, mode = md, normalized = TRUE)
    expect_equal(got[[paste0("harmonic_", md)]], unname(ref_h / max(ref_h)),
                 tolerance = 1e-12, info = md)
    invisible(NULL)
  })
  # Cutoff restricts both to short paths.
  got_cut <- centrality(m, measures = c("closeness", "harmonic"), cutoff = 1.2)
  expect_equal(got_cut$closeness_all,
               unname(igraph::closeness(g, mode = "all", cutoff = 1.2)), tolerance = 1e-12)
  expect_equal(got_cut$harmonic_all,
               unname(igraph::harmonic_centrality(g, mode = "all", cutoff = 1.2)),
               tolerance = 1e-12)
  p <- stats::runif(14L)
  got_pr <- centrality(m, measures = "pagerank", personalized = p, damping = 0.7)
  expect_equal(got_pr$pagerank,
               unname(igraph::page_rank(g, personalized = p, damping = 0.7)$vector),
               tolerance = 1e-10)
})

test_that("alpha and power fail with cograph_singular_system on a singular system", {
  # A directed 6-cycle has eigenvalue 1, so I - A^T is exactly singular at alpha = 1.
  cyc <- matrix(0, 6L, 6L)
  cyc[cbind(seq_len(6L), c(2:6, 1L))] <- 1
  expect_error(centrality(cyc, measures = "alpha"), class = "cograph_singular_system")
  # A single undirected edge has eigenvalues +-1, so I - A is singular for power.
  edge <- matrix(c(0, 1, 1, 0), 2L, 2L)
  expect_error(centrality(edge, measures = "power"), class = "cograph_singular_system")
  # An edgeless graph is not a failed solve: power is NaN by definition (0/0).
  iso <- matrix(0, 3L, 3L)
  expect_true(all(is.nan(centrality(iso, measures = "power")$power)))
  # And a well-posed system solves.
  star <- matrix(0, 5L, 5L)
  star[1L, -1L] <- 1
  star[-1L, 1L] <- 1
  expect_true(all(is.finite(centrality(star, measures = c("alpha", "power"))$alpha)))
})

test_that("self-loops are counted igraph's way in coreness, laplacian and constraint", {
  skip_if_not_installed("igraph")
  m <- matrix(0, 6L, 6L)
  m[cbind(c(1, 2, 3, 4, 5, 1, 2), c(2, 3, 4, 5, 6, 1, 2))] <- 1
  m <- pmax(m, t(m))
  m[6L, 6L] <- 1   # a vertex whose only tie is its loop
  m[6L, 5L] <- m[5L, 6L] <- 0
  g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", weighted = TRUE)
  got <- centrality(m, measures = c("coreness", "laplacian", "constraint"))
  expect_equal(got$coreness_all, unname(igraph::coreness(g)))
  ref_lap <- vapply(seq_len(6L), function(v) {
    d <- igraph::degree(g, v)
    d^2 + d + 2 * sum(igraph::degree(g, igraph::neighbors(g, v)))
  }, numeric(1))
  expect_equal(got$laplacian, ref_lap)
  expect_equal(got$constraint, unname(igraph::constraint(g)))
  # Directed loops under every mode.
  d <- .port_zoo_matrix(7L, 0.4, directed = TRUE, weighted = FALSE, seed = 41L)
  diag(d)[c(2L, 5L)] <- 1
  gd <- igraph::graph_from_adjacency_matrix(d, mode = "directed", weighted = TRUE)
  lapply(c("all", "out", "in"), function(md) {
    expect_equal(centrality(d, measures = "coreness", mode = md)[[paste0("coreness_", md)]],
                 unname(igraph::coreness(gd, mode = md)), info = md)
    invisible(NULL)
  })
})

test_that("degree-type measures on igraph-object input take the unweighted reading", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(6L)
  df <- centrality(g, measures = c("strength", "eccentricity", "kreach"))
  expect_equal(df$strength_all, rep(2, 6L))
  expect_equal(df$eccentricity_all, rep(3, 6L))
  expect_equal(df$kreach_all, rep(5L, 6L))
})

test_that("the ported core never reaches igraph", {
  old <- options(cograph.forbid_igraph = TRUE)
  on.exit(options(old), add = TRUE)
  m <- .port_zoo_matrix(12L, 0.35, directed = TRUE, weighted = TRUE, seed = 51L)
  rownames(m) <- colnames(m) <- paste0("v", seq_len(12L))
  expect_no_error(centrality(m, measures = .port_core_measures))
  expect_no_error(centrality(m, measures = .port_core_measures, weighted = FALSE,
                             mode = "in", normalized = TRUE))
  expect_no_error(centrality(m, measures = "transitivity",
                             transitivity_type = "global"))
  expect_no_error(centrality(m, measures = "diffusion",
                             diffusion_method = "power_series"))
  expect_no_error(edge_centrality(m))
  expect_no_error(edge_centrality(m, invert_weights = TRUE))
  u <- pmax(m, t(m))
  expect_no_error(centrality(u, measures = .port_core_measures))
  expect_no_error(edge_centrality(u))
  # The one family deliberately left on igraph is reported as a leak, not
  # silently computed.
  expect_error(centrality(m, measures = "flow_betweenness"),
               class = if (requireNamespace("igraph", quietly = TRUE)) "cograph_igraph_leak" else "cograph_needs_igraph")
})

test_that("permutation invariance of the ported core", {
  m <- .port_zoo_matrix(10L, 0.4, directed = TRUE, weighted = TRUE, seed = 61L)
  rownames(m) <- colnames(m) <- paste0("v", seq_len(10L))
  set.seed(62)
  perm <- sample.int(10L)
  mp <- m[perm, perm]
  # voterank breaks ties by vertex index and is excluded from the invariance
  # check; every other measure is a function of the labelled graph alone.
  ms <- setdiff(.port_core_measures, "voterank")
  a <- centrality(m, measures = ms)
  b <- centrality(mp, measures = ms)
  b <- b[match(a$node, b$node), ]
  expect_equal(b[, -1L], a[, -1L], tolerance = 1e-9, ignore_attr = TRUE)
})
