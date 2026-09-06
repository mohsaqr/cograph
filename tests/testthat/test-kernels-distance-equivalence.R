# ===========================================================================
# .cg_distances() equivalence with igraph::distances()
# ===========================================================================
# The dependency-free Dijkstra must reproduce igraph exactly across a graph
# zoo chosen to hit the cases where conventions diverge: disconnected
# components, isolates, degenerate sizes, weights, and direction.

test_that("graph zoo: .cg_distances matches igraph::distances", {
  skip_if_not_installed("igraph")

  build <- function(n, density, directed, weighted, seed) {
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

  grid <- expand.grid(
    n = c(1L, 2L, 3L, 5L, 8L, 12L, 20L),
    density = c(0, 0.1, 0.35, 0.7, 1),
    directed = c(FALSE, TRUE),
    weighted = c(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )

  mismatches <- character(0)
  worst <- 0

  for (i in seq_len(nrow(grid))) {
    cfg <- grid[i, ]
    m <- build(cfg$n, cfg$density, cfg$directed, cfg$weighted, seed = 1000L + i)
    g <- igraph::graph_from_adjacency_matrix(
      m,
      mode = if (cfg$directed) "directed" else "undirected",
      weighted = if (cfg$weighted) TRUE else NULL,
      diag = FALSE
    )
    w <- if (cfg$weighted) NULL else NA
    modes <- if (cfg$directed) c("all", "out", "in") else "all"

    for (md in modes) {
      ref <- igraph::distances(g, mode = md, weights = w)
      got <- .cg_distances(m, md)
      dimnames(ref) <- NULL
      dimnames(got) <- NULL
      if (!isTRUE(all.equal(got, ref, tolerance = 1e-10))) {
        fin <- is.finite(got) & is.finite(ref)
        d <- if (any(fin)) max(abs(got[fin] - ref[fin])) else Inf
        worst <- max(worst, d)
        mismatches <- c(mismatches, sprintf(
          "n=%d dens=%.2f %s %s mode=%s (maxdiff %.3g)",
          cfg$n, cfg$density,
          if (cfg$directed) "directed" else "undirected",
          if (cfg$weighted) "weighted" else "unweighted", md, d))
      }
    }
  }

  expect_identical(mismatches, character(0))
})

test_that("closed-form graphs give exact geodesics", {
  # A path graph's distances are |i - j|; no reference package needed.
  n <- 6L
  m <- matrix(0, n, n)
  idx <- cbind(seq_len(n - 1L), seq_len(n - 1L) + 1L)
  m[idx] <- 1
  m[idx[, c(2, 1)]] <- 1
  expect_equal(.cg_distances(m, "all"), abs(outer(seq_len(n), seq_len(n), "-")) * 1)

  # A star: centre is 1 from every leaf, leaves are 2 from each other.
  s <- matrix(0, 5L, 5L)
  s[1L, -1L] <- 1
  s[-1L, 1L] <- 1
  d <- .cg_distances(s, "all")
  expect_equal(unname(d[1L, -1L]), rep(1, 4L))
  expect_equal(unname(d[2L, 3L]), 2)
})

test_that("disconnected and degenerate cases behave", {
  # Two isolated vertices: unreachable is Inf, self is 0.
  d <- .cg_distances(matrix(0, 2L, 2L), "all")
  expect_equal(diag(d), c(0, 0))
  expect_true(all(is.infinite(d[row(d) != col(d)])))

  # n = 1 and n = 0 must not error.
  expect_equal(.cg_distances(matrix(0, 1L, 1L), "all"), matrix(0, 1L, 1L))
  expect_equal(dim(.cg_distances(matrix(numeric(0), 0L, 0L), "all")), c(0L, 0L))
})

test_that("negative weights raise a classed condition, never a wrong number", {
  m <- matrix(c(0, -1, -1, 0), 2L, 2L)
  expect_error(.cg_distances(m, "all"), class = "cograph_negative_weights")
})

test_that("relabelling permutes distances identically", {
  set.seed(42)
  n <- 9L
  m <- matrix(0, n, n)
  idx <- which(row(m) != col(m))
  m[idx[stats::runif(length(idx)) < 0.4]] <- 1
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  p <- sample(n)
  expect_equal(.cg_distances(m[p, p], "all"), .cg_distances(m, "all")[p, p])
})

# ===========================================================================
# Spectral kernels
# ===========================================================================

test_that("subgraph and alpha match igraph across the zoo", {
  skip_if_not_installed("igraph")
  set.seed(7)
  mismatches <- character(0)
  for (n in c(2L, 4L, 7L, 11L)) {
    for (dens in c(0.2, 0.5, 0.9)) {
      m <- matrix(0, n, n)
      idx <- which(row(m) != col(m))
      m[idx[stats::runif(length(idx)) < dens]] <- 1
      m[lower.tri(m)] <- t(m)[lower.tri(m)]      # undirected
      g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", diag = FALSE)

      got <- .cg_subgraph(m, n, directed = FALSE)
      ref <- as.numeric(igraph::subgraph_centrality(g, diag = FALSE))
      if (!isTRUE(all.equal(got, ref, tolerance = 1e-5)))
        mismatches <- c(mismatches, sprintf("subgraph n=%d dens=%.1f", n, dens))

      # alpha = 1 can make (I - A) singular; igraph errors where this kernel
      # returns NaN, so there is nothing to compare on those graphs.
      ref_a <- tryCatch(
        as.numeric(igraph::alpha_centrality(g, exo = 1, tol = 1e-7,
                                            loops = FALSE, sparse = TRUE)),
        error = function(e) NULL)
      if (!is.null(ref_a)) {
        got_a <- .cg_alpha(m, n, 1)
        if (!isTRUE(all.equal(got_a, ref_a, tolerance = 1e-6)))
          mismatches <- c(mismatches, sprintf("alpha n=%d dens=%.1f", n, dens))
      }
    }
  }
  expect_identical(mismatches, character(0))
})

test_that("communicability is a true matrix exponential", {
  # Nilpotent A: exp(A) terminates at I + A + A^2/2, so no reference needed.
  a <- matrix(0, 3L, 3L); a[1L, 2L] <- 1; a[2L, 3L] <- 1
  expect_equal(as.matrix(Matrix::expm(Matrix::Matrix(a))),
               diag(3L) + a + (a %*% a) / 2, ignore_attr = TRUE)

  # Symmetric input must give the same answer down both code paths.
  s <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3L, 3L)
  expect_equal(.cg_communicability(s, 3L),
               rowSums(as.matrix(Matrix::expm(Matrix::Matrix(s)))),
               tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("power centrality reports NaN, not zero, on an edgeless graph", {
  # igraph rescales by the sum of squares; with no edges that is 0/0.
  expect_true(all(is.nan(.cg_power(matrix(0, 4L, 4L), 4L, 1))))
})

# ===========================================================================
# Path family
# ===========================================================================

test_that("betweenness, closeness, harmonic, eccentricity match igraph", {
  skip_if_not_installed("igraph")
  set.seed(11)
  mismatches <- character(0)

  for (n in c(3L, 6L, 10L, 15L)) {
    for (dens in c(0.15, 0.4, 0.8)) {
      for (directed in c(FALSE, TRUE)) {
        for (weighted in c(FALSE, TRUE)) {
          m <- matrix(0, n, n)
          idx <- which(row(m) != col(m))
          take <- idx[stats::runif(length(idx)) < dens]
          m[take] <- if (weighted) stats::runif(length(take), 0.1, 1) else 1
          if (!directed) m[lower.tri(m)] <- t(m)[lower.tri(m)]
          g <- igraph::graph_from_adjacency_matrix(
            m, mode = if (directed) "directed" else "undirected",
            weighted = if (weighted) TRUE else NULL, diag = FALSE)
          w <- if (weighted) NULL else NA
          tag <- sprintf("n=%d dens=%.2f %s %s", n, dens,
                         if (directed) "dir" else "und",
                         if (weighted) "wt" else "unwt")

          bw <- .cg_betweenness(
            .cg_mode_weights(m, if (directed) "out" else "all"), n, directed)
          ref_bw <- as.numeric(igraph::betweenness(g, weights = w,
                                                   directed = directed))
          if (!isTRUE(all.equal(bw, ref_bw, tolerance = 1e-9)))
            mismatches <- c(mismatches, paste("betweenness", tag))

          d <- .cg_distances(m, "all")
          checks <- list(
            closeness = list(.cg_closeness(d, n),
                             as.numeric(igraph::closeness(g, weights = w, mode = "all"))),
            harmonic = list(.cg_harmonic(d, n),
                            as.numeric(igraph::harmonic_centrality(g, weights = w, mode = "all"))),
            eccentricity = list(.cg_eccentricity(d, n),
                                as.numeric(igraph::eccentricity(g, weights = w, mode = "all")))
          )
          for (nm in names(checks)) {
            got <- checks[[nm]][[1]]; ref <- checks[[nm]][[2]]
            na_got <- is.na(got) | is.nan(got); na_ref <- is.na(ref) | is.nan(ref)
            cmp <- !na_got & !na_ref
            okay <- identical(na_got, na_ref) &&
              (!any(cmp) || isTRUE(all.equal(got[cmp], ref[cmp], tolerance = 1e-9)))
            if (!okay) mismatches <- c(mismatches, paste(nm, tag))
          }
        }
      }
    }
  }
  expect_identical(mismatches, character(0))
})

test_that("betweenness has closed-form values on a star and a path", {
  # Star centre lies on every leaf-to-leaf geodesic: (k-1)(k-2)/2 with k leaves.
  k <- 6L
  s <- matrix(0, k + 1L, k + 1L)
  s[1L, -1L] <- 1; s[-1L, 1L] <- 1
  bw <- .cg_betweenness(s, k + 1L, directed = FALSE)
  expect_equal(bw[1L], k * (k - 1) / 2)
  expect_equal(unname(bw[-1L]), rep(0, k))

  # Path graph: interior vertex i has (i-1)(n-i) pairs through it.
  n <- 5L
  p <- matrix(0, n, n)
  ii <- cbind(seq_len(n - 1L), seq_len(n - 1L) + 1L)
  p[ii] <- 1; p[ii[, c(2, 1)]] <- 1
  expect_equal(.cg_betweenness(p, n, directed = FALSE),
               (seq_len(n) - 1) * (n - seq_len(n)))
})

# ===========================================================================
# Iterative family
# ===========================================================================

test_that("pagerank and coreness match igraph across the zoo", {
  skip_if_not_installed("igraph")
  set.seed(19)
  mismatches <- character(0)
  for (n in c(4L, 8L, 14L)) {
    for (dens in c(0.15, 0.4, 0.75)) {
      for (directed in c(FALSE, TRUE)) {
        m <- matrix(0, n, n)
        idx <- which(row(m) != col(m))
        m[idx[stats::runif(length(idx)) < dens]] <- 1
        if (!directed) m[lower.tri(m)] <- t(m)[lower.tri(m)]
        g <- igraph::graph_from_adjacency_matrix(
          m, mode = if (directed) "directed" else "undirected", diag = FALSE)
        tag <- sprintf("n=%d dens=%.2f %s", n, dens, if (directed) "dir" else "und")

        if (!isTRUE(all.equal(.cg_pagerank(m, n),
                              as.numeric(igraph::page_rank(g)$vector),
                              tolerance = 1e-6)))
          mismatches <- c(mismatches, paste("pagerank", tag))

        if (!isTRUE(all.equal(.cg_coreness(m, n, directed),
                              as.numeric(igraph::coreness(g, mode = "all")),
                              tolerance = 1e-9)))
          mismatches <- c(mismatches, paste("coreness", tag))
      }
    }
  }
  expect_identical(mismatches, character(0))
})

test_that("eigenvector satisfies its defining equation", {
  # Stronger than comparing to a reference: igraph's ARPACK returns an
  # all-zero vector on some connected directed graphs, so agreement with it
  # is not evidence of correctness. A^T v = lambda v is.
  set.seed(23)
  for (n in c(5L, 9L, 13L)) {
    for (directed in c(FALSE, TRUE)) {
      m <- matrix(0, n, n)
      idx <- which(row(m) != col(m))
      m[idx[stats::runif(length(idx)) < 0.4]] <- 1
      if (!directed) m[lower.tri(m)] <- t(m)[lower.tri(m)]
      if (all(m == 0)) next
      v <- .cg_eigenvector(m, n)
      lambda <- max(Re(eigen(if (isSymmetric(unname(m))) m else t(m))$values))
      if (lambda > 1e-8) {
        expect_equal(as.numeric(crossprod(m, v)), lambda * v, tolerance = 1e-8)
      }
    }
  }
})

test_that("bipartite graphs do not defeat eigenvector centrality", {
  # A star is bipartite: power iteration oscillates forever between the two
  # sides. The centre must score 1 and each of k leaves 1/sqrt(k).
  k <- 3L
  s <- matrix(0, k + 1L, k + 1L)
  s[1L, -1L] <- 1; s[-1L, 1L] <- 1
  v <- .cg_eigenvector(s, k + 1L)
  expect_equal(v[1L], 1)
  expect_equal(unname(v[-1L]), rep(1 / sqrt(k), k), tolerance = 1e-10)
})

test_that("edgeless graphs report the uniform vector, not zero", {
  expect_equal(.cg_eigenvector(matrix(0, 4L, 4L), 4L), rep(1, 4L))
  h <- .cg_hits(matrix(0, 4L, 4L), 4L)
  expect_equal(h$hub, rep(1, 4L))
  expect_equal(h$authority, rep(1, 4L))
})

# ===========================================================================
# Local / neighbourhood family
# ===========================================================================

test_that("constraint, transitivity, leverage and degree match igraph", {
  skip_if_not_installed("igraph")
  set.seed(29)
  mismatches <- character(0)
  agree <- function(got, ref) {
    na_got <- is.na(got) | is.nan(got); na_ref <- is.na(ref) | is.nan(ref)
    cmp <- !na_got & !na_ref
    identical(na_got, na_ref) &&
      (!any(cmp) || isTRUE(all.equal(got[cmp], ref[cmp], tolerance = 1e-9)))
  }
  for (n in c(3L, 6L, 10L, 14L)) {
    for (dens in c(0.15, 0.4, 0.75)) {
      for (directed in c(FALSE, TRUE)) {
        m <- matrix(0, n, n)
        idx <- which(row(m) != col(m))
        m[idx[stats::runif(length(idx)) < dens]] <- 1
        if (!directed) m[lower.tri(m)] <- t(m)[lower.tri(m)]
        g <- igraph::graph_from_adjacency_matrix(
          m, mode = if (directed) "directed" else "undirected", diag = FALSE)
        tag <- sprintf("n=%d dens=%.2f %s", n, dens, if (directed) "dir" else "und")

        if (!agree(.cg_constraint(m, n), as.numeric(igraph::constraint(g))))
          mismatches <- c(mismatches, paste("constraint", tag))
        # Compared against cograph's own centrality(), not raw
        # igraph::transitivity(): on directed input the two disagree, and
        # centrality() is the contract this kernel has to preserve.
        ref_tr <- suppressWarnings(as.numeric(unlist(
          centrality(m, measures = "transitivity", mode = "all",
                     directed = directed, weighted = FALSE, normalized = FALSE,
                     loops = FALSE, invert_weights = FALSE)$transitivity)))
        if (!agree(.cg_local_transitivity(m, n, directed), ref_tr))
          mismatches <- c(mismatches, paste("transitivity", tag))
        if (!agree(.cg_degree(m, directed, "all"),
                   as.numeric(igraph::degree(g, mode = "all", loops = FALSE))))
          mismatches <- c(mismatches, paste("degree", tag))
      }
    }
  }
  expect_identical(mismatches, character(0))
})

test_that("a reciprocated dyad counts twice on a directed graph", {
  # igraph's mode="all" degree double-counts reciprocation, so a vertex whose
  # only neighbour is reciprocated has degree 2 and scores 0, not NaN.
  m <- matrix(0, 3L, 3L)
  m[1L, 3L] <- 1; m[3L, 1L] <- 1; m[2L, 1L] <- 1
  expect_equal(.cg_local_transitivity(m, 3L, directed = TRUE), c(0, NaN, 0))
  expect_equal(.cg_degree(m, directed = TRUE, "all"), c(3, 1, 2))
  # Undirected reading of the same matrix collapses the dyad to one edge.
  expect_true(is.nan(.cg_local_transitivity(m, 3L, directed = FALSE)[3L]))
})

test_that("constraint is NaN for an isolate, never zero", {
  m <- matrix(0, 3L, 3L)
  m[1L, 2L] <- 1; m[2L, 1L] <- 1
  expect_true(is.nan(.cg_constraint(m, 3L)[3L]))
  expect_true(is.nan(.cg_leverage(m, 3L)[3L]))
})

# ===========================================================================
# Linear-solve and peeling family
# ===========================================================================

test_that("katz and hubbell match centiserve's construction", {
  skip_if_not_installed("igraph")
  set.seed(37)
  for (n in c(4L, 7L, 11L)) {
    m <- matrix(0, n, n)
    idx <- which(row(m) != col(m))
    m[idx[stats::runif(length(idx)) < 0.3]] <- 1
    m[lower.tri(m)] <- t(m)[lower.tri(m)]
    # Katz: (I - alpha A^T)^-1 1, solved directly as the reference does.
    expect_equal(.cg_katz(m, 0.1),
                 as.numeric(solve(diag(1, n) - 0.1 * t(m)) %*% rep(1, n)),
                 tolerance = 1e-9)
  }
})

test_that("hubbell refuses a divergent weight factor rather than guessing", {
  # K3 at weightfactor 0.5 puts the spectral radius exactly at 1: the Neumann
  # series does not converge, so a number here would be meaningless.
  k3 <- matrix(1, 3L, 3L); diag(k3) <- 0
  expect_true(all(is.na(.cg_hubbell(k3, 0.5))))
  # Well inside the radius it must agree with the direct solve.
  expect_equal(.cg_hubbell(k3, 0.1),
               as.numeric(solve(diag(1, 3L) - 0.1 * k3) %*% rep(1, 3L)),
               tolerance = 1e-9)
})

test_that("effective size and onion layers have known values", {
  # A star: the centre's neighbours share nothing, so effective size = k.
  k <- 4L
  s <- matrix(0, k + 1L, k + 1L)
  s[1L, -1L] <- 1; s[-1L, 1L] <- 1
  expect_equal(.cg_effective_size(s, directed = FALSE)[1L], k)
  # A complete graph is a single onion layer.
  kk <- matrix(1, 5L, 5L); diag(kk) <- 0
  expect_equal(length(unique(.cg_onion(kk, directed = FALSE))), 1L)
})

test_that("diversity is zero when a vertex has one edge, one when uniform", {
  # Entropy of a single weight is undefined; the convention is 0.
  m <- matrix(0, 3L, 3L); m[1L, 2L] <- 0.4; m[2L, 1L] <- 0.4
  expect_equal(.cg_diversity(m, weighted = TRUE, directed = FALSE)[1L], 0)
  # Equal weights on two edges maximise normalised entropy at 1.
  u <- matrix(0, 3L, 3L)
  u[1L, 2L] <- 0.5; u[2L, 1L] <- 0.5; u[1L, 3L] <- 0.5; u[3L, 1L] <- 0.5
  expect_equal(.cg_diversity(u, weighted = TRUE, directed = FALSE)[1L], 1)
})

# ===========================================================================
# Community-aware and directed-prestige family
# ===========================================================================

test_that("community-aware measures require a partition and say so", {
  b <- matrix(0, 4L, 4L); b[1L, 2L] <- 1; b[2L, 1L] <- 1
  adj <- .cg_adjlist(b, directed = FALSE, "all")
  # Without a partition there is no module to be central within: NaN, not 0.
  expect_true(all(is.nan(.cg_participation(adj, .cg_degree(b, FALSE, "all")))))
  expect_true(all(is.nan(.cg_within_module_z(adj))))
})

test_that("within-module z is NaN when a module has no spread", {
  # Every member with the same within-degree gives sd = 0; a z-score would be
  # a division by zero dressed up as a finding.
  b <- matrix(1, 4L, 4L); diag(b) <- 0
  adj <- .cg_adjlist(b, directed = FALSE, "all")
  expect_true(all(is.nan(.cg_within_module_z(adj, membership = rep(1L, 4L)))))
})

test_that("participation is 0 inside one module and rises when ties spread", {
  b <- matrix(0, 4L, 4L)
  for (e in list(c(1, 2), c(1, 3), c(1, 4))) { b[e[1], e[2]] <- 1; b[e[2], e[1]] <- 1 }
  adj <- .cg_adjlist(b, directed = FALSE, "all")
  deg <- .cg_degree(b, FALSE, "all")
  expect_equal(.cg_participation(adj, deg, membership = rep(1L, 4L))[1L], 0)
  # Three neighbours in three modules: 1 - 3*(1/3)^2 = 2/3.
  expect_equal(.cg_participation(adj, deg, membership = c(1L, 1L, 2L, 3L))[1L],
               1 - ((1 / 3)^2 + (1 / 3)^2 + (1 / 3)^2))
})

test_that("directed-only measures return NA on undirected input", {
  b <- matrix(0, 3L, 3L); b[1L, 2L] <- 1; b[2L, 1L] <- 1
  expect_true(all(is.na(.cg_prestige_domain(b, FALSE, directed = FALSE))))
  expect_true(all(is.na(.cg_leaderrank(b, directed = FALSE))))
  expect_true(all(is.na(.cg_trophic_level(b, directed = FALSE))))
})

test_that("expected influence keeps sign, unlike a magnitude sum", {
  # A strong negative tie must pull influence down; abs() would hide it.
  m <- matrix(0, 3L, 3L)
  m[1L, 2L] <- 0.8; m[2L, 1L] <- 0.8
  m[1L, 3L] <- -0.8; m[3L, 1L] <- -0.8
  expect_equal(.cg_expected_influence(m, "all", 1L)[1L], 0)
})

# ===========================================================================
# Flow, walk and role families
# ===========================================================================

test_that("flow_betweenness is left on igraph by design", {
  # Matching it is POSSIBLE -- igraph's decomposition is fixed by a cleanup
  # phase (excess return, then DFS cycle cancelling) and transcribing that
  # reproduced all 99 fixtures bit-exactly. It is not kept because the cost is
  # ~510 lines, 18% of this kernel corpus, for 1 of 89 measures, and it is the
  # only component that would transcribe igraph's C internals.
  #
  # Guard against reintroduction: a kernel here would be dead code unless
  # wired in, and wiring it in would make cograph depend on a reimplementation
  # of igraph's arc ordering.
  expect_false(exists(".cg_flow_betweenness"))
  expect_false(exists(".cg_max_flow"))
  expect_false(exists(".cg_pr_max_flow"))

  # The supported from-scratch alternative is the current-flow version, which
  # is a linear solve and therefore uniquely defined.
  expect_true(exists(".cg_current_flow_betweenness"))
})

test_that("stationary distribution is a genuine fixed point", {
  # pi P = pi, checked directly rather than against another implementation.
  set.seed(17)
  n <- 6L
  a <- matrix(0, n, n)
  idx <- which(row(a) != col(a))
  a[idx[stats::runif(length(idx)) < 0.6]] <- 1
  a[lower.tri(a)] <- t(a)[lower.tri(a)]
  deg <- rowSums(a); deg[deg == 0] <- 1
  p <- a / deg
  pi_v <- .cg_stationary(p, n)
  expect_equal(as.numeric(crossprod(p, pi_v)), pi_v, tolerance = 1e-8)
  expect_equal(sum(pi_v), 1, tolerance = 1e-10)
})

test_that("markov family is NA on a disconnected graph, not per-component", {
  b <- matrix(0, 4L, 4L)
  b[1L, 2L] <- 1; b[2L, 1L] <- 1
  b[3L, 4L] <- 1; b[4L, 3L] <- 1
  r <- .cg_mfpt(b, 4L, directed = FALSE)
  expect_true(all(is.na(r$markov)))
  expect_true(all(is.na(r$random_walk)))
})

test_that("brokerage roles partition the open two-paths", {
  # a -> v -> c with no a -> c shortcut: every such triple lands in exactly
  # one of the five roles, so the counts must sum to the number of triples.
  set.seed(31)
  n <- 7L
  b <- matrix(0, n, n)
  idx <- which(row(b) != col(b))
  b[idx[stats::runif(length(idx)) < 0.35]] <- 1
  memb <- c(1L, 1L, 2L, 2L, 3L, 3L, 1L)
  roles <- c("coordinator", "itinerant", "representative",
             "gatekeeper", "liaison")
  totals <- rowSums(vapply(roles, function(r)
    .cg_brokerage(b, memb, r, directed = TRUE), numeric(n)))
  triples <- 0
  for (v in seq_len(n)) {
    for (a in which(b[, v] != 0)) {
      for (cc in which(b[v, ] != 0)) {
        if (a != cc && b[a, cc] == 0) triples <- triples + 1
      }
    }
  }
  expect_equal(sum(totals), triples)
})

test_that("spanning tree is NaN on a disconnected graph", {
  b <- matrix(0, 4L, 4L)
  b[1L, 2L] <- 1; b[2L, 1L] <- 1
  expect_true(all(is.nan(.cg_spanning_tree(b, 4L, FALSE, FALSE))))
})

test_that("brokerage assigns each role to the right triple", {
  # One open two-path a -> v -> c per graph, with memberships chosen so that
  # exactly one role can apply. A rule that merely partitions the triples
  # correctly would still pass a totals check; this pins the labels.
  triple <- function() {
    b <- matrix(0, 3L, 3L)
    b[1L, 2L] <- 1; b[2L, 3L] <- 1   # a=1, v=2, c=3, no 1->3
    b
  }
  b <- triple()
  role_of <- function(memb) {
    roles <- c("coordinator", "itinerant", "representative",
               "gatekeeper", "liaison")
    hit <- roles[vapply(roles, function(r)
      .cg_brokerage(b, memb, r, directed = TRUE)[2L] == 1, logical(1L))]
    hit
  }
  expect_equal(role_of(c(1L, 1L, 1L)), "coordinator")     # all one group
  expect_equal(role_of(c(1L, 2L, 1L)), "itinerant")       # a, c share; v outside
  expect_equal(role_of(c(1L, 1L, 2L)), "representative")  # a, v share
  expect_equal(role_of(c(1L, 2L, 2L)), "gatekeeper")      # v, c share
  expect_equal(role_of(c(1L, 2L, 3L)), "liaison")         # all distinct
})

test_that("stationary picks eigenvalue 1, not merely the largest", {
  # A directed graph with a sink makes P substochastic, so its dominant
  # eigenvalue is below 1 and the two selection rules diverge. On a fully
  # stochastic chain they coincide, which is why a connected undirected
  # example cannot tell them apart.
  n <- 4L
  a <- matrix(0, n, n)
  a[1L, 2L] <- 1; a[2L, 3L] <- 1; a[3L, 1L] <- 1; a[3L, 4L] <- 1
  deg <- rowSums(a); deg[deg == 0] <- 1
  p <- a / deg
  pi_v <- .cg_stationary(p, n)
  vals <- Re(eigen(t(p))$values)
  expect_lt(max(vals), 1 - 1e-9)          # no eigenvalue reaches 1
  # The chosen eigenvalue must be the one nearest 1, not the largest.
  chosen <- vals[which.min(abs(vals - 1))]
  expect_equal(chosen, max(vals[abs(vals - 1) == min(abs(vals - 1))]))
  expect_equal(sum(pi_v), 1, tolerance = 1e-10)
})

test_that("infection matches cograph's self-avoiding-walk count", {
  skip_if_not_installed("igraph")
  # Enumerating self-avoiding walks is exponential, so this stays on small
  # graphs; the fixture set covers the larger ones separately.
  set.seed(101)
  mismatches <- character(0)
  for (n in c(4L, 6L, 8L, 10L)) {
    for (dens in c(0.2, 0.4, 0.6)) {
      for (directed in c(FALSE, TRUE)) {
        m <- matrix(0, n, n)
        idx <- which(row(m) != col(m))
        m[idx[stats::runif(length(idx)) < dens]] <- 1
        if (!directed) m[lower.tri(m)] <- t(m)[lower.tri(m)]
        g <- igraph::graph_from_adjacency_matrix(
          m, mode = if (directed) "directed" else "undirected", diag = FALSE)
        got <- .cg_infection(m, directed)
        ref <- as.numeric(calculate_infection(g))
        if (!isTRUE(all.equal(as.numeric(got), ref, tolerance = 1e-9)))
          mismatches <- c(mismatches, sprintf("n=%d dens=%.1f %s", n, dens,
                                              if (directed) "dir" else "und"))
      }
    }
  }
  expect_identical(mismatches, character(0))
})

# ===========================================================================
# mode = "in" / "out"
# ===========================================================================
# The JSON fixture set carries only "_all" columns, so these branches have no
# fixture coverage at all and are checked against centrality() directly.

test_that("mode-aware kernels match centrality() at in and out", {
  skip_if_not_installed("igraph")
  gen <- function(n, dens, weighted, seed) {
    set.seed(seed)
    m <- matrix(0, n, n)
    idx <- which(row(m) != col(m))
    take <- idx[stats::runif(length(idx)) < dens]
    m[take] <- if (weighted) stats::runif(length(take), 0.1, 1) else 1
    m
  }
  kern <- list(
    degree = function(b, wm, md, n) .cg_degree(b, TRUE, md),
    strength = function(b, wm, md, n) .cg_strength(wm, TRUE, md),
    closeness = function(b, wm, md, n) .cg_closeness(.cg_distances(wm, md), n),
    harmonic = function(b, wm, md, n) .cg_harmonic(.cg_distances(wm, md), n),
    eccentricity = function(b, wm, md, n) .cg_eccentricity(.cg_distances(wm, md), n),
    coreness = function(b, wm, md, n) .cg_coreness(b, n, TRUE, md),
    leverage = function(b, wm, md, n) .cg_leverage(wm, n, TRUE, md),
    lobby = function(b, wm, md, n) .cg_lobby(b, TRUE, md),
    lin = function(b, wm, md, n) .cg_lin(.cg_distances(wm, md), n),
    harary = function(b, wm, md, n) .cg_harary(.cg_distances(wm, md), n),
    kreach = function(b, wm, md, n) .cg_kreach(.cg_distances(wm, md), n, 3),
    diffusion = function(b, wm, md, n) .cg_diffusion(b, TRUE, md, 1),
    lac = function(b, wm, md, n) .cg_lac(b, TRUE, md),
    semilocal = function(b, wm, md, n) .cg_semilocal(.cg_adjlist(b, TRUE, md))
  )
  agree <- function(a, r) {
    na1 <- is.na(a) | is.nan(a); na2 <- is.na(r) | is.nan(r)
    i1 <- is.infinite(a); i2 <- is.infinite(r)
    cmp <- !na1 & !na2 & !i1 & !i2
    identical(na1, na2) && identical(i1, i2) &&
      (!any(cmp) || isTRUE(all.equal(a[cmp], r[cmp], tolerance = 1e-8)))
  }
  mismatches <- character(0)
  grid <- expand.grid(n = c(5L, 8L), dens = c(0.25, 0.5),
                      weighted = c(FALSE, TRUE))
  for (md in c("out", "in")) {
    for (i in seq_len(nrow(grid))) {
      cfg <- grid[i, ]
      m <- gen(cfg$n, cfg$dens, cfg$weighted, 700L + i)
      b <- (m != 0) * 1
      wm <- if (cfg$weighted) m else b
      for (k in names(kern)) {
        ref <- suppressWarnings(tryCatch(as.numeric(centrality(
          m, measures = k, mode = md, directed = TRUE,
          weighted = cfg$weighted, normalized = FALSE, loops = FALSE,
          invert_weights = FALSE)[[2L]]), error = function(e) NULL))
        if (is.null(ref)) next
        got <- as.numeric(kern[[k]](b, wm, md, cfg$n))
        if (!agree(got, ref))
          mismatches <- c(mismatches, sprintf("%s/%s n=%d", k, md, cfg$n))
      }
    }
  }
  expect_identical(mismatches, character(0))
})

test_that("average_distance propagates Inf, matching cograph", {
  # The JSON fixtures cannot represent Inf (the generator's clean_vec maps
  # every non-finite value to NA), so they must not be the oracle here.
  m <- matrix(0, 3L, 3L); m[1L, 2L] <- 1; m[2L, 1L] <- 1
  got <- .cg_average_distance(.cg_distances(m, "all"), 3L)
  expect_true(all(is.infinite(got)))
  expect_false(any(is.na(got)))
})

# ===========================================================================
# Regressions from the adversarial review
# ===========================================================================

test_that("self-loops count twice where both endpoints are counted", {
  skip_if_not_installed("igraph")
  # An undirected loop, and a directed loop under mode="all", contribute 2 --
  # treating the diagonal as an ordinary entry undercounts every looped vertex.
  m <- matrix(c(2, 3, 0,
                3, 0, 4,
                0, 4, 5), 3L, 3L, byrow = TRUE)
  b <- (m != 0) * 1
  gu <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", weighted = TRUE)
  gd <- igraph::graph_from_adjacency_matrix(m, mode = "directed", weighted = TRUE)

  expect_equal(.cg_degree(b, FALSE, "all", loops = TRUE),
               as.numeric(igraph::degree(gu, loops = TRUE)))
  expect_equal(.cg_degree(b, FALSE, "all", loops = FALSE),
               as.numeric(igraph::degree(gu, loops = FALSE)))
  expect_equal(.cg_strength(m, FALSE, "all", loops = TRUE),
               as.numeric(igraph::strength(gu, loops = TRUE)))
  for (md in c("all", "out", "in")) {
    expect_equal(.cg_degree(b, TRUE, md, loops = TRUE),
                 as.numeric(igraph::degree(gd, mode = md, loops = TRUE)))
    expect_equal(.cg_strength(m, TRUE, md, loops = TRUE),
                 as.numeric(igraph::strength(gd, mode = md, loops = TRUE)))
  }
})

test_that("local_hindex honours mode", {
  b <- matrix(0, 5L, 5L)
  b[1L,2L] <- 1; b[1L,3L] <- 1; b[2L,3L] <- 1
  b[3L,1L] <- 1; b[3L,4L] <- 1; b[4L,2L] <- 1; b[5L,4L] <- 1
  for (md in c("all", "out", "in")) {
    ref <- suppressWarnings(as.numeric(centrality(b, measures = "local_hindex",
      mode = md, directed = TRUE, weighted = FALSE, normalized = FALSE,
      loops = FALSE, invert_weights = FALSE)[[2L]]))
    expect_equal(.cg_local_hindex(b, TRUE, md), ref)
  }
  # The three modes must not all collapse to the same answer here.
  expect_false(identical(.cg_local_hindex(b, TRUE, "out"),
                         .cg_local_hindex(b, TRUE, "all")))
})

test_that("dominant eigenvalue: max modulus, tie-broken by real part", {
  # Signed: the spectral radius is NEGATIVE, so selecting by real part is wrong.
  ms <- matrix(c(0,-1,2, 0,0,1, 1,0,0), 3L, 3L, byrow = TRUE)
  ref <- suppressWarnings(as.numeric(centrality(ms, measures = "eigenvector",
    mode = "all", directed = TRUE, weighted = TRUE, normalized = FALSE,
    loops = FALSE, invert_weights = FALSE)[[2L]]))
  expect_equal(.cg_eigenvector(ms, 3L), ref, tolerance = 1e-6)

  # Periodic: a directed 3-cycle has eigenvalues 1, w, w^2 -- all modulus 1.
  # Selecting by modulus alone can land on a complex one.
  mc <- matrix(0, 3L, 3L); mc[1L,2L] <- 1; mc[2L,3L] <- 1; mc[3L,1L] <- 1
  expect_equal(.cg_eigenvector(mc, 3L), rep(1, 3L), tolerance = 1e-9)
})

test_that("negative weights are refused, never silently walked", {
  ms <- matrix(c(0,-1,2, 0,0,1, 1,0,0), 3L, 3L, byrow = TRUE)
  # Unguarded, PageRank's iteration diverges to ~1e137 rather than failing.
  expect_error(.cg_pagerank(ms, 3L), class = "cograph_negative_weights")
  expect_error(.cg_alpha(ms, 3L, 1), class = "cograph_negative_weights")
  expect_error(.cg_distances(ms, "all"), class = "cograph_negative_weights")
})

test_that("personalized pagerank matches the reference", {
  m <- matrix(0, 3L, 3L); m[1L,2L] <- 1; m[2L,3L] <- 1; m[3L,1L] <- 1
  pv <- c(0.6, 0.3, 0.1)
  ref <- suppressWarnings(as.numeric(centrality(m, measures = "pagerank",
    mode = "all", directed = TRUE, weighted = FALSE, normalized = FALSE,
    loops = FALSE, invert_weights = FALSE, personalized = pv)[[2L]]))
  expect_equal(.cg_pagerank(m, 3L, personalized = pv), ref, tolerance = 1e-8)
  # Uniform reset must still give the plain answer.
  expect_equal(.cg_pagerank(m, 3L), rep(1/3, 3L), tolerance = 1e-9)
  expect_error(.cg_pagerank(m, 3L, personalized = c(-1, 1, 1)),
               class = "cograph_bad_input")
})

test_that("cutoff truncates paths as igraph does", {
  skip_if_not_installed("igraph")
  p <- matrix(0, 4L, 4L)
  ii <- cbind(seq_len(3L), seq_len(3L) + 1L)
  p[ii] <- 1; p[ii[, c(2, 1)]] <- 1
  g <- igraph::graph_from_adjacency_matrix(p, mode = "undirected", diag = FALSE)
  expect_equal(.cg_betweenness(p, 4L, FALSE, cutoff = 1),
               as.numeric(igraph::betweenness(g, cutoff = 1)))
  expect_equal(.cg_closeness(.cg_distances(p, "all", cutoff = 1), 4L),
               as.numeric(igraph::closeness(g, cutoff = 1)))
  # Without a cutoff the interior vertices do separate pairs.
  expect_gt(sum(.cg_betweenness(p, 4L, FALSE)), 0)
})

test_that("global transitivity is a graph-level quantity", {
  skip_if_not_installed("igraph")
  tri <- matrix(0, 4L, 4L)
  for (e in list(c(1,2), c(1,3), c(2,3), c(3,4))) {
    tri[e[1], e[2]] <- 1; tri[e[2], e[1]] <- 1
  }
  g <- igraph::graph_from_adjacency_matrix(tri, mode = "undirected", diag = FALSE)
  expect_equal(.cg_global_transitivity(tri),
               igraph::transitivity(g, type = "global"))
  # It is NOT the mean of the local coefficients.
  expect_false(isTRUE(all.equal(.cg_global_transitivity(tri),
    mean(.cg_local_transitivity(tri, 4L, FALSE), na.rm = TRUE))))
})

test_that("strong components survive a graph too deep for recursion", {
  # The recursive Tarjan overflowed R's node stack here; the iterative form
  # must not.
  n <- 2501L
  big <- matrix(0, n, n)
  big[1L, -1L] <- 1
  idx <- cbind(2:(n - 1L), 3:n)
  big[idx] <- 1
  expect_equal(length(.cg_strong_components(big)), n)
  expect_equal(.cg_dmnc(big, TRUE, "out", 1.7)[1L], 2499)
})

test_that("shortest-path ties use a scale-relative tolerance", {
  # At a distance of 2e9 an absolute 1.5e-8 epsilon stops recognising ties
  # the reference still treats as tied.
  m <- matrix(0, 4L, 4L)
  m[1L,2L] <- 1e9; m[2L,1L] <- 1e9
  m[2L,4L] <- 1e9; m[4L,2L] <- 1e9
  m[1L,3L] <- 1e9; m[3L,1L] <- 1e9
  m[3L,4L] <- 1e9 + 10; m[4L,3L] <- 1e9 + 10
  ref <- suppressWarnings(as.numeric(centrality(m, measures = "stress",
    mode = "all", directed = FALSE, weighted = TRUE, normalized = FALSE,
    loops = FALSE, invert_weights = FALSE)[[2L]]))
  expect_equal(.cg_stress(.cg_mode_weights(m, "all"), 4L, FALSE, TRUE), ref)
})

test_that("the kernels are actually reachable from centrality()", {
  # The distance family routes through .cg_distances via .cg_path_matrix.
  # Without this the kernels are dead code and the public path is unchanged.
  expect_true(any(grepl("\\.cg_distances", readLines("../../R/centrality.R"))))
  set.seed(4); n <- 10L
  m <- matrix(0, n, n)
  idx <- which(row(m) != col(m))
  m[idx[stats::runif(length(idx)) < 0.4]] <- 1
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  for (k in c("radiality", "lin", "harary", "wiener", "barycenter")) {
    v <- suppressWarnings(as.numeric(centrality(m, measures = k, mode = "all",
      directed = FALSE, weighted = FALSE, normalized = FALSE, loops = FALSE,
      invert_weights = FALSE)[[2L]]))
    expect_length(v, n)
    expect_true(all(is.finite(v)))
  }
})

test_that("communicability kernels stay faithful to their references", {
  # The two reference functions differ: calculate_communicability() applies
  # t(V) unconditionally (wrong on asymmetric input), while
  # calculate_communicability_betweenness() correctly uses solve(V). Since
  # these kernels are wired into centrality(), both must reproduce their own
  # reference rather than one shared "corrected" behaviour.
  m <- matrix(c(0,0,1, 1,0,0, 1,0,0), 3L, 3L, byrow = TRUE)
  ref <- suppressWarnings(as.numeric(centrality(m, measures = "communicability",
    mode = "all", directed = TRUE, weighted = FALSE, normalized = FALSE,
    loops = FALSE, invert_weights = FALSE)[[2L]]))
  expect_equal(.cg_communicability(m, 3L), ref, tolerance = 1e-8)

  # A singular reduced eigenbasis aborts the reference outright, so the whole
  # vector is NA -- never a partially-filled result.
  expect_true(all(is.na(.cg_communicability_betweenness(m, 3L))))
})

test_that("expm by eigendecomposition matches a known closed form", {
  # Symmetric route, checked against the terminating series for a nilpotent
  # matrix embedded symmetrically.
  s <- matrix(c(0,1,1, 1,0,1, 1,1,0), 3L, 3L)
  expect_equal(.cg_expm_eigen(s, TRUE),
               as.matrix(Matrix::expm(Matrix::Matrix(s))),
               tolerance = 1e-9, ignore_attr = TRUE)
  # Asymmetric route: with distinct, well-separated eigenvalues the
  # eigendecomposition is well conditioned and agrees with a true matrix
  # exponential. (On an ill-conditioned basis the two routes legitimately
  # diverge -- which is exactly when the reference returns NA.)
  a <- matrix(c(1, 2, 0,
                0, 3, 0,
                0, 0, 5), 3L, 3L, byrow = TRUE)
  expect_equal(.cg_expm_eigen(a, FALSE),
               as.matrix(Matrix::expm(Matrix::Matrix(a))),
               tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("weighted reaching centrality breaks shortest-path ties as the reference does", {
  # Two equal-cost routes 1->2->3 and 1->4->3; the reference takes the one
  # through vertex 4. Getting this wrong changes the reported value.
  m <- matrix(0, 4L, 4L)
  m[1L, 2L] <- 2; m[1L, 4L] <- 2; m[2L, 3L] <- 3; m[3L, 4L] <- 3
  ref <- suppressWarnings(as.numeric(centrality(m, measures = "reaching_local",
    mode = "all", directed = TRUE, weighted = TRUE, normalized = FALSE,
    loops = FALSE, invert_weights = FALSE)[[2L]]))
  expect_equal(.cg_reaching_local((m != 0) * 1, m, TRUE, "all"), ref,
               tolerance = 1e-8)
  # Untied graphs must be unaffected by the tie rule.
  set.seed(5)
  n <- 7L
  w <- matrix(0, n, n)
  idx <- which(row(w) != col(w))
  take <- idx[stats::runif(length(idx)) < 0.4]
  w[take] <- stats::runif(length(take), 0.1, 1)
  ref2 <- suppressWarnings(as.numeric(centrality(w, measures = "reaching_local",
    mode = "all", directed = TRUE, weighted = TRUE, normalized = FALSE,
    loops = FALSE, invert_weights = FALSE)[[2L]]))
  expect_equal(.cg_reaching_local((w != 0) * 1, w, TRUE, "all"), ref2,
               tolerance = 1e-8)
})

test_that("current-flow betweenness matches NetworkX and the published algorithm", {
  # Cross-language check. The expected values are networkx 3.6.1,
  # current_flow_betweenness_centrality(G, normalized = TRUE), computed on
  # exactly this edge list. Unlike flow_betweenness this measure is a linear
  # solve on the Laplacian pseudoinverse, so it has one answer and IS portable.
  el <- rbind(c(1,2), c(1,4), c(1,6), c(2,3), c(2,5), c(2,6),
              c(3,4), c(3,6), c(4,5), c(5,6))
  n <- 6L
  A <- matrix(0, n, n)
  A[el] <- 1
  A[el[, c(2, 1)]] <- 1

  nx <- c(0.1933333333, 0.2733333333, 0.1933333333,
          0.2333333333, 0.1933333333, 0.2733333333)
  expect_equal(.cg_current_flow_betweenness(A, n, FALSE), nx, tolerance = 1e-9)

  # And agrees with cograph's own igraph-backed route.
  ref <- suppressWarnings(as.numeric(centrality(A,
    measures = "current_flow_betweenness", mode = "all", directed = FALSE,
    weighted = FALSE, normalized = FALSE, loops = FALSE,
    invert_weights = FALSE)[[2L]]))
  expect_equal(.cg_current_flow_betweenness(A, n, FALSE), ref, tolerance = 1e-8)

  # The published pseudocode normalises by n(n-1); NetworkX and cograph use
  # (n-1)(n-2), the count of ordered pairs a node can actually mediate. The
  # unnormalised vectors are identical, so the two differ by exactly that
  # constant ratio -- 1.5 at n = 6.
  expect_equal((n * (n - 1)) / ((n - 1) * (n - 2)), 1.5)
})
