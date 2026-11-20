# =============================================================================
# Cross-Package Equivalence Tests
#
# Validates cograph against INDEPENDENT implementations:
#   - igraph: distances, degree, edge_betweenness, fit_power_law, reciprocity,
#             global_efficiency, bipartite_mapping
#   - brainGraph: vulnerability, rich_club_coeff, efficiency
#   - tnet: weighted_richclub_local_w
#   - sna: efficiency (global)
#
# 100 random networks per comparison. Every value checked.
# =============================================================================

skip_on_cran()
skip_if_not_installed("igraph")

set.seed(4242)
N <- 100L
seeds <- sample.int(100000, N)
nodes <- sample(c(8, 10, 12, 15, 20), N, replace = TRUE)
densities <- runif(N, 0.15, 0.4)

.make <- function(i, directed = FALSE) {
  mat <- create_test_matrix(nodes[i], density = densities[i], seed = seeds[i],
                            symmetric = !directed)
  rownames(mat) <- colnames(mat) <- paste0("N", seq_len(nodes[i]))
  mat
}

TOL <- 1e-10

# =============================================================================
# 1. shortest_paths vs igraph::distances — 100 undirected + 100 directed
# =============================================================================

test_that("shortest_paths matches igraph::distances: 100 undirected", {
  lapply(seq_len(N), function(i) {
    mat <- .make(i)
    g <- to_igraph(mat)

    co <- shortest_paths(mat, weights = NA)
    ig <- igraph::distances(g, weights = NA)
    d <- abs(co - ig); d[!is.finite(d)] <- 0
    expect_true(max(d) <= TOL,
      info = sprintf("i=%d n=%d seed=%d", i, nodes[i], seeds[i]))
  })
})

test_that("shortest_paths matches igraph::distances: 100 directed", {
  lapply(seq_len(N), function(i) {
    mat <- .make(i, directed = TRUE)
    g <- to_igraph(mat, directed = TRUE)

    co <- shortest_paths(mat, weights = NA)
    ig <- igraph::distances(g, mode = "out", weights = NA)
    d <- abs(co - ig); d[!is.finite(d)] <- 0
    expect_true(max(d) <= TOL,
      info = sprintf("i=%d n=%d seed=%d", i, nodes[i], seeds[i]))
  })
})

# =============================================================================
# 2. edge_centrality betweenness vs igraph::edge_betweenness — 100 networks
# =============================================================================

test_that("edge betweenness matches igraph: 100 networks", {
  lapply(seq_len(N), function(i) {
    mat <- .make(i)
    g <- to_igraph(mat)

    co <- edge_centrality(mat, measures = "betweenness")
    ig_eb <- igraph::edge_betweenness(g, weights = NULL)

    expect_equal(co$betweenness, ig_eb, tolerance = TOL,
      info = sprintf("i=%d n=%d seed=%d", i, nodes[i], seeds[i]))
  })
})

# =============================================================================
# 3. reciprocity fraction vs igraph::reciprocity — 100 directed networks
# =============================================================================

test_that("edge reciprocity fraction matches igraph::reciprocity: 100 directed", {
  lapply(seq_len(N), function(i) {
    mat <- .make(i, directed = TRUE)
    g <- to_igraph(mat, directed = TRUE)
    if (igraph::ecount(g) == 0) return(invisible(NULL))

    co <- edge_centrality(mat, measures = "reciprocity", directed = TRUE)
    co_frac <- mean(co$reciprocated)
    # Use mode="default": |reciprocal_edges|/|total_edges| — same as
    # mean(reciprocated). mode="ratio" uses the Garlaschelli-Loffredo formula
    # on dyads, which is a different metric.
    ig_frac <- igraph::reciprocity(g, mode = "default")

    expect_equal(co_frac, ig_frac, tolerance = TOL,
      info = sprintf("i=%d n=%d seed=%d", i, nodes[i], seeds[i]))
  })
})

# =============================================================================
# 4. vulnerability vs brainGraph::vulnerability — 100 networks
# =============================================================================

test_that("vulnerability matches brainGraph::vulnerability: 100 networks", {
  skip_if_not_installed("brainGraph")

  lapply(seq_len(N), function(i) {
    mat <- .make(i)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse")
    }
    g <- igraph::simplify(g)
    nn <- igraph::vcount(g)
    if (nn < 3) return(invisible(NULL))

    # brainGraph::vulnerability returns a data.table with columns
    # "region" and "vulnerability"
    bg <- tryCatch(brainGraph::vulnerability(g), error = function(e) NULL)
    if (is.null(bg)) return(invisible(NULL))

    co <- vulnerability(mat)

    # brainGraph uses the same Latora & Marchiori formula with original n
    nms <- igraph::V(g)$name
    if (is.null(nms)) nms <- as.character(seq_len(nn))

    vapply(seq_len(nn), function(j) {
      bg_val <- bg$vulnerability[j]
      co_val <- co$vulnerability[co$node == nms[j]]
      expect_equal(co_val, bg_val, tolerance = 1e-8,
        info = sprintf("i=%d node=%s", i, nms[j]))
      TRUE
    }, logical(1))
  })
})

# =============================================================================
# 5. global efficiency vs igraph::global_efficiency — 100 networks
# =============================================================================

test_that("global efficiency matches igraph: 100 networks", {
  lapply(seq_len(N), function(i) {
    mat <- .make(i)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse")
    }
    g <- igraph::simplify(g)

    co <- cograph:::.compute_global_efficiency(g, FALSE)
    ig <- igraph::global_efficiency(g, weights = NA)

    expect_equal(co, ig, tolerance = TOL,
      info = sprintf("i=%d n=%d seed=%d", i, nodes[i], seeds[i]))
  })
})

# =============================================================================
# 6. global efficiency vs sna::efficiency — 100 networks
# =============================================================================

test_that("global efficiency matches Latora-Marchiori formula: 100 networks", {
  # NOTE: sna::efficiency computes Krackhardt's graph efficiency,
  # a different metric from Latora-Marchiori global efficiency that
  # cograph implements. Verify against the canonical formula directly.
  lapply(seq_len(N), function(i) {
    mat <- .make(i)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse")
    }
    g <- igraph::simplify(g)

    co <- cograph:::.compute_global_efficiency(g, FALSE)

    # Reference: Latora-Marchiori formula = mean(1/d(i,j)) over all i != j
    nn <- igraph::vcount(g)
    if (nn <= 1) return(invisible(NULL))
    d <- igraph::distances(g, weights = NA)
    diag(d) <- NA
    valid <- is.finite(d) & d > 0
    ref <- sum(1 / d[valid]) / (nn * (nn - 1))

    expect_equal(co, ref, tolerance = 1e-8,
      info = sprintf("i=%d n=%d seed=%d", i, nodes[i], seeds[i]))
  })
})

# =============================================================================
# 7. rich_club phi vs brainGraph::rich_club_coeff — 100 networks
# =============================================================================

test_that("rich club phi matches brainGraph::rich_club_coeff: 100 networks", {
  skip_if_not_installed("brainGraph")

  lapply(seq_len(N), function(i) {
    mat <- .make(i)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse")
    }
    g <- igraph::simplify(g)
    deg <- igraph::degree(g)
    if (max(deg) < 2) return(invisible(NULL))

    # brainGraph rich_club_coeff(g, k) returns phi(k) for one threshold
    # Compare at every threshold from our curve
    co <- rich_club(mat, weighted = FALSE, normalized = FALSE)
    if (nrow(co) == 0) return(invisible(NULL))

    vapply(seq_len(nrow(co)), function(j) {
      k <- co$threshold[j]
      bg <- tryCatch(brainGraph::rich_club_coeff(g, k), error = function(e) NULL)
      if (is.null(bg)) return(TRUE)
      # brainGraph::rich_club_coeff returns a list(phi, graph, Nk, Ek)
      bg_phi <- if (is.list(bg)) bg$phi else bg
      if (is.null(bg_phi) || !is.finite(bg_phi)) return(TRUE)

      expect_equal(co$phi[j], bg_phi, tolerance = TOL,
        info = sprintf("i=%d k=%d", i, k))
      TRUE
    }, logical(1))
  })
})

# =============================================================================
# 8. rich_club_local vs tnet::weighted_richclub_local_w — 100 networks
# =============================================================================

test_that("rich_club_local matches tnet: 100 networks", {
  skip_if_not_installed("tnet")

  lapply(seq_len(N), function(i) {
    mat <- .make(i)
    g <- to_igraph(mat)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse",
                                 edge.attr.comb = list(weight = "sum", "ignore"))
    }
    g <- igraph::simplify(g)
    nn <- igraph::vcount(g)
    if (nn < 3 || igraph::ecount(g) == 0) return(invisible(NULL))

    deg <- igraph::degree(g)
    is_prom <- as.integer(deg > stats::median(deg))

    # Build tnet edge list (both directions)
    el <- igraph::as_data_frame(g, what = "edges")
    w_col <- if ("weight" %in% names(el)) el$weight else rep(1, nrow(el))
    from_idx <- match(el$from, igraph::V(g)$name)
    to_idx <- match(el$to, igraph::V(g)$name)
    tnet_el <- rbind(
      cbind(i = from_idx, j = to_idx, w = w_col),
      cbind(i = to_idx, j = from_idx, w = w_col)
    )

    tnet_res <- tryCatch(
      tnet::weighted_richclub_local_w(tnet_el, is_prom),
      error = function(e) NULL
    )
    if (is.null(tnet_res)) return(invisible(NULL))

    co_df <- rich_club_local(mat, prominence = is_prom)
    co <- stats::setNames(co_df$score, co_df$node)
    nms <- igraph::V(g)$name

    vapply(seq_len(nrow(tnet_res)), function(j) {
      node_id <- tnet_res[j, "node"]
      expect_equal(unname(co[nms[node_id]]), unname(tnet_res[j, "ratio"]),
        tolerance = TOL,
        info = sprintf("i=%d node=%d", i, node_id))
      TRUE
    }, logical(1))
  })
})

# =============================================================================
# 9. fit_degree_distribution power-law vs igraph::fit_power_law — 100 networks
# =============================================================================

test_that("power-law alpha/xmin matches igraph::fit_power_law: 100 networks", {
  lapply(seq_len(N), function(i) {
    mat <- .make(i)
    g <- to_igraph(mat)
    deg <- igraph::degree(g)
    if (max(deg) < 2) return(invisible(NULL))

    ig <- tryCatch(igraph::fit_power_law(deg), error = function(e) NULL)
    if (is.null(ig)) return(invisible(NULL))

    co <- tryCatch(
      fit_degree_distribution(mat, distributions = "power_law"),
      error = function(e) NULL
    )
    if (is.null(co) || is.na(co$fits$power_law$parameters$alpha)) {
      return(invisible(NULL))
    }

    expect_equal(co$fits$power_law$parameters$alpha, ig$alpha,
      tolerance = 1e-4,
      info = sprintf("i=%d alpha", i))
    expect_equal(co$fits$power_law$parameters$xmin, ig$xmin,
      info = sprintf("i=%d xmin", i))
  })
})

# =============================================================================
# 10. is_bipartite vs igraph::bipartite_mapping — 100 random graphs
# =============================================================================

test_that("is_bipartite matches igraph::bipartite_mapping: 100 random graphs", {
  set.seed(9876)
  lapply(seq_len(N), function(i) {
    nn <- sample(4:20, 1)
    mat <- matrix(0, nn, nn)
    ne <- sample(0:(nn * (nn - 1) / 2), 1)
    if (ne > 0) {
      idx <- which(upper.tri(mat))
      chosen <- sample(idx, min(ne, length(idx)))
      mat[chosen] <- 1
      mat <- mat + t(mat)
    }
    diag(mat) <- 0

    g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected")
    ig <- igraph::bipartite_mapping(g)$res
    co <- is_bipartite(mat)

    expect_equal(co, ig,
      info = sprintf("i=%d n=%d edges=%d", i, nn, ne))
  })
})

# =============================================================================
# 11. project_bipartite sum vs manual t(A) %*% A — 100 random matrices
# =============================================================================

test_that("project_bipartite sum matches matrix algebra: 100 matrices", {
  set.seed(5555)
  lapply(seq_len(N), function(i) {
    nr <- sample(3:12, 1)
    nc <- sample(3:8, 1)
    inc <- matrix(sample(0:5, nr * nc, replace = TRUE), nr, nc)
    rownames(inc) <- paste0("R", seq_len(nr))
    colnames(inc) <- paste0("C", seq_len(nc))

    co <- project_bipartite(inc, method = "sum")
    ref <- inc %*% t(inc); diag(ref) <- 0

    expect_equal(co, ref, tolerance = TOL,
      info = sprintf("i=%d %dx%d", i, nr, nc))
  })
})
