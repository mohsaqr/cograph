# ===========================================================================
# Port-adjacent equivalence: structural kernels, group centrality, dispersion
# and the wrangling helpers, native vs their previous igraph implementation
# ===========================================================================
# Every reference here is computed with igraph directly on
# graph_from_adjacency_matrix(), never through to_igraph(): the old
# network_to_igraph() path crashes when the last node is isolated, so the
# "before" side is only available for networks that survive it.

# --- corpus and helpers ------------------------------------------------------

.pa_corpus <- function() {
  nets <- test_networks(tier = c("real_small", "degenerate"))
  nets[nets$n > 0L, , drop = FALSE]
}

.pa_igraph <- function(m) {
  directed <- !isSymmetric(unname(m))
  g <- igraph::graph_from_adjacency_matrix(
    m, mode = if (directed) "directed" else "undirected", weighted = TRUE, diag = TRUE)
  if (!is.null(rownames(m))) igraph::V(g)$name <- rownames(m)
  g
}

# The wrangling verbs still route through to_igraph(); that path fails when
# the last node carries no edge (D1 in docs/network-wrangling-plan.md).
.pa_verb_crashes <- function(m) {
  n <- nrow(m)
  n > 0L && any(m != 0) && all(m[n, ] == 0) && all(m[, n] == 0)
}

.pa_verb_crashes_any <- function(nets) {
  vapply(nets$matrix, .pa_verb_crashes, logical(1))
}

# unlist() of all-empty results is NULL; normalise so identical() is meaningful.
.pa_collect <- function(x) as.character(unlist(x))

# The igraph object the verbs themselves used to see: built from the
# cograph_network edge list (which drops self-loops and re-weights), with
# trailing isolates added back.
.pa_igraph_net <- function(m) {
  net <- as_cograph(m)
  edges <- get_edges(net)
  g <- igraph::graph_from_edgelist(cbind(edges$from, edges$to), directed = isTRUE(net$directed))
  g <- igraph::add_vertices(g, nrow(m) - igraph::vcount(g))
  igraph::E(g)$weight <- edges$weight
  g
}

# HITS is only defined up to the dominant eigenspace of A A^T; when that
# eigenvalue is repeated (every bipartite graph) igraph returns an
# ARPACK-dependent basis vector and no implementation can be pinned to it.
.pa_hits_unique <- function(m) {
  w <- unname(m)
  ev <- sort(eigen(tcrossprod(w), symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
  length(ev) < 2L || ev[1L] <= 0 || (ev[1L] - ev[2L]) > 1e-8 * ev[1L]
}

.pa_muffle_negative <- function(expr) {
  withCallingHandlers(expr, cograph_negative_weights = function(w) {
    invokeRestart("muffleWarning")
  })
}

# Reference implementations: the pre-port bodies of group_centrality() and
# dispersion(), kept verbatim in logic so the port is pinned to them.
.pa_ref_group_betweenness <- function(g, C, normalized = TRUE) {
  n <- igraph::vcount(g)
  V_minus_C <- setdiff(seq_len(n), C)
  if (length(V_minus_C) < 2L) return(0)
  pairs <- expand.grid(s = V_minus_C, t = V_minus_C)
  pairs <- pairs[pairs$s != pairs$t, , drop = FALSE]
  total <- sum(vapply(seq_len(nrow(pairs)), function(i) {
    paths <- igraph::all_shortest_paths(g, from = pairs$s[i], to = pairs$t[i],
                                        weights = NA)$res
    if (length(paths) == 0L) return(0)
    through <- vapply(paths, function(p) {
      pv <- as.integer(p)
      length(pv) > 2L && any(pv[-c(1L, length(pv))] %in% C)
    }, logical(1))
    sum(through) / length(paths)
  }, numeric(1)))
  if (normalized) {
    k <- length(V_minus_C)
    total <- total / (k * (k - 1L))
  }
  total
}

.pa_ref_group_closeness <- function(g, C) {
  n <- igraph::vcount(g)
  V_minus_C <- setdiff(seq_len(n), C)
  if (length(V_minus_C) == 0L) return(0)
  D <- igraph::distances(g, v = V_minus_C, to = C, mode = "out", weights = NA)
  d_vec <- apply(D, 1, min)
  s <- sum(d_vec[is.finite(d_vec)])
  if (s == 0) return(0)
  length(V_minus_C) / s
}

.pa_ref_group_degree <- function(g, C, mode = "all") {
  n <- igraph::vcount(g)
  if (!igraph::is_directed(g)) mode <- "all"
  nbrs <- unlist(lapply(C, function(c) as.integer(igraph::neighbors(g, c, mode = mode))))
  k <- n - length(C)
  if (k == 0L) return(0)
  length(setdiff(unique(nbrs), C)) / k
}

.pa_ref_dispersion <- function(g, u = NULL, v = NULL) {
  n <- igraph::vcount(g)
  nbrs_of <- function(node) as.integer(igraph::neighbors(g, node, mode = "out"))
  disp_pair <- function(u_i, v_i) {
    u_nbrs <- nbrs_of(u_i)
    ST <- intersect(nbrs_of(v_i), u_nbrs)
    set_uv <- c(u_i, v_i)
    total <- 0L
    if (length(ST) >= 2L) {
      pairs <- utils::combn(ST, 2L)
      total <- sum(vapply(seq_len(ncol(pairs)), function(p) {
        s <- pairs[1L, p]; t <- pairs[2L, p]
        nbrs_s <- setdiff(intersect(u_nbrs, nbrs_of(s)), set_uv)
        !(t %in% nbrs_s) && length(intersect(nbrs_s, nbrs_of(t))) == 0L
      }, logical(1)))
    }
    emb <- length(ST)
    if (emb != 0) total / emb else total
  }
  if (!is.null(u) && !is.null(v)) return(disp_pair(u, v))
  if (!is.null(u)) {
    u_nbrs <- nbrs_of(u)
    return(stats::setNames(vapply(u_nbrs, function(v_i) disp_pair(u, v_i), numeric(1)),
                           as.character(u_nbrs)))
  }
  rows <- lapply(seq_len(n), function(uu) {
    vv <- nbrs_of(uu)
    data.frame(from = rep(uu, length(vv)), to = vv,
               dispersion = vapply(vv, function(v_i) disp_pair(uu, v_i), numeric(1)))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# --- structural kernels vs igraph -------------------------------------------

test_that("graph zoo: articulation points, bridges and ego masks match igraph", {
  skip_if_not_installed("igraph")

  build <- function(n, density, directed, seed, loop) {
    set.seed(seed)
    m <- matrix(0, n, n)
    if (n > 1L && density > 0) {
      idx <- which(row(m) != col(m))
      take <- idx[stats::runif(length(idx)) < density]
      m[take] <- stats::runif(length(take), 0.1, 1)
      if (!directed) m[lower.tri(m)] <- t(m)[lower.tri(m)]
    }
    if (loop && n > 0L) m[1L, 1L] <- 1
    m
  }
  grid <- expand.grid(
    n = c(1L, 2L, 3L, 5L, 8L, 12L, 20L),
    density = c(0, 0.1, 0.25, 0.7, 1),
    directed = c(FALSE, TRUE),
    loop = c(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  mismatches <- .pa_collect(lapply(seq_len(nrow(grid)), function(i) {
    cfg <- grid[i, ]
    m <- build(cfg$n, cfg$density, cfg$directed, 2000L + i, cfg$loop)
    g <- igraph::graph_from_adjacency_matrix(
      m, mode = if (cfg$directed) "directed" else "undirected", weighted = TRUE, diag = TRUE)
    el <- igraph::as_edgelist(g, names = FALSE)
    tag <- sprintf("n=%d dens=%.2f %s loop=%s", cfg$n, cfg$density,
                   if (cfg$directed) "dir" else "undir", cfg$loop)
    out <- character(0)
    if (!identical(sort(as.integer(igraph::articulation_points(g))),
                   .cg_articulation_points(m))) out <- c(out, paste(tag, "articulation"))
    if (!identical(seq_len(nrow(el)) %in% as.integer(igraph::bridges(g)),
                   .cg_bridges(m, el))) out <- c(out, paste(tag, "bridges"))
    seeds <- seq_len(min(cfg$n, 2L))
    for (ord in 0:3) for (md in c("all", "out", "in")) {
      ref <- sort(unique(unlist(lapply(
        igraph::ego(g, order = ord, nodes = seeds, mode = md), as.integer))))
      got <- which(.cg_ego_mask(m, seeds, ord, md))
      if (!identical(ref, got)) out <- c(out, paste(tag, "ego", ord, md))
    }
    out
  }))
  expect_identical(mismatches, character(0))
})

test_that("corpus: bridges are never parallel edges, articulation matches igraph", {
  skip_if_not_installed("igraph")
  nets <- .pa_corpus()
  bad <- .pa_collect(lapply(seq_len(nrow(nets)), function(i) {
    m <- nets$matrix[[i]]
    g <- .pa_igraph(m)
    el <- igraph::as_edgelist(g, names = FALSE)
    out <- character(0)
    if (!identical(sort(as.integer(igraph::articulation_points(g))),
                   .cg_articulation_points(m))) out <- c(out, paste(nets$name[i], "articulation"))
    if (!identical(seq_len(nrow(el)) %in% as.integer(igraph::bridges(g)),
                   .cg_bridges(m, el))) out <- c(out, paste(nets$name[i], "bridges"))
    out
  }))
  expect_identical(bad, character(0))
})

test_that("edge betweenness matrix matches igraph::edge_betweenness on the corpus", {
  skip_if_not_installed("igraph")
  nets <- .pa_corpus()
  nets <- nets[!nets$signed, , drop = FALSE]
  bad <- .pa_collect(lapply(seq_len(nrow(nets)), function(i) {
    m <- nets$matrix[[i]]
    g <- .pa_igraph(m)
    el <- igraph::as_edgelist(g, names = FALSE)
    if (nrow(el) == 0L) return(character(0))
    # igraph's tie handling is scale-dependent on weights of order 1e-9
    # (the same graph scaled by 1e9 matches exactly); see LEARNINGS.md.
    if (identical(nets$name[i], "weights_1e-9")) return(character(0))
    ref <- igraph::edge_betweenness(g, directed = igraph::is_directed(g))
    got <- .cg_edge_betweenness_structure(unname(m), nrow(m), igraph::is_directed(g))[el]
    if (isTRUE(all.equal(ref, got, tolerance = 1e-10))) character(0) else nets$name[i]
  }))
  expect_identical(bad, character(0))
})

test_that("geodesic counts agree with igraph::all_shortest_paths", {
  skip_if_not_installed("igraph")
  m <- test_network("kite")
  g <- .pa_igraph(m)
  a <- unname(m); diag(a) <- 0
  d <- .cg_distances(a, "out")
  sigma <- .cg_geodesic_counts(a, d)
  n <- nrow(m)
  ref <- outer(seq_len(n), seq_len(n), Vectorize(function(s, t) {
    if (s == t) return(1)
    length(igraph::all_shortest_paths(g, from = s, to = t, weights = NA)$res)
  }))
  expect_equal(unname(sigma), ref)
})

test_that("structural kernels: closed-form invariants", {
  # Path graph: every inner vertex is a cut vertex, every edge a bridge.
  n <- 7L
  p <- matrix(0, n, n)
  idx <- cbind(seq_len(n - 1L), seq_len(n - 1L) + 1L)
  p[idx] <- 1; p[idx[, 2:1]] <- 1
  expect_identical(.cg_articulation_points(p), 2:6)
  expect_true(all(.cg_bridges(p, idx)))
  # Ring: no cut vertex, no bridge; any node reaches all within n %/% 2 hops.
  r <- p; r[1L, n] <- r[n, 1L] <- 1
  expect_identical(.cg_articulation_points(r), integer(0))
  expect_false(any(.cg_bridges(r, rbind(idx, c(1L, n)))))
  expect_true(all(.cg_ego_mask(r, 1L, n %/% 2, "all")))
  expect_identical(which(.cg_ego_mask(r, 1L, 0L, "all")), 1L)
  # Permutation invariance of the articulation set.
  set.seed(3)
  perm <- sample(n)
  q <- p[perm, perm]
  expect_identical(sort(perm[.cg_articulation_points(q)]), 2:6)
  # Empty graph.
  expect_identical(.cg_articulation_points(matrix(0, 0L, 0L)), integer(0))
  expect_identical(.cg_bridges(matrix(0, 3L, 3L), matrix(integer(0), 0L, 2L)), logical(0))
})

test_that("edge betweenness kernel refuses negative weights by class", {
  m <- matrix(c(0, -1, 1, -1, 0, 1, 1, 1, 0), 3L, 3L)
  expect_error(.cg_edge_betweenness_structure(m, 3L, FALSE), class = "cograph_negative_weights")
})

# --- group centrality and dispersion vs their previous implementation -------

test_that("group_centrality reproduces the igraph implementation on the corpus", {
  skip_if_not_installed("igraph")
  nets <- .pa_corpus()
  nets <- nets[!nets$signed, , drop = FALSE]
  bad <- .pa_collect(lapply(seq_len(nrow(nets)), function(i) {
    m <- nets$matrix[[i]]
    n <- nrow(m)
    g <- .pa_igraph(m)
    set.seed(1)
    C <- sort(sample(n, min(n, max(1L, n %/% 4L))))
    out <- character(0)
    # The reference betweenness is O(n^2) igraph calls; cap it at 40 nodes.
    if (n <= 40L && !isTRUE(all.equal(.pa_ref_group_betweenness(g, C),
                                       group_centrality(m, nodes = C, measure = "betweenness")))) {
      out <- c(out, paste(nets$name[i], "betweenness"))
    }
    if (!isTRUE(all.equal(.pa_ref_group_closeness(g, C),
                          group_centrality(m, nodes = C, measure = "closeness")))) {
      out <- c(out, paste(nets$name[i], "closeness"))
    }
    for (md in c("all", "out", "in")) {
      if (!isTRUE(all.equal(.pa_ref_group_degree(g, C, md),
                            group_centrality(m, nodes = C, measure = "degree", mode = md)))) {
        out <- c(out, paste(nets$name[i], "degree", md))
      }
    }
    out
  }))
  expect_identical(bad, character(0))
})

test_that("group_centrality: hand-computed values and error paths", {
  # Path 1-2-3-4-5: {3} separates 1,2 from 4,5 -> 8 ordered pairs of 12.
  n <- 5L
  p <- matrix(0, n, n)
  idx <- cbind(seq_len(n - 1L), seq_len(n - 1L) + 1L)
  p[idx] <- 1; p[idx[, 2:1]] <- 1
  expect_equal(group_centrality(p, nodes = 3, measure = "betweenness"), 8 / 12)
  expect_equal(group_centrality(p, nodes = 3, measure = "betweenness", normalized = FALSE), 8)
  # closeness: distances to node 3 are 2,1,1,2 -> 4 / 6
  expect_equal(group_centrality(p, nodes = 3, measure = "closeness"), 4 / 6)
  # degree: neighbours of {3} outside the group = {2, 4} of 4 others
  expect_equal(group_centrality(p, nodes = 3, measure = "degree"), 0.5)
  expect_error(group_centrality(p, nodes = "a"), "node names not available")
  rownames(p) <- colnames(p) <- letters[1:5]
  expect_error(group_centrality(p, nodes = "z"), "unknown nodes")
  expect_error(group_centrality(p, nodes = 9), "out of range")
})

test_that("dispersion reproduces the igraph implementation, including loops", {
  skip_if_not_installed("igraph")
  nets <- .pa_corpus()
  nets <- nets[!nets$signed & nets$n <= 40L, , drop = FALSE]
  bad <- .pa_collect(lapply(seq_len(nrow(nets)), function(i) {
    m <- nets$matrix[[i]]
    g <- .pa_igraph(m)
    out <- character(0)
    if (!isTRUE(all.equal(.pa_ref_dispersion(g), dispersion(m)))) {
      out <- c(out, paste(nets$name[i], "all"))
    }
    if (!isTRUE(all.equal(.pa_ref_dispersion(g, u = 1L), dispersion(m, u = 1)))) {
      out <- c(out, paste(nets$name[i], "u"))
    }
    out
  }))
  expect_identical(bad, character(0))
})

# --- estrada_index / trophic_incoherence ------------------------------------

test_that("estrada_index equals the sum of exp(eigenvalues) of the binary adjacency", {
  m <- test_network("karate")
  a <- (unname(m) != 0) * 1
  expect_equal(estrada_index(m), sum(exp(eigen(a, symmetric = TRUE, only.values = TRUE)$values)))
  expect_identical(estrada_index(matrix(0, 0L, 0L)), 0)
})

test_that("trophic_incoherence: coherent chain is 0, undirected warns NA, cycle is NA", {
  chain <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3L, 3L, byrow = TRUE)
  expect_equal(trophic_incoherence(chain), 0)
  expect_warning(val <- trophic_incoherence(chain + t(chain)), "directed")
  expect_identical(val, NA_real_)
  cyc <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3L, 3L, byrow = TRUE)
  expect_identical(trophic_incoherence(cyc), NA_real_)
  # A single self-loop is the only edge: cannibalism = FALSE leaves nothing.
  loop <- matrix(c(1, 1, 0, 0), 2L, 2L, byrow = TRUE)
  expect_identical(trophic_incoherence(loop, cannibalism = FALSE), 0)
  expect_identical(trophic_incoherence(matrix(c(1, 0, 0, 0), 2L, 2L), cannibalism = FALSE), NA_real_)
})

# --- wrangling helpers: eager and lazy centralities vs igraph ---------------

test_that("filter_nodes' eager centralities match igraph on every corpus network", {
  skip_if_not_installed("igraph")
  nets <- .pa_corpus()
  nets <- nets[!nets$signed, , drop = FALSE]
  bad <- .pa_collect(lapply(seq_len(nrow(nets)), function(i) {
    m <- nets$matrix[[i]]
    g <- .pa_igraph(m)
    w <- igraph::E(g)$weight
    dir <- igraph::is_directed(g)
    ref <- list(
      degree = igraph::degree(g, mode = "all"),
      indegree = igraph::degree(g, mode = "in"),
      outdegree = igraph::degree(g, mode = "out"),
      strength = igraph::strength(g, mode = "all", weights = w),
      instrength = igraph::strength(g, mode = "in", weights = w),
      outstrength = igraph::strength(g, mode = "out", weights = w),
      betweenness = igraph::betweenness(g, weights = w, directed = dir),
      closeness = igraph::closeness(g, mode = "all", weights = w),
      eigenvector = igraph::eigen_centrality(g, weights = w, directed = dir)$vector,
      pagerank = igraph::page_rank(g, weights = w, directed = dir)$vector,
      hub = igraph::hits_scores(g, weights = w)$hub,
      authority = igraph::hits_scores(g, weights = w)$authority
    )
    got <- .compute_centrality_vars(.cg_graph(m))
    if (!.pa_hits_unique(m)) ref <- ref[setdiff(names(ref), c("hub", "authority"))]
    # .cg_betweenness() / .cg_closeness() (kernels-path.R, not part of this
    # port) resolve path-length ties with an absolute 1e-15 epsilon; igraph's
    # tie rule differs at the 1e-9 weight scale, so those two are excluded on
    # this one network until the path kernels adopt igraph's rule.
    if (identical(nets$name[i], "weights_1e-9")) ref <- ref[setdiff(names(ref), c("betweenness", "closeness"))]
    unlist(lapply(names(ref), function(k) {
      r <- unname(ref[[k]]); v <- unname(got[[k]])
      if (isTRUE(all.equal(r, v, tolerance = 1e-8))) character(0) else paste(nets$name[i], k)
    }))
  }))
  expect_identical(bad, character(0))
})

test_that("lazy context variables match igraph components, coreness, cut vertices, bridges", {
  skip_if_not_installed("igraph")
  nets <- .pa_corpus()
  needed <- c("component", "component_size", "is_largest_component",
              "neighborhood_size", "k_core", "is_articulation", "is_bridge_endpoint")
  bad <- .pa_collect(lapply(seq_len(nrow(nets)), function(i) {
    m <- nets$matrix[[i]]
    g <- .pa_igraph(m)
    comp <- igraph::components(g)
    br <- igraph::bridges(g)
    ends <- if (length(br)) unique(as.vector(igraph::ends(g, br, names = FALSE))) else integer(0)
    n <- nrow(m)
    ref <- list(
      component = comp$membership,
      component_size = comp$csize[comp$membership],
      is_largest_component = comp$membership == which.max(comp$csize),
      neighborhood_size = igraph::degree(g, mode = "all"),
      k_core = igraph::coreness(g, mode = "all"),
      is_articulation = seq_len(n) %in% as.integer(igraph::articulation_points(g)),
      is_bridge_endpoint = seq_len(n) %in% ends
    )
    # The verbs hand the helpers the loopless igraph built from the
    # cograph_network; the reference is built the same way.
    g <- .pa_igraph_net(m)
    comp <- igraph::components(g)
    br <- igraph::bridges(g)
    ends <- if (length(br)) unique(as.vector(igraph::ends(g, br, names = FALSE))) else integer(0)
    ref <- list(
      component = comp$membership,
      component_size = comp$csize[comp$membership],
      is_largest_component = comp$membership == which.max(comp$csize),
      neighborhood_size = igraph::degree(g, mode = "all"),
      k_core = igraph::coreness(g, mode = "all"),
      is_articulation = seq_len(n) %in% as.integer(igraph::articulation_points(g)),
      is_bridge_endpoint = seq_len(n) %in% ends
    )
    got <- .compute_lazy_context(g, needed)
    unlist(lapply(needed, function(k) {
      if (isTRUE(all.equal(unname(ref[[k]]), unname(got[[k]])))) character(0) else paste(nets$name[i], k)
    }))
  }))
  expect_identical(bad, character(0))
})

test_that("lazy edge metrics match igraph in the caller's edge order", {
  skip_if_not_installed("igraph")
  nets <- .pa_corpus()
  nets <- nets[!nets$signed & !.pa_verb_crashes_any(nets), , drop = FALSE]
  bad <- .pa_collect(lapply(seq_len(nrow(nets)), function(i) {
    m <- nets$matrix[[i]]
    net <- as_cograph(m)
    edges <- get_edges(net)
    if (nrow(edges) == 0L) return(character(0))
    g <- .pa_igraph(m)
    # Rebuild the igraph in the cograph edge order so ids line up.
    g_rows <- igraph::graph_from_edgelist(cbind(edges$from, edges$to), directed = igraph::is_directed(g))
    g_rows <- igraph::add_vertices(g_rows, nrow(m) - igraph::vcount(g_rows))
    igraph::E(g_rows)$weight <- edges$weight
    dir <- igraph::is_directed(g_rows)
    deg <- igraph::degree(g_rows, mode = "all")
    str <- igraph::strength(g_rows, mode = "all")
    ref <- list(
      from_degree = deg[edges$from], to_degree = deg[edges$to],
      from_strength = str[edges$from], to_strength = str[edges$to],
      edge_betweenness = igraph::edge_betweenness(g_rows, directed = dir),
      is_bridge = seq_len(nrow(edges)) %in% as.integer(igraph::bridges(g_rows)),
      is_mutual = if (!dir) rep(TRUE, nrow(edges)) else
        vapply(seq_len(nrow(edges)), function(e) any(edges$from == edges$to[e] & edges$to == edges$from[e]), logical(1))
    )
    if (identical(nets$name[i], "weights_1e-9")) ref$edge_betweenness <- NULL  # see the eager test
    got <- .compute_lazy_edge_metrics(g_rows, edges, get_nodes(net), names(ref), "louvain")
    unlist(lapply(names(ref), function(k) {
      if (isTRUE(all.equal(unname(ref[[k]]), unname(got[[k]]), tolerance = 1e-8))) character(0) else paste(nets$name[i], k)
    }))
  }))
  expect_identical(bad, character(0))
})

# --- wrangling verbs end to end ---------------------------------------------

test_that("filter_nodes / select_nodes / select_edges / select_component agree with igraph references", {
  skip_if_not_installed("igraph")
  nets <- .pa_corpus()
  # The before-side (and the verbs' own to_igraph() call) needs a graph whose
  # last node carries an edge; the negative-weight networks are covered by
  # the classed-warning test below.
  nets <- nets[!nets$signed & !.pa_verb_crashes_any(nets), , drop = FALSE]
  bad <- .pa_collect(lapply(seq_len(nrow(nets)), function(i) {
    m <- nets$matrix[[i]]
    g <- .pa_igraph_net(m)
    labels <- as.character(get_nodes(as_cograph(m))$label)
    out <- character(0)

    # filter_nodes(degree >= 2)
    ref_deg <- labels[igraph::degree(g, mode = "all") >= 2]
    got_deg <- if (length(ref_deg)) as.character(filter_nodes(m, degree >= 2)$nodes$label) else {
      expect_warning(r <- filter_nodes(m, degree >= 2), "No nodes match")
      as.character(r$nodes$label)
    }
    if (!identical(sort(ref_deg), sort(got_deg))) out <- c(out, paste(nets$name[i], "filter_nodes"))

    # select_nodes(top = 3, by = "pagerank"); skip label check on a boundary tie
    pr <- igraph::page_rank(g, directed = igraph::is_directed(g))$vector
    ord <- order(pr, decreasing = TRUE)
    top <- min(3L, nrow(m))
    tie <- nrow(m) > top && abs(pr[ord[top]] - pr[ord[top + 1L]]) < 1e-8
    got_pr <- as.character(select_nodes(m, top = 3, by = "pagerank")$nodes$label)
    if (!tie && !identical(sort(labels[ord[seq_len(top)]]), sort(got_pr))) {
      out <- c(out, paste(nets$name[i], "select_nodes"))
    }

    # select_edges(is_bridge)
    el <- igraph::as_edgelist(g, names = FALSE)
    br <- el[as.integer(igraph::bridges(g)), , drop = FALSE]
    ref_br <- sort(paste(labels[pmin(br[, 1L], br[, 2L])], labels[pmax(br[, 1L], br[, 2L])]))
    res <- withCallingHandlers(select_edges(m, is_bridge),
      warning = function(w) if (grepl("No edges match", conditionMessage(w))) invokeRestart("muffleWarning"))
    got <- res$edges
    res_labels <- as.character(res$nodes$label)
    got_br <- if (nrow(got)) sort(paste(res_labels[pmin(got$from, got$to)], res_labels[pmax(got$from, got$to)])) else character(0)
    if (!identical(ref_br, got_br)) out <- c(out, paste(nets$name[i], "select_edges"))

    # select_component(): largest weak component
    comp <- igraph::components(g)
    ref_cmp <- sort(labels[comp$membership == which.max(comp$csize)])
    got_cmp <- sort(as.character(select_component(m)$nodes$label))
    if (!identical(ref_cmp, got_cmp)) out <- c(out, paste(nets$name[i], "select_component"))
    out
  }))
  expect_identical(bad, character(0))
})

test_that("path-based filters on negative weights give NA with a classed warning, not an error", {
  skip_if_not_installed("igraph")  # the verbs' own entry still calls to_igraph() (wrangling plan)
  m <- test_network("negative_weights_all")
  expect_warning(select_nodes(m, top = 2, by = "betweenness"), class = "cograph_negative_weights")
  expect_warning(select_nodes(m, top = 2, by = "closeness"), class = "cograph_negative_weights")
  expect_warning(select_nodes(m, top = 2, by = "pagerank"), class = "cograph_negative_weights")
  expect_warning(filter_nodes(m, degree >= 1), class = "cograph_negative_weights")
  vars <- .pa_muffle_negative(.compute_centrality_vars(.cg_graph(m)))
  expect_true(all(is.na(vars$betweenness)))
  expect_true(all(is.na(vars$closeness)))
  expect_true(all(is.na(vars$pagerank)))
  expect_false(anyNA(vars$degree))
  expect_false(anyNA(vars$strength))
  expect_warning(select_edges(m, edge_betweenness > 0), class = "cograph_negative_weights")
})

test_that("the ported verbs never reach igraph through the native path", {
  skip_if_not_installed("igraph")  # the verbs' own entry still calls to_igraph() (wrangling plan)
  m <- test_network("karate")
  withr::local_options(cograph.forbid_igraph = TRUE)
  expect_no_error(group_centrality(m, nodes = c("Mr Hi", "John A"), measure = "betweenness"))
  expect_no_error(group_centrality(m, nodes = 1:3, measure = "closeness"))
  expect_no_error(group_centrality(m, nodes = 1:3, measure = "degree"))
  expect_no_error(estrada_index(m))
  expect_no_error(trophic_incoherence(test_network("dag_10")))
  expect_no_error(dispersion(m, u = 1, v = 34))
  expect_no_error(filter_nodes(m, betweenness > 0))
  expect_no_error(select_nodes(m, top = 3, by = "pagerank"))
  expect_no_error(select_nodes(m, is_articulation | k_core >= 3))
  expect_no_error(select_edges(m, is_bridge))
  expect_no_error(select_edges(m, edge_betweenness > 10 & is_mutual))
  expect_no_error(select_component(m))
  expect_no_error(select_neighbors(m, of = "Mr Hi", order = 2))
})

test_that("helpers accept a context or an igraph object alike", {
  skip_if_not_installed("igraph")
  m <- test_network("kite")
  cg <- .cg_graph(m)
  g <- .pa_igraph(m)
  expect_equal(.compute_single_centrality(cg, "coreness"), .compute_single_centrality(g, "coreness"))
  expect_equal(.compute_centrality_vars(cg), .compute_centrality_vars(g))
  nodes <- get_nodes(as_cograph(m))
  expect_identical(.select_by_component(cg, nodes, "largest"), .select_by_component(g, nodes, "largest"))
  expect_identical(.select_by_neighbors(cg, nodes, 1L, 1L), .select_by_neighbors(g, nodes, 1L, 1L))
  expect_identical(.cg_edge_rows(g), igraph::as_edgelist(g, names = FALSE) |> (\(e) matrix(as.integer(e), ncol = 2L))())
})
