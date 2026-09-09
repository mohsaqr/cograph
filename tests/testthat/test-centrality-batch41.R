# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("relative entropy is the normalised geometric mean on stars", {
  # Leaves carry no shortest path, so equation (4) betweenness is zero and
  # the geometric mean annihilates them: the centre takes the whole share.
  for (q in c(2, 3, 5, 9)) {
    star <- igraph::make_star(q + 1, mode = "undirected")
    expect_equal(unname(centrality_relative_entropy(star)),
                 c(1, numeric(q)))
    # Degree and closeness alone keep every node in play. Degree gives the
    # centre 1/2 against 1/(2q) per leaf; equation (3) closeness gives it
    # 1/q against 1/(2q-1), so the ratio of geometric means is sqrt(2q-1).
    leaf <- 1 / (q + sqrt(2 * q - 1))
    expect_equal(unname(centrality_relative_entropy(
      star, re_indexes = c("degree", "closeness")
    )), c(sqrt(2 * q - 1) * leaf, rep(leaf, q)))
  }
})

test_that("relative entropy is uniform on vertex-transitive graphs", {
  for (n in 4:8) {
    expect_equal(unname(centrality_relative_entropy(igraph::make_ring(n))),
                 rep(1 / n, n))
  }
  # Every index is constant, so equations (8) and (9) both give the uniform
  # distribution whatever direction each index is declared to have.
  expect_equal(unname(centrality_relative_entropy(
    igraph::make_ring(6),
    re_indexes = c("degree", "n_components", "largest_component"),
    re_negative = c("degree", "n_components")
  )), rep(1 / 6, 6))
})

test_that("a single index reproduces its own distribution on a path", {
  path <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  # Equation (8) on the degree sequence 1, 2, 2, 2, 1.
  expect_equal(unname(centrality_relative_entropy(path,
                                                  re_indexes = "degree")),
               c(1, 2, 2, 2, 1) / 8)
  # Equation (6) by hand: 5/4, 9/16, 5/8, 9/16, 5/4, summing to 17/4. As a
  # negative index equation (9) divides the complements by |V| - 1 = 4.
  expect_equal(unname(centrality_relative_entropy(path,
                                                  re_indexes = "constraint")),
               c(3 / 17, 59 / 272, 29 / 136, 59 / 272, 3 / 17))
  # Declaring the same index positive inverts the reading.
  expect_equal(unname(centrality_relative_entropy(
    path, re_indexes = "constraint", re_negative = character()
  )), c(5 / 17, 9 / 68, 5 / 34, 9 / 68, 5 / 17))
})

test_that("relative entropy reproduces the published Kite tables", {
  edges <- c(9, 10, 9, 8, 9, 4, 9, 7, 10, 6, 10, 5, 10, 7, 8, 7, 8, 4,
             6, 7, 6, 5, 7, 4, 7, 5, 4, 5, 4, 3, 5, 3, 3, 2, 2, 1)
  kite <- igraph::make_graph(edges, directed = FALSE)
  a <- as.matrix(igraph::as_adjacency_matrix(kite))
  ctx <- .cg_re_context(a)
  # Chen, Wang and Luo (2016) Table 1 and Table 3, at the printed precision.
  expect_equal(.cg_re_index(ctx, "degree"), c(1, 2, 3, 5, 5, 3, 6, 3, 4, 4))
  expect_equal(round(.cg_re_index(ctx, "closeness"), 5),
               c(0.03448, 0.04762, 0.06667, 0.07143, 0.07143, 0.05556,
                 0.06667, 0.05556, 0.05882, 0.05882))
  expect_equal(round(.cg_re_index(ctx, "betweenness"), 2),
               c(0, 16, 28, 16.67, 16.67, 0, 7.33, 0, 1.67, 1.67))
  expect_equal(round(.cg_re_index(ctx, "constraint"), 4),
               c(1.25, 0.5556, 0.4944, 0.4701, 0.4701, 0.7059, 0.4746,
                 0.7059, 0.5783, 0.5783))
  expect_equal(.cg_re_index(ctx, "n_components"),
               c(1, 2, 2, 1, 1, 1, 1, 1, 1, 1))
  expect_equal(.cg_re_index(ctx, "largest_component"),
               c(9, 8, 7, 9, 9, 9, 9, 9, 9, 9))
  # Table 2, in the three printed columns. The paper integrated its own
  # rounded Table 1 and Table 3 columns, so the exact graph agrees only to
  # about 4e-5; recomputing from the printed columns closes the gap.
  printed <- list(
    c(0, 0.127939, 0.177613, 0.180540, 0.180540, 0, 0.151213, 0,
      0.091078, 0.091078),
    c(0.092119, 0.131109, 0.131936, rep(0.092119, 7)),
    c(0, 0.145025, 0.180857, 0.162205, 0.162205, 0, 0.144126, 0,
      0.102791, 0.102791)
  )
  sets <- list(c("degree", "closeness", "betweenness", "constraint"),
               c("n_components", "largest_component"), .cg_re_vocabulary())
  rounded <- list(
    degree = c(1, 2, 3, 5, 5, 3, 6, 3, 4, 4),
    closeness = c(0.03448, 0.04762, 0.06667, 0.07143, 0.07143, 0.05556,
                  0.06667, 0.05556, 0.05882, 0.05882),
    betweenness = c(0, 16, 28, 16.67, 16.67, 0, 7.33, 0, 1.67, 1.67),
    constraint = c(1.25, 0.5556, 0.4944, 0.4701, 0.4701, 0.7059, 0.4746,
                   0.7059, 0.5783, 0.5783),
    n_components = c(1, 2, 2, 1, 1, 1, 1, 1, 1, 1),
    largest_component = c(9, 8, 7, 9, 9, 9, 9, 9, 9, 9)
  )
  for (k in seq_along(sets)) {
    exact <- unname(centrality_relative_entropy(kite,
                                                re_indexes = sets[[k]]))
    expect_lt(max(abs(exact - printed[[k]])), 4e-5)
    logs <- vapply(sets[[k]], function(nm) {
      .cg_re_distribution(rounded[[nm]], nm,
                          nm %in% .cg_re_negative_default())
    }, numeric(10))
    from_printed <- exp(rowMeans(matrix(logs, nrow = 10)))
    expect_lt(max(abs(from_printed / sum(from_printed) - printed[[k]])), 5e-7)
  }
})

test_that("closeness sums over reachable partners on disconnected graphs", {
  # Equation (3) sums over all of V and is infinite here, which would leave
  # the index identically zero. The cograph extension sums the reachable
  # partners: 1/3 at an end, 1/2 at a centre of each three-node path.
  two_paths <- igraph::disjoint_union(
    igraph::make_graph(c(1, 2, 2, 3), directed = FALSE),
    igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
  )
  end <- 1 / (4 + 2 * sqrt(3))
  expect_equal(unname(centrality_relative_entropy(
    two_paths, re_indexes = c("degree", "closeness")
  )), c(end, sqrt(3) * end, end, end, sqrt(3) * end, end))
  # An isolate reaches nobody and invests nowhere, so cograph gives it
  # closeness zero and constraint zero; its degree of zero already zeroes it.
  with_isolate <- igraph::disjoint_union(
    igraph::make_ring(4), igraph::make_empty_graph(1, directed = FALSE)
  )
  ctx <- .cg_re_context(as.matrix(igraph::as_adjacency_matrix(with_isolate)))
  expect_equal(.cg_re_index(ctx, "closeness")[5], 0)
  expect_equal(.cg_re_index(ctx, "constraint")[5], 0)
  expect_equal(unname(centrality_relative_entropy(with_isolate)),
               c(rep(0.25, 4), 0))
})

test_that("relative entropy refuses the cases the maps leave undefined", {
  # Equation (4) betweenness is identically zero on a complete graph, so
  # equation (8) divides by zero. Never a silent vector of zeros.
  for (n in 2:5) {
    expect_error(centrality_relative_entropy(igraph::make_full_graph(n)),
                 class = "cograph_undefined_index")
  }
  expect_error(centrality_relative_entropy(igraph::make_full_graph(4)),
               "betweenness")
  # Degree is identically zero without edges.
  expect_error(centrality_relative_entropy(
    igraph::make_empty_graph(3, directed = FALSE), re_indexes = "degree"
  ), class = "cograph_undefined_index")
  # Equation (9) divides by |V| - 1, which vanishes on a single node.
  expect_error(centrality_relative_entropy(
    igraph::make_full_graph(1), re_indexes = "largest_component"
  ), class = "cograph_undefined_index")
  # An empty graph has no scores rather than an undefined one.
  expect_equal(centrality_relative_entropy(igraph::make_empty_graph(0)),
               stats::setNames(numeric(), character()))
})

test_that("relative entropy validates its parameters", {
  ring <- igraph::make_ring(6)
  for (bad in list("nope", c("degree", "nope"), character(),
                   c("degree", "degree"), 1:2, NA_character_, NULL)) {
    expect_error(centrality_relative_entropy(ring, re_indexes = bad),
                 "re_indexes")
  }
  expect_error(centrality_relative_entropy(ring, re_indexes = "nope"),
               class = "cograph_unknown_measure")
  for (bad in list("closeness", 5, NA_character_)) {
    expect_error(centrality_relative_entropy(
      ring, re_indexes = c("degree", "betweenness"), re_negative = bad
    ), "re_negative")
  }
  # Validation happens before any graph work, so an empty graph still errors.
  expect_error(centrality_relative_entropy(igraph::make_empty_graph(0),
                                           re_indexes = "nope"),
               "re_indexes")
})

test_that("relative entropy input projections and contracts are explicit", {
  g <- igraph::make_graph(c(1, 2, 2, 1, 2, 3, 2, 3, 3, 3, 3, 4, 4, 1),
                          directed = TRUE)
  igraph::E(g)$weight <- c(2, 8, 3, 9, 7, 4, 6)
  igraph::V(g)$name <- c("b", "c", "a", "d")
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(3, 1, 4, 2)
  score <- centrality_relative_entropy(g)
  expect_named(score, c("b", "c", "a", "d"))
  expect_equal(sum(score), 1)
  expect_equal(centrality(g, measures = "relative_entropy")$relative_entropy,
               unname(score))
  expect_equal(centrality_relative_entropy(g, simplify = FALSE, mode = "in",
                                           loops = TRUE, cutoff = 1,
                                           invert_weights = TRUE), score)
  expect_equal(centrality_relative_entropy(g, weighted = FALSE), score)
  expect_equal(centrality_relative_entropy(a[perm, perm]), score[perm])
  # Raw output already sums to one, so normalization only rescales it.
  expect_equal(centrality_relative_entropy(g, normalized = TRUE),
               score / max(score))
  meta <- list_centralities()
  expect_false(meta$uses_weights[meta$measure == "relative_entropy"])
  expect_false(meta$mode_aware[meta$measure == "relative_entropy"])
  expect_false(meta$costly[meta$measure == "relative_entropy"])
  expect_true("relative_entropy" %in% .cg_no_mode_measures())
  expect_true(all(.cg_re_negative_default() %in% .cg_re_vocabulary()))
})

test_that("a tier keeps going when relative entropy has no value", {
  # Betweenness vanishes on a complete graph, so the measure is undefined.
  # Naming it raises; a tier that merely included it warns and gives NA.
  clique <- igraph::make_full_graph(3)
  expect_error(centrality(clique, measures = "relative_entropy"),
               class = "cograph_undefined_index")
  expect_error(centrality(clique, measures = c("degree", "relative_entropy")),
               class = "cograph_undefined_index")
  # The tier raises other measures' own "returning NA" warnings too, so only
  # the one this guard emits is kept.
  only_guard <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      if (!inherits(w, "cograph_undefined_measure")) {
        invokeRestart("muffleWarning")
      }
    })
  }
  expect_warning(
    df <- only_guard(centrality(clique, type = "all",
                                membership = c(1, 1, 2))),
    class = "cograph_undefined_measure"
  )
  expect_true("relative_entropy" %in% names(df))
  expect_equal(df$relative_entropy, rep(NA_real_, 3))
  expect_equal(df$degree_all, c(2, 2, 2))
  # A tier still propagates anything that is not an undefined-index condition.
  expect_error(centrality(clique, type = "all", mode = "nonsense"))
  # And a defined graph gives the tier the ordinary value.
  ring <- suppressWarnings(centrality(igraph::make_ring(5), type = "all",
                                      membership = rep(1, 5)))
  expect_equal(ring$relative_entropy, rep(0.2, 5))
})
