# centrality() and its wrappers for three measures defined by counting what flows out of a node:
# - bridging capital (Jackson 2019, arXiv:1711.09504v3, section 3.3, p. 18; Social Choice and Welfare 54, 2020):
#   for every arc i -> j, the drop in the value-weighted expected walk counts sum_{h <= T} P^h when that one entry
#   of P is set to zero; a node's score sums the drops over its outgoing arcs.
# - beta measure (van den Brink and Gilles 1992, Tilburg FEW 565, Definition 2.1 and Example 2.2): the sum over a
#   node's successors of one over their number of predecessors; the negative variant is the same on the
#   transposed graph (Boldi and Vigna 2014, p. 26).
# - Expected Force and its modified form (Lawyer 2015, Scientific Reports 5:8665, eqs. 1-2): the entropy of the
#   boundary-edge counts over all orderings of two transmissions from the seed; the modified form multiplies it by
#   log(alpha * degree).
# None of the three has an implementation in an R or Python package; each definition is written out below in base
# R, separately from cograph's code (no igraph call in R/centrality-batch29.R, -31.R, -32.R). Van den Brink and
# Gilles print the beta scores of their Example 2.2.

exact <- 1e-10
independent <- "independent: closed forms and hand derivations from the definition"
karate <- zoob_karate()
directed <- ref_connected_graph(12, density = 0.2, seed = 43L, directed = TRUE)
probabilities <- ref_graph(8, density = 0.35, seed = 41L, directed = TRUE, weighted = TRUE)

# Jackson (2019), section 3.3: E(P) = sum_{h = 1..T} P^h; criticality of arc (i, j) = sum_st V_st (E(P) - E(Q))_st
# with Q = P except Q_ij = 0. A walk that uses the arc counts once, however often it uses it.
walk_total <- function(p, steps) {
  Reduce(`+`, lapply(seq_len(steps), \(h) Reduce(`%*%`, rep(list(p), h))))
}
bridging_definition <- function(p, steps, values = matrix(1, nrow(p), ncol(p))) {
  base <- walk_total(p, steps)
  arcs <- which(p > 0, arr.ind = TRUE)
  criticality <- vapply(seq_len(nrow(arcs)), \(k) {
    cut <- p
    cut[arcs[k, 1L], arcs[k, 2L]] <- 0
    sum(values * (base - walk_total(cut, steps)))
  }, numeric(1))
  vapply(seq_len(nrow(p)), \(i) sum(criticality[arcs[, 1L] == i]), numeric(1))
}

test_that("bridging_capital equals Jackson's single-arc deletion written out", {
  chain <- zoob_graph(3, c(1, 2, 2, 3), directed = TRUE, labels = c("A", "B", "C"))
  chain["A", "B"] <- 0.5
  chain["B", "C"] <- 0.2
  values <- matrix(1, 3, 3)
  values[1, 3] <- 5
  lapply(1:3, \(steps) {
    expect_reference(centrality_bridging_capital(zoob_complete(2), steps), bridging_definition(zoob_complete(2), steps),
                     reference = independent, covers = c("centrality()", "centrality_bridging_capital()"),
                     quantity = sprintf("bridging_capital, one edge, %d steps", steps), tolerance = exact)
  })
  expect_reference(centrality_bridging_capital(matrix(0.5, 1, 1), 3), bridging_definition(matrix(0.5, 1, 1), 3),
                   reference = independent, covers = c("centrality()", "centrality_bridging_capital()"), quantity = "bridging_capital, loop of 0.5, 3 steps",
                   tolerance = exact)
  expect_reference(centrality_bridging_capital(chain), bridging_definition(chain, 2), reference = independent,
                   covers = c("centrality()", "centrality_bridging_capital()"), quantity = "bridging_capital, weighted chain, 2 steps", tolerance = exact)
  expect_reference(centrality_bridging_capital(chain, 2, values), bridging_definition(chain, 2, values),
                   reference = independent, covers = c("centrality()", "centrality_bridging_capital()"),
                   quantity = "bridging_capital, weighted chain, source-destination values", tolerance = exact)
  path3 <- zoob_graph(3, c(1, 2, 2, 3), directed = TRUE)
  expect_reference(centrality_bridging_capital(path3, 2), bridging_definition(path3, 2), reference = independent,
                   covers = c("centrality()", "centrality_bridging_capital()"), quantity = "bridging_capital, directed path of 3, 2 steps", tolerance = exact)
  # Values on one source-destination pair only, read along the arcs and against them.
  only_ac <- matrix(0, 3, 3)
  only_ac[1, 3] <- 7
  expect_reference(centrality_bridging_capital(chain, 2, only_ac), bridging_definition(chain, 2, only_ac),
                   reference = independent, covers = c("centrality()", "centrality_bridging_capital()"),
                   quantity = "bridging_capital, weighted chain, value on A to C only", tolerance = exact)
  expect_reference(centrality_bridging_capital(t(chain), 2, only_ac), bridging_definition(t(chain), 2, only_ac),
                   reference = independent, covers = c("centrality()", "centrality_bridging_capital()"),
                   quantity = "bridging_capital, reversed chain, value on A to C only", tolerance = exact)
  expect_reference(centrality_bridging_capital(t(chain), 2, t(only_ac)), bridging_definition(t(chain), 2, t(only_ac)),
                   reference = independent, covers = c("centrality()", "centrality_bridging_capital()"),
                   quantity = "bridging_capital, reversed chain, value on C to A only", tolerance = exact)
  expect_reference(centrality_bridging_capital(probabilities, 3), bridging_definition(probabilities, 3),
                   reference = independent, covers = c("centrality()", "centrality_bridging_capital()"),
                   quantity = "bridging_capital, directed probability graph, 3 steps", tolerance = exact)
})

# van den Brink and Gilles (1992), Definition 2.1, on the simple digraph (loops removed, an undirected edge is two
# arcs): beta(i) = sum over successors j of 1 / (number of predecessors of j). Negative: on the transpose.
beta_definition <- function(graph, direction = c("positive", "negative")) {
  direction <- match.arg(direction)
  arcs <- (graph > 0) * 1
  diag(arcs) <- 0
  if (direction == "negative") arcs <- t(arcs)
  predecessors <- colSums(arcs)
  as.numeric(arcs %*% ifelse(predecessors > 0, 1 / pmax(predecessors, 1), 0))
}

diamond <- zoob_graph(4, c(1, 2, 1, 3, 2, 4, 3, 4), directed = TRUE)

test_that("beta_measure reproduces van den Brink and Gilles' Example 2.2", {
  # Example 2.2 (arcs 1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4), printed pp. 3-4 as the exact fractions (2, 1/2, 1/2, 0):
  # exact values, so the tolerance is that of exact arithmetic.
  expect_reference(centrality_beta_measure(diamond), c(2, 1 / 2, 1 / 2, 0),
                   reference = "published: van den Brink and Gilles 1992, Example 2.2, pp. 3-4",
                   covers = c("centrality()", "centrality_beta_measure()"),
                   quantity = "beta_measure, positive, Example 2.2 diamond", tolerance = exact)
})

test_that("beta_measure equals Definition 2.1 written out, in both directions", {
  in_star <- zoob_graph(5, cbind(2:5, 1L), directed = TRUE)
  branching <- zoob_graph(5, c(1, 2, 1, 3, 2, 3, 4, 3), directed = TRUE)
  pieces <- zoob_union(zoob_ring(3), zoob_star(3), matrix(0, 1, 1))
  graphs <- list(`Example 2.2 diamond` = diamond, `in-star of 4` = in_star, `star of 4` = zoob_star(4),
                 `complete graph on 5` = zoob_complete(5), `branching digraph with an isolate` = branching,
                 `ring, star and isolate` = pieces, `karate club` = karate, `directed graph` = directed)
  lapply(names(graphs), \(name) {
    lapply(c("positive", "negative"), \(direction) {
      expect_reference(centrality_beta_measure(graphs[[name]], direction), beta_definition(graphs[[name]], direction),
                       reference = independent, covers = c("centrality()", "centrality_beta_measure()"),
                       quantity = sprintf("beta_measure, %s, %s", direction, name), tolerance = exact)
    })
  })
})

# Lawyer (2015), eqs. 1-2: for seed i, every sequence of two transmissions (i -> j, then u -> k with u in {i, j}
# and k not yet infected) is one outcome, distinct parents and orders included; D is the number of arcs from
# {i, j, k} to the uninfected; ExF = -sum p log p with p = D / sum D (outcomes with D = 0 add nothing).
expected_force_definition <- function(graph) {
  arcs <- (graph > 0) * 1
  diag(arcs) <- 0
  vapply(seq_len(nrow(arcs)), \(i) {
    boundary <- unlist(lapply(which(arcs[i, ] > 0), \(j) {
      infected <- c(i, j)
      reached <- which(arcs[infected, , drop = FALSE] > 0, arr.ind = TRUE)[, "col"]
      reached <- reached[!reached %in% infected]
      vapply(reached, \(k) sum(arcs[c(i, j, k), -c(i, j, k), drop = FALSE]), numeric(1))
    }))
    p <- boundary[boundary > 0] / sum(boundary)
    -sum(p * log(p))
  }, numeric(1))
}

test_that("expected_force equals Lawyer's enumeration of two transmissions", {
  out_star <- zoob_graph(5, cbind(1L, 2:5), directed = TRUE)
  graphs <- list(`complete graph on 5` = zoob_complete(5), `star of 4` = zoob_star(4), `ring of 4` = zoob_ring(4),
                 `directed diamond` = diamond, `out-star of 4` = out_star, `path of 4` = zoob_path(4),
                 `karate club` = karate, `directed graph` = directed)
  lapply(names(graphs), \(name) {
    expect_reference(centrality_expected_force(graphs[[name]]), expected_force_definition(graphs[[name]]),
                     reference = independent, covers = c("centrality()", "centrality_expected_force()"),
                     quantity = paste0("expected_force, ", name), tolerance = exact)
  })
})

test_that("modified_expected_force equals log(alpha * degree) times the enumerated Expected Force", {
  graphs <- list(`star of 4` = zoob_star(4), `karate club` = karate)
  lapply(names(graphs), \(name) {
    lapply(c(2, 3), \(alpha) {
      expect_reference(centrality_modified_expected_force(graphs[[name]], exf_alpha = alpha),
                       log(alpha * rowSums(graphs[[name]])) * expected_force_definition(graphs[[name]]),
                       reference = independent, covers = c("centrality()", "centrality_modified_expected_force()"),
                       quantity = sprintf("modified_expected_force, alpha %g, %s", alpha, name), tolerance = exact)
    })
  })
})
