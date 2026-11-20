# Randomized-shortest-path betweenness, centrality_rsp_betweenness() (Kivimaki, Lebichot, Saramaki and Saerens
# 2016, Scientific Reports 6:19668), against computations written out from the source's equations: closed forms on
# a single edge, a directed cycle and the complete graph; equation (15) as printed on strongly connected graphs; and
# the double sum of equations (8) and (14), with the zero rule for an unreachable pair (below equation (9)), on
# graphs that are not strongly connected. cograph evaluates the collapsed form, so the double sum is a separate
# route to the same number. No other package implements this measure (NetworkToolbox::rspbc is not installed on the
# ci machine).
#
# Graphs are built in base R by helper-zoo-b.R and helper-reference.R. Costs are the default rsp_cost = "inverse"
# (C = 1 / w), unit costs on these 0/1 graphs.

independent <- "independent: closed forms and hand derivations from the definition"
# Solving an n x n linear system and summing n^2 terms: agreement to rounding, relative to the score's size.
exact <- 1e-10

# Hop distances from every node (rows) to every node (columns), by breadth-first growth of the reached set.
rsp_hops <- function(a) {
  n <- nrow(a)
  adjacent <- unname(a) > 0
  start <- list(reach = diag(n) > 0, d = ifelse(diag(n) > 0, 0, Inf))
  Reduce(\(state, step) {
    reach <- (state$reach %*% adjacent) > 0 | state$reach
    state$d[reach & !state$reach] <- step
    list(reach = reach, d = state$d)
  }, seq_len(max(n - 1L, 1L)), start)$d
}

# The fundamental matrix Z = (I - P_ref * exp(-beta C))^-1 (equations (2)-(4)), P_ref the row-normalised
# adjacency, C = 1 / w on the arcs.
rsp_fundamental <- function(a, beta) {
  a <- unname(a)
  diag(a) <- 0
  pref <- a / pmax(rowSums(a), 1)
  cost <- ifelse(a > 0, 1 / a, 0)
  solve(diag(nrow(a)) - pref * exp(-beta * cost))
}

# Equation (15) as printed: diag(Z (1 / Z - n Diag(1 / z_ii))' Z). Defined only when every z_st > 0.
rsp_equation_15 <- function(a, beta) {
  z <- rsp_fundamental(a, beta)
  recip <- 1 / z
  diag(z %*% t(recip - nrow(a) * diag(diag(recip), nrow(a))) %*% z)
}

# Equations (8) and (14) summed over the ordered pairs (s, t) with t reachable from s:
#   score_i = sum_{s, t} (z_si / z_st - z_ti / z_tt) z_it,
# written as two matrix products: first[i, t] = sum_s r_st z_si / z_st and second[i, t] = (sum_s r_st) z_ti / z_tt.
rsp_double_sum <- function(a, beta) {
  z <- rsp_fundamental(a, beta)
  n <- nrow(z)
  reach <- is.finite(rsp_hops(a))
  first <- crossprod(z, ifelse(reach, 1 / z, 0))
  second <- t(z / diag(z)) * rep(colSums(reach), each = n)
  rowSums(z * (first - second))
}

test_that("rsp_betweenness of a single edge is one at every beta", {
  # With u = exp(-beta), Z = [[1, u], [u, 1]] / (1 - u^2), and equation (15) gives p - q^2 / p = 1 at both nodes.
  lapply(c(0.001, 0.5, 20), \(beta) {
    expect_reference(centrality_rsp_betweenness(zoob_path(2), rsp_beta = beta), c(1, 1), reference = independent,
                     covers = c("centrality()", "centrality_rsp_betweenness()"),
                     quantity = sprintf("rsp_betweenness, single edge, beta %g", beta), tolerance = exact,
                     measure = "scaled")
  })
})

test_that("rsp_betweenness of a directed cycle is n (n - 1) / 2", {
  # On C_n, z_st = u^d(s, t) / (1 - u^n): equation (14) is the indicator that i lies on the path s -> t with i != t,
  # so the score counts ordered pairs and beta cancels.
  cases <- expand.grid(n = c(3L, 5L, 8L), beta = c(0.01, 4))
  Map(\(n, beta) {
    expect_reference(centrality_rsp_betweenness(zoob_ring(n, directed = TRUE), rsp_beta = beta, directed = TRUE),
                     rep(n * (n - 1) / 2, n), reference = independent,
                     covers = c("centrality()", "centrality_rsp_betweenness()"),
                     quantity = sprintf("rsp_betweenness, directed cycle n = %d, beta %g", n, beta), tolerance = exact,
                     measure = "scaled")
  }, cases$n, cases$beta)
})

test_that("rsp_betweenness of a complete graph equals its closed form", {
  # Sherman-Morrison on ((1 + a) I - a J), a = exp(-beta) / (n - 1): z_ii = p, z_ij = q, and equation (14) summed by
  # symmetry over the n - 1 targets.
  closed <- function(n, beta) {
    u <- exp(-beta)
    v <- 1 - u
    p <- ((n - 1) * v + u) / (v * (n - 1 + u))
    q <- u / (v * (n - 1 + u))
    (n - 1) * (p - q^2 / p + (n - 2) * (q - q^2 / p))
  }
  cases <- expand.grid(n = c(3L, 5L, 7L), beta = c(0.01, 0.5, 2))
  Map(\(n, beta) {
    expect_reference(centrality_rsp_betweenness(zoob_complete(n), rsp_beta = beta), rep(closed(n, beta), n),
                     reference = independent, covers = c("centrality()", "centrality_rsp_betweenness()"),
                     quantity = sprintf("rsp_betweenness, complete graph n = %d, beta %g", n, beta), tolerance = exact,
                     measure = "scaled")
  }, cases$n, cases$beta)
})

test_that("rsp_betweenness equals equation (15) on strongly connected graphs", {
  lapply(c(0.01, 0.3, 1, 2), \(beta) {
    expect_reference(centrality_rsp_betweenness(zoob_kite(), rsp_beta = beta), rsp_equation_15(zoob_kite(), beta),
                     reference = independent, covers = c("centrality()", "centrality_rsp_betweenness()"),
                     quantity = sprintf("rsp_betweenness, equation (15), kite, beta %g", beta), tolerance = exact,
                     measure = "scaled")
  })
  expect_reference(centrality_rsp_betweenness(zoob_ring(8), rsp_beta = 1), rsp_equation_15(zoob_ring(8), 1),
                   reference = independent, covers = c("centrality()", "centrality_rsp_betweenness()"),
                   quantity = "rsp_betweenness, equation (15), ring n = 8, beta 1", tolerance = exact, measure = "scaled")
})

test_that("rsp_betweenness equals the double sum of equations (8) and (14)", {
  out_tree <- zoob_graph(10L, cbind((seq_len(9L) + 1L) %/% 2L, seq_len(9L) + 1L), directed = TRUE)
  graphs <- list(kite = zoob_kite(), `ring n = 7` = zoob_ring(7), `star of 5` = zoob_star(5),
                 `directed out-tree` = out_tree,
                 `random directed` = ref_graph(9, 0.4, seed = 24L, directed = TRUE))
  cases <- expand.grid(graph = names(graphs), beta = c(0.01, 1), stringsAsFactors = FALSE)
  Map(\(graph, beta) {
    expect_reference(centrality_rsp_betweenness(graphs[[graph]], rsp_beta = beta),
                     rsp_double_sum(graphs[[graph]], beta), reference = independent,
                     covers = c("centrality()", "centrality_rsp_betweenness()"),
                     quantity = sprintf("rsp_betweenness, double sum, %s, beta %g", graph, beta), tolerance = exact,
                     measure = "scaled")
  }, cases$graph, cases$beta)
})
