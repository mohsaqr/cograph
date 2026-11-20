# network_small_world(): sigma = (C / C_rand) / (L / L_rand) (Humphries and Gurney 2008), C the global
# transitivity, L the mean distance over connected pairs, C_rand and L_rand the means over `n_random` G(n, m)
# graphs drawn from the caller's stream. Two kinds of evidence, both statistical in the sense of the standard:
#
# - a property that holds for any draw: when every graph with n nodes and m edges is isomorphic to the input
#   (the complete graph, the complete graph less one edge), C_rand = C and L_rand = L, so sigma = 1;
# - a Monte Carlo comparison: the expected sigma of a sparse graph, from 4000 G(n, m) graphs drawn in base R
#   (sample.int over the node pairs, transitivity and breadth-first distances written out), against the
#   function's estimate with n_random = 100 on five seeds. The tolerance is 4 standard errors of the estimate,
#   from the delta method on log sigma with the variances and covariance of C_rand and L_rand over the reference's
#   own draws (for 1/100 + 1/4000 of a draw), fixed before the run. A probe of 40 seeds on this graph before
#   the file was run passed 40 of 40 (largest |z| = 3.35).

exact <- 1e-10

#' Global transitivity and mean distance over connected pairs of an undirected 0/1 matrix
sw_stats <- function(binary) {
  degree <- rowSums(binary)
  triples <- sum(degree * (degree - 1))
  hops <- vr_hops(binary)
  connected <- row(hops) != col(hops) & is.finite(hops)
  c(C = if (triples > 0) sum(diag(binary %*% binary %*% binary)) / triples else NaN, L = mean(hops[connected]))
}

#' The expected sigma of a graph and the standard error of its estimate from `n_random` comparison graphs
sw_reference <- function(graph, draws = 4000L, n_random = 100L, seed = 2026L) {
  binary <- vr_neighbours(graph)
  n <- nrow(binary)
  m <- sum(binary) / 2
  pairs <- which(upper.tri(binary), arr.ind = TRUE)
  random <- ref_with_seed(seed, vapply(seq_len(draws), \(i) {
    drawn <- matrix(0, n, n)
    drawn[pairs[sample.int(nrow(pairs), m), , drop = FALSE]] <- 1
    sw_stats(drawn + t(drawn))
  }, numeric(2)))
  observed <- sw_stats(binary)
  c_mean <- mean(random["C", ])
  l_mean <- mean(random["L", ])
  sigma <- (observed[["C"]] / c_mean) / (observed[["L"]] / l_mean)
  spread <- stats::var(t(random))
  log_var <- spread["C", "C"] / c_mean^2 + spread["L", "L"] / l_mean^2 - 2 * spread["C", "L"] / (c_mean * l_mean)
  list(sigma = sigma, se = sigma * sqrt(log_var * (1 / n_random + 1 / draws)))
}

test_that("sigma is 1 when every comparison graph is isomorphic to the input", {
  complete <- matrix(1, 6, 6, dimnames = list(letters[1:6], letters[1:6]))
  diag(complete) <- 0
  less_one <- complete
  less_one[1, 2] <- 0
  less_one[2, 1] <- 0
  lapply(1:5, \(seed) {
    expect_reference(ref_with_seed(seed, network_small_world(complete, n_random = 5)), 1,
                     reference = "independent: G(n, m) of a complete graph is the complete graph",
                     covers = "network_small_world()", quantity = sprintf("sigma, complete graph of 6, seed %d", seed),
                     tolerance = exact)
    expect_reference(ref_with_seed(seed, network_small_world(less_one, n_random = 5)), 1,
                     reference = "independent: G(n, m) of a complete graph less one edge is isomorphic to it",
                     covers = "network_small_world()",
                     quantity = sprintf("sigma, complete graph of 6 less one edge, seed %d", seed), tolerance = exact)
  })
})

test_that("sigma agrees with its Monte Carlo expectation from G(n, m) graphs drawn in base R", {
  graph <- ref_connected_graph(16, density = 0.3, seed = 5L)
  truth <- sw_reference(graph)
  lapply(1:5, \(seed) {
    expect_reference(ref_with_seed(seed, network_small_world(graph, n_random = 100)), truth$sigma,
                     reference = "independent: 4000 G(n, m) graphs drawn and measured in base R",
                     covers = "network_small_world()",
                     quantity = sprintf("Monte Carlo summary: sigma, n_random = 100, seed %d", seed),
                     tolerance = 4 * truth$se)
  })
})
