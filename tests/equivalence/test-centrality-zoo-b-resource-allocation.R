# centrality_ira() and centrality_iira(): iterative resource allocation (Ren, Zeng, Chen, Liao and Liu 2014, EPL
# 106, 48005) and its propagation-feature variant (Zhong, Liu and Shang 2015, Physics Letters A 379, 2272-2276).
# Both are compared with the values the papers print for their figure graph, and with the steady state and the
# t-step products written out below from the papers' equations.
#
# Definitions used here, with theta the k-shell index and A the adjacency matrix:
#   IRA (Ren 2014 eqs. 1-3, alpha = 1): I(t + 1) = M I(t), I(0) = 1, M_ij = A_ij theta_i / sum_k A_kj theta_k.
#     M is column-stochastic, and x_i = theta_i * sum_j A_ij theta_j satisfies M x = x (substitute it), so the
#     steady state is x scaled to sum to n (the resource is conserved).
#   IIRA (Zhong 2015 eq. 5): the same with row i multiplied by psi_i = 1 - (1 - beta)^k_i, k_i the degree, and
#     I(t) = M^t 1 after t = 50 steps.

# Ren et al. 2014, figure 1: paper labels 0-4 are nodes 1-5; panels (b) to (d) add one edge each.
ira_panel <- function(panel) {
  edges <- c(3, 2, 3, 1, 2, 1, 1, 4, 1, 5, 3, 4, 4, 5, 2, 5)
  zoob_graph(5L, edges[seq_len(10L + 2L * (panel - 1L))])
}

# The k-shell index by peeling (Seidman 1983): remove every node of degree at most k, repeat, then raise k.
ira_kshell <- function(graph) {
  peel <- function(alive, shell, k) {
    if (!any(alive)) return(shell)
    degree <- rowSums(graph[, alive, drop = FALSE])
    low <- alive & degree <= k
    if (any(low)) peel(alive & !low, replace(shell, low, k), k) else peel(alive, shell, k + 1)
  }
  stats::setNames(peel(rep(TRUE, nrow(graph)), numeric(nrow(graph)), 0), rownames(graph))
}

# The IRA steady state from Ren 2014 eqs. (1)-(3), as derived in the header.
ira_steady_state <- function(graph) {
  theta <- ira_kshell(graph)
  x <- theta * as.numeric(graph %*% theta)
  x / sum(x) * nrow(graph)
}

# The IIRA score after `steps` products with Zhong 2015's eq. (5) matrix.
iira_product <- function(graph, beta = 0.2, steps = 50L) {
  theta <- ira_kshell(graph)
  psi <- 1 - (1 - beta)^rowSums(graph)
  received <- as.numeric(crossprod(graph, theta))
  allocation <- graph * outer(psi * theta, received, \(numerator, denominator) numerator / denominator)
  Reduce(\(resource, step) as.numeric(allocation %*% resource), seq_len(steps), rep(1, nrow(graph))) |>
    stats::setNames(rownames(graph))
}

independent <- "independent: closed forms and hand derivations from the definition"
# IRA stops when the largest change between iterates falls below ira_tol = 1e-6 (Ren 2014's epsilon), so an
# iterate sits within about 1e-6 of the limit, never closer by construction: 1e-5 allows for that.
iterative <- 1e-5

test_that("IRA equals the steady state Ren et al. (2014) print for figure 1(a)", {
  # Page 3 prints I(50) = [15/8, 5/4, 5/4, 5/16, 5/16] as exact fractions.
  expect_reference(centrality_ira(ira_panel(1L)), c(15 / 8, 5 / 4, 5 / 4, 5 / 16, 5 / 16),
                   reference = "published: Ren et al. 2014, p. 3 (I(50) of figure 1(a))",
                   covers = c("centrality()", "centrality_ira()"), quantity = "ira, figure 1(a), steady state",
                   tolerance = iterative)
})

test_that("IRA equals Ren et al. (2014) Table 1", {
  # Table 1 prints two decimals: half a unit in the last place is 0.005, absolute.
  printed <- list(c(1.88, 1.25, 1.25, 0.31, 0.31), c(1.59, 0.91, 1.36, 0.91, 0.23),
                  c(1.43, 0.71, 1.07, 1.07, 0.71), c(1.25, 0.94, 0.94, 0.94, 0.94))
  lapply(2:4, \(panel) {
    expect_reference(centrality_ira(ira_panel(panel)), printed[[panel]],
                     reference = "published: Ren et al. 2014, Table 1", covers = c("centrality()", "centrality_ira()"),
                     quantity = sprintf("ira, figure 1(%s)", letters[panel]), tolerance = 0.005)
  })
  panel_a <- centrality_ira(ira_panel(1L))
  expect_reference(panel_a[2:5], printed[[1L]][2:5], reference = "published: Ren et al. 2014, Table 1",
                   covers = c("centrality()", "centrality_ira()"), quantity = "ira, figure 1(a), nodes 1-4 (paper labels)",
                   tolerance = 0.005)
  expect_known_difference(panel_a[1], printed[[1L]][1], reference = "published: Ren et al. 2014, Table 1",
                          covers = c("centrality()", "centrality_ira()"), quantity = "ira, figure 1(a), node 0 (paper label)",
                          tolerance = 0.005,
                          reason = paste("The limit is 15/8 = 1.875, which the paper rounds up to 1.88. The iteration",
                                         "approaches 15/8 from below and stops at ira_tol = 1e-6, at 1.8749997, just",
                                         "outside the half unit; the exact limit is checked to 1e-5 against p. 3."))
})

test_that("IRA equals its steady state written out from Ren et al. (2014) eqs. (1)-(3)", {
  kite <- zoob_graph(4L, c(1, 2, 1, 3, 2, 3, 1, 4))
  graphs <- list(`figure 1(b)` = ira_panel(2L), `figure 1(c)` = ira_panel(3L), `figure 1(d)` = ira_panel(4L),
                 `triangle with a pendant` = kite, `ring of 7` = zoob_ring(7L), `complete graph of 5` = zoob_complete(5L))
  Map(\(graph, name) {
    expect_reference(centrality_ira(graph), ira_steady_state(graph), reference = independent,
                     covers = c("centrality()", "centrality_ira()"), quantity = paste0("ira, steady state, ", name),
                     tolerance = iterative)
  }, graphs, names(graphs))
  # In a clique every share is 1 / (m - 1) whatever the mass or exponent, so I(1) = I(0) = 1.
  expect_reference(centrality_ira(zoob_complete(5L), ira_mass = "degree", ira_alpha = 0.5), rep(1, 5),
                   reference = independent, covers = c("centrality()", "centrality_ira()"),
                   quantity = "ira, complete graph of 5, degree mass, alpha 0.5", tolerance = iterative)
  # The four-node path is bipartite with equal classes, so it converges; its classes 2 and 2 leave no -1 mode.
  expect_reference(centrality_ira(zoob_path(4L)), c(2, 4, 4, 2) / 3, reference = independent,
                   covers = c("centrality()", "centrality_ira()"), quantity = "ira, steady state, path of 4",
                   tolerance = iterative)
})

test_that("IIRA equals the I(50) Zhong et al. (2015) print for figure 2", {
  # The preprint prints I(50) = [8.19e-20, 4.32e-20, 4.32e-20, 6.7e-21, 6.7e-21] (beta = 0.2, theta = k-shell):
  # the last printed digit is in the 1e-22 place for every entry, so half a unit is 5e-23, absolute.
  expect_reference(centrality_iira(ira_panel(1L)), c(8.19e-20, 4.32e-20, 4.32e-20, 6.7e-21, 6.7e-21),
                   reference = "published: Zhong et al. 2015, Fig. 2 (I(50))",
                   covers = c("centrality()", "centrality_iira()"), quantity = "iira, figure 2, I(50)",
                   tolerance = 5e-23)
})

test_that("IIRA equals the 50-step product written out from Zhong et al. (2015) eq. (5)", {
  graphs <- list(`figure 2` = ira_panel(1L), `figure 1(d) of Ren 2014` = ira_panel(4L),
                 `triangle with a pendant` = zoob_graph(4L, c(1, 2, 1, 3, 2, 3, 1, 4)), kite = zoob_kite())
  # Fifty matrix products of exact rationals: a relative 1e-10 allows for the rounding of the products.
  Map(\(graph, name) {
    expect_reference(centrality_iira(graph), iira_product(graph), reference = independent,
                     covers = c("centrality()", "centrality_iira()"), quantity = paste0("iira, I(50), ", name),
                     tolerance = 1e-10, measure = "relative")
  }, graphs, names(graphs))
  # Closed forms: on a clique or a cycle every column of the matrix sums to psi, so I(t) = psi^t; a star returns
  # to a flat vector every second step, I(2k) = (beta psi_centre)^k, and I(1) = (q psi_centre, beta / q, ...).
  expect_reference(centrality_iira(zoob_complete(5L), iira_steps = 6), rep((1 - 0.8^4)^6, 5), reference = independent,
                   covers = c("centrality()", "centrality_iira()"), quantity = "iira, complete graph of 5, 6 steps",
                   tolerance = 1e-10, measure = "relative")
  expect_reference(centrality_iira(zoob_ring(6L), iira_beta = 0.3, iira_steps = 9), rep((1 - 0.7^2)^9, 6),
                   reference = independent, covers = c("centrality()", "centrality_iira()"),
                   quantity = "iira, ring of 6, beta 0.3, 9 steps", tolerance = 1e-10, measure = "relative")
  expect_reference(centrality_iira(zoob_star(3L), iira_steps = 10), rep((0.2 * (1 - 0.8^3))^5, 4),
                   reference = independent, covers = c("centrality()", "centrality_iira()"),
                   quantity = "iira, star of 3 leaves, 10 steps", tolerance = 1e-10, measure = "relative")
  expect_reference(centrality_iira(zoob_star(3L), iira_steps = 1), c(3 * (1 - 0.8^3), rep(0.2 / 3, 3)),
                   reference = independent, covers = c("centrality()", "centrality_iira()"),
                   quantity = "iira, star of 3 leaves, 1 step", tolerance = 1e-10, measure = "relative")
})
