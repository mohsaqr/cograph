# degree_distribution() (the degrees and their frequency table behind the plot) and fit_degree_distribution()
# (maximum-likelihood fits of power-law, exponential, Poisson and geometric degree models, Clauset, Shalizi & Newman
# 2009). degree_distribution() reads igraph::degree(), so igraph is `internal`; NetworkX's degree() and
# degree_histogram() are the counted reference. The fits are cograph's own closed forms; the reference is stats: the
# densities stats::dpois(), dgeom(), dexp() and the continuous power law, maximised numerically with
# stats::optimize(). With xmin = NULL the power law is fitted by igraph::fit_power_law() (internal), so the power law
# is compared at a fixed xmin. The Kolmogorov-Smirnov columns are not compared (cograph calls stats::ks.test() for
# two of them).
#
# Graphs: random graphs from helper-reference.R.

undirected <- ref_graph(40, density = 0.15, seed = 131L)
directed <- ref_graph(30, density = 0.15, seed = 132L, directed = TRUE)

exact <- 1e-10
# stats::optimize() stops when the bracket is below its tolerance: the maximiser is known to about 1e-7 relative,
# and the log-likelihood, flat at its maximum, to far less.
optimised <- 1e-6

draw_off <- \() {
  grDevices::pdf(NULL)
  invisible()
}

test_that("degree_distribution() degrees and table equal NetworkX", {
  draw_off()
  on.exit(grDevices::dev.off(), add = TRUE)
  nx <- ref_networkx(undirected, c(degree = "dict(G.degree())", histogram = "nx.degree_histogram(G)"))
  result <- degree_distribution(undirected)
  histogram <- stats::setNames(as.numeric(unlist(nx$histogram)), seq_along(unlist(nx$histogram)) - 1L)
  expect_reference(result$degree, nx$degree, reference = "networkx", covers = "degree_distribution()",
                   quantity = "degree, undirected", tolerance = exact)
  expect_reference(c(result$table), histogram[histogram > 0], reference = "networkx", covers = "degree_distribution()",
                   quantity = "number of nodes of each degree, undirected", tolerance = exact)
  on_directed <- ref_networkx(directed, c(in_degree = "dict(G.in_degree())", degree = "dict(G.degree())"))
  expect_reference(degree_distribution(directed, mode = "in")$degree, on_directed$in_degree, reference = "networkx",
                   covers = "degree_distribution()", quantity = "in-degree, directed", tolerance = exact)
  expect_reference(degree_distribution(directed)$degree, on_directed$degree, reference = "networkx",
                   covers = "degree_distribution()", quantity = "degree, mode all, directed", tolerance = exact)
})

test_that("fit_degree_distribution() fits equal the maximum of the stats densities", {
  fit <- fit_degree_distribution(undirected, distributions = c("exponential", "poisson", "geometric"))
  degree <- rowSums(d1_binary(undirected))
  k <- degree[degree >= 1]
  n <- length(k)
  maximise <- \(loglik, interval) stats::optimize(loglik, interval, maximum = TRUE, tol = 1e-12)
  poisson <- maximise(\(lambda) sum(stats::dpois(k, lambda, log = TRUE)), c(0.01, 50))
  geometric <- maximise(\(p) sum(stats::dgeom(k, p, log = TRUE)), c(1e-4, 1 - 1e-4))
  exponential <- maximise(\(rate) sum(stats::dexp(k, rate, log = TRUE)), c(1e-4, 50))
  expect_reference(c(poisson = fit$fits$poisson$parameters$lambda, geometric = fit$fits$geometric$parameters$p,
                     exponential = fit$fits$exponential$parameters$lambda),
                   c(poisson = poisson$maximum, geometric = geometric$maximum, exponential = exponential$maximum),
                   reference = "stats", covers = "fit_degree_distribution()",
                   quantity = "maximum-likelihood parameter, degrees >= 1", tolerance = optimised, measure = "relative")
  loglik <- c(poisson = poisson$objective, geometric = geometric$objective, exponential = exponential$objective)
  expect_reference(data.frame(distribution = fit$comparison$distribution, aic = fit$comparison$aic,
                              bic = fit$comparison$bic),
                   data.frame(distribution = names(loglik), aic = -2 * loglik + 2, bic = -2 * loglik + log(n)),
                   reference = "stats", covers = "fit_degree_distribution()",
                   quantity = "AIC and BIC at the maximum, degrees >= 1", tolerance = exact, measure = "relative",
                   key = "distribution")
})

test_that("fit_degree_distribution() power law at a fixed xmin equals the maximum of the continuous power law", {
  fit <- fit_degree_distribution(undirected, distributions = "power_law", xmin = 4)
  degree <- rowSums(d1_binary(undirected))
  k <- degree[degree >= 4]
  # Continuous power law above xmin: f(k) = (alpha - 1) / xmin * (k / xmin)^(-alpha) (Clauset et al. 2009, eq. 2.2).
  power_law <- stats::optimize(\(alpha) sum(log((alpha - 1) / 4) - alpha * log(k / 4)), c(1.0001, 20),
                               maximum = TRUE, tol = 1e-12)
  expect_reference(fit$fits$power_law$parameters$alpha, power_law$maximum, reference = "stats",
                   covers = "fit_degree_distribution()", quantity = "power-law exponent, xmin = 4",
                   tolerance = optimised, measure = "relative")
  expect_reference(fit$fits$power_law$loglik, power_law$objective, reference = "stats",
                   covers = "fit_degree_distribution()", quantity = "power-law log-likelihood, xmin = 4",
                   tolerance = exact, measure = "relative")
})
