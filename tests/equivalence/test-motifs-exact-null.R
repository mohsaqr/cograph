# Exact-null fixture for the instance-mode (subgraphs) permutation test.
#
# Every unit is small enough to enumerate all permutations of its target
# stubs, so the probability that a triple takes each MAN type in that unit is
# known exactly (types assigned by igraph::triad_census, not by cograph). The
# null count of a triple-type across units is then a Poisson-binomial whose
# mean, standard deviation and two-sided tail are computed by convolution.
# cograph's Monte Carlo expected, z and p must agree with these within their
# Monte Carlo error.

skip_on_cran()
skip_if_not_installed("igraph")

.exact_perms <- function(v) {
  if (length(v) <= 1L) return(list(v))
  do.call(c, lapply(seq_along(v), function(i) {
    lapply(.exact_perms(v[-i]), function(p) c(v[i], p))
  }))
}

.exact_man <- c("003", "012", "102", "021D", "021U", "021C", "111D", "111U",
                "030T", "030C", "201", "120D", "120U", "120C", "210", "300")

# Probability of each MAN type, and of each labelled six-bit pattern, for
# every node triple in one unit, by full enumeration of the target-stub
# permutations. Loops are dropped and parallel edges collapse, as in the
# observed classification. Types come from igraph; codes are read straight off
# the permuted adjacency (bits 1->2, 2->1, 1->3, 3->1, 2->3, 3->2).
.exact_unit_probs <- function(from, to, labels) {
  rs <- match(from, labels)
  cs <- match(to, labels)
  triples <- utils::combn(length(labels), 3)
  bit_rows <- c(1L, 2L, 1L, 3L, 2L, 3L)
  bit_cols <- c(2L, 1L, 3L, 1L, 3L, 2L)
  per_perm <- lapply(.exact_perms(seq_along(cs)), function(p) {
    a <- matrix(0L, length(labels), length(labels))
    a[cbind(rs, cs[p])] <- 1L
    diag(a) <- 0L
    vapply(seq_len(ncol(triples)), function(j) {
      sub <- a[triples[, j], triples[, j]]
      g <- igraph::graph_from_adjacency_matrix(sub, mode = "directed")
      code <- sum(sub[cbind(bit_rows, bit_cols)] * 2^(0:5))
      c(type = match(.exact_man[igraph::triad_census(g) == 1L], .exact_man),
        code = code)
    }, numeric(2))
  })
  key <- apply(triples, 2, function(t) paste(labels[t], collapse = " - "))
  n_perms <- length(per_perm)
  do.call(rbind, lapply(seq_len(ncol(triples)), function(j) {
    type_j <- vapply(per_perm, function(m) m["type", j], numeric(1))
    code_j <- vapply(per_perm, function(m) m["code", j], numeric(1))
    rbind(
      data.frame(triad = key[j], cell = .exact_man, strict = FALSE,
                 prob = tabulate(type_j, 16L) / n_perms),
      data.frame(triad = key[j], cell = as.character(0:63), strict = TRUE,
                 prob = tabulate(code_j + 1L, 64L) / n_perms)
    )
  }))
}

# Poisson-binomial distribution of a sum of independent Bernoulli(p_u).
.exact_pbinom_pmf <- function(p) {
  Reduce(function(pmf, pu) c(pmf, 0) * (1 - pu) + c(0, pmf) * pu, p, 1)
}

.exact_check <- function(strict) {
  labels <- c("A", "B", "C", "D")
  # Unit shapes chosen so the triple A-B-C takes different observed types and
  # every type has an interior, non-degenerate null probability.
  shapes <- list(
    cycle  = list(from = c("A", "B", "C"), to = c("B", "C", "A")),
    chain4 = list(from = c("A", "B", "C", "D"), to = c("B", "C", "D", "A")),
    mixed  = list(from = c("A", "B", "A", "C"), to = c("B", "A", "C", "B"))
  )
  plan <- c(cycle = 4L, chain4 = 3L, mixed = 3L)
  unit_shape <- rep(names(plan), plan)
  el <- do.call(rbind, Map(function(shape, id) {
    data.frame(actor = sprintf("u%02d", id), from = shapes[[shape]]$from,
               to = shapes[[shape]]$to)
  }, unit_shape, seq_along(unit_shape)))

  n_perm <- 20000L
  sg <- subgraphs(el, pattern = "all", min_count = 1, min_transitions = 1,
                  n_perm = n_perm, seed = 42, strict = strict)
  res <- as.data.frame(sg)
  expect_gt(nrow(res), 3L)
  cell <- if (strict) as.character(res$pattern_code) else res$type

  probs <- lapply(shapes, function(s) {
    pr <- .exact_unit_probs(s$from, s$to, labels)
    pr[pr$strict == strict, ]
  })
  exact <- do.call(rbind, lapply(seq_len(nrow(res)), function(r) {
    p_u <- vapply(unit_shape, function(shape) {
      pr <- probs[[shape]]
      pr$prob[pr$triad == res$triad[r] & pr$cell == cell[r]]
    }, numeric(1))
    pmf <- .exact_pbinom_pmf(p_u)
    k <- seq_along(pmf) - 1L
    mu <- sum(p_u)
    sigma <- sqrt(sum(p_u * (1 - p_u)))
    tail <- sum(pmf[abs(k - mu) >= abs(res$observed[r] - mu) - 1e-9])
    data.frame(mu = mu, sigma = sigma, p = min(tail, 1))
  }))

  # Four Monte Carlo standard errors of the mean.
  tol_mean <- 4 * exact$sigma / sqrt(n_perm) + 1e-9
  expect_true(all(abs(res$expected - exact$mu) <= tol_mean))

  # z: compare where the null is non-degenerate, allowing the Monte Carlo
  # error of the mean and of the sd.
  ok <- exact$sigma > 0
  z_exact <- (res$observed[ok] - exact$mu[ok]) / exact$sigma[ok]
  tol_z <- 4 * (1 + abs(z_exact) / sqrt(2)) / sqrt(n_perm)
  expect_true(all(abs(res$z[ok] - z_exact) <= tol_z))

  # p: add-one Monte Carlo estimate of the exact two-sided tail.
  tol_p <- 1 / (n_perm + 1) + 4 * sqrt(exact$p * (1 - exact$p) / n_perm)
  expect_true(all(abs(res$p - exact$p) <= tol_p))
}

test_that("strict subgraphs() null matches the exact enumerated null", {
  .exact_check(strict = TRUE)
})

test_that("shape-level subgraphs() null matches the exact enumerated null", {
  .exact_check(strict = FALSE)
})
