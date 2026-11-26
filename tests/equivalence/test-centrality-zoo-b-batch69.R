# Four measures of cograph's centrality batch 69 against NetworkX and values computed by the source audits, and the
# two Zoo labels batch 69 maps to centrality_pagerank():
#   EigenTrust: Kamvar, Schlosser and Garcia-Molina (2003), WWW '03, equations (4)-(5), is pagerank with
#     damping = 1 - a and personalized = the pre-trust (the measure `eigentrust` was removed on 2026-11-26).
#   Seeley's index (Seeley 1949, restated by Vigna 2016, eq. 1) is pagerank with damping = 1 on a network where every
#     node has an out-tie and the walk has one closed class that is aperiodic; outside that class pagerank's power
#     iteration from the uniform vector does not return it, so only graphs inside it are compared.
#   centrality_rcfb(): Agryzkov, Tortosa and Vicent (2019), Applied Mathematics and Computation 347:600-615,
#     equations (14)-(20) and Algorithm 2 of the accepted manuscript.
#   centrality_m_centrality(): Ibnoulouafi, El Haziti and Cherifi (2018), J. Stat. Mech. 073407, equations (6)-(11).
#   centrality_wksd(): Namtirtha, Dutta and Dutta (2018, 2020) as restated by Shetty and Bhattacharjee (2025),
#     equations (3)-(4), and Lu and Dong (2020), equations (2.5)-(2.6).
#   centrality_mapping_entropy_betweenness(): Gialampoukidis et al. (2016), EISIC, as transcribed by the Zoo (2.251).
# None of the sources prints a node score, so there is no published fixture. The "independent: source audit" values
# below were computed by the audits in local_testing_and_equivalence/candidate_review40/ and candidate_review54/ from
# the definitions, by programs that share no code with cograph; they check the transcription, not the papers.

seeley_audit <- "independent: source audit, candidate_review40/eigentrust_seeley_worked.csv (exact fractions)"
rcfb_audit <- "independent: source audit, candidate_review54/rcfb_check.R (Krackhardt kite)"
m_audit <- paste("independent: source audits, candidate_review40/m-centrality_worked.csv and",
                 "candidate_review54/group5_check.R")
wksd_audit <- "independent: source audit, candidate_review54/wksd_worked.csv"
exact <- 1e-10
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()
kite <- zoob_kite()
# A 4-clique and a triangle joined by a long path, with one pendant.
long <- zoob_graph(14L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12,
                          11, 13, 12, 13, 13, 14))
graphs <- list(undirected = undirected, sparse = sparse, karate = karate, long = long)
weighted <- ref_connected_graph(12, density = 0.3, seed = 47L, weighted = TRUE, digits = 6L)
directed <- ref_graph(10, density = 0.3, seed = 48L, directed = TRUE, weighted = TRUE, digits = 6L)
strong <- ref_connected_graph(10, density = 0.3, seed = 50L, directed = TRUE, weighted = TRUE, digits = 6L)

compare_networkx <- function(measure, call, tolerance = exact, on = graphs, ...) {
  wrapper <- match.fun(paste0("centrality_", measure))
  Map(\(graph, name) {
    expect_reference(wrapper(graph, ...), ref_networkx(graph, c(value = call))$value, reference = "networkx",
                     covers = c("centrality()", sprintf("centrality_%s()", measure)),
                     quantity = sprintf("%s, %s", measure, name), tolerance = tolerance)
  }, on, names(on))
}

# --- EigenTrust and Seeley's index as pagerank settings --------------------------------------------------------------

# Seeley's index needs an out-tie at every node, and pagerank's iteration returns it only when the walk has one closed
# class (the eigenvalue 1 is simple) and is aperiodic (no other eigenvalue of modulus 1). Base R, no cograph.
seeley_class <- function(graph) {
  out <- rowSums(graph)
  if (any(out <= 0)) return(FALSE)
  moduli <- abs(eigen(t(graph / out), only.values = TRUE)$values)
  sum(abs(moduli - 1) < 1e-9) == 1L
}

test_that("pagerank with damping = 1 reproduces the audit's value of Seeley's index on the triangle with a pendant", {
  paw <- zoob_graph(4L, c(1, 2, 1, 3, 2, 3, 3, 4))
  stopifnot("the paw must be in the class where the identity holds" = seeley_class(paw))
  expect_reference(centrality_pagerank(paw, damping = 1), stats::setNames(c(1, 1, 3 / 2, 1 / 2) / 4, 1:4),
                   reference = seeley_audit, covers = c("centrality()", "centrality_pagerank()"),
                   quantity = "Seeley's index as pagerank(damping = 1), triangle with a pendant", tolerance = exact,
                   note = "the audit's path of four and cycle of four are periodic: outside the class, not compared")
})

# Seeley (Vigna 2016, eq. 1): the left eigenvector of the row-normalised matrix for the eigenvalue 1, by numpy.eig.
seeley <- paste0(
  "(lambda M: (lambda w, V: (lambda v: dict(zip(G, v / v.sum())))(np.real(V[:, np.argmin(abs(w - 1))])))(",
  "*np.linalg.eig(M.T)))((lambda A: A / A.sum(axis=1, keepdims=True))(nx.to_numpy_array(G, nodelist=list(G),",
  " weight='weight')))")

test_that("Seeley's index is pagerank with damping = 1 where the walk has one aperiodic closed class", {
  on <- list(undirected = undirected, karate = karate, long = long, weighted = weighted, strong = strong)
  stopifnot("every compared graph must be in the class where the identity holds" =
              all(vapply(on, seeley_class, logical(1))))
  compare_networkx("pagerank", seeley, tolerance = 1e-9, on = on, damping = 1)
})

# EigenTrust's equation (5), t = (1 - a) C' t + a p, iterated 4000 times from p in NumPy; C is the row-normalised
# local trust with loops dropped and negative values clipped, and a row without trust takes p (equation 4).
eigentrust <- function(a, pretrust) paste0(
  "(lambda A, n, p: (lambda C: dict(zip(G, __import__('functools').reduce(lambda t, _: (1 - ", a, ") * C.T @ t + ",
  a, " * p, range(4000), p))))(np.where(A.sum(1, keepdims=True) > 0, A / np.where(A.sum(1, keepdims=True) > 0,",
  " A.sum(1, keepdims=True), 1), p[None, :])))(np.maximum(nx.to_numpy_array(G, nodelist=list(G),",
  " weight='weight') * (1 - np.eye(len(G))), 0), len(G), (lambda q: q / q.sum())(np.array([",
  paste(sprintf("%.17g", pretrust), collapse = ", "), "])))")

test_that("EigenTrust's equation (5) in NumPy is pagerank with damping = 1 - a and the pre-trust as personalized", {
  on <- list(directed = directed, sparse = sparse, weighted = weighted, karate = karate)
  settings <- list(list(a = 0.05, pretrust = \(n) rep(1, n), label = "uniform pre-trust"),
                   list(a = 0.15, pretrust = \(n) (seq_len(n) %% 3) * 1, label = "pre-trust 1, 2, 0, 1, 2, 0, ..."),
                   list(a = 0.2, pretrust = \(n) c(1, numeric(n - 1L)), label = "pre-trust on node 1 only"),
                   list(a = 0.5, pretrust = \(n) (seq_len(n) %% 3) * 1, label = "pre-trust 1, 2, 0, 1, 2, 0, ..."))
  Map(\(graph, name) {
    lapply(settings, \(s) {
      p <- s$pretrust(nrow(graph))
      expect_reference(centrality_pagerank(graph, damping = 1 - s$a, personalized = p),
                       ref_networkx(graph, c(value = eigentrust(s$a, p)))$value, reference = "networkx",
                       covers = c("centrality()", "centrality_pagerank()"), tolerance = exact,
                       quantity = sprintf("EigenTrust eq. 5 as pagerank, a = %s, %s, %s", s$a, s$label, name),
                       note = if (name == "sparse") "one node has no tie, so its trust goes to the pre-trust")
    })
  }, on, names(on))
})

# --- rcfb ----------------------------------------------------------------------------------------------------------

test_that("rcfb reproduces the audit's values on the Krackhardt kite", {
  covers <- c("centrality()", "centrality_rcfb()")
  expect_reference(centrality_rcfb(kite),
                   stats::setNames(c(0.137973, 0.137973, 0.109595, 0.208514, 0.109595, 0.264392, 0.264392, 0.370000,
                                     0.250000, 0.090000), 1:10),
                   reference = rcfb_audit, covers = covers, quantity = "RCFB, limit epsilon -> 0 (L pseudo-inverse)",
                   tolerance = 5e-7, note = "printed to six decimals by the audit; Algorithm 2 as printed")
  # The audit prints the distances to two significant figures.
  limit <- centrality_rcfb(kite)
  gap <- \(e) max(abs(centrality_rcfb(kite, rcfb_epsilon = e) - limit))
  expect_reference(gap(1e-4), 9.4e-05, reference = rcfb_audit, covers = covers,
                   quantity = "RCFB on the kite, largest distance to the limit at epsilon = 1e-4", tolerance = 5e-7)
  expect_reference(gap(1e-6), 9.4e-07, reference = rcfb_audit, covers = covers,
                   quantity = "RCFB on the kite, largest distance to the limit at epsilon = 1e-6", tolerance = 5e-9)
})

# The pseudo-inverse from numpy.linalg.eigh: np.linalg.pinv returned a wrong pseudo-inverse of the karate club's
# Laplacian on the CI machine (|L P L - L| = 0.10) while eigh and svd were exact (3e-14).
eigh_pinv <- "(lambda w, V: (V[:, w > 1e-9] / w[w > 1e-9]) @ V[:, w > 1e-9].T)(*np.linalg.eigh(L))"
rcfb <- function(inverse = eigh_pinv) paste0(
  "(lambda W, n: (lambda L: (lambda P, E: dict(zip(G, [sum(abs(W[i, j] * (P[i, s] - P[j, s])) for s in range(n) for",
  " (i, j) in E if (W[i, j] * (P[i, s] - P[j, s]) > 0 and i == v) or (W[i, j] * (P[i, s] - P[j, s]) < 0 and j == v))",
  " / n for v in range(n)])))(", inverse, ", [(i, j) for i in range(n) for j in range(i + 1, n) if W[i, j] > 0]))(",
  "np.diag(W.sum(1)) - W))(nx.to_numpy_array(G, nodelist=list(G), weight='weight'), len(G))")

test_that("rcfb equals Algorithm 2 over NumPy's eigendecomposition and inverse", {
  compare_networkx("rcfb", rcfb(), tolerance = 1e-9, on = c(graphs, list(weighted = weighted)))
  compare_networkx("rcfb", rcfb("np.linalg.inv(1e-3 * np.eye(n) + L)"), tolerance = 1e-9,
                   on = list(undirected = undirected, weighted = weighted), rcfb_epsilon = 1e-3)
})

# --- m_centrality --------------------------------------------------------------------------------------------------

test_that("m_centrality reproduces the audits' values on the Krackhardt kite", {
  worked <- c(1.6755614985543257, 1.6755614985543257, 1.870741909293688, 3.0, 1.870741909293688, 2.014139762081791,
              2.014139762081791, 1.5294757955390366, 1.0, 0.6863171970260245)
  expect_reference(centrality_m_centrality(kite), stats::setNames(worked, 1:10), reference = m_audit,
                   covers = c("centrality()", "centrality_m_centrality()"),
                   quantity = "M-centrality, kite (mu = 0.3726343940520481)", tolerance = exact,
                   note = "two audits, NetworkX and R/igraph, agree to about 1e-15")
})

m_centrality <- paste0(
  "(lambda k, ks, n: (lambda dd: (lambda E: (lambda mu: {i: mu * ks[i] + (1 - mu) * dd[i] for i in G})(0.5 if",
  " (min(sum(ks.values()), sum(dd.values())) <= 0 or n < 2 or 2 - sum(E) < 1e-12) else (1 - E[0]) / (2 -",
  " sum(E))))([-sum(x / sum(d.values()) * np.log(x / sum(d.values())) for x in d.values() if x > 0) / np.log(n)",
  " if n > 1 and sum(d.values()) > 0 else 1.0 for d in (ks, dd)]))({i: (k[i] * sum(abs(k[j] - k[i]) for j in",
  " G[i]) / sum(k[j] for j in G[i])) if k[i] > 0 else 0.0 for i in G}))(dict(G.degree()), nx.core_number(G),",
  " len(G))")

test_that("m_centrality equals equations (6)-(11) over NetworkX's core numbers", {
  compare_networkx("m_centrality", m_centrality)
})

# --- wksd ----------------------------------------------------------------------------------------------------------

test_that("wksd reproduces the audit's values on two small networks", {
  toy <- zoob_graph(5L, c(1, 2, 2, 3, 3, 4, 2, 4, 4, 5))
  covers <- c("centrality()", "centrality_wksd()")
  settings <- list(c(0.2, 0.9), c(0.9, 0.2), c(1, 1))
  toy_values <- list(c(2.64, 13.68, 10.56, 13.68, 2.64), c(3.41, 19.84, 13.64, 19.84, 3.41), c(10, 55, 40, 55, 10))
  kite_values <- list(c(50.4, 50.4, 36.63, 81.9, 36.63, 62.16, 62.16, 20.88, 4.55, 1.43),
                      c(78.12, 78.12, 50.49, 151.2, 50.49, 110.67, 110.67, 37.82, 8.4, 2.2))
  Map(\(s, values) {
    expect_reference(centrality_wksd(toy, wksd_alpha = s[1], wksd_mu = s[2]), stats::setNames(values, 1:5),
                     reference = wksd_audit, covers = covers, tolerance = 1e-9,
                     quantity = sprintf("Wksd, five-node graph, alpha = %s, mu = %s", s[1], s[2]))
  }, settings, toy_values)
  Map(\(s, values) {
    expect_reference(centrality_wksd(kite, wksd_alpha = s[1], wksd_mu = s[2]), stats::setNames(values, 1:10),
                     reference = wksd_audit, covers = covers, tolerance = 1e-9,
                     quantity = sprintf("Wksd, Krackhardt kite, alpha = %s, mu = %s", s[1], s[2]))
  }, settings[1:2], kite_values)
})

wksd <- function(alpha, mu) paste0(
  "(lambda c: (lambda x: {i: x[i] * sum(x[j] for j in G[i]) for i in G})({i: ", alpha, " * G.degree(i) + ", mu,
  " * c[i] for i in G}))(nx.core_number(G))")

test_that("wksd equals the restated formula over NetworkX's core numbers", {
  compare_networkx("wksd", wksd(0.2, 0.9), wksd_alpha = 0.2, wksd_mu = 0.9)
  compare_networkx("wksd", wksd(0.9, 0.2), wksd_alpha = 0.9, wksd_mu = 0.2)
})

# --- mapping_entropy_betweenness -----------------------------------------------------------------------------------

meb <- paste0("(lambda B: {i: -B[i] * sum(np.log(B[j]) for j in G[i] if B[j] > 0) for i in G})(",
              "nx.betweenness_centrality(G, normalized=True))")

test_that("mapping_entropy_betweenness equals the formula over NetworkX's normalised betweenness", {
  compare_networkx("mapping_entropy_betweenness", meb, on = c(graphs, list(kite = kite)))
})
