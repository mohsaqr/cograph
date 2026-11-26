# Four measures of cograph's centrality batch 55, against NetworkX, igraph, a published example and definitions
# written out in base R.
#   centrality_con(): common out-neighbor score, Bonato, Eikmeier, Gleich and Malik (2019), Complex Networks and
#     Their Applications VIII, pp. 105-116, section 2: CON(u) = sum_{v != u} sum_k min(A_uk, A_vk).
#   centrality_emi(): Li, Cai, Li and Du (2020), Entropy 22(1):52, equations (22), (24), (25), p. 9, base-10 logs.
#   centrality_nwrank(): Wang, Duenas-Osorio and Padgett (2015), Scientific Reports 5:15141, equations (5)-(7), p. 3:
#     the principal eigenvector of W H, W the node-link incidence matrix and H the shares of equation (7).
#   centrality_biased_random_walk(): Takes and Kosters (2011), Proceedings of BNAIC 2011, pp. 215-222: the stationary
#     distribution of the walk of Algorithm 1 (the paper estimates it by one finite walk).
# No other package implements these measures; NetworkX and igraph supply the neighbour sets, common-neighbour
# counts, edge betweenness and PageRank they are assembled from.

independent <- "independent: closed forms and hand derivations from the definition"
figure_3 <- "published: Li, Cai, Li and Du 2020, Fig. 3 and its worked example, p. 10"
exact <- 1e-10
# The paper prints five decimals: half a unit in the last printed place.
printed_5dp <- 5e-6
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
directed <- ref_connected_graph(12, density = 0.25, seed = 43L, directed = TRUE)
karate <- ref_karate()

test_that("con equals the common out-neighbour count assembled from NetworkX's successor sets", {
  directed_call <- c(con = "{u: sum(len(set(G.successors(u)) & set(G.successors(v))) for v in G if v != u) for u in G}")
  undirected_call <- c(con = "{u: sum(len(set(G[u]) & set(G[v])) for v in G if v != u) for u in G}")
  expect_reference(centrality_con(directed), ref_networkx(directed, directed_call)$con, reference = "networkx",
                   covers = c("centrality()", "centrality_con()"), quantity = "con, directed", tolerance = exact)
  expect_reference(centrality_con(undirected), ref_networkx(undirected, undirected_call)$con, reference = "networkx",
                   covers = c("centrality()", "centrality_con()"), quantity = "con, undirected", tolerance = exact)
  expect_reference(centrality_con(karate), ref_networkx(karate, undirected_call)$con, reference = "networkx",
                   covers = c("centrality()", "centrality_con()"), quantity = "con, karate", tolerance = exact)
})

test_that("con counts the smaller multiplicity", {
  # 1 -> 3 twice and 1 -> 4 once; 2 -> 3 once and 2 -> 4 three times: min(2, 1) + min(1, 3) = 2 for both.
  multi <- matrix(0, 4L, 4L, dimnames = list(1:4, 1:4))
  multi[cbind(c(1, 1, 2, 2), c(3, 4, 3, 4))] <- c(2, 1, 1, 3)
  expect_reference(centrality_con(multi), c(`1` = 2, `2` = 2, `3` = 0, `4` = 0), reference = independent,
                   covers = c("centrality()", "centrality_con()"), quantity = "con, multiplicities", tolerance = exact)
})

test_that("emi reproduces the mutual information printed for the paper's Fig. 3", {
  # Neighbour sets a: {b, c, d}, b: {a, c}, c: {a, b}, d: {a}. The printed S and K columns belong to the directed
  # weighted form of the measure (equation 23), which cograph does not implement; MI is the same in both forms.
  fig3 <- zoob_graph(4L, c(1, 2, 2, 3, 3, 1, 1, 4), labels = c("a", "b", "c", "d"))
  degree <- rowSums(fig3)
  shares <- fig3 * matrix(degree, 4L, 4L, byrow = TRUE) / as.numeric(fig3 %*% degree)
  entropy <- -rowSums(ifelse(fig3 > 0, shares * log10(shares), 0))
  expect_reference(centrality_emi(fig3) - entropy, c(a = -0.03876, b = -0.05062, c = -0.05062, d = 0),
                   reference = figure_3, covers = c("centrality()", "centrality_emi()"),
                   quantity = "emi minus its entropy term (MI), Fig. 3", tolerance = printed_5dp)
})

test_that("emi equals equations (22), (24) and (25) evaluated over NetworkX's neighbour sets", {
  call <- c(emi = paste0(
    "{i: -sum((G.degree(j) / sum(G.degree(l) for l in G[i])) * np.log10(G.degree(j) / sum(G.degree(l) for l in G[i]))",
    " for j in G[i]) + sum((len(set(G[i]) & set(G[j])) / (G.degree(i) + G.degree(j)))",
    " * np.log10((G.degree(i) + G.degree(j) - len(set(G[i]) & set(G[j]))) / (G.degree(i) + G.degree(j)))",
    " for j in G[i]) for i in G}"))
  Map(\(graph, name) {
    expect_reference(centrality_emi(graph), ref_networkx(graph, call)$emi, reference = "networkx",
                     covers = c("centrality()", "centrality_emi()"), quantity = sprintf("emi, %s", name),
                     tolerance = exact)
  }, list(undirected, karate), c("undirected", "karate"))
})

# The principal eigenvector of W H with igraph's edge betweenness in equation (7).
nwrank_definition <- function(graph, alpha) {
  g <- ref_igraph(graph)
  ends <- igraph::ends(g, igraph::E(g), names = FALSE)
  n <- nrow(graph)
  m <- nrow(ends)
  degree <- igraph::degree(g)
  between <- igraph::edge_betweenness(g, directed = FALSE)
  w <- matrix(0, n, m)
  w[cbind(ends[, 1L], seq_len(m))] <- 1
  w[cbind(ends[, 2L], seq_len(m))] <- 1
  h <- vapply(seq_len(n), \(j) {
    at <- ends[, 1L] == j | ends[, 2L] == j
    far <- ifelse(ends[, 1L] == j, ends[, 2L], ends[, 1L])
    ifelse(at, alpha * degree[far] / sum(degree[far[at]]) + (1 - alpha) * between / sum(between[at]), 0)
  }, numeric(m))
  vector <- Re(eigen(w %*% h)$vectors[, 1L])
  ref_named(vector / sum(vector), graph)
}

test_that("nwrank equals the principal eigenvector of W H with igraph's edge betweenness", {
  skip_without_reference("igraph")
  Map(\(graph, name) {
    lapply(c(0, 0.5, 1), \(alpha) {
      expect_reference(centrality_nwrank(graph, nwrank_alpha = alpha), nwrank_definition(graph, alpha),
                       reference = "igraph", covers = c("centrality()", "centrality_nwrank()"),
                       quantity = sprintf("nwrank, alpha %s, %s", format(alpha), name), tolerance = exact)
    })
  }, list(undirected, karate, zoob_kite()), c("undirected", "karate", "kite"))
})

# The move matrix of the biased walk, P(v -> w) proportional to alpha f_deg(w) + (1 - alpha) f_nd(w).
brw_moves <- function(graph, alpha) {
  n <- nrow(graph)
  degree <- rowSums(graph)
  common <- (graph %*% graph) * graph
  f_nd <- 1 - vapply(seq_len(n), \(v) sum(ifelse(degree > 1 & graph[v, ] > 0, common[, v] / ((degree - 1) * degree[v]), 0)),
                     numeric(1))
  weights <- graph * matrix(alpha * (1 - 1 / degree) + (1 - alpha) * f_nd, n, n, byrow = TRUE)
  weights
}

test_that("the biased random walk equals igraph's PageRank on the graph weighted by the move weights", {
  skip_without_reference("igraph")
  # PageRank with damping 1 - p on a directed graph whose arc v -> w weighs alpha f_deg(w) + (1 - alpha) f_nd(w) is
  # the stationary distribution of the same chain.
  Map(\(graph, name) {
    weights <- brw_moves(unname(graph), 0.5)
    g <- igraph::graph_from_adjacency_matrix(weights, mode = "directed", weighted = TRUE)
    expected <- igraph::page_rank(g, damping = 0.85, weights = igraph::E(g)$weight)$vector
    expect_reference(centrality_biased_random_walk(graph), ref_named(expected, graph), reference = "igraph",
                     covers = c("centrality()", "centrality_biased_random_walk()"),
                     quantity = sprintf("biased_random_walk, alpha 0.5, jump 0.15, %s", name), tolerance = 1e-8)
  }, list(undirected, karate, zoob_kite()), c("undirected", "karate", "kite"))
})

test_that("the biased random walk is uniform on a ring", {
  expect_reference(centrality_biased_random_walk(zoob_ring(9L)), rep(1 / 9, 9), reference = independent,
                   covers = c("centrality()", "centrality_biased_random_walk()"),
                   quantity = "biased_random_walk, ring n = 9", tolerance = exact)
})
