# centrality() and its wrappers for the measures centiserve (Jalili et al. 2015) computes: distance-based closeness
# variants, neighbourhood measures, Markov, LeaderRank, Hubbell, Katz, pairwise disconnectivity, current-flow
# closeness and communicability betweenness. centiserve is plain R over igraph; cograph computes these natively.
#
# Three centiserve functions are not compared, because the reference is wrong:
# - centiserve::salsa() returns the eigenvalues of the hub or authority matrix, not a score per node. cograph's SALSA
#   is checked against the in-degree identity of Lempel and Moran (2001) instead.
# - centiserve::dmnc() takes the vertex ids of the largest neighbourhood component from `which()` on the subgraph and
#   reads them as ids of the whole graph, so it counts the edges of the wrong vertices. cograph's DMNC is checked
#   against the definition (Lin et al. 2008).
# - centiserve::centroid() tests `u != i` with `u` left over from an earlier loop (the last vertex), so it skips the
#   last vertex instead of the node itself. cograph's centroid is checked against the definition (Wuchty and Stadler
#   2003).
# centiserve::epc() draws its percolations with runif() inside the function and cograph with its own seed, so the
# two estimates cannot be compared number by number: EPC is left out.
#
# Graphs: connected 0/1 graphs (a ring through every node), so no measure divides by an unreachable pair and no
# weight enters. centiserve reads the graph through igraph objects built by igraph from the same matrix.

undirected <- ref_connected_graph(12, density = 0.3, seed = 21L)
directed <- ref_connected_graph(12, density = 0.25, seed = 22L, directed = TRUE)
# Not strongly connected (113 of 144 ordered pairs reachable): pairwise disconnectivity is not trivial on it.
sparse_directed <- ref_graph(12, density = 0.15, seed = 24L, directed = TRUE)
# No cut vertex: centiserve::closeness.vitality() needs every node's removal to leave the graph connected.
biconnected <- ref_connected_graph(12, density = 0.45, seed = 31L)

# Integer counts, or a few additions and one division of integers: agreement to the last bits is expected.
exact <- 1e-10

# ClusterRank from Chen, Gao, Lu and Zhou (2013): Eq. 4 s_i = f(c_i) sum_{j in Gamma_i} (k_j + 1), Eq. 3
# f(c) = 10^-c, Eq. 1 c_i = (arcs among Gamma_i) / (k_i (k_i - 1)). Gamma_i: the out-neighbours (followers) on a
# directed graph, the neighbours on an undirected one; k the out-degree (the degree); NaN below two neighbours.
clusterrank_definition <- function(adjacency) {
  ties <- (adjacency != 0) * 1
  diag(ties) <- 0
  k <- rowSums(ties)
  vapply(seq_len(nrow(ties)), \(i) {
    gamma <- which(ties[i, ] == 1)
    if (length(gamma) < 2) return(NaN)
    10^-(sum(ties[gamma, gamma]) / (k[i] * (k[i] - 1))) * sum(k[gamma] + 1)
  }, numeric(1))
}

test_that("distance-based closeness variants equal centiserve", {
  skip_without_reference(c("centiserve", "igraph"))
  graph <- ref_igraph(undirected)
  graph_directed <- ref_igraph(directed)
  # Radiality, residual (Dangalchev) closeness and average distance follow their papers since cograph 2.7.20
  # (owner, 2026-11-22: the literature wins): a sum over the other nodes j != i. centiserve's code also sums the
  # node itself (radiality: + (D + 1) / (n - 1); residual closeness: + 2^0 = 1) and divides the average distance by
  # n + 1, although its help states n - 1. Compared as stated differences, and with centiserve's value less the
  # self term (both graphs are strongly connected, so D is igraph::diameter() of the graph centiserve reads).
  n_nodes <- nrow(undirected)
  self_term <- \(graph) (igraph::diameter(graph) + 1) / (n_nodes - 1)
  radiality_reason <- paste("centiserve::radiality() sums D + 1 - d(v, w) over every w, v itself included, which",
                            "adds (D + 1) / (n - 1); Valente and Foreman (1998) sum over the other nodes.")
  expect_known_difference(centrality_radiality(undirected), centiserve::radiality(graph), reference = "centiserve",
                          covers = c("centrality()", "centrality_radiality()"), quantity = "radiality, undirected",
                          tolerance = exact, reason = radiality_reason,
                          agrees_with = "centiserve::radiality() less (D + 1) / (n - 1)")
  expect_reference(centrality_radiality(undirected), centiserve::radiality(graph) - self_term(graph),
                   reference = "centiserve", covers = c("centrality()", "centrality_radiality()"),
                   quantity = "radiality, undirected, centiserve less its self term", tolerance = exact)
  expect_known_difference(centrality_radiality(directed, mode = "out"), centiserve::radiality(graph_directed, mode = "out"),
                          reference = "centiserve", covers = c("centrality()", "centrality_radiality()"),
                          quantity = "radiality, mode out, directed", tolerance = exact, reason = radiality_reason,
                          agrees_with = "centiserve::radiality() less (D + 1) / (n - 1)")
  expect_reference(centrality_radiality(directed, mode = "out"),
                   centiserve::radiality(graph_directed, mode = "out") - self_term(graph_directed),
                   reference = "centiserve", covers = c("centrality()", "centrality_radiality()"),
                   quantity = "radiality, mode out, directed, centiserve less its self term", tolerance = exact)
  expect_reference(centrality_lin(undirected), centiserve::lincent(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_lin()"), quantity = "lin, undirected", tolerance = exact)
  expect_reference(centrality_lin(directed, mode = "in"), centiserve::lincent(graph_directed, mode = "in"),
                   reference = "centiserve", covers = c("centrality()", "centrality_lin()"),
                   quantity = "lin, mode in, directed", tolerance = exact)
  expect_reference(centrality_decay(undirected, decay_parameter = 0.5), centiserve::decay(graph, decay = 0.5),
                   reference = "centiserve", covers = c("centrality()", "centrality_decay()"),
                   quantity = "decay 0.5, undirected", tolerance = exact)
  expect_reference(centrality_decay(directed, mode = "out", decay_parameter = 0.3),
                   centiserve::decay(graph_directed, mode = "out", decay = 0.3), reference = "centiserve",
                   covers = c("centrality()", "centrality_decay()"), quantity = "decay 0.3, mode out, directed",
                   tolerance = exact)
  # Generalised closeness with decay parameter delta is decay centrality under a second name.
  expect_reference(centrality_generalized_closeness(undirected, decay_parameter = 0.7), centiserve::decay(graph, decay = 0.7),
                   reference = "centiserve", covers = c("centrality()", "centrality_generalized_closeness()"),
                   quantity = "generalized closeness 0.7, undirected", tolerance = exact)
  residual_reason <- paste("centiserve::closeness.residual() sums 2^-d(i, j) over every j, i itself included (+1),",
                           "although its help states j != i, Dangalchev's (2006) definition.")
  expect_known_difference(centrality_residual_closeness(undirected), centiserve::closeness.residual(graph),
                          reference = "centiserve", covers = c("centrality()", "centrality_residual_closeness()"),
                          quantity = "residual closeness, undirected", tolerance = exact, reason = residual_reason,
                          agrees_with = "centiserve::closeness.residual() less 1")
  expect_reference(centrality_residual_closeness(undirected), centiserve::closeness.residual(graph) - 1,
                   reference = "centiserve", covers = c("centrality()", "centrality_residual_closeness()"),
                   quantity = "residual closeness, undirected, centiserve less its self term", tolerance = exact)
  expect_reference(centrality_residual_closeness(directed, mode = "out"),
                   centiserve::closeness.residual(graph_directed, mode = "out") - 1, reference = "centiserve",
                   covers = c("centrality()", "centrality_residual_closeness()"),
                   quantity = "residual closeness, mode out, directed, centiserve less its self term", tolerance = exact)
  # Dangalchev's closeness is the same quantity as residual closeness under a second name.
  expect_reference(centrality_dangalchev(undirected), centiserve::closeness.residual(graph) - 1, reference = "centiserve",
                   covers = c("centrality()", "centrality_dangalchev()"),
                   quantity = "dangalchev, undirected, centiserve less its self term", tolerance = exact)
  expect_reference(centrality_barycenter(undirected), centiserve::barycenter(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_barycenter()"), quantity = "barycenter, undirected",
                   tolerance = exact)
  expect_known_difference(centrality_average_distance(undirected), centiserve::averagedis(graph), reference = "centiserve",
                          covers = c("centrality()", "centrality_average_distance()"), quantity = "average distance, undirected",
                          tolerance = exact,
                          reason = paste("centiserve::averagedis() divides the sum of distances by n + 1 although its help",
                                         "states sum(dis(u, w) / (n - 1)), the mean over the other nodes."),
                          agrees_with = "centiserve::averagedis() times (n + 1) / (n - 1)")
  expect_reference(centrality_average_distance(undirected), centiserve::averagedis(graph) * (n_nodes + 1) / (n_nodes - 1),
                   reference = "centiserve", covers = c("centrality()", "centrality_average_distance()"),
                   quantity = "average distance, undirected, centiserve's sum over n - 1", tolerance = exact)
  expect_reference(centrality_average_distance(directed, mode = "out"),
                   centiserve::averagedis(graph_directed, mode = "out") * (n_nodes + 1) / (n_nodes - 1),
                   reference = "centiserve", covers = c("centrality()", "centrality_average_distance()"),
                   quantity = "average distance, mode out, directed, centiserve's sum over n - 1", tolerance = exact)
  expect_reference(centrality_closeness_vitality(biconnected), centiserve::closeness.vitality(ref_igraph(biconnected)),
                   reference = "centiserve", covers = c("centrality()", "centrality_closeness_vitality()"),
                   quantity = "closeness vitality, undirected, no cut vertex", tolerance = exact, measure = "scaled")
  # centiserve's Latora closeness is the sum of inverse distances: cograph's harmonic centrality.
  expect_reference(centrality_harmonic(undirected), centiserve::closeness.latora(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_harmonic()"), quantity = "harmonic (Latora closeness), undirected",
                   tolerance = exact)
})

test_that("k-reach equals centiserve's geokpath()", {
  skip_without_reference(c("centiserve", "igraph"))
  expect_reference(centrality_kreach(undirected, k = 3), centiserve::geokpath(ref_igraph(undirected), k = 3),
                   reference = "centiserve", covers = c("centrality()", "centrality_kreach()"),
                   quantity = "kreach k 3, undirected", tolerance = exact)
  expect_reference(centrality_kreach(directed, mode = "out", k = 2),
                   centiserve::geokpath(ref_igraph(directed), mode = "out", k = 2), reference = "centiserve",
                   covers = c("centrality()", "centrality_kreach()"), quantity = "kreach k 2, mode out, directed",
                   tolerance = exact)
})

test_that("neighbourhood measures equal centiserve", {
  skip_without_reference(c("centiserve", "igraph"))
  graph <- ref_igraph(undirected)
  # Lobby index (Korn, Schubert and Telcs 2009, Definition 1): the h-index of the neighbours' degrees. centiserve
  # takes neighborhood(order = 1), which holds the node itself (cograph <= 2.7.19 did the same).
  h_index <- \(values) {
    values <- sort(values, decreasing = TRUE)
    sum(values >= seq_along(values))
  }
  lobby_definition <- \(adjacency, closed = FALSE) {
    degree <- rowSums(adjacency != 0 | t(adjacency) != 0)
    vapply(seq_len(nrow(adjacency)), \(i) {
      h_index(degree[c(if (closed) i, which(adjacency[i, ] != 0 | adjacency[, i] != 0))])
    }, numeric(1))
  }
  expect_reference(centrality_lobby(undirected), ref_named(lobby_definition(undirected), undirected),
                   reference = "independent: the lobby index from Korn, Schubert and Telcs (2009), Definition 1, base R",
                   covers = c("centrality()", "centrality_lobby()"), quantity = "lobby, undirected", tolerance = exact)
  # centiserve is compared on the nodes where its closed neighbourhood changes the index, chosen from the graph by
  # the two definitions above (not from either program's numbers). On the other nodes the two conventions give the
  # same number by construction, so centiserve's value there is no evidence about the definition cograph follows
  # and is not recorded as agreement with it (audit A3, 2026-11-23).
  differs <- lobby_definition(undirected, closed = TRUE) != lobby_definition(undirected)
  expect_known_difference(centrality_lobby(undirected)[differs], centiserve::lobby(graph)[differs],
                          reference = "centiserve",
                          covers = c("centrality()", "centrality_lobby()"),
                          quantity = "lobby, undirected, nodes where the node's own degree changes the index",
                          tolerance = exact,
                          reason = paste("centiserve::lobby() takes the h-index over neighborhood(order = 1), which holds",
                                         "the node itself; Korn et al. (2009) count the node's neighbours only."),
                          agrees_with = "the h-index over the closed neighbourhood")
  expect_reference(centrality_bottleneck(undirected), centiserve::bottleneck(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_bottleneck()"), quantity = "bottleneck, undirected",
                   tolerance = exact)
  expect_reference(centrality_mnc(undirected), centiserve::mnc(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_mnc()"), quantity = "mnc, undirected", tolerance = exact)
  expect_reference(centrality_cross_clique(undirected), centiserve::crossclique(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_cross_clique()"), quantity = "cross clique, undirected",
                   tolerance = exact)
  expect_reference(centrality_semilocal(undirected), centiserve::semilocal(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_semilocal()"), quantity = "semilocal, undirected",
                   tolerance = exact)
  # ClusterRank (Chen, Gao, Lu and Zhou 2013, Eq. 3-4): 10^-c_i times the sum of k_j + 1 over the neighbours.
  # centiserve multiplies by c_i itself (cograph <= 2.7.19 did the same).
  expect_reference(centrality_clusterrank(undirected), ref_named(clusterrank_definition(undirected), undirected),
                   reference = "independent: ClusterRank from Chen, Gao, Lu and Zhou (2013), Eq. 1-4, base R",
                   covers = c("centrality()", "centrality_clusterrank()"), quantity = "clusterrank, undirected",
                   tolerance = exact)
  # centiserve's own sum, damped as the paper says: its value over its own c_i (igraph transitivity()) times
  # 10^-c_i, where c_i > 0 (every node of this graph).
  clustering <- igraph::transitivity(graph, type = "local")
  expect_reference(centrality_clusterrank(undirected),
                   ref_named(centiserve::clusterrank(graph) / clustering * 10^-clustering, undirected),
                   reference = "centiserve", covers = c("centrality()", "centrality_clusterrank()"),
                   quantity = "clusterrank, undirected, centiserve's sum damped by 10^-c (Chen et al. 2013, Eq. 3)",
                   tolerance = exact)
  expect_known_difference(centrality_clusterrank(undirected), centiserve::clusterrank(graph), reference = "centiserve",
                          covers = c("centrality()", "centrality_clusterrank()"), quantity = "clusterrank, undirected",
                          tolerance = exact,
                          reason = paste("centiserve::clusterrank() multiplies the neighbours' sum by the clustering",
                                         "coefficient c_i; Chen et al. (2013), Eq. 3, damp it by f(c_i) = 10^-c_i."),
                          agrees_with = "centiserve::clusterrank() / c_i * 10^-c_i")
  expect_reference(centrality_entropy(undirected), centiserve::entropy(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_entropy()"), quantity = "entropy, undirected", tolerance = exact)
  expect_reference(centrality_topological_coefficient(undirected), centiserve::topocoefficient(graph),
                   reference = "centiserve", covers = c("centrality()", "centrality_topological_coefficient()"),
                   quantity = "topological coefficient, undirected", tolerance = exact)
  expect_reference(centrality_leverage(undirected), centiserve::leverage(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_leverage()"), quantity = "leverage, undirected", tolerance = exact)
  expect_reference(centrality_diffusion(undirected), centiserve::diffusion.degree(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_diffusion()"), quantity = "diffusion degree, undirected",
                   tolerance = exact)
  expect_reference(centrality_laplacian(undirected), centiserve::laplacian(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_laplacian()"), quantity = "laplacian, undirected",
                   tolerance = exact)
})

test_that("neighbourhood measures with a direction equal centiserve, mode out", {
  skip_without_reference(c("centiserve", "igraph"))
  graph <- ref_igraph(directed)
  out_degree <- rowSums(directed != 0)
  lobby_out <- \(closed = FALSE) vapply(seq_len(nrow(directed)), \(i) {
    values <- sort(out_degree[c(if (closed) i, which(directed[i, ] != 0))], decreasing = TRUE)
    sum(values >= seq_along(values))
  }, numeric(1))
  expect_reference(centrality_lobby(directed, mode = "out"), ref_named(lobby_out(), directed),
                   reference = "independent: the lobby index from Korn, Schubert and Telcs (2009), Definition 1, base R",
                   covers = c("centrality()", "centrality_lobby()"), quantity = "lobby, mode out, directed", tolerance = exact)
  # As on the undirected graph: centiserve on the nodes where its closed neighbourhood changes the index, chosen
  # from the graph by the two definitions.
  differs <- lobby_out(closed = TRUE) != lobby_out()
  expect_known_difference(centrality_lobby(directed, mode = "out")[differs],
                          centiserve::lobby(graph, mode = "out")[differs],
                          reference = "centiserve", covers = c("centrality()", "centrality_lobby()"),
                          quantity = "lobby, mode out, directed, nodes where the node's own degree changes the index",
                          tolerance = exact,
                          reason = paste("centiserve::lobby() takes the h-index over neighborhood(order = 1), which holds",
                                         "the node itself; Korn et al. (2009) count the node's neighbours only."),
                          agrees_with = "the h-index over the closed neighbourhood")
  # Semi-local centrality on a directed graph with mode "all" (Chen et al. 2012): neighbour sets, a node tied in
  # both directions one neighbour, as centiserve's neighborhood() (cograph 2.7.19 and earlier listed it twice).
  expect_reference(centrality_semilocal(directed), centiserve::semilocal(graph, mode = "all"),
                   reference = "centiserve", covers = c("centrality()", "centrality_semilocal()"),
                   quantity = "semilocal, mode all, directed", tolerance = exact)
  # ClusterRank on a directed graph (Chen et al. 2013, Eq. 1): a link i -> j makes j a follower of i; the sum runs
  # over the followers (out-neighbours) of their out-degrees + 1, and c_i counts the arcs among the followers over
  # k_i (k_i - 1). centiserve::clusterrank(directed = TRUE) takes the same followers, out-degrees and coefficient
  # and multiplies by c_i instead of 10^-c_i.
  expect_reference(centrality_clusterrank(directed), ref_named(clusterrank_definition(directed), directed),
                   reference = "independent: ClusterRank from Chen, Gao, Lu and Zhou (2013), Eq. 1-4, base R",
                   covers = c("centrality()", "centrality_clusterrank()"), quantity = "clusterrank, directed",
                   tolerance = exact)
  # centiserve's own followers' sum, damped as the paper says, where its coefficient c_i (its
  # .cs.transitivity.directed()) is above 0; where c_i = 0 centiserve's value is 0 and gives no sum.
  directed_clustering <- centiserve:::.cs.transitivity.directed(graph)
  damped <- ref_named(centiserve::clusterrank(graph) / directed_clustering * 10^-directed_clustering, directed)
  positive <- directed_clustering > 0
  expect_reference(centrality_clusterrank(directed)[positive], damped[positive],
                   reference = "centiserve", covers = c("centrality()", "centrality_clusterrank()"),
                   quantity = "clusterrank, directed, centiserve's sum damped by 10^-c, nodes with c > 0",
                   tolerance = exact)
  expect_not_comparable(centrality_clusterrank(directed)[!positive], damped[!positive],
                        reference = "centiserve", covers = c("centrality()", "centrality_clusterrank()"),
                        quantity = "clusterrank, directed, centiserve's sum damped by 10^-c, nodes with c = 0",
                        reason = paste("centiserve::clusterrank() is 0 at a node whose clustering coefficient is 0,",
                                       "so its neighbours' sum, and the paper's value, cannot be read from it."))
  expect_known_difference(centrality_clusterrank(directed), centiserve::clusterrank(graph), reference = "centiserve",
                          covers = c("centrality()", "centrality_clusterrank()"), quantity = "clusterrank, directed",
                          tolerance = exact,
                          reason = paste("centiserve::clusterrank(directed = TRUE) multiplies the followers' sum by the",
                                         "directed clustering coefficient c_i; Chen et al. (2013), Eq. 3, damp it by",
                                         "f(c_i) = 10^-c_i."),
                          agrees_with = "centiserve::clusterrank() / c_i * 10^-c_i")
  expect_reference(centrality_leverage(directed, mode = "out"), centiserve::leverage(graph, mode = "out"),
                   reference = "centiserve", covers = c("centrality()", "centrality_leverage()"),
                   quantity = "leverage, mode out, directed", tolerance = exact)
  expect_reference(centrality_diffusion(directed, mode = "out"), centiserve::diffusion.degree(graph, mode = "out"),
                   reference = "centiserve", covers = c("centrality()", "centrality_diffusion()"),
                   quantity = "diffusion degree, mode out, directed", tolerance = exact)
  # MNC on a directed graph takes the largest strongly connected component of the neighbourhood (owner,
  # 2026-11-22), as centiserve::mnc() (clusters(mode = 'strong')) and cograph's own DMNC do; cograph 2.7.18 and
  # earlier took the weakly connected one (N1 3 against 1, N4 5 against 1), corrected in 2.7.19.
  expect_reference(centrality_mnc(directed, mode = "out"), centiserve::mnc(graph, mode = "out"),
                   reference = "centiserve", covers = c("centrality()", "centrality_mnc()"),
                   quantity = "mnc, mode out, directed", tolerance = exact)
})

test_that("Markov centrality equals centiserve's markovcent()", {
  skip_without_reference(c("centiserve", "igraph"))
  # Both invert a fundamental matrix of the walk, built in different ways: relative 1e-9 for the two inversions.
  expect_reference(centrality_markov(undirected), centiserve::markovcent(ref_igraph(undirected)),
                   reference = "centiserve", covers = c("centrality()", "centrality_markov()"),
                   quantity = "markov, undirected", tolerance = 1e-9, measure = "relative")
})

test_that("LeaderRank equals centiserve's leaderrank()", {
  skip_without_reference(c("centiserve", "igraph"))
  # centiserve iterates the walk until the mean relative change of one step falls below 2e-5 and returns that
  # iterate. cograph runs the same power iteration from the same start (1 per node, 0 at the ground node) with the
  # same stopping rule, so the two iterates agree to rounding (difference 0 in run 20261123-104021); 1e-10 relative.
  # (Was 1e-3, the distance of either iterate from the limit; tightened 2026-11-23, audit.)
  expect_reference(centrality_leaderrank(directed), centiserve::leaderrank(ref_igraph(directed)),
                   reference = "centiserve", covers = c("centrality()", "centrality_leaderrank()"),
                   quantity = "leaderrank, directed", tolerance = 1e-10, measure = "relative")
})

test_that("Hubbell and Katz centrality equal centiserve's hubbell() and katzcent()", {
  skip_without_reference(c("centiserve", "igraph"))
  # One linear solve each; the spectral radius of the undirected graph is 4.98, so both series converge.
  expect_reference(centrality_hubbell(undirected, hubbell_weight = 0.1),
                   centiserve::hubbell(ref_igraph(undirected), weightfactor = 0.1), reference = "centiserve",
                   covers = c("centrality()", "centrality_hubbell()"), quantity = "hubbell 0.1, undirected",
                   tolerance = 1e-9, measure = "relative")
  expect_reference(centrality_katz(undirected, katz_alpha = 0.1), centiserve::katzcent(ref_igraph(undirected), alpha = 0.1),
                   reference = "centiserve", covers = c("centrality()", "centrality_katz()"),
                   quantity = "katz 0.1, undirected", tolerance = 1e-9, measure = "relative")
  # Not on a directed graph: centiserve::katzcent() compares alpha with 1 / eigen(A)$values[1], which is complex
  # for a non-symmetric A, and stops with "invalid comparison with complex values".
})

test_that("pairwise disconnectivity equals centiserve's pairwisedis()", {
  skip_without_reference(c("centiserve", "igraph"))
  expect_reference(centrality_pairwisedis(sparse_directed), centiserve::pairwisedis(ref_igraph(sparse_directed)),
                   reference = "centiserve", covers = c("centrality()", "centrality_pairwisedis()"),
                   quantity = "pairwisedis, directed, not strongly connected", tolerance = exact)
  expect_reference(centrality_pairwisedis(directed), centiserve::pairwisedis(ref_igraph(directed)),
                   reference = "centiserve", covers = c("centrality()", "centrality_pairwisedis()"),
                   quantity = "pairwisedis, directed, strongly connected", tolerance = exact)
})

test_that("current-flow closeness equals centiserve's closeness.currentflow()", {
  skip_without_reference(c("centiserve", "igraph"))
  # Both solve the reduced Laplacian; relative 1e-9 for the solves.
  expect_reference(centrality_current_flow_closeness(undirected), centiserve::closeness.currentflow(ref_igraph(undirected)),
                   reference = "centiserve", covers = c("centrality()", "centrality_current_flow_closeness()"),
                   quantity = "current-flow closeness, undirected", tolerance = 1e-9, measure = "relative")
})

test_that("communicability betweenness equals centiserve's communibet(normalized = TRUE)", {
  skip_without_reference(c("centiserve", "igraph", "expm"))
  # centiserve 1.0.0 checks for expm with requireNamespace("expm", logical.return = TRUE), an argument
  # loadNamespace() refuses. Before R 4.6 requireNamespace() returns TRUE early for a loaded namespace, so loading
  # expm here makes the check pass alone as in the suite; from R 4.6 it calls loadNamespace() whenever it is given
  # an extra argument, and communibet() cannot run at all. The skip runs centiserve's own check and says so.
  loadNamespace("expm")
  skip_if(!suppressWarnings(requireNamespace("expm", logical.return = TRUE, quietly = TRUE)),
          "centiserve 1.0.0's own check for expm fails in this R (R >= 4.6), so communibet() refuses to run")
  # centiserve takes exp(A) from expm (Pade with scaling and squaring), cograph from an eigendecomposition: two
  # routes to the matrix exponential that agree to about 1e-12 relative on a 12-node graph, so 1e-9.
  expect_reference(centrality_communicability_betweenness(undirected),
                   centiserve::communibet(ref_igraph(undirected), normalized = TRUE), reference = "centiserve",
                   covers = c("centrality()", "centrality_communicability_betweenness()"),
                   quantity = "communicability betweenness, undirected", tolerance = 1e-9, measure = "relative")
})

test_that("DMNC equals its definition (Lin et al. 2008)", {
  expect_reference(centrality_dmnc(undirected, dmnc_epsilon = 1.67), ref_dmnc(undirected, epsilon = 1.67),
                   reference = "independent: DMNC from its definition, Lin et al. 2008, base R",
                   covers = c("centrality()", "centrality_dmnc()"), quantity = "dmnc epsilon 1.67, undirected",
                   tolerance = exact)
  expect_reference(centrality_dmnc(biconnected), ref_dmnc(biconnected, epsilon = 1.7),
                   reference = "independent: DMNC from its definition, Lin et al. 2008, base R",
                   covers = c("centrality()", "centrality_dmnc()"), quantity = "dmnc epsilon 1.7, denser undirected",
                   tolerance = exact)
})

test_that("SALSA authority scores equal the in-degree identity of Lempel and Moran (2001)", {
  # On a graph whose authority graph is connected (here: strongly connected), the stationary authority score of a
  # node is proportional to its in-degree; cograph scales the scores to a maximum of 1.
  in_degree <- colSums(directed)
  expect_reference(centrality_salsa(directed), in_degree / max(in_degree),
                   reference = "independent: SALSA authority = in-degree share, Lempel and Moran 2001",
                   covers = c("centrality()", "centrality_salsa()"), quantity = "salsa authority, directed",
                   tolerance = 1e-10)
})

test_that("centroid value equals its definition (Wuchty and Stadler 2003)", {
  # The definition takes the minimum of f(v, i) over i != v. cograph 2.7.18 and earlier included i = v (f(v, v) = 0,
  # a loop bug of centiserve::centroid()), which capped the scores at 0 (N4 here); corrected in 2.7.19.
  expect_reference(centrality_centroid(undirected), ref_centroid(undirected),
                   reference = "independent: centroid value from its definition, Wuchty and Stadler 2003, base R",
                   covers = c("centrality()", "centrality_centroid()"), quantity = "centroid, undirected",
                   tolerance = exact)
})
