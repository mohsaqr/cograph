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

test_that("distance-based closeness variants equal centiserve", {
  skip_without_reference(c("centiserve", "igraph"))
  graph <- ref_igraph(undirected)
  graph_directed <- ref_igraph(directed)
  expect_reference(centrality_radiality(undirected), centiserve::radiality(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_radiality()"), quantity = "radiality, undirected",
                   tolerance = exact)
  expect_reference(centrality_radiality(directed, mode = "out"), centiserve::radiality(graph_directed, mode = "out"),
                   reference = "centiserve", covers = c("centrality()", "centrality_radiality()"),
                   quantity = "radiality, mode out, directed", tolerance = exact)
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
  expect_reference(centrality_residual_closeness(undirected), centiserve::closeness.residual(graph),
                   reference = "centiserve", covers = c("centrality()", "centrality_residual_closeness()"),
                   quantity = "residual closeness, undirected", tolerance = exact)
  expect_reference(centrality_residual_closeness(directed, mode = "out"),
                   centiserve::closeness.residual(graph_directed, mode = "out"), reference = "centiserve",
                   covers = c("centrality()", "centrality_residual_closeness()"),
                   quantity = "residual closeness, mode out, directed", tolerance = exact)
  # Dangalchev's closeness is the same quantity as residual closeness under a second name.
  expect_reference(centrality_dangalchev(undirected), centiserve::closeness.residual(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_dangalchev()"), quantity = "dangalchev, undirected",
                   tolerance = exact)
  expect_reference(centrality_barycenter(undirected), centiserve::barycenter(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_barycenter()"), quantity = "barycenter, undirected",
                   tolerance = exact)
  expect_reference(centrality_average_distance(undirected), centiserve::averagedis(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_average_distance()"), quantity = "average distance, undirected",
                   tolerance = exact)
  expect_reference(centrality_average_distance(directed, mode = "out"),
                   centiserve::averagedis(graph_directed, mode = "out"), reference = "centiserve",
                   covers = c("centrality()", "centrality_average_distance()"),
                   quantity = "average distance, mode out, directed", tolerance = exact)
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
  expect_reference(centrality_lobby(undirected), centiserve::lobby(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_lobby()"), quantity = "lobby, undirected", tolerance = exact)
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
  expect_reference(centrality_clusterrank(undirected), centiserve::clusterrank(graph), reference = "centiserve",
                   covers = c("centrality()", "centrality_clusterrank()"), quantity = "clusterrank, undirected",
                   tolerance = exact)
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
  expect_reference(centrality_lobby(directed, mode = "out"), centiserve::lobby(graph, mode = "out"),
                   reference = "centiserve", covers = c("centrality()", "centrality_lobby()"),
                   quantity = "lobby, mode out, directed", tolerance = exact)
  expect_reference(centrality_leverage(directed, mode = "out"), centiserve::leverage(graph, mode = "out"),
                   reference = "centiserve", covers = c("centrality()", "centrality_leverage()"),
                   quantity = "leverage, mode out, directed", tolerance = exact)
  expect_reference(centrality_diffusion(directed, mode = "out"), centiserve::diffusion.degree(graph, mode = "out"),
                   reference = "centiserve", covers = c("centrality()", "centrality_diffusion()"),
                   quantity = "diffusion degree, mode out, directed", tolerance = exact)
  # Seen in the first run and reproduced from the definition: cograph's MNC takes the largest weakly connected
  # component of the out-neighbourhood, centiserve (and cograph's own DMNC) the largest strongly connected one.
  expect_reference(centrality_mnc(directed, mode = "out"), centiserve::mnc(graph, mode = "out"),
                   reference = "centiserve", covers = c("centrality()", "centrality_mnc()"),
                   quantity = "mnc, mode out, directed", tolerance = exact,
                   defect = paste("cograph centrality_mnc() on a directed graph takes the largest weakly connected",
                                  "component of the neighbourhood; MNC must use the largest strongly connected one",
                                  "(owner, 2026-11-22), as centiserve::mnc() (clusters(mode = 'strong')) and cograph's",
                                  "own dmnc do (N1 3 against 1, N4 5 against 1). centiserve agrees with the definition;",
                                  "FINDINGS.md"))
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
  # iterate; the error left at that stopping rule is of the order of 1e-4 of each score, so 1e-3 relative.
  expect_reference(centrality_leaderrank(directed), centiserve::leaderrank(ref_igraph(directed)),
                   reference = "centiserve", covers = c("centrality()", "centrality_leaderrank()"),
                   quantity = "leaderrank, directed", tolerance = 1e-3, measure = "relative")
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
  # cograph takes the minimum of f(v, i) over every node i, v included, where f(v, v) = 0; the definition takes it
  # over i != v. The two differ wherever a node is closer to more nodes than every other node is (N4 here).
  expect_reference(centrality_centroid(undirected), ref_centroid(undirected),
                   reference = "independent: centroid value from its definition, Wuchty and Stadler 2003, base R",
                   covers = c("centrality()", "centrality_centroid()"), quantity = "centroid, undirected",
                   tolerance = exact,
                   defect = paste("cograph centrality_centroid() takes the minimum of f(v, i) with i = v included",
                                  "(f(v, v) = 0), copying a loop bug of centiserve::centroid(), so no score exceeds 0;",
                                  "the definition of Wuchty and Stadler (2003) takes i != v (computed here in base R);",
                                  "FINDINGS.md"))
})
