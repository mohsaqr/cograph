# centrality() and its wrappers for the key-player measures of keyplayer (An and Liu 2016): fragmentation (Borgatti
# 2006), M-reach degree (the number of nodes within M steps: cograph's k-reach), M-reach closeness (inverse distances
# within M steps over n - 1: cograph's harmonic closeness with a cutoff, normalised) and diffusion (Banerjee et al.
# 2013: walks of length 1 to T leaving the node: cograph's diffusion centrality with q = 1). keyplayer computes
# distances with sna::geodist() (large = FALSE) on the unweighted matrix (binary = TRUE).

undirected <- ref_connected_graph(12, density = 0.3, seed = 21L)
directed <- ref_connected_graph(12, density = 0.25, seed = 22L, directed = TRUE)
# A graph with a cut vertex and a pendant chain, where removing a node can disconnect it.
sparse <- ref_connected_graph(12, density = 0.1, seed = 27L)

exact <- 1e-10

test_that("fragmentation equals keyplayer's fragment()", {
  skip_without_reference(c("keyplayer", "sna"))
  expect_reference(centrality_fragmentation(undirected),
                   ref_named(keyplayer::fragment(undirected, binary = TRUE, large = FALSE), undirected),
                   reference = "keyplayer", covers = c("centrality()", "centrality_fragmentation()"),
                   quantity = "fragmentation, undirected", tolerance = exact)
  expect_reference(centrality_fragmentation(sparse),
                   ref_named(keyplayer::fragment(sparse, binary = TRUE, large = FALSE), sparse),
                   reference = "keyplayer", covers = c("centrality()", "centrality_fragmentation()"),
                   quantity = "fragmentation, sparse undirected", tolerance = exact)
})

test_that("k-reach equals keyplayer's M-reach degree", {
  skip_without_reference(c("keyplayer", "sna"))
  reach_undirected <- keyplayer::mreach.degree(undirected, M = 2, binary = TRUE, large = FALSE)
  reach_directed <- keyplayer::mreach.degree(directed, M = 2, binary = TRUE, large = FALSE)
  expect_reference(centrality_kreach(undirected, k = 2), ref_named(reach_undirected[, "outdegree"], undirected),
                   reference = "keyplayer", covers = c("centrality()", "centrality_kreach()"),
                   quantity = "kreach k 2, undirected", tolerance = exact)
  expect_reference(centrality_kreach(directed, mode = "out", k = 2), ref_named(reach_directed[, "outdegree"], directed),
                   reference = "keyplayer", covers = c("centrality()", "centrality_kreach()"),
                   quantity = "kreach k 2, mode out, directed", tolerance = exact)
  expect_reference(centrality_kreach(directed, mode = "in", k = 2), ref_named(reach_directed[, "indegree"], directed),
                   reference = "keyplayer", covers = c("centrality()", "centrality_kreach()"),
                   quantity = "kreach k 2, mode in, directed", tolerance = exact)
})

test_that("harmonic closeness with a cutoff equals keyplayer's M-reach closeness", {
  skip_without_reference(c("keyplayer", "sna"))
  # keyplayer divides by (n - 1) times the largest inverse distance, which is 1 on a graph with a tie.
  closeness_undirected <- keyplayer::mreach.closeness(undirected, M = 2, binary = TRUE, large = FALSE)
  closeness_directed <- keyplayer::mreach.closeness(directed, M = 2, binary = TRUE, large = FALSE)
  expect_reference(centrality_harmonic(undirected, cutoff = 2, normalized = TRUE),
                   ref_named(closeness_undirected[, "outdegree"], undirected), reference = "keyplayer",
                   covers = c("centrality()", "centrality_harmonic()"), quantity = "harmonic, cutoff 2, normalised, undirected",
                   tolerance = exact)
  expect_reference(centrality_outharmonic(directed, cutoff = 2, normalized = TRUE),
                   ref_named(closeness_directed[, "outdegree"], directed), reference = "keyplayer",
                   covers = c("centrality()", "centrality_outharmonic()"),
                   quantity = "out-harmonic, cutoff 2, normalised, directed", tolerance = exact)
  expect_reference(centrality_inharmonic(directed, cutoff = 2, normalized = TRUE),
                   ref_named(closeness_directed[, "indegree"], directed), reference = "keyplayer",
                   covers = c("centrality()", "centrality_inharmonic()"),
                   quantity = "in-harmonic, cutoff 2, normalised, directed", tolerance = exact)
})

test_that("diffusion centrality with q = 1 equals keyplayer's diffusion()", {
  skip_without_reference(c("keyplayer", "matpow"))
  expect_reference(centrality_diffusion_centrality(undirected, diffusion_q = 1, diffusion_steps = 3),
                   ref_named(keyplayer::diffusion(undirected, T = 3), undirected), reference = "keyplayer",
                   covers = c("centrality()", "centrality_diffusion_centrality()"),
                   quantity = "diffusion centrality q 1, T 3, undirected", tolerance = exact, measure = "scaled")
  expect_reference(centrality_diffusion_centrality(directed, diffusion_q = 1, diffusion_steps = 4),
                   ref_named(keyplayer::diffusion(directed, T = 4), directed), reference = "keyplayer",
                   covers = c("centrality()", "centrality_diffusion_centrality()"),
                   quantity = "diffusion centrality q 1, T 4, directed", tolerance = exact, measure = "scaled")
})
