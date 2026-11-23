# centrality() on tna objects against tna::centralities(). tna is the owner's package, but by the owner's ruling
# (2026-11-21, verify.json `references`) it is the gold standard and counts as open-source: cograph converts its
# objects and does not call it for these computations, so the comparisons are counted as outside evidence.
# Moved from tests/testthat/test-centrality.R.
#
# tna_network (auto for a tna object) switches cograph to tna's conventions: diffusion by the power series of
# the transition matrix without loops, min-max scaled, and Onnela's weighted clustering for transitivity.

transitions <- matrix(c(0.4, 0.3, 0.3,
                        0.2, 0.5, 0.3,
                        0.1, 0.2, 0.7), 3, 3, byrow = TRUE, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

test_that("diffusion under tna conventions equals tna's Diffusion", {
  skip_without_reference("tna")
  model <- tna::tna(transitions)
  # tna >= 1.5.8 min-max scales Diffusion whatever `normalize`; older versions return the raw sums, so
  # `normalize = TRUE` is asked for.
  diffusion <- ref_named(tna::centralities(model, measures = "Diffusion", normalize = TRUE)$Diffusion, transitions)
  expect_reference(centrality(model, measures = "diffusion"), ref_nodes(diffusion_all = diffusion),
                   reference = "tna", covers = "centrality()", quantity = "diffusion, tna object", tolerance = 1e-12,
                   key = "node")
  expect_reference(centrality(transitions, measures = "diffusion", diffusion_method = "power_series", loops = FALSE),
                   ref_nodes(diffusion_all = diffusion), reference = "tna", covers = "centrality()",
                   quantity = "diffusion, matrix, power_series without loops", tolerance = 1e-12, key = "node")
  expect_reference(centrality(transitions, measures = "diffusion", tna_network = TRUE), ref_nodes(diffusion_all = diffusion),
                   reference = "tna", covers = "centrality()", quantity = "diffusion, matrix, tna_network = TRUE",
                   tolerance = 1e-12, key = "node")
  # Negative control: cograph's default (Kandhway and Kuri's degree diffusion) is a different measure.
  expect_differs(centrality(transitions, measures = "diffusion"), ref_nodes(diffusion_all = diffusion),
                 reference = "tna", covers = "centrality()", quantity = "diffusion, matrix, default kandhway_kuri",
                 tolerance = 1e-3, key = "node")
})

test_that("transitivity under tna conventions equals tna's Clustering", {
  skip_without_reference("tna")
  model <- tna::tna(transitions)
  clustering <- ref_named(tna::centralities(model, measures = "Clustering")$Clustering, transitions)
  expect_reference(centrality(model, measures = "transitivity"), ref_nodes(transitivity = clustering),
                   reference = "tna", covers = "centrality()", quantity = "transitivity, tna object", tolerance = 1e-12,
                   key = "node")
  expect_reference(centrality(transitions, measures = "transitivity", transitivity_type = "onnela"),
                   ref_nodes(transitivity = clustering), reference = "tna", covers = "centrality()",
                   quantity = "transitivity, matrix, onnela", tolerance = 1e-12, key = "node")
  expect_reference(centrality(transitions, measures = "transitivity", tna_network = TRUE),
                   ref_nodes(transitivity = clustering), reference = "tna", covers = "centrality()",
                   quantity = "transitivity, matrix, tna_network = TRUE", tolerance = 1e-12, key = "node")
  # Negative control: the default Watts-Strogatz coefficient is unweighted and differs.
  expect_differs(centrality(transitions, measures = "transitivity"), ref_nodes(transitivity = clustering),
                 reference = "tna", covers = "centrality()", quantity = "transitivity, matrix, default local",
                 tolerance = 1e-3, key = "node")
})
