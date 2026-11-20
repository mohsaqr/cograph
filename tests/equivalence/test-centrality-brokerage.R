# The five Gould-Fernandez (1989) brokerage roles of centrality() and their wrappers, against sna::brokerage(), whose
# raw.nli table counts, for each node, the two-paths a -> v -> c it brokers by the groups of a, v and c:
# w_I coordinator, w_O itinerant, b_IO representative, b_OI gatekeeper, b_O liaison.

directed <- ref_graph(14, density = 0.25, seed = 41L, directed = TRUE)
groups <- rep(c("a", "b", "c"), length.out = 14L)
# Two groups of unequal size, and a denser graph with reciprocated ties.
dense <- ref_graph(12, density = 0.4, seed = 42L, directed = TRUE)
two_groups <- rep(c(1, 2), times = c(5L, 7L))

roles <- c(w_I = "brokerage_coordinator", w_O = "brokerage_itinerant", b_IO = "brokerage_representative",
           b_OI = "brokerage_gatekeeper", b_O = "brokerage_liaison")

# Counts of two-paths: exact integers.
exact <- 1e-10

test_that("the five brokerage roles equal sna's brokerage()$raw.nli", {
  skip_without_reference("sna")
  raw <- sna::brokerage(directed, cl = groups)$raw.nli
  expect_reference(centrality(directed, measures = unname(roles), membership = groups),
                   ref_nodes(brokerage_coordinator = raw[, "w_I"], brokerage_itinerant = raw[, "w_O"],
                             brokerage_representative = raw[, "b_IO"], brokerage_gatekeeper = raw[, "b_OI"],
                             brokerage_liaison = raw[, "b_O"]),
                   reference = "sna", covers = "centrality()", quantity = "brokerage, three groups", tolerance = exact,
                   key = "node")
  raw_dense <- sna::brokerage(dense, cl = two_groups)$raw.nli
  expect_reference(centrality(dense, measures = unname(roles), membership = two_groups),
                   ref_nodes(brokerage_coordinator = raw_dense[, "w_I"], brokerage_itinerant = raw_dense[, "w_O"],
                             brokerage_representative = raw_dense[, "b_IO"], brokerage_gatekeeper = raw_dense[, "b_OI"],
                             brokerage_liaison = raw_dense[, "b_O"]),
                   reference = "sna", covers = "centrality()", quantity = "brokerage, two groups, reciprocated ties",
                   tolerance = exact, key = "node")
})

test_that("each brokerage wrapper equals its column of sna's brokerage()$raw.nli", {
  skip_without_reference("sna")
  raw <- sna::brokerage(directed, cl = groups)$raw.nli
  expect_reference(centrality_brokerage_coordinator(directed, membership = groups), raw[, "w_I"], reference = "sna",
                   covers = c("centrality()", "centrality_brokerage_coordinator()"),
                   quantity = "brokerage coordinator (w_I), three groups", tolerance = exact)
  expect_reference(centrality_brokerage_itinerant(directed, membership = groups), raw[, "w_O"], reference = "sna",
                   covers = c("centrality()", "centrality_brokerage_itinerant()"),
                   quantity = "brokerage itinerant (w_O), three groups", tolerance = exact)
  expect_reference(centrality_brokerage_representative(directed, membership = groups), raw[, "b_IO"], reference = "sna",
                   covers = c("centrality()", "centrality_brokerage_representative()"),
                   quantity = "brokerage representative (b_IO), three groups", tolerance = exact)
  expect_reference(centrality_brokerage_gatekeeper(directed, membership = groups), raw[, "b_OI"], reference = "sna",
                   covers = c("centrality()", "centrality_brokerage_gatekeeper()"),
                   quantity = "brokerage gatekeeper (b_OI), three groups", tolerance = exact)
  expect_reference(centrality_brokerage_liaison(directed, membership = groups), raw[, "b_O"], reference = "sna",
                   covers = c("centrality()", "centrality_brokerage_liaison()"),
                   quantity = "brokerage liaison (b_O), three groups", tolerance = exact)
})
