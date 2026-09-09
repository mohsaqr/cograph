# Path-based centralities depend on the ordering of path lengths, not their
# scale, so multiplying every weight by a constant must leave betweenness,
# stress and the closeness family's ranking unchanged. igraph fails this at
# tiny scales because of its absolute epsilon; the native kernels must not.

test_that("betweenness and stress are invariant to weight scale down to 1e-18", {
  big <- test_network("weights_1e9")
  small <- test_network("weights_1e-9")
  expect_equal(unname(small), unname(big * 1e-18))
  for (ms in c("betweenness", "stress", "load")) {
    # one invariant per measure
    a <- centrality(big, measures = ms)[[2L]]
    b <- centrality(small, measures = ms)[[2L]]
    expect_equal(a, b, tolerance = 1e-12, info = ms)
  }
})

test_that("closeness and harmonic scale exactly with the weights", {
  big <- test_network("weights_1e9")
  small <- test_network("weights_1e-9")
  for (ms in c("closeness", "harmonic")) {
    # distances scale by 1e-18, so the reciprocal measures scale by 1e18
    a <- centrality(big, measures = ms)[[2L]]
    b <- centrality(small, measures = ms)[[2L]]
    expect_equal(b, a * 1e18, tolerance = 1e-10, info = ms)
  }
})
