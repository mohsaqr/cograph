# centrality() and its wrappers for the greedy seed-selection heuristics of the Centrality Zoo, DegreeDiscountIC
# and SingleDiscount (Chen, Wang & Yang 2009). The selection is replayed from its definition in base R
# (helper-zoo-a.R): pick the node of largest discounted degree, lowest index on a tie, discount its neighbours,
# repeat. The node picked at step s scores (n - s + 1) / n, as cograph reports the selection order.

defined <- "independent: the selection rule written in base R (helper-zoo-a.R)"
exact <- 1e-10

star5 <- za_star(5)
bridge6 <- za_bridge6()
karate <- ref_karate()
connected <- ref_connected_graph(10, density = 0.3, seed = 21L)

test_that("DegreeDiscountIC replays the selection dd = d - 2t - (d - t) t p, p = 0.01", {
  expect_reference(centrality_degree_discount(star5), za_degree_discount(star5), reference = defined, covers = c("centrality()", "centrality_degree_discount()"),
                   quantity = "degree_discount, star of 5", tolerance = exact)
  expect_reference(centrality_degree_discount(bridge6), za_degree_discount(bridge6), reference = defined,
                   covers = c("centrality()", "centrality_degree_discount()"), quantity = "degree_discount, two triangles", tolerance = exact)
  expect_reference(centrality_degree_discount(connected), za_degree_discount(connected), reference = defined,
                   covers = c("centrality()", "centrality_degree_discount()"), quantity = "degree_discount, connected random graph", tolerance = exact)
  expect_reference(centrality_degree_discount(karate), za_degree_discount(karate), reference = defined,
                   covers = c("centrality()", "centrality_degree_discount()"), quantity = "degree_discount, karate", tolerance = exact)
  # By hand on the two triangles: C (degree 3, before D); then E (2 beats D's 3 - 2 - 2 * 0.01 = 0.98); then A
  # (A, B, F at -0.01, A first); F (-0.01 beats B -2, D -1.02); B (-2 beats D -3); D.
  expect_reference(centrality_degree_discount(bridge6), c(C = 6, E = 5, A = 4, F = 3, B = 2, D = 1) / 6,
                   reference = defined, covers = c("centrality()", "centrality_degree_discount()"), quantity = "degree_discount, two triangles, by hand",
                   tolerance = exact)
})

test_that("SingleDiscount replays the selection dd = d - t", {
  expect_reference(centrality_single_discount(star5), za_degree_discount(star5, single = TRUE), reference = defined,
                   covers = c("centrality()", "centrality_single_discount()"), quantity = "single_discount, star of 5", tolerance = exact)
  expect_reference(centrality_single_discount(bridge6), za_degree_discount(bridge6, single = TRUE),
                   reference = defined, covers = c("centrality()", "centrality_single_discount()"), quantity = "single_discount, two triangles", tolerance = exact)
  expect_reference(centrality_single_discount(connected), za_degree_discount(connected, single = TRUE),
                   reference = defined, covers = c("centrality()", "centrality_single_discount()"), quantity = "single_discount, connected random graph",
                   tolerance = exact)
  expect_reference(centrality_single_discount(karate), za_degree_discount(karate, single = TRUE), reference = defined,
                   covers = c("centrality()", "centrality_single_discount()"), quantity = "single_discount, karate", tolerance = exact)
})
