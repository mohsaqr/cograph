# centrality() and its wrappers for the distance-distribution and dimension measures of the Centrality Zoo:
# distance entropy, local dimension, local information dimensionality, local dimension at a fixed radius, fuzzy
# local dimension and local volume dimension. Each is compared with its definition written in base R
# (helper-zoo-a.R: ring sizes from powers of the adjacency matrix, slopes by least squares) and, where the
# source paper prints a worked example, with the printed numbers.
#
# Graphs: small named graphs built in helper-zoo-a.R, Zachary's karate club, and seeded random graphs from
# helper-reference.R (one of them, `scattered`, has isolated nodes, whose scores are NaN on both sides).

defined <- "independent: the measure's definition written in base R (helper-zoo-a.R)"
exact <- 1e-10

path4 <- za_path(4)
path5 <- za_path(5)
star5 <- za_star(5)
karate <- ref_karate()
connected <- ref_connected_graph(10, density = 0.3, seed = 21L)
scattered <- ref_graph(14, density = 0.2, seed = 30L)
chain3 <- za_graph(c(1, 2, 2, 3), 3, directed = TRUE, names = LETTERS[1:3])

# Wen & Deng (2019), Example 2.1 / Fig. 2: node 1 is the centre of rings of 4, 5, 4 and 4 nodes, each ring
# hanging from the first node of the previous one.
ring_sizes <- c(4, 5, 4, 4)
ring_nodes <- split(seq_len(18)[-1L], rep(seq_along(ring_sizes), ring_sizes))
layered <- za_graph(c(rbind(1, ring_nodes[[1]]), rbind(ring_nodes[[1]][[1]], ring_nodes[[2]]),
                      rbind(ring_nodes[[2]][[1]], ring_nodes[[3]]), rbind(ring_nodes[[3]][[1]], ring_nodes[[4]])), 18)

# Wen & Jiang (2019), Fig. 3: the kite network (1 Beverly, 2 Andre, 3 Carol, 4 Fernando, 5 Garth, 6 Ed,
# 7 Diane, 8 Heather, 9 Ike, 10 Jane).
kite <- za_graph(c(1, 2, 1, 7, 1, 6, 1, 5, 2, 3, 2, 7, 2, 4, 3, 7, 3, 4, 4, 7, 4, 5, 4, 8, 5, 7, 5, 6, 5, 8,
                   6, 7, 8, 9, 9, 10), 10)

test_that("distance entropy equals the entropy of the ring sizes (Stella & De Domenico 2018)", {
  expect_reference(centrality_distance_entropy(path4), za_distance_entropy(za_hops(path4)), reference = defined,
                   covers = c("centrality()", "centrality_distance_entropy()"), quantity = "distance_entropy, path of 4", tolerance = exact)
  expect_reference(centrality_distance_entropy(star5), za_distance_entropy(za_hops(star5)), reference = defined,
                   covers = c("centrality()", "centrality_distance_entropy()"), quantity = "distance_entropy, star of 5", tolerance = exact)
  expect_reference(centrality_distance_entropy(karate), za_distance_entropy(za_hops(karate)), reference = defined,
                   covers = c("centrality()", "centrality_distance_entropy()"), quantity = "distance_entropy, karate", tolerance = exact)
  expect_reference(centrality_distance_entropy(scattered), za_distance_entropy(za_hops(scattered)),
                   reference = defined, covers = c("centrality()", "centrality_distance_entropy()"), quantity = "distance_entropy, graph with isolates",
                   tolerance = exact)
  # Out-distances are the rows of the distance matrix, in-distances its columns.
  expect_reference(centrality_distance_entropy(chain3, mode = "out"), za_distance_entropy(za_hops(chain3)),
                   reference = defined, covers = c("centrality()", "centrality_distance_entropy()"), quantity = "distance_entropy, mode out, directed chain",
                   tolerance = exact)
  expect_reference(centrality_distance_entropy(chain3, mode = "in"), za_distance_entropy(t(za_hops(chain3))),
                   reference = defined, covers = c("centrality()", "centrality_distance_entropy()"), quantity = "distance_entropy, mode in, directed chain",
                   tolerance = exact)
  # By hand on the path A-B-C-D: an end sees distances 1, 2, 3 once each (entropy 1); an inner node sees
  # 1, 1, 2, so p = (2/3, 1/3) over a span of two distances.
  inner <- -(2 / 3 * log(2 / 3) + 1 / 3 * log(1 / 3)) / log(2)
  expect_reference(centrality_distance_entropy(path4), c(A = 1, B = inner, C = inner, D = 1), reference = defined,
                   covers = c("centrality()", "centrality_distance_entropy()"), quantity = "distance_entropy, path of 4, by hand", tolerance = exact)
})

test_that("local dimension reproduces Wen & Deng (2019), Example 2.1", {
  # Printed to four decimals (0.9231): half a unit in the last place, 5e-5.
  expect_reference(centrality_local_dimension(layered)[["v1"]], 0.9231,
                   reference = "published: Wen & Deng 2019, Example 2.1 (Fig. 2)",
                   covers = c("centrality()", "centrality_local_dimension()"),
                   quantity = "local_dimension, centre of rings 4, 5, 4, 4", tolerance = 5e-5)
})

test_that("local dimension equals the slope of log ball size on log radius (Silva & Costa 2013)", {
  expect_reference(centrality_local_dimension(layered), za_local_dimension(za_hops(layered)), reference = defined,
                   covers = c("centrality()", "centrality_local_dimension()"), quantity = "local_dimension, layered rings", tolerance = exact)
  # The star's hub has one radius: the discretised derivative r n(1) / B(1) = 4 / 5.
  expect_reference(centrality_local_dimension(star5), za_local_dimension(za_hops(star5)), reference = defined,
                   covers = c("centrality()", "centrality_local_dimension()"), quantity = "local_dimension, star of 5", tolerance = exact)
  expect_reference(centrality_local_dimension(karate), za_local_dimension(za_hops(karate)), reference = defined,
                   covers = c("centrality()", "centrality_local_dimension()"), quantity = "local_dimension, karate", tolerance = exact)
  expect_reference(centrality_local_dimension(scattered), za_local_dimension(za_hops(scattered)), reference = defined,
                   covers = c("centrality()", "centrality_local_dimension()"), quantity = "local_dimension, graph with isolates", tolerance = exact)
})

test_that("local information dimensionality equals its definition (Wen & Deng 2020)", {
  expect_reference(centrality_local_information_dimension(path5), za_local_information_dimension(za_hops(path5)),
                   reference = defined, covers = c("centrality()", "centrality_local_information_dimension()"), quantity = "local_information_dimension, path of 5",
                   tolerance = exact)
  expect_reference(centrality_local_information_dimension(star5), za_local_information_dimension(za_hops(star5)),
                   reference = defined, covers = c("centrality()", "centrality_local_information_dimension()"), quantity = "local_information_dimension, star of 5",
                   tolerance = exact)
  expect_reference(centrality_local_information_dimension(karate), za_local_information_dimension(za_hops(karate)),
                   reference = defined, covers = c("centrality()", "centrality_local_information_dimension()"), quantity = "local_information_dimension, karate",
                   tolerance = exact)
  # By hand on the path A-...-E (N = 5): the middle node has one box, (1 + ln(3/5)) n(1) / N with n(1) = 2;
  # an end has boxes of 2 and 3 nodes at l = 1, 2, and the measure is minus the slope of -p ln p on ln l.
  p <- c(2, 3) / 5
  end <- (p[[2]] * log(p[[2]]) - p[[1]] * log(p[[1]])) / log(2)
  expect_reference(centrality_local_information_dimension(path5)[c("A", "C", "E")],
                   c(A = end, C = (1 + log(3 / 5)) * 2 / 5, E = end), reference = defined, covers = c("centrality()", "centrality_local_information_dimension()"),
                   quantity = "local_information_dimension, path of 5, by hand", tolerance = exact)
})

test_that("local dimension at a fixed radius equals r n(r) / B(r) (Silva & Costa 2013, eq. 4)", {
  path9 <- za_path(9)
  expect_reference(centrality_local_dimension_fixed(path9, ld_radius = 1), za_local_dimension_fixed(za_hops(path9), 1),
                   reference = defined, covers = c("centrality()", "centrality_local_dimension_fixed()"), quantity = "local_dimension_fixed, radius 1, path of 9",
                   tolerance = exact)
  expect_reference(centrality_local_dimension_fixed(path9, ld_radius = 3), za_local_dimension_fixed(za_hops(path9), 3),
                   reference = defined, covers = c("centrality()", "centrality_local_dimension_fixed()"), quantity = "local_dimension_fixed, radius 3, path of 9",
                   tolerance = exact)
  expect_reference(centrality_local_dimension_fixed(star5), za_local_dimension_fixed(za_hops(star5)),
                   reference = defined, covers = c("centrality()", "centrality_local_dimension_fixed()"), quantity = "local_dimension_fixed, radius 2, star of 5",
                   tolerance = exact)
  expect_reference(centrality_local_dimension_fixed(karate), za_local_dimension_fixed(za_hops(karate)),
                   reference = defined, covers = c("centrality()", "centrality_local_dimension_fixed()"), quantity = "local_dimension_fixed, radius 2, karate",
                   tolerance = exact)
  # By hand: the centre of a path of 9 at radius r has a ring of 2 and a ball of 2r + 1.
  expect_reference(vapply(1:4, \(r) centrality_local_dimension_fixed(path9, ld_radius = r)[["E"]], numeric(1)),
                   (1:4) * 2 / (2 * (1:4) + 1), reference = defined, covers = c("centrality()", "centrality_local_dimension_fixed()"),
                   quantity = "local_dimension_fixed, centre of a path of 9, radius 1 to 4, by hand", tolerance = exact)
})

test_that("fuzzy local dimension reproduces Wen & Jiang (2019), Table 1", {
  # Table 1 prints four decimals and truncates them toward zero (all ten values truncate to the printed ones;
  # four of them would round to another digit, e.g. 0.30160 printed 0.3015). The true value then lies within
  # one unit of the printed one, away from zero: the reference is the middle of that interval, printed + 5e-5
  # (minus for the negative value), and the tolerance its half-width, 5e-5.
  printed <- c(0.3609, 0.3609, 0.3015, 0.4554, 0.4554, 0.3015, 0.4442, 0.0760, 0.0375, -0.1163)
  expect_reference(centrality_fuzzy_local_dimension(kite), printed + sign(printed) * 5e-5,
                   reference = "published: Wen & Jiang 2019, Table 1",
                   covers = c("centrality()", "centrality_fuzzy_local_dimension()"),
                   quantity = "fuzzy_local_dimension, kite network", tolerance = 5e-5)
})

test_that("fuzzy local dimension ranks the karate club as Wen & Jiang (2019), Table 6", {
  # The top ten nodes in the printed order; nodes 28 and 31 tie and the paper lists the lower one first, so the
  # ranking breaks ties by node number.
  scores <- centrality_fuzzy_local_dimension(karate)
  expect_reference(head(order(-scores, seq_along(scores)), 10), c(1, 34, 33, 3, 2, 32, 24, 28, 31, 30),
                   reference = "published: Wen & Jiang 2019, Table 6",
                   covers = c("centrality()", "centrality_fuzzy_local_dimension()"),
                   quantity = "fuzzy_local_dimension, karate, top ten nodes in order", tolerance = 0)
})

test_that("fuzzy local dimension equals its definition (Wen & Jiang 2019)", {
  expect_reference(centrality_fuzzy_local_dimension(kite), za_fuzzy_local_dimension(za_hops(kite)), reference = defined,
                   covers = c("centrality()", "centrality_fuzzy_local_dimension()"), quantity = "fuzzy_local_dimension, kite network", tolerance = exact)
  expect_reference(centrality_fuzzy_local_dimension(karate), za_fuzzy_local_dimension(za_hops(karate)),
                   reference = defined, covers = c("centrality()", "centrality_fuzzy_local_dimension()"), quantity = "fuzzy_local_dimension, karate", tolerance = exact)
  # The star's hub reaches everyone in one step: one radius, no slope (NaN on both sides).
  expect_reference(centrality_fuzzy_local_dimension(star5), za_fuzzy_local_dimension(za_hops(star5)),
                   reference = defined, covers = c("centrality()", "centrality_fuzzy_local_dimension()"), quantity = "fuzzy_local_dimension, star of 5", tolerance = exact)
})

test_that("local volume dimension equals the slope of log volume on log radius (Li & Deng 2021)", {
  expect_reference(centrality_local_volume_dimension(path5), za_local_volume_dimension(path5), reference = defined,
                   covers = c("centrality()", "centrality_local_volume_dimension()"), quantity = "local_volume_dimension, path of 5", tolerance = exact)
  expect_reference(centrality_local_volume_dimension(star5), za_local_volume_dimension(star5), reference = defined,
                   covers = c("centrality()", "centrality_local_volume_dimension()"), quantity = "local_volume_dimension, star of 5", tolerance = exact)
  expect_reference(centrality_local_volume_dimension(karate), za_local_volume_dimension(karate), reference = defined,
                   covers = c("centrality()", "centrality_local_volume_dimension()"), quantity = "local_volume_dimension, karate", tolerance = exact)
  # By hand on the path (degrees 1, 2, 2, 2, 1): an end has volumes 3, 5, 7, 8 at l = 1 .. 4, the middle 6, 8.
  slope <- \(y) za_slope(log(seq_along(y)), log(y))
  expect_reference(centrality_local_volume_dimension(path5)[c("A", "C")], c(A = slope(c(3, 5, 7, 8)), C = slope(c(6, 8))),
                   reference = defined, covers = c("centrality()", "centrality_local_volume_dimension()"), quantity = "local_volume_dimension, path of 5, by hand",
                   tolerance = exact)
})
