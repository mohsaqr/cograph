# centrality() and its wrappers for the Centrality Zoo measures built on information, games and spreading
# orders: access and hide information, rumor centrality, entropy variation and the three Shapley value games.
# Each is compared with its definition written in base R (helper-zoo-a.R), and rumor centrality also with the
# value printed in its source paper.

defined <- "independent: the measure's definition written in base R (helper-zoo-a.R)"
exact <- 1e-10

star5 <- za_star(5)
path5 <- za_path(5)
karate <- ref_karate()
connected <- ref_connected_graph(10, density = 0.3, seed = 21L)

# Shah & Zaman (2011), Fig. 5: the tree with edges 1-2, 1-3, 2-4, 2-5.
fig5 <- za_graph(c(1, 2, 1, 3, 2, 4, 2, 5), 5)

test_that("access and hide information are the row and column means of the search information", {
  # Rosvall et al. (2005): S(i, j) = -log2 of the probability of following a shortest i-j path when each step
  # picks one of the links not just used; access averages S(i, .) over the N nodes, hide averages S(., i).
  on_star <- za_search_information(star5)
  on_connected <- za_search_information(connected)
  on_karate <- za_search_information(karate)
  expect_reference(centrality_access_information(star5), rowMeans(on_star), reference = defined, covers = c("centrality()", "centrality_access_information()"),
                   quantity = "access_information, star of 5", tolerance = exact)
  expect_reference(centrality_hide_information(star5), colMeans(on_star), reference = defined, covers = c("centrality()", "centrality_hide_information()"),
                   quantity = "hide_information, star of 5", tolerance = exact)
  expect_reference(centrality_access_information(connected), rowMeans(on_connected), reference = defined,
                   covers = c("centrality()", "centrality_access_information()"), quantity = "access_information, connected random graph", tolerance = exact)
  expect_reference(centrality_hide_information(connected), colMeans(on_connected), reference = defined,
                   covers = c("centrality()", "centrality_hide_information()"), quantity = "hide_information, connected random graph", tolerance = exact)
  expect_reference(centrality_access_information(karate), rowMeans(on_karate), reference = defined,
                   covers = c("centrality()", "centrality_access_information()"), quantity = "access_information, karate", tolerance = exact)
  expect_reference(centrality_hide_information(karate), colMeans(on_karate), reference = defined,
                   covers = c("centrality()", "centrality_hide_information()"), quantity = "hide_information, karate", tolerance = exact)
  # By hand on the star K(1,4): hub to leaf costs log2(4) = 2 bits, leaf to hub 0, leaf to leaf log2(3).
  expect_reference(centrality_access_information(star5),
                   c(A = 4 * 2 / 5, B = 3 * log2(3) / 5, C = 3 * log2(3) / 5, D = 3 * log2(3) / 5, E = 3 * log2(3) / 5),
                   reference = defined, covers = c("centrality()", "centrality_access_information()"), quantity = "access_information, star of 5, by hand",
                   tolerance = exact)
  expect_reference(centrality_hide_information(star5),
                   c(A = 0, B = (2 + 3 * log2(3)) / 5, C = (2 + 3 * log2(3)) / 5, D = (2 + 3 * log2(3)) / 5,
                     E = (2 + 3 * log2(3)) / 5),
                   reference = defined, covers = c("centrality()", "centrality_hide_information()"), quantity = "hide_information, star of 5, by hand",
                   tolerance = exact)
})

test_that("rumor centrality reproduces Shah & Zaman (2011), Fig. 5", {
  # The paper prints R(1, G) = 5! / (5 * 3) = 8 and lists the eight permitted orders: an exact count, compared
  # through exp() of the returned logarithm, so the tolerance only covers that round trip.
  expect_reference(exp(centrality_rumor(fig5))[["v1"]], 8, reference = "published: Shah & Zaman 2011, Fig. 5",
                   covers = c("centrality()", "centrality_rumor()"), quantity = "rumor, node 1 of the Fig. 5 tree",
                   tolerance = 1e-10, measure = "relative")
})

test_that("rumor centrality on a tree equals n! over the product of the subtree sizes", {
  # Shah & Zaman (2011), eq. 10, on the log scale cograph returns.
  expect_reference(centrality_rumor(fig5), za_rumor_log(fig5), reference = defined, covers = c("centrality()", "centrality_rumor()"),
                   quantity = "rumor, Fig. 5 tree", tolerance = exact)
  expect_reference(centrality_rumor(path5), za_rumor_log(path5), reference = defined, covers = c("centrality()", "centrality_rumor()"),
                   quantity = "rumor, path of 5", tolerance = exact)
  tree9 <- za_random_tree(9, seed = 3L)
  tree12 <- za_random_tree(12, seed = 4L)
  expect_reference(centrality_rumor(tree9), za_rumor_log(tree9), reference = defined, covers = c("centrality()", "centrality_rumor()"),
                   quantity = "rumor, random tree of 9", tolerance = exact)
  expect_reference(centrality_rumor(tree12), za_rumor_log(tree12), reference = defined, covers = c("centrality()", "centrality_rumor()"),
                   quantity = "rumor, random tree of 12", tolerance = exact)
  # By hand on the path of 5: the ends have one spreading order, the middle node 4! / (2 * 2) = 6.
  expect_reference(exp(centrality_rumor(path5)), c(A = 1, B = 4, C = 6, D = 4, E = 1), reference = defined,
                   covers = c("centrality()", "centrality_rumor()"), quantity = "rumor, path of 5, spreading orders by hand", tolerance = 1e-10,
                   measure = "relative")
})

test_that("entropy variation is the entropy lost when the node is deleted (Ai 2017)", {
  expect_reference(centrality_entropy_variation(star5), za_entropy_variation(star5), reference = defined,
                   covers = c("centrality()", "centrality_entropy_variation()"), quantity = "entropy_variation_degree, star of 5", tolerance = exact)
  expect_reference(centrality_entropy_variation(connected), za_entropy_variation(connected), reference = defined,
                   covers = c("centrality()", "centrality_entropy_variation()"), quantity = "entropy_variation_degree, connected random graph", tolerance = exact)
  expect_reference(centrality_entropy_variation(path5, of = "betweenness"), za_entropy_variation(path5, "betweenness"),
                   reference = defined, covers = c("centrality()", "centrality_entropy_variation()"), quantity = "entropy_variation_betweenness, path of 5",
                   tolerance = exact)
  expect_reference(centrality_entropy_variation(connected, of = "betweenness"),
                   za_entropy_variation(connected, "betweenness"), reference = defined, covers = c("centrality()", "centrality_entropy_variation()"),
                   quantity = "entropy_variation_betweenness, connected random graph", tolerance = exact)
  # Directed chain A -> B -> C: out-degrees (1, 1, 0), in-degrees (0, 1, 1). Every deletion leaves one degree
  # 1 and one 0 (entropy 0), so every node loses the whole entropy log 2.
  chain <- za_graph(c(1, 2, 2, 3), 3, directed = TRUE, names = LETTERS[1:3])
  expect_reference(centrality_entropy_variation(chain, mode = "out"), za_entropy_variation(chain), reference = defined,
                   covers = c("centrality()", "centrality_entropy_variation()"), quantity = "entropy_variation_degree, mode out, directed chain", tolerance = exact)
  expect_reference(centrality_entropy_variation(chain, mode = "in"), za_entropy_variation(t(chain)),
                   reference = defined, covers = c("centrality()", "centrality_entropy_variation()"), quantity = "entropy_variation_degree, mode in, directed chain",
                   tolerance = exact)
})

test_that("the Shapley value games equal the closed forms of Michalak et al. (2013)", {
  expect_reference(centrality_shapley_game1(star5), za_shapley(star5, 1), reference = defined, covers = c("centrality()", "centrality_shapley_game1()"),
                   quantity = "shapley_game1, star of 5", tolerance = exact)
  expect_reference(centrality_shapley_game1(connected), za_shapley(connected, 1), reference = defined, covers = c("centrality()", "centrality_shapley_game1()"),
                   quantity = "shapley_game1, connected random graph", tolerance = exact)
  expect_reference(centrality_shapley_game1(karate), za_shapley(karate, 1), reference = defined, covers = c("centrality()", "centrality_shapley_game1()"),
                   quantity = "shapley_game1, karate", tolerance = exact)
  expect_reference(centrality_shapley_game2(star5), za_shapley(star5, 2), reference = defined, covers = c("centrality()", "centrality_shapley_game2()"),
                   quantity = "shapley_game2, k = 2, star of 5", tolerance = exact)
  expect_reference(centrality_shapley_game2(connected, shapley_k = 3), za_shapley(connected, 2, k = 3),
                   reference = defined, covers = c("centrality()", "centrality_shapley_game2()"), quantity = "shapley_game2, k = 3, connected random graph",
                   tolerance = exact)
  expect_reference(centrality_shapley_game2(karate), za_shapley(karate, 2), reference = defined, covers = c("centrality()", "centrality_shapley_game2()"),
                   quantity = "shapley_game2, k = 2, karate", tolerance = exact)
  expect_reference(centrality_shapley_game3(connected), za_shapley(connected, 3), reference = defined, covers = c("centrality()", "centrality_shapley_game3()"),
                   quantity = "shapley_game3, cutoff 2, connected random graph", tolerance = exact)
  expect_reference(centrality_shapley_game3(connected, shapley_cutoff = 3), za_shapley(connected, 3, cutoff = 3),
                   reference = defined, covers = c("centrality()", "centrality_shapley_game3()"), quantity = "shapley_game3, cutoff 3, connected random graph",
                   tolerance = exact)
  expect_reference(centrality_shapley_game3(karate), za_shapley(karate, 3), reference = defined, covers = c("centrality()", "centrality_shapley_game3()"),
                   quantity = "shapley_game3, cutoff 2, karate", tolerance = exact)
  # By hand on the star, game 1: the hub gets 1/5 for itself and 1/2 from each leaf; a leaf 1/2 and 1/5.
  expect_reference(centrality_shapley_game1(star5),
                   c(A = 1 / 5 + 4 / 2, B = 1 / 2 + 1 / 5, C = 1 / 2 + 1 / 5, D = 1 / 2 + 1 / 5, E = 1 / 2 + 1 / 5),
                   reference = defined, covers = c("centrality()", "centrality_shapley_game1()"), quantity = "shapley_game1, star of 5, by hand", tolerance = exact)
})
