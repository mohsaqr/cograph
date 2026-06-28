make_ego_adj <- function() {
  adj <- matrix(c(0, 1, 1, 0, 0,
                  1, 0, 1, 0, 0,
                  1, 1, 0, 1, 1,
                  0, 0, 1, 0, 1,
                  0, 0, 1, 1, 0), 5, 5, byrow = TRUE)
  rownames(adj) <- colnames(adj) <- LETTERS[1:5]
  adj
}

test_that("ego_networks returns one tidy row per ego", {
  skip_if_no_igraph()
  adj <- make_ego_adj()
  en <- ego_networks(adj)

  expect_s3_class(en, "cograph_ego_networks")
  expect_s3_class(en, "data.frame")
  expect_equal(nrow(en), 5)
  expect_identical(en$node, LETTERS[1:5])
  expect_true(all(c("size", "ego_ties", "ego_density", "alter_ties",
                    "alter_density", "effective_size", "constraint")
                  %in% names(en)))
})

test_that("ego_networks hub node has the largest size and lowest constraint", {
  skip_if_no_igraph()
  en <- ego_networks(make_ego_adj())
  # C is the hub: connected to A, B, D, E
  expect_equal(en$size[en$node == "C"], 4)
  expect_equal(which.max(en$size), which(en$node == "C"))
  expect_equal(which.min(en$constraint), which(en$node == "C"))
})

test_that("ego_networks effective_size/constraint match centrality()", {
  skip_if_no_igraph()
  adj <- make_ego_adj()
  en <- ego_networks(adj)
  es <- centrality(adj, measures = "effective_size")
  con <- centrality(adj, measures = "constraint")
  expect_equal(en$effective_size, es$effective_size)
  expect_equal(en$constraint, con$constraint)
})

test_that("ego_networks subsets and preserves requested node order", {
  skip_if_no_igraph()
  en <- ego_networks(make_ego_adj(), nodes = c("C", "A"))
  expect_identical(en$node, c("C", "A"))
})

test_that("ego_networks order > 1 sets Burt measures to NA", {
  skip_if_no_igraph()
  en <- ego_networks(make_ego_adj(), order = 2)
  expect_true(all(is.na(en$effective_size)))
  expect_true(all(is.na(en$constraint)))
})

test_that("ego_networks rejects unknown nodes", {
  skip_if_no_igraph()
  expect_error(ego_networks(make_ego_adj(), nodes = "Z"), "Unknown node")
})

test_that("ego_networks accepts integer node indices", {
  skip_if_no_igraph()
  en <- ego_networks(make_ego_adj(), nodes = c(3L, 1L))
  expect_identical(en$node, c("C", "A"))
})

test_that("ego_networks ignores self-loops in densities (TNA diagonals)", {
  skip_if_no_igraph()
  # Self-transition matrix: non-zero diagonal would inflate densities above 1
  # if loops were counted (igraph::edge_density excludes loops from denominator)
  m <- matrix(c(2, 1, 1,
                0, 3, 1,
                1, 0, 1), 3, 3, byrow = TRUE)
  rownames(m) <- colnames(m) <- c("X", "Y", "Z")
  en <- ego_networks(m, directed = TRUE)
  expect_true(all(en$ego_density >= 0 & en$ego_density <= 1, na.rm = TRUE))
  expect_true(all(en$alter_density >= 0 & en$alter_density <= 1, na.rm = TRUE))
})
