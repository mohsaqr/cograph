# Regression tests for four corrections of cograph 2.7.19: as_mcml() of a
# node-level group_tna without data, the modularity community_spinglass()
# reports, network_summary() girth, and select_bridges() on an edge table
# whose order differs from igraph's. Expected values are worked by hand.

.named_square <- function(m, labels) {
  dimnames(m) <- list(labels, labels)
  m
}

# ---------------------------------------------------------------------------
# 1. as_mcml() keeps the group's macro model
# ---------------------------------------------------------------------------

test_that("as_mcml() of as_tna(csum()) keeps the macro transitions", {
  skip_if_not_installed("tna")
  labels <- c("A1", "A2", "B1", "B2")
  m <- .named_square(matrix(0, 4, 4), labels)
  m["A1", "A2"] <- 2; m["A2", "A1"] <- 1; m["A1", "B1"] <- 1
  m["B1", "B2"] <- 3; m["B2", "A2"] <- 2; m["B2", "B1"] <- 1
  clusters <- list(A = c("A1", "A2"), B = c("B1", "B2"))
  # Summed cluster table: A->A 3, A->B 1, B->A 2, B->B 4. Row shares:
  # A (3/4, 1/4), B (1/3, 2/3). The rebuilt macro was the identity.
  expected <- matrix(c(3 / 4, 1 / 3, 1 / 4, 2 / 3), 2, 2,
                     dimnames = list(c("A", "B"), c("A", "B")))
  rebuilt <- as_mcml(as_tna(csum(m, clusters, method = "sum", type = "tna")))
  expect_equal(rebuilt$macro$weights, expected)
  expect_equal(rebuilt$macro$weights,
               csum(m, clusters, method = "sum", type = "tna")$macro$weights,
               ignore_attr = TRUE)
})

test_that("as_mcml() refuses a node-level group_tna with no data and no macro model", {
  skip_if_not_installed("tna")
  labels <- c("A1", "A2", "B1", "B2")
  m <- .named_square(matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), 4, 4), labels)
  groups <- as_tna(csum(m, list(A = c("A1", "A2"), B = c("B1", "B2")), type = "tna"))
  groups[["macro"]] <- NULL
  expect_error(as_mcml(groups), class = "cograph_bad_input")
})

# ---------------------------------------------------------------------------
# 2. community_spinglass() reports the partition's modularity
# ---------------------------------------------------------------------------

test_that("community_spinglass() modularity is the modularity of its partition", {
  skip_if_not_installed("igraph")
  # Two weighted triangles joined by one light edge.
  labels <- paste0("N", 1:6)
  m <- .named_square(matrix(0, 6, 6), labels)
  edges <- rbind(c(1, 2, 3), c(1, 3, 2), c(2, 3, 4), c(4, 5, 2), c(4, 6, 5),
                 c(5, 6, 1), c(3, 4, 0.5))
  m[edges[, 1:2]] <- edges[, 3]
  m <- m + t(m)
  # Newman's weighted modularity from its definition:
  # Q = sum_ij (A_ij - k_i k_j / 2W) [c_i = c_j] / 2W.
  base_modularity <- function(a, membership) {
    k <- rowSums(a)
    two_w <- sum(a)
    same <- outer(membership, membership, `==`)
    sum((a - outer(k, k) / two_w) * same) / two_w
  }
  found <- community_spinglass(m, seed = 1)
  expect_equal(attr(found, "modularity"), base_modularity(m, found$community))
  alias <- com_sg(m, seed = 2)
  expect_equal(attr(alias, "modularity"), base_modularity(m, alias$community))
})

# ---------------------------------------------------------------------------
# 3. network_summary() girth: a reciprocated pair is not a cycle
# ---------------------------------------------------------------------------

test_that("network_summary() girth equals network_girth() and ignores reciprocated pairs", {
  skip_if_not_installed("igraph")
  # regulation_net's shortest cycle on the simple undirected skeleton has
  # length 3; network_summary() reported 2 (the reciprocated pairs).
  expect_equal(network_summary(regulation_net, extended = TRUE, digits = NULL)$girth, 3)
  expect_equal(network_girth(regulation_net), 3)
  # 1 <-> 2, 2 -> 3, 3 -> 1: the skeleton is a triangle.
  m <- .named_square(matrix(c(0, 1, 0, 1, 0, 0, 1, 1, 0), 3, 3), paste0("N", 1:3))
  expect_equal(network_girth(m), 3)
  expect_equal(network_summary(m, extended = TRUE, digits = NULL)$girth, 3)
})

# ---------------------------------------------------------------------------
# 4. select_bridges() flags its own edge table
# ---------------------------------------------------------------------------

test_that("select_bridges() keeps the bridges when the edge order differs from igraph's", {
  # Triangle N1-N2-N3 with pendants N4 (on N1) and N5 (on N3). Row by row the
  # edges are 1-2, 1-3, 1-4, 2-3, 3-5; column by column 1-2, 1-3, 2-3, 1-4,
  # 3-5. The bridges are N1-N4 and N3-N5; flags in the first order applied to
  # the second kept N2-N3 and dropped N1-N4.
  labels <- paste0("N", 1:5)
  m <- .named_square(matrix(0, 5, 5), labels)
  m[rbind(c(1, 2), c(1, 3), c(2, 3), c(1, 4), c(3, 5))] <- 1
  m <- m + t(m)
  # N2 keeps no edge: select_bridges() says so and keeps the node.
  expect_warning(kept <- as.data.frame(select_bridges(m)), "no edges left")
  expect_setequal(paste(pmin(kept$from, kept$to), pmax(kept$from, kept$to)),
                  c("N1 N4", "N3 N5"))
})
