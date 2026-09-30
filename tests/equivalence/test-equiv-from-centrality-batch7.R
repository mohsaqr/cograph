# Equivalence tests moved from tests/testthat/test-centrality-batch7.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
path4 <- matrix(c(0, 1, 0, 0,
                  1, 0, 1, 0,
                  0, 1, 0, 1,
                  0, 0, 1, 0), 4, 4)
rownames(path4) <- colnames(path4) <- LETTERS[1:4]
path5 <- matrix(0, 5, 5)
path5[cbind(1:4, 2:5)] <- 1
path5 <- path5 + t(path5)
rownames(path5) <- colnames(path5) <- LETTERS[1:5]
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1
star5 <- star5 + t(star5)
rownames(star5) <- colnames(star5) <- LETTERS[1:5]
bridge6 <- matrix(0, 6, 6)
bridge6[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
bridge6 <- bridge6 + t(bridge6)
rownames(bridge6) <- colnames(bridge6) <- LETTERS[1:6]
bridge_membership <- c(1, 1, 1, 2, 2, 2)
layers <- c(4, 5, 4, 4)
n_layered <- 1 + sum(layers)
ring_ids <- split(seq_len(n_layered)[-1], rep(seq_along(layers), layers))
layered <- matrix(0, n_layered, n_layered)
layered[cbind(1, ring_ids[[1]])] <- 1
layered[cbind(ring_ids[[1]][1], ring_ids[[2]])] <- 1
layered[cbind(ring_ids[[2]][1], ring_ids[[3]])] <- 1
layered[cbind(ring_ids[[3]][1], ring_ids[[4]])] <- 1
layered <- layered + t(layered)
rownames(layered) <- colnames(layered) <- paste0("v", seq_len(n_layered))
chain3 <- matrix(c(0, 1, 0,
                   0, 0, 1,
                   0, 0, 0), 3, 3, byrow = TRUE)
rownames(chain3) <- colnames(chain3) <- LETTERS[1:3]
entropy_bits <- function(p) -sum(p * log(p)) / log(length(p))
batch7 <- c("distance_entropy", "local_dimension",
            "local_information_dimension", "neighborhood_connectivity",
            "modularity_vitality")

# ---- equivalence tests ----

test_that("modularity kernel matches igraph::modularity", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  set.seed(11)
  # Sequential sweep: each random graph is one independent expectation and
  # the seed stream must advance in order for the run to be reproducible.
  for (i in 1:12) {
    directed <- i %% 2 == 0
    weighted <- i %% 3 == 0
    g <- igraph::sample_gnp(sample(6:14, 1), 0.35, directed = directed)
    if (igraph::ecount(g) < 3) next
    w <- if (weighted) stats::runif(igraph::ecount(g), 0.5, 3) else NULL
    if (weighted) igraph::E(g)$weight <- w
    memb <- sample(1:3, igraph::vcount(g), replace = TRUE)
    m <- cograph:::.cg_path_matrix(g, w)
    expect_equal(cograph:::.cg_modularity(m, memb),
                 igraph::modularity(g, memb, weights = w),
                 info = sprintf("graph %d directed=%s weighted=%s",
                                i, directed, weighted))
  }
})

test_that("neighborhood_connectivity matches igraph::knn", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  v <- centrality(g, measures = "neighborhood_connectivity")
  expect_equal(v$neighborhood_connectivity_all,
               igraph::knn(g, weights = NA)$knn)

  set.seed(5)
  gd <- igraph::sample_gnp(15, 0.3, directed = TRUE)
  out <- centrality(gd, measures = "neighborhood_connectivity", mode = "out")
  ref <- igraph::knn(gd, mode = "out", neighbor.degree.mode = "out",
                     weights = NA)$knn
  ref[is.nan(ref)] <- 0
  expect_equal(out$neighborhood_connectivity_out, ref)
})
