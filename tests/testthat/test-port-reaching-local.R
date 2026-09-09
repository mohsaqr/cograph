# Weighted local reaching centrality against the pre-port algorithm rebuilt
# on igraph (one shortest path per target under length = total/weight, then
# the mean of the original weights along it). Continuous random weights make
# ties impossible, so the comparison is exact.

.reaching_reference <- function(m, directed) {
  g <- igraph::graph_from_adjacency_matrix(m, mode = if (directed) "directed" else "undirected", weighted = TRUE)
  n <- igraph::vcount(g); w <- igraph::E(g)$weight; edge_dist <- sum(w) / w
  vapply(seq_len(n), function(v) {
    paths <- suppressWarnings(igraph::shortest_paths(g, from = v, mode = "all", weights = edge_dist, output = "vpath")$vpath)
    avg <- vapply(paths, function(p) {
      p <- as.integer(p); plen <- length(p) - 1L
      if (plen <= 0L) return(0)
      eids <- igraph::get_edge_ids(g, vp = as.vector(rbind(p[-length(p)], p[-1L])))
      sum(w[eids]) / plen
    }, numeric(1))
    sum(avg) / (n - 1)
  }, numeric(1))
}

test_that("reaching_local equals the igraph-path reference on tie-free weighted graphs", {
  skip_if_not_installed("igraph")
  set.seed(42)
  design <- expand.grid(n = c(8L, 12L, 20L, 30L), directed = c(FALSE, TRUE), rep = 1:3, KEEP.OUT.ATTRS = FALSE)
  for (i in seq_len(nrow(design))) {
    # one random graph per design row
    n <- design$n[i]; directed <- design$directed[i]
    m <- matrix(0, n, n); idx <- which(row(m) != col(m)); take <- idx[stats::runif(length(idx)) < 0.3]
    m[take] <- stats::runif(length(take), 0.5, 3)
    if (!directed) m[lower.tri(m)] <- t(m)[lower.tri(m)]
    got <- centrality(m, measures = "reaching_local")$reaching_local
    expect_equal(got, .reaching_reference(m, directed), tolerance = 1e-12, info = paste("row", i))
  }
})
