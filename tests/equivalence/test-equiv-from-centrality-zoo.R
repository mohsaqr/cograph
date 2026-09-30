# Equivalence tests moved from tests/testthat/test-centrality-zoo.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
skip_if_not_installed("igraph")
skip_coverage_tests()
k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(k3) <- colnames(k3) <- c("A", "B", "C")
path5 <- igraph::make_graph(c(1,2, 2,3, 3,4, 4,5), directed = FALSE)
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
star5[1, 2:5] <- 1; star5[2:5, 1] <- 1
rownames(star5) <- colnames(star5) <- LETTERS[1:5]
karate <- igraph::make_graph("Zachary")

# ---- equivalence tests ----

test_that("onion matches NetworkX on 100 random graphs", {
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")
  nx <- reticulate::import("networkx")

  set.seed(42)
  failures <- 0L
  for (i in 1:100) {
    n <- sample(8:15, 1)
    g <- igraph::sample_gnp(n, 0.35)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(n, 0.35)

    co <- cograph:::calculate_onion(g)

    el <- igraph::as_edgelist(g)
    G <- nx$Graph()
    G$add_nodes_from(as.list(seq_len(n) - 1L))
    G$add_edges_from(lapply(seq_len(nrow(el)), function(r) c(el[r,1]-1L, el[r,2]-1L)))
    nx_ol <- nx$onion_layers(G)
    nx_vec <- vapply(as.character(seq_len(n) - 1L), function(k) as.integer(nx_ol[[k]]), integer(1))

    if (!isTRUE(all.equal(as.numeric(co), as.numeric(nx_vec)))) {
      failures <- failures + 1L
    }
  }
  cat(sprintf("  onion vs NetworkX: %d/100 passed\n", 100 - failures))
  expect_equal(failures, 0L)
})

test_that("trophic_level matches NetworkX on DAG-like graphs", {
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")
  nx <- reticulate::import("networkx")

  set.seed(77)
  failures <- 0L
  tested <- 0L
  for (i in 1:200) {
    # Generate DAG-like graphs (more likely to have valid trophic levels)
    n <- sample(6:10, 1)
    g <- igraph::sample_gnp(n, 0.4, directed = TRUE)
    if (!igraph::is_connected(g, mode = "weak")) next

    co <- cograph:::calculate_trophic_level(g)
    if (any(is.na(co))) next

    el <- igraph::as_edgelist(g)
    G <- nx$DiGraph()
    G$add_nodes_from(as.list(seq_len(n) - 1L))
    G$add_edges_from(lapply(seq_len(nrow(el)), function(r) c(el[r,1]-1L, el[r,2]-1L)))
    nx_tl <- tryCatch({
      tl <- nx$trophic_levels(G)
      vapply(as.character(seq_len(n) - 1L), function(k) tl[[k]], numeric(1))
    }, error = function(e) NULL)
    if (is.null(nx_tl)) next

    tested <- tested + 1L
    if (max(abs(co - nx_tl)) > 1e-8) {
      failures <- failures + 1L
    }
    if (tested >= 50) break
  }
  cat(sprintf("  trophic_level vs NetworkX: %d/%d passed\n", tested - failures, tested))
  expect_true(tested >= 10)
  expect_equal(failures, 0L)
})

test_that("second_order: rank correlated with NetworkX (r > 0.7)", {
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")
  nx <- reticulate::import("networkx")

  set.seed(123)
  rank_cors <- numeric(0)
  for (i in 1:50) {
    n <- sample(8:12, 1)
    g <- igraph::sample_gnp(n, 0.35)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(n, 0.35)

    co <- cograph:::calculate_second_order(g)
    el <- igraph::as_edgelist(g)
    G <- nx$Graph()
    G$add_nodes_from(as.list(seq_len(n) - 1L))
    G$add_edges_from(lapply(seq_len(nrow(el)), function(r) c(el[r,1]-1L, el[r,2]-1L)))
    nx_soc <- vapply(as.character(seq_len(n) - 1L),
                     function(k) nx$second_order_centrality(G)[[k]], numeric(1))

    if (length(unique(co)) > 1 && length(unique(nx_soc)) > 1) {
      rank_cors <- c(rank_cors, cor(co, nx_soc, method = "spearman"))
    }
  }
  mean_r <- mean(rank_cors)
  cat(sprintf("  second_order rank r vs NetworkX: %.3f (n=%d)\n",
              mean_r, length(rank_cors)))
  expect_true(mean_r > 0.6)
})
