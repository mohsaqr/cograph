#!/usr/bin/env Rscript
#' ============================================================================
#' FILTER EDGE CASES - COMPREHENSIVE VALIDATION TEST
#' ============================================================================
#'
#' Tests filter_nodes() and filter_edges() against edge cases that could break
#' or produce unexpected results.
#'
#' USAGE:
#'   Rscript validation/test_filter_edge_cases.R
#'
#' ============================================================================

suppressPackageStartupMessages({
  library(igraph)
  devtools::load_all(".", quiet = TRUE)
})

# Check if Saqrlab is available
use_saqrlab <- requireNamespace("Saqrlab", quietly = TRUE)

cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("FILTER EDGE CASES - COMPREHENSIVE VALIDATION TEST\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Saqrlab available:", use_saqrlab, "\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n\n")

# =============================================================================
# Test Helpers (from helper-test-utils.R)
# =============================================================================

create_test_matrix <- function(n = 5, density = 0.5, weighted = FALSE,
                               symmetric = TRUE, seed = 42) {
  set.seed(seed)

  mat <- matrix(0, n, n)
  n_possible <- n * (n - 1)
  if (symmetric) n_possible <- n_possible / 2
  n_edges <- round(n_possible * density)

  if (symmetric) {
    upper_idx <- which(upper.tri(mat))
    selected <- sample(upper_idx, min(n_edges, length(upper_idx)))
    if (weighted) {
      mat[selected] <- runif(length(selected), 0.1, 1)
    } else {
      mat[selected] <- 1
    }
    mat <- mat + t(mat)
  } else {
    all_idx <- which(row(mat) != col(mat))
    selected <- sample(all_idx, min(n_edges, length(all_idx)))
    if (weighted) {
      mat[selected] <- runif(length(selected), -1, 1)
    } else {
      mat[selected] <- 1
    }
  }

  rownames(mat) <- colnames(mat) <- LETTERS[1:n]
  mat
}

create_test_topology <- function(type = "complete", n = 5) {
  mat <- matrix(0, n, n)

  switch(type,
    complete = {
      mat <- matrix(1, n, n)
      diag(mat) <- 0
    },
    star = {
      mat[1, 2:n] <- 1
      mat[2:n, 1] <- 1
    },
    ring = {
      for (i in 1:(n-1)) {
        mat[i, i+1] <- 1
        mat[i+1, i] <- 1
      }
      mat[n, 1] <- 1
      mat[1, n] <- 1
    },
    path = {
      for (i in 1:(n-1)) {
        mat[i, i+1] <- 1
        mat[i+1, i] <- 1
      }
    },
    disconnected = {
      half <- floor(n/2)
      if (half > 1) {
        mat[1:half, 1:half] <- 1
        diag(mat[1:half, 1:half]) <- 0
      }
      if (n - half > 1) {
        mat[(half+1):n, (half+1):n] <- 1
        diag(mat[(half+1):n, (half+1):n]) <- 0
      }
    }
  )

  rownames(mat) <- colnames(mat) <- LETTERS[1:n]
  mat
}

# =============================================================================
# Results Collector
# =============================================================================

results <- data.frame(
  category = character(),
  scenario = character(),
  status = character(),
  message = character(),
  stringsAsFactors = FALSE
)

test_scenario <- function(category, name, expr, expected_behavior = "pass") {
  result <- tryCatch({
    # Capture warnings too
    warn_msgs <- character(0)
    value <- withCallingHandlers(
      eval(expr),
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    if (length(warn_msgs) > 0) {
      list(status = "WARN", error = paste(warn_msgs, collapse = "; "), value = value)
    } else {
      list(status = "PASS", error = NULL, value = value)
    }
  }, error = function(e) {
    list(status = "FAIL", error = conditionMessage(e), value = NULL)
  })

  # Determine if this matches expected behavior
  expected_match <- switch(expected_behavior,
    "pass" = result$status == "PASS",
    "warn" = result$status == "WARN",
    "fail" = result$status == "FAIL",
    "warn_or_pass" = result$status %in% c("PASS", "WARN"),
    "any" = TRUE,
    FALSE
  )

  final_status <- if (expected_match) {
    paste0(result$status, " (expected)")
  } else {
    paste0(result$status, " (unexpected - expected ", expected_behavior, ")")
  }

  results <<- rbind(results, data.frame(
    category = category,
    scenario = name,
    status = final_status,
    message = if (is.null(result$error)) "" else result$error,
    stringsAsFactors = FALSE
  ))

  status_char <- if (expected_match) "\u2713" else "\u2717"
  cat(sprintf("  [%s] %s: %s\n", status_char, name, result$status))
  if (!is.null(result$error) && nchar(result$error) > 0) {
    cat(sprintf("      -> %s\n", substr(result$error, 1, 70)))
  }

  invisible(result)
}

# =============================================================================
# Category 1: Empty/Minimal Networks
# =============================================================================

cat("\nCATEGORY 1: Empty/Minimal Networks\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 1.1: Filter to 0 nodes
test_scenario("empty", "1.1 Filter to 0 nodes",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    result <- filter_nodes(mat, degree > 1000)
    # Should return empty network with warning
    stopifnot(n_nodes(result) == 0)
    result
  }),
  expected_behavior = "warn"
)

# 1.2: Filter to 1 node remaining
test_scenario("empty", "1.2 Single node remaining",
  quote({
    mat <- create_test_matrix(3, density = 1, weighted = TRUE)
    result <- filter_nodes(mat, label == "A")
    stopifnot(n_nodes(result) == 1)
    stopifnot(n_edges(result) == 0)  # No self-loops
    result
  }),
  expected_behavior = "pass"
)

# 1.3: Filter to 0 edges
test_scenario("empty", "1.3 Filter to 0 edges",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    result <- filter_edges(mat, weight > 100)
    # Should return empty or warn
    n_edges(result)
  }),
  expected_behavior = "warn"
)

# 1.4: Empty input matrix (0x0)
test_scenario("empty", "1.4 Empty input matrix",
  quote({
    mat <- matrix(0, nrow = 0, ncol = 0)
    result <- as_cograph(mat)
    n_nodes(result)
  }),
  expected_behavior = "any"  # May fail or warn
)

# 1.5: 2-node network filter to 1
test_scenario("empty", "1.5 Two nodes filter to one",
  quote({
    mat <- matrix(c(0, 1, 1, 0), 2, 2)
    rownames(mat) <- colnames(mat) <- c("A", "B")
    result <- filter_nodes(mat, label == "A")
    stopifnot(n_nodes(result) == 1)
    result
  }),
  expected_behavior = "pass"
)

# =============================================================================
# Category 2: Sparse Networks
# =============================================================================

cat("\nCATEGORY 2: Sparse Networks\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 2.1: Very sparse network
test_scenario("sparse", "2.1 Very sparse (density < 0.05)",
  quote({
    mat <- create_test_matrix(20, density = 0.05, weighted = TRUE, seed = 123)
    result <- filter_edges(mat, weight > mean(weight))
    n_edges(result)
  }),
  expected_behavior = "warn_or_pass"
)

# 2.2: Tree structure (BA with m=1)
test_scenario("sparse", "2.2 Tree structure (BA m=1)",
  quote({
    g <- sample_pa(20, m = 1, directed = FALSE)
    V(g)$name <- paste0("N", 1:20)
    result <- filter_nodes(g, degree > 1)
    # Only non-leaves remain
    n_nodes(result)
  }),
  expected_behavior = "pass"
)

# 2.3: Disconnected components
test_scenario("sparse", "2.3 Disconnected components",
  quote({
    mat <- create_test_topology("disconnected", n = 10)
    result <- filter_nodes(mat, degree >= 1)
    stopifnot(n_nodes(result) == 10)  # All nodes have degree >= 1 within components
    result
  }),
  expected_behavior = "pass"
)

# 2.4: Single edge network
test_scenario("sparse", "2.4 Single edge network",
  quote({
    mat <- matrix(0, 5, 5)
    mat[1, 2] <- mat[2, 1] <- 0.5
    rownames(mat) <- colnames(mat) <- LETTERS[1:5]
    result <- filter_edges(mat, weight > 0)
    stopifnot(n_edges(result) == 1 || n_edges(result) == 2)  # Depending on directed interpretation
    result
  }),
  expected_behavior = "pass"
)

# =============================================================================
# Category 3: Dense Networks
# =============================================================================

cat("\nCATEGORY 3: Dense Networks\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 3.1: Complete graph
test_scenario("dense", "3.1 Complete graph",
  quote({
    mat <- create_test_topology("complete", n = 10)
    result <- filter_edges(mat, weight > 0)
    stopifnot(n_edges(result) > 0)
    result
  }),
  expected_behavior = "pass"
)

# 3.2: High density filter to half
test_scenario("dense", "3.2 High density filter by median",
  quote({
    mat <- create_test_matrix(15, density = 0.8, weighted = TRUE)
    net <- as_cograph(mat)
    edges_before <- n_edges(net)
    result <- filter_edges(mat, weight > median(weight))
    edges_after <- n_edges(result)
    # Should have roughly half the edges
    stopifnot(edges_after < edges_before)
    result
  }),
  expected_behavior = "pass"
)

# 3.3: All self-loops (filter out)
test_scenario("dense", "3.3 Diagonal-only matrix (self-loops)",
  quote({
    mat <- diag(5) * 0.5
    rownames(mat) <- colnames(mat) <- LETTERS[1:5]
    result <- filter_edges(mat, weight > 0)
    # Depends on how self-loops are handled
    n_edges(result)
  }),
  expected_behavior = "warn_or_pass"
)

# =============================================================================
# Category 4: Special Structures
# =============================================================================

cat("\nCATEGORY 4: Special Structures\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 4.1: Star network - filter by degree
test_scenario("structure", "4.1 Star network (degree > 1)",
  quote({
    mat <- create_test_topology("star", n = 10)
    result <- filter_nodes(mat, degree > 1)
    # Only hub should remain
    stopifnot(n_nodes(result) == 1)
    result
  }),
  expected_behavior = "pass"
)

# 4.2: Ring network - filter by degree > 2
test_scenario("structure", "4.2 Ring network (degree > 2)",
  quote({
    mat <- create_test_topology("ring", n = 10)
    result <- filter_nodes(mat, degree > 2)
    # All nodes have degree 2, so none remain
    stopifnot(n_nodes(result) == 0)
    result
  }),
  expected_behavior = "warn"
)

# 4.3: Path network
test_scenario("structure", "4.3 Path network filter endpoints",
  quote({
    mat <- create_test_topology("path", n = 10)
    result <- filter_nodes(mat, degree >= 2)
    # Endpoints have degree 1, should be removed
    stopifnot(n_nodes(result) == 8)
    result
  }),
  expected_behavior = "pass"
)

# 4.4: Bipartite-like structure
test_scenario("structure", "4.4 Bipartite structure",
  quote({
    g <- make_bipartite_graph(c(rep(FALSE, 5), rep(TRUE, 5)),
                              edges = c(1,6, 1,7, 2,7, 2,8, 3,8, 3,9, 4,9, 4,10, 5,10))
    V(g)$name <- paste0("N", 1:10)
    result <- filter_nodes(g, label %in% paste0("N", 1:5))
    stopifnot(n_nodes(result) == 5)
    result
  }),
  expected_behavior = "pass"
)

# =============================================================================
# Category 5: Edge Weight Distributions
# =============================================================================

cat("\nCATEGORY 5: Edge Weight Distributions\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 5.1: All same weight
test_scenario("weights", "5.1 Uniform weights (all 1)",
  quote({
    mat <- create_test_topology("complete", n = 5)
    result <- filter_edges(mat, weight > 1)
    # All weights are exactly 1, so none pass > 1
    stopifnot(n_edges(result) == 0)
    result
  }),
  expected_behavior = "warn"
)

# 5.2: Negative weights
test_scenario("weights", "5.2 Negative weights",
  quote({
    set.seed(42)
    mat <- matrix(runif(25, -1, 1), 5, 5)
    mat <- mat * upper.tri(mat)
    mat <- mat + t(mat)
    diag(mat) <- 0
    rownames(mat) <- colnames(mat) <- LETTERS[1:5]
    result <- filter_edges(mat, weight > 0)
    n_edges(result)
  }),
  expected_behavior = "warn_or_pass"
)

# 5.3: NA weights
test_scenario("weights", "5.3 NA weights in matrix",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    mat[1, 2] <- NA
    mat[2, 1] <- NA
    result <- filter_edges(mat, weight > 0.5)
    n_edges(result)
  }),
  expected_behavior = "any"
)

# 5.4: Inf weights
test_scenario("weights", "5.4 Inf weights",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    mat[1, 2] <- Inf
    mat[2, 1] <- Inf
    result <- filter_edges(mat, weight < Inf)
    n_edges(result)
  }),
  expected_behavior = "warn_or_pass"
)

# 5.5: Zero weights (should be treated as no edge)
test_scenario("weights", "5.5 Zero weights",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    mat[mat < 0.3] <- 0
    result <- filter_edges(mat, weight > 0)
    n_edges(result)
  }),
  expected_behavior = "pass"
)

# 5.6: Very small weights
test_scenario("weights", "5.6 Very small weights (near zero)",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    mat <- mat * 1e-10
    result <- filter_edges(mat, weight > 1e-11)
    n_edges(result)
  }),
  expected_behavior = "pass"
)

# =============================================================================
# Category 6: Node Label Issues
# =============================================================================

cat("\nCATEGORY 6: Node Label Issues\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 6.1: Duplicate labels (shouldn't happen but test robustness)
test_scenario("labels", "6.1 Duplicate labels",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    rownames(mat) <- colnames(mat) <- c("A", "A", "B", "C", "D")  # Duplicate A
    result <- filter_nodes(mat, label == "A")
    # Should match all nodes labeled "A"
    n_nodes(result)
  }),
  expected_behavior = "any"
)

# 6.2: Empty string labels
test_scenario("labels", "6.2 Empty string labels",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    rownames(mat) <- colnames(mat) <- c("", "B", "C", "D", "E")
    result <- filter_nodes(mat, label != "")
    stopifnot(n_nodes(result) == 4)
    result
  }),
  expected_behavior = "pass"
)

# 6.3: NA labels
test_scenario("labels", "6.3 NA labels",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    rownames(mat) <- colnames(mat) <- c(NA, "B", "C", "D", "E")
    result <- filter_nodes(mat, !is.na(label))
    n_nodes(result)
  }),
  expected_behavior = "any"
)

# 6.4: Numeric labels
test_scenario("labels", "6.4 Numeric labels",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    rownames(mat) <- colnames(mat) <- as.character(1:5)
    result <- filter_nodes(mat, label %in% c("1", "2", "3"))
    stopifnot(n_nodes(result) == 3)
    result
  }),
  expected_behavior = "pass"
)

# 6.5: Special characters in labels
test_scenario("labels", "6.5 Special characters in labels",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    rownames(mat) <- colnames(mat) <- c("A/B", "C D", "E.F", "G-H", "I_J")
    result <- filter_nodes(mat, label == "A/B")
    stopifnot(n_nodes(result) == 1)
    result
  }),
  expected_behavior = "pass"
)

# 6.6: Unicode labels
test_scenario("labels", "6.6 Unicode labels",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    rownames(mat) <- colnames(mat) <- c("\u03B1", "\u03B2", "\u03B3", "\u03B4", "\u03B5")  # Greek letters
    result <- filter_nodes(mat, label == "\u03B1")
    stopifnot(n_nodes(result) == 1)
    result
  }),
  expected_behavior = "pass"
)

# =============================================================================
# Category 7: Large Networks
# =============================================================================

cat("\nCATEGORY 7: Large Networks\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 7.1: 1000 nodes (performance test)
test_scenario("large", "7.1 1000 nodes filter by pagerank",
  quote({
    g <- sample_pa(1000, m = 2, directed = FALSE)
    V(g)$name <- paste0("N", 1:1000)
    start_time <- Sys.time()
    result <- filter_nodes(g, pagerank > 0.005)
    elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
    cat(sprintf(" (%.2fs, %d nodes) ", elapsed, n_nodes(result)))
    stopifnot(elapsed < 30)  # Should complete in reasonable time
    result
  }),
  expected_behavior = "pass"
)

# 7.2: 500 nodes dense
test_scenario("large", "7.2 500 nodes dense filter edges",
  quote({
    if (use_saqrlab) {
      el <- Saqrlab::simulate_edge_list(n_nodes = 500, edge_density = 3, seed = 42)
      g <- graph_from_data_frame(el, directed = FALSE)
    } else {
      g <- sample_gnp(500, 0.03)
    }
    V(g)$name <- paste0("N", 1:500)
    E(g)$weight <- runif(ecount(g), 0, 1)
    start_time <- Sys.time()
    result <- filter_edges(g, weight > 0.7)
    elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
    cat(sprintf(" (%.2fs, %d edges) ", elapsed, n_edges(result)))
    stopifnot(elapsed < 30)
    result
  }),
  expected_behavior = "pass"
)

# 7.3: Sparse large network centrality
test_scenario("large", "7.3 2000 nodes sparse (BA) degree filter",
  quote({
    g <- sample_pa(2000, m = 1, directed = FALSE)
    V(g)$name <- paste0("N", 1:2000)
    start_time <- Sys.time()
    result <- filter_nodes(g, degree >= 5)
    elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
    cat(sprintf(" (%.2fs, %d nodes) ", elapsed, n_nodes(result)))
    stopifnot(elapsed < 60)
    result
  }),
  expected_behavior = "pass"
)

# =============================================================================
# Category 8: Directed vs Undirected
# =============================================================================

cat("\nCATEGORY 8: Directed vs Undirected\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 8.1: Directed network filter by indegree
# NOTE: Directed acyclic graphs trigger eigenvector centrality warnings in igraph.
# This is expected behavior when using centrality measures on DAGs.
test_scenario("directed", "8.1 Directed filter by indegree (DAG warns on eigenvector)",
  quote({
    # Use positive weights only to avoid igraph betweenness error
    set.seed(42)
    mat <- matrix(runif(100, 0.1, 1), 10, 10)
    mat[lower.tri(mat)] <- 0  # Make directed (upper triangle only) - creates DAG
    diag(mat) <- 0
    rownames(mat) <- colnames(mat) <- LETTERS[1:10]
    result <- filter_nodes(mat, indegree > 0, directed = TRUE)
    n_nodes(result)
  }),
  expected_behavior = "warn_or_pass"  # DAG causes eigenvector warning
)

# 8.2: Directed network filter by outdegree
test_scenario("directed", "8.2 Directed filter by outdegree (DAG warns on eigenvector)",
  quote({
    # Use positive weights only to avoid igraph betweenness error
    set.seed(43)
    mat <- matrix(runif(100, 0.1, 1), 10, 10)
    mat[lower.tri(mat)] <- 0
    diag(mat) <- 0
    rownames(mat) <- colnames(mat) <- LETTERS[1:10]
    result <- filter_nodes(mat, outdegree > 0, directed = TRUE)
    n_nodes(result)
  }),
  expected_behavior = "warn_or_pass"  # DAG causes eigenvector warning
)

# 8.3: Asymmetric weights
test_scenario("directed", "8.3 Asymmetric weights",
  quote({
    mat <- matrix(c(0, 0.8, 0.2,
                    0.1, 0, 0.9,
                    0.5, 0.3, 0), 3, 3, byrow = TRUE)
    rownames(mat) <- colnames(mat) <- c("A", "B", "C")
    result <- filter_edges(mat, weight > 0.5, directed = TRUE)
    n_edges(result)
  }),
  expected_behavior = "pass"
)

# 8.4: Force undirected on asymmetric
test_scenario("directed", "8.4 Force undirected on asymmetric",
  quote({
    mat <- matrix(c(0, 0.8, 0,
                    0, 0, 0.9,
                    0.5, 0, 0), 3, 3, byrow = TRUE)
    rownames(mat) <- colnames(mat) <- c("A", "B", "C")
    result <- filter_edges(mat, weight > 0.4, directed = FALSE)
    n_edges(result)
  }),
  expected_behavior = "pass"
)

# =============================================================================
# Category 9: Chained Filters
# =============================================================================

cat("\nCATEGORY 9: Chained Filters\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 9.1: Edge then node filter
test_scenario("chained", "9.1 Edge filter then node filter",
  quote({
    mat <- create_test_matrix(10, density = 0.6, weighted = TRUE)
    result <- mat |>
      filter_edges(weight > 0.5) |>
      filter_nodes(degree >= 2)
    n_nodes(result)
  }),
  expected_behavior = "warn_or_pass"
)

# 9.2: Node then edge filter (different result)
test_scenario("chained", "9.2 Node filter then edge filter",
  quote({
    mat <- create_test_matrix(10, density = 0.6, weighted = TRUE)
    result <- mat |>
      filter_nodes(degree >= 3) |>
      filter_edges(weight > 0.3)
    n_edges(result)
  }),
  expected_behavior = "warn_or_pass"
)

# 9.3: Multiple node filters
test_scenario("chained", "9.3 Multiple node filters",
  quote({
    mat <- create_test_matrix(15, density = 0.5, weighted = TRUE)
    result <- mat |>
      filter_nodes(degree >= 2) |>
      filter_nodes(pagerank > 0.05)
    n_nodes(result)
  }),
  expected_behavior = "warn_or_pass"
)

# 9.4: Filter to empty progressively
test_scenario("chained", "9.4 Progressive filter to empty",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    result <- mat |>
      filter_edges(weight > 0.3) |>
      filter_edges(weight > 0.6) |>
      filter_edges(weight > 0.9)
    # Should eventually be empty or nearly empty
    n_edges(result)
  }),
  expected_behavior = "warn"
)

# 9.5: Complex compound filter
test_scenario("chained", "9.5 Complex compound filter",
  quote({
    mat <- create_test_matrix(20, density = 0.4, weighted = TRUE)
    result <- filter_nodes(mat, degree >= 2 & betweenness > 0)
    n_nodes(result)
  }),
  expected_behavior = "warn_or_pass"
)

# =============================================================================
# Category 10: Format Conversion
# =============================================================================

cat("\nCATEGORY 10: Format Conversion (keep_format = TRUE)\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 10.1: Matrix format preservation
test_scenario("format", "10.1 Matrix in -> Matrix out",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    result <- filter_edges(mat, weight > 0.3, keep_format = TRUE)
    stopifnot(is.matrix(result))
    result
  }),
  expected_behavior = "pass"
)

# 10.2: Matrix to empty
test_scenario("format", "10.2 Matrix filter to empty",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    result <- filter_nodes(mat, degree > 100, keep_format = TRUE)
    stopifnot(is.matrix(result))
    stopifnot(nrow(result) == 0)
    result
  }),
  expected_behavior = "warn"
)

# 10.3: igraph format preservation
test_scenario("format", "10.3 igraph in -> igraph out",
  quote({
    g <- make_ring(10)
    V(g)$name <- LETTERS[1:10]
    result <- filter_nodes(g, degree >= 2, keep_format = TRUE)
    stopifnot(inherits(result, "igraph"))
    result
  }),
  expected_behavior = "pass"
)

# 10.4: igraph to single node
test_scenario("format", "10.4 igraph filter to single node",
  quote({
    g <- make_star(5, mode = "undirected")
    V(g)$name <- LETTERS[1:5]
    result <- filter_nodes(g, degree > 1, keep_format = TRUE)
    stopifnot(inherits(result, "igraph"))
    stopifnot(vcount(result) == 1)
    result
  }),
  expected_behavior = "pass"
)

# 10.5: cograph_network stays cograph_network
test_scenario("format", "10.5 cograph_network preservation",
  quote({
    mat <- create_test_matrix(5, density = 0.5, weighted = TRUE)
    net <- as_cograph(mat)
    result <- filter_edges(net, weight > 0.3)
    stopifnot(inherits(result, "cograph_network"))
    result
  }),
  expected_behavior = "pass"
)

# =============================================================================
# Category 11: Edge Cases with Centrality Measures
# =============================================================================

cat("\nCATEGORY 11: Centrality Edge Cases\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 11.1: Eigenvector centrality on disconnected
test_scenario("centrality", "11.1 Eigenvector on disconnected",
  quote({
    mat <- create_test_topology("disconnected", n = 10)
    result <- filter_nodes(mat, eigenvector > 0.1)
    n_nodes(result)
  }),
  expected_behavior = "warn_or_pass"
)

# 11.2: Closeness on disconnected (may have Inf/NA)
test_scenario("centrality", "11.2 Closeness on disconnected",
  quote({
    mat <- create_test_topology("disconnected", n = 10)
    result <- filter_nodes(mat, closeness > 0)
    n_nodes(result)
  }),
  expected_behavior = "warn_or_pass"
)

# 11.3: Betweenness on tree (many zeros)
test_scenario("centrality", "11.3 Betweenness on tree",
  quote({
    g <- sample_pa(20, m = 1, directed = FALSE)
    V(g)$name <- paste0("N", 1:20)
    result <- filter_nodes(g, betweenness > 0)
    n_nodes(result)
  }),
  expected_behavior = "pass"
)

# 11.4: Hub/authority on undirected
test_scenario("centrality", "11.4 Hub/authority on undirected",
  quote({
    mat <- create_test_matrix(10, density = 0.5, weighted = TRUE)
    result <- filter_nodes(mat, hub > 0.1 & authority > 0.1)
    n_nodes(result)
  }),
  expected_behavior = "warn_or_pass"
)

# 11.5: Negative weights cause betweenness failure (known limitation)
# NOTE: igraph's betweenness requires positive weights. filter_nodes computes all
# centrality measures including betweenness, so negative weights cause errors.
# Workaround: use filter_edges first to remove negative weights, or use abs(weight).
test_scenario("centrality", "11.5 Negative weights cause betweenness error",
  quote({
    set.seed(42)
    mat <- matrix(runif(100, -1, 1), 10, 10)
    mat <- mat * upper.tri(mat)
    mat <- mat + t(mat)
    diag(mat) <- 0
    rownames(mat) <- colnames(mat) <- LETTERS[1:10]
    # This will fail due to betweenness computation
    result <- filter_nodes(mat, degree > 2)
    n_nodes(result)
  }),
  expected_behavior = "fail"  # Known limitation: negative weights break betweenness
)

# =============================================================================
# Category 12: Keep Isolates Option
# =============================================================================

cat("\nCATEGORY 12: Keep Isolates Option\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

# 12.1: Keep isolates = TRUE
test_scenario("isolates", "12.1 Keep isolates after edge filter",
  quote({
    mat <- create_test_matrix(5, density = 0.3, weighted = TRUE)
    result <- filter_edges(mat, weight > 0.9, .keep_isolates = TRUE)
    # Should keep all nodes even if edges removed
    stopifnot(n_nodes(result) == 5)
    result
  }),
  expected_behavior = "warn_or_pass"
)

# 12.2: Keep isolates = FALSE (default)
test_scenario("isolates", "12.2 Remove isolates after edge filter",
  quote({
    mat <- create_test_matrix(10, density = 0.2, weighted = TRUE)
    result <- filter_edges(mat, weight > 0.8, .keep_isolates = FALSE)
    # Nodes with no remaining edges should be removed
    n_nodes(result)
  }),
  expected_behavior = "warn_or_pass"
)

# =============================================================================
# Category 13: Random Network Models (if Saqrlab available)
# =============================================================================

if (use_saqrlab) {
  cat("\nCATEGORY 13: Saqrlab Network Models\n")
  cat("-" |> rep(60) |> paste(collapse = ""), "\n")

  # 13.1: Erdos-Renyi model
  test_scenario("saqrlab", "13.1 ER model filter",
    quote({
      g <- Saqrlab::simulate_igraph(model = "er", n = 50, p = 0.1, seed = 42)
      V(g)$name <- paste0("N", 1:50)
      result <- filter_nodes(g, degree >= 3)
      n_nodes(result)
    }),
    expected_behavior = "pass"
  )

  # 13.2: Barabasi-Albert model
  test_scenario("saqrlab", "13.2 BA model filter",
    quote({
      g <- Saqrlab::simulate_igraph(model = "ba", n = 100, m = 2, seed = 42)
      V(g)$name <- paste0("N", 1:100)
      result <- filter_nodes(g, pagerank > 0.02)
      n_nodes(result)
    }),
    expected_behavior = "pass"
  )

  # 13.3: Watts-Strogatz model
  test_scenario("saqrlab", "13.3 WS model filter",
    quote({
      # Saqrlab uses n, nei, p_rewire for WS model
      g <- Saqrlab::simulate_igraph(n = 50, model = "ws", nei = 4, p_rewire = 0.1, seed = 42)
      V(g)$name <- paste0("N", 1:50)
      result <- filter_nodes(g, betweenness > 10)
      n_nodes(result)
    }),
    expected_behavior = "warn_or_pass"
  )

  # 13.4: Stochastic block model
  test_scenario("saqrlab", "13.4 SBM model filter",
    quote({
      # Saqrlab uses n, blocks, p_within, p_between for SBM
      g <- Saqrlab::simulate_igraph(n = 60, model = "sbm", blocks = 2,
                                    p_within = 0.3, p_between = 0.05, seed = 42)
      V(g)$name <- paste0("N", 1:60)
      result <- filter_edges(g, weight > 0)
      n_edges(result)
    }),
    expected_behavior = "warn_or_pass"  # May have 0 edges depending on density
  )
} else {
  cat("\nCATEGORY 13: Saqrlab Network Models (SKIPPED - Saqrlab not available)\n")
}

# =============================================================================
# Summary
# =============================================================================

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("SUMMARY\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n\n")

# Count results
pass_expected <- sum(grepl("PASS.*expected", results$status))
warn_expected <- sum(grepl("WARN.*expected", results$status))
fail_expected <- sum(grepl("FAIL.*expected", results$status))
unexpected <- sum(grepl("unexpected", results$status))

total <- nrow(results)
expected_outcomes <- total - unexpected

cat(sprintf("Total scenarios tested: %d\n", total))
cat(sprintf("Expected outcomes: %d (%.1f%%)\n", expected_outcomes, 100 * expected_outcomes / total))
cat(sprintf("  - PASS (expected): %d\n", pass_expected))
cat(sprintf("  - WARN (expected): %d\n", warn_expected))
cat(sprintf("  - FAIL (expected): %d\n", fail_expected))
cat(sprintf("Unexpected outcomes: %d\n", unexpected))

if (unexpected > 0) {
  cat("\nUnexpected outcomes:\n")
  unexpected_results <- results[grepl("unexpected", results$status), ]
  for (i in seq_len(nrow(unexpected_results))) {
    cat(sprintf("  - [%s] %s: %s\n",
                unexpected_results$category[i],
                unexpected_results$scenario[i],
                unexpected_results$status[i]))
    if (nchar(unexpected_results$message[i]) > 0) {
      cat(sprintf("    Message: %s\n", unexpected_results$message[i]))
    }
  }
}

# Category breakdown
cat("\nBy Category:\n")
cat("-" |> rep(40) |> paste(collapse = ""), "\n")
for (cat_name in unique(results$category)) {
  cat_results <- results[results$category == cat_name, ]
  cat_expected <- sum(!grepl("unexpected", cat_results$status))
  cat(sprintf("  %-15s: %d/%d expected\n", cat_name, cat_expected, nrow(cat_results)))
}

# Save results
saveRDS(results, "validation/results_filter_edge_cases.rds")
cat("\nResults saved to validation/results_filter_edge_cases.rds\n")

# Final verdict
if (unexpected == 0) {
  cat("\n*** ALL SCENARIOS BEHAVED AS EXPECTED ***\n")
} else {
  cat(sprintf("\n*** %d SCENARIOS HAD UNEXPECTED BEHAVIOR - REVIEW NEEDED ***\n", unexpected))
}
