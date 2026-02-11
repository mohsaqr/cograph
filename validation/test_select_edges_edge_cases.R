# Edge Case Tests for select_edges()
# Run: Rscript validation/test_select_edges_edge_cases.R

library(devtools)
load_all(".")

cat("=== Edge Case Tests for select_edges() ===\n\n")

errors <- 0
tests <- 0

test <- function(name, expr) {
  tests <<- tests + 1
  result <- tryCatch({
    expr
    cat("PASS:", name, "\n")
    TRUE
  }, error = function(e) {
    cat("FAIL:", name, "\n")
    cat("  Error:", conditionMessage(e), "\n")
    errors <<- errors + 1
    FALSE
  }, warning = function(w) {
    cat("WARN:", name, "\n")
    cat("  Warning:", conditionMessage(w), "\n")
    TRUE
  })
}

# =============================================================================
# 1. Empty and Minimal Networks
# =============================================================================
cat("\n--- 1. Empty and Minimal Networks ---\n")

# Empty network (no edges)
test("Empty network (no edges)", {
  mat <- matrix(0, 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- select_edges(mat, weight > 0)
  stopifnot(n_edges(result) == 0)
})

# Single node (impossible to have edges)
test("Single node network", {
  mat <- matrix(0, 1, 1)
  rownames(mat) <- colnames(mat) <- "A"
  result <- select_edges(mat, weight > 0)
  stopifnot(n_edges(result) == 0)
})

# Single edge
test("Single edge network", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, weight > 0)
  stopifnot(n_edges(result) == 1)
})

# =============================================================================
# 2. Self-loops
# =============================================================================
cat("\n--- 2. Self-loops ---\n")

test("Network with self-loops", {
  mat <- matrix(c(0.5, 1, 1, 0.3), 2, 2)  # Diagonal has self-loops
  rownames(mat) <- colnames(mat) <- c("A", "B")
  net <- as_cograph(mat)
  # Check if self-loops are present
  edges <- get_edges(net)
  cat("  Edges in network:", nrow(edges), "\n")
})

# =============================================================================
# 3. Filter Results
# =============================================================================
cat("\n--- 3. Filter Results ---\n")

test("All edges filtered out", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, weight > 1)  # No edges > 1
  stopifnot(n_edges(result) == 0)
})

test("All edges selected", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, weight > 0)
  stopifnot(n_edges(result) == 1)
})

test("No edges match expression", {
  mat <- matrix(c(0, 0.5, 0.8, 0.5, 0, 0.3, 0.8, 0.3, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- select_edges(mat, weight > 10)
  stopifnot(n_edges(result) == 0)
})

# =============================================================================
# 4. Special Weight Values
# =============================================================================
cat("\n--- 4. Special Weight Values ---\n")

test("Negative weights", {
  mat <- matrix(c(0, -0.5, 0.8, -0.5, 0, -0.3, 0.8, -0.3, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Filter by weight
  result1 <- select_edges(mat, weight > 0)
  stopifnot(n_edges(result1) == 1)  # Only A-C

  # Filter by abs_weight
  result2 <- select_edges(mat, abs_weight > 0.4)
  stopifnot(n_edges(result2) == 2)  # A-B(-0.5), A-C(0.8)
})

test("Zero weights", {
  mat <- matrix(c(0, 0, 0.5, 0, 0, 0.3, 0.5, 0.3, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- select_edges(mat, weight > 0)
  stopifnot(n_edges(result) == 2)  # A-C, B-C
})

test("Mixed positive/negative/zero weights", {
  mat <- matrix(c(0, -0.5, 0, 0.8,
                  -0.5, 0, 0.3, 0,
                  0, 0.3, 0, -0.2,
                  0.8, 0, -0.2, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- select_edges(mat, weight != 0)
  cat("  Non-zero edges:", n_edges(result), "\n")
})

# =============================================================================
# 5. Invalid/Edge Case Parameters
# =============================================================================
cat("\n--- 5. Invalid/Edge Case Parameters ---\n")

test("involving non-existent node", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, involving = "Z")  # Z doesn't exist
  stopifnot(n_edges(result) == 0)
})

test("involving empty vector", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, involving = character(0))
  stopifnot(n_edges(result) == 0)
})

test("between with empty set", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, between = list(c("A"), character(0)))
  stopifnot(n_edges(result) == 0)
})

test("between with overlapping sets", {
  mat <- matrix(c(0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  # B is in both sets
  result <- select_edges(mat, between = list(c("A", "B"), c("B", "C")))
  cat("  Edges between overlapping sets:", n_edges(result), "\n")
})

test("top = 0", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, top = 0)
  stopifnot(n_edges(result) == 0)
})

test("top > n_edges", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, top = 100)  # Only 1 edge exists
  stopifnot(n_edges(result) == 1)
})

test("top with negative value", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, top = -5)
  stopifnot(n_edges(result) == 0)
})

test("involving with out-of-range index", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, involving = 100)  # Index 100 doesn't exist
  stopifnot(n_edges(result) == 0)
})

# =============================================================================
# 6. Directed Networks
# =============================================================================
cat("\n--- 6. Directed Networks ---\n")

test("Directed network - asymmetric", {
  mat <- matrix(c(0, 0.5, 0, 0, 0, 0.3, 0.8, 0, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- select_edges(mat, weight > 0, directed = TRUE)
  cat("  Directed edges:", n_edges(result), "\n")
})

test("Directed network - mutual_only", {
  # Create directed network with some mutual edges
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 0, 0), 3, 3)  # A<->B mutual, B->C one-way
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- select_edges(mat, mutual_only = TRUE, directed = TRUE)
  cat("  Mutual edges:", n_edges(result), "\n")
  stopifnot(n_edges(result) == 2)  # A->B and B->A
})

test("Undirected network - mutual_only (should select all)", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- select_edges(mat, mutual_only = TRUE)
  stopifnot(n_edges(result) == 3)  # All edges are "mutual" in undirected
})

# =============================================================================
# 7. Bridge Detection Edge Cases
# =============================================================================
cat("\n--- 7. Bridge Detection ---\n")

test("Network with no bridges (complete graph)", {
  mat <- matrix(1, 4, 4)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- select_edges(mat, bridges_only = TRUE)
  stopifnot(n_edges(result) == 0)  # Complete graph has no bridges
})

test("Network where all edges are bridges (tree)", {
  mat <- matrix(0, 4, 4)
  mat[1,2] <- mat[2,1] <- 1
  mat[2,3] <- mat[3,2] <- 1
  mat[3,4] <- mat[4,3] <- 1
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- select_edges(mat, bridges_only = TRUE)
  stopifnot(n_edges(result) == 3)  # All edges are bridges in a tree
})

test("Single edge network - is bridge", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, bridges_only = TRUE)
  stopifnot(n_edges(result) == 1)
})

# =============================================================================
# 8. Community Detection Edge Cases
# =============================================================================
cat("\n--- 8. Community Detection ---\n")

test("All nodes in same community", {
  mat <- matrix(1, 3, 3)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- select_edges(mat, same_community)
  stopifnot(n_edges(result) == 3)  # All in same community
})

test("Each node in own community (disconnected)", {
  mat <- matrix(0, 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  # No edges, so no same_community edges
  result <- select_edges(mat, same_community)
  stopifnot(n_edges(result) == 0)
})

test("Cross-community edges only", {
  # Two clear communities
  mat <- matrix(0, 4, 4)
  mat[1,2] <- mat[2,1] <- 1  # Community 1
  mat[3,4] <- mat[4,3] <- 1  # Community 2
  mat[2,3] <- mat[3,2] <- 0.1  # Bridge
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- select_edges(mat, !same_community)
  cat("  Cross-community edges:", n_edges(result), "\n")
})

# =============================================================================
# 9. Endpoint Metrics Edge Cases
# =============================================================================
cat("\n--- 9. Endpoint Metrics ---\n")

test("from_degree and to_degree", {
  mat <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), 3, 3)  # A has degree 2, B and C have degree 1
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- select_edges(mat, from_degree >= 2 | to_degree >= 2)
  stopifnot(n_edges(result) == 2)  # Both edges involve A
})

test("Edge where both endpoints have same degree", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, from_degree == to_degree)
  stopifnot(n_edges(result) == 1)  # Both have degree 1
})

# =============================================================================
# 10. Combined Modes
# =============================================================================
cat("\n--- 10. Combined Modes ---\n")

test("involving + top", {
  mat <- matrix(c(0, .5, .8, .3,
                  .5, 0, .2, .6,
                  .8, .2, 0, .4,
                  .3, .6, .4, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  # Top 2 edges involving A
  result <- select_edges(mat, involving = "A", top = 2)
  stopifnot(n_edges(result) == 2)

  # Verify they're the top 2 among A's edges
  edges <- get_edges(result)
  cat("  Top 2 weights involving A:", sort(edges$weight, decreasing = TRUE), "\n")
})

test("between + expression", {
  mat <- matrix(c(0, .5, .8, .3,
                  .5, 0, .2, .6,
                  .8, .2, 0, .4,
                  .3, .6, .4, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  # Edges between {A,B} and {C,D} with weight > 0.5
  result <- select_edges(mat, between = list(c("A","B"), c("C","D")), weight > 0.5)
  cat("  Edges between sets with weight > 0.5:", n_edges(result), "\n")
})

test("bridges_only + top", {
  # Tree with varying weights
  mat <- matrix(0, 4, 4)
  mat[1,2] <- mat[2,1] <- 0.5
  mat[2,3] <- mat[3,2] <- 0.8
  mat[3,4] <- mat[4,3] <- 0.3
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- select_edges(mat, bridges_only = TRUE, top = 2)
  stopifnot(n_edges(result) == 2)
  edges <- get_edges(result)
  cat("  Top 2 bridges:", sort(edges$weight, decreasing = TRUE), "\n")
})

test("All modes combined", {
  mat <- matrix(c(0, .5, .8, .3,
                  .5, 0, .2, .6,
                  .8, .2, 0, .4,
                  .3, .6, .4, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  # Multiple conditions
  result <- select_edges(mat,
                         involving = c("A", "B"),
                         top = 3,
                         weight > 0.3)
  cat("  Combined result edges:", n_edges(result), "\n")
})

# =============================================================================
# 11. Format Preservation
# =============================================================================
cat("\n--- 11. Format Preservation ---\n")

test("Matrix input, keep_format = TRUE", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_edges(mat, weight > 0, keep_format = TRUE)
  stopifnot(is.matrix(result))
})

test("cograph_network input", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  net <- as_cograph(mat)
  result <- select_edges(net, weight > 0)
  stopifnot(inherits(result, "cograph_network"))
})

test("igraph input, keep_format = TRUE", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
  result <- select_edges(g, weight > 0, keep_format = TRUE)
  stopifnot(inherits(result, "igraph"))
})

# =============================================================================
# 12. Chaining Operations
# =============================================================================
cat("\n--- 12. Chaining Operations ---\n")

test("Multiple select_edges calls", {
  mat <- matrix(c(0, .5, .8, .3,
                  .5, 0, .2, .6,
                  .8, .2, 0, .4,
                  .3, .6, .4, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- mat |>
    select_edges(weight > 0.3) |>
    select_edges(top = 2)

  stopifnot(n_edges(result) == 2)
})

test("select_edges then select_nodes", {
  mat <- matrix(c(0, .5, .8, .3,
                  .5, 0, .2, .6,
                  .8, .2, 0, .4,
                  .3, .6, .4, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- mat |>
    select_edges(weight > 0.5) |>
    select_nodes(degree >= 1)

  cat("  After edge+node filtering: nodes =", n_nodes(result), ", edges =", n_edges(result), "\n")
})

# =============================================================================
# 13. Keep Isolates
# =============================================================================
cat("\n--- 13. Keep Isolates ---\n")

test("keep_isolates = FALSE (default)", {
  mat <- matrix(c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- select_edges(mat, involving = "A")
  stopifnot(n_nodes(result) == 2)  # Only A and B (C, D are isolates)
})

test("keep_isolates = TRUE", {
  mat <- matrix(c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- select_edges(mat, involving = "A", .keep_isolates = TRUE)
  stopifnot(n_nodes(result) == 4)  # All nodes kept
  stopifnot(n_edges(result) == 1)  # Only A-B edge
})

test("All edges removed, keep_isolates = TRUE", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")

  result <- select_edges(mat, weight > 100, .keep_isolates = TRUE)
  stopifnot(n_nodes(result) == 2)
  stopifnot(n_edges(result) == 0)
})

# =============================================================================
# 14. Edge Betweenness Edge Cases
# =============================================================================
cat("\n--- 14. Edge Betweenness ---\n")

test("Top by edge_betweenness", {
  mat <- matrix(0, 5, 5)
  mat[1,2] <- mat[2,1] <- 1
  mat[2,3] <- mat[3,2] <- 1  # This should have highest betweenness
  mat[3,4] <- mat[4,3] <- 1
  mat[4,5] <- mat[5,4] <- 1
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  result <- select_edges(mat, top = 1, by = "edge_betweenness")
  stopifnot(n_edges(result) == 1)

  # The middle edge should have highest betweenness
  edges <- get_edges(result)
  nodes <- get_nodes(result)
  cat("  Highest betweenness edge:", nodes$label[edges$from], "-", nodes$label[edges$to], "\n")
})

test("Filter by edge_betweenness expression", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- select_edges(mat, edge_betweenness > 0)
  cat("  Edges with betweenness > 0:", n_edges(result), "\n")
})

# =============================================================================
# 15. is_bridge in Expression
# =============================================================================
cat("\n--- 15. is_bridge Expression ---\n")

test("is_bridge in expression", {
  mat <- matrix(0, 4, 4)
  mat[1,2] <- mat[2,1] <- 1
  mat[2,3] <- mat[3,2] <- 1  # Bridge
  mat[3,4] <- mat[4,3] <- 1
  mat[1,4] <- mat[4,1] <- 1  # Creates cycle, so 2-3 is still bridge
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- select_edges(mat, is_bridge)
  cat("  Bridges found:", n_edges(result), "\n")
})

test("!is_bridge (non-bridge edges)", {
  mat <- matrix(0, 4, 4)
  mat[1,2] <- mat[2,1] <- 1
  mat[2,3] <- mat[3,2] <- 1
  mat[3,4] <- mat[4,3] <- 1
  mat[1,4] <- mat[4,1] <- 1  # Creates cycle
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- select_edges(mat, !is_bridge)
  cat("  Non-bridge edges:", n_edges(result), "\n")
})

# =============================================================================
# 16. is_mutual in Expression
# =============================================================================
cat("\n--- 16. is_mutual Expression ---\n")

test("is_mutual in directed network", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 0, 0), 3, 3)  # A->B, B->A (mutual), B->C (not mutual)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- select_edges(mat, is_mutual, directed = TRUE)
  cat("  Mutual edges:", n_edges(result), "\n")
})

test("!is_mutual (one-way edges)", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 0, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- select_edges(mat, !is_mutual, directed = TRUE)
  cat("  One-way edges:", n_edges(result), "\n")
})

# =============================================================================
# 17. Convenience Functions Edge Cases
# =============================================================================
cat("\n--- 17. Convenience Functions ---\n")

test("select_bridges on bridgeless graph", {
  mat <- matrix(1, 3, 3)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- select_bridges(mat)
  stopifnot(n_edges(result) == 0)
})

test("select_top_edges with n=0", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(mat) <- colnames(mat) <- c("A", "B")
  result <- select_top_edges(mat, n = 0)
  stopifnot(n_edges(result) == 0)
})

test("select_edges_involving with multiple nodes", {
  mat <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- select_edges_involving(mat, nodes = c("A", "D"))
  cat("  Edges involving A or D:", n_edges(result), "\n")
})

test("select_edges_between with same node in both sets", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  # B is in both sets
  result <- select_edges_between(mat, set1 = c("A", "B"), set2 = c("B", "C"))
  cat("  Edges between overlapping sets:", n_edges(result), "\n")
})

# =============================================================================
# Summary
# =============================================================================
cat("\n=== Summary ===\n")
cat("Tests run:", tests, "\n")
cat("Errors:", errors, "\n")

if (errors == 0) {
  cat("\nAll edge case tests passed!\n")
} else {
  cat("\nSome tests failed. Please review.\n")
}
