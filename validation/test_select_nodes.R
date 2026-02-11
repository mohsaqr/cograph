# Test script for select_nodes() function
# Run: Rscript validation/test_select_nodes.R

library(devtools)
load_all(".")

cat("=== Testing select_nodes() ===\n\n")

# Create test network
set.seed(123)
mat <- matrix(runif(100), 10, 10)
mat <- mat * upper.tri(mat) + t(mat * upper.tri(mat))
diag(mat) <- 0
rownames(mat) <- colnames(mat) <- LETTERS[1:10]

cat("Test network: 10 nodes (A-J)\n\n")

# -----------------------------------------------------------------------------
# Test 1: Expression-based filtering (lazy)
# -----------------------------------------------------------------------------
cat("Test 1: Expression-based filtering (lazy)\n")
result1 <- select_nodes(mat, degree >= 5)
cat("  select_nodes(mat, degree >= 5)\n")
cat("  Selected nodes:", get_labels(result1), "\n")
cat("  Expected: nodes with degree >= 5\n\n")

# -----------------------------------------------------------------------------
# Test 2: By name
# -----------------------------------------------------------------------------
cat("Test 2: Selection by name\n")
result2 <- select_nodes(mat, name = c("A", "B", "C"))
cat("  select_nodes(mat, name = c('A', 'B', 'C'))\n")
cat("  Selected nodes:", get_labels(result2), "\n")
cat("  Expected: A, B, C\n\n")

# -----------------------------------------------------------------------------
# Test 3: By index
# -----------------------------------------------------------------------------
cat("Test 3: Selection by index\n")
result3 <- select_nodes(mat, index = c(1, 3, 5))
cat("  select_nodes(mat, index = c(1, 3, 5))\n")
cat("  Selected nodes:", get_labels(result3), "\n")
cat("  Expected: A, C, E\n\n")

# -----------------------------------------------------------------------------
# Test 4: Top N by centrality
# -----------------------------------------------------------------------------
cat("Test 4: Top N by centrality\n")
result4 <- select_nodes(mat, top = 3, by = "pagerank")
cat("  select_nodes(mat, top = 3, by = 'pagerank')\n")
cat("  Selected nodes:", get_labels(result4), "\n")
cat("  Expected: top 3 nodes by PageRank\n\n")

# Verify with manual PageRank
g <- to_igraph(mat)
pr <- igraph::page_rank(g)$vector
cat("  PageRank values:\n")
print(sort(pr, decreasing = TRUE))
cat("\n")

# -----------------------------------------------------------------------------
# Test 5: Neighborhood selection
# -----------------------------------------------------------------------------
cat("Test 5: Neighborhood selection\n")
result5 <- select_nodes(mat, neighbors_of = "A", order = 1)
cat("  select_nodes(mat, neighbors_of = 'A', order = 1)\n")
cat("  Selected nodes:", get_labels(result5), "\n")
cat("  Expected: A and all direct neighbors of A\n\n")

# -----------------------------------------------------------------------------
# Test 6: Component selection
# -----------------------------------------------------------------------------
cat("Test 6: Component selection (creating disconnected network)\n")
# Create disconnected network
disc_mat <- matrix(0, 6, 6)
disc_mat[1, 2] <- disc_mat[2, 1] <- 1
disc_mat[1, 3] <- disc_mat[3, 1] <- 1
disc_mat[4, 5] <- disc_mat[5, 4] <- 1
disc_mat[5, 6] <- disc_mat[6, 5] <- 1
disc_mat[4, 6] <- disc_mat[6, 4] <- 1
rownames(disc_mat) <- colnames(disc_mat) <- LETTERS[1:6]

result6 <- select_nodes(disc_mat, component = "largest")
cat("  select_nodes(disc_mat, component = 'largest')\n")
cat("  Selected nodes:", get_labels(result6), "\n")
cat("  Expected: D, E, F (component with 3 nodes)\n\n")

result6b <- select_nodes(disc_mat, component = "A")
cat("  select_nodes(disc_mat, component = 'A')\n")
cat("  Selected nodes:", get_labels(result6b), "\n")
cat("  Expected: A, B, C (component containing A)\n\n")

# -----------------------------------------------------------------------------
# Test 7: Global context variables
# -----------------------------------------------------------------------------
cat("Test 7: Global context variables\n")
result7 <- select_nodes(disc_mat, is_largest_component)
cat("  select_nodes(disc_mat, is_largest_component)\n")
cat("  Selected nodes:", get_labels(result7), "\n")
cat("  Expected: D, E, F\n\n")

# -----------------------------------------------------------------------------
# Test 8: Combined selection (AND logic)
# -----------------------------------------------------------------------------
cat("Test 8: Combined selection (AND logic)\n")
result8 <- select_nodes(mat, top = 5, name = c("A", "B", "C", "D", "E", "F"))
cat("  select_nodes(mat, top = 5, name = c('A', 'B', 'C', 'D', 'E', 'F'))\n")
cat("  Selected nodes:", get_labels(result8), "\n")
cat("  Expected: top 5 by degree among A-F only\n\n")

# -----------------------------------------------------------------------------
# Test 9: Convenience functions
# -----------------------------------------------------------------------------
cat("Test 9: Convenience functions\n")

result9a <- select_neighbors(mat, of = "A")
cat("  select_neighbors(mat, of = 'A')\n")
cat("  Selected nodes:", get_labels(result9a), "\n\n")

result9b <- select_component(disc_mat, which = "largest")
cat("  select_component(disc_mat, which = 'largest')\n")
cat("  Selected nodes:", get_labels(result9b), "\n\n")

result9c <- select_top(mat, n = 3)
cat("  select_top(mat, n = 3)\n")
cat("  Selected nodes:", get_labels(result9c), "\n\n")

# -----------------------------------------------------------------------------
# Test 10: Format preservation
# -----------------------------------------------------------------------------
cat("Test 10: Format preservation\n")
result10 <- select_nodes(mat, degree >= 5, keep_format = TRUE)
cat("  select_nodes(mat, degree >= 5, keep_format = TRUE)\n")
cat("  Result class:", class(result10), "\n")
cat("  Expected: matrix\n\n")

# -----------------------------------------------------------------------------
# Test 11: Articulation points and k-core
# -----------------------------------------------------------------------------
cat("Test 11: Articulation points\n")
# Create a network with clear articulation point
art_mat <- matrix(0, 5, 5)
art_mat[1, 2] <- art_mat[2, 1] <- 1
art_mat[2, 3] <- art_mat[3, 2] <- 1
art_mat[3, 4] <- art_mat[4, 3] <- 1
art_mat[3, 5] <- art_mat[5, 3] <- 1
rownames(art_mat) <- colnames(art_mat) <- c("A", "B", "C", "D", "E")

result11 <- select_nodes(art_mat, is_articulation)
cat("  select_nodes(art_mat, is_articulation)\n")
cat("  Selected nodes:", get_labels(result11), "\n")
cat("  Expected: B, C (articulation points)\n\n")

# -----------------------------------------------------------------------------
# Test 12: K-core
# -----------------------------------------------------------------------------
cat("Test 12: K-core selection\n")
result12 <- select_nodes(mat, k_core >= 2)
cat("  select_nodes(mat, k_core >= 2)\n")
cat("  Selected nodes:", get_labels(result12), "\n")
cat("  Expected: nodes in 2-core or higher\n\n")

cat("=== All tests completed ===\n")
