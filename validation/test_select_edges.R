# Test script for select_edges() function
# Run: Rscript validation/test_select_edges.R

library(devtools)
load_all(".")

cat("=== Testing select_edges() ===\n\n")

# Create test network
adj <- matrix(c(0, .5, .8, 0,
                .5, 0, .3, .6,
                .8, .3, 0, .4,
                 0, .6, .4, 0), 4, 4, byrow = TRUE)
rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")

cat("Test network: 4 nodes (A-D), 5 edges\n")
cat("Edges: A-B(0.5), A-C(0.8), B-C(0.3), B-D(0.6), C-D(0.4)\n\n")

# -----------------------------------------------------------------------------
# Test 1: Expression-based filtering
# -----------------------------------------------------------------------------
cat("Test 1: Expression-based filtering\n")
result1 <- select_edges(adj, weight > 0.5)
cat("  select_edges(adj, weight > 0.5)\n")
cat("  Edges remaining:", n_edges(result1), "\n")
cat("  Expected: 2 edges (A-C: 0.8, B-D: 0.6)\n\n")

# -----------------------------------------------------------------------------
# Test 2: Top N edges
# -----------------------------------------------------------------------------
cat("Test 2: Top N edges by weight\n")
result2 <- select_edges(adj, top = 3)
cat("  select_edges(adj, top = 3)\n")
cat("  Edges remaining:", n_edges(result2), "\n")
edges2 <- get_edges(result2)
cat("  Weights:", edges2$weight, "\n")
cat("  Expected: 3 edges with highest weights (0.8, 0.6, 0.5)\n\n")

# -----------------------------------------------------------------------------
# Test 3: Edges involving specific nodes
# -----------------------------------------------------------------------------
cat("Test 3: Edges involving specific nodes\n")
result3 <- select_edges(adj, involving = "A")
cat("  select_edges(adj, involving = 'A')\n")
cat("  Edges remaining:", n_edges(result3), "\n")
cat("  Expected: 2 edges (A-B, A-C)\n\n")

result3b <- select_edges(adj, involving = c("A", "D"))
cat("  select_edges(adj, involving = c('A', 'D'))\n")
cat("  Edges remaining:", n_edges(result3b), "\n")
cat("  Expected: 4 edges (A-B, A-C, B-D, C-D)\n\n")

# -----------------------------------------------------------------------------
# Test 4: Edges between node sets
# -----------------------------------------------------------------------------
cat("Test 4: Edges between node sets\n")
result4 <- select_edges(adj, between = list(c("A", "B"), c("C", "D")))
cat("  select_edges(adj, between = list(c('A', 'B'), c('C', 'D')))\n")
cat("  Edges remaining:", n_edges(result4), "\n")
cat("  Expected: 3 edges (A-C, B-C, B-D)\n\n")

# -----------------------------------------------------------------------------
# Test 5: Bridge edges
# -----------------------------------------------------------------------------
cat("Test 5: Bridge edges\n")
# Create network with clear bridge
bridge_mat <- matrix(0, 5, 5)
bridge_mat[1, 2] <- bridge_mat[2, 1] <- 1
bridge_mat[2, 3] <- bridge_mat[3, 2] <- 1  # Bridge
bridge_mat[3, 4] <- bridge_mat[4, 3] <- 1
bridge_mat[4, 5] <- bridge_mat[5, 4] <- 1
bridge_mat[3, 5] <- bridge_mat[5, 3] <- 1
rownames(bridge_mat) <- colnames(bridge_mat) <- LETTERS[1:5]

result5 <- select_edges(bridge_mat, bridges_only = TRUE)
cat("  select_edges(bridge_mat, bridges_only = TRUE)\n")
cat("  Bridge edges:", n_edges(result5), "\n")
cat("  Expected: 2 bridges (A-B, B-C)\n\n")

# -----------------------------------------------------------------------------
# Test 6: Computed metrics
# -----------------------------------------------------------------------------
cat("Test 6: Computed edge metrics\n")
result6 <- select_edges(adj, abs_weight > 0.5)
cat("  select_edges(adj, abs_weight > 0.5)\n")
cat("  Edges remaining:", n_edges(result6), "\n\n")

result6b <- select_edges(adj, from_degree >= 3 | to_degree >= 3)
cat("  select_edges(adj, from_degree >= 3 | to_degree >= 3)\n")
cat("  Edges remaining:", n_edges(result6b), "\n")
cat("  (Selects edges where either endpoint has degree >= 3)\n\n")

# -----------------------------------------------------------------------------
# Test 7: Edge betweenness
# -----------------------------------------------------------------------------
cat("Test 7: Top by edge betweenness\n")
result7 <- select_edges(adj, top = 2, by = "edge_betweenness")
cat("  select_edges(adj, top = 2, by = 'edge_betweenness')\n")
cat("  Edges remaining:", n_edges(result7), "\n\n")

# -----------------------------------------------------------------------------
# Test 8: Combined modes (AND logic)
# -----------------------------------------------------------------------------
cat("Test 8: Combined modes (AND logic)\n")
result8 <- select_edges(adj, involving = "B", top = 2)
cat("  select_edges(adj, involving = 'B', top = 2)\n")
cat("  Edges remaining:", n_edges(result8), "\n")
cat("  Expected: Top 2 edges among those involving B\n\n")

# -----------------------------------------------------------------------------
# Test 9: Same community
# -----------------------------------------------------------------------------
cat("Test 9: Same community edges\n")
# Create network with clear community structure
comm_mat <- matrix(0, 6, 6)
# Community 1: A, B, C (densely connected)
comm_mat[1, 2] <- comm_mat[2, 1] <- 1
comm_mat[2, 3] <- comm_mat[3, 2] <- 1
comm_mat[1, 3] <- comm_mat[3, 1] <- 1
# Community 2: D, E, F (densely connected)
comm_mat[4, 5] <- comm_mat[5, 4] <- 1
comm_mat[5, 6] <- comm_mat[6, 5] <- 1
comm_mat[4, 6] <- comm_mat[6, 4] <- 1
# Bridge between communities
comm_mat[3, 4] <- comm_mat[4, 3] <- 0.1
rownames(comm_mat) <- colnames(comm_mat) <- LETTERS[1:6]

result9 <- select_edges(comm_mat, same_community)
cat("  select_edges(comm_mat, same_community)\n")
cat("  Same-community edges:", n_edges(result9), "\n")
cat("  Expected: 6 edges (3 in each community), excluding bridge C-D\n\n")

result9b <- select_edges(comm_mat, !same_community)
cat("  select_edges(comm_mat, !same_community)\n")
cat("  Cross-community edges:", n_edges(result9b), "\n")
cat("  Expected: 1 edge (C-D bridge)\n\n")

# -----------------------------------------------------------------------------
# Test 10: Convenience functions
# -----------------------------------------------------------------------------
cat("Test 10: Convenience functions\n")

result10a <- select_bridges(bridge_mat)
cat("  select_bridges(bridge_mat)\n")
cat("  Bridges:", n_edges(result10a), "\n\n")

result10b <- select_top_edges(adj, n = 2)
cat("  select_top_edges(adj, n = 2)\n")
cat("  Edges:", n_edges(result10b), "\n\n")

result10c <- select_edges_involving(adj, nodes = "C")
cat("  select_edges_involving(adj, nodes = 'C')\n")
cat("  Edges:", n_edges(result10c), "\n\n")

result10d <- select_edges_between(adj, set1 = "A", set2 = c("C", "D"))
cat("  select_edges_between(adj, set1 = 'A', set2 = c('C', 'D'))\n")
cat("  Edges:", n_edges(result10d), "\n\n")

# -----------------------------------------------------------------------------
# Test 11: Format preservation
# -----------------------------------------------------------------------------
cat("Test 11: Format preservation\n")
result11 <- select_edges(adj, weight > 0.5, keep_format = TRUE)
cat("  select_edges(adj, weight > 0.5, keep_format = TRUE)\n")
cat("  Result class:", class(result11), "\n")
cat("  Expected: matrix\n\n")

# -----------------------------------------------------------------------------
# Test 12: Keep isolates
# -----------------------------------------------------------------------------
cat("Test 12: Keep isolates\n")
result12a <- select_edges(adj, involving = "A", .keep_isolates = FALSE)
cat("  select_edges(adj, involving = 'A', .keep_isolates = FALSE)\n")
cat("  Nodes:", n_nodes(result12a), "\n")
cat("  Expected: 2 nodes (A, B or A, C depending on edges)\n\n")

result12b <- select_edges(adj, involving = "A", .keep_isolates = TRUE)
cat("  select_edges(adj, involving = 'A', .keep_isolates = TRUE)\n")
cat("  Nodes:", n_nodes(result12b), "\n")
cat("  Expected: 4 nodes (all nodes kept, D is isolate)\n\n")

cat("=== All tests completed ===\n")
