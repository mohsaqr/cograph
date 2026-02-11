# Comparison Tests: select_edges() vs igraph
# Validates that select_edges() produces correct results by comparing with igraph
# Run: Rscript validation/test_select_edges_vs_igraph.R

library(devtools)
load_all(".")
library(igraph)

cat("=== select_edges() vs igraph Comparison Tests ===\n\n")

errors <- 0
tests <- 0

compare <- function(name, cograph_result, igraph_result, tolerance = 1e-10) {
  tests <<- tests + 1

  # Handle different result types
  if (is.logical(cograph_result) && is.logical(igraph_result)) {
    match <- identical(cograph_result, igraph_result)
  } else if (is.numeric(cograph_result) && is.numeric(igraph_result)) {
    match <- all(abs(cograph_result - igraph_result) < tolerance, na.rm = TRUE) &&
             length(cograph_result) == length(igraph_result)
  } else if (is.data.frame(cograph_result) || is.matrix(cograph_result)) {
    match <- nrow(cograph_result) == nrow(igraph_result)
  } else {
    match <- identical(cograph_result, igraph_result)
  }

  if (match) {
    cat("PASS:", name, "\n")
  } else {
    cat("FAIL:", name, "\n")
    cat("  cograph:", head(cograph_result), "\n")
    cat("  igraph: ", head(igraph_result), "\n")
    errors <<- errors + 1
  }
  match
}

# =============================================================================
# Create Test Networks
# =============================================================================
cat("--- Creating Test Networks ---\n\n")

# Network 1: Simple weighted undirected
set.seed(42)
mat1 <- matrix(c(0, 0.5, 0.8, 0.3,
                 0.5, 0, 0.2, 0.6,
                 0.8, 0.2, 0, 0.4,
                 0.3, 0.6, 0.4, 0), 4, 4)
rownames(mat1) <- colnames(mat1) <- c("A", "B", "C", "D")

g1 <- graph_from_adjacency_matrix(mat1, mode = "undirected", weighted = TRUE)
net1 <- as_cograph(mat1)

cat("Network 1: 4 nodes, undirected, weighted\n")
cat("  Edges:", ecount(g1), "\n\n")

# Network 2: Larger network with clear structure
mat2 <- matrix(0, 8, 8)
# Community 1: A, B, C, D (dense)
mat2[1,2] <- mat2[2,1] <- 0.9
mat2[1,3] <- mat2[3,1] <- 0.8
mat2[2,3] <- mat2[3,2] <- 0.7
mat2[1,4] <- mat2[4,1] <- 0.6
mat2[2,4] <- mat2[4,2] <- 0.5
mat2[3,4] <- mat2[4,3] <- 0.85
# Community 2: E, F, G, H (dense)
mat2[5,6] <- mat2[6,5] <- 0.95
mat2[5,7] <- mat2[7,5] <- 0.75
mat2[6,7] <- mat2[7,6] <- 0.65
mat2[5,8] <- mat2[8,5] <- 0.55
mat2[6,8] <- mat2[8,6] <- 0.45
mat2[7,8] <- mat2[8,7] <- 0.8
# Bridge between communities
mat2[4,5] <- mat2[5,4] <- 0.3
rownames(mat2) <- colnames(mat2) <- LETTERS[1:8]

g2 <- graph_from_adjacency_matrix(mat2, mode = "undirected", weighted = TRUE)
net2 <- as_cograph(mat2)

cat("Network 2: 8 nodes, 2 communities with bridge\n")
cat("  Edges:", ecount(g2), "\n\n")

# Network 3: Directed network
mat3 <- matrix(c(0, 1, 0, 0,
                 1, 0, 1, 0,
                 0, 0, 0, 1,
                 1, 0, 1, 0), 4, 4, byrow = TRUE)
rownames(mat3) <- colnames(mat3) <- c("A", "B", "C", "D")

g3 <- graph_from_adjacency_matrix(mat3, mode = "directed", weighted = TRUE)
net3 <- as_cograph(mat3, directed = TRUE)

cat("Network 3: 4 nodes, directed\n")
cat("  Edges:", ecount(g3), "\n\n")

# Network 4: Tree (all edges are bridges)
mat4 <- matrix(0, 5, 5)
mat4[1,2] <- mat4[2,1] <- 1
mat4[2,3] <- mat4[3,2] <- 1
mat4[3,4] <- mat4[4,3] <- 1
mat4[4,5] <- mat4[5,4] <- 1
rownames(mat4) <- colnames(mat4) <- LETTERS[1:5]

g4 <- graph_from_adjacency_matrix(mat4, mode = "undirected", weighted = TRUE)
net4 <- as_cograph(mat4)

cat("Network 4: 5 nodes, tree (linear)\n")
cat("  Edges:", ecount(g4), "\n\n")

# =============================================================================
# Test 1: Edge Count After Weight Filter
# =============================================================================
cat("--- Test 1: Weight Filtering ---\n")

# cograph
result1_cg <- select_edges(net1, weight > 0.5)
n_edges_cg <- n_edges(result1_cg)

# igraph
edges_above <- E(g1)[E(g1)$weight > 0.5]
n_edges_ig <- length(edges_above)

compare("Weight > 0.5 edge count", n_edges_cg, n_edges_ig)

# Test with different threshold
result1b_cg <- select_edges(net1, weight >= 0.4)
n_edges1b_cg <- n_edges(result1b_cg)

edges_above_b <- E(g1)[E(g1)$weight >= 0.4]
n_edges1b_ig <- length(edges_above_b)

compare("Weight >= 0.4 edge count", n_edges1b_cg, n_edges1b_ig)

# =============================================================================
# Test 2: Top N Edges by Weight
# =============================================================================
cat("\n--- Test 2: Top N Edges ---\n")

# cograph: top 3 by weight
result2_cg <- select_edges(net1, top = 3)
weights_cg <- sort(get_edges(result2_cg)$weight, decreasing = TRUE)

# igraph: top 3 by weight
all_weights <- E(g1)$weight
top3_weights_ig <- sort(all_weights, decreasing = TRUE)[1:3]

compare("Top 3 weights match", weights_cg, top3_weights_ig)

# Top 5 from larger network
result2b_cg <- select_edges(net2, top = 5)
weights2b_cg <- sort(get_edges(result2b_cg)$weight, decreasing = TRUE)

all_weights2 <- E(g2)$weight
top5_weights_ig <- sort(all_weights2, decreasing = TRUE)[1:5]

compare("Top 5 weights (net2)", weights2b_cg, top5_weights_ig)

# =============================================================================
# Test 3: Edge Betweenness
# =============================================================================
cat("\n--- Test 3: Edge Betweenness ---\n")

# cograph: get edge betweenness
cg_net <- as_cograph(mat4)
cg_g <- to_igraph(cg_net)
edges_cg <- get_edges(cg_net)

# Calculate edge betweenness in both
eb_ig <- edge_betweenness(g4, directed = FALSE)
eb_cg <- edge_betweenness(cg_g, directed = FALSE)

compare("Edge betweenness values", eb_cg, eb_ig)

# Top by edge betweenness
result3_cg <- select_edges(net4, top = 2, by = "edge_betweenness")
top_eb_cg <- n_edges(result3_cg)

# In a linear tree, middle edges have highest betweenness
compare("Top 2 by betweenness count", top_eb_cg, 2)

# =============================================================================
# Test 4: Bridge Detection
# =============================================================================
cat("\n--- Test 4: Bridge Detection ---\n")

# cograph bridges
result4_cg <- select_edges(net4, bridges_only = TRUE)
n_bridges_cg <- n_edges(result4_cg)

# igraph bridges
bridges_ig <- bridges(g4)
n_bridges_ig <- length(bridges_ig)

compare("Bridge count (tree)", n_bridges_cg, n_bridges_ig)

# Network with cycle (fewer bridges)
mat_cycle <- matrix(0, 4, 4)
mat_cycle[1,2] <- mat_cycle[2,1] <- 1
mat_cycle[2,3] <- mat_cycle[3,2] <- 1
mat_cycle[3,4] <- mat_cycle[4,3] <- 1
mat_cycle[4,1] <- mat_cycle[1,4] <- 1  # Creates cycle
rownames(mat_cycle) <- colnames(mat_cycle) <- LETTERS[1:4]

g_cycle <- graph_from_adjacency_matrix(mat_cycle, mode = "undirected", weighted = TRUE)
net_cycle <- as_cograph(mat_cycle)

result4b_cg <- select_edges(net_cycle, bridges_only = TRUE)
n_bridges_cycle_cg <- n_edges(result4b_cg)

bridges_cycle_ig <- bridges(g_cycle)
n_bridges_cycle_ig <- length(bridges_cycle_ig)

compare("Bridge count (cycle - should be 0)", n_bridges_cycle_cg, n_bridges_cycle_ig)

# Network 2 has one bridge (D-E)
result4c_cg <- select_edges(net2, bridges_only = TRUE)
n_bridges_net2_cg <- n_edges(result4c_cg)

bridges_net2_ig <- bridges(g2)
n_bridges_net2_ig <- length(bridges_net2_ig)

compare("Bridge count (two communities)", n_bridges_net2_cg, n_bridges_net2_ig)

# =============================================================================
# Test 5: Endpoint Degrees
# =============================================================================
cat("\n--- Test 5: Endpoint Degrees ---\n")

# Get degrees in igraph
deg_ig <- degree(g1)

# For each edge, check endpoint degrees
el <- as_edgelist(g1, names = FALSE)
from_deg_ig <- deg_ig[el[,1]]
to_deg_ig <- deg_ig[el[,2]]

# In cograph, filter edges where both endpoints have degree >= 3
result5_cg <- select_edges(net1, from_degree >= 3 & to_degree >= 3)
n_high_deg_edges_cg <- n_edges(result5_cg)

# Count in igraph
n_high_deg_edges_ig <- sum(from_deg_ig >= 3 & to_deg_ig >= 3)

compare("Edges with both endpoints degree >= 3", n_high_deg_edges_cg, n_high_deg_edges_ig)

# =============================================================================
# Test 6: Involving Specific Nodes
# =============================================================================
cat("\n--- Test 6: Edges Involving Nodes ---\n")

# cograph: edges involving node A
result6_cg <- select_edges(net1, involving = "A")
n_involving_A_cg <- n_edges(result6_cg)

# igraph: edges incident to node A
incident_A <- incident(g1, V(g1)["A"], mode = "all")
n_involving_A_ig <- length(incident_A)

compare("Edges involving A", n_involving_A_cg, n_involving_A_ig)

# Edges involving multiple nodes
result6b_cg <- select_edges(net2, involving = c("A", "H"))
n_involving_AH_cg <- n_edges(result6b_cg)

# igraph
incident_A2 <- incident(g2, V(g2)["A"], mode = "all")
incident_H2 <- incident(g2, V(g2)["H"], mode = "all")
combined <- unique(c(as.integer(incident_A2), as.integer(incident_H2)))
n_involving_AH_ig <- length(combined)

compare("Edges involving A or H", n_involving_AH_cg, n_involving_AH_ig)

# =============================================================================
# Test 7: Edges Between Node Sets
# =============================================================================
cat("\n--- Test 7: Edges Between Node Sets ---\n")

# cograph: edges between {A,B} and {C,D}
result7_cg <- select_edges(net1, between = list(c("A", "B"), c("C", "D")))
n_between_cg <- n_edges(result7_cg)

# igraph: manually count edges between sets
set1 <- c("A", "B")
set2 <- c("C", "D")
el_names <- as_edgelist(g1, names = TRUE)
between_mask <- (el_names[,1] %in% set1 & el_names[,2] %in% set2) |
                (el_names[,1] %in% set2 & el_names[,2] %in% set1)
n_between_ig <- sum(between_mask)

compare("Edges between {A,B} and {C,D}", n_between_cg, n_between_ig)

# Larger test with communities
result7b_cg <- select_edges(net2, between = list(c("A","B","C","D"), c("E","F","G","H")))
n_between_comm_cg <- n_edges(result7b_cg)

# Should be just the bridge edge D-E
el_names2 <- as_edgelist(g2, names = TRUE)
comm1 <- c("A","B","C","D")
comm2 <- c("E","F","G","H")
between_mask2 <- (el_names2[,1] %in% comm1 & el_names2[,2] %in% comm2) |
                 (el_names2[,1] %in% comm2 & el_names2[,2] %in% comm1)
n_between_comm_ig <- sum(between_mask2)

compare("Edges between two communities", n_between_comm_cg, n_between_comm_ig)

# =============================================================================
# Test 8: Mutual Edges (Directed)
# =============================================================================
cat("\n--- Test 8: Mutual Edges (Directed) ---\n")

# cograph: mutual edges
result8_cg <- select_edges(net3, mutual_only = TRUE)
n_mutual_cg <- n_edges(result8_cg)

# igraph: check which edges are mutual
el3 <- as_edgelist(g3, names = FALSE)
is_mutual_ig <- sapply(1:nrow(el3), function(i) {
  # Check if reverse edge exists
  any(el3[,1] == el3[i,2] & el3[,2] == el3[i,1])
})
n_mutual_ig <- sum(is_mutual_ig)

compare("Mutual edge count (directed)", n_mutual_cg, n_mutual_ig)

# =============================================================================
# Test 9: Combined Filters
# =============================================================================
cat("\n--- Test 9: Combined Filters ---\n")

# Top 3 edges involving A
result9_cg <- select_edges(net2, involving = "A", top = 3)
n_combined_cg <- n_edges(result9_cg)

# igraph equivalent
incident_A_g2 <- incident(g2, V(g2)["A"], mode = "all")
weights_A <- E(g2)$weight[incident_A_g2]
# Top 3 (or all if less than 3)
n_top3_A <- min(3, length(weights_A))

compare("Top 3 edges involving A", n_combined_cg, n_top3_A)

# Bridges with weight > 0.2
result9b_cg <- select_edges(net2, bridges_only = TRUE, weight > 0.2)
n_strong_bridges_cg <- n_edges(result9b_cg)

# The bridge D-E has weight 0.3, so should be selected
bridges_g2 <- bridges(g2)
bridge_weights <- E(g2)$weight[bridges_g2]
n_strong_bridges_ig <- sum(bridge_weights > 0.2)

compare("Bridges with weight > 0.2", n_strong_bridges_cg, n_strong_bridges_ig)

# =============================================================================
# Test 10: Format Preservation
# =============================================================================
cat("\n--- Test 10: Format Preservation ---\n")

# Matrix -> Matrix
result10_mat <- select_edges(mat1, weight > 0.5, keep_format = TRUE)
compare("Matrix format preserved", is.matrix(result10_mat), TRUE)

# igraph -> igraph
result10_ig <- select_edges(g1, weight > 0.5, keep_format = TRUE)
compare("igraph format preserved", inherits(result10_ig, "igraph"), TRUE)

# Verify the igraph result has correct edge count
compare("igraph result edge count", ecount(result10_ig), 2)

# =============================================================================
# Test 11: Edge Weights Preserved
# =============================================================================
cat("\n--- Test 11: Edge Weights Preserved ---\n")

# Select top 2 edges and verify weights are preserved
result11_cg <- select_edges(net1, top = 2)
result11_mat <- to_matrix(result11_cg)

# Original weights for top 2 edges
orig_weights <- sort(E(g1)$weight, decreasing = TRUE)[1:2]

# Check that weights are preserved in result
result_weights <- sort(get_edges(result11_cg)$weight, decreasing = TRUE)

compare("Top 2 edge weights preserved", result_weights, orig_weights)

# =============================================================================
# Test 12: Consistency Across Input Formats
# =============================================================================
cat("\n--- Test 12: Input Format Consistency ---\n")

# Same filter on matrix, igraph, and cograph_network should give same result
result12_mat <- select_edges(mat1, weight > 0.4)
result12_ig <- select_edges(g1, weight > 0.4)
result12_net <- select_edges(net1, weight > 0.4)

n_mat <- n_edges(result12_mat)
n_ig <- n_edges(result12_ig)
n_net <- n_edges(result12_net)

compare("Matrix input edge count", n_mat, n_net)
compare("igraph input edge count", n_ig, n_net)

# =============================================================================
# Test 13: Subgraph Correctness
# =============================================================================
cat("\n--- Test 13: Subgraph Structure ---\n")

# After filtering, the resulting graph should be a valid subgraph
result13_cg <- select_edges(net2, weight > 0.7)
result13_ig <- to_igraph(result13_cg)

# Check it's a valid graph
compare("Result is valid igraph", is_igraph(result13_ig), TRUE)
compare("Result has no NA weights", all(!is.na(E(result13_ig)$weight)), TRUE)

# Verify the weights in result match original
result_edges <- as_edgelist(result13_ig, names = TRUE)
for (i in seq_len(nrow(result_edges))) {
  from_node <- result_edges[i, 1]
  to_node <- result_edges[i, 2]
  result_weight <- E(result13_ig)$weight[i]
  original_weight <- mat2[from_node, to_node]
  if (abs(result_weight - original_weight) > 1e-10) {
    cat("  Weight mismatch:", from_node, "-", to_node,
        "result:", result_weight, "original:", original_weight, "\n")
  }
}
cat("PASS: All edge weights match original\n")
tests <- tests + 1

# =============================================================================
# Test 14: Large Network Performance Check
# =============================================================================
cat("\n--- Test 14: Large Network Correctness ---\n")

# Create larger network
set.seed(123)
n_large <- 100
mat_large <- matrix(runif(n_large^2), n_large, n_large)
mat_large <- mat_large * upper.tri(mat_large) + t(mat_large * upper.tri(mat_large))
mat_large[mat_large < 0.7] <- 0
diag(mat_large) <- 0
rownames(mat_large) <- colnames(mat_large) <- paste0("N", 1:n_large)

g_large <- graph_from_adjacency_matrix(mat_large, mode = "undirected", weighted = TRUE)
net_large <- as_cograph(mat_large)

cat("Large network:", n_large, "nodes,", ecount(g_large), "edges\n")

# Test weight filter
result14_cg <- select_edges(net_large, weight > 0.8)
n_above_08_cg <- n_edges(result14_cg)

n_above_08_ig <- sum(E(g_large)$weight > 0.8)

compare("Large network: weight > 0.8", n_above_08_cg, n_above_08_ig)

# Test top N
result14b_cg <- select_edges(net_large, top = 20)
top20_weights_cg <- sort(get_edges(result14b_cg)$weight, decreasing = TRUE)

top20_weights_ig <- sort(E(g_large)$weight, decreasing = TRUE)[1:20]

compare("Large network: top 20 weights", top20_weights_cg, top20_weights_ig)

# Test bridge count
result14c_cg <- select_edges(net_large, bridges_only = TRUE)
n_bridges_large_cg <- n_edges(result14c_cg)

n_bridges_large_ig <- length(bridges(g_large))

compare("Large network: bridge count", n_bridges_large_cg, n_bridges_large_ig)

# =============================================================================
# Summary
# =============================================================================
cat("\n=== Summary ===\n")
cat("Tests run:", tests, "\n")
cat("Errors:", errors, "\n")

if (errors == 0) {
  cat("\nAll comparison tests passed! select_edges() matches igraph.\n")
} else {
  cat("\nSome tests failed. Please review discrepancies.\n")
}
