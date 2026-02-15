# Test script for enhanced mcml() and plot_mcml()
library(devtools)
load_all(".")

# Create test data
set.seed(42)
mat <- matrix(runif(36, 0.1, 0.5), 6, 6)
diag(mat) <- 0
colnames(mat) <- rownames(mat) <- LETTERS[1:6]

# Define clusters
clusters <- list(
  Alpha = c("A", "B"),
  Beta = c("C", "D"),
  Gamma = c("E", "F")
)

# Test 1: mcml() with new structure
cat("=== Testing mcml() new structure ===\n")
data <- mcml(mat, clusters)

cat("\n--- Top-level fields (backward compat) ---\n")
cat("$tna exists:", !is.null(data$tna), "\n")
cat("$inits exists:", !is.null(data$inits), "\n")
cat("$between_weights exists:", !is.null(data$between_weights), "\n")

cat("\n--- NEW: $summary field ---\n")
cat("$summary exists:", !is.null(data$summary), "\n")
cat("$summary names:", paste(names(data$summary), collapse = ", "), "\n")
cat("$summary$tna equals $tna:", identical(data$summary$tna, data$tna), "\n")

cat("\n--- NEW: $within field ---\n")
cat("$within exists:", !is.null(data$within), "\n")
cat("$within names:", paste(names(data$within), collapse = ", "), "\n")
cat("\nAlpha cluster TNA:\n")
print(round(data$within$Alpha$tna, 3))
cat("\nAlpha cluster weights:\n")
print(round(data$within$Alpha$weights, 3))

# Test 2: mcml with within = FALSE
cat("\n=== Testing mcml() with within = FALSE ===\n")
data_no_within <- mcml(mat, clusters, within = FALSE)
cat("$within is NULL:", is.null(data_no_within$within), "\n")
cat("$summary still exists:", !is.null(data_no_within$summary), "\n")

# Test 3: Print method
cat("\n=== Testing print method ===\n")
print(data)

# Test 4: Plotting with different modes
cat("\n=== Creating test plots ===\n")

# Setup PNG output
png("tmp/mcml_test_plots.png", width = 1200, height = 800)
par(mfrow = c(2, 2))

# Plot 1: Default (weights mode)
plot_mcml(mat, clusters,
          summary_edge_labels = TRUE,
          title = "Mode: weights (default)")

# Plot 2: TNA mode
plot_mcml(mat, clusters,
          mode = "tna",
          summary_edge_labels = TRUE,
          title = "Mode: tna (probabilities)")

# Plot 3: Weights with within labels
plot_mcml(mat, clusters,
          mode = "weights",
          edge_labels = TRUE,
          summary_edge_labels = TRUE,
          title = "Weights with edge labels")

# Plot 4: TNA with within labels
plot_mcml(mat, clusters,
          mode = "tna",
          edge_labels = TRUE,
          summary_edge_labels = TRUE,
          title = "TNA with edge labels")

dev.off()
cat("Plots saved to tmp/mcml_test_plots.png\n")

cat("\n=== All tests completed! ===\n")
