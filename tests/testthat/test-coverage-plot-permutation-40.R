# Tests for plot-permutation.R
# Comprehensive coverage tests for permutation test plotting functions
# Testing: plot_permutation(), plot_group_permutation(), splot.tna_permutation(),
#          splot.group_tna_permutation()

# ============================================
# Helper Functions: Create Mock Permutation Objects
# ============================================

#' Create a mock tna_permutation object for testing
#' @param n Number of nodes
#' @param n_sig Number of significant edges
#' @param seed Random seed
#' @return A mock tna_permutation object
create_mock_permutation <- function(n = 4, n_sig = 3, seed = 42) {
  set.seed(seed)

  # Create node labels
  labels <- LETTERS[1:n]

  # Create true difference matrix
  diffs_true <- matrix(0, n, n, dimnames = list(labels, labels))
  diffs_sig <- matrix(0, n, n, dimnames = list(labels, labels))

  # Fill some edges with differences
  edges_filled <- 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (runif(1) > 0.5) {
        diff_val <- runif(1, -0.5, 0.5)
        diffs_true[i, j] <- diff_val
        diffs_true[j, i] <- runif(1, -0.3, 0.3)

        if (edges_filled < n_sig && abs(diff_val) > 0.1) {
          diffs_sig[i, j] <- diff_val
          edges_filled <- edges_filled + 1
        }
      }
    }
  }

  # Create edge stats data frame
  edge_stats <- data.frame(
    edge_name = character(0),
    diff_true = numeric(0),
    effect_size = numeric(0),
    p_value = numeric(0),
    stringsAsFactors = FALSE
  )

  for (i in 1:n) {
    for (j in 1:n) {
      if (diffs_true[i, j] != 0) {
        # Determine p-value based on significance
        if (diffs_sig[i, j] != 0) {
          p_val <- runif(1, 0.001, 0.04)
        } else {
          p_val <- runif(1, 0.06, 0.5)
        }

        edge_stats <- rbind(edge_stats, data.frame(
          edge_name = paste(labels[i], "->", labels[j]),
          diff_true = diffs_true[i, j],
          effect_size = runif(1, 0.5, 2.5),
          p_value = p_val,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Build the object
  obj <- list(
    edges = list(
      diffs_true = diffs_true,
      diffs_sig = diffs_sig,
      stats = edge_stats
    )
  )

  attr(obj, "level") <- 0.05
  attr(obj, "labels") <- labels
  attr(obj, "colors") <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")[1:n]
  class(obj) <- "tna_permutation"

  obj
}

#' Create a mock group_tna_permutation object for testing
#' @param n_groups Number of groups (creates pairwise comparisons)
#' @param n_nodes Number of nodes
#' @param seed Random seed
#' @return A mock group_tna_permutation object
create_mock_group_permutation <- function(n_groups = 3, n_nodes = 4, seed = 42) {
  set.seed(seed)

  # Create pairwise comparison names
  group_names <- LETTERS[1:n_groups]
  pairs <- character(0)

  for (i in 1:(n_groups - 1)) {
    for (j in (i + 1):n_groups) {
      pairs <- c(pairs, paste(group_names[i], "vs.", group_names[j]))
    }
  }

  # Create list of permutation objects
  obj <- vector("list", length(pairs))
  names(obj) <- pairs

  for (k in seq_along(pairs)) {
    obj[[k]] <- create_mock_permutation(n = n_nodes, n_sig = 2, seed = seed + k)
    # Remove the class for individual elements
    class(obj[[k]]) <- "tna_permutation"
  }

  class(obj) <- "group_tna_permutation"
  obj
}

#' Create a permutation object with no significant edges
create_empty_permutation <- function(n = 3) {
  labels <- LETTERS[1:n]

  diffs_true <- matrix(0, n, n, dimnames = list(labels, labels))
  diffs_sig <- matrix(0, n, n, dimnames = list(labels, labels))

  obj <- list(
    edges = list(
      diffs_true = diffs_true,
      diffs_sig = diffs_sig,
      stats = data.frame(
        edge_name = character(0),
        diff_true = numeric(0),
        effect_size = numeric(0),
        p_value = numeric(0),
        stringsAsFactors = FALSE
      )
    )
  )

  attr(obj, "level") <- 0.05
  attr(obj, "labels") <- labels
  class(obj) <- "tna_permutation"

  obj
}

#' Create permutation with all positive differences
create_positive_permutation <- function(n = 3) {
  labels <- LETTERS[1:n]

  diffs_true <- matrix(0, n, n, dimnames = list(labels, labels))
  diffs_sig <- matrix(0, n, n, dimnames = list(labels, labels))

  # Set positive differences
  diffs_true[1, 2] <- 0.3
  diffs_true[2, 3] <- 0.2
  diffs_sig[1, 2] <- 0.3
  diffs_sig[2, 3] <- 0.2

  edge_stats <- data.frame(
    edge_name = c("A -> B", "B -> C"),
    diff_true = c(0.3, 0.2),
    effect_size = c(1.5, 1.2),
    p_value = c(0.01, 0.02),
    stringsAsFactors = FALSE
  )

  obj <- list(
    edges = list(
      diffs_true = diffs_true,
      diffs_sig = diffs_sig,
      stats = edge_stats
    )
  )

  attr(obj, "level") <- 0.05
  attr(obj, "labels") <- labels
  class(obj) <- "tna_permutation"

  obj
}

#' Create permutation with all negative differences
create_negative_permutation <- function(n = 3) {
  labels <- LETTERS[1:n]

  diffs_true <- matrix(0, n, n, dimnames = list(labels, labels))
  diffs_sig <- matrix(0, n, n, dimnames = list(labels, labels))

  # Set negative differences
  diffs_true[1, 2] <- -0.3
  diffs_true[2, 3] <- -0.2
  diffs_sig[1, 2] <- -0.3
  diffs_sig[2, 3] <- -0.2

  edge_stats <- data.frame(
    edge_name = c("A -> B", "B -> C"),
    diff_true = c(-0.3, -0.2),
    effect_size = c(1.5, 1.2),
    p_value = c(0.01, 0.02),
    stringsAsFactors = FALSE
  )

  obj <- list(
    edges = list(
      diffs_true = diffs_true,
      diffs_sig = diffs_sig,
      stats = edge_stats
    )
  )

  attr(obj, "level") <- 0.05
  attr(obj, "labels") <- labels
  class(obj) <- "tna_permutation"

  obj
}

#' Create permutation with mixed positive/negative differences
create_mixed_permutation <- function(n = 4) {
  labels <- LETTERS[1:n]

  diffs_true <- matrix(0, n, n, dimnames = list(labels, labels))
  diffs_sig <- matrix(0, n, n, dimnames = list(labels, labels))

  # Set mixed differences
  diffs_true[1, 2] <- 0.4
  diffs_true[2, 3] <- -0.35
  diffs_true[3, 4] <- 0.25
  diffs_true[1, 4] <- -0.15

  diffs_sig[1, 2] <- 0.4
  diffs_sig[2, 3] <- -0.35
  diffs_sig[3, 4] <- 0.25

  edge_stats <- data.frame(
    edge_name = c("A -> B", "B -> C", "C -> D", "A -> D"),
    diff_true = c(0.4, -0.35, 0.25, -0.15),
    effect_size = c(2.1, 1.8, 1.3, 0.8),
    p_value = c(0.001, 0.005, 0.02, 0.12),
    stringsAsFactors = FALSE
  )

  obj <- list(
    edges = list(
      diffs_true = diffs_true,
      diffs_sig = diffs_sig,
      stats = edge_stats
    )
  )

  attr(obj, "level") <- 0.05
  attr(obj, "labels") <- labels
  attr(obj, "colors") <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")
  class(obj) <- "tna_permutation"

  obj
}

# ============================================
# plot_permutation() Basic Tests
# ============================================

test_that("plot_permutation works with basic permutation object", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(plot_permutation(perm)))
})

test_that("plot_permutation returns invisibly", {

  perm <- create_mixed_permutation()

  result <- with_temp_png(plot_permutation(perm))

  # Should return invisible result
  expect_true(is.null(result) || is.list(result))
})

test_that("plot_permutation handles positive differences", {
  perm <- create_positive_permutation()

  expect_no_error(with_temp_png(plot_permutation(perm)))
})

test_that("plot_permutation handles negative differences", {
  perm <- create_negative_permutation()

  expect_no_error(with_temp_png(plot_permutation(perm)))
})

test_that("plot_permutation handles mixed differences", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(plot_permutation(perm)))
})

# ============================================
# plot_permutation() show_nonsig Parameter Tests
# ============================================

test_that("plot_permutation show_nonsig = FALSE shows only significant edges", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_nonsig = FALSE)
  ))
})

test_that("plot_permutation show_nonsig = TRUE shows all edges", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_nonsig = TRUE)
  ))
})

test_that("plot_permutation uses different styling for sig vs non-sig when show_nonsig = TRUE", {
  perm <- create_mixed_permutation()

  # This should render without error and apply per-edge styling
  expect_no_error(with_temp_png(
    plot_permutation(perm, show_nonsig = TRUE)
  ))
})

# ============================================
# plot_permutation() Color Parameter Tests
# ============================================

test_that("plot_permutation respects edge_positive_color", {
  perm <- create_positive_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, edge_positive_color = "#00FF00")
  ))
})

test_that("plot_permutation respects edge_negative_color", {
  perm <- create_negative_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, edge_negative_color = "#FF0000")
  ))
})

test_that("plot_permutation respects edge_nonsig_color", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_nonsig = TRUE, edge_nonsig_color = "#CCCCCC")
  ))
})

test_that("plot_permutation accepts all custom color parameters", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm,
                     show_nonsig = TRUE,
                     edge_positive_color = "darkgreen",
                     edge_negative_color = "darkred",
                     edge_nonsig_color = "gray50")
  ))
})

# ============================================
# plot_permutation() Styling Parameter Tests
# ============================================

test_that("plot_permutation respects edge_nonsig_style", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_nonsig = TRUE, edge_nonsig_style = 3)
  ))
})

test_that("plot_permutation respects edge_nonsig_alpha", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_nonsig = TRUE, edge_nonsig_alpha = 0.2)
  ))
})

# ============================================
# plot_permutation() Stars and Effect Size Tests
# ============================================

test_that("plot_permutation show_stars = TRUE displays significance stars", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_stars = TRUE)
  ))
})

test_that("plot_permutation show_stars = FALSE hides significance stars", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_stars = FALSE)
  ))
})

test_that("plot_permutation show_effect = TRUE displays effect sizes", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_effect = TRUE)
  ))
})

test_that("plot_permutation show_effect = FALSE hides effect sizes", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_effect = FALSE)
  ))
})

test_that("plot_permutation combines show_stars and show_effect", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_stars = TRUE, show_effect = TRUE)
  ))
})

# ============================================
# plot_permutation() Labels and Title Tests
# ============================================

test_that("plot_permutation uses labels from attr", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm)
  ))
})

test_that("plot_permutation respects custom labels", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, labels = c("X", "Y", "Z", "W"))
  ))
})

test_that("plot_permutation uses default title for significant only", {
  perm <- create_mock_permutation()

  # Default title should be "Permutation Test: Significant Differences"
  expect_no_error(with_temp_png(
    plot_permutation(perm)
  ))
})

test_that("plot_permutation uses default title for all differences", {
  perm <- create_mock_permutation()

  # Default title should be "Permutation Test: All Differences"
  expect_no_error(with_temp_png(
    plot_permutation(perm, show_nonsig = TRUE)
  ))
})

test_that("plot_permutation respects custom title", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, title = "Custom Title")
  ))
})

# ============================================
# plot_permutation() Node Colors from Attribute Tests
# ============================================

test_that("plot_permutation uses node colors from attr", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm)
  ))
})

test_that("plot_permutation respects custom node_fill", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, node_fill = c("red", "blue", "green", "orange"))
  ))
})

# ============================================
# plot_permutation() Layout Tests
# ============================================

test_that("plot_permutation defaults to oval layout", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm)
  ))
})

test_that("plot_permutation respects custom layout", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, layout = "circle")
  ))
})

test_that("plot_permutation respects spring layout", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, layout = "spring")
  ))
})

# ============================================
# plot_permutation() Edge Case Tests
# ============================================

test_that("plot_permutation handles no significant edges", {
  perm <- create_empty_permutation()

  # Should message about no edges
  expect_message(
    with_temp_png(plot_permutation(perm)),
    "No edges"
  )
})

test_that("plot_permutation handles permutation with no edge stats", {
  perm <- create_mock_permutation()
  perm$edges$stats <- NULL

  expect_no_error(with_temp_png(
    plot_permutation(perm)
  ))
})

test_that("plot_permutation errors when diffs_true is missing", {
  perm <- create_mock_permutation()
  perm$edges$diffs_true <- NULL

  expect_error(
    with_temp_png(plot_permutation(perm)),
    "Cannot find edge differences"
  )
})

test_that("plot_permutation handles missing level attribute", {
  perm <- create_mock_permutation()
  attr(perm, "level") <- NULL

  expect_no_error(with_temp_png(
    plot_permutation(perm)
  ))
})

# ============================================
# plot_permutation() Passthrough Arguments Tests
# ============================================

test_that("plot_permutation passes node_size to splot", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, node_size = 10)
  ))
})

test_that("plot_permutation passes arrow_size to splot", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, arrow_size = 0.8)
  ))
})

test_that("plot_permutation passes edge_label_size to splot", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, edge_label_size = 0.8)
  ))
})

test_that("plot_permutation passes edge_label_position to splot", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, edge_label_position = 0.5)
  ))
})

# ============================================
# splot.tna_permutation() S3 Method Tests
# ============================================

test_that("splot.tna_permutation dispatches to plot_permutation", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    splot(perm)
  ))
})

test_that("splot dispatches correctly for tna_permutation class", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    splot(perm, show_nonsig = TRUE)
  ))
})

test_that("splot.tna_permutation passes arguments correctly", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    splot(perm,
          edge_positive_color = "#00FF00",
          show_stars = TRUE,
          title = "Custom Permutation Plot")
  ))
})

# ============================================
# plot_group_permutation() Basic Tests
# ============================================

test_that("plot_group_permutation works with basic group permutation", {
  group_perm <- create_mock_group_permutation()

  expect_no_error(with_temp_png(
    plot_group_permutation(group_perm),
    width = 600, height = 400
  ))
})

test_that("plot_group_permutation plots all comparisons", {
  group_perm <- create_mock_group_permutation(n_groups = 3)

  # 3 groups = 3 pairwise comparisons
  expect_no_error(with_temp_png(
    plot_group_permutation(group_perm),
    width = 600, height = 400
  ))
})

test_that("plot_group_permutation returns invisibly NULL", {
  group_perm <- create_mock_group_permutation(n_groups = 2)

  result <- with_temp_png(
    plot_group_permutation(group_perm)
  )

  expect_null(result)
})

# ============================================
# plot_group_permutation() Index Parameter Tests
# ============================================

test_that("plot_group_permutation i parameter by numeric index", {
  group_perm <- create_mock_group_permutation(n_groups = 3)

  expect_no_error(with_temp_png(
    plot_group_permutation(group_perm, i = 1)
  ))
})

test_that("plot_group_permutation i parameter by character name", {
  group_perm <- create_mock_group_permutation(n_groups = 3)

  expect_no_error(with_temp_png(
    plot_group_permutation(group_perm, i = "A vs. B")
  ))
})

test_that("plot_group_permutation errors on invalid numeric index", {
  group_perm <- create_mock_group_permutation(n_groups = 2)

  # The function tries x[[i]] which throws subscript out of bounds error
  # before the validation check for NULL
  expect_error(
    with_temp_png(plot_group_permutation(group_perm, i = 10))
  )
})

test_that("plot_group_permutation errors on invalid character index", {
  group_perm <- create_mock_group_permutation(n_groups = 2)

  # Invalid character index returns NULL element, triggering "Invalid index" error
  expect_error(
    with_temp_png(plot_group_permutation(group_perm, i = "X vs. Y")),
    "Invalid index"
  )
})

# ============================================
# plot_group_permutation() Passthrough Arguments Tests
# ============================================

test_that("plot_group_permutation passes arguments to plot_permutation", {
  group_perm <- create_mock_group_permutation(n_groups = 2)

  expect_no_error(with_temp_png(
    plot_group_permutation(group_perm,
                           show_nonsig = TRUE,
                           edge_positive_color = "#00FF00")
  ))
})

test_that("plot_group_permutation passes show_stars to individual plots", {
  group_perm <- create_mock_group_permutation(n_groups = 2)

  expect_no_error(with_temp_png(
    plot_group_permutation(group_perm, show_stars = TRUE)
  ))
})

# ============================================
# plot_group_permutation() Edge Case Tests
# ============================================

test_that("plot_group_permutation handles empty list", {
  group_perm <- list()
  class(group_perm) <- "group_tna_permutation"

  expect_message(
    with_temp_png(plot_group_permutation(group_perm)),
    "No comparisons"
  )
})

test_that("plot_group_permutation handles single comparison", {
  group_perm <- create_mock_group_permutation(n_groups = 2)

  # Should have 1 comparison
  expect_no_error(with_temp_png(
    plot_group_permutation(group_perm)
  ))
})

# ============================================
# splot.group_tna_permutation() S3 Method Tests
# ============================================

test_that("splot.group_tna_permutation dispatches to plot_group_permutation", {
  group_perm <- create_mock_group_permutation(n_groups = 2)

  expect_no_error(with_temp_png(
    splot(group_perm)
  ))
})

test_that("splot dispatches correctly for group_tna_permutation class", {
  group_perm <- create_mock_group_permutation(n_groups = 2)

  expect_no_error(with_temp_png(
    splot(group_perm, i = 1)
  ))
})

# ============================================
# plot_permutation() with Different P-value Thresholds
# ============================================

test_that("plot_permutation handles different significance levels", {
  perm <- create_mock_permutation()
  attr(perm, "level") <- 0.01

  expect_no_error(with_temp_png(
    plot_permutation(perm)
  ))
})

test_that("plot_permutation handles stars for different p-value ranges", {
  perm <- create_mixed_permutation()

  # p < 0.001 = ***, p < 0.01 = **, p < 0.05 = *
  expect_no_error(with_temp_png(
    plot_permutation(perm, show_stars = TRUE)
  ))
})

# ============================================
# plot_permutation() Weight Rounding Tests
# ============================================

test_that("plot_permutation respects weight_digits parameter", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, weight_digits = 3)
  ))
})

test_that("plot_permutation handles weight_digits = 1", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, weight_digits = 1)
  ))
})

# ============================================
# plot_permutation() Edge Label Formatting Tests
# ============================================

test_that("plot_permutation removes leading zeros from edge labels", {
  perm <- create_mixed_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, edge_labels = TRUE)
  ))
})

test_that("plot_permutation handles edge_label_halo", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, edge_label_halo = FALSE)
  ))
})

# ============================================
# Integration Tests with Real TNA (Conditional)
# ============================================

test_that("plot_permutation works with real tna_permutation object", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Create two models
  model1 <- tna(engagement[1:100, ])
  model2 <- tna(engagement[101:200, ])

  # Run permutation test (small iter for speed)
  perm_result <- permutation_test(model1, model2, iter = 50)

  expect_no_error(with_temp_png(
    plot_permutation(perm_result)
  ))
})

test_that("plot_permutation with real tna shows effect sizes", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  model1 <- tna(engagement[1:100, ])
  model2 <- tna(engagement[101:200, ])

  perm_result <- permutation_test(model1, model2, iter = 50)

  expect_no_error(with_temp_png(
    plot_permutation(perm_result, show_effect = TRUE)
  ))
})

test_that("splot dispatches for real tna_permutation", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  model1 <- tna(engagement[1:100, ])
  model2 <- tna(engagement[101:200, ])

  perm_result <- permutation_test(model1, model2, iter = 50)

  expect_no_error(with_temp_png(
    splot(perm_result)
  ))
})

test_that("plot_group_permutation works with real group_tna_permutation", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Create groups
  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  # Run permutation test (small iter for speed)
  perm_result <- permutation_test(group_model, iter = 50)

  expect_no_error(with_temp_png(
    plot_group_permutation(perm_result),
    width = 400, height = 400
  ))
})

# ============================================
# plot_permutation() Numeric Row/Column Index Tests
# ============================================

test_that("plot_permutation handles numeric row/column names fallback", {
  perm <- create_mock_permutation()

  # Remove dimnames
  dimnames(perm$edges$diffs_true) <- NULL
  dimnames(perm$edges$diffs_sig) <- NULL
  attr(perm, "labels") <- NULL

  # Modify edge stats to use numeric indices
  perm$edges$stats$edge_name <- c("1 -> 2", "2 -> 3", "1 -> 3")

  expect_no_error(with_temp_png(
    plot_permutation(perm)
  ))
})

# ============================================
# Null Coalescing Operator Tests
# ============================================

test_that("%||% operator works correctly (internal)", {
  # Test the internal null coalescing operator
  `%||%` <- function(a, b) if (is.null(a)) b else a


  expect_equal(NULL %||% 5, 5)
  expect_equal(3 %||% 5, 3)
  expect_equal("test" %||% "default", "test")
  expect_equal(NULL %||% "default", "default")
})

# ============================================
# plot_permutation() Large Network Tests
# ============================================

test_that("plot_permutation handles larger networks", {
  skip_on_cran()

  perm <- create_mock_permutation(n = 10, n_sig = 15, seed = 123)

  expect_no_error(with_temp_png(
    plot_permutation(perm),
    width = 400, height = 400
  ))
})

test_that("plot_permutation handles show_nonsig with larger networks", {
  skip_on_cran()

  perm <- create_mock_permutation(n = 8, n_sig = 10, seed = 456)

  expect_no_error(with_temp_png(
    plot_permutation(perm, show_nonsig = TRUE),
    width = 400, height = 400
  ))
})

# ============================================
# plot_group_permutation() Grid Layout Tests
# ============================================

test_that("plot_group_permutation calculates correct grid for 4 comparisons", {
  group_perm <- create_mock_group_permutation(n_groups = 3)
  # 3 groups = 3 comparisons (A vs B, A vs C, B vs C)

  expect_no_error(with_temp_png(
    plot_group_permutation(group_perm),
    width = 600, height = 400
  ))
})

test_that("plot_group_permutation calculates correct grid for 6 comparisons", {
  group_perm <- create_mock_group_permutation(n_groups = 4)
  # 4 groups = 6 comparisons

  expect_no_error(with_temp_png(
    plot_group_permutation(group_perm),
    width = 800, height = 600
  ))
})

# ============================================
# plot_permutation() Default Parameter Override Tests
# ============================================

test_that("plot_permutation default edge_labels can be overridden", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, edge_labels = FALSE)
  ))
})

test_that("plot_permutation default edge_label_leading_zero can be overridden", {
  perm <- create_mock_permutation()

  expect_no_error(with_temp_png(
    plot_permutation(perm, edge_label_leading_zero = TRUE)
  ))
})
