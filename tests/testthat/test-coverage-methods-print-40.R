# Test file: test-coverage-methods-print-40.R
# Tests for S3 print methods in methods-print.R and related files
# Target: 100% coverage for print methods

# ==============================================================================
# Tests for print.cograph_network (methods-print.R)
# ==============================================================================

# --- List-based format (has n_nodes as list element) ---

test_that("print.cograph_network works with list-based n_nodes format", {
  net <- list(
    n_nodes = 3,
    n_edges = 3,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B", "A"), to = c("B", "C", "C")),
    weight = c(1, 1, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network:", output)))
  expect_true(any(grepl("3.*nodes", output)))
  expect_true(any(grepl("3.*edges", output)))
  expect_true(any(grepl("undirected", output)))
})

test_that("print.cograph_network shows directed status in list format", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = TRUE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(0.5, 0.8)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("directed", output)))
})

test_that("print.cograph_network shows weight range with different weights", {
  net <- list(
    n_nodes = 3,
    n_edges = 3,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B", "A"), to = c("B", "C", "C")),
    weight = c(0.2, 0.5, 0.8)
  )

  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
  expect_true(any(grepl("0\\.2.*to.*0\\.8", output)))
})

test_that("print.cograph_network shows equal weights message", {
  net <- list(
    n_nodes = 3,
    n_edges = 3,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B", "A"), to = c("B", "C", "C")),
    weight = c(1, 1, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("all equal", output)))
})

test_that("print.cograph_network shows layout status when set", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*set", output)))
})

test_that("print.cograph_network shows layout: none when not set", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = NA, y = NA),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

test_that("print.cograph_network returns invisible x for list format", {
  net <- list(
    n_nodes = 3,
    n_edges = 2,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net) <- c("cograph_network", "list")

  result <- print(net)
  expect_identical(result, net)
})

test_that("print.cograph_network handles network with zero edges in list format", {
  net <- list(
    n_nodes = 3,
    n_edges = 0,
    directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = character(0), to = character(0)),
    weight = numeric(0)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network:", output)))
  expect_true(any(grepl("0.*edges", output)))
})

# --- Attribute-based format (backward compatibility) ---

test_that("print.cograph_network handles attr-based format", {
  net <- list(
    edges = data.frame(from = "A", to = "B", weight = 1),
    weight = c(1)
  )
  attr(net, "n_nodes") <- 2
  attr(net, "n_edges") <- 1
  attr(net, "directed") <- FALSE
  attr(net, "nodes") <- data.frame(name = c("A", "B"), x = c(0, 1), y = c(0, 1))
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network:", output)))
  expect_true(any(grepl("2.*nodes", output)))
})

test_that("print.cograph_network handles attr-format with directed TRUE", {
  net <- list(
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(0.2, 0.8)
  )
  attr(net, "n_nodes") <- 3
  attr(net, "n_edges") <- 2
  attr(net, "directed") <- TRUE
  attr(net, "nodes") <- data.frame(name = c("A", "B", "C"), x = NA, y = NA)
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("directed", output)))
})

test_that("print.cograph_network handles attr-format with weight range", {
  net <- list(
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(0.2, 0.8)
  )
  attr(net, "n_nodes") <- 3
  attr(net, "n_edges") <- 2
  attr(net, "directed") <- TRUE
  attr(net, "nodes") <- data.frame(name = c("A", "B", "C"), x = NA, y = NA)
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
  expect_true(any(grepl("to", output)))
})

test_that("print.cograph_network handles attr-format with equal weights", {
  net <- list(
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  attr(net, "n_nodes") <- 3
  attr(net, "n_edges") <- 2
  attr(net, "directed") <- FALSE
  attr(net, "nodes") <- data.frame(name = c("A", "B", "C"), x = NA, y = NA)
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("all equal", output)))
})

test_that("print.cograph_network handles attr-format with layout set", {
  net <- list(
    edges = data.frame(from = "A", to = "B")
  )
  attr(net, "n_nodes") <- 2
  attr(net, "n_edges") <- 1
  attr(net, "directed") <- FALSE
  attr(net, "nodes") <- data.frame(name = c("A", "B"), x = c(0, 1), y = c(0, 1))
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*set", output)))
})

test_that("print.cograph_network handles attr-format with layout none", {
  net <- list(
    edges = data.frame(from = "A", to = "B")
  )
  attr(net, "n_nodes") <- 2
  attr(net, "n_edges") <- 1
  attr(net, "directed") <- FALSE
  attr(net, "nodes") <- data.frame(name = c("A", "B"), x = NA, y = NA)
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

test_that("print.cograph_network returns invisible x for attr format", {
  net <- list(
    edges = data.frame(from = "A", to = "B"),
    weight = 1
  )
  attr(net, "n_nodes") <- 2
  attr(net, "n_edges") <- 1
  attr(net, "directed") <- FALSE
  attr(net, "nodes") <- data.frame(name = c("A", "B"), x = c(0, 1), y = c(0, 1))
  class(net) <- c("cograph_network", "list")

  result <- print(net)
  expect_identical(result, net)
})

# --- Fallback format ---

test_that("print.cograph_network fallback for unknown format", {
  net <- list(data = "something")
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network object", output)))
})

test_that("print.cograph_network fallback returns invisible x", {
  net <- list(data = "something")
  class(net) <- c("cograph_network", "list")

  result <- print(net)
  expect_identical(result, net)
})

test_that("print.cograph_network works with actual cograph() output", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(print(net))
  # The current cograph() output uses fallback
  expect_true(any(grepl("Cograph network", output)))
})

# ==============================================================================
# Tests for print.cograph_motifs (motifs.R)
# ==============================================================================

test_that("print.cograph_motifs shows basic info", {
  result <- list(
    size = 3,
    directed = TRUE,
    method = "rewire",
    n_random = 100,
    counts = c(motif_1 = 5, motif_2 = 3, motif_3 = 0),
    null_mean = c(motif_1 = 2.5, motif_2 = 3.0, motif_3 = 0),
    z_scores = c(motif_1 = 3.5, motif_2 = 0.0, motif_3 = NA),
    p_values = c(motif_1 = 0.001, motif_2 = 0.5, motif_3 = NA),
    significant = c(motif_1 = TRUE, motif_2 = FALSE, motif_3 = FALSE)
  )
  class(result) <- "cograph_motifs"

  output <- capture.output(print(result))
  expect_true(any(grepl("Network Motif Analysis", output)))
  expect_true(any(grepl("3-node motifs", output)))
  expect_true(any(grepl("directed", output)))
})

test_that("print.cograph_motifs shows undirected motifs", {
  result <- list(
    size = 3,
    directed = FALSE,
    method = "erdos_renyi",
    n_random = 50,
    counts = c(motif_1 = 2),
    null_mean = c(motif_1 = 2.0),
    z_scores = c(motif_1 = 0.0),
    p_values = c(motif_1 = 0.5),
    significant = c(motif_1 = FALSE)
  )
  class(result) <- "cograph_motifs"

  output <- capture.output(print(result))
  expect_true(any(grepl("undirected", output)))
})

test_that("print.cograph_motifs shows no significant motifs message", {
  result <- list(
    size = 3,
    directed = FALSE,
    method = "erdos_renyi",
    n_random = 50,
    counts = c(motif_1 = 2, motif_2 = 1),
    null_mean = c(motif_1 = 2.0, motif_2 = 1.0),
    z_scores = c(motif_1 = 0.0, motif_2 = 0.0),
    p_values = c(motif_1 = 0.5, motif_2 = 0.5),
    significant = c(motif_1 = FALSE, motif_2 = FALSE)
  )
  class(result) <- "cograph_motifs"

  output <- capture.output(print(result))
  expect_true(any(grepl("No significantly", output)))
})

test_that("print.cograph_motifs shows significant motifs table", {
  result <- list(
    size = 3,
    directed = TRUE,
    method = "rewire",
    n_random = 100,
    counts = c(over = 10, under = 5),
    null_mean = c(over = 2, under = 10),
    z_scores = c(over = 4.0, under = -3.0),
    p_values = c(over = 0.001, under = 0.002),
    significant = c(over = TRUE, under = TRUE)
  )
  class(result) <- "cograph_motifs"

  output <- capture.output(print(result))
  expect_true(any(grepl("Significant motifs:", output)))
})

test_that("print.cograph_motifs shows over/under-represented counts", {
  result <- list(
    size = 3,
    directed = TRUE,
    method = "rewire",
    n_random = 100,
    counts = c(over = 10, under = 5, neutral = 3),
    null_mean = c(over = 2, under = 10, neutral = 3),
    z_scores = c(over = 4.0, under = -3.0, neutral = 0.0),
    p_values = c(over = 0.001, under = 0.002, neutral = 0.5),
    significant = c(over = TRUE, under = TRUE, neutral = FALSE)
  )
  class(result) <- "cograph_motifs"

  output <- capture.output(print(result))
  expect_true(any(grepl("Over-represented", output)))
  expect_true(any(grepl("Under-represented", output)))
})

test_that("print.cograph_motifs returns invisible x", {
  result <- list(
    size = 3,
    directed = TRUE,
    method = "rewire",
    n_random = 10,
    counts = c(m1 = 1),
    null_mean = c(m1 = 1),
    z_scores = c(m1 = 0),
    p_values = c(m1 = 0.5),
    significant = c(m1 = FALSE)
  )
  class(result) <- "cograph_motifs"

  ret <- print(result)
  expect_s3_class(ret, "cograph_motifs")
})

# ==============================================================================
# Tests for print.cograph_motif_analysis (motifs.R)
# ==============================================================================

test_that("print.cograph_motif_analysis shows basic info", {
  result <- list(
    params = list(
      pattern = "all",
      edge_method = "any",
      edge_threshold = 0,
      n_individuals = 10,
      n_states = 5,
      significance = FALSE
    ),
    type_summary = data.frame(type = c("003", "102"), count = c(5, 3)),
    results = data.frame(
      triad = c("A-B-C", "D-E-F"),
      type = c("003", "102"),
      observed = c(5, 3)
    )
  )
  class(result) <- "cograph_motif_analysis"

  output <- capture.output(print(result))
  expect_true(any(grepl("Motif Analysis", output)))
  expect_true(any(grepl("Pattern:", output)))
})

test_that("print.cograph_motif_analysis shows edge method threshold", {
  result <- list(
    params = list(
      pattern = "all",
      edge_method = "threshold",
      edge_threshold = 0.1,
      n_individuals = 10,
      n_states = 5,
      significance = TRUE
    ),
    type_summary = data.frame(type = c("003", "102"), count = c(5, 3)),
    results = data.frame(
      triad = c("A-B-C", "D-E-F"),
      type = c("003", "102"),
      observed = c(5, 3),
      expected = c(3.0, 2.0),
      z = c(2.5, 1.5),
      sig = c("*", "")
    )
  )
  class(result) <- "cograph_motif_analysis"

  output <- capture.output(print(result))
  expect_true(any(grepl("threshold", output)))
})

test_that("print.cograph_motif_analysis respects n parameter", {
  results_df <- data.frame(
    triad = paste0("triad_", 1:30),
    type = rep("003", 30),
    observed = 1:30
  )
  result <- list(
    params = list(
      pattern = "all",
      edge_method = "any",
      edge_threshold = 0,
      n_individuals = 10,
      n_states = 5,
      significance = FALSE
    ),
    type_summary = data.frame(type = "003", count = 30),
    results = results_df
  )
  class(result) <- "cograph_motif_analysis"

  output <- capture.output(print(result, n = 5))
  expect_true(any(grepl("Top 5 triads", output)))
})

test_that("print.cograph_motif_analysis with significance columns", {
  result <- list(
    params = list(
      pattern = "all",
      edge_method = "any",
      edge_threshold = 0,
      n_individuals = 10,
      n_states = 5,
      significance = TRUE
    ),
    type_summary = data.frame(type = c("003"), count = c(5)),
    results = data.frame(
      triad = c("A-B-C"),
      type = c("003"),
      observed = c(5),
      expected = c(3.0),
      z = c(2.5),
      sig = c("*")
    )
  )
  class(result) <- "cograph_motif_analysis"

  output <- capture.output(print(result))
  expect_true(any(grepl("Motif Analysis", output)))
})

test_that("print.cograph_motif_analysis returns invisible x", {
  result <- list(
    params = list(
      pattern = "all",
      edge_method = "any",
      edge_threshold = 0,
      n_individuals = 5,
      n_states = 3,
      significance = FALSE
    ),
    type_summary = data.frame(type = "003", count = 1),
    results = data.frame(triad = "A-B-C", type = "003", observed = 1)
  )
  class(result) <- "cograph_motif_analysis"

  ret <- print(result)
  expect_s3_class(ret, "cograph_motif_analysis")
})

# ==============================================================================
# Tests for print.cograph_temporal_motifs (motifs.R)
# ==============================================================================

test_that("print.cograph_temporal_motifs shows basic info", {
  result <- list(
    params = list(
      n_windows = 5,
      pattern = "all",
      window_size = 10,
      step = 5
    ),
    summary = data.frame(
      window = 1:5,
      type = rep("003", 5),
      count = c(3, 5, 2, 4, 1)
    )
  )
  class(result) <- "cograph_temporal_motifs"

  output <- capture.output(print(result))
  expect_true(any(grepl("Temporal Motif Analysis", output)))
  expect_true(any(grepl("Windows:", output)))
  expect_true(any(grepl("Window size:", output)))
})

test_that("print.cograph_temporal_motifs with empty summary", {
  result <- list(
    params = list(
      n_windows = 5,
      pattern = "all",
      window_size = 10,
      step = 5
    ),
    summary = data.frame(window = integer(0), type = character(0), count = integer(0))
  )
  class(result) <- "cograph_temporal_motifs"

  output <- capture.output(print(result))
  expect_true(any(grepl("No motifs found", output)))
})

test_that("print.cograph_temporal_motifs aggregates by type", {
  result <- list(
    params = list(
      n_windows = 3,
      pattern = "all",
      window_size = 5,
      step = 2
    ),
    summary = data.frame(
      window = c(1, 1, 2, 2, 3),
      type = c("003", "102", "003", "102", "003"),
      count = c(2, 3, 4, 1, 2)
    )
  )
  class(result) <- "cograph_temporal_motifs"

  output <- capture.output(print(result))
  expect_true(any(grepl("Total occurrences by type", output)))
})

test_that("print.cograph_temporal_motifs returns invisible x", {
  result <- list(
    params = list(n_windows = 2, pattern = "all", window_size = 5, step = 2),
    summary = data.frame(window = 1, type = "003", count = 1)
  )
  class(result) <- "cograph_temporal_motifs"

  ret <- print(result)
  expect_s3_class(ret, "cograph_temporal_motifs")
})

# ==============================================================================
# Tests for print.cograph_triad_persistence (motifs.R)
# ==============================================================================

test_that("print.cograph_triad_persistence shows basic info by type", {
  result <- list(
    params = list(
      by = "type",
      n_windows = 5
    ),
    summary = list(
      n_items = 10,
      n_persistent = 3,
      n_transient = 2,
      n_emerging = 2,
      n_fading = 2,
      n_sporadic = 1
    ),
    triads = data.frame(
      triad = c("A-B-C", "D-E-F"),
      type = c("003", "102"),
      persistence = c(0.8, 0.4),
      windows = c(4, 2),
      status = c("persistent", "transient")
    )
  )
  class(result) <- "cograph_triad_persistence"

  output <- capture.output(print(result))
  expect_true(any(grepl("Triad Persistence Analysis", output)))
  expect_true(any(grepl("MAN types", output)))
  expect_true(any(grepl("Persistent:", output)))
})

test_that("print.cograph_triad_persistence shows by triad", {
  result <- list(
    params = list(
      by = "triad",
      n_windows = 5
    ),
    summary = list(
      n_items = 5,
      n_persistent = 1,
      n_transient = 1,
      n_emerging = 1,
      n_fading = 1,
      n_sporadic = 1
    ),
    triads = data.frame(
      triad = "A-B-C",
      type = "003",
      persistence = 0.6,
      windows = 3,
      status = "transient"
    )
  )
  class(result) <- "cograph_triad_persistence"

  output <- capture.output(print(result))
  expect_true(any(grepl("Triads", output)))
})

test_that("print.cograph_triad_persistence with empty triads", {
  result <- list(
    params = list(
      by = "triad",
      n_windows = 5
    ),
    summary = list(
      n_items = 0,
      n_persistent = 0,
      n_transient = 0,
      n_emerging = 0,
      n_fading = 0,
      n_sporadic = 0
    ),
    triads = data.frame(
      triad = character(0),
      type = character(0),
      persistence = numeric(0),
      windows = integer(0),
      status = character(0)
    )
  )
  class(result) <- "cograph_triad_persistence"

  output <- capture.output(print(result))
  expect_true(any(grepl("No triads found", output)))
})

test_that("print.cograph_triad_persistence respects n parameter", {
  triads_df <- data.frame(
    triad = paste0("triad_", 1:30),
    type = rep("003", 30),
    persistence = seq(1, 0.03, length.out = 30),
    windows = 30:1,
    status = rep(c("persistent", "transient", "sporadic"), 10)
  )
  result <- list(
    params = list(by = "triad", n_windows = 30),
    summary = list(
      n_items = 30,
      n_persistent = 10,
      n_transient = 10,
      n_emerging = 0,
      n_fading = 0,
      n_sporadic = 10
    ),
    triads = triads_df
  )
  class(result) <- "cograph_triad_persistence"

  output <- capture.output(print(result, n = 5))
  expect_true(any(grepl("Top 5 triads", output)))
})

test_that("print.cograph_triad_persistence returns invisible x", {
  result <- list(
    params = list(by = "type", n_windows = 3),
    summary = list(
      n_items = 1, n_persistent = 1, n_transient = 0,
      n_emerging = 0, n_fading = 0, n_sporadic = 0
    ),
    triads = data.frame(
      triad = "A-B-C", type = "003", persistence = 1.0,
      windows = 3, status = "persistent"
    )
  )
  class(result) <- "cograph_triad_persistence"

  ret <- print(result)
  expect_s3_class(ret, "cograph_triad_persistence")
})

# ==============================================================================
# Tests for print.cograph_cluster_significance (cluster-metrics.R)
# ==============================================================================

test_that("print.cograph_cluster_significance shows basic info", {
  result <- list(
    method = "rewire",
    n_random = 100,
    observed_modularity = 0.45,
    null_mean = 0.20,
    null_sd = 0.05,
    z_score = 5.0,
    p_value = 0.0001,
    null_values = rnorm(100, 0.20, 0.05)
  )
  class(result) <- "cograph_cluster_significance"

  output <- capture.output(print(result))
  expect_true(any(grepl("Cluster Significance Test", output)))
  expect_true(any(grepl("Null model", output)))
  expect_true(any(grepl("Observed modularity", output)))
})

test_that("print.cograph_cluster_significance shows highly significant (p < 0.001)", {
  result <- list(
    method = "rewire",
    n_random = 100,
    observed_modularity = 0.45,
    null_mean = 0.20,
    null_sd = 0.05,
    z_score = 5.0,
    p_value = 0.0001,
    null_values = rnorm(100, 0.20, 0.05)
  )
  class(result) <- "cograph_cluster_significance"

  output <- capture.output(print(result))
  expect_true(any(grepl("Highly significant", output)))
})

test_that("print.cograph_cluster_significance shows very significant (p < 0.01)", {
  result <- list(
    method = "rewire",
    n_random = 100,
    observed_modularity = 0.35,
    null_mean = 0.20,
    null_sd = 0.05,
    z_score = 3.0,
    p_value = 0.005,
    null_values = rnorm(100, 0.20, 0.05)
  )
  class(result) <- "cograph_cluster_significance"

  output <- capture.output(print(result))
  expect_true(any(grepl("Very significant", output)))
})

test_that("print.cograph_cluster_significance shows significant (p < 0.05)", {
  result <- list(
    method = "rewire",
    n_random = 100,
    observed_modularity = 0.30,
    null_mean = 0.20,
    null_sd = 0.05,
    z_score = 2.0,
    p_value = 0.03,
    null_values = rnorm(100, 0.20, 0.05)
  )
  class(result) <- "cograph_cluster_significance"

  output <- capture.output(print(result))
  expect_true(any(grepl("Significant community structure", output)))
})

test_that("print.cograph_cluster_significance shows not significant (p >= 0.05)", {
  result <- list(
    method = "erdos_renyi",
    n_random = 100,
    observed_modularity = 0.22,
    null_mean = 0.20,
    null_sd = 0.05,
    z_score = 0.4,
    p_value = 0.35,
    null_values = rnorm(100, 0.20, 0.05)
  )
  class(result) <- "cograph_cluster_significance"

  output <- capture.output(print(result))
  expect_true(any(grepl("No significant", output)))
})

test_that("print.cograph_cluster_significance handles NA p-value", {
  result <- list(
    method = "rewire",
    n_random = 100,
    observed_modularity = 0.45,
    null_mean = 0.20,
    null_sd = 0,
    z_score = Inf,
    p_value = NA,
    null_values = rep(0.20, 100)
  )
  class(result) <- "cograph_cluster_significance"

  output <- capture.output(print(result))
  # Should not crash and should show basic info
  expect_true(any(grepl("Cluster Significance Test", output)))
  # Should not show conclusion when p-value is NA
  expect_false(any(grepl("Conclusion", output)))
})

test_that("print.cograph_cluster_significance returns invisible x", {
  result <- list(
    method = "rewire",
    n_random = 50,
    observed_modularity = 0.3,
    null_mean = 0.2,
    null_sd = 0.05,
    z_score = 2.0,
    p_value = 0.02,
    null_values = rnorm(50, 0.2, 0.05)
  )
  class(result) <- "cograph_cluster_significance"

  ret <- print(result)
  expect_s3_class(ret, "cograph_cluster_significance")
})

# ==============================================================================
# Tests for print.cluster_summary (cluster-metrics.R)
# ==============================================================================

test_that("print.cluster_summary shows basic info", {
  result <- structure(
    list(
      cluster_names = c("1", "2", "3"),
      cluster_sizes = c(5, 3, 2),
      between = matrix(c(0, 0.2, 0.1, 0.2, 0, 0.15, 0.1, 0.15, 0), 3, 3),
      within = c(0.5, 0.4, 0.3),
      method = "louvain",
      directed = FALSE
    ),
    class = "cluster_summary"
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("Cluster Summary", output)))
  expect_true(any(grepl("louvain", output)))
  expect_true(any(grepl("Clusters:", output)))
  expect_true(any(grepl("Between-cluster matrix", output)))
})

test_that("print.cluster_summary shows cluster sizes", {
  result <- structure(
    list(
      cluster_names = c("A", "B"),
      cluster_sizes = c(10, 5),
      between = matrix(c(0, 0.3, 0.3, 0), 2, 2),
      within = c(0.6, 0.4),
      method = "fast_greedy",
      directed = TRUE
    ),
    class = "cluster_summary"
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("Cluster sizes:", output)))
  expect_true(any(grepl("10, 5", output)))
})

test_that("print.cluster_summary returns invisible x", {
  result <- structure(
    list(
      cluster_names = c("1", "2"),
      cluster_sizes = c(3, 2),
      between = matrix(c(0, 0.2, 0.2, 0), 2, 2),
      within = c(0.5, 0.4),
      method = "walktrap",
      directed = TRUE
    ),
    class = "cluster_summary"
  )

  ret <- print(result)
  expect_identical(ret, result)
})

# ==============================================================================
# Tests for print.cluster_quality (cluster-metrics.R)
# ==============================================================================

test_that("print.cluster_quality shows basic info", {
  result <- structure(
    list(
      global = list(
        modularity = 0.45,
        coverage = 0.78,
        n_clusters = 3
      ),
      per_cluster = data.frame(
        cluster = 1:3,
        size = c(5, 3, 2),
        internal = c(0.6, 0.5, 0.4),
        external = c(0.2, 0.3, 0.2)
      )
    ),
    class = "cluster_quality"
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("Cluster Quality Metrics", output)))
  expect_true(any(grepl("Modularity:", output)))
  expect_true(any(grepl("Coverage:", output)))
  expect_true(any(grepl("Per-cluster metrics", output)))
})

test_that("print.cluster_quality shows global metrics", {
  result <- structure(
    list(
      global = list(
        modularity = 0.55,
        coverage = 0.85,
        n_clusters = 4
      ),
      per_cluster = data.frame(
        cluster = 1:4,
        size = c(5, 4, 3, 2),
        internal = c(0.7, 0.6, 0.5, 0.4),
        external = c(0.15, 0.2, 0.25, 0.2)
      )
    ),
    class = "cluster_quality"
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("Clusters:.*4", output)))
  expect_true(any(grepl("Global metrics", output)))
})

test_that("print.cluster_quality returns invisible x", {
  result <- structure(
    list(
      global = list(
        modularity = 0.35,
        coverage = 0.65,
        n_clusters = 2
      ),
      per_cluster = data.frame(
        cluster = 1:2,
        size = c(4, 3),
        internal = c(0.5, 0.4),
        external = c(0.3, 0.2)
      )
    ),
    class = "cluster_quality"
  )

  ret <- print(result)
  expect_identical(ret, result)
})

# ==============================================================================
# Tests for print.cograph_communities (communities.R)
# ==============================================================================

test_that("print.cograph_communities shows basic info", {
  skip_if_not_installed("igraph")

  # Create a proper igraph communities object
  g <- igraph::make_ring(6)
  igraph::V(g)$name <- letters[1:6]

  # Use actual community detection to get a proper object
  communities <- igraph::cluster_louvain(g)
  result <- communities
  result$algorithm <- "louvain"
  result$names <- letters[1:6]
  class(result) <- c("cograph_communities", class(communities))

  output <- capture.output(print(result))
  expect_true(any(grepl("Community structure", output)) ||
                any(grepl("louvain", output)))
})

test_that("print.cograph_communities without node names", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4)
  communities <- igraph::cluster_louvain(g)
  result <- communities
  result$algorithm <- "fast_greedy"
  result$names <- NULL
  class(result) <- c("cograph_communities", class(communities))

  # Should not error when names is NULL
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("all print methods return invisible self", {
  # Test cograph_network (list format)
  net <- list(
    n_nodes = 3, n_edges = 2, directed = FALSE,
    nodes = data.frame(name = c("A", "B", "C"), x = c(0, 1, 0.5), y = c(0, 0, 1)),
    edges = data.frame(from = c("A", "B"), to = c("B", "C")),
    weight = c(1, 1)
  )
  class(net) <- c("cograph_network", "list")
  result <- print(net)
  expect_s3_class(result, "cograph_network")

  # Test cograph_motifs
  motifs <- list(
    size = 3, directed = TRUE, method = "rewire", n_random = 10,
    counts = c(m1 = 1), null_mean = c(m1 = 1), z_scores = c(m1 = 0),
    p_values = c(m1 = 0.5), significant = c(m1 = FALSE)
  )
  class(motifs) <- "cograph_motifs"
  result <- print(motifs)
  expect_s3_class(result, "cograph_motifs")

  # Test cluster_summary
  cs <- structure(
    list(
      cluster_names = "1", cluster_sizes = 3,
      between = matrix(0, 1, 1), within = 0.5,
      method = "test", directed = FALSE
    ),
    class = "cluster_summary"
  )
  result <- print(cs)
  expect_s3_class(result, "cluster_summary")

  # Test cluster_quality
  cq <- structure(
    list(
      global = list(modularity = 0.4, coverage = 0.7, n_clusters = 1),
      per_cluster = data.frame(cluster = 1, size = 3, internal = 0.5, external = 0.2)
    ),
    class = "cluster_quality"
  )
  result <- print(cq)
  expect_s3_class(result, "cluster_quality")
})

test_that("print output is human readable", {
  net <- list(
    n_nodes = 5, n_edges = 6, directed = TRUE,
    nodes = data.frame(
      name = c("A", "B", "C", "D", "E"),
      x = c(0, 1, 2, 0.5, 1.5),
      y = c(0, 0, 0, 1, 1)
    ),
    edges = data.frame(
      from = c("A", "A", "B", "B", "C", "D"),
      to = c("B", "C", "C", "D", "E", "E")
    ),
    weight = c(0.1, 0.5, 0.3, 0.8, 0.2, 0.9)
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  combined <- paste(output, collapse = " ")

  # Output should contain recognizable network information

  expect_true(grepl("Cograph", combined))
  expect_true(grepl("5", combined))  # n_nodes
  expect_true(grepl("6", combined))  # n_edges
})
