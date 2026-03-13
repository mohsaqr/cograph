# Tests for unified motifs() API
# Covers: auto-detection helpers, edgelist_to_trans_array, motifs(), subgraphs()

# ================================================================
# Task 1: Auto-detection helpers
# ================================================================

test_that(".detect_actor_column finds standard names", {
  df <- data.frame(from = "A", to = "B", session_id = "S1")
  expect_equal(.detect_actor_column(df), "session_id")

  df2 <- data.frame(from = "A", to = "B", session = "S1")
  expect_equal(.detect_actor_column(df2), "session")

  df3 <- data.frame(from = "A", to = "B", actor = "u1")
  expect_equal(.detect_actor_column(df3), "actor")

  df4 <- data.frame(from = "A", to = "B", participant = "p1")
  expect_equal(.detect_actor_column(df4), "participant")
})

test_that(".detect_actor_column returns NULL when no match", {
  df <- data.frame(from = "A", to = "B", weight = 1)
  expect_null(.detect_actor_column(df))
})

test_that(".detect_actor_column is case-insensitive", {
  df <- data.frame(from = "A", to = "B", Session_ID = "S1")
  expect_equal(.detect_actor_column(df), "Session_ID")
})

test_that(".detect_actor_column respects priority order", {
  df <- data.frame(from = "A", to = "B", id = "i1", session = "S1")
  expect_equal(.detect_actor_column(df), "session")
})

test_that(".detect_actor_column returns NULL for non-data.frame", {
  expect_null(.detect_actor_column("not a df"))
  expect_null(.detect_actor_column(NULL))
})

test_that(".detect_order_column finds standard names", {
  df <- data.frame(from = "A", to = "B", timestamp = 1)
  expect_equal(.detect_order_column(df), "timestamp")

  df2 <- data.frame(from = "A", to = "B", order = 1)
  expect_equal(.detect_order_column(df2), "order")

  df3 <- data.frame(from = "A", to = "B", time = 1)
  expect_equal(.detect_order_column(df3), "time")

  df4 <- data.frame(from = "A", to = "B", step = 1)
  expect_equal(.detect_order_column(df4), "step")
})

test_that(".detect_order_column returns NULL when no match", {
  df <- data.frame(from = "A", to = "B", weight = 1)
  expect_null(.detect_order_column(df))
})

test_that(".detect_order_column returns NULL for non-data.frame", {
  expect_null(.detect_order_column(42))
})

# ================================================================
# Task 2: .edgelist_to_trans_array
# ================================================================

test_that(".edgelist_to_trans_array builds correct 3D array", {
  el <- data.frame(
    from = c("A", "B", "A", "B", "C", "B"),
    to   = c("B", "C", "C", "A", "B", "C"),
    stringsAsFactors = FALSE
  )
  # No actor column — treat as single group
  result <- .edgelist_to_trans_array(el)
  expect_equal(dim(result$trans)[1], 1L)
  expect_equal(dim(result$trans)[2], 3L)
  expect_equal(dim(result$trans)[3], 3L)
  expect_equal(sort(result$labels), c("A", "B", "C"))

  # With actor column — two groups
  el$group <- c("g1", "g1", "g1", "g2", "g2", "g2")
  result2 <- .edgelist_to_trans_array(el, actor_col = "group")
  expect_equal(dim(result2$trans)[1], 2L)
})

test_that(".edgelist_to_trans_array respects weights", {
  el <- data.frame(
    from = c("A", "A"), to = c("B", "B"), weight = c(3, 2),
    group = c("g1", "g2"), stringsAsFactors = FALSE
  )
  result <- .edgelist_to_trans_array(el, actor_col = "group")
  a_idx <- match("A", result$labels)
  b_idx <- match("B", result$labels)
  expect_equal(result$trans[1, a_idx, b_idx], 3)
  expect_equal(result$trans[2, a_idx, b_idx], 2)
})

test_that(".edgelist_to_trans_array handles windowing (tumbling)", {
  el <- data.frame(
    from = c("A", "B", "C", "A", "C", "B"),
    to   = c("B", "C", "A", "C", "B", "A"),
    order = 1:6,
    stringsAsFactors = FALSE
  )
  result <- .edgelist_to_trans_array(el, order_col = "order",
                                      window = 3, window_type = "tumbling")
  expect_equal(dim(result$trans)[1], 2L)
})

test_that(".edgelist_to_trans_array handles windowing (rolling)", {
  el <- data.frame(
    from = c("A", "B", "C", "A"),
    to   = c("B", "C", "A", "C"),
    order = 1:4,
    stringsAsFactors = FALSE
  )
  result <- .edgelist_to_trans_array(el, order_col = "order",
                                      window = 3, window_type = "rolling")
  expect_equal(dim(result$trans)[1], 2L)
})

test_that(".edgelist_to_trans_array windowing with actor groups", {
  el <- data.frame(
    from   = c("A", "B", "C", "A", "B", "C", "A", "B"),
    to     = c("B", "C", "A", "C", "A", "B", "C", "A"),
    group  = c("g1", "g1", "g1", "g1", "g2", "g2", "g2", "g2"),
    order  = c(1, 2, 3, 4, 1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  result <- .edgelist_to_trans_array(el, actor_col = "group",
                                      order_col = "order",
                                      window = 2, window_type = "tumbling")
  # Each group has 4 edges / 2 = 2 windows, so 2 groups × 2 = 4 total

  expect_equal(dim(result$trans)[1], 4L)
})

# ================================================================
# Task 3: Core motifs() function - input dispatch
# ================================================================

test_that("motifs works with raw matrix (aggregate)", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, significance = FALSE)
  expect_s3_class(result, "cograph_motif_result")
  expect_true("results" %in% names(result))
  expect_true("type_summary" %in% names(result))
  expect_equal(result$level, "aggregate")
})

test_that("motifs works with igraph object", {
  skip_if_not_installed("igraph")
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE)
  result <- motifs(g, significance = FALSE)
  expect_s3_class(result, "cograph_motif_result")
})

test_that("motifs works with cograph_network from matrix", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  net <- as_cograph(mat)
  result <- motifs(net, significance = FALSE)
  expect_s3_class(result, "cograph_motif_result")
})

test_that("motifs works with data.frame edge list", {
  el <- data.frame(
    from = c("A", "B", "A", "B", "C", "A", "C", "B", "A", "C"),
    to   = c("B", "C", "C", "A", "B", "B", "A", "A", "C", "B"),
    stringsAsFactors = FALSE
  )
  result <- motifs(el, significance = FALSE)
  expect_s3_class(result, "cograph_motif_result")
  expect_equal(result$level, "aggregate")
})

test_that("motifs census works with tna object (individual level)", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod, significance = FALSE)
  expect_s3_class(result, "cograph_motif_result")
  expect_false(result$named_nodes)
  expect_equal(result$level, "individual")
  expect_true(result$n_units > 1)
  expect_true("type" %in% names(result$results))
})

# --- Auto-detection tests ---

test_that("motifs auto-detects session column from cograph edge list", {
  el <- data.frame(
    from = c("A", "B", "A", "B", "C", "A",
             "A", "B", "C", "A", "B", "C"),
    to   = c("B", "C", "C", "A", "B", "B",
             "C", "A", "B", "B", "C", "A"),
    session_id = c(rep("s1", 6), rep("s2", 6)),
    stringsAsFactors = FALSE
  )
  net <- as_cograph(el)
  result <- motifs(net, significance = FALSE, min_transitions = 1)
  expect_equal(result$level, "individual")
  expect_equal(result$n_units, 2L)
})

test_that("motifs actor= overrides auto-detection", {
  el <- data.frame(
    from = c("A", "B", "A", "B", "C", "A",
             "A", "B", "C", "A", "B", "C"),
    to   = c("B", "C", "C", "A", "B", "B",
             "C", "A", "B", "B", "C", "A"),
    session_id = c(rep("s1", 6), rep("s2", 6)),
    project = rep("p1", 12),
    stringsAsFactors = FALSE
  )
  net <- as_cograph(el)
  result <- motifs(net, actor = "project", significance = FALSE,
                   min_transitions = 1)
  expect_equal(result$n_units, 1L)
})

# --- Windowing tests ---

test_that("motifs with window parameter creates windowed groups", {
  el <- data.frame(
    from = c("A", "B", "C", "A", "B", "C"),
    to   = c("B", "C", "A", "C", "A", "B"),
    session_id = rep("s1", 6),
    order = 1:6,
    stringsAsFactors = FALSE
  )
  net <- as_cograph(el)
  result <- motifs(net, window = 3, window_type = "tumbling",
                   significance = FALSE, min_transitions = 1)
  expect_equal(result$n_units, 2L)
})

# --- Pattern filtering tests ---

test_that("motifs pattern argument filters types", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result_all <- motifs(mat, pattern = "all", significance = FALSE)
  result_tri <- motifs(mat, pattern = "triangle", significance = FALSE)
  expect_true(length(result_tri$type_summary) <= length(result_all$type_summary))
})

test_that("motifs include= filters to specific types", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, include = "030T", significance = FALSE)
  if (!is.null(result)) {
    expect_true(all(result$results$type == "030T"))
  }
})

# --- Census significance ---

test_that("motifs census has significance by default", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, n_perm = 10, seed = 42)
  expect_true(result$params$significance)
  expect_true("z" %in% names(result$results))
  expect_true("p" %in% names(result$results))
})

test_that("motifs census can disable significance", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, significance = FALSE)
  expect_false(result$params$significance)
  expect_false("z" %in% names(result$results))
})

test_that("motifs census significance (individual level)", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod, n_perm = 10, seed = 42)
  expect_true(result$params$significance)
  expect_true("z" %in% names(result$results))
})

# --- Subgraphs (instance mode) ---

test_that("subgraphs returns named node triples", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod, significance = FALSE)
  expect_s3_class(result, "cograph_motif_result")
  expect_true(result$named_nodes)
  expect_true("triad" %in% names(result$results))
})

test_that("subgraphs with significance", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod, n_perm = 10, seed = 42)
  expect_true(result$params$significance)
  expect_true("z" %in% names(result$results))
})

test_that("subgraphs can disable significance", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod, significance = FALSE)
  expect_false(result$params$significance)
  expect_false("z" %in% names(result$results))
})

test_that("subgraphs shows message with defaults", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  expect_message(subgraphs(Mod, significance = FALSE), "triangle patterns")
})

test_that("subgraphs suppresses message with explicit pattern", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  expect_no_message(subgraphs(Mod, pattern = "all", significance = FALSE))
})

test_that("subgraphs from matrix (aggregate)", {
  mat <- matrix(c(0, 3, 2, 0, 0, 0, 5, 1, 0, 0, 0, 4, 2, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- subgraphs(mat, significance = FALSE, pattern = "all",
                      min_count = NULL)
  if (!is.null(result)) {
    expect_true("triad" %in% names(result$results))
  }
})

# --- Error handling ---

test_that("motifs rejects unsupported input", {
  expect_error(motifs("string"), "Unsupported input type")
})

test_that("motifs returns NULL with no motifs found", {
  mat <- matrix(0, 4, 4)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  expect_message(motifs(mat, significance = FALSE), "No motifs found")
})

# ================================================================
# Task 4: Print method
# ================================================================

test_that("print.cograph_motif_result works for census", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, significance = FALSE)
  expect_output(print(result), "Motif Census")
  expect_output(print(result), "aggregate")
})

test_that("print.cograph_motif_result works for instances", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod, significance = FALSE)
  expect_output(print(result), "Motif Subgraphs")
})

test_that("print.cograph_motif_result shows significance info", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, n_perm = 10, seed = 42)
  expect_output(print(result), "Significance")
})

# ================================================================
# Task 5: Plot method
# ================================================================

test_that("plot.cograph_motif_result works for types", {
  skip_if_not_installed("ggplot2")
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, pattern = "all", significance = FALSE)
  expect_no_error(plot(result, type = "types"))
})

test_that("plot.cograph_motif_result works for significance type", {
  skip_if_not_installed("ggplot2")
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, n_perm = 10, seed = 42)
  expect_no_error(plot(result, type = "significance"))
})

test_that("plot.cograph_motif_result errors for significance without data", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, significance = FALSE)
  expect_error(plot(result, type = "significance"), "Significance data not available")
})

test_that("plot.cograph_motif_result works for triads (instance mode)", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod, significance = FALSE, top = 6)
  with_temp_png({
    expect_no_error(plot(result, type = "triads", n = 6, ncol = 3))
  })
})

# ================================================================
# Task 7: Integration tests
# ================================================================

test_that("motifs end-to-end with coding dataset via tna", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)

  # Basic census
  m <- motifs(Mod, significance = FALSE)
  expect_s3_class(m, "cograph_motif_result")
  expect_equal(m$level, "individual")
  expect_true(nrow(m$results) > 0)

  # With significance
  m_sig <- motifs(Mod, significance = TRUE, n_perm = 10, seed = 42)
  expect_true("z" %in% names(m_sig$results))

  # With pattern filter
  m_all <- motifs(Mod, pattern = "all", significance = FALSE)
  expect_true(nrow(m_all$results) >= nrow(m$results))

  # Top N
  m_top <- motifs(Mod, significance = FALSE, top = 3)
  expect_equal(nrow(m_top$results), 3L)
})

test_that("motifs end-to-end with synthetic edge list", {
  set.seed(42)
  states <- c("Plan", "Execute", "Monitor", "Adapt")
  n_sessions <- 5
  edges_per_session <- 20

  edge_list <- do.call(rbind, lapply(seq_len(n_sessions), function(s) {
    from_states <- sample(states, edges_per_session, replace = TRUE)
    to_states <- sample(states, edges_per_session, replace = TRUE)
    data.frame(
      from = from_states,
      to = to_states,
      session_id = paste0("s", s),
      project = paste0("p", ((s - 1) %/% 3) + 1),
      order = seq_len(edges_per_session),
      stringsAsFactors = FALSE
    )
  }))

  net <- as_cograph(edge_list)

  # Auto-detect session_id
  m <- motifs(net, significance = FALSE, min_transitions = 1)
  expect_equal(m$level, "individual")
  expect_equal(m$n_units, n_sessions)

  # Manual actor override
  m_proj <- motifs(net, actor = "project", significance = FALSE,
                   min_transitions = 1)
  expect_equal(m_proj$n_units, 2L)

  # Windowed
  m_win <- motifs(net, window = 10, window_type = "tumbling",
                  significance = FALSE, min_transitions = 1)
  expect_true(m_win$n_units > m$n_units)
})

test_that("motifs matrix without rownames", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  result <- motifs(mat, significance = FALSE)
  expect_s3_class(result, "cograph_motif_result")
  # Labels should be auto-generated V1..V4
  expect_true(all(grepl("^V", result$params$labels)))
})

test_that("motifs seed reproducibility", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  r1 <- motifs(Mod, n_perm = 10, seed = 42)
  r2 <- motifs(Mod, n_perm = 10, seed = 42)
  expect_equal(r1$results$z, r2$results$z)
})

# ================================================================
# Coverage: additional branches
# ================================================================

test_that("motifs cograph_network from matrix (no edge list)", {
  mat <- matrix(c(0, 3, 2, 0, 0, 0, 5, 1, 0, 0, 0, 4, 2, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  net <- as_cograph(mat)
  result <- motifs(net, significance = FALSE)
  expect_s3_class(result, "cograph_motif_result")
  expect_equal(result$level, "aggregate")
})

test_that("motifs igraph without weights and without names", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(5, directed = TRUE)
  result <- motifs(g, significance = FALSE, pattern = "all", min_transitions = 1)
  if (!is.null(result)) {
    expect_s3_class(result, "cograph_motif_result")
    expect_true(all(grepl("^V", result$params$labels)))
  }
})

test_that("motifs edge_method = 'expected'", {
  mat <- matrix(c(0, 3, 2, 0, 0, 0, 5, 1, 0, 0, 0, 4, 2, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, edge_method = "expected", edge_threshold = 1.5,
                   significance = FALSE, pattern = "all")
  expect_s3_class(result, "cograph_motif_result")
})

test_that("motifs census significance with edge_method = 'expected' (individual)", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod, edge_method = "expected", edge_threshold = 1.5,
                   significance = TRUE, n_perm = 5, seed = 42)
  expect_true("z" %in% names(result$results))
})

test_that("subgraphs instance mode: no motifs found returns NULL", {
  mat <- matrix(0, 4, 4)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  expect_message(
    subgraphs(mat, significance = FALSE, pattern = "all", min_count = NULL),
    "No motifs found"
  )
})

test_that("subgraphs significance without min_count", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod, min_count = NULL, n_perm = 5, seed = 42,
                      pattern = "all")
  expect_true("z" %in% names(result$results))
})

test_that("subgraphs min_count too high yields no results", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  expect_message(
    subgraphs(Mod, significance = FALSE, min_count = 99999, pattern = "all"),
    "No motifs with count"
  )
})

test_that("print with window info", {
  el <- data.frame(
    from = c("A", "B", "C", "A", "B", "C"),
    to   = c("B", "C", "A", "C", "A", "B"),
    session_id = rep("s1", 6),
    order = 1:6,
    stringsAsFactors = FALSE
  )
  net <- as_cograph(el)
  result <- motifs(net, window = 3, window_type = "tumbling",
                   significance = FALSE, min_transitions = 1)
  expect_output(print(result), "Window")
})

test_that("plot census triads falls back to patterns", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, pattern = "all", significance = FALSE)
  with_temp_png({
    expect_no_error(plot(result, type = "triads"))
  })
})

test_that("plot patterns type works", {
  mat <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- motifs(mat, pattern = "all", significance = FALSE)
  with_temp_png({
    expect_no_error(plot(result, type = "patterns"))
  })
})

test_that("motifs cograph_network edge list without actor column", {
  # Edge list with from/to but no session/actor/user/id column
  el <- data.frame(
    from = c("A", "B", "A", "C", "B", "C"),
    to   = c("B", "C", "C", "A", "A", "B"),
    weight = c(3, 2, 1, 4, 2, 5),
    stringsAsFactors = FALSE
  )
  net <- as_cograph(el)
  result <- motifs(net, significance = FALSE, pattern = "all")
  expect_s3_class(result, "cograph_motif_result")
  expect_equal(result$level, "aggregate")
})

test_that("subgraphs with edge_method = 'expected' (instance)", {
  mat <- matrix(c(0, 3, 2, 0, 0, 0, 5, 1, 0, 0, 0, 4, 2, 0, 0, 0),
                4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  result <- subgraphs(mat, edge_method = "expected", edge_threshold = 1.5,
                      significance = FALSE, pattern = "all", min_count = NULL)
  if (!is.null(result)) {
    expect_s3_class(result, "cograph_motif_result")
  }
})
