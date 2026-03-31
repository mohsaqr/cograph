# Tests for plot_simplicial() with net_hon and net_hypa objects

skip_on_cran()

# ============================================
# Mock factories (no nestimate dependency)
# ============================================

create_mock_net_hon <- function(higher_order = TRUE, use_labels = FALSE) {
  states <- if (use_labels) {
    c("adapt", "cohesion", "consensus", "discuss")
  } else {
    c("1", "2", "3", "4")
  }
  if (higher_order) {
    edges <- data.frame(
      path = if (use_labels) {
        c("adapt -> cohesion", "cohesion -> consensus",
          "adapt -> cohesion -> consensus",
          "cohesion -> consensus -> discuss")
      } else {
        c("1 -> 2", "2 -> 3", "1 -> 2 -> 3", "2 -> 3 -> 4")
      },
      from = if (use_labels) {
        c("adapt", "cohesion", "adapt -> cohesion",
          "cohesion -> consensus")
      } else {
        c("1", "2", "1 -> 2", "2 -> 3")
      },
      to = if (use_labels) {
        c("cohesion", "consensus", "consensus", "discuss")
      } else {
        c("2", "3", "3", "4")
      },
      count = c(10L, 8L, 5L, 3L),
      probability = c(0.5, 0.4, 0.25, 0.15),
      from_order = c(1L, 1L, 2L, 2L),
      to_order = c(2L, 2L, 3L, 3L),
      stringsAsFactors = FALSE
    )
  } else {
    edges <- data.frame(
      path = if (use_labels) {
        c("adapt -> cohesion", "cohesion -> consensus",
          "consensus -> discuss")
      } else {
        c("1 -> 2", "2 -> 3", "3 -> 4")
      },
      from = states[1:3],
      to = states[2:4],
      count = c(10L, 8L, 6L),
      probability = c(0.5, 0.4, 0.3),
      from_order = c(1L, 1L, 1L),
      to_order = c(2L, 2L, 2L),
      stringsAsFactors = FALSE
    )
  }
  structure(list(
    edges = edges,
    first_order_states = states,
    matrix = matrix(0, 4, 4, dimnames = list(states, states))
  ), class = "net_hon")
}

create_mock_net_hypa <- function(has_anomalies = TRUE, use_labels = FALSE) {
  if (has_anomalies) {
    scores <- data.frame(
      path = if (use_labels) {
        c("adapt -> cohesion -> consensus",
          "cohesion -> consensus -> discuss",
          "adapt -> consensus -> discuss",
          "consensus -> adapt -> cohesion")
      } else {
        c("1 -> 2 -> 3", "2 -> 3 -> 4", "1 -> 3 -> 4", "3 -> 1 -> 2")
      },
      anomaly = c("over", "under", "normal", "over"),
      ratio = c(5.0, 3.2, 1.0, 4.1),
      stringsAsFactors = FALSE
    )
  } else {
    scores <- data.frame(
      path = if (use_labels) {
        c("adapt -> cohesion -> consensus",
          "cohesion -> consensus -> discuss")
      } else {
        c("1 -> 2 -> 3", "2 -> 3 -> 4")
      },
      anomaly = c("normal", "normal"),
      ratio = c(1.0, 0.9),
      stringsAsFactors = FALSE
    )
  }
  nodes <- if (use_labels) {
    c("adapt\x01cohesion", "cohesion\x01consensus",
      "consensus\x01discuss", "adapt\x01consensus")
  } else {
    c("1\x012", "2\x013", "3\x014", "1\x013")
  }
  structure(list(
    scores = scores,
    nodes = nodes,
    adjacency = matrix(0, 4, 4)
  ), class = "net_hypa")
}

# Mock tna object with labels for numeric ID translation
create_mock_tna <- function() {
  labels <- c("adapt", "cohesion", "consensus", "discuss")
  mat <- matrix(0.25, 4, 4, dimnames = list(labels, labels))
  diag(mat) <- 0
  structure(list(
    weights = mat,
    labels = labels,
    inits = rep(0.25, 4),
    data = NULL
  ), class = "tna")
}

# ============================================
# .extract_hon_pathways tests
# ============================================

test_that(".extract_hon_pathways extracts higher-order edges", {
  hon <- create_mock_net_hon()
  paths <- cograph:::.extract_hon_pathways(hon)
  expect_length(paths, 2)
  expect_equal(paths[1], "1 2 -> 3")
  expect_equal(paths[2], "2 3 -> 4")
})

test_that(".extract_hon_pathways returns empty for first-order only", {
  hon <- create_mock_net_hon(higher_order = FALSE)
  paths <- cograph:::.extract_hon_pathways(hon)
  expect_length(paths, 0)
  expect_identical(paths, character(0))
})

test_that(".extract_hon_pathways translates numeric IDs with label_map", {
  hon <- create_mock_net_hon()
  label_map <- c("1" = "adapt", "2" = "cohesion",
                 "3" = "consensus", "4" = "discuss")
  paths <- cograph:::.extract_hon_pathways(hon, label_map = label_map)
  expect_length(paths, 2)
  expect_equal(paths[1], "adapt cohesion -> consensus")
  expect_equal(paths[2], "cohesion consensus -> discuss")
})

test_that(".extract_hon_pathways sorts by count descending", {
  hon <- create_mock_net_hon()
  # count: 5, 3 — first HO edge has count=5
  paths <- cograph:::.extract_hon_pathways(hon)
  expect_equal(paths[1], "1 2 -> 3")  # count=5
  expect_equal(paths[2], "2 3 -> 4")  # count=3
})

# ============================================
# .extract_hypa_pathways tests
# ============================================

test_that(".extract_hypa_pathways extracts anomalous paths", {
  hypa <- create_mock_net_hypa()
  paths <- cograph:::.extract_hypa_pathways(hypa)
  expect_length(paths, 3)
  # Sorted by ratio descending: 5.0, 4.1, 3.2
  expect_equal(paths[1], "1 2 -> 3")   # ratio=5.0
  expect_equal(paths[2], "3 1 -> 2")   # ratio=4.1
  expect_equal(paths[3], "2 3 -> 4")   # ratio=3.2
})

test_that(".extract_hypa_pathways filters by type", {
  hypa <- create_mock_net_hypa()
  over <- cograph:::.extract_hypa_pathways(hypa, type = "over")
  expect_length(over, 2)

  under <- cograph:::.extract_hypa_pathways(hypa, type = "under")
  expect_length(under, 1)
  expect_equal(under, "2 3 -> 4")
})

test_that(".extract_hypa_pathways returns empty for no anomalies", {
  hypa <- create_mock_net_hypa(has_anomalies = FALSE)
  paths <- cograph:::.extract_hypa_pathways(hypa)
  expect_length(paths, 0)
  expect_identical(paths, character(0))
})

test_that(".extract_hypa_pathways translates with label_map", {
  hypa <- create_mock_net_hypa()
  label_map <- c("1" = "adapt", "2" = "cohesion",
                 "3" = "consensus", "4" = "discuss")
  paths <- cograph:::.extract_hypa_pathways(hypa, label_map = label_map)
  expect_true("adapt cohesion -> consensus" %in% paths)
  expect_true("cohesion consensus -> discuss" %in% paths)
})

# ============================================
# .build_hon_label_map tests
# ============================================

test_that(".build_hon_label_map returns named vector from tna", {
  model <- create_mock_tna()
  lm <- cograph:::.build_hon_label_map(model)
  expect_equal(lm, c("1" = "adapt", "2" = "cohesion",
                      "3" = "consensus", "4" = "discuss"))
})

test_that(".build_hon_label_map returns NULL for non-tna", {
  expect_null(cograph:::.build_hon_label_map(NULL))
  mat <- matrix(0, 3, 3)
  expect_null(cograph:::.build_hon_label_map(mat))
})

# ============================================
# .extract_blob_states for HON/HYPA
# ============================================

test_that(".extract_blob_states returns first_order_states for net_hon", {
  hon <- create_mock_net_hon()
  states <- cograph:::.extract_blob_states(hon)
  expect_equal(states, c("1", "2", "3", "4"))
})

test_that(".extract_blob_states extracts unique states from net_hypa nodes", {
  hypa <- create_mock_net_hypa()
  states <- cograph:::.extract_blob_states(hypa)
  expect_true(is.character(states))
  expect_true(all(c("1", "2", "3", "4") %in% states))
  expect_equal(states, sort(unique(states)))
})

# ============================================
# plot_simplicial(x, hon) — tna + HON
# ============================================

test_that("plot_simplicial(tna, hon) translates labels and renders", {
  model <- create_mock_tna()
  hon <- create_mock_net_hon()
  expect_no_error(p <- with_temp_png(
    plot_simplicial(model, hon, dismantled = TRUE)
  ))
  expect_true(inherits(p, "grob") || is.list(p))
})

test_that("plot_simplicial(tna, hypa) translates labels and renders", {
  model <- create_mock_tna()
  hypa <- create_mock_net_hypa()
  expect_no_error(p <- with_temp_png(
    plot_simplicial(model, hypa, dismantled = TRUE)
  ))
  expect_true(inherits(p, "grob") || is.list(p))
})

# ============================================
# plot_simplicial(hon) — HON as x
# ============================================

test_that("plot_simplicial(hon) auto-extracts with numeric labels", {
  hon <- create_mock_net_hon()
  expect_no_error(p <- with_temp_png(plot_simplicial(hon)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_simplicial(hon) with label states works directly", {
  hon <- create_mock_net_hon(use_labels = TRUE)
  expect_no_error(p <- with_temp_png(plot_simplicial(hon)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_simplicial returns NULL for HON without HO edges", {
  hon <- create_mock_net_hon(higher_order = FALSE)
  expect_message(
    result <- plot_simplicial(hon),
    "No higher-order pathways"
  )
  expect_null(result)
})

# ============================================
# plot_simplicial(hypa) — HYPA as x
# ============================================

test_that("plot_simplicial(hypa) auto-extracts anomalous paths", {
  hypa <- create_mock_net_hypa()
  expect_no_error(p <- with_temp_png(plot_simplicial(hypa)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_simplicial returns NULL for HYPA without anomalies", {
  hypa <- create_mock_net_hypa(has_anomalies = FALSE)
  expect_message(
    result <- plot_simplicial(hypa),
    "No anomalous pathways"
  )
  expect_null(result)
})

# ============================================
# plot_simplicial(tna, hon) returns NULL for no HO edges
# ============================================

test_that("plot_simplicial(tna, hon) with no HO edges returns NULL", {
  model <- create_mock_tna()
  hon <- create_mock_net_hon(higher_order = FALSE)
  expect_message(
    result <- plot_simplicial(model, hon),
    "No higher-order pathways"
  )
  expect_null(result)
})

test_that("plot_simplicial(tna, hypa) with no anomalies returns NULL", {
  model <- create_mock_tna()
  hypa <- create_mock_net_hypa(has_anomalies = FALSE)
  expect_message(
    result <- plot_simplicial(model, hypa),
    "No anomalous pathways"
  )
  expect_null(result)
})

# ============================================
# max_pathways limiting
# ============================================

test_that("max_pathways limits number of pathways displayed", {
  model <- create_mock_tna()
  hon <- create_mock_net_hon()
  # HON has 2 HO pathways; limit to 1
  expect_no_error(p <- with_temp_png(
    plot_simplicial(model, hon, max_pathways = 1)
  ))
  expect_s3_class(p, "ggplot")
})

test_that("max_pathways = NULL shows all pathways", {
  hon <- create_mock_net_hon()
  expect_no_error(p <- with_temp_png(
    plot_simplicial(hon, max_pathways = NULL)
  ))
  expect_s3_class(p, "ggplot")
})

# ============================================
# User pathways override auto-extraction
# ============================================

test_that("plot_simplicial uses user pathways over auto-extraction for HON", {
  hon <- create_mock_net_hon(use_labels = TRUE)
  custom <- c("adapt cohesion -> discuss", "consensus discuss -> adapt")
  expect_no_error(p <- with_temp_png(
    plot_simplicial(hon, pathways = custom)
  ))
  expect_s3_class(p, "ggplot")
})

# ============================================
# Error when pathways missing and x is not HON/HYPA
# ============================================

test_that("plot_simplicial errors when pathways NULL and x is plain matrix", {
  mat <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  expect_error(
    plot_simplicial(mat),
    "pathways.*must be provided"
  )
})

test_that("plot_simplicial errors when pathways NULL and x is NULL", {
  expect_error(
    plot_simplicial(x = NULL),
    "pathways.*must be provided"
  )
})

# ============================================
# Regression: existing functionality unchanged
# ============================================

test_that("plot_simplicial still works with explicit pathways (regression)", {
  mat <- matrix(runif(16), 4, 4,
                dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  diag(mat) <- 0
  expect_no_error(p <- with_temp_png(
    plot_simplicial(mat, c("A B -> C", "B C -> D"))
  ))
  expect_s3_class(p, "ggplot")
})

test_that("plot_simplicial with NULL x and explicit pathways still works", {
  expect_no_error(p <- with_temp_png(
    plot_simplicial(pathways = c("A B -> C", "X Y -> Z"))
  ))
  expect_s3_class(p, "ggplot")
})

# ============================================
# Direct tna/netobject support (requires Nestimate)
# ============================================

test_that("plot_simplicial(tna) auto-builds HON from sequence data", {
  skip_if_not_installed("Nestimate")
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  expect_no_error(p <- with_temp_png(
    plot_simplicial(model, max_pathways = 5)
  ))
  expect_s3_class(p, "ggplot")
})

test_that("plot_simplicial(tna, method='hypa') auto-builds HYPA", {
  skip_if_not_installed("Nestimate")
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  expect_no_error(p <- with_temp_png(
    plot_simplicial(model, method = "hypa", max_pathways = 5)
  ))
  expect_s3_class(p, "ggplot")
})

test_that("plot_simplicial(tna) dismantled uses grid layout", {
  skip_if_not_installed("Nestimate")
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  expect_no_error(p <- with_temp_png(
    plot_simplicial(model, max_pathways = 4, dismantled = TRUE, ncol = 2)
  ))
  expect_true(inherits(p, "grob") || is.list(p))
})

test_that("plot_simplicial(netobject) auto-builds HON", {
  skip_if_not_installed("Nestimate")
  skip_if_no_tna()
  df <- as.data.frame(tna::tna(tna::group_regulation)$data)
  net <- Nestimate::build_network(df, method = "tna")
  expect_no_error(p <- with_temp_png(
    plot_simplicial(net, max_pathways = 5)
  ))
  expect_s3_class(p, "ggplot")
})

# ============================================
# .extract_sequence_data tests
# ============================================

test_that(".extract_sequence_data converts tna data to labeled df", {
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  df <- cograph:::.extract_sequence_data(model)
  expect_s3_class(df, "data.frame")
  # Should have character values (state labels), not numeric
  expect_true(is.character(df[[1]]))
})

test_that(".extract_sequence_data returns NULL for unsupported types", {
  expect_null(cograph:::.extract_sequence_data(NULL))
  expect_null(cograph:::.extract_sequence_data(matrix(0, 3, 3)))
})

# ============================================
# .build_higher_order tests
# ============================================

test_that(".build_higher_order errors without Nestimate", {
  # Can't really test this if Nestimate is installed, so just test bad method
  skip_if_not_installed("Nestimate")
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  expect_error(
    cograph:::.build_higher_order(model, method = "bad"),
    "method must be"
  )
})

test_that(".build_higher_order errors for objects without data", {
  skip_if_not_installed("Nestimate")
  tna_no_data <- structure(list(
    weights = matrix(0, 3, 3), labels = c("A", "B", "C"),
    inits = c(1/3, 1/3, 1/3), data = NULL
  ), class = "tna")
  expect_error(
    cograph:::.build_higher_order(tna_no_data),
    "Cannot extract sequence data"
  )
})

# ============================================
# ncol parameter for grid layout
# ============================================

test_that("ncol controls grid columns in dismantled mode", {
  hon <- create_mock_net_hon()
  expect_no_error(p <- with_temp_png(
    plot_simplicial(hon, dismantled = TRUE, ncol = 1)
  ))
  expect_true(inherits(p, "grob") || is.list(p))
})
