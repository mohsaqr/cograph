# Regression tests for multi-step transition plots.
#
# plot_transitions() reaches .plot_transitions_multi() from two branches: a
# list of transition matrices, and a multi-column data frame that is turned
# into consecutive transition matrices. The data-frame branch used to forward
# only a subset of the styling arguments, so `value_min`, the label/title/value
# colour, fontface and nudge settings, and `total_fontface` were accepted and
# then silently dropped for that input alone.
#
# The invariant these tests protect is parity: the same data described either
# way must respond to these arguments identically.

# Same transitions expressed as a data frame and as a list of matrices.
multi_step_data_frame <- function(n = 200, seed = 1) {
  set.seed(seed)
  data.frame(
    T1 = sample(c("A", "B", "C"), n, replace = TRUE),
    T2 = sample(c("A", "B", "C"), n, replace = TRUE),
    T3 = sample(c("A", "B", "C"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

multi_step_matrices <- function(df) {
  lapply(seq_len(ncol(df) - 1), function(i) {
    as.matrix(table(df[[i]], df[[i + 1]]))
  })
}

# Every text layer the plot draws, as one data frame.
drawn_labels <- function(p) {
  layers <- ggplot2::ggplot_build(p)$data
  keep <- Filter(function(l) "label" %in% names(l), layers)
  if (length(keep) == 0L) {
    return(data.frame(colour = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(keep, function(l) {
    data.frame(colour = as.character(l$colour), stringsAsFactors = FALSE)
  }))
}

test_that("value_min filters flow labels for both multi-step inputs", {
  df <- multi_step_data_frame()
  mats <- multi_step_matrices(df)
  # Every flow is far below the threshold, so all flow labels must go.
  threshold <- sum(unlist(mats)) + 1

  from_df_all <- nrow(drawn_labels(
    plot_alluvial(df, show_values = TRUE, value_min = 0)
  ))
  from_df_filtered <- nrow(drawn_labels(
    plot_alluvial(df, show_values = TRUE, value_min = threshold)
  ))
  from_mat_all <- nrow(drawn_labels(
    plot_alluvial(mats, show_values = TRUE, value_min = 0)
  ))
  from_mat_filtered <- nrow(drawn_labels(
    plot_alluvial(mats, show_values = TRUE, value_min = threshold)
  ))

  expect_lt(from_df_filtered, from_df_all)
  expect_lt(from_mat_filtered, from_mat_all)
  # Parity: the same data described either way must filter identically.
  expect_equal(from_df_all, from_mat_all)
  expect_equal(from_df_filtered, from_mat_filtered)
})

test_that("label_color reaches multi-step plots built from a data frame", {
  df <- multi_step_data_frame()
  mats <- multi_step_matrices(df)

  expect_true(any(drawn_labels(plot_alluvial(df, label_color = "red"))$colour
                  == "red"))
  expect_true(any(drawn_labels(plot_alluvial(mats, label_color = "red"))$colour
                  == "red"))
})

test_that("the data-frame branch forwards every argument the callee takes", {
  # A structural guard: the two call sites drifted apart once, and comparing
  # the forwarded names against the callee's formals catches it without
  # rendering anything.
  callee <- names(formals(cograph:::.plot_transitions_multi))
  body_lines <- deparse(body(cograph::plot_transitions))
  starts <- grep("\\.plot_transitions_multi\\(", body_lines)
  expect_length(starts, 2L)

  forwarded <- lapply(starts, function(start) {
    stop_at <- start
    depth <- 0L
    repeat {
      depth <- depth + lengths(regmatches(
        body_lines[stop_at], gregexpr("\\(", body_lines[stop_at])
      )) - lengths(regmatches(
        body_lines[stop_at], gregexpr("\\)", body_lines[stop_at])
      ))
      if (depth <= 0L) break
      stop_at <- stop_at + 1L
    }
    chunk <- paste(body_lines[start:stop_at], collapse = " ")
    unique(gsub("^\\s*|\\s*$", "", regmatches(
      chunk, gregexpr("[A-Za-z_][A-Za-z0-9_.]*(?=\\s*=)", chunk, perl = TRUE)
    )[[1L]]))
  })

  # `matrices` is passed positionally, so exclude it from the comparison.
  expected <- setdiff(callee, "matrices")
  for (passed in forwarded) {
    expect_setequal(intersect(passed, callee), expected)
  }
})
