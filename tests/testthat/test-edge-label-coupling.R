# Regression tests for the edge-label cex coupling invariant.
# Default edge_label_size = mean(label_cex) * EDGE_LABEL_NODE_CEX_FRACTION
# when the user doesn't pass it. User-explicit values skip coupling.

harmony_matrix <- function() {
  nm <- c("A", "B", "C", "D", "E")
  m <- matrix(0, 5, 5, dimnames = list(nm, nm))
  set.seed(1)
  for (i in 1:4) for (j in (i + 1):5) {
    m[i, j] <- m[j, i] <- round(runif(1, -0.3, 0.3), 2)
  }
  m
}

test_that("default edge_label_size couples to node label cex", {
  m <- harmony_matrix()

  render_node_cex <- function(w, h, res) {
    with_temp_png({
      p <- splot(m, labels = TRUE, legend = FALSE, psych_styling = TRUE,
                 edge_labels = TRUE)
      mean(p$nodes$label_size)
    }, width = w, height = h, res = res)
  }

  node_small <- render_node_cex(700, 500, 96)
  node_big   <- render_node_cex(1400, 1400, 96)

  # Coupling makes the node/edge cex ratio identical across canvases.
  f <- cograph:::EDGE_LABEL_NODE_CEX_FRACTION
  ratio_small <- node_small / (node_small * f)
  ratio_big   <- node_big   / (node_big   * f)
  expect_equal(ratio_small, ratio_big, tolerance = 1e-9)
  expect_equal(ratio_small, 1 / f, tolerance = 1e-6)
})

test_that("user-explicit edge_label_size is preserved (no coupling)", {
  m <- harmony_matrix()
  with_temp_png({
    expect_silent({
      p <- splot(m, labels = TRUE, edge_labels = TRUE, legend = FALSE,
                 psych_styling = TRUE, edge_label_size = 0.8)
    })
    expect_s3_class(p, "cograph_network")
  }, width = 700, height = 500, res = 96)
})

test_that("user-explicit donut_bg_color survives psych_styling override", {
  # Regression: psych_styling block used to overwrite donut_bg_color whenever
  # it equaled the splot signature default ("gray90"), even when the user had
  # explicitly passed that value. User intent must beat styling defaults.
  m <- harmony_matrix()

  capture_bg <- function(extra_args) {
    captured <- NULL
    orig <- get("expand_param", envir = asNamespace("cograph"))
    unlockBinding("expand_param", asNamespace("cograph"))
    assign("expand_param", function(x, n, name, ...) {
      if (identical(name, "donut_bg_color")) captured <<- x
      orig(x, n, name, ...)
    }, envir = asNamespace("cograph"))
    lockBinding("expand_param", asNamespace("cograph"))
    on.exit({
      unlockBinding("expand_param", asNamespace("cograph"))
      assign("expand_param", orig, envir = asNamespace("cograph"))
      lockBinding("expand_param", asNamespace("cograph"))
    }, add = TRUE)

    with_temp_png({
      do.call(splot, c(list(x = m, labels = TRUE, legend = FALSE,
                            psych_styling = TRUE), extra_args))
    }, width = 600, height = 600, res = 96)
    captured
  }

  # Implicit default: psych_styling supplies its own ("white").
  expect_equal(capture_bg(list()), "white")
  # User passes the splot signature default value explicitly — must survive.
  expect_equal(capture_bg(list(donut_bg_color = "gray90")), "gray90")
  # User passes a non-default value — must also survive.
  expect_equal(capture_bg(list(donut_bg_color = "lightblue")), "lightblue")
})

test_that("user-explicit theme-overlap colors survive theme override", {
  # Regression: theme block at splot.R:857-859 used value-equality against the
  # signature defaults to detect "user didn't pass it" — silently overriding
  # users who passed those exact defaults together with `theme = ...`.
  m <- harmony_matrix()

  # Capture label colors via the leaf renderer; capture edge colors via the
  # resolve_edge_colors helper that turns positive/negative into per-edge cols.
  capture_label_colors <- function(extra_args) {
    captured <- character()
    orig <- get("draw_node_label_base", envir = asNamespace("cograph"))
    unlockBinding("draw_node_label_base", asNamespace("cograph"))
    assign("draw_node_label_base", function(..., col) {
      captured <<- c(captured, col)
      orig(..., col = col)
    }, envir = asNamespace("cograph"))
    lockBinding("draw_node_label_base", asNamespace("cograph"))
    on.exit({
      unlockBinding("draw_node_label_base", asNamespace("cograph"))
      assign("draw_node_label_base", orig, envir = asNamespace("cograph"))
      lockBinding("draw_node_label_base", asNamespace("cograph"))
    }, add = TRUE)
    with_temp_png({
      do.call(splot, c(list(x = m, labels = TRUE, legend = FALSE,
                            theme = "dark"), extra_args))
    }, width = 600, height = 600, res = 96)
    unique(captured)
  }

  capture_edge_args <- function(extra_args) {
    captured <- list()
    orig <- get("resolve_edge_colors", envir = asNamespace("cograph"))
    unlockBinding("resolve_edge_colors", asNamespace("cograph"))
    assign("resolve_edge_colors",
           function(edges, edge_color, positive_color, negative_color, ...) {
             captured <<- list(positive = positive_color, negative = negative_color)
             orig(edges, edge_color, positive_color, negative_color, ...)
           }, envir = asNamespace("cograph"))
    lockBinding("resolve_edge_colors", asNamespace("cograph"))
    on.exit({
      unlockBinding("resolve_edge_colors", asNamespace("cograph"))
      assign("resolve_edge_colors", orig, envir = asNamespace("cograph"))
      lockBinding("resolve_edge_colors", asNamespace("cograph"))
    }, add = TRUE)
    with_temp_png({
      do.call(splot, c(list(x = m, labels = TRUE, legend = FALSE,
                            theme = "dark"), extra_args))
    }, width = 600, height = 600, res = 96)
    captured
  }

  # User explicitly passes "black" (= signature default) alongside theme:
  # must beat theme's label color.
  expect_equal(capture_label_colors(list(label_color = "black")), "black")
  # Edge colors: explicit literal-default values must also survive the theme.
  ec_pos <- capture_edge_args(list(edge_positive_color = "#2E7D32"))
  expect_equal(ec_pos$positive, "#2E7D32")
  ec_neg <- capture_edge_args(list(edge_negative_color = "#C62828"))
  expect_equal(ec_neg$negative, "#C62828")
})

test_that("from_tna() override of NULL stores NULL instead of deleting", {
  # Regression: `params[[nm]] <- NULL` deletes the key (R list NULL trap).
  # User passing `from_tna(model, donut_fill = NULL)` to opt out of the
  # auto-donut feature was silently ignored.
  skip_if_not_installed("tna")
  model <- tna::tna(tna::group_regulation)
  params <- from_tna(model, donut_fill = NULL, plot = FALSE)
  expect_true("donut_fill" %in% names(params))
  expect_null(params$donut_fill)
})

test_that("scaling = 'fixed' gives label cex 1.0 (no visual-scale)", {
  m <- harmony_matrix()
  with_temp_png({
    p <- splot(m, labels = TRUE, legend = FALSE, psych_styling = TRUE,
               scaling = "fixed")
    expect_true(all(abs(p$nodes$label_size - 1.0) < 1e-6))
  }, width = 700, height = 500, res = 96)
})
