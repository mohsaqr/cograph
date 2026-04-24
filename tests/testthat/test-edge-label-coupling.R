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

test_that("scaling = 'fixed' gives label cex 1.0 (no visual-scale)", {
  m <- harmony_matrix()
  with_temp_png({
    p <- splot(m, labels = TRUE, legend = FALSE, psych_styling = TRUE,
               scaling = "fixed")
    expect_true(all(abs(p$nodes$label_size - 1.0) < 1e-6))
  }, width = 700, height = 500, res = 96)
})
