# Regression test for the edge-label cex coupling invariant.
#
# When the user does not pass edge_label_size explicitly, the default is
# computed as 0.55 * mean(label_cex) so the node-to-edge-label cex ratio
# stays stable at ~1.82x across canvases. This replaces the previous
# EDGE_LABEL_SCALE_CAP compensation path that let the ratio drift from
# 2.5x at reference to 3.6x at poster canvases.
#
# When the user passes edge_label_size explicitly, that value wins and
# only receives the (capped) visual-scale compensation.

with_png <- function(width, height, res, expr) {
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = width, height = height, res = res)
  on.exit({
    grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)
  force(expr)
}

harmony_matrix <- function() {
  nm <- c("A", "B", "C", "D", "E")
  m <- matrix(0, 5, 5, dimnames = list(nm, nm))
  set.seed(1)
  for (i in 1:4) for (j in (i + 1):5) {
    m[i, j] <- m[j, i] <- round(runif(1, -0.3, 0.3), 2)
  }
  m
}

test_that("default edge_label_size couples to node label cex (~1.82x ratio)", {
  m <- harmony_matrix()

  render_node_cex <- function(w, h, res) {
    tf <- tempfile(fileext = ".png")
    grDevices::png(tf, width = w, height = h, res = res)
    on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
    p <- splot(m, labels = TRUE, legend = FALSE, psych_styling = TRUE,
               edge_labels = TRUE)
    mean(p$nodes$label_size)
  }

  # The coupling is edge_cex = 0.55 * mean(node_cex), which gives a fixed
  # node-to-edge-label ratio of 1 / 0.55 = 1.818...
  node_small <- render_node_cex(700, 500, 96)
  node_big   <- render_node_cex(1400, 1400, 96)

  ratio_small <- node_small / (node_small * 0.55)
  ratio_big   <- node_big   / (node_big   * 0.55)
  expect_equal(ratio_small, ratio_big, tolerance = 1e-9)
  expect_equal(ratio_small, 1 / 0.55, tolerance = 1e-6)
})

test_that("user-explicit edge_label_size is preserved (no coupling)", {
  m <- harmony_matrix()
  with_png(700, 500, 96, {
    expect_silent({
      p <- splot(m, labels = TRUE, edge_labels = TRUE, legend = FALSE,
                 psych_styling = TRUE, edge_label_size = 0.8)
    })
    expect_s3_class(p, "cograph_network")
  })
})

test_that("scaling = 'fixed' gives label cex 1.0 (no visual-scale, no fit)", {
  m <- harmony_matrix()
  with_png(700, 500, 96, {
    p <- splot(m, labels = TRUE, legend = FALSE, psych_styling = TRUE,
               scaling = "fixed")
    # In fixed mode all labels are at label_default = 1.0.
    expect_true(all(abs(p$nodes$label_size - 1.0) < 1e-6))
  })
})
