# Verifies that every multi-panel plot function accepting `combined = TRUE`
# (default) sets up its own par(mfrow) grid and restores it, while
# `combined = FALSE` leaves par("mfrow") untouched so the caller's external
# layout (e.g. panel_layout()) survives.

skip_if_no_device <- function() {
  if (!capabilities("png") && !capabilities("cairo")) {
    skip("no graphics device available") # nocov
  }
}

# Helper: open a null PDF device, run code with par("mfrow") pre-set, and
# return the par state observed BEFORE the call and AFTER the call. The
# function under test is `fn`; if `combined = FALSE`, par("mfrow") must be
# identical before and after.
with_null_dev <- function(expr) {
  skip_if_no_device()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

# ---- plot_netobject_group ---------------------------------------------------

test_that("plot_netobject_group(combined = FALSE) preserves par(mfrow)", {
  mat <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")
  net1 <- as_cograph(mat)
  net2 <- as_cograph(mat * 0.5)
  net3 <- as_cograph(mat * 0.25)
  grp <- structure(list(G1 = net1, G2 = net2, G3 = net3),
                   class = c("netobject_group", "list"))

  with_null_dev({
    graphics::par(mfrow = c(1, 1))
    before <- graphics::par("mfrow")
    plot_netobject_group(grp, combined = FALSE)
    after <- graphics::par("mfrow")
    expect_identical(before, after)
  })

  # And combined = TRUE still works (default path, no regression)
  with_null_dev({
    expect_silent(plot_netobject_group(grp, combined = TRUE))
  })
})

# ---- plot_netobject_ml ------------------------------------------------------

test_that("plot_netobject_ml(combined = FALSE) preserves par(mfrow)", {
  mat <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")
  btw <- as_cograph(mat)
  wth <- as_cograph(mat * 0.6)
  ml <- structure(list(between = btw, within = wth),
                  class = c("netobject_ml", "list"))

  with_null_dev({
    graphics::par(mfrow = c(1, 1))
    before <- graphics::par("mfrow")
    plot_netobject_ml(ml, combined = FALSE)
    after <- graphics::par("mfrow")
    expect_identical(before, after)
  })

  with_null_dev({
    expect_silent(plot_netobject_ml(ml, combined = TRUE))
  })
})

# ---- plot_network_evolution -------------------------------------------------

test_that("plot_network_evolution(combined = FALSE) preserves par(mfrow)", {
  set.seed(1)
  edges <- data.frame(
    from = sample(LETTERS[1:5], 30, replace = TRUE),
    to   = sample(LETTERS[1:5], 30, replace = TRUE),
    week = sample(1:4, 30, replace = TRUE)
  )

  with_null_dev({
    graphics::par(mfrow = c(1, 1))
    before <- graphics::par("mfrow")
    plot_network_evolution(edges, time = "week", combined = FALSE)
    after <- graphics::par("mfrow")
    expect_identical(before, after)
  })

  with_null_dev({
    expect_silent(plot_network_evolution(edges, time = "week", combined = TRUE))
  })
})

# ---- splot() group cascade --------------------------------------------------

test_that("splot() group_tna cascade respects combined = FALSE", {
  mat1 <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
  mat2 <- mat1 * 0.5
  mat3 <- mat1 * 0.3
  colnames(mat1) <- rownames(mat1) <- c("A", "B", "C")
  colnames(mat2) <- rownames(mat2) <- c("A", "B", "C")
  colnames(mat3) <- rownames(mat3) <- c("A", "B", "C")

  # Minimal tna-shaped object: list with $weights and $labels, classed "tna"
  make_tna <- function(m) {
    obj <- list(weights = m, labels = colnames(m), inits = NULL, data = NULL)
    class(obj) <- c("tna", "list")
    obj
  }
  grp <- structure(
    list(g1 = make_tna(mat1), g2 = make_tna(mat2), g3 = make_tna(mat3)),
    class = c("group_tna", "list")
  )

  with_null_dev({
    graphics::par(mfrow = c(1, 1))
    before <- graphics::par("mfrow")
    splot(grp, combined = FALSE)
    after <- graphics::par("mfrow")
    expect_identical(before, after)
  })

  with_null_dev({
    expect_silent(splot(grp, combined = TRUE))
  })
})

# ---- plot.tna_disparity -----------------------------------------------------

test_that("plot.tna_disparity(type = 'comparison', combined = FALSE) preserves par(mfrow)", {
  mat <- matrix(c(0.0, 0.5, 0.1, 0.0,
                  0.3, 0.0, 0.4, 0.1,
                  0.1, 0.2, 0.0, 0.5,
                  0.0, 0.1, 0.3, 0.0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  disp <- disparity_filter(cograph(mat), level = 0.05)

  with_null_dev({
    graphics::par(mfrow = c(1, 1))
    before <- graphics::par("mfrow")
    plot(disp, type = "comparison", combined = FALSE)
    after <- graphics::par("mfrow")
    expect_identical(before, after)
  })

  with_null_dev({
    expect_silent(plot(disp, type = "comparison", combined = TRUE))
  })
})

# ---- panel_layout integration (the documented usage path) -------------------

test_that("panel_layout() + combined = FALSE composes correctly", {
  mat <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")
  net1 <- as_cograph(mat)
  net2 <- as_cograph(mat * 0.5)
  grp <- structure(list(G1 = net1, G2 = net2),
                   class = c("netobject_group", "list"))

  with_null_dev({
    graphics::par(mfrow = c(1, 1))
    op <- panel_layout(c(2, 1))
    expect_identical(graphics::par("mfrow"), c(2L, 1L))
    plot_netobject_group(grp, combined = FALSE)
    # combined=FALSE must NOT clobber the user's 2x1 layout
    expect_identical(graphics::par("mfrow"), c(2L, 1L))
    graphics::par(op)
    expect_identical(graphics::par("mfrow"), c(1L, 1L))
  })
})
