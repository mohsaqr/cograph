# test-coverage-splot-edges-40.R - Comprehensive tests for splot-edges.R
# Tests for edge rendering functions in base R graphics

# Load internal functions for testing
find_curve_split_index <- cograph:::find_curve_split_index
draw_curve_with_start_segment <- cograph:::draw_curve_with_start_segment
draw_straight_edge_base <- cograph:::draw_straight_edge_base
draw_curved_edge_base <- cograph:::draw_curved_edge_base
draw_self_loop_base <- cograph:::draw_self_loop_base
draw_edge_label_base <- cograph:::draw_edge_label_base
get_edge_label_position <- cograph:::get_edge_label_position
render_edges_base <- cograph:::render_edges_base
splot_angle <- cograph:::splot_angle
cent_to_edge <- cograph:::cent_to_edge
arrow_base_midpoint <- cograph:::arrow_base_midpoint
draw_arrow_base <- cograph:::draw_arrow_base
recycle_to_length <- cograph:::recycle_to_length
get_edge_order <- cograph:::get_edge_order
resolve_loop_rotation <- cograph:::resolve_loop_rotation
adjust_alpha <- cograph:::adjust_alpha

# ============================================
# FIND_CURVE_SPLIT_INDEX TESTS
# ============================================

test_that("find_curve_split_index returns 1 for n < 2", {
  expect_equal(find_curve_split_index(c(0), c(0), 0.5), 1)
  expect_equal(find_curve_split_index(numeric(0), numeric(0), 0.5), 1)
})

test_that("find_curve_split_index returns 1 for fraction <= 0", {
  x <- c(0, 1, 2, 3)
  y <- c(0, 0, 0, 0)
  expect_equal(find_curve_split_index(x, y, 0), 1)
  expect_equal(find_curve_split_index(x, y, -0.5), 1)
})

test_that("find_curve_split_index returns n for fraction >= 1", {
  x <- c(0, 1, 2, 3)
  y <- c(0, 0, 0, 0)
  expect_equal(find_curve_split_index(x, y, 1), 4)
  expect_equal(find_curve_split_index(x, y, 1.5), 4)
})

test_that("find_curve_split_index returns correct midpoint for fraction = 0.5", {
  x <- c(0, 1, 2, 3, 4)
  y <- c(0, 0, 0, 0, 0)
  # Equal spacing: at fraction 0.5, should be around index 3
  idx <- find_curve_split_index(x, y, 0.5)
  expect_true(idx >= 2 && idx <= 4)
})

test_that("find_curve_split_index handles zero-length curves", {
  x <- c(1, 1, 1)
  y <- c(1, 1, 1)
  # Total length is 0, should return 1

expect_equal(find_curve_split_index(x, y, 0.5), 1)
})

test_that("find_curve_split_index ensures at least 2 points in each segment", {
  x <- c(0, 1, 2, 3, 4)
  y <- c(0, 0, 0, 0, 0)
  # Very small fraction should not return 1
  idx <- find_curve_split_index(x, y, 0.001)
  expect_true(idx >= 2)
  # Very large fraction should not return n
  idx2 <- find_curve_split_index(x, y, 0.999)
  expect_true(idx2 <= 4)
})

# ============================================
# DRAW_CURVE_WITH_START_SEGMENT TESTS
# ============================================

test_that("draw_curve_with_start_segment handles n < 2", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # Should return invisibly without error
  result <- draw_curve_with_start_segment(c(0), c(0), "black", 1, 1)
  dev.off()

  expect_null(result)
})
test_that("draw_curve_with_start_segment draws single line when no split needed", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # start_fraction = 0, so no split needed
  draw_curve_with_start_segment(c(-0.5, 0, 0.5), c(0, 0.3, 0),
                                col = "blue", lwd = 2, lty = 1,
                                start_lty = 2, start_fraction = 0)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curve_with_start_segment draws split line with different lty", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # start_fraction = 0.3 with different lty
  draw_curve_with_start_segment(c(-0.5, 0, 0.5), c(0, 0.3, 0),
                                col = "red", lwd = 2, lty = 1,
                                start_lty = 2, start_fraction = 0.3)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curve_with_start_segment handles same start_lty and lty", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # Same lty means no split
  draw_curve_with_start_segment(c(-0.5, 0, 0.5), c(0, 0.3, 0),
                                col = "green", lwd = 1, lty = 2,
                                start_lty = 2, start_fraction = 0.5)
  dev.off()

  expect_true(file.exists(tmp))
})

# ============================================
# DRAW_STRAIGHT_EDGE_BASE TESTS
# ============================================

test_that("draw_straight_edge_base draws a simple edge", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_straight_edge_base(x1 = -0.5, y1 = 0, x2 = 0.5, y2 = 0,
                          col = "gray50", lwd = 1, lty = 1,
                          arrow = FALSE, asize = 0.02)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_straight_edge_base draws edge with arrow", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_straight_edge_base(x1 = -0.5, y1 = -0.5, x2 = 0.5, y2 = 0.5,
                          col = "blue", lwd = 2, lty = 1,
                          arrow = TRUE, asize = 0.05)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_straight_edge_base draws bidirectional edge", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_straight_edge_base(x1 = -0.5, y1 = 0, x2 = 0.5, y2 = 0,
                          col = "red", lwd = 2, lty = 1,
                          arrow = TRUE, asize = 0.04,
                          bidirectional = TRUE)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_straight_edge_base handles different line types", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # Dashed line
  draw_straight_edge_base(-0.5, 0.3, 0.5, 0.3, col = "black", lwd = 1, lty = 2)
  # Dotted line
  draw_straight_edge_base(-0.5, 0, 0.5, 0, col = "black", lwd = 1, lty = 3)
  # Dash-dot line
  draw_straight_edge_base(-0.5, -0.3, 0.5, -0.3, col = "black", lwd = 1, lty = 4)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_straight_edge_base handles start_fraction with different lty", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_straight_edge_base(-0.5, 0, 0.5, 0,
                          col = "purple", lwd = 2, lty = 1,
                          arrow = FALSE,
                          start_lty = 2, start_fraction = 0.3)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_straight_edge_base handles custom arrow_angle", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # Wide arrow
  draw_straight_edge_base(-0.5, 0.3, 0.5, 0.3,
                          col = "orange", arrow = TRUE, asize = 0.05,
                          arrow_angle = pi/4)
  # Narrow arrow
  draw_straight_edge_base(-0.5, -0.3, 0.5, -0.3,
                          col = "orange", arrow = TRUE, asize = 0.05,
                          arrow_angle = pi/8)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_straight_edge_base handles zero arrow size", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # arrow = TRUE but asize = 0 means no arrow drawn
  draw_straight_edge_base(-0.5, 0, 0.5, 0,
                          col = "gray", arrow = TRUE, asize = 0)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_straight_edge_base handles vertical edges", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_straight_edge_base(0, -0.5, 0, 0.5,
                          col = "blue", arrow = TRUE, asize = 0.04)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_straight_edge_base handles diagonal edges", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_straight_edge_base(-0.5, -0.5, 0.5, 0.5,
                          col = "green", arrow = TRUE, asize = 0.04,
                          bidirectional = TRUE)
  dev.off()

  expect_true(file.exists(tmp))
})

# ============================================
# DRAW_CURVED_EDGE_BASE TESTS
# ============================================

test_that("draw_curved_edge_base falls back to straight for curve ~0", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # Very small curve should use straight edge
  draw_curved_edge_base(-0.5, 0, 0.5, 0, curve = 1e-8,
                        col = "gray", lwd = 1, arrow = FALSE)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curved_edge_base draws positive curve", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_curved_edge_base(-0.5, 0, 0.5, 0, curve = 0.3,
                        col = "blue", lwd = 2, arrow = TRUE, asize = 0.04)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curved_edge_base draws negative curve", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_curved_edge_base(-0.5, 0, 0.5, 0, curve = -0.3,
                        col = "red", lwd = 2, arrow = TRUE, asize = 0.04)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curved_edge_base draws bidirectional curved edge", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_curved_edge_base(-0.5, 0, 0.5, 0, curve = 0.4,
                        col = "purple", lwd = 2,
                        arrow = TRUE, asize = 0.04,
                        bidirectional = TRUE)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curved_edge_base handles custom curvePivot", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # Curve peak at 30%
  draw_curved_edge_base(-0.5, 0.3, 0.5, 0.3, curve = 0.3, curvePivot = 0.3,
                        col = "orange", lwd = 2)
  # Curve peak at 70%
  draw_curved_edge_base(-0.5, -0.3, 0.5, -0.3, curve = 0.3, curvePivot = 0.7,
                        col = "cyan", lwd = 2)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curved_edge_base handles zero-length edge", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # Same start and end point - should return invisibly
  draw_curved_edge_base(0, 0, 0, 0, curve = 0.3, col = "gray")
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curved_edge_base handles start_segment styling", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_curved_edge_base(-0.5, 0, 0.5, 0, curve = 0.3,
                        col = "green", lwd = 2, lty = 1,
                        start_lty = 2, start_fraction = 0.25)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curved_edge_base handles large curvature", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_curved_edge_base(-0.3, 0, 0.3, 0, curve = 0.8,
                        col = "maroon", lwd = 2, arrow = TRUE, asize = 0.04)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_curved_edge_base handles very small curvature", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_curved_edge_base(-0.5, 0, 0.5, 0, curve = 0.01,
                        col = "navy", lwd = 1, arrow = TRUE, asize = 0.03)
  dev.off()

  expect_true(file.exists(tmp))
})

# ============================================
# DRAW_SELF_LOOP_BASE TESTS
# ============================================

test_that("draw_self_loop_base draws basic loop", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_self_loop_base(0, 0, node_size = 0.1, col = "gray50", lwd = 1)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_self_loop_base draws loop with arrow", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_self_loop_base(0, 0, node_size = 0.1, col = "blue", lwd = 2,
                      arrow = TRUE, asize = 0.03)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_self_loop_base handles different rotations", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # Loop pointing up (default)
  draw_self_loop_base(-0.5, 0.5, 0.08, col = "red", rotation = pi/2)
  # Loop pointing right
  draw_self_loop_base(0.5, 0.5, 0.08, col = "green", rotation = 0)
  # Loop pointing down
  draw_self_loop_base(-0.5, -0.5, 0.08, col = "blue", rotation = -pi/2)
  # Loop pointing left
  draw_self_loop_base(0.5, -0.5, 0.08, col = "orange", rotation = pi)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_self_loop_base handles different line types", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_self_loop_base(-0.3, 0, 0.08, col = "black", lty = 2)  # dashed
  draw_self_loop_base(0.3, 0, 0.08, col = "black", lty = 3)   # dotted
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_self_loop_base handles no arrow", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_self_loop_base(0, 0, 0.1, col = "purple", arrow = FALSE)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_self_loop_base handles custom arrow_angle", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_self_loop_base(0, 0, 0.1, col = "darkgreen",
                      arrow = TRUE, asize = 0.04, arrow_angle = pi/4)
  dev.off()

  expect_true(file.exists(tmp))
})

# ============================================
# DRAW_EDGE_LABEL_BASE TESTS
# ============================================

test_that("draw_edge_label_base returns invisibly for NULL/NA/empty label", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  result1 <- draw_edge_label_base(0, 0, label = NULL)
  result2 <- draw_edge_label_base(0, 0, label = NA)
  result3 <- draw_edge_label_base(0, 0, label = "")
  dev.off()

  expect_null(result1)
  expect_null(result2)
  expect_null(result3)
})

test_that("draw_edge_label_base draws simple label", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_edge_label_base(0, 0, label = "0.75", cex = 1, col = "gray30")
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_edge_label_base draws label with background", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_edge_label_base(0, 0, label = "0.50", cex = 1,
                       col = "black", bg = "white")
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_edge_label_base draws label without background", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_edge_label_base(0, 0, label = "test", bg = NA)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_edge_label_base draws label with shadow", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_edge_label_base(0, 0, label = "shadow",
                       shadow = TRUE, shadow_color = "gray40",
                       shadow_offset = 0.5, shadow_alpha = 0.5)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_edge_label_base draws label with halo", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_edge_label_base(0, 0, label = "halo",
                       shadow = "halo", shadow_color = "white",
                       shadow_offset = 1, shadow_alpha = 1)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_edge_label_base handles different font sizes", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_edge_label_base(-0.3, 0, label = "small", cex = 0.5)
  draw_edge_label_base(0.3, 0, label = "large", cex = 1.5)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("draw_edge_label_base handles different fonts", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  draw_edge_label_base(-0.4, 0.3, label = "plain", font = 1)
  draw_edge_label_base(0, 0.3, label = "bold", font = 2)
  draw_edge_label_base(0.4, 0.3, label = "italic", font = 3)
  dev.off()

  expect_true(file.exists(tmp))
})

# ============================================
# GET_EDGE_LABEL_POSITION TESTS
# ============================================

test_that("get_edge_label_position returns midpoint for straight edge", {
  pos <- get_edge_label_position(-1, 0, 1, 0, position = 0.5, curve = 0)
  expect_equal(pos$x, 0, tolerance = 0.01)
  expect_equal(pos$y, 0, tolerance = 0.01)
})

test_that("get_edge_label_position returns start for position = 0", {
  pos <- get_edge_label_position(-1, 0, 1, 0, position = 0, curve = 0)
  expect_equal(pos$x, -1, tolerance = 0.01)
  expect_equal(pos$y, 0, tolerance = 0.01)
})

test_that("get_edge_label_position returns end for position = 1", {
  pos <- get_edge_label_position(-1, 0, 1, 0, position = 1, curve = 0)
  expect_equal(pos$x, 1, tolerance = 0.01)
  expect_equal(pos$y, 0, tolerance = 0.01)
})

test_that("get_edge_label_position applies label_offset for straight edge", {
  pos <- get_edge_label_position(-1, 0, 1, 0, position = 0.5,
                                  curve = 0, label_offset = 0.1)
  expect_equal(pos$x, 0, tolerance = 0.01)
  # Offset is perpendicular to edge
  expect_true(abs(pos$y) > 0)
})

test_that("get_edge_label_position handles curved edge", {
  pos <- get_edge_label_position(-1, 0, 1, 0, position = 0.5,
                                  curve = 0.3, curvePivot = 0.5)
  # Should be offset from the straight line due to curve
  expect_equal(pos$x, 0, tolerance = 0.1)
  expect_true(abs(pos$y) > 0)  # Curved away from y=0
})

test_that("get_edge_label_position handles different curvePivot", {
  pos1 <- get_edge_label_position(-1, 0, 1, 0, position = 0.3,
                                   curve = 0.3, curvePivot = 0.3)
  pos2 <- get_edge_label_position(-1, 0, 1, 0, position = 0.3,
                                   curve = 0.3, curvePivot = 0.7)
  # Different pivot should give different y positions
  expect_false(abs(pos1$y - pos2$y) < 0.001)
})

test_that("get_edge_label_position handles zero-length edge", {
  pos <- get_edge_label_position(0, 0, 0, 0, position = 0.5)
  expect_equal(pos$x, 0)
  expect_equal(pos$y, 0)
})

test_that("get_edge_label_position handles NA curve", {
  pos <- get_edge_label_position(-1, 0, 1, 0, position = 0.5, curve = NA)
  expect_equal(pos$x, 0, tolerance = 0.01)
  expect_equal(pos$y, 0, tolerance = 0.01)
})

test_that("get_edge_label_position handles empty curve", {
  pos <- get_edge_label_position(-1, 0, 1, 0, position = 0.5, curve = numeric(0))
  expect_equal(pos$x, 0, tolerance = 0.01)
  expect_equal(pos$y, 0, tolerance = 0.01)
})

# ============================================
# RENDER_EDGES_BASE TESTS
# ============================================

test_that("render_edges_base handles empty edge set", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  edges <- data.frame(from = integer(0), to = integer(0))
  layout <- matrix(c(-0.5, 0.5, 0, 0), ncol = 2)
  result <- render_edges_base(edges, layout, node_sizes = 0.1)
  dev.off()

  expect_null(result)
})

test_that("render_edges_base renders simple edges", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
  layout <- matrix(c(-0.5, 0, 0.5, -0.3, 0.3, 0), ncol = 2)
  render_edges_base(edges, layout, node_sizes = 0.1,
                    edge.color = "gray50", edge.width = 1)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("render_edges_base renders self-loop", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  edges <- data.frame(from = c(1, 1), to = c(2, 1), weight = c(0.5, 0.3))
  layout <- matrix(c(-0.3, 0.3, 0, 0), ncol = 2)
  render_edges_base(edges, layout, node_sizes = 0.1,
                    edge.color = "blue", arrows = TRUE, asize = 0.03)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("render_edges_base renders curved edges", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  edges <- data.frame(from = c(1, 2), to = c(2, 1), weight = c(0.5, 0.6))
  layout <- matrix(c(-0.3, 0.3, 0, 0), ncol = 2)
  render_edges_base(edges, layout, node_sizes = 0.1,
                    curve = c(0.3, 0.3), arrows = TRUE, asize = 0.03)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("render_edges_base renders edge labels", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
  layout <- matrix(c(-0.5, 0, 0.5, -0.3, 0.3, 0), ncol = 2)
  render_edges_base(edges, layout, node_sizes = 0.1,
                    edge.labels = c("0.5", "0.8"),
                    edge.label.cex = 0.8, edge.label.bg = "white")
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("render_edges_base handles bidirectional arrows", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  edges <- data.frame(from = c(1), to = c(2), weight = c(0.7))
  layout <- matrix(c(-0.3, 0.3, 0, 0), ncol = 2)
  render_edges_base(edges, layout, node_sizes = 0.1,
                    arrows = TRUE, asize = 0.04, bidirectional = TRUE)
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("render_edges_base respects edge ordering by weight", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  # Stronger edges should be rendered on top (last)
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 3, 3),
                      weight = c(0.2, 0.9, 0.5))
  layout <- matrix(c(-0.5, 0.5, 0, 0, 0.5, -0.5), ncol = 2)
  render_edges_base(edges, layout, node_sizes = 0.1,
                    edge.color = c("lightblue", "darkblue", "blue"),
                    edge.width = c(1, 3, 2))
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("render_edges_base handles different shapes", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.5))
  layout <- matrix(c(-0.5, 0, 0.5, -0.2, 0.2, 0), ncol = 2)
  render_edges_base(edges, layout, node_sizes = 0.1,
                    shapes = c("circle", "square", "triangle"))
  dev.off()

  expect_true(file.exists(tmp))
})

test_that("render_edges_base handles custom loop rotation", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
  edges <- data.frame(from = c(1, 1), to = c(1, 2), weight = c(0.5, 0.5))
  layout <- matrix(c(0, 0.5, 0, 0), ncol = 2)
  render_edges_base(edges, layout, node_sizes = 0.1,
                    loopRotation = c(pi/4, NA),  # 45 degrees for loop
                    arrows = TRUE, asize = 0.03)
  dev.off()

  expect_true(file.exists(tmp))
})

# ============================================
# INTEGRATION TESTS WITH SPLOT
# ============================================

test_that("splot renders edges correctly with various parameters", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, edge_color = "steelblue",
                            edge_width = 2, curvature = 0.2))
  expect_true(result$success, info = result$error)
})

test_that("splot renders self-loops correctly", {
  adj <- create_test_matrix(4)
  diag(adj) <- c(1, 0, 1, 0)  # Add self-loops to nodes 1 and 3

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot renders curved reciprocal edges correctly", {
  # Directed network with reciprocal edges
  edges <- data.frame(
    from = c(1, 2, 2, 3),
    to = c(2, 1, 3, 2),
    weight = c(0.5, 0.6, 0.7, 0.4)
  )

  result <- safe_plot(splot(edges, directed = TRUE, curves = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot renders edge labels correctly", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, edge_labels = TRUE,
                            edge_label_size = 0.7,
                            edge_label_bg = "lightyellow"))
  expect_true(result$success, info = result$error)
})

test_that("splot renders arrows correctly in directed network", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE,
                            show_arrows = TRUE, arrow_size = 1.5))
  expect_true(result$success, info = result$error)
})

test_that("splot handles per-edge curvature vector", {
  edges <- data.frame(
    from = c(1, 2, 3),
    to = c(2, 3, 1),
    weight = c(0.5, 0.6, 0.7)
  )

  result <- safe_plot(splot(edges, curvature = c(0, 0.3, 0.5)))
  expect_true(result$success, info = result$error)
})

test_that("splot handles edge_style parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, edge_style = 2))  # Dashed
  expect_true(result$success, info = result$error)
})

test_that("splot handles bidirectional arrows", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, bidirectional = TRUE, arrow_size = 1.2))
  expect_true(result$success, info = result$error)
})

test_that("splot handles edge_alpha transparency", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, edge_alpha = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("splot handles curves = 'force' mode", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, curves = "force", curvature = 0.25))
  expect_true(result$success, info = result$error)
})

test_that("splot handles edge_ci underlays", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_ci = rep(0.15, n_edges),
    edge_ci_scale = 2,
    edge_ci_alpha = 0.2
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot handles edge_label_halo", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, edge_labels = TRUE, edge_label_halo = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot handles loop_rotation parameter", {
  adj <- create_test_matrix(3)
  diag(adj) <- 1  # Add self-loops

  result <- safe_plot(splot(adj, loop_rotation = pi/4))
  expect_true(result$success, info = result$error)
})

test_that("splot handles edge rendering with threshold", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, threshold = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("splot handles negative and positive edge colors", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)

  result <- safe_plot(splot(adj,
    edge_positive_color = "#228B22",
    edge_negative_color = "#DC143C"
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot handles curve_pivot parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, curves = TRUE, curvature = 0.3,
                            curve_pivot = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("splot handles complete graph with all edges curved", {
  adj <- create_test_topology("complete", n = 5)

  result <- safe_plot(splot(adj, curves = "force", curvature = 0.2))
  expect_true(result$success, info = result$error)
})

test_that("splot handles star graph with arrows", {
  adj <- create_test_topology("star", n = 5)

  result <- safe_plot(splot(adj, directed = TRUE, show_arrows = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot handles ring topology", {
  adj <- create_test_topology("ring", n = 6)

  result <- safe_plot(splot(adj, layout = "circle", curvature = 0.1))
  expect_true(result$success, info = result$error)
})
