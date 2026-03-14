# Tests for plot_simplicial and pathway parsing

# ============================================
# Pathway parsing tests
# ============================================

test_that(".parse_pathway_string handles arrow separator", {
  states <- c("plan", "cohesion", "emotion", "discuss")
  p <- cograph:::.parse_pathway_string("plan, cohesion -> emotion", states)
  expect_equal(p$source, c("plan", "cohesion"))
  expect_equal(p$target, "emotion")
})

test_that(".parse_pathway_string handles unicode arrow", {
  states <- c("plan", "cohesion", "emotion")
  p <- cograph:::.parse_pathway_string("plan cohesion \u2192 emotion", states)
  expect_equal(p$source, c("plan", "cohesion"))
  expect_equal(p$target, "emotion")
})

test_that(".parse_pathway_string handles space separator", {
  states <- c("plan", "cohesion", "emotion")
  p <- cograph:::.parse_pathway_string("plan cohesion emotion", states)
  expect_equal(p$source, c("plan", "cohesion"))
  expect_equal(p$target, "emotion")
})

test_that(".parse_pathway_string handles dash separator", {
  states <- c("plan", "cohesion", "emotion")
  p <- cograph:::.parse_pathway_string("plan-cohesion-emotion", states)
  expect_equal(p$source, c("plan", "cohesion"))
  expect_equal(p$target, "emotion")
})

test_that(".parse_pathway_string handles comma separator", {
  states <- c("plan", "cohesion", "emotion")
  p <- cograph:::.parse_pathway_string("plan, cohesion, emotion", states)
  expect_equal(p$source, c("plan", "cohesion"))
  expect_equal(p$target, "emotion")
})

test_that(".parse_pathway_string handles space-dash separator", {
  states <- c("plan", "cohesion", "emotion")
  p <- cograph:::.parse_pathway_string("plan - cohesion - emotion", states)
  expect_equal(p$source, c("plan", "cohesion"))
  expect_equal(p$target, "emotion")
})

test_that(".split_state_tokens returns empty for empty string", {
  expect_equal(cograph:::.split_state_tokens(""), character(0))
  expect_equal(cograph:::.split_state_tokens("  "), character(0))
})

test_that(".parse_pathway_string is case insensitive with known states", {
  states <- c("plan", "cohesion", "emotion")
  p <- cograph:::.parse_pathway_string("Plan Cohesion -> Emotion", states)
  expect_equal(p$source, c("plan", "cohesion"))
  expect_equal(p$target, "emotion")
})

test_that(".parse_pathway_string works without known states", {
  p <- cograph:::.parse_pathway_string("A B -> C", NULL)
  expect_equal(p$source, c("A", "B"))
  expect_equal(p$target, "C")

  p2 <- cograph:::.parse_pathway_string("X Y Z", NULL)
  expect_equal(p2$source, c("X", "Y"))
  expect_equal(p2$target, "Z")
})

test_that(".parse_pathway_string rejects single-state input", {
  expect_error(
    cograph:::.parse_pathway_string("solo", NULL),
    "at least 2 states"
  )
})

test_that(".parse_pathway_string handles arrow with multiple source states", {
  states <- c("A", "B", "C", "D")
  p <- cograph:::.parse_pathway_string("A B C -> D", states)
  expect_equal(p$source, c("A", "B", "C"))
  expect_equal(p$target, "D")
})

test_that(".parse_pathways handles character vector", {
  states <- c("A", "B", "C", "D")
  pw <- cograph:::.parse_pathways(c("A B -> C", "B C -> D"), states)
  expect_length(pw, 2)
  expect_equal(pw[[1]]$target, "C")
  expect_equal(pw[[2]]$source, c("B", "C"))
})

test_that(".parse_pathways handles list format", {
  pw <- cograph:::.parse_pathways(
    list(c("A", "B", "C"), c("B", "C", "D")), NULL
  )
  expect_length(pw, 2)
  expect_equal(pw[[1]]$source, c("A", "B"))
  expect_equal(pw[[1]]$target, "C")
  expect_equal(pw[[2]]$source, c("B", "C"))
  expect_equal(pw[[2]]$target, "D")
})

test_that(".parse_pathways rejects invalid input", {
  expect_error(cograph:::.parse_pathways(42, NULL))
})

# ============================================
# Shared helper tests
# ============================================

test_that(".smooth_blob returns closed polygon", {
  blob <- cograph:::.smooth_blob(c(0, 1), c(0, 1))
  expect_s3_class(blob, "data.frame")
  expect_true(all(c("x", "y") %in% names(blob)))
  expect_equal(blob$x[1], blob$x[nrow(blob)])
  expect_equal(blob$y[1], blob$y[nrow(blob)])
})

test_that(".smooth_blob works with single point", {
  blob <- cograph:::.smooth_blob(5, 3)
  expect_s3_class(blob, "data.frame")
  expect_true(nrow(blob) > 10)
})

test_that(".darken_colors produces valid colors", {
  cols <- c("#B0D4F1", "#FF0000")
  dark <- cograph:::.darken_colors(cols, 0.2)
  expect_length(dark, 2)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", dark)))
  orig_rgb <- grDevices::col2rgb(cols[1])
  dark_rgb <- grDevices::col2rgb(dark[1])
  expect_true(all(dark_rgb <= orig_rgb))
})

test_that(".blob_layout circle produces correct dimensions", {
  pos <- cograph:::.blob_layout(
    c("A", "B", "C"), c("A", "B", "C"), "circle", 3
  )
  expect_equal(nrow(pos), 3)
  expect_true(all(c("x", "y", "label", "state") %in% names(pos)))
  radii <- sqrt(pos$x^2 + pos$y^2)
  expect_true(all(abs(radii - 5.5) < 0.01))
})

test_that(".blob_layout accepts custom coordinates", {
  coords <- matrix(c(0, 1, 2, 0, 1, 0), ncol = 2)
  pos <- cograph:::.blob_layout(
    c("A", "B", "C"), c("A", "B", "C"), coords, 3
  )
  expect_equal(pos$x, c(0, 1, 2))
  expect_equal(pos$y, c(0, 1, 0))
})

test_that(".blob_layout rejects invalid layout", {
  expect_error(
    cograph:::.blob_layout(c("A", "B"), c("A", "B"), "grid", 2),
    "circle"
  )
})

test_that(".extract_blob_states works with matrix", {
  mat <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  expect_equal(cograph:::.extract_blob_states(mat), c("A", "B", "C"))
})

test_that(".extract_blob_states works with unnamed matrix", {
  mat <- matrix(0, 3, 3)
  expect_equal(cograph:::.extract_blob_states(mat), c("S1", "S2", "S3"))
})

test_that(".extract_blob_states returns NULL for NULL", {
  expect_null(cograph:::.extract_blob_states(NULL))
})

test_that(".extract_blob_states errors on invalid input", {
  expect_error(cograph:::.extract_blob_states("bad"), "tna object")
})

test_that(".extract_blob_states works with tna object", {
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)
  states <- cograph:::.extract_blob_states(model)
  expect_true(length(states) > 0)
  expect_true(is.character(states))
})

test_that(".extract_blob_states works with igraph object", {
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- c("A", "B", "C", "D")
  expect_equal(cograph:::.extract_blob_states(g), c("A", "B", "C", "D"))
})

test_that(".extract_blob_states works with igraph without names", {
  g <- igraph::make_ring(3)
  expect_equal(cograph:::.extract_blob_states(g), c("S1", "S2", "S3"))
})

test_that(".extract_blob_states works with cograph_network object", {
  mat <- matrix(runif(9), 3, 3, dimnames = list(c("X", "Y", "Z"), c("X", "Y", "Z")))
  diag(mat) <- 0
  net <- as_cograph(mat)
  expect_equal(cograph:::.extract_blob_states(net), c("X", "Y", "Z"))
})

# ============================================
# plot_simplicial integration tests
# ============================================

test_that("plot_simplicial works with matrix + character pathways", {
  mat <- matrix(runif(16), 4, 4,
                dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  diag(mat) <- 0

  expect_no_error(with_temp_png(
    plot_simplicial(mat, c("A B -> C", "B C -> D"))
  ))
})

test_that("plot_simplicial works with list pathways", {
  mat <- matrix(runif(16), 4, 4,
                dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  diag(mat) <- 0

  expect_no_error(with_temp_png(
    plot_simplicial(mat, list(c("A", "B", "C"), c("B", "C", "D")))
  ))
})

test_that("plot_simplicial works without network (states inferred)", {
  expect_no_error(with_temp_png(
    plot_simplicial(pathways = c("A B C", "B C D"))
  ))
})

test_that("plot_simplicial combined returns ggplot invisibly", {
  result <- with_temp_png(
    plot_simplicial(pathways = c("A B C", "B C D"))
  )
  expect_s3_class(result, "ggplot")
})

test_that("plot_simplicial dismantled returns list of ggplots", {
  result <- with_temp_png(
    plot_simplicial(pathways = c("A B C", "B C D"), dismantled = TRUE)
  )
  expect_true(is.list(result))
  expect_length(result, 2)
  expect_s3_class(result[[1]], "ggplot")
  expect_s3_class(result[[2]], "ggplot")
})

test_that("plot_simplicial returns NULL with message for empty pathways", {
  expect_message(
    result <- plot_simplicial(pathways = character(0)),
    "No pathways"
  )
  expect_null(result)
})

test_that("plot_simplicial respects custom colors", {
  expect_no_error(with_temp_png(
    plot_simplicial(
      pathways = c("A B C"),
      node_color = "#FF0000",
      target_color = "#00FF00",
      ring_color = "#0000FF",
      blob_colors = "#FFFF00",
      blob_alpha = 0.5
    )
  ))
})

test_that("plot_simplicial respects shadow = FALSE", {
  expect_no_error(with_temp_png(
    plot_simplicial(pathways = c("A B C"), shadow = FALSE)
  ))
})

test_that("plot_simplicial respects custom title", {
  p <- with_temp_png(
    plot_simplicial(pathways = c("A B C"), title = "My Title")
  )
  expect_equal(p$labels$title, "My Title")
})

test_that("plot_simplicial works with custom layout coordinates", {
  coords <- matrix(c(0, 1, 2, 0, 1, 0), ncol = 2)
  rownames(coords) <- c("A", "B", "C")

  expect_no_error(with_temp_png(
    plot_simplicial(pathways = c("A B C"), layout = coords)
  ))
})

test_that("plot_simplicial works with tna object", {
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)

  expect_no_error(with_temp_png(
    plot_simplicial(
      model, c("plan, cohesion -> emotion", "discuss, consensus -> plan")
    )
  ))
})

test_that("plot_simplicial dismantled works with tna object", {
  skip_if_no_tna()
  model <- tna::tna(tna::group_regulation)

  result <- with_temp_png(
    plot_simplicial(
      model, c("plan cohesion emotion", "discuss consensus plan"),
      dismantled = TRUE
    )
  )
  expect_true(is.list(result))
  expect_length(result, 2)
})

test_that("plot_simplicial rejects invalid x", {
  expect_error(
    plot_simplicial("not_a_matrix", c("A B C")),
    "tna object"
  )
})

test_that("plot_simplicial works with matrix without rownames", {
  mat <- matrix(runif(9), 3, 3)
  expect_no_error(with_temp_png(
    plot_simplicial(mat, c("S1 S2 S3"))
  ))
})

test_that("plot_simplicial handles various separator styles in one call", {
  states <- c("A", "B", "C", "D", "E")
  mat <- matrix(0.1, 5, 5, dimnames = list(states, states))

  expect_no_error(with_temp_png(
    plot_simplicial(mat, c("A, B -> C", "C-D-E", "A B E"))
  ))
})

test_that("plot_simplicial custom labels are used", {
  mat <- matrix(0.1, 3, 3,
                dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  p <- with_temp_png(
    plot_simplicial(mat, c("a b c"), labels = c("Alpha", "Beta", "Gamma"))
  )
  expect_s3_class(p, "ggplot")
})

test_that("blob_colors are recycled when fewer than pathways", {
  expect_no_error(with_temp_png(
    plot_simplicial(
      pathways = c("A B C", "B C D", "A C D"),
      blob_colors = c("#FF0000", "#00FF00")
    )
  ))
})

test_that("plot_simplicial respects blob_linewidth and blob_line_alpha", {
  expect_no_error(with_temp_png(
    plot_simplicial(
      pathways = c("A B C", "B C D"),
      blob_linewidth = 1.5,
      blob_line_alpha = 0.3
    )
  ))
})

test_that("plot_simplicial respects blob_linetype", {
  expect_no_error(with_temp_png(
    plot_simplicial(
      pathways = c("A B C", "B C D"),
      blob_linetype = c("dotted", "longdash")
    )
  ))
})
