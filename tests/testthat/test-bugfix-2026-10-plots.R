# Regression tests for the plotting and theme bugs fixed in 2026-10
# (bugs 28 and 30 to 36 of the help-page revision list).

# --- bug 28: plot_trajectories() with NA -----------------------------------

test_that("bug 28: plot_trajectories() draws sequences with missing states", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    Baseline = c("Light", "Light", "Intense", "Resource"),
    Week4    = c("Light", NA, "Intense", "Light"),
    Week8    = c("Resource", "Intense", NA, "Light"))

  p <- plot_trajectories(df)
  expect_s3_class(p, "ggplot")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))

  lines_df <- p$layers[[1]]$data
  segments <- unique(lines_df[, c("individual", "segment")])
  # Individual 2 is missing at Week4, so it has no segment at all;
  # individual 3 is missing at Week8, so it has only the first segment.
  expect_false(2 %in% segments$individual)
  expect_identical(sort(segments$segment[segments$individual == 3]), 1L)
  expect_identical(nrow(segments), 5L)
  expect_false(anyNA(lines_df$from_state))
  expect_false(anyNA(lines_df$to_state))

  # Node sizes count the observed states only (3 observed at Week4).
  rects <- p$layers[[2]]$data
  expect_false(anyNA(rects$label))
  expect_identical(sum(rects$total[rects$col == 2]), 3)
})

test_that("bug 28: 'first' colors use the first observed state", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  df <- data.frame(T1 = c(NA, "A", "B"), T2 = c("B", "A", "B"),
                   T3 = c("A", "B", "A"))
  p <- plot_trajectories(df, flow_color_by = "first")
  lines_df <- p$layers[[1]]$data
  expect_identical(unique(lines_df$first_state[lines_df$individual == 1]), "B")
  expect_false(anyNA(lines_df$line_color))
})

# --- bug 30: plot_network_evolution() ---------------------------------------

evolution_edges <- function() {
  data.frame(from = c("A", "B", "C", "A", "D", "B", "C", "E"),
             to   = c("B", "C", "A", "D", "E", "E", "D", "A"),
             week = c(1, 1, 2, 2, 3, 3, 4, 4))
}

test_that("bug 30: slices with cumulative = TRUE grows the network", {
  skip_on_cran()
  skip_if_not_installed("igraph")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  edges <- evolution_edges()
  nets <- expect_no_warning(
    plot_network_evolution(edges, time = "week", slices = 2, cumulative = TRUE))
  # Two equal-width bins of weeks 1-4: weeks 1-2 (4 edges), weeks 3-4 (4).
  expect_identical(vapply(nets, nrow, integer(1)), c(4L, 8L))
  nets_split <- plot_network_evolution(edges, time = "week", slices = 2)
  expect_identical(vapply(nets_split, nrow, integer(1)), c(4L, 4L))
})

test_that("bug 30: a coordinate matrix is accepted as layout", {
  skip_on_cran()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  edges <- evolution_edges()
  lay <- matrix(c(0, 1, 2, 1, 0, 0, 1, 0, -1, -1), ncol = 2,
                dimnames = list(c("E", "D", "C", "B", "A"), NULL))
  nets <- plot_network_evolution(edges, time = "week", layout = lay)
  expect_length(nets, 4L)

  coords <- cograph:::.evolution_layout_coords(lay, c("A", "B", "C", "D", "E"))
  expect_identical(rownames(coords), c("A", "B", "C", "D", "E"))
  expect_identical(unname(coords["A", ]), c(0, -1))

  expect_error(
    plot_network_evolution(edges, time = "week", layout = matrix(0, 3, 2)),
    class = "cograph_bad_parameter")
  expect_error(cograph:::.evolution_layout_coords(1:5, LETTERS[1:5]),
               class = "cograph_bad_parameter")
})

# --- bug 31: plot_heatmap() single colour on a diverging scale --------------

test_that("bug 31: one color name on a diverging scale gives no NA end", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  m <- matrix(c(0, -0.5, 0.8, 0.3, 0, -0.2, 0.6, 0.1, 0), 3,
              dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  p <- plot_heatmap(m, colors = "red")
  fills <- ggplot2::ggplot_build(p)$data[[1]]$fill
  values <- p$data$value
  # Before the fix, the high end was NA, so positive cells were transparent
  # and zero cells took the (red) mid color.
  expect_false(anyNA(fills))
  expect_false(any(grepl("00$", fills) & nchar(fills) == 9L))
  expect_identical(unique(fills[values == max(values)]), "#FF0000")
  expect_identical(unique(fills[values == 0]), "#F7F7F7")

  expect_identical(cograph:::.diverging_colors("red", c("white", "red")),
                   c("#2166AC", "#F7F7F7", "red"))
  expect_identical(cograph:::.diverging_colors(c("navy", "red"), c("navy", "red")),
                   c("navy", "white", "red"))
  expect_identical(cograph:::.diverging_colors("diverging",
                                               c("#2166AC", "#F7F7F7", "#B2182B")),
                   c("#2166AC", "#F7F7F7", "#B2182B"))
})

test_that("bug 31: the highest value maps to the chosen color", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  m <- matrix(c(0, -1, 1, 0), 2, dimnames = list(c("A", "B"), c("A", "B")))
  p <- plot_heatmap(m, colors = "red")
  b <- ggplot2::ggplot_build(p)$data[[1]]
  pd <- p$data
  expect_identical(nrow(b), nrow(pd))
  # gradient2 maps the maximum of the data to `high`, the minimum to `low`
  expect_true("#FF0000" %in% b$fill)
  expect_true("#2166AC" %in% b$fill)
  expect_true("#F7F7F7" %in% b$fill)
})

# --- plot_mcml() mode: plotted weights do not depend on mode ----------------

test_that("plot_mcml: both modes plot row-normalized weights on directed input", {
  skip_on_cran()
  clusters <- list(C1 = c("Explore", "Reflect", "Discuss"),
                   C2 = c("Plan", "Create", "Share"),
                   C3 = c("Monitor", "Adapt", "Synthesize", "Evaluate"))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  w <- plot_mcml(regulation_net, clusters, mode = "weights", directed = TRUE)
  t <- plot_mcml(regulation_net, clusters, mode = "tna", directed = TRUE)

  # Block sums, then row normalization (csum type = "tna").
  raw <- outer(names(clusters), names(clusters), Vectorize(\(a, b)
    sum(regulation_net[clusters[[a]], clusters[[b]]])))
  expect_equal(unclass(w$macro$weights), raw / rowSums(raw), ignore_attr = TRUE)
  expect_equal(unclass(t$macro$weights), unclass(w$macro$weights))
  expect_identical(c(w$meta$type, t$meta$type), c("tna", "tna"))
})

# --- bug 33: plot_ml_heatmap() node labels ----------------------------------

test_that("bug 33: each plane shows its own node names", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  clusters <- list(Plan = c("Explore", "Plan", "Monitor", "Adapt", "Reflect"),
                   Act = c("Discuss", "Synthesize", "Evaluate", "Create", "Share"))
  p <- plot_ml_heatmap(regulation_net, layer_list = clusters)
  labs <- p$layers[[3]]$data
  cols <- labs$label[labs$angle == 45]
  rows <- labs$label[labs$angle == 0]
  # Columns below the front plane are those of the front (last) layer.
  expect_identical(cols, clusters$Act)
  # Every layer's row names are drawn.
  expect_setequal(rows, unlist(clusters, use.names = FALSE))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))
})

test_that("bug 33: shared node names are drawn once", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  m1 <- matrix(1:9, 3, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  p <- plot_ml_heatmap(list(L1 = m1, L2 = m1 * 2))
  labs <- p$layers[[3]]$data
  expect_identical(labs$label, c("a", "b", "c", "a", "b", "c"))
  expect_true(cograph:::.ml_layers_share_names(list(m1, m1)))
  m2 <- m1
  dimnames(m2) <- list(c("x", "y", "z"), c("x", "y", "z"))
  expect_false(cograph:::.ml_layers_share_names(list(m1, m2)))
})

# --- bug 34: layout_oval(order = <labels>) ----------------------------------

test_that("bug 34: label order works for cograph_network input", {
  m <- matrix(1, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  diag(m) <- 0
  by_index <- layout_oval(cograph(m), order = c(3, 1, 2))
  by_label <- expect_no_warning(layout_oval(cograph(m), order = c("C", "A", "B")))
  by_r6 <- layout_oval(CographNetwork$new(m), order = c("C", "A", "B"))
  expect_equal(by_label, by_index)
  expect_equal(by_label, by_r6)
  expect_false(isTRUE(all.equal(by_label, layout_oval(cograph(m)))))
  expect_warning(layout_oval(cograph(m), order = c("C", "A", "Z")),
                 "Some labels not found")
})

# --- bug 35: sn_layout() with an invalid type --------------------------------

test_that("bug 35: sn_layout() names the accepted layouts", {
  m <- matrix(c(0, 1, 1, 0), 2)
  net <- cograph(m)
  expect_error(sn_layout(net, "nonsense"), class = "cograph_bad_parameter")
  err <- tryCatch(sn_layout(net, "nonsense"), error = identity)
  expect_match(conditionMessage(err), "Unknown layout type")
  expect_match(conditionMessage(err), "circle")
  expect_match(conditionMessage(err), "kk")
  expect_error(sn_layout(m, "nonsense"), class = "cograph_bad_parameter")
  expect_error(sn_layout(net, c("circle", "spring")), class = "cograph_bad_parameter")
  expect_error(sn_layout(net, NULL), class = "cograph_bad_parameter")
  expect_error(sn_layout(net, 3), class = "cograph_bad_parameter")
  # Valid inputs still work.
  expect_s3_class(sn_layout(net, "circle"), "cograph_network")
  expect_s3_class(sn_layout(net, matrix(c(0, 1, 0, 1), 2)), "cograph_network")
})

# --- bug 36: CographTheme clone and merge ----------------------------------

test_that("bug 36: clone_theme() and merge() keep parameters added by $set()", {
  th <- CographTheme$new(name = "mine", edge_color = "blue")
  th$set("foo", 1)
  cl <- th$clone_theme()
  expect_identical(cl$get("foo"), 1)
  expect_identical(cl$name, "mine")
  expect_identical(cl$get_all(), th$get_all())

  mg <- th$merge(list(edge_color = "red", bar = "x"))
  expect_identical(mg$get("foo"), 1)
  expect_identical(mg$get("bar"), "x")
  expect_identical(mg$get("edge_color"), "red")
  expect_identical(mg$name, "merged")

  other <- CographTheme$new()
  other$set("baz", TRUE)
  mg2 <- th$merge(other)
  expect_identical(mg2$get("baz"), TRUE)
  expect_identical(mg2$get("foo"), 1)
})
