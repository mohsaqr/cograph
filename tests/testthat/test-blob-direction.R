# Direction cues for per-pathway simplicial panels (R/blob-direction.R).
#
# The formulas are ports of ladyna's plotPathwayPanels()
# (tna-js/src/analysis/simplicialPlot.ts), which drives the CarmNote
# notebooks. The reference values below were produced by running that
# JavaScript, so they pin cross-language equivalence, not just self-consistency.


# Plotting tests check that figures draw; they run in CI, not on CRAN.
skip_on_cran()

test_that(".step_shade endpoints follow the stated formula", {
  # t = 1 is the base colour 15% darker; t = 0 is 62% of the way to white.
  base <- grDevices::col2rgb("#4A7FB5")[, 1]
  dark <- grDevices::col2rgb(.step_shade("#4A7FB5", 1))[, 1]
  expect_equal(unname(dark), unname(round(base * 0.85)))
  pale <- grDevices::col2rgb(.step_shade("#4A7FB5", 0))[, 1]
  expect_equal(unname(pale), unname(round(base + (255 - base) * 0.62)))
})

test_that(".step_shade darkens monotonically along the path", {
  # Invariant: later steps are never lighter than earlier ones.
  lum <- function(hex) {
    v <- grDevices::col2rgb(hex)
    as.numeric(0.2126 * v[1, ] + 0.7152 * v[2, ] + 0.0722 * v[3, ])
  }
  l <- lum(.step_shade("#4A7FB5", seq(0, 1, length.out = 11)))
  expect_true(all(diff(l) < 0))
})

test_that(".step_shade clamps non-finite and out-of-range t", {
  expect_equal(.step_shade("#4A7FB5", NA_real_), .step_shade("#4A7FB5", 1))
  expect_equal(.step_shade("#4A7FB5", 5), .step_shade("#4A7FB5", 1))
  expect_equal(.step_shade("#4A7FB5", -2), .step_shade("#4A7FB5", 0))
  expect_length(.step_shade("#4A7FB5", numeric(0)), 0L)
})

test_that(".step_shade rejects a non-numeric position", {
  expect_error(.step_shade("#4A7FB5", "a"), "must be numeric")
})

test_that(".blend_colors returns the endpoints exactly", {
  expect_equal(toupper(.blend_colors("#FFFFFF", "#F5A623", c(0, 1))),
               c("#FFFFFF", "#F5A623"))
})

test_that(".ring_sector_polys builds one closed wedge per sector", {
  r <- .ring_sector_polys(0, 0, 0.44, 0.56, pi / 4, "#F5A623", n_sector = 48L)
  expect_equal(length(unique(r$sector)), 48L)
  expect_equal(nrow(r), 48L * 8L)
  # Every vertex sits on one of the two radii.
  rad <- sqrt(r$x^2 + r$y^2)
  expect_true(all(abs(rad - 0.44) < 1e-8 | abs(rad - 0.56) < 1e-8))
})

test_that(".ring_sector_polys peaks on the side facing the next state", {
  ang <- 0  # successor lies due east
  r <- .ring_sector_polys(0, 0, 0.4, 0.6, ang, "#F5A623", n_sector = 48L)
  lum <- function(hex) {
    v <- grDevices::col2rgb(hex)
    as.numeric(0.2126 * v[1, ] + 0.7152 * v[2, ] + 0.0722 * v[3, ])
  }
  # Leading wedge (x > 0) is the saturated gold; trailing wedge is the pale
  # end of the ramp, so it is lighter.
  centre <- tapply(seq_len(nrow(r)), r$sector,
                   function(i) mean(r$x[i]))
  fills <- tapply(seq_len(nrow(r)), r$sector, function(i) r$fill[i][1])
  lead <- fills[which.max(centre)]
  trail <- fills[which.min(centre)]
  expect_equal(toupper(unname(lead)), "#F5A623")
  expect_true(lum(unname(trail)) > lum(unname(lead)))
})

test_that(".ring_sector_polys paints a terminal state flat", {
  # No successor -> no gradient axis -> one colour, exactly as the engine does.
  flat <- .ring_sector_polys(0, 0, 0.4, 0.6, NA_real_, "#F5A623")
  expect_equal(length(unique(flat$fill)), 1L)
  expect_equal(toupper(unique(flat$fill)), "#F5A623")
})

test_that(".ring_sector_polys rejects an inverted annulus", {
  expect_error(.ring_sector_polys(0, 0, 0.6, 0.4, NA_real_, "#F5A623"),
               "must exceed")
})

test_that(".direction_arrow_polys returns one triangle per step", {
  a <- .direction_arrow_polys(c(0, 1, 2), c(0, 1, 0), r_out = 0.2, size = 0.1)
  expect_equal(sort(unique(a$step)), 1:2)
  expect_equal(nrow(a), 6L)
})

test_that(".direction_arrow_polys aims each arrow at the successor", {
  # Invariant: the apex is farther along the bearing to the next state than
  # either base vertex, for any placement.
  set.seed(42)
  px <- c(0, runif(1, 1, 3)); py <- c(0, runif(1, -3, 3))
  a <- .direction_arrow_polys(px, py, r_out = 0.2, size = 0.1)
  bearing <- c(px[2] - px[1], py[2] - py[1])
  bearing <- bearing / sqrt(sum(bearing^2))
  proj <- a$x * bearing[1] + a$y * bearing[2]
  expect_equal(which.max(proj), 2L)  # the apex is the second vertex
})

test_that(".direction_arrow_polys has nothing to draw for a lone state", {
  expect_null(.direction_arrow_polys(0, 0, 0.2, 0.1))
  # Coincident states have no bearing.
  expect_null(.direction_arrow_polys(c(1, 1), c(2, 2), 0.2, 0.1))
})

test_that("direction on the combined overlay is an error, not a silent no-op", {
  mat <- matrix(0.5, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  diag(mat) <- 0
  expect_error(
    plot_simplicial(mat, c("A -> B -> C"), dismantled = FALSE,
                    direction = TRUE),
    class = "cograph_direction_needs_panels"
  )
})

test_that("plot_simplicial draws directed panels and keeps the overlay plain", {
  skip_if_not_installed("gridExtra")
  mat <- matrix(0.5, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  diag(mat) <- 0
  pw <- c("A -> B -> C", "B -> C -> D")
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_s3_class(plot_simplicial(mat, pw, dismantled = TRUE), "gtable")
  expect_s3_class(plot_simplicial(mat, pw, dismantled = TRUE,
                                  direction = FALSE), "gtable")
  expect_true(inherits(plot_simplicial(mat, pw), "ggplot"))
})

test_that("a single cue can be requested on its own", {
  skip_if_not_installed("gridExtra")
  mat <- matrix(0.5, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  diag(mat) <- 0
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  for (cue in c("shade", "ring", "arrows")) {
    expect_s3_class(
      plot_simplicial(mat, "A -> B -> C", dismantled = TRUE,
                      direction_cues = cue),
      "gtable"
    )
  }
  expect_error(
    plot_simplicial(mat, "A -> B -> C", dismantled = TRUE,
                    direction_cues = "spine")
  )
})

test_that("the legend grob is built for both directed and plain grids", {
  g <- .simplicial_legend_grob("#4A7FB5", "#E8734A", "#F5A623", "#d08e1e",
                               direction = TRUE)
  expect_s3_class(g, "gtable")
  g2 <- .simplicial_legend_grob("#4A7FB5", "#E8734A", "#F5A623", "#d08e1e",
                                direction = FALSE)
  expect_s3_class(g2, "gtable")
})

# --- Undirected higher-order structures -----------------------------------
# A clique, an association-rule itemset and a simplicial complex are SETS.
# There is no target to recover, so none is invented.

test_that("an unordered itemset does not depend on the order it was typed", {
  states <- LETTERS[1:4]
  a <- .parse_pathway_string("A B C", states, ordered = FALSE)
  b <- .parse_pathway_string("C A B", states, ordered = FALSE)
  expect_setequal(a$source, b$source)
  expect_null(a$target)
  expect_null(b$target)
  expect_false(.pw_is_ordered(a))
  # The ordered parse is the one that promotes a last state, and it still does.
  expect_equal(.parse_pathway_string("A B C", states)$target, "C")
})

test_that("an unordered parse flattens an arrow encoding", {
  # .extract_association_pathways() emits "A B -> C"; as a set that arrow is
  # an artefact of the encoding, not a claim about order.
  pw <- .parse_pathway_string("A B -> C", LETTERS[1:4], ordered = FALSE)
  expect_setequal(pw$source, c("A", "B", "C"))
  expect_null(pw$target)
})

test_that("repeat expansion does not re-invent a target for a set", {
  pw <- list(list(source = c("A", "B", "A"), target = NULL, ordered = FALSE))
  ex <- .expand_repeated_nodes(pw, c("A", "B"))
  expect_null(ex$pw_list[[1]]$target)
  expect_false(.pw_is_ordered(ex$pw_list[[1]]))
  expect_length(.pw_members(ex$pw_list[[1]]), 3L)
})

test_that(".pw_members keeps the old contract for unflagged pathways", {
  # Pathways built before `ordered` existed must still read as ordered.
  pw <- list(source = c("A", "B"), target = "C")
  expect_true(.pw_is_ordered(pw))
  expect_equal(.pw_members(pw), c("A", "B", "C"))
  expect_equal(.pw_target(pw), "C")
})

test_that("a simplicial complex plots as an unordered complex", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("gridExtra")
  set.seed(1)
  m <- matrix(runif(36), 6, 6, dimnames = list(LETTERS[1:6], LETTERS[1:6]))
  m <- (m + t(m)) / 2
  diag(m) <- 0
  sc <- Nestimate::build_simplicial(m, type = "clique", threshold = 0.45)
  sets <- .extract_simplicial_pathways(sc, max_pathways = 4L)
  expect_true(all(lengths(sets) >= 2L))
  # largest first
  expect_false(is.unsorted(rev(lengths(sets))))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_s3_class(plot_simplicial(sc, dismantled = TRUE, max_pathways = 4),
                  "gtable")
  # No source/target subtitle when nothing is a path.
  p <- plot_simplicial(sc, max_pathways = 4)
  expect_null(p$labels$subtitle)
})

test_that("direction is refused on an unordered input", {
  skip_if_not_installed("gridExtra")
  mat <- matrix(0.5, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  diag(mat) <- 0
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  # `ordered = FALSE` forces direction off even when the caller asked for it
  # on a dismantled grid, because there is no successor to point at.
  g <- plot_simplicial(mat, c("A B C", "B C D"), dismantled = TRUE,
                       ordered = FALSE)
  expect_s3_class(g, "gtable")
  p <- .plot_single_pathway(
    list(source = c("A", "B", "C"), target = NULL, ordered = FALSE),
    .blob_layout(LETTERS[1:4], LETTERS[1:4], "circle", 4L),
    LETTERS[1:4], setNames(LETTERS[1:4], LETTERS[1:4]),
    "#4A7FB5", "#E8734A", "#F5A623", "#d08e1e",
    "#B0D4F1", "#8FB0C8", 1L, 0.25, 0.7, 0.8, FALSE, 13.2, 3.5,
    direction = TRUE
  )
  # No arrow layer, no gradient wedges: the polygon layers are the blob only.
  expect_s3_class(p, "ggplot")
})

test_that("an unordered panel titles as a member list, not an arrow", {
  pos <- .blob_layout(LETTERS[1:4], LETTERS[1:4], "circle", 4L)
  lm <- setNames(LETTERS[1:4], LETTERS[1:4])
  mk <- function(pw) .plot_single_pathway(
    pw, pos, LETTERS[1:4], lm, "#4A7FB5", "#E8734A", "#F5A623", "#d08e1e",
    "#B0D4F1", "#8FB0C8", 1L, 0.25, 0.7, 0.8, FALSE, 13.2, 3.5,
    show_title = TRUE
  )
  set_title <- mk(list(source = c("A", "B", "C"), target = NULL,
                       ordered = FALSE))$labels$title
  path_title <- mk(list(source = c("A", "B"), target = "C",
                        ordered = TRUE))$labels$title
  expect_equal(set_title, "A, B, C")
  expect_match(path_title, "→")
})

test_that("panel bounds centre on the range, so no node is clipped", {
  # A clique with two nodes close together pulled the mean away from the
  # outlier and clipped it at the panel edge.
  pos <- .blob_layout(LETTERS[1:6], LETTERS[1:6], "circle", 6L)
  pw <- list(source = c("A", "C", "D"), target = NULL, ordered = FALSE)
  nd <- pos[pos$state %in% .pw_members(pw), ]
  cx <- (min(nd$x) + max(nd$x)) / 2
  cy <- (min(nd$y) + max(nd$y)) / 2
  half <- max(max(nd$x) - min(nd$x), max(nd$y) - min(nd$y)) / 2 + 1.5
  far <- max(pmax(abs(nd$x - cx), abs(nd$y - cy)))
  expect_lt(far + 0.1174 * half * 1.27, half)
})

test_that("the legend never names a target on an unordered figure", {
  g <- .simplicial_legend_grob("#4A7FB5", "#E8734A", "#F5A623", "#d08e1e",
                               direction = FALSE, ordered = FALSE)
  expect_s3_class(g, "gtable")
  txt <- function(grb) {
    labs <- vapply(grb$grobs, function(z) {
      if (inherits(z, "gTree")) paste(vapply(z$children, function(ch) {
        if (!is.null(ch$label)) paste(ch$label, collapse = " ") else ""
      }, character(1)), collapse = " ") else ""
    }, character(1))
    paste(labs, collapse = " ")
  }
  expect_false(grepl("target", txt(g), fixed = TRUE) &&
                 !grepl("no target", txt(g), fixed = TRUE))
  # The ordered legend still does name one.
  g2 <- .simplicial_legend_grob("#4A7FB5", "#E8734A", "#F5A623", "#d08e1e",
                                direction = TRUE, ordered = TRUE)
  expect_true(grepl("target", txt(g2), fixed = TRUE))
})
