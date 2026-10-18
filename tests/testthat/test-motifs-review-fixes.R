# Regression tests for the 2026-10-18 review of the motif code and help
# pages (cograph 2.7.11). Each block pins one confirmed finding.

skip_on_cran()

.review_edges <- function(n_sessions = 30, len = 13, n_states = 5, seed = 3) {
  saved <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  }
  on.exit(if (!is.null(saved)) assign(".Random.seed", saved, envir = globalenv()),
          add = TRUE)
  set.seed(seed)
  rows <- lapply(seq_len(n_sessions), \(i) {
    s <- sample(LETTERS[seq_len(n_states)], len, replace = TRUE)
    data.frame(from = s[-len], to = s[-1], session = paste0("S", i))
  })
  do.call(rbind, rows)
}

# Fill colours passed to graphics::points() while `expr` draws pattern panels
.panel_fills <- function(expr) {
  fills <- new.env()
  fills$v <- character(0)
  trace("points", where = asNamespace("graphics"), print = FALSE,
        tracer = bquote({
          a <- list(...)
          assign("v", c(get("v", envir = .(fills)), as.character(a$bg[1])),
                 envir = .(fills))
        }))
  on.exit(untrace("points", where = asNamespace("graphics")), add = TRUE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
  fills$v
}

# ---- motifs() / subgraphs() --------------------------------------------------

test_that("window without an actor column is a classed error", {
  el <- .review_edges()
  expect_error(motifs(el[c("from", "to")], window = 20),
               class = "cograph_window_needs_actor")
  windowed <- suppressMessages(motifs(el, window = 5, significance = FALSE))
  expect_identical(windowed$level, "individual")
})

test_that("an empty individual-level instance result is NULL", {
  el <- .review_edges()
  expect_message(
    res <- subgraphs(el, min_count = 999, n_perm = 20, seed = 1),
    "No motifs with count >= 999"
  )
  expect_null(res)
})

test_that("as_cograph(<tna>) is analysed per sequence like the tna itself", {
  skip_if_not_installed("tna")
  model <- tna::tna(tna::group_regulation)
  direct <- motifs(model, significance = FALSE)
  via_network <- motifs(as_cograph(model), significance = FALSE)
  expect_identical(via_network$level, "individual")
  expect_identical(via_network$n_units, direct$n_units)
  expect_identical(as.data.frame(via_network, what = "types"),
                   as.data.frame(direct, what = "types"))
})

test_that("plot colours and stars follow p_adj, matching sig", {
  el <- .review_edges()
  cen <- motifs(el, pattern = "all", n_perm = 200, seed = 1,
                p_adjust = "bonferroni")
  d <- as.data.frame(cen)
  expect_gt(sum(d$p < 0.05), 0)   # raw p would colour some panels
  expect_identical(sum(d$sig), 0L)
  fills <- .panel_fills(plot(cen, type = "triads"))
  expect_identical(unique(fills), "#9E9E9E")
  g <- plot(cen, type = "significance")
  expect_identical(unique(ggplot2::ggplot_build(g)$data[[1]]$fill), "#9E9E9E")
  expect_match(g$scales$scales[[1]]$labels[["over"]], "adjusted p")
})

test_that("without p_adjust the colours are unchanged", {
  el <- .review_edges()
  cen <- motifs(el, pattern = "all", n_perm = 200, seed = 1)
  n_sig <- sum(as.data.frame(cen)$sig)
  expect_gt(n_sig, 0)
  fills <- .panel_fills(plot(cen, type = "triads", n = 16))
  expect_identical(sum(fills != "#9E9E9E"), n_sig)
})

test_that("significant = TRUE filters census panels and returns the input", {
  el <- .review_edges()
  cen <- motifs(el, pattern = "all", n_perm = 200, seed = 1)
  n_sig <- sum(as.data.frame(cen)$sig)
  fills <- .panel_fills(out <- plot(cen, type = "triads", significant = TRUE,
                                    n = 16))
  expect_length(fills, n_sig)
  expect_identical(out, cen)
})

test_that("ncol reaches the pattern grid, default four columns", {
  el <- .review_edges()
  cen <- motifs(el, pattern = "all", significance = FALSE)
  grid_of <- function(...) {
    seen <- new.env()
    trace(".plot_motif_patterns", where = asNamespace("cograph"),
          print = FALSE, tracer = bquote(assign("ncol", ncol, envir = .(seen))))
    on.exit(untrace(".plot_motif_patterns", where = asNamespace("cograph")),
            add = TRUE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    plot(cen, type = "patterns", ...)
    seen$ncol
  }
  expect_identical(grid_of(), 4L)
  expect_identical(grid_of(ncol = 2), 2)
})

test_that("include records a custom pattern and suppresses the default note", {
  el <- .review_edges()
  expect_no_message(
    res <- subgraphs(el, significance = FALSE, include = c("021C", "012"),
                     min_count = 1)
  )
  expect_identical(res$params$pattern, "custom")
  expect_setequal(names(res$type_summary), c("021C", "012"))
})

test_that("params$actor records the detected column", {
  el <- .review_edges()
  expect_identical(motifs(el, significance = FALSE)$params$actor, "session")
  expect_null(motifs(regulation_net, significance = FALSE,
                     min_transitions = 0)$params$actor)
})

test_that("edge lists match columns by case and read count as weight", {
  el <- .review_edges()
  upper <- el
  names(upper) <- c("From", "To", "Session")
  expect_identical(
    as.data.frame(motifs(upper, significance = FALSE), what = "types"),
    as.data.frame(motifs(el, significance = FALSE), what = "types")
  )
  weighted <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"),
                         count = 5)
  expect_identical(motifs(weighted, significance = FALSE)$n_units, 1L)
  expect_null(suppressMessages(
    motifs(transform(weighted, count = NULL), significance = FALSE)
  ))  # three unit weights fall below min_transitions = 5
  expect_error(motifs(data.frame(source = "A", target = "B")),
               class = "cograph_bad_edgelist")
})

test_that("edge_method = 'expected' handles a unit with no transitions", {
  el <- data.frame(from = c("A", "B", "C", "A", "B", "C", "D", "D"),
                   to = c("B", "C", "A", "B", "C", "A", "D", "D"),
                   session = c(rep("S1", 6), "S2", "S2"))
  res <- motifs(el, edge_method = "expected", min_transitions = 0,
                n_perm = 50, seed = 1)
  expect_identical(as.data.frame(res)$type, "030C")
  expect_identical(cograph:::.motif_expected_matrix(matrix(0, 2, 2)),
                   matrix(0.001, 2, 2))
})

# ---- triad_census() / motif_census() ---------------------------------------

test_that("triad_census returns a tidy table equal to igraph", {
  skip_if_not_installed("igraph")
  m <- matrix(c(0, 1, 0, 1,
                0, 0, 1, 0,
                1, 0, 0, 1,
                0, 1, 0, 0), 4, 4, byrow = TRUE)
  tc <- triad_census(m)
  expect_s3_class(tc, "data.frame")
  expect_identical(names(tc), c("type", "count"))
  expect_identical(nrow(tc), 16L)
  expect_equal(tc$count,
               as.numeric(igraph::triad_census(
                 igraph::graph_from_adjacency_matrix(m))))
  expect_equal(sum(tc$count), choose(4, 3))
})

test_that("a negative weight is no tie in any census", {
  skip_if_not_installed("igraph")
  signed <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  signed["a", "b"] <- -2
  signed["b", "c"] <- 3
  signed["c", "a"] <- 3
  present <- \(d, col) d[[col]][d$count > 0]
  expect_identical(present(triad_census(signed), "type"), "021C")
  expect_identical(present(as.data.frame(
    motif_census(signed, n_random = 5, seed = 1)), "motif"), "021C")
  expect_identical(as.data.frame(
    motifs(signed, pattern = "all", significance = FALSE,
           min_transitions = 0), what = "types")$type, "021C")
  # Unequal positive weights still make a directed network.
  w <- matrix(c(0, 2, 1, 0), 2, 2)
  expect_true(attr(motif_census(w, n_random = 2, seed = 1), "directed"))
})

test_that("motif_census size 4 leaves the uncounted classes NA", {
  skip_if_not_installed("igraph")
  two_pairs <- matrix(0, 8, 8)
  two_pairs[1, 2] <- two_pairs[2, 1] <- 1
  two_pairs[3, 4] <- two_pairs[4, 3] <- 1
  m4 <- motif_census(two_pairs, size = 4, n_random = 5, seed = 1)
  reference <- igraph::motifs(
    igraph::graph_from_adjacency_matrix(two_pairs, mode = "undirected"), 4)
  expect_identical(is.na(m4$count), is.na(reference))
  expect_true(all(is.na(m4$p_value[is.na(m4$count)])))
  expect_equal(m4$count[!is.na(m4$count)], reference[!is.na(reference)])
})

# ---- extract_motifs() wrapper ----------------------------------------------

test_that("extract_motifs equals subgraphs(strict = FALSE)", {
  el <- .review_edges()
  em <- extract_motifs(data = el, id = "session", significance = TRUE,
                       n_perm = 50, seed = 1)
  sg <- subgraphs(el, strict = FALSE, min_count = 1, pattern = "triangle",
                  min_transitions = 3, n_perm = 50, seed = 1)
  got <- as.data.frame(em)
  expect_equal(got, as.data.frame(sg)[names(got)], ignore_attr = TRUE)
  expect_type(got$sig, "logical")
})

test_that("extract_motifs keeps distinct id combinations apart", {
  el <- data.frame(from = c("A", "B", "C", "A", "B", "C"),
                   to = c("B", "C", "A", "B", "C", "A"),
                   g1 = rep(c("a_b", "a"), each = 3),
                   g2 = rep(c("c", "b_c"), each = 3))
  res <- extract_motifs(data = el, id = c("g1", "g2"), min_transitions = 0)
  expect_identical(as.data.frame(res)$observed, 2L)
})

test_that("extract_motifs input contract", {
  el <- .review_edges()
  expect_error(extract_motifs(regulation_net, data = el),
               class = "cograph_data_needs_id")
  expect_warning(
    res <- extract_motifs(regulation_net, min_transitions = 0,
                          significance = TRUE),
    class = "cograph_no_units"
  )
  expect_false("z" %in% names(as.data.frame(res)))
  expect_error(extract_motifs(regulation_net, min_transitions = 0,
                              significance = TRUE, n_perm = 1), "`n_perm`")
})

test_that("extract_motifs reads get_edge_list() weights", {
  skip_if_not_installed("tna")
  model <- tna::tna(tna::group_regulation)
  edges <- get_edge_list(model)
  via_tna <- extract_motifs(model, pattern = "all", edge_method = "percent",
                            edge_threshold = 0.2)
  via_edges <- extract_motifs(data = edges, id = "id", pattern = "all",
                              edge_method = "percent", edge_threshold = 0.2)
  expect_identical(as.data.frame(via_edges, what = "types"),
                   as.data.frame(via_tna, what = "types"))
})

test_that("extract_motifs orders by_type in MAN order and keeps top after it", {
  el <- .review_edges()
  res <- extract_motifs(data = el, id = "session", pattern = "all",
                        by_type = TRUE)
  types <- as.data.frame(res)$type
  man <- cograph:::.triad_type_names()
  expect_false(is.unsorted(match(types, man)))
  top3 <- as.data.frame(extract_motifs(data = el, id = "session",
                                       pattern = "all", by_type = TRUE,
                                       top = 3))
  expect_identical(top3$type, types[1:3])
  totals <- as.data.frame(res, what = "types")
  expect_equal(sum(totals$count), sum(as.data.frame(res)$observed))
})

test_that("extract_motifs significance plot sorts by |z| and keeps strong deficits", {
  el <- .review_edges()
  res <- extract_motifs(data = el, id = "session", pattern = "all",
                        significance = TRUE, n_perm = 100, seed = 1)
  z <- as.data.frame(res)$z
  expect_false(is.unsorted(rev(abs(z[!is.na(z)]))))
})

test_that("print of an extraction reports units analysed and rows shown", {
  skip_if_not_installed("tna")
  res <- extract_motifs(tna::tna(tna::group_regulation), top = 3)
  out <- capture.output(print(res))
  expect_true(any(grepl("Units: 1710 analysed of 2000", out, fixed = TRUE)))
  expect_true(any(grepl("Triads: 3 shown of 302", out, fixed = TRUE)))
})

test_that("extract_motifs pattern panels never carry per-triple significance", {
  el <- data.frame(from = c("A", "B", "A", "C", "D", "E"),
                   to = c("B", "C", "C", "D", "E", "C"),
                   session = rep(c("S1", "S2"), each = 3))
  res <- extract_motifs(data = el, id = "session", min_transitions = 0,
                        significance = TRUE, n_perm = 30, seed = 1)
  fills <- .panel_fills(plot(res, type = "patterns"))
  expect_identical(unique(fills), "#2166AC")
})
