# Visual-regression harness for the cograph plot_* family.
#
# Renders a registry of plotters across a grid of (width x height x res x
# pointsize x renderer), writes PNGs + a manifest.json under
#   ./tmp/visual-sweep/<ISO-timestamp>/
# and copies the HTML contact sheet next to them. A Playwright script in
# tools/playwright-check.mjs visits the contact sheet to screenshot or
# verify ratios.
#
# Usage (expected from the repo root):
#
#   source("tools/visual-harness.R")
#   run_visual_sweep(mode = "quick")   # ~12 images, ~15s
#   run_visual_sweep(mode = "full")    # ~1200 images, ~10 min
#
# This file is `source()`-d rather than exported. It is a dev tool.
# Its only side effect is writing to ./tmp/visual-sweep/.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("devtools required to load_all() the package — install it first.")
}
suppressWarnings(suppressMessages(devtools::load_all(".", quiet = TRUE)))

.have_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)
.have_ragg <- requireNamespace("ragg", quietly = TRUE)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

#' Build fixtures used by every plotter. Deterministic under `seed`.
#' Fixtures that are expensive to compute (bootstrap, disparity) are cached
#' to tests/testthat/fixtures/visual-sweep/ on first use.
#' @keywords internal
visual_sweep_fixtures <- function(seed = 42) {
  set.seed(seed)
  n <- 8L
  adj <- matrix(stats::runif(n * n, -1, 1), n, n)
  diag(adj) <- 0
  adj[abs(adj) < 0.4] <- 0
  rownames(adj) <- colnames(adj) <- paste0("N", seq_len(n))

  # Directed weighted (TNA-ish)
  tna_like <- abs(adj)
  tna_like <- tna_like / pmax(rowSums(tna_like), 1e-9)

  list(
    adj = adj,
    tna_like = tna_like,
    n = n
  )
}

# ---------------------------------------------------------------------------
# Plot registry
# ---------------------------------------------------------------------------

#' Registry of named plotters. Each entry is a function `(fx)` that receives
#' the fixture list and returns the plotter's return value (may be NULL).
#' Device open/close is handled by the harness; plotters just draw.
#' @keywords internal
.plot_registry <- list(
  splot_basic = function(fx) {
    cograph::splot(fx$adj, labels = TRUE, legend = TRUE,
                   legend_edge_colors = TRUE, title = "splot_basic")
  },
  splot_weighted = function(fx) {
    cograph::splot(fx$tna_like, labels = TRUE, directed = TRUE,
                   edge_labels = TRUE,
                   legend = TRUE, legend_edge_colors = TRUE,
                   title = "splot_weighted")
  },
  splot_fixed = function(fx) {
    # Escape-hatch mode: no device compensation. Used to A/B against the
    # adaptive mode in the contact sheet.
    cograph::splot(fx$adj, labels = TRUE, legend = TRUE,
                   legend_edge_colors = TRUE, scaling = "fixed",
                   title = "splot_fixed")
  },
  splot_large = function(fx) {
    # A denser, bigger network — surfaces label-over-edge overlap issues.
    set.seed(7)
    m <- matrix(stats::runif(400), 20, 20); diag(m) <- 0; m[m < 0.65] <- 0
    rownames(m) <- colnames(m) <- paste0("x", seq_len(20))
    cograph::splot(m, labels = TRUE, legend = TRUE,
                   legend_edge_colors = TRUE, title = "splot_large")
  }
)

# ---------------------------------------------------------------------------
# Grid & sweep driver
# ---------------------------------------------------------------------------

.sweep_grid <- function(mode = c("full", "quick"),
                        plot_fns = NULL,
                        resolutions = NULL,
                        sizes = NULL,
                        pointsizes = NULL,
                        renderers = NULL) {
  mode <- match.arg(mode)
  quick <- identical(mode, "quick")

  plot_fns <- plot_fns %||% if (quick) {
    c("splot_basic", "splot_fixed")
  } else {
    names(.plot_registry)
  }
  resolutions <- resolutions %||% if (quick) c(96, 300) else c(72, 96, 150, 300)
  sizes <- sizes %||% if (quick) {
    # list of (w, h, units) triples — units = "px" uses pixel-default, "in"
    # forces inches so the DPI-only axis is exercised at 7x5".
    list(c(400, 400, 0), c(1600, 1600, 0))
  } else {
    list(
      c(400, 400, 0),
      c(800, 800, 0),
      c(1600, 1600, 0),
      c(2400, 1800, 0),
      # Inches-default — stays 7x7 regardless of res so DPI-only axis moves.
      c(7L, 7L, 1L)
    )
  }
  pointsizes <- pointsizes %||% if (quick) 12 else c(8, 12, 16)
  renderers <- renderers %||% {
    r <- "grDevices"
    if (.have_ragg) r <- c(r, "ragg")
    r
  }

  # expand.grid with list-valued sizes handled via index.
  size_idx <- seq_along(sizes)
  g <- expand.grid(
    plot_fn = plot_fns,
    res = resolutions,
    size_i = size_idx,
    pointsize = pointsizes,
    renderer = renderers,
    stringsAsFactors = FALSE
  )
  g$width <- vapply(g$size_i, function(i) sizes[[i]][1], numeric(1))
  g$height <- vapply(g$size_i, function(i) sizes[[i]][2], numeric(1))
  g$units <- vapply(g$size_i, function(i) {
    if (sizes[[i]][3] == 1L) "in" else "px"
  }, character(1))
  g$size_i <- NULL
  g
}

.device_open <- function(file, renderer, width, height, res, pointsize, units) {
  if (identical(renderer, "ragg")) {
    ragg::agg_png(file, width = width, height = height, res = res,
                  pointsize = pointsize,
                  units = if (identical(units, "in")) "in" else "px")
  } else {
    grDevices::png(file, width = width, height = height, res = res,
                   pointsize = pointsize, units = units)
  }
}

.sweep_one <- function(cell, outdir, fx, ref_cell) {
  plot_fn <- cell$plot_fn
  renderer <- cell$renderer

  fn <- .plot_registry[[plot_fn]]
  if (is.null(fn)) {
    return(list(status = "error", plot_fn = plot_fn,
                message = sprintf("Unknown plot_fn: %s", plot_fn)))
  }

  # Human-readable file name
  fname <- sprintf("%s__w%d_h%d_r%d_p%d_%s_%s.png",
                   plot_fn, as.integer(cell$width), as.integer(cell$height),
                   as.integer(cell$res), as.integer(cell$pointsize),
                   cell$units, renderer)
  fdir <- file.path(outdir, "png", plot_fn)
  if (!dir.exists(fdir)) dir.create(fdir, recursive = TRUE)
  fpath <- file.path(fdir, fname)

  t0 <- Sys.time()
  out <- tryCatch({
    .device_open(fpath, renderer, cell$width, cell$height, cell$res,
                 cell$pointsize, cell$units)
    on.exit({
      if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    }, add = TRUE)
    fn(fx)
  },
  error = function(e) structure(list(error = conditionMessage(e)),
                                 class = "sweep_error"))
  elapsed_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000

  if (inherits(out, "sweep_error")) {
    return(list(
      status = "error",
      plot_fn = plot_fn, renderer = renderer,
      width = cell$width, height = cell$height, res = cell$res,
      pointsize = cell$pointsize, units = cell$units,
      file = file.path("png", plot_fn, fname),
      message = out$error,
      elapsed_ms = elapsed_ms
    ))
  }

  # Extract geometry where available. cograph attaches these attributes when
  # the plotter delegates to splot(). Some plots (ggplot paths) will lack
  # them; we record NA in that case.
  node_diam_in <- attr(out, "cograph.node_diam_in") %||% NA_real_
  vs <- attr(out, "cograph.visual_scale")

  # Label height in inches: median label cex * pointsize / 72. Captures how
  # the fix flows through to actual pixel-size of text; node_diam_in changes
  # only with canvas, so (label_h / node_diam) is the perceptual ratio we
  # care about.
  label_cex <- if (!is.null(out$nodes) && "label_size" %in% names(out$nodes)) {
    stats::median(out$nodes$label_size, na.rm = TRUE)
  } else NA_real_
  label_h_in <- if (is.finite(label_cex)) {
    label_cex * (cell$pointsize / 72)
  } else NA_real_

  list(
    status = "ok",
    plot_fn = plot_fn, renderer = renderer,
    width = cell$width, height = cell$height, res = cell$res,
    pointsize = cell$pointsize, units = cell$units,
    file = file.path("png", plot_fn, fname),
    node_diam_in = node_diam_in,
    label_cex = label_cex,
    label_h_in = label_h_in,
    vs_text = vs$text %||% NA_real_,
    vs_line = vs$line %||% NA_real_,
    vs_point = vs$point %||% NA_real_,
    vs_raw = vs$raw %||% NA_real_,
    canvas_in_geomean = if (!is.null(vs$canvas))
      sqrt(vs$canvas[1] * vs$canvas[2]) else NA_real_,
    elapsed_ms = elapsed_ms
  )
}

# ---------------------------------------------------------------------------
# Public entry
# ---------------------------------------------------------------------------

#' Run a visual sweep, writing PNGs + manifest + HTML contact sheet under
#' `./tmp/visual-sweep/<ISO-timestamp>/`. Invisibly returns the sweep dir
#' path.
#'
#' @param mode `"quick"` (minimal set, ~15s) or `"full"` (~10 min).
#' @param plot_fns,resolutions,sizes,pointsizes,renderers Override the
#'   mode defaults to pin a specific grid.
#' @param outdir Override the output directory root (defaults to
#'   `./tmp/visual-sweep`).
#' @param seed Fixture seed for reproducibility.
#' @keywords internal
run_visual_sweep <- function(mode = c("full", "quick"),
                             plot_fns = NULL,
                             resolutions = NULL,
                             sizes = NULL,
                             pointsizes = NULL,
                             renderers = NULL,
                             outdir = NULL,
                             seed = 42) {
  if (!.have_jsonlite) {
    stop("jsonlite required — install.packages('jsonlite')")
  }
  mode <- match.arg(mode)

  ts <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
  outroot <- outdir %||% file.path(getwd(), "tmp", "visual-sweep")
  sweep_dir <- file.path(outroot, ts)
  dir.create(sweep_dir, recursive = TRUE, showWarnings = FALSE)

  fx <- visual_sweep_fixtures(seed = seed)
  g <- .sweep_grid(mode = mode, plot_fns = plot_fns,
                   resolutions = resolutions, sizes = sizes,
                   pointsizes = pointsizes, renderers = renderers)

  # Reference cell per plot_fn: w=800,h=800,res=96,ps=12,grDevices,px
  ref_cell <- list(width = 800, height = 800, res = 96,
                    pointsize = 12, renderer = "grDevices", units = "px")

  cat(sprintf("[visual-sweep] mode=%s rows=%d outdir=%s\n",
              mode, nrow(g), sweep_dir))

  rows <- mapply(function(i) {
    cell <- as.list(g[i, , drop = FALSE])
    .sweep_one(cell, sweep_dir, fx, ref_cell)
  }, seq_len(nrow(g)), SIMPLIFY = FALSE)

  # Compute ratio deltas against per-plot_fn reference cell.
  rows <- .attach_ratio_deltas(rows)

  git_sha <- tryCatch(
    trimws(system2("git", c("rev-parse", "HEAD"), stdout = TRUE,
                   stderr = FALSE)),
    error = function(e) NA_character_
  )

  manifest <- list(
    timestamp = ts,
    git_sha = git_sha,
    r_version = R.version.string,
    ragg_available = .have_ragg,
    mode = mode,
    reference_cell = ref_cell,
    n_rows = length(rows),
    rows = rows
  )
  jsonlite::write_json(
    manifest,
    file.path(sweep_dir, "manifest.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"
  )

  # Copy HTML template. Try several likely locations because this file can be
  # source()-d, sys.source()-d, or called from a different cwd by the gated
  # testthat smoke test.
  tpl_candidates <- c(
    Sys.getenv("COGRAPH_VISUAL_TEMPLATE", unset = NA),
    file.path("tools", "visual-harness-template.html"),
    file.path(getwd(), "tools", "visual-harness-template.html"),
    # When source()'d directly with the `chdir = TRUE` convention:
    "visual-harness-template.html"
  )
  tpl_candidates <- tpl_candidates[!is.na(tpl_candidates)]
  tpl <- Find(file.exists, tpl_candidates)
  if (!is.null(tpl)) {
    file.copy(tpl, file.path(sweep_dir, "index.html"), overwrite = TRUE)
  } else {
    message("[visual-sweep] HTML template not found in: ",
            paste(tpl_candidates, collapse = ", "),
            " — PNGs + manifest.json were still written.")
  }

  n_ok <- sum(vapply(rows, function(r) identical(r$status, "ok"), logical(1)))
  n_err <- length(rows) - n_ok
  cat(sprintf("[visual-sweep] done. ok=%d err=%d\n  %s/index.html\n",
              n_ok, n_err, sweep_dir))

  invisible(sweep_dir)
}

# ---------------------------------------------------------------------------
# Ratio deltas
# ---------------------------------------------------------------------------

.attach_ratio_deltas <- function(rows) {
  # Group by plot_fn, find the reference row (w=800,h=800,res=96,p=12,px,
  # grDevices), and attach `ratio_delta_pct` on every row: percent deviation
  # of (canvas / node_diam_in) ratio from the reference.
  ok <- vapply(rows, function(r) identical(r$status, "ok"), logical(1))
  plot_fns <- unique(vapply(rows, function(r) r$plot_fn %||% "", character(1)))

  for (pf in plot_fns) {
    idx_pf <- which(vapply(rows, function(r) identical(r$plot_fn, pf), logical(1)))
    ref_i <- NA_integer_
    # Preferred reference: 800x800 @ 96dpi, ps=12, px, grDevices.
    pref_match <- function(r) {
      identical(r$renderer, "grDevices") &&
        identical(as.integer(r$width), 800L) &&
        identical(as.integer(r$height), 800L) &&
        identical(as.integer(r$res), 96L) &&
        identical(as.integer(r$pointsize), 12L) &&
        identical(r$units, "px")
    }
    for (i in idx_pf) {
      if (!ok[i]) next
      if (pref_match(rows[[i]])) { ref_i <- i; break }
    }
    # Fallback: first successful row for this plot_fn.
    if (is.na(ref_i)) {
      for (i in idx_pf) {
        if (ok[i]) { ref_i <- i; break }
      }
    }
    if (is.na(ref_i)) {
      for (i in idx_pf) rows[[i]]$ratio_delta_pct <- NA_real_
      next
    }
    ref <- rows[[ref_i]]
    # Perceptual ratio: label-height / node-diameter, both in inches. Stable
    # under the fix (vs$text tracks canvas), drifts hard under scaling="fixed"
    # because label stays 12pt while node grows with canvas.
    ratio_of <- function(r) {
      if (is.finite(r$label_h_in) && is.finite(r$node_diam_in) &&
          r$node_diam_in > 0) {
        r$label_h_in / r$node_diam_in
      } else NA_real_
    }
    ref_ratio <- ratio_of(ref)
    for (i in idx_pf) {
      r <- rows[[i]]
      if (!ok[i]) { rows[[i]]$ratio_delta_pct <- NA_real_; next }
      ratio <- ratio_of(r)
      rows[[i]]$ratio_delta_pct <- if (is.finite(ratio) && is.finite(ref_ratio) &&
                                       ref_ratio > 0) {
        (ratio - ref_ratio) / ref_ratio * 100
      } else {
        NA_real_
      }
      rows[[i]]$ratio_ok <- is.finite(rows[[i]]$ratio_delta_pct) &&
        abs(rows[[i]]$ratio_delta_pct) <= 5
    }
  }
  rows
}
