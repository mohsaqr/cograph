#' @title Visual Scale Helper
#' @description Device-dependent scaling multipliers for cex/pt.cex/lwd, so that
#'   base-R plotters in the splot() family produce visually consistent ratios
#'   (label-to-node, legend-to-plot, edge-to-node) across different output
#'   devices (PNG at high DPI, large canvas, RStudio plot pane resizes).
#' @name visual-scale
#' @keywords internal
NULL

# ============================================================================
# Scale computation
# ============================================================================

#' Compute device-dependent visual scale
#'
#' Reads the active graphics device's plot-region inches and returns a set of
#' multipliers that plotters apply to text/point/line sizes so visual ratios
#' stay stable when the device canvas changes.
#'
#' At the reference canvas (about 5.9 geometric-mean inches, matching the
#' default RStudio plot pane), every multiplier is 1.0 and behaviour is
#' identical to pre-fix cograph. At larger canvases (high-DPI PNG with pixel
#' defaults where inches grow, or large explicit inches) multipliers exceed 1
#' so labels/lines scale up with nodes. At smaller canvases they shrink.
#' Multipliers are clamped to `cap` to keep thumbnails and posters legible.
#'
#' Must be called after `plot.new()` (or inside a draw) so `par("pin")` is
#' populated. Falls back to `grDevices::dev.size("in")` if `par("pin")` is not
#' yet available.
#'
#' @param reference Numeric scalar. Reference canvas size in inches
#'   (geometric mean of pin). Default 5.9 matches a typical 7x5" RStudio pane.
#' @param cap Numeric length-2 vector `c(min, max)` bounding every multiplier.
#'   Default `c(0.55, 1.9)`.
#' @return A list with named numeric multipliers and diagnostic fields:
#'   `text`, `point`, `line`, `box`, `scale`, `raw`, `canvas`, `pin`, `cra`,
#'   `ref`.
#' @keywords internal
#' @noRd
compute_visual_scale <- function(reference = NULL, cap = NULL) {
  reference <- reference %||% VISUAL_SCALE_REFERENCE
  cap <- cap %||% VISUAL_SCALE_CAP
  stopifnot(
    is.numeric(reference), length(reference) == 1L, reference > 0,
    is.numeric(cap), length(cap) == 2L, cap[1] > 0, cap[2] >= cap[1]
  )

  # Anchor to `par("pin")` (plot region), NOT `dev.size("in")` (canvas).
  # The architectural reason: cograph nodes are drawn in user-coordinates
  # scaled to pin — they physically grow/shrink with the plot region, not
  # the canvas. Margins consume a fixed number of lines (not a percentage
  # of canvas), so at small canvases pin is a much smaller fraction of the
  # canvas than at large canvases. Using dev.size makes the multiplier
  # lag behind the actual node geometry, which is why labels drift away
  # from nodes at small-canvas-high-DPI cases like 800x800@300.
  #
  # dev.size is still captured for diagnostics but is only a fallback when
  # pin is not yet populated (pre-plot.new).
  pc <- tryCatch(graphics::par(c("pin", "cra")), error = function(e) NULL)
  pin <- if (is.null(pc)) c(NA_real_, NA_real_) else pc$pin
  cra <- if (is.null(pc)) c(NA_real_, NA_real_) else pc$cra

  ds <- tryCatch(grDevices::dev.size("in"), error = function(e) NULL)
  canvas_in <- if (is.null(ds) || any(!is.finite(ds)) || any(ds <= 0)) {
    c(7, 5)
  } else {
    ds
  }

  effective_in <- if (!is.null(pin) && all(is.finite(pin)) && all(pin > 0)) {
    sqrt(pin[1] * pin[2])
  } else {
    # Pre-plot.new() fallback: approximate pin as canvas minus typical
    # margin consumption. Caller should recompute after plot.new anyway.
    sqrt(canvas_in[1] * canvas_in[2]) * 0.85
  }
  raw <- effective_in / reference

  clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)
  # ONE multiplier applied uniformly to text, point, line, box. Research
  # into qgraph, ggraph, and igraph converged on this: separating text /
  # point / line multipliers (as Phase 1 did) breaks the invariant that
  # locks the label-to-node and legend-to-plot ratios by construction.
  # A single `scale` means every visual element moves in lockstep, so
  # ratios survive any canvas change.
  scale_mult <- clamp(raw, cap[1], cap[2])

  list(
    # All four fields carry the same multiplier for callers that still
    # differentiate between them. Kept as named fields for back-compat
    # with callers and tests.
    text = scale_mult,
    point = scale_mult,
    line = scale_mult,
    box = scale_mult,
    scale = scale_mult,
    raw = raw,
    canvas = canvas_in,
    pin = pin,
    cra = cra,
    ref = reference
  )
}

# ============================================================================
# Per-draw stash (so inner helpers don't re-read par())
# ============================================================================

#' Stash a visual scale for the duration of one draw.
#'
#' Inner helpers (edge-label background boxes, shared legend) call
#' `.get_current_visual_scale()` to retrieve the active scale without paying
#' another `par()` read or having it threaded through every argument list.
#'
#' Callers must pair `.set_current_visual_scale(vs)` with
#' `on.exit(.clear_current_visual_scale(), add = TRUE)`.
#'
#' @keywords internal
#' @noRd
.set_current_visual_scale <- function(vs) {
  assign("current_visual_scale", vs, envir = .cograph_env)
  invisible(vs)
}

#' @keywords internal
#' @noRd
.clear_current_visual_scale <- function() {
  if (exists("current_visual_scale", envir = .cograph_env, inherits = FALSE)) {
    rm("current_visual_scale", envir = .cograph_env)
  }
  invisible(NULL)
}

#' Retrieve the current draw's visual scale, or a neutral identity scale.
#'
#' Returns the stash if set; otherwise computes from the live device. If that
#' also fails (e.g. no device open), returns an identity scale so callers can
#' always multiply without a NULL check.
#'
#' @keywords internal
#' @noRd
.get_current_visual_scale <- function() {
  if (exists("current_visual_scale", envir = .cograph_env, inherits = FALSE)) {
    return(get("current_visual_scale", envir = .cograph_env, inherits = FALSE))
  }
  tryCatch(compute_visual_scale(), error = function(e) .identity_visual_scale())
}

#' Identity scale — all multipliers are 1. Used when device state is unusable
#' or when `scaling = "fixed"` disables compensation.
#' Canvas is still reported so diagnostics (sweep manifests, pixel-ratio
#' overlays) can compute ratios even in fixed mode.
#' @keywords internal
#' @noRd
.identity_visual_scale <- function() {
  ds <- tryCatch(grDevices::dev.size("in"), error = function(e) NULL)
  canvas <- if (is.null(ds) || any(!is.finite(ds)) || any(ds <= 0)) {
    c(NA_real_, NA_real_)
  } else {
    ds
  }
  list(
    text = 1, point = 1, line = 1, box = 1,
    scale = 1,
    raw = 1, canvas = canvas,
    pin = c(NA_real_, NA_real_), cra = c(NA_real_, NA_real_),
    ref = VISUAL_SCALE_REFERENCE
  )
}

#' Apply a visual-scale multiplier to `x`, returning `x` unchanged if `vs` is
#' NULL or the field is missing. Never divides by zero.
#' @keywords internal
#' @noRd
.scale_by <- function(x, vs, field = "text") {
  if (is.null(vs) || is.null(vs[[field]]) || !is.finite(vs[[field]])) {
    return(x)
  }
  x * vs[[field]]
}

#' Resolve scaling mode to a visual-scale object.
#'
#' @param scaling Either `"default"` / `"legacy"` / `"visual"` (device-aware,
#'   current behaviour) or `"fixed"` (reproducibility mode — identity scale).
#'   Also accepts `NULL` (treated as default/visual).
#' @keywords internal
#' @noRd
.resolve_visual_scale <- function(scaling = "default") {
  opt_default <- getOption("cograph.visual_scaling", TRUE)
  disabled <- identical(scaling, "fixed") || !isTRUE(opt_default)
  if (disabled) {
    return(.identity_visual_scale())
  }
  compute_visual_scale()
}
