#' @title Shared Legend Renderer
#' @description Shared internal helper for base-R legends that opt into
#'   device-scale compensation (`visual_scale`). Used by core base-R plotters
#'   to avoid per-plotter size drift; some legacy direct `graphics::legend()`
#'   calls remain in older helpers.
#' @name render-legend-shared
#' @keywords internal
#' @noRd
NULL

#' Render a base-R legend with device-aware size compensation.
#'
#' A thin wrapper around `graphics::legend()` that multiplies cex / pt.cex /
#' lwd by the corresponding visual-scale multiplier before delegating. Accepts
#' positions as a keyword string (`"topright"`, etc.) or a numeric `c(x, y)`
#' in user coordinates.
#'
#' @param legend Character vector of entry labels.
#' @param col Per-entry colors.
#' @param pch Per-entry plotting symbols.
#' @param lty Per-entry line types.
#' @param lwd Per-entry line widths. Multiplied by `visual_scale$line`.
#' @param pt.cex Per-entry point cex. Multiplied by `visual_scale$point`.
#' @param pt.bg Per-entry point background colors.
#' @param position Either a keyword string (`"topright"`) or a numeric
#'   `c(x, y)` in user coordinates.
#' @param cex Base text cex, multiplied by `visual_scale$text`.
#' @param bty,bg,seg.len,title,horiz,ncol,inset,xjust,yjust
#'   Standard `graphics::legend` arguments, forwarded verbatim.
#' @param visual_scale Visual-scale list from `compute_visual_scale()`. If
#'   NULL, `.get_current_visual_scale()` is consulted; if there is none,
#'   identity (no compensation) is used.
#' @param ... Additional arguments forwarded to `graphics::legend()`.
#' @return The list returned by `graphics::legend()` (bbox of the drawn
#'   legend), invisibly.
#' @keywords internal
#' @noRd
.render_legend_base <- function(legend,
                                col = NULL, pch = NULL, lty = NULL,
                                lwd = NULL, pt.cex = NULL, pt.bg = NULL,
                                position = "topright",
                                cex = 1,
                                bty = "o", bg = "white", seg.len = 1.5,
                                title = NULL,
                                xjust = NULL, yjust = NULL,
                                horiz = FALSE, ncol = 1, inset = 0,
                                visual_scale = NULL,
                                ...) {
  if (length(legend) == 0L) return(invisible(NULL))

  vs <- visual_scale %||% .get_current_visual_scale()
  t_mult <- if (is.null(vs) || !is.finite(vs$text %||% NA_real_)) 1 else vs$text
  p_mult <- if (is.null(vs) || !is.finite(vs$point %||% NA_real_)) 1 else vs$point
  l_mult <- if (is.null(vs) || !is.finite(vs$line %||% NA_real_)) 1 else vs$line

  args <- list(
    legend = legend,
    col = col,
    pch = pch,
    lty = lty,
    lwd = if (is.null(lwd)) NULL else lwd * l_mult,
    pt.cex = if (is.null(pt.cex)) NULL else pt.cex * p_mult,
    pt.bg = pt.bg,
    bty = bty,
    bg = bg,
    cex = cex * t_mult,
    seg.len = seg.len,
    title = title,
    horiz = horiz,
    ncol = ncol,
    inset = inset
  )

  if (is.numeric(position) && length(position) >= 2L) {
    args$x <- position[1]
    args$y <- position[2]
    if (!is.null(xjust)) args$xjust <- xjust
    if (!is.null(yjust)) args$yjust <- yjust
  } else {
    args$x <- position
  }

  # Drop NULLs (graphics::legend treats NULL and missing-as-default differently
  # for some arguments; safer to omit).
  args <- args[!vapply(args, is.null, logical(1))]

  extras <- list(...)
  if (length(extras) > 0L) {
    args[names(extras)] <- extras
  }

  invisible(do.call(graphics::legend, args))
}

#' Return `TRUE` if a legend with the given content would overflow the current
#' plot region at the active visual scale — callers can expand margins before
#' drawing.
#'
#' Implemented by asking `graphics::legend(..., plot = FALSE)` for its bbox
#' and comparing against `par("usr")`.
#'
#' @keywords internal
#' @noRd
.legend_overflows <- function(..., position = "topright", visual_scale = NULL) {
  meta <- tryCatch(
    .render_legend_base(..., position = position, visual_scale = visual_scale,
                        plot = FALSE),
    error = function(e) NULL
  )
  if (is.null(meta) || is.null(meta$rect)) return(FALSE)
  usr <- graphics::par("usr")
  rect <- meta$rect
  rect$left < usr[1] || rect$left + rect$w > usr[2] ||
    rect$top - rect$h < usr[3] || rect$top > usr[4]
}

#' Margin, in lines, that a legend needs as a band outside the plot region.
#'
#' The band has to be reserved before anything is drawn, so this works in
#' inches, which need an open device but no plot: `strwidth(units = "inches")`
#' and `par("cin")` are both valid before `plot.new()`. It is an estimate of
#' `graphics::legend()`'s own geometry, deliberately a little generous;
#' [.render_legend_in_band()] measures the real box afterwards and shrinks it
#' if the estimate fell short, so a wrong guess costs size, never an overlap.
#'
#' @param legend Character vector of legend entries.
#' @param side One of `"bottom"`, `"top"`, `"left"`, `"right"`.
#' @param cex Text expansion the legend will be drawn with.
#' @param title Legend title, or `NULL`.
#' @param horiz Entries on one row?
#' @param ncol Number of columns when `horiz = FALSE`.
#' @param visual_scale Visual scale list, or `NULL` for the current device's.
#' @return A single number of margin lines.
#' @keywords internal
#' @noRd
.legend_band_lines <- function(legend, side, cex = 1, title = NULL,
                               horiz = FALSE, ncol = 1, visual_scale = NULL) {
  # [.render_legend_base()] multiplies cex by the draw's text scale, which
  # grows with the device, so the band has to grow with it. Before a plot
  # exists the scale is computed from the device size, a slight over-estimate
  # of the final (plot-region) value: generous, which is the safe direction.
  vs <- visual_scale %||% .get_current_visual_scale()
  text_scale <- vs$text %||% NA_real_
  if (is.finite(text_scale)) cex <- cex * text_scale
  cin <- graphics::par("cin")
  rows <- if (isTRUE(horiz)) 1L else ceiling(length(legend) / max(1L, ncol))
  inches <- if (side %in% c("top", "bottom")) {
    # entry rows + title row + legend()'s own top/bottom padding. `!` binds
    # looser than `+`, so the title row is computed on its own.
    title_rows <- if (is.null(title)) 0L else 1L
    (rows + title_rows + 1.5) * cex * cin[2L]
  } else {
    cols <- if (isTRUE(horiz)) length(legend) else max(1L, ncol)
    text_in <- max(graphics::strwidth(c(legend, title), units = "inches",
                                      cex = cex))
    # per column: the symbol, its gap and the inter-column padding
    cols * (text_in + 4 * cex * cin[1L])
  }
  inches / graphics::par("csi") + 0.5
}

#' Draw a legend inside the margin band on one side of the current plot.
#'
#' `legend("bottom", inset = -0.05)` cannot do this: `inset` is a fraction of
#' the plot region, so it moves the box by a sliver of its own height and the
#' rest stays on top of the plot. Here the box is measured first
#' (`plot = FALSE`), scaled down if it is larger than the band between the
#' plot region and the edge of the figure, and then anchored by coordinates at
#' the centre of that band. The legend therefore never covers the plot and is
#' never clipped by the device, whatever the device size.
#'
#' @param legend_args Named list of arguments for [.render_legend_base()],
#'   without `position` or `inset`. Must carry `cex`; `pt.cex` is scaled with
#'   it when present.
#' @param side One of `"bottom"`, `"top"`, `"left"`, `"right"`.
#' @param outer_lines Margin lines at the outer edge of the band to leave free.
#' @return The value of `graphics::legend()`, invisibly.
#' @keywords internal
#' @noRd
.render_legend_in_band <- function(legend_args, side, outer_lines = 0) {
  usr <- graphics::par("usr")
  # The figure region, not the device: under par(mfrow) the band belongs to
  # this panel only.
  fig_x <- graphics::grconvertX(c(0, 1), "nfc", "user")
  fig_y <- graphics::grconvertY(c(0, 1), "nfc", "user")
  band <- switch(side,
    bottom = list(x = fig_x, y = c(fig_y[1L], usr[3L])),
    top    = list(x = fig_x, y = c(usr[4L], fig_y[2L])),
    left   = list(x = c(fig_x[1L], usr[1L]), y = fig_y),
    right  = list(x = c(usr[2L], fig_x[2L]), y = fig_y)
  )
  if (outer_lines > 0) {
    # Give up the outermost lines of the band (a title lives there).
    keep_out <- outer_lines * graphics::par("csi")
    shrink <- switch(side,
      bottom = list(x = c(0, 0), y = c(graphics::yinch(keep_out), 0)),
      top    = list(x = c(0, 0), y = c(0, -graphics::yinch(keep_out))),
      left   = list(x = c(graphics::xinch(keep_out), 0), y = c(0, 0)),
      right  = list(x = c(0, -graphics::xinch(keep_out)), y = c(0, 0))
    )
    band <- list(x = band$x + shrink$x, y = band$y + shrink$y)
  }
  measure <- function(args) {
    do.call(.render_legend_base,
            c(args, list(position = c(0, 0), plot = FALSE)))$rect
  }
  size <- measure(legend_args)
  fit <- min(1, diff(band$x) / size$w, diff(band$y) / size$h)
  if (fit < 1) {
    # 0.97: legend() geometry is close to, not exactly, linear in cex.
    legend_args$cex <- legend_args$cex * fit * 0.97
    if (!is.null(legend_args$pt.cex)) {
      legend_args$pt.cex <- legend_args$pt.cex * fit * 0.97
    }
  }
  do.call(.render_legend_base, c(legend_args, list(
    position = c(mean(band$x), mean(band$y)),
    xjust = 0.5, yjust = 0.5, xpd = TRUE
  )))
}
