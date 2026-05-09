#' @title Shared Legend Renderer
#' @description Shared internal helper for base-R legends that opt into
#'   device-scale compensation (`visual_scale`). Used by core base-R plotters
#'   to avoid per-plotter size drift; some legacy direct `graphics::legend()`
#'   calls remain in older helpers.
#' @name render-legend-shared
#' @keywords internal
NULL

#' Render a base-R legend with device-aware size compensation.
#'
#' A thin wrapper around `graphics::legend()` that multiplies cex / pt.cex /
#' lwd by the corresponding visual-scale multiplier before delegating. Accepts
#' positions as a keyword string (`"topright"`, etc.) or a numeric `c(x, y)`
#' in user coordinates.
#'
#' @param legend Character vector of entry labels.
#' @param col Per-entry colours.
#' @param pch Per-entry plotting symbols.
#' @param lty Per-entry line types.
#' @param lwd Per-entry line widths. Multiplied by `visual_scale$line`.
#' @param pt.cex Per-entry point cex. Multiplied by `visual_scale$point`.
#' @param pt.bg Per-entry point background colours.
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
