#' Configure a custom multi-panel layout
#'
#' Sets up a multi-panel device layout for use with cograph plotting
#' functions called with \code{combined = FALSE}. The previous \code{par()}
#' settings are returned so that the caller can restore the device state.
#'
#' A length-2 \code{spec = c(nrow, ncol)} creates a uniform grid through
#' \code{graphics::par(mfrow = ...)}. A matrix \code{spec} creates a
#' non-uniform layout through \code{graphics::layout()}. The matrix values
#' number the panel cells and are read in column-major order, so
#' \code{matrix(c(1, 1, 2, 3), 2, 2)} gives one tall cell in the left column
#' and two stacked cells in the right column.
#'
#' @param spec Either a length-2 vector of positive integers
#'   \code{c(nrow, ncol)} for a uniform grid, or a numeric matrix of
#'   non-negative panel numbers with at least one positive cell, passed to
#'   \code{graphics::layout()}.
#' @param mar Numeric vector of length 4 giving panel margins. Default
#'   \code{c(2, 2, 3, 1)}.
#' @param widths,heights Optional numeric vectors of column widths and row
#'   heights, passed to \code{graphics::layout()}. They are valid only when
#'   \code{spec} is a matrix. Supplying them with a length-2 \code{spec} is
#'   an error.
#'
#' @return Invisibly, a list of the previous \code{par()} settings
#'   (\code{mar} and \code{mfrow}). Passing it to \code{graphics::par()}
#'   restores the prior device state and also clears a layout set by
#'   \code{graphics::layout()}.
#'
#' @section Combined-flag scope:
#' The \code{combined = FALSE} argument applies to the multi-panel plot
#' functions \code{plot_netobject_group()},
#' \code{plot_netobject_ml()}, \code{plot_net_bootstrap_group()},
#' \code{plot_group_permutation()}, \code{plot_difference()},
#' \code{splot.net_mlvar(type = "all")}, \code{plot_network_evolution()},
#' \code{plot.cograph_motifs(type = "network")},
#' \code{plot.cograph_motif_result(type = "patterns")},
#' \code{plot.cograph_motif_analysis(type = "patterns")},
#' \code{plot.tna_disparity(type = "comparison")}, and \code{splot()} on
#' \code{group_tna} and other list inputs. A single-network \code{splot()}
#' call plots one panel and ignores \code{combined}.
#'
#' @examples
#' op <- panel_layout(c(1, 2))
#' splot(regulation_net, combined = FALSE)
#' splot(regulation_net, layout = "circle", combined = FALSE)
#' graphics::par(op)
#'
#' @export
panel_layout <- function(spec,
                         mar     = c(2, 2, 3, 1),
                         widths  = NULL,
                         heights = NULL) {
  if (!is.numeric(mar) || length(mar) != 4L) {
    stop("panel_layout(): `mar` must be a numeric vector of length 4",
         call. = FALSE)
  }

  if (is.matrix(spec)) {
    if (!is.numeric(spec)) {
      stop("panel_layout(): matrix `spec` must be numeric", call. = FALSE)
    }
    if (any(spec < 0, na.rm = TRUE) || all(spec == 0, na.rm = TRUE)) {
      stop("panel_layout(): matrix `spec` must contain non-negative ",
           "integers and at least one positive cell", call. = FALSE)
    }
    layout_args <- list(mat = spec)
    if (!is.null(widths))  layout_args$widths  <- widths
    if (!is.null(heights)) layout_args$heights <- heights

    # Capture mfrow before installing the layout(). graphics::layout() has
    # no inverse, so the returned `old_par` carries the prior mfrow; when
    # the caller does graphics::par(old_par), R clears the layout() state
    # as a side effect of restoring mfrow.
    prior_mfrow <- graphics::par("mfrow")
    do.call(graphics::layout, layout_args)
    old_par <- graphics::par(mar = mar)
    old_par$mfrow <- prior_mfrow
  } else if (is.numeric(spec) && length(spec) == 2L) {
    if (!is.null(widths) || !is.null(heights)) {
      stop("panel_layout(): `widths` and `heights` are only valid when ",
           "`spec` is a matrix (graphics::par(mfrow=...) has no concept ",
           "of variable widths/heights)", call. = FALSE)
    }
    nr <- spec[1L]
    nc <- spec[2L]
    if (anyNA(c(nr, nc)) || nr < 1 || nc < 1 ||
        nr != as.integer(nr) || nc != as.integer(nc)) {
      stop("panel_layout(): `spec` of form c(nrow, ncol) must have ",
           "positive integer entries (got nrow=", nr, ", ncol=", nc, ")",
           call. = FALSE)
    }
    old_par <- graphics::par(mfrow = c(as.integer(nr), as.integer(nc)),
                             mar = mar)
  } else {
    stop("panel_layout(): `spec` must be c(nrow, ncol) or a numeric matrix",
         call. = FALSE)
  }

  invisible(old_par)
}
