#' Configure a custom multi-panel layout
#'
#' Sets up a multi-panel device layout for use with cograph plotting
#' functions called with \code{combined = FALSE}. Returns a \code{par()}
#' snapshot of the previous device state so the caller can restore it
#' via \code{on.exit(graphics::par(old_par))}.
#'
#' Use \code{spec = c(nrow, ncol)} for a uniform grid (delegates to
#' \code{graphics::par(mfrow = ...)}). Use \code{spec = <matrix>} for a
#' non-uniform layout (delegates to \code{graphics::layout()}); the matrix
#' values name panel cells, so \code{matrix(c(1, 1, 2, 3), 2, 2)} produces
#' one wide cell on top and two cells on the bottom row.
#'
#' @param spec Either a length-2 integer vector \code{c(nrow, ncol)} for a
#'   uniform grid, or a numeric matrix of panel positions to pass to
#'   \code{graphics::layout()}.
#' @param mar Numeric vector of length 4 giving panel margins. Default
#'   \code{c(2, 2, 3, 1)} matches cograph's multi-panel margin convention.
#' @param widths,heights Optional numeric vectors of column widths and row
#'   heights. Only valid when \code{spec} is a matrix; passed straight to
#'   \code{graphics::layout()}. Supplying them with a uniform-grid
#'   \code{spec} is an error, since \code{par(mfrow=...)} has no
#'   widths/heights concept.
#'
#' @return Invisibly returns a list of previous \code{par()} settings that
#'   can be passed back to \code{graphics::par()} to restore the prior
#'   device state. For both spec shapes the snapshot includes
#'   \code{mfrow}, so \code{par(old_par)} also resets any
#'   \code{graphics::layout()} partitioning that this call introduced.
#'
#' @section Combined-flag scope:
#' \code{panel_layout()} composes with the \code{combined = FALSE} opt-out
#' on cograph's multi-panel plot functions. Single-network calls like
#' \code{splot(some_tna_object)} do not honor \code{combined} — there is
#' nothing for it to gate. Pass \code{combined = FALSE} only to the
#' multi-panel hosts: \code{plot_netobject_group()},
#' \code{plot_netobject_ml()}, \code{plot_net_bootstrap_group()},
#' \code{plot_group_permutation()}, \code{plot_compare()},
#' \code{splot.net_mlvar(type = "all")}, \code{plot_network_evolution()},
#' \code{plot.cograph_motifs(type = "network")},
#' \code{plot.cograph_motif_result(type = "patterns")},
#' \code{plot.cograph_motif_analysis(type = "patterns")},
#' \code{plot.tna_disparity(type = "comparison")}, and \code{splot()} on
#' \code{group_tna} / similar list-of-plottables inputs.
#'
#' @examples
#' mat <- matrix(c(0, .5, .3, .5, 0, .4, .3, .4, 0), 3, 3)
#' colnames(mat) <- rownames(mat) <- c("A", "B", "C")
#' net1 <- as_cograph(mat)
#' net2 <- as_cograph(mat * 0.5)
#'
#' # Uniform 1 x 2 grid
#' op <- panel_layout(c(1, 2))
#' splot(net1, combined = FALSE)
#' splot(net2, combined = FALSE)
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
