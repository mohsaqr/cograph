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
#'   heights. Only used when \code{spec} is a matrix; passed straight to
#'   \code{graphics::layout()}.
#'
#' @return Invisibly returns a list of previous \code{par()} settings that
#'   can be passed back to \code{graphics::par()} to restore the prior
#'   device state.
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
    layout_args <- list(mat = spec)
    if (!is.null(widths))  layout_args$widths  <- widths
    if (!is.null(heights)) layout_args$heights <- heights
    do.call(graphics::layout, layout_args)
    old_par <- graphics::par(mar = mar)
  } else if (is.numeric(spec) && length(spec) == 2L) {
    nr <- as.integer(spec[1L])
    nc <- as.integer(spec[2L])
    if (is.na(nr) || is.na(nc) || nr < 1L || nc < 1L) {
      stop("panel_layout(): `spec` of form c(nrow, ncol) must have ",
           "positive integer entries", call. = FALSE)
    }
    old_par <- graphics::par(mfrow = c(nr, nc), mar = mar)
  } else {
    stop("panel_layout(): `spec` must be c(nrow, ncol) or a numeric matrix",
         call. = FALSE)
  }

  invisible(old_par)
}
