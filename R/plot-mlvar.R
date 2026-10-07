#' @title Plot Nestimate mlVAR Networks
#' @description Plot the three networks from \code{Nestimate::build_mlvar()}:
#'   temporal (directed), contemporaneous (undirected), between (undirected).
#' @name plot-mlvar
#' @keywords internal
#' @noRd
NULL

#' @details
#' \code{splot.net_mlvar()} plots one or all of the temporal, contemporaneous
#' and between-subjects networks of a \code{net_mlvar} object from
#' \code{Nestimate::build_mlvar()}.
#'
#' @param combined Logical. With \code{type = "all"} for a \code{net_mlvar}
#'   object, \code{TRUE} (default) arranges the three panels in a 1 x 3 grid,
#'   and \code{FALSE} plots them into a layout set up beforehand, for example
#'   with \code{\link{panel_layout}()}. Ignored for a single network.
#'
#' @rdname splot
#' @export
splot.net_mlvar <- function(x, type = "temporal", combined = TRUE, ...) {
  type <- .resolve_mlvar_type(type)

  if (type == "all") {
    if (combined) {
      op <- graphics::par(mfrow = c(1, 3), mar = c(2, 2, 3, 2))
      on.exit(graphics::par(op), add = TRUE)
    }
    # Strip user `title` out of ... and compose it with each panel label,
    # otherwise a user-supplied title would appear on all three panels and
    # lose the per-network identification.
    dots <- list(...)
    user_title <- dots$title
    dots$title <- NULL
    compose <- function(panel_label) {
      if (is.null(user_title)) panel_label else paste(user_title, "-", panel_label)
    }
    do.call(splot.net_mlvar,
            c(list(x, type = "temporal",        title = compose("Temporal")),        dots))
    do.call(splot.net_mlvar,
            c(list(x, type = "contemporaneous", title = compose("Contemporaneous")), dots))
    do.call(splot.net_mlvar,
            c(list(x, type = "between",         title = compose("Between-Subjects")), dots))
    return(invisible(x))
  }

  net <- x[[type]]
  if (is.null(net)) {
    stop(sprintf("Network '%s' not found in net_mlvar object", type),
         call. = FALSE)
  }

  args <- list(...)
  if (is.null(args$title)) {
    args$title <- switch(type,
      temporal        = "Temporal",
      contemporaneous = "Contemporaneous",
      between         = "Between-Subjects"
    )
  }

  do.call(splot, c(list(x = net), args))
  invisible(x)
}

#' Resolve mlvar type aliases
#' @noRd
.resolve_mlvar_type <- function(type) {
  type <- tolower(type[[1L]])
  switch(type,
    t = , temporal        = "temporal",
    c = , contemporaneous = "contemporaneous",
    b = , between         = "between",
    a = , all             = "all",
    stop("type must be one of: temporal (t), contemporaneous (c), between (b), all (a)",
         call. = FALSE)
  )
}
