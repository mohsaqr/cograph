# Classed conditions shared by the centrality calculators.
#
# Every centrality warning and error a user may want to catch carries one of
# the package's condition classes: `cograph_bad_parameter` for an argument
# outside its domain, `cograph_bad_membership` for a missing or malformed
# community partition, and `cograph_undefined_measure` for a measure that has
# no value on the input (wrong direction, not connected, singular system).

#' Raise a `cograph_bad_parameter` error
#' @param ... Message pieces, pasted together without a separator.
#' @return Does not return.
#' @keywords internal
#' @noRd
.cg_stop_bad_parameter <- function(...) {
  stop(errorCondition(paste0(...), class = "cograph_bad_parameter",
                      call = NULL))
}

#' Raise a `cograph_undefined_measure` warning
#' @param ... Message pieces, pasted together without a separator.
#' @return The message, invisibly.
#' @keywords internal
#' @noRd
.cg_warn_undefined <- function(...) {
  warning(warningCondition(paste0(...), class = "cograph_undefined_measure",
                           call = NULL))
}

#' Warn that a community-aware measure was called without `membership`
#'
#' The warning carries both `cograph_undefined_measure` (the measure has no
#' value) and `cograph_bad_membership` (the partition is what is missing).
#'
#' @param what Measure name, used in the message.
#' @return The message, invisibly.
#' @keywords internal
#' @noRd
.cg_warn_no_membership <- function(what) {
  warning(warningCondition(
    paste0(what, " requires membership; returning NA"),
    class = c("cograph_bad_membership", "cograph_undefined_measure"),
    call = NULL))
}

#' Check that `membership` has one non-missing label per node
#' @param membership Community labels.
#' @param n Number of nodes.
#' @param allow_na Whether missing labels are accepted. The brainGraph-style
#'   measures pass missing labels through to their kernels.
#' @return `membership`, invisibly; raises `cograph_bad_membership` otherwise.
#' @keywords internal
#' @noRd
.cg_check_membership <- function(membership, n, allow_na = FALSE) {
  if (length(membership) != n || (!allow_na && anyNA(membership))) {
    stop(errorCondition(
      sprintf("`membership` needs one %slabel per node (%d), got length %d",
              if (allow_na) "" else "non-missing ", n, length(membership)),
      class = "cograph_bad_membership", call = NULL))
  }
  invisible(membership)
}
