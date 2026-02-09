#' @title Deprecation Utilities
#' @description Functions for handling deprecated parameters with backwards compatibility.
#' @name utils-deprecation
#' @keywords internal
NULL

#' Handle Deprecated Parameter
#'
#' Handles backwards compatibility for renamed parameters. If the old parameter
#' name is used (not NULL), issues a deprecation warning and returns the old value.
#' Otherwise returns the new parameter value.
#'
#' For parameters with defaults, use `new_val_was_set` to indicate whether the
#' user explicitly provided the new value. If FALSE (user didn't set it) and
#' old_val is provided, the old value takes precedence.
#'
#' @param new_val The value of the new parameter name.
#' @param old_val The value of the old (deprecated) parameter name.
#' @param new_name Character string of the new parameter name (for warning message).
#' @param old_name Character string of the old parameter name (for warning message).
#' @param new_val_was_set Logical. TRUE if the user explicitly set new_val
#'   (FALSE means it's just the default). When NULL, the function checks if new_val
#'   is NULL to determine this.
#' @return The effective parameter value.
#'
#' @keywords internal
handle_deprecated_param <- function(new_val, old_val, new_name, old_name,
                                     new_val_was_set = NULL) {
  # If old_val is provided, always warn
  if (!is.null(old_val)) {
    warning(
      sprintf("'%s' is deprecated, use '%s' instead.", old_name, new_name),
      call. = FALSE
    )
    # Use old_val if new_val wasn't explicitly set
    if (is.null(new_val_was_set)) {
      # Backwards compat: if new_val is NULL, old_val takes precedence
      if (is.null(new_val)) {
        return(old_val)
      }
    } else if (!new_val_was_set) {
      # new_val was the default, so use old_val
      return(old_val)
    }
    # If new_val was explicitly set AND old_val is provided,
    # new_val wins (user explicitly chose the new param)
  }
  new_val
}

#' Convert Fontface String to Numeric
#'
#' Converts fontface string specification to numeric value used by R graphics.
#' Handles both string ("plain", "bold", "italic", "bold.italic") and
#' numeric (1, 2, 3, 4) inputs for backwards compatibility.
#'
#' @param fontface Character or numeric fontface specification.
#' @return Numeric fontface value (1=plain, 2=bold, 3=italic, 4=bold.italic).
#'
#' @keywords internal
fontface_to_numeric <- function(fontface) {
  if (is.numeric(fontface)) {
    return(fontface)
  }

  switch(fontface,
    "plain" = 1,
    "bold" = 2,
    "italic" = 3,
    "bold.italic" = 4,
    1  # default to plain
  )
}

#' Convert Numeric Fontface to String
#'
#' Converts numeric fontface value to string specification.
#' Handles both numeric (1, 2, 3, 4) and string inputs.
#'
#' @param fontface Numeric or character fontface specification.
#' @return Character fontface value ("plain", "bold", "italic", "bold.italic").
#'
#' @keywords internal
fontface_to_string <- function(fontface) {
  if (is.character(fontface)) {
    return(fontface)
  }

  switch(as.character(fontface),
    "1" = "plain",
    "2" = "bold",
    "3" = "italic",
    "4" = "bold.italic",
    "plain"  # default
  )
}
