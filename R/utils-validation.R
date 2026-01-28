#' @title Input Validation Utilities
#' @description Utility functions for validating inputs.
#' @name utils-validation
#' @keywords internal
NULL

#' Validate Network Object
#'
#' @param x Object to validate.
#' @param arg_name Argument name for error messages.
#' @keywords internal
validate_network <- function(x, arg_name = "network") {
  if (!inherits(x, "SonnetNetwork") && !inherits(x, "sonnet_network")) {
    stop(arg_name, " must be a SonnetNetwork object", call. = FALSE)
  }

  # Extract R6 object if wrapped
  if (inherits(x, "sonnet_network")) {
    x <- x$network
  }

  x
}

#' Validate Color
#'
#' @param x Color to validate.
#' @param arg_name Argument name for error messages.
#' @keywords internal
validate_color <- function(x, arg_name = "color") {
  if (is.null(x) || is.na(x)) {
    return(TRUE)
  }

  if (x == "transparent") {
    return(TRUE)
  }

  # Try to convert to RGB
  tryCatch({
    grDevices::col2rgb(x)
    TRUE
  }, error = function(e) {
    stop(arg_name, " is not a valid color: ", x, call. = FALSE)
  })
}

#' Validate Numeric Range
#'
#' @param x Value to validate.
#' @param min Minimum allowed value.
#' @param max Maximum allowed value.
#' @param arg_name Argument name for error messages.
#' @keywords internal
validate_range <- function(x, min = -Inf, max = Inf, arg_name = "value") {
  if (!is.numeric(x)) {
    stop(arg_name, " must be numeric", call. = FALSE)
  }

  if (any(x < min, na.rm = TRUE)) {
    stop(arg_name, " must be >= ", min, call. = FALSE)
  }

  if (any(x > max, na.rm = TRUE)) {
    stop(arg_name, " must be <= ", max, call. = FALSE)
  }

  TRUE
}

#' Validate Choice
#'
#' @param x Value to validate.
#' @param choices Allowed values.
#' @param arg_name Argument name for error messages.
#' @keywords internal
validate_choice <- function(x, choices, arg_name = "value") {
  if (!x %in% choices) {
    stop(arg_name, " must be one of: ", paste(choices, collapse = ", "),
         call. = FALSE)
  }
  TRUE
}

#' Validate Length Match
#'
#' @param x Vector to validate.
#' @param expected_length Expected length.
#' @param arg_name Argument name for error messages.
#' @param allow_single Allow single value (will be recycled).
#' @keywords internal
validate_length <- function(x, expected_length, arg_name = "value",
                            allow_single = TRUE) {
  if (length(x) == expected_length) {
    return(TRUE)
  }

  if (allow_single && length(x) == 1) {
    return(TRUE)
  }

  stop(arg_name, " must have length ", expected_length,
       if (allow_single) " or 1", call. = FALSE)
}

#' Recycle Value to Length
#'
#' @param x Value to recycle.
#' @param n Target length.
#' @return Recycled vector.
#' @keywords internal
recycle_to_length <- function(x, n) {
  if (length(x) == n) {
    return(x)
  }

  if (length(x) == 1) {
    return(rep(x, n))
  }

  # Recycle with warning if not evenly divisible
  rep_len(x, n)
}

#' Expand Parameter to Length (Strict)
#'
#' Expands a parameter to length n. Only accepts length 1 or length n.
#' Throws error for any other length (no silent recycling).
#'
#' @param x Value to expand.
#' @param n Target length.
#' @param name Parameter name for error message.
#' @return Vector of length n.
#' @keywords internal
expand_param <- function(x, n, name = "parameter") {
  if (length(x) == 1) {
    return(rep(x, n))
  }
  if (length(x) == n) {
    return(x)
  }
  stop(name, " must be length 1 or ", n, ", not ", length(x), call. = FALSE)
}

#' Resolve Aesthetic Value
#'
#' Resolve an aesthetic value that could be a constant, vector, or column name.
#'
#' @param value Value to resolve.
#' @param data Data frame to look up column names.
#' @param n Expected length.
#' @param default Default value if NULL.
#' @return Resolved vector of values.
#' @keywords internal
resolve_aesthetic <- function(value, data = NULL, n = NULL, default = NULL) {
  if (is.null(value)) {
    if (is.null(default)) {
      return(NULL)
    }
    value <- default
  }

  # If it's a single string and could be a column name
  if (is.character(value) && length(value) == 1 && !is.null(data)) {
    if (value %in% names(data)) {
      return(data[[value]])
    }
  }

  # Recycle to length
  if (!is.null(n)) {
    value <- recycle_to_length(value, n)
  }

  value
}
