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
  if (!inherits(x, "CographNetwork") && !inherits(x, "cograph_network")) {
    stop(arg_name, " must be a CographNetwork object", call. = FALSE)
  }

  # Extract R6 object if wrapped
  if (inherits(x, "cograph_network")) {
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

# ==============================================================================
# Label Abbreviation
# ==============================================================================

#' Abbreviate Labels
#'
#' Abbreviates labels to a maximum length, adding ellipsis if truncated.
#'
#' @param label Character vector of labels to abbreviate.
#' @param abbrev Abbreviation control:
#'   \itemize{
#'     \item NULL: No abbreviation (return labels unchanged)
#'     \item Integer: Maximum character length (truncate + ellipsis)
#'     \item "auto": Adaptive abbreviation based on label count
#'   }
#' @param n_labels Number of labels (used for "auto" mode). If NULL, uses
#'   length(label).
#' @return Character vector of (possibly abbreviated) labels.
#' @export
#' @examples
#' labels <- c("VeryLongStateName", "Short", "AnotherLongName")
#'
#' # No abbreviation
#' abbrev_label(labels, NULL)
#'
#' # Fixed max length
#' abbrev_label(labels, 5)  # "Very...", "Short", "Anot..."
#'
#' # Auto-adaptive
#' abbrev_label(labels, "auto")
abbrev_label <- function(label, abbrev = NULL, n_labels = NULL) {
  if (is.null(abbrev)) return(label)
  if (is.null(n_labels)) n_labels <- length(label)

  if (identical(abbrev, "auto") || identical(abbrev, "adaptive")) {
    # Adaptive: more labels = shorter abbreviation
    max_len <- if (n_labels <= 5) {
      Inf  # No abbreviation for 5 or fewer labels

    } else if (n_labels <= 8) {
      15
    } else if (n_labels <= 12) {
      10
    } else if (n_labels <= 20) {
      6
    } else {
      4
    }
  } else {
    max_len <- as.integer(abbrev)
  }

  if (is.infinite(max_len)) return(label)

  # Truncate labels longer than max_len
  vapply(label, function(lab) {
    if (nchar(lab) > max_len) {
      paste0(substr(lab, 1, max_len - 1), "\u2026")  # Unicode ellipsis
    } else {
      lab
    }
  }, character(1), USE.NAMES = FALSE)
}

#' @rdname abbrev_label
#' @export
label_abbrev <- abbrev_label

# ==============================================================================
# Shape Utilities
# ==============================================================================

#' Convert Shape Name to pch Value
#'
#' Maps shape names to R's base graphics pch values.
#'
#' @param shape Character vector of shape names.
#' @return Integer vector of pch values.
#' @keywords internal
#' @noRd
.shape_to_pch <- function(shape) {
  shape_map <- c(
    "circle" = 21L,
    "square" = 22L,
    "diamond" = 23L,
    "triangle" = 24L,
    "triangle_down" = 25L,
    "pentagon" = 21L,
    "hexagon" = 21L,
    "star" = 8L,
    "cross" = 3L,
    "plus" = 3L
  )

  vapply(tolower(shape), function(s) {
    if (s %in% names(shape_map)) {
      shape_map[[s]]
    } else {
      21L  # Default to circle
    }
  }, integer(1), USE.NAMES = FALSE)
}
