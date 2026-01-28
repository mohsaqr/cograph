#' @title Edge Label Template Formatting
#' @description Functions for formatting edge labels using templates with placeholders.
#' @name splot-labels
#' @keywords internal
NULL

#' Get Significance Stars from P-values
#'
#' Converts p-values to significance stars following conventional thresholds.
#'
#' @param p Numeric p-value(s).
#' @return Character vector of stars.
#' @keywords internal
get_significance_stars <- function(p) {
  if (is.null(p)) return(NULL)

  vapply(p, function(pval) {
    if (is.na(pval)) return("")
    if (pval < 0.001) return("***")
    if (pval < 0.01) return("**")
    if (pval < 0.05) return("*")
    return("")
  }, character(1))
}

#' Format P-value
#'
#' Formats a p-value with specified decimal places and prefix.
#'
#' @param p Numeric p-value.
#' @param digits Number of decimal places.
#' @param prefix Prefix string (e.g., "p=").
#' @return Formatted p-value string.
#' @keywords internal
format_pvalue <- function(p, digits = 3, prefix = "p=") {
  if (is.null(p) || is.na(p)) return("")

  if (p < 10^(-digits)) {
    paste0(prefix, "<", format(10^(-digits), nsmall = digits))
  } else {
    paste0(prefix, format(round(p, digits), nsmall = digits))
  }
}

#' Format CI Range
#'
#' Formats confidence interval bounds as a range string.
#'
#' @param lower Lower bound.
#' @param upper Upper bound.
#' @param digits Number of decimal places.
#' @param format CI format: "bracket" for `[low, up]` or "dash" for `low-up`.
#' @return Formatted CI range string.
#' @keywords internal
format_ci_range <- function(lower, upper, digits = 2, format = "bracket") {
  if (is.null(lower) || is.null(upper) || is.na(lower) || is.na(upper)) {
    return("")
  }

  low_str <- format(round(lower, digits), nsmall = digits)
  up_str <- format(round(upper, digits), nsmall = digits)

  if (format == "bracket") {
    paste0("[", low_str, ", ", up_str, "]")
  } else {
    paste0(low_str, "-", up_str)
  }
}

#' Resolve Stars from Various Inputs
#'
#' Resolves significance stars from character vectors, logical, or p-values.
#'
#' @param stars_input User input: character vector, logical, or numeric p-values.
#' @param p_values P-values for computing stars if stars_input is TRUE/numeric.
#' @param n Number of edges.
#' @return Character vector of stars.
#' @keywords internal
resolve_stars <- function(stars_input, p_values = NULL, n) {
  if (is.null(stars_input)) {
    return(rep("", n))
  }

  # If logical TRUE, compute from p-values
  if (is.logical(stars_input) && length(stars_input) == 1 && stars_input) {
    if (is.null(p_values)) {
      return(rep("", n))
    }
    return(get_significance_stars(p_values))
  }

  # If logical FALSE, no stars
  if (is.logical(stars_input) && length(stars_input) == 1 && !stars_input) {
    return(rep("", n))
  }

  # If numeric, treat as p-values
  if (is.numeric(stars_input)) {
    return(get_significance_stars(stars_input))
  }

  # If character, use directly
  if (is.character(stars_input)) {
    return(recycle_to_length(stars_input, n))
  }

  rep("", n)
}

#' Format Edge Label from Template
#'
#' Processes a template string with placeholders and substitutes values.
#'
#' @param template Template string with placeholders: \{est\}, \{range\}, \{low\}, \{up\}, \{p\}, \{stars\}.
#' @param weight Edge weight (estimate).
#' @param ci_lower Lower CI bound.
#' @param ci_upper Upper CI bound.
#' @param p_value P-value.
#' @param stars Significance stars string.
#' @param digits Decimal places for estimates.
#' @param p_digits Decimal places for p-values.
#' @param p_prefix Prefix for p-values.
#' @param ci_format CI format: "bracket" or "dash".
#' @param oneline Logical: single line format (space-separated) or multiline.
#' @return Formatted label string.
#' @keywords internal
format_edge_label_template <- function(template,
                                       weight = NA,
                                       ci_lower = NA,
                                       ci_upper = NA,
                                       p_value = NA,
                                       stars = "",
                                       digits = 2,
                                       p_digits = 3,
                                       p_prefix = "p=",
                                       ci_format = "bracket",
                                       oneline = TRUE) {
  if (is.null(template) || template == "") {
    return("")
  }

  result <- template

  # Replace {est} - estimate/weight
  if (grepl("\\{est\\}", result)) {
    est_str <- if (!is.na(weight)) {
      format(round(weight, digits), nsmall = digits)
    } else {
      ""
    }
    result <- gsub("\\{est\\}", est_str, result)
  }

  # Replace {range} - full CI range [low, up]
  if (grepl("\\{range\\}", result)) {
    range_str <- format_ci_range(ci_lower, ci_upper, digits, ci_format)
    result <- gsub("\\{range\\}", range_str, result)
  }

  # Replace {low} - CI lower bound only
  if (grepl("\\{low\\}", result)) {
    low_str <- if (!is.na(ci_lower)) {
      format(round(ci_lower, digits), nsmall = digits)
    } else {
      ""
    }
    result <- gsub("\\{low\\}", low_str, result)
  }

  # Replace {up} - CI upper bound only
  if (grepl("\\{up\\}", result)) {
    up_str <- if (!is.na(ci_upper)) {
      format(round(ci_upper, digits), nsmall = digits)
    } else {
      ""
    }
    result <- gsub("\\{up\\}", up_str, result)
  }

  # Replace {p} - p-value
  if (grepl("\\{p\\}", result)) {
    p_str <- format_pvalue(p_value, p_digits, p_prefix)
    result <- gsub("\\{p\\}", p_str, result)
  }

  # Replace {stars} - significance stars
  if (grepl("\\{stars\\}", result)) {
    stars_str <- if (!is.null(stars) && !is.na(stars)) stars else ""
    result <- gsub("\\{stars\\}", stars_str, result)
  }

  # Clean up extra whitespace
  result <- trimws(result)
  result <- gsub("\\s+", " ", result)

  result
}

#' Get Template from Style Preset
#'
#' Converts a style preset name to its corresponding template string.
#'
#' @param style Style preset: "none", "estimate", "full", "range", "stars".
#' @return Template string or NULL for "none".
#' @keywords internal
get_template_from_style <- function(style) {
  switch(style,
    "none" = NULL,
    "estimate" = "{est}",
    "full" = "{est} {range}",
    "range" = "{range}",
    "stars" = "{stars}",
    NULL
  )
}

#' Build Edge Labels from Template
#'
#' Generates edge labels for all edges using template formatting.
#'
#' @param template Template string or NULL.
#' @param style Style preset (used if template is NULL).
#' @param weights Edge weights/estimates.
#' @param ci_lower Lower CI bounds (vector).
#' @param ci_upper Upper CI bounds (vector).
#' @param p_values P-values (vector).
#' @param stars Stars input (character vector, logical, or numeric p-values).
#' @param digits Decimal places for estimates.
#' @param p_digits Decimal places for p-values.
#' @param p_prefix Prefix for p-values.
#' @param ci_format CI format: "bracket" or "dash".
#' @param oneline Logical: single line format.
#' @param n Number of edges.
#' @return Character vector of formatted labels.
#' @keywords internal
build_edge_labels_from_template <- function(template = NULL,
                                            style = "none",
                                            weights = NULL,
                                            ci_lower = NULL,
                                            ci_upper = NULL,
                                            p_values = NULL,
                                            stars = NULL,
                                            digits = 2,
                                            p_digits = 3,
                                            p_prefix = "p=",
                                            ci_format = "bracket",
                                            oneline = TRUE,
                                            n) {
  # Determine template to use
  if (is.null(template)) {
    template <- get_template_from_style(style)
  }

  if (is.null(template)) {
    return(NULL)  # "none" style, no labels
  }

  # Resolve stars
  stars_vec <- resolve_stars(stars, p_values, n)

  # Recycle inputs to length n
  if (!is.null(weights)) weights <- recycle_to_length(weights, n)
  if (!is.null(ci_lower)) ci_lower <- recycle_to_length(ci_lower, n)
  if (!is.null(ci_upper)) ci_upper <- recycle_to_length(ci_upper, n)
  if (!is.null(p_values)) p_values <- recycle_to_length(p_values, n)

  # Generate labels for each edge
  labels <- vapply(seq_len(n), function(i) {
    format_edge_label_template(
      template = template,
      weight = if (!is.null(weights)) weights[i] else NA,
      ci_lower = if (!is.null(ci_lower)) ci_lower[i] else NA,
      ci_upper = if (!is.null(ci_upper)) ci_upper[i] else NA,
      p_value = if (!is.null(p_values)) p_values[i] else NA,
      stars = stars_vec[i],
      digits = digits,
      p_digits = p_digits,
      p_prefix = p_prefix,
      ci_format = ci_format,
      oneline = oneline
    )
  }, character(1))

  labels
}
