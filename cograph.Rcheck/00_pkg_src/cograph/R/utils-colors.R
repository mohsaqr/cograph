#' @title Color Utilities
#' @description Utility functions for color manipulation.
#' @name utils-colors
#' @keywords internal
NULL

#' Adjust Color Alpha
#' @noRd
adjust_alpha <- function(color, alpha = 1) {
  if (is.na(color) || is.null(color)) {
    return(NA_character_)
  }

  # Handle transparent
  if (color == "transparent") {
    return("transparent")
  }

  # Convert to RGB
  rgb_vals <- tryCatch(
    grDevices::col2rgb(color, alpha = TRUE),
    error = function(e) NULL
  )

  if (is.null(rgb_vals)) {
    return(color)
  }

  # Apply alpha
  grDevices::rgb(
    rgb_vals[1, 1],
    rgb_vals[2, 1],
    rgb_vals[3, 1],
    alpha = round(alpha * 255),
    maxColorValue = 255
  )
}

#' Lighten or Darken Color
#' @noRd
adjust_brightness <- function(color, amount = 0.2) {
  if (is.na(color) || is.null(color)) {
    return(NA_character_)
  }

  rgb_vals <- grDevices::col2rgb(color)

  if (amount > 0) {
    # Lighten
    rgb_vals <- rgb_vals + (255 - rgb_vals) * amount
  } else {
    # Darken
    rgb_vals <- rgb_vals * (1 + amount)
  }

  rgb_vals <- pmax(0, pmin(255, rgb_vals))

  grDevices::rgb(rgb_vals[1, 1], rgb_vals[2, 1], rgb_vals[3, 1],
                 maxColorValue = 255)
}

#' Interpolate Between Colors
#' @noRd
interpolate_colors <- function(color1, color2, n) {
  grDevices::colorRampPalette(c(color1, color2))(n)
}

#' Get Contrasting Text Color
#' @noRd
contrast_text_color <- function(bg_color) {
  if (is.na(bg_color) || is.null(bg_color) || bg_color == "transparent") {
    return("black")
  }

  rgb_vals <- grDevices::col2rgb(bg_color)

  # Calculate relative luminance
  luminance <- (0.299 * rgb_vals[1, 1] + 0.587 * rgb_vals[2, 1] +
                  0.114 * rgb_vals[3, 1]) / 255

  if (luminance > 0.5) "black" else "white"
}

#' Map Values to Colors
#' @noRd
map_to_colors <- function(values, colors, limits = NULL) {
  if (is.null(limits)) {
    limits <- range(values, na.rm = TRUE)
  }

  # Normalize values to [0, 1]
  normalized <- (values - limits[1]) / (limits[2] - limits[1])
  normalized <- pmax(0, pmin(1, normalized))

  # Create color ramp
  ramp <- grDevices::colorRamp(colors)

  # Map values
  rgb_matrix <- ramp(normalized)
  grDevices::rgb(rgb_matrix[, 1], rgb_matrix[, 2], rgb_matrix[, 3],
                 maxColorValue = 255)
}
