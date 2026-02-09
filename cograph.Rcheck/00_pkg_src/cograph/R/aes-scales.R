#' @title Aesthetic Scale Functions
#' @description Functions for creating aesthetic scales.
#' @name aes-scales
#' @keywords internal
NULL

#' Create a Size Scale
#'
#' Map values to sizes.
#'
#' @param values Values to map.
#' @param range Output size range.
#' @param trans Transformation: "linear", "sqrt", "log".
#' @return Scaled values.
#' @keywords internal
scale_size <- function(values, range = c(0.03, 0.1), trans = "linear") {
  if (all(is.na(values))) return(rep(mean(range), length(values)))

  # Apply transformation
  trans_values <- switch(trans,
    linear = values,
    sqrt = sqrt(pmax(0, values)),
    log = log1p(pmax(0, values)),
    values
  )

  # Normalize
  val_range <- range(trans_values, na.rm = TRUE)
  if (diff(val_range) == 0) {
    return(rep(mean(range), length(values)))
  }

  normalized <- (trans_values - val_range[1]) / diff(val_range)
  range[1] + normalized * diff(range)
}

#' Create a Color Scale
#'
#' Map values to colors.
#'
#' @param values Values to map.
#' @param palette Color palette (vector of colors or palette function name).
#' @param limits Optional range limits.
#' @return Character vector of colors.
#' @keywords internal
scale_color <- function(values, palette = "viridis", limits = NULL) {
  if (all(is.na(values))) return(rep("gray50", length(values)))

  # Get colors
  if (is.character(palette) && length(palette) == 1) {
    # Palette name
    pal_fn <- get_palette(palette)
    if (is.null(pal_fn)) {
      # Try as a single color
      return(rep(palette, length(values)))
    }
    colors <- pal_fn(100)
  } else if (is.function(palette)) {
    colors <- palette(100)
  } else {
    colors <- palette
  }

  map_to_colors(values, colors, limits)
}

#' Create a Categorical Color Scale
#'
#' Map categorical values to colors.
#'
#' @param values Categorical values.
#' @param palette Color palette.
#' @return Character vector of colors.
#' @keywords internal
scale_color_discrete <- function(values, palette = "colorblind") {
  values <- as.factor(values)
  n_levels <- length(levels(values))

  # Get colors
  if (is.character(palette) && length(palette) == 1) {
    pal_fn <- get_palette(palette)
    if (is.null(pal_fn)) {
      colors <- rep(palette, n_levels)
    } else {
      colors <- pal_fn(n_levels)
    }
  } else if (is.function(palette)) {
    colors <- palette(n_levels)
  } else {
    colors <- rep(palette, length.out = n_levels)
  }

  colors[as.integer(values)]
}

#' Create a Width Scale
#'
#' Map values to line widths.
#'
#' @param values Values to map.
#' @param range Output width range.
#' @return Scaled values.
#' @keywords internal
scale_width <- function(values, range = c(0.5, 3)) {
  scale_size(values, range, trans = "linear")
}

#' Create an Alpha Scale
#'
#' Map values to transparency.
#'
#' @param values Values to map.
#' @param range Output alpha range.
#' @return Scaled values.
#' @keywords internal
scale_alpha <- function(values, range = c(0.3, 1)) {
  scaled <- scale_size(values, range, trans = "linear")
  pmax(0, pmin(1, scaled))
}
