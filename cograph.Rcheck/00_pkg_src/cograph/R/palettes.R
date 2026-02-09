#' @title Color Palettes
#' @description Built-in color palettes for network visualization.
#' @name palettes
NULL

#' Rainbow Palette
#'
#' Generate a rainbow color palette.
#'
#' @param n Number of colors to generate.
#' @param alpha Transparency (0-1).
#' @return Character vector of colors.
#' @export
#' @examples
#' palette_rainbow(5)
palette_rainbow <- function(n, alpha = 1) {
  grDevices::rainbow(n, alpha = alpha)
}

#' Colorblind-friendly Palette
#'
#' Generate a colorblind-friendly palette using Wong's colors.
#'
#' @param n Number of colors to generate.
#' @param alpha Transparency (0-1).
#' @return Character vector of colors.
#' @export
#' @examples
#' palette_colorblind(5)
palette_colorblind <- function(n, alpha = 1) {
  # Wong's colorblind-friendly palette
  base_colors <- c(
    "#000000",  # Black
    "#E69F00",  # Orange
    "#56B4E9",  # Sky blue
    "#009E73",  # Bluish green
    "#F0E442",  # Yellow
    "#0072B2",  # Blue
    "#D55E00",  # Vermillion
    "#CC79A7"   # Reddish purple
  )

  if (n <= length(base_colors)) {
    colors <- base_colors[seq_len(n)]
  } else {
    # Interpolate if more colors needed
    colors <- grDevices::colorRampPalette(base_colors)(n)
  }

  if (alpha < 1) {
    colors <- sapply(colors, adjust_alpha, alpha = alpha)
  }

  colors
}

#' Pastel Palette
#'
#' Generate a soft pastel color palette.
#'
#' @param n Number of colors to generate.
#' @param alpha Transparency (0-1).
#' @return Character vector of colors.
#' @export
#' @examples
#' palette_pastel(5)
palette_pastel <- function(n, alpha = 1) {
  base_colors <- c(
    "#FFB3BA",  # Pastel pink
    "#BAFFC9",  # Pastel green
    "#BAE1FF",  # Pastel blue
    "#FFFFBA",  # Pastel yellow
    "#FFDFBA",  # Pastel orange
    "#E0BBE4",  # Pastel purple
    "#957DAD",  # Pastel violet
    "#FEC8D8"   # Pastel rose
  )

  if (n <= length(base_colors)) {
    colors <- base_colors[seq_len(n)]
  } else {
    colors <- grDevices::colorRampPalette(base_colors)(n)
  }

  if (alpha < 1) {
    colors <- sapply(colors, adjust_alpha, alpha = alpha)
  }

  colors
}

#' Viridis Palette
#'
#' Generate colors from the viridis palette.
#'
#' @param n Number of colors to generate.
#' @param alpha Transparency (0-1).
#' @param option Viridis option: "viridis", "magma", "plasma", "inferno", "cividis".
#' @return Character vector of colors.
#' @export
#' @examples
#' palette_viridis(5)
palette_viridis <- function(n, alpha = 1, option = "viridis") {
  # Pre-defined viridis endpoints
  viridis_palettes <- list(
    viridis = c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725"),
    magma = c("#000004", "#3b0f70", "#8c2981", "#de4968", "#fe9f6d", "#fcfdbf"),
    plasma = c("#0d0887", "#6a00a8", "#b12a90", "#e16462", "#fca636", "#f0f921"),
    inferno = c("#000004", "#420a68", "#932667", "#dd513a", "#fca50a", "#fcffa4"),
    cividis = c("#00224e", "#123570", "#3b496c", "#575d6d", "#707173", "#8a8678")
  )

  base <- viridis_palettes[[option]]
  if (is.null(base)) base <- viridis_palettes[["viridis"]]

  colors <- grDevices::colorRampPalette(base)(n)

  if (alpha < 1) {
    colors <- sapply(colors, adjust_alpha, alpha = alpha)
  }

  colors
}

#' Blues Palette
#'
#' Generate a blue sequential palette.
#'
#' @param n Number of colors to generate.
#' @param alpha Transparency (0-1).
#' @return Character vector of colors.
#' @export
#' @examples
#' palette_blues(5)
palette_blues <- function(n, alpha = 1) {
  base_colors <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1",
                   "#6baed6", "#4292c6", "#2171b5", "#084594")
  colors <- grDevices::colorRampPalette(base_colors)(n)
  if (alpha < 1) colors <- sapply(colors, adjust_alpha, alpha = alpha)
  colors
}

#' Reds Palette
#'
#' Generate a red sequential palette.
#'
#' @param n Number of colors to generate.
#' @param alpha Transparency (0-1).
#' @return Character vector of colors.
#' @export
#' @examples
#' palette_reds(5)
palette_reds <- function(n, alpha = 1) {
  base_colors <- c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272",
                   "#fb6a4a", "#ef3b2c", "#cb181d", "#99000d")
  colors <- grDevices::colorRampPalette(base_colors)(n)
  if (alpha < 1) colors <- sapply(colors, adjust_alpha, alpha = alpha)
  colors
}

#' Diverging Palette
#'
#' Generate a diverging color palette (blue-white-red).
#'
#' @param n Number of colors to generate.
#' @param alpha Transparency (0-1).
#' @param midpoint Color for midpoint.
#' @return Character vector of colors.
#' @export
#' @examples
#' palette_diverging(5)
palette_diverging <- function(n, alpha = 1, midpoint = "white") {
  base_colors <- c("#2166ac", "#67a9cf", "#d1e5f0", midpoint,
                   "#fddbc7", "#ef8a62", "#b2182b")
  colors <- grDevices::colorRampPalette(base_colors)(n)
  if (alpha < 1) colors <- sapply(colors, adjust_alpha, alpha = alpha)
  colors
}

#' Register Built-in Palettes
#'
#' @keywords internal
register_builtin_palettes <- function() {
  register_palette("rainbow", palette_rainbow)
  register_palette("colorblind", palette_colorblind)
  register_palette("pastel", palette_pastel)
  register_palette("viridis", palette_viridis)
  register_palette("blues", palette_blues)
  register_palette("reds", palette_reds)
  register_palette("diverging", palette_diverging)
}
