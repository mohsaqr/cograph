#' @title Built-in Themes
#' @description Pre-defined themes for network visualization.
#' @name themes-builtin
NULL

#' Classic Theme
#'
#' Traditional network visualization style with blue nodes and gray edges.
#'
#' @return A CographTheme object.
#' @export
#' @examples
#' theme <- theme_cograph_classic()
theme_cograph_classic <- function() {
  CographTheme$new(
    name = "classic",
    background = "white",
    node_fill = "#4A90D9",
    node_border = "#2C5AA0",
    node_border_width = 1.5,
    edge_color = "gray50",
    edge_positive_color = "#2E7D32",
    edge_negative_color = "#C62828",
    edge_width = 1,
    label_color = "black",
    label_size = 10,
    title_color = "black",
    title_size = 14,
    legend_background = "white"
  )
}

#' Colorblind-friendly Theme
#'
#' Theme using colors distinguishable by people with color vision deficiency.
#'
#' @return A CographTheme object.
#' @export
#' @examples
#' theme <- theme_cograph_colorblind()
theme_cograph_colorblind <- function() {
  CographTheme$new(
    name = "colorblind",
    background = "white",
    node_fill = "#0072B2",
    node_border = "#004C7F",
    node_border_width = 1.5,
    edge_color = "gray50",
    edge_positive_color = "#0000FF",
    edge_negative_color = "#FF0000",
    edge_width = 1,
    label_color = "black",
    label_size = 10,
    title_color = "black",
    title_size = 14,
    legend_background = "white"
  )
}

#' Grayscale Theme
#'
#' Black and white theme suitable for print.
#'
#' @return A CographTheme object.
#' @export
#' @examples
#' theme <- theme_cograph_gray()
theme_cograph_gray <- function() {
  CographTheme$new(
    name = "gray",
    background = "white",
    node_fill = "gray70",
    node_border = "gray30",
    node_border_width = 1.5,
    edge_color = "gray50",
    edge_positive_color = "gray20",
    edge_negative_color = "gray60",
    edge_width = 1,
    label_color = "black",
    label_size = 10,
    title_color = "black",
    title_size = 14,
    legend_background = "white"
  )
}

#' Dark Theme
#'
#' Dark background theme for presentations.
#'
#' @return A CographTheme object.
#' @export
#' @examples
#' theme <- theme_cograph_dark()
theme_cograph_dark <- function() {
  CographTheme$new(
    name = "dark",
    background = "#1a1a2e",
    node_fill = "#e94560",
    node_border = "#ff6b6b",
    node_border_width = 1.5,
    edge_color = "gray60",
    edge_positive_color = "#4ecca3",
    edge_negative_color = "#fc5185",
    edge_width = 1,
    label_color = "white",
    label_size = 10,
    title_color = "white",
    title_size = 14,
    legend_background = "#1a1a2e"
  )
}

#' Minimal Theme
#'
#' Clean, minimal style with thin borders.
#'
#' @return A CographTheme object.
#' @export
#' @examples
#' theme <- theme_cograph_minimal()
theme_cograph_minimal <- function() {
  CographTheme$new(
    name = "minimal",
    background = "white",
    node_fill = "white",
    node_border = "gray40",
    node_border_width = 0.75,
    edge_color = "gray70",
    edge_positive_color = "gray40",
    edge_negative_color = "gray40",
    edge_width = 0.5,
    label_color = "gray30",
    label_size = 9,
    title_color = "gray20",
    title_size = 12,
    legend_background = "white"
  )
}

#' Viridis Theme
#'
#' Theme using viridis color palette.
#'
#' @return A CographTheme object.
#' @export
#' @examples
#' theme <- theme_cograph_viridis()
theme_cograph_viridis <- function() {
  CographTheme$new(
    name = "viridis",
    background = "white",
    node_fill = "#21918c",
    node_border = "#31688e",
    node_border_width = 1.5,
    edge_color = "gray50",
    edge_positive_color = "#5ec962",
    edge_negative_color = "#b5367a",
    edge_width = 1,
    label_color = "black",
    label_size = 10,
    title_color = "black",
    title_size = 14,
    legend_background = "white"
  )
}

#' Nature Theme
#'
#' Earth tones theme inspired by nature.
#'
#' @return A CographTheme object.
#' @export
#' @examples
#' theme <- theme_cograph_nature()
theme_cograph_nature <- function() {
  CographTheme$new(
    name = "nature",
    background = "#fefae0",
    node_fill = "#606c38",
    node_border = "#283618",
    node_border_width = 1.5,
    edge_color = "#bc6c25",
    edge_positive_color = "#606c38",
    edge_negative_color = "#9b2226",
    edge_width = 1,
    label_color = "#283618",
    label_size = 10,
    title_color = "#283618",
    title_size = 14,
    legend_background = "#fefae0"
  )
}
