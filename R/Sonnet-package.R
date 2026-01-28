#' @title Sonnet: Modern Network Visualization for R
#'
#' @description
#' A modern, extensible network visualization package that provides high-quality
#' static and interactive network plots. Sonnet accepts adjacency matrices,
#' edge lists, or igraph objects and offers customizable layouts, node shapes,
#' edge styles, and themes.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{sonnet}}: Main entry point for creating network visualizations
#'   \item \code{\link{sn_layout}}: Apply layout algorithms
#'   \item \code{\link{sn_nodes}}: Customize node aesthetics
#'   \item \code{\link{sn_edges}}: Customize edge aesthetics
#'   \item \code{\link{sn_theme}}: Apply visual themes
#'   \item \code{\link{sn_render}}: Render to device
#'   \item \code{\link{sn_ggplot}}: Convert to ggplot2 object
#' }
#'
#' @section Layouts:
#' Sonnet provides several built-in layouts:
#' \itemize{
#'   \item \code{circle}: Nodes arranged in a circle
#'   \item \code{spring}: Fruchterman-Reingold force-directed layout
#'   \item \code{groups}: Group-based circular layout
#'   \item \code{custom}: User-provided coordinates
#' }
#'
#' @section Themes:
#' Built-in themes include:
#' \itemize{
#'   \item \code{classic}: Traditional network visualization style
#'   \item \code{colorblind}: Accessible color scheme
#'   \item \code{gray}: Grayscale theme
#'   \item \code{dark}: Dark background theme
#'   \item \code{minimal}: Clean, minimal style
#' }
#'
#' @docType package
#' @name Sonnet-package
#' @aliases Sonnet
#'
#' @import R6
#' @import grid
#' @import ggplot2
#' @importFrom grDevices col2rgb rgb colorRampPalette pdf png svg dev.off adjustcolor
#' @importFrom stats runif rnorm setNames median
#' @importFrom utils modifyList
"_PACKAGE"
