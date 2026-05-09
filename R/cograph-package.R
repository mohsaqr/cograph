#' @title cograph: Modern Network Visualization for R
#'
#' @description
#' A modern, extensible network visualization package that provides high-quality
#' static network plots and ggplot2 conversions. cograph accepts adjacency
#' matrices, edge lists, or igraph objects and offers customizable layouts,
#' node shapes, edge styles, and themes.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{cograph}}: Main entry point for creating network visualizations
#'   \item \code{\link{sn_layout}}: Apply layout algorithms
#'   \item \code{\link{sn_nodes}}: Customize node aesthetics
#'   \item \code{\link{sn_edges}}: Customize edge aesthetics
#'   \item \code{\link{sn_theme}}: Apply visual themes
#'   \item \code{\link{sn_render}}: Render to device
#'   \item \code{\link{sn_ggplot}}: Convert to ggplot2 object
#' }
#'
#' @section Layouts:
#' cograph provides several built-in layouts:
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
#'   \item \code{viridis}: Viridis-based colour theme
#'   \item \code{nature}: Nature-inspired colour theme
#' }
#'
#' @section Weight conventions:
#' cograph's analytic functions follow a single convention for edge weights:
#' \itemize{
#'   \item \strong{Semantics.} A weight is a \emph{strength}: higher weight
#'     means a stronger connection (larger transition probability, thicker
#'     correlation, stronger tie). This matches the qgraph / \pkg{tna}
#'     convention and the intuition of most user-facing inputs.
#'   \item \strong{Path-based measures} (betweenness, closeness, harmonic,
#'     eccentricity, stress, load, radiality, etc.) invert weights to
#'     \emph{distances} via \code{1 / weight ^ alpha}. The \code{alpha}
#'     argument (default 1) tunes how strongly weight differences compress
#'     paths. Controlled by the \code{invert_weights} argument, which
#'     auto-detects to \code{TRUE} for tna objects and \code{FALSE} for
#'     matrices/igraph (matching native igraph / \pkg{sna} defaults).
#'   \item \strong{Non-path measures} (degree, strength, eigenvector,
#'     PageRank, transitivity, modularity, ...) use the raw weights as-is
#'     without inversion.
#'   \item \strong{Unweighted override.} Passing \code{weights = NA} to any
#'     analytic function forces unweighted behavior regardless of what is
#'     attached to the graph.
#' }
#' Individual functions may document exceptions in their own help pages.
#' Any deviation from this convention is a bug — please report.
#'
#' @docType package
#' @name cograph-package
#' @keywords internal
#'
#' @import R6
#' @import grid
#' @import ggplot2
#' @importFrom graphics abline legend lines par plot
#' @importFrom grDevices col2rgb rgb colorRampPalette pdf png svg dev.off adjustcolor
#' @importFrom stats runif rnorm setNames median
#' @importFrom utils modifyList
"_PACKAGE"
