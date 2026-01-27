#' @title Scaling Constants
#' @description Central scaling constants for parameter alignment between splot/soplot.
#' @name scale-constants
#' @keywords internal
NULL

#' qgraph Scaling Constants (Exact Values)
#'
#' Scaling constants that exactly replicate qgraph's visual formulas.
#' Used by sonplot() for qgraph-compatible network visualization.
#'
#' @format A list with the following elements:
#' \describe{
#'   \item{vsize_base}{Base multiplier in vsize formula: 8}
#'   \item{vsize_decay}{Decay constant in vsize formula: 80}
#'   \item{vsize_min}{Minimum added to vsize: 1}
#'   \item{vsize_factor}{Scale factor to convert vsize to user coordinates: 0.015}
#'   \item{esize_base}{Base multiplier in esize formula: 15}
#'   \item{esize_decay}{Decay constant in esize formula: 90}
#'   \item{esize_min}{Minimum added to esize: 1}
#'   \item{esize_unweighted}{Default edge width for unweighted networks: 2}
#'   \item{cent2edge_divisor}{Divisor in cent2edge formula: 17.5}
#'   \item{cent2edge_reference}{Reference value in cent2edge: 2.16}
#'   \item{cent2edge_plot_ref}{Plot reference size: 7}
#'   \item{curve_ref_diagonal}{Diagonal reference for curve normalization: sqrt(98)}
#'   \item{arrow_factor}{Arrow size scale factor: 0.06}
#' }
#'
#' @keywords internal
QGRAPH_SCALE <- list(
  # vsize formula: 8 * exp(-n/80) + 1
  vsize_base = 8,
  vsize_decay = 80,
  vsize_min = 1,
  vsize_factor = 0.012,  # Calibrated: converts vsize units to user coordinates

  # esize formula: 15 * exp(-n/90) + 1
  # Note: qgraph's esize ~15 visually corresponds to lwd ~4
  # Use esize_scale to convert qgraph esize to lwd
  esize_base = 15,
  esize_decay = 90,
  esize_min = 1,
  esize_unweighted = 2,
  esize_scale = 0.27,  # Calibrated: qgraph_esize * scale = lwd

  # Cent2Edge constants (for exact qgraph boundary calculations)
  cent2edge_divisor = 17.5,
  cent2edge_reference = 2.16,
  cent2edge_plot_ref = 7,

  # Curve normalization: sqrt(pin[1]^2 + pin[2]^2) / sqrt(7^2 + 7^2)
  curve_ref_diagonal = sqrt(7^2 + 7^2),

  # Arrow sizing
  # Bumped from 0.02 to 0.06 for visible arrows at default arrow_size=1
  arrow_factor = 0.06
)

#' Sonnet Scaling Constants
#'
#' Central location for all scaling factors used in splot() and soplot().
#' These constants are calibrated to produce similar visual output to qgraph
#' when using equivalent parameter values.
#'
#' @details
#' The default scaling mode uses values calibrated to match qgraph visual appearance:
#' - `node_size = 6` in Sonnet should look similar to `vsize = 6` in qgraph
#' - `label_size = 1` uses cex-style multiplier (independent of node size)
#' - `arrow_size = 1` produces consistent arrows between splot and soplot
#'
#' Legacy mode preserves the original Sonnet v1.x behavior where:
#' - Node sizes used a 0.04 scale factor
#' - Label sizes were coupled to node size (vsize * 8)
#' - Arrow sizes differed between splot (0.03) and soplot (0.015)
#'
#' @format A list with the following elements:
#' \describe{
#'   \item{node_factor}{Scale factor applied to node_size parameter}
#'   \item{node_default}{Default node size when not specified}
#'   \item{label_default}{Default label size (cex multiplier)}
#'   \item{label_coupled}{Whether label size is coupled to node size}
#'   \item{edge_base}{Base edge width}
#'   \item{edge_scale}{Edge width scale factor}
#'   \item{edge_default}{Default edge width}
#'   \item{arrow_factor}{Scale factor for arrow sizes}
#'   \item{arrow_default}{Default arrow size}
#' }
#'
#' @keywords internal
SONNET_SCALE <- list(
  # Node sizing: node_size=6 should look like qgraph vsize=6
  # Calibrated: 6 * 0.015 = 0.09 user coords (similar visual size to qgraph)
  node_factor = 0.015,
  node_default = 6,

  # Label sizing: independent of node, cex-style
  # label_size=1 is the baseline (like cex=1 in base R)

  label_default = 1,
  label_coupled = FALSE,

  # Edge sizing (legacy simple parameters)
  edge_base = 0.5,
  edge_scale = 3,
  edge_default = 1,

  # Edge width scaling (qgraph-matched + extensions)
  # Output range [min_width, max_width] for scaled edges
  edge_width_range = c(0.1, 4),
  # Scaling mode: "linear", "log", "sqrt", "rank"
  edge_scale_mode = "linear",
  # Default cut = 75th percentile when NULL
  edge_cut_quantile = 0.75,
  # Default width when no weights present
  edge_width_default = 1,

  # Arrow sizing - unified between splot and soplot
  # Calibrated to produce visible arrows at default arrow_size=1
  arrow_factor = 0.06,
  arrow_default = 1,

  # soplot-specific: NPC coordinates
  # When converting node_size for soplot (NPC coords), use this factor
  # Calibrated: splot uses ~2.6 user coord range, soplot uses 1.0 NPC
  # To match: 0.015 / 2.6 ≈ 0.006
  soplot_node_factor = 0.006
)

#' Legacy Scaling Constants (Pre-v2.0 Behavior)
#'
#' Scaling constants that preserve the original Sonnet v1.x behavior.
#' Use `scaling = "legacy"` to enable these values.
#'
#' @format A list with the same structure as \code{SONNET_SCALE}
#' @keywords internal
SONNET_SCALE_LEGACY <- list(
  # Original splot values
  node_factor = 0.04,
  node_default = 3,

  # Label size coupled to node size (vsize * 8)
  label_default = NULL,
  label_coupled = TRUE,

  # Edge sizing (unchanged)
  edge_base = 0.5,
  edge_scale = 3,
  edge_default = NULL,

  # Edge width scaling (legacy uses simpler linear scaling)
  edge_width_range = c(0.5, 4),
  edge_scale_mode = "linear",
  edge_cut_quantile = 0.75,
  edge_width_default = 1,

  # Original arrow factors
  # splot used 0.03, soplot used 0.015
  arrow_factor = 0.03,
  arrow_factor_soplot = 0.015,
  arrow_default = 1,

  # soplot-specific (original behavior, adjusted for coordinate system)
  soplot_node_factor = 0.004
)

#' Get Scaling Constants
#'
#' Returns the appropriate scaling constants based on the scaling mode.
#'
#' @param scaling Character: "default" for qgraph-matched scaling,
#'   "legacy" for pre-v2.0 behavior.
#' @return A list of scaling constants.
#' @keywords internal
get_scale_constants <- function(scaling = "default") {
  if (identical(scaling, "legacy")) {
    SONNET_SCALE_LEGACY
  } else {
    SONNET_SCALE
  }
}

#' Compute Adaptive Base Edge Size
#'
#' Calculates the maximum edge width that decreases with more nodes.
#' Inspired by qgraph but scaled for line widths (not pixels).
#'
#' @param n_nodes Number of nodes in the network.
#' @param directed Whether the network is directed (directed networks use thinner edges).
#' @return Numeric maximum edge width (suitable for lwd parameter).
#'
#' @details
#' The formula produces reasonable line widths:
#' - 3 nodes: ~5
#' - 10 nodes: ~4.5
#' - 50 nodes: ~3
#' - 100 nodes: ~2
#' - 200 nodes: ~1.2
#'
#' For directed networks, the size is reduced by 30% (minimum 1).
#'
#' @keywords internal
compute_adaptive_esize <- function(n_nodes, directed = FALSE) {
  # Scaled formula for reasonable line widths (0.5 to ~6)
  # Uses gentler decay than qgraph's pixel-based formula
  esize <- 4 * exp(-n_nodes / 150) + 1.5

  if (directed) {
    esize <- max(esize * 0.7, 1)
  }

  esize
}

#' Scale Edge Widths Based on Weights
#'
#' Unified edge width scaling function that supports multiple scaling modes,
#' two-tier cutoff system (like qgraph), and output range specification.
#'
#' @param weights Numeric vector of edge weights.
#' @param esize Base edge size. NULL uses adaptive sizing based on n_nodes.
#' @param n_nodes Number of nodes (for adaptive esize calculation).
#' @param directed Whether network is directed (affects adaptive esize).
#' @param mode Scaling mode: "linear", "log", "sqrt", or "rank".
#' @param maximum Max weight for normalization. NULL for auto-detect.
#' @param minimum Min weight threshold. Edges below this get minimum width.
#' @param cut Two-tier cutoff threshold. NULL = auto (75th percentile),
#'   0 = disabled (continuous scaling), positive number = manual threshold.
#' @param range Output width range as c(min_width, max_width).
#' @return Numeric vector of scaled edge widths.
#'
#' @details
#' ## Scaling Modes
#'
#' - **linear** (default): Direct proportional scaling, matches qgraph behavior.
#' - **log**: Logarithmic scaling for wide weight ranges. Uses log1p for stability.
#' - **sqrt**: Square root scaling for moderate compression.
#' - **rank**: Rank-based scaling for equal visual spacing regardless of weight distribution.
#'
#' ## Two-Tier System (cut parameter)
#'
#' When cut > 0, edges are divided into two tiers:
#' - Below cut: Minimal width variation (20% of range)
#' - Above cut: Full width scaling (80% of range)
#'
#' This matches qgraph's behavior where weak edges are visually de-emphasized.
#'
#' @examples
#' \dontrun{
#' weights <- c(0.1, 0.3, 0.5, 0.8, 1.0)
#'
#' # Linear scaling (default)
#' scale_edge_widths(weights, mode = "linear")
#'
#' # Log scaling for wide ranges
#' scale_edge_widths(c(0.01, 0.1, 1, 10, 100), mode = "log")
#'
#' # With two-tier cut
#' scale_edge_widths(weights, cut = 0.5)
#'
#' # Rank-based (equal visual spacing)
#' scale_edge_widths(weights, mode = "rank", cut = 0)
#' }
#'
#' @keywords internal
scale_edge_widths <- function(weights,
                               esize = NULL,
                               n_nodes = NULL,
                               directed = FALSE,
                               mode = "linear",
                               maximum = NULL,
                               minimum = 0,
                               cut = NULL,
                               range = c(0.5, 4)) {
  if (length(weights) == 0) return(numeric(0))

  # Use absolute values
  abs_weights <- abs(weights)


  # Determine effective range for edge widths

  # Priority: if esize is explicitly provided, it overrides range[2]
  # Otherwise, use range as-is (respecting user's edge_width_range)
  if (!is.null(esize)) {
    # esize explicitly provided - use it as max
    effective_range <- c(range[1], esize)
  } else {
    # No esize - use range directly (user's edge_width_range is respected)
    effective_range <- range
  }

  # Auto-detect maximum
  if (is.null(maximum)) {
    maximum <- max(abs_weights, na.rm = TRUE)
  }
  if (maximum == 0 || is.na(maximum)) maximum <- 1

  # Apply scaling mode to normalize weights
  normalized <- switch(mode,
    "linear" = abs_weights / maximum,
    "log" = log1p(abs_weights) / log1p(maximum),
    "sqrt" = sqrt(abs_weights) / sqrt(maximum),
    "rank" = {
      r <- rank(abs_weights, ties.method = "average", na.last = "keep")
      min_r <- min(r, na.rm = TRUE)
      max_r <- max(r, na.rm = TRUE)
      if (max_r > min_r) {
        (r - min_r) / (max_r - min_r)
      } else {
        rep(0.5, length(abs_weights))
      }
    },
    abs_weights / maximum  # fallback to linear
  )

  # Handle NA values
  normalized[is.na(normalized)] <- 0

  # Clamp to [0, 1]
  normalized <- pmin(pmax(normalized, 0), 1)

  # Simple proportional mapping to effective_range
  # (cut parameter now only affects transparency, not width)
  widths <- effective_range[1] + normalized * (effective_range[2] - effective_range[1])

  # Apply minimum threshold (set to min width)
  widths[abs_weights < minimum | is.na(abs_weights)] <- effective_range[1]

  widths
}
