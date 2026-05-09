#' @title Scaling Constants
#' @description Central scaling constants for parameter alignment between splot/soplot.
#' @name scale-constants
#' @keywords internal
NULL

#' qgraph Scaling Constants (Exact Values)
#'
#' Scaling constants that exactly replicate qgraph's visual formulas.
#' Used by splot() for qgraph-compatible network visualization.
#'
#' @format A list with the following elements:
#' \describe{
#'   \item{vsize_base}{Base multiplier in vsize formula: 8}
#'   \item{vsize_decay}{Decay constant in vsize formula: 80}
#'   \item{vsize_min}{Minimum added to vsize: 1}
#'   \item{vsize_factor}{Scale factor to convert vsize to user coordinates: 0.012}
#'   \item{esize_base}{Base multiplier in esize formula: 15}
#'   \item{esize_decay}{Decay constant in esize formula: 90}
#'   \item{esize_min}{Minimum added to esize: 1}
#'   \item{esize_unweighted}{Default edge width for unweighted networks: 2}
#'   \item{esize_scale}{Scale factor converting qgraph esize to line width: 0.27}
#'   \item{cent2edge_divisor}{Divisor in cent2edge formula: 17.5}
#'   \item{cent2edge_reference}{Reference value in cent2edge: 2.16}
#'   \item{cent2edge_plot_ref}{Plot reference size: 7}
#'   \item{curve_ref_diagonal}{Diagonal reference for curve normalization: sqrt(98)}
#'   \item{arrow_factor}{Arrow size scale factor: 0.04}
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
  # Visible but not overpowering at default arrow_size=1
  arrow_factor = 0.04
)

#' cograph Scaling Constants
#'
#' Central location for all scaling factors used in splot() and soplot().
#' These constants are calibrated to produce similar visual output to qgraph
#' when using equivalent parameter values.
#'
#' @details
#' The default scaling mode uses values calibrated to match qgraph visual appearance:
#' - `node_size = 6` in cograph should look similar to `vsize = 6` in qgraph
#' - `label_size = 1` uses cex-style multiplier (independent of node size)
#' - `arrow_size = 1` produces consistent arrows between splot and soplot
#'
#' Legacy mode preserves the original cograph v1.x behavior where:
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
#'   \item{edge_width_range}{Default output range for scaled edge widths}
#'   \item{edge_scale_mode}{Default edge scaling mode}
#'   \item{edge_cut_quantile}{Default cut quantile used by callers}
#'   \item{edge_width_default}{Default edge width when weights are unavailable}
#'   \item{arrow_factor}{Scale factor for arrow sizes}
#'   \item{arrow_default}{Default arrow size}
#'   \item{soplot_node_factor}{Node-size factor for soplot NPC coordinates}
#'   \item{tna_edge_color}{Default TNA edge color}
#' }
#'
#' @keywords internal
COGRAPH_SCALE <- list(
  # Node sizing: node_size=7 should look like qgraph vsize=7
  # Calibrated: 7 * 0.015 = 0.105 user coords (similar visual size to qgraph)
  node_factor = 0.015,
  node_default = 7,

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
  # Visible but not overpowering at default arrow_size=1
  arrow_factor = 0.04,
  arrow_default = 1,

  # soplot-specific: NPC coordinates
  # When converting node_size for soplot (NPC coords), use this factor
  # Calibrated: splot uses ~2.6 user coord range, soplot uses 1.0 NPC
  # To match: 0.015 / 2.6 ≈ 0.006
  soplot_node_factor = 0.006,

  # TNA default edge color (dark blue)
  tna_edge_color = "#003355"
)

#' Legacy Scaling Constants (Pre-v2.0 Behavior)
#'
#' Scaling constants that preserve the original cograph v1.x behavior.
#' Use `scaling = "legacy"` to enable these values.
#'
#' @format A list with the same structure as \code{COGRAPH_SCALE}
#' @keywords internal
COGRAPH_SCALE_LEGACY <- list(
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
    COGRAPH_SCALE_LEGACY
  } else {
    COGRAPH_SCALE
  }
}

# ============================================================================
# Visual-scale (device-dependent) constants
# ============================================================================

#' Reference plot region for visual-scale computation (inches, geometric
#' mean of `par("pin")`). 5.6 matches `par("pin")` at an RStudio 7x5" pane
#' with splot's tight default margins (`margins = c(0.1, 0.1, 0.1, 0.1)`):
#' pin ~ 6.96 x 4.96, geomean ~5.88. Recalibrated from the previous 5.9
#' (canvas geomean) to 5.6 (pin geomean) after diagnosis showed the
#' multiplier must track pin to stay in lockstep with user-coord nodes.
#' At the default device this produces `scale = 1.0` and existing plots
#' render unchanged.
#' @keywords internal
#' @noRd
VISUAL_SCALE_REFERENCE <- 5.6

#' Hard bounds on every visual-scale multiplier.
#'
#' Widened to `[0.35, 2.3]` after research into qgraph/ggraph/igraph showed
#' the prior `[0.55, 1.9]` was over-conservative:
#'
#' - Floor 0.35: at 800x800@300dpi (2.67" canvas) raw is 0.45, which now
#'   passes through unclamped — so labels/edges shrink to the *actual*
#'   ratio their canvas demands instead of being held artificially large.
#'   At extreme thumbnails (< 1" canvas) the floor still engages.
#' - Ceiling 2.3: at 1200x1200@96dpi (12.5" canvas) raw is 2.12, which now
#'   passes through — so labels grow proportionally at poster-size canvases
#'   instead of being artificially suppressed.
#'
#' Below ~0.8" canvas the layout is infeasible regardless of cex — users
#' should suppress labels/legend/title rather than rely on further scaling.
#' @keywords internal
#' @noRd
VISUAL_SCALE_CAP <- c(0.35, 2.3)

#' Edge-label-specific scale cap.
#'
#' Tighter ceiling than the main `VISUAL_SCALE_CAP` because edge labels are
#' *annotations* (weight values like ".19"), not primary content. They
#' should shrink with the canvas (to avoid overwhelming small plots) but
#' grow less aggressively than node labels at poster sizes — otherwise at
#' a 14"+ canvas with `vs$scale = 2.3`, edge-label cex reaches ~0.9 which
#' is visually competing with node labels rather than supporting them.
#' Ceiling 1.6 caps edge-label scaling roughly at the halfway point of
#' node-label growth; floor 0.35 matches the main cap so tiny canvases
#' aren't doubly-clamped. Only applied when the user passes
#' `edge_label_size` explicitly; the auto-default path uses
#' `EDGE_LABEL_NODE_CEX_FRACTION` coupling instead.
#' @keywords internal
#' @noRd
EDGE_LABEL_SCALE_CAP <- c(0.35, 1.6)

#' Fraction of the node label cex used for the auto-default edge label cex.
#'
#' When the user does not pass `edge_label_size` explicitly, the default
#' is `mean(label_cex) * EDGE_LABEL_NODE_CEX_FRACTION`. A fixed fraction
#' locks the node-to-edge-label cex ratio at `1 / fraction` on every
#' canvas (with 0.55 that is ~1.82x, calibrated to keep edge labels
#' readable without competing with node labels).
#' @keywords internal
#' @noRd
EDGE_LABEL_NODE_CEX_FRACTION <- 0.55

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
#' Unified edge width scaling function that supports multiple scaling modes
#' and output range specification.
#'
#' @param weights Numeric vector of edge weights.
#' @param esize Maximum edge size. If NULL, \code{range[2]} is used.
#' @param n_nodes Number of nodes. Accepted for caller compatibility; not used
#'   by this scaler.
#' @param directed Whether network is directed. Accepted for caller
#'   compatibility; not used by this scaler.
#' @param mode Scaling mode: "linear", "log", "sqrt", or "rank".
#' @param maximum Max weight for normalization. NULL for auto-detect.
#' @param minimum Min weight threshold. Edges below this get minimum width.
#' @param cut Accepted for caller compatibility. Width scaling is continuous
#'   in the current implementation; cutoff handling is performed by callers
#'   for other aesthetics such as transparency.
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
#' @keywords internal
scale_edge_widths <- function(weights,
                               esize = NULL,
                               n_nodes = NULL,
                               directed = FALSE,
                               mode = "linear",
                               maximum = NULL,
                               minimum = 0,
                               cut = NULL,
                               range = c(0.5, 4),
                               visual_scale = NULL) {
  if (length(weights) == 0) return(numeric(0))

  # Validate scale mode

  valid_modes <- c("linear", "log", "sqrt", "rank")
  if (!mode %in% valid_modes) {
    stop("edge_scale_mode must be one of: ", paste(valid_modes, collapse = ", "),
         ". Got: '", mode, "'", call. = FALSE)
  }

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

  # Device-dependent compensation: scale the mapped output (not the range),
  # so the "thinnest to thickest" rank mapping is preserved while absolute
  # lwd tracks the output canvas.
  if (!is.null(visual_scale) && !is.null(visual_scale$line) &&
      is.finite(visual_scale$line)) {
    widths <- widths * visual_scale$line
  }

  widths
}
