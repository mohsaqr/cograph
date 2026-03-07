#' Plot Multi-Cluster Multi-Layer Network
#'
#' Produces a two-layer hierarchical visualization of a clustered network.
#' The **bottom layer** shows every node arranged inside elliptical cluster
#' shells with full within-cluster and between-cluster edges drawn at the
#' individual-node level. The **top layer** collapses each cluster into a
#' single summary pie-chart node whose colored slice represents the proportion
#' of within-cluster flow, with edges carrying the aggregated between-cluster
#' weights. Dashed inter-layer lines connect each detail node to its
#' corresponding summary node, making the hierarchical mapping explicit.
#'
#' Use \code{plot_mcml} when you need a simultaneous micro/macro view of
#' cluster structure — the bottom layer reveals internal cluster dynamics while
#' the top layer provides a bird's-eye summary. For a flat multi-cluster plot
#' without the summary layer, see \code{\link{plot_mtna}}. For stacked
#' multilevel/multiplex layers, see \code{\link{plot_mlna}}.
#'
#' @details
#' \strong{Two workflows:}
#' \enumerate{
#'   \item \strong{Direct}: pass a weight matrix (or tna / cograph_network
#'     object) together with \code{cluster_list}. The function calls
#'     \code{\link{cluster_summary}} internally to compute aggregated weights.
#'   \item \strong{Pre-computed}: call \code{\link{cluster_summary}} yourself,
#'     inspect or modify the result, then pass the \code{cluster_summary}
#'     object as \code{x}. This avoids redundant computation when you plot
#'     the same clustering repeatedly with different visual settings.
#' }
#'
#' \strong{Mode:}
#' \itemize{
#'   \item \code{"weights"} (default) — displays raw aggregated edge values.
#'     Use this when the absolute magnitude of transitions matters.
#'   \item \code{"tna"} — row-normalizes the summary matrix to transition
#'     probabilities (rows sum to 1) and automatically enables edge labels
#'     on both layers (unless you explicitly set \code{edge_labels} or
#'     \code{summary_edge_labels} to \code{FALSE}).
#' }
#'
#' \strong{Layout logic:}
#' Bottom-layer clusters are arranged on a circle of radius \code{spacing},
#' flattened by the perspective \code{skew_angle}. Nodes inside each cluster
#' sit on a smaller circle of radius \code{shape_size * node_radius_scale}.
#' The top-layer summary nodes are placed on an oval above the bottom layer
#' whose proportions are controlled by \code{top_layer_scale}.
#'
#' @section Input Formats:
#' \code{x} accepts four types:
#' \describe{
#'   \item{\strong{matrix}}{A square numeric weight matrix with row/column
#'     names matching the node identifiers in \code{cluster_list}.}
#'   \item{\strong{tna}}{A TNA model object. The \code{$weights} matrix is
#'     extracted automatically.}
#'   \item{\strong{cograph_network}}{A cograph network object. Weights are
#'     extracted via \code{to_matrix()} and node metadata (display labels)
#'     is read from the \code{$nodes} data frame.}
#'   \item{\strong{cluster_summary}}{A pre-computed summary from
#'     \code{\link{cluster_summary}}. When this type is passed, the
#'     \code{cluster_list}, \code{aggregation}, and \code{nodes} parameters
#'     are ignored because the summary already contains everything needed.}
#' }
#'
#' @section Edge Types:
#' The plot contains four distinct edge categories, each with its own set
#' of visual parameters:
#' \describe{
#'   \item{\strong{Within-cluster (bottom)}}{Edges connecting nodes inside
#'     the same cluster shell. Controlled by \code{edge_width_range},
#'     \code{edge_alpha}, \code{edge_labels}, \code{edge_label_size},
#'     \code{edge_label_color}, and \code{edge_label_digits}.}
#'   \item{\strong{Between-cluster (bottom)}}{Edges from one cluster shell
#'     to another, drawn between shell borders. Controlled by
#'     \code{between_edge_width_range} and \code{between_edge_alpha}.}
#'   \item{\strong{Summary (top)}}{Edges between summary pie-chart nodes
#'     in the top layer. Controlled by \code{summary_edge_width_range},
#'     \code{summary_edge_alpha}, \code{summary_edge_labels},
#'     \code{summary_edge_label_size}, \code{summary_arrows}, and
#'     \code{summary_arrow_size}.}
#'   \item{\strong{Inter-layer (dashed)}}{Dashed lines connecting each
#'     detail node to its cluster's summary node. Controlled by
#'     \code{inter_layer_alpha}.}
#' }
#'
#' @section Customization Quick Reference:
#' \tabular{ll}{
#'   \strong{Visual element}       \tab \strong{Key parameters} \cr
#'   Cluster spacing / perspective  \tab \code{spacing}, \code{skew_angle} \cr
#'   Cluster shell appearance       \tab \code{shape_size}, \code{shell_alpha}, \code{shell_border_width}, \code{colors} \cr
#'   Detail nodes                   \tab \code{node_size}, \code{node_shape}, \code{node_border_color} \cr
#'   Detail labels                  \tab \code{show_labels}, \code{label_size}, \code{label_abbrev}, \code{label_color}, \code{label_position} \cr
#'   Summary nodes                  \tab \code{summary_size}, \code{cluster_shape}, \code{summary_border_color}, \code{summary_border_width} \cr
#'   Summary labels                 \tab \code{summary_labels}, \code{summary_label_size}, \code{summary_label_color}, \code{summary_label_position} \cr
#'   Within-cluster edges           \tab \code{edge_width_range}, \code{edge_alpha}, \code{edge_labels} \cr
#'   Between-cluster edges          \tab \code{between_edge_width_range}, \code{between_edge_alpha} \cr
#'   Summary edges                  \tab \code{summary_edge_width_range}, \code{summary_edge_alpha}, \code{summary_edge_labels}, \code{summary_arrows} \cr
#'   Inter-layer lines              \tab \code{inter_layer_alpha} \cr
#'   Top-layer layout               \tab \code{top_layer_scale}, \code{inter_layer_gap} \cr
#'   Title / legend                 \tab \code{title}, \code{subtitle}, \code{legend}, \code{legend_position} \cr
#' }
#'
#' @param x A weight matrix, \code{tna} object, \code{cograph_network}, or
#'   \code{cluster_summary} object. When a \code{cluster_summary} is provided
#'   (e.g., from \code{\link{cluster_summary}}), all aggregation has already
#'   been performed and the \code{cluster_list}, \code{aggregation}, and
#'   \code{nodes} parameters are ignored. See the \strong{Input Formats}
#'   section for details.
#' @param cluster_list How to assign nodes to clusters. Accepts:
#'   \itemize{
#'     \item A \strong{named list} of character vectors — each element
#'       contains the node names belonging to that cluster, and the list
#'       names become the cluster labels (e.g.,
#'       \code{list(GroupA = c("A","B"), GroupB = c("C","D"))}).
#'     \item A \strong{string} giving a column name in the node metadata
#'       (from a \code{cograph_network}) to use as the grouping variable.
#'     \item \code{NULL} — attempt auto-detection from common column names
#'       (\code{cluster}, \code{group}, etc.) in node metadata.
#'   }
#'   Ignored when \code{x} is a \code{cluster_summary}.
#' @param mode What values to display on edges:
#'   \describe{
#'     \item{\code{"weights"}}{(default) Shows raw aggregated edge values.
#'       Useful when absolute magnitudes (e.g., total co-occurrences) matter.}
#'     \item{\code{"tna"}}{Row-normalizes the summary matrix so each row
#'       sums to 1, producing transition probabilities. Automatically enables
#'       \code{edge_labels} and \code{summary_edge_labels} unless you
#'       explicitly set them to \code{FALSE}.}
#'   }
#' @param layer_spacing Vertical distance between the bottom and top layers.
#'   \code{NULL} (default) auto-calculates a gap that prevents overlap based
#'   on cluster positions and shell sizes. Increase for more vertical
#'   separation; decrease to make the plot more compact.
#' @param spacing Distance from the center to each cluster's position in the
#'   bottom layer. Larger values spread clusters farther apart. Default 3.
#' @param shape_size Radius of each cluster's elliptical shell in the bottom
#'   layer. Increase when nodes overlap or shells feel cramped. Default 1.2.
#' @param summary_size Size of the pie-chart summary nodes in the top layer.
#'   Controls the visual radius of each pie chart. Default 4.
#' @param skew_angle Perspective tilt angle in degrees (0–90). At 0 the
#'   bottom layer is viewed from directly above (fully circular); at 90 it
#'   collapses to a flat line. Values around 45–70 give a natural table-top
#'   perspective. Default 60.
#' @param aggregation Method for collapsing individual edge weights into
#'   between-cluster and within-cluster summaries:
#'   \describe{
#'     \item{\code{"sum"}}{(default) Total flow — appropriate when you care
#'       about the volume of all transitions between clusters.}
#'     \item{\code{"mean"}}{Average flow per node pair — useful when clusters
#'       differ in size and you want a size-normalized comparison.}
#'     \item{\code{"max"}}{Strongest single edge — highlights the dominant
#'       connection between each pair of clusters.}
#'   }
#'   Ignored when \code{x} is a \code{cluster_summary}.
#' @param minimum Edge weight threshold. Edges with absolute weight below
#'   this value are not drawn. Set to a small positive value (e.g., 0.01)
#'   to remove visual noise from near-zero edges. Default 0 (show all).
#' @param colors Character vector of colors for the clusters. The first
#'   color is applied to the first cluster, and so on. Must have length
#'   equal to the number of clusters, or it will be recycled. When
#'   \code{NULL} (default), colors are auto-generated from a colorblind-safe
#'   palette.
#' @param legend Logical. Whether to draw a legend mapping cluster names to
#'   colors. Default \code{TRUE}.
#' @param show_labels Logical. Show node labels in the bottom layer.
#'   Default \code{TRUE}. Set to \code{FALSE} for dense networks where
#'   labels create clutter.
#' @param nodes Node metadata data frame for custom display labels. Must
#'   contain a \code{label} column whose values match the row/column names
#'   of the weight matrix. If a \code{labels} column also exists, those
#'   values are used as display text (e.g., full names instead of codes).
#'   Display priority: \code{labels} column > \code{label} column.
#'   Ignored when \code{x} is a \code{cluster_summary} or
#'   \code{cograph_network} (which carries its own node metadata).
#' @param label_size Text size (\code{cex}) for bottom-layer node labels.
#'   \code{NULL} (default) auto-scales to 0.6. Increase for readability
#'   in publication figures; decrease for dense networks.
#' @param label_abbrev Controls label abbreviation to reduce overlap:
#'   \itemize{
#'     \item \code{NULL} — no abbreviation (show full labels).
#'     \item An \strong{integer} — truncate labels to this many characters.
#'     \item \code{"auto"} — adaptively abbreviates based on the total
#'       number of nodes: more nodes triggers shorter abbreviations.
#'   }
#' @param node_size Size of individual detail nodes in the bottom layer.
#'   This controls the pie-chart radius for each node. Default 1.8.
#' @param node_shape Shape for detail nodes in the bottom layer. Supported
#'   values: \code{"circle"}, \code{"square"}, \code{"diamond"},
#'   \code{"triangle"}. Can be a single value applied to all nodes or a
#'   character vector of length equal to the number of nodes (one shape
#'   per node). Default \code{"circle"}.
#' @param cluster_shape Shape for summary nodes in the top layer. Same
#'   supported values as \code{node_shape}. Can be a single value or a
#'   vector of length equal to the number of clusters. Default
#'   \code{"circle"}.
#' @param title Main plot title displayed above the figure. Default
#'   \code{NULL} (no title).
#' @param subtitle Subtitle displayed below the title. Default \code{NULL}
#'   (no subtitle).
#' @param title_size Text size (\code{cex.main}) for the title. Default 1.2.
#' @param subtitle_size Text size (\code{cex.sub}) for the subtitle.
#'   Default 0.9.
#' @param legend_position Where to place the legend: \code{"right"},
#'   \code{"left"}, \code{"top"}, \code{"bottom"}, or \code{"none"} to
#'   suppress it entirely. Default \code{"right"}.
#' @param legend_size Text size (\code{cex}) for legend labels. Default 0.7.
#' @param legend_pt_size Point size (\code{pt.cex}) for legend symbols.
#'   Default 1.2.
#' @param summary_labels Logical. Show cluster name labels next to the
#'   summary pie-chart nodes in the top layer. Default \code{TRUE}.
#' @param summary_label_size Text size for summary labels. Default 0.8.
#' @param summary_label_position Position of summary labels relative to
#'   nodes: 1 = below, 2 = left, 3 = above, 4 = right. Default 3 (above).
#' @param summary_label_color Color for summary labels. Default
#'   \code{"gray20"}.
#' @param summary_arrows Logical. Draw arrowheads on summary-layer directed
#'   edges. Set to \code{FALSE} for undirected networks. Default \code{TRUE}.
#' @param summary_arrow_size Size of arrowheads on summary edges. Default
#'   0.10.
#' @param between_arrows Logical. Draw arrowheads on between-cluster edges
#'   in the bottom layer. Default \code{FALSE}.
#' @param edge_width_range Numeric vector \code{c(min, max)} controlling the
#'   line-width range for \strong{within-cluster} edges in the bottom layer.
#'   The weakest edge gets \code{min} and the strongest gets \code{max}.
#'   Default \code{c(0.3, 1.3)}.
#' @param between_edge_width_range Numeric vector \code{c(min, max)} for
#'   \strong{between-cluster} edges in the bottom layer (shell-to-shell
#'   lines). Default \code{c(0.5, 2.0)}.
#' @param summary_edge_width_range Numeric vector \code{c(min, max)} for
#'   \strong{summary} edges in the top layer. Default \code{c(0.5, 2.0)}.
#' @param edge_alpha Transparency (0–1) for within-cluster edges. Lower
#'   values make these edges more subtle, keeping focus on between-cluster
#'   structure. Default 0.35.
#' @param between_edge_alpha Transparency (0–1) for between-cluster edges
#'   in the bottom layer. Default 0.6.
#' @param summary_edge_alpha Transparency (0–1) for summary-layer edges.
#'   Default 0.7.
#' @param inter_layer_alpha Transparency (0–1) for the dashed inter-layer
#'   lines connecting detail nodes to their summary node. Lower values make
#'   these scaffolding lines less visually dominant. Default 0.5.
#' @param edge_labels Logical. Show numeric weight labels on within-cluster
#'   edges. Default \code{FALSE} (automatically set to \code{TRUE} when
#'   \code{mode = "tna"}).
#' @param edge_label_size Text size for within-cluster edge labels.
#'   Default 0.5.
#' @param edge_label_color Color for within-cluster edge labels. Default
#'   \code{"gray40"}.
#' @param edge_label_digits Number of decimal places for edge weight labels
#'   on both layers. Default 2.
#' @param summary_edge_labels Logical. Show numeric weight labels on
#'   summary-layer edges. Default \code{FALSE} (automatically set to
#'   \code{TRUE} when \code{mode = "tna"}).
#' @param summary_edge_label_size Text size for summary edge labels.
#'   Default 0.6.
#' @param top_layer_scale Numeric vector \code{c(x_scale, y_scale)}
#'   controlling the horizontal and vertical radii of the oval on which
#'   summary nodes are placed, as multiples of \code{spacing}. Widen with
#'   \code{c(1.0, 0.25)} or flatten with \code{c(0.8, 0.15)} to adjust the
#'   top-layer shape. Default \code{c(0.8, 0.25)}.
#' @param inter_layer_gap Vertical gap between the top of the bottom layer
#'   and the bottom of the top layer, as a multiple of \code{spacing}.
#'   Increase to separate the layers more. Default 0.6.
#' @param node_radius_scale Radius of the circle on which nodes are
#'   arranged inside each cluster shell, as a fraction of
#'   \code{shape_size}. Increase to push nodes outward toward the shell
#'   border; decrease to pack them tighter. Default 0.55.
#' @param shell_alpha Fill transparency (0–1) for cluster shells. Higher
#'   values make shells more opaque, giving stronger visual grouping but
#'   potentially obscuring edges. Default 0.15.
#' @param shell_border_width Line width for cluster shell borders. Default 2.
#' @param node_border_color Border color for detail nodes in the bottom
#'   layer. Default \code{"gray30"}.
#' @param summary_border_color Border color for summary pie-chart nodes.
#'   Default \code{"gray20"}.
#' @param summary_border_width Border line width for summary nodes.
#'   Default 2.
#' @param label_color Text color for detail node labels. Default
#'   \code{"gray20"}.
#' @param label_position Position of detail node labels: 1 = below,
#'   2 = left, 3 = above, 4 = right. Default 3.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the \code{cluster_summary} object used for
#'   plotting. This object can be passed back to \code{plot_mcml()} to
#'   avoid recomputation, inspected with \code{print()}, or fed to
#'   \code{\link{as_tna}} for further analysis.
#'
#' @export
#'
#' @seealso
#' \code{\link{cluster_summary}} for pre-computing aggregated cluster data,
#' \code{\link{plot_mtna}} for flat multi-cluster visualization (no summary
#'   layer),
#' \code{\link{plot_mlna}} for stacked multilevel/multiplex layer
#'   visualization,
#' \code{\link{aggregate_weights}} for the low-level weight aggregation
#'   used internally,
#' \code{\link{detect_communities}} for algorithmic cluster detection
#'
#' @examples
#' # --- Setup: create a test matrix ---
#' mat <- matrix(runif(36), 6, 6)
#' diag(mat) <- 0
#' colnames(mat) <- rownames(mat) <- LETTERS[1:6]
#'
#' clusters <- list(
#'   Cluster1 = c("A", "B"),
#'   Cluster2 = c("C", "D"),
#'   Cluster3 = c("E", "F")
#' )
#'
#' # 1. Basic usage — pass matrix + clusters directly
#' plot_mcml(mat, clusters)
#'
#' # 2. Pre-compute with cluster_summary for reuse
#' cs <- cluster_summary(mat, clusters)
#' plot_mcml(cs)
#'
#' \dontrun{
#' # 3. TNA mode — transition probabilities with edge labels
#' plot_mcml(mat, clusters, mode = "tna")
#'
#' # 4. Custom shapes — different shape per cluster
#' plot_mcml(mat, clusters,
#'   node_shape = "diamond",
#'   cluster_shape = c("circle", "square", "triangle")
#' )
#'
#' # 5. Styling — custom colors, transparency, edge widths
#' plot_mcml(mat, clusters,
#'   colors = c("#1b9e77", "#d95f02", "#7570b3"),
#'   edge_alpha = 0.5,
#'   between_edge_alpha = 0.8,
#'   shell_alpha = 0.25,
#'   edge_width_range = c(0.5, 2.0)
#' )
#'
#' # 6. Edge labels on both layers
#' plot_mcml(mat, clusters,
#'   edge_labels = TRUE,
#'   summary_edge_labels = TRUE,
#'   edge_label_digits = 1
#' )
#'
#' # 7. Layout tuning — adjust spacing, perspective, and layer gap
#' plot_mcml(mat, clusters,
#'   spacing = 4,
#'   skew_angle = 45,
#'   top_layer_scale = c(1.0, 0.3),
#'   inter_layer_gap = 0.8
#' )
#'
#' # 8. With mean aggregation for size-normalized comparison
#' plot_mcml(mat, clusters,
#'   aggregation = "mean",
#'   title = "Mean-aggregated cluster network"
#' )
#'
#' # 9. Label abbreviation for dense networks
#' big_mat <- matrix(runif(400), 20, 20)
#' diag(big_mat) <- 0
#' colnames(big_mat) <- rownames(big_mat) <- paste0("Node_", 1:20)
#' big_clusters <- list(
#'   Alpha = paste0("Node_", 1:7),
#'   Beta  = paste0("Node_", 8:14),
#'   Gamma = paste0("Node_", 15:20)
#' )
#' plot_mcml(big_mat, big_clusters, label_abbrev = "auto")
#'
#' # 10. Minimal clean plot — no legend, no labels, no arrows
#' plot_mcml(mat, clusters,
#'   legend = FALSE,
#'   show_labels = FALSE,
#'   summary_labels = FALSE,
#'   summary_arrows = FALSE
#' )
#' }
plot_mcml <- function(
    x,
    cluster_list = NULL,
    mode = c("weights", "tna"),
    layer_spacing = NULL,
    spacing = 3,
    shape_size = 1.2,
    summary_size = 4,
    skew_angle = 60,
    aggregation = c("sum", "mean", "max"),
    minimum = 0,
    colors = NULL,
    legend = TRUE,
    show_labels = TRUE,
    nodes = NULL,
    label_size = NULL,
    label_abbrev = NULL,
    node_size = 1.8,
    node_shape = "circle",
    cluster_shape = "circle",
    # Title & Legend
    title = NULL,
    subtitle = NULL,
    title_size = 1.2,
    subtitle_size = 0.9,
    legend_position = "right",
    legend_size = 0.7,
    legend_pt_size = 1.2,
    # Summary labels
    summary_labels = TRUE,
    summary_label_size = 0.8,
    summary_label_position = 3,
    summary_label_color = "gray20",
    # Summary arrows
    summary_arrows = TRUE,
    summary_arrow_size = 0.10,
    # Edge control
    between_arrows = FALSE,
    edge_width_range = c(0.3, 1.3),
    between_edge_width_range = c(0.5, 2.0),
    summary_edge_width_range = c(0.5, 2.0),
    edge_alpha = 0.35,
    between_edge_alpha = 0.6,
    summary_edge_alpha = 0.7,
    inter_layer_alpha = 0.5,
    # Edge labels
    edge_labels = FALSE,
    edge_label_size = 0.5,
    edge_label_color = "gray40",
    edge_label_digits = 2,
    summary_edge_labels = FALSE,
    summary_edge_label_size = 0.6,
    # Layout fine-tuning
    top_layer_scale = c(0.8, 0.25),
    inter_layer_gap = 0.6,
    node_radius_scale = 0.55,
    # Shell styling
    shell_alpha = 0.15,
    shell_border_width = 2,
    # Node styling
    node_border_color = "gray30",
    summary_border_color = "gray20",
    summary_border_width = 2,
    # Label styling
    label_color = "gray20",
    label_position = 3,
    ...
) {
  aggregation <- match.arg(aggregation)
  mode <- match.arg(mode)

  # For mode = "tna", show edge labels by default (like tplot/splot with tna)
  # Check if user explicitly set these parameters
 explicit_args <- names(match.call())
  if (mode == "tna") {
    if (!"edge_labels" %in% explicit_args) {
      edge_labels <- TRUE
    }
    if (!"summary_edge_labels" %in% explicit_args) {
      summary_edge_labels <- TRUE
    }
  }

  # ============================================================================
  # Get or compute cluster_summary
  # ============================================================================

  if (inherits(x, "cluster_summary")) {
    cs <- x
  } else {
    # Extract nodes_df for display labels
    nodes_df <- NULL
    if (inherits(x, "cograph_network")) {
      nodes_df <- x$nodes
    }
    if (is.data.frame(nodes)) {
      nodes_df <- nodes
    }

    # Map aggregation to method
    cs <- cluster_summary(x, cluster_list, method = aggregation, type = "tna",
                          compute_within = TRUE)

    # Store nodes_df and display_labels for visualization
    cs$nodes_df <- nodes_df
  }

  # ============================================================================
  # Extract data from cluster_summary
  # ============================================================================

  cluster_list <- cs$clusters
  cluster_names <- names(cluster_list)
  n_clusters <- cs$meta$n_clusters
  n <- cs$meta$n_nodes

  # Get original weight matrix for within-cluster visualization
  # We need raw weights, so re-extract if needed
  if (inherits(x, "cluster_summary")) {
    # Need to get weights from somewhere - use the input
    # For cluster_summary, we stored processed weights, need raw
    # Use within$X$weights which are raw (before normalization)
    weights <- NULL  # Will use within data directly
  } else if (inherits(x, "cograph_network")) {
    weights <- if (!is.null(x$weights)) x$weights else to_matrix(x)
  } else if (inherits(x, "tna")) {
    weights <- x$weights
  } else {
    weights <- x
  }

  # Get node labels
  if (!is.null(weights)) {
    lab <- rownames(weights)
    if (is.null(lab)) lab <- as.character(seq_len(n))
  } else {
    # Reconstruct from cluster_list
    lab <- unlist(cluster_list, use.names = FALSE)
  }

  # Get display labels from nodes_df
  nodes_df <- cs$nodes_df
  display_labels <- if (!is.null(nodes_df)) {
    if ("labels" %in% names(nodes_df)) {
      nodes_df$labels
    } else if ("label" %in% names(nodes_df)) {
      nodes_df$label
    } else {
      lab
    }
  } else {
    lab
  }

  # Get cluster indices
  cluster_idx <- lapply(cluster_list, function(nodes_vec) match(nodes_vec, lab))

  # Between-cluster weights (processed based on type)
  bw <- cs$between$weights

  # Add self-loop values (within-cluster totals) to diagonal
  # This represents the total transition probability staying within each cluster
  if (!is.null(cs$within)) {
    for (cl_name in names(cs$within)) {
      if (cl_name %in% rownames(bw)) {
        within_w <- cs$within[[cl_name]]$weights
        # Sum all within-cluster transitions (normalized)
        diag_val <- sum(within_w, na.rm = TRUE) / nrow(within_w)
        bw[cl_name, cl_name] <- diag_val
      }
    }
  }

  # For edge labels, use processed weights for display
  sw_labels <- bw

  # Expand node_shape to vector if needed
  node_shape <- rep_len(node_shape, n)

  # Colors
  pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
           "#0072B2", "#D55E00", "#CC79A7", "#999999")
  if (is.null(colors)) colors <- rep_len(pal, n_clusters)

  # Expand cluster_shape to vector if needed
  cluster_shape <- rep_len(cluster_shape, n_clusters)

  # ============================================================================
  # Layout computation
  # ============================================================================

  # Perspective: table view (flat plane seen from above at angle)
  skew_rad <- skew_angle * pi / 180
  compress <- cos(skew_rad)  # flatten y for table-like view

  # Bottom layer: cluster centers (flat plane)
  angles <- pi/2 - (seq_len(n_clusters) - 1) * 2 * pi / n_clusters
  bx_base <- spacing * cos(angles)
  by_base <- spacing * sin(angles)
  bx <- bx_base
  by <- by_base * compress

  # Auto-calculate layer_spacing to ensure no overlap
  bottom_top <- max(by) + shape_size * compress
  bottom_bottom <- min(by) - shape_size * compress

  if (is.null(layer_spacing)) {
    layer_spacing <- (bottom_top - bottom_bottom) + 2
  }

  # Top layer positioned above bottom layer
  gap <- spacing * inter_layer_gap
  top_base_y <- bottom_top + gap

  # Top layer: oval layout with spaced nodes
  top_radius_x <- spacing * top_layer_scale[1]
  top_radius_y <- spacing * top_layer_scale[2]

  tx <- top_radius_x * cos(angles)
  ty <- top_radius_y * sin(angles) + top_base_y

  # Edge weight scaling
  max_sw <- max(bw)
  if (max_sw == 0) max_sw <- 1

  # For within-cluster edges, need max from raw weights
  if (!is.null(weights)) {
    max_w <- max(abs(weights), na.rm = TRUE)
    if (is.na(max_w) || max_w == 0) max_w <- 1
  } else {
    # Get from within data
    max_w <- 1
    if (!is.null(cs$within)) {
      all_within_w <- unlist(lapply(cs$within, function(w) w$weights))
      if (length(all_within_w) > 0) {
        max_w <- max(abs(all_within_w), na.rm = TRUE)
        if (is.na(max_w) || max_w == 0) max_w <- 1
      }
    }
  }

  # Helper: get point on ellipse edge facing target
  shell_edge <- function(cx, cy, tx, ty, rx, ry) {
    a <- atan2((ty - cy) / ry, (tx - cx) / rx)
    c(cx + rx * cos(a), cy + ry * sin(a))
  }

  # ============================================================================
  # Plot setup
  # ============================================================================

  # Plot limits (tight padding)
  pad <- shape_size * 0.3
  xlim <- range(c(bx, tx)) + c(-shape_size - pad, shape_size + pad)
  ylim <- range(c(by, ty)) + c(-shape_size * compress - pad, shape_size + pad)

  old_par <- graphics::par(mar = c(0.2, 0.2, 0.2, 0.2))
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

  # ============================================================================
  # DRAW INTER-LAYER CONNECTIONS FIRST (behind everything)
  # ============================================================================

  node_positions <- vector("list", n_clusters)
  node_r <- shape_size * node_radius_scale

  for (i in seq_len(n_clusters)) {
    idx <- cluster_idx[[i]]
    n_nodes <- length(idx)
    if (n_nodes == 1) {
      node_positions[[i]] <- list(x = bx[i], y = by[i], angles = pi/2)
    } else {
      na <- pi/2 - (seq_len(n_nodes) - 1) * 2 * pi / n_nodes
      node_x <- node_r * cos(na)
      node_y <- node_r * sin(na) * compress
      node_positions[[i]] <- list(
        x = bx[i] + node_x,
        y = by[i] + node_y,
        angles = na  # Store original angles for label positioning
      )
    }
    # Draw dashed line from each node to summary node
    for (j in seq_along(node_positions[[i]]$x)) {
      graphics::segments(
        node_positions[[i]]$x[j], node_positions[[i]]$y[j],
        tx[i], ty[i],
        col = grDevices::adjustcolor(colors[i], inter_layer_alpha),
        lty = 2, lwd = 1
      )
    }
  }

  # ============================================================================
  # TOP LAYER (summary network)
  # ============================================================================

  summary_arrow_sz <- summary_arrow_size
  pie_radius <- 0.35  # Pie chart radius in plot units

  # 1. Draw summary nodes as PIE CHARTS first (so edges draw on top)
  for (i in seq_len(n_clusters)) {
    # Self-loop proportion (within-cluster) vs between-cluster
    self_val <- bw[i, i]
    other_val <- sum(bw[i, -i])
    total <- self_val + other_val

    if (total > 0) {
      self_prop <- self_val / total
    } else {
      self_prop <- 0
    }

    # Draw "other" slice first (light gray background)
    if (self_prop < 1) {
      theta <- seq(0, 2 * pi, length.out = 60)
      graphics::polygon(tx[i] + pie_radius * cos(theta),
                        ty[i] + pie_radius * sin(theta),
                        col = "gray90", border = NA)
    }

    # Draw "self" slice (cluster color) - starts from top
    if (self_prop > 0.001) {
      start_angle <- pi / 2
      end_angle <- start_angle - self_prop * 2 * pi
      n_pts <- max(10, round(50 * self_prop))
      angles <- seq(start_angle, end_angle, length.out = n_pts)
      slice_x <- c(tx[i], tx[i] + pie_radius * cos(angles), tx[i])
      slice_y <- c(ty[i], ty[i] + pie_radius * sin(angles), ty[i])
      graphics::polygon(slice_x, slice_y, col = colors[i], border = NA)
    }

    # Draw border circle on top
    theta <- seq(0, 2 * pi, length.out = 60)
    graphics::lines(tx[i] + pie_radius * cos(theta),
                    ty[i] + pie_radius * sin(theta),
                    col = summary_border_color, lwd = summary_border_width)
  }

  # 2. Draw summary edges ON TOP of pies (arrows visible at pie edge)
  if (max_sw > 0) {
    for (i in seq_len(n_clusters)) {
      for (j in seq_len(n_clusters)) {
        if (i != j && bw[i, j] > minimum) {
          lwd <- summary_edge_width_range[1] +
            (summary_edge_width_range[2] - summary_edge_width_range[1]) *
            bw[i, j] / max_sw
          edge_col <- grDevices::adjustcolor(colors[i], summary_edge_alpha)
          angle <- atan2(ty[j] - ty[i], tx[j] - tx[i])

          # Start line at source pie edge, end arrow at target pie edge
          src_x <- tx[i] + pie_radius * cos(angle)
          src_y <- ty[i] + pie_radius * sin(angle)
          tip_x <- tx[j] - pie_radius * cos(angle)
          tip_y <- ty[j] - pie_radius * sin(angle)

          if (summary_arrows) {
            line_end_x <- tip_x - summary_arrow_sz * cos(angle)
            line_end_y <- tip_y - summary_arrow_sz * sin(angle)
            graphics::segments(src_x, src_y, line_end_x, line_end_y,
                               col = edge_col, lwd = lwd)
            arrow_col <- colors[i]  # opaque arrow so line doesn't bleed through
            draw_arrow_base(tip_x, tip_y, angle, summary_arrow_sz,
                            col = arrow_col, border = arrow_col, lwd = lwd)
          } else {
            graphics::segments(src_x, src_y, tip_x, tip_y,
                               col = edge_col, lwd = lwd)
          }

          if (summary_edge_labels) {
            # Place label at 70% along edge (near target, avoids overlap)
            lbl_x <- src_x + (tip_x - src_x) * 0.7
            lbl_y <- src_y + (tip_y - src_y) * 0.7
            # Offset slightly perpendicular to edge
            perp <- angle + pi / 2
            lbl_x <- lbl_x + 0.08 * cos(perp)
            lbl_y <- lbl_y + 0.08 * sin(perp)
            graphics::text(lbl_x, lbl_y,
                           labels = round(sw_labels[i, j], edge_label_digits),
                           cex = summary_edge_label_size,
                           col = edge_label_color)
          }
        }
      }
    }
  }

  # 3. Draw self-loops on summary pies
  if (max_sw > 0) {
    loop_radius <- 0.15
    for (i in seq_len(n_clusters)) {
      if (bw[i, i] > minimum) {
        lwd <- summary_edge_width_range[1] +
          (summary_edge_width_range[2] - summary_edge_width_range[1]) *
          bw[i, i] / max_sw
        edge_col <- grDevices::adjustcolor(colors[i], summary_edge_alpha)

        # Loop rotation pointing outward from pie arrangement center
        loop_rot <- atan2(ty[i] - mean(ty), tx[i] - mean(tx))

        # Loop center placed outside the pie
        loop_cx <- tx[i] + (pie_radius + loop_radius) * cos(loop_rot)
        loop_cy <- ty[i] + (pie_radius + loop_radius) * sin(loop_rot)

        # Draw arc (~270 degrees, open toward the pie)
        n_pts <- 40
        arc_start <- loop_rot + pi + 0.75
        arc_end <- loop_rot + pi - 0.25
        if (arc_end < arc_start) arc_end <- arc_end + 2 * pi
        angles <- seq(arc_start, arc_end, length.out = n_pts)
        loop_x <- loop_cx + loop_radius * cos(angles)
        loop_y <- loop_cy + loop_radius * sin(angles)

        graphics::lines(loop_x, loop_y, col = edge_col, lwd = lwd)

        # Arrow at start of arc, pointing toward pie center
        if (summary_arrows) {
          arr_angle <- atan2(ty[i] - loop_y[1], tx[i] - loop_x[1])
          arrow_col <- colors[i]
          draw_arrow_base(loop_x[1], loop_y[1], arr_angle,
                          summary_arrow_sz * 0.8, col = arrow_col)
        }

        # Loop label at the outward tip of the loop
        if (summary_edge_labels) {
          lbl_x <- loop_cx + loop_radius * 1.3 * cos(loop_rot)
          lbl_y <- loop_cy + loop_radius * 1.3 * sin(loop_rot)
          graphics::text(lbl_x, lbl_y,
                         labels = round(sw_labels[i, i], edge_label_digits),
                         cex = summary_edge_label_size,
                         col = edge_label_color)
        }
      }
    }
  }

  # 4. Summary labels - perpendicular to loop direction (solution 5)
  if (summary_labels) {
    lbl_offset <- 0.45
    for (i in seq_len(n_clusters)) {
      if (summary_label_position == 1) {
        lbl_x <- tx[i]; lbl_y <- ty[i] - lbl_offset
      } else if (summary_label_position == 2) {
        lbl_x <- tx[i] - lbl_offset; lbl_y <- ty[i]
      } else if (summary_label_position == 4) {
        lbl_x <- tx[i] + lbl_offset; lbl_y <- ty[i]
      } else {
        lbl_x <- tx[i]; lbl_y <- ty[i] + lbl_offset
      }
      graphics::text(lbl_x, lbl_y,
                     labels = cluster_names[i],
                     cex = summary_label_size,
                     col = summary_label_color)
    }
  }

  # ============================================================================
  # BOTTOM LAYER (detailed clusters)
  # ============================================================================

  # Between-cluster edges (shell to shell)
  shell_rx <- shape_size
  shell_ry <- shape_size * compress
  between_arrow_sz <- 0.12
  if (max_sw > 0) {
    for (i in seq_len(n_clusters)) {
      for (j in seq_len(n_clusters)) {
        if (i != j && bw[i, j] > minimum) {
          p1 <- shell_edge(bx[i], by[i], bx[j], by[j], shell_rx, shell_ry)
          p2 <- shell_edge(bx[j], by[j], bx[i], by[i], shell_rx, shell_ry)
          lwd <- between_edge_width_range[1] +
            (between_edge_width_range[2] - between_edge_width_range[1]) *
            bw[i, j] / max_sw
          edge_col <- grDevices::adjustcolor(colors[i], between_edge_alpha)
          if (between_arrows) {
            angle <- atan2(p2[2] - p1[2], p2[1] - p1[1])
            tip_x <- p2[1]
            tip_y <- p2[2]
            line_end_x <- tip_x - between_arrow_sz * cos(angle)
            line_end_y <- tip_y - between_arrow_sz * sin(angle)
            graphics::segments(p1[1], p1[2], line_end_x, line_end_y,
                               col = edge_col, lwd = lwd)
            draw_arrow_base(tip_x, tip_y, angle, between_arrow_sz,
                            col = edge_col, border = edge_col, lwd = lwd)
          } else {
            graphics::segments(p1[1], p1[2], p2[1], p2[2],
                               col = edge_col, lwd = lwd)
          }
        }
      }
    }
  }

  # Cluster shells and nodes
  for (i in seq_len(n_clusters)) {
    idx <- cluster_idx[[i]]
    n_nodes <- length(idx)
    cl_name <- cluster_names[i]

    # Shell (ellipse for table-view perspective)
    theta <- seq(0, 2 * pi, length.out = 60)
    shell_x <- shape_size * cos(theta)
    shell_y <- shape_size * sin(theta) * compress
    graphics::polygon(
      bx[i] + shell_x,
      by[i] + shell_y,
      border = colors[i],
      col = grDevices::adjustcolor(colors[i], shell_alpha),
      lwd = shell_border_width
    )

    # Node positions (use pre-computed)
    nx <- node_positions[[i]]$x
    ny <- node_positions[[i]]$y

    # Within-cluster edges
    if (n_nodes > 1) {
      # Get within-cluster weights
      within_w <- if (!is.null(cs$within) && cl_name %in% names(cs$within)) {
        cs$within[[cl_name]]$weights
      } else if (!is.null(weights)) { # nocov start
        w <- weights[idx, idx]
        diag(w) <- 0
        w # nocov end
      } else {
        NULL
      }

      if (!is.null(within_w)) {
        # Node visual radius and arrow size
        node_vis_r <- node_size * 0.04
        arrow_size <- 0.06

        for (j in seq_len(n_nodes)) {
          for (k in seq_len(n_nodes)) {
            if (j != k) {
              w <- within_w[j, k]
              if (!is.na(w) && w > minimum) {
                lwd <- edge_width_range[1] +
                  (edge_width_range[2] - edge_width_range[1]) * w / max_w
                edge_col <- grDevices::adjustcolor(colors[i], edge_alpha)

                # Calculate edge angle
                angle <- atan2(ny[k] - ny[j], nx[k] - nx[j])

                # Arrow tip at node edge
                tip_x <- nx[k] - node_vis_r * cos(angle)
                tip_y <- ny[k] - node_vis_r * sin(angle)

                # Line ends at arrow base
                line_end_x <- tip_x - arrow_size * cos(angle)
                line_end_y <- tip_y - arrow_size * sin(angle)

                # Draw edge line
                graphics::segments(nx[j], ny[j], line_end_x, line_end_y,
                                   col = edge_col, lwd = lwd)

                # Draw filled arrow using splot style
                draw_arrow_base(tip_x, tip_y, angle, arrow_size,
                                col = edge_col, border = edge_col, lwd = lwd)

                # Edge label - position at 1/3 along edge (closer to source)
                if (edge_labels) {
                  lbl_x <- nx[j] + (nx[k] - nx[j]) * 0.35
                  lbl_y <- ny[j] + (ny[k] - ny[j]) * 0.35
                  graphics::text(lbl_x, lbl_y,
                                 labels = round(w, edge_label_digits),
                                 cex = edge_label_size,
                                 col = edge_label_color)
                }
              }
            }
          }
        }
      }
    }

    # Nodes as PIE CHARTS showing self-transition proportion
    node_pie_r <- node_size * 0.035  # Pie radius in plot units

    for (ni in seq_along(nx)) {
      # Get self-transition proportion for this node
      self_val <- 0
      other_val <- 1
      if (!is.null(within_w)) {
        node_row <- within_w[ni, ]
        self_val <- within_w[ni, ni]  # Diagonal = self-transition
        other_val <- sum(node_row) - self_val
        total <- self_val + other_val
        if (total > 0) {
          self_prop <- self_val / total
        } else {
          self_prop <- 0
        }
      } else {
        self_prop <- 0
      }

      # Draw "other" slice (light version of cluster color)
      if (self_prop < 1) {
        theta <- seq(0, 2 * pi, length.out = 40)
        graphics::polygon(nx[ni] + node_pie_r * cos(theta),
                          ny[ni] + node_pie_r * sin(theta),
                          col = grDevices::adjustcolor(colors[i], 0.3),
                          border = NA)
      }

      # Draw "self" slice (full cluster color)
      if (self_prop > 0.001) { # nocov start
        start_angle <- pi / 2
        end_angle <- start_angle - self_prop * 2 * pi
        n_pts <- max(10, round(40 * self_prop))
        angles <- seq(start_angle, end_angle, length.out = n_pts)
        slice_x <- c(nx[ni], nx[ni] + node_pie_r * cos(angles), nx[ni])
        slice_y <- c(ny[ni], ny[ni] + node_pie_r * sin(angles), ny[ni])
        graphics::polygon(slice_x, slice_y, col = colors[i], border = NA)
      } # nocov end

      # Border
      theta <- seq(0, 2 * pi, length.out = 40)
      graphics::lines(nx[ni] + node_pie_r * cos(theta),
                      ny[ni] + node_pie_r * sin(theta),
                      col = node_border_color, lwd = 1.5)
    }

    # Node labels - position on side (left or right only)
    if (isTRUE(show_labels)) {
      lbl_text <- display_labels[idx]
      if (!is.null(label_abbrev)) {
        lbl_text <- abbrev_label(lbl_text, label_abbrev, n)
      }
      lbl_cex <- if (is.null(label_size)) 0.6 else label_size

      # Use original angles for outward direction, but only left or right
      node_angles <- node_positions[[i]]$angles
      for (ni in seq_along(nx)) {
        angle <- node_angles[ni]
        # Only use left (pos=2) or right (pos=4) based on angle
        if (abs(angle) < pi/2) {
          lbl_pos <- 4  # right
        } else {
          lbl_pos <- 2  # left
        }
        graphics::text(nx[ni], ny[ni], labels = lbl_text[ni], cex = lbl_cex,
                       pos = lbl_pos, offset = 0.4, col = label_color)
      }
    }
  }

  # Title and subtitle
  if (!is.null(title)) {
    graphics::title(main = title, cex.main = title_size)
  }
  if (!is.null(subtitle)) {
    graphics::title(sub = subtitle, cex.sub = subtitle_size, line = -0.5)
  }

  # Legend (positioned based on legend_position)
  if (legend && legend_position != "none") {
    legend_x <- switch(legend_position,
      "right" = max(bx) + shape_size * 0.5,
      "left" = min(bx) - shape_size * 0.5,
      "top" = mean(c(min(bx), max(bx))),
      "bottom" = mean(c(min(bx), max(bx))),
      max(bx) + shape_size * 0.5  # default to right
    )
    legend_y <- switch(legend_position,
      "right" = mean(c(max(by), min(ty))),
      "left" = mean(c(max(by), min(ty))),
      "top" = max(ty) + 1,
      "bottom" = min(by) - 1,
      mean(c(max(by), min(ty)))  # default
    )
    legend_horiz <- legend_position %in% c("top", "bottom")
    legend_xjust <- switch(legend_position,
      "right" = 0,
      "left" = 1,
      "top" = 0.5,
      "bottom" = 0.5,
      0
    )
    legend_yjust <- switch(legend_position,
      "right" = 0.5,
      "left" = 0.5,
      "top" = 0,
      "bottom" = 1,
      0.5
    )

    graphics::legend(
      x = legend_x,
      y = legend_y,
      legend = cluster_names, pch = 21, pt.bg = colors,
      col = "gray30", pt.cex = legend_pt_size, cex = legend_size, bty = "n",
      xjust = legend_xjust, yjust = legend_yjust, horiz = legend_horiz
    )
  }

  invisible(cs)
}

#' mcml - Deprecated alias for cluster_summary
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use \code{\link{cluster_summary}} instead. This function is provided for
#' backward compatibility only.
#'
#' @param x Weight matrix, tna object, cograph_network, or cluster_summary object
#' @param cluster_list Named list of node vectors per cluster
#' @param aggregation How to aggregate edge weights: "sum", "mean", "max"
#' @param as_tna Logical. If TRUE, return a tna-compatible object
#' @param nodes Node metadata
#' @param within Logical. Compute within-cluster matrices
#' @return A cluster_summary object (or tna if as_tna = TRUE)
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' mat <- matrix(runif(100, 0, 0.3), 10, 10)
#' diag(mat) <- 0
#' colnames(mat) <- rownames(mat) <- paste0("N", 1:10)
#' clusters <- list(C1 = paste0("N", 1:5), C2 = paste0("N", 6:10))
#' mcml(mat, clusters)
#' }
mcml <- function(x,
                 cluster_list = NULL,
                 aggregation = c("sum", "mean", "max"),
                 as_tna = FALSE,
                 nodes = NULL,
                 within = TRUE) {

  aggregation <- match.arg(aggregation)

  # Call cluster_summary with mapped parameters
  cs <- cluster_summary(x, cluster_list, method = aggregation, type = "tna",
                        compute_within = within)

  # Store nodes metadata for display labels (backward compat)
  if (is.data.frame(nodes)) {
    cs$nodes_df <- nodes
  } else if (inherits(x, "cograph_network") && !is.null(x$nodes)) {
    cs$nodes_df <- x$nodes
  }

  if (as_tna) {
    return(as_tna(cs))
  }

  cs
}
