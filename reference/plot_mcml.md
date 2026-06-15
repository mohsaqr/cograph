# Plot Multi-Cluster Multi-Layer Network

Produces a two-layer hierarchical visualization of a clustered network.
The **bottom layer** shows every node arranged inside elliptical cluster
shells with full within-cluster and between-cluster edges drawn at the
individual-node level. The **top layer** collapses each cluster into a
single summary pie-chart node whose colored slice represents, by
default, the cluster's share of the initial state distribution (see
`summary_pie` for the alternative self-retention interpretation), with
edges carrying the aggregated between-cluster weights. Dashed
inter-layer lines connect each detail node to its corresponding summary
node, making the hierarchical mapping explicit.

## Usage

``` r
plot_mcml(
  x,
  cluster_list = NULL,
  mode = c("weights", "tna"),
  theme = c("classic", "rich", "light"),
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
  node_size = 2.4,
  node_shape = "circle",
  cluster_shape = "circle",
  title = NULL,
  subtitle = NULL,
  title_size = 1.2,
  subtitle_size = 0.9,
  legend_position = "right",
  legend_size = 0.7,
  legend_pt_size = 1.2,
  summary_labels = TRUE,
  summary_label_size = 0.8,
  summary_label_position = 3,
  summary_label_color = "gray20",
  summary_arrows = TRUE,
  summary_arrow_size = 0.1,
  node_donut = NULL,
  node_donut_inner_ratio = 0.55,
  summary_donut_inner_ratio = 0.6,
  summary_donut_show_value = FALSE,
  curved_edges = NULL,
  summary_curve = NULL,
  summary_pie = c("inits", "self"),
  edge_color_by = c("auto", "cluster", "sign"),
  edge_positive_color = "#2E7D32",
  edge_negative_color = "#C62828",
  between_arrows = FALSE,
  edge_width_range = c(0.3, 1.3),
  between_edge_width_range = c(0.5, 2),
  summary_edge_width_range = c(0.5, 2),
  edge_alpha = 0.35,
  between_edge_alpha = 0.6,
  summary_edge_alpha = 0.7,
  inter_layer_alpha = 0.5,
  edge_labels = FALSE,
  edge_label_size = 0.5,
  edge_label_color = "gray40",
  edge_label_digits = 2,
  summary_edge_labels = FALSE,
  summary_edge_label_size = 0.6,
  top_layer_scale = c(0.8, 0.25),
  inter_layer_gap = 0.6,
  node_radius_scale = 0.55,
  shell_alpha = 0.15,
  shell_border_width = 0.75,
  node_border_color = "gray30",
  node_border_width = 0.4,
  summary_border_color = "gray20",
  summary_border_width = 0.6,
  label_color = "gray20",
  label_position = 3,
  directed = NULL,
  ...
)
```

## Arguments

- x:

  A weight matrix, `tna` object, `cograph_network`, `cluster_summary`,
  or `mcml`/`mcml_pc` object (the latter from
  `Nestimate::build_mcml_pc()`, rendered undirected via its
  `meta$directed` flag). When a `cluster_summary` is provided (e.g.,
  from [`csum`](https://sonsoles.me/cograph/reference/csum.md)), all
  aggregation has already been performed and the `cluster_list`,
  `aggregation`, and `nodes` parameters are ignored. See the **Input
  Formats** section for details.

- cluster_list:

  How to assign nodes to clusters. Accepts:

  - A **named list** of character vectors — each element contains the
    node names belonging to that cluster, and the list names become the
    cluster labels (e.g.,
    `list(GroupA = c("A","B"), GroupB = c("C","D"))`).

  - A **string** giving a column name in the node metadata (from a
    `cograph_network`) to use as the grouping variable.

  - `NULL` — attempt auto-detection from common column names (`cluster`,
    `group`, etc.) in node metadata.

  Ignored when `x` is a `cluster_summary`.

- mode:

  What values to display on edges:

  `"weights"`

  :   (default) Shows raw aggregated edge values. Useful when absolute
      magnitudes (e.g., total co-occurrences) matter.

  `"tna"`

  :   Row-normalizes the summary matrix so each row sums to 1, producing
      transition probabilities. Automatically enables `edge_labels` and
      `summary_edge_labels` unless you explicitly set them to `FALSE`.

- theme:

  Visual preset controlling node and edge styling. One of:

  `"classic"`

  :   (default) The historical look — pie-chart nodes and straight
      summary edges, with thin borders and slightly larger detail nodes.

  `"rich"`

  :   Donut nodes on both layers plus curved (qgraph-style) summary
      edges and splot self-loops.

  `"light"`

  :   Like `"rich"` but with no cluster-shell outline and a softer shell
      fill.

  The granular style arguments (`node_donut`, `curved_edges`) override
  the preset when supplied.

- layer_spacing:

  Vertical distance between the bottom and top layers. `NULL` (default)
  auto-calculates a gap that prevents overlap based on cluster positions
  and shell sizes. Increase for more vertical separation; decrease to
  make the plot more compact.

- spacing:

  Distance from the center to each cluster's position in the bottom
  layer. Larger values spread clusters farther apart. Default 3.

- shape_size:

  Radius of each cluster's elliptical shell in the bottom layer.
  Increase when nodes overlap or shells feel cramped. Default 1.2.

- summary_size:

  Size of the pie-chart summary nodes in the top layer. Controls the
  visual radius of each pie chart. Default 4.

- skew_angle:

  Perspective tilt angle in degrees (0–90). At 0 the bottom layer is
  viewed from directly above (fully circular); at 90 it collapses to a
  flat line. Values around 45–70 give a natural table-top perspective.
  Default 60.

- aggregation:

  Method for collapsing individual edge weights into between-cluster and
  within-cluster summaries:

  `"sum"`

  :   (default) Total flow — appropriate when you care about the volume
      of all transitions between clusters.

  `"mean"`

  :   Average flow per node pair — useful when clusters differ in size
      and you want a size-normalized comparison.

  `"max"`

  :   Strongest single edge — highlights the dominant connection between
      each pair of clusters.

  Ignored when `x` is a `cluster_summary`.

- minimum:

  Edge weight threshold. Edges with absolute weight below this value are
  not drawn. Set to a small positive value (e.g., 0.01) to remove visual
  noise from near-zero edges. Default 0 (show all).

- colors:

  Character vector of colors for the clusters. The first color is
  applied to the first cluster, and so on. Must have length equal to the
  number of clusters, or it will be recycled. When `NULL` (default),
  colors are auto-generated from a colorblind-safe palette.

- legend:

  Logical. Whether to draw a legend mapping cluster names to colors.
  Default `TRUE`.

- show_labels:

  Logical. Show node labels in the bottom layer. Default `TRUE`. Set to
  `FALSE` for dense networks where labels create clutter.

- nodes:

  Node metadata data frame for custom display labels. Must contain a
  `label` column whose values match the row/column names of the weight
  matrix. If a `labels` column also exists, those values are used as
  display text (e.g., full names instead of codes). Display priority:
  `labels` column \> `label` column. Ignored when `x` is a
  `cluster_summary` or `cograph_network` (which carries its own node
  metadata).

- label_size:

  Text size (`cex`) for bottom-layer node labels. `NULL` (default)
  auto-scales to 0.6. Increase for readability in publication figures;
  decrease for dense networks.

- label_abbrev:

  Controls label abbreviation to reduce overlap:

  - `NULL` — no abbreviation (show full labels).

  - An **integer** — truncate labels to this many characters.

  - `"auto"` — adaptively abbreviates based on the total number of
    nodes: more nodes triggers shorter abbreviations.

- node_size:

  Size of individual detail nodes in the bottom layer. This controls the
  pie-chart radius for each node. Default 2.4.

- node_shape:

  Shape for detail nodes in the bottom layer. Supported values:
  `"circle"`, `"square"`, `"diamond"`, `"triangle"`. Can be a single
  value applied to all nodes or a character vector of length equal to
  the number of nodes (one shape per node). Default `"circle"`.

- cluster_shape:

  Accepted for backward compatibility. Summary nodes are currently drawn
  as pie charts, so this parameter does not change their shape.

- title:

  Main plot title displayed above the figure. Default `NULL` (no title).

- subtitle:

  Subtitle displayed below the title. Default `NULL` (no subtitle).

- title_size:

  Text size (`cex.main`) for the title. Default 1.2.

- subtitle_size:

  Text size (`cex.sub`) for the subtitle. Default 0.9.

- legend_position:

  Where to place the legend: `"right"`, `"left"`, `"top"`, `"bottom"`,
  or `"none"` to suppress it entirely. Default `"right"`.

- legend_size:

  Text size (`cex`) for legend labels. Default 0.7.

- legend_pt_size:

  Point size (`pt.cex`) for legend symbols. Default 1.2.

- summary_labels:

  Logical. Show cluster name labels next to the summary pie-chart nodes
  in the top layer. Default `TRUE`.

- summary_label_size:

  Text size for summary labels. Default 0.8.

- summary_label_position:

  Position of summary labels relative to nodes: 1 = below, 2 = left, 3 =
  above, 4 = right. Default 3 (above).

- summary_label_color:

  Color for summary labels. Default `"gray20"`.

- summary_arrows:

  Logical. Draw arrowheads on summary-layer directed edges. Default
  `TRUE`. For fully undirected networks prefer `directed = FALSE`, which
  also suppresses these arrowheads and draws each symmetric edge pair
  only once.

- summary_arrow_size:

  Size of arrowheads on summary edges. Default 0.10.

- node_donut:

  Logical or `NULL`. Force donut node rendering on (`TRUE`) or off
  (`FALSE`), overriding `theme`. `NULL` (default) follows the preset
  (donut for `"rich"`/`"light"`).

- node_donut_inner_ratio:

  Hole size (0–1) of the detail-node donut ring. Default 0.55.

- summary_donut_inner_ratio:

  Hole size (0–1) of the top-layer summary donut ring. Default 0.6.

- summary_donut_show_value:

  Logical. Print the fill proportion in the center of each summary
  donut. Default `FALSE`.

- curved_edges:

  Logical or `NULL`. Force curved summary edges on or off, overriding
  `theme`. `NULL` (default) follows the preset.

- summary_curve:

  Numeric or `NULL`. Curvature of curved summary edges (only used when
  curved). `NULL` auto-selects (0.25 for directed, straight for
  undirected).

- summary_pie:

  Character scalar controlling what the colored slice of the top-layer
  pie chart represents. One of:

  `"inits"`

  :   (default) The cluster's share of the initial state distribution
      (`cs$macro$inits[i]`). Answers "how often do sequences start in
      this cluster?" Summed across clusters the colored slices equal 1.

  `"self"`

  :   The cluster's self-retention share of out-strength
      (`bw[i, i] / rowSums(bw)[i]`). Answers "how sticky is this cluster
      — how much of its outgoing flow loops back to itself?" Each pie is
      normalized independently.

- edge_color_by:

  How to color edges on all layers:

  `"auto"`

  :   (default) Color edges by their cluster when the weights are
      non-negative (transition networks), but switch to sign-based
      coloring automatically when any negative weight is present
      (correlation / association networks).

  `"cluster"`

  :   Always color edges by the source cluster's color.

  `"sign"`

  :   Always color edges by weight sign — positive in
      `edge_positive_color`, negative in `edge_negative_color`.

  Sign coloring uses each edge's absolute weight for the threshold
  (`minimum`) and line-width scaling, so negative edges are drawn rather
  than dropped.

- edge_positive_color:

  Color for positive-weight edges when sign coloring is active. Default
  `"#2E7D32"` (green).

- edge_negative_color:

  Color for negative-weight edges when sign coloring is active. Default
  `"#C62828"` (red).

- between_arrows:

  Logical. Draw arrowheads on between-cluster edges in the bottom layer.
  Default `FALSE`.

- edge_width_range:

  Numeric vector `c(min, max)` controlling the line-width range for
  **within-cluster** edges in the bottom layer. The weakest edge gets
  `min` and the strongest gets `max`. Default `c(0.3, 1.3)`.

- between_edge_width_range:

  Numeric vector `c(min, max)` for **between-cluster** edges in the
  bottom layer (shell-to-shell lines). Default `c(0.5, 2.0)`.

- summary_edge_width_range:

  Numeric vector `c(min, max)` for **summary** edges in the top layer.
  Default `c(0.5, 2.0)`.

- edge_alpha:

  Transparency (0–1) for within-cluster edges. Lower values make these
  edges more subtle, keeping focus on between-cluster structure. Default
  0.35.

- between_edge_alpha:

  Transparency (0–1) for between-cluster edges in the bottom layer.
  Default 0.6.

- summary_edge_alpha:

  Transparency (0–1) for summary-layer edges. Default 0.7.

- inter_layer_alpha:

  Transparency (0–1) for the dashed inter-layer lines connecting detail
  nodes to their summary node. Lower values make these scaffolding lines
  less visually dominant. Default 0.5.

- edge_labels:

  Logical. Show numeric weight labels on within-cluster edges. Default
  `FALSE` (automatically set to `TRUE` when `mode = "tna"`).

- edge_label_size:

  Text size for within-cluster edge labels. Default 0.5.

- edge_label_color:

  Color for within-cluster edge labels. Default `"gray40"`.

- edge_label_digits:

  Number of decimal places for edge weight labels on both layers.
  Default 2.

- summary_edge_labels:

  Logical. Show numeric weight labels on summary-layer edges. Default
  `FALSE` (automatically set to `TRUE` when `mode = "tna"`).

- summary_edge_label_size:

  Text size for summary edge labels. Default 0.6.

- top_layer_scale:

  Numeric vector `c(x_scale, y_scale)` controlling the horizontal and
  vertical radii of the oval on which summary nodes are placed, as
  multiples of `spacing`. Widen with `c(1.0, 0.25)` or flatten with
  `c(0.8, 0.15)` to adjust the top-layer shape. Default `c(0.8, 0.25)`.

- inter_layer_gap:

  Vertical gap between the top of the bottom layer and the bottom of the
  top layer, as a multiple of `spacing`. Increase to separate the layers
  more. Default 0.6.

- node_radius_scale:

  Radius of the circle on which nodes are arranged inside each cluster
  shell, as a fraction of `shape_size`. Increase to push nodes outward
  toward the shell border; decrease to pack them tighter. Default 0.55.

- shell_alpha:

  Fill transparency (0–1) for cluster shells. Higher values make shells
  more opaque, giving stronger visual grouping but potentially obscuring
  edges. Default 0.15.

- shell_border_width:

  Line width for cluster shell borders. Default 0.75 (thin).
  `theme = "light"` drops the outline entirely.

- node_border_color:

  Border color for detail nodes in the bottom layer. Default `"gray30"`.

- node_border_width:

  Line width for detail-node borders in the bottom layer. Default 0.4
  (thin). Increase for heavier outlines.

- summary_border_color:

  Border color for summary pie-chart nodes. Default `"gray20"`.

- summary_border_width:

  Border line width for summary nodes. Default 0.6 (thin).

- label_color:

  Text color for detail node labels. Default `"gray20"`.

- label_position:

  Accepted for backward compatibility. Detail labels are currently
  positioned automatically to the left or right of each node.

- directed:

  Logical or `NULL`. `NULL` (default) auto-detects: a
  `cluster_summary`/`mcml` input uses its own `$meta$directed` flag;
  other objects use their `$directed` field when present; a plain matrix
  is undirected when symmetric (the same contract as
  [`splot`](https://sonsoles.me/cograph/reference/splot.md)). When
  `TRUE`, every non-zero cell of the weight matrices is drawn as a
  directed edge with an arrowhead. When `FALSE` (undirected, e.g.
  co-occurrence weights): arrowheads are suppressed on all three edge
  layers (within-cluster, between-cluster, and summary), each symmetric
  pair is drawn once instead of twice (the upper triangle is used; a
  warning is issued if the weights are not symmetric), edge labels move
  to the edge midpoint, and matrix input is aggregated with
  `type = "cooccurrence"` (symmetrized counts) instead of the
  row-normalized `type = "tna"`. Overrides `summary_arrows` and
  `between_arrows`.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns the `cluster_summary` object used for plotting. This
object can be passed back to `plot_mcml()` to avoid recomputation,
inspected with [`print()`](https://rdrr.io/r/base/print.html), or fed to
[`as_tna`](https://sonsoles.me/cograph/reference/as_tna.md) for further
analysis.

## Details

Use `plot_mcml` when you need a simultaneous micro/macro view of cluster
structure — the bottom layer reveals internal cluster dynamics while the
top layer provides a bird's-eye summary. For a flat multi-cluster plot
without the summary layer, see
[`plot_mtna`](https://sonsoles.me/cograph/reference/plot_mtna.md). For
stacked multilevel/multiplex layers, see
[`plot_mlna`](https://sonsoles.me/cograph/reference/plot_mlna.md).

**Two workflows:**

1.  **Direct**: pass a weight matrix (or tna / cograph_network object)
    together with `cluster_list`. The function calls
    [`csum`](https://sonsoles.me/cograph/reference/csum.md) internally
    to compute aggregated weights.

2.  **Pre-computed**: call
    [`csum`](https://sonsoles.me/cograph/reference/csum.md) yourself,
    inspect or modify the result, then pass the `cluster_summary` object
    as `x`. This avoids redundant computation when you plot the same
    clustering repeatedly with different visual settings.

**Mode:**

- `"weights"` (default) — displays raw aggregated edge values. Use this
  when the absolute magnitude of transitions matters.

- `"tna"` — row-normalizes the summary matrix to transition
  probabilities (rows sum to 1) and automatically enables edge labels on
  both layers (unless you explicitly set `edge_labels` or
  `summary_edge_labels` to `FALSE`).

**Directionality:** `directed = NULL` (default) auto-detects
directedness from the input: `cluster_summary`/`mcml` objects carry it
in `$meta$directed`, and plain matrices are treated as undirected when
symmetric. Directed edges get arrowheads; undirected weights (e.g.,
co-occurrence aggregations) are drawn as a single plain line per
symmetric pair on every layer, with no arrowheads. Pass
`directed = TRUE`/`FALSE` to override the detection.

**Layout logic:** Bottom-layer clusters are arranged on a circle of
radius `spacing`, flattened by the perspective `skew_angle`. Nodes
inside each cluster sit on a smaller circle of radius
`shape_size * node_radius_scale`. The top-layer summary nodes are placed
on an oval above the bottom layer whose proportions are controlled by
`top_layer_scale`.

## Input Formats

`x` accepts four types:

- **matrix**:

  A square numeric weight matrix with row/column names matching the node
  identifiers in `cluster_list`.

- **tna**:

  A TNA model object. The `$weights` matrix is extracted automatically.

- **cograph_network**:

  A cograph network object. Weights are extracted via
  [`to_matrix()`](https://sonsoles.me/cograph/reference/to_matrix.md)
  and node metadata (display labels) is read from the `$nodes` data
  frame.

- **cluster_summary**:

  A pre-computed summary from
  [`csum`](https://sonsoles.me/cograph/reference/csum.md). When this
  type is passed, the `cluster_list`, `aggregation`, and `nodes`
  parameters are ignored because the summary already contains everything
  needed.

## Edge Types

The plot contains four distinct edge categories, each with its own set
of visual parameters:

- **Within-cluster (bottom)**:

  Edges connecting nodes inside the same cluster shell. Controlled by
  `edge_width_range`, `edge_alpha`, `edge_labels`, `edge_label_size`,
  `edge_label_color`, and `edge_label_digits`.

- **Between-cluster (bottom)**:

  Edges from one cluster shell to another, drawn between shell borders.
  Controlled by `between_edge_width_range` and `between_edge_alpha`.

- **Summary (top)**:

  Edges between summary pie-chart nodes in the top layer. Controlled by
  `summary_edge_width_range`, `summary_edge_alpha`,
  `summary_edge_labels`, `summary_edge_label_size`, `summary_arrows`,
  and `summary_arrow_size`.

- **Inter-layer (dashed)**:

  Dashed lines connecting each detail node to its cluster's summary
  node. Controlled by `inter_layer_alpha`.

## Customization Quick Reference

|  |  |
|----|----|
| **Visual element** | **Key parameters** |
| Cluster spacing / perspective | `spacing`, `skew_angle` |
| Cluster shell appearance | `shape_size`, `shell_alpha`, `shell_border_width`, `colors` |
| Detail nodes | `node_size`, `node_shape`, `node_border_color` |
| Detail labels | `show_labels`, `label_size`, `label_abbrev`, `label_color`, `label_position` |
| Summary nodes | `summary_size`, `summary_border_color`, `summary_border_width` |
| Summary labels | `summary_labels`, `summary_label_size`, `summary_label_color`, `summary_label_position` |
| Within-cluster edges | `edge_width_range`, `edge_alpha`, `edge_labels` |
| Between-cluster edges | `between_edge_width_range`, `between_edge_alpha` |
| Summary edges | `summary_edge_width_range`, `summary_edge_alpha`, `summary_edge_labels`, `summary_arrows` |
| Directed vs undirected | `directed` |
| Inter-layer lines | `inter_layer_alpha` |
| Top-layer layout | `top_layer_scale`, `inter_layer_gap` |
| Title / legend | `title`, `subtitle`, `legend`, `legend_position` |

## See also

[`csum`](https://sonsoles.me/cograph/reference/csum.md) for
pre-computing aggregated cluster data,
[`plot_mtna`](https://sonsoles.me/cograph/reference/plot_mtna.md) for
flat multi-cluster visualization (no summary layer),
[`plot_mlna`](https://sonsoles.me/cograph/reference/plot_mlna.md) for
stacked multilevel/multiplex layer visualization,
[`aggregate_weights`](https://sonsoles.me/cograph/reference/aggregate_weights.md)
for the low-level weight aggregation used internally,
[`detect_communities`](https://sonsoles.me/cograph/reference/detect_communities.md)
for algorithmic cluster detection

## Examples

``` r
mat <- matrix(runif(36), 6, 6); diag(mat) <- 0
colnames(mat) <- rownames(mat) <- LETTERS[1:6]
clusters <- list(C1 = c("A","B"), C2 = c("C","D"), C3 = c("E","F"))
plot_mcml(mat, clusters)

# \donttest{
cs <- csum(mat, clusters)
plot_mcml(cs, mode = "tna", edge_labels = TRUE)

# }
```
