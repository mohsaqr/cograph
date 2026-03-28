# Plot Network as Heatmap

Visualizes a network adjacency/weight matrix as a heatmap. Supports
single networks, multi-cluster networks (block diagonal), and
multi-layer networks (group_tna).

## Usage

``` r
plot_heatmap(
  x,
  cluster_list = NULL,
  cluster_spacing = 0,
  show_legend = TRUE,
  legend_position = "right",
  legend_title = "Weight",
  colors = "viridis",
  limits = NULL,
  midpoint = NULL,
  na_color = "grey90",
  show_values = FALSE,
  value_size = 2.5,
  value_color = "black",
  value_fontface = "plain",
  value_fontfamily = "sans",
  value_halo = NULL,
  value_digits = 2,
  show_diagonal = TRUE,
  diagonal_color = NULL,
  cluster_labels = TRUE,
  cluster_borders = TRUE,
  border_color = "black",
  border_width = 0.5,
  row_labels = NULL,
  col_labels = NULL,
  show_axis_labels = TRUE,
  axis_text_size = 8,
  axis_text_angle = 45,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  threshold = 0,
  aspect_ratio = 1,
  ...
)
```

## Arguments

- x:

  Network input: matrix, CographNetwork, tna, igraph, group_tna, or any
  object cograph accepts.

- cluster_list:

  Optional list of character vectors defining node clusters. Creates a
  block-structured heatmap with clusters along diagonal.

- cluster_spacing:

  Gap size between clusters (in cell units). Default 0.

- show_legend:

  Logical: display color legend? Default TRUE.

- legend_position:

  Position: "right" (default), "left", "top", "bottom", "none".

- legend_title:

  Title for legend. Default "Weight".

- colors:

  Color palette: vector of colors for gradient, or a palette name
  ("viridis", "heat", "blues", "reds", "diverging"). Default "viridis".

- limits:

  Numeric vector c(min, max) for color scale. NULL for auto.

- midpoint:

  Midpoint for diverging scales. NULL for auto (0 if data spans
  neg/pos).

- na_color:

  Color for NA values. Default "grey90".

- show_values:

  Logical: display values in cells? Default FALSE.

- value_size:

  Text size for cell values. Default 2.5.

- value_color:

  Color for cell value text. Default "black".

- value_fontface:

  Font face for values: "plain", "bold", "italic", "bold.italic".
  Default "plain".

- value_fontfamily:

  Font family for values: "sans", "serif", "mono". Default "sans".

- value_halo:

  Halo color behind value labels for readability on dark cells. Set to a
  color (e.g., "white") to enable, or NULL (default) to disable.

- value_digits:

  Decimal places for values. Default 2.

- show_diagonal:

  Logical: show diagonal values? Default TRUE.

- diagonal_color:

  Optional color for diagonal cells. NULL uses scale.

- cluster_labels:

  Logical: show cluster/layer labels? Default TRUE.

- cluster_borders:

  Logical: draw borders around clusters? Default TRUE.

- border_color:

  Color for cluster borders. Default "black".

- border_width:

  Width of cluster borders. Default 0.5.

- row_labels:

  Row labels. NULL for auto (rownames or indices).

- col_labels:

  Column labels. NULL for auto (colnames or indices).

- show_axis_labels:

  Logical: show axis tick labels? Default TRUE.

- axis_text_size:

  Size of axis labels. Default 8.

- axis_text_angle:

  Angle for x-axis labels. Default 45.

- title:

  Plot title. Default NULL.

- subtitle:

  Plot subtitle. Default NULL.

- xlab:

  X-axis label. Default NULL.

- ylab:

  Y-axis label. Default NULL.

- threshold:

  Minimum absolute value to display. Values with
  `abs(value) < threshold` are set to zero. Default 0.

- aspect_ratio:

  Aspect ratio. Default 1 (square cells).

- ...:

  Additional arguments (currently unused).

## Value

A ggplot2 object.

## Details

For multi-cluster networks, provide `cluster_list` as a named list where
each element is a vector of node names belonging to that cluster. The
heatmap will be reordered to show clusters as blocks along the diagonal.

For group_tna objects (multiple separate networks), each network becomes
a diagonal block. Off-diagonal blocks are empty (no inter-layer edges)
unless the networks share nodes.

## Examples

``` r
# \donttest{
# Single network
m <- matrix(runif(25), 5, 5)
rownames(m) <- colnames(m) <- LETTERS[1:5]
plot_heatmap(m)


# With clusters
clusters <- list(Group1 = c("A", "B"), Group2 = c("C", "D", "E"))
plot_heatmap(m, cluster_list = clusters)


# Custom colors and legend
plot_heatmap(m, colors = "heat", limits = c(0, 1), show_values = TRUE)

# }

# \donttest{
# Multi-layer (group_tna) — requires tna package and sequence data
if (requireNamespace("tna", quietly = TRUE)) {
  mod <- tna::tna(tna::group_regulation)
  plot_heatmap(mod)
}

# }
```
