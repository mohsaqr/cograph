# Plot Alluvial Diagram

Creates an alluvial (Sankey) diagram showing aggregated flows between
states. This is an alias for
[`plot_transitions()`](https://sonsoles.me/cograph/reference/plot_transitions.md)
with aggregated flows (default).

## Usage

``` r
plot_alluvial(
  x,
  from_title = "From",
  to_title = "To",
  title = NULL,
  from_colors = NULL,
  to_colors = NULL,
  flow_fill = "#888888",
  flow_alpha = 0.4,
  flow_color_by = NULL,
  flow_border = NA,
  flow_border_width = 0.5,
  node_width = 0.08,
  node_border = NA,
  node_spacing = 0.02,
  label_size = 3.5,
  label_position = c("beside", "inside", "above", "below", "outside"),
  label_halo = TRUE,
  label_color = "black",
  label_fontface = "plain",
  label_nudge = 0.02,
  title_size = 5,
  title_color = "black",
  title_fontface = "bold",
  curve_strength = 0.6,
  show_values = FALSE,
  value_position = c("center", "origin", "destination", "outside_origin",
    "outside_destination"),
  value_size = 3,
  value_color = "black",
  value_halo = NULL,
  value_fontface = "bold",
  value_nudge = 0.03,
  value_min = 0,
  show_totals = FALSE,
  total_size = 4,
  total_color = "white",
  total_fontface = "bold",
  conserve_flow = TRUE,
  min_flow = 0,
  threshold = 0,
  value_digits = 2,
  column_gap = 1
)
```

## Arguments

- x:

  Input data in one of several formats:

  - A transition matrix (rows = from, cols = to, values = counts)

  - Two vectors: pass `before` as x and `after` as second argument
    (contingency table computed automatically, like chi-square)

  - A 2-column data frame (raw observations; table computed
    automatically)

  - A data frame with columns: from, to, count

  - A list of matrices for multi-step transitions

- from_title:

  Title for the left column. Default "From". For multi-step, use a
  vector of titles (e.g., c("T1", "T2", "T3", "T4")).

- to_title:

  Title for the right column. Default "To". Ignored for multi-step.

- title:

  Optional plot title. Applied via ggplot2::labs(title = title).

- from_colors:

  Colors for left-side nodes. Default uses palette.

- to_colors:

  Colors for right-side nodes. Default uses palette.

- flow_fill:

  Fill color for flows. Default "#888888" (grey). Ignored if
  flow_color_by is set.

- flow_alpha:

  Alpha transparency for flows. Default 0.4.

- flow_color_by:

  Color flows by "source", "destination", or NULL (use flow_fill).
  Default NULL.

- flow_border:

  Border color for flows. Default NA (no border).

- flow_border_width:

  Line width for flow borders. Default 0.5.

- node_width:

  Width of node rectangles (0-1 scale). Default 0.08.

- node_border:

  Border color for nodes. Default NA (no border).

- node_spacing:

  Vertical spacing between nodes (0-1 scale). Default 0.02.

- label_size:

  Size of node labels. Default 3.5.

- label_position:

  Position of node labels: "beside" (default), "inside", "above",
  "below", "outside". Applied to first and last columns. See
  `mid_label_position` for middle columns.

- label_halo:

  Logical: add white halo around labels for readability? Default TRUE.

- label_color:

  Color of state name labels. Default "black".

- label_fontface:

  Font face of state name labels ("plain", "bold", "italic",
  "bold.italic"). Default "plain".

- label_nudge:

  Distance between node edge and label (in plot units). Default 0.02.
  Increase for more spacing.

- title_size:

  Size of column titles. Default 5.

- title_color:

  Color of column title text. Default "black".

- title_fontface:

  Font face of column titles. Default "bold".

- curve_strength:

  Controls bezier curve shape (0-1). Default 0.6.

- show_values:

  Logical: show transition counts on flows? Default FALSE.

- value_position:

  Position of flow values: "center", "origin", "destination",
  "outside_origin", "outside_destination". Default "center".

- value_size:

  Size of value labels on flows. Default 3.

- value_color:

  Color of value labels. Default "black".

- value_halo:

  Logical: add halo around flow value labels? Default NULL (inherits
  from `label_halo`).

- value_fontface:

  Font face of flow value labels. Default "bold".

- value_nudge:

  Distance of value labels from node edge when using "origin" or
  "destination" positions. Default 0.03.

- value_min:

  Minimum count to show a flow value label. Default 0 (show all). Use to
  hide small flows (e.g., `value_min = 100`).

- show_totals:

  Logical: show total counts on nodes? Default FALSE.

- total_size:

  Size of total labels. Default 4.

- total_color:

  Color of total labels. Default "white".

- total_fontface:

  Font face of total labels. Default "bold".

- conserve_flow:

  Logical: should left and right totals match? Default TRUE. When FALSE,
  each side scales independently (allows for "lost" or "gained" items).

- min_flow:

  Minimum flow value to display. Default 0 (show all).

- threshold:

  Minimum edge weight to display. Flows below this value are removed.
  Combined with `min_flow`: effective minimum is
  `max(threshold, min_flow)`. Default 0.

- value_digits:

  Number of decimal places for flow value labels and node totals.
  Default 2.

- column_gap:

  Horizontal spread of columns (0-1). Default 1 uses full width. Use
  smaller values (e.g., 0.6) to bring columns closer together.

## Value

A ggplot2 object.

## See also

[`plot_transitions`](https://sonsoles.me/cograph/reference/plot_transitions.md),
[`plot_trajectories`](https://sonsoles.me/cograph/reference/plot_trajectories.md)

## Examples

``` r
mat <- matrix(c(50, 10, 5, 15, 40, 10), 2, 3)
rownames(mat) <- c("A", "B")
colnames(mat) <- c("X", "Y", "Z")
plot_alluvial(mat)

```
