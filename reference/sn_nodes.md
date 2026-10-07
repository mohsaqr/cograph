# Set Node Aesthetics

Sets the visual properties of the nodes in a network plot. Every
argument except `network` defaults to `NULL`, which leaves the current
setting unchanged; the defaults stated below are the values used at
render time.

## Usage

``` r
sn_nodes(
  network,
  size = NULL,
  shape = NULL,
  node_svg = NULL,
  svg_preserve_aspect = NULL,
  fill = NULL,
  border_color = NULL,
  border_width = NULL,
  alpha = NULL,
  label_size = NULL,
  label_color = NULL,
  label_position = NULL,
  show_labels = NULL,
  pie_values = NULL,
  pie_colors = NULL,
  pie_border_width = NULL,
  donut_fill = NULL,
  donut_values = NULL,
  donut_color = NULL,
  donut_colors = NULL,
  donut_border_width = NULL,
  donut_inner_ratio = NULL,
  donut_bg_color = NULL,
  donut_shape = NULL,
  donut_show_value = NULL,
  donut_value_size = NULL,
  donut_value_color = NULL,
  donut_value_fontface = NULL,
  donut_value_fontfamily = NULL,
  donut_value_digits = NULL,
  donut_value_prefix = NULL,
  donut_value_suffix = NULL,
  donut_value_format = NULL,
  donut2_values = NULL,
  donut2_colors = NULL,
  donut2_inner_ratio = NULL,
  label_fontface = NULL,
  label_fontfamily = NULL,
  label_hjust = NULL,
  label_vjust = NULL,
  label_angle = NULL,
  node_names = NULL
)
```

## Arguments

- network:

  A cograph_network object, matrix, data.frame, or igraph object.
  Matrices and other inputs are auto-converted.

- size:

  Node size. Can be a single value, vector (per-node), or column name.

- shape:

  Node shape. One of the shapes listed by
  [`list_shapes()`](https://sonsoles.me/cograph/reference/shapes.md)
  (for example "circle", "square", "triangle", "diamond", "pentagon",
  "hexagon", "ellipse", "heart", "star", "pie", "donut", "cross",
  "rectangle"), or a custom SVG shape registered with
  [`register_svg_shape()`](https://sonsoles.me/cograph/reference/shapes.md).

- node_svg:

  Custom SVG for the node shape: a path to an SVG file or an inline SVG
  string. It is registered as a temporary shape and replaces `shape`.

- svg_preserve_aspect:

  Logical: maintain the SVG aspect ratio? The value is stored with the
  network, but the renderer always preserves the aspect ratio, so it has
  no effect at present.

- fill:

  Node fill color. Can be a single color, vector, or column name.

- border_color:

  Node border color.

- border_width:

  Node border width.

- alpha:

  Node opacity in \[0, 1\]. Values outside this range raise an error.

- label_size:

  Label text size.

- label_color:

  Label text color.

- label_position:

  Label position: "center" (render default), "above", "below", "left" or
  "right".

- show_labels:

  Logical. Show node labels? Default TRUE.

- pie_values:

  For pie shape: list or matrix of values for pie segments. Each element
  corresponds to a node and contains values for its segments.

- pie_colors:

  For pie shape: colors for pie segments.

- pie_border_width:

  Border width for pie chart nodes.

- donut_fill:

  For donut shape: numeric value (0-1) specifying fill proportion. A
  value of 0.5 fills half of the ring and 1 fills the whole ring. Can be
  a single value (all nodes) or vector (per-node values).

- donut_values:

  Deprecated. Use `donut_fill`. Ignored when `donut_fill` is supplied.

- donut_color:

  For donut shape: fill color(s) for the donut ring. Single color sets
  fill for all nodes. Two colors set fill and background for all nodes.
  More than 2 colors set per-node fill colors (recycled to n_nodes). The
  render default is a "maroon" fill on a "gray90" background.

- donut_colors:

  Deprecated. Use `donut_color`. Ignored when `donut_color` is supplied.

- donut_border_width:

  Border width for donut chart nodes.

- donut_inner_ratio:

  For donut shape: inner radius ratio (0-1). Default 0.5.

- donut_bg_color:

  For donut shape: background color for unfilled portion.

- donut_shape:

  For donut: base shape for ring ("circle", "square", "hexagon",
  "triangle", "diamond", "pentagon"). Default NULL, which inherits the
  ring shape from the node's own shape (hexagon nodes get hexagon
  donuts); set it explicitly to override that for every node.

- donut_show_value:

  For donut shape: show value in center? Default FALSE.

- donut_value_size:

  For donut shape: font size for center value.

- donut_value_color:

  For donut shape: color for center value text.

- donut_value_fontface:

  For donut shape: font face for center value ("plain", "bold",
  "italic", "bold.italic"). Default "bold".

- donut_value_fontfamily:

  For donut shape: font family for center value ("sans", "serif",
  "mono"). Default "sans".

- donut_value_digits:

  For donut shape: decimal places for value display. Default 2.

- donut_value_prefix:

  For donut shape: text before value (e.g., "\$"). Default "".

- donut_value_suffix:

  For donut shape: text after value (e.g., "%"). Default "".

- donut_value_format:

  For donut shape: a function that formats the center value. It replaces
  `donut_value_digits`. A non-function raises an error.

- donut2_values:

  For double donut: list of values for inner donut ring.

- donut2_colors:

  For double donut: colors for inner donut ring segments.

- donut2_inner_ratio:

  For double donut: inner radius ratio for inner donut ring. Default
  0.4.

- label_fontface:

  Font face for node labels: "plain", "bold", "italic", "bold.italic".
  Default "plain".

- label_fontfamily:

  Font family for node labels: "sans", "serif", "mono", or system font.
  Default "sans".

- label_hjust:

  Horizontal justification for node labels (0=left, 0.5=center,
  1=right). Default 0.5.

- label_vjust:

  Vertical justification for node labels (0=bottom, 0.5=center, 1=top).
  Default 0.5.

- label_angle:

  Text rotation angle in degrees for node labels. Default 0.

- node_names:

  Alternative names for legend (separate from display labels).

## Value

The input as a `cograph_network` object, with the supplied settings
merged into its node aesthetics. It can be piped to further
customization or plotting functions.

## Details

### Vectorization

The arguments `size`, `shape`, `fill`, `border_color`, `border_width`,
`alpha`, `label_size`, `label_color`, `label_position` and `node_names`
accept a single value, which is applied to all nodes, or a per-node
vector. A vector of another length is recycled to the number of nodes
without a warning. A single string that matches a column of the node
data frame is replaced by that column.

### Donut Charts

A donut node shows one proportion in \[0, 1\] per node. `donut_fill`
sets the proportion, `donut_color` the ring color, `donut_shape` the
base shape of the ring, and `donut_show_value = TRUE` prints the value
in the center.

## See also

[`sn_edges`](https://sonsoles.me/cograph/reference/sn_edges.md) for edge
customization,
[`cograph`](https://sonsoles.me/cograph/reference/cograph.md) for
network creation,
[`splot`](https://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](https://sonsoles.me/cograph/reference/soplot.md) for
plotting,
[`sn_layout`](https://sonsoles.me/cograph/reference/sn_layout.md) for
layout algorithms,
[`sn_theme`](https://sonsoles.me/cograph/reference/sn_theme.md) for
visual themes

## Examples

``` r
cograph(regulation_net) |>
  sn_nodes(fill = "steelblue", shape = "square") |>
  splot()
```
