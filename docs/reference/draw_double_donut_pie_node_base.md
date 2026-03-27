# Draw Double Donut with Inner Pie

Renders a node with two concentric donut rings and an optional inner pie
chart. From outside to inside: outer donut ring, inner donut ring,
center pie.

## Usage

``` r
draw_double_donut_pie_node_base(
  x,
  y,
  size,
  donut_values = NULL,
  donut_colors = NULL,
  donut2_values = NULL,
  donut2_colors = NULL,
  pie_values = NULL,
  pie_colors = NULL,
  pie_default_color = NULL,
  outer_inner_ratio = 0.7,
  inner_inner_ratio = 0.4,
  bg_color = "gray90",
  border.col = "black",
  border.width = 1,
  pie_border.width = NULL,
  donut_border.width = NULL
)
```

## Arguments

- x, y:

  Node center coordinates.

- size:

  Outer radius.

- donut_values:

  Values for outer donut ring (vector for segments, or single 0-1 for
  progress).

- donut_colors:

  Colors for outer donut segments.

- donut2_values:

  Values for inner donut ring (vector for segments, or single 0-1 for
  progress).

- donut2_colors:

  Colors for inner donut segments.

- pie_values:

  Numeric vector for center pie segments.

- pie_colors:

  Vector of colors for pie segments.

- pie_default_color:

  Default color for pie when pie_colors is NULL.

- outer_inner_ratio:

  Where outer donut ends (inner radius as ratio of outer radius).
  Default 0.7.

- inner_inner_ratio:

  Where inner donut ends (inner radius as ratio of outer radius).
  Default 0.4.

- bg_color:

  Background color for unfilled portions.

- border.col:

  Border color.

- border.width:

  Border line width.

- pie_border.width:

  Border width for pie slice dividers.

- donut_border.width:

  Border width for donut rings.
