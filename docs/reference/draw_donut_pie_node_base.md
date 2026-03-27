# Draw Donut with Inner Pie

Renders a node with outer donut ring and inner pie chart.

## Usage

``` r
draw_donut_pie_node_base(
  x,
  y,
  size,
  donut_value = 1,
  donut_color = "#4A90D9",
  pie_values = NULL,
  pie_colors = NULL,
  pie_default_color = NULL,
  inner_ratio = 0.5,
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

- donut_value:

  Single value (0-1) for donut progress.

- donut_color:

  Fill color for donut ring.

- pie_values:

  Numeric vector for pie segments.

- pie_colors:

  Vector of colors for pie segments.

- pie_default_color:

  Default color for pie when pie_colors is NULL.

- inner_ratio:

  Ratio of inner to outer radius.

- bg_color:

  Background color.

- border.col:

  Border color.

- border.width:

  Border line width.

- pie_border.width:

  Border width for pie slice dividers (NULL = use border.width \* 0.5).

- donut_border.width:

  Border width for donut ring (NULL = use border.width).
