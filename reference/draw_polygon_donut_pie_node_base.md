# Draw Polygon Donut with Inner Pie

Renders a polygon-shaped donut node with a circular pie chart inside.
Supports hexagon, square, diamond, triangle, pentagon shapes for the
outer ring.

## Usage

``` r
draw_polygon_donut_pie_node_base(
  x,
  y,
  size,
  donut_value = 1,
  donut_color = "#4A90D9",
  donut_shape = "hexagon",
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

  Value (0-1) for the donut ring fill proportion.

- donut_color:

  Color for the filled portion of donut.

- donut_shape:

  Shape of the donut: "hexagon", "square", "diamond", "triangle",
  "pentagon".

- pie_values:

  Numeric vector for pie slices.

- pie_colors:

  Colors for pie slices.

- pie_default_color:

  Default pie color if pie_colors not provided.

- inner_ratio:

  Ratio of inner to outer radius (0-1).

- bg_color:

  Background color of the donut ring.

- border.col:

  Border color.

- border.width:

  Border line width.

- pie_border.width:

  Border width for pie slice dividers.

- donut_border.width:

  Border width for donut ring.
