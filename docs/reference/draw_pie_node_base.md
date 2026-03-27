# Draw Pie Chart Node

Renders a node as a pie chart with multiple colored segments. The pie is
drawn slightly inside the node boundary to leave room for arrows.

## Usage

``` r
draw_pie_node_base(
  x,
  y,
  size,
  values,
  colors = NULL,
  default_color = NULL,
  border.col = "black",
  border.width = 1,
  pie_border.width = NULL
)
```

## Arguments

- x, y:

  Node center coordinates.

- size:

  Node radius.

- values:

  Numeric vector of values (will be normalized to proportions).

- colors:

  Vector of colors for each segment.

- default_color:

  Fallback color when colors is NULL and values length is 1.

- border.col:

  Border color.

- border.width:

  Border line width.

- pie_border.width:

  Border width for pie slice dividers (NULL = use border.width).
