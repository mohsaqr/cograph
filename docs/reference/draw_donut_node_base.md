# Draw Donut Chart Node

Renders a node as a donut chart with an inner hole. The donut shows a
fill proportion (0-1) as an arc starting from 12 o'clock.

## Usage

``` r
draw_donut_node_base(
  x,
  y,
  size,
  values,
  colors = NULL,
  default_color = NULL,
  inner_ratio = 0.5,
  bg_color = "gray90",
  center_color = "white",
  border.col = "black",
  border.width = 1,
  donut_border.width = NULL,
  outer_border.col = NULL,
  border.lty = 1,
  show_value = TRUE,
  value_cex = 0.8,
  value_col = "black",
  value_fontface = "bold",
  value_fontfamily = "sans",
  value_digits = 2,
  value_prefix = "",
  value_suffix = ""
)
```

## Arguments

- x, y:

  Node center coordinates.

- size:

  Outer radius.

- values:

  Single numeric value (0-1) specifying fill proportion. 0.1 = 10%
  filled arc, 0.5 = 50% filled, 1.0 = full ring.

- colors:

  Fill color for the donut ring.

- default_color:

  Fallback color when colors is NULL.

- inner_ratio:

  Ratio of inner to outer radius (0-1). Default 0.5.

- bg_color:

  Background color for unfilled portion. Default "gray90".

- border.col:

  Border color.

- border.width:

  Border line width.

- donut_border.width:

  Border width for donut ring (NULL = use border.width).

- show_value:

  Logical: show value in center? Default FALSE.

- value_cex:

  Text size for center value.

- value_col:

  Text color for center value.

- value_fontface:

  Font face for center value ("plain", "bold", "italic", "bold.italic").

- value_fontfamily:

  Font family for center value ("sans", "serif", "mono").

- value_digits:

  Decimal places for value display.

- value_prefix:

  Text before value (e.g., "\$").

- value_suffix:

  Text after value (e.g., "%").
