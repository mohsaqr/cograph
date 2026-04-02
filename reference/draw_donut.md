# Draw Donut Node

Draw a donut chart node showing a fill proportion (0-1) as an arc. The
fill starts from 12 o'clock (top) and fills clockwise.

## Usage

``` r
draw_donut(
  x,
  y,
  size,
  fill,
  border_color,
  border_width,
  alpha = 1,
  values = NULL,
  colors = NULL,
  inner_ratio = 0.5,
  bg_color = "gray90",
  show_value = TRUE,
  value_size = 8,
  value_color = "black",
  value_fontface = "bold",
  value_fontfamily = "sans",
  value_digits = 2,
  value_prefix = "",
  value_suffix = "",
  value_format = NULL,
  donut_border_width = NULL,
  ...
)
```

## Arguments

- x, y:

  Node center coordinates (NPC units).

- size:

  Node radius (NPC units).

- fill:

  Fill color for the donut ring.

- border_color:

  Border color.

- border_width:

  Border line width.

- alpha:

  Transparency (0-1).

- values:

  Single numeric value (0-1) specifying fill proportion. 0.1 = 10%
  filled arc, 0.5 = 50% filled, 1.0 = full ring.

- colors:

  Override fill color (optional).

- inner_ratio:

  Ratio of inner to outer radius (0-1). Default 0.5.

- bg_color:

  Background color for unfilled portion. Default "gray90".

- show_value:

  Logical: show value in center? Default FALSE.

- value_size:

  Font size for center value.

- value_color:

  Color for center value text.

- value_fontface:

  Font face for center value.

- value_fontfamily:

  Font family for center value.

- value_digits:

  Decimal places for value display.

- value_prefix:

  Text before value (e.g., "\$").

- value_suffix:

  Text after value (e.g., "%").

- value_format:

  Custom format function (overrides digits).

- donut_border_width:

  Border width for donut ring (NULL = use border_width).
