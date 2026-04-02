# Draw Double Donut with Inner Pie Node

Draw a node with two concentric donut rings and an optional inner pie
chart. From outside to inside: outer donut ring, inner donut ring,
center pie.

## Usage

``` r
draw_double_donut_pie(
  x,
  y,
  size,
  fill,
  border_color,
  border_width,
  alpha = 1,
  donut_values = NULL,
  donut_colors = NULL,
  donut2_values = NULL,
  donut2_colors = NULL,
  pie_values = NULL,
  pie_colors = NULL,
  outer_inner_ratio = 0.7,
  inner_inner_ratio = 0.4,
  bg_color = "gray90",
  pie_border_width = NULL,
  donut_border_width = NULL,
  ...
)
```

## Arguments

- pie_border_width:

  Border width for pie segments (optional).

- donut_border_width:

  Border width for donut rings (optional).
