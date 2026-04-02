# Draw Pie Node

Draw a pie chart node with multiple segments.

## Usage

``` r
draw_pie(
  x,
  y,
  size,
  fill,
  border_color,
  border_width,
  alpha = 1,
  values = NULL,
  colors = NULL,
  pie_border_width = NULL,
  default_color = NULL,
  ...
)
```

## Arguments

- pie_border_width:

  Border width for pie segments (optional, defaults to border_width \*
  0.5).

- default_color:

  Fallback color when colors is NULL and there's a single segment.
